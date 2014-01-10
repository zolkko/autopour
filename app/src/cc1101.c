#ifdef __AVR__
#include <util/delay.h>
#endif

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include "ccx_hw.h"
#include "rf.h"
#include "cc1101.h"


#ifdef __AVR__
#define _DELAY_US(x) _delay_us(x)
#endif

/*

// receive data via RF, rxData must be at least CCx_PACKT_LEN bytes long
bool cc1101_receive(const rf_handle_t * rf, uint8_t * data, uint8_t * data_len, uint8_t * src_addr, uint8_t * dest_addr, uint8_t * rssi , uint8_t * lqi)
{
    uint8_t stat;

    cc1101_read(rf, CCx_RXFIFO, data_len);
    if (*data_len == 0)
    {
        return false;
    }
    
    cc1101_read(rf, CCx_RXFIFO, dest_addr);
    cc1101_read(rf, CCx_RXFIFO, src_addr);
    *data_len -= 2;  // discard address bytes from payloadLen

    cc1101_burst_read(rf, CCx_RXFIFO, data, *data_len);
    
    if (rssi != NULL) {
        cc1101_read(rf, CCx_RXFIFO, rssi);
        *rssi = cc1101_rssi_decode(*rssi);
    } else {
        cc1101_read(rf, CCx_RXFIFO, NULL);
    }

    if (lqi != NULL) {
        stat = cc1101_read(rf, CCx_RXFIFO, lqi);
        if ((*lqi & 0x80) == 0) {
            return false;
        }
        *lqi = *lqi & 0x7F; // strip off the CRC bit
    } else {
        stat = cc1101_read(rf, CCx_RXFIFO, NULL);
    }

    // handle potential RX overflows by flushing the RF FIFO as described in section 10.1 of the CC 1100 datasheet
    if ((stat & 0xF0) == 0x60) { //Modified by Icing. When overflows, STATE[2:0] = 110
        cc1101_strobe(rf, CCx_SFRX); // flush the RX buffer
        return false;
    }

    return true;
}
*/

#include "rf.h"
#include "ccx_hw.h"


static uint8_t cc1101_version(const rf_t * self);

static uint8_t cc1101_part_number(const rf_t * self);

static void cc1101_reset(const ccx_hw_t *);

static void cc1101_configure(const ccx_hw_t *);

static uint8_t cc1101_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data);

static uint8_t cc1101_write(const ccx_hw_t * hw, uint8_t addr, uint8_t data);

static uint8_t cc1101_burst_write(const ccx_hw_t * self, uint8_t addr, const uint8_t * data, uint8_t data_size);

static uint8_t cc1101_burst_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data, uint8_t data_size);

static uint8_t cc1101_strobe(const ccx_hw_t * self, uint8_t addr);

#define cc1101_strobe_flush_tx(X)  cc1101_strobe(X, CCx_SFTX)

#define cc1101_strobe_fast_tx(X)   cc1101_strobe(X, CCx_SFSTXON)

#define cc1101_strobe_flush_rx(X)  cc1101_strobe(X, CCx_SFRX)

#define cc1101_strobe_ide(X)       cc1101_strobe(X, CCx_SIDLE)

#define cc1101_strobe_transmit(X)  cc1101_strobe(X, CCx_STX)

#define cc1101_strobe_calibrate(X) cc1101_strobe(X, CCx_SCAL)

#define cc1101_strobe_nop(X)       cc1101_strobe(X, CCx_SNOP)


#define DECL_HW(X, V) ccx_hw_t * X = (ccx_hw_t *) V->priv


uint8_t cc1101_strobe(const ccx_hw_t * self, uint8_t addr)
{
	return ccx_write(self, addr);
}


uint8_t cc1101_status_reg(const ccx_hw_t * self, uint8_t addr, uint8_t * data)
{
	uint8_t status = ccx_write(self, addr | CC1101_RW_BIT_bm);
	if (data != NULL) {
		*data = ccx_write(self, addr);
	} else {
		ccx_write(self, addr);
	}
	return status;
}


uint8_t cc1101_write(const ccx_hw_t * self, uint8_t addr, uint8_t data)
{
	uint8_t status = ccx_write(self, addr);
	ccx_write(self, data);
	return status;
}


uint8_t cc1101_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data)
{
	uint8_t status = ccx_write(self, addr | CC1101_RW_BIT_bm);
	if (data != NULL) {
		*data = ccx_write(self, 0);
	} else {
		ccx_write(self, 0);
	}
	return status;
}


uint8_t cc1101_burst_write(const ccx_hw_t * self, uint8_t addr, const uint8_t * data, uint8_t data_size)
{
	uint8_t status = ccx_write(self, addr | CC1101_BURST_BIT_bm);
    for (uint8_t i = 0; i < data_size; i++) {
		ccx_write(self, data[i]);
    }
    return status;	
}


uint8_t cc1101_burst_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data, uint8_t data_size)
{
	uint8_t status = ccx_write(self, addr | CC1101_RW_BIT_bm | CC1101_BURST_BIT_bm);
	for (uint8_t i = 0; i < data_size; i++) {
		data[i] = ccx_write(self, 0);
	}
	return status;
}


/**
 * Strobe chip select low/high
 * I use here smallest possible value because cc1101 supports up to 10Mhz clock
 * and about 6.5Mhz strobe for bulk write operation.
 */
void cc1101_reset(const ccx_hw_t * hw)
{
	ccx_chip_select(hw);
    _DELAY_US(1);
	ccx_chip_release(hw);

    _DELAY_US(50);

    while ( ccx_ready(hw) );

	ccx_chip_select(hw);
    while ( ! ccx_ready(hw) );

	ccx_write(hw, CCx_SRES);

    while ( ! ccx_ready(hw) );
	ccx_chip_release(hw);
}


void cc1101_configure(const ccx_hw_t * hw)
{
	ccx_chip_select(hw);
	while ( !ccx_ready(hw) ) ;

	//
    cc1101_write(hw, CCx_FSCTRL1, 0x08);  // FSCTRL1   Frequency synthesizer control.
    cc1101_write(hw, CCx_FSCTRL0, 0x00);  // FSCTRL0   Frequency synthesizer control.
    
    cc1101_write(hw, CCx_FREQ2, 0x23);    // FREQ2     Frequency control word, high unsigned char.
    cc1101_write(hw, CCx_FREQ1, 0x31);    // FREQ1     Frequency control word, middle unsigned char.
    cc1101_write(hw, CCx_FREQ0, 0x3B);    // FREQ0     Frequency control word, low unsigned char.
    
    cc1101_write(hw, CCx_MDMCFG4, 0x7B);  // MDMCFG4   Modem configuration.
    cc1101_write(hw, CCx_MDMCFG3, 0x83);  // MDMCFG3   Modem configuration.
    cc1101_write(hw, CCx_MDMCFG2, 0x03);  // MDMCFG2   Modem configuration.
    cc1101_write(hw, CCx_MDMCFG1, 0x22);  // MDMCFG1   Modem configuration.
    cc1101_write(hw, CCx_MDMCFG0, 0xF8);  // MDMCFG0   Modem configuration.
    
    cc1101_write(hw, CCx_CHANNR, 0x00);   // CHANNR    Channel number.
    cc1101_write(hw, CCx_DEVIATN, 0x42);  // DEVIATN   Modem deviation setting (when FSK modulation is enabled).
    cc1101_write(hw, CCx_FREND1, 0xB6);   // FREND1    Front end RX configuration.
    cc1101_write(hw, CCx_FREND0, 0x10);   // FREND0    Front end TX configuration.
    cc1101_write(hw, CCx_MCSM0, 0x18);    // MCSM0     Main Radio Control State Machine configuration.
    cc1101_write(hw, CCx_FOCCFG, 0x1D);   // FOCCFG    Frequency Offset Compensation Configuration.
    cc1101_write(hw, CCx_BSCFG, 0x1C);    // BSCFG     Bit synchronization Configuration.
    
    cc1101_write(hw, CCx_AGCCTRL2, 0xC7); // AGCCTRL2  AGC control.
    cc1101_write(hw, CCx_AGCCTRL1, 0x00); // AGCCTRL1  AGC control.
    cc1101_write(hw, CCx_AGCCTRL0, 0xB2); // AGCCTRL0  AGC control.
    
    cc1101_write(hw, CCx_FSCAL3, 0xEA);   // FSCAL3    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSCAL2, 0x2A);   // FSCAL2    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSCAL1, 0x00);   // FSCAL1    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSCAL0, 0x1F);   // FSCAL0    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSTEST, 0x59);   // FSTEST    Frequency synthesizer calibration.
    
    cc1101_write(hw, CCx_TEST2, 0x81);    // TEST2     Various test settings.
    cc1101_write(hw, CCx_TEST1, 0x35);    // TEST1     Various test settings.
    cc1101_write(hw, CCx_TEST0, 0x09);    // TEST0     Various test settings.
    
    cc1101_write(hw, CCx_FIFOTHR, 0x0E);  // FIFOTHR   RXFIFO and TXFIFO thresholds.
    
    cc1101_write(hw, CCx_IOCFG2, 0x09);   // IOCFG2    GDO2 output pin configuration.0x09-CCA mode, 0x2E-High impedance
    
    // IOCFG0D - GDO0 output pin configuration.
    cc1101_write(hw, CCx_IOCFG0D, GDOx_CFG_RX_THR_RX_EMPTY);
    
    cc1101_write(hw, CCx_PKTCTRL1, 0x04); // PKTCTRL1  Packet automation control. bit2 = 1 append RSSI and LQI ,bit2 = 0 not append
    cc1101_write(hw, CCx_PKTCTRL0, 0x05); // PKTCTRL0  Packet automation control.
    cc1101_write(hw, CCx_ADDR, 0x00);     // ADDR      Device address.
    cc1101_write(hw, CCx_PKTLEN, CCx_PACKT_LEN);
	//

	ccx_chip_release(hw);
}


uint8_t cc1101_version(const rf_t * self)
{
    DECL_HW(hw, self);

	ccx_chip_select(hw);
	while ( !ccx_ready(hw) ) ;

	uint8_t version = 0;
	cc1101_read(hw, CCx_VERSION, &version);

	ccx_chip_release(hw);

	return version;
}


uint8_t cc1101_part_number(const rf_t * self)
{
	DECL_HW(hw, self);

	ccx_chip_select(hw);
	while ( !ccx_ready(hw) ) ;

	uint8_t part_number;
	cc1101_read(hw, CCx_PARTNUM, &part_number);

	ccx_chip_release(hw);

	return part_number;
}


uint8_t cc1101_rssi_decode(uint8_t rssi_enc)
{
	unsigned char rssi;
	
	// is actually dataRate dependent, but for simplicity assumed to be fixed.
	unsigned char rssi_offset = 74;

	// RSSI is coded as 2's complement see section 17.3 RSSI of the cc1100 data-sheet
	if (rssi_enc >= 128)
		rssi = (( rssi_enc - 256) >> 1) - rssi_offset;
	else
		rssi = (rssi_enc >> 1) - rssi_offset;
	
	return rssi;
}


int8_t cc1101_transmit(const rf_t * self, const uint8_t * data, uint8_t data_size, uint8_t src_addr, uint8_t dst_addr)
{
	DECL_HW(hw, self);
	uint8_t status;
	
	ccx_chip_select(hw);
	while ( !ccx_ready(hw) ) ;

	cc1101_write(hw, CCx_TXFIFO, data_size + 2);
	cc1101_write(hw, CCx_TXFIFO, dst_addr);
	cc1101_write(hw, CCx_TXFIFO, src_addr);
	status = cc1101_burst_write(hw, CCx_TXFIFO, data, data_size);
	ccx_chip_release(hw);

	ccx_chip_select(hw);
	while ( !ccx_ready(hw) ) ;

	status &= CC1101_STATUS_STATE_bm;
	while (status != CC1101_STATUS_STATE_IDLE_bm && status != CC1101_STATUS_STATE_RX_bm) {
		status = cc1101_strobe(hw, CCx_SNOP) & CC1101_STATUS_STATE_bm;
	}

	uint8_t marc_state;
	do {
		cc1101_read(hw, CCx_MARCSTATE, &marc_state);
		marc_state &= CC1101_MARC_bm;
	} while (marc_state != CC1101_MARC_IDLE_gc && marc_state != CC1101_MARC_RX_gc && marc_state != CC1101_MARC_RX_END_gc && marc_state != CC1101_MARC_RX_RST_gc);

	cc1101_strobe(hw, CCx_STX);
	ccx_chip_release(hw);

	do {
		ccx_chip_select(hw);
		while ( !ccx_ready(hw) ) ;

		status = cc1101_strobe_nop(hw);

		switch (status & CC1101_STATUS_STATE_bm) {
			case CC1101_STATUS_STATE_TX_bm:
			case CC1101_STATUS_STATE_FSTXON_bm:
				ccx_chip_release(hw);
				continue;

			case CC1101_STATUS_STATE_CALIBRATE_bm:
			case CC1101_STATUS_STATE_SETTLING_bm:
				ccx_chip_release(hw);
				continue;

			case CC1101_STATUS_STATE_UNDERFLOW_bm:
				cc1101_strobe_flush_tx(hw);
				ccx_chip_release(hw);
				return RF_TRANSMIT_UNDERFLOW;

			case CC1101_STATUS_STATE_RX_bm:
				cc1101_strobe_transmit(hw);
				ccx_chip_release(hw);
				continue;

			default:
				ccx_chip_release(hw);
				return (status & CC1101_STATUS_FIFO_BYTES_bm) > 0 ? RF_TRANSMIT_OK : RF_TRANSMIT_FAIL;
		}
	} while (true);
}


void cc1101_init(rf_t * rf, ccx_hw_t * hw)
{
    rf->version = &cc1101_version;
	rf->part_number = &cc1101_part_number;
	rf->transmit = &cc1101_transmit;
    rf->priv = hw;
	
	cc1101_reset(hw);
	cc1101_configure(hw);
}
