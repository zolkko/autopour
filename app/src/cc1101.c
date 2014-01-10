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

static inline void cc1101_wait_transmission_allowed(const ccx_hw_t * self);

static inline uint8_t cc1101_wait_transmission_finished(const ccx_hw_t * self);

static int8_t cc1101_receive(const rf_t * self, uint8_t * data, uint8_t * data_len, uint8_t * src_addr, uint8_t * dst_addr);

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
	
	cc1101_write(hw, CCx_IOCFG2, 0x09);                      // IOCFG2    GDO2 output pin configuration.0x09-CCA mode, 0x2E-High impedance
	// TODO: IOGDO1
	cc1101_write(hw, CCx_IOCFG0D, GDOx_CFG_RX_THR_RX_EMPTY); // IOCFG0D - GDO0 output pin configuration.
    cc1101_write(hw, CCx_FIFOTHR, 0x0E);                     // FIFOTHR   RXFIFO and TXFIFO thresholds.
	// TODO: CCx_SYNC1
	// TODO: CCX_SYNC0
	cc1101_write(hw, CCx_PKTLEN, CCx_PACKT_LEN);
	
    cc1101_write(hw, CCx_PKTCTRL1, 0x04); // PKTCTRL1  Packet automation control. bit2 = 1 append RSSI and LQI ,bit2 = 0 not append
    cc1101_write(hw, CCx_PKTCTRL0, 0x05); // PKTCTRL0  Packet automation control.
    cc1101_write(hw, CCx_ADDR, 0x00);     // ADDR      Device address.
    cc1101_write(hw, CCx_CHANNR, 0x00);   // CHANNR    Channel number.
	

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
	
    cc1101_write(hw, CCx_DEVIATN, 0x42);  // DEVIATN   Modem deviation setting (when FSK modulation is enabled).
	
	// TODO: CCx_MCSM2
	// TODO: CCx_MCSM1
	cc1101_write(hw, CCx_MCSM0, 0x18);    // MCSM0     Main Radio Control State Machine configuration.

	cc1101_write(hw, CCx_FOCCFG, 0x1D);   // FOCCFG    Frequency Offset Compensation Configuration.
    cc1101_write(hw, CCx_BSCFG, 0x1C);    // BSCFG     Bit synchronization Configuration.

    cc1101_write(hw, CCx_AGCCTRL2, 0xC7); // AGCCTRL2  AGC control.
    cc1101_write(hw, CCx_AGCCTRL1, 0x00); // AGCCTRL1  AGC control.
    cc1101_write(hw, CCx_AGCCTRL0, 0xB2); // AGCCTRL0  AGC control.

	// TODO: CCx_WOREVT1
	// TODO: CCx_WOREVT0
	// TODO: CCx_WORCTRL
	
    cc1101_write(hw, CCx_FREND1, 0xB6);   // FREND1    Front end RX configuration.
    cc1101_write(hw, CCx_FREND0, 0x10);   // FREND0    Front end TX configuration.

    cc1101_write(hw, CCx_FSCAL3, 0xEA);   // FSCAL3    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSCAL2, 0x2A);   // FSCAL2    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSCAL1, 0x00);   // FSCAL1    Frequency synthesizer calibration.
    cc1101_write(hw, CCx_FSCAL0, 0x1F);   // FSCAL0    Frequency synthesizer calibration.
	
	// TODO: CCx_RCCTRL1
	// TODO: CCx_RCCTRL0
	
    cc1101_write(hw, CCx_FSTEST, 0x59);   // FSTEST    Frequency synthesizer calibration.
	
	// TODO: CCx_PTEST
	// TODO: CCx_AGCTEST
    
    cc1101_write(hw, CCx_TEST2, 0x81);    // TEST2     Various test settings.
    cc1101_write(hw, CCx_TEST1, 0x35);    // TEST1     Various test settings.
    cc1101_write(hw, CCx_TEST0, 0x09);    // TEST0     Various test settings.

	ccx_chip_release(hw);
}


uint8_t cc1101_version(const rf_t * self)
{
    DECL_HW(hw, self);

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	uint8_t version = 0;
	cc1101_read(hw, CCx_VERSION, &version);

	ccx_chip_release(hw);

	return version;
}


uint8_t cc1101_part_number(const rf_t * self)
{
	DECL_HW(hw, self);

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

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

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	cc1101_write(hw, CCx_TXFIFO, data_size + 2);
	cc1101_write(hw, CCx_TXFIFO, dst_addr);
	cc1101_write(hw, CCx_TXFIFO, src_addr);
	cc1101_burst_write(hw, CCx_TXFIFO, data, data_size);
	ccx_chip_release(hw);

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	cc1101_wait_transmission_allowed(hw);

	cc1101_strobe_transmit(hw);

	uint8_t result = cc1101_wait_transmission_finished(hw);

	ccx_chip_release(hw);

	return result;
}


/**
 * As described in Errata http://www.ti.com/lit/er/swrz020d/swrz020d.pdf during transmission
 * it is important to read status byte twice and only once both statuses are equals it value
 * is considered valid.
 */
void cc1101_wait_transmission_allowed(const ccx_hw_t * self)
{
	uint8_t state1;
	uint8_t state2;

	do {
		cc1101_read(self, CCx_MARCSTATE, &state1);
		state1 &= CC1101_MARC_bm;

		cc1101_read(self, CCx_MARCSTATE, &state2);
		state2 &= CC1101_MARC_bm;

	} while (state1 != state2 &&
             CC1101_MARC_IDLE_gc != state1 &&
			 CC1101_MARC_RX_gc != state1 &&
			 CC1101_MARC_RX_END_gc != state1 &&
			 CC1101_MARC_RX_RST_gc != state1);
}


/**
 * Method assumes that TXOFF moved state-machine into IDLE state after the
 * transfer operation complete.
 */
uint8_t cc1101_wait_transmission_finished(const ccx_hw_t * self)
{
	uint8_t status1 = 0;
	uint8_t status2 = 0;

	do {
		cc1101_read(self, CCx_MARCSTATE, &status1);
		status1 &= CC1101_MARC_bm;

		cc1101_read(self, CCx_MARCSTATE, &status2);
		status2 &= CC1101_MARC_bm;

		if (status1 != status2) {
			continue;
		}

		switch (status1 & CC1101_MARC_bm) {
			case CC1101_MARC_TXFIFO_UNDERFLOW_gc:
				cc1101_strobe_flush_tx(self);
				return RF_TRANSMIT_UNDERFLOW;

			case CC1101_MARC_IDLE_gc:
				return RF_TRANSMIT_OK;

			case CC1101_MARC_RX_END_gc:
			case CC1101_MARC_RX_RST_gc:
			case CC1101_MARC_RX_gc:
				// TX-if-CCA
				cc1101_strobe_transmit(self);
				continue;

			default:
				// the rest of states are considered as transitional
				continue;
		}
	} while (true);
}


// TODO: In order to make the interface more generic I need to remove RSSI and LQI parameters
// receive data via RF, rxData must be at least CCx_PACKT_LEN bytes long
int8_t cc1101_receive(const rf_t * self, uint8_t * data, uint8_t * data_len, uint8_t * src_addr, uint8_t * dst_addr)
{
	DECL_HW(hw, self);
	uint8_t stat;

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	cc1101_read(hw, CCx_RXFIFO, data_len);
	if (*data_len == 0)
	{
		return RF_RECEIVE_FAIL;
	}

	cc1101_read(hw, CCx_RXFIFO, dst_addr);
	cc1101_read(hw, CCx_RXFIFO, src_addr);
	*data_len -= 2;

	cc1101_burst_read(hw, CCx_RXFIFO, data, *data_len);
	ccx_chip_release(hw);

	// TODO: wait for reception finished

	return RF_RECEIVE_OK;
}


void cc1101_init(rf_t * rf, ccx_hw_t * hw)
{
    rf->version = &cc1101_version;
	rf->part_number = &cc1101_part_number;
	rf->transmit = &cc1101_transmit;
	rf->receive = &cc1101_receive;
	rf->can_receive = NULL;
    rf->priv = hw;

	cc1101_reset(hw);
	cc1101_configure(hw);
}
