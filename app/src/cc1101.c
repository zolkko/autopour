#ifdef __AVR__
#include <util/delay.h>
#endif
#include "cc1101.h"


#ifdef __AVR__
#define _DELAY_US(x) _delay_us(x)
#endif


extern bool __impl_cc1101_is_data(const rf_handle_t * rf);

extern void cc1101_impl_wait_chip_ready(const rf_handle_t * rf);


#define cc1101_wait_chip_ready(X) cc1101_impl_wait_chip_ready(X)


bool cc1101_is_data(const rf_handle_t * rf)
{
    return __impl_cc1101_is_data(rf);
}


/**
 * Before making actual read the code tests [3:0] status bits to
 * determine available bytes in RX buffer.
 */
uint8_t cc1101_read(const rf_handle_t * rf, uint8_t addr, uint8_t * data)
{
    rf->select();

    cc1101_wait_chip_ready(rf);

    uint8_t result = rf->write(addr | 0x80);
    
    if ((result & 0x07) != 0) {
        if (data != NULL) {
            *data = rf->write(0);
        } else {
            rf->write(0);
        }
    } else {
        if (data != NULL) {
            *data = 0;
        }
    }

    rf->release();

    return result;
}


uint8_t cc1101_burst_read(const rf_handle_t * rf, uint8_t addr, uint8_t * data, uint8_t size)
{
    rf->select();

    cc1101_wait_chip_ready(rf);

    uint8_t result = rf->write(addr | 0xc0);
    for (uint8_t i = 0; i < size; i++) {
        data[i] = rf->write(0);
    }

    rf->release();

    return result;
}


uint8_t cc1101_write(const rf_handle_t * rf, uint8_t addr, uint8_t data)
{
    rf->select();

    cc1101_wait_chip_ready(rf);

    uint8_t result = rf->write(addr);
    if ((result & 0x07) != 0) {
        rf->write(data);
    }

    rf->release();

    return result;
}


uint8_t cc1101_burst_write(const rf_handle_t * rf, uint8_t addr, uint8_t * data, uint8_t size)
{
    rf->select();

    cc1101_wait_chip_ready(rf);

    uint8_t result = rf->write(addr | 0x40);
    for (uint8_t i = 0; i < size; i++) {
        rf->write(data[i]);
    }

    rf->release();

    return result;
}


uint8_t cc1101_strobe(const rf_handle_t * rf, uint8_t addr)
{
    rf->select();

    cc1101_wait_chip_ready(rf);

    uint8_t result = rf->write(addr);

    rf->release();

    return result;
}

/**
 * Strobe chip select low/high
 * I use here smallest possible value because cc1101 supports up to 10Mhz clock
 * and about 6.5Mhz strobe for bulk write operation.
 */
void cc1101_poweron_reset(const rf_handle_t * rf)
{
    rf->select();
    _DELAY_US(1);
    rf->release();
    
    _DELAY_US(50);
    
    while ( rf->ready() );
    
    rf->select();
    while ( !rf->ready() );
    rf->write(CCx_SRES);
    while ( !rf->ready() );
    rf->release(); 
}


void cc1101_initialize_registers(const rf_handle_t * rf)
{
    cc1101_write(rf, CCx_FSCTRL1, 0x08);  // FSCTRL1   Frequency synthesizer control.
    cc1101_write(rf, CCx_FSCTRL0, 0x00);  // FSCTRL0   Frequency synthesizer control.
    
    cc1101_write(rf, CCx_FREQ2, 0x23);    // FREQ2     Frequency control word, high unsigned char.
    cc1101_write(rf, CCx_FREQ1, 0x31);    // FREQ1     Frequency control word, middle unsigned char.
    cc1101_write(rf, CCx_FREQ0, 0x3B);    // FREQ0     Frequency control word, low unsigned char.
    
    cc1101_write(rf, CCx_MDMCFG4, 0x7B);  // MDMCFG4   Modem configuration.
    cc1101_write(rf, CCx_MDMCFG3, 0x83);  // MDMCFG3   Modem configuration.
    cc1101_write(rf, CCx_MDMCFG2, 0x03);  // MDMCFG2   Modem configuration.
    cc1101_write(rf, CCx_MDMCFG1, 0x22);  // MDMCFG1   Modem configuration.
    cc1101_write(rf, CCx_MDMCFG0, 0xF8);  // MDMCFG0   Modem configuration.
    
    cc1101_write(rf, CCx_CHANNR, 0x00);   // CHANNR    Channel number.
    cc1101_write(rf, CCx_DEVIATN, 0x42);  // DEVIATN   Modem deviation setting (when FSK modulation is enabled).
    cc1101_write(rf, CCx_FREND1, 0xB6);   // FREND1    Front end RX configuration.
    cc1101_write(rf, CCx_FREND0, 0x10);   // FREND0    Front end TX configuration.
    cc1101_write(rf, CCx_MCSM0, 0x18);    // MCSM0     Main Radio Control State Machine configuration.
    cc1101_write(rf, CCx_FOCCFG, 0x1D);   // FOCCFG    Frequency Offset Compensation Configuration.
    cc1101_write(rf, CCx_BSCFG, 0x1C);    // BSCFG     Bit synchronization Configuration.
    
    cc1101_write(rf, CCx_AGCCTRL2, 0xC7); // AGCCTRL2  AGC control.
    cc1101_write(rf, CCx_AGCCTRL1, 0x00); // AGCCTRL1  AGC control.
    cc1101_write(rf, CCx_AGCCTRL0, 0xB2); // AGCCTRL0  AGC control.
    
    cc1101_write(rf, CCx_FSCAL3, 0xEA);   // FSCAL3    Frequency synthesizer calibration.
    cc1101_write(rf, CCx_FSCAL2, 0x2A);   // FSCAL2    Frequency synthesizer calibration.
    cc1101_write(rf, CCx_FSCAL1, 0x00);   // FSCAL1    Frequency synthesizer calibration.
    cc1101_write(rf, CCx_FSCAL0, 0x1F);   // FSCAL0    Frequency synthesizer calibration.
    cc1101_write(rf, CCx_FSTEST, 0x59);   // FSTEST    Frequency synthesizer calibration.
    
    cc1101_write(rf, CCx_TEST2, 0x81);    // TEST2     Various test settings.
    cc1101_write(rf, CCx_TEST1, 0x35);    // TEST1     Various test settings.
    cc1101_write(rf, CCx_TEST0, 0x09);    // TEST0     Various test settings.
    
    cc1101_write(rf, CCx_FIFOTHR, 0x0E);  // FIFOTHR   RXFIFO and TXFIFO thresholds.
    
    cc1101_write(rf, CCx_IOCFG2, 0x09);   // IOCFG2    GDO2 output pin configuration.0x09-CCA mode, 0x2E-High impedance
    
    // IOCFG0D - GDO0 output pin configuration.
    cc1101_write(rf, CCx_IOCFG0D, GDOx_CFG_RX_THR_RX_EMPTY);
    
    cc1101_write(rf, CCx_PKTCTRL1, 0x04); // PKTCTRL1  Packet automation control. bit2 = 1 append RSSI and LQI ,bit2 = 0 not append
    cc1101_write(rf, CCx_PKTCTRL0, 0x05); // PKTCTRL0  Packet automation control.
    cc1101_write(rf, CCx_ADDR, 0x00);     // ADDR      Device address.
    cc1101_write(rf, CCx_PKTLEN, CCx_PACKT_LEN);
}


/**
 * TODO: only one execution thread should be able to 
 */
void cc1101_transmit(const rf_handle_t * rf, uint8_t * data, uint8_t size, uint8_t src_addr, uint8_t dst_addr)
{
    uint8_t stat;
    uint8_t tx_buff_len;
    
    cc1101_strobe_ide(rf);
    cc1101_strobe_flush_tx(rf);
    
    stat = cc1101_read(rf, CCx_TXBYTES, &tx_buff_len);
    
    cc1101_write(rf, CCx_TXFIFO, size + 2);
    cc1101_write(rf, CCx_TXFIFO, dst_addr);
    cc1101_write(rf, CCx_TXFIFO, src_addr);
    cc1101_burst_write(rf, CCx_TXFIFO, data, size);
    
    stat = cc1101_read(rf, CCx_TXBYTES, &tx_buff_len);
    
    cc1101_strobe_transmit(rf);
 
    while (1) {
        stat = cc1101_read(rf, CCx_TXBYTES, &tx_buff_len);
        // test transmit buffer length ignoring underflow flag
        if (0 == (tx_buff_len & 0x7f)) {
            break;
        } else {
            cc1101_strobe_transmit(rf);
        }
    }
}


uint8_t cc1101_rssi_decode(uint8_t rssi_enc) {
    unsigned char rssi;
    
    // is actually dataRate dependant, but for simplicity assumed to be fixed.
    unsigned char rssi_offset = 74;

    // RSSI is coded as 2's complement see section 17.3 RSSI of the cc1100 datasheet
    if (rssi_enc >= 128)
        rssi = (( rssi_enc - 256) >> 1) - rssi_offset;
    else
        rssi = (rssi_enc >> 1) - rssi_offset;
        
    return rssi;
}


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
