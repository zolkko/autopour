#ifdef __AVR__
#include <util/delay.h>
#endif
#include "cc1101.h"


#ifdef __AVR__
#define _DELAY_US(x) _delay_us(x)
#endif


uint8_t cc1101_read(const rf_handle_t * rf, uint8_t addr, uint8_t * data)
{
    while ( !rf->ready() );
    
    uint8_t result = rf->write(addr | 0x80);
    *data = rf->write(0);
    
    return result;
}


uint8_t cc1101_burst_read(const rf_handle_t * rf, uint8_t addr, uint8_t * data, uint8_t size)
{
    while ( !rf->ready() );

    uint8_t result = rf->write(addr | 0xc0);
    for (uint8_t i = 0; i < size; i++) {
        data[i] = rf->write(0);
    }
    return result;
}


uint8_t cc1101_burst_write(const rf_handle_t * rf, uint8_t addr, uint8_t * data, uint8_t size)
{
    while ( !rf->ready() );

    uint8_t result = rf->write(addr | 0x40);
    for (uint8_t i = 0; i < size; i++) {
        rf->write(data[i]);
    }
    return result;
}


uint8_t cc1101_write(const rf_handle_t * rf, uint8_t addr, uint8_t data)
{
    while ( !rf->ready() );
    
    uint8_t result = rf->write(addr);
    rf->write(data);
    
    return result;
}


uint8_t cc1101_strobe(const rf_handle_t * rf, uint8_t addr)
{
    while ( ! rf->ready() );
    return rf->write(addr);
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
