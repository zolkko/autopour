
include "cc1101.h"


void cc1101_poweron_reset(const rf_handle_t * rf)
{
    // Strobe chip select low/high
    // I use here smallest possible value because cc1101 supports up to 10Mhz clock
    // and about 6.5Mhz strobe for bulk write operation.
    rf->select();
    _delay_us(1);
    rf->release();
    
    _delay_us(50);
    while ((PORTD.IN & PIN6_bm) != PIN6_bm);
    
    // Wait for CHIP_RDYn
    rf->select();
    while ( ! rf->ready() );
    
    // Send SRES strobe
    uint8_t input_data = rf->write(0x30);
    
    // Wait for SO goes low
    while ( !rf->ready() );
    
    // Setup GDO0 function
    uint8_t data_hi = rf->write(0x80 | 0x02);
    uint8_t data_lo = rf->write(0xff);
    
    rf->release();
}
