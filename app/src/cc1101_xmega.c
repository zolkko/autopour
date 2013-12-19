
#include <stdbool.h>
#include <avr/io.h>
#include "cc1101_xmega.h"
#include "cc1101.h"


void __impl_hw_initialize(void)
{
    // Set Clock, MOSI and SS signals as output.
    // SS and Clock should be set high and MOSI low to avoid potential problems with pin control mode.
    SPID.CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc; // | SPI_PRESCALER_DIV4_gc;

    // Initialize SPI module
    // PD7 - SCK
    // PD6 - MISO
    // PD5 - MOSI
    // PD4 - Slave Select
    //
    // The prescaler should be 8 because 2000000 / 250000 = 8
    // SPI_DORD_bm - should be cleared because logic sends MSB first
    // In brust mode it is possible to run SPI interface up to 6.5Mhz

    // Set SCK, MOSI and SS as Output
    PORTD.DIRSET = PIN7_bm | PIN5_bm | PIN4_bm;
    PORTD.OUTSET = PIN7_bm | PIN4_bm;
    PORTD.OUTCLR = PIN5_bm;
     
    // And MISO as Input in Totem-Pole with Pull-up configuration
    // to support situations when cc1101 module is not
    // connected or is broken.
     
    PORTD.DIRCLR = PIN6_bm;
    PORTD.PIN6CTRL = PORT_OPC_PULLUP_gc;   
}


uint8_t __impl_hw_spi_write(uint8_t data);


void __impl_chip_select(void);


void __impl_chip_release(void);


bool __impl_chip_ready(void);


uint8_t __impl_hw_write(uint8_t data)
{
    SPID.DATA = data;
    while (true) {
        uint8_t flags = SPID.STATUS;
        if ((flags & SPI_IF_bm) != 0) {
            return SPID.DATA;
        } else if ((flags & SPI_WRCOL_bm) != 0) {
            SPID.DATA = data;
        }
    }
}


void __impl_hw_select(void)
{
    PORTD.OUTCLR = PIN4_bm;
}


void __impl_hw_release(void)
{
    PORTD.OUTSET = PIN4_bm;
}


bool __impl_hw_ready(void)
{
    return (PORTD.IN & PIN6_bm) == 0;
}


void __impl_handle_initialize(rf_handle_t * rf)
{
    rf->select = &__impl_hw_select;
    rf->release = &__impl_hw_release;
    rf->ready = &__impl_hw_ready;
    rf->write = &__impl_hw_write;
}

