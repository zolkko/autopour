#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include "cc1101_xmega.h"
#include "cc1101.h"


// PORTA
#define PIN_GDO2 PIN0_bm
#define PIN_GDO0 PIN1_bm


// PORTD
#define PIN_SS  PIN4_bm
#define PIN_SI  PIN5_bm
#define PIN_SO  PIN6_bm
#define PIN_SCK PIN7_bm


ISR(PORTA_INT0_vect)
{
}


ISR(PORTA_INT1_vect)
{
}


/*
 * Method initializes required ports and SPI module.
 * SPI module should be initialized after PORT initialization, otherwise
 * it would be impossible to read out PIN_SO value because SPI module
 * redefine it's value???
 */
void __impl_hw_initialize(void)
{
    // Set SCK, MOSI and SS as Output
    PORTD.DIRSET = PIN_SCK | PIN_SI | PIN_SS;
    PORTD.OUTSET = PIN_SCK | PIN_SS;
    PORTD.OUTCLR = PIN_SI;

    // Initialize pins which will handle GDO0 and GDO2
    PORTA.DIRCLR = PIN_GDO0 | PIN_GDO2;
    PORTA.INTCTRL = PORT_INT1LVL_MED_gc | PORT_INT0LVL_MED_gc;
    PORTA.INT0MASK = PIN_GDO2;
    PORTA.INT1MASK = PIN_GDO0;
    PORTA.PIN0CTRL |= PORT_ISC_RISING_gc;
    PORTA.PIN1CTRL |= PORT_ISC_RISING_gc;
    
    PMIC.CTRL |= PMIC_MEDLVLEN_bm;

    // And MISO as Input in Totem-Pole with Pull-up configuration
    // to support situations when cc1101 module is not
    // connected or is broken.

    PORTD.DIRCLR = PIN_SO;
    PORTD.PIN6CTRL = PORT_OPC_PULLUP_gc;

    // Enable SPI module
    SPID.CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc | SPI_PRESCALER_DIV4_gc;
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
    PORTD.OUTCLR = PIN_SS;
}


void __impl_hw_release(void)
{
    PORTD.OUTSET = PIN_SS;
}


bool __impl_hw_ready(void)
{
    return (PORTD.IN & PIN_SO) == 0;
}


void __impl_handle_initialize(rf_handle_t * rf)
{
    rf->select = &__impl_hw_select;
    rf->release = &__impl_hw_release;
    rf->ready = &__impl_hw_ready;
    rf->write = &__impl_hw_write;
}
