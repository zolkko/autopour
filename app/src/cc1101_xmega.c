#include "config.h"
#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include "cc1101_xmega.h"
#include "cc1101.h"
#include "FreeRTOS.h"
#include "task.h"
#include "semphr.h"

// TODO: move all related data into handle's private field

// PORTA
#define PIN_GDO2 PIN0_bm
#define PIN_GDO0 PIN1_bm
#define PIN_SO_CLONE PIN2_bm

// PORTD
#define PIN_SS  PIN4_bm
#define PIN_SI  PIN5_bm
#define PIN_SO  PIN6_bm
#define PIN_SCK PIN7_bm


// SPI module
#define CC1101_SPI SPID


static xSemaphoreHandle __has_data = NULL;


ISR(PORTA_INT0_vect)
{
}


ISR(PORTA_INT1_vect)
{
    xSemaphoreGiveFromISR(__has_data, NULL);
}


bool __impl_cc1101_is_data(const rf_handle_t * rf)
{
    return (PORTA.IN & PIN_GDO0) != PIN_GDO0;
}


bool __impl_cc1101_has_data(portTickType blockTime) {
    return xSemaphoreTake(__has_data, blockTime) == pdTRUE;
}

bool __impl_cc1101_has_data_release()
{
    return xSemaphoreGive(__has_data);
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
    // SO_CLONE is used because I think that PIN_SO cannnot be read out when it is
    // configured in SPI mode, but I am not sure.
    PORTA.DIRCLR = PIN_GDO0 | PIN_GDO2 | PIN_SO_CLONE;
    //PORTA.INTCTRL = PORT_INT1LVL_MED_gc | PORT_INT0LVL_MED_gc;
    //PORTA.INT0MASK = PIN_GDO2;
    //PORTA.INT1MASK = PIN_GDO0;
    //PORTA.PIN0CTRL |= PORT_ISC_RISING_gc;
    //PORTA.PIN1CTRL |= PORT_ISC_RISING_gc;
    
    PMIC.CTRL |= PMIC_MEDLVLEN_bm;

    // And MISO as Input in Totem-Pole with Pull-up configuration
    // to support situations when cc1101 module is not
    // connected or is broken.

    // PORTD.DIRCLR = PIN_SO;
    // PORTD.PIN6CTRL = PORT_OPC_PULLUP_gc;

    // Enable SPI module
    CC1101_SPI.CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc; // | SPI_PRESCALER_DIV4_gc;
}


uint8_t __impl_hw_spi_write(uint8_t data);


void __impl_chip_select(void);


void __impl_chip_release(void);


bool __impl_chip_ready(void);


/**
 * This implementation allows context switch while execution code
 * waits for operation complete
 */
void cc1101_impl_wait_chip_ready(const rf_handle_t * rf)
{
    while ( !rf->ready() ) {
        // taskYIELD();
    }
}


uint8_t __impl_hw_write(uint8_t data)
{
    CC1101_SPI.DATA = data;
    
    while (true) {
        uint8_t flags = CC1101_SPI.STATUS;
        if (flags & SPI_IF_bm) {
            uint8_t result = CC1101_SPI.DATA;
            return result;
        } else if (flags & SPI_WRCOL_bm) {
            CC1101_SPI.DATA = data;
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
    // return (PORTD.IN & PIN_SO) == 0;
    return (PORTA.IN & PIN_SO_CLONE) == 0;
}


void __impl_handle_initialize(rf_handle_t * rf)
{
    rf->select = &__impl_hw_select;
    rf->release = &__impl_hw_release;
    rf->ready = &__impl_hw_ready;
    rf->write = &__impl_hw_write;

    __has_data = xSemaphoreCreateBinary();
    if ((PORTA.OUT & PIN_GDO0) == PIN_GDO0) {
        xSemaphoreGive(__has_data);
    }
}
