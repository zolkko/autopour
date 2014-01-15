
#include <stdint.h>
#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include "ccx_hw.h"
#include "ccx_hw_xmega.h"
#include "FreeRTOS.h"


static void __impl_chip_select(const ccx_hw_t * self);

static uint8_t __impl_write (const ccx_hw_t * self, uint8_t data);

static bool __impl_ready (const ccx_hw_t * self);

static bool __impl_gdo0(const ccx_hw_t * self);

static bool __impl_gdo2(const ccx_hw_t * self);

static void __impl_chip_release (const ccx_hw_t * self);

static inline void ccx_hw_xmega_init_spi(ccx_xmega_hw_t * conf);

static inline volatile uint8_t * port_pin_control(PORT_t * port, uint8_t pin);

#define DECL_HANDLE(X, V) ccx_xmega_hw_t * X = (ccx_xmega_hw_t *) V->priv


void __impl_chip_select(const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    handle->ss_port->OUTCLR = handle->ss_pin;
}


uint8_t __impl_write (const ccx_hw_t * self, uint8_t data)
{
    DECL_HANDLE(handle, self);
    SPI_t * spi = handle->spi;

    spi->DATA = data;
    while (true) {
        uint8_t flags = spi->STATUS;
        if (flags & SPI_IF_bm) {
            return spi->DATA;
        } else if (flags & SPI_WRCOL_bm) {
            spi->DATA = data;
        }
    }
}


void __impl_chip_release (const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    handle->ss_port->OUTSET = handle->ss_pin;
}


bool __impl_ready (const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    return (handle->in_so_port->IN & handle->in_so_pin) == 0;
}


bool __impl_gdo0(const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    return (handle->gdo0_port->IN & handle->gdo0_pin) != 0;
}


bool __impl_gdo2(const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    return (handle->gdo2_port->IN & handle->gdo2_pin) != 0;
}


void ccx_hw_xmega_init_spi(ccx_xmega_hw_t * conf)
{
    // Clock, Slave Select, Slave Input and Output
    conf->spi_port->DIRSET = conf->sck_pin | conf->si_pin;
    conf->spi_port->OUTSET = conf->sck_pin;
    conf->spi_port->OUTCLR = conf->si_pin;

    conf->ss_port->DIRSET = conf->ss_pin;
    conf->ss_port->OUTSET = conf->ss_pin;

    conf->spi_port->DIRCLR = conf->so_pin;

    // So pin duplicate
    conf->in_so_port->DIRCLR = conf->in_so_pin;

    // TODO: pull-up so pin and duplicate (input so pin)

    conf->spi->CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc; // | SPI_PRESCALER_DIV4_gc;
}


void __impl_wait_ready(const ccx_hw_t * hw)
{
	while ( !ccx_ready(hw) ) {
		portYIELD();
	}
}


/**
 * GDO0 interrupt vector
 */
ISR(PORTA_INT0_vect)
{
}


/**
 * GDO2 interrupt vector
 */
ISR(PORTA_INT1_vect)
{
}


volatile uint8_t * port_pin_control(PORT_t * port, uint8_t pin)
{
    switch (pin)
    {
        case PIN0_bm:
            return &(port->PIN0CTRL);

        case PIN1_bm:
            return &(port->PIN1CTRL);

        case PIN2_bm:
            return &(port->PIN2CTRL);
            
        case PIN3_bm:
            return &(port->PIN3CTRL);
        
        case PIN4_bm:
            return &(port->PIN4CTRL);
            
        case PIN5_bm:
            return &(port->PIN5CTRL);
            
        case PIN6_bm:
            return &(port->PIN6CTRL);
            
        default:
            return &(port->PIN7CTRL);
    }
}


ccx_hw_t * ccx_hw_xmega_init(ccx_hw_t * hw_if, ccx_xmega_hw_t * conf)
{
    hw_if->chip_select = &__impl_chip_select;
    hw_if->write = &__impl_write;
    hw_if->ready = &__impl_ready;
	hw_if->wait_ready = &__impl_wait_ready;
    hw_if->gdo0 = &__impl_gdo0;
    hw_if->gdo2 = &__impl_gdo2;
    hw_if->chip_release = &__impl_chip_release;
    hw_if->priv = conf;

    conf->gdo0_port->DIRCLR = conf->gdo0_pin;
    volatile uint8_t * gdo0_pinctrl = port_pin_control(conf->gdo0_port, conf->gdo0_pin);
    *gdo0_pinctrl = (*gdo0_pinctrl) | PORT_ISC_RISING_gc;
    conf->gdo0_port->INT0MASK = conf->gdo0_pin;
    conf->gdo0_port->INTCTRL |= PORT_INT0LVL_MED_gc;

    conf->gdo2_port->DIRCLR = conf->gdo2_pin;
    volatile uint8_t * gdo2_pinctrl = port_pin_control(conf->gdo2_port, conf->gdo2_pin);
    *gdo2_pinctrl = (*gdo2_pinctrl) | PORT_ISC_RISING_gc;
    conf->gdo2_port->INT1MASK = conf->gdo2_pin;
    conf->gdo2_port->INTCTRL |= PORT_INT1LVL_MED_gc;

    PMIC.CTRL |= PMIC_MEDLVLEN_bm;

    ccx_hw_xmega_init_spi(conf);

    return hw_if;
}
