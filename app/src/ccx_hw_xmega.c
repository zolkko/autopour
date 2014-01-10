
#include <stdint.h>
#include <stdbool.h>
#include <avr/io.h>
#include "ccx_hw.h"
#include "ccx_hw_xmega.h"
#include "FreeRTOS.h"


static void __impl_chip_select(const ccx_hw_t * self);

static uint8_t __impl_write (const ccx_hw_t * self, uint8_t data);

static bool __impl_ready (const ccx_hw_t * self);

static bool __impl_gdo0(const ccx_hw_t * self);

static bool __impl_gdo2(const ccx_hw_t * self);

static void __impl_chip_release (const ccx_hw_t * self);

static inline void ccx_hw_xmega_init_slave_output(PORT_t * port, uint8_t pin);

static inline void ccx_hw_xmega_init_spi(SPI_t * spi);

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
    return (handle->so_port->IN & handle->so_pin) == 0;
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


void ccx_hw_xmega_init_slave_output(PORT_t * port, uint8_t pin)
{
    port->DIRCLR = pin;
    switch (pin) {
        case PIN0_bm:
            port->PIN0CTRL = PORT_OPC_PULLUP_gc;
            break;
        case PIN1_bm:
            port->PIN1CTRL = PORT_OPC_PULLUP_gc;
            break;
        case PIN2_bm:
            port->PIN2CTRL = PORT_OPC_PULLUP_gc;
            break;
        case PIN3_bm:
            port->PIN3CTRL = PORT_OPC_PULLUP_gc;
            break;
        case PIN4_bm:
            port->PIN4CTRL = PORT_OPC_PULLUP_gc;
            break;
        case PIN5_bm:
            port->PIN5CTRL = PORT_OPC_PULLUP_gc;
            break;
        case PIN6_bm:
            port->PIN6CTRL = PORT_OPC_PULLUP_gc;
            break;
        default:
            port->PIN7CTRL = PORT_OPC_PULLUP_gc;
            break;
    }
}


void ccx_hw_xmega_init_spi(SPI_t * spi)
{
    spi->CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc; // | SPI_PRESCALER_DIV4_gc;
}


void __impl_wait_ready(const ccx_hw_t * hw)
{
	while ( !ccx_ready(hw) ) {
		portYIELD();
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
    conf->gdo2_port->DIRCLR = conf->gdo2_pin;

    ccx_hw_xmega_init_slave_output(conf->so_port, conf->so_pin);
    ccx_hw_xmega_init_spi(conf->spi);

    return hw_if;
}
