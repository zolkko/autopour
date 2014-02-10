
#include <stdint.h>
#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>


#include <FreeRTOS.h>
#include <semphr.h>


#include "ccx_hw.h"
#include "ccx_hw_xmega.h"


static void ccx_xmega_chip_select(const ccx_hw_t * self);

static void ccx_xmega_chip_release(const ccx_hw_t * self);


static uint8_t ccx_xmega_write(const ccx_hw_t * self, uint8_t data);

static bool ccx_xmega_ready(const ccx_hw_t * self);


static bool ccx_xmega_gdo0(const ccx_hw_t * self);

static void ccx_xmega_enable_gdo0(const ccx_hw_t * self);

static void ccx_xmega_disable_gdo0(const ccx_hw_t * self);

static bool ccx_xmega_wait_gdo0(const ccx_hw_t * self, portTickType timeout);


static bool ccx_xmega_gdo1(const ccx_hw_t * self);

static void ccx_xmega_enable_gdo1(const ccx_hw_t * self);

static void ccx_xmega_disable_gdo1(const ccx_hw_t * self);

static bool ccx_xmega_wait_gdo1(const ccx_hw_t * self, portTickType timeout);


static bool ccx_xmega_gdo2(const ccx_hw_t * self);

static void ccx_xmega_enable_gdo2(const ccx_hw_t * self);

static void ccx_xmega_disable_gdo2(const ccx_hw_t * self);

static bool ccx_xmega_wait_gdo2(const ccx_hw_t * self, portTickType timeout);


static inline void ccx_hw_xmega_init_spi(ccx_xmega_hw_t * conf);

static inline volatile uint8_t * port_pin_control(PORT_t * port, uint8_t pin);


#define DECL_HANDLE(X, V)   ccx_xmega_hw_t * X = ((ccx_hw_xmega_priv_t *) V->priv)->conf
#define DECL_PRIV_HW(X, V ) ccx_xmega_hw_t * X = ((ccx_hw_xmega_priv_t *) V->priv)->conf

#define RF_PMIC_LEVEL_bm PMIC_MEDLVLEN_bm

#define GDO0_INT_LVL_gc PORT_INT0LVL_MED_gc

#define GDO2_INT_LVL_gc PORT_INT1LVL_MED_gc


typedef struct
{
	ccx_xmega_hw_t * conf;
} ccx_hw_xmega_priv_t;


typedef struct
{
    ccx_isr_proc_t handler;
    void * data;
} ccx_isr_handler_t;


static xSemaphoreHandle gdo0_semaphore = NULL;

static ccx_isr_handler_t gdo0_handler = {NULL, NULL};

static xSemaphoreHandle gdo2_semaphore = NULL;

static ccx_isr_handler_t gdo2_handler = {NULL, NULL};


/**
 * GDO0 interrupt vector
 */
ISR(PORTA_INT0_vect)
{
    static portBASE_TYPE task_woken;
    if (gdo0_semaphore != NULL) {
        if (gdo0_handler.handler != NULL) {
            gdo0_handler.handler(gdo0_handler.data);
        }
        task_woken = pdFALSE;
        xSemaphoreGiveFromISR(gdo0_semaphore, &task_woken);
        if (task_woken) {
            portYIELD();
        }            
    }
}


/**
 * GDO2 interrupt vector
 */
ISR(PORTA_INT1_vect)
{
    static portBASE_TYPE task_woken;
    if (gdo2_semaphore != NULL) {
        if (gdo2_handler.handler != NULL) {
            gdo2_handler.handler(gdo2_handler.data);
        }
        task_woken = pdFALSE;
        xSemaphoreGiveFromISR(gdo2_semaphore, &task_woken);
        if (task_woken) {
            portYIELD();
        }
    }
}


void ccx_xmega_chip_select(const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    handle->ss_port->OUTCLR = handle->ss_pin;
}


uint8_t ccx_xmega_write(const ccx_hw_t * self, uint8_t data)
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


void ccx_xmega_chip_release(const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    handle->ss_port->OUTSET = handle->ss_pin;
}


bool ccx_xmega_ready(const ccx_hw_t * self)
{
    DECL_HANDLE(handle, self);
    return (handle->in_so_port->IN & handle->in_so_pin) == 0;
}


bool ccx_xmega_gdo0(const ccx_hw_t * self)
{
    DECL_PRIV_HW(handle, self);
    return (handle->gdo0_port->IN & handle->gdo0_pin) != 0;
}


bool ccx_xmega_gdo1(const ccx_hw_t * self)
{
	DECL_HANDLE(handle, self);
	return (handle->in_so_port->IN & handle->in_so_pin) != 0;
}


bool ccx_xmega_gdo2(const ccx_hw_t * self)
{
    DECL_PRIV_HW(handle, self);
    return (handle->gdo2_port->IN & handle->gdo2_pin) != 0;
}

/** clear interrupt flag if any and enable it */
void ccx_xmega_enable_gdo0(const ccx_hw_t * self)
{
	DECL_PRIV_HW(priv, self);
	priv->gdo0_port->INTFLAGS &= ~(PORT_INT0IF_bm);
	priv->gdo0_port->INT0MASK |= priv->gdo0_pin;
}


void ccx_xmega_enable_gdo1(const ccx_hw_t * self)
{
}


void ccx_xmega_enable_gdo2(const ccx_hw_t * self)
{
	DECL_PRIV_HW(priv, self);
	priv->gdo2_port->INTFLAGS &= ~(PORT_INT1IF_bm);
	priv->gdo2_port->INT1MASK |= priv->gdo2_pin;
}


void ccx_xmega_disable_gdo0(const ccx_hw_t * self)
{
	DECL_PRIV_HW(priv, self);
    priv->gdo0_port->INT0MASK &= ~(priv->gdo0_pin);
}

void ccx_xmega_disable_gdo1(const ccx_hw_t * self)
{
}

void ccx_xmega_disable_gdo2(const ccx_hw_t * self)
{
	DECL_PRIV_HW(priv, self);
    priv->gdo2_port->INT1MASK &= ~(priv->gdo2_pin);
}


bool ccx_xmega_wait_gdo0(const ccx_hw_t * self, portTickType timeout)
{
	return xSemaphoreTake(gdo0_semaphore, timeout);
}


bool ccx_xmega_wait_gdo1(const ccx_hw_t * self, portTickType timeout)
{
	return false;
}


bool ccx_xmega_wait_gdo2(const ccx_hw_t * self, portTickType timeout)
{
	return xSemaphoreTake(gdo2_semaphore, timeout);
}


void ccx_xmega_set_handler_gdo0(const ccx_hw_t * hw, ccx_isr_proc_t handler, void * data)
{
    gdo0_handler.handler = handler;
    gdo0_handler.data = data;
}


void ccx_xmega_set_handler_gdo1(const ccx_hw_t * hw, ccx_isr_proc_t handler, void * data)
{
}


void ccx_xmega_set_handler_gdo2(const ccx_hw_t * hw, ccx_isr_proc_t handler, void * data)
{
    gdo2_handler.handler = handler;
    gdo2_handler.data = data;
}


void ccx_xmega_clear_handler_gdo0(const ccx_hw_t * hw)
{
    gdo0_handler.handler = NULL;
    gdo0_handler.data = NULL;
}


void ccx_xmega_clear_handler_gdo1(const ccx_hw_t * hw)
{
}


void ccx_xmega_clear_handler_gdo2(const ccx_hw_t * hw)
{
    gdo2_handler.handler = NULL;
    gdo2_handler.data = NULL;
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

    conf->spi->CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc | SPI_CLK2X_bm; // | SPI_PRESCALER_DIV4_gc;
}


void ccx_xmega_wait_ready(const ccx_hw_t * hw)
{
	while ( !ccx_ready(hw) ) {
		portYIELD();
	}
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


/**
 * GDOx interrupts should be disabled by default
 */
ccx_hw_t * ccx_hw_xmega_init(ccx_hw_t * hw_if, ccx_xmega_hw_t * conf)
{
	if (gdo0_semaphore == NULL) {
		gdo0_semaphore = xSemaphoreCreateBinary();
	}

	if (gdo2_semaphore == NULL) {
		gdo2_semaphore = xSemaphoreCreateBinary();
	}

    hw_if->chip_select = &ccx_xmega_chip_select;
    hw_if->chip_release = &ccx_xmega_chip_release;

    hw_if->write = &ccx_xmega_write;
    hw_if->ready = &ccx_xmega_ready;
	hw_if->wait_ready = &ccx_xmega_wait_ready;

    hw_if->gdo0 = &ccx_xmega_gdo0;
	hw_if->gdo1 = &ccx_xmega_gdo1;
    hw_if->gdo2 = &ccx_xmega_gdo2;

	hw_if->wait_gdo0 = &ccx_xmega_wait_gdo0;
	hw_if->wait_gdo1 = &ccx_xmega_wait_gdo1;
	hw_if->wait_gdo2 = &ccx_xmega_wait_gdo2;

	hw_if->enable_gdo0 = &ccx_xmega_enable_gdo0;
	hw_if->enable_gdo1 = &ccx_xmega_enable_gdo1;
	hw_if->enable_gdo2 = &ccx_xmega_enable_gdo2;

	hw_if->disable_gdo0 = &ccx_xmega_disable_gdo0;
	hw_if->disable_gdo1 = &ccx_xmega_disable_gdo1;
	hw_if->disable_gdo2 = &ccx_xmega_disable_gdo2;
    
    hw_if->set_handler_gdo0 = &ccx_xmega_set_handler_gdo0;
    hw_if->set_handler_gdo1 = &ccx_xmega_set_handler_gdo1;
    hw_if->set_handler_gdo2 = &ccx_xmega_set_handler_gdo2;
    
    hw_if->clear_handler_gdo0 = &ccx_xmega_clear_handler_gdo0;
    hw_if->clear_handler_gdo1 = &ccx_xmega_clear_handler_gdo1;
    hw_if->clear_handler_gdo2 = &ccx_xmega_clear_handler_gdo2;

	ccx_hw_xmega_priv_t * priv = pvPortMalloc(sizeof(ccx_hw_xmega_priv_t));
	priv->conf = conf;

    hw_if->priv = priv;

    conf->gdo0_port->DIRCLR = conf->gdo0_pin;
    volatile uint8_t * gdo0_pinctrl = port_pin_control(conf->gdo0_port, conf->gdo0_pin);
    *gdo0_pinctrl = (*gdo0_pinctrl) | PORT_ISC_RISING_gc;
    conf->gdo0_port->INT0MASK &= ~(conf->gdo0_pin);
    conf->gdo0_port->INTCTRL |= GDO0_INT_LVL_gc;

    conf->gdo2_port->DIRCLR = conf->gdo2_pin;
    volatile uint8_t * gdo2_pinctrl = port_pin_control(conf->gdo2_port, conf->gdo2_pin);
    *gdo2_pinctrl = (*gdo2_pinctrl) | PORT_ISC_RISING_gc;
    conf->gdo2_port->INT1MASK &= ~(conf->gdo2_pin);
    conf->gdo2_port->INTCTRL |= GDO2_INT_LVL_gc;

    PMIC.CTRL |= RF_PMIC_LEVEL_bm;

    ccx_hw_xmega_init_spi(conf);

    return hw_if;
}
