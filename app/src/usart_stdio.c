#include "config.h"
#include <stdbool.h>
#include <stdio.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>
#include <semphr.h>
#include "usart_stdio.h"


static FILE _uart_stdout;

#define USART_BUFFER_SIZE 32
#define USART_FLUSH_PERIOD 10
#define USART_FLUSH_TASK_PRIORITY 2

static char * input_buffer;
static uint8_t input_buffer_len;
xSemaphoreHandle input_buffer_lock;

static char * dma_buffer;
static uint8_t dma_buffer_len;
xSemaphoreHandle dma_buffer_lock;

static char buffer_a[USART_BUFFER_SIZE];
static char buffer_b[USART_BUFFER_SIZE];


static void usart_print_buffer(void);
static int usart_putchar(char c, FILE * stream);
static void usart_print_buffer_task(void * params);
static bool usart_set_baudrate(USART_t *usart, uint32_t baud, uint32_t cpu_hz);


/**
 * Function swaps input and DMA buffers and trigger DMA transfer.
 */
void usart_print_buffer(void)
{
    char * tmp_buff_ptr = dma_buffer;

    dma_buffer = input_buffer;
    dma_buffer_len = input_buffer_len;

    input_buffer = tmp_buff_ptr;
    input_buffer_len = 0;

    DMA.CH0.SRCADDR0 = ((uint16_t) dma_buffer) & 0xff;
    DMA.CH0.SRCADDR1 = (((uint16_t) dma_buffer) >> 8) & 0xff;
    DMA.CH0.SRCADDR2 = 0;

    DMA.CH0.TRFCNT = dma_buffer_len;
    DMA.CH0.CTRLA |= DMA_CH_ENABLE_bm;    
}


/*
 * Prints character 'c' into UART module
 */
int usart_putchar(char c, FILE * stream)
{
    if (stream == stdout) {
        if (xSemaphoreTake(input_buffer_lock, portMAX_DELAY)) {
            if (input_buffer_len < USART_BUFFER_SIZE) {
                input_buffer[input_buffer_len++] = c;
                xSemaphoreGive(input_buffer_lock);
                return 0;
            } else {
                /*
                 * When input buffer overruns it is important to check scheduler state. Because if acquire a
                 * mutex when scheduler is not running no one will be able to restore context or even
                 * count passed ticks.
                 */
                portTickType ticks = xTaskGetSchedulerState() == taskSCHEDULER_RUNNING ? portMAX_DELAY : 0;
                if (xSemaphoreTake(dma_buffer_lock, ticks)) {
                    usart_print_buffer();
                    input_buffer[input_buffer_len++] = c;
                    // Trigger DMA transfer if input buffer overrun
                    xSemaphoreGive(input_buffer_lock);
                    return 0;
                } else {
                    xSemaphoreGive(input_buffer_lock);
                }
            }
        }
    }
    return 1;
}


/**
 * The task gets executed every USART_FLUSH_PERIOD and
 * prints buffer using DMA to USART transfer.
 */
void usart_print_buffer_task(void * params)
{
    do {
        if (xSemaphoreTake(input_buffer_lock, portMAX_DELAY)) {
            if (input_buffer_len != 0) {
                if (xSemaphoreTake(dma_buffer_lock, portMAX_DELAY)) {
                    usart_print_buffer();
                    // DMA buffer lock will be released in ISR
                }
            }
            xSemaphoreGive(input_buffer_lock);
        }
        
        vTaskDelay(USART_FLUSH_PERIOD);
        
    } while (true);
}

/**
 * Interrupt handle should set transmission complete flag
 */
ISR (DMA_CH0_vect)
{
    if (DMA.INTFLAGS & DMA_CH0TRNIF_bm) {
        DMA.INTFLAGS |= DMA_CH0TRNIF_bm;
    } else {
        DMA.INTFLAGS |= DMA_CH0ERRIF_bm;
    }
    xSemaphoreGiveFromISR(dma_buffer_lock, NULL);
}


bool usart_set_baudrate(USART_t *usart, uint32_t baud, uint32_t cpu_hz)
{
	int8_t exp;
	uint32_t div;
	uint32_t limit;
	uint32_t ratio;
	uint32_t min_rate;
	uint32_t max_rate;

	/*
	 * Check if the hardware supports the given baud rate
	 */
	// 8 = (2^0) * 8 * (2^0) = (2^BSCALE_MIN) * 8 * (BSEL_MIN)
	max_rate = cpu_hz / 8;
	// 4194304 = (2^7) * 8 * (2^12) = (2^BSCALE_MAX) * 8 * (BSEL_MAX+1)
	min_rate = cpu_hz / 4194304;

	if (!((usart)->CTRLB & USART_CLK2X_bm)) {
		max_rate /= 2;
		min_rate /= 2;
	}

	if ((baud > max_rate) || (baud < min_rate)) {
		return false;
	}

	/*
	 * Check if double speed is enabled.
	 */
	if (!((usart)->CTRLB & USART_CLK2X_bm)) {
		baud *= 2;
	}

	/*
	 * Find the lowest possible exponent.
	 */
	limit = 0xfffU >> 4;
	ratio = cpu_hz / baud;

	for (exp = -7; exp < 7; exp++) {
		if (ratio < limit) {
			break;
		}

		limit <<= 1;

		if (exp < -3) {
			limit |= 1;
		}
	}

	/*
	 * Depending on the value of exp, scale either the input frequency or
	 * the target baud rate. By always scaling upwards, we never introduce
	 * any additional inaccuracy.
	 *
	 * We are including the final divide-by-8 (aka. right-shift-by-3) in this
	 * operation as it ensures that we never exceeed 2**32 at any point.
	 *
	 * The formula for calculating BSEL is slightly different when exp is
	 * negative than it is when exp is positive.
	 */
	if (exp < 0) {
		/*
		 * We are supposed to subtract 1, then apply BSCALE. We want to apply
		 * BSCALE first, so we need to turn everything inside the parenthesis
		 * into a single fractional expression.
		 */
		cpu_hz -= 8 * baud;
		/*
		 * If we end up with a left-shift after taking the final divide-by-8
		 * into account, do the shift before the divide. Otherwise, left-shift
		 * the denominator instead (effectively resulting in an overall right
		 * shift.)
		 */
		if (exp <= -3) {
			div = ((cpu_hz << (-exp - 3)) + baud / 2) / baud;
		} else {
			baud <<= exp + 3;
			div = (cpu_hz + baud / 2) / baud;
		}
	} else {
		/*
		 * We will always do a right shift in this case, but we need to shift
		 * three extra positions because of the divide-by-8.
		 */
		baud <<= exp + 3;
		div = (cpu_hz + baud / 2) / baud - 1;
	}

	(usart)->BAUDCTRLB = (uint8_t)(((div >> 8) & 0X0F) | (exp << 4));
	(usart)->BAUDCTRLA = (uint8_t)div;

	return true;
}


void usart_init()
{
    // Initialize input and DMA buffers
    input_buffer = buffer_a;
    input_buffer_len = 0;
    input_buffer_lock = xSemaphoreCreateMutex();
    xSemaphoreGive(input_buffer_lock);
 
    dma_buffer = buffer_b;
    dma_buffer_len = 0;
    dma_buffer_lock = xSemaphoreCreateMutex();
    xSemaphoreGive(dma_buffer_lock);
    
    // Initialize DMAC
    DMA.CTRL = DMA_ENABLE_bm | DMA_PRIMODE_RR0123_gc;
    
    DMA.CH0.CTRLA = DMA_CH_SINGLE_bm | DMA_CH_BURSTLEN_1BYTE_gc;
    DMA.CH0.CTRLB = DMA_CH_TRNINTLVL_LO_gc | DMA_CH_ERRINTLVL_LO_gc;
    DMA.CH0.ADDRCTRL = DMA_CH_SRCDIR_INC_gc | DMA_CH_DESTDIR_FIXED_gc | DMA_CH_SRCRELOAD_TRANSACTION_gc;
    
    USART_t * selected_usart = &USART_SERIAL;
    
    DMA.CH0.TRIGSRC = DMA_CH_TRIGSRC_USARTC0_DRE_gc;
    DMA.CH0.REPCNT = 1;

    DMA.CH0.DESTADDR0 = ((uint16_t)&USART_SERIAL.DATA) & 0xff;
    DMA.CH0.DESTADDR1 = (((uint16_t)&USART_SERIAL.DATA) >> 8) & 0xff;
    DMA.CH0.DESTADDR2 = 0;

    // Initialize USART module    
    PORTC.DIRSET = 0xff; // TODO: set only TX pin
    
    (USART_SERIAL).CTRLC = ((USART_SERIAL).CTRLC & (~USART_CMODE_gm)) | USART_CMODE_ASYNCHRONOUS_gc;
    (USART_SERIAL).CTRLC = (uint8_t)USART_SERIAL_CHAR_LENGTH | USART_SERIAL_PARITY | (USART_SERIAL_STOP_BIT ? USART_SBMODE_bm : 0);
    usart_set_baudrate(&USART_SERIAL, USART_SERIAL_BAUDRATE, F_CPU);
    (USART_SERIAL).CTRLB |= USART_TXEN_bm | USART_RXEN_bm;
    
    DMA.INTFLAGS |= DMA_CH0TRNIF_bm | DMA_CH0ERRIF_bm;
    PMIC.CTRL |= PMIC_LOLVLEN_bm;

    // Reassign standard output to the USART module
    _uart_stdout.put = usart_putchar;
    _uart_stdout.get = NULL;
    _uart_stdout.flags = _FDEV_SETUP_WRITE;
    _uart_stdout.udata = 0;

    stdout = &_uart_stdout;
    
    // Install task
    xTaskCreate(usart_print_buffer_task, (const signed char *)"usart-printf", 128, NULL, configMAX_PRIORITIES - 1, NULL);
}
