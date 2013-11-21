#include "config.h"
#include <stdbool.h>
#include <stdio.h>
#include <avr/io.h>
#include "usart_stdio.h"


static int uart_putchar(char, FILE *);

static FILE _uart_stdout;


/*
 * Prints character 'c' into UART module
 */
static int uart_putchar(char c, FILE * stream)
{
    do { } while (((USART_SERIAL).STATUS & USART_DREIF_bm) == 0);
    (USART_SERIAL).DATA = c;
    return 0;
}


static bool usart_set_baudrate(USART_t *usart, uint32_t baud, uint32_t cpu_hz)
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
    PORTC.DIRSET = 0xff; // TODO: set only TX pin
    
    (USART_SERIAL).CTRLC = ((USART_SERIAL).CTRLC & (~USART_CMODE_gm)) | USART_CMODE_ASYNCHRONOUS_gc;
    (USART_SERIAL).CTRLC = (uint8_t)USART_SERIAL_CHAR_LENGTH | USART_SERIAL_PARITY | (USART_SERIAL_STOP_BIT ? USART_SBMODE_bm : 0);
    usart_set_baudrate(&USART_SERIAL, USART_SERIAL_BAUDRATE, F_CPU);
    (USART_SERIAL).CTRLB |= USART_TXEN_bm | USART_RXEN_bm;

    // Reassign standard output to the UART
    _uart_stdout.put = uart_putchar;
    _uart_stdout.get = NULL;
    _uart_stdout.flags = _FDEV_SETUP_WRITE;
    _uart_stdout.udata = 0;

    stdout = &_uart_stdout;
}
