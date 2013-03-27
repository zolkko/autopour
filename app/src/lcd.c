
#include <avr/io.h>
#include <avr/interrupt.h>
#include "lcd.h"
#include "th2028a.h"

ISR(TCC0_OVF_vect)
{
	th2028a_draw();
}

/*
 * It assumes that when LCD works the system is clocked by
 * 2Mhz internal generator.
 */
void lcd_init(void)
{
	th2028a_init();
	
	LCD_TIMER.PER   = LCD_TIMER_PERIOD;
	LCD_TIMER.CTRLA = TC_CLKSEL_DIV1_gc;
	
	PMIC.CTRL |= LCD_PMIC_INTERRUPT_PRIORITY;
}

/*
 * Enables Timer/Counter interrupt. This leads to LCD refreshing.
 */
void lcd_enable(void)
{
	LCD_TIMER.INTCTRLA = TC_OVFINTLVL_LO_gc;
}

/*
 * Method clears LCD output and then disables Timer/Counter
 * interrupt. This leads to LCD disable.
 */
void lcd_disable(void)
{
	LCD_TIMER.INTCTRLA = TC_OVFINTLVL_OFF_gc;
	th2028a_turnoff();
}
