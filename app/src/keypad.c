
#include <avr/io.h>
#include <avr/interrupt.h>
#include "keypad.h"


ISR(KEYPAD_INT0_INTERRUPT(KEYPAD_PORT))
{
	// TODO: debounce
}

/*
 * Initializes buttons as pulled-up input sources. STAT button also detects
 * falling edge and triggers interrupt0.
 */
void keypad_init(void)
{
	KEYPAD_PORT.DIRCLR = _BV(KEYPAD_BUTTON_STAT) | _BV(KEYPAD_BUTTON_FUNC);
	KEYPAD_PORT.KEYPAD_BUTTON_CTRL(KEYPAD_BUTTON_STAT) = PORT_OPC_PULLUP_gc | PORT_ISC_FALLING_gc;
	KEYPAD_PORT.KEYPAD_BUTTON_CTRL(KEYPAD_BUTTON_FUNC) = PORT_OPC_PULLUP_gc;
	KEYPAD_PORT.INT0MASK = _BV(KEYPAD_BUTTON_STAT);
	KEYPAD_PORT.INTCTRL = (KEYPAD_PORT.INTCTRL & ~PORT_INT0LVL_gm) | PORT_INT0LVL_HI_gc;
	
	PMIC.CTRL |= KEYPAD_PMIC_INTERRUPT_PRIORITY;
}
