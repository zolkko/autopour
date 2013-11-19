
#include "config.h"
#include <stddef.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include "lcd.h"
#include "keypad.h"

int main(void)
{
	cli();
	
	lcd_init();
	keypad_init();
	
	th2028a_show(TH2028A_HEART_PIN);
	th2028a_show(TH2028A_BAR1_PIN);
	th2028a_show(TH2028A_BAR2_PIN);
	th2028a_show(TH2028A_BAR3_PIN);
	th2028a_show(TH2028A_BAR4_PIN);
	th2028a_show(TH2028A_BAR5_PIN);
	th2028a_show(TH2028A_BATTERY_PIN);
	
	sei();
	
	lcd_enable();
	
	while (1) {
		// do nothing
	}
	
	return 0;
}
