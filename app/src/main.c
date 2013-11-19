
#include "config.h"
#include <stddef.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include "sys.h"
#include "rtc.h"
#include "lcd.h"
#include "keypad.h"
#include "evt.h"

#define DEFAULT_EVENT_TIMEOUT 100

struct State
{
	uint8_t value;	
};

struct State state;

int main(void)
{
	cli();
	/*
	sys_init();
	rtc_init();
	lcd_init();
	
	struct EvtQueue * evt = evt_new(DEFAULT_EVENT_TIMEOUT);
	
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
	keypad_enable();
	
	struct EvtQueueItem * event = NULL;
	while (1) {
		event = evt_dequeue(evt);
		switch (state.value) {
			case 1:
				break;
			default:
				break;
		}
		// TODO: dispatch event
		if (event) {
			evt_free_item(event);
		}
	}*/
	
	return 0;
}
