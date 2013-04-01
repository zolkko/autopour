
#include <stddef.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include "keypad.h"


#define BTN_PRESS_bp 1
#define BTN_OVF_bp 2
#define BTN_bm 3

#define BTN_STAT 0
#define BTN_STAT_bp (BTN_PRESS_bp << BTN_STAT)
#define BTN_STAT_OVF_bp (BTN_OVF_bp << BTN_STAT)
#define BTN_STAT_bm (BTN_bm << BTN_STAT)

#define BTN_FUNC 2
#define BTN_FUNC_bp (BTN_PRESS_bp << BTN_FUNC)
#define BTN_FUNC_OVF_bp (BTN_OVF_bp << BTN_FUNC)
#define BTN_FUNC_bm (BTN_bm << BTN_FUNC)

static uint8_t  btn_flags = 0;
static uint16_t stat_cnt = 0;
static uint16_t func_cnt = 0;

typedef void (*button_handler)();

static inline void handler_button_keydown(uint8_t btn_offset, uint16_t * cnt);

static inline void handler_button_keyup(uint8_t btn_offset, uint16_t * cnt, button_handler handler);

ISR(TCC1_OVF_vect)
{
	if (btn_flags & BTN_STAT) {
		btn_flags |= BTN_STAT_OVF_bp;
	}
	
	if (btn_flags & BTN_FUNC) {
		btn_flags |= BTN_FUNC_OVF_bp;
	}
}

ISR(PORTF_INT0_vect)
{
	if (KEYPAD_TIMER.CTRLA & KEYPAD_TIMER_DIV) {
		uint8_t btn = KEYPAD_PORT.IN & _BV(KEYPAD_BUTTON_STAT);
		uint8_t pre = btn_flags & BTN_STAT_bp;
		if (!(btn || pre)) {
			handler_button_keydown(BTN_STAT, &stat_cnt);
		} else if (btn && pre) {
			handler_button_keyup(BTN_STAT, &stat_cnt, NULL);
		}
		
		btn = KEYPAD_PORT.INT0MASK & _BV(KEYPAD_BUTTON_FUNC);
		pre = btn_flags & BTN_FUNC_bp;
		if (!(btn || pre)) {
			handler_button_keydown(BTN_FUNC, &func_cnt);
		} else if (btn && pre) {
			handler_button_keyup(BTN_FUNC, &func_cnt, NULL);
		}
	}	
}

void handler_button_keydown(uint8_t btn_offset, uint16_t * cnt)
{
	btn_flags = (btn_flags & ~(BTN_bm << btn_offset)) | (BTN_PRESS_bp << btn_offset);
	*cnt = KEYPAD_TIMER.CNT;
}

void handler_button_keyup(uint8_t btn_offset, uint16_t * cnt, button_handler handler)
{
	uint16_t len = 0;
	if (btn_flags & (BTN_OVF_bp << btn_offset)) {
		len = (0xffff - (*cnt)) + KEYPAD_TIMER.CNT;
	} else {
		len = KEYPAD_TIMER.CNT - (*cnt);
	}
	
	btn_flags &= ~(BTN_bm << btn_offset);
	
	if (len > 80) {
		if (handler != NULL) {
			handler();
		}
	}
}

/*
 * Initializes buttons as pulled-up input sources. STAT button also detects
 * falling edge and triggers interrupt0.
 */
void keypad_init(void)
{
	KEYPAD_PORT.DIRCLR = _BV(KEYPAD_BUTTON_STAT) | _BV(KEYPAD_BUTTON_FUNC);
	KEYPAD_PORT.KEYPAD_BUTTON_CTRL(KEYPAD_BUTTON_STAT) = PORT_OPC_PULLUP_gc | PORT_ISC_BOTHEDGES_gc;
	KEYPAD_PORT.KEYPAD_BUTTON_CTRL(KEYPAD_BUTTON_FUNC) = PORT_OPC_PULLUP_gc | PORT_ISC_BOTHEDGES_gc;
	KEYPAD_PORT.INT0MASK = _BV(KEYPAD_BUTTON_STAT) | _BV(KEYPAD_BUTTON_FUNC);
	KEYPAD_PORT.INTCTRL = (KEYPAD_PORT.INTCTRL & ~PORT_INT0LVL_gm) | KEYPAD_INTERRUPT_PRIORITY;
	
	KEYPAD_TIMER.INTCTRLA = KEYPAD_TIMER_INTERRUPT_PRIORITY;
	KEYPAD_TIMER.PER = 0xffff;
	KEYPAD_TIMER.CNT = 0;
	KEYPAD_TIMER.CTRLA = TC_CLKSEL_OFF_gc;
	
	PMIC.CTRL |= KEYPAD_PMIC_INTERRUPT_PRIORITY | KEYPAD_TIMER_PMIC_INTERRUPT_PRIORITY;
}

void keypad_enable(void)
{
	KEYPAD_TIMER.CNT = 0;
	KEYPAD_TIMER.CTRLA = KEYPAD_TIMER_DIV;
}

void keypad_disable(void)
{
	KEYPAD_TIMER.CTRLA = TC_CLKSEL_OFF_gc;
	btn_flags = 0;
	stat_cnt = 0;
	func_cnt = 0;	
}
