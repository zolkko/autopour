
#include <stddef.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/atomic.h>
#include "keypad.h"

/*
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

typedef void (*button_handler)();

static inline void handle_button(uint8_t input, uint8_t btn_flags_snap, uint8_t btn_num, uint8_t btn_offset, uint16_t * cnt, button_handler handler);

static inline void handle_button_keydown(uint8_t btn_offset, uint16_t * cnt);

static inline void handle_button_keyup(uint8_t btn_offset, uint16_t * cnt, button_handler handler);

static uint8_t btn_flags = 0;

static uint16_t stat_cnt = 0;

static uint16_t func_cnt = 0;

ISR(TCC1_OVF_vect)
{
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		if (btn_flags & BTN_STAT) {
			btn_flags |= BTN_STAT_OVF_bp;
		}
		
		if (btn_flags & BTN_FUNC) {
			btn_flags |= BTN_FUNC_OVF_bp;
		}
	}	
}

ISR(PORTF_INT0_vect)
{
	if (KEYPAD_TIMER.CTRLA & KEYPAD_TIMER_DIV) {
		uint8_t in = KEYPAD_PORT.IN;
		uint8_t btn_flags_snap;
		ATOMIC_BLOCK(ATOMIC_FORCEON) {
			btn_flags_snap = btn_flags;
		}		
		handle_button(in, btn_flags_snap, KEYPAD_BUTTON_STAT, BTN_STAT, &stat_cnt, NULL);
		handle_button(in, btn_flags_snap, KEYPAD_BUTTON_FUNC, BTN_FUNC, &func_cnt, NULL);
	}	
}

void handle_button(uint8_t input, uint8_t btn_flags_snap, uint8_t btn_num, uint8_t btn_offset, uint16_t * cnt, button_handler handler)
{
	uint8_t btn = input & _BV(btn_num);
	uint8_t pre = btn_flags_snap & (BTN_PRESS_bp << btn_offset);
	if (!(btn || pre)) {
		handle_button_keydown(btn_offset, &cnt);
	} else if (btn && pre) {
		handle_button_keyup(btn_offset, &cnt, handler);
	}
}

void handle_button_keydown(uint8_t btn_offset, uint16_t * cnt)
{
	btn_flags = (btn_flags & ~(BTN_bm << btn_offset)) | (BTN_PRESS_bp << btn_offset);
	*cnt = KEYPAD_TIMER.CNT;
}

void handle_button_keyup(uint8_t btn_offset, uint16_t * cnt, button_handler handler)
{
	uint16_t len = 0;
	if (btn_flags & (BTN_OVF_bp << btn_offset)) {
		len = (0xffff - (*cnt)) + KEYPAD_TIMER.CNT;
	} else {
		len = KEYPAD_TIMER.CNT - (*cnt);
	}
	
	btn_flags &= ~(BTN_bm << btn_offset);
	
	if (len > KEYPAD_MIN_INTERVAL) {
		if (handler != NULL) {
			handler();
		}
	}
}
*/
/*
 * Initializes buttons as pulled-up input sources.
 */
/*
void keypad_init(void)
{
	KEYPAD_PORT.DIRCLR = _BV(KEYPAD_BUTTON_STAT) | _BV(KEYPAD_BUTTON_FUNC);
	KEYPAD_PORT.KEYPAD_BUTTON_CTRL(KEYPAD_BUTTON_STAT) = PORT_OPC_PULLUP_gc | PORT_ISC_BOTHEDGES_gc;
	KEYPAD_PORT.KEYPAD_BUTTON_CTRL(KEYPAD_BUTTON_FUNC) = PORT_OPC_PULLUP_gc | PORT_ISC_BOTHEDGES_gc;
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
	KEYPAD_PORT.INT0MASK = _BV(KEYPAD_BUTTON_STAT) | _BV(KEYPAD_BUTTON_FUNC);
}

void keypad_disable(void)
{
	KEYPAD_PORT.INT0MASK = 0;
	KEYPAD_TIMER.CTRLA = TC_CLKSEL_OFF_gc;
	btn_flags = 0;
	stat_cnt = 0;
	func_cnt = 0;	
}
*/
