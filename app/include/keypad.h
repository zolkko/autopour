
#ifndef KEYPAD_H_
#define KEYPAD_H_

#include "config.h"

#ifndef KEYPAD_PORT
#error Please define keypad port
#endif

#ifndef KEYPAD_BUTTON_STAT
#error Please define KEYPAD_BUTTON_STAT symbol
#endif

#ifndef KEYPAD_BUTTON_FUNC
#error Please define KEYPAD_BUTTON_FUNC symbol
#endif

#ifndef KEYPAD_TIMER_PMIC_INTERRUPT_PRIORITY
#error Please deinfe KEYPAD_TIMER_PMIC_INTERRUPT_PRIORITY
#endif

#ifndef KEYPAD_TIMER
#error Please define keypad timer
#endif

#ifndef KEYPAD_TIMER_DIV
#error Please define KEYPAD_TIMER_DIV symbol
#endif

#ifndef KEYPAD_MIN_INTERVAL
#error Please define KEYPAD_MIN_INTERVAL
#endif

#ifndef KEYPAD_PMIC_INTERRUPT_PRIORITY
#error Please define KEYPAD_PMIC_INTERRUPT_PRIORITY symbol
#endif

#if (KEYPAD_TIMER_PMIC_INTERRUPT_PRIORITY == PMIC_LOLVLEN_bm)
#	define KEYPAD_TIMER_INTERRUPT_PRIORITY TC_OVFINTLVL_LO_gc
#elif (KEYPAD_TIMER_PMIC_INTERRUPT_PRIORITY == PMIC_MEDLVLEN_bm)
#	define KEYPAD_TIMER_INTERRUPT_PRIORITY TC_OVFINTLVL_MED_gc
#else
#	define KEYPAD_TIMER_INTERRUPT_PRIORITY TC_OVFINTLVL_HI_gc
#endif

#if (KEYPAD_PMIC_INTERRUPT_PRIORITY == PMIC_LOLVLEN_bm)
#	define KEYPAD_INTERRUPT_PRIORITY  PORT_INT0LVL_LO_gc
#elif (KEYPAD_PMIC_INTERRUPT_PRIORITY == PMIC_MEDLVLEN_bm)
#	define KEYPAD_INTERRUPT_PRIORITY PORT_INT0LVL_MED_gc
#else
#	define KEYPAD_INTERRUPT_PRIORITY PORT_INT0LVL_HI_gc
#endif

#define _KEYPAD_BUTTON_CTRL(x) PIN ## x ## CTRL
#define KEYPAD_BUTTON_CTRL(x) _KEYPAD_BUTTON_CTRL(x)

void keypad_init(void);

void keypad_enable(void);

void keypad_disable(void);

#endif
