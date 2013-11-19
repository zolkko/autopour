
#ifndef KEYPAD_H_
#define KEYPAD_H_

#include "config.h"

#ifndef KEYPAD_PORT
#error PLease define keypad port
#endif

#ifndef KEYPAD_BUTTON_STAT
#error Please define KEYPAD_BUTTON_STAT symbol
#endif

#ifndef KEYPAD_BUTTON_FUNC
#error Please define KEYPAD_BUTTON_FUNC symbol
#endif

#ifndef KEYPAD_PMIC_INTERRUPT_PRIORITY
#error Please define KEYPAD_PMIC_INTERRUPT_PRIORITY symbol
#endif

#if (KEYPAD_PMIC_INTERRUPT_PRIORITY == PMIC_HILVLEN_bm)
#	define KEYPAD_INTERRUPT_PRIORITY  PORT_INT0LVL_LO_gc
#elif (KEYPAD_PMIC_INTERRUPT_PRIORITY == PMIC_MEDLVLEN_bm)
#	define KEYPAD_INTERRUPT_PRIORITY PORT_INT0LVL_MED_gc
#else
#	define KEYPAD_INTERRUPT_PRIORITY PORT_INT0LVL_HI_gc
#endif

#define KEYPAD_INT0_INTERRUPT(x) x ## _INT0_vect


#define _KEYPAD_BUTTON_CTRL(x) PIN ## x ## CTRL
#define KEYPAD_BUTTON_CTRL(x) _KEYPAD_BUTTON_CTRL(x)

void keypad_init(void);

#endif
