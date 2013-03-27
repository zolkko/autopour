// TODO: move port/pin defines into config file.

#ifndef KEYPAD_H_
#define KEYPAD_H_

#ifndef KEYPAD_INTERRUPT_PRIORITY
#define KEYPAD_INTERRUPT_PRIORITY PORT_INT0LVL_LO_gc
#endif

#ifndef KEYPAD_PMIC_INTERRUPT_PRIORITY
#define KEYPAD_PMIC_INTERRUPT_PRIORITY PMIC_LOLVLEN_bm
#endif

#ifndef KEYPAD_PORT
#define KEYPAD_PORT PORTF
#endif

#define KEYPAD_INT0_INTERRUPT(x) x ## _INT0_vect

#ifndef KEYPAD_BUTTON_STAT
#define KEYPAD_BUTTON_STAT 7
#endif

#ifndef KEYPAD_BUTTON_FUNC
#define KEYPAD_BUTTON_FUNC 6
#endif

#define _KEYPAD_BUTTON_CTRL(x) PIN ## x ## CTRL
#define KEYPAD_BUTTON_CTRL(x) _KEYPAD_BUTTON_CTRL(x)

void keypad_init(void);

#endif
