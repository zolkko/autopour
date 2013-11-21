
#ifndef LCD_H_
#define LCD_H_

#include "config.h"

/*
#ifndef LCD_PMIC_INTERRUPT_PRIORITY
#error Please define LCD_PMIC_INTERRUPT_PRIORITY symbol
#endif

#ifndef LCD_TIMER
#error Please define Time/Counter register to be used as LCD refresh clock
#endif

#ifndef LCD_TIMER_PERIOD
#error Please define LCD_TIMER_PERIOD symbol dexxxs timer period 
#endif

#if (LCD_PMIC_INTERRUPT_PRIORITY == PMIC_LOLVLEN_bm)
#	define LCD_INTERRUPT_PRIORITY TC_OVFINTLVL_LO_gc
#elif (LCD_PMIC_INTERRUPT_PRIORITY == PMIC_MEDLVLEN_bm)
#	define LCD_INTERRUPT_PRIORITY TC_OVFINTLVL_MED_gc
#else
#	define LCD_INTERRUPT_PRIORITY TC_OVFINTLVL_HI_gc
#endif
*/
/*
 * Initializes LCD clock source.
 */
//void lcd_init(void);

/*
 * Enables Timer/Counter interrupt what leads to LCD refreshing.
 */
//void lcd_enable(void);

/*
 * Disables Timer/Counter interrupt and then clears LCD screen.
 */
//void lcd_disable(void);

#endif
