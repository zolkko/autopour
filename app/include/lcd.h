
#ifndef LCD_H_
#define LCD_H_

#ifndef LCD_INTERRUPT_PRIORITY
#define LCD_INTERRUPT_PRIORITY TC_OVFINTLVL_LO_gc
#endif

#ifndef LCD_PMIC_INTERRUPT_PRIORITY
#define LCD_PMIC_INTERRUPT_PRIORITY PMIC_LOLVLEN_bm
#endif

#ifndef LCD_TIMER
#define LCD_TIMER TCC0
#endif

#ifndef LCD_TIMER_PERIOD
#define LCD_TIMER_PERIOD 0x0a
#endif

/*
 * Initializes LCD clock source.
 */
void lcd_init(void);

/*
 * Enables Timer/Counter interrupt what leads to LCD refreshing.
 */
void lcd_enable(void);

/*
 * Disables Timer/Counter interrupt and then clears LCD screen.
 */
void lcd_disable(void);

#endif
