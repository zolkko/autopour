/*
 * th2028a.c - simple driver for LCD TH2028A (R)
 */

#include <stddef.h>
#include <avr/io.h>
#include "th2028a.h"


/*
 * Internal LCD state
 */
static uint8_t _lcd_state = 0x00;

/*
 * I hope this expression will be evaluated at compile time.
 */
#define LCD_PORT_MASK ((1 << TH2028A_COMM_PIN) & (1 << TH2028A_HEART_PIN) & (1 << TH2028A_BAR1_PIN) & (1 << TH2028A_BAR2_PIN) & (1 << TH2028A_BAR3_PIN) & (1 << TH2028A_BAR4_PIN) & (1 << TH2028A_BAR5_PIN) & (1 << TH2028A_BATTERY_PIN))

void
th2028a_init(void)
{
    _lcd_state = 0x00;
    TH2028A_PORT.OUTCLR = LCD_PORT_MASK;
    TH2028A_PORT.DIRSET = LCD_PORT_MASK;
}

/*
 * Method highlites specified symbol.
 * In order to hightlite a symbol it has to have oposite value
 * to COMM input.
 */
void
th2028a_show(const uint8_t symbol)
{
    if (_lcd_state & (1 << TH2028A_COMM_PIN))
        _lcd_state &= ~(1 << symbol);
    else
        _lcd_state |= (1 << symbol);
}

/*
 * Method hides specified symbol.
 * Logic does an oposide thing to th2028a_show method.
 */
void
th2028a_hide(const uint8_t symbol)
{
    if (_lcd_state & (1 << TH2028A_COMM_PIN))
        _lcd_state |= (1 << symbol);
    else
        _lcd_state &= ~(1 << symbol);
}

/*
 * Method draws current state and then shifts
 * LCD register into the next state.
 */
void
th2028a_draw(void)
{
    TH2028A_PORT.OUT = _lcd_state;
    _lcd_state ^= LCD_PORT_MASK;
}

