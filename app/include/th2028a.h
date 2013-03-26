/*
 * th2028a.h - simple driver for LCD TH2028A (R)
 */

#ifndef TH2028A_H_
#define TH2028A_H_

/*
 * Default pin configuration
 */
#ifndef TH2028A_PORT
#error Define TH2028A_PORT of type PORT_t
#endif

#ifndef TH2028A_COMM_PIN
#error Define TH2028A_COMM_PIN
#endif

#ifndef TH2028A_HEART_PIN
#error Define TH2028A_HEART_PIN
#endif

#ifndef TH2028A_BAR1_PIN
#error Define TH2028A_BAR1_PIN
#endif

#ifndef TH2028A_BAR2_PIN
#error Define TH2028A_BAR2_PIN
#endif

#ifndef TH2028A_BAR3_PIN
#error Define TH2028A_BAR3_PIN
#endif

#ifndef TH2028A_BAR4_PIN
#error Define TH2028A_BAR4_PIN
#endif

#ifndef TH2028A_BAR5_PIN
#error Define TH2028A_BAR5_PIN
#endif

#ifndef TH2028A_BATTERY_PIN
#error Define TH2028A_BATTERY_PIN
#endif

/*
 * Initializes LCD, port and state variable
 */
void th2028a_init(void);

/*
 * Methods show specified symbol on LCD screen.
 * symbol parameter is one of TH2028A_*_PIN constants.
 */
void th2028a_show(const uint8_t symbol);

/*
 * Method hides specified symbol on LCD screen.
 */
void th2028a_hide(const uint8_t symbol);

/*
 * Methods draws current LCD state and then shifts LCD into the next state.
 *
 * In order to display the information LCD screens must continuously
 * update their state. State refresh is done by inverting its bits.
 * 20kHz refresh rate will be enough. But for TH2028A even lower frequency
 * value will be suficient.
 */
void th2028a_draw(void);

#endif

