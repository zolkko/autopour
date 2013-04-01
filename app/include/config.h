#ifndef CONFIG_H_
#define CONFIG_H_

#define F_CPU           2000000UL

// LCD definition
#define TH2028A_PORT PORTA
#define TH2028A_COMM_PIN 7
#define TH2028A_BAR1_PIN 0
#define TH2028A_BAR2_PIN 1
#define TH2028A_BAR3_PIN 2
#define TH2028A_BAR4_PIN 3
#define TH2028A_BAR5_PIN 4
#define TH2028A_HEART_PIN 5
#define TH2028A_BATTERY_PIN 6


#define LCD_PMIC_INTERRUPT_PRIORITY PMIC_LOLVLEN_bm
#define LCD_TIMER TCC0
#define LCD_TIMER_PERIOD 32

// Keypad definitions
#define KEYPAD_PORT PORTF
#define KEYPAD_BUTTON_STAT 7
#define KEYPAD_BUTTON_FUNC 6

#define KEYPAD_PMIC_INTERRUPT_PRIORITY PMIC_HILVLEN_bm
#define KEYPAD_TIMER_PMIC_INTERRUPT_PRIORITY PMIC_MEDLVLEN_bm
#define KEYPAD_TIMER TCC1
#define KEYPAD_TIMER_DIV TC_CLKSEL_DIV256_gc

// RTC
#define RTC_PRESCALLER RTC_PRESCALER_DIV1_gc
#define RTC_DEFAULT_PERIOD 10
#define RTC_PMIC_INTERRUPT_PRIORITY PMIC_MEDLVLEN_bm

#endif
