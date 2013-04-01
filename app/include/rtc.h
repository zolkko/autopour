
#ifndef RTC_H_
#define RTC_H_

#include "config.h"

/*
 * Default time is used counted from the last time user interacted with
 * the system. After this time 
 */
#ifndef RTC_DEFAULT_PERIOD
#error Please define RTC_DEFAULT_PERIOD which dexxxs interval length of this timer in seconds.
#endif

#ifndef RTC_PRESCALLER
#error Please define RTC_PRESCALLER prescaller value
#endif

#ifndef RTC_PMIC_INTERRUPT_PRIORITY
#error Please define RTC_PMIC_INTERRUPT_PRIORITY
#endif

#if (RTC_PMIC_INTERRUPT_PRIORITY == PMIC_LOLVLEN_bm)
#	define RTC_INTERRUPT_PRIORITY RTC_OVFINTLVL_LO_gc
#elif (RTC_PMIC_INTERRUPT_PRIORITY == PMIC_MEDLVLEN_bm)
#	define RTC_INTERRUPT_PRIORITY RTC_OVFINTLVL_MED_gc
#else
#	define RTC_INTERRUPT_PRIORITY RTC_OVFINTLVL_HI_gc
#endif

#define RTC_SYNC(X) do { } while (RTC.STATUS & RTC_SYNCBUSY_bm); (X)

void rtc_init(void);

/*
 * Stops clock for power saving mode
 */
void rtc_disable(void);

/*
 * Starts clock again
 */
void rtc_enable(void);

/*
 * Sets RTC overflow interrupt interval in seconds.
 */
void rtc_set_interval(uint8_t interval);

#endif
