
#include <avr/io.h>
#include <avr/interrupt.h>
#include "rtc.h"

ISR(RTC_OVF_vect)
{
	PORTE.DIRTGL = 1;
}

void rtc_init(void)
{
	CLK.RTCCTRL = CLK_RTCSRC_RCOSC_gc | CLK_RTCEN_bm; // 1kHz
	
	RTC_SYNC(RTC.PER = RTC_DEFAULT_PERIOD);
	RTC_SYNC(RTC.CNT = 0);
	RTC_SYNC(RTC.COMP = 0);
	RTC_SYNC(RTC.CTRL = (RTC.CTRL & ~RTC_PRESCALER_gm ) | RTC_PRESCALER_DIV1024_gc);
	
	RTC.INTCTRL = RTC_OVFINTLVL_MED_gc;
	
	// enable interrupts
	PMIC.CTRL |= RTC_PMIC_INTERRUPT_PRIORITY;
}

void rtc_enable(void)
{
	// TODO:
}

void rtc_disable(void)
{
	// TODO:	
}
