#include <avr/io.h>
#include <util/atomic.h>
#include "sys.h"


static void sys_osc_initialize(void);
static void critical_write(volatile uint8_t * address, uint8_t value);


void critical_write(volatile uint8_t * address, uint8_t value)
{
    volatile uint8_t * tmpAddr = address;
    RAMPZ = 0;
    asm volatile(
    "movw r30,  %0"       "\n\t"
    "ldi  r16,  %2"       "\n\t"
    "out   %3, r16"       "\n\t"
    "st     Z,  %1"       "\n\t"
    :
    : "r" (tmpAddr), "r" (value), "i"(CCP_IOREG_gc), "i" (&CCP)
    : "r16", "r30", "r31"
    );
}


/**
 * Enables 2MHz oscillator and set it as the system
 * clock source
 */
void sys_osc_initialize(void)
{
    ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
        OSC.CTRL |= OSC_RC2MEN_bm;
        do {} while(!(OSC.STATUS & OSC_RC2MRDY_bm));
        critical_write(&(CLK.CTRL), CLK_SCLKSEL_RC32M_gc);
    }
}


void sys_init(void)
{
    sys_osc_initialize();
}
