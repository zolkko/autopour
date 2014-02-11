#include <avr/io.h>
#include <util/atomic.h>
#include "sys.h"


static void critical_write(volatile uint8_t * address, uint8_t value);
static void sys_enable_pll(void);
static void sys_osc_initialize(void);


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
 * Enable PLL and run the system on 2Mhz * 5 == 10Mhz
 */
void sys_enable_pll(void)
{
    ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
        OSC.CTRL &= ~OSC_PLLEN_bm;
        OSC.PLLCTRL = OSC_PLLSRC_RC2M_gc | (OSC_PLLFAC_gm & 0x05);
        OSC.CTRL |= OSC_PLLEN_bm;
        do { } while (!(OSC.STATUS & OSC_PLLRDY_bm)) ;
        critical_write(&(CLK.PSCTRL), CLK_PSADIV_1_gc | CLK_PSBCDIV_1_1_gc);
        critical_write(&(CLK.CTRL), CLK_SCLKSEL_PLL_gc);
	}
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
        critical_write(&(CLK.CTRL), CLK_SCLKSEL_RC2M_gc);
    }
}


void sys_init(void)
{
    sys_osc_initialize();
	sys_enable_pll();
}
