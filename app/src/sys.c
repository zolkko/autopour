<<<<<<< HEAD

#include <avr/io.h>
#include "config.h"
#include "sys.h"


void sys_init(void)
{
	// CLOCK, SLEEP etc
}
=======

#include <avr/io.h>
#include "config.h"
#include "sys.h"

/*
 * Initializes system clocks. For now only 2Mhz
 * and internal 32kHz clocks are used.
 */
static void sys_init_clock(void);

static void sys_init_clock(void)
{
	PORTE.DIRSET = 1;
	
	// enable 32kHz internal oscillator
	OSC.CTRL |= OSC_RC32KEN_bm;
	do {
	} while ((OSC.STATUS & OSC_RC32KRDY_bm) == 0);
}

void sys_init(void)
{
	sys_init_clock();
}
>>>>>>> 506b2275984fe1401599d55251f7bf92ea493b1c
