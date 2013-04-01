
#ifndef SYS_H_
#define SYS_H_

/*
 * System clocks, power saving mode configuring
 */
void sys_init(void);

/*
 * Method switches off unnecessary peripherals
 * and runs CPU into IDLE mode.
 */
void sys_sleep(void);

#endif
