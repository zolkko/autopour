#ifndef CC1101_XMEGA_H_
#define CC1101_XMEGA_H_

#include "cc1101.h"


void __impl_hw_initialize(void);

void __impl_handle_initialize(rf_handle_t * rf);

bool __impl_cc1101_is_data(const rf_handle_t * rf);

void cc1101_impl_wait_chip_ready(const rf_handle_t * rf);


#endif /* CC1101_XMEGA_H_ */
