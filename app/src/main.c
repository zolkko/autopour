#include "config.h"
#include <stddef.h>
#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#include <FreeRTOS.h>
#include <task.h>

#include <stdio.h>
#include "sys.h"
#include "cc1101.h"
#include "usart_stdio.h"

#include "ccx_hw.h"
#include "ccx_hw_xmega.h"


void app_task(void * params)
{
    printf("FreeRTOS 7.6 XMega initialized\r\n");

    rf_handle_t * rf = (rf_handle_t *)params;
    
    cc1101_select(rf);
    cc1101_wait_chip_ready(rf);

    uint8_t part_number = 0;
    cc1101_read(rf, CCx_PARTNUM, &part_number);
    printf("Transceiver cc1101 part number %d\r\n", part_number);

    uint8_t chip_version = 0;
    cc1101_read(rf, CCx_VERSION, &chip_version);
    printf("Transceiver cc1101 version %d\r\n", chip_version);

    uint8_t status = cc1101_strobe_ide(rf);
    while ((status & CC1101_STATUS_STATE_bm) != CC1101_STATUS_STATE_IDLE_bm) {
        status = cc1101_nop(rf);
    }
    // Auto calibration should be turned on
    cc1101_strobe_flush_rx(rf);
    cc1101_strobe_flush_tx(rf);
    cc1101_release(rf);

    uint8_t counter = 0;
    while (true) {
        uint8_t buff[20];
        int written = snprintf((char *) buff, 20, "main-task %d\r\n", counter);
        if (written > 0) {
            printf((const char *)buff);
            cc1101_transmit(rf, buff, written, 0, 0);
        }
        vTaskDelay(1000);
        counter++;
    }

    vTaskDelete(NULL);
}


ccx_hw_t rf_hw;
extern ccx_xmega_hw_t ccx_hw_default;


int main(void)
{
    rf_handle_t rf;

    sei();

    sys_init();
    usart_init();

    ccx_hw_xmega_init(&rf_hw, &ccx_hw_default);

    cc1101_hw_initialize();
    cc1101_initialize(&rf);
    cc1101_poweron_reset(&rf);
    cc1101_initialize_registers(&rf);

    if (xTaskCreate(app_task, (const signed char *)"main-task", 512, &rf, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

	cli();
    
    vTaskStartScheduler();

reset_controller:

    do {} while (true);
	
	return 0;
}
