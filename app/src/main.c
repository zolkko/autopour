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


void app_task(void * params)
{
    printf("FreeRTOS 7.6 XMega initialized\r\n");

    rf_handle_t * rf = (rf_handle_t *)params;
    
    // try to identify radio-IC
    printf("Detecting radio...");
    uint8_t chip_version = 0;
    uint8_t stat = cc1101_read(rf, CCx_VERSION, &chip_version);
    printf("\t[OK]\r\n");
    printf("Radio cc1101 version %d\t[OK]\r\n", chip_version);

    cc1101_strobe_ide(rf);
    cc1101_strobe_flush_rx(rf);
    cc1101_strobe_flush_tx(rf);

    uint8_t counter = 0;
    while (true) {
        char * buff[20];
        int written = snprintf(buff, 20, "main-task %d\r\n", counter);
        if (written > 0) {
            printf(buff);
            cc1101_transmit(rf, buff, written, 0, 0);
        }
        vTaskDelay(1000);
        counter++;
    }

    vTaskDelete(NULL);
}


int main(void)
{
    rf_handle_t rf;

    sei();

    sys_init();
    usart_init();

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
