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
#include "usart_stdio.h"


#include "rf.h"
#include "ccx_hw.h"
#include "ccx_hw_xmega.h"
#include "cc1101.h"


void app_task(void * params)
{
    printf("FreeRTOS 7.6 XMega initialized\r\n");

	rf_t * rf = (rf_t *)params;

	uint8_t version = rf_version(rf);
	uint8_t partnum = rf_part_number(rf);

    printf("Transceiver cc1101 part number %d version %d\r\n", partnum, version);

    uint8_t counter = 0;
    while (true) {
        #define BUFF_SIZE 60
        uint8_t buff[BUFF_SIZE];
        buff[0] = 0;
        buff[1] = 0;
        buff[2] = 0;
        
        int written = snprintf((char *) (&buff[3]), BUFF_SIZE - 3, "xxxxxxx xxx zz this string comes from main-task %d\r\n", counter);
        if (written > 0) {
            // printf((const char *)(&buff[3]));
            buff[0] = written + 2;
            rf_send(rf, buff, written + 3);
        }
        
        vTaskDelay(1000);
        counter++;
    }

    vTaskDelete(NULL);
}


rf_t rf;
ccx_hw_t rf_hw;
extern ccx_xmega_hw_t ccx_hw_default;


int main(void)
{
	taskDISABLE_INTERRUPTS();

    sys_init();
    usart_init();

    ccx_hw_xmega_init(&rf_hw, &ccx_hw_default);
	cc1101_init(&rf, &rf_hw);

    if (xTaskCreate(app_task, (const signed char *)"main-task", 512, &rf, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

    taskENABLE_INTERRUPTS();

    vTaskStartScheduler();

reset_controller:

    do {} while (true);
	
	return 0;
}
