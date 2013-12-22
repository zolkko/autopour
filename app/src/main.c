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
    printf("FreeRTOS 7.6 XMega initialized...\r\n");

    while (true) {
        vTaskSuspendAll();
        printf("task-1\r\n");
        xTaskResumeAll();
        
        vTaskDelay(10);
    }
    
    vTaskDelete(NULL);
}


int main(void)
{
    rf_handle_t rf;
    
    sys_init();
    usart_init();

    cc1101_hw_initialize();
    cc1101_initialize(&rf);
    
    cc1101_poweron_reset(&rf);

    if (xTaskCreate(app_task, (const signed char *)"main-task", 256, NULL, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

	cli();
    
    vTaskStartScheduler();

reset_controller:

    do {} while (true);
	
	return 0;
}
