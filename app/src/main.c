#include "config.h"
#include <stddef.h>
#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>

#include <FreeRTOS.h>
#include <task.h>

#include <stdio.h>
#include "usart_stdio.h"



/**
 * This will be the main task
 */
void TestFunction(void * params)
{
    uint32_t i = 0;
    while (true) {
        i++;
        
        vTaskSuspendAll();
        printf("task-1\r\n");
        xTaskResumeAll();
        
        vTaskDelay(10);
        if (i > 2000) {
            i = 0;
        }
    }
    vTaskDelete(NULL);
}

void AnotherTask(void * params)
{
    uint32_t i = 0;
    while (true) {
        i++;
        
        vTaskSuspendAll();
        printf("task-2\r\n");
        xTaskResumeAll();
        
        vTaskDelay(100);
        if (i > 1000) {
            i = 0;
        }
    }
    vTaskDelete(NULL);
}

int main(void)
{
    usart_init();
    
    if (xTaskCreate(TestFunction, (const signed char *)"main-task", 128, NULL, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

    if (xTaskCreate(AnotherTask, (const signed char *)"second-task", 128, NULL, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

	cli();
    
    vTaskStartScheduler();

reset_controller:

    do {} while (true);
	
	return 0;
}
