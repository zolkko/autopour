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


/**
 * Initialize SPI module
 *
 * PD7 - SCK
 * PD6 - MISO
 * PD5 - MOSI
 *
 * PD4 - Slave Select
 *
 * The prescaler should be 8 because 2000000 / 250000 = 8
 * SPI_DORD_bm - should be cleared because logic sends MSB first
 *
 * In brust mode it is possible to run SPI interface up to 6.5Mhz
 */
void spi_init(void)
{
    SPID.CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc | SPI_PRESCALER_DIV4_gc;
}

uint8_t spi_rw(uint8_t data)
{
    SPID.DATA = data;
    while (true) {
        uint8_t flags = SPID.STATUS;
        if ((flags & SPI_IF_bm) != 0) {
            return SPID.DATA;
        } else if ((flags & SPI_WRCOL_bm) != 0) {
            SPID.DATA = data;
        }
    }
}


#define CC1101_CHIP_SELECT()  (PORTD.OUTCLR = PIN4_bm)
#define CC1101_CHIP_RELEASE() (PORTD.OUTSET = PIN4_bm)

#define CC1101_CHIP_RDY()   ((PORTD.IN & PIN6_bm) == 0)
#define CC1101_CHIP_NOT_RDY (PORTD.IN & PIN6_bm)


/**
 * Set Clock, MOSI and SS signals as output.
 * SS and Clock should be set high and MOSI low to avoid potential problems with pin control mode.
 */

void cc1101_init_port(void)
{
     // Set SCK, MOSI and SS as Output
     PORTD.DIRSET = PIN7_bm | PIN5_bm | PIN4_bm;
     PORTD.OUTSET = PIN7_bm | PIN4_bm;
     PORTD.OUTCLR = PIN5_bm;
     
     // And MISO as Input in Totem-Pole with Pull-up configuration
     // to support situations when cc1101 module is not
     // connected or is broken.
     
     PORTD.DIRCLR = PIN6_bm;
     PORTD.PIN6CTRL = PORT_OPC_PULLUP_gc;   
}


void __impl_chip_select(void)
{
    CC1101_CHIP_SELECT();
}


void __impl_chip_release(void)
{
    CC1101_CHIP_RELEASE();
}


bool __impl_chip_ready(void)
{
    return CC1101_CHIP_RDY();
}


uint8_t __impl_chip_write(uint8_t data)
{
    return spi_rw(data);
}


int main(void)
{
    rf_handle_t rf;
    
    sys_init();
    usart_init();
    
    cc1101_init_port();
    spi_init();
    
    rf.select = &__impl_chip_select;
    rf.release = &__impl_chip_release;
    rf.ready = &__impl_chip_ready;
    rf.write = &__impl_chip_write;
    
    cc1101_poweron_reset(&rf);

    if (xTaskCreate(TestFunction, (const signed char *)"main-task", 128, NULL, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

    if (xTaskCreate(AnotherTask, (const signed char *)"second-task", 128, NULL, 1, NULL) != pdTRUE) {
        goto reset_controller;
    }

	cli();
    
    printf("FreeRTOS 7.6 XMega initialized\r\n");
    
    vTaskStartScheduler();

reset_controller:

    do {} while (true);
	
	return 0;
}
