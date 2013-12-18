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
    SPID.CTRL = SPI_ENABLE_bm | SPI_MASTER_bm | SPI_MODE_0_gc; // | SPI_PRESCALER_DIV4_gc;
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

/**
 * CC1101 power-on reset routing
 */
void cc1101_poweron_reset(void)
{    
    // Strobe chip select low/high
    // I use here smallest possible value because cc1101 supports up to 10Mhz clock
    CC1101_CHIP_SELECT();
    _delay_us(1);
    CC1101_CHIP_RELEASE();
    
    _delay_us(50);
    while ((PORTD.IN & PIN6_bm) != PIN6_bm);
    
    // Wait for CHIP_RDYn
    CC1101_CHIP_SELECT();
    while ( !CC1101_CHIP_RDY() );
    
    // Send SRES strobe
    uint8_t input_data = spi_rw(0x30);
    
    // Wait for SO goes low
    while ( !CC1101_CHIP_RDY() );
    
    // Setup GDO0 function
    uint8_t data1 = spi_rw(0x80 | 0x02);
    uint8_t data2 = spi_rw(0xff);
    
    CC1101_CHIP_RELEASE();
}


uint8_t cc1101_read(uint8_t reg)
{
    CC1101_CHIP_SELECT();
    while ( !CC1101_CHIP_NOT_RDY() );
    spi_rw(reg);
    uint8_t result = spi_rw(0xff);
    CC1101_CHIP_RELEASE();
    
    return result;
}

void cc1101_write(uint8_t reg, uint8_t data)
{
    CC1101_CHIP_SELECT();
    while ( !CC1101_CHIP_NOT_RDY() );
    
    spi_rw(reg | 0x80);
    spi_rw(data);
    
    CC1101_CHIP_RELEASE();
}


void cc1101_initialize_registers(void)
{
    cc1101_write(CCx_FSCTRL1, 0x08);  // FSCTRL1   Frequency synthesizer control
    cc1101_write(CCx_FSCTRL0, 0x00);  // FSCTRL0   Frequency synthesizer control.
    
    cc1101_write(CCx_FREQ2, 0x23);    // FREQ2     Frequency control word, high unsigned char.
    cc1101_write(CCx_FREQ1, 0x31);    // FREQ1     Frequency control word, middle unsigned char.
    cc1101_write(CCx_FREQ0, 0x3B);    // FREQ0     Frequency control word, low unsigned char.
    
    cc1101_write(CCx_MDMCFG4, 0x7B);  // MDMCFG4   Modem configuration.
    cc1101_write(CCx_MDMCFG3, 0x83);  // MDMCFG3   Modem configuration.
    cc1101_write(CCx_MDMCFG2, 0x03);  // MDMCFG2   Modem configuration.
    cc1101_write(CCx_MDMCFG1, 0x22);  // MDMCFG1   Modem configuration.
    cc1101_write(CCx_MDMCFG0, 0xF8);  // MDMCFG0   Modem configuration.
    
    cc1101_write(CCx_CHANNR, 0x00);   // CHANNR    Channel number.
    cc1101_write(CCx_DEVIATN, 0x42);  // DEVIATN   Modem deviation setting (when FSK modulation is enabled).
    cc1101_write(CCx_FREND1, 0xB6);   // FREND1    Front end RX configuration.
    cc1101_write(CCx_FREND0, 0x10);   // FREND0    Front end TX configuration.
    cc1101_write(CCx_MCSM0, 0x18);    // MCSM0     Main Radio Control State Machine configuration.
    cc1101_write(CCx_FOCCFG, 0x1D);   // FOCCFG    Frequency Offset Compensation Configuration.
    cc1101_write(CCx_BSCFG, 0x1C);    // BSCFG     Bit synchronization Configuration.
    
    cc1101_write(CCx_AGCCTRL2, 0xC7); // AGCCTRL2  AGC control.
    cc1101_write(CCx_AGCCTRL1, 0x00); // AGCCTRL1  AGC control.
    cc1101_write(CCx_AGCCTRL0, 0xB2); // AGCCTRL0  AGC control.
    
    cc1101_write(CCx_FSCAL3, 0xEA);   // FSCAL3    Frequency synthesizer calibration.
    cc1101_write(CCx_FSCAL2, 0x2A);   // FSCAL2    Frequency synthesizer calibration.
    cc1101_write(CCx_FSCAL1, 0x00);   // FSCAL1    Frequency synthesizer calibration.
    cc1101_write(CCx_FSCAL0, 0x1F);   // FSCAL0    Frequency synthesizer calibration.
    cc1101_write(CCx_FSTEST, 0x59);   // FSTEST    Frequency synthesizer calibration.
    
    cc1101_write(CCx_TEST2, 0x81);    // TEST2     Various test settings.
    cc1101_write(CCx_TEST1, 0x35);    // TEST1     Various test settings.
    cc1101_write(CCx_TEST0, 0x09);    // TEST0     Various test settings.
    cc1101_write(CCx_FIFOTHR, 0x0E);  // FIFOTHR   RXFIFO and TXFIFO thresholds.
    cc1101_write(CCx_IOCFG2, 0x09);   // IOCFG2    GDO2 output pin configuration.0x09-CCA mode, 0x2E-High impedance
    cc1101_write(CCx_IOCFG0D, 0x01);  // IOCFG0D   GDO0 output pin configuration. Refer to SmartRF?Studio User Manual for detailed pseudo register explanation.
    cc1101_write(CCx_PKTCTRL1, 0x04); // PKTCTRL1  Packet automation control. bit2 = 1 append RSSI and LQI ,bit2 = 0 not append
    cc1101_write(CCx_PKTCTRL0, 0x05); // PKTCTRL0  Packet automation control.
    cc1101_write(CCx_ADDR, 0x00);     // ADDR      Device address.
    cc1101_write(CCx_PKTLEN, CCx_PACKT_LEN);
}


int main(void)
{
    sys_init();
    usart_init();
    
    cc1101_init_port();
    spi_init();
    cc1101_poweron_reset();

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
