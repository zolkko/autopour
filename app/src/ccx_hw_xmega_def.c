
#include <stddef.h>
#include <stdbool.h>
#include <avr/io.h>
#include "ccx_hw.h"
#include "ccx_hw_xmega.h"


// GDO0
#define CC1101_GDO0_PORT PORTA
#define CC1101_GDO0_PIN  PIN1_bm

// GDO1 / Slave Out
#define CC1101_XSO_PORT  PORTA
#define CC1101_XSO_PIN   PIN2_bm

// GDO2
#define CC1101_GDO2_PORT PORTA
#define CC1101_GDO2_PIN  PIN0_bm

// SPI
#define CC1101_SPI       SPID
#define CC1101_SPI_PORT  PORTD
#define CC1101_SS_PIN    PIN4_bm
#define CC1101_SI_PIN    PIN5_bm
#define CC1101_SO_PIN    PIN6_bm
#define CC1101_SCK_PIN   PIN7_bm


ccx_xmega_hw_t ccx_hw_default = {
    &CC1101_SPI,
    
    &CC1101_SPI_PORT,  // spi_port
    CC1101_SCK_PIN,
    CC1101_SI_PIN,
    CC1101_SO_PIN,

    &CC1101_SPI_PORT,  // ss_port
    CC1101_SS_PIN,     // ss_pin

    &CC1101_XSO_PORT,  // in_so_port
    CC1101_XSO_PIN,    // in_so_pin

    &CC1101_GDO0_PORT, // gdo0_port
    CC1101_GDO0_PIN,   // gdo0_pin

    &CC1101_GDO2_PORT, // gdo2_port
    CC1101_GDO2_PIN,   // gdo2_pin

    NULL
};
