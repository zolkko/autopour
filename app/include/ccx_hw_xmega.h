
#ifndef CCX_HW_XMEGA_H_
#define CCX_HW_XMEGA_H_


typedef struct ccx_xmega_hw {
    SPI_t * spi;
    
    PORT_t * ss_port;
    uint8_t ss_pin;
    
    PORT_t * so_port;
    uint8_t so_pin;
    
    PORT_t * gdo0_port;
    uint8_t gdo0_pin;
    
    PORT_t * gdo2_port;
    uint8_t gdo2_pin;
    
    void * priv;
} ccx_xmega_hw_t;


ccx_hw_t * ccx_hw_xmega_init(ccx_hw_t *, ccx_xmega_hw_t *);


#endif /* CCX_HW_XMEGA_H_ */
