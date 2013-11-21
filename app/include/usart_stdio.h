#ifndef usart_stdio_h
#define usart_stdio_h

#define USART_SERIAL                     USARTC0
#define USART_SERIAL_BAUDRATE            9600
#define USART_SERIAL_CHAR_LENGTH         USART_CHSIZE_8BIT_gc
#define USART_SERIAL_PARITY              USART_PMODE_DISABLED_gc
#define USART_SERIAL_STOP_BIT            1


#ifdef __cplusplus
extern "C" {
#endif

/*
 * Initialize USART module
 */
extern void usart_init();

#ifdef __cplusplus
}
#endif

#endif /* usart_stdio_h */
