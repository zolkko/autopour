
#ifndef RF_H_
#define RF_H_

typedef struct __rf_t {
    uint8_t (*version)          (const struct __rf_t * self);
	uint8_t (*part_number)      (const struct __rf_t * self);
	int8_t  (*receive)          (const struct __rf_t * self, uint8_t * data, uint8_t * data_size, uint8_t * src_addr, uint8_t * dst_addr);
	uint8_t (*can_receive)      (const struct __rf_t * self, portTickType ticks);

	/** write a packet into transceiver buffer */
    int8_t  (*prepare)          (const struct __rf_t * self, const void * payload, uint16_t payload_len);
	int8_t  (*transmit)         (const struct __rf_t * self);
	int8_t  (*send)             (const struct __rf_t * self, const void * payload, uint16_t payload_len);

	int8_t  (*read)             (const struct __rf_t * self, void * buffer, uint16_t buffer_len);
	uint8_t (*channel_clear)    (const struct __rf_t * self);

	uint8_t (*receiving_packet) (const struct __rf_t * self);
	uint8_t (*pending_packet)   (const struct __rf_t * self);

	uint8_t (*rx_on)            (const struct __rf_t * self);
	uint8_t (*rx_off)           (const struct __rf_t * self);

	uint8_t (*sleep)            (const struct __rf_t * self);

    void * priv;
} rf_t;


#define RF_TX_OK         1
#define RF_TX_ERR        0
#define RF_TX_COLLISION -1
#define RF_TX_NOACK     -2
#define RF_TX_TIMEOUT   -3


#define rf_version(X)               X->version(X)

#define rf_part_number(X)           X->part_number(X);

#define RF_TRANSMIT_OK              1
#define RF_TRANSMIT_FAIL            0
#define RF_TRANSMIT_UNDERFLOW      -1
#define RF_TRANSMIT_TIMEOUT        -2
#define rf_transmit(X, D, S, T, F)  X->transmit(X, D, S, T, F)

#define RF_RECEIVE_OK               1
#define RF_RECEIVE_FAIL             0
#define RF_RECEIVE_TIMEOUT         -1
#define rf_receive(X, D, S, T, F)   X->receive(X, D, S, T, F)

#define RF_MAX_TIMEOUT 0xffff
#define rf_can_receive(X, T)        X->can_receive(X, T)


#define rf_prepare(X, P, L)         X->prepare(X, P, L)
#define rf_senf(X, B, L)            X->send(X, B, L)
#define rf_read(X, B, L)            X->read(X, B, L)
#define rf_sleep(X)                 X->sleep(X)


#endif /* RF_H_ */
