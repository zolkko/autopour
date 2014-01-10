
#ifndef RF_H_
#define RF_H_

typedef struct __rf_t {    
    uint8_t (*version)     (const struct __rf_t * self);
	uint8_t (*part_number) (const struct __rf_t * self);
	int8_t  (*transmit)    (const struct __rf_t * self, const uint8_t * data, uint8_t data_size, uint8_t src_addr, uint8_t dst_addr);
	
    void * priv;
} rf_t;


#define rf_version(X)               X->version(X)

#define rf_part_number(X)           X->part_number(X);


#define RF_TRANSMIT_OK         1
#define RF_TRANSMIT_FAIL       0
#define RF_TRANSMIT_UNDERFLOW -1
#define rf_transmit(X, D, S, T, F)  X->transmit(X, D, S, T, F)


#endif /* RF_H_ */