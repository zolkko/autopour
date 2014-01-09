
#ifndef RF_H_
#define RF_H_

typedef struct __rf_t {    
    uint8_t (*version) (const struct __rf_t);
    void * priv;
} rf_t;


#define rf_version(X)     X->version(X)


#endif /* RF_H_ */
