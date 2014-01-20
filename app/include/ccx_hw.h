
#ifndef CCX_HW_H_
#define CCX_HW_H_


typedef struct ccx_hw {
    void (*chip_select)  (const struct ccx_hw * self);
    uint8_t (*write)     (const struct ccx_hw * self, uint8_t data);
    bool (*ready)        (const struct ccx_hw * self);
	void (*wait_ready)   (const struct ccx_hw * self);
    bool (*gdo0)         (const struct ccx_hw * self);
	bool (*wait_gdo0)    (const struct ccx_hw * self, portTickType timeout);
	bool (*gdo1)         (const struct ccx_hw * self);
	bool (*wait_gdo1)    (const struct ccx_hw * self, portTickType timeout);
    bool (*gdo2)         (const struct ccx_hw * self);
	bool (*wait_gdo2)    (const struct ccx_hw * self, portTickType timeout);
    void (*chip_release) (const struct ccx_hw * self);
    void * priv;
} ccx_hw_t;


#define ccx_chip_select(X)  X->chip_select(X)

#define ccx_write(X, A)     X->write(X, A)

#define ccx_ready(X)        X->ready(X)

#define ccx_wait_ready(X)   X->wait_ready(X)

#define ccx_gdo0(X)         X->gdo0(X)

#define ccx_wait_gdo0(X, T) X->wait_gdo0(X, T)

#define ccx_gdo1(X)         X->gdo1(X)

#define ccx_wait_gdo1(X, T) X->wait_gdo1(X, T)

#define ccx_gdo2(X)         X->gdo2(X)

#define ccx_wait_gdo2(X, T) X->wait_gdo2(X, T)

#define ccx_chip_release(X) X->chip_release(X)


#endif /* CCX_HW_H_ */
