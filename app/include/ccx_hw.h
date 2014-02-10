
#ifndef CCX_HW_H_
#define CCX_HW_H_


typedef void (*ccx_isr_proc_t) (void * data);


typedef struct ccx_hw {
    void (*chip_select)  (const struct ccx_hw * self);
    uint8_t (*write)     (const struct ccx_hw * self, uint8_t data);
    bool (*ready)        (const struct ccx_hw * self);
	
	void (*wait_ready)   (const struct ccx_hw * self);
    
	bool (*gdo0)         (const struct ccx_hw * self);
	void (*enable_gdo0)  (const struct ccx_hw * self);
	void (*disable_gdo0) (const struct ccx_hw * self);
	bool (*wait_gdo0)    (const struct ccx_hw * self, portTickType timeout);
    void (*set_handler_gdo0) (const struct ccx_hw * self, ccx_isr_proc_t  handler, void * data);
    void (*clear_handler_gdo0) (const struct ccx_hw * self);
	
	bool (*gdo1)         (const struct ccx_hw * self);
	void (*enable_gdo1)  (const struct ccx_hw * self);
	void (*disable_gdo1) (const struct ccx_hw * self);
	bool (*wait_gdo1)    (const struct ccx_hw * self, portTickType timeout);
    void (*set_handler_gdo1) (const struct ccx_hw * self, ccx_isr_proc_t  handler, void * data);
    void (*clear_handler_gdo1) (const struct ccx_hw * self);
	
    bool (*gdo2)         (const struct ccx_hw * self);
	void (*enable_gdo2)  (const struct ccx_hw * self);
	void (*disable_gdo2) (const struct ccx_hw * self);
	bool (*wait_gdo2)    (const struct ccx_hw * self, portTickType timeout);
    void (*set_handler_gdo2) (const struct ccx_hw * self, ccx_isr_proc_t  handler, void * data);
    void (*clear_handler_gdo2) (const struct ccx_hw * self);
	
    void (*chip_release) (const struct ccx_hw * self);

    void * priv;
} ccx_hw_t;


#define ccx_chip_select(X)  X->chip_select(X)

#define ccx_write(X, A)     X->write(X, A)

#define ccx_ready(X)        X->ready(X)

#define ccx_wait_ready(X)   X->wait_ready(X)


#define ccx_gdo0(X)                   X->gdo0(X)
#define ccx_enable_gdo0(X)            X->enable_gdo0(X)
#define ccx_disable_gdo0(X)           X->disable_gdo0(X)
#define ccx_wait_gdo0(X, T)           X->wait_gdo0(X, T)
#define ccx_set_handler_gdo0(X, H, D) X->set_handler_gdo0(X, H, D)
#define ccx_clear_handler_gdo0(X)     X->clear_handler_gdo0(X)


#define ccx_gdo1(X)                   X->gdo1(X)
#define ccx_enable_gdo1(X)            X->enable_gdo1(X)
#define ccx_disable_gdo1(X)           X->disable_gdo1(X)
#define ccx_wait_gdo1(X, T)           X->wait_gdo1(X, T)
#define ccx_set_handler_gdo1(X, H, D) X->set_handler_gdo1(X, H, D)
#define ccx_clear_handler_gdo1(X)     X->clear_handler_gdo1(X)


#define ccx_gdo2(X)                   X->gdo2(X)
#define ccx_enable_gdo2(X)            X->enable_gdo2(X)
#define ccx_disable_gdo2(X)           X->disable_gdo2(X)
#define ccx_wait_gdo2(X, T)           X->wait_gdo2(X, T)
#define ccx_set_handler_gdo2(X, H, D) X->set_handler_gdo2(X, H, D)
#define ccx_clear_handler_gdo2(X)     X->clear_handler_gdo2(X)


#define ccx_chip_release(X) X->chip_release(X)

#define ccx_init_cleanup(X) X->init_cleanup(X)


#endif /* CCX_HW_H_ */
