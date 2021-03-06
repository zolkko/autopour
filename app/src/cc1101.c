#ifdef __AVR__
#include <util/delay.h>
#endif

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>


#include "ccx_hw.h"
#include "rf.h"
#include "cc1101.h"


#ifdef __AVR__
#include <avr/pgmspace.h>
#define _DELAY_US(x) _delay_us(x)
#define CC1101_REG_LOCATION PROGMEM
#endif


#define CC1101_FIFO_THR                   11
#define CC1101_TX_FIFO_SIZE               20
#define CC1101_TX_FIFO_CHUNK_SIZE         20
#define CC1101_MAX_PACKET_LENGTH          127


typedef struct {
    uint8_t * buff;
    uint8_t   buff_length;

	ccx_hw_t * hw;
	xSemaphoreHandle lock;
} rf_private_t;


static uint8_t cc1101_version(const rf_t * self);

static uint8_t cc1101_part_number(const rf_t * self);

static void cc1101_reset(const ccx_hw_t *);

static void cc1101_configure(const ccx_hw_t *);

static uint8_t cc1101_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data);

static uint8_t cc1101_write(const ccx_hw_t * hw, uint8_t addr, uint8_t data);

static uint8_t cc1101_burst_write(const ccx_hw_t * self, uint8_t addr, const uint8_t * data, uint8_t data_size);

static uint8_t cc1101_burst_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data, uint8_t data_size);

static uint8_t cc1101_strobe(const ccx_hw_t * self, uint8_t addr);

#define cc1101_strobe_flush_tx(X)  cc1101_strobe(X, CCx_SFTX)

#define cc1101_strobe_fast_tx(X)   cc1101_strobe(X, CCx_SFSTXON)

#define cc1101_strobe_flush_rx(X)  cc1101_strobe(X, CCx_SFRX)

#define cc1101_strobe_ide(X)       cc1101_strobe(X, CCx_SIDLE)

#define cc1101_strobe_transmit(X)  cc1101_strobe(X, CCx_STX)

#define cc1101_strobe_calibrate(X) cc1101_strobe(X, CCx_SCAL)

#define cc1101_strobe_receive(X)   cc1101_strobe(X, CCx_SRX)

#define cc1101_strobe_idle(X)      cc1101_strobe(X, CCx_SIDLE)

#define cc1101_strobe_nop(X)       cc1101_strobe(X, CCx_SNOP)

static inline void cc1101_wait_transmission_allowed(const ccx_hw_t * self);

static inline uint8_t cc1101_wait_transmission_finished(const ccx_hw_t * self);

static int8_t cc1101_receive(const rf_t * self, uint8_t * data, uint8_t * data_len, uint8_t * src_addr, uint8_t * dst_addr);

static uint8_t cc1101_can_receive(const rf_t * self, portTickType ticks);

#define DECL_HW(X, V) ccx_hw_t * X = ((rf_private_t *) V->priv)->hw

#define DECL_PRIV(X, V) rf_private_t * X = V->priv

#define DECL_LOCK(X, V) xSemaphoreHandle X = ((rf_private_t *) V->priv)->lock

#define acquire_lock(X) xSemaphoreTake(X, portMAX_DELAY)

#define release_lock(X) xSemaphoreGive(X)


const uint8_t cc1101_cfg[] CC1101_REG_LOCATION = {
	GDOx_CFG_TX_THR_TX_THR_gc,	// CCx_IOCFG2
	GDOx_CFG_HI_Z,				// CCx_IOGDO1 - default 3-state
	GDOx_CFG_SYNC_WORD_gc,	    // CCx_IOCFG0D

	0x0e,						// CCx_FIFOTHR

	0xd3,						// CCx_SYNC1 - 8 MSB 16-bit sync word
	0x91,						// CCX_SYNC0 - 8 LSB 16-bit sync word

	CCx_PACKT_LEN,				// CCx_PKTLEN

	0x04,						// CCx_PKTCTRL1
	0x05,						// CCx_PKTCTRL0

	0x00,						// CCx_ADDR
	0x00,						// CCx_CHANNR

	0x08,						// CCx_FSCTRL1
	0x00,						// CCx_FSCTRL0

	0x23,						// CCx_FREQ2
	0x31,						// CCx_FREQ1
	0x3B,						// CCx_FREQ0

	0x7B,						// CCx_MDMCFG4
	0x83,						// CCx_MDMCFG3
	0x03,						// CCx_MDMCFG2
	0x22,						// CCx_MDMCFG1
	0xF8,						// CCx_MDMCFG0

	0x42,						// CCx_DEVIATN

	0x07,						// CCx_MCSM2  - 0x07 timeout for sync word search (until end of packet)
	0x30,						// CCx_MCSM1  - 0x30

	0x18,						// CCx_MCSM0

	0x1D,						// CCx_FOCCFG
	0x1C,						// CCx_BSCFG

	0xC7,						// CCx_AGCCTRL2
	0x00,						// CCx_AGCCTRL1
	0xB2,						// CCx_AGCCTRL0

	0x87,						// CCx_WOREVT1 - 0x87
	0x6b,						// CCx_WOREVT0 - 0x6b
	0xf8,						// CCx_WORCTRL - 0xf8

	0xB6,						// CCx_FREND1
	0x10,						// CCx_FREND0

	0xEA,						// CCx_FSCAL3
	0x2A,						// CCx_FSCAL2
	0x00,						// CCx_FSCAL1
	0x1F,						// CCx_FSCAL0

	0x41,						// CCx_RCCTRL1 - 0x41
	0x00,						// CCx_RCCTRL0 - 0x00

	0x59,						// CCx_FSTEST

	0x7f,						// CCx_PTEST   - 0x7f
	0x3f,						// CCx_AGCTEST - 0x3f

	0x81,						// CCx_TEST2
	0x35,						// CCx_TEST1
	0x09,						// CCx_TEST0
};


uint8_t cc1101_strobe(const ccx_hw_t * self, uint8_t addr)
{
	return ccx_write(self, addr);
}


uint8_t cc1101_status_reg(const ccx_hw_t * self, uint8_t addr, uint8_t * data)
{
	uint8_t status = ccx_write(self, addr | CC1101_RW_BIT_bm);
	if (data != NULL) {
		*data = ccx_write(self, addr);
	} else {
		ccx_write(self, addr);
	}
	return status;
}


uint8_t cc1101_write(const ccx_hw_t * self, uint8_t addr, uint8_t data)
{
	uint8_t status = ccx_write(self, addr);
	ccx_write(self, data);
	return status;
}


uint8_t cc1101_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data)
{
	uint8_t status = ccx_write(self, addr | CC1101_RW_BIT_bm);
	if (data != NULL) {
		*data = ccx_write(self, 0);
	} else {
		ccx_write(self, 0);
	}
	return status;
}


uint8_t cc1101_burst_write(const ccx_hw_t * self, uint8_t addr, const uint8_t * data, uint8_t data_size)
{
	uint8_t status = ccx_write(self, addr | CC1101_BURST_BIT_bm);
    for (uint8_t i = 0; i < data_size; i++) {
		ccx_write(self, data[i]);
    }
    return status;	
}


uint8_t cc1101_burst_read(const ccx_hw_t * self, uint8_t addr, uint8_t * data, uint8_t data_size)
{
	uint8_t status = ccx_write(self, addr | CC1101_RW_BIT_bm | CC1101_BURST_BIT_bm);
	for (uint8_t i = 0; i < data_size; i++) {
		data[i] = ccx_write(self, 0);
	}
	return status;
}


/**
 * Strobe chip select low/high
 * I use here smallest possible value because cc1101 supports up to 10Mhz clock
 * and about 6.5Mhz strobe for bulk write operation.
 */
void cc1101_reset(const ccx_hw_t * hw)
{
	ccx_chip_select(hw);
    _DELAY_US(1);
	ccx_chip_release(hw);

    _DELAY_US(50);

    while ( ccx_ready(hw) ) ;

	ccx_chip_select(hw);
    while ( ! ccx_ready(hw) );

	ccx_write(hw, CCx_SRES);

    while ( ! ccx_ready(hw) );
	ccx_chip_release(hw);
}


void cc1101_configure(const ccx_hw_t * hw)
{
    uint8_t data[sizeof(cc1101_cfg)];

	memcpy_PF(data, cc1101_cfg, sizeof(cc1101_cfg));

	ccx_chip_select(hw);
	while ( !ccx_ready(hw) ) ;
	
	cc1101_burst_write(hw, CCx_REG_BEGIN, data, sizeof(cc1101_cfg));

	ccx_chip_release(hw);
}


uint8_t cc1101_version(const rf_t * self)
{
    DECL_HW(hw, self);
	DECL_LOCK(lock, self);

	while ( !acquire_lock(lock) ) ;

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	uint8_t version = 0;
	cc1101_read(hw, CCx_VERSION, &version);

	ccx_chip_release(hw);
	
	release_lock(lock);

	return version;
}


uint8_t cc1101_part_number(const rf_t * self)
{
	DECL_HW(hw, self);
	DECL_LOCK(lock, self);
	
	while ( !acquire_lock(lock) ) ;

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	uint8_t part_number;
	cc1101_read(hw, CCx_PARTNUM, &part_number);

	ccx_chip_release(hw);

	release_lock(lock);

	return part_number;
}


uint8_t cc1101_rssi_decode(uint8_t rssi_enc)
{
	unsigned char rssi;
	
	// is actually dataRate dependent, but for simplicity assumed to be fixed.
	unsigned char rssi_offset = 74;

	// RSSI is coded as 2's complement see section 17.3 RSSI of the cc1100 data-sheet
	if (rssi_enc >= 128)
		rssi = (( rssi_enc - 256) >> 1) - rssi_offset;
	else
		rssi = (rssi_enc >> 1) - rssi_offset;
	
	return rssi;
}


/**
 * As described in Errata http://www.ti.com/lit/er/swrz020d/swrz020d.pdf during transmission
 * it is important to read status byte twice and only once both statuses are equals it value
 * is considered valid.
 */
void cc1101_wait_transmission_allowed(const ccx_hw_t * self)
{
	uint8_t state1;
	uint8_t state2;

	do {
		cc1101_read(self, CCx_MARCSTATE, &state1);
		state1 &= CC1101_MARC_bm;

		cc1101_read(self, CCx_MARCSTATE, &state2);
		state2 &= CC1101_MARC_bm;

	} while (state1 != state2 &&
             CC1101_MARC_IDLE_gc != state1 &&
			 CC1101_MARC_RX_gc != state1 &&
			 CC1101_MARC_RX_END_gc != state1 &&
			 CC1101_MARC_RX_RST_gc != state1);
}


/**
 * Method assumes that TXOFF moved state-machine into IDLE state after the
 * transfer operation complete.
 */
uint8_t cc1101_wait_transmission_finished(const ccx_hw_t * self)
{
	uint8_t state1 = 0;
	uint8_t state2 = 0;

	do {
		cc1101_read(self, CCx_MARCSTATE, &state1);
		state1 &= CC1101_MARC_bm;

		cc1101_read(self, CCx_MARCSTATE, &state2);
		state2 &= CC1101_MARC_bm;

		if (state1 != state2) {
			continue;
		}

		switch (state1 & CC1101_MARC_bm) {
			case CC1101_MARC_TXFIFO_UNDERFLOW_gc:
				cc1101_strobe_flush_tx(self);
				return RF_TRANSMIT_UNDERFLOW;

			case CC1101_MARC_IDLE_gc:
				return RF_TRANSMIT_OK;

			case CC1101_MARC_RX_END_gc:
			case CC1101_MARC_RX_RST_gc:
			case CC1101_MARC_RX_gc:
				// TX-if-CCA
				cc1101_strobe_transmit(self);
				continue;

			default:
				// the rest of states are considered as transitional
				continue;
		}
	} while (true);
}


// TODO: In order to make the interface more generic I need to remove RSSI and LQI parameters
// receive data via RF, rxData must be at least CCx_PACKT_LEN bytes long
int8_t cc1101_receive(const rf_t * self, uint8_t * data, uint8_t * data_len, uint8_t * src_addr, uint8_t * dst_addr)
{
	DECL_HW(hw, self);
	DECL_LOCK(lock, self);
	
	if ( !acquire_lock(lock) ) {
		return RF_RECEIVE_TIMEOUT;
	}

	ccx_chip_select(hw);
	ccx_wait_ready(hw);

	cc1101_read(hw, CCx_RXFIFO, data_len);
	if (*data_len == 0)
	{
		return RF_RECEIVE_FAIL;
	}

	cc1101_read(hw, CCx_RXFIFO, dst_addr);
	cc1101_read(hw, CCx_RXFIFO, src_addr);
	*data_len -= 2;

	cc1101_burst_read(hw, CCx_RXFIFO, data, *data_len);
	ccx_chip_release(hw);

	// TODO: wait for reception finished

	release_lock(lock);

	return RF_RECEIVE_OK;
}


uint8_t cc1101_can_receive(const rf_t * self, portTickType timeout)
{
    DECL_HW(hw, self);
    DECL_LOCK(lock, self);

    if (!acquire_lock(lock)) {
        return 0;
    }

    ccx_chip_select(hw);
    ccx_wait_ready(hw);

    // TODO: test RX buffer before sleep because it already may has some data
    // TODO: calibrate

    cc1101_strobe_receive(hw);

	if (ccx_wait_gdo0(hw, timeout) && ccx_gdo0(hw))
    {
		uint8_t in_bytes1 = 0;
		uint8_t in_bytes2 = 0;

		do {
			cc1101_read(hw, CCx_RXBYTES, &in_bytes1);
			cc1101_read(hw, CCx_RXBYTES, &in_bytes2);

			if (in_bytes1 == in_bytes2) {
				break;
			}
		} while (true) ;

		cc1101_strobe_idle(hw);

		uint8_t state1 = 0;
		uint8_t state2 = 0;
		do {
			cc1101_read(hw, CCx_MARCSTATE, &state1);
			state1 &= CC1101_MARC_bm;

			cc1101_read(hw, CCx_MARCSTATE, &state2);
			state2 &= CC1101_MARC_bm;
		} while (state1 != state2 || state1 != CC1101_MARC_IDLE_gc);
    
		ccx_chip_release(hw);
		release_lock(lock);

		return in_bytes1;
	} else {
		ccx_chip_release(hw);
		release_lock(lock);

		return 0;
	}
}


int8_t cc1101_prepare(const rf_t * self, const void * payload, uint8_t payload_len)
{
    if (payload_len > CC1101_MAX_PACKET_LENGTH) {
        return RF_TX_TOO_LONG;
    }
    
    DECL_LOCK(lock, self);

	if (!acquire_lock(lock)) {
		return RF_TX_TIMEOUT;
	}

    DECL_HW(hw, self);
    DECL_PRIV(priv, self);

    ccx_chip_select(hw);
	cc1101_strobe_flush_tx(hw);
    
    // Set fixed packet length to the tail of the packet length to be transmitted
    cc1101_write(hw, CCx_PKTLEN, payload_len);
    cc1101_write(hw, CCx_PKTCTRL0, CCx_PKTCTRL0_CRC_EN_bm | CCx_PKTCTRL0_LENGTH_FIXED_bm);

    // Compute and populate data to be transmitted during first TX call
    uint8_t bytes_to_send = payload_len > CC1101_TX_FIFO_SIZE ? CC1101_TX_FIFO_SIZE : payload_len;
    priv->buff = payload + bytes_to_send;
    priv->buff_length = payload_len - bytes_to_send;

    cc1101_burst_write(hw, CCx_TXFIFO, (const uint8_t *) payload, bytes_to_send);
    ccx_chip_release(hw);

	release_lock(lock);

    return RF_TX_OK;
}


/**
 * This module knows nothing about a packet structure
 */
int8_t cc1101_transmit(const rf_t * self)
{
    DECL_LOCK(lock, self);

    if ( !acquire_lock(lock) ) {
        return RF_TX_TIMEOUT;
    }

    DECL_HW(hw, self);
    DECL_PRIV(priv, self);

    ccx_chip_select(hw);
    ccx_wait_ready(hw);

    cc1101_wait_transmission_allowed(hw);
    
    // Associated to the TX FIFO: asserts when TX FIFO is filled above TXFIFO_THR
    // De-asserts when TX FIFO is drained below TX_FIFOR_THR
    cc1101_write(hw, CCx_IOCFG2, GDOx_CFG_TX_THR_TX_THR_gc | GDOx_CFG_INV_bm);

    // Asserts when sync word transceived/received de-asserts when
    // packet has been transmitted plus inversion
    cc1101_write(hw, CCx_IOCFG0, GDOx_CFG_SYNC_WORD_gc | GDOx_CFG_INV_bm);
    
    uint8_t current_size = 0;
    cc1101_read(hw, CCx_TXBYTES, &current_size);

    // ccx_enable_gdo2(hw);
    cc1101_strobe_transmit(hw);
    ccx_chip_release(hw);
    
    int chunk_counter = 0;

    ccx_chip_select(hw);
    ccx_wait_ready(hw);
    while (priv->buff_length > 0) {
        if (ccx_gdo2(hw)/* || ccx_wait_gdo2(hw, 0xffff)*/) {
            uint8_t * buff_ref = priv->buff;
            uint8_t * buff_len = priv->buff_length;

            // try to prevent underflow condition
            do {
                cc1101_write(hw, CCx_TXFIFO, *buff_ref);
                buff_ref++;
                buff_len--;
            }
            while (buff_len > 0 && ccx_gdo2(hw));

            uint8_t is_underflow = 0;
            uint8_t test_underflow = 0;

            cc1101_read(hw, CCx_TXBYTES, &test_underflow);
            if (test_underflow & 0x80) {
                is_underflow = 1;
            }

            if (buff_len > 0) {
                uint8_t chunk_size = buff_len > CC1101_TX_FIFO_CHUNK_SIZE ? CC1101_TX_FIFO_CHUNK_SIZE : buff_len;
                cc1101_burst_write(hw, CCx_TXFIFO, (const uint8_t *) buff_ref, chunk_size);
                ccx_chip_release(hw);

                buff_ref += chunk_size;
                buff_len -= chunk_size;

                ccx_chip_select(hw);
                ccx_wait_ready(hw);
            }

            cc1101_read(hw, CCx_TXBYTES, &test_underflow);
            if (test_underflow & 0x80) {
                is_underflow = 1;
            }

            priv->buff = buff_ref;
            priv->buff_length = buff_len;
            chunk_counter++;
        }
    }
    ccx_chip_release(hw);
    ccx_disable_gdo2(hw);
    
    // TODO: test for underflow or timeout (need to flush TX buffer in this case)

    // ccx_chip_select(hw);
    // ccx_wait_ready(hw);
    // cc1101_wait_transmission_finished(hw);
    // ccx_chip_release(hw);
    
    release_lock(lock);

    return RF_TX_OK;
}


int8_t cc1101_send(const rf_t * self, const void * payload, uint8_t payload_len)
{
	int8_t result = cc1101_prepare(self, payload, payload_len);
	if (RF_TX_OK != result) {
		return result;
	}

	return cc1101_transmit(self);
}


void cc1101_init(rf_t * rf, ccx_hw_t * hw)
{
    rf->version = &cc1101_version;
	rf->part_number = &cc1101_part_number;
    rf->prepare = &cc1101_prepare;
	rf->transmit = &cc1101_transmit;
    rf->send = &cc1101_send;
	rf->receive = &cc1101_receive;
	rf->can_receive = &cc1101_can_receive;

	rf_private_t * priv = (rf_private_t *) pvPortMalloc(sizeof(rf_private_t));
	priv->hw = hw;
	priv->lock = xSemaphoreCreateMutex();

    rf->priv = priv;

	cc1101_reset(hw);
	cc1101_configure(hw);
}
