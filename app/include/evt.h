
#ifndef EVT_H_
#define EVT_H_

typedef uint8_t evt_type;

typedef uint8_t evt_timeout;

struct EvtQueueItem
{
	evt_type type;
	struct EvtQueueItem * prev;
	struct EvtQueueItem * next;
};

struct EvtQueue
{
	struct EvtQueueItem * first;
	struct EvtQueueItem * last;
	evt_timeout timeout;
};


struct EvtQueue * evt_new(evt_timeout timeout);

struct EvtQueueItem * evt_enqueue(struct EvtQueue * queue, evt_type type);

struct EvtQueueItem * evt_dequeue(struct EvtQueue * queue);

void evt_free_item(struct EvtQueueItem * item);

void evt_free(struct EvtQueue * queue);

#endif
