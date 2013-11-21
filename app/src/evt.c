#include <stddef.h>
#include <util/atomic.h>
#include "evt.h"
/*
struct EvtQueue * evt_new(evt_timeout timeout)
{
    struct EvtQueue * queue = (struct EvtQueue *) malloc(sizeof(struct EvtQueue));
    if (queue) {
        queue->timeout = timeout;
    }
    return queue;
}

struct EvtQueueItem * evt_enqueue(struct EvtQueue * queue, evt_type type)
{
    struct EvtQueueItem * item = (struct EvtQueueItem *) malloc(sizeof(struct EvtQueueItem));
    if (item != NULL) {
        item->type = type;
        item->next = NULL;
		
		ATOMIC_BLOCK(ATOMIC_FORCEON) {
			if (queue->first == NULL) {
				queue->first = item;
				queue->last = item;
			} else {
				queue->last->next = item;
				queue->last = item;
			}
		}
    }
    return item;
}

struct EvtQueueItem * evt_dequeue(struct EvtQueue * queue)
{
	struct EvtQueueItem * item = NULL;
	
	ATOMIC_BLOCK(ATOMIC_FORCEON) {
		item = queue->first;
		queue->first = item->next;
		if (queue->first == NULL) {
			queue->last = NULL;
		}
	}
	
    item->next = NULL;
    return item;
}

void evt_free_item(struct EvtQueueItem * item)
{
    free(item);
}

void evt_free(struct EvtQueue * queue)
{
    struct EvtQueueItem * item = NULL;

    ATOMIC_BLOCK(ATOMIC_FORCEON) {
		if (queue != NULL){
			item = queue->first;
			queue->first = NULL;
			queue->last = NULL;
			free(queue);
		}
	}	
    // lock end

    while (item != NULL) {
        struct EvtQueueItem * tmp = item->next;
        evt_free_item(item);
        item = tmp;
    }
}
*/
