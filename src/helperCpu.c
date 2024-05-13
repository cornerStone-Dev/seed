// helperCpu.c
#include "../localTypes.h"

#if 0

typedef struct ProcessorFifo {
	volatile u32 status;
	volatile u32 write;
	volatile u32 read;
} ProcessorFifo;

#define NUM_OF_EVENTS (1<<3)
static Event events[NUM_OF_EVENTS];
static u32   eventIndex;

/*e*/
void
helper_init(void) __attribute__((noreturn));/*p*/

void
helper_init(void)
{
	{
		// clear error flags in FIFO
		ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
		fifo->status = 0x0C;
		// disable all interrupts
		u32 *intDisable = (void*)PPB_INTERRUPT_DISABLE;
		*intDisable = 0xFFFFFFFF;
		// clear all pending interrupts
		u32 *intPend = (void*)PPB_INTERRUPT_CLEAR_PEND;
		*intPend = 0xFFFFFFFF;
		// initialize fifoPtrs
		//~ fifoPtrs->writer = (void*)FIFO_BUFFER;
		//~ fifoPtrs->reader = FIFO_BUFFER;
		// enable SIO interrupt and tasking interrupts
		u32 *intEnable = (void*)PPB_INTERRUPT_ENABLE;
		*intEnable = (1<<26)|(1<<27)|(1<<28); // (1<<16)|
		// set interrupt priorities
		u32 *intPriority = (void*)PPB_NVIC_IPR6;
		*intPriority++ = (1<<22)|(2<<30);
		*intPriority = (3<<6);
		// disable deep sleep and disable "wake up on every possible event"
		u32 *sysCtrl = (void*)PPB_SYS_CNTRL;
		*sysCtrl = 0;
		// set sysTick and PendSv to lowest priority
		intPriority = (void*)PPB_SYS_SHPR2;
		*intPriority = (3<<30)|(3<<22);
		intPriority--; *intPriority = (3<<30); // set svc to lowest too
		core1ServerSetup();
	}
	// never return
	while(1){
		asm("wfe");
	}
}

/*e*/
void
helper_sendEvent(void *function, void *data)/*p;*/
{
	ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
	asm("CPSID i"); // disable interrupts
	u32 index = eventIndex;
	// reserve current index
	eventIndex = (index+1) & (NUM_OF_EVENTS-1);
	// write out event data
	events[index].function = function;
	events[index].data     = data;
	// send pointer to event data
	fifo->write = (u32)&events[index];
	asm("CPSIE i"); // enable interrupts
}

/*e*/
void
helper_sendMsg1(u32 data1)/*p;*/
{
	ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
	fifo->write = data1;
}

/*e*/
void
helper_sendMsg2(u32 data1, u32 data2)/*p;*/
{
	ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
	fifo->write = data1;
	fifo->write = data2;
}

/*e*/
void
helper_sendMsg3(u32 data1, u32 data2, u32 data3)/*p;*/
{
	ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
	fifo->write = data1;
	fifo->write = data2;
	fifo->write = data3;
}

/*e*/
void
helper_sendMsg4(u32 data1, u32 data2, u32 data3, u32 data4)/*p;*/
{
	ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
	fifo->write = data1;
	fifo->write = data2;
	fifo->write = data3;
	fifo->write = data4;
}

/*e*/
void
helper_unlock(void)/*p;*/
{
	ProcessorFifo *fifo = (void*)SIO_FIFO_ST;
	// drain input FIFO
	while (fifo->status&1)
	{
		(void)fifo->read;
	}
	// send a zero
	fifo->write = 0;
	// send an event
	asm("sev");
	// read fifo
	while ((fifo->status&1) == 0) { }
	(void)fifo->read;
	//==========================================================================
	// send a 1
	fifo->write = 1;
	// send an event
	asm("sev");
	// read fifo
	while ((fifo->status&1) == 0) { }
	(void)fifo->read;
	//==========================================================================
	// send a vector table
	fifo->write = (u32)vector_table;
	// send an event
	asm("sev");
	// read fifo
	while ((fifo->status&1) == 0) { }
	(void)fifo->read;
	//==========================================================================
	// send a stack pointer
	fifo->write = SECOND_STACK;
	// send an event
	asm("sev");
	// read fifo
	while ((fifo->status&1) == 0) { }
	(void)fifo->read;
	//==========================================================================
	// send an entry point
	fifo->write = (u32)helper_init;
	// send an event
	asm("sev");
	// read fifo
	while ((fifo->status&1) == 0) { }
	(void)fifo->read;
}


#endif

