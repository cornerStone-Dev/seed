#include "../localTypes.h"

typedef struct {
	void	*arg1;
	void	*arg2;
	void 	(*function)(void *arg1, void *arg2);
} Task;

#define NUM_TASKS  85
typedef struct {
	u16		r;
	u16		w;
	Task	tasks[NUM_TASKS];
} TaskQueue;

TaskQueue q;

/*e*/void
task_enqueue(void *function, void *arg1, void *arg2)/*p;*/
{
	asm("CPSID i");  // disable interrupts
	q.tasks[q.w].function	= function;
	q.tasks[q.w].arg1	= arg1;
	q.tasks[q.w].arg2	= arg2;
	q.w += 1;
	if (q.w == NUM_TASKS) { q.w = 0; }
	asm("CPSIE i"); // enable interrupts
}

/*e*/void
task_exec(void)/*p;*/
{
	u32 read = q.r;
	u32 write = q.w;
	while (read != write)
	{
		void (*function)(void *arg1,void *arg2)=q.tasks[read].function;
		void *arg1 = q.tasks[read].arg1;
		void *arg2 = q.tasks[read].arg2;
		read += 1;
		if (read == NUM_TASKS) { read = 0; }
		q.r = read;
		//~ task_enter(arg1, arg2, function);
		function(arg1, arg2);
	}
}

#if 0

typedef struct StackLayout {
	u32 R4;
	u32 R5;
	u32 R6;
	u32 R7;
	u32 LR;
	u32 R0;
	u32 R1;
	u32 R2;
	u32 R3;
	u32 R12;
	u32 LR_processDoesNotReturn;
	u32 PC;
	u32 PSR;
} StackLayout;

/*e*/
void
os_createProcess(void *entryPoint, void *arg1, void *arg2)/*p;*/
{
	// get memory for the stack
	u32 *memory = allocStack();
	// use part of stack as process information
	ProcessInfo *newProcess = (ProcessInfo *)memory;
	// stack grows down, therefore the stack starts at the highest address
	StackLayout *stackPointer = (void*)&memory[512/4 - 13];
	// stack grows downward
	stackPointer->PSR = (1<<24); // PSR thumb bit
	stackPointer->PC = (u32)os_processWrapper; // pointer to function to resume
	// stackPointer[10]  LR which has no meaning in this context
	// stackPointer[9]   R12 which has no meaning in this context
	// stackPointer->R3  R3 unused argument
	stackPointer->R2 = (u32)entryPoint; // R2 function to call by wrapper
	stackPointer->R1 = (u32)arg2; // R1
	stackPointer->R0 = (u32)arg1; // R0
	stackPointer->LR = 0xFFFFFFF9; // LR indicate to return from interrupt
	// stackPointer[3]   R7 which has no meaning in this context
	// stackPointer[2]   R6 which has no meaning in this context
	// stackPointer[1]   R5 which has no meaning in this context
	// stackPointer[0]   R4 which has no meaning in this context
	newProcess->stack = (void*)stackPointer;
	os_processEnqueue(newProcess, 0);
	return;
}

#endif
