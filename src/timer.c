// timer.c
#include "../localTypes.h"


//~ typedef struct TimerMemMap {
	//~ volatile u32 timeWriteHigh;
	//~ volatile u32 timeWriteLow;
	//~ volatile u32 timeReadHigh;
	//~ volatile u32 timeReadLow;
	//~ volatile u32 alarm[4];
	//~ volatile u32 armed;
	//~ volatile u32 timeReadHighRaw;
	//~ volatile u32 timeReadLowRaw;
	//~ volatile u32 debugPause;
	//~ volatile u32 pause;
	//~ volatile u32 intr;
	//~ volatile u32 inte;
	//~ volatile u32 intf;
	//~ volatile u32 ints;
//~ } TimerMemMap;

typedef struct TimerData {
	u32 *nextProcess;
} TimerData;

typedef struct TimersInfo {
	u32       usageBitMap;
	TimerData alarm[4];
} TimersInfo;

//~ static TimersInfo timerInfo;

/*e*/void
enableWatchDogTick(void)/*p;*/
{
	u32 *watchDogTickReg = (void*)0x4005802C;
	*watchDogTickReg = 12|(1<<9);
}

/*e*/void
timerInit(void)/*p;*/
{
	TimerMemMap *timer = (void*)TIMER_BASE;
	timer->intr = 0xFF;
	timer->inte = (1<<0)|(1<<1)|(1<<2)|(1<<3);
}

/*e*/
s32 timer_set(u32 selectedTimer, u32 micoseconds)/*p;*/
{
	if (micoseconds < 10) { micoseconds = 10; }
	u32 retVal = 0;
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentTime = timer->timeReadLowRaw;
	u32 targetTime = currentTime + micoseconds;
	timer->alarm[selectedTimer] = targetTime;
	return retVal;
}

static u32 periodCount;
static u32 alarmClockValue;

/*e*/u32
setAlarmClockValue(u32 increment)/*p;*/
{
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentTime = timer->timeReadLowRaw;
	alarmClockValue = currentTime + increment;
	return alarmClockValue;
}

/*e*/
void alarm1ISR_HK(void)/*p;*/
{
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentAlarm = timer->ints;
	timer->intr = currentAlarm;
	alarmClockValue += 2500;
	timer->alarm[0] = alarmClockValue;
}

/*e*/
void alarm1ISR(void)/*p;*/
{
	// this represents the top level task and fastest running task
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentAlarm = timer->ints;
	timer->intr = currentAlarm;
	u32 nextTime = timer->timeReadLowRaw + 250;
	timer->alarm[0] = nextTime;
	//~ io_printin(endSysTimer());
	
	
	//~ u32 currentTime = timer->timeReadLowRaw;
	//~ u32 targetTime = alarmClockValue;
	//~ if (currentTime != targetTime) { io_printsn("RMS broken!"); }
	
	// 500ms tasking
	if ( (periodCount & 0x001) == 0)
	{
		u32 *intSet = (void*)PPB_INTERRUPT_SET_PEND;
		u32 intsToSet = 0;
		intsToSet |= (1<<26); 
		// 1ms tasking
		if ( (periodCount & 0x003) == 0)
		{
			intsToSet |= (1<<27);
			// 2ms tasking
			if ( (periodCount & 0x007) == 0)
			{
				intsToSet |= (1<<28);
				if ( (periodCount & 0xFFF) == 0) { io_ledToggle(); }
			}
		}
		*intSet = intsToSet;
	}
	// 2560ms period 0x3FF (0x007 was last I could see with eye, 20ms on/off)
	
	
	//~ startSysTimer();
	//~ io_printin(endSysTimer());
	
	
	// increment period count
	periodCount++;
}

/*e*/
void alarm2ISR(void)/*p;*/
{
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentAlarm = timer->ints;
	timer->intr = currentAlarm;
}

/*e*/
void alarm3ISR(void)/*p;*/
{
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentAlarm = timer->ints;
	timer->intr = currentAlarm;
}

/*e*/
void alarm4ISR(void)/*p;*/
{
	TimerMemMap *timer = (void*)TIMER_BASE;
	u32 currentAlarm = timer->ints;
	timer->intr = currentAlarm;
}

/*e*/
void SDI_1(void)/*p;*/
{
	u32 *intClear = (void*)PPB_INTERRUPT_CLEAR_PEND;
	*intClear = (1<<26);
}

/*e*/
void SDI_2(void)/*p;*/
{
	u32 *intClear = (void*)PPB_INTERRUPT_CLEAR_PEND;
	*intClear = (1<<27);
}

/*e*/
void SDI_3(void)/*p;*/
{
	u32 *intClear = (void*)PPB_INTERRUPT_CLEAR_PEND;
	*intClear = (1<<28);
	uart0processOutputs();
	uart0processInputs();
}

/*e*/
void timer_sleepMs(s32 ms)/*p;*/
{
	ms = ms * 2;
	while(ms > 0)
	{
		asm("wfi");
		ms -= 1;
	}
}
