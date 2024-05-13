#include "../localTypes.h"

#if 0

typedef struct Chunk {
	struct Chunk	*next;
	u32				size;
	u32				data[1];
} Chunk;

typedef struct {
	u8          *heapCursor;
	Chunk       *chunks;
} Heap;

static Heap h = {
	(u8*)HEAP_TOP_ADDR,
	0
};

static void *zalloc_internal(u32 nByte)
{
	asm("CPSID i"); // disable interrupts
	Chunk **cursor = &h.chunks;
	Chunk *target = 0;
	while (*cursor)
	{
		if ((*cursor)->size == nByte)
		{
			target = *cursor;
			*cursor = (*cursor)->next;
			break;
		}
		cursor = &(*cursor)->next;
	}
	if (target == 0)
	{
		h.heapCursor -= nByte;
		target = (void*)h.heapCursor;
	}
	asm("CPSIE i"); // enable interrupts

	target->size = nByte;
	u32 *mem = target->data;
	u32 *end = (u32*)((u32)mem + nByte - 8);
	do {
		mem[0] = 0;
		mem[1] = 0;
		mem += 2;
	} while (mem < end);
	return target->data;
}


/*
** Free an outstanding memory allocation.
*/
static void free_internal(void *pOld)
{
	u8 *raw = pOld;
	Chunk *target = (void*)(raw-8);
	target->next = h.chunks;
	h.chunks = target;
}


void free(void *pOld)
{
	if (pOld==0) { return; }
	asm("CPSID i");  // disable interrupts
	//~ os_takeSpinLock(LOCK_MEMORY_ALLOC);
	free_internal(pOld);
	//~ os_giveSpinLock(LOCK_MEMORY_ALLOC);
	asm("CPSIE i"); // enable interrupts
}


void *zalloc(u32 nByte)
{
	// if nByte is 0 -> return 0 to be consistent with realloc
	if (nByte==0) { return 0; }
	//~ asm("CPSID i"); // disable interrupts
	//~ os_takeSpinLock(LOCK_MEMORY_ALLOC);
	void *memory = zalloc_internal((nByte + 15) & -8);
	//~ os_giveSpinLock(LOCK_MEMORY_ALLOC);
	//~ asm("CPSIE i"); // enable interrupts
	return memory;
}

#endif


// Section typesafe
typedef struct Mem5Link Mem5Link;
struct Mem5Link {
  int next;       /* Index of next free chunk */
  int prev;       /* Index of previous free chunk */
};

/*
** Maximum size of any allocation is ((1<<LOGMAX)*ATOM_SIZE). Since
** ATOM_SIZE is always at least 8 and 32-bit integers are used,
** it is not actually possible to reach this limit.
*/
#define LOGMAX 13
#define ATOM_SIZE 32

/*
** Masks used for mem5.aCtrl[] elements.
*/
#define CTRL_LOGSIZE  0x1f    /* Log2 Size of this block */
#define CTRL_FREE     0x20    /* True if not checked out */

/*
** All of the static variables used by this module are collected
** into a single structure named "mem5".  This is to keep the
** static variables organized and to reduce namespace pollution
** when this module is combined with other in the amalgamation.
*/
static struct Mem5Global {
	/*
	** Memory available for allocation
	*/
	int nBlock;      /* Number of szAtom sized blocks in zPool */
	u8 *zPool;       /* Memory available to be allocated */

	/*
	** Lists of free blocks.  aiFreelist[0] is a list of free blocks of
	** size ATOM_SIZE.  aiFreelist[1] holds blocks of size szAtom*2.
	** aiFreelist[2] holds free blocks of size szAtom*4.  And so forth.
	*/
	int aiFreelist[LOGMAX+1];

	/*
	** Space for tracking which blocks are checked out and the size
	** of each block.  One byte per block.
	*/
	u8 *aCtrl;
} mem5;

/*
** Assuming mem5.zPool is divided up into an array of Mem5Link
** structures, return a pointer to the idx-th such link.
*/
#define MEM5LINK(idx) ((Mem5Link *)(&mem5.zPool[(idx)*ATOM_SIZE]))

/*
** Unlink the chunk at mem5.aPool[i] from list it is currently
** on.  It should be found on mem5.aiFreelist[iLogsize].
*/
static void memsys5Unlink(int i, int iLogsize){
	int next, prev;

	next = MEM5LINK(i)->next;
	prev = MEM5LINK(i)->prev;
	if( prev<0 ){
		mem5.aiFreelist[iLogsize] = next;
	}else{
		MEM5LINK(prev)->next = next;
	}
	if( next>=0 ){
		MEM5LINK(next)->prev = prev;
	}
}

/*
** Link the chunk at mem5.aPool[i] so that is on the iLogsize
** free list.
*/
static void memsys5Link(int i, int iLogsize){
	int x;
	x = MEM5LINK(i)->next = mem5.aiFreelist[iLogsize];
	MEM5LINK(i)->prev = -1;
	if( x>=0 ){
		MEM5LINK(x)->prev = i;
	}
	mem5.aiFreelist[iLogsize] = i;
}

/*
** Return the size of an outstanding allocation, in bytes.
** This only works for chunks that are currently checked out.
*/
static int memsys5Size(void *p){
	int iSize, i;
	i = (int)(((u8 *)p-mem5.zPool)/ATOM_SIZE);
	iSize = ATOM_SIZE * (1 << (mem5.aCtrl[i]&CTRL_LOGSIZE));
	return iSize;
}

/*
** Return a block of memory of at least nBytes in size.
** Return NULL if unable.  Return NULL if nBytes==0.
**
** The caller guarantees that nByte is positive.
**
** The caller has obtained a mutex prior to invoking this
** routine so there is never any chance that two or more
** threads can be in this routine at the same time.
*/
static void *memsys5MallocUnsafe(int iLogsize){
	int i;           /* Index of a mem5.aPool[] slot */
	int iBin;        /* Index into mem5.aiFreelist[] */

	/* Make sure mem5.aiFreelist[iLogsize] contains at least one free
	** block.  If not, then split a block of the next larger power of
	** two in order to create a new free block of size iLogsize.
	*/
	for(iBin=iLogsize; mem5.aiFreelist[iBin]<0; iBin++){}
	if( iBin>=LOGMAX ){ return 0; }
	i = mem5.aiFreelist[iBin];
	mem5.aCtrl[i] = iLogsize;
	memsys5Unlink(i, iBin);
	while( iBin>iLogsize ){
		int newSize;
		iBin--;
		newSize = 1 << iBin;
		mem5.aCtrl[i+newSize] = CTRL_FREE | iBin;
		memsys5Link(i+newSize, iBin);
	}
	/* Return a pointer to the allocated memory. */
	return (void*)&mem5.zPool[i*ATOM_SIZE];
}

/*
** Free an outstanding memory allocation.
*/
static void memsys5FreeUnsafe(void *pOld){
	u32 size, iLogsize;
	int iBlock;

	/* Set iBlock to the index of the block pointed to by pOld in 
	** the array of ATOM_SIZE byte blocks pointed to by mem5.zPool.
	*/
	iBlock = (int)(((u8 *)pOld-mem5.zPool)/ATOM_SIZE);

	iLogsize = mem5.aCtrl[iBlock];
	// if is already free we are done.
	if (iLogsize & CTRL_FREE) { return; }
	size = 1<<iLogsize;
	mem5.aCtrl[iBlock] = iLogsize + CTRL_FREE;
	while( 1 ) // ALWAYS(iLogsize<LOGMAX)
	{
		int iBuddy;
		if( (iBlock>>iLogsize) & 1 ){
			iBuddy = iBlock - size;
		}else{
			iBuddy = iBlock + size;
			if( iBuddy>=mem5.nBlock ) break;
		}
		if( mem5.aCtrl[iBuddy]!=(CTRL_FREE | iLogsize) ) break;
		memsys5Unlink(iBuddy, iLogsize);
		iLogsize++;
		if( iBuddy<iBlock ){
			mem5.aCtrl[iBuddy] = CTRL_FREE | iLogsize;
			mem5.aCtrl[iBlock] = 0;
			iBlock = iBuddy;
		}else{
			mem5.aCtrl[iBlock] = CTRL_FREE | iLogsize;
			mem5.aCtrl[iBuddy] = 0;
		}
		size *= 2;
	}
	memsys5Link(iBlock, iLogsize);
}

/*
** Allocate nBytes of memory. CycleCount, Min:170 Max:926
*/
/*e*/void*
zalloc(int nBytes)/*p;*/
{
	void *p = 0;
	if( (nBytes & 0xFFFC0000) == 0 ){
		int iFullSz;     /* Size of allocation rounded up to power of 2 */
		int iLogsize;    /* Log2 of iFullSz/POW2_MIN */
		/* Round nByte up to the next valid power of two */
		for(iFullSz=ATOM_SIZE,iLogsize=0;iFullSz<nBytes;iFullSz*=2,iLogsize++){}
		asm("CPSID i"); // disable interrupts
		p = memsys5MallocUnsafe(iLogsize);
		asm("CPSIE i"); // enable interrupts
		rom_func.memset(p,0,nBytes);
	}
	return p; 
}

/*
** Free memory. CycleCount, Min:145 Max:1278
**
** The outer layer memory allocator prevents this routine from
** being called with pPrior==0.
*/
/*e*/void
free(void *pPrior)/*p;*/
{
  if (pPrior == 0) { return; }
  asm("CPSID i"); // disable interrupts
  memsys5FreeUnsafe(pPrior);
  asm("CPSIE i"); // enable interrupts
}

/*
** Round up a request size to the next valid allocation size.  If
** the allocation is too large to be handled by this allocation system,
** return 0.
**
** All allocations must be a power of two and must be expressed by a
** 32-bit signed integer.  Hence the largest allocation is 0x40000000
** or 1073741824 bytes.
*/
static int memsys5Roundup(int n){
  int iFullSz;
  if( n<=ATOM_SIZE*2 ){
    if( n<=ATOM_SIZE ) return ATOM_SIZE;
    return ATOM_SIZE*2;
  }
  if( n>0x10000000 ){
    if( n>0x40000000 ) return 0;
    if( n>0x20000000 ) return 0x40000000;
    return 0x20000000;
  }
  for(iFullSz=ATOM_SIZE*8; iFullSz<n; iFullSz *= 4);
  if( (iFullSz/2)>=n ) return iFullSz/2;
  return iFullSz;
}

/*
** Change the size of an existing memory allocation.
**
*/
/*e*/void*
memsys5Realloc(void *pPrior, int nBytes)/*p;*/
{
	int nOld;
	void *p;
	if( nBytes==0 ){ free(pPrior); return 0; }
	nBytes = memsys5Roundup(nBytes);
	nOld = memsys5Size(pPrior);
	if( nBytes<=nOld ){
		return pPrior;
	}
	p = zalloc(nBytes);
	if( p ){
		rom_func.memcpy(p, pPrior, nOld);
		free(pPrior);
	}
	return p;
}
/*
** Initialize the memory allocator.
**
** This routine is not threadsafe.  The caller must be holding a mutex
** to prevent multiple threads from entering at the same time.
*/
/*e*/void
memsys5Init(void)/*p;*/
{
	int ii;            /* Loop counter */
	int nByte;         /* Number of bytes of memory available to this allocator */
	u8 *zByte;         /* Memory usable by this allocator */
	int iOffset;       /* An offset into mem5.aCtrl[] */


	/* For the purposes of this routine, disable the mutex */

	/* The size of a Mem5Link object must be a power of two.  Verify that
	** this is case.
	*/

	nByte = 6144*33;
	zByte = (u8*)START_OF_REG_RAM;

	mem5.nBlock = (nByte / (ATOM_SIZE+sizeof(u8)));
	mem5.zPool = zByte;
	mem5.aCtrl = (u8 *)&mem5.zPool[mem5.nBlock*ATOM_SIZE];

	for(ii=0; ii<=LOGMAX; ii++){
		mem5.aiFreelist[ii] = -1;
	}
	mem5.aiFreelist[LOGMAX] = 0;
	iOffset = 0;
	for(ii=LOGMAX; ii>=0; ii--){
		int nAlloc = (1<<ii);
		if( (iOffset+nAlloc)<=mem5.nBlock ){
			mem5.aCtrl[iOffset] = ii | CTRL_FREE;
			memsys5Link(iOffset, ii);
			iOffset += nAlloc;
		}
	}
}

/*e*/
void printMemStats(void)/*p;*/
{
	u32 totalMemory = 6144 * 32;
	io_printi(totalMemory);
	io_prints(": bytes of memory under stewardship.\n");
	u32     memReady = 0;
	s32	cursor   = 0;
	u32     memSize  = 32;
	for (u32 x = 0; x < LOGMAX; x++)
	{
		io_printi(memSize);
		io_prints(":");
		cursor = mem5.aiFreelist[x];
		u32 nodeCount = 0;
		while (cursor != -1)
		{
			memReady += memSize;
			//~ io_prints("#");
			nodeCount++;
			cursor = MEM5LINK(cursor)->next;
		}
		while (nodeCount > 0)
		{
			io_prints("#");
			nodeCount--;
		}
		io_prints("\n");
		memSize *= 2;
	}
	io_printi(totalMemory - memReady);
	io_printsn(": bytes of memory in use.");
	io_printi(memReady);
	io_printsn(": bytes of memory remaining.");
	//~ pengum_memoryLeft();
}

