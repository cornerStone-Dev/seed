// compiler.c
#include "../localTypes.h"
#include "../gram.h"

//~ u32 myParser[64];
Context c;


static u32 regStack[64];
static void* retStack[64];



/*e*/
void eCompile_Init(void)/*p;*/
{
	//~ Context *c = (Context*)myParser;
	c.compileBase		= (void*)__bss_end__;
	c.compileCursor	= (void*)__bss_end__;
	//~ c.scope 			= c->scopes;
	//~ ParseInit(0);
	//~ mc_spaceForPrologue(c);
}

/*e*/
void eCompile(void)/*p;*/
{
	Token *t = (Token*)&c;
while(1){
	term_processCharacter();
	do {
		tokenize(t);
		//~ io_printin(t->type); //io_txByte('\n');
		// send token to parser
		if (t->type > 0) { Parse(t->type, t); }
		if (c.executeExpr)
		{
			c.executeExpr = 0;
			c.nextReg = 0;
			c.compileCursor = c.compileBase;
			io_printin(interpreterLoop(c.compileBase, regStack, &retStack[64]));
			//~ io_printin(regStack[1]);
			//~ io_printin(regStack[2]);
		}
	} while(t->type != 0);
}
}

//~ VirtualInstruction instuctions[] =
//~ {
	//~ 0,
	//~ 0,
	//~ goToNext,
	// Done
	// binaryOp 			Dest = src1 op src2
	// BinaryOpWithConst	Dest = src1 op const
	// UnaryOP				Dest = op(src1)
	// MovRegs				Dest = src1
	// LoadConst			Dest = const
	// LoadConstWShift		Dest = shift(const, const)
	// Call					call(Dest)
	// jump					ip = ip + const
	// cjump				if (c==true) ip = ip + const
	// fCall				ip = ip + const EnterSequence
	// fReturn				ExitSequence
	// globals
	// memMove
	
	
//~ };
//~ VirtualInstruction2 instuctions2[] =
//~ {
	//~ 0,
	//~ 0,
	//~ addRegs2,
	// Done
	// binaryOp 			Dest = src1 op src2
	// BinaryOpWithConst	Dest = src1 op const
	// UnaryOP				Dest = op(src1)
	// MovRegs				Dest = src1
	// LoadConst			Dest = const
	// LoadConstWShift		Dest = shift(const, const)
	// Call					call(Dest)
	// jump					ip = ip + const
	// cjump				if (c==true) ip = ip + const
	// fCall				ip = ip + const EnterSequence
	// fReturn				ExitSequence
	// globals
	// memMove
	
	
//~ };

#define VM_REG_SIZE 5
#define VM_INST_SIZE 16
#define VM_INSTRUCTION_MASK 0x3F

static inline u32 getDest(u32 inst) { return inst >> (VM_INST_SIZE-VM_REG_SIZE); }
static inline u32 getArg1(u32 inst){return inst<<(VM_REG_SIZE+16)>>(VM_INST_SIZE-VM_REG_SIZE+16);}
//~ static inline u32 getArg2(u32 inst){return inst<<2*VM_REG_SIZE>>VM_INST_SIZE-VM_REG_SIZE;}
//~ static inline u32 getArg3(u32 inst){return inst<<3*VM_REG_SIZE>>VM_INST_SIZE-VM_REG_SIZE;}

typedef s32 (*GenericCall)(u32 a, u32 b, u32 c, u32 d);

// Fetech Decode Execute
/*e*/s32 
interpreterLoop(u16 * restrict ip, u32 * restrict sp, void ** restrict rp)/*p;*/ //
{
	u16 inst;
while(1){
	// Fetch
	inst = *ip++;
	// Decode
	u32 op = inst & VM_INSTRUCTION_MASK;
	u32 dest = getDest(inst);
	u32 arg1 = sp[dest];
	u32 arg2 = sp[getArg1(inst)];
	// Execute
	switch (op){
	case I_ADD:	{ sp[dest] = arg1 + arg2; continue;}
	case I_SUB:	{ sp[dest] = arg1 - arg2; continue;}
	case I_MUL:	{ sp[dest] = arg1 * arg2; continue;}
	case I_DIV:	{ sp[dest] = asmDiv(arg2, arg1); continue;}
	case I_BOR:	{ sp[dest] = arg1 | arg2; continue;}
	case I_BAND:	{ sp[dest] = arg1 & arg2; continue;}
	case I_XOR:	{ sp[dest] = arg1 ^ arg2; continue;}
	case I_LSH:	{ sp[dest] = arg1 << arg2; continue;}
	case I_RSH:	{ sp[dest] = arg1 >> arg2; continue;}
	case I_BCLR:	{ sp[dest] = arg1 &~arg2; continue;}
	case I_EQ:	{ sp[dest] = arg1==arg2?1:0; continue;}
	case I_NE:	{ sp[dest] = arg1!=arg2?1:0; continue;}
	case I_LT:	{ sp[dest] = (s32)arg1<(s32)arg2?1:0; continue;}
	case I_LE:	{ sp[dest] = (s32)arg1<=(s32)arg2?1:0; continue;}
	case I_GT:	{ sp[dest] = (s32)arg1>(s32)arg2?1:0; continue;}
	case I_GE:	{ sp[dest] = (s32)arg1>=(s32)arg2?1:0; continue;}
	case I_MOD:	{ sp[dest] = asmMod(arg2, arg1); continue;}
	case I_SET1:	{ *(u8*)arg1 = arg2; continue;}
	case I_SET2:	{ *(u16*)arg1 = arg2; continue;}
	case I_SET4:	{ *(u32*)arg1 = arg2; continue;}
	case SUBI_MED_INT:	{ sp[dest] = *ip++; continue;}
	case SUBI_LRG_INT:	{sp[dest]=*ip++;sp[dest]=((*ip++)<<16)+sp[dest];continue;}
	case SUBI_BRANCH_ZERO:	{if(sp[dest]!=0){continue;} ip+=*(s16*)ip;continue;}
	case SUBI_BRANCH_NOT_ZERO:	{if(sp[dest]==0){continue;}}
	case SUBI_BRANCH:	{ip+=*(s16*)ip;continue;}
	case SUBI_CALL:		{rp-=2;rp[0]=ip+1;rp[1]=sp;sp+=dest;ip+=*(s16*)ip;continue;}
	case SUBI_RET:		{sp[0]=sp[dest];sp[1]=sp[dest+1];sp=rp[1];ip=rp[0];rp+=2;continue;}
	case SUBI_BNOT:		{sp[dest]=~sp[dest]; continue;}
	case SUBI_NEG:		{sp[dest]=-((s32)sp[dest]); continue;}
	case SUBI_ABS:		{s32 val=sp[dest]; sp[dest]=val>=0?val:-val; continue;}
	case SUBI_NOT:		{sp[dest]= !sp[dest]; continue;}
	case SUBI_GET1:		{sp[dest]= *(u8*)sp[dest]; continue;}
	case SUBI_GET2:		{sp[dest]= *(u16*)sp[dest]; continue;}
	case SUBI_GET4:		{sp[dest]= *(s32*)sp[dest]; continue;}
	case SUBI_CALL_ADDR:	{u32 tgt=sp[dest];
		rp-=2;rp[0]=ip;rp[1]=sp;sp+=dest+1;ip=(s16*)tgt; continue;}
	case SUBI_CALL_C:	{u32 tgt=*ip++;tgt=((*ip++)<<16)+tgt;
	GenericCall f=(void*)tgt;sp[dest]=f(sp[dest],sp[dest+1],sp[dest+2],sp[dest+3]); continue;}
	case SUBI_EXIT:		{return sp[0];}
	case I_MOVE:{ sp[dest] = arg2; break;}
	case I_SMALL_INT:{ sp[dest] = getArg1(inst); break;}
	default: continue;
	}
}
}

static u8 *instrName[]= {
	"I_ADD                ",
	"I_SUB                ",
	"I_MUL                ",
	"I_DIV                ",
	"I_BOR                ",
	"I_BAND               ",
	"I_XOR                ",
	"I_LSH                ",
	"I_RSH                ",
	"I_BCLR               ",
	"I_EQ                 ",
	"I_NE                 ",
	"I_LT                 ",
	"I_LE                 ",
	"I_GT                 ",
	"I_GE                 ",
	"I_MOD                ",
	"I_SET1               ",
	"I_SET2               ",
	"I_SET4               ",
	"SUBI_MED_INT         ",
	"SUBI_LRG_INT         ",
	"SUBI_BRANCH_ZERO     ",
	"SUBI_BRANCH_NOT_ZERO ",
	"SUBI_BRANCH          ",
	"SUBI_CALL            ",
	"SUBI_RET             ",
	"SUBI_BNOT            ",
	"SUBI_NEG             ",
	"SUBI_ABS             ",
	"SUBI_NOT             ",
	"SUBI_GET1            ",
	"SUBI_GET2            ",
	"SUBI_GET4            ",
	"SUBI_CALL_ADDR       ",
	"SUBI_CALL_C          ",
	"SUBI_LD_GLBL         ",
	"SUBI_ST_GLBL         ",
	"SUBI_EXIT            ",
	"I_MOVE               ",
	"I_SMALL_INT          ",
};

static u32 MC_lineNum;

static void
printMachineCodeLineNum(void)
{
	u32 lineNum = MC_lineNum++;
	if (lineNum < 10) {
		io_prints("000");
	} else if (lineNum < 100) {
		io_prints("00");
	} else if (lineNum < 1000) {
		io_prints("0");
	}
	io_printi(lineNum);
	io_prints(": ");
}

/*e*/void
setLineNumber(u32 lineNum)/*p;*/
{
	MC_lineNum = lineNum;
}

// Fetech Decode Execute
/*e*/u16*
printInstruction(u16 *ip)/*p;*/
{
	u32 inst = *ip++;
	// Decode
	u32 op = inst & VM_INSTRUCTION_MASK;
	printMachineCodeLineNum();
	io_prints(instrName[op]);
	u32 dest = getDest(inst);
	u32 argR = getArg1(inst);
	// Execute
	switch (op){
	case I_ADD:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] + ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_SUB:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] - ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_MUL:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] * ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_DIV:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] / ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_BOR:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] | ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_BAND:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] & ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_XOR:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] ^ ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_LSH:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] << ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_RSH:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] >> ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_BCLR:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] &~ ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_EQ:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] == ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_NE:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] != ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_LT:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] < ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_LE:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] <= ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_GT:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] > ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_GE:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] >= ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_MOD:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("r[");io_printi(dest);io_prints("] % ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_SET1:	{
		io_prints("1BYTE_PTR(r[");io_printi(dest);io_prints("]) = ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_SET2:	{
		io_prints("2BYTE_PTR(r[");io_printi(dest);io_prints("]) = ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_SET4:	{
		io_prints("4BYTE_PTR(r[");io_printi(dest);io_prints("]) = ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case SUBI_MED_INT:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("LIT: ");io_printin(*ip++); MC_lineNum++; break;}
	case SUBI_LRG_INT:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		u32 val = *ip++;val=((*ip++)<<16)+val;
		io_prints("LIT: ");io_printin(val); MC_lineNum+=2; break; }
	case SUBI_BRANCH_ZERO:	{
		io_prints("if r[");io_printi(dest);io_prints("]==0 GOTO ");
		io_printin(MC_lineNum+*(s16*)ip); MC_lineNum++; ip++; break; }
	case SUBI_BRANCH_NOT_ZERO:	{
		io_prints("if r[");io_printi(dest);io_prints("]!=0 GOTO ");
		io_printin(MC_lineNum+*(s16*)ip); MC_lineNum++; ip++; break; }
	case SUBI_BRANCH:	{
		io_prints("GOTO ");io_printin(MC_lineNum+*(s16*)ip);
		MC_lineNum++; ip++; break; }
	case SUBI_CALL:		{MC_lineNum++; ip++; break;}
	//~ case SUBI_RET:		{sp[0]=sp[dest];sp[1]=sp[dest+1];sp=rp[1];ip=rp[0];rp+=2;continue;}
	case SUBI_BNOT:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("~r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_NEG:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("-r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_ABS:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("abs(r[");io_printi(dest);io_printsn("])"); break; }
	case SUBI_NOT:	{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("not r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_GET1:	{
		io_prints("r[");io_printi(dest);io_prints("]) = ");
		io_prints("1BYTE_PTR(r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_GET2:	{
		io_prints("r[");io_printi(dest);io_prints("]) = ");
		io_prints("2BYTE_PTR(r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_GET4:	{
		io_prints("r[");io_printi(dest);io_prints("]) = ");
		io_prints("4BYTE_PTR(r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_CALL_ADDR:	{
		io_prints("CALL r[");io_printi(dest);io_printsn("]"); break; }
	case SUBI_CALL_C:	{ MC_lineNum+=2; ip+=2; break; }
	//~ case SUBI_EXIT:		{return sp[0];}
	case I_MOVE:	{
		io_prints("r[");io_printi(dest);io_prints("]) = ");
		io_prints("r[");io_printi(argR);io_printsn("]"); break; }
	case I_SMALL_INT:{
		io_prints("r[");io_printi(dest);io_prints("] = ");
		io_prints("LIT: ");io_printin(argR);break;}
	default: break;
	}
	return ip;
}



