// tokenize.c
#include "../localTypes.h"
#include "../inc/tokenize_i.h"
#include "../gram.h"

#define CLASS1 0x04
#define WORD_BODY 1
#define MULTI_OP 7
#define TOK_COMMENT (-1)

enum{
	WORD_FUNCTION,
	WORD_INLINE_FUNCTION1,
	WORD_INLINE_FUNCTION2,
	WORD_GLOBAL,
	WORD_CONSTANT,
	WORD_LOCAL,
};

enum{
	BLOCK_NONE,
	BLOCK_WORD,
	BLOCK_COND,
	BLOCK_ELSE,
	BLOCK_WHILE,
	BLOCK_WHILE_COND,
	BLOCK_DO,
	BLOCK_CASE,
	BLOCK_CASE_COND,
	BLOCK_RETURN,
	BLOCK_ONCE,
	BLOCK_STRUCT,
};

enum{
	NUL=0*CLASS1, // NULL
	DIV=1*CLASS1, // DIVISION OR FORWARD SLASH
	PRC=2*CLASS1, // PERCENT
	STA=3*CLASS1, // STAR
	LIN=4*CLASS1, // VERTICLE LINE
	CAR=5*CLASS1, // CARROT
	TIL=6*CLASS1, // TILDA
	PLU=7*CLASS1, // PLUS
	AMP=8*CLASS1, // AMPERSAND
	MIN=9*CLASS1+WORD_BODY, // MINUS
	SCO=10*CLASS1, // SEMI-COLON *ONLY AFTER WORD
	COL=11*CLASS1, // COLON *ONLY AFTER WORD TODO!!!
	ATS=11*CLASS1, // AT SIGN *ONLY AFTER WORD
	LPA=12*CLASS1, // LEFT PAREN *ONLY AFTER WORD?
	LTH=13*CLASS1, // LESS THAN START OF COMPARATOR
	GTH=14*CLASS1, // GREATER THAN START OF COMPARATOR
	EQU=15*CLASS1, // EQUALS START OF COMPARATOR
	LBL=16*CLASS1, // LEFT BLOCK START OF ANON WORD
	HAS=17*CLASS1, // HASH OR POUND
	TIC=18*CLASS1, // TICK
	COM=19*CLASS1, // COMMA
	RBL=20*CLASS1, // RIGHT BLOCK END OF BLOCK
	DOL=21*CLASS1, // DOLLAR START OF DEREFERNCE OPERATOR
	LBR=22*CLASS1, // LEFT BRACKET START OF IMMEDIATE MODE
	BNG=23*CLASS1+WORD_BODY, // BANG(!) START OF COMPARATOR
	QMK=24*CLASS1+WORD_BODY, // QUESTION MARK
	DIG=25*CLASS1+WORD_BODY, // DIGIT
	ALP=26*CLASS1+WORD_BODY, // ALPHABET
	DQO=27*CLASS1, // DOUBLE QUOTE START OF STRING
	SQO=28*CLASS1, // SINGLE QUOTE START OF CHAR LITERAL
	RPA=29*CLASS1, // RIGHT PAREN
	RBR=30*CLASS1, // RIGHT BRACKET END OF IMMEDIATE MODE
	DOT=31*CLASS1, // DOT OR PERIOD
	BSL=31*CLASS1, // BACK SLASH
	WSP=32*CLASS1, // WHITE SPACE
	BAD=34*CLASS1, // BAD CHARACTER
	TYP=35*CLASS1, // TYPE FOR PRINTING
	//DOT=15*CLASS1, // DOT OR PERIOD
};

static const unsigned char class[] = {
/*         x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xa  xb  xc  xd  xe  xf */
/* 0x */  NUL,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,WSP,WSP,BAD,WSP,WSP,BAD,BAD,
/* 1x */  BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,BAD,
/* 2x */  WSP,BNG,DQO,HAS,DOL,PRC,AMP,SQO,LPA,RPA,STA,PLU,COM,MIN,ALP,DIV,
/* 3x */  DIG,DIG,DIG,DIG,DIG,DIG,DIG,DIG,DIG,DIG,COL,SCO,LTH,EQU,GTH,QMK,
/* 4x */  ATS,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* 5x */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,LBR,BSL,RBR,CAR,ALP,
/* 6x */  TIC,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* 7x */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,LBL,LIN,RBL,TIL,BAD,
/* 8x */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* 9x */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* Ax */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* Bx */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* Cx */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* Dx */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* Ex */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,
/* Fx */  ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,ALP,TYP,TYP,TYP
};

static s32
s2I(u8 *b, u8 **e)
{
	s32 result     = 0;
	//~ s32 isNegative = 1;
	//~ if (*b == '-')
	//~ {
		//~ b++;
		//~ isNegative = -1;
	//~ }

	if (*b == '0' && *(b+1) == 'x')
	{
		// process hex numbers
		b += 2;
		tryAnotherByte:
		if ( (*b >= '0') && (*b <= '9') )
		{
			result = (result * 16) + (*b - '0');
			b++;
			goto tryAnotherByte;
		} else if ( (*b >= 'A') && (*b <= 'F') ) {
			result = (result * 16) + (*b - ('A' - 10));
			b++;
			goto tryAnotherByte;
		} else {
			goto end;
		}
	}

	while( (*b >= '0') && (*b <= '9') )
	{
		result = (result * 10) + (*b - '0');
		b++;
	}

	end:
	//~ result = isNegative * result;
	//~ if (e) { *e = b; }
	*e = b;

	return result;
}

/*e*/
u8*
tokenize(u8 *string, Token *t)/*p;*/
{
	u8 *cursor = string;
	//~ u8 *start;
	u8 byte;
	loop:
	//~ start = cursor;
	byte = class[*cursor++] >> 2;
	switch (byte)
{
	case NUL>>2: { t->type = byte; break; }
	case DIV>>2: { cursor = consumeDiv(cursor, t, byte); break; }
	case PRC>>2: { t->type = byte; break; }
	case STA>>2: { t->type = byte; break; }
	case LIN>>2: { t->type = byte; break; }
	case CAR>>2: { t->type = byte; break; }
	case TIL>>2: { t->type = byte; break; }
	case PLU>>2: { t->type = byte; break; }
	case AMP>>2: { cursor = consumeAmp(cursor, t, byte); break; }
	case MIN>>2: { t->type = byte; break; }
	case SCO>>2: { t->type = byte; t->string = 0; break; }
	case ATS>>2: { t->type = byte; break; }
	case LPA>>2: { t->type = byte; break; }
	case LTH>>2: { cursor = consumeLthan(cursor, t, byte); break; }
	case GTH>>2: { cursor = consumeGthan(cursor, t, byte); break; }
	case EQU>>2: { cursor = consumeEqu(cursor, t, byte); break; }
	case LBL>>2: { t->type = byte; /*t->string = 0;*/ break; }
	case HAS>>2: { t->type = byte; break; }
	case TIC>>2: { t->type = byte; break; }
	case COM>>2: { t->type = byte; break; }
	case RBL>>2: { t->type = byte; break; }
	case DOL>>2: { t->type = byte; break; }
	case LBR>>2: { t->type = byte; break; }
	case BNG>>2: { cursor = consumeBng(cursor, t, byte); break; }
	case QMK>>2: { t->type = byte; break; }
	case DQO>>2: { t->type = byte; break; }
	case SQO>>2: { t->type = byte; break; }
	case RPA>>2: { t->type = byte; t->string = 0; break; }
	case RBR>>2: { t->type = byte; break; }
	case DOT>>2: { t->type = byte; break; }
	//~ case COL>>2: { io_prints("Invalid starting character, aborting\n"); break; }
	//~ case ATS>>2: { io_prints("Invalid starting character, aborting\n"); break; }
	//~ case LPA>>2: { io_prints("Invalid starting character, aborting\n"); break; }
	//~ case LTH>>2: { cursor = compileLth(cursor); goto loop; }
	//~ case GTH>>2: { cursor = compileGth(cursor); goto loop; }
	//~ case EQU>>2: { cursor = compileEqu(cursor); goto loop; }
	//~ case LBL>>2: { advComileStub(0); goto loop; }
	//~ case HAS>>2: { cursor = compileHas(cursor); goto loop; }
	//~ case TIC>>2: { io_prints("Invalid starting character, aborting\n"); break; }
	//~ case COM>>2: { compileComma(); goto loop; }
	//~ case RBL>>2: { cursor = compileRbl(cursor); goto loop; }
	//~ case DOL>>2: { cursor = compileDol(cursor); goto loop; }
	//~ case LBR>>2: { advComileStub(77); goto loop; }
	//~ case BNG>>2: { cursor = compileBng(cursor); goto loop; }
	//~ case QMK>>2: { printMemStats(); goto loop; /*break;*/ }
	case DIG>>2: { cursor = consumeNumLit(cursor, t); break; }
	case ALP>>2: { cursor = consumeAlpha(cursor, t); break; }
	//~ case DQO>>2: { cursor = consumeStringLit(cursor); goto loop; }
	//~ case SQO>>2: { cursor = consumeCharLit(cursor); goto loop; }
	//~ case RPA>>2: { if (*cursor == '{') {mc_setParams();cursor++;} else {io_printsn("Error: Right Paren alone.");} goto loop; }
	//~ case RBR>>2: { io_prints("Invalid starting character, aborting\n"); break; }
	//~ case BSL>>2: { while(*cursor != '\n'){cursor++;} cursor++; goto loop; }
	case WSP>>2: { goto loop; }
	//~ case BAD>>2: { io_prints("Bad input byte detected, aborting\n"); break; }
	//~ case TYP>>2: { cursor = printInterpolatedString2(cursor); goto loop; }
	//~ default: { io_prints("default detected, aborting\n"); break; }
	default: { goto loop; }
}	
	return cursor;
}

/*e*/static u8*
consumeNumLit(u8 *cursor, Token *t)/*i;*/
{
	u8 *newCursor;
	t->type = INT_LIT;
	t->length = s2I(cursor - 1, &newCursor);
	return newCursor;
}

/*e*/static u8*
consumeGthan(u8 *cursor, Token *t, u8 byte)/*i;*/
{	
		 if(class[*cursor]==GTH) {t->type=RSHIFT;  cursor++;}
	else if(class[*cursor]==EQU) {t->type=GTHAN_EQ;cursor++;}
	else { t->type = byte; }
	return cursor;
}

/*e*/static u8*
consumeLthan(u8 *cursor, Token *t, u8 byte)/*i;*/
{	
		 if(class[*cursor]==LTH) {t->type=LSHIFT;  cursor++;}
	else if(class[*cursor]==EQU) {t->type=LTHAN_EQ;cursor++;}
	else { t->type = byte; }
	return cursor;
}

/*e*/static u8*
consumeEqu(u8 *cursor, Token *t, u8 byte)/*i;*/
{	
	if(class[*cursor]==EQU) {t->type=L_EQUALS;cursor++;}
	else { t->type = byte; }
	return cursor;
}

/*e*/static u8*
consumeBng(u8 *cursor, Token *t, u8 byte)/*i;*/
{	
	if(class[*cursor]==EQU) {t->type=L_NEQUALS;cursor++;}
	else { t->type = byte; }
	return cursor;
}

/*e*/static u8*
consumeAmp(u8 *cursor, Token *t, u8 byte)/*i;*/
{
	if(class[*cursor]==TIL) {t->type=BITCLEAR;cursor++;}
	else { t->type = byte; }
	return cursor;
}

/*e*/static u8*
consumeDiv(u8 *cursor, Token *t, u8 byte)/*i;*/
{	
	if(class[*cursor]==DIV) {
		t->type= TOK_COMMENT;
		while (*cursor != '\n') {cursor++;}
		cursor++;
	}
	else { t->type = byte; }
	return cursor;
}

/*e*/static s32
builtInWord(u8 *start, u32 length)/*i;*/
{	
	s32 tokenType = 0;
	u8 *table = builtInWordTable;
	while (1) {
		u32 wordLength = *table++;
		if (wordLength == 0) { break; }
		if (wordLength != length) { goto failedMatch; }
		for (u32 i = 0; i < wordLength; i++)
		{
			if (table[i] != start[i]) { goto failedMatch; }
		}
		// success
		return tokenType + IF;
		// failure
		failedMatch:
		table += wordLength;
		tokenType++;
	}
	return 0;
}

/*e*/static u8*
consumeAlpha(u8 *cursor, Token *t)/*i;*/
{
	u8 *start = cursor - 1;
	u8 byte = *cursor;
	while (class[byte] & WORD_BODY)
	{
		cursor++;
		byte = *cursor;
	}
	// alpha parts of words cannot end with -, it is used as an operator
	while (*(cursor - 1) == '-') { cursor--; }
	u32 wordLength = cursor - start;
	
	// check for built in words
	s32 tokenType = builtInWord(start, wordLength);
	// short circuit to enable faster iteration
	if (tokenType == FN + 1) { REBOOT(); }
	//~ io_printin(tokenType);
	t->string = start;
	t->length = wordLength;
	if (tokenType != 0)
	{
		t->type = tokenType;
	} else {
		t->type = IDENT;
	}
	#if 0
	byte = class[*cursor] >> 2;
	switch (byte)
{
	case DIV>>2: { cursor = compilePostfixDiv(cursor, start, wordLength); goto done; }
	case PRC>>2: { cursor = compilePostfixPrc(cursor, start, wordLength); goto done; }
	case STA>>2: { cursor = compilePostfixSta(cursor, start, wordLength); goto done; }
	case LIN>>2: { cursor = compilePostfixLin(cursor, start, wordLength); goto done; }
	case CAR>>2: { cursor = compilePostfixCar(cursor, start, wordLength); goto done; }
	case PLU>>2: { cursor = compilePostfixPlu(cursor, start, wordLength); goto done; }
	case AMP>>2: { cursor = compilePostfixAmp(cursor, start, wordLength); goto done; }
	case MIN>>2: { cursor = compilePostfixMin(cursor, start, wordLength); goto done; }
	case SCO>>2: { cursor++; createVar(start, wordLength); goto done; }
	case COL>>2: { cursor++; createConstant(start, wordLength); goto done; }
	case ATS>>2: { cursor++; pushAddressOf(start, wordLength); goto done; }
	case LPA>>2: { cursor++; createWordFunction(start, wordLength); c.insideParams = 1; goto done; }
	case LTH>>2: { cursor = compilePostfixLth(cursor, start, wordLength); goto done; }
	case GTH>>2: { cursor = compilePostfixGth(cursor, start, wordLength); goto done; }
	case EQU>>2: { cursor++; assignVar(start, wordLength);goto done; }
	case LBL>>2: { cursor++; createWordFunction(start, wordLength); goto done; }
	default : break;
}
	// see if we are inside params
	if (c.insideParams)
	{
		createLocal(start, wordLength);
		goto done;
	}
	Tree *word = resolveWord(start, wordLength);
	if (word != 0)
	{
		u16 *code;
		switch (word->type)
		{
			case WORD_FUNCTION:
			callWord((u32)word->value);
			break;
			case WORD_INLINE_FUNCTION1:
			code = (u16*)((u32)word->value-1);
			putMachineCode(*code);
			break;
			case WORD_INLINE_FUNCTION2:
			code = (u16*)((u32)word->value-1);
			putMachineCode(*code++);
			putMachineCode(*code);
			break;
			case WORD_GLOBAL:
			mc_ldrGlobal(word->value);
			break;
			case WORD_CONSTANT:
			mc_integerLit((u32)word->value);
			break;
			case WORD_LOCAL:
			mc_ldrLocal((u32)word->value);
			break;
		}
	}
	
	done:
	#endif
	return cursor;
}

