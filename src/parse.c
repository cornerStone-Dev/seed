// parse.c
#include "../localTypes.h"
#include "../gram.h"
enum {
	AST_LOAD_VAR = IDENT,
	AST_STORE_VAR,
	INT_LIT_SMALL,
	INT_LIT_LARGE,
};
#include "../inc/parse_i.h"

enum{
	TYPE_FUNCTION,
	TYPE_GLOBAL,
	TYPE_CONSTANT,
	TYPE_LOCAL,
};

enum{
	BLOCK_FUNCTION,
	BLOCK_COND,
	BLOCK_ELSE,
	BLOCK_WHILE,
	BLOCK_WHILE_COND,
	BLOCK_STRUCT,
};

#define VM_REG_SIZE 5
#define VM_INST_SIZE 16
#define VM_INSTRUCTION_MASK 0x3F

static inline u32 setDest(u32 reg) { return reg <<(VM_INST_SIZE-VM_REG_SIZE); }
static inline u32 setArg1(u32 reg) { return reg <<(VM_INST_SIZE-(VM_REG_SIZE*2)); }
static inline u32 setArg2(u32 reg) { return reg << (32-(VM_REG_SIZE*3)); }
static inline u32 setArg3(u32 reg) { return reg << (32-(VM_REG_SIZE*4)); }
static inline u32 min(u32 x, u32 y) { return x<y?x:y; }

static u32
smallInt(u32 dest, u32 val)
{
	u32 code = I_SMALL_INT;
	code += setArg1(val) + setDest(dest);
	return code;
}

static u32
medInt(u32 dest)
{
	u32 code = SUBI_MED_INT;
	code += setDest(dest);
	return code;
}

static u32
bigInt(u32 dest)
{
	u32 code = SUBI_LRG_INT;
	code += setDest(dest);
	return code;
}
static void
compileVal(u32 val, u32 reg)
{
	if (val <= 31)
	{
		*c.compileCursor++ = smallInt(reg, val);
	} else if (val <= 65535) {
		*c.compileCursor++ = medInt(reg);
		*c.compileCursor++ = val;
	} else {
		*c.compileCursor++ = bigInt(reg);
		*c.compileCursor++ = val & 0xFFFF;
		*c.compileCursor++ = val >> 16;
	}
}

static u32
compileMov(u32 dest, u32 reg)
{
	u32 code = I_MOVE;
	code += setDest(dest) + setArg1(reg);
	return code;
}

static u32
compileReturn(u32 dest)
{
	u32 code = SUBI_RET;
	code += setDest(dest);
	return code;
}

static u32
compileNamedFunctionCall(u32 dest)
{
	u32 code = SUBI_CALL;
	return code + setDest(dest);
}

/*e*/void
parse_intLit(Token *t)/*p;*/
{
	u32 reg = c.nextReg++;
	compileVal(t->length, reg);
	t->as.val.regNum = reg;
}

static void
compileBinOp(u32 dest, u32 arg1, u32 code)
{
	code += setDest(dest) + setArg1(arg1);
	*c.compileCursor++ = code;
}
/*e*/void
parse_binaryOp(Token *l, Token *r, u32 code)/*p;*/
{
	u32 destReg = min(l->as.val.regNum, r->as.val.regNum);
	compileBinOp(destReg,r->as.val.regNum,code);
	c.nextReg = destReg + 1;
}

static void
compileUnaryOp(u32 dest, u32 inst)
{
	u32 code = setDest(dest) + inst;
	*c.compileCursor++ = code;
}
/*e*/void
parse_unaryOp(Token *l, Token *r, u32 code)/*p;*/
{
	compileUnaryOp(r->as.val.regNum,code);
	*l = *r;
}

/*e*/u32
compilebranch(u32 dest, s32 jump, u32 code)/*p;*/
{
	return code + setDest(dest) + ((u32)jump<<10>>5);
}
/*e*/u32
branchInstr(u32 dest, u32 inst)/*p;*/
{
	u32 code = inst + setDest(dest);
	return code;
}
/*e*/void
parse_lAndP1(Token *l)/*p;*/
{
	// record location
	l->as.brn.inst = c.compileCursor;
	// make room for branch
	*c.compileCursor++ = 0;
	*c.compileCursor++ = 0;
	c.nextReg--;
}
/*e*/void
parse_lAndP2(Token *l)/*p;*/
{
	*l->as.brn.inst++ = branchInstr(l->as.brn.regNum, SUBI_BRANCH_ZERO);
	*l->as.brn.inst = c.compileCursor - l->as.brn.inst;
}

/*e*/void
parse_lOrP1(Token *l)/*p;*/
{
	// record location
	l->as.brn.inst = c.compileCursor;
	// make room for branch
	*c.compileCursor++ = 0;
	*c.compileCursor++ = 0;
	c.nextReg--;
}
/*e*/void
parse_lOrP2(Token *l)/*p;*/
{
	*l->as.brn.inst++ = branchInstr(l->as.brn.regNum, SUBI_BRANCH_NOT_ZERO);
	*l->as.brn.inst = c.compileCursor - l->as.brn.inst;
}

/*e*/void
parse_elseBranch(Token *l)/*p;*/
{
	*l->as.brn.inst++ = branchInstr(0,SUBI_BRANCH);
	*l->as.brn.inst = c.compileCursor - l->as.brn.inst;
}

/*e*/Tree *
createGlobalName(u8 *start, u32 length)/*p;*/
{
	Tree *name = tree_find(c.globals, start, length);
	if (name)
	{
		io_prints("[Warning] global \"");
		io_printsl(start, length);
		io_printsn("\" is being redefined.");
	} else {
		tree_add(&c.globals, start, length, 0);
		name = tree_find(c.globals, start, length);
	}
	return name;
}

/*e*/Tree *
createLocalName(u8 *start, u32 length)/*p;*/
{
	Tree *name = tree_find(c.blocks->names, start, length);
	if (name)
	{
		io_prints("[Warning] local \"");
		io_printsl(start, length);
		io_printsn("\" is being redefined.");
	} else {
		tree_add(&c.blocks->names, start, length, 0);
		name = tree_find(c.blocks->names, start, length);
	}
	return name;
}

/*e*/Tree *
resolveName(u8 *start, u32 length)/*p;*/
{
	Tree *name = tree_find(c.globals, start, length);
	if (!name) {
		Block *cursor = c.blocks;
		while (cursor)
		{
			name = tree_find(cursor->names, start, length);
			if (name) { break; }
			cursor = cursor->next;
		}
	}
	return name;
}

/*e*/Block*
pushNewBlock(u32 type)/*p;*/
{
	Block *newBlock = zalloc(sizeof(Block));
	newBlock->blockType = type;
	if (c.blocks)
	{
		newBlock->varNum = c.blocks->varNum;
	}
	c.blocks = list_prepend(newBlock, c.blocks);
	return newBlock;
}

/*e*/void
popBlock(void)/*p;*/
{
	Block *current = list_removeFirst(&c.blocks);
	if (current->varNum > c.varNumHighWater)
	{
		c.varNumHighWater = current->varNum;
	}
	tree_free(current->names);
	free(current);
}


/*e*/void
parse_func_start(Token *funcTok)/*p;*/
{
	// we are inside a top level statement
	// create global name for this function
	io_printsln(funcTok->string, funcTok->length);
	Tree *name = createGlobalName(funcTok->string, funcTok->length);
	name->type = TYPE_FUNCTION;
	name->value  = (void*)c.compileBase;
	io_printin((u32)name->value);
	c.currentFunction = name;
	// create a new block for the function
	pushNewBlock(BLOCK_FUNCTION);
	// reset parameters to starting values for function compiliation
	c.nextReg = 0;
	c.error = 0;
	c.varNumHighWater = 0;
}

/*e*/void
parse_param(Token *paramTok)/*p;*/
{
	Tree *name = createLocalName(paramTok->string, paramTok->length);
	name->value  = (void*)(u32)c.blocks->varNum++;
	c.nextReg++;
}

/*e*/void
parse_func_end()/*p;*/
{
	// we are inside a top level statement
	// create global name for this function
	//~ Tree *name = createGlobalName(funcTok->string, funcTok->length);
	//~ name->type = TYPE_FUNCTION;
	//~ name->value  = (void*)((u32)c.compileBase+1);
	//~ c.currentFunction = name;
	//~ // create a new block for the function
	//~ pushNewBlock(BLOCK_FUNCTION);
	//~ // reset parameters to starting values for function compiliation
	//~ c.nextReg = 0;
	//~ c.error = 0;
	// check for errors
	popBlock();
	if (c.error)
	{
		c.error = 0;
		io_prints(c.currentFunction->key);
		io_printsn(": canceled, there was an error.");
		tree_del(&c.globals, c.currentFunction->key, c.currentFunction->keyLen);
	} else {
		u32 wordSize = ((u32)c.compileCursor - (u32)c.compileBase);
		io_prints(c.currentFunction->key);
		io_prints(": defined, # bytes ");
		io_printin(wordSize);
		c.compileBase = c.compileCursor;
	}
	// reset state
	c.currentFunction = 0;
	c.nextReg = 0;
	c.varNumHighWater = 0;
	c.compileCursor = c.compileBase;
}

/*e*/void
parse_return(Token *expr)/*p;*/
{
	//~ if (expr)
	//~ {
		//~ *c.compileCursor++ = compileMov(0, --c.nextReg);
	//~ }
	*c.compileCursor++ = compileReturn(--c.nextReg);
}

/*e*/void
parse_namedFunctionCall(Token *expr, u32 numArgs)/*p;*/
{
	if (expr->type != IDENT) { return io_printsn("dynamic function call not implemented"); }
	Tree *name = expr->as.var.name;
	if (name == 0 ) { io_printsn("parse_namedFunctionCall: Symbol not resolved."); }
	io_printsln(expr->string, expr->length);
	*c.compileCursor++ = compileNamedFunctionCall(c.blocks->varNum);
	*c.compileCursor = (u16*)name->value - c.compileCursor; c.compileCursor++;
	io_printin((u32)name->value);
	io_printin((u32)(c.compileCursor-1));
	io_printin((s32)(s16)*(c.compileCursor-1));
}

/*e*/void
parse_ident(Token *identTok, s32 lookAhead)/*p;*/
{
	Tree *name = resolveName(identTok->string, identTok->length);
	if (name == 0 ) { io_printsn("Error: Symbol not resolved."); }
	identTok->as.var.name = name;
	if (lookAhead != LPAREN && lookAhead != EQUALS)
	{
		*c.compileCursor++ = compileMov(c.nextReg++, (u32)name->value);
		io_printsn("load register.");
	}
}


#if 0

Tree*
resolveName(Context *c, String *name)
{
	LocalScope *cursor = c->scope;
	do {
		Tree *target = tree_find(cursor->symbols, name);
		if (target) { return target; }
		cursor--;
	} while (cursor >= c->scopes);
	io_printsn("Error: Symbol not resolved.");
	c->error = 1;
	return 0;
}


void parse_ident(AST *a, Context *c, Token *t, s32 lookAhead)
{
	if (lookAhead == EQUALS)
	{
		a->as.sym.type = AST_STORE_VAR;
	} else {
		a->as.sym.type = AST_LOAD_VAR;
	}
	String *name = string_create(t->string, t->length);
	Tree *res = resolveName(c, name);
	a->as.sym.var = res;
	t->is.list.head = a;
}

static Tree *
makeDecl(Context *c, u8 *bytes, u32 length)
{
	String *key = string_create(bytes, length);
	LocalScope *scope = c->scope;
	u32 varNum = scope->varNum;
	Tree *decl = tree_add(&scope->symbols, key, (void*)varNum);
	if (decl)
	{
		// an entry already existed
		if (scope == c->scopes)
		{
			io_printsn("Warning: redefinition of global that existed.");
		} else {
			io_printsn("Warning: redefinition of local that existed.");
		}
	} else {
		// a new entry was created
		decl = tree_find(scope->symbols, key);
		if (scope == c->scopes)
		{
			decl->type = TYPE_GLOBAL;
		} else {
			decl->type = TYPE_LOCAL;
			scope->varNum++;
		}
	}
	return decl;
}

/*e*/
void parse_varDecl(AST *a, Context *c, Token *l, Token *r)/*p;*/
{
	Tree *decl = makeDecl(c, r->string, r->length);
	a->as.sym.type  = AST_STORE_VAR;
	a->as.sym.var   = decl;
	l->is.list.head = a;
}

/*e*/
void parse_binaryBool(AST *a, Token *l, s32 type, Token *r)/*p;*/
{
	a->as.bBool.type1 = type;
	a->as.bBool.type2 = type;
	a->as.bBool.siblings = r->is.list.head;
	a->children = l->is.list.head;
	l->is.list.head->siblings = (AST*)&a->as.bBool.children;
	l->is.list.head = a;
}

/*e*/
void parse_funcCall(AST *a, Context *c, Token *func, Token *args)/*p;*/
{
	AST *funcAst  = func->is.list.head;
	AST *argsAst  = args->is.list.head;
	if (funcAst->as.fun.type == AST_LOAD_VAR
	&& funcAst->as.fun.var->type == TYPE_FUNCTION)
	{
		funcAst->as.fun.type = LPAREN;
	} else {
		// TODO fix this to be generic call
		a->as.fun.type = LPAREN;
		funcAst->siblings = a;
	}
	
	u32 numArgs = 0;
	// count numArgs and find end
	if (argsAst != 0) {
		numArgs++;
		while (argsAst->siblings != 0)
		{
			numArgs++;
			argsAst = argsAst->siblings;
		}
	}
	funcAst->as.fun.numArgs = numArgs;
	if (numArgs)
	{
		argsAst->siblings  = funcAst;
		func->is.list.head = args->is.list.head;
	}
	return;
}

/*e*/
void parse_completeIf(Context *c, AST *head)/*p;*/
{
	while (head)
	{
		cg_completeIf(c, head);
		head = head->siblings;
	}
}

/*e*/
void parse_completeWhile(Context *c, Token *w)/*p;*/
{
	// emit jump to top of while loop
	u16 *jump = (u16*)w->length;
	*c->compileCursor = armBranch(jump - c->compileCursor - 2);
	c->compileCursor++;
	AST *a      = w->is.list.head;
	jump = (u16*)a->as.iLit.value;
	*jump = armCond(0, c->compileCursor - jump - 2);
}

/*e*/
void parse_else(Context *c, Token *cond, Token *op)/*p;*/
{
	// emit jump to skip else
	u16 *elseJump = c->compileCursor;
	AST *a      = cond->is.list.head;
	*c->compileCursor++ = armBranch(0);
	// complete the current if
	u16 *jump = (u16*)a->as.iLit.value;
	*jump = armCond(0, c->compileCursor - jump - 2);
	// push else
	AST *new = alloc();
	new->siblings = a->siblings;
	cond->is.list.head = new;
	new->as.iLit.value = (s32)elseJump;
}

/*e*/
void parseParams(Context *c, Token *arg)/*p;*/
{
	//~ Tree *decl = 
	c->numParams++;
	makeDecl(c, arg->string, arg->length);
	
	
}

/*e*/void
enterScope(Context *c)/*p;*/
{
	LocalScope *prev = c->scope;
	LocalScope *next = c->scope + 1;
	next->varNum = prev->varNum;
	c->scope = next;
}

/*e*/void
leaveScope(Context *c)/*p;*/
{
	c->scope->symbols = 0;
	if (c->scope->varNum > c->varNumHighWater) {
		c->varNumHighWater = c->scope->varNum;
	}
	c->scope--;
}

/*e*/
void parse_funcStart(Context *c, Token *name)/*p;*/
{
	// make delcaration for function
	Tree *decl = makeDecl(c, name->string, name->length);
	u16 *functionEnd = c->compileBase;
	u16 *functionStart = functionEnd + 2;
	decl->value = (void*)((u32)functionStart + 1);
	decl->type  = TYPE_FUNCTION;
	// enter scope so parameters are added correctly
	enterScope(c);
}

/*e*/
void parse_funcEnd(Context *c)/*p;*/
{
	// leave scope so parameters are added correctly
	leaveScope(c);
	if (c->error)
	{
		c->error = 0;
	} else {
		mc_finalizeFunction(c);
		c->compileBase = c->compileCursor;
	}
	// reset state
	mc_spaceForPrologue(c);
}

#endif
