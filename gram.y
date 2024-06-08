// gram.y

%include{
#define u32 size_t
#include "localTypes.h"
#define YYNOERRORRECOVERY 1
#define YYPARSEFREENEVERNULL
#define YYSTACKDEPTH 64
#define NDEBUG
#define PARSE_STATIC_ENGINE
#define assert(x)
//Context*c=(Context*)yypParser;
}

//~ %extra_context {CompilerContext *c}

%token F_SHLASH PERCENT STAR LINE CARROT TILDA PLUS AMPER MINUS SEMI COLON
		LPAREN LTHAN GTHAN EQUALS LBLOCK HASH TICK COMMA RBLOCK DOLLAR LBRACK
		BANG QMARK DIGIT ALPHA DQUOTE SQUOTE RPAREN RBRACK PERIOD LSHIFT RSHIFT
		LTHAN_EQ GTHAN_EQ L_EQUALS L_NEQUALS BITCLEAR IF L_OR L_AND ABS NOT VAR
		ELSE WHILE RETURN FN INT_LIT STRING_LIT IDENT.
%syntax_error {
	//~ if (yymajor == 0) { return; }
	//~ Context *c = (Context *)yypParser;
	//~ if (c.error) { return; }
	c.error = 1;
	yyStackEntry *bot = yypParser->yystack + 1;
	yyStackEntry *top = yypParser->yytos;
	while (++bot <= top)
	{
	uart0_outByte('[');io_prints((u8*)yyTokenName[bot->major]);uart0_outByte(']');
	//~ txByte('[');io_printi(bot->minor.yy0.type);txByte(']');
	}
	uart0_outByte('[');io_prints((u8*)yyTokenName[yymajor]);io_printsn("]<- invalid");
	i_printCurrLineInfo();
	io_printin(yypParser->yytos->stateno);
	for (u32 i = 0; i < YYNTOKEN; i++)
	{
		int a = yy_find_shift_action(i, yypParser->yytos->stateno);
		if (a != YY_ERROR_ACTION) {
			io_prints("possible valid token:");
			io_printsn((u8*)yyTokenName[i]);
		}
	}
	// clear out everything from the stack
	yypParser->yytos = yypParser->yystack;
	//~ io_printi(yymajor);
	//~ io_printsn(": Syntax Error\n");
}

%token_type   {Token}
%default_type {Token}

program ::= top_level_stmt_list NEVER.

top_level_stmt_list ::= top_level_stmt.
top_level_stmt_list ::= top_level_stmt_list top_level_stmt.

//~ top_level_stmt ::= stmt_expr.
top_level_stmt_wrap ::= stmt.
{
	// set flag and emit exit op code
	c.executeExpr=1;
	*c.compileCursor++ = SUBI_EXIT;
	u16 *cursor = c.compileBase;

	setBaseCodePointer(cursor);
	while(cursor != c.compileCursor)
	{
		cursor=printInstruction(cursor);
		uart0_outByte('\n');
	}

	cursor = c.compileBase;
	io_printsn("Trace Execution:");
	intr_traceOn(cursor);

	//~ io_printsn("top_level_stmt");
	//~ Context *c = (Context *)yypParser;
	//~ if (c.error)
	//~ {
		//~ c.error = 0;
	//~ } else {
		//~ if (c.compileCursor != c.compileBase + 5)
		//~ {
			//~ s32 (*eval)(void) = (void*)mc_finalizeFunction((void*)yypParser);
			//~ io_printin(eval());
		//~ }
	//~ }
	//~ // reset state
	//~ mc_spaceForPrologue(c);
}
//~ top_level_stmt ::= stmt_if. { io_printsn("top_level_stmt"); }
//~ top_level_stmt ::= stmt_while. { io_printsn("top_level_stmt"); }
//~ top_level_stmt ::= stmt_var_decl. { io_printsn("top_level_stmt"); }
top_level_stmt ::= stmt_func_decl. //{ io_printsn("top_level_stmt"); }

top_level_stmt ::= top_level_stmt_wrap SEMI.
top_level_stmt ::= SEMI.

func_start ::= FN IDENT(A). { parse_func_start(&A); }
stmt_func_decl ::= func_start params stmt_compound. { parse_func_end(); }

params ::= LPAREN RPAREN. { c.currentFunction->tClass = 0; }
params ::= LPAREN ident_list(B) RPAREN. { c.currentFunction->tClass = B.length; }

ident_list ::= IDENT(A).	{ parse_param(&A); A.length = 1; }
ident_list ::= ident_list(A) COMMA IDENT(B). { parse_param(&B); A.length++; }

if_start ::= IF(A) expr(B).				{ parse_lAndP1(&B);A = B;}
//~ stmt_if ::= stmt_if_con.	//{ parse_completeIf((void*)yypParser,A.is.list.head); }
//~ stmt_if_con ::= if_start(A) stmt_compound.		{ parse_landP2(c, &A);}
//~ stmt_if ::= stmt_if_else stmt_compound. //{ parse_completeIf((void*)yypParser,A.is.list.head); }
//~ stmt_if_con ::= stmt_if_else if_start stmt_compound. //{ B.is.list.head->siblings = A.is.list.head; A.is.list.head = B.is.list.head;}
//~ stmt_if_else ::= stmt_if_con ELSE.//{ parse_else((void*)yypParser,&A, &B); }

//~ stmt_if ::= stmt_if else_start stmt_compound.
//~ stmt_if ::= stmt_if else_start if_start stmt_compound.
else_start ::= ELSE(A). { A.as.brn.inst=c.compileCursor;*c.compileCursor++=0;*c.compileCursor++=0;}
stmt_if ::= if_start(A) stmt_compound. { parse_lAndP2(&A);}
stmt_if ::= else_part(A) stmt_compound.	{ parse_elseBranch(&A);}
stmt_if ::= else_part(A) stmt_if.	{ parse_elseBranch(&A);}
else_part ::= if_start(A) stmt_compound else_start(B). { parse_lAndP2(&A);c.nextReg=A.as.brn.regNum;A = B;}

stmt_return ::= RETURN.			{parse_return(0);}
stmt_return ::= RETURN expr(A).	{parse_return(&A);}

//~ while_start ::= while_tok expr.
while_expr ::= expr(A). { parse_lAndP1(&A);}
while_tok::=WHILE(A). { A.as.brn.inst=c.compileCursor;}
stmt_while ::= while_tok(A) while_expr(B) stmt_compound. { *c.compileCursor++=branchInstr(0,SUBI_BRANCH);*c.compileCursor=A.as.brn.inst-c.compileCursor-1;c.compileCursor++;parse_lAndP2(&B);}

stmt_compound ::= lblock stmt_list RBLOCK.	//{ leaveScope((void*)yypParser); }
stmt_compound ::= LBLOCK RBLOCK.
lblock ::= LBLOCK.				//			{ enterScope((void*)yypParser); }

//~ stmt_var_decl ::= var_decl SEMI.
//~ stmt_var_decl ::= var_decl(A) EQUALS expr SEMI.{parse_binaryOp(alloc(),&C,B.type,&A); A.is.list.head = C.is.list.head;}
stmt_var_decl ::= var_decl EQUALS expr.//{parse_binaryOp(alloc(),&C,B.type,&A); A.is.list.head = C.is.list.head;}
var_decl ::= VAR IDENT.// { parse_varDecl(alloc(),(void*)yypParser, &A, &B); }

stmt_list ::= stmt.
stmt_list ::= stmt_list stmt.

stmt ::= stmt_compound.
stmt ::= stmt_var_decl.	//{ cg_trav((void*)yypParser, A.is.list.head); }
stmt ::= stmt_if.
stmt ::= stmt_return.	//{ cg_trav((void*)yypParser, A.is.list.head); }
stmt ::= stmt_while.
stmt ::= stmt_expr.		//{ cg_trav((void*)yypParser, A.is.list.head); }


//~ stmt_expr ::= e_expr_union SEMI.
//~ stmt_expr ::= SEMI.
stmt_expr ::= e_expr_union.
//~ stmt_expr ::= SEMI.
e_expr_union ::= e_expr.
e_expr_union ::= e_expr_assign.

// if (x == 0 && y == 0) && (a == 1 || b == 0)
// evaluated expressions, code will be generated here
//~ e_expr ::= expr.				//{ cg_trav((void*)yypParser, A.is.list.head); }
e_expr ::= expr. [EQUALS]
e_expr_assign ::= expr_assign.//{ cg_trav((void*)yypParser, A.is.list.head); }
expr_assign ::= expr EQUALS expr.//{parse_binaryOp(alloc(),&C,B.type,&A); A.is.list.head = C.is.list.head;}

// literals and parens
expr ::= INT_LIT(A). {  parse_intLit(&A); }
expr ::= STRING_LIT.
expr ::= IDENT(A). { parse_ident(&A, yyLookahead); }
expr ::= LPAREN(A) expr(B) RPAREN.	{ A = B; }
// postfix
expr ::= expr PERIOD IDENT.
expr ::= expr LBRACK expr RBRACK.
expr ::= expr COLON expr.
// function call
expr ::= funcStart(A) args(B). { parse_namedFunctionCall(&A,B.length); }
funcStart ::= expr LPAREN. // need to start recording exprs
args ::= RPAREN(A). 					{ A.length = 0; }
args ::= arg_list RPAREN.
arg_list ::= expr(A).					{ A.length = 1; }
arg_list ::= arg_list(A) COMMA expr.	{ A.length++; }
// prefix
expr ::= TILDA(A) expr(B).		{ parse_unaryOp(&A, &B, SUBI_BNOT);}
expr ::= MINUS(A) expr(B). [TILDA]	{ parse_unaryOp(&A, &B, SUBI_NEG);}
expr ::= ABS(A)   expr(B).		{ parse_unaryOp(&A, &B, SUBI_ABS);}
// boolean prefix
expr ::= NOT(A)   expr(B).		{ parse_unaryOp(&A, &B, SUBI_NOT);}
// binary math expressions
expr ::= expr(A) PLUS expr(B).		{ parse_binaryOp(&A, &B, I_ADD);}
expr ::= expr(A) MINUS expr(B).		{ parse_binaryOp(&A, &B, I_SUB);}
expr ::= expr(A) STAR expr(B).		{ parse_binaryOp(&A, &B, I_MUL);}
expr ::= expr(A) F_SHLASH expr(B).	{ parse_binaryOp(&A, &B, I_DIV);}
expr ::= expr(A) PERCENT expr(B).	{ parse_binaryOp(&A, &B, I_MOD);}
// binary bitwise expressions
expr ::= expr(A) LINE expr(B).		{ parse_binaryOp(&A, &B, I_BOR);}
expr ::= expr(A) AMPER expr(B).		{ parse_binaryOp(&A, &B, I_BAND);}
expr ::= expr(A) CARROT expr(B).	{ parse_binaryOp(&A, &B, I_XOR);}
expr ::= expr(A) LSHIFT expr(B).	{ parse_binaryOp(&A, &B, I_LSH);}
expr ::= expr(A) RSHIFT expr(B).	{ parse_binaryOp(&A, &B, I_RSH);}
expr ::= expr(A) BITCLEAR expr(B).	{ parse_binaryOp(&A, &B, I_BCLR);}
// boolean tests
expr ::= expr(A) L_EQUALS expr(B).	{ parse_binaryOp(&A, &B, I_EQ);}
expr ::= expr(A) L_NEQUALS expr(B).	{ parse_binaryOp(&A, &B, I_NE);}
expr ::= expr(A) LTHAN expr(B).		{ parse_binaryOp(&A, &B, I_LT);}
expr ::= expr(A) GTHAN expr(B).		{ parse_binaryOp(&A, &B, I_GT);}
expr ::= expr(A) LTHAN_EQ expr(B).	{ parse_binaryOp(&A, &B, I_LE);}
expr ::= expr(A) GTHAN_EQ expr(B).	{ parse_binaryOp(&A, &B, I_GE);}
// binary boolean combinations
l_or_start ::= expr(A) L_OR.		{ parse_lOrP1(&A);}
expr ::= l_or_start(A) expr. [L_OR]	{ parse_lOrP2(&A);}
l_and_start ::= expr(A) L_AND.		{ parse_lAndP1(&A);}
expr ::= l_and_start(A) expr.[L_AND]	{ parse_lAndP2(&A);}

// precedence
%right		RETURN.

%right		EQUALS.
%left		L_AND L_OR.
%nonassoc	L_EQUALS L_NEQUALS LTHAN GTHAN LTHAN_EQ GTHAN_EQ.
%left		LINE AMPER CARROT LSHIFT RSHIFT BITCLEAR.
%left		PLUS MINUS.
%left		STAR F_SHLASH PERCENT.
%right		TILDA NOT ABS.
%left		PERIOD COLON LBRACK LPAREN.

%left		INT_LIT STRING_LIT IDENT.

