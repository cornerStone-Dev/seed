#include "../localTypes.h"

#define TERM_NUM_LINES 16


//~ enum {
	//~ TERM_STATE_NONE,
	//~ TERM_STATE_ESC1,
	//~ TERM_STATE_ESC2,
//~ };

typedef struct LineInfo {
	u32 cursor;
	u32 size;
	u8  line[120];
} LineInfo;

typedef struct TerminalInfo {
	u32 state;
	s32 currentLine;
} TerminalInfo;

static TerminalInfo term;
static LineInfo inputLine;
static LineInfo lines[TERM_NUM_LINES];

static void incrementCurrentLine(void)
{
	if ( (term.currentLine + 1) == TERM_NUM_LINES) {
		term.currentLine = 0;
	} else {
		term.currentLine++;
	}
}

static void decrementCurrentLine(void)
{
	if ( (term.currentLine - 1) == -1) {
		term.currentLine = TERM_NUM_LINES - 1;
	} else {
		term.currentLine--;
	}
}

static void clearLine(void)
{
	io_prints("\x1B[2K");
	return;
}

static void deleteCharacter(void)
{
	if (inputLine.cursor == 0) { return; }
	inputLine.cursor--;
	inputLine.size--;
	copyForward(&inputLine.line[inputLine.cursor+1],
		&inputLine.line[inputLine.cursor], inputLine.size - inputLine.cursor);
	return;
}

static void printTerminalLine(void)
{
	io_prints("\r>");
	inputLine.line[inputLine.size] = 0; // null terminate
	io_prints(inputLine.line);
	uart0_outByte(' ');
	u32 moveCursorBack = inputLine.size - inputLine.cursor;
	do {
		uart0_outByte(0x08);
	} while (moveCursorBack--);
	return;
}

static void insertCharacter(u32 input)
{
	inputLine.cursor++;
	inputLine.size++;
	// check for last character case and skip copy and output
	if (inputLine.cursor != inputLine.size){
		copyBackward(&inputLine.line[inputLine.cursor-1],
			&inputLine.line[inputLine.cursor], inputLine.size - inputLine.cursor);
		inputLine.line[inputLine.cursor-1] = input;
		printTerminalLine();
	} else {
		inputLine.line[inputLine.cursor-1] = input;
		uart0_outByte(input);
	}
	return;
}

/*e*/
u8* term_processCharacter(void)/*p;*/
{
	u32 input;
	TERM_STATE_NONE:
	input = i_get();
	//~ io_printhn(input); return;
	if (input == 0x1B) // start of escape sequence
	{
		goto TERM_STATE_ESC1;
	}
	if (input == 0x0D) // end of line
	{
		uart0_outByte('\n'); // restart line
		if (inputLine.cursor == 0)
		{
			io_prints("\r>");
			goto TERM_STATE_NONE;
		}
		// create process to handle input line
		inputLine.line[inputLine.size] = 0; // null terminate
		// save current line in to lines
		rom_func.memcpy(&lines[term.currentLine], &inputLine, 128);
		u8 *string = lines[term.currentLine].line;
		//~ task_enqueue(pengum_compile, lines[term.currentLine].line, 0);
		//~ task_enqueue(io_prints, "\r>", 0);
		i_push("\x08", 1);
		//~ i_push(lines[term.currentLine].line, inputLine.size+1);
		//~ io_printh(*(u32*)lines[term.currentLine].line);
		incrementCurrentLine();
		setZeroWait();
		setZero(&lines[term.currentLine], 128);
		inputLine.cursor = 0;
		inputLine.size = 0;
		//~ os_createProcess(io_prints, "\n>", 0);
		return string;
	}
	if (input == 0x7F || input == 0x08) // backspace input
	{
		deleteCharacter();
		printTerminalLine();
		goto TERM_STATE_NONE;
	}
	if (input != 0x00)
	{
		insertCharacter(input);
	}
	goto TERM_STATE_NONE;
	
	TERM_STATE_ESC1:
	input = i_get();
	if (input == 0x5B) // start of escape sequence
	{
		
	} else {
		goto TERM_STATE_NONE;
	}
	//~ if (term.state == TERM_STATE_ESC2)
	input = i_get();
	if (input == 0x41) // up arrow
	{
		clearLine();
		decrementCurrentLine();
		rom_func.memcpy(&inputLine, &lines[term.currentLine],  128);
		printTerminalLine();
	} else if (input == 0x42) { // down arrow
		clearLine();
		rom_func.memcpy(&inputLine, &lines[term.currentLine],  128);
		incrementCurrentLine();
		printTerminalLine();
	} else if (input == 0x43) { // right arrow
		if (inputLine.cursor >= inputLine.size) { goto TERM_STATE_NONE; }
		inputLine.cursor++;
		io_prints("\x1B\x5B\x43"); // echo arrow
	} else if (input == 0x44) { // left arrow
		if (inputLine.cursor == 0) { goto TERM_STATE_NONE; }
		inputLine.cursor--;
		uart0_outByte('\x08');
	}
	goto TERM_STATE_NONE;
}
