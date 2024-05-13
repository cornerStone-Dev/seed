// headerGenerator.c
#include <stdio.h>   /* foundational lib */
#include <stdlib.h>  /* foundational lib */
#include <string.h>  /* foundational lib */
#include <stdbool.h> /* bool type */
#include <stdint.h>  /* std size int types */
typedef uint8_t  u8;
typedef int8_t   s8;
typedef uint32_t u32;
typedef int32_t  s32;
typedef uint64_t u64;
typedef int64_t  s64;
//~ #define NDEBUG
#include <assert.h>

typedef struct EndOfOutputs {
	u8  *internal;
	s8  *external;
} EndOfOutputs;

static u8*
loadFile(u8 *file_name)
{
	FILE *pFile;
	u8 *buffer;
	u64 fileSize, result;
	
	pFile = fopen(file_name, "rb");
	if (pFile==NULL) {
		fputs ("File error, cannot open source file\n",stderr);
		exit(1);
	}
	// obtain file size:
	fseek(pFile , 0 , SEEK_END);
	fileSize = ftell(pFile);
	rewind(pFile);
	// allocate memory to contain the whole file:
	buffer = malloc(fileSize+1);
	if (buffer == NULL) {
		fputs ("Memory error\n",stderr);
		exit(2);
	}
	// copy the file into the buffer:
	result = fread (buffer,1,fileSize,pFile);
	if (result != fileSize) {
		fputs ("Reading error\n",stderr);
		exit(3);
	}
	/* 0x00 terminate buffer, leave return in sub file */
	buffer[fileSize]=0;
	fclose(pFile);
	return buffer;
}

static s32
toUpper(s32 c)
{
	if (c >= 'a' && c <= 'z')
	{
		c -= 0x20;
	}
	return c;
}

static void
genHeaderGuardName(u8 *externalFileName, u8 *output)
{
	u64 length = strlen(externalFileName);
	// cut off ending
	length -= 2;
	u64 i;
	for (i = 0; i < length; i++)
	{
		output[i] = toUpper(externalFileName[i]);
	}
	output[i] = 0;
}


/*!re2c                              // start of re2c block
	
	// comments and white space
	scm = "//" [^\n\x00]* "\n";
	wsp = [ \r\t];
	
	identifier = [a-zA-Z_][a-zA-Z_0-9!$?-]*;
	
*/                                   // end of re2c block

static EndOfOutputs
generate(u8 *sourceCode, u8 *internalHeader, u8 *externalHeader)
{
	u8  *YYCURSOR = sourceCode;
	u8  *YYMARKER;
	u8  *start;
	u8  *exportTarget = 0;
	u64  length;

loop: // label for looping within the lexxer

	start = YYCURSOR;

	/*!re2c                          // start of re2c block **/
	re2c:define:YYCTYPE = "u8";      //   configuration that defines YYCTYPE
	re2c:yyfill:enable  = 0;         //   configuration that turns off YYFILL

	* { 
		goto loop;
	}

	[\x00] {
		EndOfOutputs e;
		e.internal = internalHeader;
		e.external = externalHeader;
		return e;
	}
	
	"//" {
		while(*YYCURSOR!='\n') { YYCURSOR++; }
		YYCURSOR++;
		goto loop;
	}
	
	"/*e*/" { // mark for export
		exportTarget = YYCURSOR;
		goto loop;
	}
	
	"/*i;*/" { // mark end of export and type
		if (exportTarget != 0)
		{
			length = start - exportTarget;
			memmove(internalHeader, exportTarget, length);
			internalHeader += length;
			*internalHeader++ = ';';
			exportTarget = 0;
		}
		else {
			printf("No matching opening tag!\n");
		}
		goto loop;
	}
	
	"/*p;*/" { // mark end of export and type
		if (exportTarget != 0)
		{
			length = start - exportTarget;
			memmove(externalHeader, exportTarget, length);
			externalHeader += length;
			*externalHeader++ = ';';
			exportTarget = 0;
		}
		else {
			printf("No matching opening tag!\n");
		}
		goto loop;
	}
	
	"/*i*/" { // mark end of export and type
		if (exportTarget != 0)
		{
			length = start - exportTarget;
			memmove(internalHeader, exportTarget, length);
			internalHeader += length;
			exportTarget = 0;
		}
		else {
			printf("No matching opening tag!\n");
		}
		goto loop;
	}
	
	"/*p*/" { // mark end of export and type
		if (exportTarget != 0)
		{
			length = start - exportTarget;
			memmove(externalHeader, exportTarget, length);
			externalHeader += length;
			exportTarget = 0;
		}
		else {
			printf("No matching opening tag!\n");
		}
		goto loop;
	}

	*/                               // end of re2c block	
}


int main(int argc, char **argv)
{
	(void)argc;
	(void)argv;
	if (argc < 2)
	{
		printf("Please provide name of source file.\n");
		return 1;
	}
	//~ printf("opening file %s.\n", argv[1]);
	u8 *sourceCode = loadFile(argv[1]);
	u8 *internal = malloc(32*1024);
	u8 *external = malloc(32*1024);
	u8 *output, *end;
	u8  preProcessorHeader[256];
	genHeaderGuardName((argv[1])+4, preProcessorHeader);
	
	//~ printf("Hello World!\n");
	EndOfOutputs e = generate(sourceCode, internal,
	external + sprintf(external,
		"#ifndef %s_HEADER_H\n"
		"#define %s_HEADER_H\n",
		preProcessorHeader, 
		preProcessorHeader)
	 );
	*e.internal++ = '\n';
	*e.internal = 0;
	*e.external++ = '\n';
	*e.external = 0;
	//~ printf("%s", output);
	output = internal;
	end = e.internal;
	// make new file locations
	u8 outFileName[256];
	strcpy(outFileName, "inc/");
	strcat(outFileName, (argv[1])+4);
	outFileName[strlen(outFileName) - 2] = 0;
	strcat(outFileName, "_i.h");
	FILE *outputFile = fopen ( outFileName, "w" );
	if (outputFile==NULL) {fputs ("File error",stderr); exit (1);}
	fwrite (output , sizeof(char), (end-output), outputFile);
	fflush (outputFile); 
	fclose (outputFile);
	output = external;
	e.external += sprintf(e.external,"#endif\n");
	end = e.external;
	outFileName[strlen(outFileName) - 4] = 0;
	strcat(outFileName, ".h");
	outputFile = fopen ( outFileName, "w" );
	if (outputFile==NULL) {fputs ("File error",stderr); exit (1);}
	fwrite (output , sizeof(char), (end-output), outputFile);
	fflush (outputFile); 
	fclose (outputFile);
	//~ printf("Goodbye World!\n");
	return 0;
}

