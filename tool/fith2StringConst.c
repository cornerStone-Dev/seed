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
#define NDEBUG
#include <assert.h>

typedef struct Tree {
	struct Tree	*next[2];
	void		*value;
	u8		type;
	u8		tClass;
	u8		level;
	u8		keyLen;
	u8		key[4];
} Tree;

static Tree nil = { {&nil, &nil}, 0,0,0,0,0,{0,0,0,0}};


static Tree*
skew(Tree *root)
{
	if (root->level == root->next[0]->level)
	{
		Tree *save = root->next[0];
		root->next[0] = save->next[1];
		save->next[1] = root;
		root = save;
	}
	return root;
}

static Tree*
split(Tree *root)
{
    if (root->level == root->next[1]->next[1]->level)
    {
        Tree *save = root->next[1];
        root->next[1] = save->next[0];
        save->next[0] = root;
        root = save;
        root->level++;
    }
    return root;
}

static inline s32
keyCmp(u8 *key1, u8 *key2, s32 key1Len, s32 key2Len)__attribute__((always_inline));

static inline s32
keyCmp(u8 *key1, u8 *key2, s32 key1Len, s32 key2Len)
{
	s32 c1, c2, i = 0;
	
	while(1){
		c1=key1[i];
		c2=key2[i++];
		c1-=c2;
		if (c1 != 0) { return c1; }
		if (i >= key1Len) { return key1Len - key2Len; }
	}
}

static u32
treeSize(u32 keyLen)
{
	return sizeof(Tree) + (keyLen / 4 * 4); // round up to 4 bytes
}

static Tree*
makeNode(u8 *key, u32 keyLen, void *value)
{
	Tree *tree;
	u32      i = 0;
	tree = (Tree*)((((uintptr_t)malloc(treeSize(keyLen)+16))+15)&-16);
	tree->next[0] = tree->next[1] = &nil;
	tree->value   = value;
	tree->level   = 1;
	tree->keyLen  = keyLen;
	do {tree->key[i] = key[i]; i++; } while(i < keyLen);
	tree->key[keyLen] = 0; // null terminate
	return tree;
}

Tree*
tree_find(Tree *tree, u8 *key, u32 keyLen)
{
	if (tree != 0) {
	while (tree != &nil) {
		s32 result = keyCmp(key, tree->key, keyLen, tree->keyLen);
		if (result == 0) {
			return tree; // target found
		} else if (result > 0) {
			tree = tree->next[1];
		} else {
			tree = tree->next[0];
		}
	}}
	return 0;
}

void
tree_free(Tree *tree)
{
	if (tree == &nil || tree == 0) { return; }
	tree_free(tree->next[0]);
	tree_free(tree->next[1]);
	free(tree);
}

void
tree_print(Tree *tree, u32 indent)
{
	if(tree == &nil || tree == 0)
	{
		//~ for(u32 i=0; i<indent;i++)
		//~ {
			//~ io_prints("  ");
		//~ }
		//~ io_prints("NIL\n");
		return;
	}
	
	tree_print(tree->next[1], indent+1);
	for(u32 i=0; i<indent;i++)
	{
		printf("  ");
	}
	//~ io_printi(tree->level);
	printf("%s",(char*)tree->key);printf("\n");
	//~ printf("%s:%ld:%d\n",tree->key, root->val, root->level);
	tree_print(tree->next[0], indent+1);
}

// returns a tree if one already exists with the same key, otherwise 0
Tree*
tree_add(Tree **treep, u8 *key, u32 keyLen, void *value)
{
	Tree *tree = *treep;
	Tree **roots[21];
	Tree ***tCursor = roots;
	*tCursor++ = treep;
	// go down tree until you find the insertion point
	if (tree != 0) {
	while (tree != &nil)
	{
		//~ io_printi(tree->level); io_prints(" ");
		s32 result = keyCmp(key, tree->key, keyLen, tree->keyLen);
		if (result == 0) { return tree; }
		if (result > 0)
		{
			*tCursor++ = &tree->next[1];
			tree = tree->next[1];
		} else {
			*tCursor++ = &tree->next[0];
			tree = tree->next[0];
		}
	}}
	// tree == &nil && roots[i-1] is our root
	tCursor--;
	**tCursor = makeNode(key, keyLen, value);

	// rebalance on the way up the tree
	while (tCursor > roots)
	{
		tree = (Tree*)(((uintptr_t)*tCursor-->>4)<<4);
		tree = skew(tree);
		tree = split(tree);
		**tCursor = tree;
	}
	return 0;
}

#if 0	// removed because of work around used in makeNode to get an aligned poiner
	// on 32 bit systems
// returns value of deleted tree or 0 if none was found
void*
tree_del(Tree **treep, u8 *key, u32 keyLen)
{
	Tree *tree = *treep;
	Tree **roots[21];
	Tree ***tCursor = roots;
	*tCursor++ = treep;
	// go down tree until you find the deletion target
	if (tree == 0) { return 0; }
	while (1)
	{
		if (tree == &nil) { return 0; }
		s32 result = keyCmp(key, tree->key, keyLen, tree->keyLen);
		if (result == 0) { break; }
		if (result > 0)
		{
			*tCursor++ = &tree->next[1];
			tree = tree->next[1];
		} else {
			*tCursor++ = &tree->next[0];
			tree = tree->next[0];
		}
	}
	// *roots[i] == garbage, *roots[i-1] == nil, *roots[i-2] == last node parent
	tCursor--;
	Tree **targetParent = *tCursor;
	Tree *target = tree;
	void *value = target->value;
	if (target->level == 1)
	{
		// if target is a leaf get rid of it, this is majority of cases
		*targetParent = target->next[1];
		free(target);
	} else {
		// target is not a leaf, find predecessor, swap, remove
		tCursor++;
		Tree ***heirSpot = tCursor++;
		Tree **parent = &target->next[0];
		Tree *heir = target->next[0];
		while (heir->next[1] != &nil)
		{
			parent = &heir->next[1];
			*tCursor++ = parent;
			heir = heir->next[1];
			
		}
		tCursor--;
		// if heir->next[1]==&nil && heir->level==1 then heir->next[0]==&nil
		// swap with target
		*targetParent = heir;
		*parent = &nil;
		heir->next[0] = target->next[0];
		heir->next[1] = target->next[1];
		heir->level   = target->level;
		// add in heir into the array of pointers
		*heirSpot = &heir->next[0];
		free(target);
	}

	// rebalance up the tree
	while (tCursor > roots)
	{
		tree = (Tree*)(((uintptr_t)*tCursor-->>4)<<4);
		// see if we need a rebalance after the deletion
		if (   tree->next[0]->level < tree->level - 1
			|| tree->next[1]->level < tree->level - 1)
		{
			tree->level--;
			if (tree->next[1]->level > tree->level)
			{
				tree->next[1]->level = tree->level;
			}
			tree = skew(tree);
			tree->next[1] = skew(tree->next[1]);
			if (tree->next[1] != &nil) {
				tree->next[1]->next[1] = skew(tree->next[1]->next[1]);
			}
			tree = split(tree);
			tree->next[1] = split(tree->next[1]);
			**tCursor = tree;
		}
	}
	return value;
}
#endif

static void tree_destroyr(Tree *root)
{
	if (root == 0 || root == &nil) { return; }
	tree_destroyr(root->next[0]);
	tree_destroyr(root->next[1]);
	free(root->value);
	free(root);
}

void tree_destroy(Tree **rootp)
{
	Tree *root = *rootp;
	if (root == 0) { return; }
	*rootp = 0;
	tree_destroyr(root);
}

static Tree *filesIncluded;

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

static u8*
getNextLine(u8 *doc)
{
	while(*doc!='\n'&&*doc!=0){doc++;}
	return doc;
}

static u8*
outputSourceCode(u8 *name, u8 *out);

static u8*
outputLine(u8 *doc, u8 *out)
{
	// inlcude logic
	if (doc[0]=='\\'&&doc[1]=='#')
	{ 
		u8 *putZero = doc + 2;
		while (*putZero > ' ') { putZero++; }
		*putZero++ = 0;
		return outputSourceCode(doc + 2, out);
	}
	if (*doc=='\\'||*doc=='\n'||*doc==0) { return out; }
	if (doc[0]=='/'&&doc[1]=='/') { return out; }
	*out++ = '\"';
	while(*doc!='\n'&&*doc!=0)
	{
		switch (*doc)
		{
		case 0 ... 31:	*out++ = ' '; break;
		case '\\':	*out++ = '\\'; *out++ = '\\'; break;
		case '\"':	*out++ = '\\'; *out++ = '\"'; break;
		default: *out++ = *doc; break;
		}
		doc++;
	}
	*out++ = '\\';
	*out++ = 'n';
	*out++ = '\"';
	*out++ = '\n';
	return out;
}

static u8*
outputSourceCode(u8 *name, u8 *out)
{
	Tree *exists = tree_add(&filesIncluded,name, strlen(name), 0);
	if (exists) { return out; }
	printf("opening file %s.\n", name);
	u8 *sourceCode = loadFile(name);
	u8 *start = sourceCode;
	u8 *end;
	printf("Starting the copy.\n");
	while (1)
	{
		end = getNextLine(start);
		u8 endChar = *end;
		out = outputLine(start, out);
		start = end + 1;
		if (endChar == 0) { break; }
	}
	free(sourceCode);
	return out;
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
	u8 *fileName = strdup(argv[1]);
	argv[1][strlen(argv[1]) - 5] = 0;
	u8 *outputData = calloc(512*1024,1);
	
	strcpy(outputData, "char *G");
	strcat(outputData, (argv[1]));
	strcat(outputData, "= ");
	u8 *cursor = outputData + strlen(outputData);
	
	
	cursor = outputSourceCode(fileName, cursor);
	
	*cursor++ = ';';
	*cursor = 0;
	printf("Copy finished.\n");
	
	// make new file locations
	u8 outFileName[256];
	strcpy(outFileName, "../src/");
	strcat(outFileName, (argv[1]));
	strcat(outFileName, ".c");
	// write out results
	FILE *outputFile = fopen ( outFileName, "w" );
	if (outputFile==NULL) {fputs ("File error",stderr); exit (1);}
	fwrite (outputData , sizeof(char), (cursor-outputData), outputFile);
	fflush (outputFile); 
	fclose (outputFile);
	return 0;
}

