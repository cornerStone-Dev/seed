// tree.c
#include "../localTypes.h"

//~ #define TREE_INVARIANT_TESTING

//~ #if 0

static Tree nil = { {&nil, &nil}, };


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
	tree = zalloc(treeSize(keyLen));
	tree->next[0] = tree->next[1] = &nil;
	tree->value   = value;
	tree->level   = 1;
	tree->keyLen  = keyLen;
	do {tree->key[i] = key[i]; i++; } while(i < keyLen);
	tree->key[keyLen] = 0; // null terminate
	return tree;
}

/*e*/Tree*
tree_find(Tree *tree, u8 *key, u32 keyLen)/*p;*/
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

/*e*/void
tree_free(Tree *tree)/*p;*/
{
	if (tree == &nil || tree == 0) { return; }
	tree_free(tree->next[0]);
	tree_free(tree->next[1]);
	free(tree);
}

/*e*/void
correctParams(Tree *tree, u32 index)/*p;*/
{
	if (tree == &nil || tree == 0) { return; }
	correctParams(tree->next[0], index);
	tree->value = (void*)(index - (u32)tree->value);
	correctParams(tree->next[1], index);
}


/*e*/void
tree_print(Tree *tree, u32 indent)/*p;*/
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
		io_prints("  ");
	}
	//~ io_printi(tree->level);
	io_printsn(tree->key);
	//~ printf("%s:%ld:%d\n",tree->key, root->val, root->level);
	tree_print(tree->next[0], indent+1);
}

// returns a tree if one already exists with the same key, otherwise 0
/*e*/Tree*
tree_add(Tree **treep, u8 *key, u32 keyLen, void *value)/*p;*/
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
		tree = (Tree*)(((u32)*tCursor-->>3)<<3);
		tree = skew(tree);
		tree = split(tree);
		**tCursor = tree;
	}
	return 0;
}

// returns value of deleted tree or 0 if none was found
/*e*/void*
tree_del(Tree **treep, u8 *key, u32 keyLen)/*p;*/
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
		tree = (Tree*)(((u32)*tCursor-->>3)<<3);
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

static void tree_destroyr(Tree *root)
{
	if (root == 0 || root == &nil) { return; }
	tree_destroyr(root->next[0]);
	tree_destroyr(root->next[1]);
	free(root->value);
	free(root);
}

/*e*/void tree_destroy(Tree **rootp)/*p;*/
{
	Tree *root = *rootp;
	if (root == 0) { return; }
	*rootp = 0;
	tree_destroyr(root);
}

//~ #endif

#define NUM_TREES 201

#ifdef TREE_INVARIANT_TESTING

/*e*/void
tree_verify(Tree *tree)/*p;*/
{
	if(tree == &nil) { return; }
	// The following invariants hold for AA trees
	// The level of every leaf node is one.
	if (tree->next[0] == &nil && tree->next[1] == &nil && tree->level != 1)
	{
		io_printsn("leaf node that is not 1");
	}
	// The level of every left child is exactly one less than that of its parent
	if (tree->next[0] != &nil && tree->next[0]->level != (tree->level-1))
	{
		io_printsn("left child is NOT one less than that of its parent");
	}
	// The level of every right child is equal to or one less than that of its parent.
	if (tree->next[1] != &nil 
		&& (   (tree->next[1]->level != (tree->level-1)) 
			&& (tree->next[1]->level != tree->level ) ) )
	{
		io_printsn("right child is NOT one less or equal than that of its parent");
	}
	// The level of every right grandchild is strictly less than that of its grandparent.
	if (tree->next[1] != &nil && tree->next[1]->next[1] != &nil
		&& tree->next[1]->next[1]->level >=  tree->level )
	{
		io_printsn("right grandchild is NOT strictly less than");
	}
	// Every node of level greater than one has two children.
	if (tree->level > 1 && (tree->next[0] == &nil || tree->next[1] == &nil) )
	{
		io_printsn("node of level greater than one has < two children");
	}
	tree_verify(tree->next[0]);
	tree_verify(tree->next[1]);
}

/*e*/
void treeTest(void)/*p;*/
{
	Tree *tree = 0;
	avlNode *node = 0;
	u8 buff[64];
	u8 *cursor;
	printMemStats();
	for (s32 i = 0; i < NUM_TREES; i++)
	{
		cursor = i2s(i, buff);
		startSysTimer();
		//~ tree = tree_insert(tree, buff, cursor - buff, 0);
		tree_add(&tree, buff, cursor - buff, 0);
		//~ avl_insert(&node, buff, cursor - buff, 0);
		io_printin(endSysTimer());
		tree_verify(tree);
	}
	io_printsn("***********************************************");
	//~ tree_print(tree, 0);
	printMemStats();
	
	for (s32 i = 0; i < NUM_TREES / 2; i++)
	{
		cursor = i2s(i, buff);
		//~ startSysTimer();
		//~ tree = tree_remove(tree, buff, cursor - buff);
		tree_del(&tree, buff, cursor - buff);
		//~ avl_delete(&node, buff, cursor - buff);
		//~ io_printin(endSysTimer());
		//~ tree_verify(tree);
	}
	//~ io_printsn("***********************************************");
	//~ tree_print(tree, 0);
	for (s32 i = NUM_TREES / 2; i < NUM_TREES; i++)
	{
		cursor = i2s(i, buff);
		//~ startSysTimer();
		//~ tree = tree_remove(tree, buff, cursor - buff);
		tree_del(&tree, buff, cursor - buff);
		//~ avl_delete(&node, buff, cursor - buff);
		//~ u32 res = endSysTimer();
		//~ io_printi(i);io_prints("  "); 
		//~ io_printin(res);
		//~ tree_verify(tree);
	}
	io_printsn("***********************************************");
	//~ tree_print(tree, 0);
	printMemStats();
}
#endif
