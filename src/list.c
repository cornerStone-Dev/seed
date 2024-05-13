// list.c
/*e*/
typedef struct List {
	struct List *next;
} List;/*p*/

// add to front of the list
/*e*/
void*
list_append(void *elementp, void *listp)/*p;*/
{
	List *list    = listp;
	List *element = elementp;
	if (list == 0)
	{
		// the list is currently empty
		element->next = element;
	} else {
		// the list has 2 or more items
		element->next = list->next;
		list->next = element;
	}
	return element;
}

// add to front of the list
/*e*/
void*
list_prepend(void *elementp, void *listp)/*p;*/
{
	List *list    = listp;
	List *element = elementp;
	if (list == 0)
	{
		// the list is currently empty
		element->next = element;
	} else {
		// the list has 2 or more items
		element->next = list->next;
		list->next = element;
		element = list;
	}
	return element;
}

// remove first from list, needs a List **list
/*e*/
void*
list_removeFirst(void *listpp)/*p;*/
{
	List **listAddr  = listpp;
	List *listEnd    = *listAddr;
	if (listEnd == 0) { return 0; }
	List *listFront  = listEnd->next;
	if (listEnd == listFront)
	{
		// this is the only item in the queue, set list to zero
		*listAddr = 0;
	} else {
		// two or more items in the queue
		listEnd->next = listFront->next;
	}
	return listFront;
}

// get first from list, needs a List *list
/*e*/
void*
list_getFirst(void *listp)/*p;*/
{
	List *listEnd    = listp;
	if (listEnd == 0) { return 0; }
	List *listFront  = listEnd->next;
	return listFront;
}
