#include "../gestauts/lib/headers/avl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef char* String;

AVL_DEF(String, String)

String cloneString(String str) {
	int size = strlen(str) + 1;

	String clone = (String)malloc(sizeof(char) * size);
	strncpy(clone, str, size);

	return clone;
}

int compareString(String* n, String* a, String b) {
	String key = n ? (*n) : (*a);
	int cmp = strcmp(key, b);
	
	printf("\nCOMPARED >>> %s <<< WITH >>> %s <<< AND GOT %d", key, b, cmp);

	return cmp;
}


void printStringAVL(StringAVLNode t) {
	if (t) {
		if ( avlStringGetLeftChild(t) )
			printStringAVL( avlStringGetLeftChild(t) );
		
		printf("%s\n", t -> content);

		if ( avlStringGetRightChild(t) )
			printStringAVL( avlStringGetRightChild(t) );
	}
}

int main() {
	StringAVL t = avlNewComplete(String, &compareString, NULL, NULL, &cloneString);

	avlStringInsert(t, "ABC");
	avlStringInsert(t, "Abacate");
	avlStringInsert(t, "Abc Def");
	avlStringInsert(t, "A lot of fruits");
	avlStringInsert(t, "Ananas");
	avlStringInsert(t, "Acb Fed");
	avlStringInsert(t, "Abacaxi");
	avlStringInsert(t, "Abacate");
	

	putchar('\n');
	putchar('\n');
	printStringAVL( t->root );
	printf("%d\n", strcmp("ABCD", "ZY"));
}
