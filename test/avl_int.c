#include "../gestauts/lib/headers/avl.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

AVL_DEF(int, int)

void colidingInt (int* a, int* b) {
	(*a) = (*a) + (*b);
}

int cloneInt(int a) { return a; }

int compareInt(int *n, int *a, int b) {
	int cmp;
	int key = n ? (*n) : (*a);

	if ( key < b) cmp = -1;
	else if (key > b) cmp = 1;
	else cmp = 0;

	return cmp;
}


int getAVLHeight( intAVLNode t) {
	int l, r;
	int h = 0;
	
	if (t) {

		l = getAVLHeight( avlintGetLeftChild(t) );
		r = getAVLHeight( avlintGetRightChild(t) );

		h = ( l > r ) ? (l + 1) : (r + 1);
	}

	return h;
}

void printintAVL(intAVLNode t) {
	if (t) {
		if ( avlintGetLeftChild(t) )
			printintAVL( avlintGetLeftChild(t) );
		
		printf("Node contains %d\n", t -> content);
		printf( "HEIGHT: %d\n\n", getAVLHeight(t) );
		
		if ( avlintGetRightChild(t) )
			printintAVL( avlintGetRightChild(t) );
	}
}

int main() {
	intAVL t, it;
	int a, b;
	int i, j;
	int* keeper;

	keeper = (int *)malloc(sizeof(int));

	t = avlNew(int, &compareInt, &colidingInt, NULL, &cloneInt);

	it = avlNew(int, &compareInt, &colidingInt, NULL, &cloneInt);

	a = 1; b = 1;
	
	printf("### FIRST AVL ###\n");
	j = avlInsert(int, t, a);
	printf("INSERTED a=1 WITH VALUE %d\n", j);
	j = avlInsert(int, t, b);
	printf("INSERTED b=1 WITH VALUE %d\n", j);

	printintAVL(t->root);

	putchar('\n');
	printf("### SECOND AVL### \n");
	for (i = 0; i < 15; i++)
		avlInsert(int, it, i);

	printintAVL(it->root);
	
	printf("### Finding and retrieving 0 ###\n");

	avlintFind(it, 0, keeper);
	printf("FOUND: %d\n", *keeper);

	avlDestroy(int, t);
	avlDestroy(int, it);
	free(keeper);

	return 0;
}
