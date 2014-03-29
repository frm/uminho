#include "../gestauts/lib/headers/avl.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct twice {
	int key;
	int val;
} Twice;

AVL_DEF(Twice, int)

Twice newTwice(int key, int val) {
	Twice t;
	t.key = key;
	t.val = val;
	return t;
}

void colidingTwice(Twice* a, Twice* b){
	(a -> val) += (b -> val);
}

void deleteTwice(Twice t) { }

Twice cloneTwice(Twice t) {
	Twice n;
	n.key = t.key;
	n.val = t.val;

	return t;
}

int compareTwice(int *n, Twice* t, Twice b) {
	int cmp;
	int key = n ? (*n) : (t -> key);

	if ( key > b.key ) cmp = 1;
	else if ( key < b.key ) cmp = -1;
	else cmp = 0;

	return cmp;
}

void printTwice(Twice t) {
	printf("KEY:%d VALUE:%d\n", t.key, t.val);
}

int getAVLHeight( TwiceAVLNode t) {
	int l, r;
	int h = 0;
	
	if (t) {

		l = getAVLHeight( avlTwiceGetLeftChild(t) );
		r = getAVLHeight( avlTwiceGetRightChild(t) );

		h = ( l > r ) ? (l + 1) : (r + 1);
	}

	return h;
}

void printTwiceAVL(TwiceAVLNode t) {
	if (t) {
		if ( avlTwiceGetLeftChild(t) )
			printTwiceAVL( avlTwiceGetLeftChild(t) );
		
		printTwice(t -> content);
		printf( "HEIGHT: %d\n\n", getAVLHeight(t) );
		
		if ( avlTwiceGetRightChild(t) )
			printTwiceAVL( avlTwiceGetRightChild(t) );
	}
}


void insert_update_test() {
	TwiceAVL t;
	int i, j;
	Twice tw;

	t = avlNew(Twice, &compareTwice, &colidingTwice, &deleteTwice, &cloneTwice);

	printf("### FIRST AVL ###\n");
	printf("INSERTING (1, 1), (2, 2), (3, 3), (4, 4)\n\n");
	
	for (i = 1; i < 5; i++) {
		tw = newTwice(i, i);
		j = avlInsert(Twice, t, tw);
		printf("THIS JUST IN: (%d, %d) WITH VALUE %d", i, i, j);
	}

	printTwiceAVL(t->root);

	printf("INSERTING (1, 2)\n");
	tw = newTwice(1, 2);
	j = avlInsert(Twice, t, tw);
	printf("UPDATE VALUE %d", j);
	printTwiceAVL(t->root);

	avlTwiceDestroy(t);
}

void rotation_find_test(){
	int i;
	Twice tw;
	Twice *tn = (Twice *)malloc(sizeof(Twice));
	TwiceAVL t = avlNew(Twice, &compareTwice, &colidingTwice, &deleteTwice, &cloneTwice);

	printf("### SECOND AVL ###\n");
	printf("INSERTING (i,i) up to 10\n\n");

	for (i = 1; i < 11; i++) {
		tw = newTwice(i, i);
		avlInsert(Twice, t, tw);
	}

	printTwiceAVL(t->root);

	printf("FINDING 5 AND 11\n");
	avlTwiceFind(t, 5, tn);
	printf("FOUND (%d, %d)\n", tn->key, tn->val);
	if (avlTwiceFind(t, 11, tn) > 0) printf("FOUND 11\n");
	else printf("11? NO SUCH THING\n");

	printf("UPDATING (2, 2) TO (2, 3) and 11\n");
	tw.key = 2;
	tw.val = 3;
	avlTwiceUpdate(t, tw);
	printTwiceAVL(t -> root);

	tw.key = 11;
	tw.val = 0;
	if (avlTwiceUpdate(t, tw) > 0) printf("UPDATED 11\n");
	else printf("NO SUCH THING AS 11\n");

	avlTwiceDestroy(t);
}


void clone_destroy_test() {
	int i = 0;
	Twice tw;
	TwiceAVL t, clone;
	t = avlNew(Twice, &compareTwice, &colidingTwice, &deleteTwice, &cloneTwice);

	printf("\n### THIRD AVL ###\n");

	while ( i < 10 ) {
		tw = newTwice(i, i);
		avlTwiceInsert(t, tw);
		i++;
	}

	clone = avlTwiceClone(t);

	printf("ORIGINAL AVL ADDRESS: %d ROOT ADDRESS: %d\n", t, (t->root) );
	printf("CLONED AVL ADDRESS: %d ROOT ADDRESS: %d\n", clone, (clone->root) );

	avlTwiceDestroy(t);
	avlTwiceDestroy(clone);

	printf("ORIGINAL AVL ADDRESS: %d ROOT ADDRESS: %d\n", t, (t->root) );
	printf("CLONED AVL ADDRESS: %d ROOT ADDRESS: %d\n", clone, (clone->root) );

}

void yield_test() {
	Twice tw, ary[20];
	int i;
	TwiceAVL t = avlNew(Twice, &compareTwice, &colidingTwice, &deleteTwice, &cloneTwice);

	for (i = 0; i < 20; i++) {
		tw = newTwice(i, i);
		avlTwiceInsert(t, tw);
	}

	for (i = 0; i < 10; i++)
		avlYield(Twice, t, ary + i);
	
	printf("PRINTING FIRST 10 ELEMENTS\n");
	for (i = 0; i < 10; i++)
		printf("#%d : (%d, %d)\n", i, ary[i].key, ary[i].val);

	for (i = 10; i < 20; i++)
		avlYield(Twice, t, ary + i);

	printf("\nPRINTING LAST 10 ELEMENTS\n");
	for (i = 10; i < 20; i++)
		printf("#%d : (%d, %d)\n", i, ary[i].key, ary[i].val);
}

int main() {
	insert_update_test();
	rotation_find_test();
	clone_destroy_test();
	return 0;
}
