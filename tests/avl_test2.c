#include "../gestauts/lib/headers/avl.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#define STRCAP				50

/* Defining a twin */

typedef struct twin {
	int i;
	char c[50];
	int index;
} Twin;

/* Apparently need to add a cloning, deleting, comparing and colision */

int getTwinKey(Twin t) {
	return t.i;
}

char* getTwinStr(Twin t) {
	char* str = (char*)malloc(sizeof(char) * t.index);
	strncpy(str, t.c, t.index + 1);
	return str;
}

int setTwinStr(Twin *t, char* str) {
	int size = strlen(str) + 1;
	if (size >= STRCAP) return -1;

	strncpy( t->c, str, size );
	return 0;
}

int appendTwinStr(Twin *t, char* str) {
	int size = strlen(str) + 1;
	if (size + t->index >= STRCAP) return -1;

	strncpy(t->c + t->index, str, size);
	return 0;
}

Twin* newTwin() {
	Twin* t =(struct twin*)malloc(sizeof(struct twin));
	t -> i = 0;
	(t -> c)[0] = '\0';
	t -> i = 0;
	return t;
}

void deleteTwin(Twin* t) {
	free(t);
}

int cmpTwin(Twin a, Twin b) {
	int res;
	if (a.i > b.i) res = -1;
	else if (a.i < b.i) res = 1;
	else res = 0;

	return res;
}

void colidingTwin(Twin* a, Twin* b) {
	strncpy(a->c + a->index, b -> c, strlen(b -> c) + 1);
}

Twin* newTwinFrom(Twin *a) {
	Twin *t = newTwin();
	t -> i = a -> i;
	t -> index = a -> index;
	strncpy(t->c, a -> c, sizeof(char)* a -> index);
	return t;
}

void printTwin(Twin t) {
	printf("KEY: %d\nSTR:%s\nINDEX:%d\n", t.i, t.c, t.index);
}

/* End of Twin definition, starting testing the AVL */

AVL_DEF(Twin, int)

int main() {
	TwinAVL tavl;
	Twin* fst, snd, trd;
	int val;

	tavl = avlNew(Twin, &cmpTwin, &colidingTwin, &deleteTwin, &newTwinFrom);
	fst = newTwin();
	snd = newTwinFrom(fst);
	
	val = avlInsert(Twin, tavl, *fst);
	printf("INSERTED FST WITH VALUE:%d\n", val);
	val = avlInsert(Twin, tavl, *snd);
	printf("INSERTED SND WITH VALUE:%d\n", val);

	for (val = 0; val < 40; val++) {

		trd = newTwin();
		trd -> i = val;
		avlInsert(Twin, tavl, *trd);
	}


	return 0;
}
