#include "../includes/vector.h"
#include <stdio.h>

typedef struct Publication_s {
	int x;
	int y;
	int z;
	char u;
} Publication;

typedef struct Brol_s {
	char a;
	char b;
	char c;
	int d;
} Brol;

int cmp(Brol x, Brol y) {
	if (x.d == y.d)
		return 0;
	else
		return 1;
}

VECTOR_DEF_HEADER(Publication)
VECTOR_DEF_HEADER(Brol)

VECTOR_DEF(Publication)
VECTOR_DEF(Brol)

int main() {
	PublicationVector pubVec = vecNew(Publication, 4);
	BrolVector brolVec = vecNew(Brol, 10);
	Publication testPub;
	Brol testBrol;
	int i;

	for (i = 0; i < 20; i++) {
		testPub.x = i*3;
		testPub.y = i*3 + 1;
		testPub.z = i*3 + 2;
		testPub.u = 'a' + i;

		testBrol.a = 'a' + i;
		testBrol.b = 'a' + i + 1;
		testBrol.c = 'a' + i + 2;
		testBrol.d = i;

		vecAppend(Brol, brolVec, testBrol);
		vecAppend(Publication, pubVec, testPub);
	}

	for (i = 0; i < 20; i++) {
		vecGet(Publication, pubVec, i, &testPub);
		printf("%d %d %d %c\n", testPub.x, testPub.y, testPub.z, testPub.u);
	}

	putchar('\n');

	for (i = 0; i < 20; i++) {
		vecGet(Brol, brolVec, i, &testBrol);
		printf("%c %c %c %d\n", testBrol.a, testBrol.b, testBrol.c, testBrol.d);
	}

	testBrol.d = 2000;
	testBrol.a = 'p';
	vecUpdate(Brol, brolVec, 2, testBrol);

	testBrol.d = 1;
	testBrol.a = 'j';
	vecGet(Brol, brolVec, 2, &testBrol);

	printf("%c %c %c %d\n", testBrol.a, testBrol.b, testBrol.c, testBrol.d);
 	
 	testBrol.d = 10;

 	i = vecFind(Brol, brolVec, &cmp, testBrol, &testBrol);
 	printf("- %d - %c %c %c %d\n", i, testBrol.a, testBrol.b, testBrol.c, testBrol.d);

	vecDestroy(Brol, brolVec);
	vecDestroy(Publication, pubVec);
	return 0;
}
