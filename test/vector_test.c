#include "../gestauts/lib/headers/vector.h"
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

void deleteBrol(Brol b) {
	return;
}

void deletePub(Publication p) {
	return;
}

Brol cloneBrol(Brol b) {
	return b;
}

Publication clonePub(Publication p) {
	return p;
}

VECTOR_DEF(Publication)
VECTOR_DEF(Brol)

int main() {
	PublicationVector pubVec = vecNew(Publication, 4, &deletePub, &clonePub);
	BrolVector brolVec = vecNew(Brol, 10, &deleteBrol, &cloneBrol);
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
 	
	vecDestroy(Brol, brolVec);
	vecDestroy(Publication, pubVec);
	return 0;
}
