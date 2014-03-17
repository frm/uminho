#include "vector.h"
#include <stdio.h>

typedef struct Publication_s {
	int x;
	int y;
	int z;
	char u;
} Publication;

VECTOR_DEF(Publication);

int main() {
	PublicationVector vec = vecNew(Publication, 4);
	Publication new;
	int i;

	for (i = 0; i < 20; i++) {
		new.x = i*3;
		new.y = i*3 + 1;
		new.z = i*3 + 2;
		new.u = 'a' + i;

		vecAppend(Publication, vec, new);
	}

	for (i = 0; i < 20; i++) {
		vecGet(Publication, vec, i, &new);

		printf("%d %d %d %c\n", new.x, new.y, new.z, new.u);
	}

	vecDelete(Publication, vec);
	return 0;
}
