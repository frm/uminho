#include "../gestauts/lib/headers/stack.h"
#include <stdio.h>

typedef struct Brutal_s {
    int x;
    int y;
} *Brutal;

STACK_DEF(Brutal)

void deleteInt(Brutal n) {
    free(n);
    return;
}

Brutal cloneInt(Brutal n) {
    Brutal b;

    b = (Brutal)malloc(sizeof(struct Brutal_s));

    b->x = n->x;
    b->y = n->y;

    return b;
}

int main(void) {
    int i;
    Brutal b;
    BrutalStack stack;

    stack = stackBrutalNew(&deleteInt, &cloneInt);

    b = (Brutal)malloc(sizeof(struct Brutal_s));

    for (i = 0; i < 30; i++) {
        b->x = i;
        b->y = 30 - i;
        stackBrutalPush(stack, b);
    }

    for (i = 0; i < 30; i++) {
        stackBrutalPull(stack, &b);
        printf("%d %d\n", b->x, b->y);
        free(b);
    }

    stackBrutalDestroy(stack);

    return 0;
}