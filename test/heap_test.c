#include "../includes/heap.h"
#include <stdio.h>

typedef struct Brol_s {
    int x;
    int y;
} Brol;

HEAP_DEF_HEADER(Brol)
HEAP_DEF(Brol)

int cmpBrol(Brol x, Brol y) {
    if (x.x > y.x)
        return 1;
    else if (x.x < y.x)
        return -1;
    else
        return 0;
}

int main() {
    BrolHeap bheap;
    Brol b;
    int i;

    bheap = heapNew(Brol, 10, &cmpBrol);

   
    b.x = 5;
    b.y = 10;
    heapInsert(Brol, bheap, b);
    b.x = 8;
    b.y = 15;
    heapInsert(Brol, bheap, b);
    b.x = 1;
    b.y = 2;
    heapInsert(Brol, bheap, b);

    for (i = 0; i < 2; i++) {
        heapGet(Brol, bheap, &b);
        printf("%d %d\n", b.x, b.y);
    }

    putchar('\n');

    b.x = 9;
    b.y = 14;
    heapInsert(Brol, bheap, b);
    b.x = 1;
    b.y = 3;
    heapInsert(Brol, bheap, b);
    b.x = 12;
    b.y = 14;
    heapInsert(Brol, bheap, b);
    b.x = 8;
    b.y = 3;
    heapInsert(Brol, bheap, b);

    for (i = 0; i < 4; i++) {
        heapGet(Brol, bheap, &b);
        printf("%d %d\n", b.x, b.y);
    }

    heapDestroy(Brol, bheap);

    return 0;
}