#include "../gestauts/modules/avl.h"
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

AVL_DEF(Publication);
AVL_DEF(Brol);

int cmpBrol(Brol x, Brol y) {
    if (x.d > y.d)
        return 1;
    else if (x.d < y.d)
        return -1;
    else
        return 0;
}

int cmpPublication(Publication x, Publication y) {
    if (x.x > y.x)
        return 1;
    else
        return 2;
}

void printBrol(Brol b) {
    printf("%d %c %c %c\n", b.d, b.a, b.b, b.c);
} 

void printBrolAVL(BrolAVLNode node) {
    if (node) {
        if (avlGetLeftChild(Brol, node))
            printBrolAVL(avlGetLeftChild(Brol, node));

        printBrol(avlGetNodeContent(Brol, node));

        if (avlGetRightChild(Brol, node))
            printBrolAVL(avlGetRightChild(Brol, node));
    }
}

int main(void) {
    BrolAVL b;
    Brol br;
    int i;

    b = avlNew(Brol, &cmpBrol);

    for (i = 0; i < 10; i++) {
        br.a = 'a' + i;
        br.b = 'a' + i + 1;
        br.c = 'a' + i + 2;
        if (i % 2 == 0)
            br.d = i;
        else
            br.d = -i;

        avlInsert(Brol, b, br);


    }

    printBrolAVL(b->root);

    avlDestroy(Brol, b);
    return 0;
}