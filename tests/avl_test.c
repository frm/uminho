#include "../gestauts/modules/avl.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TOTAL 300000

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

AVL_DEF(Brol)
AVL_DEF(Publication)


int cmpBrol(Brol x, Brol y) {
    if (x.d > y.d){
        return 1;
    }
    else if (x.d < y.d){
        return -1;
    }
    else
        return 0;
}

int cmpPublication(Publication x, Publication y) {
    if (x.x < y.x){
        return 1;
    }
    else if (x.x > y.x) {
        return -1;
    }
    else
        return 0;
}

void printPublicationAVL(PublicationAVLNode node) {
    if (node) {
        if (avlGetLeftChild(Publication, node))
            printPublicationAVL(avlGetLeftChild(Publication, node));

        printf("%d %d\n", node->content.x, node->balance);

        if (avlGetRightChild(Publication, node))
            printPublicationAVL(avlGetRightChild(Publication, node));
    }
}

void printBrolAVL(BrolAVLNode node) {
    if (node) {
        if (avlGetLeftChild(Brol, node))
            printBrolAVL(avlGetLeftChild(Brol, node));

        /*printBrol(avlGetNodeContent(Brol, node));*/
        printf("%d %d\n", node->content.d, node->balance);

        if (avlGetRightChild(Brol, node))
            printBrolAVL(avlGetRightChild(Brol, node));
    }
}

int heightB(BrolAVLNode node) {
    int l, r;

    if (!node)
        return 0;

    l = heightB(avlGetLeftChild(Brol, node));
    r = heightB(avlGetRightChild(Brol, node));

    if (r > l) {
        printf("Value: %d Height: %d\n", node->content.d, r + 1);
        return r + 1;
    }
    else{
        printf("Value: %d Height: %d\n", node->content.d, l + 1);
        return l + 1;
    }
}

int heightP(PublicationAVLNode node) {
    int l, r;

    if (!node)
        return 0;

    l = heightP(avlGetLeftChild(Publication, node));
    r = heightP(avlGetRightChild(Publication, node));

    if (r > l)
        return r + 1;
    else
        return l + 1;
}

int main(void) {
    BrolAVL b;
    PublicationAVL p;
    Publication pu;
    Brol br;
    int i, j;

    b = avlNew(Brol, &cmpBrol);
    p = avlNew(Publication, &cmpPublication);

    srand(time(NULL));

    br.a = 'a';
    br.b = 'a';
    br.c = 'a';

    for (i = 0; i < TOTAL; i++) {

        j = rand();

        pu.x = j;
        pu.y = i;
        pu.z = i + 5;
        pu.u = 'a' + i;
        
        avlInsert(Publication, p, pu);
    }

    printf("SIMPLE LEFT ROTATION: \n\n");

    br.d = 1;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 3;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    avlDestroy(Brol, b);
    b = avlNew(Brol, &cmpBrol);

    printf("LEFT RIGHT ROTATION: \n\n");

    br.d = 3;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 1;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    avlDestroy(Brol, b);
    b = avlNew(Brol, &cmpBrol);

    printf("SIMPLE RIGHT ROTATION: \n\n");

    br.d = 3;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 1;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    avlDestroy(Brol, b);
    b = avlNew(Brol, &cmpBrol);

    printf("RIGHT LEFT ROTATION: \n\n");

    br.d = 1;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 3;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    avlInsert(Brol, b, br);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    printf("\n\nPublication AVL height: %d\n\nTotal Elements: %d\n\n", heightP(avlGetRoot(Publication, p)), TOTAL);

    avlDestroy(Publication, p);
    avlDestroy(Brol, b);
    return 0;
}