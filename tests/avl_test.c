#include "../gestauts/lib/headers/avl.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TOTAL 300000
#define newBrolAVL
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

AVL_DEF(Brol, int)
AVL_DEF(Publication, int)


int cmpBrol(int *n, Brol *p, Brol y) {
    int key;

    if (n)
        key = *n;
    else
        key = p->d;

    if (key > y.d){
        return 1;
    }
    else if (key < y.d){
        return -1;
    }
    else
        return 0;
}

int cmpPublication(int *n, Publication *p, Publication y) {
    int key;

    if (n)
        key = *n;
    else
        key = p->x;

    if (key < y.x){
        return 1;
    }
    else if (key > y.x) {
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

        printf("%d %c %c %c\n", node->content.d, node->content.a, node->content.b, node->content.c);

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

void colBrol(Brol *content, Brol *newContent) {
    content->a = newContent->a;
    content->b += 2;

    return;
}

void delBrol(Brol content){
    return;
}

Brol cloneBrol(Brol b) {
    return b;
}
int main(void) {
    BrolAVL b, b2;
    PublicationAVL p;
    Publication pu;
    Brol br;
    int i, j;

    b = avlNew(Brol, &cmpBrol, &colBrol, &delBrol, &cloneBrol);
    /*p = avlNew(Publication, &cmpPublication);*/

    srand(time(NULL));

    br.a = 'a';
    br.b = 'a';
    br.c = 'a';

    /*
    for (i = 0; i < TOTAL; i++) {

        j = rand();

        pu.x = j;
        pu.y = i;
        pu.z = i + 5;
        pu.u = 'a' + i;
        
        avlInsert(Publication, p, pu);
    }
    */

    printf("SIMPLE LEFT ROTATION: \n\n");

    br.d = 1;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 3;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    avlDestroy(Brol, b);
    b = avlNew(Brol, &cmpBrol, &colBrol, &delBrol, &cloneBrol);

    printf("LEFT RIGHT ROTATION: \n\n");

    br.d = 3;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 1;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    avlDestroy(Brol, b);
    b = avlNew(Brol, &cmpBrol, &colBrol, &delBrol, &cloneBrol);

    printf("SIMPLE RIGHT ROTATION: \n\n");

    br.d = 3;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 1;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    avlDestroy(Brol, b);
    b = avlNew(Brol, &cmpBrol, &colBrol, &delBrol, &cloneBrol);

    printf("RIGHT LEFT ROTATION: \n\n");

    br.d = 1;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 3;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    br.d = 2;
    i = avlInsert(Brol, b, br);
    printf("INSERT: %d\n", i);
    heightB(avlGetRoot(Brol, b));
    putchar('\n');

    /*printf("\n\nPublication AVL height: %d\n\nTotal Elements: %d\n\n", heightP(avlGetRoot(Publication, p)), TOTAL);*/

    /*avlDestroy(Publication, p);*/

    avlFind(Brol, b, 2, &br);

    br.a = 'd';
    br.d = 2;

    printf("INSERTING: d a a 2\nEXPECTED: d c a 2\n");

    i = avlInsert(Brol, b, br);

    br.a = 'x';

    avlFind(Brol, b, 2, &br);
    
    printf("RESULT: %c %c %c %d\n\n", br.a, br.b, br.c, br.d);

    br.a = 'x';
    br.b = 'y';
    br.c = 'z';
    br.d = 2;

    avlUpdate(Brol, b, br);

    br.a = 'd';

    avlFind(Brol, b, 2, &br);

    printf("INSERTING: x y z 2\nEXPECTED: x y z 2\n");

    printf("RESULT: %c %c %c %d\n\n", br.a, br.b, br.c, br.d);

    b2 = avlClone(Brol, b);

    printf("CLONING AVL\n");

    avlFind(Brol, b, 2, &br);

    printf("AVL1:\n");

    printBrolAVL(b->root);

    printf("AVL2:\n");

    printBrolAVL(b2->root);

    putchar('\n');

    printf("REPLACING NODE 3 ON AVL2 WITH:\n l o l\n\n");

    br.a = 'l';
    br.b = 'o';
    br.c = 'l';
    br.d = 3;

    avlUpdate(Brol, b2, br);

    printf("AVL1:\n");

    printBrolAVL(b->root);

    printf("AVL2:\n");

    printBrolAVL(b2->root);

    putchar('\n');

    for (i = 0; i < 50; i++) {

        br.a = rand() % 26 + 'a';
        br.b = rand() % 26 + 'a';
        br.c = rand() % 26 + 'a';
        br.d = rand() % 100;

        if (i % 2)
            avlInsert(Brol, b, br);
        else
            avlInsert(Brol, b2, br);
    }

    printf("AVL1:\n");

    printBrolAVL(b->root);

    printf("AVL2:\n");

    printBrolAVL(b2->root);

    putchar('\n');

    printf("DESTROYING AVL1\n\n");

    avlDestroy(Brol, b);

    printf("AVL2:\n");

    printBrolAVL(b2->root);

    printf("YIELD TEST:\n");

    while (!avlYield(Brol, b2, &br))
        printf("%d %c %c %c\n", br.d, br.a, br.b, br.c);    

    avlDestroy(Brol, b2);
    return 0;
}