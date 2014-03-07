#include <stdio.h>
#include "../gestauts/lib/headers/avl.h"
#include <string.h>

typedef char* Author;

AVL_DEF(Author, Author)

int cmp(Author *key, Author *notkey, Author shit) {
    Author lol;

    if (key)
        lol = *key;
    else
        lol = *notkey;

    return strcmp(lol, shit);
}

Author clone(Author a) {
    Author n = (Author)malloc(sizeof(char) * (strlen(a) + 1));

    strncpy(n, a, strlen(a) + 1);

    return n;
}

void del(Author a) {
    free(a);
}

int main() {
    AuthorAVL avl;
    int i;
    char temp[50];
    Author ret;

    avl = avlNewComplete(Author, &cmp, NULL, &del, &clone);

    for (i = 0; i < 6; i++) {
        scanf("%[^\n]\n", temp);

        avlInsert(Author, avl, temp);
    }

    putchar('\n');

    for (i = 0; i < 6; i++) {
        avlYield(Author, avl, &ret);

        printf("%s\n", ret);
    }

    return 0;
}