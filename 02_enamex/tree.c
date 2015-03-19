#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node {
    char* content;
    struct node *left;
    struct node *right;
} node, *tree;

tree* new_node(char* content) {
    tree *new;
    *new = (tree)malloc( sizeof(struct node) );
    (*new) -> content = strdup(content);
    (*new) -> left = NULL;
    (*new) -> right = NULL;
    return new;
}

int _add_to(tree* t, tree n) {
    if(*t == NULL) {
        *t = n;
        return 1;
    }

    int r = strncmp( (*t) -> content, n -> content, strlen((*t) -> content + 1) );

    if (r == 0)
        return 0;
    else if (r > 0)
        return _add_to(&( (*t) -> right), n);
    else
        return _add_to(&( (*t) -> left), n);
}

int add_to(tree *t, char* content) {
    return _add_to(t, *new_node(content));
}

int print_tree(tree t, int i) {
    printf("### NODE %d:\nCONTENT: %s\n\n", ++i, t -> content);
    i = print_tree(t -> left, i + 1);
    i = print_tree(t -> right, i + 1);
    return i + 1;
}
/*
int main() {
    tree *t = NULL;
    t = new_node("BRAGA");
    printf("YO\n");
    add_to(t, "ALCANTARA");
    add_to(t, "PORTO");
    add_to(t, "LISBOA");
    print_tree(*t, 0);
    return 0;
}
*/

