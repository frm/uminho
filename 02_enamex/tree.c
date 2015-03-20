#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node {
    struct node* left;
    struct node* right;
    char* content;
} *node;

char* clone_content(char* content) {
    return strdup(content);
}

node new_node(char* content) {
    node n = NULL;
    n = (node)malloc( sizeof(struct node) );
    n -> left = NULL;
    n -> right = NULL;
    n -> content = clone_content(content);
    return n;
}

int compare(char* c1, char* c2) {
    return strncmp(c1, c2, strlen(c1));
}

void collision(node t, node n) {
    printf("COLISION\n");
    return;
}

int __add_to(node* t, node n) {
    if(!*t) {
        *t = n;
        return 1;
    }

    int r = compare(n -> content, (*t) -> content);

    if(r == 0) {
        collision(*t, n);
        return 0;
    }
    else if (r > 0)
        return __add_to(& (*t) -> right, n);
    else
        return __add_to(& (*t) -> left, n);
}

int add_to(node* t, char* content) {
    return __add_to(t, new_node(content));
}

void print_tree(node t) {
    if(!t) return;
    print_tree(t -> left);
    printf("%s\n", t -> content);
    print_tree(t -> right);
}

node* new_tree() {
    node* n = (node*)malloc(sizeof(node));
    *n = NULL;
    return n;
}

int main() {
    node* t = new_tree();
    add_to(t, "BRAGA");
    add_to(t, "GUIMARAES");
    add_to(t, "ALCANTARA");
    add_to(t, "BRAGA");
    print_tree(*t);
    return 0;
}
