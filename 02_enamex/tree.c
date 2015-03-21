#include "tree.h"

struct node {
    struct node* left;
    struct node* right;
    struct node* subtree;
    char* content;
};

char* clone_content(char* content) {
    return strdup(content);
}

node new_node(char* content) {
    node n = NULL;
    n = (node)malloc( sizeof(struct node) );
    n -> left = NULL;
    n -> right = NULL;
    n -> subtree = NULL;
    n -> content = clone_content(content);
    return n;
}

static int compare(char* c1, char* c2) {
    return strncmp(c1, c2, strlen(c1));
}

static int __add_to(node* t, node n) {
    if(!*t) {
        *t = n;
        return 1;
    }

    int r = compare(n -> content, (*t) -> content);

    if(r == 0)
        return 0;
    else if (r > 0)
        return __add_to(& (*t) -> right, n);
    else
        return __add_to(& (*t) -> left, n);
}

int create_and_add_to(node* t, char* content) {
    return __add_to(t, new_node(content));
}

int add_to(node* t, node n) {
    return __add_to(t, n);
}

void __print_tree(node t, int flag) {
    if(!t) return;
    __print_tree(t -> left, flag);
    char* type = flag ? "SUBTREE:\t" : "TREE: ";
    printf("%s%s\n", type, t -> content);
    __print_tree(t -> subtree, 1);
    __print_tree(t -> right, flag);
}

void print_tree(node t) {
    __print_tree(t, 0);
}

int add_to_subtree(node* t, char* key, node n) {
    if(! *t)
        return 0;
    int r = compare( (*t) -> content, key);

    if( r == 0 ) {
        return add_to( &(*t) -> subtree, n);
    }
    else if (r > 0)
        return add_to_subtree( &(*t) -> left, key, n);
    else
        return add_to_subtree( &(*t) -> right, key, n);
}

int create_and_add_to_subtree(node* n, char* key, char* content) {
    return add_to_subtree(n, key, new_node(content));
}

int update_content(node* t, char* key, char* content) {
    if(*t == NULL)
        return 0;

    int r = compare( (*t) -> content, key);
    if( r == 0 ) {
        (*t) -> content = strdup(content);
        return 1;
    }
    else if (r > 0)
        return update_content( &(*t) -> left, key, content);
    else
        return update_content( &(*t) -> right, key, content);
}

int update_subtree(node* t, char* main_key, char* sub_key, char* content) {
    if(! *t)
        return 0;

    int r = compare( (*t) -> content, main_key);

    if( r == 0 ) {
        return update_content( &(*t) -> subtree, sub_key, content);
    }
    else if (r > 0)
        return update_content( &(*t) -> right, main_key, content);
    else
        return update_content( &(*t) -> left, main_key, content);
}

node* new_tree() {
    node* n = (node*)malloc(sizeof(node));
    *n = NULL;
    return n;
}


int main() {
    node* t = new_tree();
    create_and_add_to(t, "BRAGA");
    create_and_add_to(t, "GUIMARAES");
    create_and_add_to(t, "ALCANTARA");
    create_and_add_to(t, "BRAGA");
    create_and_add_to_subtree(t, "BRAGA", "DUME");
    create_and_add_to_subtree(t, "BRAGA", "REAL");
    print_tree(*t);
    return 0;
}


