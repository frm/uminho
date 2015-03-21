#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _TREE_H
#define _TREE_H

typedef struct node *node;

node new_node(char* content);

int create_and_add_to(node* t, char* content);

int add_to(node* t, node n);

void print_tree(node t);

int add_to_subtree(node* t, char* key, node n);

int create_and_add_to_subtree(node* n, char* key, char* content);

int update_content(node* t, char* key, char* content);

int update_subtree(node* t, char* main_key, char* sub_key, char* content);

node* new_tree();

#endif

