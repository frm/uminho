#ifndef AUTHOR_INDEX_TREE_H_
#define AUTHOR_INDEX_TREE_H_

#include "../../lib/headers/avl.h"

typedef char* Author;

AVL_DEF_HEADER(Author, Author)

int insertAuthor(char* author);

int getListOfAuthorsBy(char initial, char** author_list, int number_displays, int* number_read);
void rewindAuthor(char initial);

void initAuthorTree();
void deleteAuthorIndexTree();

#ifdef DEBUG
    void printAuthorIndex();
#endif

#endif

