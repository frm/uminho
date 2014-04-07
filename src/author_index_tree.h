#ifndef AUTHOR_INDEX_TREE_H_
#define AUTHOR_INDEX_TREE_H_

#include <avl.h>

typedef char* Author;

AVL_DEF_HEADER(Author, Author)

typedef AuthorAVL AuthorTree;

AuthorTree authorTreeNew();
void authorTreeDestroy(AuthorTree);

int authorTreeInsert(AuthorTree, Author);
int authorTreeYield(AuthorTree, Author *);
void authorTreeRewindGenerator(AuthorTree);

#ifdef DEBUG
    void printAuthorIndex();
#endif

#endif

