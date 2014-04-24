#ifndef AUTHOR_TREE_H_
#define AUTHOR_TREE_H_

#include <avl.h>

#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )

typedef char* Author;

AVL_DEF_HEADER(Author, Author)

typedef AuthorAVL AuthorTree;

AuthorTree authorTreeNew();
void authorTreeDestroy(AuthorTree);

int authorTreeInsert(AuthorTree, Author);
int authorTreeExists(AuthorTree, Author);
int authorTreeYield(AuthorTree, Author*);
void authorTreeRewindGenerator(AuthorTree);
AuthorTree authorTreeClone(AuthorTree tree);
Author* authorTreeToString(AuthorTree, int*);

#ifdef DEBUG
    void printAuthorIndex();
#endif

#endif

