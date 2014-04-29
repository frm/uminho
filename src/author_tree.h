#ifndef AUTHOR_TREE_H_
#define AUTHOR_TREE_H_

#include <author.h>

#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )

typedef struct AuthorAVL_s *AuthorTree;

/* Creating a new author AVL */
AuthorTree authorTreeNew();

/* Deleting an author AVL */
void authorTreeDestroy(AuthorTree);

/* Inserting to an author AVL */
int authorTreeInsert(AuthorTree, Author);

/* Checking if an author exits in an author AVL */
int authorTreeExists(AuthorTree, Author);

/* Yielding authors from an author AVL */
int authorTreeYield(AuthorTree, Author*);

/* Rewinding an author AVL generator */
void authorTreeRewindGenerator(AuthorTree);

/* Cloning an author AVL */
AuthorTree authorTreeClone(AuthorTree tree);

/* Converting an author AVL to an array of strings */
AuthorArray authorTreeToString(AuthorTree, int*);

#ifdef DEBUG
    void printAuthorIndex();
#endif

#endif

