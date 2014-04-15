#ifndef AUTHOR_CATALOG_AUTHORS_H_
#define AUTHOR_CATALOG_AUTHORS_H_

#include <string.h>
#include <avl.h>
#include "author_tree.h"
#include "author_catalog.h"

typedef struct year_publ_pair		YearPublPair;
typedef struct co_author_publ_pair 	CoAuthorPublPair;
typedef struct author_info			AuthorInfo;
typedef struct year_info			YearInfo;

AVL_DEF_HEADER(YearPublPair, int)
AVL_DEF_HEADER(CoAuthorPublPair, Author)
AVL_DEF_HEADER(AuthorInfo, Author)

typedef AuthorInfoAVL				AuthorInfoTree;

int authorInfoTreeInsert(AuthorInfoTree tree, AuthorInfo new);
int authorInfoTreeYield(AuthorInfoTree tree, AuthorInfo *ret);
void authorInfoTreeRewindGenerator(AuthorInfoTree tree);
void authorInfoTreeDestroy(AuthorInfoTree tree);
AuthorInfoTree authorInfoTreeNew();
AuthorInfoTree authorInfoTreeClone(AuthorInfoTree tree);

AuthorInfo newAuthorInfo(Author name);
void deleteAuthorInfo(AuthorInfo info);
int authorInfoAddCoAuthor(AuthorInfo author, Author coauthor);
int authorInfoAddYear(AuthorInfo author, int year);

#endif

