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

struct year_publ_pair {
    int year;
    int nr_publications;
};

AVL_DEF_HEADER(YearPublPair, int)

struct co_author_publ_pair {
    Author coauthor;
    int nr_publications;
};

AVL_DEF_HEADER(CoAuthorPublPair, Author)

struct author_info {
    Author author;
    YearPublPairAVL publications_info;
    CoAuthorPublPairAVL coauthors_info;
};

AVL_DEF_HEADER(AuthorInfo, Author)

typedef AuthorInfoAVL				AuthorInfoTree;

int authorInfoTreeInsert(AuthorInfoTree, AuthorInfo);
int authorInfoTreeYield(AuthorInfoTree, AuthorInfo *);
int authorInfoGetAuthorPublicationsInYear(AuthorInfoTree, Author, int);
void authorInfoTreeRewindGenerator(AuthorInfoTree);
void authorInfoTreeDestroy(AuthorInfoTree);
AuthorInfoTree authorInfoTreeNew();
AuthorInfoTree authorInfoTreeClone(AuthorInfoTree);

AuthorInfo newAuthorInfo(Author);
void deleteAuthorInfo(AuthorInfo);
int authorInfoAddCoAuthor(AuthorInfo, Author);
int authorInfoAddYear(AuthorInfo, int);
int has_coauthors(AuthorInfo author);

CoAuthorPublPair cloneCoAuthorPublPair(CoAuthorPublPair);
void deleteCoAuthorPublPair(CoAuthorPublPair);

#ifdef DEBUG2
    #include <stdio.h>
    void authorInfoTreePrint(AuthorInfoTree);
#endif

#endif

