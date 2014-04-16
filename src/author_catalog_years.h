#ifndef AUTHOR_CATALOG_YEARS_H_
#define AUTHOR_CATALOG_YEARS_H_

#include <avl.h>
#include <string.h>
#include <stdlib.h>

#include "author_tree.h"
#include "author_catalog_years.h"

typedef struct year_entry {
    int year;
    AuthorTree authors;
} YearEntry;

AVL_DEF_HEADER(YearEntry, int)

typedef YearEntryAVL YearTree;

int yearTreeInsert(YearTree, YearEntry);
int yearTreeYield(YearTree, YearEntry *);
int yearTreeYieldAuthorFromYear(YearTree, int, char **);
void yearTreeRewindGenerator(YearTree tree);
void yearTreeDestroy(YearTree tree);
YearTree yearTreeNew();
YearTree yearTreeClone(YearTree tree);

int yearEntryAddAuthor(YearEntry year, Author author);
YearEntry newYearEntry(int year);
void deleteYearEntry(YearEntry goodbye);

#endif

