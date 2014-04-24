#ifndef AUTHOR_CATALOG_YEARS_H_
#define AUTHOR_CATALOG_YEARS_H_

#include <avl.h>
#include <string.h>
#include <stdlib.h>

#include "author_tree.h"
#include "author_catalog_years.h"

typedef struct year_entry {
    int year;
    int total_authors;
    AuthorTree authors;
} YearEntry;

AVL_DEF_HEADER(YearEntry, int)

typedef YearEntryAVL YearTree;

int yearEntryGetYear(YearEntry);
int yearEntryGetTotalAuthors(YearEntry);
AuthorTree yearEntryGetAuthors(YearEntry);
void yearEntrySetTotalAuthors(YearEntry*, int);

int yearTreeInsert(YearTree, YearEntry);
int yearTreeYield(YearTree, YearEntry *);
int yearTreeYieldAuthorFromYear(YearTree, int, char **);
int yearTreeFind(YearTree, int, YearEntry*);
void yearTreeRewindGenerator(YearTree);
void yearTreeDestroy(YearTree);
YearTree yearTreeNew();
YearTree yearTreeClone(YearTree);
int yearTreeExists(YearTree, int);

int yearEntryAddAuthor(YearEntry, char*);
int yearEntryYieldAuthor(YearEntry, char**);
YearEntry newYearEntry(int);
void deleteYearEntry(YearEntry);

#endif

