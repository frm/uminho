#ifndef AUTHOR_CATALOG_YEARS_H_
#define AUTHOR_CATALOG_YEARS_H_

#include <avl.h>
#include <string.h>
#include <stdlib.h>
#include "author_tree.h"

typedef struct year_entry	YearEntry;

AVL_DEF_HEADER(YearEntry, int)

typedef YearEntryAVL		YearTree

int yearTreeInsert(YearTree tree, YearEntry new);
int yearTreeYield(YearTree tree, YearEntry *buffer);
void yearTreeRewindGenerator(YearTree tree);
void yearTreeDestroy(YearTree tree);
YearTree yearTreeNew();
YearTree yearTreeClone(YearTree tree);

YearEntry newYearEntry(int year);

#endif

