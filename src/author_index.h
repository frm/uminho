#ifndef AUTHOR_INDEX_H
#define AUTHOR_INDEX_H

#include "author_index_stats.h"
#include "author_index_tree.h"

void initializeAuthorIndex();
void deleteAuthorIndex();

int insertAuthor(char *);
int getListOfAuthorsByInitial(char, char **, int, int *);
void rewindGeneratorByInitial(char);

#endif

