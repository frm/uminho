#ifndef AUTHOR_INDEX_H_
#define AUTHOR_INDEX_H_

#include "author_index_stats.h"
#include "author_tree.h"

#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )


void initializeAuthorIndex();
void deleteAuthorIndex();

int insertAuthor(char *);
int getListOfAuthorsByInitial(char, char **, int, int *);
void rewindGeneratorByInitial(char);

#endif

