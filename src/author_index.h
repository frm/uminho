#ifndef AUTHOR_INDEX_H_
#define AUTHOR_INDEX_H_

#include "author_index_stats.h"
#include "author_tree.h"
#include <ctype.h>

#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )
#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)


void initializeAuthorIndex();
void deleteAuthorIndex();

int insertAuthor(char *);
int getListOfAuthorsByInitial(char, char **, int, int *);
void rewindGeneratorByInitial(char);

int isAuthor(char* str);

#endif
