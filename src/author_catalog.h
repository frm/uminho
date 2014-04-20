#ifndef AUTHOR_CATALOG_H_
#define AUTHOR_CATALOG_H_

#include "author_catalog_authors.h"
#include "author_catalog_years.h"

void initializeAuthorCatalog();
void deleteAuthorCatalog();

int insertToCatalog(Author*, int);
int getAuthorPublicationsInYear(Author, int);
char **getTopAuthorsInYear(int, int);
int getSoloAuthors();
int** getYearPublMatrix(Author author, int* size);
void deleteIntMatrix(int** matrix);

#ifdef DEBUG2
    #include <stdio.h>
    void printCatalog();
#endif

#endif

