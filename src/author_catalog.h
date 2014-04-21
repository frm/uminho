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
void deleteTopCoauthors(Author* list, int size);
char** getMostCoauthor(Author author, int* nr_coauthors, int* nr_publications);

#ifdef DEBUG2
    #include <stdio.h>
    void printCatalog();
#endif

#endif

