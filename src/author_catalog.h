#ifndef AUTHOR_CATALOG_H
#define AUTHOR_CATALOG_H

#include <author.h>

/* Initializing the Author Catalog */
void initializeAuthorCatalog();
/* Deleting the author Catalog */
void deleteAuthorCatalog();
/* Inserting an array of strings containing the tokens of a publication */
int insertToCatalog(char**, int);
/* Query 4 - returning the total of solo authors */
int getTotalSoloAuthors();
/* Return the total number of publications of a given authors */
int getYearPublicationsBy(Author, int);
/* Returns the number of years in the ret array. fstYear will store the number represented by ret[0] */
int catalogAuthorPublByYear(Author, int *fstYear, int **ret);
/* Query 8 - Returns an array of strings with the top coauthor(s) of the given author */
AuthorArray getMostPublCoauthor(Author, int* nr_coauthors, int* nr_publications);
/* Query 12 - Get the array with the n authors who published the most in a given year */
int catalogTopAuthorsInYear(int year, int n, AuthorArray *ret);
/* Query 9 - Get the array with the authors who published in every year in the given interval */
int catalogAuthorsByInterval(int fst, int snd, AuthorArray *ret);

#ifdef DEBUG2
int getHashColisions();
#endif

#endif
