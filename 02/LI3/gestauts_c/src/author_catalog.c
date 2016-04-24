#include "author_catalog.h"
#include "author_map.h"

#include <stdio.h> /* sscanf */

#define HASH_SIZE       300000
static HashMap          CatalogAuthors;

void initializeAuthorCatalog() {
    CatalogAuthors = newHashMap(HASH_SIZE);
}

void deleteAuthorCatalog() {
    deleteHashMap(CatalogAuthors);
}

/* Deconstructing an array of strings
 * Inserting each string to the catalog
 * Note that author_buffer[size] contains the year
 */
int insertToCatalog(char** author_list, int size) {
    int i, j, year;
    sscanf( author_list[size], "%d\n", &year );

    for (i = 0; i < size; i++) {
/* i < size as well as j < size because if we put i < size - 1, the last author won't add the year of publication */
        for (j = i + 1; j < size; j++) {
            HashMapAddAuthor( CatalogAuthors, author_list[i], author_list[j] );
            HashMapAddAuthor( CatalogAuthors, author_list[j], author_list[i] );
        }

        HashMapAddYear( CatalogAuthors, author_list[i], year );
    }

    return 0;
}


/* Query 5 - returning the total number of solo authors */
int getTotalSoloAuthors() {
    return getSoloAuthors(CatalogAuthors);
}

/* Returning the number of publications of a given author in a given year */
int getYearPublicationsBy(Author name, int year) {
    return getPublicationsInYear(CatalogAuthors, year, name);
}

/* Returns the number of years in the ret array. fstYear will store the number represented by ret[0] */
int catalogAuthorPublByYear(Author author, int *fstYear, int **ret) {
    return getAuthorPublByYear(CatalogAuthors, author, fstYear, ret);
}

/* Query 8 - Returns an array of strings with the top coauthor(s) of the given author */
char** getMostPublCoauthor(Author author, int* nr_coauthors, int* nr_publications) {
    return getCoauthorWithMostPubl(CatalogAuthors, author, nr_coauthors, nr_publications);
}

/* Query 12 - Get the array with the n authors who published the most in a given year */
int catalogTopAuthorsInYear(int year, int n, char ***ret){
    return getTopAuthorsInYear(CatalogAuthors, year, n, ret);
}

/* Query 9 - Get the array with the authors who published in every year in the given interval */
int catalogAuthorsByInterval(int fst, int snd, Author **ret) {
    return getAuthorsByInterval(CatalogAuthors, fst, snd, ret);
}
