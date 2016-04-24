#ifndef AUTHOR_MAP_H
#define AUTHOR_MAP_H

#include <author.h>

/* Defining an Hash of AuthorInfo
 * Resolved by chaining (called buckets)
 */

typedef struct hash *HashMap;

/* Adds an author and its coauthor info to the hash */
int HashMapAddAuthor(HashMap, Author, Author);
/* Adds the yearinfo to the corresponding author in hash */
int HashMapAddYear(HashMap, Author, int);
int getSoloAuthors(HashMap);
/* Returning the number of publications of an author in a given year */
int getPublicationsInYear(HashMap, int, Author);
/* Returns the number of years in the parameter ret used to return the array with publications by year
 * the fstYear parameter is used to return the year represented by the position 0 of the array */
int getAuthorPublByYear(HashMap table, Author author, int *fstYear, int **ret);
/* Returns an array of strings with the coauthors whom the author most published with */
AuthorArray getCoauthorWithMostPubl(HashMap table, Author author, int* nr_coauthors, int* nr_publications);
/* Returns the number of authors in the parameter ret used to return the array with the n most published
 * authors in a given year*/
int getTopAuthorsInYear(HashMap table, int year, int n, AuthorArray *ret);
/* Returns the number of authors in the parameter ret used to return the array with the authors who
 * published every year in the interval [fst, snd] */
int getAuthorsByInterval(HashMap table, int fst, int snd, AuthorArray *ret);
/* Defines a new hash map with the given size */
HashMap newHashMap(int size);
/* Deletes given hash */
void deleteHashMap(HashMap hash);

#endif
