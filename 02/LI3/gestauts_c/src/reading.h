#ifndef READING_H_
#define READING_H_

#include <author.h>

/* Receives a file name and populates the databases */
int read_file(char* filename);
/* Returns the name of the file read */
char* fileName();
/* Returns the number of read publications */
int nrPublications();
/* Returns the number of different authors */
int nrAuthors();
/* Returns the minimum read year */
int minYear();
/* Returns the maximum read year */
int maxYear();
/* Returns the shortest read name */
char* shortestName();
/* Returns the longest read name */
char* longestName();
/* Returns the average length of the read names */
double nameAverage();
/* Returns authors started with given initial */
AuthorArray getAuthorsBy(char initial, int* size);
/* Returns the total number of publications in a given interval */
int getYearsTotalByInterval(int, int);
/* Returns an array with publications. Returns the limits in the min and max parameters */
int *getYearsTotal(int *min, int *max);
/* Yields the next year's CSV. Returns 1 if it's the last */
int yieldYearCSV(char **);
/* Initializes the modules */
void initializeGestauts();
/* Deletes the modules */
void leaveGestauts();
/* Returns total number of publications with a given number of coauthors for a given year */
int getYearCoAuthorsTotal(int, int);
/* Returns the number of authors in the AuthorArray ret used to return the top n authors from a year */
int topAuthorsInYear(int year, int n, AuthorArray *ret);
/* Query 3 */
int authorPublicationsInYear(Author, int);
/* Query 13 */
float getAuthorYearRatio(Author, int);
/* Query 4 */
int totalSoloAuthors();
void deleteAuthorPublicationsMatrix(int** matrix);
/* Query 8 */
AuthorArray getTopCoauthors(Author name, int* nr_coauthors, int* nr_publications);
/* Returns the list of authors who published in a year */
AuthorArray getPublishedAuthorsInYear(int, int, int*);
/* Returns the number of items in the array ret used to return the publications in every year by an author
 * fstYear stores the year represented by ret[0] */
int getAuthorPublicationsByYear(Author author, int *fstYear, int **ret);
/* Returns the number of items in the array ret used to return the authors who published in every year
 * in the given interval */
int authorsByInterval(int fst, int snd, AuthorArray *ret);
/* Deletes an array of strings */
void deleteStringBuffer(char** list, int size);

#endif
