#ifndef AUTHOR_INDEX_H_
#define AUTHOR_INDEX_H_

#include <author.h>

/* Initializing the module */
void initializeAuthorIndex();
/* Deletes the module */
void deleteAuthorIndex();

/* Inserts the author into the correct tree */
int insertAuthor(Author);
/* Returns an array of strings containing the authors started with given initial */
AuthorArray getListOfAuthorsByInitial(char, int*);

/* Checks if string is an author */
int isAuthor(char* str);

Author getLongestAuthorName();
Author getShortestAuthorName();
double getAverage();
double getNumberAuthors();
int getMaxYear();
int getMinYear();
int getNrPublications();


/* Updates the total length and number of authors */
void addToLength(int len);
/* Checks if min and max years need update (and updates them) */
void checkForYear(int new_year);
/* Checks if shortest and longest name need update */
void checkForLength(Author author);

#endif
