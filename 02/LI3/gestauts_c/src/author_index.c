#include "author_index.h"
#include "author_tree.h"

#include <ctype.h>
#include <strutil.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

/* Checks if letter is correct */
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )
/* Determines the index of the array */
#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)

static AuthorTree letterIndex[27];  /* struct used for Index - Array of 27 Author Trees (1 for each letter + special chars) */
static double totalLength;          /* Total sum length of every author */
static double nr_authors;           /* Total number of non repeting authors */
static int nr_publications;         /* Total number of publications */
static int min_year;                /* Minimum year where there was a publication */
static int max_year;                /* Max year with a publication */
static Author longest_name;         /* Name with most chars */
static Author shortest_name;        /* Name with least chars */

static void initAuthorStats() {
    longest_name = str_dup("AA\0");
    shortest_name = str_dup("AAAAAAAAA\0");
    totalLength = 0.0;
    nr_authors = 0.0;
    nr_publications = 0;
    min_year = INT_MAX;
    max_year = 0;
}

static void deleteAuthorIndexStats() {
    free(longest_name);
    free(shortest_name);
}


void initializeAuthorIndex() {
	int i;

    initAuthorStats();

    for (i = 0; i < 27; i++)
        letterIndex[i] = authorTreeNew();
}

int insertAuthor(Author author) {
    return authorTreeInsert(letterIndex[ GET_CHAR_INDEX(author[0]) ], author);
}

/* Returning an array of strings containing the authors inside a tree */
AuthorArray getListOfAuthorsByInitial(char initial, int* size) {
    return authorTreeToString(letterIndex[ GET_CHAR_INDEX(initial) ], size);
}

void deleteAuthorIndex() {
    int i;

    deleteAuthorIndexStats();

    for (i = 0; i < 27; i++)
        authorTreeDestroy(letterIndex[i]);
}
/* Checking if a string is an author
 * Using !isdigit because isalpha returns false for special chars
 */
int isAuthor(char* str) { return !isdigit(str[0]); }

/* Updating length and number of authors after insertion */
void addToLength(int len) {
    totalLength += (double)len;
    nr_authors++;
}

/* Simple getters for statics variables */
Author getLongestAuthorName() { return str_dup(longest_name); }
Author getShortestAuthorName() { return str_dup(shortest_name); }

double getAverage() { return totalLength/nr_authors; }
double getNumberAuthors() { return nr_authors; }

int getMaxYear() {
    return max_year;
}

int getMinYear() {
    return min_year;
}

int getNrPublications() {
    return nr_publications;
}

/* Checks if new_year is a new maximum or minimum */
void checkForYear(int new_year) {
    if (new_year > max_year) max_year = new_year;
    if (new_year < min_year) min_year = new_year;
    nr_publications++;
}
/*
 * Using pointer to pointer, to make destination point to the same memory address as
 * longest/shortest_name. When using only char*, realloc was allocating memory
 * but not setting longest/shortest_name to point there
 */
static void set_author(Author source, Author* destination) {
    int size = strlen(source);
    *destination = (Author)realloc( *destination, sizeof(char) * (size + 1) );
    memcpy( *destination, source, sizeof(char) * (size + 1) );
}

/* Sees if shortest_name and longest_name need update */
void checkForLength (Author author) {
    if ( strlen(author) > strlen(longest_name) )
        set_author(author, &longest_name);

    if ( strlen(author) < strlen(shortest_name) )
        set_author(author, &shortest_name);
}
