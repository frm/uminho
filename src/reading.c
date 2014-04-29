#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <strutil.h>

#include "reading.h"
#include "author_index.h"
#include "statistics.h"
#include "author_catalog.h"

static char* fileread;

/* Given a string (containing the year of a publication) and the number of authors
 * Update the statistics
 */
static void extract_year_info(char *year_str, int coAuthors) {
	int year = atoi(year_str);
	checkForYear(year);
	statsIncrement(year, coAuthors);
}

/* Inserts a string containing an author in the author index module
 * uses the return value to decide if the author was already inserted
 */
static void extract_author_info(Author author) {
	int insertion = insertAuthor(author);

	if (insertion == 0) {
		addToLength( strlen(author) );
		checkForLength(author);
	}
}

/* Receiving a line, tokenize breaks that line by the , separator (generating a token)
 * Then determining if said token is an author or year
 * Acording to the result, the token is either inserted to the index or used for statistics
 */
static void tokenize(char* buffer) {
	AuthorArray author_buffer = (AuthorArray)malloc( sizeof(Author) * 128 );
	char* token = strtrim( strtok(buffer, ",") );
	int n = 0;

	while (token) {
		author_buffer[n] = token; /* doesn't break encapsulation because strtrim allocates mem */

		if ( isAuthor(token) ) {
			extract_author_info(token);
			n++;
		}
		else
			extract_year_info(token, n);

		token = strtrim( strtok(NULL, ",") );
	}

	insertToCatalog(author_buffer, n);
	deleteStringBuffer(author_buffer, n + 1);

}

/* Updating the name of the file that was read */
static void set_filename(char* filename) {
	free(fileread);
	fileread = str_dup(filename);
}


/* Reading a file from a given filename */
int read_file(char* filename) {
	char buffer[1024];
	FILE *file;

	/* If file to read is the current file, no need to read it again */
	if ( strcmp(filename, fileread) == 0 )
		return 1;

	file = fopen(filename, "r");

	/* ERROR HANDLING */
	if (!file)
		return -1;

	set_filename(filename);

	while( fgets(buffer, 1024, file) )
		tokenize(buffer);

	fclose(file);

#ifdef DEBUG2
	#include <stdio.h>
	printf("\n\nNUMBER OF HASH COLISIONS: %d\n\n", getHashColisions() );
#endif

	return 0;
}

/* getters for statistics variables */
char* fileName() {
	return str_dup(fileread);
}

int nrPublications() {
	return getNrPublications();
}

int nrAuthors() {
	return (int)getNumberAuthors();
}

int minYear() {
	return getMinYear();
}

int maxYear() {
	return getMaxYear();
}

char* shortestName() {
	return getShortestAuthorName();
}

char* longestName() {
	return getLongestAuthorName();
}

double nameAverage() {
	return getAverage();
}

/* Query 13 */
float getAuthorYearRatio(Author author, int year) {
	int yearTotal, authorTotal;

	/* get the year total from the statistics module */
	yearTotal = statsGetYearTotal(year);

	/* Error handling */
	if (!yearTotal)
		return -1.0;

	/* Getting the author total from the author catalog */
	authorTotal = getYearPublicationsBy(author, year);

	return ((float)authorTotal / (float)yearTotal);
}

/* Returns an array with publications. Returns the limits in the min and max parameters */
int *getYearsTotal(int *minYear, int *maxYear) {
	int min, max, year, totalYears, totalPubs, test;
	int *totals;

	min = getMinYear();
	max = getMaxYear();
	totalYears = max - min + 1;
	totals = (int *)calloc(totalYears, sizeof(int));

	for (;;) {
		test = statsYieldYearTotal(&year, &totalPubs);

		if (!test){
			totals[year - min] = totalPubs;
		}
		else {
			if (test == 1)
				totals[year - min] = totalPubs;

			break;
		}
	}

	*minYear = min;
	*maxYear = max;
	return totals;
}

/* Returns the total number of publications in a given interval */
int getYearsTotalByInterval(int first, int last) {
	int min, max, i, total;

	total = 0;
	min = getMinYear();
	max = getMaxYear();

	if (first < min)
		first = min;

	if (last > max)
		last = max;

	for (i = first; i <= last; i++) {
		total += statsGetYearTotal(i);
	}

	return total;
}

/* Query 3 - Given an author name, returns the publications of said author in a given year */
int authorPublicationsInYear(char *author, int year) {
	return getYearPublicationsBy(author, year);
}

/* Returns the number of authors in the AuthorArray ret used to return the top n authors from a year */
int topAuthorsInYear(int year, int n, char ***ret) {
	if (year > getMaxYear() || year < getMinYear())
		return 0;
	else
		return catalogTopAuthorsInYear(year, n, ret);
}

/* Yields the next year's CSV. Returns 1 if it's the last */
int yieldYearCSV(char **ret) {
	return statsYieldYearCSV(ret);
}

/* Returns total number of publications with a given number of coauthors for a given year */
int getYearCoAuthorsTotal(int year, int coAuthors) {
	return statsGetYearCoAuthorsTotal(year, coAuthors);
}

/* Query 6 - given a letter returns the array of strings with a maximum of 24 read authors */
char** getAuthorsBy(char initial, int* size) {
	return getListOfAuthorsByInitial(initial, size);
}

/* Returns the number of items in the array ret used to return the publications in every year by an author
 * fstYear stores the year represented by ret[0] */
int getAuthorPublicationsByYear(Author author, int *fstYear, int **ret) {
	return catalogAuthorPublByYear(author, fstYear, ret);
}

/* Query 4 - Returns the total number of solo authors */
int totalSoloAuthors() {
	return getTotalSoloAuthors();
}

/* Query 8 - Returns the list of authors with whom given author has most publications  */
AuthorArray getTopCoauthors(Author name, int* nr_coauthors, int* nr_publications) {
	return getMostPublCoauthor(name, nr_coauthors, nr_publications);
}

/* Deleting an array of strings */
void deleteStringBuffer(char** list, int size) {
    int i;
    for (i = 0; i < size; i++)
        free(list[i]);

    free(list);
}

/* Returns the number of items in the array ret used to return the authors who published in every year
 * in the given interval */
int authorsByInterval(int fst, int snd, AuthorArray *ret) {
	return catalogAuthorsByInterval(fst, snd, ret);
}

void initializeGestauts() {
	fileread = str_dup("\0");
	initializeAuthorIndex();
	initializeStatistics();
	initializeAuthorCatalog();
}

void leaveGestauts() {
	free(fileread);
	deleteAuthorIndex();
	deleteStatistics();
	deleteAuthorCatalog();
}


