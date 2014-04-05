#include <string.h>
#include <stdlib.h>
#include <limits.h>

static double totalLength;
static double nr_authors;
static int nr_publications;
static int min_year;
static int max_year;

static char* longest_name;
static char* shortest_name;

char* getLongestAuthorName() { return longest_name; }
char* getShortestAuthorName() { return shortest_name; }

double getAverage() { return totalLength/nr_authors; }
double getNumberAuthors() { return nr_authors; }

void addToLength(int len) {
	totalLength += (double)len;
	nr_authors++;
}

int getMaxYear() { return max_year; }
int getMinYear() { return min_year; }

int getNrPublications() { return nr_publications; }

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
static void set_author(char* source, char** destination) {
	int size = strlen(source);
	*destination = (char*)realloc( *destination, sizeof(char) * (size + 1) );
	strncpy( *destination, source, sizeof(char) * (size + 1) );
}

void checkForLength (char *author) {
	if ( strlen(author) > strlen(longest_name) )
		set_author(author, &longest_name);

	if ( strlen(author) < strlen(shortest_name) )
		set_author(author, &shortest_name);
}

void initAuthorStats() {
	totalLength = 0;
	nr_authors = 0;
	nr_publications = 0;
	max_year = 0;
	min_year = INT_MAX;

	longest_name = (char*)malloc( sizeof(char) * 3);
	strncpy(longest_name, "AA\0", sizeof(char) * 3);
	shortest_name = (char*)malloc( sizeof(char) * 10);
	strncpy(shortest_name, "AAAAAAAAA\0", sizeof(char) * 10);
}

void deleteAuthorIndexStats() {
	free(longest_name);
	free(shortest_name);
}
