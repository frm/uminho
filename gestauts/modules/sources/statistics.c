/* Questions:
 * ansi? strdup?
 * double free error
 * shared structures
 */
#include <string.h>
#include <stdlib.h>

static double totalLength = 0;
static double nr_authors = 0;
static char* longest_name;
static char* shortest_name;

void init_stats() {
	longest_name = (char*)malloc( sizeof(char) );
	strncpy(longest_name, "AAAAA\0", sizeof(char) * 5);
	shortest_name = (char*)malloc( sizeof(char) * 20);
	strncpy(shortest_name, "AAAAAAAAAAAAAAAAAAA\0", sizeof(char) * 21);
}

char* getLongestAuthorName() { return longest_name; }
char* getShortestAuthorName() { return shortest_name; }

double getAverage() { return totalLength/nr_authors; }

void addToLength(int len) {
	totalLength += (double)len;
	nr_authors++;
}

static char* set_author(char* source, char* destination) {
	char* tmp = (char*)malloc( sizeof(char) * (strlen(destination) + 1) );
	strncpy(tmp, destination, strlen(destination) + 1);
	//free(destination);
	//destination = (char *)malloc( sizeof(char) * ( strlen(source) + 1) );
	destination = realloc(destination, strlen(source) + 1);
	strncpy(destination, source, strlen(source) + 1);
	return tmp;
}

void checkForLength (char *author) {
	if ( strlen(author) > strlen(longest_name) )
		author = set_author(author, longest_name);

	if ( strlen(author) < strlen(shortest_name) )
		author = set_author(author, shortest_name);
}
