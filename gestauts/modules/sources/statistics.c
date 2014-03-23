#include <string.h>
#include <stdlib.h>

static double totalLength = 0;
static double nr_authors = 0;
static char* longest_name;
static char* shortest_name;

void init_stats() {
	longest_name = (char*)malloc( sizeof(char) );
	strncpy(longest_name, "\0", sizeof(char) );
	shortest_name = (char*)malloc( sizeof(char) * 20);
	strncpy(shortest_name, "AAAAAAAAAAAAAAAAAAA", sizeof(char) * 20);
}

char* getLongestAuthorName() { return longest_name; }
char* getShortestAuthorName() { return shortest_name; }

double getAverage() { return totalLength/nr_authors; }

void addToLength(int len) {
	totalLength += (double)len;
	nr_authors++;
}

static void set_author(char* source, char* destination) {
	free(destination);
	destination = (char*)malloc( sizeof(char) * ( strlen(source) + 1) );
	strncpy(destination, source, strlen(source) + 1);
}

void checkForLength (char *author) {
	if ( strlen(author) > strlen(longest_name) )
		set_author(author, longest_name);

	else if ( strlen(author) < strlen(shortest_name) )
		set_author(author, shortest_name);
}
