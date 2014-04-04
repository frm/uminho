#include <string.h>
#include <stdlib.h>

static double totalLength = 0;
static double nr_authors = 0;
static char* longest_name;
static char* shortest_name;

void init_author_stats() {
	longest_name = (char*)malloc( sizeof(char) * 3);
	strncpy(longest_name, "AA\0", sizeof(char) * 3);
	shortest_name = (char*)malloc( sizeof(char) * 10);
	strncpy(shortest_name, "AAAAAAAAA\0", sizeof(char) * 10);
}

char* getLongestAuthorName() { return longest_name; }
char* getShortestAuthorName() { return shortest_name; }

double getAverage() { return totalLength/nr_authors; }

void addToLength(int len) {
	totalLength += (double)len;
	nr_authors++;
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

