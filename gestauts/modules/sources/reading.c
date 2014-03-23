#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "strutil.h"
#include "statistics.h"

static void extract_year_info(char* year_str) {
	/* Function that should use year to complete statistics */
	int year = atoi(year_str);
}

static void extract_author_info(char* author) {
	/* Function that should:
	 * add author to author_index
	 * use author info to calculate statistics
	 */
	addToLength ( strlen(author) );
	checkForLength(author);
}

static int tokenize(char* buffer) {
	int nr_authors = 0;
	char *token = strtok(buffer, ",");
	
	while (token) {
#ifdef debug
		printf("%s ", token);
#endif
		/* use !isdigit() instead of isalpha because of names started with special characters */
		if ( !( isdigit(token[0]) ) ) {
			extract_author_info(token);
			nr_authors++;
		}
		else
			extract_year_info(token);

		token = strtrim( strtok(NULL, ",") );
	
	}

	return nr_authors;
}

int read_from_file(char* filename) {
	int nr_authors = 0;
	int nr_publications = 0;
	char buffer[1024];
	FILE *file = fopen(filename, "r");
	
	/* ERROR HANDLING */
	if (!file)
		return -1;
	init_stats();
	while( fgets(buffer, 1024, file) ) {
#ifdef debug
		printf("%s\n", buffer);
#endif
		nr_authors += tokenize(buffer);
		nr_publications++;
	}

	printf("FILE READ: %s\nTOTAL NUMBER OF PUBLICATIONS: %d\nTOTAL NUMBER OF AUTHORS:%d\n", filename, nr_publications, nr_authors);
	printf("LONGEST NAME: %s with length %ld\n", getLongestAuthorName(), strlen( getLongestAuthorName() ) );
	printf("SHORTEST NAME: %s with length %ld\n", getShortestAuthorName(), strlen( getShortestAuthorName() ) );
	printf("AVERAGE LENGTH: %f\n", getAverage());
	

	return 0;
}


int main() {
	int result = read_from_file("./publicx.txt");
	
	if (result == -1)
		printf("FILE DOESN'T EXIST\n");

	return 0;
}
