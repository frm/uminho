#include <stdio.h>
#include <string.h>
#include <ctype.h>

static void extract_year_info(char* year_str) {
	/* Function that should use year to complete statistics */
	int year = atoi(year_str);
	return 0;
}

static void extract_author_info(char* author) {
	/* Function that should:
	 * add author to author_index
	 * use author info to calculate statistics
	 */
	return 0;
}

static void tokenize(char* buffer) {
	char *token = strtok(buffer, ",");
	
	while (token) {
		/* debug purposes */ printf("%s ", token);
		
		token = strtok(NULL, ",");
		strtrim(token);

		isalpha(token[0]) ? extract_author_info(token) : extract_year_info(token);
	}
}

int read_from_file(char* filename) {
	char buffer[1024];
	FILE *file = fopen(filename, "r");
	
	/* ERROR HANDLING */
	if (!file)
		return -1;

	while( fgets(buffer, 1024, file) ) {
		/* debug purposes */ printf("%s\n", buffer);
		tokenize(buffer);
	}

	return 0;
}


int main() {
	int result = read_from_file("./publicx.txt");
	
	if (result == -1)
		printf("FILE DOESN'T EXIST\n");

	return 0;
}
