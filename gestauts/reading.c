#include <stdio.h>
#include <string.h>


void extract_info(char* buffer) {
}

int read_from_file(char* filename) {
	char buffer[1024];
	FILE *file = fopen(filename, "r");
	
	/* ERROR HANDLING */
	if (!file)
		return -1;

	while( fgets(buffer, 1024, file) ) {
		printf("%s\n", buffer);
		extract_info(buffer);
	}

	return 0;
}


int main() {
	int result = read_from_file("./publicx.txt");
	
	if (result == -1)
		printf("FILE DOESN'T EXIST");

	return 0;
}
