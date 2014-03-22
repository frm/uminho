#include <string.h>
#include <stdio.h>
#include <ctype.h> /* Define my own isspace function */

static void trim_right (char *str) {
	int i = strlen(str);

	while ( i > 0 && isspace(str[i-1]) )
		i--;

	str[i-1] = '\0';
}

static void trim_left (char *str) {
	int i = 0;
	int size = strlen(str);

	while ( i < size && isspace(str[0]) ){
		str++;
		i++;
	}

}

void strtrim(char* str) {
	trim_left(str);
	trim_right(str);

}

int main() {
	char* mystr = " 123456789 ";
	
	printf("BEFORE TRIMMING - length: %d, str: %s\n", (int)strlen(mystr), mystr);
	
	strtrim(mystr);

	printf("AFTER TRIMMING - length: %d, str: %s\n", (int)strlen(mystr), mystr);
}
