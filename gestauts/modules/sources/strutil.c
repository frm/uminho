#include "strutil.h"

static int new_str_end (char *str) {
	int i = strlen(str) - 1;

	while ( i > 0 && isspace(str[i]) )
		i--;

	return i;
}

static int new_str_start (char *str) {
	int i = 0;
	int size = strlen(str);

	while ( i < size && isspace(str[i]) )
		i++;
	
	return i;
}

char* strtrim(char *str) {
	int start = new_str_start(str);
	int end = new_str_end(str);
	int last_position = end - start + 1;

	memmove(str, str + start, last_position);
	str[last_position] = '\0';

	return str;
}

