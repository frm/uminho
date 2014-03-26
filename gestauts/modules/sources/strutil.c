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
	int start, end, size;
	char *new;

	if ( !str )
		return NULL;

	start = new_str_start(str);
	end = new_str_end(str);
	size = end - start + 1;
	
	new = (char*) malloc(sizeof(char) * size);
	strncpy(new, str + start, size);
	new[size] = '\0';

	return new;
}

