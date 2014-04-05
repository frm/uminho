#include "../headers/strutil.h"

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
	char *newstr;

	if ( !str )
		return NULL;

	start = new_str_start(str);
	end = new_str_end(str);
	size = end - start + 1;

	newstr = (char*) malloc( sizeof(char) * (size + 1) );
	strncpy(newstr, str + start, size);
	newstr[size] = '\0';

	return newstr;
}

