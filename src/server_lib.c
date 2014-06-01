
#include <unistd.h>		// Open (Read from named pipe)
#include <stdio.h>		// printf
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include "server.h"

static void write_to_pipe(char* str) {
	int fd = open(SERVER_NAME, O_WRONLY | O_APPEND);
	write(fd, str, strlen(str) + 1);
	close(fd);
}

static void append_prefix(char** str, char* prefix, int offset) {
	*str = (char*)realloc(*str, offset + strlen(*str) + strlen(prefix) );
	sprintf(*str, "%s%s", *str, prefix);
}

static void append_prefixes(char** str, char* prefix[]) {
	for(int i = 1; prefix[i]; i++) {
		if (i == 1)
			append_prefix(str, prefix[i], 1);
		else
			append_prefix(str, prefix[i], 2);
	}
}


int incrementar(char* prefix[], int value) {
	if(!prefix) return -1;

	char count[10];
	sprintf(count, "%d", value);

	char* new_str = (char*)calloc(
		strlen(prefix[0]) +
		strlen(count) +
		4,			// 2 ';', indicator and '\0'
		sizeof(char)
		);

	sprintf(new_str, "%s;2%s;", prefix[0], count);

	append_prefixes(&new_str, prefix);

	write_to_pipe(new_str);

	free(new_str);
	return 0;
}


int agregar(char* prefix[], int level, char* path) {
	if(!prefix) return -1;

	char count[10];
	sprintf(count, "%d", level);

	char* new_str = (char*)calloc(
		strlen(prefix[0]) +
		strlen(count) +
		strlen(path) +
		5,			// 3 ';', indicator and '\0'
		sizeof(char)
		);

	sprintf(new_str, "%s;3%s;%s;", prefix[0], count, path);

	append_prefixes(&new_str, prefix);

	write_to_pipe(new_str);

	free(new_str);
	return 0;
}
