
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>

#include "server.h"

#define PIPE_BUF		4096

/**Write the given string to the named pipe*/
static void write_to_pipe(char* str) {
	int fd = open(SERVER_NAME, O_WRONLY | O_APPEND);
	write(fd, str, strlen(str) + 1);
	close(fd);
}

/**Append the given prefix to the end of the given str, int the format chosen by us. If "offset == 1", writes a colon between the string and the prefix*/
static void append_prefix(char** str, char* prefix, int offset) {
	*str = (char*)realloc(*str, offset + strlen(*str) + strlen(prefix) );
	if (offset == 1) sprintf(*str, "%s%s", *str, prefix);
	else sprintf(*str, "%s:%s", *str, prefix);
}

/**Appends all the prefixes in a prefix array using append_prefix*/
static void append_prefixes(char** str, char* prefix[]) {
	for(int i = 1; prefix[i]; i++) {
		if (i == 1)
			append_prefix(str, prefix[i], 1);
		else
			append_prefix(str, prefix[i], 2);
	}
}

/*Main function: Incrementar; Increments the given value to the location given in prefix*/
int incrementar(char* prefix[], int value) {
	if(!prefix) return -1;

	char count[10];                     
	sprintf(count, "%d", value);		//Converts the int value to a string

	char* new_str = (char*)calloc(
		strlen(prefix[0]) +
		strlen(count) +
		4,			// 2 ';', indicator and '\0'
		sizeof(char)
		);								//sets the size of the base string

	sprintf(new_str, "%s;2%s;", prefix[0], count);   //From here to the end of the function, we append all of the information to the base string*/

	append_prefixes(&new_str, prefix);

	if ( strlen(new_str) < PIPE_BUF ) {
		write_to_pipe(new_str);
		free(new_str);
		return 0;
	}

	return 1;
}

/**Handles the errors*/
static void error_handl(int s) {
	printf("\nInvalid arguments\n");
}

/**Releases the process from the block created while writing to a file*/
static void correct_handl(int s) { }


/**Main function: Agregar; Writes info about the given arguments into a file*/
int agregar(char* prefix[], int level, char* path) {
	if(!prefix) return -1;

	signal(SIGINT, correct_handl);
	signal(SIGQUIT, error_handl);

	char pid[10];
	char count[10];
	sprintf(count, "%d", level);
	sprintf(pid, "%d", getpid());
	char* new_str = (char*)calloc(
		strlen(prefix[0]) +
		strlen(count) +
		strlen(pid) +
		strlen(path) +
		6,			// 4 ';', indicator and '\0'
		sizeof(char)
		);

	sprintf(new_str, "%s;3%s;%s;%s;", prefix[0], count, pid, path);

	append_prefixes(&new_str, prefix);

	if ( strlen(new_str) < PIPE_BUF ) {
		write_to_pipe(new_str);
		pause();
		free(new_str);
		return 0;
	}

	return 1;

}
