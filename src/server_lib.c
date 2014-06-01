
#include <unistd.h>		// Open (Read from named pipe)
#include <stdio.h>		// printf
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include "server.h"

int incrementar(char* prefix[], int value) {
	if(!prefix) return -1;

	char count[10];
	sprintf(count, "%d", value);
	printf("SIZE OF COUNT %lu\n", strlen(count));

	char* new_str = (char*)calloc(
		strlen(prefix[0]) +
		strlen(count) +
		4,			// 2 ';', indicator and '\0'
		sizeof(char)
		);

	sprintf(new_str, "%s;2%s;", prefix[0], count);
	printf("SIZE EXPECTED: %lu\n", strlen(prefix[0]) + strlen(count) + 4);
	printf("NEW_STR: %s WITH SIZE: %lu\n", new_str, strlen(new_str) + 1);
	printf("IF LAST CHAR IS NULL, THEN YES WILL BE PRINT: ");
	if (new_str[strlen(new_str)] == '\0') printf("YES\n");
	else printf("%c", new_str[strlen(new_str)]);

	for(int i = 1; prefix[i]; i++) {
		new_str = (char*)realloc(new_str, strlen(new_str) + strlen(prefix[i]) + 2);
		printf("AT ITERATION %d, NEW_STR SIZE WILL BE: %lu\n", i, strlen(new_str) + strlen(prefix[i]) + 2);
		if (i == 1) sprintf(new_str, "%s%s", new_str, prefix[i]);
		else sprintf(new_str, "%s:%s", new_str, prefix[i]);
		printf("NEW_STR SIZE IS: %lu\n", strlen(new_str) + 1);
	}

	printf("\nSTRING COMPLETE\nIF LAST CHAR IS NULL, THEN YES WILL BE PRINT: ");
	if (new_str[strlen(new_str)] == '\0') printf("YES\n");
	else printf("%c", new_str[strlen(new_str)]);
	printf("\t\t### %s\n", new_str);

	free(new_str);
	return 0;
}

/*
int incrementar(char* prefix[], int value){
	if(!prefix) return -1;
	char count[10];
	sprintf(count, "%d", value);

	char* new_str = (char*) calloc(
	strlen( prefix[0] ) +
	strlen( prefix[1] ) +
	strlen(count) +
	4,
	sizeof(char)
	); // 4 => '\0', ';' and "2"
	sprintf(new_str, "%s;%s%s;%s", prefix[0], "2", count, prefix[1]);

	for(int i = 2; prefix[i]; i++){
		new_str = (char*)realloc( new_str, strlen(new_str) + strlen( prefix[i] ) + 2 ); // 2 => '\0' and :
		sprintf(new_str, "%s:%s", new_str, prefix[i]);
	}

	int fd = open(SERVER_NAME, O_WRONLY);

	printf(" !!! INCREMENT OPPENED SERVER_NAME TO FD %d !!!\n", fd);
	write(fd, new_str, strlen(new_str) + 1);

	close(fd);

	free(new_str);
	return 0;
}


int agregar(char *prefix[], unsigned level, char *path){
	if(!prefix) return -1;
	char lvl[10];
	sprintf(lvl, "%d", level);

	char* new_str = (char*) calloc(
		strlen( prefix[0] ) +
		strlen( prefix[1] ) +
		strlen(path) +
		strlen(lvl) +
		5,
		sizeof(char)
		); // 5 => ;, '\0' and "3"
	sprintf(new_str, "%s;%s%s;%s;%s", prefix[0], "3", lvl, path, prefix[1]);

	for(int i = 2; prefix[i]; i++){
		new_str = (char*)realloc( new_str, strlen(new_str) + strlen( prefix[i] ) + 2 ); // 2 => ; and '\0'
		sprintf(new_str, "%s:%s", new_str, prefix[i]);
	}

	int fd = open(SERVER_NAME, O_WRONLY);
	printf(" !!! AGGREGATE OPPENED SERVER_NAME TO FD %d !!!\n", fd);
	puts(new_str);

	write(fd, new_str, strlen(new_str) + 1);

	close(fd);

	free(new_str);

	return 0;
}
*/
