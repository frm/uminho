
#include "../src/server.h"
#include "../src/server_lib.h"

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]){
	char* prefix[1024];
	int level = atoi(argv[1]);
	printf("FILENAME: %s\nLEVEL: %d\n", argv[2], level);
	for(int i = 0; i < argc - 3; i++) {
		prefix[i] = argv[i+3];
		printf("INDEX: %d ARGC: %d PREFIX: %s\n", i, argc, prefix[i]);
	}

	prefix[argc - 3] = NULL;
	if(prefix[1] == NULL) printf("YOU DID IT RIGHT");

	agregar(prefix, level, argv[2]);
	return 0;
}

/*
./agregar   1         this 		Braga
ARGV 0    ARGV1		ARGV2		ARGV3    ARGC => 4
level: ARGV1
filname: ARGV2
prefix[0] => ARGV3
prefix[1] => NULL
*/