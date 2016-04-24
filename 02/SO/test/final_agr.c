
#include "../src/server.h"
#include "../src/server_lib.h"

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]){
	char* prefix[1024];
	int level = atoi(argv[1]);
	for(int i = 0; i < argc - 3; i++) {
		prefix[i] = argv[i+3];
	}

	prefix[argc - 3] = NULL;

	agregar(prefix, level, argv[2]);
	return 0;
}
