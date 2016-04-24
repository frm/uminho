
#include "../src/server.h"
#include "../src/server_lib.h"

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[]){
	char* prefix[1024];
	int inc = atoi(argv[1]);
	for(int i = 0; i < argc - 2; i++)
		prefix[i] = argv[i+2];

	prefix[argc - 2] = NULL;

	incrementar(prefix, inc);
	return 0;
}
