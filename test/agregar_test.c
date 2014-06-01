
#include "../src/server.h"
#include "../src/server_lib.h"

#include <stdlib.h>

int main(int argc, char* argv[]){
	char* prefix[4] = { "Braga", "Guimarães", "Pevidém", NULL };
    agregar(prefix, 10, "./path/to/file");
	return 0;
}
