
#include "../src/server.h"
#include "../src/server_lib.h"

#include <stdlib.h>

int main(int argc, char* argv[]){
	char* prefix[4] = { "Braga", "Guimarães", "Pevidém", NULL };
	incrementar(prefix, 2);
    agregar(prefix, 0, "brolol");
	incrementar(prefix, 1);
	return 0;
}
