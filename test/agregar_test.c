
#include "../src/server.h"
#include "../src/server_lib.h"

#include <stdlib.h>

int main(){
	char* prefix[4] = { "Braga", "Guimarães", "Pevidém", NULL };
    char* prefix2[4] = { "Braga", "Braga", "Real", NULL};
    incrementar(prefix, 3);
    incrementar(prefix2, 3);
    incrementar(prefix, 4);
    incrementar(prefix2, 2);
	agregar(prefix, 0, "./deve_dar_12");
    agregar(prefix2, 1, "deve_dar_5");
	return 0;
}
