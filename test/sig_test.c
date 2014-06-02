#include <stdlib.h>
#include "../src/server_lib.h"

int main(){
	char* prefix[4] = { "Braga", "Guimarães", "Pevidém", NULL };
	agregar(prefix, 0, "brolol");
	return 0;
}
