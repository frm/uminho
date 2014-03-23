#include <string.h>
#include <stdio.h>
#include "../gestauts/lib/headers/strhandler.h"

int main() {
	char mystr[] = "  123456789  ";

	printf("BEFORE TRIMMING: length - %d, str: %s\n", (int)strlen(mystr), mystr);

	strtrim(mystr);

	printf("AFTER TRIMMING: length - %d, str: %s\n", (int)strlen(mystr), mystr);

	return 0;
}
