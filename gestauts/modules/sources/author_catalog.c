#include <string.h>
#include "../../lib/headers/avl.h"
#include "../../lib/headers/vector.h"

typedef struct year_publ {
	int year;
	int nr_publications;
} YearPubl;

typedef struct coauthor_publ {
	&(char*) coauthor;
	int nr_publications;
} CoAuthPubl;

typedef struct author_entry {
	char* name;
	YearPublVector publ_info;
	CoAuthorPublVector coauth_info;
} AuthorEntry;
