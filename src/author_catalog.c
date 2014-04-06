#include <string.h>
#include <avl.h>
#include <vector.h>

typedef struct year_publ {
	int year;
	int nr_publications;
} YearPubl;

typedef struct coauthor_publ {
	char* coauthor;
	int nr_publications;
} CoAuthPubl;
	
VECTOR_DEF(YearPubl)
VECTOR_DEF(CoAuthPubl)

typedef struct author_entry {
	char* name;
	YearPublVector publ_info;
	CoAuthPublVector coauth_info;
} AuthorEntry;


YearPubl cloneYearPubl(YearPubl original) {
	return original;
}

int addYearPublication(YearPublVector v, int year) {
	int i = 0;
	int size = getYearPublVecSize(v);
	int updated = 0;
	YearPubl updater;

	while ( i < size && !updated ) {
		vecYearPublGet(v, i, &updater);
		if ( updater.year == year ) {
			updater.nr_publications++;
			vecYearPublUpdate(v, i, updater);
			updated = 1;
		}
		i++;
	}
	if (!updated) {
		updater.nr_publications = 1;
		updater.year = year;
		vecYearPublAppend(v, updater);
	}
	
	/* Return 0 if updated, 1 if appended */
	return 1 - updated;
}


CoAuthPubl cloneCoAuthPubl(CoAuthPubl original) {
	CoAuthPubl clone;

	clone.nr_publications = original.nr_publications;
	strncpy( clone.coauthor, original.coauthor, sizeof(char) * ( strlen(original.coauthor) + 1 ) );

	return clone;
}

int addCoAuthPublication(CoAuthPublVector v, char* coauthor) {
	int i = 0;
	int size = getVecSize(v);
	int updated = 0;
	CoAuthPubl updater;

	while( i < size && !updated ) {
		vecCoAuthPublGet(v, i, &updater);
		if ( strcmp(updater.coauthor, coauthor) == 0 ) {
			updater.nr_publications++;
			vecCoAuthPublUpdate(v, i, updater);
			updated = 1;
		}
		i++;
	}

	if (!updated) {
		updater.nr_publications = 1;
		strncpy( updater.coauthor, coauthor, sizeof(char) * ( strlen(coauthor) + 1) );
		vecCoAuthPublAppend(v, updater);
	}
	
	return 1 - updated;
}


/* Your vector needs:
 * Updater with Append (needs comparing function)
 * getVecSize
 * Find
 */
