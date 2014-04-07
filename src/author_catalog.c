/*#include <string.h>
#include <avl.h>
#include <vector.h>

AuthorEntryAVL AuthorCatalogEntries[27];

typedef AuthorPtr AuthorEntry*;

typedef struct year_publ {
	int year;
	int nr_publications;
} YearPubl;

typedef struct coauthor_publ {
	AuthorPtr coauthor;
	int nr_publications;
} CoAuthPubl;
	
VECTOR_DEF(YearPubl)
VECTOR_DEF(CoAuthPubl)

typedef struct author_entry {
	char* name;
	YearPublVector publ_info;
	CoAuthPublVector coauth_info;
} AuthorEntry;

VECTOR_DEF(AuthorPtr);

typedef struct year_author {
	int year;
	int total;
	AuthorPtrVector authors;
} YearAuthor;

YearPubl cloneYearPubl(YearPubl original) {
	return original;
}

int addYearPublication(YearPublVector v, int year) {
	int updated = 0;
	int i = 0;
	YearPubl updater;

	while ( vecYearPublGet(v, i, &updater) == 0 && !updated ) {
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
	/*return 1 - updated;
}


CoAuthPubl cloneCoAuthPubl(CoAuthPubl original) {
	return original;
}

int addCoAuthPublication(CoAuthPublVector v, AuthorPtr coauthor) {
	int updated = 0;
	int i = 0;
	CoAuthPubl updater;

	while( vecCoAuthPublGet(v, i, &updater) == 0 && !updated ) {
		if ( updater.coauthor == coauthor ) {
			updater.nr_publications++;
			vecCoAuthPublUpdate(v, i, updater);
			updated = 1;
		}
		i++;
	}

	if (!updated) {
		updater.nr_publications = 1;
		updater.coauthor = coauthor;
		vecCoAuthPublAppend(v, updater);
	}

	return 1 - updated;
}

AVL_DEF(AuthorEntry, char*)

int compareAuthorEntry(char** key_search, AuthorEntry* fst, AuthorEntry snd) {
	int cmp;
	char* key = key_search ? (*key_search) : fst -> name;

	return strcmp(key, snd.name);
}

AuthorEntry cloneAuthorEntry(AuthorEntry original) {
	AuthorEntry clone;
	int size = strlen(original.name) + 1;

	clone.name = (char*)malloc( sizeof(char) * size );
	strncpy(clone.name, original.name, sizeof(char) * size);
	
	clone.coauth_info = vecCoAuthorPublClone(original.coauth_info);
	clone.publ_info = vecYearPublClone(original.publ_info);

	return clone;
}

void deleteAuthorEntry(AuthorEntry a) {
	free(a.name);
	vecCoAuthPublDestroy(a.coauth_info);
	vecYearPublDestroy(a.publ_info);
}


AuthorPtr cloneAuthorPtr(AuthorPtr original) {
	return original;
}

int addAuthorPtr(AuthorPtrVector v, AuthorPtr author) {
	int i = 0;
	int found = 0;
	AuthorPtr keeper;

	while ( vecAuthorPtrGet(v, i, &keeper) == 0 && !found ) {
		if (keeper == author)
			found = 1;
		i++;
	}
	
	if (!found)
		vecAuthorPtrAppend(v, author);
	
	return 1 - found;
}

AVL_DEF(YearAuthor, int)

int compareYearAuthor(int* key_search, YearAuthor *fst, YearAuthor snd) {
	int cmp;
	int key = key_search? *key_search : fst -> year;

	if ( key > snd.year ) cmp = 1;
	else if ( key < snd.year ) cmp = -1;
	else cmp = 0;

	return cmp;
}

YearAuthor cloneYearAuthor(YearAuthor a) {
	YearAuthor clone;
	clone.year = a.year;
	clone.total = a.total;
	clone.authors = vecAuthorPtrClone(a.authors);

	return clone;
}

/* Your vector needs:
 * Updater with Append (needs comparing function) (check my code repetition)
 * Find
 * Your AVL needs:
 * __avlInsertFind
 */
