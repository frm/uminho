#include <string.h>
#include <avl.h>
#include <vector.h>
#include "author_index_tree.h"
#include "author_catalog.h"

#define getMatrixAuthorIndex(author, i)		(author + 128 * i)

typedef struct year_author YearAuthor;
typedef struct author_entry AuthorEntry;
typedef struct coauthor_publ CoAuthPubl;
typedef struct year_publ YearPubl;
typedef AuthorEntry* AuthorPtr;

VECTOR_DEF_HEADER(YearPubl)
VECTOR_DEF_HEADER(CoAuthPubl)
AVL_DEF_HEADER(AuthorPtr, AuthorPtr)

struct year_publ {
	int year;
	int nr_publications;
};

struct coauthor_publ {
	AuthorPtr coauthor;
	int nr_publications;
};

VECTOR_DEF(YearPubl)
VECTOR_DEF(CoAuthPubl)

struct author_entry {
	char* name;
	YearPublVector publ_info;
	CoAuthPublVector coauth_info;
};

AVL_DEF(AuthorPtr, AuthorPtr)

struct year_author {
	int year;
	int total;
	AuthorPtrAVL authors;
};

AVL_DEF_HEADER(AuthorEntry, char*)
AVL_DEF_HEADER(YearAuthor, int)

static AuthorEntryAVL AuthorCatalogEntries[27];
static YearAuthorAVL AuthorCatalogYears;

static int compareAuthorPtr(AuthorPtr* key_search, AuthorPtr* fst, AuthorPtr snd) {
	int cmp;
	AuthorPtr key = key_search ? (*key_search) : (*fst);

	if ( key < snd) cmp = -1;
	else if (key > snd) cmp = 1;
	else cmp = 0;

	return cmp;
}


static int addYearPublication(YearPublVector v, int year) {
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
	return 1 - updated;
}


static int addCoAuthPublication(CoAuthPublVector v, AuthorPtr coauthor) {
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

static int compareAuthorEntry(char** key_search, AuthorEntry* fst, AuthorEntry snd) {
	char* key = key_search ? (*key_search) : fst -> name;

	return strcmp(key, snd.name);
}

static AuthorEntry cloneAuthorEntry(AuthorEntry original) {
	AuthorEntry clone;
	int size = strlen(original.name) + 1;

	clone.name = (char*)malloc( sizeof(char) * size );
	strncpy(clone.name, original.name, sizeof(char) * size);

	clone.coauth_info = vecCoAuthPublClone(original.coauth_info);
	clone.publ_info = vecYearPublClone(original.publ_info);

	return clone;
}

static void deleteAuthorEntry(AuthorEntry a) {
	free(a.name);
	vecCoAuthPublDestroy(a.coauth_info);
	vecYearPublDestroy(a.publ_info);
}

static AuthorEntry newAuthorEntry(char* name) {
	AuthorEntry newAuthorEntry;
	newAuthorEntry.name = (char*) malloc( sizeof(char) * ( strlen(name) + 1) );
	strncpy(newAuthorEntry.name, name, sizeof(char) * ( strlen(name) + 1 ) );

	newAuthorEntry.publ_info = vecNew(YearPubl, 200);
	newAuthorEntry.coauth_info = vecNew(CoAuthPubl, 20);

	return newAuthorEntry;
}

AVL_DEF(YearAuthor, int)

static int compareYearAuthor(int* key_search, YearAuthor *fst, YearAuthor snd) {
	int cmp;
	int key = key_search? *key_search : fst -> year;

	if ( key > snd.year ) cmp = 1;
	else if ( key < snd.year ) cmp = -1;
	else cmp = 0;

	return cmp;
}

static YearAuthor cloneYearAuthor(YearAuthor a) {
	YearAuthor clone;
	clone.year = a.year;
	clone.total = a.total;
	clone.authors = avlAuthorPtrClone(a.authors);

	return clone;
}

static void deleteYearAuthor(YearAuthor y) {
	avlAuthorPtrDestroy(y.authors);
}

static YearAuthor newYearAuthor(int year) {
	YearAuthor newYearAuthor;
	newYearAuthor.year = year;
	newYearAuthor.total = 0;
	newYearAuthor.authors = avlNew(AuthorPtr, &compareAuthorPtr);

	return newYearAuthor;
}

void initializeAuthorCatalog() {
	int i;
	for (i = 0; i < 27; i++)
		AuthorCatalogEntries[i] = avlNewComplete(AuthorEntry, &compareAuthorEntry, NULL, &deleteAuthorEntry, &cloneAuthorEntry);

	AuthorCatalogYears = avlNewComplete(YearAuthor, &compareYearAuthor, NULL, &deleteYearAuthor, &cloneYearAuthor);
}

void deleteAuthorCatalog() {
	int i;
	for (i = 0; i < 27; i++)
		avlAuthorEntryDestroy( AuthorCatalogEntries[i] );

	avlYearAuthorDestroy(AuthorCatalogYears);
}

static YearAuthor* getYearAuthorFromNode(YearAuthorAVL t, YearAuthor placeholder) {
	YearAuthorAVLNode node;
	__avlYearAuthorInsertFind(t, placeholder, &node);
	return &(node -> content);
}

static AuthorEntry* getAuthorEntryFromNode(AuthorEntryAVL t, AuthorEntry placeholder) {
	AuthorEntryAVLNode node;
	__avlAuthorEntryInsertFind(t, placeholder, &node);
	return &(node -> content);
}

int insertToCatalog( char *author_buffer, int size ) {
	int i, j, index, year;
	YearAuthor* year_position;
	YearAuthor current_year;

	AuthorEntry newEntry;
	AuthorEntry** location = (AuthorEntry**)malloc(sizeof(AuthorEntry*) * (size) );

	sscanf(getMatrixAuthorIndex(author_buffer, size), "%d", &year);
	current_year = newYearAuthor(year);
	year_position = getYearAuthorFromNode(AuthorCatalogYears, current_year);

	for (i = 0; i < size; i++) {
		index = getLetterIndex( getMatrixAuthorIndex(author_buffer, i)[0] );

		newEntry = newAuthorEntry( getMatrixAuthorIndex(author_buffer, i) );
		location[i] = getAuthorEntryFromNode( AuthorCatalogEntries[index], newEntry );

		deleteAuthorEntry(newEntry);
	}

	for (i = 0; i < size - 1; i++) {
		for (j = i + 1; j < size; j++) {
			addCoAuthPublication( location[i] -> coauth_info, location[j] );
			addCoAuthPublication( location[j] -> coauth_info, location[i] );
		}

		addYearPublication(location[i] -> publ_info, year);
		if ( avlAuthorPtrInsert( year_position -> authors, location[i] ) == 1 )
			( year_position -> total )++;
	}

	deleteYearAuthor(current_year);
	free(location);

	return 0;
}

