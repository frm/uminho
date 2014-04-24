#include "author_catalog_years.h"

AVL_DEF(YearEntry, int)

/* AVL Function */
int yearTreeYieldAuthorFromYear(YearTree tree, int year, char **author) {
	static YearEntry yearContent = {0, 0, NULL};
	YearEntryAVLNode node;

	if (!yearEntryGetAuthors(yearContent) || yearEntryGetYear(yearContent) != year){
		node = __avlYearEntryFind(tree->compare, tree->root, NULL, &year);

		if (!node)
			return -1;

		yearContent = node->content;
	}

	return authorTreeYield(yearEntryGetAuthors(yearContent), author);
}
/* ******************* */

YearEntry newYearEntry(int year) {
	YearEntry new;

	new.year = year;
	new.total_authors = 1;
	new.authors = authorTreeNew();

	return new;
}

static void colideYearEntry(YearEntry* inTree, YearEntry* outTree) {
	Author buffer;
	int avl_empty = 0;

	(inTree -> total_authors) += (outTree -> total_authors);

	while (!avl_empty) {
		avl_empty = authorTreeYield(outTree -> authors, &buffer);

		if ( avl_empty != -1 ) {
			authorTreeInsert(inTree -> authors, buffer);
			free(buffer);
		}
	}
}

static int compareYearEntry(int* key_search, YearEntry* fst, YearEntry snd) {
	int cmp;
	int key = key_search ? (*key_search) : (fst -> year);

	if (key > yearEntryGetYear(snd)) cmp = 1;
	else if (key < yearEntryGetYear(snd)) cmp = -1;
	else cmp = 0;

	return cmp;
}

void deleteYearEntry(YearEntry goodbye) {
	authorTreeDestroy(goodbye.authors);
}

int yearEntryGetYear(YearEntry entry) {
	return entry.year;
}

int yearEntryGetTotalAuthors(YearEntry entry) {
	return entry.total_authors;
}

void yearEntrySetTotalAuthors(YearEntry *entry, int authors) {
	entry -> total_authors = authors;
}

AuthorTree yearEntryGetAuthors(YearEntry entry) {
	return entry.authors;
}

YearEntry cloneYearEntry(YearEntry original) {
	YearEntry new;

	new.year = yearEntryGetYear(original);
	new.total_authors = yearEntryGetTotalAuthors(original);
	new.authors = authorTreeClone( yearEntryGetAuthors(original) );

	return new;
}

int yearTreeInsert(YearTree tree, YearEntry new) {
	return avlInsert(YearEntry, tree, new);
}

int yearTreeYield(YearTree tree, YearEntry* buffer) {
	return avlYield(YearEntry, tree, buffer);
}

void yearTreeRewindGenerator(YearTree tree) {
	avlRewindGenerator(YearEntry, tree);
}

YearTree yearTreeNew() {
	return avlNewComplete(YearEntry, &compareYearEntry, &colideYearEntry, &deleteYearEntry, &cloneYearEntry);
}

void yearTreeDestroy(YearTree tree) {
	avlDestroy(YearEntry, tree);
}

YearTree yearTreeClone(YearTree tree) {
	return avlClone(YearEntry, tree);
}

int yearTreeExists(YearTree tree, int year) {
	return avlExists(YearEntry, tree, year);
}

int yearEntryAddAuthor(YearEntry entry, Author author) {
    return authorTreeInsert(yearEntryGetAuthors(entry), author);
}

int yearEntryYieldAuthor(YearEntry year, char** author) {
	return authorTreeYield(year.authors, author);
}

int yearTreeFind(YearTree tree, int year, YearEntry *ret) {
	return avlFind(YearEntry, tree, year, ret);
}

