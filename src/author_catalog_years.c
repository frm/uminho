#include "author_catalog_years.h"

AVL_DEF(YearEntry, int)

/* AVL Function */
int yearTreeYieldAuthorFromYear(YearTree tree, int year, char **author) {
	static YearEntry yearContent = {0, NULL};
	YearEntryAVLNode node;

	if (!yearContent.authors || yearContent.year != year){
		node = __avlYearEntryFind(tree->compare, tree->root, NULL, &year);
		yearContent = node->content;
	}

	return authorTreeYield(yearContent.authors, author);
}
/* ******************* */

YearEntry newYearEntry(int year) {
	YearEntry new;

	new.year = year;
	new.authors = authorTreeNew();

	return new;
}

static void colideYearEntry(YearEntry* inTree, YearEntry* outTree) {
	Author buffer;
	int avl_empty = 0;

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

	if (key > snd.year) cmp = 1;
	else if (key < snd.year) cmp = -1;
	else cmp = 0;

	return cmp;
}

void deleteYearEntry(YearEntry goodbye) {
	authorTreeDestroy(goodbye.authors);
}

YearEntry cloneYearEntry(YearEntry original) {
	YearEntry new;

	new.year = original.year;
	new.authors = authorTreeClone(original.authors);

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

int yearEntryAddAuthor(YearEntry year, Author author) {
    return authorTreeInsert(year.authors, author);
}

int yearEntryFind(YearTree tree, int year, YearEntry *ret) {
	return avlFind(YearEntry, tree, year, ret);
}

