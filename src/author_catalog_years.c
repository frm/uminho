#include "author_catalog_years.h"

struct year_entry {
	int year;
	AuthorTree authors;
};

AVL_DEF(YearEntry, int)

YearEntry newYearEntry(int year) {
	YearEntry new;

	new.year = year;
	new.authors = authorTreeNew();

	return new;
}

static void colideYearEntry(YearEntry* inTree, YearEntry* outTree) {
	Author buffer;

	while ( authorTreeYield(outTree -> authors, buffer) )
		authorTreeInsert(inTree -> authors, buffer);
}

static int compareYearEntry(int* key_search, YearEntry* fst, YearEntry snd) {
	int cmp;
	int key = key_search ? (*key_search) : (fst -> year);

	if (key > snd.year) cmp = 1;
	else if (key < snd.year) cmp = -1;
	else cmp = 0;

	return cmp;
}

static void deleteYearEntry(YearEntry goodbye) {
	authorTreeDestroy(goodbye.authors);
}

static void cloneYearEntry(YearEntry original) {
	YearEntry new = newYearEntry(original.year);
	
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
	return avlClone(YearEntry, tree)
}

