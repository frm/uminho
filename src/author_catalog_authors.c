#include "author_catalog_authors.h"

/* Defining an AVL of Pairs composed of Years and number of publications in given year */
AVL_DEF(YearPublPair, int)

static YearPublPair newYearPublPair(int year) {
	YearPublPair new;

	new.year = year;
	new.nr_publications = 1;

	return new;
}

static void colideYearPublPair(YearPublPair* fst, YearPublPair *snd) {
	(fst -> nr_publications) += ( snd -> nr_publications );
}

static int compareYearPublPair(int* key_search, YearPublPair* fst, YearPublPair snd) {
	int cmp;
	int key = key_search ? (*key_search) : (fst -> year);

    
	if ( key > snd.year ) cmp = 1;
	else if ( key < snd.year ) cmp = -1;
	else cmp = 0;

	return cmp;
}

/* Defining an AVL composed of Pairs of Coauthors (Authors) and number of publications with given author */

AVL_DEF(CoAuthorPublPair, Author)

static CoAuthorPublPair newCoAuthorPublPair(Author name) {
	CoAuthorPublPair new;
	int size = strlen(name) + 1;

	new.coauthor = (Author)malloc( sizeof(char) * size );
	strncpy(new.coauthor, name, sizeof(char) * size);
	new.nr_publications = 1;

	return new;
}

static void colideCoAuthorPublPair(CoAuthorPublPair* fst, CoAuthorPublPair* snd) {
	( fst -> nr_publications )+= ( snd -> nr_publications );
}

void deleteCoAuthorPublPair(CoAuthorPublPair pair) {
	free(pair.coauthor);
}

CoAuthorPublPair cloneCoAuthorPublPair(CoAuthorPublPair original) {
	CoAuthorPublPair new = newCoAuthorPublPair(original.coauthor);
	new.nr_publications = original.nr_publications;

	return new;
}

static int compareCoAuthorPublPair(Author* key_search, CoAuthorPublPair* fst, CoAuthorPublPair snd) {
	Author key = key_search ? (*key_search) : (fst -> coauthor);

	return strcmp(key, snd.coauthor);
}


/* Defining the Author info AVL
 * This contains the author name
 * an AVL with a pair of year and nr of publications in that year
 * and an AVL with a pair of coauthor and nr of publications with that coauthor
 */

AVL_DEF(AuthorInfo, Author)

/* AVL FUNCTION */

int authorInfoGetAuthorPublicationsInYear(AuthorInfoTree tree, Author author, int year) {
    AuthorInfoAVLNode node;
    YearPublPair yearInfo;

    node = __avlAuthorInfoFind(tree->compare, tree->root, NULL, &author);

    if (!node)
        return -1;

    if (avlFind(YearPublPair, node->content.publications_info, year, &yearInfo)){
        return 0;
    }

    return yearInfo.nr_publications;
}

/* ************ */
AuthorInfo newAuthorInfo(Author name) {
	AuthorInfo new;
	int size = strlen(name) + 1;

	new.author = (Author)malloc(sizeof(char) * size);
	strncpy(new.author, name, sizeof(char) * size);

	new.publications_info = avlNewComplete(YearPublPair, &compareYearPublPair, &colideYearPublPair, NULL, NULL);
	new.coauthors_info = avlNewComplete(CoAuthorPublPair, &compareCoAuthorPublPair, &colideCoAuthorPublPair, &deleteCoAuthorPublPair, &cloneCoAuthorPublPair);

	return new;
}

void deleteAuthorInfo(AuthorInfo info) {
	free(info.author);
	avlYearPublPairDestroy(info.publications_info);
	avlCoAuthorPublPairDestroy(info.coauthors_info);
}

static AuthorInfo cloneAuthorInfo(AuthorInfo original) {
	AuthorInfo new;
	int size = strlen(original.author) + 1;

	new.author = (Author)malloc(sizeof(char) * size);
	strncpy(new.author, original.author, sizeof(char) * size);
	new.publications_info = avlYearPublPairClone(original.publications_info);
	new.coauthors_info = avlCoAuthorPublPairClone(original.coauthors_info);

	return new;
}

static int compareAuthorInfo(Author* key_search, AuthorInfo* fst, AuthorInfo snd) {
	Author key = key_search ? (*key_search) : (fst -> author);
	return strcmp(key, snd.author);
}

static void catYearPublTree(YearPublPairAVL* source, YearPublPairAVL* destination) {
	YearPublPair buffer;
	int avl_empty = 0;

	while (!avl_empty) {
		avl_empty = avlYield(YearPublPair, *source, &buffer);
		if ( avl_empty != -1 )
			avlInsert(YearPublPair, *destination, buffer);
	}
}

static void catCoAuthorPublTree(CoAuthorPublPairAVL* source, CoAuthorPublPairAVL* destination) {
	CoAuthorPublPair buffer;
	int avl_empty = 0;

	while (!avl_empty) {
		avl_empty = avlYield(CoAuthorPublPair, *source, &buffer);
		if ( avl_empty != -1 ) {
			avlInsert(CoAuthorPublPair, *destination, buffer);
			deleteCoAuthorPublPair(buffer);
		}
	}
}
static void colideAuthorInfo(AuthorInfo* inTree, AuthorInfo* new) {
	catYearPublTree( &(new -> publications_info), &(inTree -> publications_info) );
	catCoAuthorPublTree( &(new -> coauthors_info), &(inTree -> coauthors_info) );
}

int authorInfoTreeInsert(AuthorInfoTree tree, AuthorInfo new) {
	return avlInsert(AuthorInfo, tree, new);
}

int authorInfoTreeYield(AuthorInfoTree tree, AuthorInfo *ret) {
	return avlYield(AuthorInfo, tree, ret);
}

void authorInfoTreeRewindGenerator(AuthorInfoTree tree) {
	avlRewindGenerator(AuthorInfo, tree);
}

AuthorInfoTree authorInfoTreeNew() {
	return avlNewComplete(AuthorInfo, &compareAuthorInfo, &colideAuthorInfo, &deleteAuthorInfo, &cloneAuthorInfo);
}

void authorInfoTreeDestroy(AuthorInfoTree tree) {
	avlDestroy(AuthorInfo, tree);
}

AuthorInfoTree authorInfoTreeClone(AuthorInfoTree tree) {
	return avlClone(AuthorInfo, tree);
}

int authorInfoAddCoAuthor(AuthorInfo author, Author coauthor) {
	CoAuthorPublPair new = newCoAuthorPublPair(coauthor);
	int ret = avlInsert(CoAuthorPublPair, author.coauthors_info, new);
	deleteCoAuthorPublPair(new);
	return ret;
}

int authorInfoAddYear(AuthorInfo author, int year) {
	YearPublPair new = newYearPublPair(year);
	return avlInsert(YearPublPair, author.publications_info, new);
}

#ifdef DEBUG2

static void authorInfoPrint(AuthorInfo author) {
	YearPublPair pair;
	CoAuthorPublPair ca;
	int test;

	printf("%s\n", author.author);

	printf("\tPUBLICATIONS BY YEAR:\n");
	for (;;) {
    	test = avlYield(YearPublPair, author.publications_info, &pair);

    	if (!test) {
    		printf("\t\tYEAR: %d\t\tTOTAL: %d\n", pair.year, pair.nr_publications);
    	}
    	else {
    		if (test == 1) {
    			printf("\t\tYEAR: %d\t\tTOTAL: %d\n", pair.year, pair.nr_publications);
    		}

    		break;
    	}
    }

	printf("\n\tPUBLICATIONS BY COAUTHOR:\n");
	for (;;) {
    	test = avlYield(CoAuthorPublPair, author.coauthors_info, &ca);

    	if (!test) {
    		printf("\t\tCOAUTHOR: %s\n\t\t\tTOTAL: %d\n", ca.coauthor, ca.nr_publications);
    	}
    	else {
    		if (test == 1) {
    			printf("\t\tCOAUTHOR: %s\n\t\t\tTOTAL: %d\n", ca.coauthor, ca.nr_publications);
    		}

    		break;
    	}
    }
}

void authorInfoTreePrint(AuthorInfoTree tree) {
    AuthorInfo author;
    int test;

    for (;;) {
    	test = avlYield(AuthorInfo, tree, &author);

    	if (!test) {
    		authorInfoPrint(author);
    	}
    	else {
    		if (test == 1) {
    			authorInfoPrint(author);
    		}

    		break;
    	}
    }
}

#endif