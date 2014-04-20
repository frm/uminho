#include "author_catalog.h"
#include <heap.h>
#include <stdio.h>

#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )

static AuthorInfoTree	CatalogAuthors[27];
static YearTree			CatalogYears;

/* *********** HEAP FOR QUERY 12 *********** */

HEAP_DEF_HEADER(CoAuthorPublPair)
HEAP_DEF(CoAuthorPublPair)

int cmpMinCoAuthorPublPair(CoAuthorPublPair fst, CoAuthorPublPair snd) {
    if (fst.nr_publications > snd.nr_publications)
        return -1;
    else if (fst.nr_publications < snd.nr_publications)
        return 1;
    else
        return 0;
}

/* ***************************************** */

void initializeAuthorCatalog() {
	int i;

	for(i = 0; i < 27; i++) {
		CatalogAuthors[i] = authorInfoTreeNew();
	}

	CatalogYears = yearTreeNew();
}

void deleteAuthorCatalog() {
	int i;

	for(i = 0; i < 27; i++)
		authorInfoTreeDestroy(CatalogAuthors[i]);

	yearTreeDestroy(CatalogYears);
}

static void addYearToCatalog(int year, Author author) {
    AuthorInfo new_author_info = newAuthorInfo(author);
    YearEntry new_year = newYearEntry(year);

    authorInfoAddYear(new_author_info, year);
    authorInfoTreeInsert( CatalogAuthors[ GET_CHAR_INDEX(author[0]) ], new_author_info );

    yearEntryAddAuthor(new_year, author);
    yearTreeInsert(CatalogYears, new_year);

    deleteYearEntry(new_year);
    deleteAuthorInfo(new_author_info);
}

static int addAuthorToCatalog(Author author, Author coauthor) {
	AuthorInfo buffer;
	int ret;

	buffer = newAuthorInfo(author);
	authorInfoAddCoAuthor(buffer, coauthor);
	ret = authorInfoTreeInsert( CatalogAuthors[ GET_CHAR_INDEX(author[0]) ], buffer );

	deleteAuthorInfo(buffer);
	return ret;
}

int insertToCatalog(Author* author_buffer, int size) { /* Note that author_buffer[size] contains the year */
	int i, j;
	int year;
    sscanf(author_buffer[size], "%d\n", &year);

	for (i = 0; i < size; i++) {
    /* i < size as well as j < size because if we put i < size - 1, the last author won't add the year of publication */
		for (j = i + 1; j < size; j++) {
			addAuthorToCatalog( author_buffer[i], author_buffer[j] );
			addAuthorToCatalog( author_buffer[j], author_buffer[i] );
		}

	addYearToCatalog(year, author_buffer[i]);
	}

	return 0;
}

int getAuthorPublicationsInYear(Author author, int year) {
    return authorInfoGetAuthorPublicationsInYear(CatalogAuthors[GET_CHAR_INDEX(author[0])], author, year);
}

char **getTopAuthorsInYear(int year, int n) {
    CoAuthorPublPairHeap authorHeap;
    CoAuthorPublPair pair, auxPair;
    char **authorList;
    char *tempAuthor;
    char *na = "NOT AVAILABLE";
    int test, i, j, total;

    i = 0;
    auxPair.coauthor = NULL;
    authorHeap = heapNewComplete(CoAuthorPublPair, n, &cmpMinCoAuthorPublPair, &deleteCoAuthorPublPair, &cloneCoAuthorPublPair);
    authorList = (char **)malloc(sizeof(char *) * n);

    do {
        test = yearTreeYieldAuthorFromYear(CatalogYears, year, &tempAuthor);

        if (test == 0 || test == 1) {
            total = authorInfoGetAuthorPublicationsInYear(CatalogAuthors[GET_CHAR_INDEX(tempAuthor[0])], tempAuthor, year);
            pair.coauthor = tempAuthor;
            pair.nr_publications = total;
            heapInsert(CoAuthorPublPair, authorHeap, pair);
            free(pair.coauthor);
            i++;
        }
    } while(!test && i < n);

    while (!test) {
        test = yearTreeYieldAuthorFromYear(CatalogYears, year, &tempAuthor);

        if (test == 0 || test == 1) {
            total = getAuthorPublicationsInYear(tempAuthor, year);
            pair.coauthor = tempAuthor;
            pair.nr_publications = total;

            if (!auxPair.coauthor)
                heapTop(CoAuthorPublPair, authorHeap, &auxPair);

            if (cmpMinCoAuthorPublPair(pair, auxPair) == -1) {
                free(auxPair.coauthor);
                heapGet(CoAuthorPublPair, authorHeap, &auxPair);
                free(auxPair.coauthor);
                auxPair.coauthor = NULL;
                heapInsert(CoAuthorPublPair, authorHeap, pair);
            }
            free(pair.coauthor);
        }
    }

    free(auxPair.coauthor);

    for (j = i; j < n; j++) {
        authorList[j] = (char *)malloc(sizeof(char) * (strlen(na) + 1));
        strcpy(authorList[j], na);
    }

    for (j = 1; j <= i; j++) {
        heapGet(CoAuthorPublPair, authorHeap, &pair);

        authorList[i - j] = pair.coauthor;
    }

    heapDestroy(CoAuthorPublPair, authorHeap);

    return authorList;
}

int getSoloAuthors() {
	int i, avl_empty;
    int total = 0;
	AuthorInfo buffer;

	for(i = 0; i < 27; i++) {
        avl_empty = 0;

        while (!avl_empty) {
            avl_empty = authorInfoTreeYield( CatalogAuthors[i], &buffer );
            if ( avl_empty != -1 ) {
                if (! has_coauthors(buffer) ) total++;
                deleteAuthorInfo(buffer);
            }
        }
    }
	return total;
}

static int** createIntMatrix(int width, int breadth) {
	int** matrix = (int**)malloc(sizeof(int*) * width);
	while (width > 0)
		matrix[width--] = (int*)malloc(sizeof(int) * breadth);

	return matrix;
}

int** getYearPublMatrix(Author author, int* size) {
	int i = 0;
	int** matrix = createIntMatrix(128, 2);
	AuthorInfo buffer;

	authorInfoTreeFind(CatalogAuthors[ GET_CHAR_INDEX(author[0]) ], author, &buffer);

	while ( getYearPublPair(buffer, matrix[i], matrix[i] + 1) != -1 )
		i++;

	*size = i;
	deleteAuthorInfo(buffer);

	return matrix;
}


#ifdef DEBUG2

void printCatalog() {
    int i;

    for(i = 0; i < 27; i++)
        authorInfoTreePrint(CatalogAuthors[i]);
}
#endif
