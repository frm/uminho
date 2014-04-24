#include "author_catalog.h"
#include <heap.h>
#include <stdio.h>
#include "author_index_stats.h"
#include "author_tree.h"
#include <limits.h>

#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )


static AuthorInfoTree	CatalogAuthors[27];
static YearTree			CatalogYears;

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

/* *********** HEAP FOR QUERY 12 *********** */

HEAP_DEF_HEADER(CoAuthorPublPair)
HEAP_DEF(CoAuthorPublPair)

int cmpMinCoAuthorPublPair(CoAuthorPublPair fst, CoAuthorPublPair snd) {
    int nr_fst, nr_snd;

    nr_fst = cpGetNrPublications(fst);
    nr_snd = cpGetNrPublications(snd);

    if (nr_fst > nr_snd)
        return -1;
    else if (nr_fst < nr_snd)
        return 1;
    else
        return 0;
}

/* ***************************************** */

char **getTopAuthorsInYear(int year, int n) {
    CoAuthorPublPairHeap authorHeap;
    CoAuthorPublPair pair, auxPair;
    char **authorList;
    char *tempAuthor;
    char *na = "NOT AVAILABLE";
    int test, i, j, total;

    i = 0;
    auxPair = cpSetCoauthor(auxPair, NULL);
    authorHeap = heapNewComplete(CoAuthorPublPair, n, &cmpMinCoAuthorPublPair, &deleteCoAuthorPublPair, &cloneCoAuthorPublPair);
    authorList = (char **)malloc(sizeof(char *) * n);

    do {
        test = yearTreeYieldAuthorFromYear(CatalogYears, year, &tempAuthor);

        if (test == 0 || test == 1) {
            total = authorInfoGetAuthorPublicationsInYear(CatalogAuthors[GET_CHAR_INDEX(tempAuthor[0])], tempAuthor, year);
            pair = cpSetCoauthor(pair, tempAuthor);
            pair = cpSetNrPublications(pair, total);
            heapInsert(CoAuthorPublPair, authorHeap, pair);
            free(cpGetCoauthor(pair));
            i++;
        }
    } while(!test && i < n);

    while (!test) {
        test = yearTreeYieldAuthorFromYear(CatalogYears, year, &tempAuthor);

        if (test == 0 || test == 1) {
            total = getAuthorPublicationsInYear(tempAuthor, year);
            pair = cpSetCoauthor(pair, tempAuthor);
            pair = cpSetNrPublications(pair, total);

            if (!cpGetCoauthor(auxPair))
                heapTop(CoAuthorPublPair, authorHeap, &auxPair);

            if (cmpMinCoAuthorPublPair(pair, auxPair) == -1) {
                free(cpGetCoauthor(auxPair));
                heapGet(CoAuthorPublPair, authorHeap, &auxPair);
                free(cpGetCoauthor(auxPair));
                auxPair = cpSetCoauthor(auxPair, NULL);
                heapInsert(CoAuthorPublPair, authorHeap, pair);
            }
            free(cpGetCoauthor(pair));
        }
    }

    free(cpGetCoauthor(auxPair));

    for (j = i; j < n; j++) {
        authorList[j] = (char *)malloc(sizeof(char) * (strlen(na) + 1));
        strcpy(authorList[j], na);
    }

    for (j = 1; j <= i; j++) {
        heapGet(CoAuthorPublPair, authorHeap, &pair);

        authorList[i - j] = cpGetCoauthor(pair);
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
    int i = 0;
	while (i < width)
		matrix[i++] = (int*)malloc(sizeof(int) * breadth);

	return matrix;
}

void deleteIntMatrix(int** matrix) {
    int i;
    for (i = 0; i < 128; i++)
        free(matrix[i]);

    free(matrix);
}

int** getYearPublMatrix(Author author, int* size) {
	int i = 0, avl_empty = 0;
	int** matrix = createIntMatrix(128, 2);
	AuthorInfo buffer;

	if ( authorInfoTreeFind(CatalogAuthors[ GET_CHAR_INDEX(author[0]) ], author, &buffer) == -1 ) {
        *size = 0;
        return matrix;
    }

	while ( !avl_empty ) {
        avl_empty = getYearPublPair(buffer, matrix[i], matrix[i] + 1);
        if (avl_empty != -1) i++;
    }
    *size = i;

	deleteAuthorInfo(buffer);

	return matrix;
}

static int addToAuthorMatrix(Author* author_matrix, int size, Author new_author) {
    author_matrix[size] = (Author)malloc( sizeof(char) * ( strlen(new_author) + 1 ) );
    strncpy( author_matrix[size], new_author, sizeof(char) * ( strlen(new_author) + 1 ) );
    return size + 1;
}

static int restartAuthorMatrix(Author* author_matrix, int size, Author new_author) {
    int i;

    for (i = 0; i < size; i++)
        free(author_matrix[i]);

    return addToAuthorMatrix(author_matrix, 0, new_author);
}

/* Function that should return the year with smallest amount of authors */
static YearEntry getYearWithLeastAuthors(int min, int max) {
    YearEntry min_year = newYearEntry(min);
    YearEntry buffer, tmp;
    int avl_empty = 0;

    yearEntrySetTotalAuthors(&min_year, INT_MAX);

    while ( !avl_empty ) {
        avl_empty = yearTreeYield(CatalogYears, &buffer);
        if ( avl_empty != -1 && yearEntryGetYear(buffer) <= max && yearEntryGetYear(buffer) >= min ) {
            if( yearEntryGetTotalAuthors(buffer) < yearEntryGetTotalAuthors(min_year) ) {
                tmp = buffer;
                buffer = min_year;
                min_year = tmp;
            }
            deleteYearEntry(buffer);
        }
    }

    return min_year;
}

/* Simply return the author_tree within that year */
static char** getAuthorsInYear(int year, int* ret) {
    YearEntry buffer;
    char** author_list;

    yearTreeFind(CatalogYears, year, &buffer);
    author_list = authorTreeToString( yearEntryGetAuthors(buffer), ret );
    deleteYearEntry(buffer);

    return author_list;
}

static int authorPostedByYield(Author curr_author, int min, int max) {
    int posted = 1, avl_empty = 0, curr_year;
    AuthorInfo author_info;
    YearPublPair yearpubl;

    authorInfoTreeFind( CatalogAuthors[ GET_CHAR_INDEX(curr_author[0]) ], curr_author, &author_info );
    while ( !avl_empty && min <= max && posted ) {
        avl_empty = yieldYearPublPair(author_info, &yearpubl);

        if (avl_empty != -1) {
            curr_year = yearPublPairGetYear(yearpubl);

            if (curr_year < min)
                continue;
            else if (curr_year == min)
                min++;
            else
                posted = 0;
        }
    }

    deleteAuthorInfo(author_info);
    return posted;
}

static int authorPostedByFind(Author curr_author, int min, int max) {
    int posted = 1, avl_empty = 0;
    AuthorInfo author_info;

    authorInfoTreeFind( CatalogAuthors[ GET_CHAR_INDEX(curr_author[0]) ], curr_author, &author_info );

    while ( !avl_empty && min <= max && posted ) {
        if (avl_empty != -1) {
            if ( ! existsYearPublPair(author_info, min) )
                posted = 0;

            min++;
        }
    }

    deleteAuthorInfo(author_info);
    return posted;
}

/* Send the year inside each author avl */
static char** getAuthorsPostedIn(int min, int max, int find, int* ret) {
    YearEntry min_year = getYearWithLeastAuthors(min, max);
    AuthorTree authors = authorTreeNew();
    Author curr_author;
    char** author_list;
    int avl_empty = 0;
    int(* posted_function)(Author, int, int);

    if (find)
        posted_function = &authorPostedByFind;
    else
        posted_function = &authorPostedByYield;

    while ( !avl_empty ) {
        avl_empty = yearEntryYieldAuthor(min_year, &curr_author);
        if ( avl_empty != -1 ) {
            if ( posted_function(curr_author, min, max) )
                authorTreeInsert(authors, curr_author);

            free(curr_author);
        }
    }

    deleteYearEntry(min_year);
    author_list = authorTreeToString(authors, ret);
    authorTreeDestroy(authors);

    return author_list;
}

char** getAuthorsInInterval(int min, int max, int* ret) {
    int diff = max - min;
    char** author_list;

    /* Error handling */
    if ( diff < 0 ) return NULL;

    if ( diff == 0 )
        author_list = getAuthorsInYear(max, ret);

    else if ( diff < (int)( getMaxYear() - getMinYear() ) * 0.35 )
        author_list = getAuthorsPostedIn(min, max, 1, ret);

    else
        author_list = getAuthorsPostedIn(min, max, 0, ret);

    return author_list;
}

char** getMostCoauthor(Author author, int* nr_coauthors, int* nr_publications) {
    AuthorInfo author_buffer;
    CoAuthorPublPair coauthor_buffer;
    int avl_empty = 0, max_nr_publications = 0, size = 0, matrix_size = 128;
    Author* author_ary = (Author*)malloc(sizeof(Author) * matrix_size);

    if( authorInfoTreeFind(CatalogAuthors[ GET_CHAR_INDEX(author[0]) ], author, &author_buffer) != -1 ) {
        while ( !avl_empty ) {
            avl_empty = yieldCoAuthorPublPair(author_buffer, &coauthor_buffer);

            if (avl_empty != -1) {
                if ( cpGetNrPublications(coauthor_buffer) > max_nr_publications ) {
                    size = restartAuthorMatrix(author_ary, size, cpGetCoauthor(coauthor_buffer));
                    max_nr_publications = cpGetNrPublications(coauthor_buffer);
                }
                else if ( cpGetNrPublications(coauthor_buffer) == max_nr_publications ) {
                    size = addToAuthorMatrix(author_ary, size, cpGetCoauthor(coauthor_buffer));
                    if (size == matrix_size) {
                        author_ary = realloc(author_ary, sizeof(Author*) * matrix_size * 2);
                        matrix_size *= 2;
                    }
                }

                deleteCoAuthorPublPair(coauthor_buffer);
            }
        }
        deleteAuthorInfo(author_buffer);
    }

    *nr_coauthors = size;
    *nr_publications = max_nr_publications;
    return author_ary;
}

void deleteTopCoauthors(Author* list, int size) {
    int i;
    for (i = 0; i < size; i++)
        free(list[i]);

    free(list);
}

#ifdef DEBUG2

void printCatalog() {
    int i;

    for(i = 0; i < 27; i++)
        authorInfoTreePrint(CatalogAuthors[i]);
}
#endif
