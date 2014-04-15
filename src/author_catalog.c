#include "author_catalog.h"
#define GET_CHAR_INDEX(c) (normalLetter(c) ? ((int)c - (int)'A') : 26)
#define normalLetter(c)     ( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )

static AuthorInfoTree	CatalogAuthors[27];
static YearTree			CatalogYears;

static void addYearToCatalog(int year, Author author) {
	AuthorInfo buffer = newAuthorInfo(author);
	
	authorInfoAddYear(buffer, year);
	authorInfoTreeInsert( CatalogAuthors[ GET_CHAR_INDEX(author[0]) ], buffer );

	yearTreeInsert(CatalogYears, author);

	deleteAuthorInfo(buffer);
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
	int year = atoi( author_buffer[size] );

	for (i = 0; i < size - 1; i++) {
		for (j = i + 1; j < size; j++) {
			addAuthorToCatalog( author_buffer[i], author_buffer[j] );
			addAuthorToCatalog( author_buffer[j], author_buffer[i] );
		}
	
	addYearToCatalog(year, author_buffer[i]);
	}
}
