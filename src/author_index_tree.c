#include "author_index.h"
#include <string.h>

#define normalLetter(c)		( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )

AVL_DEF(Author, Author)

static AuthorAVL letterIndex[27];

static void deleteAuthor(Author author) {
	free(author);
}

static Author cloneAuthor(Author author) {
	int size = strlen(author) + 1;

	Author clone = (Author)malloc( sizeof(char) * size);
	strncpy(clone, author, size);

	return clone;
}

static int compareAuthor(Author* key_author, Author* author1, Author author2) {
	Author key = key_author ? (*key_author) : (*author1);

	return strcmp( key, author2 );
}

static int getLetterIndex(char c) {
	return normalLetter(c) ? (int)c - (int)'A' : 26;
}

int insertAuthor(char* author) {
	Author casted_author = (Author)author;
	int index = getLetterIndex( casted_author[0] );

	return avlAuthorInsert( letterIndex[index], casted_author );
}

int getListOfAuthorsBy(char initial, char** author_list, int number_displays, int* number_read) {
	int i = 0;
	int index = getLetterIndex(initial);
	int reading;

	do {
		reading = avlAuthorYield( letterIndex[index], &(author_list[i]) );
		i++;
	}
	while ( i < number_displays && reading  == 0 );

	*number_read = i;

	return reading;
}

void rewindAuthor(char initial) {
	int index = getLetterIndex(initial);
	avlAuthorRewindGenerator( letterIndex[index] );
}

void initAuthorTree() {
	int i = 0;

	while (i < 27)
		letterIndex[i++] = avlNewComplete(Author, &compareAuthor, NULL, &deleteAuthor, &cloneAuthor);
}

void deleteAuthorIndexTree() {
	int i = 0;

	while (i < 27)
		avlAuthorDestroy( letterIndex[i++] );
}

#ifdef DEBUG

	static void printAuthorAVL( AuthorAVLNode tree ) {
		if (tree) {
			AuthorAVLNode left = avlAuthorGetLeftChild(tree);
			AuthorAVLNode right = avlAuthorGetRightChild(tree);

			if (left)
				printAuthorAVL(left);

			printf("%s\n", tree -> content);

			if (right)
				printAuthorAVL(right);
		}
	}

	static void debugPrintTree( AuthorAVL t ) {
		printAuthorAVL( t -> root);
	}


	void printAuthorIndex() {
		int i;

		for (i = 0; i < 27; i++) {
			printf("\n### >>> %c <<< ###\n", (int)'A' + i);
			debugPrintTree( letterIndex[i] );
		}
	}
#endif
