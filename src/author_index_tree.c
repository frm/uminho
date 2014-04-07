#include "author_index_tree.h"
#include <string.h>

#define normalLetter(c)		( ( (c) >= 'A' ) && ( (c) <= 'Z' ) )

AVL_DEF(Author, Author)

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

	return strcmp(key, author2);
}

int authorTreeInsert(AuthorTree tree, char *author) {
	return avlInsert(Author, tree, author);
}

int authorTreeYield(AuthorTree tree, Author *ret) {
	return avlYield(Author, tree, ret);
}

void authorTreeRewindGenerator(AuthorTree tree) {
	avlRewindGenerator(Author, tree);
}

AuthorTree authorTreeNew() {
	return avlNewComplete(Author, &compareAuthor, NULL, &deleteAuthor, &cloneAuthor);
}

void authorTreeDestroy(AuthorTree tree) {
	avlDestroy(Author, tree);
}

/*
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
*/