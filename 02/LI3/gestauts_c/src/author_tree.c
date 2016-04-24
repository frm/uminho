#include "author_tree.h"
#include <string.h>
#include <avl.h>
#include <strutil.h>

AVL_DEF_HEADER(Author, Author)
AVL_DEF(Author, Author)


/* AVL delete Author */
static void deleteAuthor(Author author) {
	free(author);
}

/* AVL clone author */
static Author cloneAuthor(Author author) {
	return str_dup(author);
}

/* AVL comparison author */
static int compareAuthor(Author* key_author, Author* author1, Author author2) {
	Author key = key_author ? (*key_author) : (*author1);

	return strcmp(key, author2);
}

/* API for insertion in Author AVL */
int authorTreeInsert(AuthorTree tree, char *author) {
	return avlInsert(Author, tree, author);
}

/* API for author Tree Yield */
int authorTreeYield(AuthorTree tree, Author *ret) {
	return avlYield(Author, tree, ret);
}

void authorTreeRewindGenerator(AuthorTree tree) {
	avlRewindGenerator(Author, tree);
}

/* API for new Author AVL */
AuthorTree authorTreeNew() {
	return avlNewComplete(Author, &compareAuthor, NULL, &deleteAuthor, &cloneAuthor);
}

/* API for deleting an Author AVL */
void authorTreeDestroy(AuthorTree tree) {
	avlDestroy(Author, tree);
}

/* API for Author AVL exists */
int authorTreeExists(AuthorTree tree, Author key) {
	return avlExists(Author, tree, key);
}

/* API for cloning an Author AVL */
AuthorTree authorTreeClone(AuthorTree tree) {
	return avlClone(Author, tree);
}

/* Converting an author AVL to an array of strings */
AuthorArray authorTreeToString(AuthorTree tree, int* ret) {
    int list_size = 1024;
    Author* author_list = (Author*)malloc( sizeof(Author) * list_size );
    Author name;
    int avl_empty = 0, i = 0;

    while ( !avl_empty ) {
    	avl_empty = authorTreeYield(tree, &name);
    	if ( avl_empty != -1 ) {

            if (i == list_size) {
                list_size *= 2;
                author_list = (Author*)realloc( author_list, sizeof(Author) * list_size );
            }

            author_list[i++] = name;
    	}
    }
    *ret = i;
    return author_list;
}
