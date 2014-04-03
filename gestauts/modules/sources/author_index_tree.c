


typedef char* Author;

AVL_DEF(Author, Author)

void colidingAuthors(Author* author1, Author* author2) {
	return;
}

void deleteAuthor(Author a) {
		
}

Author cloneAuthor(Author a) {

}

int compareAuthor(Author* a, Author* b, Author c) {
	int cmp;
	Author key = a ? (*a) : (*b);

	return strcmp(key, b);
}
