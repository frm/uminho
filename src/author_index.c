#include "author_index.h"

void initializeAuthorIndex() {
	initAuthorStats();
	initAuthorTree();
}

void deleteAuthorIndex() {
    deleteAuthorIndexStats();
    deleteAuthorIndexTree();
}

