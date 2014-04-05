#ifndef AUTHOR_INDEX_TREE_H_
#define AUTHOR_INDEX_TREE_H_

int insertAuthor(char* author);

int getListOfAuthorsBy(char initial, char** author_list, int number_displays);
void rewindAuthor(char initial);

void initAuthorTree();
void deleteAuthorIndexTree();

#ifdef DEBUG
    void printAuthorIndex();
#endif

#endif

