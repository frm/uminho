#ifndef AUTHOR_INDEX_TREE_H_
#define AUTHOR_INDEX_TREE_H_

int insertAuthor(char* author);
void init_author_tree();
void printAuthorIndex();
int getListOfAuthorsBy(char initial, char** author_list, int number_displays);

#endif

