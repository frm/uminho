#include "author_index.h"

static AuthorTree letterIndex[27];


void initializeAuthorIndex() {
	int i;

    initAuthorStats();

    for (i = 0; i < 27; i++)
        letterIndex[i] = authorTreeNew();
}

int insertAuthor(char *author) {
    int index;

    index = GET_CHAR_INDEX(author[0]);

    return authorTreeInsert(letterIndex[index], author);
}

int getListOfAuthorsByInitial(char initial, char** author_list, int number_displays, int* number_read) {
    int i;
    int index;
    int reading;

    i = 0;
    index = GET_CHAR_INDEX(initial);

    do {
        reading = authorTreeYield( letterIndex[index], &(author_list[i]) );
        i++;
    }
    while ( i < number_displays && reading  == 0 );

    *number_read = i;

    return reading;
}

void rewindGeneratorByInitial(char initial) {
    int index;

    index = GET_CHAR_INDEX(initial);

    authorTreeRewindGenerator(letterIndex[index]);
}

void deleteAuthorIndex() {
    int i;

    deleteAuthorIndexStats();

    for (i = 0; i < 27; i++)
        authorTreeDestroy(letterIndex[i]);
}

