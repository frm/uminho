#include "strutil.h"

char* strcut(char* str, char fst, char snd) {
    if(! str)
        return NULL;

    int start = -1, end = 0, i = 0;
    int size = strlen(str);

    for(i = 0; i < size && str[i] != fst; i++);

    start = i + 1;

    for(++i; i < size && str[i] != snd; i++);

    end = i;

    if(start >= size || end >= size)
        return NULL;

    int mv = end - start + 1;
    char* s = (char*)malloc( sizeof(char) * mv );
    memmove(s, str + start, mv);
    s[mv - 1] = '\0';

    return s;
}

char* str_to_lower(char* str) {
    char* lower = (char*)calloc(strlen(str) + 1, sizeof(char));

    for(int i = 0; str[i]; i++)
        lower[i] = tolower(str[i]);

    return lower;
}

static int new_str_end (char *str) {
    int i = strlen(str) - 1;

    while ( i > 0 && isspace(str[i]) )
        i--;

    return i + 1;
}

static char* new_str_start (char *str) {
    while ( *str != '\0' && isspace(*str) )
        str++;

    return str;
}

char* strtrim(char *str) {
    int size;
    char *buffer, *new_str;

    if (!str || !*str)
        return NULL;

    new_str = new_str_start(str);
    size = new_str_end(new_str);

    buffer = (char*)malloc(size + 1);
    memcpy(buffer, new_str, size);
    buffer[size] = '\0';

    return buffer;
}


