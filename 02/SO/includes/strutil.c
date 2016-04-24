#include "strutil.h"

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

char* str_slice(char *str, int i) {
    return str_dup(str + i);
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

char* str_add(char* str1, char* str2) {
    char* new_str = (char*)malloc( sizeof(char) * ( strlen(str1) + strlen(str2) + 1 ) );
    memcpy( new_str, str1, strlen(str1) + 1 );
    strcat(new_str, str2);
    return new_str;
}

char* str_dup(char* str) {
    int size = strlen(str) + 1; /* avoiding multiple function calls */
    char* dup = (char*)malloc(size);
    memcpy(dup, str, size);
    return dup;
}

