#ifndef STRUTIL_H_
#define STRUTIL_H_

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

char* strcut(char* str, char fst, char snd);
char* strtrim(char *str);
char* str_to_lower(char* str);

#endif
