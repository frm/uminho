%{

#include "hash.h"
hash h;

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

void extract_info(char* str) {
    // get what's inside the first set of ' '
    char* type = strcut(str, '"', '"');
    // jump over to subtype and get what's inside the ' '
    char* subtype = strcut(strstr(str, "SUBTYPE"), '"', '"');
    // get what's between > and <
    char* value = strcut(str, '>', '<');
    if(subtype) {
        char* args[3] = {type, subtype, value};
        add_to_hash(&h, args, 3);
    }
    else {
        char* args[2] = {type, value};
        add_to_hash(&h, args, 2);
    }
    printf("\n## TYPE: %s\nSUBTYPE: %s\nVALUE: %s\n\n", type, subtype, value);
}

/*
char* get_type_value(char* str) {
    char* start = strchr( strstr(str, "TYPE"), '"') + 1;
    char* end = strrchr(start, '"');
    unsigned long last_index = strlen(start) - (end - start);
    unsigned long i = strlen(start) - last_index;
    start[i] = '\0';
    return start;
}
*/


%}

ENAMEX_START    <[\t ]*ENAMEX
TYPE            [\t ]*TYPE=\"[^\"]+\"
SUBTYPE         [\t ]*(SUBTYPE=\"[^\"]+\")?
CLOSE           [\t ]*>
VALUE           [^<]+
ENAMEX_END      <\/ENAMEX>

%%

(?i:{ENAMEX_START}{TYPE}{SUBTYPE}{CLOSE}{VALUE}{ENAMEX_END})       extract_info(yytext);


%%

int main(int argc, char** argv) {
    h = new_hash(10000);
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    printf("Hello world!\n");
    yylex();
    return 0;
}

