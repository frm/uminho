%{

#include "strutil.h"
#include "hash.h"

hash h;
int html = 0;
FILE* f;

void extract_info(char* str) {
    // get what's inside the first set of ' '
    char* type = strtrim( strcut(str, '"', '"') );
    // jump over to subtype and get what's inside the ' '
    char* subtype = strtrim( strcut(strstr(str, "SUBTYPE"), '"', '"') );
    // get what's between > and <
    char* value = strtrim( strcut(str, '>', '<') );
    if(subtype) {
        char* args[3] = {type, subtype, value};
        add_to_hash(&h, args, 3);
        free(subtype);
    }
    else {
        char* args[2] = {type, value};
        add_to_hash(&h, args, 2);
        free(type);
        free(value);
    }
    printf("\n## TYPE: %s\nSUBTYPE: %s\nVALUE: %s\n\n", type, subtype, value);
}


void print_struct() {
    if (html) {
        char* html = hash_to_html(h);
        fprintf(f, "%s", html);
        free(html);
    }
}

void print_html() {
    if(html)
        fprintf(f, "%s", yytext);
}

%}

ENAMEX_START    <[\t ]*ENAMEX
TYPE            [\t ]*TYPE=\"[^\"]+\"
SUBTYPE         [\t ]*(SUBTYPE=\"[^\"]+\")?
CLOSE           [\t ]*>
VALUE           [^<]+
ENAMEX_END      <\/ENAMEX>


YIELD           \{\{[\t ]*yield[\t ]*\}\}

%%

(?i:{ENAMEX_START}{TYPE}{SUBTYPE}{CLOSE}{VALUE}{ENAMEX_END})       extract_info(yytext);

(?i:{YIELD})     print_struct();
.                print_html();

%%

int main(int argc, char** argv) {
    h = new_hash(10000);
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    printf("Hello world!\n");
    yylex();

    html = 1;
    yyin = fopen("templates/template.html", "r");
    f = fopen("html/index.html", "w+");
    yylex();
    fclose(f);
    return 0;
}

