%%

%%

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    printf("Hello world!\n");
    yylex();
    return 0;
}

