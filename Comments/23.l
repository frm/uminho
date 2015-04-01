%x lineComment multiComment codeLine docComment
commentEnd		([^*]|[\n]|(\*[^/]))*\*\/

%{
#include "lComment.h"
#include "mComment.h"
#include "doc.h"
#include "hash.h"

hash h;
lCommentP previousLC;
mCommentP previousMC;
int lc = 0, mc = 0, dc = 0, lineNr = 1;


DocComment *currentDoc;
DocComment *docComments;

Tag *authors = NULL;
Tag *versions = NULL;

char *currAuthor = NULL;
char *currVersion = NULL;


void fill_dictionary(char** files, int size) {
    for(int i = 0; i < size; i++) {
        size_t top_size = 5;
        size_t word_size = 100;
        FILE* f = fopen(files[i], "r");
        char* args[2];
        args[0] = (char*)calloc(5, sizeof(char));
        getline(args, &top_size, f);
        args[0] = strtrim( args[0] );
        args[1] = (char*)calloc(100, sizeof(char));

        while( getline(args + 1, &word_size, f) > 0 ) {
            args[1] = strtrim(args[1]);
            add_to_hash(&h, args, 2);
        }
    }
}

char* processWord(char* str) {
    char* tok = strtok(str, " .,\t\n");

    while(tok) {
        update_at_top(&h, strtrim(tok));
        tok = strtok(NULL, " .,");
    }

    char* lang;
    int cap = get_top(&h, &lang);

    if(cap < 5) {
        lang = realloc(lang, sizeof(char) * 2);
        sprintf(lang, "?");
    }

    printf("\nFOR COMMENT: %s\nLANG: %s, COUNT: %d\n\n", str, lang, cap);
    reset_level(&h, 0);
    return lang;
}

int lineCounter(char *s){
	int i,cnt = 0;
	for(i = 0; s[i] != '\0';i++)
		if(s[i] == '\n')
			cnt++;

    return cnt;
}

void printLC(FILE *fp, lCommentP node){
    if(node->next == NULL)
    	return;

    printLC(fp, node -> next);

    fprintf(fp,"<p>Line: %d</p>\n<p>%s</p>\n",node->line,node->cText);
}

void printMC(FILE *fp, mCommentP node){
    if(node->next == NULL)
    	return;

    printMC(fp, node -> next);

    fprintf(fp,"<p>Line: %d~%d</p>\n<p>%s</p>\n",node->startLine,node->endLine,node->cText);
}

void processLC(){
	lc++;

	lCommentP new = newLComment(yytext, lineNr);

	new -> next = previousLC;
    previousLC = new;
}

void processMC(){
	mc++;

	yytext[yyleng-2] = '\0';

    processWord(yytext);
    int counter = lineCounter(yytext);

    mCommentP new = newMComment(yytext, lineNr, counter + lineNr);

    lineNr += counter;

    new -> next = previousMC;
    previousMC = new;


    BEGIN 0;
}

%}
%%
	BEGIN 0;

[^\/\n]*\/\/.*							{ processLC(); }
\/\*									{ BEGIN multiComment; }
\/\*\*                                  { BEGIN docComment; }
\n 										{ lineNr++; }


<codeLine>.+\n                          { yytext[ strlen(yytext) - 1] = '\0';
                                        currentDoc->firstLine = strdup(yytext);
                                        BEGIN 0;
                                        lineNr++;
                                        currentDoc = NULL;
                                        }

<multiComment>{commentEnd}				{ processMC(); }

<docComment>{commentEnd}                { yytext[strlen(yytext)-2] = '\0';
                                        currentDoc = newDocComment( strdup(yytext));
                                        docComments = addDocComment(docComments, currentDoc);
                                        currAuthor = getTag(yytext, "author");
                                        if( currAuthor != NULL) authors = addTag( currAuthor, authors, currentDoc);
                                        currVersion = getTag(yytext, "version");
                                        if( currVersion != NULL) versions = addTag( currVersion , versions, currentDoc);
                                        lineNr += lineCounter(yytext);
                                        BEGIN codeLine;
                                        }

<codeLine>.+							{currentDoc->firstLine = strdup(yytext);
                                        BEGIN 0;
                                        lineNr++;
                                        currentDoc = NULL;}

%%
int main(int argc, char** argv) {
    if (argc > 1)
        yyin = fopen(argv[1], "r");

    h = new_hash(1500);
    fill_dictionary(argv + 2, argc - 2);

    previousLC = initLComment();
    previousMC = initMComment();

    yylex();

    FILE *fp = fopen("comments.html","w+");
    fprintf(fp, "<!DOCTYPE html>\n<meta charset=\"UTF-8\">\n");
    fprintf(fp, "<link rel=\"stylesheet\" type=\"text/css\" href=\"../comments.css\" media=\"screen\" />\n");

    writeDocComments(docComments);
    fprintf(fp, "<a href=\"doc_comments.html\">Documentação Completa</a>");

    fprintf(fp, "\nDocumentação por Autor:\n<ul>\n");
    writeAuthors(authors,fp);
    fprintf(fp, "</ul>\nDocumentação por Versão:\n<ul>\n");
    writeVersions(versions, fp);
    fprintf(fp, "</ul>");


    fprintf(fp,"<title>\nComments\n</title>\n<body>\n<h1>\nFicheiro: %s\n</h1>\n<p>Single line comments: %d</p>\n<p>Multi-line comments: %d</p>\n<p>Documentation comments: %d</p>\n",argv[1],lc,mc,dc);



    printLC(fp,previousLC);
    printMC(fp,previousMC);



    fprintf(fp, "</body>\n</html>");
    return 0;
}
