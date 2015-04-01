%x lineComment multiComment codeLine docComment
commentEnd		([^*]|[\n]|(\*[^/]))*\*\/

%{
#include "lComment.h"	
#include "mComment.h"

lCommentP previousLC;
mCommentP previousMC;
int lc = 0, mc = 0, dc = 0, lineNr = 1;

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
\/\*\*									{ BEGIN docComment; }
\n 										{ lineNr++; }

<multiComment>{commentEnd}				{ processMC(); }

<docComment>{commentEnd}				{ dc++; /*usar a linecounter para continuar a contar linhas*/ BEGIN codeLine; }
<codeLine>.*							{ /*Recolher a linha extra*/ BEGIN 0; }
<codeLine>\n 							{ lineNr++; }

%%
int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    previousLC = newLComment();
    previousMC = newMComment();

    yylex();

    FILE *fp = fopen("comments.html","w+");
    fprintf(fp,"<!DOCTYPE html>\n<html>\n<title>\nComments\n</title>\n<body>\n<h1>\nFicheiro: %s\n</h1>\n<p>Single line comments: %d</p>\n<p>Multi-line comments: %d</p>\n<p>Documentation comments: %d</p>\n",argv[1],lc,mc,dc);

    printLC(fp,previousLC);
    printMC(fp,previousMC);

    fprintf(fp, "</body>\n</html>");
    return 0;
}
