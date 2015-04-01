%x lineComment multiComment codeLine docComment
commentEnd		([^*]|[\n]|(\*[^/]))*\*\/

%{
#include "mcList.h"
mCommentP currentMC, previousMC;
int lc = 0, mc = 0, dc = 0, lineNr = 1;

int lineCounter(char *s){
	int i,cnt;
	for(i = 0; s[i] != '\0';i++)
		if(s[i] == '\n')
			cnt++;
	return cnt;
}

void printMC(FILE *fp, mCommentP p){
	mCommentP aux = p;
    if(aux == NULL)
    	return;

    aux = aux->next;
    printMC(fp, aux);

    fprintf(fp,"<p>Line: %d~%d</p>\n<p>%s</p>\n",currentMC->startLine,currentMC->endLine,currentMC->cText);
}

void processMC(){
	mc++;

	yytext[yyleng-2] = '\0';
	currentMC->startLine = lineNr;
	currentMC->cText = strdup(yytext);
	lineNr += lineCounter(currentMC->cText);
	currentMC->endLine = lineNr;

	currentMC->next = previousMC;
	previousMC = currentMC;

	BEGIN 0;
}

%}
%%
	BEGIN 0;

[^\/\n]*\/\/.*							{ lc++; printf("\nInline string:%s on line %d",yytext,lineNr); }
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

    previousMC = newMComment();
    currentMC = newMComment();

    yylex();

    FILE *fp = fopen("comments.html","w+");
    fprintf(fp,"<!DOCTYPE html>\n<html>\n<title>\nComments\n</title>\n<body>\n<h1>\nFicheiro: %s\n</h1>\n<p>Single line comments: %d</p>\n<p>Multi-line comments: %d</p>\n<p>Documentation comments: %d</p>\n",argv[1],lc,mc,dc);
    
    /*Imprime comentários multi-linha no ficheiro html*/
    printf("%s",currentMC->cText);
	printf("%s",currentMC->next->cText);
    //printMC(fp, currentMC);

    fprintf(fp, "</body>\n</html>");
    return 0;
}