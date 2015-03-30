%x lineComment multiComment codeLine docComment
commentEnd		([^*]|[\n]|(\*[^/]))*\*\/

%{
	#include "mcList.h"
	mCommentListP myMC, baseMC;
	mCommentP currentMC;
	int lc = 0, mc = 0, dc = 0, lineNr = 1;
	char *aux;
%}
%%
	BEGIN 0;

[^\/\n]*\/\/.*							{ lc++; printf("\nInline string:%s on line %d",yytext,lineNr); }
\/\*									{ BEGIN multiComment; }
\/\*\*									{ BEGIN docComment; }
\n 										{ lineNr++; }

<multiComment>{commentEnd}				{ mc++; yytext[yyleng-2] = '\0'; currentMC->startLine = lineNr; currentMC->cText = strdup(yytext); lineNr += lineCounter(currentMC->cText); currentMC->endLine = lineNr; currentMC->endLine = lineNr; BEGIN codeLine; }

<codeLine>.*							{ currentMC->nextLine = strdup(yytext); addMComment(myMC,currentMC); currentMC = newMComment(); BEGIN 0; }
<codeLine>\n 							{ lineNr++; }

<docComment>{commentEnd}				{ dc++; BEGIN 0; }

%%
int lineCounter(char *s){
	int i,cnt;
	for(i = 0; s[i] != '\0';i++)
		if(s[i] == '\n')
			cnt++;
	return cnt;
}

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    myMC = init();
    baseMC = init();
    currentMC = newMComment();
    baseMC->cursor = currentMC;
    yylex();



    printf("\nSingle Line Comments: %d", lc);
    printf("\nMulti-Line Comments: %d", mc);
    printf("\nDocumentation Comments: %d\n", dc);
    FILE *fp = fopen("comments.html","w+");
    fprintf(fp,"<!DOCTYPE html>\n<html>\n<title>\nComments\n</title>\n<body>\n<h1>\nFicheiro: %s\n</h1>\n<p>Single line comments: %d</p>\n<p>Multi-line comments: %d</p>\n<p>Documentation comments: %d</p>\n",argv[1],lc,mc,dc);
    
    currentMC = baseMC->cursor;
    while(currentMC != NULL){
    	fprintf(fp,"<p>Line: %d~%d</p>\n<p>%s</p>\n<p>%s</p>\n",currentMC->startLine,currentMC->endLine,currentMC->cText,currentMC->nextLine);
    	currentMC = currentMC->next;
    }
    fprintf(fp, "</body>\n</html>");
    return 0;
}

/*TODO
imprimir linhas single line
whatLanguage nos multi line
*/

/*
\/\*[^*]+([^*]|[\n]|(\*[^/]))*\*\/
\/\*\*([^*]|[\n]|(\*[^/]))*\*\/
.*\/\/.*
*/