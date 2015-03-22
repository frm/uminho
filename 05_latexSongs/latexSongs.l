%{
	#include <stdio.h>
	#include <string.h>
    #include "linkedList.h"
    SongListP mySongs;
    int metaSize = 0;
    char *aux;
    /*title,from,author,lyrics,music,singer,lyricsText*/
    char *info[7];
%}

%%
[tT][iI][tT][lL][eE]:" ".*          aux = yytext + 7; info[0] = strdup(aux);
[fF][rR][oO][mM]:" ".*              aux = yytext + 6; info[1] = strdup(aux);
[aA][uU][tT][hH][oO][rR]:" ".*      aux = yytext + 8; info[2] = strdup(aux);
[lL][yY][rR][iI][cC][sS]:" ".*      aux = yytext + 8; info[3] = strdup(aux);
[mM][uU][sS][iI][cC]:" ".*          aux = yytext + 7; info[4] = strdup(aux);
[sS][iI][nN][gG][eE][rR]:" ".*      aux = yytext + 8; info[5] = strdup(aux);
\n\n(.|\n)*#{2}\-+                  aux = yytext + 2; info[6] = strdup(aux);
[^"##"]\n\n(.|\n)*                  aux = yytext + 2; info[6] = strdup(aux);

%%

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");


    char *s = "\\usepackage";
    mySongs = init();

    yylex();

    /*int i;
    for(i = 0; i<7;i++){
        printf("String: %s\n",info[i]);
    }*/

    addSong(mySongs,info);
    SongP p = mySongs->cursor;

    while(p != NULL){
        //creates document name, taking the song name into account
        char buffer[100];
        snprintf(buffer, 100, "%s.tex", p->title);
        FILE *fp = fopen(buffer,"w");

        fprintf(fp, "\\documentclass{article}\n%s[portugues]{babel}\n%s[T1]{fontenc}\n%s[latin1]{inputenc}\n%s{t1enc}\n%s{aeguill}\n",s,s,s,s,s);
        
        //checks what type of author should be written in the file
        if(p->author != NULL)
            fprintf(fp, "\\author{%s}\n",p->author);
        else if(p->lyrics != NULL && p->music != NULL)
            //http://tex.stackexchange.com/questions/4805/whats-the-correct-use-of-author-when-multiple-authors
            fprintf(fp, "\\author{%s,%s}\n",p->lyrics,p->music);

        fprintf(fp, "\\title{%s}\n\\begin{document}\n\\maketitle\n\\section{Letra}\n%s\n",p->title, p->lyricsText);
        if(p->singer != NULL)
            fprintf(fp, "\\begin{flushleft}\n%s\n\\end{flushleft}\n",p->singer);
        fprintf(fp,"\\end{document}");
        fclose(fp);

        p = p->next;
    }

    return 0;
}