%{
	#include <stdio.h>
	#include <string.h>
    #include "linkedList.h"
    SongListP mySongs;
    char *aux;
    /*title,from,author,lyrics,music,singer,lyricsText*/
    char *info[7];
%}

%%
[tT][iI][tT][lL][eE]:" ".*          {info[0] = strdup(yytext + 7);}
[fF][rR][oO][mM]:" ".*              {info[1] = strdup(yytext + 6);}
[aA][uU][tT][hH][oO][rR]:" ".*      {info[2] = strdup(yytext + 8);}
[lL][yY][rR][iI][cC][sS]:" ".*      {info[3] = strdup(yytext + 8);}
[mM][uU][sS][iI][cC]:" ".*          {info[4] = strdup(yytext + 7);}
[sS][iI][nN][gG][eE][rR]:" ".*      {info[5] = strdup(yytext + 8);}

\n\n[^#]+(##-|[^#])               	{info[6] = strdup(yytext + 2);addSong(mySongs,info);
									//clean
									int i; for(i = 0; i < 7; i++) info[i] = NULL;
									}

%%

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    char *s = "\\usepackage";
    mySongs = init();

    yylex();

    SongP p = mySongs->cursor;

    while(p != NULL){
        //creates document name, taking the song name into account
        char buffer[100];
        snprintf(buffer, 100, "%s.tex", p->title);
        FILE *fp = fopen(buffer,"w+");

        fprintf(fp, "\\documentclass{article}\n%s[portugues]{babel}\n%s[T1]{fontenc}\n%s[latin1]{inputenc}\n%s{t1enc}\n%s{aeguill}\n",s,s,s,s,s);
        
        //checks what type of author should be written in the file
        if(p->author != NULL)
            fprintf(fp, "\\author{%s}\n",p->author);
        else if(p->lyrics != NULL && p->music != NULL)
            fprintf(fp, "\\author{%s ; %s}\n",p->lyrics,p->music);

        //title and lyrics text. I use verbatim so I don't have to worry with \n and whitespaces
        fprintf(fp, "\\title{%s}\n\\begin{document}\n\\maketitle\n\\section*{Letra}\n\\begin{verbatim}\n%s\n\\end{verbatim}\n",p->title, p->lyricsText);

        //writes singer to file, centered to the right
        if(p->singer != NULL)
            fprintf(fp, "\\begin{flushleft}\n%s\n\\end{flushleft}\n",p->singer);

        fprintf(fp,"\\end{document}");
        fclose(fp);

        p = p->next;
    }

    return 0;
}