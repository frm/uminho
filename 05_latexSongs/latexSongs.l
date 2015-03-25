%{
	#include <stdio.h>
	#include <string.h>
    #include "linkedList.h"
    SongListP mySongs;
    SongP currentSong;
%}
%%

[tT][iI][tT][lL][eE]:" ".*          {currentSong->title = strdup(yytext + 7);}
[fF][rR][oO][mM]:" ".*              {currentSong->from = strdup(yytext + 6);}
[aA][uU][tT][hH][oO][rR]:" ".*      {currentSong->author = strdup(yytext + 8);}
[lL][yY][rR][iI][cC][sS]:" ".*      {currentSong->lyrics =strdup(yytext + 8);}
[mM][uU][sS][iI][cC]:" ".*          {currentSong->music = strdup(yytext + 7);}
[sS][iI][nN][gG][eE][rR]:" ".*      {currentSong->singer = strdup(yytext + 8);}

\n\n[^#]+(##-|[^#])               	{if(yytext[yyleng-1] == '-') yytext[yyleng-4] = '\0';
									currentSong->lyricsText = strdup(yytext + 2); addSong(mySongs,currentSong);
									currentSong = newSong();}

%%

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    /*just to save space*/
    char *s = "\\usepackage";

    mySongs = init();
    currentSong = newSong();

    yylex();

    SongP p = mySongs->cursor;

    while(p != NULL){
        /*creates document name, taking the song name into account*/
        char buffer[100];
        snprintf(buffer, 100, "%s.tex", p->title);
        FILE *fp = fopen(buffer,"w+");

        fprintf(fp, "\\documentclass{article}\n%s[portugues]{babel}\n%s[T1]{fontenc}\n%s[latin1]{inputenc}\n%s{t1enc}\n%s{aeguill}\n",s,s,s,s,s);
        
        /*checks what type of author should be written in the file*/
        if(p->author != NULL)
            fprintf(fp, "\\author{%s}\n",p->author);
        else if(p->lyrics != NULL && p->music != NULL)
            fprintf(fp, "\\author{%s ; %s}\n",p->lyrics,p->music);

        /*title and lyrics text. I use verbatim so I don't have to worry with \n and whitespaces*/
        fprintf(fp, "\\title{%s}\n\\begin{document}\n\\maketitle\n\\section*{Letra}\n\\begin{verbatim}\n%s\n\\end{verbatim}\n",p->title, p->lyricsText);

        /*writes singer to file, centered to the right*/
        if(p->singer != NULL)
            fprintf(fp, "\\begin{flushleft}\n%s\n\\end{flushleft}\n",p->singer);

        fprintf(fp,"\\end{document}");
        fclose(fp);

        p = p->next;
    }

    return 0;
}