%x lText

%{
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <unistd.h>
	#include <stdio.h>
	#include <string.h>
    #include "linkedList.h"

    SongListP mySongs;
    SongP currentSong, p;
    char *res, *ss = "\\usepackage";

%}
%%
    BEGIN 0;

(?i:title):" ".*        {currentSong->title = strdup(yytext + 7);}
(?i:author):" ".*       {currentSong->author = strdup(yytext + 8);}
(?i:lyrics):" ".*       {currentSong->lyrics =strdup(yytext + 8);}
(?i:music):" ".*        {currentSong->music = strdup(yytext + 7);}
(?i:singer):" ".*       {currentSong->singer = strdup(yytext + 8);}


\n\n                            BEGIN lText; printf("BUGA\n");
<lText>[a-zA-ZÇ-ÑÀ-Ù!" "?\n]+   { res = (char*)realloc(res, sizeof(char) * (strlen(res) + yyleng+1) ); strcat(res,yytext);}
<lText>#{2}-+                   { currentSong->lyricsText = strdup(res); addSong(mySongs,currentSong);
                                  currentSong = newSong(); BEGIN 0;
                                }

.                           ;

%%

int main(int argc, char** argv) {
    if (argc == 2)
        yyin = fopen(argv[1], "r");

    mySongs = init();
    currentSong = newSong();
    res = (char *) calloc(sizeof(char),1);

    yylex();

currentSong->lyricsText = strdup(res); addSong(mySongs,currentSong);
                                  currentSong = newSong();


    printf("ASDASD\n");
    struct stat t;
    if(stat("cancoes", &t) == -1)
        mkdir("cancoes", 0700);

    p = mySongs->cursor;

    while(p != NULL){
        /*creates document name, taking the song name into account*/
        char buffer[100];
        snprintf(buffer, 100, "./cancoes/%s.tex", p->title);
        FILE *fp = fopen(buffer,"w+");

        fprintf(fp, "\\documentclass{article}\n%s[portugues]{babel}\n%s[T1]{fontenc}\n%s[latin1]{inputenc}\n%s{t1enc}\n%s{aeguill}\n",ss,ss,ss,ss,ss);
        
        /*checks what type of author should be written in the file*/
        if(p->author != NULL)
            fprintf(fp, "\\author{%s}\n",p->author);
        else if(p->lyrics != NULL && p->music != NULL)
            fprintf(fp, "\\author{%s ; %s}\n",p->lyrics,p->music);

        /*title and lyrics text. I use verbatim so I don't have to worry with \n and whitespaces*/
        fprintf(fp, "\\title{%s}\n\\begin{document}\n\\maketitle\n\\section{Letra}\n\\begin{verbatim}\n%s\n",p->title, p->lyricsText);

        /*writes singer to file, centered to the right*/
        if(p->singer != NULL)
            fprintf(fp, "\\hspace{1cm}%s\n",p->singer);

        fprintf(fp,"\\end{verbatim}\n\\end{document}");
        fclose(fp);

        p = p->next;
    }

    return 0;
}