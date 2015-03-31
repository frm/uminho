#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct song{
	char *title;
	char *author;
	char *lyrics;
	char *music;
	char *singer;
	char *lyricsText;
	struct song *next;
} *SongP, Song;

typedef struct songList {
    struct song *cursor;
} *SongListP, SongList;

SongListP init(){
	SongListP r = malloc (sizeof(SongList));
	r->cursor = NULL;
	return r;
}

SongP newSong(){
	SongP newSong = (SongP) malloc (sizeof(Song));
	newSong->next = NULL;
	return newSong;
}

void addSong(SongListP sl, SongP ns){
	ns->next = sl->cursor;
	sl->cursor = ns;
}