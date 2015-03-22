#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct song{
	char *title;
	char *from;
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

SongP newSong(char *info[7]){
	int stringSize = 0;
	SongP newSong = (SongP) malloc (sizeof(Song));

	if(info[0] != NULL) newSong->title = strdup(info[0]);
	if(info[1] != NULL) newSong->from = strdup(info[1]);
	if(info[2] != NULL) newSong->author = strdup(info[2]);
	if(info[3] != NULL) newSong->lyrics = strdup(info[3]);
	if(info[4] != NULL) newSong->music = strdup(info[4]);
	if(info[5] != NULL) newSong->singer = strdup(info[5]);
	if(info[6] != NULL) newSong->lyricsText = strdup(info[6]);
	
	newSong->next = NULL;
	return newSong;
}

int addSong(SongListP sl, char *info[7]){

	SongP ns = newSong(info);
	
	ns->next = sl->cursor;
	sl->cursor = ns;
	return 1;
}