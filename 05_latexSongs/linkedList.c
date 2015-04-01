#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "linkedList.h"

SongP initSong(){
	SongP newSong = (SongP) malloc (sizeof(Song));
	newSong -> title = NULL;
	newSong -> author = NULL;
	newSong -> lyrics = NULL;
	newSong -> music = NULL;
	newSong -> singer = NULL;
	newSong -> lyricsText = NULL;
	newSong -> next = NULL;
	return newSong;
}

SongP newSong(char* t, char* a, char* l, char* m, char* s, char* lt){
	SongP newSong = (SongP) malloc (sizeof(Song));
	if(t != NULL)
		newSong -> title = strdup(t);
	if(a != NULL)
		newSong -> author = strdup(a);
	if(l != NULL)
		newSong -> lyrics = strdup(l);
	if(m != NULL)
		newSong -> music = strdup(m);
	if(s != NULL)
		newSong -> singer = strdup(s);
	if(lt != NULL)
		newSong -> lyricsText = strdup(lt);
	newSong -> next = NULL;
	return newSong;
}