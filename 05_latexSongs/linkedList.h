typedef struct song{
	char *title;
	char *author;
	char *lyrics;
	char *music;
	char *singer;
	char *lyricsText;
	struct song *next;
} *SongP, Song;
	
SongP initSong();

SongP newSong(char* t, char* a, char* l, char* m, char* s, char* lt);