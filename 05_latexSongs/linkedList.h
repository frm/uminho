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

SongListP init();
	
SongP newSong();

int addSong(SongListP sl, SongP ns);