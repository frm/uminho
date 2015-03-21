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

SongListP init();

int addSong(SongListP sl, char *info[6]);