typedef struct mComment{
	int startLine;
	int endLine;
	int language;
	char *cText;
	struct mComment *next;
} *mCommentP, mComment;

mCommentP initMComment();

mCommentP newMComment();

