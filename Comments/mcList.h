typedef struct mComment{
	int startLine;
	int endLine;
	int language;
	char *cText;
	char *nextLine;
	struct mComment *next;
} *mCommentP, mComment;

typedef struct mCommentList{
	struct mComment *cursor;
} *mCommentListP, mCommentList;

mCommentListP init();

mCommentP newMComment();

void addMComment(mCommentListP mcl, mCommentP mc);