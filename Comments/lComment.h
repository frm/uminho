typedef struct lComment{
	int line;
	char *cText;
	struct lComment *next;
} *lCommentP, lComment;

lCommentP initLComment();

lCommentP newLComment();

