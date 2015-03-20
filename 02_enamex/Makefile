CFLAGS ?= -Wall -Wextra -pedantic -Wno-unused-function
LDFLAGS += -lfl
EXEC = parser.out
LEX = flex

VALGRIND = valgrind --tool=memcheck --leak-check=yes

LEX_FILES = $(shell ls *.l)
C_FILES = $(shell find . -name *.c) $(LEX_FILES:%.l=%.c)
O_FILES = $(C_FILES:%.c=%.o)

.PHONY: clean debug leak-check
.SECONDARY:
%.yy.c: %.l
	@echo "\n### LEX GENERATING $@"
	$(LEX) -o $@ $<

%.o: %.yy.c
	@echo "\n### COMPILING $@"
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(EXEC): $(O_FILES)
	@echo "\n### LINKING $@"
	$(LINK.c) $(OUTPUT_OPTION) $^ $(LOADLIBES) $(LDLIBS)

debug: CFLAGS += -g -DDEBUG
debug: $(EXEC)

leak-check: CFLAGS += -g
leak-check: $(EXEC)
	 $(VALGRIND)./$(EXEC)

osx: LDFLAGS:=$(filter-out -lfl,$(LDFLAGS))
osx: LDFLAGS+= -ll
osx: $(EXEC)

clean:
	@echo "### CLEANING UP\n"
	@cat ../.make/asciiart/maid.art
	@echo ""
	$(RM) *.o
	$(RM) *.yy.c
	$(RM) $(EXEC)
	$(RM) *.log

