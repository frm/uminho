CC = clang
CFLAGS ?= -Wall -Wextra -ansi -pedantic
CPPFLAGS ?= -Imodules -Ilib -Iincludes

EXEC = gestauts

src_%.o: src/%.c src/%.h
	@echo "		C $@"
	$(COMPILE.c) $(OUTPUT_OPTION) $<

.PHONY: clean

$(EXEC): src/*.c includes/*.c 
	$(LINK.c) $(OUTPUT_OPTION) $^ $(LOADLIBES) $(LDLIBS)

debug: CFLAGS += -g

test: CFLAGS += -g -DDEBUG

final: CC = gcc
final: CFLAGS += -O2

clean:
	@echo "CLEANING UP"
	@cat .make/asciiart/maid.art
	$(RM) *.o
	$(RM) $(EXEC)

