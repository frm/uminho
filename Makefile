CC = clang
CFLAGS ?= -Wall -Wextra -ansi -pedantic
CPPFLAGS ?= -Imodules -Ilib -Iincludes

EXEC = gestauts

src_%.o: src/%.c src/%.h
	$(COMPILE.c) $(OUTPUT_OPTION) $<

.PHONY: clean

$(EXEC): src/*.c lib/*.c 
	$(LINK.c) $(OUTPUT_OPTION) $^ $(LOADLIBES) $(LDLIBS)

clean:
	$(RM) *.o
	$(RM) $(EXEC)

