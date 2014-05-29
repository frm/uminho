CC = clang
CFLAGS ?= -Wall -Wextra -Wno-unused-parameter -Wno-unused-function -Wno-unused-result -pedantic
CPPFLAGS ?= -Imodules -Ilib -Iincludes

EXEC = server

src_%.o: src/%.c src/%.h
	@echo "     C $@"
	$(COMPILE.c) $(OUTPUT_OPTION) $<

.PHONY: clean debug final leak-check gcov

$(EXEC): src/*.c includes/*.c
	$(LINK.c) $(OUTPUT_OPTION) $^ $(LOADLIBES) $(LDLIBS)

debug: CFLAGS += -g -DDEBUG
debug: $(EXEC)

leak-check: CFLAGS += -g
leak-check: $(EXEC)
	valgrind --tool=memcheck --leak-check=yes ./$(EXEC)

gcov: CFLAGS += -fprofile-arcs -ftest-coverage
gcov: $(EXEC)

final: CFLAGS += -O2
final: CC = gcc
final: $(EXEC)

clean:
	@echo "CLEANING UP"
	@cat ./.make/asciiart/maid.art
	$(RM) *.o *.log *.out $(EXEC)

