CC = gcc 
CFLAGS ?= -Wall -Wextra -Wno-unused-function -O2 -Wno-unused-result -ansi -pedantic
CPPFLAGS ?= -Imodules -Ilib -Iincludes

EXEC = gestauts

src_%.o: src/%.c src/%.h
	@echo "		C $@"
	$(COMPILE.c) $(OUTPUT_OPTION) $<

.PHONY: clean debug dsmall final leak-check gcov

$(EXEC): src/*.c includes/*.c 
	$(LINK.c) $(OUTPUT_OPTION) $^ $(LOADLIBES) $(LDLIBS)

debug: CFLAGS += -g -DDEBUG
debug: $(EXEC)

dsmall: CFLAGS += -g -DDEBUG2
dsmall: $(EXEC)

leak-check: CFLAGS += -g
leak-check: $(EXEC)
	valgrind --tool=memcheck --leak-check=yes ./$(EXEC)

gcov: CFLAGS+= -fprofile-arcs -ftest-coverage
gcov: $(EXEC)

clean:
	@echo "CLEANING UP"
	@cat ./.make/asciiart/maid.art
	$(RM) *.o
	$(RM) $(EXEC)
	$(RM) *.csv
	$(RM) *.q10
	$(RM) *.log

