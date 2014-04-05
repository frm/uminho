CC = gcc
LINK.o = $(CC) $(LDFLAGS) $(TARGET_ARCH)
CFLAGS ?= -Wall -Wextra -ansi -pedantic

LIB_DIR = lib/sources
MOD_DIR = modules/sources

EXECS = gestauts.out

$(LIB_DIR)/%: $(LIB_DIR)/%.o
	$@(LINK.o) $^ $(LOADLIBES) $(LDLIBS) -o $@

$(MOD_DIR)/%: $(MOD_DIR)/%.o
	$@(LINK.o) $^ $(LOADLIBES) $(LDLIBS) -o $@

$(LIB_DIR)/%.o: $(LIB_DIR)/%.c
	@$(COMPILE.c) $(OUTPUT_OPTION) $<

$(MOD_DIR)/%.o: $(MOD_DIR)/%.c
	@$(COMPILE.c) $(OUTPUT_OPTION) $<

COMPILE = $(CC) $(CFLAGS) $(LIB_DIR)/*.c $(MOD_DIR)/*.c -o $(EXECS)

.PHONY = debug clean default

default: CFLAGS+=-O2
default:
		$(COMPILE)	

test: CC=clang
test: CFLAGS+=-DDEBUG -g
test:
		$(COMPILE)

debug: CC=clang
debug: CFLAGS+= -g
debug:
		$(COMPILE)
clean:
	@$(RM) $(LIB_DIR)/*.o
	@$(RM) $(MOD_DIR)/*.o
	@$(RM) $(EXECS)
