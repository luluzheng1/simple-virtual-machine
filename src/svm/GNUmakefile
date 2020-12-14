TOP := ../..

BUILD = $(TOP)/build
BIN = $(TOP)/bin

UNITS = testfuns vmheap vmrun vmstate vmrun vmstring value iformat \
        printbuf print check-expect testfuns vtable vmerror
MAINS = svm-test


# Module 2 adds the ability to load virtual object code, plus the svm binary

UNITS += loader iparsers tokens name itable instructions svmdebug disasm
MAINS += svm svm-loader-instructions


MAINS += svm-dis


UNITS += gcdebug vstack


OBJ := $(UNITS:%=$(BUILD)/%.o)
MOBJ := $(MAINS:%=$(BUILD)/%.o)

EXE := $(MAINS:%=$(BIN)/%)

DEPS := $(OBJ:.o=.d)

CPPFLAGS += -MMD -MP

CFLAGS = -std=c11 -pedantic -Wall -Wextra -W -g -O2 -I/usr/include

.PHONY: all obj clean
all: $(EXE)
obj: $(OBJ) $(MOBJ)

clean:
	rm -f $(OBJ) $(MOBJ) $(DEPS) $(EXE)


.PHONY: show
show: 
	echo $(OBJ) $(MOBJ)

.PHONY: svm-test

svm-test: $(BIN)/svm-test

$(BIN)/svm-test: $(OBJ) $(BUILD)/svm-test.o
	gcc $(CFLAGS) -o $@ $^ -lm

$(BIN)/svm: $(OBJ) $(BUILD)/svm.o
	gcc $(CFLAGS) -o $@ $^ -lm

$(BIN)/svm-loader-instructions: $(OBJ) $(BUILD)/svm-loader-instructions.o
	gcc $(CFLAGS) -o $@ $^ -lm

$(BIN)/svm-dis: $(OBJ) $(BUILD)/svm-dis.o
	gcc $(CFLAGS) -o $@ $^ -lm




$(BUILD)/%.o: %.c
	gcc $(CFLAGS) -MMD -MF $(BUILD)/$*.d -lprofiler -o $@ -c -Wall -std=c11 $<


-include $(DEPS)
