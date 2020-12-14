TOP := ../..

BUILD = $(TOP)/build
BIN = $(TOP)/bin

UNITS = testfuns iformat vmheap vmrun vmstate vmrun vmstring value \
        printbuf print check-expect vtable vmerror
MAINS = svm+ 


# Module 2 adds the ability to load virtual object code, plus the svm binary

UNITS += svm iformat loader iparsers tokens name itable instructions svmdebug disasm




UNITS += gcdebug vstack


OBJ := $(UNITS:%=$(BUILD)/%.o)
MOBJ := $(MAINS:%=$(BUILD)/%.o)

EXE := $(MAINS:%=$(BIN)/%)

DEPS := $(OBJ:.o=.d)

CPPFLAGS += -MMD -MP

CFLAGS = -std=c++17 -Wall -Wextra -W -g 

.PHONY: all obj clean
all: $(EXE)
obj: $(OBJ) $(MOBJ)

clean:
	rm -f $(OBJ) $(MOBJ) $(DEPS) $(EXE)


.PHONY: show
show: 
	echo $(OBJ) $(MOBJ)

.PHONY: svm-test


$(BIN)/svm+: $(OBJ) $(BUILD)/svm.o
	g++ -v $(CFLAGS) -o $@ $^ -lm



$(BUILD)/%.o: %.cpp
	g++ $(CFLAGS) -MMD -MF $(BUILD)/$*.d -o $@ -c -Wall -Werror $<


-include $(DEPS)
