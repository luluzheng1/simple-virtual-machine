TOP := ../..

BUILD=$(TOP)/build
BIN=$(TOP)/bin

BASES=pp all main wpp impossible
MLBS=$(BASES:%=%.mlb)
CMS=$(BASES:%=%.cm)
SRCS=$(shell echo *.sml)

MAINS=uft.opt uft_sml

.PHONY: mosml

mosml: $(BIN)/uft_sml

.PHONY: clean

clean:
	rm -f *.ui *.uo
	rm -f $(BIN)/uft_sml $(BIN)/uft.opt

.PHONY: all

all: $(MAINS:%=$(BIN)/%)

.PHONY: mlton

mlton: $(BIN)/uft.opt

$(BIN)/uft.opt: $(MLBS) $(SRCS)
	mlton -output $@ -verbose 1 main.mlb

%.mlb: %.cm
	cm2mlb $^ > $@ || rm -f $@

$(BIN)/uft_sml: $(SRCS) $(CMS)
	$(TOP)/bin/mosml-compile-cm -o $@ main.cm
