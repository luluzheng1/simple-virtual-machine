// Defines functions used to load virtual object code.

// Although you will add code to file loader.c, where these
// functions are defined, you will not normally need to define,
// modify, or call any of the functions directly.  These functions
// are called from `main` in file svm.c, which launches the
// entire SVM.

// Note: If you want, you can define an SVM instruction that loads
// virtual object code dynamically.  In production systems, this
// capability is very useful!  Such an instruction would likely
// call `loadmodule`.

#ifndef LOADER_INCLUDED
#define LOADER_INCLUDED

#include <stdbool.h>
#include <stdio.h>

#include "vmstate.h"

typedef struct modules
{
  struct VMFunction *module;
  struct modules *next;
} * Modules;

Modules loadmodules(VMStateP vm, FILE *input);
// Load all the modules from the given input file.
// If there is no more input or all lines are blank, return NULL.
// Otherwise read and all the modules.
// Any bad or unexpected input causes a checked run-time error.
//
// Any literals in the modules are added to the given VM's literal pool.
//
// The spine of the result is allocated with malloc and must be freed;
// the individual modules are allocated with vmalloc and must be GC'ed.

void freemodules(Modules *msp);
// free the spine of the list *msp and set *msp = NULL

#endif
