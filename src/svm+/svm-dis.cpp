#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "disasm.h"
#include "loader.h"
#include "itable.h"
#include "print.h"
#include "svmdebug.h"
#include "vmheap.h"
#include "vmstate.h"
#include "vmstring.h"

static const char *showpc;

static void dismodules(VMState vm, Modules ms) {
  for ( ; ms; ms = ms->next) {
    for (int i = 0; i < ms->module->size; i++) {
      if (showpc) printf("%3d: ", i);
      printasm(stdout, vm, ms->module->instructions[i]);
      fputs("\n", stdout);
    }
  }
}

int main(int argc, char **argv) {
    itable_init();
    heap_init();
    Vmstring_init();
    installprinters();
    VMState vm = newstate();
    showpc = svmdebug_value("pc");

    if (argc == 1) {
      Modules ms = loadmodules(vm, stdin);
      dismodules(vm, ms);
      freemodules(&ms);
    } else {
      for (int i = 1; i < argc; i++) {
        FILE *exe = strcmp(argv[i], "-") == 0 ? stdin : fopen(argv[i], "r");
        assert(exe);
        Modules ms = loadmodules(vm, exe);
        dismodules(vm, ms);
        freemodules(&ms);
        if (exe != stdin)
          fclose(exe);
      }
    }

    for (int i = 0; i < literal_count(vm); i++) {
      Value v = literal_value(vm, i);
      if (v.tag == VMFunction) {
        printf("------------------------------------\n");
        printf(";    function at literal %d\n", i);
        struct VMFunction *f = v.f;
        for (int j = 0; j < f->size; j++) {
          if (showpc) printf("%3d: ", j);
          printasm(stdout, vm, f->instructions[j]);
          fputs("\n", stdout);
        }
      }
    }
        
    freestatep(&vm);
    heap_shutdown();
    name_cleanup();
    Vmstring_finish();
    return EXIT_SUCCESS;
}
