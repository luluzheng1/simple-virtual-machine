// Main program: Loads and runs SVM modules

// Defines a classic `main` function with a classic structure:
//
//   1. Initialize modules that need initialization.
//   2. Process either `stdin` or every file named on the command line.
//   3. Finalize modules that need finalization.
//
// If I've done my job, you don't need to edit this.  Or even look at it.

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "check-expect.h"
#include "loader.h"
#include "itable.h"
#include "print.h"
#include "svmdebug.h"
#include "vmheap.h"
#include "vmrun.h"
#include "vmstate.h"
#include "vmstring.h"

static void runmodules(struct VMState *vm, Modules ms)
{
  for (; ms; ms = ms->next)
  {
    vmrun(vm, ms->module);
  }
}

int main(int argc, char **argv)
{
  itable_init();
  VMString_init();
  installprinters();
  heap_init();
  VMStateP vm = newstate();

  if (argc == 1)
  {
    Modules ms = loadmodules(vm, stdin);
    runmodules(vm, ms);
    freemodules(&ms);
    report_unit_tests();
  }
  else
  {
    for (int i = 1; i < argc; i++)
    {
      FILE *exe = strcmp(argv[i], "-") == 0 ? stdin : fopen(argv[i], "r");
      assert(exe);
      Modules ms = loadmodules(vm, exe);
      runmodules(vm, ms);
      report_unit_tests();
      freemodules(&ms);
      if (exe != stdin)
        fclose(exe);
    }
  }
  freestatep(&vm);
  heap_shutdown();
  name_cleanup();
  VMString_finish();
  svmdebug_finalize();
  return EXIT_SUCCESS;
}
