#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

struct Activation
{
  Instruction *ip;
  Value *register_window;
  Value *dest;
  uint32_t rW;
  uint32_t pc;
  struct VMFunction *fun;
};

#endif
