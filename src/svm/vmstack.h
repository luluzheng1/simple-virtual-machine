#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

typedef struct Activation
{
  Value *register_window;
  Value *dest;
  uint32_t rW;
  uint32_t pc;
  struct VMFunction *fun;
} Activation;

#endif
