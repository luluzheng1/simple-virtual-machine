// Memory management and literal addition for VMState

// You'll complete this file as part of module 1

#define _POSIX_C_SOURCE 200809L

#include <cassert>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "vtable.h"
#include "value.h"
#include "vmheap.h"

void freestatep(VMStateP *sp)
{
    assert(sp and (*sp != NULL));
    VMStateP vm = *sp;
    free(vm);
    *sp = NULL;
}

VMStateP newstate(void)
{
    VMStateP vm = (VMStateP)malloc(sizeof *vm);
    vm->fun = NULL;
    vm->pc = 0;

    assert(vm != NULL);

    for (int i = 0; i < NUM_REGISTERS; i++)
    {
        vm->registers[i] = nilValue;
    }
    vm->globals = VTable_new(HINT_NUM_GLOBALS);
    for (int i = 0; i < NUM_LITERALS; i++)
    {
        vm->literals[i] = nilValue;
    }
    vm->num_literals = 0;
    vm->stack_length = 0;
    return vm;
}

int literal_slot(VMStateP state, Value literal)
{
    assert(state->num_literals < NUM_LITERALS);
    state->literals[state->num_literals++] = literal;
    return state->num_literals - 1;
}

Value literal_value(VMStateP state, unsigned index)
{
    assert(index < NUM_LITERALS);
    return state->literals[index];
}

int literal_count(VMStateP state)
{
    return state->num_literals;
}
