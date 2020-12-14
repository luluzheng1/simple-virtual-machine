// Heart of the VM: runs instructions until told to halt

// You'll write a small `vmrun` function in module 1.  You'll pay
// some attention to performance, but you'll implement only a few
// instructions.  You'll add other instructions as needed in future modules.

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>

#include "check-expect.h"
#include "iformat.h"
#include "value.h"
#include "vmstate.h"
#include "vmrun.h"
#include "vmstack.h"
#include "svmdebug.h"
#include "disasm.h"
#include "print.h"
#include "vmsizes.h"

#include "vmerror.h"
#include "vmheap.h"
#include "vmstring.h"
#include "vtable.h"

void vmrun(VMStateP vm, struct VMFunction *fun)
{

    // Instruction *ip = fun->instructions;

#define VMSAVE() (void)0
#define VMLOAD() (void)0

#define GC()  \
    VMSAVE(); \
    gc(vm);   \
    VMLOAD();

    vm->fun = fun;
    vm->pc = 0;
    uint8_t X, Y, Z;
    uint16_t YZ;
    Value *reg0 = &vm->registers[0];
    uint32_t regWindow = 0;
    const char *dump_decode = svmdebug_value("decode");
    const char *dump_call = svmdebug_value("call");
    (void)dump_call; // make it OK not to use `dump_call`

    while (true)
    {
        Instruction i = vm->fun->instructions[vm->pc];
        vm->pc++;
        X = uX(i);
        Y = uY(i);
        Z = uZ(i);

        Opcode op = opcode(i);

        if (dump_decode)
            idump(stderr, vm, 0, i, regWindow, reg0 + X, reg0 + Y, reg0 + Z);

        switch (op)
        {
        case Halt:
            return;
        case GC:
        {
            gc(vm);
            break;
        }
        case Goto:
        {
            int32_t XYZ = iXYZ(i);
            if (XYZ < 0 && gc_needed)
            {
                GC();
            }
            vm->pc += XYZ - 1;
            break;
        }
        case Not:
        {
            X = uX(i);
            reg0[X] = mkBooleanValue(!AS_BOOLEAN(vm, reg0[X]));
            break;
        }
        case BitwiseNot:
        {
            X = uX(i);
            uint32_t num = AS_NUMBER(vm, reg0[X]);
            reg0[X] = mkNumberValue(~num);
            break;
        }
        case Print:
        {
            X = uX(i);
            print("%v\n", reg0[X]);
            break;
        }
        case Error:
        {
            X = uX(i);
            printf("\033[0;31m");
            print("%v\n", reg0[X]);
            printf("\033[0m");
            assert(0);
        }
        case ProjectBool:
        {
            X = uX(i);
            Y = uY(i);
            Value val = reg0[X];
            reg0[Y] = mkBooleanValue(AS_BOOLEAN(vm, val));
            break;
        }
        case Mov:
        {
            X = uX(i);
            Y = uY(i);
            reg0[X] = reg0[Y];
            break;
        }
        case If:
        {
            X = uX(i);
            if (!AS_BOOLEAN(vm, reg0[X]))
            {
                vm->pc++;
            }
            break;
        }
        case Zero:
        {
            X = uX(i);
            reg0[X] = mkNumberValue(0);
            break;
        }
        case Add:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);

            Value Yval = reg0[Y];
            Value Zval = reg0[Z];
            reg0[X] = mkNumberValue(AS_NUMBER(vm, Yval) + AS_NUMBER(vm, Zval));
            break;
        }
        case Subtract:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);

            Value Yval = reg0[Y];
            Value Zval = reg0[Z];

            reg0[X] = mkNumberValue(AS_NUMBER(vm, Yval) - AS_NUMBER(vm, Zval));
            break;
        }
        case Multiply:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);

            Value Yval = reg0[Y];
            Value Zval = reg0[Z];

            reg0[X] = mkNumberValue(AS_NUMBER(vm, Yval) * AS_NUMBER(vm, Zval));
            break;
        }
        case Equal2:
        case Equal:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);

            Value Yval = reg0[Y];
            Value Zval = reg0[Z];

            reg0[X] = mkBooleanValue(eqvalue(Yval, Zval));
            break;
        }
        case Check:
        {
            X = uX(i);
            YZ = uYZ(i);
            assert(YZ <= vm->num_literals);
            check(AS_CSTRING(vm, vm->literals[YZ]), reg0[X]);
            break;
        }
        case Expect:
        {
            X = uX(i);
            YZ = uYZ(i);
            assert(YZ < vm->num_literals);
            expect(AS_CSTRING(vm, vm->literals[YZ]), reg0[X]);
            break;
        }
        case Assert:
        {
            X = uX(i);
            YZ = uYZ(i);
            assert(YZ < vm->num_literals);
            check_assert(AS_CSTRING(vm, vm->literals[YZ]), reg0[X]);
            break;
        }
        case LoadLiteral:
        {
            X = uX(i);
            YZ = uYZ(i);
            assert(YZ < vm->num_literals);
            reg0[X] = vm->literals[YZ];
            break;
        }
        case SetGlobal:
        {
            X = uX(i);
            YZ = uYZ(i);
            // string name(AS_CSTRING(vm, vm->literals[YZ]));
            VTable_put(vm->globals, vm->literals[YZ], reg0[X]);
            break;
        }
        case GetGlobal:
        {
            X = uX(i);
            YZ = uYZ(i);
            reg0[X] = VTable_get(vm->globals, vm->literals[YZ]);
            break;
        }
        case Call:
        {
            if (gc_needed)
            {
                GC();
            }
            X = uX(i); //destreg or r
            Y = uY(i); //funreg or r_0
            Z = uZ(i); //lastreg or r_n

            Value callee = reg0[Y];
            struct VMFunction *funcode = NULL;
            if (callee.tag == VMFunction)
            {
                funcode = callee.f;
            }
            else if (callee.tag == VMClosure)
            {
                funcode = callee.hof->f;
            }
            else
            {
                runerror(vm, "Attempted to call a non function\n");
            }
            Activation act = {NULL, reg0, reg0 + X, regWindow, vm->pc, vm->fun};
            if (vm->stack_length == STACK_SIZE)
            {
                runerror(vm, "Recursion too deep, stack overflow\n");
            }
            if (regWindow + (Z - Y) + funcode->nregs > NUM_REGISTERS)
            {
                runerror(vm, "Register file overflow\n");
            }

            regWindow += (Z - Y);
            vm->call_stack[vm->stack_length] = act;
            vm->stack_length++;
            reg0 = reg0 + Y;
            GCVALIDATE(funcode);
            vm->fun = funcode;
            vm->pc = 0;
            // ip = funcode->instructions;

            break;
        }
        case Return:
        {
            X = uX(i);
            Activation act = vm->call_stack[vm->stack_length - 1];
            // ip = act.ip;
            vm->fun = act.fun;
            vm->pc = act.pc;
            *act.dest = reg0[X];
            reg0 = act.register_window;
            regWindow = act.rW;
            vm->stack_length--;
            break;
        }
        case TailCall:
        {
            if (gc_needed)
            {
                GC();
            }
            X = uX(i); //funreg
            Y = uY(i); //lastarg
            Value callee = reg0[X];
            struct VMFunction *funcode = NULL;
            if (callee.tag == VMFunction)
            {
                funcode = callee.f;
            }
            else if (callee.tag == VMClosure)
            {
                funcode = callee.hof->f;
            }
            else
            {
                runerror(vm, "Attempted to call a non function\n");
            }

            if (regWindow + funcode->nregs > NUM_REGISTERS)
            {
                runerror(vm, "Register file overflow\n");
            }
            if (Y - X != funcode->arity)
            {
                runerror(vm, "Arity mismatch");
            }
            memmove(reg0, (reg0 + X), sizeof(*reg0) * (Y - X + 1));
            // ip = funcode->instructions;
            GCVALIDATE(funcode);
            vm->fun = funcode;
            vm->pc = 0;
            break;
        }
        case IsNumber:
        {
            X = uX(i);
            Y = uY(i);

            reg0[Y] = mkBooleanValue((reg0[X].tag == Number));
            break;
        }
        case IsSymbol:
        {
            X = uX(i);
            Y = uY(i);

            reg0[X] = mkBooleanValue((reg0[Y].tag == String));
            break;
        }
        case IsFunction:
        {
            X = uX(i);
            Y = uY(i);

            reg0[X] = mkBooleanValue((reg0[Y].tag == VMFunction));
            break;
        }
        case Cons:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);

            Value v1 = reg0[Y];
            Value v2 = reg0[Z];
            struct VMBlock *p = (struct VMBlock *)vmalloc_raw(vmsize_cons());
            GCINIT(*p);
            p->nslots = 2;
            p->slots[0] = v1;
            p->slots[1] = v2;
            reg0[X] = mkConsValue(p);
            break;
        }
        case Car:
        {
            X = uX(i);
            Y = uY(i);
            Value v1 = reg0[Y];
            struct VMBlock *b = AS_CONS_CELL(vm, v1);
            reg0[X] = b->slots[0];
            break;
        }
        case Cdr:
        {
            X = uX(i);
            Y = uY(i);
            Value v1 = reg0[Y];
            struct VMBlock *b = AS_CONS_CELL(vm, v1);
            reg0[X] = b->slots[1];
            break;
        }
        case IsBoolean:
        {
            X = uX(i);
            Y = uY(i);

            reg0[Y] = mkBooleanValue((reg0[X].tag == Boolean));
            break;
        }
        case IsPair:
        {
            X = uX(i);
            Y = uY(i);

            if (reg0[Y].tag == ConsCell || reg0[Y].tag == Block)
            {
                struct VMBlock *b = AS_CONS_CELL(vm, reg0[Y]);
                reg0[X] = mkBooleanValue(b->nslots == 2);
            }
            else
            {
                reg0[X] = mkBooleanValue(false);
            }
            break;
        }
        case IsNull:
        {
            X = uX(i);
            Y = uY(i);
            reg0[X] = mkBooleanValue((reg0[Y].tag == Emptylist));
            break;
        }
        case IsNil:
        {
            X = uX(i);
            Y = uY(i);

            reg0[Y] = mkBooleanValue((reg0[X].tag == Nil));
            break;
        }
        case Greater:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);
            Value v1 = reg0[Y];
            Value v2 = reg0[Z];
            Number_T n1 = AS_NUMBER(vm, v1);
            Number_T n2 = AS_NUMBER(vm, v2);
            reg0[X] = mkBooleanValue(n1 > n2);
            break;
        }
        case Less:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);
            Value v1 = reg0[Y];
            Value v2 = reg0[Z];
            Number_T n1 = AS_NUMBER(vm, v1);
            Number_T n2 = AS_NUMBER(vm, v2);
            reg0[X] = mkBooleanValue(n1 < n2);
            break;
        }
        case MkClosure:
        {
            X = uX(i);
            Y = uY(i);
            Z = uZ(i);
            Value v1 = reg0[Y];
            struct VMFunction *f = AS_VMFUNCTION(vm, v1);
            struct VMClosure *p = (struct VMClosure *)vmalloc_raw(vmsize_closure(Z));
            GCINIT(*p);
            p->f = f;
            p->nslots = Z;
            reg0[X] = mkClosureValue(p);
            break;
        }
        case GetClSlot:
        {
            struct VMClosure *cls = AS_CLOSURE(vm, reg0[Y]);
            reg0[X] = cls->captured[Z];
            break;
        }
        case SetClSlot:
        {
            struct VMClosure *cls = AS_CLOSURE(vm, reg0[X]);
            cls->captured[Z] = reg0[Y];
            break;
        }
        default:
            printf("%d\n", op);
            printf("Opcode not implemented\n");
            abort();
        }
    }
}
