//// Parsers for loading virtual object code.

// From module 2 onward, you'll need to know how these work so that
// you can associate an appropriate parser with each opcode in your
// instruction table (file instructions.c).  There is one parser for
// each encoding function in header file "iformat.h", plus one for
// parsing an instruction that includes a literal (which eventually
// gets encoded in format R1U16).

// In module 2, you'll implement function `parseR1LIT` in file iparsers.c.

#ifndef IPARSERS_INCLUDED
#define IPARSERS_INCLUDED

#include <inttypes.h>
#include <stdio.h>

#include "iformat.h"
#include "tokens.h"

typedef struct VMState *VMStateP;

typedef Instruction (*InstructionParser)(VMStateP, Opcode, Tokens, unsigned *maxregp);
// Consume all the tokens (which represent operands),
// update *maxregp with the largest register seen,
// and return the encoded instruction.  If the opcode
// calls for a literal, ensures the literal appears in the VM's pool.

Instruction parseR3(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR2(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR0(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR2U8(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1U16(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR0I24(VMStateP, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1LIT(VMStateP, Opcode, Tokens, unsigned *maxregp);

#endif
