// Defines all the opcodes used in the VM

// When you're thinking about new instructions, define them here first.
// It's OK for an opcode to be defined here even if it is not implemented
// anywhere.  But if you want to *run* an instruction (module 1) or *load*
// an instruction (module 2), the opcode has to be defined here first.

#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

typedef enum opcode
{
    Halt, // R0
    GC,
    Goto,  // R0I24
    Print, // R1
    Printu,
    Println,
    Not,
    BitwiseNot,
    Zero,
    Return,
    Error,
    If,
    ProjectBool, // R2
    Mov,
    IsFunction,
    IsPair,
    IsSymbol,
    IsNumber,
    IsBoolean,
    IsNull,
    IsNil,
    Cdr,
    Car,
    Hash,
    TailCall,
    LoadLiteral, // R1LIT
    SetGlobal,
    GetGlobal,
    Check,
    Expect,
    Assert,
    Cons, // R3
    Equal,
    Equal2,
    Greater,
    Less,
    Idiv,
    Divide,
    Multiply,
    Subtract,
    Add,
    Call,
    MkClosure,
    GetClSlot,
    SetClSlot,
    Unimp, // stand-in for opcodes not yet implemented
} Opcode;

int isgetglobal(Opcode code); // update this for your SVM, in instructions.c

#endif
