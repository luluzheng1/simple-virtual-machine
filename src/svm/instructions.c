// List of all opcodes, parsers, and unparsers

// You'll develop this list from module 2 onward.  Every time
// you add a new instruction, you'll add an entry here.
// You'll also define the opcode in file opcodes.h,
// and you'll add a case to your `vmrun` function.

#include "iformat.h"
#include "name.h"
#include "itable.h"

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

instruction_info instructions[] = {
    {"halt", Halt, parseR0, "halt"},
    {"print", Print, parseR1, "print rX"},
    {"error", Error, parseR1, "error rX"},
    {"printu", Printu, parseR1, "printu rX"},
    {"println", Println, parseR1, "println rX"},
    {"return", Return, parseR1, "return rX"},
    {"error", Error, parseR1, "error rX"},
    {"loadliteral", LoadLiteral, parseR1LIT, "rX := LIT"},
    {"check", Check, parseR1LIT, "check LIT, rX"},
    {"expect", Expect, parseR1LIT, "expect LIT, rX"},
    {"check-assert", Assert, parseR1LIT, "assert LIT, rX"},
    {"projectbool", ProjectBool, parseR2, "projectbool rY := Bool(rX)"},
    {"zero", Zero, parseR1, "rX := 0"},
    {"setglobal", SetGlobal, parseR1LIT, "globals[LIT] := rX"},
    {"getglobal", GetGlobal, parseR1LIT, "rX := globals[LIT]"},
    {"if", If, parseR1, "if rX"},
    {"goto", Goto, parseR0I24, "ip := OFFSET"},
    {"mov", Mov, parseR2, "rX := rY"},
    {"!", Not, parseR2, "rX := !Bool(rY)"},
    {"~", BitwiseNot, parseR1, "rX := ~rX"},
    {"function?", IsFunction, parseR2, "rX := function? rY"},
    {"pair?", IsPair, parseR2, "rX := pair? rY"},
    {"symbol?", IsSymbol, parseR2, "rX := symbol? rY"},
    {"number?", IsNumber, parseR2, "rX := number? rY"},
    {"boolean?", IsBoolean, parseR2, "rX := boolean? rY"},
    {"null?", IsNull, parseR2, "rX := null? rY"},
    {"nil?", IsNil, parseR2, "rX := nil? rY"},
    {"cdr", Cdr, parseR2, "rX := cdr rY"},
    {"car", Car, parseR2, "rX := car rY"},
    {"hash", Hash, parseR2, "rY := hash rX"},
    {"tailcall", TailCall, parseR2, "tailcall rX (rX+1, ..., rY)"},
    {"cons", Cons, parseR3, "rX := rY cons rZ"},
    {"eq", Equal, parseR3, "rX := rY eq rZ"},
    {"=", Equal2, parseR3, "rX := rY = rZ"},
    {">", Greater, parseR3, "rX := rY > rZ"},
    {"<", Less, parseR3, "rX := rY < rZ"},
    {"<=", LessEq, parseR3, "rX := rY <= rZ"},
    {"idiv", Idiv, parseR3, "rX := rY idiv rZ"},
    {"/", Divide, parseR3, "rX := rY / rZ"},
    {"*", Multiply, parseR3, "rX := rY * rZ"},
    {"-", Subtract, parseR3, "rX := rY - rZ"},
    {"+", Add, parseR3, "rX := rY + rZ"},
    {"call", Call, parseR3, "rX := call rY (rY+1, ..., rZ)"},
    {"mkclosure", MkClosure, parseR3, "rX := closure[rY, Z]"},
    {"getclslot", GetClSlot, parseR3, "rX := rY.Z"},
    {"setclslot", SetClSlot, parseR3, "rX.Z := rY"},
    {"gc", GC, parseR0, "gc"},
    {"set-car!", SetCar, parseR2, "rX.0-> rY"},
    {"set-cdr!", SetCdr, parseR2, "rX.1-> rY"},
    {"&&", ShortAnd, parseR3, "rX = rY && rZ"},
    {"||", ShortOr, parseR3, "rX = rY || rZ"}};

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);

int isgetglobal(Opcode code)
{
  (void)code;
  return 0; // change this for your SVM
}
