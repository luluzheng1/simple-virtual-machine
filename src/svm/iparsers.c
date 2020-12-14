//// Parsers for loading virtual object code.

// In module 2, you add parsers `parseR1LIT` to this file.
// The other parsers may serve as examples you can build on.

#include <assert.h>
#include <stdlib.h>

#include "iformat.h"
#include "iparsers.h"
#include "vmstate.h"
#include "print.h"

#define SEE(R)         \
  do                   \
  {                    \
    if ((R) > *maxreg) \
      *maxreg = (R);   \
  } while (0)

static Value get_literal(Tokens *litp, const char *input);

Instruction parseR3(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t regZ = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  SEE(regY);
  SEE(regZ);
  return eR3(opcode, regX, regY, regZ);
}

Instruction parseR2(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  SEE(regY);
  return eR2(opcode, regX, regY);
}

Instruction parseR1(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  return eR1(opcode, regX);
}

Instruction parseR0(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  (void)maxreg;
  assert(operands == NULL);
  return eR0(opcode);
}

Instruction parseR1U16(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  (void)maxreg;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint32_t immediate = tokens_get_int(&operands, NULL);
  assert(operands == NULL);
  assert(immediate == (uint16_t)immediate);
  SEE(regX);
  return eR1U16(opcode, regX, immediate);
}

Instruction parseR2U8(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t k = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  SEE(regY);
  return eR3(opcode, regX, regY, k);
}

Instruction parseR0I24(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  (void)vm;
  (void)maxreg;
  int32_t immediate = tokens_get_int(&operands, NULL);
  assert(immediate == ((immediate << 8) >> 8));
  assert(operands == NULL);
  return eR0I24(opcode, immediate);
}

static Name truename, falsename, nilname, emptyname, stringname;

static void initnames(void)
{
  if (truename == NULL)
  {
    truename = strtoname("true");
    falsename = strtoname("false");
    nilname = strtoname("nil");
    emptyname = strtoname("emptylist");
    stringname = strtoname("string");
  }
}
Instruction parseR1LIT(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg)
{
  initnames(); // before comparing names, you must call this function
  uint8_t regX = tokens_get_byte(&operands, NULL);
  Value v = get_literal(&operands, NULL);
  int slot = literal_slot(vm, v);
  assert(operands == NULL);
  SEE(regX);
  return eR1U16(opcode, regX, slot);
}

char buff[1 << 6];
static Value parsenameliteral(Name parsed_name, Tokens *litp, const char *input)
{

  if (parsed_name == truename)
  {
    return mkBooleanValue(true);
  }
  else if (parsed_name == falsename)
  {
    return mkBooleanValue(false);
  }
  else if (parsed_name == nilname)
  {
    return nilValue;
  }
  else if (parsed_name == emptyname)
  {
    return emptylistValue;
  }
  else if (parsed_name == stringname)
  {
    int length = tokens_get_int(litp, input);

    for (int i = 0; i < length; i++)
    {
      buff[i] = tokens_get_byte(litp, input);
    }
    Vmstring str = Vmstring_new(buff, length);
    return mkStringValue(str);
  }
  assert(false);
}

static Value get_literal(Tokens *litp, const char *input)
{

  // Figure out what type the head of token linked list is
  // Switch on the type
  // Place into literal pool
  switch (first_token_type(*litp))
  {
  case TNAME:
  {
    // Do further string parsing.
    Name parsed_name = tokens_get_name(litp, input);
    return parsenameliteral(parsed_name, litp, input);
  }
  case TU32:
  {
    return mkNumberValue(tokens_get_signed_number(litp, input));
  }
  case TDOUBLE:
  {
    return mkNumberValue(tokens_get_signed_number(litp, input));
  }
  default:
    assert(false);
  }
  return nilValue;
}
