// Sophisticated strings for the VM

// I'm planning to sweep this one under the rug.  There's a lot going on
// here, but I want to focus on other issues.

// The code is based on Lua strings, OK as per their MIT license.
#include <iostream>
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "vmstring.h"
#include "vmheap.h"

#ifndef VM_MAXSHORTLEN
#define VM_MAXSHORTLEN 40
#endif

#ifndef VM_MINSTRTABSIZE
#define VM_MINSTRTABSIZE 128 // always a power of 2
#endif

typedef struct stringtable
{
  int population;     /* number of strings stored */
  int nbuckets;       /* size of bucket array */
  VMStringP *buckets; // linked by next_interned fields
} stringtable;

static inline size_t stepsize(size_t len)
{
  // rate at which we scan characters in strings
  // caps out number of chars hashed in the range 16..32
  return (len >> 5) + 1;
}

static inline bool islonglen(size_t len)
{
  return len > VM_MAXSHORTLEN;
}

static inline bool islong(VMStringP hs)
{
  return islonglen(hs->length);
}

static char *bytes(VMStringP a)
{
  return &(a->bytes[0]);
}

bool VMString_eqlong(VMStringP a, VMStringP b)
{
  assert(a && b);
  size_t alen = a->length;
  assert(islonglen(alen) && islonglen(b->length));
  return a == b ||
         (alen == b->length && memcmp(bytes(a), bytes(b), alen) == 0);
}

static uint32_t seed = 0x00beef00;
// note vulnerability to DoS attack: https://stackoverflow.com/questions/9241230

uint32_t VMString_hash_bytes(const char *str, size_t l)
{
  // straight outta Lua
  unsigned int h = seed ^ (unsigned int)l;
  size_t step = stepsize(l);
  for (; l >= step; l -= step)
    h ^= ((h << 5) + (h >> 2) + (unsigned char)(str[l - 1]));
  assert(h != 0);
  return h;
}

uint32_t VMString_hash_slow(VMStringP s)
{
  if (s->hash == 0)
    s->hash = VMString_hashlong(s);
  return s->hash;
}

uint32_t VMString_hashlong(VMStringP hs)
{
  assert(islong(hs));
  if (hs->hash == 0)
  { /* no hash? */
    hs->hash = VMString_hash_bytes(bytes(hs), hs->length);
    if (hs->hash == 0)
      hs->hash = 0xfeeb; // the hash is there now
  }
  return hs->hash;
}

static struct stringtable tb; // the table

static VMStringP keep_live(VMStringP s)
{
  while (s && s->forwarded == NULL)
  { // dead
    // fprintf(stderr, "String '%s' dies\n", s->bytes);
    s = s->next_interned;
  }

  if (s)
  {
    // fprintf(stderr, "String '%s' lives!\n", s->bytes);
    // fprintf(stderr, "  It points to %s\n", s->next_interned ? s->next_interned->bytes : "nothing");
    s->forwarded->next_interned = keep_live(s->next_interned);
    return s->forwarded;
  }
  else
  {
    return NULL;
  }
}

void VMString_drop_dead_strings(void)
{
  for (int i = 0; i < tb.nbuckets; i++)
    tb.buckets[i] = keep_live(tb.buckets[i]);
}

static void resize(int newsize)
{
  if (newsize > tb.nbuckets)
  { /* grow table if needed */
    tb.buckets = (VMStringP *)realloc(tb.buckets, newsize * sizeof(tb.buckets[0]));
    // vulnerable to arithmetic overflow in multiplication
    for (int i = tb.nbuckets; i < newsize; i++)
      tb.buckets[i] = NULL;
  }
  for (int i = 0; i < tb.nbuckets; i++)
  { /* rehash */
    VMStringP p = (VMStringP)tb.buckets[i];
    tb.buckets[i] = NULL;
    while (p)
    {                                     /* for each node in the list */
      VMStringP next = p->next_interned;  /* save next */
      unsigned int h = p->hash % newsize; /* new position */
      p->next_interned = tb.buckets[h];   /* chain it */
      tb.buckets[h] = p;
      p = next;
    }
  }
  if (newsize < tb.nbuckets)
  { /* shrink table if needed */
    /* vanishing slice should be empty */
    assert(tb.buckets[newsize] == NULL && tb.buckets[tb.nbuckets - 1] == NULL);
    tb.buckets = (VMStringP *)realloc(tb.buckets, newsize * sizeof(tb.buckets[0]));
  }
  tb.nbuckets = newsize;
}

void VMString_init(void)
{
  tb.population = 0;
  tb.nbuckets = VM_MINSTRTABSIZE;
  tb.buckets = (VMStringP *)calloc(tb.nbuckets, sizeof(tb.buckets[0]));
  assert(tb.buckets);
}

void VMString_finish(void)
{
  tb.population = 0;
  tb.nbuckets = 0;
  free(tb.buckets);
  tb.buckets = NULL;
}

static VMStringP allocstring(size_t len, uint32_t hash)
{
  // initializes everything except the bytes -- even the terminating '\0'

  VMStringP hs = (VMStringP)malloc(VMString_objsize(len));
  GCINIT(*hs);
  hs->length = len;
  hs->hash = hash;
  hs->next_interned = NULL;
  GCINIT(*hs);
  hs->bytes[len] = '\0';
  return hs;
}

void VMString_remove(VMStringP hs)
{
  // called by Lua GC
  // (not likely how things will work with copying GC)
  VMStringP *p = &tb.buckets[hs->hash % tb.nbuckets];
  while (*p != hs) /* find previous element */
    p = &(*p)->next_interned;
  *p = (*p)->next_interned; /* remove element from its list */
  tb.population--;
}

static VMStringP intern_short(const char *str, size_t len, VMStringP alloced)
{
  // returns existing copy or creates a new one
  assert(!islonglen(len));
  uint32_t h = VMString_hash_bytes(str, len);
  VMStringP *list = &tb.buckets[h % tb.nbuckets];
  assert(str != NULL); /* otherwise 'memcmp'/'memcpy' are undefined */
  for (VMStringP hs = *list;
       hs != NULL;
       hs = hs->next_interned)
  {
    if (len == hs->length &&
        (memcmp(str, bytes(hs), len * sizeof(char)) == 0))
    {
      /* found! */
      // if dead, might have to mark it live for GC
      return hs;
    }
  }
  if (tb.population >= tb.nbuckets && tb.nbuckets <= INT_MAX / 2)
  {
    resize(tb.nbuckets * 2);
    list = &tb.buckets[h % tb.nbuckets]; /* recompute with new size */
  }
  VMStringP hs;
  if (alloced != NULL)
  {
    hs = alloced;
    hs->hash = h;
  }
  else
  {
    hs = allocstring(len, h);
    memcpy(bytes(hs), str, len * sizeof(char));
  }
  hs->next_interned = *list;
  *list = hs;
  tb.population++;
  return hs;
}

VMStringP VMString_new(const char *str, size_t len)
{
  if (!islonglen(len))
    return intern_short(str, len, NULL);
  else
  {
    VMStringP hs;
    if (len >= (SIZE_MAX - sizeof(*hs)) / sizeof(char))
      assert(0); // too big!
    hs = allocstring(len, 0);
    memcpy(bytes(hs), str, len * sizeof(char));
    return hs;
  }
}

VMStringP VMString_newc(const char *str)
{
  // N.B. Lua caches these!
  return VMString_new(str, strlen(str));
}

////////////////////////////////////////////////////////////////

struct StringBufferO
{
  unsigned bogus; // just a placeholder; it's really a VMString
                  // with hash acting as fill count
};

StringBuffer VMString_buffer(size_t length)
{
  return (StringBuffer)allocstring(length, 0);
}

void VMString_putc(StringBuffer b, char c)
{
  VMStringP hs = (VMStringP)b;
  assert(hs && hs->hash < hs->length);
  hs->bytes[hs->hash++] = c;
}

void VMString_puts(StringBuffer b, VMStringP s)
{
  VMStringP hs = (VMStringP)b;
  assert(hs && s);
  size_t slen = s->length;
  assert(hs->hash + slen <= hs->length);
  memcpy(hs->bytes + hs->hash, s->bytes, slen);
  hs->hash += slen;
}

VMStringP VMString_of_buffer(StringBuffer *bp)
{
  VMStringP hs = (VMStringP)(*bp);
  assert(hs->hash == hs->length);
  if (islong(hs))
    hs->hash = 0;
  else
    hs = intern_short(bytes(hs), hs->length, hs);
  *bp = NULL;
  return hs;
}
