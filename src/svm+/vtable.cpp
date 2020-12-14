// Implementation of hash table, modified from Hanson CII

// Will have to be looked at in module 12, but not until then.
#include <assert.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "gcmeta.h"
#include "vtable.h"
#include "vmheap.h"
#include "vmsizes.h"

#include "print.h"

#define T VTable_T
struct VTable_T_
{
  GCMETA(VTable_T_)
  int size;   // number of buckets
  int length; // population
  unsigned timestamp;
  struct binding
  {
    VTable_T_::binding *link;
    Value key;
    Value value;
  } * *buckets;
};

void *firstbucket(T table)
{
  void *p = table->buckets[0];
  if (p)
    fprintf(stderr, "First bucket is at %p\n", (void *)&table->buckets[0]);
  return p;
}

void *firstbucketaddr(T table)
{
  return &table->buckets[0];
}

extern T *VTable_forwarded_ptr(T vtable)
{
  assert(vtable);
  return &vtable->forwarded;
}

size_t VTable_size(int nbuckets)
{
  T table;
  return sizeof(*table) + nbuckets * sizeof(table->buckets[0]);
}

T VTable_new(int hint)
{
  int i;
  static int primes[] = {5, 5, 11, 23, 47, 97, 197, 397, 509, 1021, 2053, 4093,
                         8191, 16381, 32771, 65521, INT_MAX};
  // smaller sizes borrowed from Lua 3.0
  assert(hint >= 0);
  for (i = 1; primes[i] < hint; i++)
    ;
  T table = (T)malloc(VTable_size(primes[i - 1]));
  GCINIT(*table);
  table->size = primes[i - 1];
  table->buckets = (VTable_T_::binding **)(table + 1);
  for (i = 0; i < table->size; i++)
    table->buckets[i] = NULL;
  table->length = 0;
  table->timestamp = 0;
  return table;
}

Value VTable_get(T table, Value key)
{
  int i;
  VTable_T_::binding *p;
  assert(table);
  uint32_t h = hashvalue(key);
  i = h % table->size;
  for (p = table->buckets[i]; p; p = p->link)
    if (identical(key, p->key))
      break;
    else if (0)
    {
      fprintf(stderr, "Tags %d and %d: ", key.tag, p->key.tag);
      fprintf(stderr, "Payloads %p and %p\n", (void *)key.s, (void *)p->key.s);
      heapsearch("parameter's payload", key.s);
      heapsearch("stored key's payload", p->key.s);

      fprint(stderr, "Key %v not identical to stored key %v", key, p->key);
    }
  if (0 && p == NULL)
  {
    for (int i = 0; i < table->size; i++)
      if (table->buckets[i])
        fprint(stderr, "bucket %d has key %v\n", i, table->buckets[i]->key);
  }
  return p ? p->value : nilValue;
}

void VTable_put(T table, Value key, Value value)
{
  int i;
  VTable_T_::binding *p;
  Value prev;
  assert(table);
  if (value.tag == Nil)
  {
    VTable_remove(table, key);
  }
  else
  {
    i = hashvalue(key) % table->size;
    for (p = table->buckets[i]; p; p = p->link)
      if (identical(key, p->key))
        break;
    if (p == NULL)
    {
      p = (VTable_T_::binding *)vmalloc_raw(sizeof(*p));
      p->key = key;
      p->link = table->buckets[i];
      table->buckets[i] = p;
      table->length++;
      prev = nilValue;
    }
    else
      prev = p->value;
    p->value = value;
    table->timestamp++;
    (void)prev;
    //	return prev;
  }
}
int VTable_length(T table)
{
  assert(table);
  return table->length;
}

void VTable_internal_values(T table, void visit(Value *vp))
{
  assert(table);
  assert(visit);
  //  fprintf(stderr, "table has %d buckets holding %d elements\n", table->size, table->length);
  for (int i = 0; i < table->size; i++)
    for (VTable_T_::binding *p = table->buckets[i]; p; p = p->link)
    {
      //      fprint(stderr, "visiting internal pair { %v |--> %v }\n", p->key, p->value);
      //      fprintf(stderr, "before visit, internal key payload is %p\n", (void*)p->key.s);
      visit(&p->key);
      visit(&p->value);
      //      fprintf(stderr, "after visit, internal key payload is %p\n", (void*)p->key.s);
    }
}

static VTable_T_::binding *copy_chain(VTable_T_::binding *p)
{
  if (p == NULL)
  {
    return p;
  }
  else
  {
    VTable_T_::binding *copy = (VTable_T_::binding *)vmalloc_raw(sizeof(*p));
    //    fprint(stderr, "coping pair { %v |--> %v }\n", p->key, p->value);
    copy->key = p->key;
    copy->value = p->value;
    copy->link = copy_chain(p->link);
    return copy;
  }
}

T VTable_copy(T old)
{
  assert(old);

  VTable_T nw = (VTable_T)malloc(VTable_size(old->size));
  GCINIT(*nw);

  memcpy(nw, old, sizeof(*nw));
  nw->buckets = (VTable_T_::binding **)(nw + 1);
  for (int i = 0; i < old->size; i++)
    nw->buckets[i] = copy_chain(old->buckets[i]);
  return nw;
}

void VTable_remove(T table, Value key)
{
  int i;
  VTable_T_::binding **pp;
  assert(table);
  table->timestamp++;
  i = hashvalue(key) % table->size;
  for (pp = &table->buckets[i]; *pp; pp = &(*pp)->link)
    if (identical(key, (*pp)->key))
    {
      VTable_T_::binding *p = (VTable_T_::binding *)*pp;
      // Value value = p->value;
      *pp = p->link;
      // FREE(p);
      table->length--;
      //			// return value;
    }
  //	return nilValue;
}