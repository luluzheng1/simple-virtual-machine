// Implementation of the VM heap

// Modules 1 to 10: Nothing to see here.
// Modules 11: You'll reclaim and recycle heap pages.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gcdebug.h"
#include "print.h"
#include "svmdebug.h"
#include "value.h"
#include "vmheap.h"
#include "vmsizes.h"
#include "vstack.h"
#include "vmstate.h"
#include "vtable.h"

#ifndef NOVALGRIND

#include <valgrind/memcheck.h>
#define MIN(x, y) ((x > y) ? y : x)

#else

#define VALGRIND_CREATE_BLOCK(p, n, s) ((void)(p), (void)(n), (void)(s))
#define VALGRIND_CREATE_MEMPOOL(p, n, z) ((void)(p), (void)(n), (void)(z))
#define VALGRIND_DESTROY_MEMPOOL(p) ((void)(p))
#define VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(p, n) \
  ((void)(p), (void)(n))
#define VALGRIND_MAKE_MEM_DEFINED(p, n) ((void)(p), (void)(n))
#define VALGRIND_MAKE_MEM_UNDEFINED(p, n) ((void)(p), (void)(n))
#define VALGRIND_MAKE_MEM_NOACCESS(p, n) ((void)(p), (void)(n))
#define VALGRIND_MEMPOOL_ALLOC(p1, p2, n) ((void)(p1), (void)(p2), (void)(n))
#define VALGRIND_MEMPOOL_FREE(p1, p2) ((void)(p1), (void)(p2))

#endif

#define SMALLHEAP /* triggers frequent collections (helps debugging) */

/********************************* HEAP PAGES ********************************/

#ifdef SMALLHEAP
#define PAGESIZE 6400 // bytes in one page, just big enough for globals table
#else
#define PAGESIZE (32 * 1024) // bytes in one page
#endif

// alignment code from Hanson, C Interfaces and Implementations, MIT license

union align
{
#ifdef MAXALIGN
  char pad[MAXALIGN];
#else
  int i;
  long l;
  long *lp;
  void *p;
  void (*fp)(void);
  float f;
  double d;
  long double ld;
#endif
};

typedef struct page
{
  struct page *link;
  union align a;
  // empty space here not declared, just like Hanson
} * Page;

/************************ MAIN HEAP DATA STRUCTURES ************************/

Page current;
// current page, linked to older pages that have already been used
// to satisfy allocation requests ("allocated")
//
// Initialized by `fresh_allocation_page`, and may be updated
// by an allocation.

Page available;
// empty pages that can be used to satisfy future allocation requests

static char *next, *limit;
// if not NULL, pointers into current page

static struct count
{ // things that can be counted on lists
  struct
  {
    int pages;
    int objects;         // number of objects allocated
    int bytes_requested; // number of bytes requested for those objects
  } current;
  struct
  {
    int pages;
  } available; // this is free space; there aren't any objects
} count;       // C semantics guarantee all 0 at startup

/* TERMINOLOGY AND INVARIANTS:

     - Bytes in { *p | next <= p < limit } and in all pages reachable
       from `available` are not in use and are available to satisfy
       future allocation requests.

     - Bytes in { *p | current <=  p < next } and in all pages reachable
       from `current->link` are "in use" or "unavailable."

       Between collections, these bytes are occupied by objects that either
       were live at the last collection or were allocated after the last
       collection.  During a collection, these bytes are occupied by gray
       and black objects.

     - The fields of the `count` struct accurately reflect the number of pages
       in each list.

     - Except during a collection, every page in the managed heap is on
       exactly one of the two lists.

       During a collection, white objects (from-space) may be held in a
       separate list whose head is kept in a private variable that is local
       to the garbage collector.

 */

//// operations on heap data structures

static void acquire_available_page(void);
// Acquires a new page from the OS and puts it on the `available` list

static void take_available_page(void);
// Takes an available page and makes it `current`,
// getting a fresh page from the OS if needed.
// Sets `next` and `limit` so the allocator will
// start (or continue) allocating from that page

static int make_available(Page pages);
// Take every page on the list and add it to the available list.
// Mark it for VALGRIND as unallocated and uninitialized.

static int free_pages(Page pages);
// Gives every page on the list back to the OS, and returns
// the number of pages so freed.

static int make_invalid_and_stomp(Page pages);
// For debugging: takes every page on the list, fills it with ones,
// tells valgrind it's off limits, arranges never to use that memory again,
// and puts it on the `invalid` list.  NOT TESTED!
Page invalid; // list of invalid pages

static bool onpagelist(void *p, Page pages);
// tells if the address lies within one of the pages on the list

/******************** GARBAGE-COLLECTOR DATA STRUCTURES ********************/

static VStack_T gray; // stack of gray objects
                      // Only used during a collection, but we keep it persistent so we
                      // don't have to allocate one and then free it at every collection.

/* Color invariants:

     - A white object has a NULL forwarding pointer and 
       any pointers it contains point into from-space

     - A gray or black object has been copied into to-space.
       The original object's forwarding pointer (in from-space)
       is non-NULL and points to the to-space copy.

     - Any pointers that a gray object contains point into from-space.

     - Any pointers that a black object contains point into to-space.

     - Every gray object appears on the `gray` stack exactly once.

 */

/******** INITIALIZATION AND FINALIZATION (WITH STATISTICS) ****************/

static struct
{ // totals of various actions (for statistics)
  int allocations;
  int bytes_requested;
  int bytes_copied;
  int collections;
} total;

bool gc_in_progress; // controls accounting of requests

void heap_init(void)
{
  (void)make_invalid_and_stomp; // you may use this to debug, but if there are no bugs,
                                // it is not needed
  gray = VStack_new();
  gc_debug_init();
  take_available_page();
}

void heap_shutdown(void)
{
  int ccount = free_pages(current);
  int acount = free_pages(available);
  assert(ccount == count.current.pages);
  assert(acount == count.available.pages);

  current = available = NULL;

  free_pages(invalid);

  assert(gray);
  VStack_free(gray);
  gray = NULL;

  if (svmdebug_value("gcstats"))
  {
    fprint(stderr, "Requested %, bytes in %, allocations\n",
           total.bytes_requested, total.allocations);
    fprint(stderr, "%, garbage collections copied %, bytes\n",
           total.collections, total.bytes_copied);
    double copies_per_alloc =
        (double)total.bytes_copied / (double)total.bytes_requested;
    int decicopies = 100.0 * copies_per_alloc + 0.5;
    fprintf(stderr,
            "The collector copied %.2f byte%s for every byte requested\n",
            copies_per_alloc, decicopies == 100 ? "" : "s");
    fprint(stderr, "At exit, heap contained %, used pages "
                   "and %, available pages\n",
           count.current.pages, count.available.pages);
    fprint(stderr, "Total heap size is %, bytes held in %, pages\n",
           PAGESIZE * (count.current.pages + count.available.pages),
           count.current.pages + count.available.pages);
  }
}

/************  TEST-AND-INCREMENT, PAGE-BASED ALLOCATOR ************/

/* This allocator is the fundamental reason for writing
   a copying collector.  Free space is nearly contiguous
   (just broken into pages), so there is never any need
   to search for a free block of memory.  And if `next`
   is kept in a machine register, which is true in
   native-code systems, it is _extremely_ efficient. */

// we have to know when an object is considered too big
// to allocate on a page

#define NPAYLOAD (PAGESIZE - sizeof(struct page *) - sizeof(union align))

#ifdef SMALLHEAP
#define SMALL_OBJECT_LIMIT NPAYLOAD /* too big to fit in a page? */
#else
#define SMALL_OBJECT_LIMIT (NPAYLOAD / 4) /* too big to copy? */
#endif

static inline size_t roundup(size_t n, size_t block)
{
  return ((n + block - 1) / block) * block;
}

static inline void *alloc_small(size_t n)
{
  // allocate a small object

  // because of alignment, actual size may be larger than requested size
  assert(n > 0);
  long nbytes = (long)roundup(n, sizeof(union align));

  // ensure enough room for the object
  assert(nbytes < limit - (char *)&current->a); // not too big for any page
  if (next + nbytes > limit)                    // <---- this is "the test"
    take_available_page();                      // may trigger heap growth if
                                                // the growth policy is out of whack
  assert(nbytes <= limit - next);

  // allocate the object and mark it for memory tracking
  void *object = next;
  next += nbytes; // <---- this is "the increment"
  VALGRIND_MEMPOOL_ALLOC(current, object, n);

  // track statistics
  count.current.objects++;
  count.current.bytes_requested += n;

  if (gc_in_progress)
  {
    total.bytes_copied += n;
  }
  else
  {
    total.allocations++;
    total.bytes_requested += n;
  }

  return object;
}

void *vmalloc_raw(size_t n)
{
  // the external interface; does not set any GC metadata
  if (n <= SMALL_OBJECT_LIMIT)
  {
    return alloc_small(n);
  }
  else
  {
    fprintf(stderr, "Large-object allocator not implemented for object of size %zu\n", n);
    assert(0);
    return NULL;
  }
}

void *vmcalloc_raw(size_t num, size_t size)
{
  void *block = vmalloc_raw(num * size);
  memset(block, 0, num * size);
  return block;
}

/*******  TRIGGERING GARABAGE COLLECTION AND HEAP GROWTH ********/

static int availability_floor = 0; // trigger lots of collections!
                                   // When count.available.pages drops to this value (or less),
                                   // a garbage collection is needed.  The minimum safe value
                                   // is half the heap size (rounded up), and once things are
                                   // working, there is no reason to make it any larger.

bool gc_needed = false;
// signals to VM to call gc() at next safe point

static void growheap(double gamma, int nlive);
// as needed, acquire new available pages until
// the number of pages on the heap is at least `gamma * nlive`

static double target_gamma(VTable_T globals);
// gamma value to aim for according to global variable &gamma,
// or if that is not a number, a very conservative default.

/****************************************************************/

/*********** CRITICAL GC FUNCTIONS ******************/

static void remember(Value v);
// add the value to the list of gray objects

static void forward_payload(Value *vp);
// takes a pointer to a value and establishes these postconditions:
//   - if the value's payload is allocated on the heap, it points
//     to the unique copy of that payload in to-space
//   - if the value's payload is allocated on the heap, the
//     value is either gray or black
//
// the postcondition is established by conditionally copying
// the payload and remembering the object

static struct VMString *copy_string(struct VMString *p);
static struct VMFunction *copy_function(struct VMFunction *p);
static struct VMClosure *copy_closure(struct VMClosure *p);
static struct VMBlock *copy_block(struct VMBlock *p);
extern VTable_T VTable_copy(VTable_T);
// Each one allocates fresh memory in to-space and
// copies a payload unconditionally.
// In the book, they correspond to `*hp = *p` and `hp++` on page 277b.

static inline struct VMString *forward_string(struct VMString *p, bool *changed_color);
static inline struct VMFunction *forward_function(struct VMFunction *p, bool *changed_color);
static inline struct VMClosure *forward_closure(struct VMClosure *p, bool *changed_color);
static inline struct VMBlock *forward_block(struct VMBlock *p, bool *changed_color);
static inline VTable_T forward_table(VTable_T p, bool *changed_color);
// Each one takes a pointer into from-space and returns a pointer
// to the correponding object in to-space, copying if necessary.
// If copying was necessary, then the object changed color, in which case
// `true` is written through the Boolean pointer (if non-NULL).

///////////// implementations of copy functions

static struct VMString *copy_string(struct VMString *s)
{
  assert(s);                           // paranoia strikes deep
  assert(s->forwarded == NULL);        //
  size_t n = vmsize_string_payload(s); // size of the payload to be copied
  VMStringP nw = (VMStringP)vmalloc_raw(n);
  GCINIT(*nw);
  memcpy(nw, s, n); // does the copy
  return nw;
}

static struct VMFunction *copy_function(struct VMFunction *s)
{
  assert(s->forwarded == NULL);
  size_t n = vmsize_fun_payload(s);
  struct VMFunction *nw = (struct VMFunction *)vmalloc_raw(n);
  GCINIT(*nw);
  memcpy(nw, s, n);
  return nw;
}

static struct VMBlock *copy_block(struct VMBlock *p)
{
  assert(p->forwarded == NULL);
  size_t n = vmsize_block_payload(p);
  struct VMBlock *nw = (struct VMBlock *)vmalloc_raw(n);
  GCINIT(*nw);
  memcpy(nw, p, n);
  return nw;
}

static struct VMClosure *copy_closure(struct VMClosure *p)
{
  assert(p->forwarded == NULL);
  size_t n = vmsize_closure_payload(p);
  struct VMClosure *nw = (struct VMClosure *)vmalloc_raw(n);
  GCINIT(*nw);
  memcpy(nw, p, n);
  return nw;
}

///////////// implementations of forward functions

static inline struct VMString *forward_string(struct VMString *p, bool *changed)
{
  if (p->forwarded)
  {                      // corresponds to book's `p->alt == FORWARD`,
    return p->forwarded; // on page 277b
  }
  else
  {
    p->forwarded = copy_string(p); // does the other case in the book
    if (changed)
      *changed = true; // not needed in book (to-space is the gray queue)
    return p->forwarded;
  }
}

static inline struct VMFunction *forward_function(struct VMFunction *p, bool *changed)
{
  if (p->forwarded)
  {
    return p->forwarded;
  }
  else
  {
    p->forwarded = copy_function(p);
    if (changed)
      *changed = true;
    return p->forwarded;
  }
}

static inline struct VMClosure *forward_closure(struct VMClosure *p, bool *changed)
{
  if (p->forwarded)
  {
    return p->forwarded;
  }
  else
  {
    p->forwarded = copy_closure(p);
    if (changed)
      *changed = true;
    return p->forwarded;
  }
}

static inline struct VMBlock *forward_block(struct VMBlock *p, bool *changed)
{
  if (p->forwarded)
  {
    return p->forwarded;
  }
  else
  {
    p->forwarded = copy_block(p);
    if (changed)
      *changed = true;
    return p->forwarded;
  }
}

static inline VTable_T forward_table(VTable_T table, bool *changed)
{
  VTable_T *fp = VTable_forwarded_ptr(table);
  if (*fp)
  {
    return *fp;
  }
  else
  {
    VTable_T nw = VTable_copy(table);
    *fp = nw;
    if (changed)
      *changed = true;
    return nw;
  }
}

static void remember(Value v)
{
  gcprint("Enqueuing %v\n", v);
  VStack_push(gray, v);
}

static void forward_payload(Value *vp)
{
  bool changed_color = false;
  switch (vp->tag)
  {
  //// cases with payloads not allocated on the heap (or no payload)
  case Nil:
  case Boolean:
  case Number:
  case Emptylist:
    return; // nothing on the heap

  //// cases with heap-allocated payloads
  //// that don't themselves contain payload pointers
  case String:
    vp->s = forward_string(vp->s, NULL);
    return; // does not contain Values,
            // so needn't be remembered (goes straight to black)

  case VMFunction:
    vp->f = forward_function(vp->f, NULL);
    return; // like string, goes straight to black

  //// cases with heap-allocated payloads
  //// that might contain payload pointers
  case ConsCell:
  case Block:
    vp->block = forward_block(vp->block, &changed_color);
    if (changed_color)
      remember(*vp);
    return;

  case VMClosure:
    vp->hof = forward_closure(vp->hof, &changed_color);
    if (changed_color)
      remember(*vp);
    return;

  case Table:
    vp->table = forward_table(vp->table, &changed_color);
    if (changed_color)
      remember(*vp);
    return;

  //// other cases
  case LightUserdata:
  case CFunction:
    return; // external memory, not on managed heap

  case Seq:
    assert(0 && "sequences unimplemented");

  default:
    assert(0);
  }
}

////////////////////  scanning functions

static void scan_value(Value v);
static void scan_activation(struct Activation *);
static void scan_vmstate(struct VMState *vm);
// These functions scan an object, forwarding any payload
// pointers that the object contains.  When scanning is complete,
// the object is black.

extern void gc(struct VMState *vm);
// Copy all live objects to to-space,
// then move the old from-space pages to `available`.
// Also increment `total.collections` and clear flag `gc_needed`.

static void scan_value(Value v)
{
  // this function is roughly analogous
  // to the book's `scanloc` function on page 282b
  switch (v.tag)
  {
  case Nil:
  case Boolean:
  case Number:
  case Emptylist:
    assert(0 && "tried to scan a value with no heap-allocated payload");
  case LightUserdata:
  case CFunction:
    assert(0 && "tried to scan a value with external memory");
  case String:
  case VMFunction:
    assert(0 && "tried to scan a payload that contains no pointers");

  // blocks and closures very similar: forward the payload in each slot
  // (analogous to PAIR case on page 282b)
  case ConsCell:
  case Block:
  {
    struct VMBlock *p = v.block;
    int n = p->nslots;
    for (int i = 0; i < n; i++)
      forward_payload(&p->slots[i]);
    return;
  }
  case VMClosure:
  {
    struct VMClosure *p = v.hof;
    p->f = forward_function(p->f, NULL);
    // contains no pointers, so needn't check for color change
    int n = p->nslots;
    for (int i = 0; i < n; i++)
      forward_payload(&p->captured[i]);
    return;
  }

  // table representation is abstract, so it's done with a higher-order function
  case Table:
  {
    VTable_T table = v.table;
    VTable_internal_values(table, forward_payload);
    return;
  }

  case Seq:
    assert(0 && "sequences unimplemented");
  default:
    assert(0 && "bad tag");
  }
}

static void scan_activation(struct Activation *p)
{
  assert(p);
  p->fun = forward_function(p->fun, NULL);
}

static void scan_vmstate(struct VMState *vm)
{
  assert(vm);
  uint32_t max_rw = 255;
  Activation last_act;

  // Check if we have an activation record.
  // If we do, we can refer to it for max reg window.
  if (vm->stack_length > 0)
  {
    last_act = vm->call_stack[vm->stack_length - 1];
    max_rw = MIN(last_act.rW + 255, NUM_REGISTERS);
  }

  for (uint32_t i = 0; i < max_rw; i++)
  {
    forward_payload(&(vm->registers[i]));
  }
  // see book chapter page 265 about roots

  // roots: all registers that can affect future computation
  //    (these hold local variables and formal paramters as on page 265)
  //    (hint: don't scan high-numbered registers that can't
  //     affect future computations because they aren't used)

  // roots: all literal slots that are in use
  for (uint32_t i = 0; i < vm->num_literals; i++)
  {
    forward_payload(&(vm->literals[i]));
  }
  // roots: each function on the call stack
  for (uint32_t i = 0; i < vm->stack_length; i++)
  {
    scan_activation(&vm->call_stack[i]);
  }

  // root: the currently running function (which might not be on the call stack)
  vm->fun = forward_function(vm->fun, NULL);
  // root: the global-variable table
  //    (hint: copy the table to to-space,
  //     then scan it with `mkTableValue` and `scan_value`)

  // vm->globals = forward_table(vm->globals, NULL);
  // scan_value(mkTableValue(vm->globals));

  // root: any other field of `struct VMState` that could lead to a `Value`
}

extern void gc(struct VMState *vm)
{
  assert(vm);
  // 1. Capture the list of allocated pages from `current`,
  //        and reset `current` include just one available page.
  //        I recommend capturing the list of allocated pages
  //        in a local variable called `fromspace`.
  /*
  Grab all the allocated pages (including the current pages) and 
  keep a pointer to them (in a local variable). These are y
  our from-space pages.
   Your to-space pages are in available.
  */
  Page fromspace = current;
  current = NULL;

  take_available_page();

  // Set flag `gc_in_progress` (so statistics are tracked correctly).
  gc_in_progress = true;

  // Color all the roots gray using `scan_vmstate`.
  scan_vmstate(vm);

  int live_data = 0;

  // While the gray stack is not empty, pop a value and scan it.
  while (!VStack_isempty(gray))
  {
    Value v = VStack_pop(gray);
    scan_value(v);
    live_data++;
  }

  // Call `VMString_drop_dead_strings()`.
  VMString_drop_dead_strings();
  // Take the pages captured in step 1 and make them available.
  int reclaimed = make_available(fromspace);
  count.current.pages -= reclaimed;

  // Use `growheap` to acquire more available pages until the
  // ratio of heap size to live data meets what you get from
  // `target_gamma`.  (The amount of live data is the number of
  // pages copied to `current` in steps 3 and 4.)
  if (live_data > 0)
  {
    double target = target_gamma(vm->globals);
    double ratio = (count.available.pages + count.current.pages) * PAGESIZE / live_data;
    while (ratio < target)
    {

      growheap(target, live_data);
      ratio = (count.available.pages + count.current.pages) * PAGESIZE / live_data;
    }
  }
  // Update counter `total.collections` and
  // flags `gc_needed` and `gc_in_progress`.
  gc_in_progress = false;
  gc_needed = false;
  total.collections++;

  // If `svmdebug_value("gcstats")` is set and contains a + sign,
  // print statistics as suggested by exercise 2 on page 299.

  if (svmdebug_value("gcstats") && strchr(svmdebug_value("gcstats"), '+'))
  {
    fprintf(stderr, "Heap contains %d pages of which %d are live (ratio %.2f)\n",
            count.available.pages + count.current.pages, count.current.pages,
            (double)(count.available.pages + count.current.pages) / (double)count.current.pages);
    fprint(stderr, "%d of %d objects holding %, of %, requested bytes survived\n",
           -1, -1, -1, -1); // you fill in here
    fprintf(stderr, "Survival rate is %.1f%% of objects and %.1f%% of bytes\n",
            -1.0, -1.0); // you fill in here
  }
}

static void growheap(double gamma, int nlive)
{
  bool grew = false;
  // printf("num pages: %d, nlive: %d, gamma: %f\n", (count.available.pages + count.current.pages) * PAGESIZE, nlive, gamma);
  while ((count.available.pages + count.current.pages) * PAGESIZE < nlive * gamma)
  {
    acquire_available_page();
    grew = true;
  }

  if (grew && svmdebug_value("growheap"))
    fprintf(stderr, "Grew heap to %d pages\n",
            count.current.pages + count.available.pages);

  availability_floor = count.available.pages;
  // eventually you'll add code here to enlarge the heap
  //     and to update `availability_floor`
}

/********************  PAGE FUNCTIONS **********************************/

bool onpagelist(void *p, Page page)
{
  for (; page; page = page->link)
  {
    char *first = (char *)&page->a;
    char *limit = (char *)page + PAGESIZE;
    char *test = (char *)p;
    if (first <= test && test < limit)
      return true;
  }
  return false;
}

static void acquire_available_page(void)
{
  Page p = (Page)malloc(PAGESIZE);
  assert(p);
  VALGRIND_CREATE_MEMPOOL(p, 0, 0);
  VALGRIND_CREATE_BLOCK(p, PAGESIZE, "managed page");
  p->link = available;
  available = p;
  count.available.pages++;
  gcprintf("Acquired a new page\n");
}

static Page newpage(void)
{
  // returns a page from the `available` list, or if necessary from the OS
  if (0 && current != NULL)
  {
    heap_shutdown();
    extern void name_cleanup(void);
    name_cleanup();
    VMString_finish();
    extern VMStateP *globalvmp;
    freestatep(globalvmp);

    exit(0);
  }
  if (available == NULL)
    acquire_available_page();

  assert(available);
  Page nw = available;
  available = available->link;
  count.available.pages--;
  if (count.available.pages <= availability_floor)
    gc_needed = true;
  return nw;
}

static void take_available_page(void)
{
  Page nw = newpage();
  nw->link = current;
  current = nw;
  assert(current);
  count.current.pages++;
  next = (char *)&current->a;
  limit = (char *)current + PAGESIZE;
}

static int free_pages(Page p)
{ // returns # of pages freed
  int count = 0;
  while (p)
  {
    count++;
    Page next = p->link;
    VALGRIND_DESTROY_MEMPOOL(p);
    free(p);
    p = next;
  }
  return count;
}

/************************** RANDOM UTILITY FUNCTIONS **************************/

static int make_invalid_and_stomp(Page pages)
{
  int invalidated = 0;
  while (pages)
  {
    Page p = pages;
    pages = p->link;
    p->link = invalid;
    invalid = p;
    memset(p + 1, 0xff, PAGESIZE - sizeof(*p));
    VALGRIND_MAKE_MEM_NOACCESS(p, PAGESIZE);
    invalidated++;
  }
  return invalidated;
}

static int make_available(Page pages)
{
  int reclaimed = 0;
  while (pages)
  {
    Page p = pages;
    pages = p->link;
    VALGRIND_DESTROY_MEMPOOL(p);
    VALGRIND_CREATE_MEMPOOL(p, 0, 0);
    p->link = available;
    available = p;
    reclaimed++;
    count.available.pages++;
  }
  return reclaimed;
}

void search(const char *what, void *p, Page fromspace)
{
  bool found = 0;
  if (onpagelist(p, current))
  {
    found = 1;
    fprintf(stderr, "%s on correct (current) list\n", what);
  }
  if (fromspace && onpagelist(p, fromspace))
  {
    found = 1;
    fprintf(stderr, "%s stranded in fromspace\n", what);
  }
  if (onpagelist(p, available))
  {
    found = 1;
    fprintf(stderr, "%s is available?"
                    "?!\n",
            what);
  }
  if (!found)
    fprintf(stderr, "Cannot find %s anywhere?\n", what);
}
void xsearch(const char *what, void *p)
{
  search(what, p, NULL);
}

static double target_gamma(VTable_T globals)
{
  double gamma = 2.1; // very conservative defaul
  Value vmgamma = VTable_get(globals, mkStringValue(VMString_newc("&gamma")));
  if (vmgamma.tag == Number)
  {
    gamma = vmgamma.n;
  }
  if (gamma >= 100.0)
    gamma = gamma / 100.0;
  if (gamma < 2.0)
    gamma = 2.0;

  return gamma;
}
