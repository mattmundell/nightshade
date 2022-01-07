/*
 * Generational Conservative Garbage Collector for CMUCL x86.
 *
 * This code was written by Douglas T. Crosher, based on Public Domain
 * codes from Carnegie Mellon University. This code has been placed in
 * the public domain, and is provided 'as is'.
 *
 * Douglas Crosher, 1996, 1997, 1998, 1999.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/gencgc.c,v 1.26 2002/01/28 20:19:39 pmai Exp $
 *
 */

#include <stdio.h>
#include <signal.h>
#include "lisp.h"
#include "arch.h"
#include "internals.h"
#include "os.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "interr.h"
#include "gencgc.h"

// FIX  (FIX assumes #define LISPOBJ(thing) thing)
void *alien_nil = (void*) NIL;

// FIX
void* hack_ptr(void* obj) { return PTR((int)obj); }

// FIX
os_vm_prot_t os_vm_prot_read = OS_VM_PROT_READ;
os_vm_prot_t os_vm_prot_write = OS_VM_PROT_WRITE;
os_vm_prot_t os_vm_prot_execute = OS_VM_PROT_EXECUTE;
os_vm_prot_t os_vm_prot_all = OS_VM_PROT_ALL;

#define gc_abort() lose("GC invariant lost!  File \"%s\", line %d\n", \
			__FILE__, __LINE__)

#if 0
#define gc_assert(ex) do { \
	if (!(ex)) gc_abort(); \
} while (0)
#else
#define gc_assert(ex)
#endif


/*
 * The number of generations, an extra is added to this for use as a temp.
 */
#define NUM_GENERATIONS 6

/* Debugging variables. */

/*
 * The verbose level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1.
 */
unsigned gencgc_verbose = 0;
//unsigned gencgc_verbose = 2;

/*
 * To enable the use of page protection to help avoid the scavenging
 * of pages that don't have pointers to younger generations.
 */
#ifdef __NetBSD__
/* NetBSD on x86 has no way to retrieve the faulting address in the
 * SIGSEGV handler, so for the moment we can't use page protection. */
boolean  enable_page_protection = FALSE;
#else
boolean  enable_page_protection = TRUE;
#endif

/*
 * Hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set to NUM_GENERATIONS to disable.
 */
int verify_gens = NUM_GENERATIONS;

/*
 * Enable a pre-scan verify of generation 0 before it's GCed.
 */
boolean pre_verify_gen_0 = FALSE;

/*
 * Enable checking for bad pointers after gc_free_heap called from purify.
 */
boolean verify_after_free_heap = FALSE;

/*
 * Enable the printing of a note when code objects are found in the
 * dynamic space during a heap verify.
 */
boolean verify_dynamic_code_check = FALSE;

/*
 * Enable the checking of code objects for fixup errors after they are
 * transported.
 */
boolean check_code_fixups = FALSE;

/*
 * To enable unmapping of a page and re-mmaping it to have it zero filled.
 * Note: this can waste a lot of swap on FreeBSD and Open/NetBSD(?) so
 * don't unmap.
 */
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
boolean gencgc_unmap_zero = FALSE;
#else
boolean gencgc_unmap_zero = TRUE;
#endif

/*
 * Enable checking that newly allocated regions are zero filled.
 */
boolean gencgc_zero_check = FALSE;

boolean gencgc_enable_verify_zero_fill = FALSE;

/*
 * Enable checking that free pages are zero filled during gc_free_heap
 * called after purify.
 */
boolean gencgc_zero_check_during_free_heap = FALSE;

/*
 * The minimum size for a large object.
 */
unsigned large_object_size = 4 * PAGE_SIZE;

/*
 * Enable the filtering of stack/register pointers. This could reduce
 * the number of invalid pointers accepted. It will probably degrades
 * interrupt safety during object initialisation.
 */
boolean enable_pointer_filter = TRUE;


/*
 * The total bytes allocated. Seen by (dynamic-usage)
 */
unsigned long bytes_allocated = 0;

/*
 * GC trigger; a value of 0xffffffff represents disabled.
 */
unsigned long auto_gc_trigger = 0xffffffff;

/*
 * The src. and dest. generations. Set before a GC starts scavenging.
 */
int from_space;
//static int from_space;
int new_space;
//static int new_space;


/*
 * GC structures and variables.
 */

/*
 * Number of pages within the dynamic heap, setup from the size of the
 * dynamic space.
 */
unsigned dynamic_space_pages;

/*
 * An array of page structures is statically allocated.
 * This helps quickly map between an address its page structure.
 */
struct page *page_table;

/*
 * Heap base, needed for mapping addresses to page structures.
 */
//static void *heap_base = NULL;
void *heap_base = NULL;

/*
 * Calculate the start address for the given page number.
 */
//inline void *page_address(int page_num)
void *page_address(int page_num)
{
  return heap_base + PAGE_SIZE * page_num;
}

/*
 * Find the page index within the page_table for the given address.
 * Returns -1 on failure.
 */
inline int find_page_index(void *addr)
{
  int index = addr-heap_base;

  if (index >= 0) {
    index = (unsigned int) index / PAGE_SIZE;
    if (index < dynamic_space_pages)
      return index;
  }

  return -1;
}

/*
 * A structure to hold the state of a generation.
 */
struct generation {

  /* The first page that gc_alloc checks on its next call. */
  int  alloc_start_page;

  /* The first page that gc_alloc_unboxed checks on its next call. */
  int  alloc_unboxed_start_page;

  /*
   * The first page that gc_alloc_large (boxed) considers on its next call.
   * Although it always allocates after the boxed_region.
   */
  int  alloc_large_start_page;

  /*
   * The first page that gc_alloc_large (unboxed) considers on its next call.
   * Although it always allocates after the current_unboxed_region.
   */
  int  alloc_large_unboxed_start_page;

  /* The bytes allocate to this generation. */
  int  bytes_allocated;

  /* The number of bytes at which to trigger a GC */
  int  gc_trigger;

  /* To calculate a new level for gc_trigger */
  int  bytes_consed_between_gc;

  /* The number of GCs since the last raise. */
  int  num_gc;

  /*
   * The average age at after which a GC will raise objects to the
   * next generation.
   */
  int  trigger_age;

  /*
   * The cumulative sum of the bytes allocated to this generation. It
   * is cleared after a GC on this generations, and update before new
   * objects are added from a GC of a younger generation. Dividing by
   * the bytes_allocated will give the average age of the memory in
   * this generation since its last GC.
   */
  int  cum_sum_bytes_allocated;

  /*
   * A minimum average memory age before a GC will occur helps prevent
   * a GC when a large number of new live objects have been added, in
   * which case a GC could be a waste of time.
   */
  double  min_av_mem_age;
};

/*
 * An array of generation structures. There needs to be one more
 * generation structure than actual generations as the oldest
 * generations is temporarily raised then lowered.
 */
//static struct generation generations[NUM_GENERATIONS + 1];
struct generation generations[NUM_GENERATIONS + 1];

/*
 * The oldest generation that will currently be GCed by default.
 * Valid values are: 0, 1, ... (NUM_GENERATIONS - 1)
 *
 * The default of (NUM_GENERATIONS - 1) enables GC on all generations.
 *
 * Setting this to 0 effectively disables the generational nature of
 * the GC. In some applications generational GC may not be useful
 * because there are no long-lived objects.
 *
 * An intermediate value could be handy after moving long-lived data
 * into an older generation so an unnecessary GC of this long-lived
 * data can be avoided.
 */
unsigned int  gencgc_oldest_gen_to_gc = NUM_GENERATIONS - 1;


/*
 * The maximum free page in the heap is maintained and used to update
 * ALLOCATION_POINTER which is used by the room function to limit its
 * search of the heap. XX Gencgc obviously needs to be better
 * integrated with the lisp code.
 */
//static int  last_free_page;
int  last_free_page;


/*
 * Misc. heap functions.
 */

/*
 * Count the number of write protected pages within the given generation.
 */
static int count_write_protect_generation_pages(int generation)
{
  int i;
  int cnt = 0;
  int mmask, mflags;

  mmask = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK
    | PAGE_GENERATION_MASK;
  mflags = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK | generation;

  for (i = 0; i < last_free_page; i++)
    if (PAGE_FLAGS(i, mmask) == mflags)
      cnt++;
  return cnt;
}

/*
 * Count the number of pages within the given generation.
 */
static int count_generation_pages(int generation)
{
  int i;
  int cnt = 0;
  int mmask, mflags;

  mmask = PAGE_ALLOCATED_MASK | PAGE_GENERATION_MASK;
  mflags = PAGE_ALLOCATED_MASK | generation;

  for (i = 0; i < last_free_page; i++)
    if (PAGE_FLAGS(i, mmask) == mflags)
      cnt++;
  return cnt;
}

/*
 * Count the number of dont_move pages.
 */
static int count_dont_move_pages(void)
{
  int i;
  int cnt = 0;
  int mmask;

  mmask = PAGE_ALLOCATED_MASK | PAGE_DONT_MOVE_MASK;

  for (i = 0; i < last_free_page; i++)
    if (PAGE_FLAGS(i, mmask) == mmask)
      cnt++;
  return cnt;
}

/*
 * Work through the pages and add up the number of bytes used for the
 * given generation.
 */
static int generation_bytes_allocated (int generation)
{
  int i;
  int bytes_allocated = 0;
  int mmask, mflags;

  mmask = PAGE_ALLOCATED_MASK | PAGE_GENERATION_MASK;
  mflags = PAGE_ALLOCATED_MASK | generation;

  for (i = 0; i < last_free_page; i++) {
    if (PAGE_FLAGS(i, mmask) == mflags)
      bytes_allocated += page_table[i].bytes_used;
  }
  return bytes_allocated;
}

/*
 * Return the average age of the memory in a generation.
 */
static double gen_av_mem_age(int gen)
{
  if (generations[gen].bytes_allocated == 0)
    return 0.0;

  return (double) generations[gen].cum_sum_bytes_allocated /
		(double) generations[gen].bytes_allocated;
}

/*
 * The verbose argument controls how much to print out:
 * 0 for normal level of detail; 1 for debugging.
 */
static void print_generation_stats(int  verbose)
{
  int i, gens;
  int fpu_state[27];

  /*
   * This code uses the FP instructions which may be setup for Lisp so
   * they need to be saved and reset for C.
   */
  fpu_save(fpu_state);

  /* Number of generations to print out. */
  if (verbose)
    gens = NUM_GENERATIONS + 1;
  else
    gens = NUM_GENERATIONS;

  /* Print the heap stats */
  fprintf(stderr, "   Generation Boxed Unboxed LB   LUB    Alloc  Waste   Trig    WP  GCs Mem-age\n");

  for (i = 0; i < gens; i++) {
    int j;
    int boxed_cnt = 0;
    int unboxed_cnt = 0;
    int large_boxed_cnt = 0;
    int large_unboxed_cnt = 0;

    for (j = 0; j < last_free_page; j++) {
      int flags = page_table[j].flags;
      if ((flags & PAGE_GENERATION_MASK) == i) {
	if (flags & PAGE_ALLOCATED_MASK) {
	  /*
	   * Count the number of boxed and unboxed pages within the
	   * given generation.
	   */
	  if (flags & PAGE_UNBOXED_MASK)
	    if (flags & PAGE_LARGE_OBJECT_MASK)
	      large_unboxed_cnt++;
	    else
	      unboxed_cnt++;
	  else
	    if (flags & PAGE_LARGE_OBJECT_MASK)
	      large_boxed_cnt++;
	    else
	      boxed_cnt++;
	}
      }
    }

    gc_assert(generations[i].bytes_allocated == generation_bytes_allocated(i));
    fprintf(stderr, "   %8d: %5d %5d %5d %5d %8d %5d %8d %4d %3d %7.4f\n",
	    i, boxed_cnt, unboxed_cnt, large_boxed_cnt, large_unboxed_cnt,
	    generations[i].bytes_allocated,
	    PAGE_SIZE * count_generation_pages(i) -
	    generations[i].bytes_allocated,
	    generations[i].gc_trigger,
	    count_write_protect_generation_pages(i),
	    generations[i].num_gc,
	    gen_av_mem_age(i));
  }
  fprintf(stderr, "   Total bytes alloc=%d\n", bytes_allocated);

  fpu_restore(fpu_state);
}



/*
 * Allocation routines.
 *
 *
 * To support quick and inline allocation, regions of memory can be
 * allocated and then allocated from with just a free pointer and a
 * check against an end address.
 *
 * Since objects can be allocated to spaces with different properties
 * e.g. boxed/unboxed, generation, ages; there may need to be many
 * allocation regions.
 *
 * Each allocation region may be start within a partly used page.
 * Many features of memory use are noted on a page wise basis,
 * E.g. the generation; so if a region starts within an existing
 * allocated page it must be consistent with this page.
 *
 * During the scavenging of the newspace, objects will be transported
 * into an allocation region, and pointers updated to point to this
 * allocation region. It is possible that these pointers will be
 * scavenged again before the allocation region is closed, E.g. due to
 * trans_list which jumps all over the place to cleanup the list. It
 * is important to be able to determine properties of all objects
 * pointed to when scavenging, E.g to detect pointers to the
 * oldspace. Thus it's important that the allocation regions have the
 * correct properties set when allocated, and not just set when
 * closed.  The region allocation routines return regions with the
 * specified properties, and grab all the pages, setting there
 * properties appropriately, except that the amount used is not known.
 *
 * These regions are used to support quicker allocation using just a
 * free pointer. The actual space used by the region is not reflected
 * in the pages tables until it is closed. It can't be scavenged until
 * closed.
 *
 * When finished with the region it should be closed, which will
 * update the page tables for the actual space used returning unused
 * space. Further it may be noted in the new regions which is
 * necessary when scavenging the newspace.
 *
 * Large objects may be allocated directly without an allocation
 * region, the page tables are updated immediately.
 *
 * Unboxed objects don't contain points to other objects so don't need
 * scavenging. Further they can't contain pointers to younger
 * generations so WP is not needed.  By allocating pages to unboxed
 * objects the whole page never needs scavenging or write protecting.
 */

/*
 * Only using two regions at present, both are for the current
 * newspace generation.
 */
struct alloc_region  boxed_region;
struct alloc_region  unboxed_region;

/*
 * X hack. current lisp code uses the following. Need coping in/out.
 */
void *current_region_free_pointer;
void *current_region_end_addr;

/* The generation currently being allocated to. X */
//static int  gc_alloc_generation;
int  gc_alloc_generation;

/*
 * Find a new region with room for at least the given number of bytes.
 *
 * It starts looking at the current generations alloc_start_page. So
 * may pick up from the previous region if there is enough space. This
 * keeps the allocation contiguous when scavenging the newspace.
 *
 * The alloc_region should have been closed by a call to
 * gc_alloc_update_page_tables, and will thus be in an empty state.
 *
 * To assist the scavenging functions, write protected pages are not
 * used. Free pages should not be write protected.
 *
 * It is critical to the conservative GC that the start of regions be
 * known. To help achieve this only small regions are allocated at a
 * time.
 *
 * During scavenging, pointers may be found that point within the
 * current region and the page generation must be set so pointers to
 * the from space can be recognised.  So the generation of pages in
 * the region are set to gc_alloc_generation.  To prevent another
 * allocation call using the same pages, all the pages in the region
 * are allocated, although they will initially be empty.
 */
static void gc_alloc_new_region(int nbytes, int unboxed,
				struct alloc_region *alloc_region)
{
  int first_page;
  int last_page;
  int region_size;
  int restart_page;
  int bytes_found;
  int num_pages;
  int i;
  int mmask, mflags;

#if 0
  fprintf(stderr, "alloc_new_region for %d bytes from gen %d\n",
	  nbytes, gc_alloc_generation);
#endif

  /* Check that the region is in a reset state. */
  gc_assert(alloc_region->first_page == 0
	    && alloc_region->last_page == -1
	    && alloc_region->free_pointer == alloc_region->end_addr);

  if (unboxed)
    restart_page = generations[gc_alloc_generation].alloc_unboxed_start_page;
  else
    restart_page = generations[gc_alloc_generation].alloc_start_page;

  /*
   * Search for a contiguous free region of at least nbytes with the
   * given properties: boxed/unboxed, generation. First setting up the
   * mask and matching flags.
   */

  mmask = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK
    | PAGE_LARGE_OBJECT_MASK | PAGE_DONT_MOVE_MASK
    | PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK;
  mflags = PAGE_ALLOCATED_MASK | (unboxed << PAGE_UNBOXED_SHIFT)
    | gc_alloc_generation;

  do {
    first_page = restart_page;

    /*
     * First search for a page with at least 32 bytes free, that is
     * not write protected, or marked dont_move.
     */

    while (first_page < dynamic_space_pages) {
      int flags = page_table[first_page].flags;
      if (!(flags & PAGE_ALLOCATED_MASK)
	  || ((flags & mmask) == mflags &&
	      page_table[first_page].bytes_used < PAGE_SIZE - 32))
	break;
      first_page++;
    }

    /* Check for a failure */
    if (first_page >= dynamic_space_pages) {
      fprintf(stderr, "*A2 gc_alloc_new_region failed, nbytes=%d.\n", nbytes);
      print_generation_stats(1);
      exit(1);
    }

    gc_assert(!PAGE_WRITE_PROTECTED(first_page));

#if 0
    fprintf(stderr, "  first_page=%d bytes_used=%d\n",
	    first_page, page_table[first_page].bytes_used);
#endif

    /*
     * Now search forward to calculate the available region size.  It
     * tries to keeps going until nbytes are found and the number of
     * pages is greater than some level. This helps keep down the
     * number of pages in a region.
     */
    last_page = first_page;
    bytes_found = PAGE_SIZE - page_table[first_page].bytes_used;
    num_pages = 1;
    while ((bytes_found < nbytes || num_pages < 2)
	   && last_page < dynamic_space_pages - 1
	   && !PAGE_ALLOCATED(last_page + 1)) {
      last_page++;
      num_pages++;
      bytes_found += PAGE_SIZE;
      gc_assert(!PAGE_WRITE_PROTECTED(last_page));
    }

    region_size = (PAGE_SIZE - page_table[first_page].bytes_used)
      + PAGE_SIZE * (last_page - first_page);

    gc_assert(bytes_found == region_size);

#if 0
    fprintf(stderr, "  last_page=%d bytes_found=%d num_pages=%d\n",
	    last_page, bytes_found, num_pages);
#endif

    restart_page = last_page + 1;
  }
  while (restart_page < dynamic_space_pages && bytes_found < nbytes);

  /* Check for a failure */
  if (restart_page >= dynamic_space_pages && bytes_found < nbytes) {
    fprintf(stderr, "*A1 gc_alloc_new_region failed, nbytes=%d.\n", nbytes);
    print_generation_stats(1);
    exit(1);
  }

#if 0
  fprintf(stderr, "gc_alloc_new_region gen %d: %d bytes: from pages %d to %d: addr=%x\n",
	  gc_alloc_generation, bytes_found, first_page, last_page,
	  page_address(first_page));
#endif

  /* Setup the alloc_region. */
  alloc_region->first_page = first_page;
  alloc_region->last_page = last_page;
  alloc_region->start_addr = page_table[first_page].bytes_used
    + page_address(first_page);
  alloc_region->free_pointer = alloc_region->start_addr;
  alloc_region->end_addr = alloc_region->start_addr + bytes_found;

  if (gencgc_zero_check) {
    int *p;
    for(p = (int *)alloc_region->start_addr;
	p < (int *)alloc_region->end_addr; p++)
      if (*p != 0)
	fprintf(stderr, "** new region not zero @ %x\n",p);
  }

  /* Setup the pages. */

  /* The first page may have already been in use. */
  if (page_table[first_page].bytes_used == 0) {
    PAGE_FLAGS_UPDATE(first_page, mmask, mflags);
    page_table[first_page].first_object_offset = 0;
  }

  gc_assert(PAGE_ALLOCATED(first_page));
  gc_assert(PAGE_UNBOXED_VAL(first_page) == unboxed);
  gc_assert(PAGE_GENERATION(first_page) == gc_alloc_generation);
  gc_assert(!PAGE_LARGE_OBJECT(first_page));

  for (i = first_page + 1; i <= last_page; i++) {
    PAGE_FLAGS_UPDATE(i, PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK
		      | PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK,
		      PAGE_ALLOCATED_MASK | (unboxed << PAGE_UNBOXED_SHIFT)
		      | gc_alloc_generation);
    /*
     * This may not be necessary for unboxed regions (think it was
     * broken before!)
     */
    page_table[i].first_object_offset =
      alloc_region->start_addr - page_address(i);
  }

  /* Bump up the last_free_page */
  if (last_page + 1 > last_free_page) {
    last_free_page = last_page + 1;
    SetSymbolValue(ALLOCATION_POINTER,
		   (lispobj) ((char *) heap_base +
			      PAGE_SIZE * last_free_page));
  }
}



/*
 * If the record_new_objects flag is 2 then all new regions created
 * are recorded.
 *
 * If it's 1 then then it is only recorded if the first page of the
 * current region is <= new_areas_ignore_page. This helps avoid
 * unnecessary recording when doing full scavenge pass.
 *
 * The new_object structure holds the page, byte offset, and size of
 * new regions of objects. Each new area is placed in the array of
 * these structures pointed to by new_areas; new_areas_index holds the
 * offset into new_areas.
 *
 * If new_area overflows NUM_NEW_AREAS then it stops adding them. The
 * later code must detect this an handle it, probably by doing a full
 * scavenge of a generation.
 */

#define NUM_NEW_AREAS 512
//static int record_new_objects = 0;
int record_new_objects = 0;
//static int new_areas_ignore_page;
int new_areas_ignore_page;
struct new_area {
  int  page;
  int  offset;
  int  size;
};
//static struct new_area (*new_areas)[];
struct new_area (*new_areas)[];
//static int new_areas_index;
int new_areas_index;
int max_new_areas;

/* Add a new area to new_areas. */
//static void add_new_area(int first_page, int offset, int size)
void add_new_area(int first_page, int offset, int size)
{
  unsigned new_area_start,c;
  int i;

  /* Ignore if full */
  if (new_areas_index >= NUM_NEW_AREAS)
    return;

  switch (record_new_objects) {
  case 0:
    return;
  case 1:
    if (first_page > new_areas_ignore_page)
      return;
    break;
  case 2:
    break;
  default:
    gc_abort();
  }

  new_area_start = PAGE_SIZE * first_page + offset;

  /*
   * Search backwards for a prior area that this follows from.  If
   * found this will save adding a new area.
   */
  for (i = new_areas_index - 1, c = 0; i >= 0 && c < 8; i--, c++) {
    unsigned area_end = PAGE_SIZE * (*new_areas)[i].page
      + (*new_areas)[i].offset + (*new_areas)[i].size; 
#if 0
    fprintf(stderr, "*S1 %d %d %d %d\n", i, c, new_area_start, area_end);
#endif
    if (new_area_start == area_end) {
#if 0
      fprintf(stderr, "-> Adding to [%d] %d %d %d with %d %d %d:\n",
	      i, (*new_areas)[i].page, (*new_areas)[i].offset ,
	      (*new_areas)[i].size, first_page, offset, size);
#endif
      (*new_areas)[i].size += size;
      return;
    }
  }
#if 0
  fprintf(stderr, "*S1 %d %d %d\n",i,c,new_area_start);
#endif

  (*new_areas)[new_areas_index].page = first_page;
  (*new_areas)[new_areas_index].offset = offset;
  (*new_areas)[new_areas_index].size = size;
#if 0
  fprintf(stderr, "  new_area %d page %d offset %d size %d\n",
	  new_areas_index, first_page, offset, size);
#endif
  new_areas_index++;

  /* Note the max new_areas used. */
  if (new_areas_index > max_new_areas)
    max_new_areas = new_areas_index;
}


/*
 * Update the tables for the alloc_region. The region may be added to
 * the new_areas.
 *
 * When done the alloc_region is setup so that the next quick alloc
 * will fail safely and thus a new region will be allocated. Further
 * it is safe to try and re-update the page table of this reset
 * alloc_region.
 */
void gc_alloc_update_page_tables(int unboxed,
				 struct alloc_region *alloc_region)
{
  int more;
  int first_page;
  int next_page;
  int bytes_used;
  int orig_first_page_bytes_used;
  int region_size;
  int byte_cnt;

  // FIX
  //fprintf(stderr, "alloc_region: %x\n", alloc_region);

#if 0
  fprintf(stderr, "gc_alloc_update_page_tables to gen %d: ",
	  gc_alloc_generation);
#endif

  first_page = alloc_region->first_page;

  /* Catch an unused alloc_region. */
  if (first_page == 0 && alloc_region->last_page == -1)
    return;

  next_page = first_page + 1;

  /* Skip if no bytes were allocated */
  if (alloc_region->free_pointer != alloc_region->start_addr) {
    orig_first_page_bytes_used = page_table[first_page].bytes_used;

    gc_assert(alloc_region->start_addr == page_address(first_page) +
	      page_table[first_page].bytes_used);

    /* All the pages used need to be updated */

    /* Update the first page. */

#if 0    
    fprintf(stderr, "0");
#endif

    /* If the page was free then setup the gen, and first_object_offset. */
    if (page_table[first_page].bytes_used == 0)
      gc_assert(page_table[first_page].first_object_offset == 0);

    gc_assert(PAGE_ALLOCATED(first_page));
    gc_assert(PAGE_UNBOXED_VAL(first_page) == unboxed);
    gc_assert(PAGE_GENERATION(first_page) == gc_alloc_generation);
    gc_assert(!PAGE_LARGE_OBJECT(first_page));

    byte_cnt = 0;

    /*
     * Calc. the number of bytes used in this page. This is not always
     * the number of new bytes, unless it was free.
     */
    more = 0;
    bytes_used = alloc_region->free_pointer - page_address(first_page);
    if (bytes_used > PAGE_SIZE) {
      bytes_used = PAGE_SIZE;
      more = 1;
    }
    page_table[first_page].bytes_used = bytes_used;
    byte_cnt += bytes_used;

    /*
     * All the rest of the pages should be free. Need to set their
     * first_object_offset pointer to the start of the region, and set
     * the bytes_used.
     */
    while (more) {
#if 0
      fprintf(stderr, "+")
#endif
      gc_assert(PAGE_ALLOCATED(next_page));
      gc_assert(PAGE_UNBOXED_VAL(next_page) == unboxed);
      gc_assert(page_table[next_page].bytes_used == 0);
      gc_assert(PAGE_GENERATION(next_page) == gc_alloc_generation);
      gc_assert(!PAGE_LARGE_OBJECT(next_page));

      gc_assert(page_table[next_page].first_object_offset ==
		alloc_region->start_addr - page_address(next_page));

      /* Calc. the number of bytes used in this page. */
      more = 0;
      bytes_used = alloc_region->free_pointer - page_address(next_page);
      if (bytes_used > PAGE_SIZE) {
	bytes_used = PAGE_SIZE;
	more = 1;
      }
      page_table[next_page].bytes_used = bytes_used;
      byte_cnt += bytes_used;

      next_page++;
    }

    region_size = alloc_region->free_pointer - alloc_region->start_addr;
    bytes_allocated += region_size;
    generations[gc_alloc_generation].bytes_allocated += region_size;

    gc_assert(byte_cnt - orig_first_page_bytes_used == region_size);

    /*
     * Set the generations alloc restart page to the last page of
     * the region.
     */
    if (unboxed)
      generations[gc_alloc_generation].alloc_unboxed_start_page = next_page-1;
    else
      generations[gc_alloc_generation].alloc_start_page = next_page - 1;

    /* Add the region to the new_areas if requested. */
    if (!unboxed)
      add_new_area(first_page, orig_first_page_bytes_used, region_size);

#if 0
    fprintf(stderr, "  gc_alloc_update_page_tables update %d bytes to gen %d\n",
	    region_size, gc_alloc_generation);
#endif
  }
  else
    /*
     * No bytes allocated. Unallocate the first_page if there are 0 bytes_used.
     */
    if (page_table[first_page].bytes_used == 0)
      page_table[first_page].flags &= ~PAGE_ALLOCATED_MASK;

  /* Unallocate any unused pages. */
  while (next_page <= alloc_region->last_page) {
    gc_assert(page_table[next_page].bytes_used == 0);
    page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
    next_page++;
  }

  /* Reset the alloc_region. */
  alloc_region->first_page = 0;
  alloc_region->last_page = -1;
  alloc_region->start_addr = page_address(0);
  alloc_region->free_pointer = page_address(0);
  alloc_region->end_addr = page_address(0);

#if 0
  fprintf(stderr, "\n");
#endif
}



static inline void *gc_quick_alloc(int nbytes);

/*
 * Allocate a possibly large object.
 */
static void *gc_alloc_large(int  nbytes, int unboxed,
			    struct alloc_region *alloc_region)
{
  int first_page;
  int last_page;
  int region_size;
  int restart_page;
  int bytes_found;
  int num_pages;
  int orig_first_page_bytes_used;
  int byte_cnt;
  int more;
  int bytes_used;
  int next_page;
  int large = (nbytes >= large_object_size);
  int mmask, mflags;

#if 0
  if (nbytes > 200000)
    fprintf(stderr, "*** alloc_large %d\n", nbytes);
#endif

#if 0
  fprintf(stderr, "gc_alloc_large for %d bytes from gen %d\n",
	  nbytes, gc_alloc_generation);
#endif

  /*
   * If the object is small, and there is room in the current region
   * then allocation it in the current region.
   */
  if (!large && alloc_region->end_addr - alloc_region->free_pointer >= nbytes)
    return gc_quick_alloc(nbytes);

  /*
   * Search for a contiguous free region of at least nbytes. If it's a
   * large object then align it on a page boundary by searching for a
   * free page.
   */

  /*
   * To allow the allocation of small objects without the danger of
   * using a page in the current boxed region, the search starts after
   * the current boxed free region. XX could probably keep a page
   * index ahead of the current region and bumped up here to save a
   * lot of re-scanning.
   */
  if (unboxed)
    restart_page = generations[gc_alloc_generation].alloc_large_unboxed_start_page;
  else
    restart_page = generations[gc_alloc_generation].alloc_large_start_page;
  if (restart_page <= alloc_region->last_page)
    restart_page = alloc_region->last_page + 1;

  /* Setup the mask and matching flags. */

  mmask = PAGE_ALLOCATED_MASK | PAGE_WRITE_PROTECTED_MASK
    | PAGE_LARGE_OBJECT_MASK | PAGE_DONT_MOVE_MASK
    | PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK;
  mflags = PAGE_ALLOCATED_MASK | (unboxed << PAGE_UNBOXED_SHIFT)
    | gc_alloc_generation;

  do {
    first_page = restart_page;

    if (large)
      while (first_page < dynamic_space_pages && PAGE_ALLOCATED(first_page))
	first_page++;
    else
      while (first_page < dynamic_space_pages) {
	int flags = page_table[first_page].flags;
	if (!(flags & PAGE_ALLOCATED_MASK)
	    || ((flags & mmask) == mflags &&
		page_table[first_page].bytes_used < PAGE_SIZE - 32))
	  break;
	first_page++;
      }

    /* Check for a failure */
    if (first_page >= dynamic_space_pages) {
      fprintf(stderr, "*A2 gc_alloc_large failed, nbytes=%d.\n", nbytes);
      print_generation_stats(1);
      exit(1);
    }

    gc_assert(!PAGE_WRITE_PROTECTED(first_page));

#if 0
    fprintf(stderr, "  first_page=%d bytes_used=%d\n",
	    first_page, page_table[first_page].bytes_used);
#endif

    last_page = first_page;
    bytes_found = PAGE_SIZE - page_table[first_page].bytes_used;
    num_pages = 1;
    while (bytes_found < nbytes
	   && last_page < dynamic_space_pages - 1
	   && !PAGE_ALLOCATED(last_page + 1)) {
      last_page++;
      num_pages++;
      bytes_found += PAGE_SIZE;
      gc_assert(!PAGE_WRITE_PROTECTED(last_page));
    }

    region_size = (PAGE_SIZE - page_table[first_page].bytes_used)
      + PAGE_SIZE * (last_page - first_page);

    gc_assert(bytes_found == region_size);

#if 0    
    fprintf(stderr, "  last_page=%d bytes_found=%d num_pages=%d\n",
	    last_page, bytes_found, num_pages);
#endif

    restart_page = last_page + 1;
  }
  while ((restart_page < dynamic_space_pages) && (bytes_found < nbytes));

  /* Check for a failure */
  if (restart_page >= dynamic_space_pages && bytes_found < nbytes) {
    fprintf(stderr, "*A1 gc_alloc_large failed, nbytes=%d.\n", nbytes);
    print_generation_stats(1);
    exit(1);
  }

#if 0  
  if (large)
    fprintf(stderr, "gc_alloc_large gen %d: %d of %d bytes: from pages %d to %d: addr=%x\n",
	    gc_alloc_generation, nbytes, bytes_found,
	    first_page, last_page, page_address(first_page));
#endif

  gc_assert(first_page > alloc_region->last_page);
  if (unboxed)
    generations[gc_alloc_generation].alloc_large_unboxed_start_page =
      last_page;
  else
    generations[gc_alloc_generation].alloc_large_start_page = last_page;

  /* Setup the pages. */
  orig_first_page_bytes_used = page_table[first_page].bytes_used;

  /*
   * If the first page was free then setup the gen, and
   * first_object_offset.
   */

  if (large)
    mflags |= PAGE_LARGE_OBJECT_MASK;
  if (page_table[first_page].bytes_used == 0) {
    PAGE_FLAGS_UPDATE(first_page, mmask, mflags);
    page_table[first_page].first_object_offset = 0;
  }

  gc_assert(PAGE_ALLOCATED(first_page));
  gc_assert(PAGE_UNBOXED_VAL(first_page) == unboxed);
  gc_assert(PAGE_GENERATION(first_page) == gc_alloc_generation);
  gc_assert(PAGE_LARGE_OBJECT_VAL(first_page) == large);

  byte_cnt = 0;

  /*
   * Calc. the number of bytes used in this page. This is not
   * always the number of new bytes, unless it was free.
   */
  more = 0;
  bytes_used = nbytes + orig_first_page_bytes_used;
  if (bytes_used > PAGE_SIZE) {
    bytes_used = PAGE_SIZE;
    more = 1;
  }
  page_table[first_page].bytes_used = bytes_used;
  byte_cnt += bytes_used;

  next_page = first_page + 1;

  /*
   * All the rest of the pages should be free. Need to set their
   * first_object_offset pointer to the start of the region, and set
   * the bytes_used.
   */
  while (more) {
#if 0
    fprintf(stderr, "+");
#endif

    gc_assert(!PAGE_ALLOCATED(next_page));
    gc_assert(page_table[next_page].bytes_used == 0);
    PAGE_FLAGS_UPDATE(next_page, mmask, mflags);

    page_table[next_page].first_object_offset =
      orig_first_page_bytes_used - PAGE_SIZE * (next_page - first_page);

    /* Calc. the number of bytes used in this page. */
    more = 0;
    bytes_used = nbytes + orig_first_page_bytes_used - byte_cnt;
    if (bytes_used > PAGE_SIZE) {
      bytes_used = PAGE_SIZE;
      more = 1;
    }
    page_table[next_page].bytes_used = bytes_used;
    byte_cnt += bytes_used;

    next_page++;
  }

  gc_assert(byte_cnt - orig_first_page_bytes_used == nbytes);

  bytes_allocated += nbytes;
  generations[gc_alloc_generation].bytes_allocated += nbytes;

  /* Add the region to the new_areas if requested. */
  if (!unboxed)
    add_new_area(first_page, orig_first_page_bytes_used, nbytes);

  /* Bump up the last_free_page */
  if (last_page + 1 > last_free_page) {
    last_free_page = last_page + 1;
    SetSymbolValue(ALLOCATION_POINTER,
		   (lispobj) ((char *) heap_base +
			      PAGE_SIZE * last_free_page));
  }

  return (void *) (page_address(first_page) + orig_first_page_bytes_used);
}

/*
 * Allocate bytes from the boxed_region. It first checks if there is
 * room, if not then it calls gc_alloc_new_region to find a new region
 * with enough space. A pointer to the start of the region is returned.
 */
static void *gc_alloc(int nbytes)
{
  void *new_free_pointer;

#if 0
  fprintf(stderr, "gc_alloc %d\n",nbytes);
#endif

  /* Check if there is room in the current alloc region. */
  new_free_pointer = boxed_region.free_pointer + nbytes;

  if (new_free_pointer <= boxed_region.end_addr) {
    /* If so then allocate from the current alloc region. */
    void *new_obj = boxed_region.free_pointer;
    boxed_region.free_pointer = new_free_pointer;

    /* Check if the alloc region is almost empty. */
    if (boxed_region.end_addr - boxed_region.free_pointer <= 32) {
      /* If so finished with the current region. */
      gc_alloc_update_page_tables(0, &boxed_region);
      /* Setup a new region. */
      gc_alloc_new_region(32, 0, &boxed_region);
    }
    return (void *) new_obj;
  }

  /* Else not enough free space in the current region. */

  /*
   * If there is a bit of room left in the current region then
   * allocate a large object.
   */
  if (boxed_region.end_addr - boxed_region.free_pointer > 32)
    return gc_alloc_large(nbytes, 0, &boxed_region);

  /* Else find a new region. */

  /* Finished with the current region. */
  gc_alloc_update_page_tables(0, &boxed_region);

  /* Setup a new region. */
  gc_alloc_new_region(nbytes, 0, &boxed_region);

  /* Should now be enough room. */

  /* Check if there is room in the current region. */
  new_free_pointer = boxed_region.free_pointer + nbytes;

  if (new_free_pointer <= boxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void *new_obj = boxed_region.free_pointer;
    boxed_region.free_pointer = new_free_pointer;

    /* Check if the current region is almost empty. */
    if (boxed_region.end_addr - boxed_region.free_pointer <= 32) {
      /* If so find, finished with the current region. */
      gc_alloc_update_page_tables(0, &boxed_region);

      /* Setup a new region. */
      gc_alloc_new_region(32, 0, &boxed_region);
    }

    return (void *) new_obj;
  }

  /* Shouldn't happen? */
  gc_assert(0);
}

/*
 * Allocate space from the boxed_region. If there is not enough free
 * space then call gc_alloc to do the job. A pointer to the start of
 * the region is returned.
 */
static inline void *gc_quick_alloc(int nbytes)
{
  void *new_free_pointer;

  /* Check if there is room in the current region. */
  new_free_pointer = boxed_region.free_pointer + nbytes;

  if (new_free_pointer <= boxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void  *new_obj = boxed_region.free_pointer;
    boxed_region.free_pointer = new_free_pointer;
    return (void *) new_obj;
  }

  /* Else call gc_alloc */
  return gc_alloc(nbytes);
}

/*
 * Allocate space for the boxed object. If it is a large object then
 * do a large alloc else allocate from the current region. If there is
 * not enough free space then call gc_alloc to do the job. A pointer
 * to the start of the region is returned.
 */
static inline void *gc_quick_alloc_large(int nbytes)
{
  void *new_free_pointer;

  if (nbytes >= large_object_size)
    return gc_alloc_large(nbytes,0,&boxed_region);

  /* Check if there is room in the current region. */
  new_free_pointer = boxed_region.free_pointer + nbytes;

  if (new_free_pointer <= boxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void *new_obj = boxed_region.free_pointer;
    boxed_region.free_pointer = new_free_pointer;
    return (void *) new_obj;
  }

  /* Else call gc_alloc */
  return gc_alloc(nbytes);
}




static void *gc_alloc_unboxed(int nbytes)
{
  void *new_free_pointer;

#if 0
  fprintf(stderr, "gc_alloc_unboxed %d\n",nbytes);
#endif

  /* Check if there is room in the current region. */
  new_free_pointer = unboxed_region.free_pointer + nbytes;

  if (new_free_pointer <= unboxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void *new_obj = unboxed_region.free_pointer;
    unboxed_region.free_pointer = new_free_pointer;

    /* Check if the current region is almost empty. */
    if ((unboxed_region.end_addr - unboxed_region.free_pointer) <= 32) {
      /* If so finished with the current region. */
      gc_alloc_update_page_tables(1, &unboxed_region);

      /* Setup a new region. */
      gc_alloc_new_region(32, 1, &unboxed_region);
    }

    return (void *) new_obj;
  }

  /* Else not enough free space in the current region. */

  /*
   * If there is a bit of room left in the current region then
   * allocate a large object.
   */
  if (unboxed_region.end_addr - unboxed_region.free_pointer > 32)
    return gc_alloc_large(nbytes, 1, &unboxed_region);

  /* Else find a new region. */

  /* Finished with the current region. */
  gc_alloc_update_page_tables(1,&unboxed_region);

  /* Setup a new region. */
  gc_alloc_new_region(nbytes,1,&unboxed_region);

  /* Should now be enough room. */

  /* Check if there is room in the current region. */
  new_free_pointer = unboxed_region.free_pointer + nbytes;

  if (new_free_pointer <= unboxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void *new_obj = unboxed_region.free_pointer;
    unboxed_region.free_pointer = new_free_pointer;

    /* Check if the current region is almost empty. */
    if ((unboxed_region.end_addr - unboxed_region.free_pointer) <= 32) {
      /* If so find, finished with the current region. */
      gc_alloc_update_page_tables(1, &unboxed_region);

      /* Setup a new region. */
      gc_alloc_new_region(32, 1, &unboxed_region);
    }

    return (void *) new_obj;
  }

  /* Shouldn't happen? */
  gc_assert(0);
}

static inline void *gc_quick_alloc_unboxed(int nbytes)
{
  void *new_free_pointer;

  /* Check if there is room in the current region. */
  new_free_pointer = unboxed_region.free_pointer + nbytes;

  if (new_free_pointer <= unboxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void *new_obj = unboxed_region.free_pointer;
    unboxed_region.free_pointer = new_free_pointer;

    return (void *) new_obj;
    }

  /* Else call gc_alloc */
  return gc_alloc_unboxed(nbytes);
}

/*
 * Allocate space for the object. If it is a large object then do a
 * large alloc else allocate from the current region. If there is not
 * enough free space then call gc_alloc to do the job.
 *
 * A pointer to the start of the region is returned.
 */
static inline void *gc_quick_alloc_large_unboxed(int nbytes)
{
  void *new_free_pointer;

  if (nbytes >= large_object_size)
    return gc_alloc_large(nbytes,1,&unboxed_region);

  /* Check if there is room in the current region. */
  new_free_pointer = unboxed_region.free_pointer + nbytes;

  if (new_free_pointer <= unboxed_region.end_addr) {
    /* If so then allocate from the current region. */
    void *new_obj = unboxed_region.free_pointer;
    unboxed_region.free_pointer = new_free_pointer;

    return (void *) new_obj;
  }

  /* Else call gc_alloc */
  return gc_alloc_unboxed(nbytes);
}

/***************************************************************************/


/* Scavenging/transporting routines derived from gc.c */

static int (*scavtab[256])(lispobj *where, lispobj object);
static lispobj (*transother[256])(lispobj object);
int (*sizetab[256])(lispobj *where);
//static int (*sizetab[256])(lispobj *where);

struct weak_pointer *weak_pointers;
//static struct weak_pointer *weak_pointers;
struct scavenger_hook *scavenger_hooks = (struct scavenger_hook *) NIL;
//static struct scavenger_hook *scavenger_hooks = (struct scavenger_hook *) NIL;

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))


/* Predicates */

static inline boolean from_space_p(lispobj obj)
{
  int page_index = (void*) obj - heap_base;
  return page_index >= 0
    && (page_index = (unsigned int) page_index / PAGE_SIZE) < dynamic_space_pages
    && PAGE_GENERATION(page_index) == from_space;
}

static inline boolean new_space_p(lispobj obj)
{
  int page_index = (void*) obj - heap_base;
  return page_index >= 0
    && (page_index = (unsigned int) page_index / PAGE_SIZE) < dynamic_space_pages
    && PAGE_GENERATION(page_index) == new_space;
}


/* Copying Objects */


/* Copying Boxed Objects */
static inline lispobj copy_object(lispobj object, int nwords)
{
  int tag;
  lispobj *new;
  lispobj *source, *dest;

  gc_assert(Pointerp(object));
  gc_assert(from_space_p(object));
  gc_assert((nwords & 0x01) == 0);

  /* get tag of object */
  tag = LowtagOf(object);

  /* allocate space */
  new = gc_quick_alloc(nwords*4);

  dest = new;
  source = (lispobj *) PTR(object);

  /* copy the object */
  while (nwords > 0) {
    dest[0] = source[0];
    dest[1] = source[1];
    dest += 2;
    source += 2;
    nwords -= 2;
  }

  /* return lisp pointer of new object */
  return (lispobj) new | tag;
}

/*
 * Copying Large Boxed Objects. If the object is in a large object
 * region then it is simply promoted, else it is copied. If it's large
 * enough then it's copied to a large object region.
 *
 * Vectors may have shrunk. If the object is not copied the space
 * needs to be reclaimed, and the page_tables corrected.
 */
static lispobj copy_large_object(lispobj object, int nwords)
{
  int tag;
  lispobj *new;
  lispobj *source, *dest;
  int first_page;

  gc_assert(Pointerp(object));
  gc_assert(from_space_p(object));
  gc_assert((nwords & 0x01) == 0);

  if (gencgc_verbose && nwords > 1024 * 1024)
    fprintf(stderr, "** copy_large_object: %d\n", nwords * 4);

  /* Check if it's a large object. */
  first_page = find_page_index((void *) object);
  gc_assert(first_page >= 0);

  if (PAGE_LARGE_OBJECT(first_page)) {
    /* Promote the object. */
    int remaining_bytes;
    int next_page;
    int bytes_freed;
    int old_bytes_used;
    int mmask, mflags;

    /*
     * Note: Any page write protection must be removed, else a later
     * scavenge_newspace may incorrectly not scavenge these pages.
     * This would not be necessary if they are added to the new areas,
     * but lets do it for them all (they'll probably be written
     * anyway?).
     */

    gc_assert(page_table[first_page].first_object_offset == 0);

    next_page = first_page;
    remaining_bytes = nwords * 4;
    while (remaining_bytes > PAGE_SIZE) {
      gc_assert(PAGE_GENERATION(next_page) == from_space);
      gc_assert(PAGE_ALLOCATED(next_page));
      gc_assert(!PAGE_UNBOXED(next_page));
      gc_assert(PAGE_LARGE_OBJECT(next_page));
      gc_assert(page_table[next_page].first_object_offset ==
		PAGE_SIZE * (first_page - next_page));
      gc_assert(page_table[next_page].bytes_used == PAGE_SIZE);

      PAGE_FLAGS_UPDATE(next_page, PAGE_GENERATION_MASK, new_space);

      /*
       * Remove any write protection.  Should be able to religh on the
       * WP flag to avoid redundant calls.
       */
      if (PAGE_WRITE_PROTECTED(next_page)) {
	os_protect(page_address(next_page), PAGE_SIZE, OS_VM_PROT_ALL);
	page_table[next_page].flags &= ~PAGE_WRITE_PROTECTED_MASK;
      }
      remaining_bytes -= PAGE_SIZE;
      next_page++;
    }

    /*
     * Now only one page remains, but the object may have shrunk so
     * there may be more unused pages which will be freed.
     */

    /* Object may have shrunk but shouldn't have grown - check. */
    gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

    PAGE_FLAGS_UPDATE(next_page, PAGE_GENERATION_MASK, new_space);
    gc_assert(PAGE_ALLOCATED(next_page));
    gc_assert(!PAGE_UNBOXED(next_page));

    /* Adjust the bytes_used. */
    old_bytes_used = page_table[next_page].bytes_used;
    page_table[next_page].bytes_used = remaining_bytes;

    bytes_freed = old_bytes_used - remaining_bytes;

    mmask = PAGE_ALLOCATED_MASK | PAGE_UNBOXED_MASK | PAGE_LARGE_OBJECT_MASK
      | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | from_space;

    /* Free any remaining pages; needs care. */
    next_page++;
    while (old_bytes_used == PAGE_SIZE &&
	   PAGE_FLAGS(next_page, mmask) == mflags &&
	   page_table[next_page].first_object_offset == PAGE_SIZE * (first_page
								     - next_page)) {
      /*
       * Checks out OK, free the page. Don't need to both zeroing
       * pages as this should have been done before shrinking the
       * object. These pages shouldn't be write protected as they
       * should be zero filled.
       */
      gc_assert(!PAGE_WRITE_PROTECTED(next_page));

      old_bytes_used = page_table[next_page].bytes_used;
      page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
      page_table[next_page].bytes_used = 0;
      bytes_freed += old_bytes_used;
      next_page++;
    }

    if (gencgc_verbose && bytes_freed > 0)
      fprintf(stderr, "* copy_large_boxed bytes_freed %d\n", bytes_freed);

    generations[from_space].bytes_allocated -= 4 * nwords + bytes_freed;
    generations[new_space].bytes_allocated += 4 * nwords;
    bytes_allocated -= bytes_freed;

    /* Add the region to the new_areas if requested. */
    add_new_area(first_page, 0, nwords * 4);

    return object;
  }
  else {
    /* get tag of object */
    tag = LowtagOf(object);

    /* allocate space */
    new = gc_quick_alloc_large(nwords * 4);

    dest = new;
    source = (lispobj *) PTR(object);

    /* copy the object */
    while (nwords > 0) {
      dest[0] = source[0];
      dest[1] = source[1];
      dest += 2;
      source += 2;
      nwords -= 2;
    }

    /* return lisp pointer of new object */
    return (lispobj) new | tag;
  }
}

/* Copying UnBoxed Objects. */
static inline lispobj copy_unboxed_object(lispobj object, int nwords)
{
  int tag;
  lispobj *new;
  lispobj *source, *dest;

  gc_assert(Pointerp(object));
  gc_assert(from_space_p(object));
  gc_assert((nwords & 0x01) == 0);

  /* get tag of object */
  tag = LowtagOf(object);

  /* allocate space */
  new = gc_quick_alloc_unboxed(nwords*4);

  dest = new;
  source = (lispobj *) PTR(object);

  /* Copy the object */
  while (nwords > 0) {
    dest[0] = source[0];
    dest[1] = source[1];
    dest += 2;
    source += 2;
    nwords -= 2;
  }

  /* Return lisp pointer of new object. */
  return (lispobj) new | tag;
}


/*
 * Copying Large Unboxed Objects. If the object is in a large object
 * region then it is simply promoted, else it is copied. If it's large
 * enough then it's copied to a large object region.
 *
 * Bignums and vectors may have shrunk. If the object is not copied
 * the space needs to be reclaimed, and the page_tables corrected.
 */
static lispobj copy_large_unboxed_object(lispobj object, int nwords)
{
  int tag;
  lispobj *new;
  lispobj *source, *dest;
  int first_page;

  gc_assert(Pointerp(object));
  gc_assert(from_space_p(object));
  gc_assert((nwords & 0x01) == 0);

  if (gencgc_verbose && nwords > 1024 * 1024)
    fprintf(stderr, "** copy_large_unboxed_object: %d\n", nwords * 4);

  /* Check if it's a large object. */
  first_page = find_page_index((void *) object);
  gc_assert(first_page >= 0);

  if (PAGE_LARGE_OBJECT(first_page)) {
    /*
     * Promote the object. Note: Unboxed objects may have been
     * allocated to a BOXED region so it may be necessary to change
     * the region to UNBOXED.
     */
    int remaining_bytes;
    int next_page;
    int bytes_freed;
    int old_bytes_used;
    int mmask, mflags;

    gc_assert(page_table[first_page].first_object_offset == 0);

    next_page = first_page;
    remaining_bytes = nwords * 4;
    while (remaining_bytes > PAGE_SIZE) {
      gc_assert(PAGE_GENERATION(next_page) == from_space);
      gc_assert(PAGE_ALLOCATED(next_page));
      gc_assert(PAGE_LARGE_OBJECT(next_page));
      gc_assert(page_table[next_page].first_object_offset ==
		PAGE_SIZE * (first_page - next_page));
      gc_assert(page_table[next_page].bytes_used == PAGE_SIZE);

      PAGE_FLAGS_UPDATE(next_page, PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK,
			PAGE_UNBOXED_MASK | new_space);
      remaining_bytes -= PAGE_SIZE;
      next_page++;
    }

    /*
     * Now only one page remains, but the object may have shrunk so
     * there may be more unused pages which will be freed.
     */

    /* Object may have shrunk but shouldn't have grown - check. */
    gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

    PAGE_FLAGS_UPDATE(next_page, PAGE_ALLOCATED_MASK | PAGE_UNBOXED_MASK
		      | PAGE_GENERATION_MASK,
		      PAGE_ALLOCATED_MASK | PAGE_UNBOXED_MASK | new_space);

    /* Adjust the bytes_used. */
    old_bytes_used = page_table[next_page].bytes_used;
    page_table[next_page].bytes_used = remaining_bytes;

    bytes_freed = old_bytes_used - remaining_bytes;

    mmask = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK
      | PAGE_GENERATION_MASK;
    mflags = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | from_space;

    /* Free any remaining pages; needs care. */
    next_page++;
    while (old_bytes_used == PAGE_SIZE &&
	   PAGE_FLAGS(next_page, mmask) == mflags &&
	   page_table[next_page].first_object_offset == PAGE_SIZE * (first_page
								     - next_page)) {
      /*
       * Checks out OK, free the page. Don't need to both zeroing
       * pages as this should have been done before shrinking the
       * object. These pages shouldn't be write protected, even if
       * boxed they should be zero filled.
       */
      gc_assert(!PAGE_WRITE_PROTECTED(next_page));

      old_bytes_used = page_table[next_page].bytes_used;
      page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
      page_table[next_page].bytes_used = 0;
      bytes_freed += old_bytes_used;
      next_page++;
    }

    if (gencgc_verbose && bytes_freed > 0)
      fprintf(stderr, "* copy_large_unboxed bytes_freed %d\n", bytes_freed);

    generations[from_space].bytes_allocated -= 4 * nwords + bytes_freed;
    generations[new_space].bytes_allocated += 4 * nwords;
    bytes_allocated -= bytes_freed;

    return object;
  }
  else {
    /* get tag of object */
    tag = LowtagOf(object);

    /* allocate space */
    new = gc_quick_alloc_large_unboxed(nwords * 4);

    dest = new;
    source = (lispobj *) PTR(object);

    /* copy the object */
    while (nwords > 0) {
      dest[0] = source[0];
      dest[1] = source[1];
      dest += 2;
      source += 2;
      nwords -= 2;
    }

    /* return lisp pointer of new object */
    return (lispobj) new | tag;
  }
}


/* Scavenging */

#define DIRECT_SCAV 0

static void scavenge(lispobj *start, long nwords)
{
  while (nwords > 0) {
    lispobj object;
    int words_scavenged;

    object = *start;

    gc_assert(object != 0x01); /* Not a forwarding pointer. */

#if DIRECT_SCAV
    words_scavenged = (scavtab[TypeOf(object)])(start, object);
#else
    if (Pointerp(object))
      /* It be a pointer. */
      if (from_space_p(object)) {
	/*
	 * It currently points to old space.  Check for a forwarding
	 * pointer.
	 */
	lispobj *ptr = (lispobj *) PTR(object);
	lispobj first_word = *ptr;

	if(first_word == 0x01) {
	  /* Yep, there be a forwarding pointer. */
	  *start = ptr[1];
	  words_scavenged = 1;
	}
	else
	  /* Scavenge that pointer. */
	  words_scavenged = (scavtab[TypeOf(object)])(start, object);
      }
      else
	/* It points somewhere other than oldspace.  Leave it alone. */
	words_scavenged = 1;
    else
      if ((object & 3) == 0)
	/* It's a fixnum.  Real easy. */
	words_scavenged = 1;
      else
	/* It's some random header object. */
	words_scavenged = (scavtab[TypeOf(object)])(start, object);
#endif

    start += words_scavenged;
    nwords -= words_scavenged;
  }
  gc_assert(nwords == 0);
}


/* Code and Code-Related Objects */

#define RAW_ADDR_OFFSET (6 * sizeof(lispobj) - type_FunctionPointer)

static lispobj trans_function_header(lispobj object);
static lispobj trans_boxed(lispobj object);

#if DIRECT_SCAV
static int scav_function_pointer(lispobj *where, lispobj object)
{
  gc_assert(Pointerp(object));

  if (from_space_p(object)) {
    lispobj first, *first_pointer;

    /*
     * Object is a pointer into from space - check to see if it has
     * been forwarded.
     */
    first_pointer = (lispobj *) PTR(object);
    first = *first_pointer;

    if (first == 0x01) {
      /* Forwarded */
      *where = first_pointer[1];
      return 1;
    }
    else {
      int type;
      lispobj copy;

      /*
       * Must transport object -- object may point to either a
       * function header, a closure function header, or to a closure
       * header.
       */

      type = TypeOf(first);
      switch (type) {
      case type_FunctionHeader:
      case type_ClosureFunctionHeader:
	copy = trans_function_header(object);
	break;
      default:
	copy = trans_boxed(object);
	break;
      }

      if (copy != object) {
	/* Set forwarding pointer. */
	first_pointer[0] = 0x01;
	first_pointer[1] = copy;
      }

      first = copy;
    }

    gc_assert(Pointerp(first));
    gc_assert(!from_space_p(first));

    *where = first;
  }
  return 1;
}
#else
static int scav_function_pointer(lispobj *where, lispobj object)
{
  lispobj *first_pointer;
  lispobj copy;

  gc_assert(Pointerp(object));

  /* Object is a pointer into from space - no a FP. */
  first_pointer = (lispobj *) PTR(object);

  /*
   * Must transport object -- object may point to either a function
   * header, a closure function header, or to a closure header.
   */

  switch (TypeOf(*first_pointer)) {
  case type_FunctionHeader:
  case type_ClosureFunctionHeader:
    copy = trans_function_header(object);
    break;
  default:
    copy = trans_boxed(object);
    break;
  }

  if (copy != object) {
    /* Set forwarding pointer */
    first_pointer[0] = 0x01;
    first_pointer[1] = copy;
  }

  gc_assert(Pointerp(copy));
  gc_assert(!from_space_p(copy));

  *where = copy;

  return 1;
}
#endif

/*
 * Scan a x86 compiled code objected, looking for possible fixups that
 * have been missed after a move.
 *
 * Two types of fixups are needed:
 *	1. Absolution fixups to within the code object.
 *	2. Relative fixups to outside the code object.
 *
 * Currently only absolution fixups to the constant vector, or to the
 * code area are checked.
 */
void sniff_code_object(struct code *code, unsigned displacement)
{
  int nheader_words, ncode_words, nwords;
  void *p;
  void *constants_start_addr, *constants_end_addr;
  void *code_start_addr, *code_end_addr;
  int fixup_found = 0;

  if (!check_code_fixups)
    return;

  /*
   * It's ok if it's byte compiled code. The trace table offset will
   * be a fixnum if it's x86 compiled code - check.
   */
  if (code->trace_table_offset & 0x3) {
#if 0
    fprintf(stderr, "*** Sniffing byte compiled code object at %x.\n",code);
#endif
    return;
  }

  /* Else it's x86 machine code. */

  ncode_words = fixnum_value(code->code_size);
  nheader_words = HeaderValue(*(lispobj *) code);
  nwords = ncode_words + nheader_words;

  constants_start_addr = (void *) code + 5 * 4;
  constants_end_addr = (void *) code + nheader_words * 4;
  code_start_addr = (void *) code + nheader_words * 4;
  code_end_addr = (void *) code + nwords * 4;

  /* Work through the unboxed code. */
  for (p = code_start_addr; p < code_end_addr; p++) {
    void *data = *(void **) p;
    unsigned d1 = *((unsigned char *) p - 1);
    unsigned d2 = *((unsigned char *) p - 2);
    unsigned d3 = *((unsigned char *) p - 3);
    unsigned d4 = *((unsigned char *) p - 4);
    unsigned d5 = *((unsigned char *) p - 5);
    unsigned d6 = *((unsigned char *) p - 6);

    /*
     * Check for code references.
     *
     * Check for a 32 bit word that looks like an absolute reference
     * to within the code area of the code object.
     */
    if (data >= code_start_addr - displacement
	&& data < code_end_addr - displacement) {
      /* Function header */
      if (d4 == 0x5e
	  && ((unsigned) p - 4 - 4 * HeaderValue(*((unsigned *) p - 1))) == (unsigned) code) {
	/* Skip the function header */
	p += 6 * 4 - 4 - 1;
	continue;
      }
      /* Push imm32 */
      if (d1 == 0x68) {
	fixup_found = 1;
	fprintf(stderr, "Code ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		p, d6,d5,d4,d3,d2,d1, data);
	fprintf(stderr, "***  Push $0x%.8x\n", data);
      }
      /* Mov [reg-8],imm32 */
      if (d3 == 0xc7
	  && (d2 == 0x40 || d2 == 0x41 || d2 == 0x42 || d2 == 0x43
	      || d2 == 0x45 || d2 == 0x46 || d2 == 0x47)
	  && d1 == 0xf8) {
	fixup_found = 1;
	fprintf(stderr, "Code ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		p, d6,d5,d4,d3,d2,d1, data);
	fprintf(stderr, "***  Mov [reg-8],$0x%.8x\n", data);
      }
      /* Lea reg, [disp32] */
      if (d2 == 0x8d && (d1 & 0xc7) == 5) {
	fixup_found = 1;
	fprintf(stderr, "Code ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		p, d6,d5,d4,d3,d2,d1, data);
	fprintf(stderr, "***  Lea reg,[$0x%.8x]\n", data);
      }
    }

    /*
     * Check for constant references.
     *
     * Check for a 32 bit word that looks like an absolution reference
     * to within the constant vector. Constant references will be
     * aligned.
     */
    if (data >= constants_start_addr - displacement
	&& data < constants_end_addr - displacement
	&& ((unsigned) data & 0x3) == 0) {
      /*  Mov eax,m32 */
      if (d1 == 0xa1) {
	fixup_found = 1;
	fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		p, d6, d5, d4, d3, d2, d1, data);
	fprintf(stderr, "***  Mov eax,0x%.8x\n", data);
      }

      /*  Mov m32,eax */
      if (d1 == 0xa3) {
	fixup_found = 1;
	fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		p, d6, d5, d4, d3, d2, d1, data);
	fprintf(stderr, "***  Mov 0x%.8x,eax\n", data);
      }

      /* Cmp m32,imm32 */		
      if (d1 == 0x3d && d2 == 0x81) {
	fixup_found = 1;
	fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		p, d6, d5, d4, d3, d2, d1, data);
	/* XX Check this */
	fprintf(stderr, "***  Cmp 0x%.8x,immed32\n", data);
      }

      /* Check for a mod=00, r/m=101 byte. */
      if ((d1 & 0xc7) == 5) {
	/* Cmp m32,reg */
	if (d2 == 0x39) {
	  fixup_found = 1;
	  fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		  p, d6, d5, d4, d3, d2, d1, data);
	  fprintf(stderr, "***  Cmp 0x%.8x,reg\n", data);
	}
	/* Cmp reg32,m32 */
	if (d2 == 0x3b) {
	  fixup_found = 1;
	  fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		  p, d6, d5, d4, d3, d2, d1, data);
	  fprintf(stderr, "***  Cmp reg32,0x%.8x\n", data);
	}
	/* Mov m32,reg32 */
	if (d2 == 0x89) {
	  fixup_found = 1;
	  fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		  p, d6, d5, d4, d3, d2, d1, data);
	  fprintf(stderr, "***  Mov 0x%.8x,reg32\n", data);
	}
	/* Mov reg32,m32 */
	if (d2 == 0x8b) {
	  fixup_found = 1;
	  fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		  p, d6, d5, d4, d3, d2, d1, data);
	  fprintf(stderr, "***  Mov reg32,0x%.8x\n", data);
	}
	/* Lea reg32,m32 */
	if (d2 == 0x8d) {
	  fixup_found = 1;
	  fprintf(stderr, "Abs. const. ref. @ %x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
		  p, d6, d5, d4, d3, d2, d1, data);
	  fprintf(stderr, "***  Lea reg32,0x%.8x\n", data);
	}
      }
    }
  }

  /* If anything was found print out some info. on the code object. */
  if (fixup_found) {
    fprintf(stderr, "*** Compiled code object at %x: header_words=%d code_words=%d .\n",
	    code, nheader_words, ncode_words);
    fprintf(stderr, "*** Const. start = %x; end= %x; Code start = %x; end = %x\n",
	    constants_start_addr, constants_end_addr,
	    code_start_addr, code_end_addr);
  }
}

static void apply_code_fixups(struct code *old_code, struct code *new_code)
{
  int nheader_words, ncode_words, nwords;
  void *constants_start_addr, *constants_end_addr;
  void *code_start_addr, *code_end_addr;
  lispobj fixups = NIL;
  unsigned displacement = (unsigned) new_code - (unsigned) old_code;
  struct vector *fixups_vector;

  /*
   * It's ok if it's byte compiled code. The trace table offset will
   * be a fixnum if it's x86 compiled code - check.
   */
  if (new_code->trace_table_offset & 0x3) {
#if 0
    fprintf(stderr, "*** Byte compiled code object at %x.\n", new_code);
#endif
    return;
  }

  /* Else it's x86 machine code. */
  ncode_words = fixnum_value(new_code->code_size);
  nheader_words = HeaderValue(*(lispobj *) new_code);
  nwords = ncode_words + nheader_words;
#if 0
  fprintf(stderr, "*** Compiled code object at %x: header_words=%d code_words=%d .\n",
	  new_code, nheader_words, ncode_words);
#endif
  constants_start_addr = (void *) new_code + 5 * 4;
  constants_end_addr = (void *) new_code + nheader_words * 4;
  code_start_addr = (void *) new_code + nheader_words * 4;
  code_end_addr = (void *)new_code + nwords*4;
#if 0
  fprintf(stderr, "*** Const. start = %x; end= %x; Code start = %x; end = %x\n",
	  constants_start_addr, constants_end_addr,
	  code_start_addr, code_end_addr);
#endif

  /*
   * The first constant should be a pointer to the fixups for this
   * code objects - Check.
   */
  fixups = new_code->constants[0];

  /*
   * It will be 0 or the unbound-marker if there are no fixups, and
   * will be an other pointer if it is valid.
   */
  if (fixups == 0 || fixups == type_UnboundMarker || !Pointerp(fixups)) {
    /* Check for possible errors. */
    if (check_code_fixups)
      sniff_code_object(new_code, displacement);

#if 0
    fprintf(stderr, "Fixups for code object not found!?\n");
    fprintf(stderr, "*** Compiled code object at %x: header_words=%d code_words=%d .\n",
	    new_code, nheader_words, ncode_words);
    fprintf(stderr, "*** Const. start = %x; end= %x; Code start = %x; end = %x\n",
	    constants_start_addr, constants_end_addr,
	    code_start_addr, code_end_addr);
#endif
    return;
  }

  fixups_vector = (struct vector *) PTR(fixups);

  /* Could be pointing to a forwarding pointer. */
  if (Pointerp(fixups) && find_page_index((void*) fixups_vector) != -1
      && fixups_vector->header == 0x01) {
#if 0
    fprintf(stderr, "* FF\n");
#endif
    /* If so then follow it. */
    fixups_vector = (struct vector *) PTR((lispobj) fixups_vector->length);
  }

#if 0
  fprintf(stderr, "Got the fixups\n");
#endif

  if (TypeOf(fixups_vector->header) == type_SimpleArrayUnsignedByte32) {
    /*
     * Got the fixups for the code block.  Now work through the
     * vector, and apply a fixup at each address.
     */
    int length = fixnum_value(fixups_vector->length);
    int i;
    for (i = 0; i < length; i++) {
      unsigned offset = fixups_vector->data[i];
      /* Now check the current value of offset. */
      unsigned old_value = *(unsigned *) ((unsigned) code_start_addr + offset);

      /*
       * If it's within the old_code object then it must be an
       * absolute fixup (relative ones are not saved).
       */
      if (old_value >= (unsigned) old_code
	  && old_value < (unsigned) old_code + nwords * 4)
	/* So add the dispacement. */
	*(unsigned *) ((unsigned) code_start_addr + offset) = old_value
	  + displacement;
      else
	/*
	 * It is outside the old code object so it must be a relative
	 * fixup (absolute fixups are not saved). So subtract the
	 * displacement.
	 */
	*(unsigned *) ((unsigned) code_start_addr + offset) = old_value
	  - displacement;
    }
  }

  /* Check for possible errors. */
  if (check_code_fixups)
    sniff_code_object(new_code, displacement);
}

static struct code * trans_code(struct code *code)
{
  struct code *new_code;
  lispobj l_code, l_new_code;
  int nheader_words, ncode_words, nwords;
  unsigned long displacement;
  lispobj fheaderl, *prev_pointer;

#if 0
  fprintf(stderr, "\nTransporting code object located at 0x%08x.\n",
	  (unsigned long) code);
#endif

  /* If object has already been transported, just return pointer */
  if (*(lispobj *) code == 0x01)
    return (struct code*) (((lispobj *) code)[1]);

  gc_assert(TypeOf(code->header) == type_CodeHeader);

  /* prepare to transport the code vector */
  l_code = (lispobj) code | type_OtherPointer;

  ncode_words = fixnum_value(code->code_size);
  nheader_words = HeaderValue(code->header);
  nwords = ncode_words + nheader_words;
  nwords = CEILING(nwords, 2);

  l_new_code = copy_large_object(l_code, nwords);
  new_code = (struct code *) PTR(l_new_code);

  /* May not have been moved. */
  if (new_code == code)
    return new_code;

  displacement = l_new_code - l_code;

#if 0
  fprintf(stderr, "Old code object at 0x%08x, new code object at 0x%08x.\n",
	  (unsigned long) code, (unsigned long) new_code);
  fprintf(stderr, "Code object is %d words long.\n", nwords);
#endif

  /* set forwarding pointer */
  ((lispobj *) code)[0] = 0x01;
  ((lispobj *) code)[1] = l_new_code;

  /*
   * Set forwarding pointers for all the function headers in the code
   * object; also fix all self pointers.
   */

  fheaderl = code->entry_points;
  prev_pointer = &new_code->entry_points;

  while (fheaderl != NIL) {
    struct function *fheaderp, *nfheaderp;
    lispobj nfheaderl;

    fheaderp = (struct function *) PTR(fheaderl);
    gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);

    /*
     * Calcuate the new function pointer and the new function header.
     */
    nfheaderl = fheaderl + displacement;
    nfheaderp = (struct function *) PTR(nfheaderl);

    /* set forwarding pointer */
    ((lispobj *) fheaderp)[0] = 0x01;
    ((lispobj *) fheaderp)[1] = nfheaderl;

    /* Fix self pointer */
    nfheaderp->self = nfheaderl + RAW_ADDR_OFFSET;

    *prev_pointer = nfheaderl;

    fheaderl = fheaderp->next;
    prev_pointer = &nfheaderp->next;
  }

#if 0
  sniff_code_object(new_code, displacement);
#endif
  apply_code_fixups(code, new_code);

  return new_code;
}

static int scav_code_header(lispobj *where, lispobj object)
{
  struct code *code;
  int nheader_words, ncode_words, nwords;
  lispobj fheaderl;
  struct function *fheaderp;

  code = (struct code *) where;
  ncode_words = fixnum_value(code->code_size);
  nheader_words = HeaderValue(object);
  nwords = ncode_words + nheader_words;
  nwords = CEILING(nwords, 2);

  /* Scavenge the boxed section of the code data block */
  scavenge(where + 1, nheader_words - 1);

  /*
   * Scavenge the boxed section of each function object in the code
   * data block
   */
  fheaderl = code->entry_points;
  while (fheaderl != NIL) {
    fheaderp = (struct function *) PTR(fheaderl);
    gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);

    scavenge(&fheaderp->name, 1);
    scavenge(&fheaderp->arglist, 1);
    scavenge(&fheaderp->type, 1);

    fheaderl = fheaderp->next;
  }

  return nwords;
}

static lispobj trans_code_header(lispobj object)
{
	struct code *ncode;

	ncode = trans_code((struct code *) PTR(object));
	return (lispobj) ncode | type_OtherPointer;
}

static int size_code_header(lispobj *where)
{
	struct code *code;
	int nheader_words, ncode_words, nwords;

	code = (struct code *) where;

	ncode_words = fixnum_value(code->code_size);
	nheader_words = HeaderValue(code->header);
	nwords = ncode_words + nheader_words;
	nwords = CEILING(nwords, 2);

	return nwords;
}


static int scav_return_pc_header(lispobj *where, lispobj object)
{
    fprintf(stderr, "GC lossage.  Should not be scavenging a ");
    fprintf(stderr, "Return PC Header.\n");
    fprintf(stderr, "where = 0x%08x, object = 0x%08x",
	    (unsigned long) where, (unsigned long) object);
    lose(NULL);
    return 0;
}

static lispobj trans_return_pc_header(lispobj object)
{
  struct function *return_pc;
  unsigned long offset;
  struct code *code, *ncode;

  fprintf(stderr, "*** trans_return_pc_header: will this work?\n");

  return_pc = (struct function *) PTR(object);
  offset = HeaderValue(return_pc->header) * 4;

  /* Transport the whole code object */
  code = (struct code *) ((unsigned long) return_pc - offset);
  ncode = trans_code(code);

  return ((lispobj) ncode + offset) | type_OtherPointer;
}

/*
 * On the 386, closures hold a pointer to the raw address instead of
 * the function object.
 */
#ifdef i386
static int scav_closure_header(lispobj *where, lispobj object)
{
  struct closure *closure;
  lispobj fun;

  closure = (struct closure *)where;
  fun = closure->function - RAW_ADDR_OFFSET;
  scavenge(&fun, 1);
  /* The function may have moved so update the raw address. But don't
     write unnecessarily. */
  if (closure->function != fun + RAW_ADDR_OFFSET)
    closure->function = fun + RAW_ADDR_OFFSET;

  return 2;
}
#endif

static int scav_function_header(lispobj *where, lispobj object)
{
    fprintf(stderr, "GC lossage.  Should not be scavenging a ");
    fprintf(stderr, "Function Header.\n");
    fprintf(stderr, "where = 0x%08x, object = 0x%08x",
	    (unsigned long) where, (unsigned long) object);
    lose(NULL);
    return 0;
}

static lispobj trans_function_header(lispobj object)
{
  struct function *fheader;
  unsigned long offset;
  struct code *code, *ncode;

  fheader = (struct function *) PTR(object);
  offset = HeaderValue(fheader->header) * 4;

  /* Transport the whole code object */
  code = (struct code *) ((unsigned long) fheader - offset);
  ncode = trans_code(code);

  return ((lispobj) ncode + offset) | type_FunctionPointer;
}


/* Instances */

#if DIRECT_SCAV
static int scav_instance_pointer(lispobj *where, lispobj object)
{
  if (from_space_p(object)) {
    lispobj first, *first_pointer;

    /*
     * object is a pointer into from space.  check to see if it has
     * been forwarded
     */
    first_pointer = (lispobj *) PTR(object);
    first = *first_pointer;

    if (first == 0x01)
      /* Forwarded. */
      first = first_pointer[1];
    else {
      first = trans_boxed(object);
      gc_assert(first != object);
      /* Set forwarding pointer */
      first_pointer[0] = 0x01;
      first_pointer[1] = first;
    }
    *where = first;
  }
  return 1;
}
#else
static int scav_instance_pointer(lispobj *where, lispobj object)
{
  lispobj copy, *first_pointer;

  /* Object is a pointer into from space - not a FP */
  copy = trans_boxed(object);

  gc_assert(copy != object);

  first_pointer = (lispobj *) PTR(object);

  /* Set forwarding pointer. */
  first_pointer[0] = 0x01;
  first_pointer[1] = copy;
  *where = copy;

  return 1;
}
#endif


/* Lists and Conses */

static lispobj trans_list(lispobj object);

#if DIRECT_SCAV
static int scav_list_pointer(lispobj *where, lispobj object)
{
  gc_assert(Pointerp(object));

  if (from_space_p(object)) {
    lispobj first, *first_pointer;

    /*
     * Object is a pointer into from space - check to see if it has
     * been forwarded.
     */
    first_pointer = (lispobj *) PTR(object);
    first = *first_pointer;

    if (first == 0x01)
      /* Forwarded. */
      first = first_pointer[1];
    else {
      first = trans_list(object);

      /* Set forwarding pointer */
      first_pointer[0] = 0x01;
      first_pointer[1] = first;
    }

    gc_assert(Pointerp(first));
    gc_assert(!from_space_p(first));
    *where = first;
  }
  return 1;
}
#else
static int scav_list_pointer(lispobj *where, lispobj object)
{
  lispobj first, *first_pointer;

  gc_assert(Pointerp(object));

  /* Object is a pointer into from space - not FP */

  first = trans_list(object);
  gc_assert(first != object);

  first_pointer = (lispobj *) PTR(object);

  /* Set forwarding pointer */
  first_pointer[0] = 0x01;
  first_pointer[1] = first;

  gc_assert(Pointerp(first));
  gc_assert(!from_space_p(first));
  *where = first;
  return 1;
}
#endif

static lispobj trans_list(lispobj object)
{
  lispobj new_list_pointer;
  struct cons *cons, *new_cons;
  lispobj cdr;

  gc_assert(from_space_p(object));

  cons = (struct cons *) PTR(object);

  /* copy 'object' */
  new_cons = (struct cons *) gc_quick_alloc(sizeof(struct cons));
  new_cons->car = cons->car;
  new_cons->cdr = cons->cdr; /* updated later */
  new_list_pointer = (lispobj) new_cons | LowtagOf(object);

  /* Grab the cdr before it is clobbered */
  cdr = cons->cdr;

  /* Set forwarding pointer (clobbers start of list). */
  cons->car = 0x01;
  cons->cdr = new_list_pointer;

  /* Try to linearize the list in the cdr direction to help reduce paging. */  
  while (1) {
    lispobj  new_cdr;
    struct cons *cdr_cons, *new_cdr_cons;

    if (LowtagOf(cdr) != type_ListPointer || !from_space_p(cdr)
	|| *((lispobj *) PTR(cdr)) == 0x01)
      break;

    cdr_cons = (struct cons *) PTR(cdr);

    /* copy 'cdr' */
    new_cdr_cons = (struct cons*) gc_quick_alloc(sizeof(struct cons));
    new_cdr_cons->car = cdr_cons->car;
    new_cdr_cons->cdr = cdr_cons->cdr;
    new_cdr = (lispobj) new_cdr_cons | LowtagOf(cdr);

    /* Grab the cdr before it is clobbered */
    cdr = cdr_cons->cdr;

    /* Set forwarding pointer */
    cdr_cons->car = 0x01;
    cdr_cons->cdr = new_cdr;

    /*
     * Update the cdr of the last cons copied into new space to keep
     * the newspace scavenge from having to do it.
     */
    new_cons->cdr = new_cdr;

    new_cons = new_cdr_cons;
  }

  return new_list_pointer;
}


/* Scavenging and Transporting Other Pointers */

#if DIRECT_SCAV
static int scav_other_pointer(lispobj *where, lispobj object)
{
  gc_assert(Pointerp(object));

  if (from_space_p(object)) {
    lispobj first, *first_pointer;

    /*
     * Object is a pointer into from space.  check to see if it has
     * been forwarded.
     */
    first_pointer = (lispobj *) PTR(object);
    first = *first_pointer;

    if (first == 0x01) {
      /* Forwarded. */
      first = first_pointer[1];
      *where = first;
    } else {
      first = (transother[TypeOf(first)])(object);

      if (first != object) {
	/* Set forwarding pointer */
	first_pointer[0] = 0x01;
	first_pointer[1] = first;
	*where = first;
      }
    }

    gc_assert(Pointerp(first));
    gc_assert(!from_space_p(first));
  }
  return 1;
}
#else
static int scav_other_pointer(lispobj *where, lispobj object)
{
  lispobj first, *first_pointer;

  gc_assert(Pointerp(object));

  /* Object is a pointer into from space - not FP */
  first_pointer = (lispobj *) PTR(object);

  first = (transother[TypeOf(*first_pointer)])(object);

  if (first != object) {
    /* Set forwarding pointer */
    first_pointer[0] = 0x01;
    first_pointer[1] = first;
    *where = first;
  }

  gc_assert(Pointerp(first));
  gc_assert(!from_space_p(first));

  return 1;
}
#endif


/* Immediate, Boxed, and Unboxed Objects */

static int size_pointer(lispobj *where)
{
    return 1;
}

static int scav_immediate(lispobj *where, lispobj object)
{
    return 1;
}

static lispobj trans_immediate(lispobj object)
{
    fprintf(stderr, "GC lossage.  Trying to transport an immediate!?\n");
    lose(NULL);
    return NIL;
}

static int size_immediate(lispobj *where)
{
    return 1;
}


static int scav_boxed(lispobj *where, lispobj object)
{
    return 1;
}

static lispobj trans_boxed(lispobj object)
{
	lispobj header;
	unsigned long length;

	gc_assert(Pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return copy_object(object, length);
}

static lispobj trans_boxed_large(lispobj object)
{
	lispobj header;
	unsigned long length;

	gc_assert(Pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return copy_large_object(object, length);
}

static int size_boxed(lispobj *where)
{
	lispobj header;
	unsigned long length;

	header = *where;
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return length;
}

static int scav_fdefn(lispobj *where, lispobj object)
{
  struct fdefn *fdefn;

  fdefn = (struct fdefn *)where;

  if ((char *) (fdefn->function + RAW_ADDR_OFFSET) == fdefn->raw_addr) {
    scavenge(where + 1, sizeof(struct fdefn) / sizeof(lispobj) - 1);

    /* Don't write unnecessarily */
    if (fdefn->raw_addr != (char *)(fdefn->function + RAW_ADDR_OFFSET))
      fdefn->raw_addr = (char *)(fdefn->function + RAW_ADDR_OFFSET);

    return sizeof(struct fdefn) / sizeof(lispobj);
  }
  else
    return 1;
}

static int scav_unboxed(lispobj *where, lispobj object)
{
	unsigned long length;

	length = HeaderValue(object) + 1;
	length = CEILING(length, 2);

	return length;
}

static lispobj trans_unboxed(lispobj object)
{
	lispobj header;
	unsigned long length;


	gc_assert(Pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return copy_unboxed_object(object, length);
}

static lispobj trans_unboxed_large(lispobj object)
{
	lispobj header;
	unsigned long length;


	gc_assert(Pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return copy_large_unboxed_object(object, length);
}

static int size_unboxed(lispobj *where)
{
	lispobj header;
	unsigned long length;

	header = *where;
	length = HeaderValue(header) + 1;
	length = CEILING(length, 2);

	return length;
}


/* Vector-Like Objects */

#define NWORDS(x,y) (CEILING((x),(y)) / (y))

static int scav_string(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	/*
	 * NOTE: Strings contain one more byte of data than the length
	 * slot indicates.
	 */

	vector = (struct vector *) where;
	length = fixnum_value(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}

static lispobj trans_string(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	/*
	 * NOTE: Strings contain one more byte of data than the length
	 * slot indicates.
	 */

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_string(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	/*
	 * NOTE: Strings contain one more byte of data than the length
	 * slot indicates.
	 */

	vector = (struct vector *) where;
	length = fixnum_value(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}

#if 0
static int scav_vector(lispobj *where, lispobj object)
{
    if (HeaderValue(object) == subtype_VectorValidHashing)
        *where = (subtype_VectorMustRehash << type_Bits) | type_SimpleVector;

    return 1;
}
#endif

int gencgc_hash = 1;

static int scav_vector(lispobj *where, lispobj object)
{
  unsigned int kv_length;
  lispobj *kv_vector;
  unsigned int  length;
  lispobj *hash_table;
  lispobj empty_symbol;
  unsigned int  *index_vector, *next_vector, *hash_vector;
  lispobj weak_p_obj;
  unsigned next_vector_length;

  if (HeaderValue(object) != subtype_VectorValidHashing)
    return 1;

  if (!gencgc_hash) {
    /* Set for backward compatibility. */
    *where = (subtype_VectorMustRehash << type_Bits) | type_SimpleVector;
    return 1;
  }

  kv_length = fixnum_value(where[1]);
  kv_vector = where+2;  /* Skip the header and length. */
#if 0
  fprintf(stderr, "* kv_length = %d\n", kv_length);
#endif

  /* Scavenge element 0 which may be a hash-table structure. */
  scavenge(where + 2, 1);
  if (!Pointerp(where[2])) {
    fprintf(stderr, "* Not hash table pointer? %x\n", where[2]);
    return 3;
  }
  hash_table = (lispobj *) PTR(where[2]);
#if 0
  fprintf(stderr, "* hash_table = %x\n", hash_table);
#endif
  if (TypeOf(hash_table[0]) != type_InstanceHeader) {
    fprintf(stderr, "* Hash table not instance? %x\n", hash_table[0]);
    return 3;
  }

  /* Scavenge element 1 which should be an :empty symbol. */
  scavenge(where + 3, 1);
  if (!Pointerp(where[3])) {
    fprintf(stderr, "* Not :empty symbol pointer? %x\n", where[3]);
    return 4;
  }
  empty_symbol = where[3];
#if 0
  fprintf(stderr, "* empty_symbol = %x\n", empty_symbol);
#endif
  if (TypeOf(*(lispobj *) PTR(empty_symbol)) != type_SymbolHeader) {
    fprintf(stderr, "* empty symbol not symbol? %x\n",
	    *(lispobj *) PTR(empty_symbol));
    return 4;
  }

  /*
   * Scavenge hash table which will fix the positions of the other
   * needed objects.
   */
  scavenge(hash_table,16);

  /* Cross check the kv_vector. */
  if (where != (lispobj *) PTR(hash_table[9])) {
    fprintf(stderr, "* hash_table table!=this table? %x\n", hash_table[9]);
    return 4;
  }

  /* Weak-p */
  weak_p_obj = hash_table[10];
#if 0
  fprintf(stderr, "* weak-p = %x\n", weak_p_obj);
#endif

  /* Index vector */
  {
    lispobj index_vector_obj = hash_table[13];

    if (Pointerp(index_vector_obj) &&
	TypeOf(*(lispobj *) PTR(index_vector_obj)) == type_SimpleArrayUnsignedByte32) {
      index_vector = (unsigned int *) PTR(index_vector_obj) + 2;
#if 0
      fprintf(stderr, "* index_vector = %x\n", index_vector);
#endif
      length = fixnum_value(((unsigned int *) PTR(index_vector_obj))[1]);
#if 0
      fprintf(stderr, "* length = %d\n", length);
#endif
    } else {
      fprintf(stderr, "* invalid index_vector? %x\n", index_vector_obj);
      return 4;
    }
  }

  /* Next vector */
  {
    lispobj next_vector_obj = hash_table[14];

    if (Pointerp(next_vector_obj) &&
	TypeOf(*(lispobj *) PTR(next_vector_obj)) == type_SimpleArrayUnsignedByte32) {
      next_vector = (unsigned int *) PTR(next_vector_obj) + 2;
#if 0
      fprintf(stderr, "* next_vector = %x\n", next_vector);
#endif
      next_vector_length = fixnum_value(((unsigned int *) PTR(next_vector_obj))[1]);
#if 0
      fprintf(stderr, "* next_vector_length = %d\n", next_vector_length);
#endif
    } else {
      fprintf(stderr, "* invalid next_vector? %x\n", next_vector_obj);
      return 4;
    }
  }

  /* Maybe Hash vector */
  {
    lispobj hash_vector_obj = hash_table[15];

    if (Pointerp(hash_vector_obj) &&
	TypeOf(*(lispobj *) PTR(hash_vector_obj)) == type_SimpleArrayUnsignedByte32) {
      hash_vector = (unsigned int *) PTR(hash_vector_obj) + 2;
#if 0
      fprintf(stderr, "* hash_vector = %x\n", hash_vector);
#endif
      gc_assert(fixnum_value(((unsigned int *) PTR(hash_vector_obj))[1])
		== next_vector_length);
    } else {
      hash_vector = NULL;
#if 0
      fprintf(stderr, "* No hash_vector: %x\n", hash_vector_obj);
#endif
    }
  }

  /*
   * These lengths could be different as the index_vector can be a
   * different length to the others, a larger index_vector could help
   * reduce collisions.
   */
  gc_assert(next_vector_length * 2 == kv_length);

  /* Now all setup */

  /* Work through the KV vector */
  {
    int i;
    for (i = 1; i < next_vector_length; i++) {
      lispobj old_key = kv_vector[2 * i];
      unsigned int  old_index = (old_key & 0x1fffffff) % length;

      /* Scavenge the Key and Value */
      scavenge(&kv_vector[2 * i], 2);

      /* Check if the Key has moved and is EQ based */
      {
	lispobj new_key = kv_vector[2 * i];
	unsigned int new_index = (new_key & 0x1fffffff) % length;

	if (old_index != new_index &&
	    (!hash_vector || hash_vector[i] == 0x80000000) &&
	    (new_key != empty_symbol || kv_vector[2 * i] != empty_symbol)) {

#if 0
	  fprintf(stderr, "* EQ key %d moved from %x to %x; index %d to %d\n",
		  i, old_key, new_key, old_index, new_index);
#endif

	  if (index_vector[old_index] != 0) {
#if 0
	    fprintf(stderr, "*P1 %d\n", index_vector[old_index]);
#endif

	    /* Unlink the key from the old_index chain. */
	    if (index_vector[old_index] == i) {
#if 0
	      fprintf(stderr, "*P2a %d\n", next_vector[i]);
#endif
	      index_vector[old_index] = next_vector[i];
	      /* Link it into the needing rehash chain. */
	      next_vector[i] = fixnum_value(hash_table[11]);
	      hash_table[11] = make_fixnum(i);
#if 0
	      fprintf(stderr, "*P2\n");
#endif
	    } else {
	      unsigned prior = index_vector[old_index];
	      unsigned next = next_vector[prior];

#if 0
	      fprintf(stderr, "*P3a %d %d\n", prior, next);
#endif

	      while (next != 0) {
#if 0
		fprintf(stderr, "*P3b %d %d\n", prior, next);
#endif
		if (next == i) {
		  /* Unlink it */
		  next_vector[prior] = next_vector[next];
		  /* Link it into the needing rehash chain. */
		  next_vector[next] = fixnum_value(hash_table[11]);
		  hash_table[11] = make_fixnum(next);
#if 0
		  fprintf(stderr, "*P3\n");
#endif
		  break;
		}
		prior = next;
		next = next_vector[next];
	      };
	    }
	  }
	}
      }
    }
  }
  return CEILING(kv_length + 2, 2);
}


static lispobj trans_vector(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);

	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_large_object(object, nwords);
}

static int size_vector(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}


static int scav_vector_bit(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return nwords;
}

static lispobj trans_vector_bit(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_bit(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return nwords;
}


static int scav_vector_unsigned_byte_2(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return nwords;
}

static lispobj trans_vector_unsigned_byte_2(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_unsigned_byte_2(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return nwords;
}


static int scav_vector_unsigned_byte_4(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return nwords;
}

static lispobj trans_vector_unsigned_byte_4(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_unsigned_byte_4(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return nwords;
}


static int scav_vector_unsigned_byte_8(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}

static lispobj trans_vector_unsigned_byte_8(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_unsigned_byte_8(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}


static int scav_vector_unsigned_byte_16(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return nwords;
}

static lispobj trans_vector_unsigned_byte_16(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_unsigned_byte_16(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return nwords;
}


static int scav_vector_unsigned_byte_32(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}

static lispobj trans_vector_unsigned_byte_32(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_unsigned_byte_32(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}


static int scav_vector_single_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}

static lispobj trans_vector_single_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_single_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}


static int scav_vector_double_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return nwords;
}

static lispobj trans_vector_double_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_double_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return nwords;
}


#ifdef type_SimpleArrayLongFloat
static int scav_vector_long_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 3 + 2, 2);

	return nwords;
}

static lispobj trans_vector_long_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 3 + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_long_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 3 + 2, 2);

	return nwords;
}
#endif


#ifdef type_SimpleArrayComplexSingleFloat
static int scav_vector_complex_single_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return nwords;
}

static lispobj trans_vector_complex_single_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_complex_single_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return nwords;
}
#endif

#ifdef type_SimpleArrayComplexDoubleFloat
static int scav_vector_complex_double_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 4 + 2, 2);

	return nwords;
}

static lispobj trans_vector_complex_double_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 4 + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_complex_double_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 4 + 2, 2);

	return nwords;
}
#endif


#ifdef type_SimpleArrayComplexLongFloat
static int scav_vector_complex_long_float(lispobj *where, lispobj object)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 6 + 2, 2);

	return nwords;
}

static lispobj trans_vector_complex_long_float(lispobj object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(Pointerp(object));

	vector = (struct vector *) PTR(object);
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 6 + 2, 2);

	return copy_large_unboxed_object(object, nwords);
}

static int size_vector_complex_long_float(lispobj *where)
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = fixnum_value(vector->length);
	nwords = CEILING(length * 6 + 2, 2);

	return nwords;
}
#endif


/* Weak Pointers */

/*
 * XX Hack adapted from cgc.c; These don't work too well with the
 * gencgc as a list of the weak pointers is maintained within the
 * objects which causes writes to the pages. A limited attempt is made
 * to avoid unnecessary writes, but this needs a re-think.
 */

#define WEAK_POINTER_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static int scav_weak_pointer(lispobj *where, lispobj object)
{
  struct weak_pointer *wp = weak_pointers;
  /*
   * Push the weak pointer onto the list of weak pointers.  Do I have
   * to watch for duplicates? Originally this was part of trans_weak_pointer
   * but that didn't work in the case where the WP was in a promoted region.
   */

  /* Check if it's already in the list. */
  while(wp != NULL) {
    if(wp == (struct weak_pointer*) where)
      break;
    wp = wp->next;
  }
  if(wp == NULL) {
    /* Add it to the start of the list. */
    wp = (struct weak_pointer*) where;
    if (wp->next != weak_pointers)
      wp->next = weak_pointers;
#if 0
    else 
      fprintf(stderr, "Avoided write to weak pointer.\n");
#endif
    weak_pointers = wp;
  }

  /*
   * Do not let GC scavenge the value slot of the weak pointer (that
   * is why it is a weak pointer).
   */

  return WEAK_POINTER_NWORDS;
}

static lispobj trans_weak_pointer(lispobj object)
{
  lispobj copy;
  struct weak_pointer *wp;

  gc_assert(Pointerp(object));

#if defined(DEBUG_WEAK)
  printf("Transporting weak pointer from 0x%08x\n", object);
#endif

  /*
   * Need to remember where all the weak pointers are that have been
   * transported so they can be fixed up in a post-GC pass.
   */

  copy = copy_object(object, WEAK_POINTER_NWORDS);
#if 0
  wp = (struct weak_pointer *) PTR(copy);
#endif

  /* Push the weak pointer onto the list of weak pointers. */
#if 0
  wp->next = weak_pointers;
  weak_pointers = wp;
#endif

  return copy;
}

static int size_weak_pointer(lispobj *where)
{
  return WEAK_POINTER_NWORDS;
}

void scan_weak_pointers(void)
{
  struct weak_pointer *wp;
  for (wp = weak_pointers; wp != NULL; wp = wp->next) {
    lispobj value = wp->value;
    lispobj *first_pointer;

    first_pointer = (lispobj *) PTR(value);

#if 0    
    fprintf(stderr, "Weak pointer at 0x%08x\n", (unsigned long) wp);
    fprintf(stderr, "Value: 0x%08x\n", (unsigned long) value);
#endif

    if (Pointerp(value) && from_space_p(value)) {
      /*
       * Now, we need to check if the object has been forwarded.  If
       * it has been, the weak pointer is still good and needs to be
       * updated. Otherwise, the weak pointer needs to be nil'ed out.
       */

      if (first_pointer[0] == 0x01)
	wp->value = first_pointer[1];
      else {
	/* Break it */
#if 0
	fprintf(stderr, "Broken.\n");
#endif
	wp->value = NIL;
	wp->broken = T;
      }
    }
  }
}


/* Scavenged Hooks */

#define SCAVENGER_HOOK_NWORDS \
	CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

static int scav_scavenger_hook(lispobj *where, lispobj object)
{
  struct scavenger_hook *scav_hook = (struct scavenger_hook *) where;
  lispobj old_value = scav_hook->value;

#if 0
  fprintf(stderr, "scav scav_hook %x; value %x\n", where, old_value);
#endif

  /* Scavenge the value */
  scavenge(where + 1, 1);

  if (scav_hook->value != old_value) {
    /* Value object has moved */
#if 0
    fprintf(stderr, "   value object moved to %x\n", scav_hook->value);
#endif

    /* Check if this hook is already noted. */
#if 0
    fprintf(stderr, "   next=%x sh hooks=%x\n",
	    scav_hook->next, scavenger_hooks);
#endif
    if (scav_hook->next == NULL) {
#if 0
      fprintf(stderr, "   adding to scavenger_hooks\n");
#endif
      scav_hook->next = scavenger_hooks;
      scavenger_hooks = (struct scavenger_hook *) ((int) where |
						   type_OtherPointer);
    }
  }

  /* Scavenge the function and the tail scavenge_hook */
  return 2;
}

static lispobj trans_scavenger_hook(lispobj object)
{
  lispobj copy;
  gc_assert(Pointerp(object));
#if 0
  printf("Transporting scav pointer from 0x%08x\n", object);
#endif
  copy = copy_object(object, SCAVENGER_HOOK_NWORDS);
  return copy;
}

static int
size_scavenger_hook(lispobj *where)
{
	return SCAVENGER_HOOK_NWORDS;
}


/* Initialization */

static int scav_lose(lispobj *where, lispobj object)
{
    fprintf(stderr, "GC lossage.  No scavenge function for object 0x%08x\n",
	    (unsigned long) object);
    lose(NULL);
    return 0;
}

static lispobj trans_lose(lispobj object)
{
    fprintf(stderr, "GC lossage.  No transport function for object 0x%08x\n",
	    (unsigned long) object);
    lose(NULL);
    return NIL;
}

static int size_lose(lispobj *where)
{
	fprintf(stderr, "Size lossage.  No size function for object at 0x%08x\n",
		(unsigned long) where);
	fprintf(stderr, "First word of object: 0x%08x\n",
		(unsigned long) *where);
	return 1;
}

static void gc_init_tables(void)
{
	int i;

	/* Scavenge Table */
	for (i = 0; i < 256; i++)
		scavtab[i] = scav_lose;

	for (i = 0; i < 32; i++) {
		scavtab[type_EvenFixnum | (i << 3)] = scav_immediate;
		scavtab[type_FunctionPointer | (i<<3)] = scav_function_pointer;
		/* OtherImmediate0 */
		scavtab[type_ListPointer | (i << 3)] = scav_list_pointer;
		scavtab[type_OddFixnum | (i << 3)] = scav_immediate;
		scavtab[type_InstancePointer | (i<<3)] = scav_instance_pointer;
		/* OtherImmediate1 */
		scavtab[type_OtherPointer | (i << 3)] = scav_other_pointer;
	}

	scavtab[type_Bignum] = scav_unboxed;
	scavtab[type_Ratio] = scav_boxed;
	scavtab[type_SingleFloat] = scav_unboxed;
	scavtab[type_DoubleFloat] = scav_unboxed;
#ifdef type_LongFloat
	scavtab[type_LongFloat] = scav_unboxed;
#endif
	scavtab[type_Complex] = scav_boxed;
#ifdef type_ComplexSingleFloat
	scavtab[type_ComplexSingleFloat] = scav_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
	scavtab[type_ComplexDoubleFloat] = scav_unboxed;
#endif
#ifdef type_ComplexLongFloat
	scavtab[type_ComplexLongFloat] = scav_unboxed;
#endif
	scavtab[type_SimpleArray] = scav_boxed;
	scavtab[type_SimpleString] = scav_string;
	scavtab[type_SimpleBitVector] = scav_vector_bit;
	scavtab[type_SimpleVector] = scav_vector;
	scavtab[type_SimpleArrayUnsignedByte2] = scav_vector_unsigned_byte_2;
	scavtab[type_SimpleArrayUnsignedByte4] = scav_vector_unsigned_byte_4;
	scavtab[type_SimpleArrayUnsignedByte8] = scav_vector_unsigned_byte_8;
	scavtab[type_SimpleArrayUnsignedByte16] = scav_vector_unsigned_byte_16;
	scavtab[type_SimpleArrayUnsignedByte32] = scav_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
	scavtab[type_SimpleArraySignedByte8] = scav_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
	scavtab[type_SimpleArraySignedByte16] = scav_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
	scavtab[type_SimpleArraySignedByte30] = scav_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
	scavtab[type_SimpleArraySignedByte32] = scav_vector_unsigned_byte_32;
#endif
	scavtab[type_SimpleArraySingleFloat] = scav_vector_single_float;
	scavtab[type_SimpleArrayDoubleFloat] = scav_vector_double_float;
#ifdef type_SimpleArrayLongFloat
	scavtab[type_SimpleArrayLongFloat] = scav_vector_long_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	scavtab[type_SimpleArrayComplexSingleFloat] = scav_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	scavtab[type_SimpleArrayComplexDoubleFloat] = scav_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
	scavtab[type_SimpleArrayComplexLongFloat] = scav_vector_complex_long_float;
#endif
	scavtab[type_ComplexString] = scav_boxed;
	scavtab[type_ComplexBitVector] = scav_boxed;
	scavtab[type_ComplexVector] = scav_boxed;
	scavtab[type_ComplexArray] = scav_boxed;
	scavtab[type_CodeHeader] = scav_code_header;
	/*scavtab[type_FunctionHeader] = scav_function_header;*/
	/*scavtab[type_ClosureFunctionHeader] = scav_function_header;*/
	/*scavtab[type_ReturnPcHeader] = scav_return_pc_header;*/
#ifdef i386
	scavtab[type_ClosureHeader] = scav_closure_header;
	scavtab[type_FuncallableInstanceHeader] = scav_closure_header;
	scavtab[type_ByteCodeFunction] = scav_closure_header;
	scavtab[type_ByteCodeClosure] = scav_closure_header;
	scavtab[type_DylanFunctionHeader] = scav_closure_header;
#else
	scavtab[type_ClosureHeader] = scav_boxed;
	scavtab[type_FuncallableInstanceHeader] = scav_boxed;
	scavtab[type_ByteCodeFunction] = scav_boxed;
	scavtab[type_ByteCodeClosure] = scav_boxed;
	scavtab[type_DylanFunctionHeader] = scav_boxed;
#endif
	scavtab[type_ValueCellHeader] = scav_boxed;
        scavtab[type_SymbolHeader] = scav_boxed;
	scavtab[type_BaseChar] = scav_immediate;
	scavtab[type_Sap] = scav_unboxed;
	scavtab[type_UnboundMarker] = scav_immediate;
	scavtab[type_WeakPointer] = scav_weak_pointer;
        scavtab[type_InstanceHeader] = scav_boxed;
        scavtab[type_Fdefn] = scav_fdefn;
        scavtab[type_ScavengerHook] = scav_scavenger_hook;

	/* Transport Other Table */
	for (i = 0; i < 256; i++)
		transother[i] = trans_lose;

	transother[type_Bignum] = trans_unboxed_large;
	transother[type_Ratio] = trans_boxed;
	transother[type_SingleFloat] = trans_unboxed;
	transother[type_DoubleFloat] = trans_unboxed;
#ifdef type_LongFloat
	transother[type_LongFloat] = trans_unboxed;
#endif
	transother[type_Complex] = trans_boxed;
#ifdef type_ComplexSingleFloat
	transother[type_ComplexSingleFloat] = trans_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
	transother[type_ComplexDoubleFloat] = trans_unboxed;
#endif
#ifdef type_ComplexLongFloat
	transother[type_ComplexLongFloat] = trans_unboxed;
#endif
	transother[type_SimpleArray] = trans_boxed_large;
	transother[type_SimpleString] = trans_string;
	transother[type_SimpleBitVector] = trans_vector_bit;
	transother[type_SimpleVector] = trans_vector;
	transother[type_SimpleArrayUnsignedByte2] = trans_vector_unsigned_byte_2;
	transother[type_SimpleArrayUnsignedByte4] = trans_vector_unsigned_byte_4;
	transother[type_SimpleArrayUnsignedByte8] = trans_vector_unsigned_byte_8;
	transother[type_SimpleArrayUnsignedByte16] = trans_vector_unsigned_byte_16;
	transother[type_SimpleArrayUnsignedByte32] = trans_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
	transother[type_SimpleArraySignedByte8] = trans_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
	transother[type_SimpleArraySignedByte16] = trans_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
	transother[type_SimpleArraySignedByte30] = trans_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
	transother[type_SimpleArraySignedByte32] = trans_vector_unsigned_byte_32;
#endif
	transother[type_SimpleArraySingleFloat] = trans_vector_single_float;
	transother[type_SimpleArrayDoubleFloat] = trans_vector_double_float;
#ifdef type_SimpleArrayLongFloat
	transother[type_SimpleArrayLongFloat] = trans_vector_long_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	transother[type_SimpleArrayComplexSingleFloat] = trans_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	transother[type_SimpleArrayComplexDoubleFloat] = trans_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
	transother[type_SimpleArrayComplexLongFloat] = trans_vector_complex_long_float;
#endif
	transother[type_ComplexString] = trans_boxed;
	transother[type_ComplexBitVector] = trans_boxed;
	transother[type_ComplexVector] = trans_boxed;
	transother[type_ComplexArray] = trans_boxed;
	transother[type_CodeHeader] = trans_code_header;
	transother[type_FunctionHeader] = trans_function_header;
	transother[type_ClosureFunctionHeader] = trans_function_header;
	transother[type_ReturnPcHeader] = trans_return_pc_header;
	transother[type_ClosureHeader] = trans_boxed;
	transother[type_FuncallableInstanceHeader] = trans_boxed;
	transother[type_ByteCodeFunction] = trans_boxed;
	transother[type_ByteCodeClosure] = trans_boxed;
	transother[type_ValueCellHeader] = trans_boxed;
	transother[type_SymbolHeader] = trans_boxed;
	transother[type_BaseChar] = trans_immediate;
	transother[type_Sap] = trans_unboxed;
	transother[type_UnboundMarker] = trans_immediate;
	transother[type_WeakPointer] = trans_weak_pointer;
        transother[type_InstanceHeader] = trans_boxed;
	transother[type_Fdefn] = trans_boxed;
        transother[type_ScavengerHook] = trans_scavenger_hook;

	/* Size table */

	for (i = 0; i < 256; i++)
		sizetab[i] = size_lose;

	for (i = 0; i < 32; i++) {
		sizetab[type_EvenFixnum | (i << 3)] = size_immediate;
		sizetab[type_FunctionPointer | (i << 3)] = size_pointer;
		/* OtherImmediate0 */
		sizetab[type_ListPointer | (i << 3)] = size_pointer;
		sizetab[type_OddFixnum | (i << 3)] = size_immediate;
		sizetab[type_InstancePointer | (i << 3)] = size_pointer;
		/* OtherImmediate1 */
		sizetab[type_OtherPointer | (i << 3)] = size_pointer;
	}

	sizetab[type_Bignum] = size_unboxed;
	sizetab[type_Ratio] = size_boxed;
	sizetab[type_SingleFloat] = size_unboxed;
	sizetab[type_DoubleFloat] = size_unboxed;
#ifdef type_LongFloat
	sizetab[type_LongFloat] = size_unboxed;
#endif
	sizetab[type_Complex] = size_boxed;
#ifdef type_ComplexSingleFloat
	sizetab[type_ComplexSingleFloat] = size_unboxed;
#endif
#ifdef type_ComplexDoubleFloat
	sizetab[type_ComplexDoubleFloat] = size_unboxed;
#endif
#ifdef type_ComplexLongFloat
	sizetab[type_ComplexLongFloat] = size_unboxed;
#endif
	sizetab[type_SimpleArray] = size_boxed;
	sizetab[type_SimpleString] = size_string;
	sizetab[type_SimpleBitVector] = size_vector_bit;
	sizetab[type_SimpleVector] = size_vector;
	sizetab[type_SimpleArrayUnsignedByte2] = size_vector_unsigned_byte_2;
	sizetab[type_SimpleArrayUnsignedByte4] = size_vector_unsigned_byte_4;
	sizetab[type_SimpleArrayUnsignedByte8] = size_vector_unsigned_byte_8;
	sizetab[type_SimpleArrayUnsignedByte16] = size_vector_unsigned_byte_16;
	sizetab[type_SimpleArrayUnsignedByte32] = size_vector_unsigned_byte_32;
#ifdef type_SimpleArraySignedByte8
	sizetab[type_SimpleArraySignedByte8] = size_vector_unsigned_byte_8;
#endif
#ifdef type_SimpleArraySignedByte16
	sizetab[type_SimpleArraySignedByte16] = size_vector_unsigned_byte_16;
#endif
#ifdef type_SimpleArraySignedByte30
	sizetab[type_SimpleArraySignedByte30] = size_vector_unsigned_byte_32;
#endif
#ifdef type_SimpleArraySignedByte32
	sizetab[type_SimpleArraySignedByte32] = size_vector_unsigned_byte_32;
#endif
	sizetab[type_SimpleArraySingleFloat] = size_vector_single_float;
	sizetab[type_SimpleArrayDoubleFloat] = size_vector_double_float;
#ifdef type_SimpleArrayLongFloat
	sizetab[type_SimpleArrayLongFloat] = size_vector_long_float;
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	sizetab[type_SimpleArrayComplexSingleFloat] = size_vector_complex_single_float;
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	sizetab[type_SimpleArrayComplexDoubleFloat] = size_vector_complex_double_float;
#endif
#ifdef type_SimpleArrayComplexLongFloat
	sizetab[type_SimpleArrayComplexLongFloat] = size_vector_complex_long_float;
#endif
	sizetab[type_ComplexString] = size_boxed;
	sizetab[type_ComplexBitVector] = size_boxed;
	sizetab[type_ComplexVector] = size_boxed;
	sizetab[type_ComplexArray] = size_boxed;
	sizetab[type_CodeHeader] = size_code_header;
#if 0
	/* Shouldn't see these so just lose if it happens */
	sizetab[type_FunctionHeader] = size_function_header;
	sizetab[type_ClosureFunctionHeader] = size_function_header;
	sizetab[type_ReturnPcHeader] = size_return_pc_header;
#endif
	sizetab[type_ClosureHeader] = size_boxed;
	sizetab[type_FuncallableInstanceHeader] = size_boxed;
	sizetab[type_ValueCellHeader] = size_boxed;
	sizetab[type_SymbolHeader] = size_boxed;
	sizetab[type_BaseChar] = size_immediate;
	sizetab[type_Sap] = size_unboxed;
	sizetab[type_UnboundMarker] = size_immediate;
	sizetab[type_WeakPointer] = size_weak_pointer;
        sizetab[type_InstanceHeader] = size_boxed;
	sizetab[type_Fdefn] = size_boxed;
        sizetab[type_ScavengerHook] = size_scavenger_hook;
}



/*
 * Scan an area looking for an object which encloses the given
 * pointer. Returns the object start on success or NULL on failure.
 */
static lispobj* search_space(lispobj *start, size_t words, lispobj *pointer)
{
  while(words > 0) {
    size_t count = 1;
    lispobj thing = *start;

    /* If thing is an immediate then this is a cons */
    if (Pointerp(thing)
	|| (thing & 3) == 0 /* fixnum */
	|| TypeOf(thing) == type_BaseChar
	|| TypeOf(thing) == type_UnboundMarker)
      count = 2;
    else
      count = (sizetab[TypeOf(thing)])(start);

    /* Check if the pointer is within this object? */
    if (pointer >= start && pointer < start + count) {
      /* Found it. */
#if 0
      fprintf(stderr, "* Found %x in %x %x\n", pointer, start, thing);
#endif
      return start;
    }

    /* Round up the count */
    count = CEILING(count, 2);

    start += count;
    words -= count;
  }
  return NULL;
}

static lispobj* search_read_only_space(lispobj *pointer)
{
  lispobj* start = (lispobj*) READ_ONLY_SPACE_START;
  lispobj* end = (lispobj*) SymbolValue(READ_ONLY_SPACE_FREE_POINTER);
  if (pointer < start || pointer >= end)
    return NULL;
  return search_space(start, pointer + 2 - start, pointer);
}

static lispobj* search_static_space(lispobj *pointer)
{
  lispobj* start = (lispobj*) static_space;
  lispobj* end = (lispobj*) SymbolValue(STATIC_SPACE_FREE_POINTER);
  if (pointer < start || pointer >= end)
    return NULL;
  return search_space(start, pointer + 2 - start, pointer);
}

/*
 * Faster version for searching the dynamic space. This will work even
 * if the object is in a current allocation region.
 */
lispobj *search_dynamic_space(lispobj *pointer)
{
  int  page_index = find_page_index(pointer);
  lispobj *start;

  /* Address may be invalid - do some checks. */
  if (page_index == -1 || !PAGE_ALLOCATED(page_index))
    return NULL;
  start = (lispobj *) ((void *) page_address(page_index)
		       + page_table[page_index].first_object_offset);
  return search_space(start, pointer + 2 - start, pointer);
}

//static int valid_dynamic_space_pointer(lispobj *pointer)
int valid_dynamic_space_pointer(lispobj *pointer)
{
  lispobj *start_addr;

  /* Find the object start address */
  if ((start_addr = search_dynamic_space(pointer)) == NULL)
    return FALSE;

  /*
   * Need to allow raw pointers into Code objects for return
   * addresses. This will also pickup pointers to functions in code
   * objects.
   */
  if (TypeOf(*start_addr) == type_CodeHeader)
    /* X Could do some further checks here. */
    return TRUE;

  /*
   * If it's not a return address then it needs to be a valid lisp pointer.
   */
  if (!Pointerp((lispobj)pointer))
    return FALSE;

  /*
   * Check that the object pointed to is consistent with the pointer
   * low tag.
   */
  switch (LowtagOf((lispobj)pointer)) {
  case type_FunctionPointer:
    /*
     * Start_addr should be the enclosing code object, or a closure
     * header.
     */
    switch (TypeOf(*start_addr)) {
    case type_CodeHeader:
      /* This case is probably caught above. */
      break;
    case type_ClosureHeader:
    case type_FuncallableInstanceHeader:
    case type_ByteCodeFunction:
    case type_ByteCodeClosure:
    case type_DylanFunctionHeader:
      if ((int) pointer != (int) start_addr + type_FunctionPointer) {
	if (gencgc_verbose)
	  fprintf(stderr, "*Wf2: %x %x %x\n",
		  pointer, start_addr, *start_addr);
	return FALSE;
      }
      break;
    default:
      if (gencgc_verbose)
	fprintf(stderr, "*Wf3: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    break;
  case type_ListPointer:
    if ((int) pointer != (int) start_addr + type_ListPointer) {
      if (gencgc_verbose)
	fprintf(stderr, "*Wl1: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    /* Is it plausible cons? */
    if((Pointerp(start_addr[0])
	|| (start_addr[0] & 3) == 0 /* fixnum */
	|| TypeOf(start_addr[0]) == type_BaseChar
	|| TypeOf(start_addr[0]) == type_UnboundMarker)
       && (Pointerp(start_addr[1])
	   || (start_addr[1] & 3) == 0 /* fixnum */
	   || TypeOf(start_addr[1]) == type_BaseChar
	   || TypeOf(start_addr[1]) == type_UnboundMarker))
      break;
    else {
      if (gencgc_verbose)
	fprintf(stderr, "*Wl2: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
  case type_InstancePointer:
    if ((int) pointer != (int) start_addr + type_InstancePointer) {
      if (gencgc_verbose)
	fprintf(stderr, "*Wi1: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    if (TypeOf(start_addr[0]) != type_InstanceHeader) {
      if (gencgc_verbose)
	fprintf(stderr, "*Wi2: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    break;
  case type_OtherPointer:
    if ((int) pointer != (int) start_addr + type_OtherPointer) {
      if (gencgc_verbose)
	fprintf(stderr, "*Wo1: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    /* Is it plausible?  Not a cons. X should check the headers. */
    if(Pointerp(start_addr[0]) || (start_addr[0] & 3) == 0) {
      if (gencgc_verbose)
	fprintf(stderr, "*Wo2: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    switch (TypeOf(start_addr[0])) {
    case type_UnboundMarker:
    case type_BaseChar:
      if (gencgc_verbose)
	fprintf(stderr, "*Wo3: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;

      /* Only pointed to by function pointers? */
    case type_ClosureHeader:
    case type_FuncallableInstanceHeader:
    case type_ByteCodeFunction:
    case type_ByteCodeClosure:
    case type_DylanFunctionHeader:
      if (gencgc_verbose)
	fprintf(stderr, "*Wo4: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;

    case type_InstanceHeader:
      if (gencgc_verbose)
	fprintf(stderr, "*Wo5: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;

      /* The valid other immediate pointer objects */
    case type_SimpleVector:
    case type_Ratio:
    case type_Complex:
#ifdef type_ComplexSingleFloat
    case type_ComplexSingleFloat:
#endif
#ifdef type_ComplexDoubleFloat
    case type_ComplexDoubleFloat:
#endif
#ifdef type_ComplexLongFloat
    case type_ComplexLongFloat:
#endif
    case type_SimpleArray:
    case type_ComplexString:
    case type_ComplexBitVector:
    case type_ComplexVector:
    case type_ComplexArray:
    case type_ValueCellHeader:
    case type_SymbolHeader:
    case type_Fdefn:
    case type_CodeHeader:
    case type_Bignum:
    case type_SingleFloat:
    case type_DoubleFloat:
#ifdef type_LongFloat
    case type_LongFloat:
#endif
    case type_SimpleString:
    case type_SimpleBitVector:
    case type_SimpleArrayUnsignedByte2:
    case type_SimpleArrayUnsignedByte4:
    case type_SimpleArrayUnsignedByte8:
    case type_SimpleArrayUnsignedByte16:
    case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
    case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
    case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
    case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
    case type_SimpleArraySignedByte32:
#endif
    case type_SimpleArraySingleFloat:
    case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
    case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
    case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
    case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
    case type_SimpleArrayComplexLongFloat:
#endif
    case type_Sap:
    case type_WeakPointer:
    case type_ScavengerHook:
      break;

    default:
      if (gencgc_verbose)
	fprintf(stderr, "*Wo6: %x %x %x\n", pointer, start_addr, *start_addr);
      return FALSE;
    }
    break;
  default:
    if (gencgc_verbose)
      fprintf(stderr, "*W?: %x %x %x\n", pointer, start_addr, *start_addr);
    return FALSE;
  }

  /* Looks good */
  return TRUE;
}


/*
 * Adjust large bignum and vector objects.  This will adjust the
 * allocated region if the size has shrunk, and move unboxed objects
 * into unboxed pages. The pages are not promoted here, and the
 * promoted region is not added to the new_regions; this is really
 * only designed to be called from preserve_pointer. Shouldn't fail if
 * this is missed, just may delay the moving of objects to unboxed
 * pages, and the freeing of pages.
 */
static void maybe_adjust_large_object(lispobj *where)
{
  int first_page;
  int nwords;
  int remaining_bytes;
  int next_page;
  int bytes_freed;
  int old_bytes_used;
  int unboxed;
  int mmask, mflags;

  /* Check if it's a vector or bignum object. */
  switch (TypeOf(where[0])) {
  case type_SimpleVector:
    unboxed = FALSE;
    break;
  case type_Bignum:
  case type_SimpleString:
  case type_SimpleBitVector:
  case type_SimpleArrayUnsignedByte2:
  case type_SimpleArrayUnsignedByte4:
  case type_SimpleArrayUnsignedByte8:
  case type_SimpleArrayUnsignedByte16:
  case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
  case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
  case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
  case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
  case type_SimpleArraySignedByte32:
#endif
  case type_SimpleArraySingleFloat:
  case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
  case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
  case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
  case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
  case type_SimpleArrayComplexLongFloat:
#endif
    unboxed = TRUE;
    break;
  default:
    return;
  }

  /* Find its current size. */
  nwords = (sizetab[TypeOf(where[0])])(where);

  first_page = find_page_index((void *) where);
  gc_assert(first_page >= 0);

  /*
   * Note: Any page write protection must be removed, else a later
   * scavenge_newspace may incorrectly not scavenge these pages.  This
   * would not be necessary if they are added to the new areas, but
   * lets do it for them all (they'll probably be written anyway?).
   */

  gc_assert(page_table[first_page].first_object_offset == 0);

  next_page = first_page;
  remaining_bytes = nwords * 4;
  while (remaining_bytes > PAGE_SIZE) {
    gc_assert(PAGE_GENERATION(next_page) == from_space);
    gc_assert(PAGE_ALLOCATED(next_page));
    gc_assert(PAGE_LARGE_OBJECT(next_page));
    gc_assert(page_table[next_page].first_object_offset ==
	      PAGE_SIZE * (first_page - next_page));
    gc_assert(page_table[next_page].bytes_used == PAGE_SIZE);

    PAGE_FLAGS_UPDATE(next_page, PAGE_UNBOXED_MASK,
		      unboxed << PAGE_UNBOXED_SHIFT);

    /*
     * Shouldn't be write protected at this stage. Essential that the
     * pages aren't.
     */
    gc_assert(!PAGE_WRITE_PROTECTED(next_page));
    remaining_bytes -= PAGE_SIZE;
    next_page++;
  }

  /*
   * Now only one page remains, but the object may have shrunk so
   * there may be more unused pages which will be freed.
   */

  /* Object may have shrunk but shouldn't have grown - check. */
  gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

  page_table[next_page].flags |= PAGE_ALLOCATED_MASK;
  PAGE_FLAGS_UPDATE(next_page, PAGE_UNBOXED_MASK,
		    unboxed << PAGE_UNBOXED_SHIFT);
  gc_assert(PAGE_UNBOXED(next_page) == PAGE_UNBOXED(first_page));

  /* Adjust the bytes_used. */
  old_bytes_used = page_table[next_page].bytes_used;
  page_table[next_page].bytes_used = remaining_bytes;

  bytes_freed = old_bytes_used - remaining_bytes;

  mmask = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | PAGE_GENERATION_MASK;
  mflags = PAGE_ALLOCATED_MASK | PAGE_LARGE_OBJECT_MASK | from_space;

  /* Free any remaining pages; needs care. */
  next_page++;
  while (old_bytes_used == PAGE_SIZE &&
	 PAGE_FLAGS(next_page, mmask) == mflags &&
	 page_table[next_page].first_object_offset == PAGE_SIZE * (first_page
								   - next_page)) {
    /*
     * Checks out OK, free the page. Don't need to bother zeroing
     * pages as this should have been done before shrinking the
     * object. These pages shouldn't be write protected as they should
     * be zero filled.
     */
    gc_assert(!PAGE_WRITE_PROTECTED(next_page));

    old_bytes_used = page_table[next_page].bytes_used;
    page_table[next_page].flags &= ~PAGE_ALLOCATED_MASK;
    page_table[next_page].bytes_used = 0;
    bytes_freed += old_bytes_used;
    next_page++;
  }

  if (gencgc_verbose && bytes_freed > 0)
    fprintf(stderr, "* adjust_large_object freed %d\n", bytes_freed);

  generations[from_space].bytes_allocated -= bytes_freed;
  bytes_allocated -= bytes_freed;

  return;
}


/*
 * Take a possible pointer to a list object and mark the page_table so
 * that it will not need changing during a GC.
 *
 * This involves locating the page it points to, then backing up to
 * the first page that has its first object start at offset 0, and
 * then marking all pages dont_move from the first until a page that
 * ends by being full, or having free gen.
 *
 * This ensures that objects spanning pages are not broken.
 *
 * It is assumed that all the page static flags have been cleared at
 * the start of a GC.
 *
 * Also assumes the current gc_alloc region has been flushed and the
 * tables updated.
 */
static void preserve_pointer(void *addr)
{
  int addr_page_index = find_page_index(addr);
  int first_page;
  int i;
  unsigned region_unboxed;

  /* Address is quite likely to have been invalid - do some checks. */
  if (addr_page_index == -1
      || !PAGE_ALLOCATED(addr_page_index)
      || page_table[addr_page_index].bytes_used == 0
      || PAGE_GENERATION(addr_page_index) != from_space
      /* Skip if already marked dont_move */
      || PAGE_DONT_MOVE(addr_page_index))
    return;

  region_unboxed = PAGE_UNBOXED(addr_page_index);

  /* Check the offset within the page */
  if (((int) addr & 0xfff) > page_table[addr_page_index].bytes_used)
    return;

  if (enable_pointer_filter && !valid_dynamic_space_pointer(addr))
    return;

  /*
   * Work backwards to find a page with a first_object_offset of 0.
   * The pages should be contiguous with all bytes used in the same
   * gen. Assumes the first_object_offset is negative or zero.
   */
  first_page = addr_page_index;
  while (page_table[first_page].first_object_offset != 0) {
    first_page--;
    /* Do some checks */
    gc_assert(page_table[first_page].bytes_used == PAGE_SIZE);
    gc_assert(PAGE_GENERATION(first_page) == from_space);
    gc_assert(PAGE_ALLOCATED(first_page));
    gc_assert(PAGE_UNBOXED(first_page) == region_unboxed);
  }

  /*
   * Adjust any large objects before promotion as they won't be copied
   * after promotion.
   */
  if (PAGE_LARGE_OBJECT(first_page)) {
    maybe_adjust_large_object(page_address(first_page));
    /*
     * If a large object has shrunk then addr may now point to a free
     * adea in which case it's ignored here. Note it gets through the
     * valid pointer test above because the tail looks like conses.
     */
    if (!PAGE_ALLOCATED(addr_page_index)
	|| page_table[addr_page_index].bytes_used == 0
	/* Check the offset within the page */
	|| ((int) addr & 0xfff) > page_table[addr_page_index].bytes_used) {
      fprintf(stderr, "*W ignore pointer 0x%x to freed area of large object\n",
	      addr);
      return;
    }
    /* May have moved to unboxed pages. */
    region_unboxed = PAGE_UNBOXED(first_page);
  }

  /*
   * Now work forward until the end of this contiguous area is found,
   * marking all pages as dont_move.
   */
  for (i = first_page; ;i++) {
    gc_assert(PAGE_ALLOCATED(i));
    gc_assert(PAGE_UNBOXED(i) == region_unboxed);

    /* Mark the page static */
    page_table[i].flags |= PAGE_DONT_MOVE_MASK;
#if 0
    fprintf(stderr, "#%d,", i);
#endif

    /*
     * Move the page to the new_space. XX I'd rather not do this but
     * the GC logic is not quite able to copy with the static pages
     * remaining in the from space. This also requires the generation
     * bytes_allocated counters be updated.
     */
    PAGE_FLAGS_UPDATE(i, PAGE_GENERATION_MASK, new_space);
    generations[new_space].bytes_allocated += page_table[i].bytes_used;
    generations[from_space].bytes_allocated -= page_table[i].bytes_used;

    /*
     * Essential that the pages are not write protected as they may
     * have pointers into the old-space which need
     * scavenging. Shouldn't be write protected at this stage.
     */
    gc_assert(!PAGE_WRITE_PROTECTED(i));

    /* Check if this is the last page in this contiguous block */
    if (page_table[i].bytes_used < PAGE_SIZE
	/* Or it is PAGE_SIZE and is the last in the block */
	|| !PAGE_ALLOCATED(i + 1)
	|| page_table[i + 1].bytes_used == 0 /* Next page free */
	|| PAGE_GENERATION(i + 1) != from_space /* Diff. gen */
	|| page_table[i + 1].first_object_offset == 0)
      break;
  }

  /* Check that the page is now static */
  gc_assert(PAGE_DONT_MOVE(addr_page_index));

  return;
}

#ifdef CONTROL_STACKS
/* Scavenge the thread stack conservative roots. */
static void scavenge_thread_stacks(void)
{
  lispobj thread_stacks = SymbolValue(CONTROL_STACKS);
  int type = TypeOf(thread_stacks);

  if (LowtagOf(thread_stacks) == type_OtherPointer) {
    struct vector *vector = (struct vector *) PTR(thread_stacks);
    int length, i;
    if (TypeOf(vector->header) != type_SimpleVector)
      return;
    length = fixnum_value(vector->length);    
    for (i = 0; i < length; i++) {
      lispobj stack_obj = vector->data[i];
      if (LowtagOf(stack_obj) == type_OtherPointer) {
	struct vector *stack = (struct vector *) PTR(stack_obj);
	int vector_length;
	if (TypeOf(stack->header) != type_SimpleArrayUnsignedByte32)
	  return;
	vector_length = fixnum_value(stack->length);
	if (gencgc_verbose > 1 && vector_length <= 0)
	  fprintf(stderr, "*W control stack vector length %d\n",
		  vector_length);
	if (vector_length > 0) {
	  unsigned int stack_pointer = stack->data[0];
	  if (stack_pointer < control_stack ||
	      stack_pointer > control_stack_end)
	    fprintf(stderr, "*E Invalid stack pointer %x\n", stack_pointer);
	  if (stack_pointer > control_stack &&
	      stack_pointer < control_stack_end) {
	    unsigned int length = ((int) control_stack_end - stack_pointer) / 4;
	    int j;
	    if (length >= vector_length)
	      fprintf(stderr, "*E Invalid stack size %d >= vector length %d\n",
		      length, vector_length);
	    if (gencgc_verbose > 1)
	      fprintf(stderr, "Scavenging %d words of control stack %d of length %d words.\n",
		      length,i, vector_length);
	    for (j = 0; j < length; j++)
	      preserve_pointer((void *) stack->data[1 + j]);
	  }
	}
      }
    }
  }
}
#endif


/*
 * If the given page is not write protected, then scan it for pointers
 * to younger generations or the top temp. generation, if no
 * suspicious pointers are found then the page is write protected.
 *
 * Care is taken to check for pointers to the current gc_alloc region
 * if it is a younger generation or the temp. generation. This frees
 * the caller from doing a gc_alloc_update_page_tables. Actually the
 * gc_alloc_generation does not need to be checked as this is only
 * called from scavenge_generation when the gc_alloc generation is
 * younger, so it just checks if there is a pointer to the current
 * region.
 *
 * It returns 1 if the page was write protected, else 0.
 */
static int update_page_write_prot(unsigned page)
{
  int gen = PAGE_GENERATION(page);
  int j;
  int wp_it = 1;
  void **page_addr = (void **) page_address(page);
  int num_words = page_table[page].bytes_used / 4;

  /* Shouldn't be a free page. */
  gc_assert(PAGE_ALLOCATED(page));
  gc_assert(page_table[page].bytes_used != 0);

  /* Skip if it's already write protected or an unboxed page. */
  if (PAGE_WRITE_PROTECTED(page) || PAGE_UNBOXED(page))
    return 0;

  /*
   * Scan the page for pointers to younger generations or the top
   * temp. generation.
   */

  for (j = 0; j < num_words; j++) {
    void *ptr = *(page_addr + j);
    int index = find_page_index(ptr);

    /* Check that it's in the dynamic space */
    if (index != -1)
      if (/* Does it point to a younger or the temp. generation? */
	  (PAGE_ALLOCATED(index)
	   && page_table[index].bytes_used != 0
	   && (PAGE_GENERATION(index) < gen
	       || PAGE_GENERATION(index) == NUM_GENERATIONS))

	  /* Or does it point within a current gc_alloc region? */
	  || (boxed_region.start_addr <= ptr
	      && ptr <= boxed_region.free_pointer)
	  || (unboxed_region.start_addr <= ptr
	      && ptr <= unboxed_region.free_pointer)) {
	wp_it = 0;
	break;
      }
  }

  if (wp_it == 1) {
    /* Write protect the page */
#if 0
    fprintf(stderr, "* WP page %d of gen %d\n", page, gen);
#endif

    os_protect((void *) page_addr, PAGE_SIZE,
	       OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

    /* Note the page as protected in the page tables */
    page_table[page].flags |= PAGE_WRITE_PROTECTED_MASK;
  }

  return wp_it;
}

/*
 * Scavenge a generation.
 *
 * This will not resolve all pointers when generation is the new
 * space, as new objects may be added which are not check here - use
 * scavenge_newspace generation.
 *
 * Write protected pages should not have any pointers to the
 * from_space so do need scavenging; Thus write protected pages are
 * not always scavenged. There is some code to check that these pages
 * are not written; but to check fully the write protect pages need to
 * be scavenged by disabling the code to skip them.
 *
 * Under the current scheme when a generation is GCed the younger
 * generations will be empty. So, when a generation is being GCed it
 * is only necessary to scavenge the older generations for pointers
 * not the younger. So a page that does not have pointers to younger
 * generations does not need to be scavenged.
 *
 * The write protection can be used to note pages that don't have
 * pointers to younger pages. But pages can be written without having
 * pointers to younger generations. After the pages are scavenged here
 * they can be scanned for pointers to younger generations and if
 * there are none the page can be write protected.
 *
 * One complication is when the newspace is the top temp. generation.
 *
 * Enabling SC_GEN_CK scavenges the write protect pages and checks
 * that none were written, which they shouldn't be as they should have
 * no pointers to younger generations.  This breaks down for weak
 * pointers as the objects contain a link to the next and are written
 * if a weak pointer is scavenged. Still it's a useful check.
 */

static void scavenge_generation(int generation)
{
  int i;
  int num_wp = 0;

#define SC_GEN_CK 0
#if SC_GEN_CK
  /* Clear the write_protected_cleared flags on all pages */
  for (i = 0; i < dynamic_space_pages; i++)
    page_table[i].flags &= ~PAGE_WRITE_PROTECTED_CLEADED_MASK;
#endif

  for (i = 0; i < last_free_page; i++) {
    if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == generation) {
      int last_page;

      /* This should be the start of a contiguous block */
      gc_assert(page_table[i].first_object_offset == 0);

      /*
       * Need to find the full extent of this contiguous block in case
       * objects span pages.
       */

      /*
       * Now work forward until the end of this contiguous area is
       * found. Small areas are preferred as there is a better chance
       * of its pages being write protected.
       */
      for (last_page = i; ;last_page++)
	/* Check if this is the last page in this contiguous block */
	if (page_table[last_page].bytes_used < PAGE_SIZE
	    /* Or it is PAGE_SIZE and is the last in the block */
	    || !PAGE_ALLOCATED(last_page + 1)
	    || PAGE_UNBOXED(last_page + 1)
	    || page_table[last_page + 1].bytes_used == 0
	    || PAGE_GENERATION(last_page + 1) != generation
	    || page_table[last_page + 1].first_object_offset == 0)
	  break;

      /*
       * Do a limited check for write_protected pages. If all pages
       * are write_protected then no need to scavenge.
       */
      {
	int j, all_wp = 1;
	for (j = i; j <= last_page; j++)
	  if (!PAGE_WRITE_PROTECTED(j)) {
	    all_wp = 0;
	    break;
	  }
#if !SC_GEN_CK
	if (all_wp == 0)
#endif
	  {
	    scavenge(page_address(i), (page_table[last_page].bytes_used
				       + PAGE_SIZE * (last_page - i)) / 4);

	    /*
	     * Now scan the pages and write protect those that don't
	     * have pointers to younger generations.
	     */
	    if (enable_page_protection)
	      for (j = i; j <= last_page; j++)
		num_wp += update_page_write_prot(j);
	  }
      }
      i = last_page;
    }
  }

  if (gencgc_verbose > 1 && num_wp != 0)
    fprintf(stderr, "Write protected %d pages within generation %d\n",
	    num_wp, generation);

#if SC_GEN_CK
  /*
   * Check that none of the write_protected pages in this generation
   * have been written to.
   */
  for (i = 0; i < dynamic_space_pages; i++)
    if (PAGE_ALLOCATED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == generation
	&& PAGE_WRITE_PROTECTED_CLEARED(i)) {
      fprintf(stderr, "*** scavenge_generation %d: write protected page %d written to?\n", generation, i);
      fprintf(stderr, "*** page: bytes_used=%d first_object_offset=%d dont_move=%d\n",
	      page_table[i].bytes_used,
	      page_table[i].first_object_offset,
	      PAGE_DONT_MOVE(i));
    }
#endif

}


/*
 * Scavenge a newspace generation. As it is scavenged new objects may
 * be allocated to it; these will also need to be scavenged. This
 * repeats until there are no more objects unscavenged in the newspace
 * generation.
 *
 * To help improve the efficiency, areas written are recorded by
 * gc_alloc and only these scavenged. Sometimes a little more will be
 * scavenged, but this causes no harm. An easy check is done that the
 * scavenged bytes equals the number allocated in the previous
 * scavenge.
 *
 * Write protected pages are not scanned except if they are marked
 * don't move in which case they may have been promoted and still have
 * pointers to the from space.
 *
 * Write protect pages could potentially be written by alloc however
 * to avoid having to handle re-scavenging of write_protect pages
 * gc_alloc does not write to write_protected pages.
 *
 * New areas of objects allocated are record alternatively in the two
 * new_areas arrays below.
 */
static struct new_area new_areas_1[NUM_NEW_AREAS];
static struct new_area new_areas_2[NUM_NEW_AREAS];

/*
 * Do one full scan of the new space generation. This is not enough to
 * complete the job as new objects may be added to the generation in
 * the process which are not scavenged.
 */
static void scavenge_newspace_generation_one_scan(int generation)
{
  int i;

#if 0
  fprintf(stderr, "Starting one full scan of newspace generation %d\n",
	  generation);
#endif

  for (i = 0; i < last_free_page; i++) {
    if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == generation
	&& (!PAGE_WRITE_PROTECTED(i)
	    /* This may be redundant as WP is now cleared before promotion. */
	    || PAGE_DONT_MOVE(i))) {
      int last_page;

      /* The scavenge will start at the first_object_offset of page i */

      /*
       * Need to find the full extent of this contiguous block in case
       * objects span pages.
       */

      /*
       * Now work forward until the end of this contiguous area is
       * found. Small areas are preferred as there is a better chance
       * of its pages being write protected.
       */
      for (last_page = i; ; last_page++)
	/* Check if this is the last page in this contiguous block */
	if (page_table[last_page].bytes_used < PAGE_SIZE
	    /* Or it is PAGE_SIZE and is the last in the block */
	    || !PAGE_ALLOCATED(last_page + 1)
	    || PAGE_UNBOXED(last_page + 1)
	    || page_table[last_page + 1].bytes_used == 0
	    || PAGE_GENERATION(last_page + 1) != generation
	    || page_table[last_page + 1].first_object_offset == 0)
	  break;

      /*
       * Do a limited check for write_protected pages. If all pages
       * are write_protected then no need to scavenge. Except if the
       * pages are marked dont_move.
       */
      {
	int j, all_wp = 1;
	for (j = i; j <= last_page; j++)
	  if (!PAGE_WRITE_PROTECTED(j) || PAGE_DONT_MOVE(j)) {
	    all_wp = 0;
	    break;
	  }
#if !SC_NS_GEN_CK
	if (all_wp == 0) 
#endif
	  {
	    int size;

	    /* Calc. the size */
	    if (last_page == i)
	      size = (page_table[last_page].bytes_used
		      - page_table[i].first_object_offset) / 4;
	    else
	      size = (page_table[last_page].bytes_used +
		      PAGE_SIZE * (last_page - i) -
		      page_table[i].first_object_offset) / 4;

	    {
#if SC_NS_GEN_CK
	      int a1 = bytes_allocated;
#endif		    
#if 0
	      fprintf(stderr, "scavenge(%x,%d)\n",
		      page_address(i) + page_table[i].first_object_offset,
		      size);
#endif

	      new_areas_ignore_page = last_page;

	      scavenge(page_address(i) + page_table[i].first_object_offset,
		       size);

#if SC_NS_GEN_CK
	      /* Flush the alloc regions updating the tables. */
	      gc_alloc_update_page_tables(0, &boxed_region);
	      gc_alloc_update_page_tables(1, &unboxed_region);

	      if (all_wp != 0 && a1 != bytes_allocated) {
		fprintf(stderr, "*** scav.new.gen. alloc'ed over %d to %d\n",
			i, last_page);
		fprintf(stderr, "*** page: bytes_used=%d first_object_offset=%d dont_move=%d wp=%d wpc=%d\n",
			page_table[i].bytes_used,
			page_table[i].first_object_offset,
			PAGE_DONT_MOVE(i),
			PAGE_WRITE_PROTECTED(i),
			PAGE_PROTECTED_CLEARED(i));
	      }
#endif
	    }
	  }
      }

      i = last_page;
    }
  }
}

/* Do a complete scavenge of the newspace generation */
static void scavenge_newspace_generation(int generation)
{
  int i;

  /* The new_areas array currently being written to by gc_alloc */
  struct new_area  (*current_new_areas)[] = &new_areas_1;
  int current_new_areas_index;

  /* The new_areas created but the previous scavenge cycle */
  struct new_area  (*previous_new_areas)[] = NULL;
  int previous_new_areas_index;

#define SC_NS_GEN_CK 0
#if SC_NS_GEN_CK
  /* Clear the write_protected_cleared flags on all pages */
  for (i = 0; i < dynamic_space_pages; i++)
    page_table[i].flags &= ~PAGE_WRITE_PROTECTED_CLEARED;
#endif

  /* Flush the current regions updating the tables. */
  gc_alloc_update_page_tables(0, &boxed_region);
  gc_alloc_update_page_tables(1, &unboxed_region);

  /* Turn on the recording of new areas by gc_alloc. */
  new_areas = current_new_areas;
  new_areas_index = 0;

  /*
   * Don't need to record new areas that get scavenged anyway during
   * scavenge_newspace_generation_one_scan.
   */
  record_new_objects = 1;

  /* Start with a full scavenge */
  scavenge_newspace_generation_one_scan(generation);

  /* Record all new areas now. */
  record_new_objects = 2;

  /* Flush the current regions updating the tables. */
  gc_alloc_update_page_tables(0, &boxed_region);
  gc_alloc_update_page_tables(1, &unboxed_region);

  /* Grab new_areas_index */
  current_new_areas_index = new_areas_index;

#if 0
  fprintf(stderr, "First scan finished; current_new_areas_index=%d\n",
	  current_new_areas_index);
#endif

  while (current_new_areas_index > 0) {
    /* Move the current to the previous new areas */
    previous_new_areas = current_new_areas;
    previous_new_areas_index = current_new_areas_index;

    /*
     * Scavenge all the areas in previous new areas. Any new areas
     * allocated are saved in current_new_areas.
     */

    /*
     * Allocate an array for current_new_areas; alternating between
     * new_areas_1 and 2.
     */
    if (previous_new_areas == &new_areas_1)
      current_new_areas = &new_areas_2;
    else
      current_new_areas = &new_areas_1;

    /* Setup for gc_alloc */
    new_areas = current_new_areas;
    new_areas_index = 0;

    /* Check if previous_new_areas had overflowed */
    if (previous_new_areas_index >= NUM_NEW_AREAS) {
      /*
       * New areas of objects allocated have been lost so need to do a
       * full scan to be sure! If this becomes a problem try
       * increasing NUM_NEW_AREAS.
       */
      if (gencgc_verbose)
	fprintf(stderr, "** new_areas overflow, doing full scavenge\n");

      /*
       * Don't need to record new areas that get scavenge anyway
       * during scavenge_newspace_generation_one_scan.
       */
      record_new_objects = 1;

      scavenge_newspace_generation_one_scan(generation);

      /* Record all new areas now. */
      record_new_objects = 2;

      /* Flush the current regions updating the tables. */
      gc_alloc_update_page_tables(0, &boxed_region);
      gc_alloc_update_page_tables(1, &unboxed_region);
    } else {
      /* Work through previous_new_areas */
      for (i = 0; i < previous_new_areas_index; i++) {
	int page = (*previous_new_areas)[i].page;
	int offset = (*previous_new_areas)[i].offset;
	int size = (*previous_new_areas)[i].size / 4;
	gc_assert((*previous_new_areas)[i].size % 4 == 0);

#if 0	
	fprintf(stderr, "*S page %d offset %d size %d\n",page,offset,size*4);
#endif
	scavenge(page_address(page)+offset, size);
      }

      /* Flush the current regions updating the tables. */
      gc_alloc_update_page_tables(0, &boxed_region);
      gc_alloc_update_page_tables(1, &unboxed_region);
    }

    /* Grab new_areas_index */
    current_new_areas_index = new_areas_index;

#if 0
    fprintf(stderr, "Re-scan finished; current_new_areas_index=%d\n",
	    current_new_areas_index);
#endif
  }

  /* Turn off recording of areas allocated by gc_alloc */
  record_new_objects = 0;

#if SC_NS_GEN_CK
  /*
   * Check that none of the write_protected pages in this generation
   * have been written to.
   */
  for (i = 0; i < dynamic_space_pages; i++)
    if (PAGE_ALLOCATED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == generation
	&& PAGE_WRITE_PROTECTED_CLEARED(i)
	&& !PAGE_DONT_MOVE(i))
      fprintf(stderr, "*** scav.new.gen. %d: write protected page %d written to? dont_move=%d\n",
	      generation, i, PAGE_DONT_MOVE(i));
#endif
}



/*
 * Un-write-protect all the pages in from_space. This is done at the
 * start of a GC else there may be many page faults while scavenging
 * the newspace (I've seen drive the system time to 99%). These pages
 * would need to be unprotected anyway before unmapping in
 * free_oldspace; not sure what effect this has on paging?.
 */
//static void unprotect_oldspace(void)
void unprotect_oldspace(void)
{
  int i;

  for (i = 0; i < last_free_page; i++)
    if (PAGE_ALLOCATED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == from_space) {
      void *page_start;

      page_start = (void *) page_address(i);

      /*
       * Remove any write protection.  Should be able to rely on the
       * WP flag to avoid redundant calls.
       */
      if (PAGE_WRITE_PROTECTED(i)) {
	os_protect(page_start, PAGE_SIZE, OS_VM_PROT_ALL);
	page_table[i].flags &= ~PAGE_WRITE_PROTECTED_MASK;
      }
    }
}

/*
 * Work through all the pages and free any in from_space.  This
 * assumes that all objects have been copied or promoted to an older
 * generation. Bytes_allocated and the generation bytes_allocated
 * counter are updated.  The number of bytes freed is returned.
 */
extern void i586_bzero(void *addr, int nbytes);
static int free_oldspace(void)
{
  int bytes_freed = 0;
  int first_page, last_page;

  first_page = 0;

  do {
    /* Find a first page for the next region of pages. */
    while (first_page < last_free_page
	   && (!PAGE_ALLOCATED(first_page)
	       || page_table[first_page].bytes_used == 0
	       || PAGE_GENERATION(first_page) != from_space))
      first_page++;

    if (first_page >= last_free_page)
      break;

    /* Find the last page of this region. */
    last_page = first_page;

    do {
      /* Free the page */
      bytes_freed += page_table[last_page].bytes_used;
      generations[PAGE_GENERATION(last_page)].bytes_allocated -= page_table[last_page].bytes_used;
      page_table[last_page].flags &= ~PAGE_ALLOCATED_MASK;
      page_table[last_page].bytes_used = 0;

      /*
       * Remove any write protection.  Should be able to rely on the
       * WP flag to avoid redundant calls.
       */
      {
	void  *page_start = (void *)page_address(last_page);

	if (PAGE_WRITE_PROTECTED(last_page)) {
	  os_protect(page_start, PAGE_SIZE, OS_VM_PROT_ALL);
	  page_table[last_page].flags &= ~PAGE_WRITE_PROTECTED_MASK;
	}
      }
      last_page++;
    }
    while (last_page < last_free_page
	   && PAGE_ALLOCATED(last_page)
	   && page_table[last_page].bytes_used != 0
	   && PAGE_GENERATION(last_page) == from_space);

    /* Zero pages from first_page to (last_page - 1) */
    if (gencgc_unmap_zero) {
      void *page_start, *addr;

      page_start = (void *) page_address(first_page);

      os_invalidate(page_start, PAGE_SIZE * (last_page - first_page));
      addr = os_validate(page_start, PAGE_SIZE * (last_page - first_page));
      if(addr == NULL || addr != page_start)
	fprintf(stderr, "gc_zero: page moved, 0x%08x ==> 0x%08x!\n",
		page_start, addr);
    } else {
      int *page_start;

      page_start = (int *) page_address(first_page);
      i586_bzero(page_start, PAGE_SIZE * (last_page - first_page));
    }

    first_page = last_page;
  }
  while(first_page < last_free_page);

  bytes_allocated -= bytes_freed;
  return bytes_freed;
}



/* Print out some information about a pointer at the given address. */
static void print_ptr(lispobj *addr)
{
  /* If addr is in the dynamic space then print out the page information. */
  int pi1 = find_page_index((void*) addr);

  if(pi1 != -1)
    fprintf(stderr, "  %x: page %d  alloc %d unboxed %d gen %d  bytes_used %d  offset %d  dont_move %d\n",
	    addr, pi1,
	    PAGE_ALLOCATED(pi1),
	    PAGE_UNBOXED(pi1),
	    PAGE_GENERATION(pi1),
	    page_table[pi1].bytes_used,
	    page_table[pi1].first_object_offset,
	    PAGE_DONT_MOVE(pi1));
  fprintf(stderr, "  %x %x %x %x (%x) %x %x %x %x\n",
	  *(addr - 4), *(addr - 3), *(addr - 2), *(addr - 1), *(addr - 0),
	  *(addr + 1), *(addr + 2), *(addr + 3), *(addr + 4));
}

extern int  undefined_tramp;

void verify_space(lispobj*start, size_t words)
//static void verify_space(lispobj*start, size_t words)
{
  int dynamic_space = (find_page_index((void*) start) != -1);
  int readonly_space = (READ_ONLY_SPACE_START <= (int) start &&
		(int) start < SymbolValue(READ_ONLY_SPACE_FREE_POINTER));

  while(words > 0) {
    size_t count = 1;
    lispobj thing = *(lispobj*) start;

    if(Pointerp(thing)) {
      int page_index = find_page_index((void*)thing);
      int to_readonly_space = (READ_ONLY_SPACE_START <= thing &&
		      thing < SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
      int to_static_space = ((int) static_space <= thing &&
			 thing < SymbolValue(STATIC_SPACE_FREE_POINTER));

      /* Does it point to the dynamic space? */
      if(page_index != -1) {
	/*
	 * If it's within the dynamic space it should point to a used
	 * page.  X Could check the offset too.
	 */
	if (PAGE_ALLOCATED(page_index)
	    && page_table[page_index].bytes_used == 0) {
	  fprintf(stderr, "*** Ptr %x @ %x sees free page.\n", thing, start);
	  print_ptr(start);
	}

	/* Check that it doesn't point to a forwarding pointer! */
	if (*((lispobj *) PTR(thing)) == 0x01) {
	  fprintf(stderr, "*** Ptr %x @ %x sees forwarding ptr.\n",
		  thing, start);
	  print_ptr(start);
	}

	/*
	 * Check that its not in the RO space as it would then be a
	 * pointer from the RO to the dynamic space.
	 */
	if (readonly_space) {
	  fprintf(stderr, "*** Ptr to dynamic space %x, from RO space %x\n",
		  thing, start);
	  print_ptr(start);
	}

	/*
	 * Does it point to a plausible object? This check slows it
	 * down a lot.
	 */
#if 0
	if (!valid_dynamic_space_pointer((lispobj *) thing)) {
	  fprintf(stderr, "*** Ptr %x to invalid object %x\n", thing, start);
	  print_ptr(start);
	}
#endif
      } else
	/* Verify that it points to another valid space */
	if (!to_readonly_space && !to_static_space
	    && thing != (int) &undefined_tramp) {
	  fprintf(stderr, "*** Ptr %x @ %x sees Junk\n", thing, start);
	  print_ptr(start);
	}
    } else
      if (thing & 0x3) /* Skip fixnums */
	switch(TypeOf(*start)) {
	  /* Boxed objects. */
	case type_SimpleVector:
	case type_Ratio:
	case type_Complex:
	case type_SimpleArray:
	case type_ComplexString:
	case type_ComplexBitVector:
	case type_ComplexVector:
	case type_ComplexArray:
	case type_ClosureHeader:
	case type_FuncallableInstanceHeader:
	case type_ByteCodeFunction:
	case type_ByteCodeClosure:
	case type_DylanFunctionHeader:
	case type_ValueCellHeader:
	case type_SymbolHeader:
	case type_BaseChar:
	case type_UnboundMarker:
	case type_InstanceHeader:
	case type_Fdefn:
	case type_ScavengerHook:
	  count = 1;
	  break;

	case type_CodeHeader:
	  {
	    lispobj object = *start;
	    struct code *code;
	    int nheader_words, ncode_words, nwords;
	    lispobj fheaderl;
	    struct function *fheaderp;

	    code = (struct code *) start;

	    /* Check that it's not in the dynamic space. */
	    if (dynamic_space
		/*
		 * It's ok if it's byte compiled code. The trace table
		 * offset will be a fixnum if it's x86 compiled code - check.
		 */
		&& !(code->trace_table_offset & 0x3)
		/* Only when enabled */
		&& verify_dynamic_code_check)
	      fprintf(stderr, "*** Code object at %x in the dynamic space\n",
		      start);

	    ncode_words = fixnum_value(code->code_size);
	    nheader_words = HeaderValue(object);
	    nwords = ncode_words + nheader_words;
	    nwords = CEILING(nwords, 2);
	    /* Scavenge the boxed section of the code data block */
	    verify_space(start + 1, nheader_words - 1);

	    /*
	     * Scavenge the boxed section of each function object in
	     * the code data block.
	     */
	    fheaderl = code->entry_points;
	    while (fheaderl != NIL) {
	      fheaderp = (struct function *) PTR(fheaderl);
	      gc_assert(TypeOf(fheaderp->header) == type_FunctionHeader);
	      verify_space(&fheaderp->name, 1);
	      verify_space(&fheaderp->arglist, 1);
	      verify_space(&fheaderp->type, 1);
	      fheaderl = fheaderp->next;
	    }
	    count = nwords;
	    break;
	  }

	/* Unboxed objects */
	case type_Bignum:
	case type_SingleFloat:
	case type_DoubleFloat:
#ifdef type_ComplexLongFloat
	case type_LongFloat:
#endif
#ifdef type_ComplexSingleFloat
	case type_ComplexSingleFloat:
#endif
#ifdef type_ComplexDoubleFloat
	case type_ComplexDoubleFloat:
#endif
#ifdef type_ComplexLongFloat
	case type_ComplexLongFloat:
#endif
	case type_SimpleString:
	case type_SimpleBitVector:
	case type_SimpleArrayUnsignedByte2:
	case type_SimpleArrayUnsignedByte4:
	case type_SimpleArrayUnsignedByte8:
	case type_SimpleArrayUnsignedByte16:
	case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
	case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
	case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
	case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
	case type_SimpleArraySignedByte32:
#endif
	case type_SimpleArraySingleFloat:
	case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayComplexLongFloat
	case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
	case type_SimpleArrayComplexLongFloat:
#endif
	case type_Sap:
	case type_WeakPointer:
	  count = (sizetab[TypeOf(*start)])(start);
	  break;

	default:
	  gc_abort();
	}
    start += count;
    words -= count;
  }
}

// FIX
void verify_space1(int page, size_t words)
{
  verify_space(page_address(page), words);
}

static void verify_gc(void)
{
  int read_only_space_size =
    (lispobj*) SymbolValue(READ_ONLY_SPACE_FREE_POINTER)
    - (lispobj*) READ_ONLY_SPACE_START;
  int static_space_size =
    (lispobj*) SymbolValue(STATIC_SPACE_FREE_POINTER)
    - (lispobj*) static_space;
  int binding_stack_size =
    (lispobj*) SymbolValue(BINDING_STACK_POINTER)
    - (lispobj*) BINDING_STACK_START;

  verify_space((lispobj*) READ_ONLY_SPACE_START, read_only_space_size);
  verify_space((lispobj*) static_space, static_space_size);
  verify_space((lispobj*) BINDING_STACK_START, binding_stack_size);
  verify_space((lispobj*) &scavenger_hooks, 1);
}

//static void verify_generation(int  generation)
void verify_generation(int  generation)
{
  int i;

  for (i = 0; i < last_free_page; i++) {
    if (PAGE_ALLOCATED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == generation) {
      int last_page;
      int region_unboxed = PAGE_UNBOXED(i);

      /* This should be the start of a contiguous block */
      gc_assert(page_table[i].first_object_offset == 0);

      /*
       * Need to find the full extent of this contiguous block in case
       * objects span pages.
       */

      /*
       * Now work forward until the end of this contiguous area is
       * found.
       */
      for (last_page = i; ; last_page++)
	/* Check if this is the last page in this contiguous block */
	if (page_table[last_page].bytes_used < PAGE_SIZE
	    /* Or it is PAGE_SIZE and is the last in the block */
	    || !PAGE_ALLOCATED(last_page + 1)
	    || PAGE_UNBOXED(last_page + 1) != region_unboxed
	    || page_table[last_page + 1].bytes_used == 0
	    || PAGE_GENERATION(last_page + 1) != generation
	    || page_table[last_page + 1].first_object_offset == 0)
	  break;

      verify_space(page_address(i),
		   (page_table[last_page].bytes_used +
		    PAGE_SIZE * (last_page - i)) / 4);
      i = last_page;
    }
  }
}

#if 1
int verify_generation1(int i, int last_page, int region_unboxed, int generation)
{
//  int i;
//
//   for (i = 0; i < last_free_page; i++) {
//     if (PAGE_ALLOCATED(i)
// 	&& page_table[i].bytes_used != 0
// 	&& PAGE_GENERATION(i) == generation) {
//       int last_page;
//       int region_unboxed = PAGE_UNBOXED(i);

      /* This should be the start of a contiguous block */
      gc_assert(page_table[i].first_object_offset == 0);

      /*
       * Need to find the full extent of this contiguous block in case
       * objects span pages.
       */

      /*
       * Now work forward until the end of this contiguous area is
       * found.
       */
      for (last_page = i; ; last_page++)
	/* Check if this is the last page in this contiguous block */
	if (page_table[last_page].bytes_used < PAGE_SIZE
	    /* Or it is PAGE_SIZE and is the last in the block */
	    || !PAGE_ALLOCATED(last_page + 1)
	    || ((PAGE_UNBOXED(last_page + 1)) ? 1 : 0) != region_unboxed
	    || page_table[last_page + 1].bytes_used == 0
	    || PAGE_GENERATION(last_page + 1) != generation
	    || page_table[last_page + 1].first_object_offset == 0)
	  break;

         verify_space(page_address(i),
 		      (page_table[last_page].bytes_used +
 		      PAGE_SIZE * (last_page - i)) / 4);

	return last_page;

//       i = last_page;
//     }
//   }
}
#endif

/* Check the all the free space is zero filled. */
static void verify_zero_fill(void)
{
  int page;

  for (page = 0; page < last_free_page; page++) {
    if (!PAGE_ALLOCATED(page)) {
      /* The whole page should be zero filled. */
      int *start_addr = (int *) page_address(page);
      int size = 1024;
      int i;
      for(i = 0; i < size; i++)
	if (start_addr[i] != 0)
	  fprintf(stderr, "** free page not zero @ %x\n", start_addr + i);
    } else {
      int free_bytes = PAGE_SIZE - page_table[page].bytes_used;
      if (free_bytes > 0) {
	int *start_addr = (int *) ((int) page_address(page)
				   + page_table[page].bytes_used);
	int size = free_bytes / 4;
	int i;
	for(i = 0; i < size; i++)
	  if (start_addr[i] != 0)
	    fprintf(stderr, "** free region not zero @ %x\n", start_addr + i);
      }
    }
  }
}

/* External entry point for verify_zero_fill */
void gencgc_verify_zero_fill(void)
{
  /* Flush the alloc regions updating the tables. */
  boxed_region.free_pointer = current_region_free_pointer;
  gc_alloc_update_page_tables(0, &boxed_region);
  gc_alloc_update_page_tables(1, &unboxed_region);
  fprintf(stderr, "* Verifying zero fill\n");
  verify_zero_fill();
  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;
}

static void verify_dynamic_space(void)
{
  int i;

  for (i = 0; i < NUM_GENERATIONS; i++)
    verify_generation(i);

  if (gencgc_enable_verify_zero_fill)
    verify_zero_fill();
}



void os_protect1(int page, os_vm_size_t len, os_vm_prot_t protection)
{
  // page_start = (void *) page_address(i);
  os_protect(page_address(page), len, protection);
}

//void write_protect_generation_pages1(int i)
void write_protect_generation_pages1(void* page_start, int i)
{
//   int i;
// 
//   gc_assert(generation < NUM_GENERATIONS);
// 
//   for (i = 0; i < last_free_page; i++)
//     if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
// 	&& page_table[i].bytes_used != 0
// 	&& PAGE_GENERATION(i) == generation)  {
//      void *page_start;

//      page_start = (void *) page_address(i);
	fprintf(stderr, "==== was .%x.\n", page_start);
	fprintf(stderr, "==== would have been .%x.\n", page_address(i));

      os_protect(page_start, PAGE_SIZE, OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

//       /* Note the page as protected in the page tables */
//       page_table[i].flags |= PAGE_WRITE_PROTECTED_MASK;
//     }
// 
//   if (gencgc_verbose > 1)
//     fprintf(stderr, "Write protected %d of %d pages in generation %d.\n",
// 	    count_write_protect_generation_pages(generation),
// 	    count_generation_pages(generation),
// 	    generation);
}

#if 1
/*
 * Write protect all the dynamic boxed pages in the given
 * generation.
 */
//static void write_protect_generation_pages(int generation)
void write_protect_generation_pages(int generation)
{
  int i;

  gc_assert(generation < NUM_GENERATIONS);

  for (i = 0; i < last_free_page; i++)
    if (PAGE_ALLOCATED(i) && !PAGE_UNBOXED(i)
	&& page_table[i].bytes_used != 0
	&& PAGE_GENERATION(i) == generation)  {
      void *page_start;

      page_start = (void *) page_address(i);

      os_protect(page_start, PAGE_SIZE, OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

      /* Note the page as protected in the page tables */
      page_table[i].flags |= PAGE_WRITE_PROTECTED_MASK;
    }

  if (gencgc_verbose > 1)
    fprintf(stderr, "Write protected %d of %d pages in generation %d.\n",
	    count_write_protect_generation_pages(generation),
	    count_generation_pages(generation),
	    generation);
}
#endif

/*
 * Garbage collect a generation. If raise is 0 the remains of the
 * generation are not raised to the next generation.
 */
//static void	garbage_collect_generation(int generation, int raise)
void	garbage_collect_generation(int generation, int raise)
{
  unsigned long i;
  unsigned long read_only_space_size, static_space_size;

  gc_assert(generation <= NUM_GENERATIONS - 1);

  /* The oldest generation can't be raised. */
  gc_assert(generation != NUM_GENERATIONS - 1 || raise == 0);

  /* Initialise the weak pointer list. */
  weak_pointers = NULL;

  /*
   * When a generation is not being raised it is transported to a
   * temporary generation (NUM_GENERATIONS), and lowered when
   * done. Setup this new generation. There should be no pages
   * allocated to it yet.
   */
  if (!raise)
    gc_assert(generations[NUM_GENERATIONS].bytes_allocated == 0);

  /* Set the global src and dest. generations */
  from_space = generation;
  if (raise)
    new_space = generation + 1;
  else
    new_space = NUM_GENERATIONS;

  /*
   * Change to a new space for allocation, reseting the alloc_start_page.
   */

  gc_alloc_generation = new_space;
  generations[new_space].alloc_start_page = 0;
  generations[new_space].alloc_unboxed_start_page = 0;
  generations[new_space].alloc_large_start_page = 0;
  generations[new_space].alloc_large_unboxed_start_page = 0;

  /*
   * Before any pointers are preserved, the dont_move flags on the
   * pages need to be cleared.
   */
  for (i = 0; i < last_free_page; i++)
    page_table[i].flags &= ~PAGE_DONT_MOVE_MASK;

  /*
   * Un-write-protect the old-space pages. This is essential for the
   * promoted pages as they may contain pointers into the old-space
   * which need to be scavenged. It also helps avoid unnecessary page
   * faults as forwarding pointer are written into them. They need to
   * be un-protected anyway before unmapping later.
   */
  unprotect_oldspace();

  /* Scavenge the stacks conservative roots. */
  {
    lispobj **ptr;
    for (ptr = (lispobj **) CONTROL_STACK_END - 1;
	 ptr > (lispobj **) &raise; ptr--)
      preserve_pointer(*ptr);
  }
#ifdef CONTROL_STACKS
  scavenge_thread_stacks();
#endif

  if (gencgc_verbose > 1) {
    int num_dont_move_pages = count_dont_move_pages();
    fprintf(stderr, "Non-movable pages due to conservative pointers = %d, %d bytes\n",
	    num_dont_move_pages, PAGE_SIZE * num_dont_move_pages);
  }

  /* Scavenge all the rest of the roots. */

  /*
   * Scavenge the Lisp functions of the interrupt handlers, taking
   * care to avoid SIG_DFL, SIG_IGN.
   */

  for (i = 0; i < NSIG; i++) {
    union interrupt_handler handler = interrupt_handlers[i];
    if ((handler.c != SIG_IGN) && (handler.c != SIG_DFL))
      scavenge((lispobj *) (interrupt_handlers + i), 1);
  }

  /* Scavenge the binding stack. */
  scavenge(binding_stack,
	   (lispobj *) SymbolValue(BINDING_STACK_POINTER) - binding_stack);

  /*
   * Scavenge the scavenge_hooks in case this refers to a hook added
   * in a prior generation GC. From here on the scavenger_hook will
   * only be updated with hooks already scavenged so this only needs
   * doing here.
   */

  scavenge((lispobj *) &scavenger_hooks, 1);

  if (SymbolValue(SCAVENGE_READ_ONLY_SPACE) != NIL) {
    read_only_space_size = (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER)
      - read_only_space;
    fprintf(stderr, "Scavenge read only space: %d bytes\n",
	    read_only_space_size * sizeof(lispobj));
    scavenge(read_only_space, read_only_space_size);
  }

  static_space_size = (lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER)
    - static_space;
  if (gencgc_verbose > 1)
    fprintf(stderr, "Scavenge static space: %d bytes\n",
	    static_space_size * sizeof(lispobj));
  scavenge(static_space, static_space_size);

  /*
   * All generations but the generation being GCed need to be
   * scavenged. The new_space generation needs special handling as
   * objects may be moved in - it is handle separately below.
   */
  for (i = 0; i < NUM_GENERATIONS; i++)
    if (i != generation && i != new_space)
      scavenge_generation(i);

  /*
   * Finally scavenge the new_space generation.  Keep going until no
   * more objects are moved into the new generation.
   */
  scavenge_newspace_generation(new_space);

#define RESCAN_CHECK 0
#if RESCAN_CHECK  
  /*
   * As a check re-scavenge the newspace once; no new objects should
   * be found.
   */
  {
    int old_bytes_allocated = bytes_allocated;
    int bytes_allocated;

    /* Start with a full scavenge */
    scavenge_newspace_generation_one_scan(new_space);

    scavenge((lispobj *) &scavenger_hooks, 1);

    /* Flush the current regions, updating the tables. */
    gc_alloc_update_page_tables(0, &boxed_region);
    gc_alloc_update_page_tables(1, &unboxed_region);

    bytes_allocated = bytes_allocated - old_bytes_allocated;

    if (bytes_allocated != 0)
      fprintf(stderr, "*** rescan of new_space allocated % more bytes?\n",
	      bytes_allocated);
  }
#endif

  scan_weak_pointers();

  /* Flush the current regions, updating the tables. */
  gc_alloc_update_page_tables(0, &boxed_region);
  gc_alloc_update_page_tables(1, &unboxed_region);

  /* Free the pages in oldspace, but not those marked dont_move. */
  free_oldspace();

  /*
   * If the GC is not raising the age then lower the generation back
   * to its normal generation number.
   */
  if (!raise) {
    for (i = 0; i < last_free_page; i++)
      if (page_table[i].bytes_used != 0
	  && PAGE_GENERATION(i) == NUM_GENERATIONS)
	PAGE_FLAGS_UPDATE(i, PAGE_GENERATION_MASK, generation);
    gc_assert(generations[generation].bytes_allocated == 0);
    generations[generation].bytes_allocated = generations[NUM_GENERATIONS].bytes_allocated;
    generations[NUM_GENERATIONS].bytes_allocated = 0;
  }

  /* Reset the alloc_start_page for generation. */
  generations[generation].alloc_start_page = 0;
  generations[generation].alloc_unboxed_start_page = 0;
  generations[generation].alloc_large_start_page = 0;
  generations[generation].alloc_large_unboxed_start_page = 0;

  if(generation >= verify_gens) {
    if (gencgc_verbose)
      fprintf(stderr, "Checking\n");
    verify_gc();
    verify_dynamic_space();
  }

  /* Set the new gc trigger for the GCed generation */
  generations[generation].gc_trigger = generations[generation].bytes_allocated + generations[generation].bytes_consed_between_gc;


  /* If the generation was raised clear num_gc */
  if (raise)
    generations[generation].num_gc = 0;
  else
    /* Else increase it. */
    generations[generation].num_gc++;
}

// FIX used in save.c
/* Update last_free_page then ALLOCATION_POINTER */
void	update_x86_dynamic_space_free_pointer(void)
{
  int last_page = -1;
  int i;

  for (i = 0; i < dynamic_space_pages; i++)
    if (PAGE_ALLOCATED(i) && page_table[i].bytes_used != 0)
      last_page = i;

  last_free_page = last_page + 1;

  SetSymbolValue(ALLOCATION_POINTER,
		 (lispobj) ((char *) heap_base + PAGE_SIZE * last_free_page));
}

#if 1
void	collect_garbage2(unsigned last_gen, int gen, int raise)
{
  int i;

  int gen_to_wp;

#if 0
  int gen = 0;
  int raise;

#if 0 // FIX
  fprintf(stderr, "boxed_region: %x\n", &boxed_region);
  fprintf(stderr, "unboxed_region: %x\n", &unboxed_region);
#endif

  boxed_region.free_pointer = current_region_free_pointer;

  /* Check last_gen */
  if (last_gen > NUM_GENERATIONS) {
    fprintf(stderr, "** collect_garbage: last_gen = %d. Doing a level 0 GC.\n",
	    last_gen);
    last_gen = 0;
  }

  /* Flush the alloc regions updating the tables. */
  gc_alloc_update_page_tables(0,&boxed_region);
  gc_alloc_update_page_tables(1,&unboxed_region);

  /* Verify the new objects created by lisp code. */
  if (pre_verify_gen_0) {
    fprintf(stderr, "Pre-Checking generation 0\n");
    verify_generation(0);
  }

  if (gencgc_verbose > 1)
    print_generation_stats(0);

  scavenger_hooks = NIL;

  do {
    /* Collect the generation */

    /* Never raise the oldest generation. */
    if (gen >= gencgc_oldest_gen_to_gc)
      raise = 0;
    else 
      /* Raise if: gen < last_gen */
      if (gen < last_gen)
	raise = 1;
      else
	/* Only raise if the age is >= the trigger age. */
	if (generations[gen].num_gc >= generations[gen].trigger_age)
	  raise = 1;
	else
	  raise = 0;

    if (gencgc_verbose > 1)
      fprintf(stderr, "Starting GC of generation %d with raise=%d alloc=%d trig=%d GCs=%d\n",
	      gen,
	      raise,
	      generations[gen].bytes_allocated,
	      generations[gen].gc_trigger,
	      generations[gen].num_gc);

    /*
     * If an older generation is being filled then update its memory age.
     */
    if (raise == 1)
      generations[gen + 1].cum_sum_bytes_allocated += generations[gen + 1].bytes_allocated;

    garbage_collect_generation(gen,raise);

    /* Reset the memory age cum_sum */
    generations[gen].cum_sum_bytes_allocated = 0;

    if (gencgc_verbose > 1) {
      fprintf(stderr, "GC of generation %d finished:\n", gen);
      print_generation_stats(0);
    }

    gen++;
  }
  while (gen <= gencgc_oldest_gen_to_gc
	 && (gen < last_gen
	     || (gen <= gencgc_oldest_gen_to_gc /* FIX just checked? */ && raise
		 && generations[gen].bytes_allocated > generations[gen].gc_trigger
		 && gen_av_mem_age(gen) > generations[gen].min_av_mem_age)));
#endif
  /*
   * Now if gen-1 was raised all generations before gen are empty.If
   * it wasn't raised then all generations before gen-1 are empty.
   *
   * Now objects within this gen's pages cannot pointer to younger
   * generations unless they are written to. This can be exploited by
   * write protecting the pages of gen; then when younger generations
   * are GCed only the page written need scanning.
   */
  if (raise)
    gen_to_wp = gen;
  else
    gen_to_wp = gen - 1;

  /*
   * Not much point in WPing pages in generation 0 as it is never
   * scavenged (except promoted pages).
   */
  if (gen_to_wp > 0 && enable_page_protection) {
    /* Check that they are all empty */
    for (i = 0; i < gen_to_wp; i++)
      if (generations[i].bytes_allocated != 0)
	fprintf(stderr, "*** trying to write prot. gen. %d when gen. %d is not empty\n",
		gen_to_wp, i);

    write_protect_generation_pages(gen_to_wp);
  }

  /*
   * Set gc_alloc back to generation 0. The current regions should be
   * flushed after the above GCs.
   */
  gc_assert(boxed_region.free_pointer - boxed_region.start_addr == 0);
  gc_alloc_generation = 0;

  update_x86_dynamic_space_free_pointer();

  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;

  /* Call the scavenger hook functions */
  {
    struct scavenger_hook *sh;
    for (sh = (struct scavenger_hook *) PTR((int) scavenger_hooks);
	 sh != (struct scavenger_hook *) PTR(NIL);) {
      struct scavenger_hook *sh_next = (struct scavenger_hook *) PTR((int) sh->next);
#if 0
      fprintf(stderr, "Scav hook %x; next %x; calling scav hook fn %x\n",
	      sh, sh_next, sh->function);
#endif
      funcall0(sh->function);
      sh->next = NULL;
      sh = sh_next;
    }
    scavenger_hooks = (struct scavenger_hook *) NIL;
  }
}
#endif

#if 0
//void	call_garbage_collect_generation(int gen, int raise)
void	call_garbage_collect_generation(unsigned last_gen, int gen, int raise)
{
    garbage_collect_generation(gen,raise);
}
#endif

#if 1
//void	collect_garbage1(unsigned last_gen)
void	collect_garbage1(unsigned last_gen, int gen, int raise)
{
#if 0
  int i;

  int gen_to_wp;

  int gen = 0;
  int raise;

  boxed_region.free_pointer = current_region_free_pointer;

  /* Check last_gen */
  if (last_gen > NUM_GENERATIONS) {
    fprintf(stderr, "** collect_garbage: last_gen = %d. Doing a level 0 GC.\n",
	    last_gen);
    last_gen = 0;
  }

  /* Flush the alloc regions updating the tables. */
  gc_alloc_update_page_tables(0,&boxed_region);
  gc_alloc_update_page_tables(1,&unboxed_region);

  /* Verify the new objects created by lisp code. */
  if (pre_verify_gen_0) {
    fprintf(stderr, "Pre-Checking generation 0\n");
    verify_generation(0);
  }

  if (gencgc_verbose > 1)
    print_generation_stats(0);

  scavenger_hooks = NIL;

  do {
    /* Collect the generation */

    /* Never raise the oldest generation. */
    if (gen >= gencgc_oldest_gen_to_gc)
      raise = 0;
    else 
      /* Raise if: gen < last_gen */
      if (gen < last_gen)
	raise = 1;
      else
	/* Only raise if the age is >= the trigger age. */
	if (generations[gen].num_gc >= generations[gen].trigger_age)
	  raise = 1;
	else
	  raise = 0;

    if (gencgc_verbose > 1)
      fprintf(stderr, "Starting GC of generation %d with raise=%d alloc=%d trig=%d GCs=%d\n",
	      gen,
	      raise,
	      generations[gen].bytes_allocated,
	      generations[gen].gc_trigger,
	      generations[gen].num_gc);

    /*
     * If an older generation is being filled then update its memory age.
     */
    if (raise == 1)
      generations[gen + 1].cum_sum_bytes_allocated += generations[gen + 1].bytes_allocated;
#endif
    garbage_collect_generation(gen,raise);

    /* Reset the memory age cum_sum */
    generations[gen].cum_sum_bytes_allocated = 0;

    if (gencgc_verbose > 1) {
      fprintf(stderr, "GC of generation %d finished:\n", gen);
      print_generation_stats(0);
    }
#if 0
    gen++;

  }
  while (gen <= gencgc_oldest_gen_to_gc
	 && (gen < last_gen
	     || (gen <= gencgc_oldest_gen_to_gc /* FIX just checked? */ && raise
		 && generations[gen].bytes_allocated > generations[gen].gc_trigger
		 && gen_av_mem_age(gen) > generations[gen].min_av_mem_age)));

  /*
   * Now if gen-1 was raised all generations before gen are empty.If
   * it wasn't raised then all generations before gen-1 are empty.
   *
   * Now objects within this gen's pages cannot pointer to younger
   * generations unless they are written to. This can be exploited by
   * write protecting the pages of gen; then when younger generations
   * are GCed only the page written need scanning.
   */
  if (raise)
    gen_to_wp = gen;
  else
    gen_to_wp = gen - 1;

  /*
   * Not much point in WPing pages in generation 0 as it is never
   * scavenged (except promoted pages).
   */
  if (gen_to_wp > 0 && enable_page_protection) {
    /* Check that they are all empty */
    for (i = 0; i < gen_to_wp; i++)
      if (generations[i].bytes_allocated != 0)
	fprintf(stderr, "*** trying to write prot. gen. %d when gen. %d is not empty\n",
		gen_to_wp, i);

    write_protect_generation_pages(gen_to_wp);
  }

  /*
   * Set gc_alloc back to generation 0. The current regions should be
   * flushed after the above GCs.
   */
  gc_assert(boxed_region.free_pointer - boxed_region.start_addr == 0);
  gc_alloc_generation = 0;

  update_x86_dynamic_space_free_pointer();

  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;

  /* Call the scavenger hook functions */
  {
    struct scavenger_hook *sh;
    for (sh = (struct scavenger_hook *) PTR((int) scavenger_hooks);
	 sh != (struct scavenger_hook *) PTR(NIL);) {
      struct scavenger_hook *sh_next = (struct scavenger_hook *) PTR((int) sh->next);
#if 0
      fprintf(stderr, "Scav hook %x; next %x; calling scav hook fn %x\n",
	      sh, sh_next, sh->function);
#endif
      funcall0(sh->function);
      sh->next = NULL;
      sh = sh_next;
    }
    scavenger_hooks = (struct scavenger_hook *) NIL;
  }
#endif
}
#endif

void	call_gc_alloc_update_page_tables()
{
  gc_alloc_update_page_tables(0,&boxed_region);
  gc_alloc_update_page_tables(1,&unboxed_region);
}

#if 1
/*
 * GC all generations below last_gen, raising their objects to the
 * next generation until all generations below last_gen are empty.
 * Then if last_gen is due for a GC then GC it. In the special case
 * that last_gen==NUM_GENERATIONS, the last generation is always
 * GC'ed. The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * The oldest generation to be GCed will always be
 * gencgc_oldest_gen_to_gc, partly ignoring last_gen if necessary.
 */
void	collect_garbage(unsigned last_gen)
{
  int i;

  int gen_to_wp;
  int gen = 0;
  int raise;

  boxed_region.free_pointer = current_region_free_pointer;

  /* Check last_gen */
  if (last_gen > NUM_GENERATIONS) {
    fprintf(stderr, "** collect_garbage: last_gen = %d. Doing a level 0 GC.\n",
	    last_gen);
    last_gen = 0;
  }

  /* Flush the alloc regions updating the tables. */
  gc_alloc_update_page_tables(0,&boxed_region);
  gc_alloc_update_page_tables(1,&unboxed_region);

  /* Verify the new objects created by lisp code. */
  if (pre_verify_gen_0) {
    fprintf(stderr, "Pre-Checking generation 0\n");
    verify_generation(0);
  }

  if (gencgc_verbose > 1)
    print_generation_stats(0);

  scavenger_hooks = NIL;

  do {
    /* Collect the generation */

    /* Never raise the oldest generation. */
    if (gen >= gencgc_oldest_gen_to_gc)
      raise = 0;
    else 
      /* Raise if: gen < last_gen */
      if (gen < last_gen)
	raise = 1;
      else
	/* Only raise if the age is >= the trigger age. */
	if (generations[gen].num_gc >= generations[gen].trigger_age)
	  raise = 1;
	else
	  raise = 0;

    if (gencgc_verbose > 1)
      fprintf(stderr, "Starting GC of generation %d with raise=%d alloc=%d trig=%d GCs=%d\n",
	      gen,
	      raise,
	      generations[gen].bytes_allocated,
	      generations[gen].gc_trigger,
	      generations[gen].num_gc);

    /*
     * If an older generation is being filled then update its memory age.
     */
    if (raise == 1)
      generations[gen + 1].cum_sum_bytes_allocated += generations[gen + 1].bytes_allocated;

    garbage_collect_generation(gen,raise);

    /* Reset the memory age cum_sum */
    generations[gen].cum_sum_bytes_allocated = 0;

    if (gencgc_verbose > 1) {
      fprintf(stderr, "GC of generation %d finished:\n", gen);
      print_generation_stats(0);
    }

    gen++;
  }
  while (gen <= gencgc_oldest_gen_to_gc
	 && (gen < last_gen
	     || (gen <= gencgc_oldest_gen_to_gc /* FIX just checked? */ && raise
		 && generations[gen].bytes_allocated > generations[gen].gc_trigger
		 && gen_av_mem_age(gen) > generations[gen].min_av_mem_age)));

  /*
   * Now if gen-1 was raised all generations before gen are empty.If
   * it wasn't raised then all generations before gen-1 are empty.
   *
   * Now objects within this gen's pages cannot pointer to younger
   * generations unless they are written to. This can be exploited by
   * write protecting the pages of gen; then when younger generations
   * are GCed only the page written need scanning.
   */
  if (raise)
    gen_to_wp = gen;
  else
    gen_to_wp = gen - 1;

  /*
   * Not much point in WPing pages in generation 0 as it is never
   * scavenged (except promoted pages).
   */
  if (gen_to_wp > 0 && enable_page_protection) {
    /* Check that they are all empty */
    for (i = 0; i < gen_to_wp; i++)
      if (generations[i].bytes_allocated != 0)
	fprintf(stderr, "*** trying to write prot. gen. %d when gen. %d is not empty\n",
		gen_to_wp, i);

    write_protect_generation_pages(gen_to_wp);
  }

  /*
   * Set gc_alloc back to generation 0. The current regions should be
   * flushed after the above GCs.
   */
  gc_assert(boxed_region.free_pointer - boxed_region.start_addr == 0);
  gc_alloc_generation = 0;

  update_x86_dynamic_space_free_pointer();

  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;

  /* Call the scavenger hook functions */
  {
    struct scavenger_hook *sh;
    for (sh = (struct scavenger_hook *) PTR((int) scavenger_hooks);
	 sh != (struct scavenger_hook *) PTR(NIL);) {
      struct scavenger_hook *sh_next = (struct scavenger_hook *) PTR((int) sh->next);
#if 0
      fprintf(stderr, "Scav hook %x; next %x; calling scav hook fn %x\n",
	      sh, sh_next, sh->function);
#endif
      funcall0(sh->function);
      sh->next = NULL;
      sh = sh_next;
    }
    scavenger_hooks = (struct scavenger_hook *) NIL;
  }

}
#endif


/*
 * The is called by purify when it is finished. All live objects will
 * have been moved to the RO and Static heaps. The dynamic space will
 * need a full re-initialisation. Do not bother having purify flush
 * the current allocation region, as the page_tables are re-initialised,
 * and every page is zeroed to be sure.
 */

void	gc_free_heap(void)
{
  int page;

  if (gencgc_verbose > 1)
    fprintf(stderr, "Free heap\n");

  for (page = 0; page < dynamic_space_pages; page++)
    /* Skip Free pages which should already be zero filled. */
    if (PAGE_ALLOCATED(page)) {
      void *page_start, *addr;

      /*
       * Mark the page free. The other slots are assumed invalid when
       * it is unallocated and bytes_used is 0 and it should not be
       * write protected - except that the generation is used for the
       * current region but it sets that up.
       */
      page_table[page].flags &= ~PAGE_ALLOCATED_MASK;
      page_table[page].bytes_used = 0;

      /* Zero the page. */
      page_start = (void *) page_address(page);

      /* First remove any write protection */
      os_protect(page_start, PAGE_SIZE, OS_VM_PROT_ALL);
      page_table[page].flags &= ~PAGE_WRITE_PROTECTED_MASK;

      os_invalidate(page_start, PAGE_SIZE);
      addr = os_validate(page_start, PAGE_SIZE);
      if(addr == NULL || addr != page_start)
	fprintf(stderr, "gc_zero: page moved, 0x%08x ==> 0x%08x!\n",
		page_start, addr);
    } else if (gencgc_zero_check_during_free_heap && page < 16384) {
      int *page_start;
      unsigned i;

      /* Double check that the page is zero filled. */
      gc_assert(!PAGE_ALLOCATED(page));
      gc_assert(page_table[page].bytes_used == 0);

      page_start = (int *) page_address(page);

      for(i = 0; i < 1024; i++)
	if (page_start[i] != 0)
	  fprintf(stderr, "** Free region not zero @ %x\n", page_start + i);
    }

  bytes_allocated = 0;

  /* Initialise the generations. */
  for (page = 0; page < NUM_GENERATIONS; page++) {
    generations[page].alloc_start_page = 0;
    generations[page].alloc_unboxed_start_page = 0;
    generations[page].alloc_large_start_page = 0;
    generations[page].alloc_large_unboxed_start_page = 0;
    generations[page].bytes_allocated = 0;
    generations[page].gc_trigger = 2000000;
    generations[page].num_gc = 0;
    generations[page].cum_sum_bytes_allocated = 0;
  }

  if (gencgc_verbose > 1)
    print_generation_stats(0);

  /* Initialise gc_alloc */
  gc_alloc_generation = 0;
  boxed_region.first_page = 0;
  boxed_region.last_page = -1;
  boxed_region.start_addr = page_address(0);
  boxed_region.free_pointer = page_address(0);
  boxed_region.end_addr = page_address(0);

  unboxed_region.first_page = 0;
  unboxed_region.last_page = -1;
  unboxed_region.start_addr = page_address(0);
  unboxed_region.free_pointer = page_address(0);
  unboxed_region.end_addr = page_address(0);

  last_free_page = 0;
  SetSymbolValue(ALLOCATION_POINTER, (lispobj) heap_base);

  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;

  if (verify_after_free_heap) {
    /* Check if purify has left any bad pointers. */
    if (gencgc_verbose)
      fprintf(stderr, "Checking after free_heap.\n");
    verify_gc();
  }
}



void gc_init(void)
{
  int i;

  gc_init_tables();

  heap_base = (void*) DYNAMIC_0_SPACE_START;

  /* The number of pages needed for the dynamic space - rounding up. */
  dynamic_space_pages = (dynamic_space_size + (PAGE_SIZE - 1)) / PAGE_SIZE;

  page_table = (struct page *)malloc(dynamic_space_pages*sizeof(struct page));
  if (page_table == NULL)
    {
      fprintf(stderr, "Unable to allocate page table.\n");
      exit(1);
    }

  /* Initialise each page structure. */

  for (i = 0; i < dynamic_space_pages; i++) {
    /* Initial all pages as free. */
    page_table[i].flags &= ~PAGE_ALLOCATED_MASK;
    page_table[i].bytes_used = 0;

    /* Pages are not write protected at startup. */
    page_table[i].flags &= ~PAGE_WRITE_PROTECTED_MASK;
  }

  bytes_allocated = 0;

  /* Initialise the generations. */
  for (i = 0; i < NUM_GENERATIONS; i++) {
    generations[i].alloc_start_page = 0;
    generations[i].alloc_unboxed_start_page = 0;
    generations[i].alloc_large_start_page = 0;
    generations[i].alloc_large_unboxed_start_page = 0;
    generations[i].bytes_allocated = 0;
    generations[i].gc_trigger = 2000000;
    generations[i].num_gc = 0;
    generations[i].cum_sum_bytes_allocated = 0;
    /* The tune-able parameters */
    generations[i].bytes_consed_between_gc = 2000000;
    generations[i].trigger_age = 1;
    generations[i].min_av_mem_age = 0.75;
  }

  /* Initialise gc_alloc */
  gc_alloc_generation = 0;
  boxed_region.first_page = 0;
  boxed_region.last_page = -1;
  boxed_region.start_addr = page_address(0);
  boxed_region.free_pointer = page_address(0);
  boxed_region.end_addr = page_address(0);

  unboxed_region.first_page = 0;
  unboxed_region.last_page = -1;
  unboxed_region.start_addr = page_address(0);
  unboxed_region.free_pointer = page_address(0);
  unboxed_region.end_addr = page_address(0);

  last_free_page = 0;

  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;
}

/*
 * Pickup the dynamic space from after a core load.
 *
 * The ALLOCATION_POINTER points to the end of the dynamic space.
 *  
 * XX A scan is needed to identify the closest first objects for pages.
 */

void	gencgc_pickup_dynamic(void)
{
  int page = 0;
  int addr = DYNAMIC_0_SPACE_START;
  int alloc_ptr = SymbolValue(ALLOCATION_POINTER);

  /* Initialise the first region. */
  do {
    page_table[page].flags |= PAGE_ALLOCATED_MASK;
    page_table[page].flags &= ~(PAGE_UNBOXED_MASK | PAGE_GENERATION_MASK
				| PAGE_LARGE_OBJECT_MASK);
    page_table[page].bytes_used = PAGE_SIZE;
    page_table[page].first_object_offset =
      (void *) DYNAMIC_0_SPACE_START - page_address(page);
    addr += PAGE_SIZE;
    page++;
  }
  while (addr < alloc_ptr);

  generations[0].bytes_allocated = PAGE_SIZE * page;
  bytes_allocated = PAGE_SIZE * page;

  current_region_free_pointer = boxed_region.free_pointer;
  current_region_end_addr = boxed_region.end_addr;
}




void do_pending_interrupt(void);

/*
 * Alloc is the external interface for memory allocation. It allocates
 * to generations0.  It is not called from within the garbage
 * collector as it's only external uses that need the check for heap
 * size (GC trigger) and to disable the interrupts (interrupts are
 * always disabled during a GC).
 * 
 * It is assumed by the vops that the returned space is zero
 * filled. E.g. the most significant word of a 2 word bignum in
 * move-from-unsigned.
 *
 * The check for a GC trigger is only performed when the current
 * region is full, so in most cases it's not needed.  Further maybe-gc
 * is only called once because lisp will remember the need to collect
 * garbage and get to it when it can.
 *
 * Note that this code is typically called directly from lisp code,
 * while within a pseudo atomic context.
 */

int alloc_entered = 0;

char *alloc(int nbytes)
{
  /* Check for alignment allocation problems. */
  gc_assert(((unsigned) current_region_free_pointer & 0x7) == 0
	    && (nbytes & 0x7) == 0);

  if (SymbolValue(PSEUDO_ATOMIC_ATOMIC)) {
    /* Already within a pseudo atomic. */
    void *new_free_pointer;

  retry1:
    if (alloc_entered++)
      fprintf(stderr,"* Alloc re-entered\n");

    /* Check if there is room in the current region. */
    new_free_pointer = current_region_free_pointer + nbytes;

    if (new_free_pointer <= boxed_region.end_addr) {
      /* If so then allocate from the current region. */
      void  *new_obj = current_region_free_pointer;
      current_region_free_pointer = new_free_pointer;
      alloc_entered--;
      return (void *) new_obj;
    }

    if(bytes_allocated > auto_gc_trigger) {
      /* Double the trigger. */
      auto_gc_trigger *= 2;
      alloc_entered--;
      /* Exit the pseudo atomic */
      SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(0));
      if (SymbolValue(PSEUDO_ATOMIC_INTERRUPTED) != 0)
	/* Handle any interrupts that occurred during gc_alloc */
	do_pending_interrupt();
      funcall0(SymbolFunction(MAYBE_GC));
      /* Re-enter the pseudo atomic. */
      SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0));
      SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1));
      goto retry1;
    }
    /* Call gc_alloc */
    boxed_region.free_pointer = current_region_free_pointer;
    {
      void *new_obj = gc_alloc(nbytes);
      current_region_free_pointer = boxed_region.free_pointer;
      current_region_end_addr = boxed_region.end_addr;
      alloc_entered--;
      return new_obj;
    }
  } else {
    void *result;
    void *new_free_pointer;

#if 0
    /*
     * Check that the interrupts are masked, else there could be
     * trouble if the allocation is interrupted.
     */
    sigset_t mask;
    sigprocmask(0, NULL, &mask);
    if (!sigismember(&mask, SIGINT) || !sigismember(&mask, SIGALRM))
      fprintf(stderr, "* Alloc non-atomic %x\n", mask);
#endif      

  retry2:
    /* At least wrap this allocation in a pseudo atomic to prevent
       gc_alloc from being re-entered. */
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0));
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1));

    if (alloc_entered++)
      fprintf(stderr,"* Alloc re-entered\n");

    /* Check if there is room in the current region. */
    new_free_pointer = current_region_free_pointer + nbytes;

    if (new_free_pointer <= boxed_region.end_addr) {
      /* If so then allocate from the current region. */
      void *new_obj = current_region_free_pointer;
      current_region_free_pointer = new_free_pointer;

      alloc_entered--;
      SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(0));
      if (SymbolValue(PSEUDO_ATOMIC_INTERRUPTED)) {
	/* Handle any interrupts that occurred during gc_alloc */
	do_pending_interrupt();
	goto retry2;
      }

      return (void *) new_obj;
    }

    if(bytes_allocated > auto_gc_trigger) {
      /* Double the trigger. */
      auto_gc_trigger *= 2;
      alloc_entered--;
      /* Exit the pseudo atomic */
      SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(0));
      if (SymbolValue(PSEUDO_ATOMIC_INTERRUPTED) != 0)
	/* Handle any interrupts that occurred during gc_alloc */
	do_pending_interrupt();
      funcall0(SymbolFunction(MAYBE_GC));
      goto retry2;
    }

    /* Else call gc_alloc */
    boxed_region.free_pointer = current_region_free_pointer;
    result = gc_alloc(nbytes);
    current_region_free_pointer = boxed_region.free_pointer;
    current_region_end_addr = boxed_region.end_addr;

    alloc_entered--;
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(0));
    if (SymbolValue(PSEUDO_ATOMIC_INTERRUPTED) != 0) {
      /* Handle any interrupts that occurred during gc_alloc */
      do_pending_interrupt();
      goto retry2;
    }

    return result;
  }
}


/* Noise to manipulate the gc trigger stuff. */
void set_auto_gc_trigger(unsigned long dynamic_usage)
{
  auto_gc_trigger += dynamic_usage;
}

void clear_auto_gc_trigger(void)
{
  auto_gc_trigger = 0xffffffff;
}

/* Find the code object for the given pc. Return NULL on failure */
lispobj * component_ptr_from_pc(lispobj *pc)
{
  lispobj *ptr;

  ptr = search_read_only_space(pc);

  if (!ptr)
    ptr = search_static_space(pc);

  if (!ptr)
    ptr = search_dynamic_space(pc);

  /* Found anything? Check if it is a code object.  */
  if (ptr && TypeOf(*ptr) == type_CodeHeader)
    return ptr;

  return NULL;
}
