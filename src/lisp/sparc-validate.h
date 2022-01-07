/*

 $Header: /home/CVS-cmucl/src/lisp/sparc-validate.h,v 1.8.2.3 2000/10/27 19:34:24 dtc Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

/*
 * Address map:
 *
 *	0x00000000->0x10000000  256M C code and stuff(?)
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing down.
 *	0x40000000->0x80000000 1024M Dynamic space 1
 *	0x80000000->0xc0000000 1024M Dynamic space 2
 *      0xc0000000->0xffffffff 1024M C stack, dynamic libs, etc.      
 *
 * Almost.  We leave a hole of size 32 KB at the end of each of these
 * spaces.
 *
 *
 * It may be possible to increase the size of the dynamic spaces even
 * further, but Casper H.S. Dik says shared libraries are loaded
 * directly under the stack, so we need to leave some space for the C
 * stack and shared libraries.  He also says the top of stack is
 * 0xffbf000 for Ultrasparcs in Solaris 7+, but it's 0xf0000000 for
 * sun4m (and 4u in S2.6-) 0xe0000000 for sun4d)
 *
 * Shared libraries can be mapped anywhere there's room.
 */

/*
 *
 * Note: I'm not sure why, but the sizes must be on a
 * SPARSE_BLOCK_SIZE (32 KB) boundary.  (See seg_force_resident in
 * sunos-os.c.  If not, then mapping the holes causes segfaults in
 * initialization.)
 *
 */
#define READ_ONLY_SPACE_START	(0x10000000)
#define READ_ONLY_SPACE_SIZE	(0x07ff8000) /* 128 MB - 32 KB, 256 MB max */
  
#define BINDING_STACK_START 	(0x20000000)
#define BINDING_STACK_SIZE  	(0x07ff8000) /* 128 MB - 32 KB, 128 MB max */

#define STATIC_SPACE_START  	(0x28000000)
#define STATIC_SPACE_SIZE   	(0x03ff8000) /* 128 MB - 32 KB, 256 MB max */

#define CONTROL_STACK_START 	(0x38000000)
#define CONTROL_STACK_SIZE  	(0x07ff8000) /* 128 MB - 32 KB, 128 MB max */

#define DYNAMIC_0_SPACE_START	(0x40000000)
#define DYNAMIC_1_SPACE_START	(0x80000000)

/* The default dynamic space to allocate */
#define DEFAULT_DYNAMIC_SPACE_SIZE  	(0x0fff8000) /* 256 MB - 32 KB */

/* The maximum dynamic space that we can allocate */
#define DYNAMIC_SPACE_SIZE      (0x3ff80000)    /* 1GB - 32 KB max */
