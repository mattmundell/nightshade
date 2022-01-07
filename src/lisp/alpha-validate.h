/*

 $Header: /project/cmucl/cvsroot/src/lisp/alpha-validate.h,v 1.3 1999/02/20 15:54:43 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef linux

#define READ_ONLY_SPACE_START   (0x20000000)
#define READ_ONLY_SPACE_SIZE    (0x04000000)

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x04000000)

#define DYNAMIC_0_SPACE_START	(0x30000000)
#define DYNAMIC_1_SPACE_START	(0x38000000)
#define DYNAMIC_SPACE_SIZE	(0x04000000)

#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x00100000)

#define BINDING_STACK_START	(0x70000000)
#define BINDING_STACK_SIZE	(0x00100000)

#else

#define READ_ONLY_SPACE_START   (0x10000000)
#define READ_ONLY_SPACE_SIZE    (0x15000000)

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x04000000)

#define DYNAMIC_0_SPACE_START	(0x30000000)
#define DYNAMIC_1_SPACE_START	(0x40000000)
#define DYNAMIC_SPACE_SIZE	(0x0F000000)

#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x01000000)

#define BINDING_STACK_START	(0x70000000)
#define BINDING_STACK_SIZE	(0x01000000)

#endif
