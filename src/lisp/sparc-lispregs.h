/* $Header: /home/CVS-cmucl/src/lisp/sparc-lispregs.h,v 1.4.2.1 2000/10/27 19:41:22 dtc Exp $ */



#ifdef LANGUAGE_ASSEMBLY

#ifdef __STDC__
				/* Standard C token concatenation */
#define CAT(a,b) a ## b

#else
				/* Reisser CPP token concatenation */
#define CAT(a,b) a/**/b

#endif

/*
 * Note: concatenation to non preprocessor symbols is not defined,
 * SunPRO C 2.0.1 yields % g0, perfectly valid according to ANSI
 */
#define GREG(num) CAT(%g,num)
#define OREG(num) CAT(%o,num)
#define LREG(num) CAT(%l,num)
#define IREG(num) CAT(%i,num)

#else

#define GREG(num) (num)
#define OREG(num) ((num)+8)
#define LREG(num) ((num)+16)
#define IREG(num) ((num)+24)

#endif

#define NREGS	(32)

#define reg_ZERO	GREG(0)
#define reg_ALLOC	GREG(1)
#define reg_NIL		GREG(2)
#define reg_CSP		GREG(3)
#define reg_CFP		GREG(4)
#define reg_BSP		GREG(5)
/* %g6 and %g7 are supposed to be reserved for the system */

#define reg_NL0		OREG(0)
#define reg_NL1		OREG(1)
#define reg_NL2		OREG(2)
#define reg_NL3		OREG(3)
#define reg_NL4		OREG(4)
#define reg_NL5		OREG(5)
#define reg_NSP		OREG(6)
#define reg_NARGS	OREG(7)

#define reg_A0		LREG(0)
#define reg_A1		LREG(1)
#define reg_A2		LREG(2)
#define reg_A3		LREG(3)
#define reg_A4		LREG(4)
#define reg_A5		LREG(5)
#define reg_OCFP	LREG(6)
#define reg_LRA		LREG(7)

#define reg_FDEFN	IREG(0)
#define reg_LEXENV	IREG(1)
#define reg_L0		IREG(2)
#define reg_NFP		IREG(3)
#define reg_CFUNC	IREG(4)
#define reg_CODE	IREG(5)
#define reg_LIP		IREG(7)

#define REGNAMES \
	"ZERO",		"ALLOC",	"NULL",		"CSP", \
	"CFP",		"BSP",		"%g6",		"%g7", \
        "NL0",		"NL1",		"NL2",		"NL3", \
        "NL4",		"NL5",		"NSP",		"NARGS", \
        "A0",		"A1",		"A2",		"A3", \
        "A4",		"A5",		"OCFP",		"LRA", \
        "FDEFN",	"LEXENV",	"L0",		"NFP", \
        "CFUNC",	"CODE",		"???",		"LIP"

#define BOXED_REGISTERS { \
    reg_A0, reg_A1, reg_A2, reg_A3, reg_A4, reg_A5, reg_FDEFN, reg_LEXENV, \
    reg_OCFP, reg_LRA, reg_L0, reg_CODE \
}

#ifndef LANGUAGE_ASSEMBLY

#ifdef SOLARIS

#include <ucontext.h>

extern int * solaris_register_address(struct ucontext *, int);

#define SC_REG(sc, reg) (*solaris_register_address(sc,reg))

/* short cuts */

#define SC_PC(sc) ((sc)->uc_mcontext.gregs[REG_PC])
#define SC_NPC(sc) ((sc)->uc_mcontext.gregs[REG_nPC])

#else

#define SC_REG(sc, n) (((int *)((sc)->sc_g1))[n])
#define SC_PC(sc) ((sc)->sc_pc)
#define SC_NPC(sc) ((sc)->sc_npc)

#endif /* SOLARIS */

#endif
