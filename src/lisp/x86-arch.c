/* x86-arch.c -*- Mode: C; comment-column: 40 -*-
 *
 * $Header: /home/CVS-cmucl/src/lisp/x86-arch.c,v 1.2.2.3 2000/10/21 12:42:34 dtc Exp $
 *
 */

#include <stdio.h>

#include "nightshade.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "internals.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

#define DPRINTF(test,e){if(test) fprintf e ;}

#define BREAKPOINT_INST 0xcc	/* INT3 */

unsigned long  fast_random_state = 1;

char *
arch_init(void)
{
  return "lisp.core";
}


void arch_skip_instruction(context)
struct sigcontext *context;
{
  /* Assuming we get here via an INT3 xxx instruction, the PC now
   * points to the interrupt code (lisp value) so we just move past
   * it. Skip the code, then if the code is an error-trap or
   * Cerror-trap then skip the data bytes that follow. */
  int vlen,code;

  DPRINTF(0,(stderr,"[arch_skip_inst at %x>]\n", context->sc_pc));

  /* Get and skip the lisp error code. */
  code = *(char*)context->sc_pc++;
  switch (code)
    {
    case trap_Error:
    case trap_Cerror:
      /* Lisp error arg vector length */
      vlen = *(char*)context->sc_pc++;
      /* Skip lisp error arg data bytes */
      while(vlen-- > 0)
	(char*)context->sc_pc++;
      break;

    case trap_Breakpoint:		/* Not tested */
    case trap_FunctionEndBreakpoint:	/* not tested */
      break;

    case trap_PendingInterrupt:
    case trap_Halt:
      /* Only needed to skip the Code. */
      break;

    default:
      fprintf(stderr,"[arch_skip_inst invalid code %d\n]\n",code);
      break;
    }

  DPRINTF(0,(stderr,"[arch_skip_inst resuming at %x>]\n", context->sc_pc));
}

unsigned char *
arch_internal_error_arguments(struct sigcontext *context)
{
  return (unsigned char *)(context->sc_pc+1);
}

boolean
arch_pseudo_atomic_atomic(struct sigcontext *context)
{
  return SymbolValue(PSEUDO_ATOMIC_ATOMIC);
}

void
arch_set_pseudo_atomic_interrupted(struct sigcontext *context)
{
  SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(1));
}


/* This stuff seems to get called for TRACE and debug activity */
unsigned long
arch_install_breakpoint(void *pc)
{
  unsigned long result = *(unsigned long*)pc;

  *(char*)pc = BREAKPOINT_INST;		/* x86 INT3       */
  *((char*)pc+1) = trap_Breakpoint;		/* Lisp trap code */

  return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
  *((char *)pc) = orig_inst & 0xff;
  *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;
}


#ifdef __linux__
#if 0
//-- FIX from linux-source-2.6.18/include/asm-i386/unistd.h
#define __syscall_return(type, res) \
do { \
	if ((unsigned long)(res) >= (unsigned long)(-(128 + 1))) { \
		errno = -(res); \
		res = -1; \
	} \
	return (type) (res); \
} while (0)
#define _syscall1(type,name,type1,arg1) \
type name(type1 arg1) \
{ \
long __res; \
__asm__ volatile ("push %%ebx ; movl %2,%%ebx ; int $0x80 ; pop %%ebx" \
	: "=a" (__res) \
	: "0" (__NR_##name),"ri" ((long)(arg1)) : "memory"); \
__syscall_return(type,__res); \
}
//--
// _syscall1 is part of Linux, seems to be included only when __KERNEL__.
_syscall1(int,sigreturn,struct sigcontext *,context)
#endif
#if 0
int sigreturn(struct sigcontext * context) {
    return syscall (1, context);
}
#endif
#endif

/* When single stepping single_stepping holds the original instruction
   pc location. */
unsigned int *single_stepping=NULL;
#ifndef __linux__
unsigned int  single_step_save1;
unsigned int  single_step_save2;
unsigned int  single_step_save3;
#endif

void
arch_do_displaced_inst(struct sigcontext *context, unsigned long orig_inst)
{
  unsigned int *pc = (unsigned int*)context->sc_pc;
  unsigned int flags = context->sc_efl;

  /* Put the original instruction back. */
  *((char *)pc) = orig_inst & 0xff;
  *((char *)pc + 1) = (orig_inst & 0xff00) >> 8;

#ifdef __linux__
  context->eflags |= 0x100;
#else
  /* Install helper instructions for the single step:
     pushf; or [esp],0x100; popf. */
  single_step_save1 = *(pc-3);
  single_step_save2 = *(pc-2);
  single_step_save3 = *(pc-1);
  *(pc-3) = 0x9c909090;
  *(pc-2) = 0x00240c81;
  *(pc-1) = 0x9d000001;
#endif

  single_stepping=(unsigned int*)pc;

#ifndef __linux__
  (unsigned int*)context->sc_pc = ((char *)pc-9);
#endif
}


void
sigtrap_handler(HANDLER_ARGS)
{
  unsigned int  trap;

#ifdef __linux__
  GET_CONTEXT
#endif
    /*
    fprintf(stderr,"x86sigtrap: %8x %x\n",
	    context->sc_pc, *(unsigned char *)(context->sc_pc-1));
  fprintf(stderr,"sigtrap(%d %d %x)\n",signal,code,context);*/

  if (single_stepping && (signal==SIGTRAP))
    {
      /* fprintf(stderr,"* Single step trap %x\n", single_stepping); */

#ifndef __linux__
      /* Un-install single step helper instructions. */
      *(single_stepping-3) = single_step_save1;
      *(single_stepping-2) = single_step_save2;
      *(single_stepping-1) = single_step_save3;
#else
       context->eflags ^= 0x100;
#endif
      /* Re-install the breakpoint if possible. */
      if ((int)context->sc_pc == (int)single_stepping + 1)
	fprintf(stderr,"* Breakpoint not re-install\n");
      else
	{
	  char*ptr = (char*)single_stepping;
	  *((char *)single_stepping) = BREAKPOINT_INST;	/* x86 INT3 */
	  *((char *)single_stepping+1) = trap_Breakpoint;
	}

      single_stepping=NULL;
      return;
    }

  SAVE_CONTEXT();

  /* this is just for info in case monitor wants to print an approx */
  current_control_stack_pointer = (unsigned long*)context->sc_sp;

#if defined(__linux__) && defined(i386)
  /*
   * Restore the FPU control word, setting the rounding mode to nearest.
   */

  if (contextstruct.fpstate)
    setfpucw(contextstruct.fpstate->cw & ~0xc00);
#endif

 /* On entry %eip points just after the INT3 byte and aims at the
  * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
  * number of bytes will follow, the first is the length of the byte
  * arguments to follow.  */
  trap = *(unsigned char *)(context->sc_pc);
  switch (trap)
    {
    case trap_PendingInterrupt:
      DPRINTF(0,(stderr,"<trap Pending Interrupt.>\n"));
      arch_skip_instruction(context);
      interrupt_handle_pending(context);
      break;

    case trap_Halt:
      {
#if defined(__FreeBSD__)
	int fpu_state[27];
	fpu_save(fpu_state);
#endif
	fake_foreign_function_call(context);
	lose("%%primitive halt called; the party is over.\n");
	undo_fake_foreign_function_call(context);
#if defined(__FreeBSD__)
	fpu_restore(fpu_state);
#endif
	arch_skip_instruction(context);
	break;
      }

    case trap_Error:
    case trap_Cerror:
      DPRINTF(0,(stderr,"<trap Error %d>\n",code));
#ifdef __linux__
      interrupt_internal_error(signal,contextstruct, code==trap_Cerror);
#else
      interrupt_internal_error(signal, code, context, code==trap_Cerror);
#endif
      break;

    case trap_Breakpoint:
      /*      fprintf(stderr,"*C break\n");*/
      (char*)context->sc_pc -= 1;
      handle_breakpoint(signal, code, context);
      /*      fprintf(stderr,"*C break return\n");*/
      break;

    case trap_FunctionEndBreakpoint:
      (char*)context->sc_pc -= 1;
      context->sc_pc = (int)handle_function_end_breakpoint(signal, code, context);
      break;

    default:
      DPRINTF(0,(stderr,"[C--trap default %d %d %x]\n",signal,code,context));
#ifdef __linux__
      interrupt_handle_now(signal,contextstruct);
#else
      interrupt_handle_now(signal, code, context);
#endif
      break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

extern void first_handler();
void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL ,sigtrap_handler);
    interrupt_install_low_level_handler(SIGTRAP,sigtrap_handler);
}


extern lispobj
call_into_lisp(lispobj fun, lispobj *args, int nargs);

/* These next four functions are an interface to the
 * Lisp call-in facility. Since this is C we can know
 * nothing about the calling environment. The control
 * stack might be the C stack if called from the monitor
 * or the Lisp stack if called as a result of an interrupt
 * or maybe even a separate stack. The args are most likely
 * on that stack but could be in registers depending on
 * what the compiler likes. So I try to package up the
 * args into a portable vector and let the assembly language
 * call-in function figure it out.
 */
lispobj
funcall0(lispobj function)
{
    lispobj *args = NULL;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj args[1];
    args[0] = arg0;
    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj args[2];
    args[0] = arg0;
    args[1] = arg1;
    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj args[3];
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return call_into_lisp(function, args, 3);
}
