/* $Header: /project/cmucl/cvsroot/src/lisp/monitor.c,v 1.12 2000/10/27 19:25:55 dtc Exp $ */

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <setjmp.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>

#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "vars.h"
#include "parse.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"
#include "monitor.h"
#include "print.h"
#include "arch.h"
#if 0
#include "gc.h"
#endif
#include "search.h"
#include "purify.h"
#if 0
#if defined GENCGC
#include "gencgc.h"
#endif
#endif

extern boolean isatty(int fd);

typedef void cmd(char **ptr);

static cmd call_cmd, dump_cmd, print_cmd, quit, help;
static cmd flush_cmd, search_cmd, regs_cmd, exit_cmd;
static cmd /* gc_cmd, */ print_context_cmd;
static cmd backtrace_cmd, purify_cmd, catchers_cmd;
static cmd grab_sigs_cmd;

static struct cmd {
    char *cmd, *help;
    void (*fn)(char **ptr);
} Cmds[] = {
    {"help", "Display this info", help},
    {"?", NULL, help},
    {"backtrace", "backtrace up to N frames", backtrace_cmd},
    {"call", "call FUNCTION with ARG1, ARG2, ...", call_cmd},
    {"catchers", "Print a list of all the active catchers.", catchers_cmd},
    {"context", "print interrupt context number I.", print_context_cmd},
    {"dump", "dump memory starting at ADDRESS for COUNT words.", dump_cmd},
    {"d", NULL, dump_cmd},
    {"exit", "Exit this instance of the monitor.", exit_cmd},
    {"flush", "flush all temp variables.", flush_cmd},
    /* {"gc", "collect garbage (caveat collector).", gc_cmd}, */
    {"grab-signals", "Set the signal handlers to call LDB.", grab_sigs_cmd},
    {"purify", "purify (caveat purifier).", purify_cmd},
    {"print", "print object at ADDRESS.", print_cmd},
    {"p", NULL, print_cmd},
    {"quit", "quit.", quit},
    {"regs", "display current lisp regs.", regs_cmd},
    {"search", "search for TYPE starting at ADDRESS for a max of COUNT words.", search_cmd},
    {"s", NULL, search_cmd},
    {NULL, NULL, NULL}
};


static jmp_buf curbuf;


static int visable(unsigned char c)
{
    if (c < ' ' || c > '~')
        return ' ';
    else
        return c;
}

static void dump_cmd(char **ptr)
{
    static char *lastaddr = 0;
    static int lastcount = 20;

    char *addr = lastaddr;
    int count = lastcount, displacement;

    if (more_p(ptr)) {
        addr = parse_addr(ptr);

        if (more_p(ptr))
            count = parse_number(ptr);
    }

    if (count == 0) {
        printf("COUNT must be non-zero.\n");
        return;
    }
        
    lastcount = count;

    if (count > 0)
        displacement = 4;
    else {
        displacement = -4;
        count = -count;
    }

    while (count-- > 0) {
#ifndef alpha
        printf("0x%08lX: ", (unsigned long) addr);
#else
        printf("0x%08X: ", (u32) addr);
#endif
        if (valid_addr((os_vm_address_t)addr)) {
#ifndef alpha
            unsigned long *lptr = (unsigned long *)addr;
#else
            u32 *lptr = (unsigned long *)addr;
#endif
            unsigned short *sptr = (unsigned short *)addr;
            unsigned char *cptr = (unsigned char *)addr;

            printf("0x%08lx   0x%04x 0x%04x   0x%02x 0x%02x 0x%02x 0x%02x    %c%c%c%c\n", lptr[0], sptr[0], sptr[1], cptr[0], cptr[1], cptr[2], cptr[3], visable(cptr[0]), visable(cptr[1]), visable(cptr[2]), visable(cptr[3]));
        }
        else
            printf("invalid address\n");

        addr += displacement;
    }

    lastaddr = addr;
}

static void print_cmd(char **ptr)
{
    lispobj obj = parse_lispobj(ptr);
    
    print(obj);
}

static void regs_cmd(char **ptr)
{
    printf("CSP\t=\t0x%08lX\n", (unsigned long)current_control_stack_pointer);
    printf("FP\t=\t0x%08lX\n", (unsigned long)current_control_frame_pointer);
#if !defined(ibmrt) && !defined(i386)
    printf("BSP\t=\t0x%08X\n", (unsigned long)current_binding_stack_pointer);
#endif
#ifdef i386
    printf("BSP\t=\t0x%08X\n", SymbolValue(BINDING_STACK_POINTER));
#endif

    printf("DYNAMIC\t=\t0x%08lX\n", (unsigned long)current_dynamic_space);
#if defined(ibmrt) || defined(i386)
    printf("ALLOC\t=\t0x%08lX\n", SymbolValue(ALLOCATION_POINTER));
    printf("TRIGGER\t=\t0x%08lX\n", SymbolValue(INTERNAL_GC_TRIGGER));
#else
    printf("ALLOC\t=\t0x%08X\n",
	   (unsigned long)current_dynamic_space_free_pointer);
    printf("TRIGGER\t=\t0x%08X\n", (unsigned long)current_auto_gc_trigger);
#endif
    printf("STATIC\t=\t0x%08lX\n", SymbolValue(STATIC_SPACE_FREE_POINTER));
    printf("RDONLY\t=\t0x%08lX\n", SymbolValue(READ_ONLY_SPACE_FREE_POINTER));

#ifdef MIPS
    printf("FLAGS\t=\t0x%08x\n", current_flags_register);
#endif
}

static void search_cmd(char **ptr)
{
    static int lastval = 0, lastcount = 0;
    static lispobj *start = 0, *end = 0;
    int val, count;
    lispobj *addr, obj;

    if (more_p(ptr)) {
        val = parse_number(ptr);
        if (val < 0 || val > 0xff) {
            printf("Can only search for single bytes.\n");
            return;
        }
        if (more_p(ptr)) {
            addr = (lispobj *)PTR((long)parse_addr(ptr));
            if (more_p(ptr)) {
                count = parse_number(ptr);
            }
            else {
                /* Speced value and address, but no count. Only one. */
                count = -1;
            }
        }
        else {
            /* Speced a value, but no address, so search same range. */
            addr = start;
            count = lastcount;
        }
    }
    else {
        /* Speced nothing, search again for val. */
        val = lastval;
        addr = end;
        count = lastcount;
    }

    lastval = val;
    start = end = addr;
    lastcount = count;

    printf("searching for 0x%x at 0x%08lX\n", val, (unsigned long)end);

    while (search_for_type(val, &end, &count)) {
        printf("found 0x%x at 0x%08lX:\n", val, (unsigned long)end);
        obj = *end;
        addr = end;
        end += 2;
        if (TypeOf(obj) == type_FunctionHeader)
            print((long)addr | type_FunctionPointer);
        else if (LowtagOf(obj) == type_OtherImmediate0 || LowtagOf(obj) == type_OtherImmediate1)
            print((lispobj)addr | type_OtherPointer);
        else
            print((lispobj)addr);
        if (count == -1)
            return;
    }
}

static void call_cmd(char **ptr)
{
    lispobj thing = parse_lispobj(ptr), function, result, cons, args[3];
    int numargs;

    if (LowtagOf(thing) == type_OtherPointer) {
	switch (TypeOf(*(lispobj *)(thing-type_OtherPointer))) {
	  case type_SymbolHeader:
	    for (cons = SymbolValue(INITIAL_FDEFN_OBJECTS);
		 cons != NIL;
		 cons = CONS(cons)->cdr) {
		if (FDEFN(CONS(cons)->car)->name == thing) {
		    thing = CONS(cons)->car;
		    goto fdefn;
		}
	    }
	    printf("symbol 0x%08lx is undefined.\n", thing);
	    return;

	  case type_Fdefn:
	  fdefn:
	    function = FDEFN(thing)->function;
	    if (function == NIL) {
		printf("fdefn 0x%08lx is undefined.\n", thing);
		return;
	    }
	    break;
	  default:
	    printf(
	      "0x%08lx is not a function pointer, symbol, or fdefn object.\n",
		   thing);
	    return;
	}
    }
    else if (LowtagOf(thing) != type_FunctionPointer) {
        printf("0x%08lx is not a function pointer, symbol, or fdefn object.\n",
	       thing);
        return;
    }
    else
	function = thing;

    numargs = 0;
    while (more_p(ptr)) {
	if (numargs >= 3) {
	    printf("Too many arguments.  3 at most.\n");
	    return;
	}
	args[numargs++] = parse_lispobj(ptr);
    }

    switch (numargs) {
      case 0:
	result = funcall0(function);
	break;
      case 1:
	result = funcall1(function, args[0]);
	break;
      case 2:
	result = funcall2(function, args[0], args[1]);
	break;
      case 3:
	result = funcall3(function, args[0], args[1], args[2]);
	break;
    }

    print(result);
}

static void flush_cmd(char **ptr)
{
    flush_vars();
}

static void quit(char **ptr)
{
    char buf[10];

    printf("Really quit? [y] ");
    fflush(stdout);
    fgets(buf, sizeof(buf), stdin);
    if (buf[0] == 'y' || buf[0] == 'Y' || buf[0] == '\n')
        exit(0);
}

static void help(char **ptr)
{
    struct cmd *cmd;

    for (cmd = Cmds; cmd->cmd != NULL; cmd++)
        if (cmd->help != NULL)
            printf("%s\t%s\n", cmd->cmd, cmd->help);
}

static int done;

static void exit_cmd(char **ptr)
{
    done = TRUE;
}

#if 0
static void gc_cmd(char **ptr)
{
    collect_garbage();
}
#endif

static void purify_cmd(char **ptr)
{
    purify(NIL, NIL);
}

static void print_context(struct sigcontext *context)
{
	int i;

	for (i = 0; i < NREGS; i++) {
		printf("%s:\t", lisp_register_names[i]);
#ifdef i386
		brief_print((lispobj) SC_REG(context, i*2));
#else
		brief_print((lispobj) SC_REG(context, i));
#endif
	}
	printf("PC:\t\t  0x%08lx\n", SC_PC(context));
}

static void print_context_cmd(char **ptr)
{
	int free;

	free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
	
        if (more_p(ptr)) {
		int index;

		index = parse_number(ptr);

		if ((index >= 0) && (index < free)) {
			printf("There are %d interrupt contexts.\n", free);
			printf("Printing context %d\n", index);
			print_context(lisp_interrupt_contexts[index]);
		} else {
			printf("There aren't that many/few contexts.\n");
			printf("There are %d interrupt contexts.\n", free);
		}
	} else {
		if (free == 0)
			printf("There are no interrupt contexts!\n");
		else {
			printf("There are %d interrupt contexts.\n", free);
			printf("Printing context %d\n", free - 1);
			print_context(lisp_interrupt_contexts[free - 1]);
		}
	}
}

static void backtrace_cmd(char **ptr)
{
    void backtrace(int frames);
    int n;

    if (more_p(ptr))
	n = parse_number(ptr);
    else
	n = 100;

    printf("Backtrace:\n");
    backtrace(n);
}

static void catchers_cmd(char **ptr)
{
    struct catch_block *catch;

    catch = (struct catch_block *)SymbolValue(CURRENT_CATCH_BLOCK);

    if (catch == NULL)
        printf("There are no active catchers!\n");
    else {
        while (catch != NULL) {
#ifndef i386
            printf("0x%08lX:\n\tuwp: 0x%08lX\n\tfp: 0x%08lX\n\tcode: 0x%08lx\n\tentry: 0x%08lx\n\ttag: ",
		   (unsigned long)catch, (unsigned long)(catch->current_uwp),
		   (unsigned long)(catch->current_cont),
		   catch->current_code,
		   catch->entry_pc);
#else
            printf("0x%08lX:\n\tuwp: 0x%08lX\n\tfp: 0x%08lX\n\tcode: 0x%08lx\n\tentry: 0x%08lx\n\ttag: ",
		   (unsigned long)catch, (unsigned long)(catch->current_uwp),
		   (unsigned long)(catch->current_cont),
		   component_ptr_from_pc(catch->entry_pc) + type_OtherPointer,
		   catch->entry_pc);
#endif
            brief_print((lispobj)catch->tag);
            catch = catch->previous_catch;
        }
    }
}

static void grab_sigs_cmd(char **ptr)
{
    extern void sigint_init(void);

    printf("Grabbing signals.\n");
    sigint_init();
}

static void sub_monitor(void)
{
    struct cmd *cmd, *found;
    char buf[256];
    char *line, *ptr, *token;
    int ambig;

    while (!done) {
        printf("ldb> ");
        fflush(stdout);
        line = fgets(buf, sizeof(buf), stdin);
        if (line == NULL) {
	    if (isatty(0)) {
		putchar('\n');
	        continue;
	    }
	    else {
		fprintf(stderr, "\nEOF on something other than a tty.\n");
		exit(0);
	    }
	}
        ptr = line;
        if ((token = parse_token(&ptr)) == NULL)
            continue;
        ambig = 0;
        found = NULL;
        for (cmd = Cmds; cmd->cmd != NULL; cmd++) {
            if (strcmp(token, cmd->cmd) == 0) {
                found = cmd;
                ambig = 0;
                break;
            }
            else if (strncmp(token, cmd->cmd, strlen(token)) == 0) {
                if (found)
                    ambig = 1;
                else
                    found = cmd;
            }
        }
        if (ambig)
            printf("``%s'' is ambiguous.\n", token);
        else if (found == NULL)
            printf("unknown command: ``%s''\n", token);
        else {
            reset_printer();
            (*found->fn)(&ptr);
        }
    }
}

void ldb_monitor()
{
    jmp_buf oldbuf;

    bcopy(curbuf, oldbuf, sizeof(oldbuf));

    printf("LDB monitor\n");

    setjmp(curbuf);

    sub_monitor();

    done = FALSE;

    bcopy(oldbuf, curbuf, sizeof(curbuf));
}

void throw_to_monitor()
{
    longjmp(curbuf, 1);
}
