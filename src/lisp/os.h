/*
 * $Header: /project/cmucl/cvsroot/src/lisp/os.h,v 1.9 2002/01/28 20:17:11 pmai Exp $
 *
 * Common interface for os-dependent functions.
 *
 */

#if !defined(_OS_H_INCLUDED_)

#define _OS_H_INCLUDED_

#include "lisp.h"

#ifdef MACH
#include "mach-os.h"
#else
#ifdef sun
#include "sunos-os.h"
#else
#ifdef hpux
#include "hpux-os.h"
#else
#ifdef osf1
#include "osf1-os.h"
#else
#ifdef irix
#include "irix-os.h"
#else
#ifdef __FreeBSD__
#include "FreeBSD-os.h"
#else
#ifdef __OpenBSD__
#include "OpenBSD-os.h"
#else
#ifdef __NetBSD__
#include "NetBSD-os.h"
#else
#ifdef __linux__
#include "Linux-os.h"
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif

#ifndef HANDLER_ARGS
#define HANDLER_ARGS int signal, int code, struct sigcontext *context
#endif
#ifndef CODE
#define CODE(code)  code
#endif
#ifndef CTXT_SIGMASK
#define CTXT_SIGMASK(context) context->sc_mask
#endif
#ifndef SAVE_CONTEXT
#define SAVE_CONTEXT() do {} while(0)
#endif

/* Some OSes have their reasons not to use SA_SIGINFO. */

#ifdef POSIX_SIGS
#if !defined(USE_SA_SIGINFO)
#define USE_SA_SIGINFO SA_SIGINFO
#endif
#endif

#define OS_VM_PROT_ALL (OS_VM_PROT_READ|OS_VM_PROT_WRITE|OS_VM_PROT_EXECUTE)

extern os_vm_size_t os_vm_page_size;

extern void os_init(void);
extern void os_install_interrupt_handlers(void);

extern os_vm_address_t os_allocate(os_vm_size_t len);
extern os_vm_address_t os_allocate_at(os_vm_address_t addr, os_vm_size_t len);
extern os_vm_address_t os_reallocate(os_vm_address_t addr,
				     os_vm_size_t old_len,
				     os_vm_size_t len);
extern void os_deallocate(os_vm_address_t addr, os_vm_size_t len);
extern void os_zero(os_vm_address_t addr, os_vm_size_t length);

extern os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len);
extern void os_invalidate(os_vm_address_t addr, os_vm_size_t len);
extern os_vm_address_t os_map(int fd, int offset, os_vm_address_t addr,
			      os_vm_size_t len);
extern void os_flush_icache(os_vm_address_t addr, os_vm_size_t len);
extern void os_protect(os_vm_address_t addr, os_vm_size_t len,
		       os_vm_prot_t protection);
extern boolean valid_addr(os_vm_address_t test);

#define os_trunc_to_page(addr) \
    (os_vm_address_t)(((long)(addr))&~(os_vm_page_size-1))
#define os_round_up_to_page(addr) \
    os_trunc_to_page((addr)+(os_vm_page_size-1))

#define os_trunc_size_to_page(size) \
    (os_vm_size_t)(((long)(size))&~(os_vm_page_size-1))
#define os_round_up_size_to_page(size) \
    os_trunc_size_to_page((size)+(os_vm_page_size-1))

#endif
