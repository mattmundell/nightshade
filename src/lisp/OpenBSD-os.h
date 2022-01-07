/*

 $Header: /project/cmucl/cvsroot/src/lisp/OpenBSD-os.h,v 1.1 2001/12/06 19:15:44 pmai Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>

#define MAP_ANONYMOUS MAP_ANON
#define MAP_VARIABLE 0

typedef caddr_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

#define POSIX_SIGS
#define HANDLER_ARGS int signal, siginfo_t *code, struct sigcontext *context
#define CODE(code)  ((code) ? code->si_code : 0)

#define uc_sigmask sc_mask
int sc_reg(struct sigcontext*,int);
