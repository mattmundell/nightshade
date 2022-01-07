/*

 $Header: /home/CVS-cmucl/src/lisp/FreeBSD-os.h,v 1.1.2.2 2000/05/23 16:38:11 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <osreldate.h>
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

/* I *think* this is when things became incompatible with old
   signals.
*/
#if __FreeBSD_version > 400010
#define POSIX_SIGS
int
/* If we used SA_SIGINFO in sigaction() the third argument to signal
   handlers would be a struct ucontext_t.  (The manpage for
   sigaction(2) is wrong!)  Sigcontext and ucontext_t are
   "compatible", but access to registers in a ucontext_t goes through
   the uc_mcontext field, so we just won't bother.
*/
#define USE_SA_SIGINFO 0
#define uc_sigmask sc_mask
sc_reg(struct sigcontext*,int);
#endif

