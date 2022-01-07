;;; Record definitions needed for the interface to Mach.

(in-package "MACH")

(export '(sigcontext-onstack sigcontext-mask sigcontext-pc sigcontext-regs
	  sigcontext-mdlo sigcontext-mdhi sigcontext-ownedfp sigcontext-fpregs
	  sigcontext-fpc_csr sigcontext-fpc_eir sigcontext-cause
	  sigcontext-badvaddr sigcontext-badpaddr sigcontext *sigcontext
	  indirect-*sigcontext))

(def-c-record sigcontext
  (onstack unsigned-long)
  (mask unsigned-long)
  (pc system-area-pointer)
  (regs int-array)
  (mdlo unsigned-long)
  (mdhi unsigned-long)
  (ownedfp unsigned-long)
  (fpregs int-array)
  (fpc_csr unsigned-long)
  (fpc_eir unsigned-long)
  (cause unsigned-long)
  (badvaddr system-area-pointer)
  (badpaddr system-area-pointer))
