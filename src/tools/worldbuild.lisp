;;; Build a core image from all the .fasl files that are part of the Lisp
;;; kernel.

(in-package "LISP")

(or (fboundp 'genesis)
    (load "target:compiler/generic/new-genesis"))

(defparameter lisp-files
  `(,@(when (c:backend-featurep :pmax)
	'("target:assembly/mips/assem-rtns.assem"
	  "target:assembly/mips/array.assem"
	  "target:assembly/mips/arith.assem"
	  "target:assembly/mips/alloc.assem"))
    ,@(when (c:backend-featurep :sparc)
	'("target:assembly/sparc/assem-rtns.assem"
	  "target:assembly/sparc/array.assem"
	  "target:assembly/sparc/arith.assem"
	  "target:assembly/sparc/alloc.assem"))
    ,@(when (c:backend-featurep :rt)
	'("target:assembly/rt/assem-rtns.assem"
	  "target:assembly/rt/array.assem"
	  "target:assembly/rt/arith.assem"
	  "target:assembly/rt/alloc.assem"))
    ,@(when (c:backend-featurep :hppa)
	'("target:assembly/hppa/assem-rtns.assem"
	  "target:assembly/hppa/array.assem"
	  "target:assembly/hppa/arith.assem"
	  "target:assembly/hppa/alloc.assem"))
    ,@(when (c:backend-featurep :x86)
	'("target:assembly/x86/assem-rtns.assem"
	  "target:assembly/x86/array.assem"
	  "target:assembly/x86/arith.assem"
	  "target:assembly/x86/alloc.assem"))
    ,@(when (c:backend-featurep :alpha)
	'("target:assembly/alpha/assem-rtns.assem"
	  "target:assembly/alpha/array.assem"
	  "target:assembly/alpha/arith.assem"
	  "target:assembly/alpha/alloc.assem"))
    ,@(when (c:backend-featurep :sgi)
	'("target:assembly/mips/assem-rtns.assem"
	  "target:assembly/mips/array.assem"
	  "target:assembly/mips/arith.assem"
	  "target:assembly/mips/alloc.assem"))

    "target:code/type-boot"
    "target:code/fdefinition"
    "target:code/eval"

    "target:code/struct"
    "target:code/typedefs"
    "target:code/class"
    "target:code/type"
    "target:compiler/generic/vm-type"
    "target:code/type-init"
    "target:code/error"

    "target:code/defstruct"
    "target:compiler/proclaim"
    "target:compiler/globaldb"
    "target:code/pred"

    "target:code/pathname"
    "target:code/filesys"

    "target:code/kernel"
    "target:code/bit-bash"
    "target:code/byte-interp"
    "target:code/array"
    "target:code/char"
    "target:code/table"
    "target:code/lispinit"
    "target:code/seq"
    "target:code/numbers"
    "target:code/float"
    "target:code/float-trap"
    "target:code/irrat"
    "target:code/bignum"
    "target:code/list"
    ,@(if (c:backend-featurep :hash-new)
	'("target:code/hash-new")
	'("target:code/hash"))
    "target:code/macros"
    "target:code/symbol"
    "target:code/string"
    "target:code/mipsstrops"
    "target:code/misc"
    ,@(unless (c:backend-featurep :gengc)
	'("target:code/gc"))
    ,@(when (c:backend-featurep :gengc)
	'("target:code/gengc"
	  "target:code/scavhook"))
    ,@(when (c:backend-featurep :gencgc)
	'("target:code/scavhook"))

    "target:code/save"
    ,@(if (c:backend-featurep :random-mt19937)
	  '("target:code/rand-mt19937")
	  '("target:code/rand"))
    "target:code/alieneval"
    "target:code/c-call"
    "target:code/sap"
    ,@(if (c:backend-featurep :glibc2)
	  '("target:code/unix-glibc2")
	  '("target:code/unix"))
    ,@(when (c:backend-featurep :mach)
	'("target:code/mach"
	  "target:code/mach-os"))
    ,@(when (c:backend-featurep :sunos)
	'("target:code/sunos-os"))
    ,@(when (c:backend-featurep :hpux)
        '("target:code/hpux-os"))
    ,@(when (c:backend-featurep :osf1)
	'("target:code/osf1-os"))
    ,@(when (c:backend-featurep :irix)
	'("target:code/irix-os"))
    ,@(when (c:backend-featurep :FreeBSD)
	'("target:code/bsd-os"))
    ,@(when (c:backend-featurep :Linux)
	'("target:code/linux-os"))
    "target:code/serve-event"
    "target:code/stream"
    "target:code/fd-stream"
    "target:code/print"
    "target:code/pprint"
    "target:code/format"
    "target:code/package"
    "target:code/reader"
    "target:code/load"
    ,@(when (c:backend-featurep :pmax)
	'("target:code/pmax-vm"))
    ,@(when (c:backend-featurep :sparc)
	(if (c:backend-featurep :svr4)
	    '("target:code/sparc-svr4-vm")
	    '("target:code/sparc-vm")))
    ,@(when (c:backend-featurep :rt)
	'("target:code/rt-vm"))
    ,@(when (c:backend-featurep :hppa)
	'("target:code/hppa-vm"))
    ,@(when (c:backend-featurep :x86)
	'("target:code/x86-vm"))
    ,@(when (c:backend-featurep :alpha)
	'("target:code/alpha-vm"))
    ,@(when (c:backend-featurep :sgi)
	'("target:code/sgi-vm"))

    "target:code/signal"
    "target:code/interr"
    "target:code/debug-info"
    "target:code/debug-int"
    "target:code/debug"

    ,@(when (c:backend-featurep :clx)
	'("target:code/xlib"
	  "target:code/clx-ext"))

    ,@(when (c:backend-featurep :mp)
	'("target:code/multi-proc"))))

(setf *genesis-core-name*
      #+(and mach sparc) "/usr/tmp/kernel.core"
      #-(and mach sparc) "target:lisp/kernel.core")
(setf *genesis-c-header-name* "target:lisp/internals.h")
(setf *genesis-symbol-table* "target:lisp/nightshade.nm")
(setf *genesis-map-name* "target:lisp/nightshade.map")

(when (boundp '*target-page-size*)
  (locally (declare (optimize (inhibit-warnings 3)))
    (setf *target-page-size*
	  (c:backend-page-size c:*backend*))))

(genesis lisp-files)
