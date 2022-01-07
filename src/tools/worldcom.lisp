;;; Noise to compile the Lisp world.

(in-package "USER")

(defvar *byte-compile* #+small t #-small :maybe)

(with-compiler-log-file
    ("target:compile-lisp.log"
     :optimize '(optimize (speed 2) (space 2) (inhibit-warnings 2)
			  (debug #-small 2 #+small .5)
			  (safety #-small 1 #+small 0))
     :optimize-interface '(optimize-interface (safety #-small 2 #+small 1)
					      #+small (debug .5))
     :context-declarations
     '(((:or :external (:and (:match "%") (:match "SET"))
	     (:member lisp::%put lisp::%rplaca lisp::%rplacd lisp::%puthash))
	(declare (optimize-interface (safety 2) #+small (debug 1))
		 #+small (optimize (debug 1))))
       ((:or (:and :external :macro)
	     (:match "$PARSE-"))
	(declare (optimize (safety 2))))
       ((:and :external (:match "LIST"))
	(declare (optimize (safety 1))))))
(let ((*byte-compile-top-level* nil))

;;; Set the version of the core.
;;;
(set '*version* (read-line (open "target:VERSION")))

;;; Set the build time.
;;;
(set '*build-time* (get-universal-time))

;;; these guys need to be first.
(comf "target:code/struct") ; For structures.
(comf "target:code/sysmacs" :byte-compile *byte-compile* #| FIX |# :load t)

;;; Assembly files.

#-bootstrap
(comf "target:assembly/assemfile")
#+bootstrap
(comf "target:assembly/assemfile" :byte-compile t)
(when (eq c:*backend* c:*native-backend*)
  (load "target:assembly/assemfile"))

(when (c:backend-featurep :pmax)
  (comf "target:assembly/mips/assem-rtns" :assem t)
  (comf "target:assembly/mips/array" :assem t)
  (comf "target:assembly/mips/arith" :assem t)
  (comf "target:assembly/mips/alloc" :assem t))

(when (c:backend-featurep :sparc)
  (comf "target:assembly/sparc/assem-rtns" :assem t)
  (comf "target:assembly/sparc/array" :assem t)
  (comf "target:assembly/sparc/arith" :assem t)
  (comf "target:assembly/sparc/alloc" :assem t))

(when (c:backend-featurep :rt)
  (comf "target:assembly/rt/assem-rtns" :assem t)
  (comf "target:assembly/rt/array" :assem t)
  (comf "target:assembly/rt/arith" :assem t)
  (comf "target:assembly/rt/alloc" :assem t))

(when (c:backend-featurep :hppa)
  (comf "target:assembly/hppa/assem-rtns" :assem t)
  (comf "target:assembly/hppa/array" :assem t)
  (comf "target:assembly/hppa/arith" :assem t)
  (comf "target:assembly/hppa/alloc" :assem t))

(when (c:backend-featurep :x86)
  (comf "target:assembly/x86/assem-rtns" :assem t)
  (comf "target:assembly/x86/array" :assem t)
  (comf "target:assembly/x86/arith" :assem t)
  (comf "target:assembly/x86/alloc" :assem t))

(when (c:backend-featurep :alpha)
  (comf "target:assembly/alpha/assem-rtns" :assem t)
  (comf "target:assembly/alpha/array" :assem t)
  (comf "target:assembly/alpha/arith" :assem t)
  (comf "target:assembly/alpha/alloc" :assem t))

(when (c:backend-featurep :sgi)
  (comf "target:assembly/mips/assem-rtns" :assem t)
  (comf "target:assembly/mips/array" :assem t)
  (comf "target:assembly/mips/arith" :assem t)
  (comf "target:assembly/mips/alloc" :assem t))

;;; these guys can supposedly come in any order, but not really.
;;; some are put at the end so macros don't run interpreted and stuff.

(comf "target:code/globals")
(comf "target:code/kernel")
(comf "target:code/lispinit")
(comf "target:code/fdefinition")

(comf "target:code/error")
#+small
(comf "target:code/error" :byte-compile t)

;;; prevent deftypes from taking effect at compile time so that we don't
;;; install interpreted type expanders causing the compiler to infinitely
;;; recurse.
(defvar *original-%deftype* #'lisp::%deftype)
(setf (fdefinition 'lisp::%deftype) #'list)
(comf "target:code/typedefs")

(with-compilation-unit
  (:optimize '(optimize (safety 2) (debug 2)))
  (comf "target:code/class"))

;; FIX
(comf "target:code/exports" :proceed t)
(load "target:code/exports")

(comf "target:code/eval")   ; FIX
(comf "target:code/type")
(comf "target:compiler/generic/vm-type")
(comf "target:code/type-init")
(comf "target:code/pred")
(setf (fdefinition 'lisp::%deftype) *original-%deftype*)

(comf "target:code/alieneval")
(comf "target:code/c-call")
(comf "target:code/sap")

(comf "target:code/bit-bash")
(comf "target:code/byte-interp")
(comf "target:code/array")
(if (c:backend-featurep :hash-new)
    (comf "target:code/hash-new")
    (comf "target:code/hash"))

(with-compilation-unit
  (:optimize '(optimize (safety 1)))
  (comf "target:code/list")
  (comf "target:code/seq")) ; seq must come after list

(comf "target:code/string")
(comf "target:code/mipsstrops")

(if (c:backend-featurep :glibc2)
    (comf "target:code/unix-glibc2" :proceed t)
    (comf "target:code/unix" :proceed t))

(when (c:backend-featurep :mach)
  (comf "target:code/mach")
  (comf "target:code/mach-os"))
(when (c:backend-featurep :sunos)
  (comf "target:code/sunos-os"))
(when (c:backend-featurep :hpux)
  (comf "target:code/hpux-os"))
(when (c:backend-featurep :osf1)
  (comf "target:code/osf1-os"))
(when (c:backend-featurep :irix)
  (comf "target:code/irix-os"))
(when (c:backend-featurep :BSD)
  (comf "target:code/bsd-os"))
(when (c:backend-featurep :Linux)
  (comf "target:code/linux-os"))

(when (c:backend-featurep :pmax)
  (comf "target:code/pmax-vm"))
(when (c:backend-featurep :sparc)
  (if (c:backend-featurep :svr4)
      (comf "target:code/sparc-svr4-vm")
      (comf "target:code/sparc-vm")))
(when (c:backend-featurep :rt)
  (comf "target:code/rt-vm"))
(when (c:backend-featurep :hppa)
  (comf "target:code/hppa-vm"))
(when (c:backend-featurep :x86)
  (comf "target:code/x86-vm"))
(when (c:backend-featurep :alpha)
  (comf "target:code/alpha-vm"))
(when (c:backend-featurep :sgi)
  (comf "target:code/sgi-vm"))

(comf "target:code/symbol")
(comf "target:code/bignum")
(comf "target:code/numbers")
(comf "target:code/float-trap")
(comf "target:code/float")
(comf "target:code/irrat")

(comf "target:code/type-boot")

(comf "target:compiler/proclaim")

(comf "target:code/char")
(comf "target:code/misc")
(comf "target:code/extensions" :byte-compile t)
(comf "target:code/commandline")

(unless (c:backend-featurep :gengc)
  (comf "target:code/room")
  (comf "target:code/gc")
  (comf "target:code/purify"))
(when (c:backend-featurep :gengc)
  (comf "target:code/gengc")
  (comf "target:code/scavhook"))
(when (c:backend-featurep :gencgc)
  (comf "target:code/scavhook"))

(comf "target:code/save")

(comf "target:code/stream")
(comf "target:code/print")
(comf "target:code/pprint")
#-no-runtime (comf "target:code/pprint" :byte-compile t)
(comf "target:code/format")
#-no-runtime (comf "target:code/format" :byte-compile t)

(comf "target:code/package")
(comf "target:code/reader")
#-no-runtime (comf "target:code/reader" :byte-compile t)
(comf "target:code/sharpm" :byte-compile *byte-compile*)
(comf "target:code/backq" :byte-compile *byte-compile*)

(comf "target:code/serve-event")
(comf "target:code/fd-stream")
(comf "target:code/pathname")
#-no-runtime (comf "target:code/pathname" :byte-compile t)
(comf "target:code/filesys")
#-no-runtime (comf "target:code/filesys" :byte-compile t)
(comf "target:code/load")

(comf "target:code/eval")

(comf "target:code/signal")
(comf "target:code/interr")
(comf "target:code/debug-info")
(comf "target:code/debug-int")
(comf "target:code/debug")
#+small
(comf "target:code/debug" :byte-compile t)

(comf "target:code/query" :byte-compile *byte-compile*)
(if (c:backend-featurep :random-mt19937)
    (comf "target:code/rand-mt19937")
    (comf "target:code/rand"))
(comf "target:code/ntrace" :byte-compile *byte-compile*)
(comf "target:code/profile")
(comf "target:code/sort")
(comf "target:code/time")
(comf "target:code/weak")
(comf "target:code/final")

;; FIX most rest libs? libs required by ed? make ed a lib?

;;; Later so that miscellaneous structures are defined (not crucial, but nice.)
(comf "target:code/describe" :byte-compile *byte-compile*)
(comf "target:code/tty-inspect" :byte-compile *byte-compile*)

(comf "target:code/calendar")
(comf "target:code/format-time")
#-no-runtime (comf "target:code/format-time")
(comf "target:code/parse-time")
#-no-runtime (comf "target:code/parse-time")

(comf "target:code/run-program" :proceed t)

(comf "target:code/loop" :byte-compile *byte-compile*)

(comf "target:code/foreign")
(comf "target:code/internet")
(comf "target:code/wire")
(comf "target:code/remote")
;; FIX
(if (find-package "INTERNET")
    (rename-package "INTERNET" "INTERNET" nil)
    (make-package "INTERNET" :nicknames 'nil :use nil))
(use-package '("LISP" "EXTENSIONS") "INTERNET")
(comf "target:code/inet" :load t) ; FIX load for do-remote-directory

;; FIX
(if (find-package "FTP")
    (rename-package "FTP" "FTP" nil)
    (make-package "FTP" :nicknames 'nil :use nil))
(use-package '("LISP") "FTP")

(comf "target:code/ftp")
(comf "target:code/shell")
(comf "target:code/base64")

(when (c:backend-featurep :mp)
  (comf "target:code/multi-proc"))

(comf "target:code/table")
(comf "target:code/doc")
(comf "target:code/build")

(comf "target:code/dired")
(comf "target:code/deftest")
(comf "target:code/config")
(comf "target:code/mh")
(comf "target:code/db")
(comf "target:code/terminal")
(comf "target:code/sync")
(comf "target:code/parse")
(comf "target:code/packagem")
(comf "target:code/docnode")

#+clx (comf "target:code/xlib" :load t) ; FIX load
#+clx (comf "target:code/clx-ext" :load t) ; FIX load

(comf "target:code/setf-funs")
;(comf "target:code/exports" :proceed t)

;;; Compile basic macros that we assume are already in the compilation
;;; environment.  Inhibit compile-time definition to prevent these
;;; functions from becoming interpreted.  In some cases, this is necessary
;;; for compilation to work at all, since the expander functions are lazily
;;; converted: we could go into an infinite recursion trying to convert the
;;; definition of a macro which uses itself.
;;;
(let ((c:*compile-time-define-macros* nil))
  (comf "target:code/defstruct")
  (comf "target:code/defmacro" :byte-compile *byte-compile*)
  (comf "target:compiler/globaldb")
  (comf "target:code/macros")
  #-no-runtime (comf "target:code/macros" :byte-compile t))

;;; Install the site init file.
;;;
(or (probe-file "target:site-init.lisp")
    (dired::copy-file "target:code/dist-site-init.lisp" "target:site-init.lisp"))

); let *byte-compile-top-level*

); with-compiler-log-file
