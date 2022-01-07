;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
;;; $Header: /project/cmucl/cvsroot/src/tools/worldload.lisp,v 1.96 2002/05/16 19:52:58 pmai Exp $
;;;
;;; **********************************************************************
;;;
;;; This file loads the parts of the system that aren't cold loaded and saves
;;; the resulting core image.  It writes "lisp.core" in the DEFAULT-DIRECTORY.

(in-package "LISP")

;;; Since it is unlikely that native code top-level forms are moved
;;; before being executed during worldload it is probably safe to load
;;; these into the dynamic space under CGC even without enabling
;;; dynamic space code below.
;(setf *load-x86-tlf-to-dynamic-space* t)

;;; Purify and GENCGC can move native code so all code can be loading
;;; into the dynamic space during worldload; overrides the above when
;;; enabled. Enable by default for GENCGC.  May also be safe with CGC
;;; but untested.
;(setf cl::*enable-dynamic-space-code* t)

(load (open "target:features.lisp"))

;;; Get the version of the core.
;;;
(set '*lisp-implementation-version* (read-line (open "target:VERSION")))

;;; Load the rest of the reader (maybe byte-compiled.)
(maybe-byte-load "target:code/sharpm")
(maybe-byte-load "target:code/backq")
(setq std-lisp-readtable (copy-readtable *readtable*))

;;; The pretty printer is part of the kernel core, but we don't turn it in
;;; until now.
;;;
(pp::pprint-init)


(maybe-byte-load "target:code/extensions")
(maybe-byte-load "target:code/defmacro")
(maybe-byte-load "target:code/sysmacs")

;;; Define a bunch of search lists relative to target:
;;;
(setf (ext:search-list "code:") '("target:code/"))
(setf (ext:search-list "c:") '("target:compiler/"))
(setf (ext:search-list "vm:")
      '(#+(or pmax sgi) "c:mips/"
        #+sparc "c:sparc/"
	#+rt "c:rt/"
	#+hppa "c:hppa/"
	#+x86 "c:x86/"
	#+alpha "c:alpha/"
	#+ppc "c:ppc/"
	"c:generic/"))
(setf (ext:search-list "assem:")
      '(#+(or pmax sgi) "target:assembly/mips/"
	#+sparc "target:assembly/sparc/"
	#+rt "target:assembly/rt/"
	#+hppa "target:assembly/hppa/"
	#+x86 "target:assembly/x86/"
	#+alpha "target:assembly/alpha/"
	#+ppc "target:assembly/ppc/"
	"target:assembly/"))
(setf (ext:search-list "hem:") '("target:hemlock/"))
(setf (ext:search-list "clx:") '("target:clx/"))
(setf (ext:search-list "pcl:") '("target:pcl/"))
(setf (ext:search-list "tools:") '("target:tools/"))

;;; Make sure the package structure is correct.
;;;
(maybe-byte-load "code:exports")

;;; Load random code sources.

(maybe-byte-load "code:format-time")
(maybe-byte-load "code:parse-time")
#-gengc (maybe-byte-load "code:purify")
(maybe-byte-load "code:commandline")
(maybe-byte-load "code:sort")
(maybe-byte-load "code:time")
(maybe-byte-load "code:tty-inspect")
(maybe-byte-load "code:describe")
#+random-mt19937 (maybe-byte-load "code:rand-mt19937")
#-random-mt19937 (maybe-byte-load "code:rand")
(maybe-byte-load "code:ntrace")
#-runtime (maybe-byte-load "code:profile")
(maybe-byte-load "code:weak")
(maybe-byte-load "code:final")
(maybe-byte-load "code:sysmacs")
#-gengc (maybe-byte-load "code:run-program")
(maybe-byte-load "code:query")
#-runtime (maybe-byte-load "code:internet")
#-runtime (maybe-byte-load "code:wire")
#-runtime (maybe-byte-load "code:remote")
#-runtime (maybe-byte-load "code:ftp")
(maybe-byte-load "code:foreign")
(maybe-byte-load "code:setf-funs")
(maybe-byte-load "code:module")
(maybe-byte-load "code:loop")
#-(or gengc runtime) (maybe-byte-load "code:room")
#-runtime (maybe-byte-load "code:base64")

;;; Overwrite some cold-loaded stuff with byte-compiled versions, if any.
#-(or gengc cgc)	; x86/cgc has stuff in static space.
(progn
  (byte-load-over "target:code/debug")
  (byte-load-over "target:code/error")
  (maybe-byte-load "target:code/pprint" nil)
  (maybe-byte-load "target:code/format" nil)
  (maybe-byte-load "target:code/reader" nil)
  (maybe-byte-load "target:code/pathname" nil)
  (maybe-byte-load "target:code/filesys" nil)
  (maybe-byte-load "target:code/macros" nil))

(purify :root-structures
	`(lisp::%top-level extensions:save-lisp ,lisp::fop-codes)
	:environment-name "Kernel")

;;; Load the compiler.
#-(or no-compiler runtime)
(progn
  (maybe-byte-load "c:loadcom.lisp")
  (purify :root-structures '(compile-file)
	  :environment-name "Compiler")

  (maybe-byte-load "c:loadbackend.lisp")
  ;;
  ;; If we want a small core, blow away the meta-compile time VOP info.
  ;; Redundant clrhash to work around gc leakage.
  #+small
  (progn
    (clrhash (c::backend-parsed-vops c:*backend*))
    (setf (c::backend-parsed-vops c:*backend*)
	  (make-hash-table :test #'eq)))

  (purify :root-structures (list c:*backend*)
	  :environment-name (concatenate 'string (c:backend-name c:*backend*)
					 " backend")))

;;; Hemlock.
;;;
#-(or no-hemlock runtime)
(maybe-byte-load "target:hemlock/hemlock-library")

#+(or no-compiler runtime) (proclaim '(special *target-sl*))
#-(or no-compiler runtime) (defvar *target-sl*)
(setq *target-sl* (search-list "target:"))

#+(or no-compiler runtime) (proclaim '(special *target-core-name*))
#-(or no-compiler runtime) (defvar *target-core-name*)
(setq *target-core-name* (unix-namestring "target:lisp/lisp.core" nil))

;;; Don't include the search lists used for loading in the resultant core.
;;;
(lisp::clear-all-search-lists)

;;; Set up a default for modules and target:
;;; 
(setf (search-list "modules:") '("./"))
(setf (search-list "target:") *target-sl*)

;;; Okay, build the core!
;;;
(progn
  ;; We want to be in the USER package when the command line switches run.
  (in-package "USER")
  ;; Clean random top-level specials.
  (setq - nil)
  (setq + nil)
  (setq * nil)
  (setq / nil)
  (setq ++ nil)
  (setq ** nil)
  (setq // nil)
  (setq +++ nil)
  (setq *** nil)
  (setq /// nil)
  ;; 
  ;; Enable the garbage collector.  But first fake it into thinking that
  ;; we don't need to garbage collect.  The save-lisp is going to call purify
  ;; so any garbage will be collected then.
  #-gengc (setf *need-to-collect-garbage* nil)
  #-gengc (gc-on)
  (setf *gc-run-time* 0)

  ;; Disable the loading of native code top level forms into the
  ;; dynamic space under CGC as it is potentially dangerous if a
  ;; native code top level form is executed after being moved without
  ;; fixups.
  #+x86 (setf *load-x86-tlf-to-dynamic-space* nil)

  ;;; GENCGC can move native code so all code can be loaded into the
  ;;; dynamic space; overrides the above when enabled.
  #+gencgc (setf cl::*enable-dynamic-space-code* t)
  ;;; Reset the counter of the number of native code fixups.
  #+x86 (setf x86::*num-fixups* 0)

;   ;; Maybe enable ANSI defstruct :print-function/:print-object processing
;   #-NO-PCL
;   (setq ext:*ansi-defstruct-options-p* t)
  ;;
  ;; Save the lisp.  If RUNTIME, there is nothing new to purify, so don't.
  ;; the following features are only used to control the build
  ;; process, so we remove them from the generated image
  (setq *features*
	(nreverse
	 (set-difference
	  *features* 
	  '(:runtime :no-compiler :no-pcl :no-clx :no-clm :no-hemlock))))
  (save-lisp *target-core-name*
             :root-structures
             #-(or runtime no-hemlock) `(ed ,hi::*global-command-table*)
             #+(or runtime no-hemlock) ()
             :purify #+runtime nil #-runtime t))