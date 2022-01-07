;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/gc.lisp,v 1.26 2001/04/10 13:42:44 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Garbage collection and allocation related code.
;;;
;;; Written by Christopher Hoover, Rob MacLachlan, Dave McDonald, et al.
;;; New code for MIPS port by Christopher Hoover.
;;; 

(in-package "EXTENSIONS")
(export '(*before-gc-hooks* *after-gc-hooks* gc gc-on gc-off
	  *bytes-consed-between-gcs* *gc-verbose* *gc-inhibit-hook*
	  *gc-notify-before* *gc-notify-after* get-bytes-consed
	  *gc-run-time* bytes-consed-between-gcs))

(in-package "LISP")
(export '(room))


;;;; DYNAMIC-USAGE and friends.

(declaim (special *read-only-space-free-pointer*
		  *static-space-free-pointer*))

(eval-when (compile eval)
  (defmacro c-var-frob (lisp-fun c-var-name)
    `(progn
       (declaim (inline ,lisp-fun))
       (defun ,lisp-fun ()
	 (alien:extern-alien ,c-var-name (alien:unsigned 32))))))

(c-var-frob read-only-space-start "read_only_space")
(c-var-frob static-space-start "static_space")
(c-var-frob dynamic-0-space-start "dynamic_0_space")
(c-var-frob dynamic-1-space-start "dynamic_1_space")
(c-var-frob control-stack-start "control_stack")
#+x86 (c-var-frob control-stack-end "control_stack_end")
(c-var-frob binding-stack-start "binding_stack")
(c-var-frob current-dynamic-space-start "current_dynamic_space")
(declaim (inline dynamic-usage))

#-(or cgc gencgc)
(defun dynamic-usage ()
  (the (unsigned-byte 32)
       (- (system:sap-int (c::dynamic-space-free-pointer))
	  (current-dynamic-space-start))))

#+(or cgc gencgc)
(c-var-frob dynamic-usage "bytes_allocated")

(defun static-space-usage ()
  (- (* lisp::*static-space-free-pointer* vm:word-bytes)
     (static-space-start)))

(defun read-only-space-usage ()
  (- (* lisp::*read-only-space-free-pointer* vm:word-bytes)
     (read-only-space-start)))

(defun control-stack-usage ()
#-x86 (- (system:sap-int (c::control-stack-pointer-sap)) (control-stack-start))
#+x86 (- (control-stack-end) (system:sap-int (c::control-stack-pointer-sap))) )

(defun binding-stack-usage ()
  (- (system:sap-int (c::binding-stack-pointer-sap)) (binding-stack-start)))


(defun current-dynamic-space ()
  (let ((start (current-dynamic-space-start)))
    (cond ((= start (dynamic-0-space-start))
	   0)
	  ((= start (dynamic-1-space-start))
	   1)
	  (t
	   (error "Oh no.  The current dynamic space is missing!")))))


;;;; Room.

(defun room-minimal-info ()
  (format t "Dynamic Space Usage:    ~10:D bytes.~%" (dynamic-usage))
  (format t "Read-Only Space Usage:  ~10:D bytes.~%" (read-only-space-usage))
  (format t "Static Space Usage:     ~10:D bytes.~%" (static-space-usage))
  (format t "Control Stack Usage:    ~10:D bytes.~%" (control-stack-usage))
  (format t "Binding Stack Usage:    ~10:D bytes.~%" (binding-stack-usage))
  (format t "The current dynamic space is ~D.~%" (current-dynamic-space))
  (format t "Garbage collection is currently ~:[enabled~;DISABLED~].~%"
	  *gc-inhibit*))

(defun room-intermediate-info ()
  (room-minimal-info)
  (vm:memory-usage :count-spaces '(:dynamic)
		   :print-spaces t
		   :cutoff 0.05s0
		   :print-summary nil))

(defun room-maximal-info ()
  (room-minimal-info)
  (vm:memory-usage :count-spaces '(:static :dynamic))
  (vm:instance-usage :dynamic :top-n 10)
  (vm:instance-usage :static :top-n 10))


(defun room (&optional (verbosity :default))
  "Prints to *STANDARD-OUTPUT* information about the state of internal
  storage and its management.  The optional argument controls the
  verbosity of ROOM.  If it is T, ROOM prints out a maximal amount of
  information.  If it is NIL, ROOM prints out a minimal amount of
  information.  If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information.  See also VM:MEMORY-USAGE and
  VM:INSTANCE-USAGE for finer report control."
  (fresh-line)
  (if (fboundp 'vm:memory-usage)
      (case verbosity
	((t)
	 (room-maximal-info))
	((nil)
	 (room-minimal-info))
	(:default
	 (room-intermediate-info))
	(t
	 (error "No way man!  The optional argument to ROOM must be T, NIL, ~
		 or :DEFAULT.~%What do you think you are doing?")))
      (room-minimal-info))
  (values))


;;;; GET-BYTES-CONSED.

;;;
;;; Internal State
;;; 
(defvar *last-bytes-in-use* nil)
(defvar *total-bytes-consed* 0)

(declaim (type (or index null) *last-bytes-in-use*))
(declaim (type integer *total-bytes-consed*))

;;; GET-BYTES-CONSED -- Exported
;;; 
(defun get-bytes-consed ()
  "Returns the number of bytes consed since the first time this function
  was called.  The first time it is called, it returns zero."
  (declare (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (cond ((null *last-bytes-in-use*)
	 (setq *last-bytes-in-use* (dynamic-usage))
	 (setq *total-bytes-consed* 0))
	(t
	 (let ((bytes (dynamic-usage)))
	   (incf *total-bytes-consed*
		 (the index (- bytes *last-bytes-in-use*)))
	   (setq *last-bytes-in-use* bytes))))
  *total-bytes-consed*)


;;;; Variables and Constants.

;;; The number of generations.
;;; 
#+gencgc
(defconstant *gc-generations* 6
  "The number of generations.")

;;; The default value of *BYTES-CONSED-BETWEEN-GCS* and *GC-TRIGGER*.
;;; 
(defconstant default-bytes-consed-between-gcs 2000000)

;;; This variable is the user-settable variable that specifices the
;;; minimum amount of dynamic space which must be consed before a GC
;;; will be triggered.
;;; 
(defvar *bytes-consed-between-gcs* default-bytes-consed-between-gcs
  "This number specifies the minimum number of bytes of dynamic space
   that must be consed before the next gc will occur.")
;;;
(declaim (type index *bytes-consed-between-gcs*))

;;; Public
(defvar *gc-run-time* 0
  "The total CPU time spend doing garbage collection (as reported by
   GET-INTERNAL-RUN-TIME.)")

(declaim (type index *gc-run-time*))

;;; Internal trigger.  When the dynamic usage increases beyond this
;;; amount, the system notes that a garbage collection needs to occur by
;;; setting *NEED-TO-COLLECT-GARBAGE* to T.  It starts out as NIL meaning
;;; nobody has figured out what it should be yet.
;;; 
(defvar *gc-trigger* nil)

(declaim (type (or index null) *gc-trigger*))

;;; On the RT, we store the GC trigger in a ``static'' symbol instead of
;;; letting magic C code handle it.  It gets initialized by the startup
;;; code. The X86 port defines this here because it uses the `ibmrt'
;;; feature in the C code for allocation and binding stack access and
;;; a lot of stuff wants this INTERNAL_GC_TRIGGER available as well.
#+(or ibmrt x86)
(defvar vm::*internal-gc-trigger*)

;;;
;;; The following specials are used to control when garbage collection
;;; occurs.
;;; 

;;; 
;;; *GC-INHIBIT*
;;;
;;; When non-NIL, inhibits garbage collection.
;;; 
(defvar *gc-inhibit* nil)

;;;
;;; *ALREADY-MAYBE-GCING*
;;;
;;; This flag is used to prevent recursive entry into the garbage
;;; collector.
;;; 
(defvar *already-maybe-gcing* nil)

;;; When T, indicates that the dynamic usage has exceeded the value
;;; *GC-TRIGGER*.
;;; 
(defvar *need-to-collect-garbage* nil)


;;;; GC Hooks.

;;;
;;; *BEFORE-GC-HOOKS*
;;; *AFTER-GC-HOOKS*
;;;
;;; These variables are a list of functions which are run before and
;;; after garbage collection occurs.
;;;
(defvar *before-gc-hooks* nil
  "A list of functions that are called before garbage collection occurs.
  The functions should take no arguments.")
;;; 
(defvar *after-gc-hooks* nil
  "A list of functions that are called after garbage collection occurs.
  The functions should take no arguments.")

;;;
;;; *GC-INHIBIT-HOOK*
;;; 
;;; This hook is invoked whenever SUB-GC intends to GC (unless the GC
;;; was explicitly forced by calling EXT:GC).  If the hook function
;;; returns NIL then the GC procedes; otherwise, the GC is inhibited and
;;; *GC-INHIBIT* and *NEED-TO-COLLECT-GARBAGE* are left bound to T.
;;; Presumably someone will call GC-ON later to collect the garbage.
;;;
(defvar *gc-inhibit-hook* nil
  "Should be bound to a function or NIL.  If it is a function, this
  function should take one argument, the current amount of dynamic
  usage.  The function should return NIL if garbage collection should
  continue and non-NIL if it should be inhibited.  Use with caution.")



;;;
;;; *GC-VERBOSE*
;;;
(defvar *gc-verbose* t
  "When non-NIL, causes the functions bound to *GC-NOTIFY-BEFORE* and
  *GC-NOTIFY-AFTER* to be called before and after a garbage collection
  occurs respectively.  If :BEEP, causes the default notify functions to beep
  annoyingly.")


(defun default-gc-notify-before (bytes-in-use)
  (when (eq *gc-verbose* :beep)
    (system:beep *standard-output*))
  (format t "~&[GC threshold exceeded with ~:D bytes in use.  ~
             Commencing GC.]~%" bytes-in-use)
  (finish-output))
;;;
(defparameter *gc-notify-before* #'default-gc-notify-before
  "This function bound to this variable is invoked before GC'ing (unless
  *GC-VERBOSE* is NIL) with the current amount of dynamic usage (in
  bytes).  It should notify the user that the system is going to GC.")

(defun default-gc-notify-after (bytes-retained bytes-freed new-trigger)
  (format t "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%"
	  bytes-retained bytes-freed)
  (format t "[GC will next occur when at least ~:D bytes are in use.]~%"
	  new-trigger)
  (when (eq *gc-verbose* :beep)
    (system:beep *standard-output*))
  (finish-output))
;;;
(defparameter *gc-notify-after* #'default-gc-notify-after
  "The function bound to this variable is invoked after GC'ing (unless
  *GC-VERBOSE* is NIL) with the amount of dynamic usage (in bytes) now
  free, the number of bytes freed by the GC, and the new GC trigger
  threshold.  The function should notify the user that the system has
  finished GC'ing.")


;;;; Internal GC

(in-package "UNIX")    ;; FIX

(alien:def-alien-type nil
   (struct alloc-region
	   (free-pointer (* t))
	   (end-addr (* t))
	   (first-page c-call:int)
	   (last-page c-call:int)
	   (start-addr (* t))))

(alien:def-alien-type lispobj
		      #-alpha c-call:unsigned-long
		      #+alpha #| FIX |# c-call:u32)

;; FIX Linux
(alien:def-alien-type os-vm-address-t unix::caddr-t)
(alien:def-alien-type os-vm-size-t unix::size-t)
(alien:def-alien-type os-vm-offset-t unix::off-t)
(alien:def-alien-type os-vm-prot-t c-call:int)
(defvar read-only-space-start #x10000000)

;; FIX Hack, defined in gencgc.c.
(alien:def-alien-variable "os_vm_prot_read" os-vm-prot-t)
(alien:def-alien-variable "os_vm_prot_write" os-vm-prot-t)
(alien:def-alien-variable "os_vm_prot_execute" os-vm-prot-t)
(alien:def-alien-variable "os_vm_prot_all" os-vm-prot-t)

(alien:def-alien-routine os-protect c-call:void
			 (addr os-vm-address-t)
			 (len os-vm-size-t)
			 (protection os-vm-prot-t))

; FIX next 3 generated from vm::*primitive-objects* into internals.h

(alien:def-alien-type nil
   (struct scavenger-hook
	   (header lispobj)
	   (value lispobj)
	   (function lispobj)
	   (next (* (struct scavenger-hook)))))

(alien:def-alien-type nil
   (struct code
	   (header lispobj)
	   (code-size lispobj)
	   (entry-points lispobj)
	   (debug-info lispobj)
	   (trace-table-offset lispobj)
	   (constants (array lispobj 1))))

(alien:def-alien-type nil
   (struct function
	   (header lispobj)
	   (self lispobj)
	   (next lispobj)
	   (name lispobj)
	   (arglist lispobj)
	   (type lispobj)
	   (code (array c-call:unsigned-char 1))))


(alien:def-alien-type nil
   (struct generation
	   ;; The first page that gc-alloc checks on its next call.
	   (alloc-start-page c-call:int)
	   ;; The first page that gc-alloc-unboxed checks on its next
	   ;; call.
	   (alloc-unboxed-start-page c-call:int)
	   ;; The first page that gc-alloc-large (boxed) considers on
	   ;; its next call.  Although it always allocates after the
	   ;; boxed-region.
	   (alloc-large-start-page c-call:int)
	   ;; The first page that gc-alloc-large (unboxed) considers on
	   ;; its next call.  Although it always allocates after the
	   ;; current-unboxed-region.
	   (alloc-large-unboxed-start-page c-call:int)
	   ;; The bytes allocate to this generation.
	   (bytes-allocated c-call:int)
	   ;; The number of bytes at which to trigger a GC
	   (gc-trigger c-call:int)
	   ;; To calculate a new level for gc-trigger
	   (bytes-consed-between-gc c-call:int)
	   ;; The number of GCs since the last raise.
	   (num-gc c-call:int)
	   ;; The average age at after which a GC will raise objects to
	   ;; the next generation.
	   (trigger-age c-call:int)
	   ;; The cumulative sum of the bytes allocated to this
	   ;; generation. It is cleared after a GC on this generations,
	   ;; and update before new objects are added from a GC of a
	   ;; younger generation. Dividing by the bytes-allocated will
	   ;; give the average age of the memory in this generation
	   ;; since its last GC.
	   (cum-sum-bytes-allocated c-call:int)
	   ;; A minimum average memory age before a GC will occur helps
	   ;; prevent a GC when a large number of new live objects have
	   ;; been added, in which case a GC could be a waste of time.
	   (min-av-mem-age c-call:double)))

(alien:def-alien-type nil
   (struct page
	   ;; Page flags.
	   (flags c-call:unsigned-int)

	   ;; It is important to know the offset to the first object in the
	   ;; page.  Currently it's only important to know if an object
	   ;; starts at the begining of the page in which case the offset
	   ;; would be 0
	   (first-object-offset c-call:int)
  
	   ;; The number of bytes of this page that are used. This may be
	   ;; less than the actual bytes used for pages within the current
	   ;; allocation regions. It should be 0 for all unallocated pages
	   ;; (not hard to achieve).
	   (bytes-used c-call:int)))


(alien:def-alien-variable "alien_nil" (* t))

(alien:def-alien-variable "boxed_region" (struct alloc-region))
(alien:def-alien-variable "unboxed_region" (struct alloc-region))

(alien:def-alien-variable "current_region_free_pointer" (* t))
(alien:def-alien-variable "current_region_end_addr" (* t))

(defvar num-generations 6)

(alien:def-alien-variable "generations" (array (struct generation) 7))

(alien:def-alien-variable "gencgc_oldest_gen_to_gc" c-call:unsigned-int)

(alien:def-alien-variable "gc_alloc_generation" c-call:int)

(alien:def-alien-variable "last_free_page" c-call:int)

(alien:def-alien-variable "page_table" (* (struct page)))

(alien:def-alien-variable "heap_base" (* t))

(alien:def-alien-variable "dynamic_space_pages" c-call:unsigned-int)

(alien:def-alien-variable "undefined_tramp" int)

(alien:def-alien-variable "bytes_allocated" c-call:unsigned-long)

(alien:def-alien-variable "from_space" c-call:int)
(alien:def-alien-variable "new_space" c-call:int)

#+()
(alien:def-alien-routine valid-dynamic-space-pointer c-call:int
			 (pointer (* lispobj)))

(alien:def-alien-routine fpu-save c-call:void
			 (fpu-state (* t)))
(alien:def-alien-routine fpu-restore c-call:void
			 (fpu-state (* t)))

;; FIX this calls ~back into lisp
(alien:def-alien-routine funcall0 c-call:void
			 (function lispobj))

(defvar verify-dynamic-code-check nil
  "Enable the printing of a note when code objects are found in the dynamic
   space during a heap verify.")

(defvar pre-verify-gen-0 nil
  "Enable a pre-scan verify of generation 0 before it's GCed.")

(defvar gencgc-verbose 0
 "The verbose level. All non-error messages are disabled at level 0;
  and only a few rare messages are printed at level 1.")

;; NetBSD on x86 has no way to retrieve the faulting address in the SIGSEGV
;; handler, so for the moment we can't use page protection.
(defvar enable-page-protection #-netbsd t #+netbsd nil
 "Enable the use of page protection to help avoid the scavenging of pages
  that don't have pointers to younger generations.")

; (alien:def-alien-routine hack-ptr (* c-call:void)
;  			 (obj (* c-call:void)))
; (alien:def-alien-routine hack-ptr (* c-call:void)
;  			 (obj (* c-call:void)))
; (alien:def-alien-routine hack-ptr (* c-call:void)
; 			 (obj (* t)))

(defvar lowtag-bits 3)
(defvar lowtag-mask (1- (ash 1 lowtag-bits)))
 
(defun ptr (addr)
  (sap-alien (int-sap (logand (sap-int (alien-sap addr))
			      (logcom lowtag-mask)))
	     (* t)))

#+()
(alien:def-alien-routine lose c-call:void)
#+()
(defmacro gc-assert (&rest body)
  "Continue only if body is true."
  `(or (progn ,@body) (lose)))      ;; FIX Does (lose) work?
(defmacro gc-assert (&rest body)
  "Continue only if body is true."
  (declare (ignore body)))

;; Set when the page is write protected. If it is writen into it is made
;; writable and this flag is cleared. This should always reflect the actual
;; write-protect status of a page.

(defvar page-write-protected-mask #x00000010)

;; Page allocated flag: 0 for a free page; 1 when allocated. If the page is
;; free then the following slots are invalid - well the bytes-used must be
;; 0.

(defvar page-allocated-mask #x00000040)

(defmacro page-allocated-p (page)
  `(if (zerop (logand (slot (deref page-table ,page) 'flags)
		      page-allocated-mask))
       nil t))

;; The generation that this page belongs to. This should be valid for all
;; pages that may have objects allocated, even current allocation region
;; pages - this allows the space of an object to be easily determined.

(defvar page-generation-mask #x0000000f)

(defmacro page-generation (page)
  `(logand (slot (deref page-table ,page) 'flags) page-generation-mask))

(defmacro page-flags (page mask)
  `(logand (slot (deref page-table ,page) 'flags) ,mask))

;; Unboxed region flag: 1 for unboxed objects, 0 for boxed objects.

(defvar page-unboxed-mask #x00000080)
(defvar page-unboxed-shift 7)

(defmacro page-unboxed-p (page)
  `(if (zerop (logand (slot (deref page-table ,page) 'flags)
		      page-unboxed-mask))
       nil t))

(defmacro page-unboxed-val (page)
  `(ash (logand (slot (deref page-table ,page) 'flags)
		page-unboxed-mask)
	(- page-unboxed-shift)))

;; If this page should not be moved during a GC then this flag is set. It's
;; only valid during a GC for allocated pages.

(defvar page-dont-move-mask #x00000100)
(defmacro page-dont-move-p (page)
  `(if (zerop (logand (slot (deref page-table ,page) 'flags)
		      page-dont-move-mask))
       nil t))

;; If the page is part of a large object then this flag is set. No other
;; objects should be allocated to these pages. This is only valid when the
;; page is allocated.

(defvar page-large-object-mask #x00000200)
(defvar page-large-object-shift 9)

(defmacro page-large-object-p (page)
  `(if (zerop (logand (slot (deref page-table ,page) 'flags)
		      page-large-object-mask))
       nil t))

(defvar page-size 4096
  "The smallest page size that can be independently allocated and write
   protected.")

(alien:def-alien-routine page-address (* t)
			 (page-num c-call:int))

(defmacro page-address-sap (page-num)
  "Calculate the start address for the given page number."
  `(sap+ (alien-sap heap-base) (* page-size ,page-num)))

(declaim (inline find-page-index))

(defun find-page-index (addr)
  "Find the page index within the page-table for the given address.  Return
   -1 on failure."
  (let ((index (- (sap-int (alien-sap addr))
		  (sap-int (alien-sap heap-base)))))
    (when (>= index 0)
      (setq index (truncate (/ index page-size)))
      (if (< index dynamic-space-pages)
	  (return-from find-page-index index))))
  -1)

(defun count-write-protect-generation-pages (generation)
  ;; Count the number of write protected pages within the given generation.
  (let ((cnt 0))
    (loop
      for mmask = (logior page-allocated-mask
			  page-write-protected-mask
			  page-generation-mask)
      for mflags = (logior page-allocated-mask
			   page-write-protected-mask
			   generation)
      for i from 0 to (1- last-free-page)
      do
      (if (equal (page-flags i mmask) mflags)
	  (incf cnt)))
    cnt))

(defun count-generation-pages (generation)
  ;; Count the number of pages within the given generation.
  (let ((cnt 0))
    (loop
      for mmask = (logior page-allocated-mask page-generation-mask)
      for mflags = (logior page-allocated-mask generation)
      for i from 0 to (1- last-free-page)
      do
      (if (equal (page-flags i mmask) mflags)
	  (incf cnt)))
    cnt))

(defun generation-bytes-allocated (generation)
  "Work through the pages and add up the number of bytes used for the given
   generation."
  (let ((bytes-allocated 0))
    (loop
      for mmask = (logior page-allocated-mask page-generation-mask)
      for mflags = (logior page-allocated-mask generation)
      for i from 0 to (1- last-free-page)
      do
      (if (equal (page-flags i mmask) mflags)
	  (incf bytes-allocated (slot (deref page-table i) 'bytes-used))))
    bytes-allocated))

(defun gen-av-mem-age (gen)
  (if (eq (slot (deref generations gen) 'bytes-allocated) 0)
      0.0d0)

  (/ (coerce (slot (deref generations gen) 'cum-sum-bytes-allocated)
	     'double-float)
     (coerce (slot (deref generations gen) 'bytes-allocated)
	     'double-float)))

(defmacro pointerp (obj)
  `(if (zerop (logand ,obj #x01)) nil t))

(defvar type-bits 8)
(defvar type-mask (1- (ash 1 type-bits)))
(defmacro vm-type-id-of (object)
  `(logand ,object type-mask))
(defmacro header-value (object)
  `(ash (- type-bits) ,object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scavenging/transporting routines derived from gc.c

;; FIX from internals.h
(alien:def-alien-type nil
   (struct weak-pointer
	   (header lispobj)
	   (value lispobj)
	   (broken lispobj)
	   (next (* (struct weak-pointer)))))

;static int (*scavtab[256])(lispobj *where, lispobj object);
;static lispobj (*transother[256])(lispobj object);
(alien:def-alien-variable "sizetab"
			  (array (* (function c-call:int (* lispobj)))
				 256))

(alien:def-alien-variable "weak_pointers" (* (struct weak-pointer)))

(alien:def-alien-variable "scavenger_hooks" (* (struct scavenger-hook)))

(defmacro gc-ceiling (x y) `(logand (+ ,x (1- ,y)) (logcom (1- ,y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun print-ptr (addr)
  "Print out some information about the pointer at ADDR."
  ;; If addr is in the dynamic space then print out the page information.
  (let ((pi1 (find-page-index addr)))
    (or (equal pi1 -1)
	(format *stderr*
		"  ~X: page ~D  alloc ~D unboxed ~D gen ~D  bytes-used ~D  offset ~D  dont-move ~D~%"
		addr pi1 (page-allocated-p pi1) (page-unboxed-p pi1)
		(page-generation pi1)
		(slot (deref page-table pi1) 'bytes-used)
		(slot (deref page-table pi1) 'first-object-offset)
		(page-dont-move-p pi1)))
    (flet ((off- (addr offset)
	     (deref (sap-alien (sap+ (alien-sap addr)
				     (- (* offset (alien-size lispobj :bytes))))
			       (* lispobj))))
	   (off+ (addr offset)
	     (deref (sap-alien (sap+ (alien-sap addr)
				     (* offset (alien-size lispobj :bytes)))
			       (* lispobj)))))
      (format *stderr*
	      "  ~X ~X ~X ~X (~X) ~X ~X ~X ~X~%"
	      (off- addr 4) (off- addr 3) (off- addr 2) (off- addr 1)
	      (off- addr 0)
	      (off+ addr 1) (off+ addr 2) (off+ addr 3) (off+ addr 4)))))

(alien:def-alien-routine verify-space c-call:void
			 (start (* lispobj))
			 (words unix::size-t))

#+()
(defun verify-space (start words)
  (declare (type (alien (* lispobj)) start))
  (let ((dynamic-space (if (eq (find-page-index start) -1) nil t))
	(readonly-space (and (<= read-only-space-start
				 (sap-int (alien-sap start)))
			     (< (sap-int (alien-sap start))
				lisp::*read-only-space-free-pointer*))))
    (loop
      for count = 1 then 1
      for thing = (deref start) then (deref start)
      while (> words 0)
      do
      (if (pointerp thing)
	  (let ((page-index (find-page-index
			     (sap-alien (int-sap thing) (* t))))
		(to-readonly-space
		 (and (<= read-only-space-start thing)
		      (< thing
			 lisp::*read-only-space-free-pointer*)))
		(to-static-space
		 (and (<= (lisp::static-space-start)
			  thing)
		      (< thing
			 lisp::*static-space-free-pointer*))))
	    
	    (if (equal page-index -1)
		;; Verify that thing points to a valid space.
		(or to-readonly-space
		    to-static-space
		    (equal thing
			   (sap-int (alien-sap (addr undefined-tramp))))
		    (progn
		      (format *stderr*
			      "*** Ptr ~X @ ~X sees Junk~%"
			      thing start)
		      (print-ptr start)))
		;; Thing points into dynamic space.
		(progn
		  ;; If it's within the dynamic space it should point to a
		  ;; used page.  X Could check the offset too.
		  (when (and (page-allocated-p page-index)
			     (zerop (slot (deref page-table page-index)
					  'bytes-used)))
		    (format *stderr*
			    "*** Ptr ~X @ ~X sees free page.~%"
			    thing start)
		    (print-ptr start))

		  ;; Check whether it points to a forwarding pointer!
		  (when (equal (deref (cast (ptr thing) (* lispobj)))
			       #x01)
		    (format *stderr*
			    "*** Ptr %x @ %x sees forwarding ptr.\n"
			    thing start)
		    (print-ptr start))
		  
		  ;; Check that its not in the RO space as it would then be
		  ;; a pointer from the RO to the dynamic space.
		  (when readonly-space
		    (format *stderr*
			    "*** Ptr to dynamic space %x, from RO space %x\n"
			    thing start)
		    (print-ptr start))
		  
		  ;; Does it point to a plausible object? This check slows
		  ;; it down a lot.
		  #+()
		  (or (valid-dynamic-space-pointer (cast thing (* lispobj)))
		      (progn
			(format *stderr*
				"*** Ptr %x to invalid object %x\n"
				thing start)
			(print-ptr start))))))

	  (or (zerop (logand thing #x3)) ; Skip fixnums.
	      (eval ;; FIX convert verify-space to macro? maybe gen verify-space via a macro
	       `(case (vm-type-id-of (deref start))

		  ;; Boxed objects.
		  ((,vm::simple-vector-type
		    ,vm::ratio-type ,vm::complex-type
		    ,vm::simple-array-type ,vm::complex-string-type
		    ,vm::complex-bit-vector-type
		    ,vm::complex-vector-type
		    ,vm::complex-array-type
		    ,vm::closure-header-type
		    ,vm::funcallable-instance-header-type
		    ,vm::byte-code-function-type
		    ,vm::byte-code-closure-type
		    ,vm::dylan-function-header-type
		    ,vm::value-cell-header-type
		    ,vm::symbol-header-type ,vm::base-char-type
		    ,vm::unbound-marker-type
		    ,vm::instance-header-type ,vm::fdefn-type
		    ,vm::scavenger-hook-type)
		   (setq count 1))

		  ((,vm::code-header-type)
		   (let ((object (deref start))
			 (code (cast start (* (struct code)))))
		     ;; Check that it's not in the dynamic space.
		     (if (and dynamic-space
			      ;; It's ok if it's byte compiled code.  The
			      ;; trace table offset will be a fixnum if
			      ;; it's x86 compiled code - check.
			      (zerop (logand (slot code 'trace-table-offset)
					     #x3))
			      ;; Only when enabled.
			      verify-dynamic-code-check)
			 (format *stderr*
				 "*** Code object at ~X in the dynamic space~%"
				 start))
		     
		     (let ((nheader-words (header-value object))
			   (nwords (gc-ceiling
				    (+ (fixnum-value (slot code
							   'code-size))
				       nheader-words) 2)))
		       ;; Scavenge the boxed section of the code data
		       ;; block.
		       (verify-space (sap-alien (sap+ (alien-sap start)
						      (alien-size lispobj :bytes))
						(* lispobj))
				     (1- nheader-words))
		       ;; Scavenge the boxed section of each function
		       ;; object in the code data block.
		       (loop
			 for fheaderl = (slot code 'entry-points)
			 do
			 (if (equal fheaderl alien-nil) (return))
			 (let ((fheaderp (cast (ptr fheaderl)
					       (struct function *))))
			   (gc-assert
			    (equal (vm-type-id-of (slot fheaderp
							'header))
				   vm:function-header-type))
			   (verify-space (addr (slot fheaderp 'name)) 1)
			   (verify-space (addr (slot fheaderp 'arglist) 1))
			   (verify-space (addr (slot fheaderp 'type)) 1)
			   (setq fheaderl (slot fheaderp 'next))))
		       (setq count nwords))))
	       
		  ;; Unboxed objects.
		  ((vm::bignum-type
		    vm::single-float-type
		    vm::double-float-type
		    #+long-float vm::long-float-type
		    ; FIX all these
		    ;#ifdef type_ComplexSingleFloat
		    vm::complex-single-float-type
		    ;#ifdef type_ComplexDoubleFloat
		    vm::complex-double-float-type
		    ;#ifdef type_ComplexLongFloat
		    vm::complex-long-float-type
		    vm::simple-string-type
		    vm::simple-bit-vector-type
		    vm::simple-array-unsigned-byte-2-type
		    vm::simplearrayunsigned-byte-4-type
		    vm::simple-array-unsigned-byte8-type
		    vm::simple-array-unsigned-byte16-type
		    vm::simple-array-unsigned-byte32-type
		    ;#ifdef type_SimpleArraySignedByte8
		    vm::simple-array-signed-byte-8-type
		    ;#ifdef type_SimpleArraySignedByte16
		    vm::simple-array-signed-byte-16-type
		    ;#ifdef type_SimpleArraySignedByte30
		    vm::simple-array-signed-byte-30-type
		    ;#ifdef type_SimpleArraySignedByte32
		    vm::simple-array-signed-byte-32-type
		    vm::simple-array-single-float-type
		    vm::simple-array-double-float-type
		    ;#ifdef type_SimpleArrayComplexLongFloat
		    vm::simple-array-long-float-type
		    ;#ifdef type_SimpleArrayComplexSingleFloat
		    vm::simple-array-complex-single-float-type
		    ;#ifdef type_SimpleArrayComplexDoubleFloat
		    vm::simple-array-complex-double-float-type
		    ;#ifdef type_SimpleArrayComplexLongFloat
		    vm::simple-array-complex-long-float-type
		    vm::sap-type vm::weak-pointer-type)
		   (setq count
			 (alien-funcall (deref sizetab
					       (vm-type-id-of (deref start)))
					start)))

		  (t (gc-abort))))))
	  
      (setq start
	    (sap-alien (sap+ (alien-sap start)
			     (* count (alien-size lispobj :bytes)))
		       (* lispobj)))
      (decf words count))))

#+()
(alien:def-alien-routine verify-generation1 c-call:int
			 (i c-call:int)
			 (last-page c-call:int)
			 (region-unboxed c-call:int)
			 (generation c-call:int))

#+()
(alien:def-alien-routine verify-space1 c-call:void
			 (page unix::int)
			 (words unix::size-t))

(alien:def-alien-routine verify-generation c-call:void
			 (generation c-call:int))

#+()
(defun verify-generation (generation)
  (loop for page from 0 to (1- last-free-page) do
    (when (page-allocated-p page)
      (or (zerop (slot (deref page-table page) 'bytes-used))
	  (when (equal (page-generation page) generation)
	    (let ((last-page page)
		  (region-unboxed (page-unboxed-p page)))

	      ;; This should be the start of a contiguous block.
	      (gc-assert (zerop (slot (deref page-table page)
				      'first-object-offset)))

; FIX for tracing
;  	      (setq last-page
;  		    (verify-generation1 page last-page (if region-unboxed 1 0)
;  					generation))

	      ;; Need to find the full extent of this contiguous block in
	      ;; case objects span pages.

	      ;; Now work forward until the end of this contiguous area is
	      ;; found.
	      (loop
		;; Check if this is the last page in this contiguous block.
		;; FIX Or it is page-size and is the last in the block.
		while (and (>= (slot (deref page-table last-page) 'bytes-used)
			       page-size)
			   (page-allocated-p (1+ last-page))
			   (eq (page-unboxed-p (1+ last-page)) region-unboxed))
		do

		(if (or (zerop (slot (deref page-table (1+ last-page))
				     'bytes-used))
			(if (equal (page-generation (1+ last-page))
				   generation)
			    (zerop (slot (deref page-table (1+ last-page))
					 'first-object-offset))
			    (return)))
		    (return))

		(incf last-page))

	      (verify-space (sap-alien (page-address-sap page) (* lispobj))
			    (/ (+ (slot (deref page-table last-page) 'bytes-used)
				  (* page-size (- last-page page)))
			       4))

; 	      (verify-space1 page
; 			     (/ (+ (slot (deref page-table last-page) 'bytes-used)
; 				   (* page-size (- last-page page)))
; 				4))

;; (call to c version)
;; FIX causes segfs, like page-address in write-protect-generation-pages
; 	      (verify-space (sap-alien (page-address-sap page) (* lispobj))
; 			    (/ (+ (slot (deref page-table last-page) 'bytes-used)
; 				  (* page-size (- last-page page)))
; 			       4))

	      (setq page last-page)))))))

(defun print-generation-stats (verbose)
  "Verbose controls how much to print out: 0 for normal level of detail; 1
   for debugging."
  (let (;; Number of generations to print out.
	(gens (if verbose num-generations (1+ num-generations))))
    (with-alien ((fpu-state (array c-call:int 27)))
      ;; This code uses the FP instructions which may be setup for Lisp so
      ;; they need to be saved and reset for C.
      (fpu-save (cast fpu-state (* t)))
      ;; Print the heap stats.
      (format *stderr*
	      "   Generation Boxed Unboxed LB   LUB    Alloc  Waste   Trig    WP  GCs Mem-age~%")
      (loop for i from 0 to (1- gens) do
	(let ((boxed-cnt 0) (unboxed-cnt 0)
	      (large-boxed-cnt 0) (large-unboxed-cnt 0))
	  (loop for j from 0 to (1- last-free-page)
	        for flags = (slot (deref page-table j) 'flags)
	        do
	    (when (equal (logand flags page-generation-mask) i)
	      (or (zerop (logand flags page-allocated-mask))
		  ;; Count the number of boxed and unboxed pages within the
		  ;; given generation.
		  (if (zerop (logand flags page-unboxed-mask))
		      (if (zerop (logand flags page-large-object-mask))
			  (incf boxed-cnt)
			  (incf large-boxed-cnt))
		      (if (zerop (logand flags page-large-object-mask))
			  (incf unboxed-cnt)
			  (incf large-unboxed-cnt))))))

	  (gc-assert (equal (slot (deref generations i) 'bytes-allocated)
			    (generation-bytes-allocated i)))

	  (format *stderr*
		  ;; FIX last was %7.4f
		  "   ~8D ~5D ~5D ~5D ~5D ~8D ~5D ~8D ~4D ~3D ~12,4F~%"
		  i boxed-cnt unboxed-cnt large-boxed-cnt large-unboxed-cnt
		  (slot (deref generations i) 'bytes-allocated)
		  (- (* page-size (count-generation-pages i))
		     (slot (deref generations i) 'bytes-allocated))
		  (slot (deref generations i) 'gc-trigger)
		  (count-write-protect-generation-pages i)
		  (slot (deref generations i) 'num-gc)
		  (gen-av-mem-age i))))

      (format *stderr* "   Total bytes alloc=~D~%" (lisp::dynamic-usage))

      (fpu-restore (cast fpu-state (* t))))))

(alien:def-alien-routine write-protect-generation-pages c-call:void
			 (generation c-call:int))

#+()
(alien:def-alien-routine write-protect-generation-pages1 c-call:void
			 (page-start (* t))
			 (i c-call:int))

(alien:def-alien-routine os-protect1 c-call:void
			 (page c-call:int)
			 (len os-vm-size-t)
			 (protection os-vm-prot-t))

#+()
(defun write-protect-generation-pages (generation)
  "Write protect all the dynamic boxed pages in the given generation."
  (gc-assert (< generation num-generations))
  (loop for i from 0 to (1- last-free-page) do
    (when (page-allocated-p i)
      (or (page-unboxed-p i)
	  (zerop (slot (deref page-table i) 'bytes-used))
	  (when (equal (page-generation i) generation)

;	    (format t "(page-address ~S): .~S.~%" i (page-address i))

;	    (write-protect-generation-pages1 i)
;	    (write-protect-generation-pages1 (page-address i) i)
; 	    (write-protect-generation-pages1 (sap-alien (page-address-sap i)
; 							(* t)))

 	    (os-protect1 i
 			 page-size
 			 (logior os-vm-prot-read os-vm-prot-execute))

;; FIX these ~segfs
; 	    (os-protect (sap-alien (page-address-sap i) os-vm-address-t)
; 	    (os-protect (page-address-sap i)
; 	    (os-protect (cast (page-address i) os-vm-address-t)
; 	    (os-protect (alien-sap (page-address i))
; 	    (os-protect (sap-int (alien-sap (page-address i)))  ; type err
; 			page-size
;  			(logior os-vm-prot-read os-vm-prot-execute))

	    ;; Note the page as protected in the page tables.
	    (setf (slot (deref page-table i) 'flags)
		  (logior (slot (deref page-table i) 'flags)
			  page-write-protected-mask))))))
  (when (> gencgc-verbose 1)
    (format *stderr*
	    "Write protected ~D of ~D pages in generation ~D.~%"
	    (count-write-protect-generation-pages generation)
	    (count-generation-pages generation)
	    generation)))

(alien:def-alien-routine update-x86-dynamic-space-free-pointer c-call:void)

;; FIX confirm that this is tested
#+()
(defun update-x86-dynamic-space-free-pointer ()
  "Update last-free-page then ALLOCATION_POINTER."
  (let ((last-page -1))
    (loop for i from 0 to dynamic-space-pages do
      (if (page-allocated-p i)
	  (or (zerop (slot (deref page-table i) 'bytes-used))
	      (setq last-page i))))
    (setq last-free-page (1+ last-page)))
  (setq vm::*allocation-pointer*
	(+ (sap-int (alien-sap heap-base)) (* page-size last-free-page))))

; (alien:def-alien-routine call-garbage-collect-generation c-call:void
; 			 (last-gen c-call:unsigned-int)
; 			 (gen c-call:int)
; 			 (raise c-call:int))

; (alien:def-alien-routine garbage-collect-generation c-call:void
; 			 (generation c-call:int)
; 			 (raise c-call:int))

(alien:def-alien-routine collect-garbage1 c-call:int
;(alien:def-alien-routine collect-garbage1 c-call:void
;			 (last-gen c-call:unsigned-int)
			 (last-gen c-call:int)
 			 (gen c-call:int)
 			 (raise c-call:int)
)

(alien:def-alien-routine collect-garbage2 c-call:void
			 (last-gen c-call:unsigned-int)
 			 (gen c-call:int)
 			 (raise c-call:int))

;; If the record_new_objects flag is 2 then all new regions created
;; are recorded.
;;
;; If it's 1 then then it is only recorded if the first page of the
;; current region is <= new_areas_ignore_page.  This helps avoid
;; unnecessary recording when doing full scavenge pass.
;;
;; The new_object structure holds the page, byte offset, and size of
;; new regions of objects. Each new area is placed in the array of
;; these structures pointed to by new_areas; new_areas_index holds the
;; offset into new_areas.
;;
;; If new_area overflows NUM_NEW_AREAS then it stops adding them.  The
;; later code must detect this and handle it, probably by doing a full
;; scavenge of a generation.

(alien:def-alien-routine add-new-area c-call:void
			 (first-page c-call:int)
			 (offset c-call:int)
			 (size c-call:int))

; (defvar num-new-areas 512)
; (alien:def-alien-variable "record_new_objects" c-call:int)
; (alien:def-alien-variable "new_areas_ignore_page" c-call:int)
; (alien:def-alien-type nil
;    (struct new-area
; 	   (page c-call:int)
; 	   (offset c-call:int)
; 	   (size c-call:int)))
; (alien:def-alien-variable "new_areas" (* (* (struct new-area))))
; (alien:def-alien-variable "new_areas_index" c-call:int)
; (alien:def-alien-variable "max_new_areas" c-call:int)

#+()
(defun add-new-area (first-page offset size)
  "Add a new area to new-areas."

  ;; Ignore if full.
  (if (>= new-areas-index num-new-areas)
      (return-from add-new-area))

  (cond
    ((equal record-new-objects 0)
     (return-from add-new-area))
    ((equal record-new-objects 1)
     (if (> first-page new-areas-ignore-page)
	 (return-from add-new-area)))
    ((equal record-new-objects 2))
    (t (gc-abort)))

  (let ((new-area-start (+ (* page-size first-page) offset)))

    ;; Search backwards for a prior area that this follows from.  If found
    ;; this will save adding a new area.
  
    (loop for i = (1- new-areas-index) then (1- i)
          for c = 0 then (1+ c)
          while (and (>= i 0) (< c 8)) do
      (let ((area-end (+ (* page-size
			    (slot (deref (deref new-areas) i) 'page))
			 (slot (deref (deref new-areas) i) 'offset)
			 (slot (deref (deref new-areas) i) 'size))))
	#+()
	(format *stderr* "*S1 ~D ~D ~D ~D~%" i c new-area-start area-end)
	(when (equal new-area-start area-end)
	  #+()
	  (format *stderr*
		  "-> Adding to [~D] ~D ~D ~D with ~D ~D ~D:~%"
		  i
		  (slot (deref (deref new-areas) i) 'page)
		  (slot (deref (deref new-areas) i) 'offset)
		  (slot (deref (deref new-areas) i) 'size)
		  first-page offset size)
	  (incf (slot (deref (deref new-areas) i) 'size) size)
	  (return-from add-new-area))))
    #+()
    (format *stderr* "*S1 ~D ~D ~D~%" i c new-area-start)

    (setf (slot (deref (deref new-areas) new-areas-index) 'page)
	  first-page)
    (setf (slot (deref (deref new-areas) new-areas-index) 'offset)
	  offset)
    (setf (slot (deref (deref new-areas) new-areas-index) 'size)
	  size)
    #+()
    (format *stderr*
	    "  new-area ~D page ~D offset ~D size ~D~%"
	    new-areas-index first-page offset size)
    (incf new-areas-index)

    ;; Note the max new-areas used.
    (if (> new-areas-index max-new-areas)
	(setq max-new-areas new-areas-index))))

(alien:def-alien-routine gc-alloc-update-page-tables c-call:void
			 (unboxed c-call:int)
			 (alloc-region (* (struct alloc-region))))

#+()
(defun gc-alloc-update-page-tables (unboxed alloc-region)
  "Update the tables for the Alloc-region.  The region may be added to the
   new-areas.

   When done the Alloc-region is setup so that the next quick alloc will
   fail safely and thus a new region will be allocated.  Further it is safe
   to try and re-update the page table of this reset Alloc-region.

   Unboxed must be 0 or 1 (unboxed)."
;  (declare (type (alien (* (struct alloc-region))) alloc-region))
  (declare (debug 3))

  #+()
  (format *stderr* "gc-alloc-update-page-tables to gen ~D: "
	  gc-alloc-generation)

  (let ((first-page (slot (deref alloc-region) 'first-page)))

    ;; Catch an unused alloc-region.
    (if (and (zerop first-page)
	     (equal (slot (deref alloc-region) 'last-page) -1))
	(return-from gc-alloc-update-page-tables))

    (let ((next-page (1+ first-page)))

      (if (equal (sap-int (alien-sap (slot (deref alloc-region) 'free-pointer)))
		 (sap-int (alien-sap (slot (deref alloc-region) 'start-addr))))
	  ;; No bytes allocated.  Unallocate the first-page if there are 0
	  ;; bytes-used.
	  (if (zerop (slot (deref page-table first-page) 'bytes-used))
	      (setf (slot (deref page-table first-page) 'flags)
		    (logand (slot (deref page-table first-page) 'flags)
			    (logcom page-allocated-mask))))
	  (let ((orig-first-page-bytes-used
		 (slot (deref page-table first-page) 'bytes-used))
		(byte-cnt 0)
		(more))

	    (gc-assert
	     (equal (sap-int (alien-sap (slot (deref alloc-region) 'start-addr)))
		    (+ (sap-int (alien-sap (page-address first-page)))
		       (sap-int (alien-sap (slot (deref page-table first-page)
						 'bytes-used))))))

	    ;; All the pages used need to be updated

	    ;; Update the first page.

	    #+() (format *stderr* "0")

	    ;; If the page was free then setup the gen, and
	    ;; first-object-offset.
	    (if (zerop (slot (deref page-table first-page) 'bytes-used))
		(gc-assert (zerop (slot (deref (page-table first-page)
					       'first-object-offset)))))

	    (gc-assert (page-allocated-p first-page))
	    (gc-assert (equal (page-unboxed-val first-page) unboxed))
	    (gc-assert (equal (page-generation first-page)
			      gc-alloc-generation))
	    (gc-assert (if (page-large-object-p first-page) nil t))

	    ;; Calc. the number of bytes used in this page. This is not
	    ;; always the number of new bytes, unless it was free.
    
	    (let ((bytes-used (- (sap-int (alien-sap (slot (deref alloc-region)
							   'free-pointer)))
				 (sap-int (alien-sap (page-address first-page))))))
	      (when (> bytes-used page-size)
		(setq bytes-used page-size)
		(setq more t))
	      (setf (slot (deref page-table first-page) 'bytes-used)
		    bytes-used)
	      (incf byte-cnt bytes-used)

	      ;; All the rest of the pages should be free. Need to set
	      ;; their first-object-offset pointer to the start of the
	      ;; region, and set the bytes-used.
    
	      (loop while more do
		#+() (format *stderr* "+")

		(gc-assert (page-allocated-p next-page))
		(gc-assert (equal (page-unboxed-val next-page) unboxed))
		(gc-assert (zerop (slot (deref page-table next-page)
					'bytes-used)))
		(gc-assert (equal (page-generation next-page)
				  gc-alloc-generation))
		(gc-assert (if (page-large-object next-page) nil t))

		(gc-assert (equal (slot (deref page-table next-page)
					'first-object-offset)
				  (- (sap-int (alien-sap (slot (deref alloc-region)
							       'start-addr)))
				     (sap-int (alien-sap (page-address next-page))))))

		;; Calc. the number of bytes used in this page.
		(setq more nil)
		(setq bytes-used (- (sap-int (alien-sap (slot (deref alloc-region)
							      'free-pointer)))
				    (sap-int (alien-sap (page-address next-page)))))
		(when (> bytes-used page-size)
		  (setq bytes-used page-size)
		  (setq more t))
		(setf (slot (deref page-table next-page) 'bytes-used)
		      bytes-used)
		(incf byte-cnt bytes-used)

		(incf next-page))

	      (let ((region-size (- (sap-int (alien-sap (slot (deref alloc-region)
							      'free-pointer)))
				    (sap-int (alien-sap (slot (deref alloc-region)
							      'start-addr))))))
		(incf bytes-allocated region-size)
		(incf (slot (deref generations gc-alloc-generation)
			    'bytes-allocated)
		      region-size)

		(gc-assert (equal (- byte-cnt orig-first-page-bytes-used)
				  region-size))
    
		;; Set the generations alloc restart page to the last page
		;; of the region.
		(if (zerop unboxed)
		    (setf (slot (deref generations gc-alloc-generation)
				'alloc-start-page)
			  (1- next-page))
		    (setf (slot (deref generations gc-alloc-generation)
				'alloc-unboxed-start-page)
			  (1- next-page)))

		;; Add the region to the new-areas if requested.
		(if (zerop unboxed)
		    (add-new-area first-page
				  orig-first-page-bytes-used
				  region-size))

		#+()
		(format *stderr*
			"  gc-alloc-update-page-tables update ~D bytes to gen ~D~%"
			region-size gc-alloc-generation)))))

      ;; Unallocate any unused pages.
      (loop
	while (<= next-page (slot (deref alloc-region) 'last-page)) do
	(gc-assert (zerop (slot (deref page-table next-page)
				'bytes-used)))
	(setf (slot (deref page-table next-page) 'flags)
	      (logand (slot (deref page-table next-page) 'flags)
		      (logcom page-allocated-mask)))
	(incf next-page))))

  ;; Reset the alloc-region.
  (setf (slot (deref alloc-region) 'first-page) 0)
  (setf (slot (deref alloc-region) 'last-page) -1)
  (setf (slot (deref alloc-region) 'start-addr) (page-address 0))
  (setf (slot (deref alloc-region) 'free-pointer) (page-address 0))
  (setf (slot (deref alloc-region) 'end-addr) (page-address 0))

  #+() (format *stderr* "~%"))

(alien:def-alien-routine unprotect-oldspace c-call:void)

;; Un-write-protect all the pages in from-space. This is done at the
;; start of a GC else there may be many page faults while scavenging
;; the newspace (I've seen drive the system time to 99%). These pages
;; would need to be unprotected anyway before unmapping in
;; free-oldspace; not sure what effect this has on paging?.
#+()
(defun unprotect-oldspace ()
  (loop for i from 0 to (1- last-free-page) do
    (if (page-allocated-p i)
	(or (zerop (slot (deref page-table i) 'bytes-used))
	    (when (equal (page-generation i) from-space)
	      (let ((page-start (page-address i)))
		;; Remove any write protection.  Should be able to rely on
		;; the WP flag to avoid redundant calls.
		(when (page-write-protected-p i) {
		  (os-protect page-start page-size os-vm-prot-all)
		  (setf (slot (deref page-table i) 'flags)
			(logand (slot (deref page-table i) 'flags)
				(logcom page-write-protected-mask))))))))))

#|
;; Garbage collect a generation. If raise is 0 the remains of the
;; generation are not raised to the next generation.
(defun garbage-collect-generation(int generation int raise)

  unsigned long i
  unsigned long read-only-space-size static-space-size

  (gc-assert (<= generation (1- NUM-GENERATIONS)))

  ;; The oldest generation can't be raised.
  (gc-assert (if (equal generation (1- num-generations)) (zerop raise) t))

  ;; Initialise the weak pointer list.
  (setq weak-pointers (sap-alien (int-sap 0) (* (struct weak-pointer))))

  ;; When a generation is not being raised it is transported to a
  ;; temporary generation (NUM-GENERATIONS), and lowered when
  ;; done. Setup this new generation. There should be no pages
  ;; allocated to it yet.
  
  (if (zerop raise)
      (gc-assert (zerop (slot (deref generations num-generations)
			      'bytes-allocated))))

  ;; Set the global src and dest. generations.
  (setq from-space generation)
  (setq new-space (if (zerop raise)
		      NUM-GENERATIONS
		      (1+ generation)))

  ;; Change to a new space for allocation, reseting the alloc-start-page.

  (setq gc-alloc-generation new-space)
  (setq (slot (deref generations new-space) 'alloc-start-page) 0)
  (setq (slot (deref generations new-space) 'alloc-unboxed-start-page) 0)
  (setq (slot (deref generations new-space) 'alloc-large-start-page) 0)
  (setq (slot (deref generations new-space) 'alloc-large-unboxed-start-page) 0)

  ;; Before any pointers are preserved, the dont-move flags on the pages
  ;; need to be cleared.
  
  (loop for i from 0 to (1- last-free-page) do
    (setq (slot (deref page-table i) 'flags)
	  (logand (slot (deref page-table i) 'flags)
		  (logcom page-dont-move-mask))))

  ;; Un-write-protect the old-space pages. This is essential for the
  ;; promoted pages as they may contain pointers into the old-space which
  ;; need to be scavenged. It also helps avoid unnecessary page faults as
  ;; forwarding pointer are written into them. They need to be un-protected
  ;; anyway before unmapping later.
  
  (unprotect-oldspace)

  ;; Scavenge the stacks conservative roots.
  {
    lispobj **ptr
    for (ptr = (lispobj **) CONTROL-STACK-END - 1;
	 ptr > (lispobj **) &raise; ptr--)
      preserve-pointer(*ptr)
  }
#ifdef CONTROL-STACKS
  scavenge-thread-stacks()
#endif

  if (gencgc-verbose > 1) {
    int num-dont-move-pages = count-dont-move-pages()
    fprintf(stderr "Non-movable pages due to conservative pointers = %d %d bytes\n"
	    num-dont-move-pages PAGE-SIZE * num-dont-move-pages)
  }

  ;; Scavenge all the rest of the roots.

  ;;
  ;; Scavenge the Lisp functions of the interrupt handlers, taking
  ;; care to avoid SIG-DFL, SIG-IGN.
  

  for (i = 0; i < NSIG; i++) {
    union interrupt-handler handler = interrupt-handlers[i]
    if ((handler.c != SIG-IGN) && (handler.c != SIG-DFL))
      scavenge((lispobj *) (interrupt-handlers + i) 1)
  }

  ;; Scavenge the binding stack.
  scavenge(binding-stack
	   (lispobj *) SymbolValue(BINDING-STACK-POINTER) - binding-stack)

  ;;
  ;; Scavenge the scavenge-hooks in case this refers to a hook added
  ;; in a prior generation GC. From here on the scavenger-hook will
  ;; only be updated with hooks already scavenged so this only needs
  ;; doing here.
  

  scavenge((lispobj *) &scavenger-hooks 1)

  if (SymbolValue(SCAVENGE-READ-ONLY-SPACE) != NIL) {
    read-only-space-size = (lispobj *) SymbolValue(READ-ONLY-SPACE-FREE-POINTER)
      - read-only-space
    fprintf(stderr "Scavenge read only space: %d bytes\n"
	    read-only-space-size * sizeof(lispobj))
    scavenge(read-only-space read-only-space-size)
  }

  static-space-size = (lispobj *) SymbolValue(STATIC-SPACE-FREE-POINTER)
    - static-space
  if (gencgc-verbose > 1)
    fprintf(stderr "Scavenge static space: %d bytes\n"
	    static-space-size * sizeof(lispobj))
  scavenge(static-space static-space-size)

  ;;
  ;; All generations but the generation being GCed need to be
  ;; scavenged. The new-space generation needs special handling as
  ;; objects may be moved in - it is handle separately below.
  
  for (i = 0; i < NUM-GENERATIONS; i++)
    if (i != generation && i != new-space)
      scavenge-generation(i)

  ;;
  ;; Finally scavenge the new-space generation.  Keep going until no
  ;; more objects are moved into the new generation.
  
  scavenge-newspace-generation(new-space)

#define RESCAN-CHECK 0
#if RESCAN-CHECK  
  ;;
  ;; As a check re-scavenge the newspace once; no new objects should
  ;; be found.
  
  {
    int old-bytes-allocated = bytes-allocated
    int bytes-allocated

    ;; Start with a full scavenge
    scavenge-newspace-generation-one-scan(new-space)

    scavenge((lispobj *) &scavenger-hooks 1)

    ;; Flush the current regions updating the tables.
    gc-alloc-update-page-tables(0 &boxed-region)
    gc-alloc-update-page-tables(1 &unboxed-region)

    bytes-allocated = bytes-allocated - old-bytes-allocated

    if (bytes-allocated != 0)
      fprintf(stderr "*** rescan of new-space allocated % more bytes?\n"
	      bytes-allocated)
  }
#endif

  scan-weak-pointers()

  ;; Flush the current regions updating the tables.
  gc-alloc-update-page-tables(0 &boxed-region)
  gc-alloc-update-page-tables(1 &unboxed-region)

  ;; Free the pages in oldspace, but not those marked dont-move.
  free-oldspace()

  ;;
  ;; If the GC is not raising the age then lower the generation back
  ;; to its normal generation number.
  
  if (!raise) {
    for (i = 0; i < last-free-page; i++)
      if (page-table[i].bytes-used != 0
	  && PAGE-GENERATION(i) == NUM-GENERATIONS)
	PAGE-FLAGS-UPDATE(i PAGE-GENERATION-MASK generation)
    gc-assert(generations[generation].bytes-allocated == 0)
    generations[generation].bytes-allocated = generations[NUM-GENERATIONS].bytes-allocated
    generations[NUM-GENERATIONS].bytes-allocated = 0
  }

  ;; Reset the alloc-start-page for generation.
  generations[generation].alloc-start-page = 0
  generations[generation].alloc-unboxed-start-page = 0
  generations[generation].alloc-large-start-page = 0
  generations[generation].alloc-large-unboxed-start-page = 0

  if(generation >= verify-gens) {
    if (gencgc-verbose)
      fprintf(stderr "Checking\n")
    verify-gc()
    verify-dynamic-space()
  }

  ;; Set the new gc trigger for the GCed generation
  generations[generation].gc-trigger = generations[generation].bytes-allocated + generations[generation].bytes-consed-between-gc


  ;; If the generation was raised clear num-gc
  if (raise)
    generations[generation].num-gc = 0
  else
    ;; Else increase it.
    generations[generation].num-gc++
}
|#

(alien:def-alien-routine call-gc-alloc-update-page-tables c-call:void)

(defun collect-garbage (#+gencgc last-gen)
  #-gencgc
  (unix:unix-exit 1)

  (setf (slot boxed-region 'free-pointer) current-region-free-pointer)

  (when (> last-gen lisp::*gc-generations*)
;     (format *stderr*
; 	    "** collect-garbage: last-gen = ~A. Doing a level 0 GC.~%"
; 	    last-gen)
    (setq last-gen 0))

  (call-gc-alloc-update-page-tables)

#|
  FIX even w the c version of gc-a-u-p-t this segfs when used to build another sys

;  (format *stdout* "boxed-region: ~S~%" boxed-region)
  (gc-alloc-update-page-tables 0 (alien:sap-alien (alien:alien-sap boxed-region)
						  (* (struct alloc-region))))
;  (gc-alloc-update-page-tables 0 (sap-int (alien-sap (alien:addr boxed-region))))
;  (gc-alloc-update-page-tables 0 (alien:addr boxed-region))
;  (gc-alloc-update-page-tables 0 (alien:alien-sap (alien:addr boxed-region)))
  (gc-alloc-update-page-tables 1 (alien:sap-alien (alien:alien-sap unboxed-region)
						  (* (struct alloc-region))))
;  (gc-alloc-update-page-tables 1 (sap-int (alien-sap (alien:addr unboxed-region))))
;  (gc-alloc-update-page-tables 1 (alien:addr unboxed-region))
;  (gc-alloc-update-page-tables 1 (alien:alien-sap (alien:addr unboxed-region)))
|#

  ;; Verify the new objects created by lisp code.
  (when pre-verify-gen-0
    (format *stderr* "Pre-Checking generation 0~%")
    (verify-generation 0))

  (if (> gencgc-verbose 1)
      (print-generation-stats 0))

  ;; FIX new-genesis.lisp, end of emit-c-header-aux
  (setq scavenger-hooks alien-nil)
;  (setq scavenger-hooks (cast alien-nil (* (struct scavenger-hook)))) ; quick error
;  (setq scavenger-hooks nil) ; result is alien at #x00000000
;  (format *stdout* "s-h after ~S~%" scavenger-hooks)  ; FIX printing causes ~segf!

;  (collect-garbage1 last-gen)

  (let ((gen 0) (raise))
;    (collect-garbage1 last-gen gen 0))

    (loop
      ;; Collect the generation.

      ;; Only raise the generation if there is an older one.
      (setq raise
	    (if (>= gen gencgc-oldest-gen-to-gc)
		0
		(if (< gen last-gen)
		    1
		    ;; Only raise if the age is >= the trigger age.
		    (if (>= (slot (deref generations gen) 'num-gc)
			    (slot (deref generations gen) 'trigger-age))
			1 0))))

      (if (> gencgc-verbose 1)
	  (format *stderr*
		  "Starting GC of generation ~D with raise=~D alloc=~D trig=~D GCs=~D~%"
		  gen
		  raise
		  (slot (deref generations gen) 'bytes-allocated)
		  (slot (deref generations gen) 'gc-trigger)
		  (slot (deref generations gen) 'num-gc)))
      
      ;; If an older generation is being filled then update its memory
      ;; age.
      
      (if (eq raise 1)
	  (incf (slot (deref generations (1+ gen)) 'cum-sum-bytes-allocated)
		(slot (deref generations (1+ gen)) 'bytes-allocated)))

;; FIX works using c from here to before incf (last night)
;; FIX err this way (today)

      (collect-garbage1 last-gen gen raise)

;      (garbage-collect-generation gen raise)       ; "trying to write prot. gen. 1 when gen. 0 is..."
;      (call-garbage-collect-generation gen raise)  ; "trying to write prot. gen. 1 when gen. 0 is..."
;      (call-garbage-collect-generation last-gen gen raise)

;      (collect-garbage1 last-gen gen raise)
      
;       ;; Reset the memory age cum-sum.
;       (setf (slot (deref generations gen) 'cum-sum-bytes-allocated) 0)
;       
;       (when (> gencgc-verbose 1)
; 	(format *stderr* "GC of generation ~D finished:~%" gen)
; 	(print-generation-stats 0))
      
      (incf gen)
      
      (or (and (<= gen gencgc-oldest-gen-to-gc)
	       (or (< gen last-gen)
		   (and (<= gen gencgc-oldest-gen-to-gc) ;; FIX just checked?
			raise
			(> (slot (deref generations gen) 'bytes-allocated) 
			   (slot (deref generations gen) 'gc-trigger))
			(> (gen-av-mem-age gen)
			   (slot (deref generations gen) 'min-av-mem-age)))))
	  (return)))

      (collect-garbage2 last-gen gen raise)

;     ;; Now if gen-1 was raised all generations before gen are empty. If
;     ;; it wasn't raised then all generations before gen-1 are empty.
;     ;;
;     ;; Now objects within this gen's pages cannot pointer to younger
;     ;; generations unless they are written to. This can be exploited by
;     ;; write protecting the pages of gen; then when younger generations
;     ;; are GCed only the page written need scanning.
;     (let ((gen-to-wp (if (zerop raise) (1- gen) gen)))
;       ;; Not much point in WPing pages in generation 0 as it is never
;       ;; scavenged (except promoted pages).
;       (when (and (> gen-to-wp 0) enable-page-protection)
; 	;; Check that they are all empty.
; 	(loop for i from 0 upto (1- gen-to-wp) do
; 	  (or (zerop (slot (deref generations i) 'bytes-allocated))
; 	      (format *stderr*
; 	      "*** trying to write prot. gen. ~D when gen. ~D is not empty~%"
; 		      gen-to-wp i)))
; 	(write-protect-generation-pages gen-to-wp))
; 
; ;; FIX close let here
; 
; ;     (collect-garbage1 0)
; 
;       ;; Set gc-alloc back to generation 0. The current regions should be
;       ;; flushed after the above GCs.
;       (gc-assert (eq (- (slot boxed-region 'free-pointer)
; 			(slot boxed-region 'start-addr))
; 		     0))
;       (setq gc-alloc-generation 0)
; 
;       (update-x86-dynamic-space-free-pointer)
; 
;       (setq current-region-free-pointer (slot boxed-region 'free-pointer))
;       (setq current-region-end-addr (slot boxed-region 'end-addr))
; 
;       ;; Call the scavenger hook functions.
;       (with-alien ((sh (* (struct scavenger-hook))
; 		       (cast (ptr scavenger-hooks)
; 			     (* (struct scavenger-hook)))))
; 	(loop
; 	  (if (equal (sap-int (alien-sap sh))
; 		     (sap-int (alien-sap (ptr alien-nil))))
; 	      (return))
; 	  (with-alien ((sh-next (* (struct scavenger-hook))
; 				(cast (ptr (slot (deref sh) 'next))
; 				      (* (struct scavenger-hook)))))
; 	    (funcall0 (slot (deref sh) 'function))
; 	    (setf (slot (deref sh) 'next)
; 		  ;; FIX assuming (a-s-m 0) always allocs at 0x0
; ;		  (sap-alien (int-sap 0)
; 		  (sap-alien (allocate-system-memory 0)
; 			     (* (struct scavenger-hook))))
; 	    (setq sh sh-next))))
;       (setq scavenger-hooks (cast alien-nil (* (struct scavenger-hook))))
;       )
  )
  )

(defun test-gc ()
  "Create a random list and a random vector many times, returning some of
   the created elements."
  (let ((result)
	(*random-state* (make-random-state)))
    (loop for i from 0 to 50 do
      (let* ((list-size (1+ (random 500)))
	     (vector-size (1+ (random 250)))
	     (list ())
	     (vector (make-array (1+ vector-size))))
	(loop for i from 0 to (1+ list-size) do
	  (setq list (cons (random 10000) list))
	  (let ((rand (random 8)))
	    (cond ((eq rand 0)
		   (setf (aref vector (random vector-size)) t))
		  ((eq rand 1)
		   (setf (aref vector (random vector-size))
			 (random 250)))
		  ((eq rand 2)
		   (setf (aref vector (random vector-size))
			 "test string"))
		  ((eq rand 3)
		   (let ((sublist ()))
		     (loop for i from 0 to (random 20) do
		       (setq sublist (cons (random 100000) sublist)))
		     (setf (aref vector (random vector-size))
			   sublist)))
		  ((eq rand 4)
		   (setf (aref vector (random vector-size))
			 (car list)))
;			 (nth (random (max 1 i)) list)))
		  ((eq rand 5)
		   (setf (aref vector (random vector-size))
			 (make-array (* (1+ (random 4)) page-size)
				     :initial-element (random 7777))))
		  ((eq rand 6)
		   (setf (aref vector (random vector-size))
			 (make-random-state t))))))
	(if (equal (type-of (car list)) (type-of (aref vector 0)))
	    (push (cons (car list) (aref vector 0)) result))))
    result))

(alien:def-alien-routine collect-garbage c-call:int
			 #+gencgc (last-gen c-call:int))



(in-package "LISP")

; (alien:def-alien-routine collect-garbage c-call:int
;   #+gencgc (last-gen c-call:int))

; (defun collect-garbage (#+gencgc last-gen)
;   (unix::collect-garbage1 #+gencgc last-gen))

(defun collect-garbage (#+gencgc last-gen)
  (unix::collect-garbage #+gencgc last-gen))

#-ibmrt
(alien:def-alien-routine set-auto-gc-trigger c-call:void
  (dynamic-usage c-call:unsigned-long))

#+ibmrt
(defun set-auto-gc-trigger (bytes)
  (let ((words (ash (+ (current-dynamic-space-start) bytes) -2)))
    (unless (and (fixnump words) (plusp words))
      (clear-auto-gc-trigger)
      (warn "Attempt to set GC trigger to something bogus: ~S" bytes))
    (setf rt::*internal-gc-trigger* words)))

#-ibmrt
(alien:def-alien-routine clear-auto-gc-trigger c-call:void)

#+ibmrt
(defun clear-auto-gc-trigger ()
  (setf rt::*internal-gc-trigger* -1))

;;;
;;; *INTERNAL-GC*
;;;
;;; This variables contains the function that does the real GC.  This is
;;; for low-level GC experimentation.  Do not touch it if you do not
;;; know what you are doing.
;;; 
(defvar *internal-gc* #'collect-garbage)


;;;; SUB-GC

;;;
;;; CAREFULLY-FUNCALL -- Internal
;;;
;;; Used to carefully invoke hooks.
;;; 
(defmacro carefully-funcall (function &rest args)
  `(handler-case (funcall ,function ,@args)
     (error (cond)
       (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
       nil)))

;;;
;;; SUB-GC -- Internal
;;;
;;; SUB-GC decides when and if to do a garbage collection.  The
;;; VERBOSE-P flag controls whether or not the notify functions are
;;; called.  The FORCE-P flags controls if a GC should occur even if the
;;; dynamic usage is not greater than *GC-TRIGGER*.
;;; 
;;; For GENCGC all generations < GEN will be GC'ed.
;;;
(defun sub-gc (&key (verbose-p *gc-verbose*) force-p #+gencgc (gen 0))
  (unless *already-maybe-gcing*
    (let* ((*already-maybe-gcing* t)
	   (start-time (get-internal-run-time))
	   (pre-gc-dyn-usage (dynamic-usage)))
      (unless (integerp (symbol-value '*bytes-consed-between-gcs*))
	;; The noise w/ symbol-value above is to keep the compiler from
	;; optimizing the test away because of the type declaim for
	;; *bytes-consed-between-gcs*.
	(warn "The value of *BYTES-CONSED-BETWEEN-GCS*, ~S, is not an ~
	       integer.  Reseting it to ~D." *bytes-consed-between-gcs*
	       default-bytes-consed-between-gcs)
	(setf *bytes-consed-between-gcs* default-bytes-consed-between-gcs))
      (when (and *gc-trigger* (> pre-gc-dyn-usage *gc-trigger*))
	(setf *need-to-collect-garbage* t))
      (when (or force-p
		(and *need-to-collect-garbage* (not *gc-inhibit*)))
	(when (and (not force-p)
		   *gc-inhibit-hook*
		   (carefully-funcall *gc-inhibit-hook* pre-gc-dyn-usage))
	  (setf *gc-inhibit* t)
	  (return-from sub-gc nil))
	(without-interrupts
	  (let ((*standard-output* *terminal-io*))
	    (when verbose-p
	      (carefully-funcall *gc-notify-before* pre-gc-dyn-usage))
	    (dolist (hook *before-gc-hooks*)
	      (carefully-funcall hook))
	    (when *gc-trigger*
	      (clear-auto-gc-trigger))
	    #-gencgc (funcall *internal-gc*)
	    #+gencgc (if (eq *internal-gc* #'collect-garbage)
			 (funcall *internal-gc* gen)
			 (funcall *internal-gc*))
	    (let* ((post-gc-dyn-usage (dynamic-usage))
		   (bytes-freed (- pre-gc-dyn-usage post-gc-dyn-usage)))
	      (when *last-bytes-in-use*
		(incf *total-bytes-consed*
		      (- pre-gc-dyn-usage *last-bytes-in-use*))
		(setq *last-bytes-in-use* post-gc-dyn-usage))
	      (setf *need-to-collect-garbage* nil)
	      (setf *gc-trigger*
		    (+ post-gc-dyn-usage *bytes-consed-between-gcs*))
	      (set-auto-gc-trigger *gc-trigger*)
	      (dolist (hook *after-gc-hooks*)
		(carefully-funcall hook))
	      (when verbose-p
		(carefully-funcall *gc-notify-after*
				   post-gc-dyn-usage bytes-freed
				   *gc-trigger*))))
	  (scrub-control-stack)))
      (incf *gc-run-time* (- (get-internal-run-time) start-time))))
  nil)

;;;
;;; MAYBE-GC -- Internal
;;; 
;;; This routine is called by the allocation miscops to decide if a GC
;;; should occur.  The argument, object, is the newly allocated object
;;; which must be returned to the caller.
;;; 
(defun maybe-gc (&optional object)
  (sub-gc)
  object)

;;;
;;; GC -- Exported
;;;
;;; This is the user advertised garbage collection function.
;;; 
#-gencgc
(defun gc (&optional (verbose-p *gc-verbose*))
  "Initiates a garbage collection.  The optional argument, VERBOSE-P,
  which defaults to the value of the variable *GC-VERBOSE* controls
  whether or not GC statistics are printed."
  (sub-gc :verbose-p verbose-p :force-p t))
;;;
#+gencgc
(defun gc (&key (verbose *gc-verbose*) (gen 0) (full nil))
  "Initiates a garbage collection.  The keyword :VERBOSE, which
   defaults to the value of the variable *GC-VERBOSE* controls whether or
   not GC statistics are printed. The keyword :GEN defaults to 0, and
   controls the number of generations to garbage collect."
  (sub-gc :verbose-p verbose :force-p t :gen (if full *gc-generations* gen)))


;;;; Auxiliary Functions.

(defun bytes-consed-between-gcs ()
  "Return the amount of memory that will be allocated before the next garbage
   collection is initiated.  This can be set with SETF."
  *bytes-consed-between-gcs*)
;;;
(defun %set-bytes-consed-between-gcs (val)
  (declare (type index val))
  (let ((old *bytes-consed-between-gcs*))
    (setf *bytes-consed-between-gcs* val)
    (when *gc-trigger*
      (setf *gc-trigger* (+ *gc-trigger* (- val old)))
      (cond ((<= (dynamic-usage) *gc-trigger*)
	     (clear-auto-gc-trigger)
	     (set-auto-gc-trigger *gc-trigger*))
	    (t
	     (system:scrub-control-stack)
	     (sub-gc)))))
  val)
;;;
(defsetf bytes-consed-between-gcs %set-bytes-consed-between-gcs)


(defun gc-on ()
  "Enables the garbage collector."
  (setq *gc-inhibit* nil)
  (when *need-to-collect-garbage*
    (sub-gc))
  nil)

(defun gc-off ()
  "Disables the garbage collector."
  (setq *gc-inhibit* t)
  nil)



;;;; Initialization stuff.

(defun gc-init ()
  (when *gc-trigger*
    (if (< *gc-trigger* (dynamic-usage))
	(sub-gc)
	(set-auto-gc-trigger *gc-trigger*))))
