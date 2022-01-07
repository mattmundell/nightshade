;;; Garbage collection and allocation related code.

(in-package "EXTENSIONS")

(export '(*before-gc-hooks* *after-gc-hooks* gc gc-on gc-off
	  *bytes-consed-between-gcs* *gc-verbose* *gc-inhibit-hook*
	  *gc-notify-before* *gc-notify-after* get-bytes-consed
	  *gc-run-time* bytes-consed-between-gcs))

(in-package "LISP")

(export '(room))


#[ Garbage Collection

Nightshade uses a stop-and-copy garbage collector that compacts the items
in dynamic space every time it runs.  Most users cause the system to garbage
collect (GC) frequently, long before space is exhausted.  With 16 or 24
megabytes of memory, causing GC's more frequently on less garbage allows the
system to GC without much (if any) paging.

The following functions invoke the garbage collector or control whether
automatic garbage collection is in effect:

{function:ext:gc}
{function:ext:gc-on}
{function:ext:gc-off}

[ GC Parameters ]
[ Weak Pointers ]
[ Finalization  ]
]#

#[ GC Parameters

There are a few variables that control the behavior of the garbage
collector.

{variable:ext:*bytes-consed-between-gcs*}
{variable:ext:*gc-verbose*}
{function:ext::default-gc-notify-before}
{function:ext::default-gc-notify-after}
{variable:ext:*gc-notify-before*}
{variable:ext:*gc-notify-after*}

Note that a garbage collection will happen at roughly the new threshold
printed by the default *gc-notify-after* function.  The system periodically
checks whether this threshold has been exceeded, and only then does a
garbage collection.

{variable:ext:*gc-inhibit-hook*}
{variable:ext:*before-gc-hooks*}
{variable:ext:*after-gc-hooks*}
]#


;;;; DYNAMIC-USAGE and friends.

(proclaim '(special *read-only-space-free-pointer*
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
	   (error "The current dynamic space is missing.")))))


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
	 (error "The optional argument to ROOM must be T, NIL or :DEFAULT.")))
      (room-minimal-info))
  (values))


;;;; GET-BYTES-CONSED.

;;; Internal State
;;;
(defvar *last-bytes-in-use* nil)
(defvar *total-bytes-consed* 0)

(declaim (type (or index null) *last-bytes-in-use*))
(declaim (type integer *total-bytes-consed*))

;;; GET-BYTES-CONSED -- Exported
;;;
(defun get-bytes-consed ()
  "Return the number of bytes allocated since the first time this function
   was called.  If this is the first time, return zero."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((null *last-bytes-in-use*)
	 (setq *last-bytes-in-use* (dynamic-usage))
	 (setq *total-bytes-consed* 0))
	(t
	 (let ((bytes (dynamic-usage)))
	   (incf *total-bytes-consed*
		 (the index (- bytes *last-bytes-in-use*)))
	   (setq *last-bytes-in-use* bytes))))
  *total-bytes-consed*)


;;;; Variables and constants.

(defconstant default-bytes-consed-between-gcs 2000000
  "The fallback value of *bytes-consed-between-gcs* and *gc-trigger*.")

;;; This variable is the user-settable variable that specifices the
;;; minimum amount of dynamic space which must be consed before a GC
;;; will be triggered.
;;;
(defvar *bytes-consed-between-gcs* default-bytes-consed-between-gcs
  "The minimum number of bytes of dynamic space that must be consed before
   the next GC will occur.  GC occurs automatically whenever the amount of
   memory allocated to dynamic objects exceeds the value of an internal
   variable.  After each GC, the system sets the internal variable to the
   amount of dynamic space in use at that point plus the value of the
   *bytes-consed-between-gcs*.")
;;;
(declaim (type index *bytes-consed-between-gcs*))

;;; Public
;;;
(defvar *gc-run-time* 0
  "The total CPU time (run-time) spend doing garbage collection (as
   reported by `get-internal-run-time').")

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
;;; feature in the C code for allocation and binding stack access and a lot
;;; of stuff wants this INTERNAL_GC_TRIGGER available as well.
#+(or ibmrt x86)
(defvar vm::*internal-gc-trigger*)

;;; The following specials are used to control when garbage collection
;;; occurs.
;;;

;;; *GC-INHIBIT*
;;;
;;; When true, inhibits garbage collection.
;;;
(defvar *gc-inhibit* nil)

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

;;; *BEFORE-GC-HOOKS*
;;; *AFTER-GC-HOOKS*
;;;
(defvar *before-gc-hooks* ()
  "A list of functions called before garbage collection occurs.  The
   functions should take no arguments.")
;;;
(defvar *after-gc-hooks* ()
  "A list of functions called after garbage collection occurs.  The
   functions should take no arguments.")

;;; *GC-INHIBIT-HOOK*
;;;
;;; Invoked whenever `sub-gc' intends to GC (unless the GC was explicitly
;;; forced by calling `ext:gc').
;;;
(defvar *gc-inhibit-hook* nil
  "Should be bound to a function of one argument or ().  When the system
   triggers an automatic GC and this is a function, then the system calls
   the function with the amount of dynamic space currently in use (in
   bytes).  If the function returns (), then the GC occurs; otherwise, the
   system turns off automatic GC as if by `ext:gc-off' and *gc-inhibit* and
   *need-to-collect-garbage* are left bound to T.  The writer of this hook
   is responsible for knowing when automatic GC has been turned off and for
   calling `ext:gc-on' or providing a way to call `ext:gc-on'.")

;;; *GC-VERBOSE*
;;;
(defvar *gc-verbose* ()
  "If true the functions bound to *gc-notify-before* and *gc-notify-after*
   are called before and after a garbage collection occurs, respectively.
   If :beep, causes the default notify functions to beep.")

(defun default-gc-notify-before (bytes-in-use)
  "Notify the user that the system is about to GC.  Take one argument, the
   amount of dynamic space in use before the GC (measured in bytes).  Print
   a message like

   [GC threshold exceeded with 2,107,124 bytes in use.  Commencing GC.]"
  (if (eq *gc-verbose* :beep)
      (system:beep *standard-output*))
  (format t "~&[GC threshold exceeded with ~:D bytes in use.  ~
             Commencing GC.]~%" bytes-in-use)
  (finish-output))
;;;
(defparameter *gc-notify-before* #'default-gc-notify-before
  "A function invoked before GC'ing (when *gc-verbose* is true) with the
   current amount of dynamic usage (in bytes).  It should notify the user
   that the system is going to GC.")

(defun default-gc-notify-after (bytes-retained bytes-freed new-trigger)
  "Notify the user that the system has just GC'd.

   Take three arguments, the amount of dynamic usage (in bytes) now free,
   the number of bytes freed by the GC, and the new GC trigger threshold.

   Print a message like

   [GC completed with 25,680 bytes retained and 2,096,808 bytes freed.]
   [GC will next occur when at least 2,025,680 bytes are in use.]"
  (format t "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%"
	  bytes-retained bytes-freed)
  (format t "[GC will next occur when at least ~:D bytes are in use.]~%"
	  new-trigger)
  (if (eq *gc-verbose* :beep)
      (system:beep *standard-output*))
  (finish-output))
;;;
(defparameter *gc-notify-after* #'default-gc-notify-after
  "A function invoked after GC'ing (when *gc-verbose* is true) with the
   amount of dynamic usage (in bytes) now free, the number of bytes freed
   by the GC, and the new GC trigger threshold.  The function should notify
   the user that the system has finished GC'ing.")


;;;; Internal GC

(alien:def-alien-routine collect-garbage c-call:int
  #+gencgc (last-gen c-call:int))

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

;;; *INTERNAL-GC*
;;;
;;; This variables contains the function that does the real GC.  This is
;;; for low-level GC experimentation.  Do not touch it if you do not
;;; know what you are doing.
;;;
(defvar *internal-gc* #'collect-garbage)


;;;; SUB-GC

;;; CAREFULLY-FUNCALL -- Internal
;;;
;;; Used to carefully invoke hooks.
;;;
(defmacro carefully-funcall (function &rest args)
  `(handler-case (funcall ,function ,@args)
     (error (cond)
       (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
       nil)))

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
	(system:block-interrupts
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

;;; MAYBE-GC -- Internal
;;;
;;; This routine is called by the allocation miscops to decide if a GC
;;; should occur.  The argument, object, is the newly allocated object
;;; which must be returned to the caller.
;;;
(defun maybe-gc (&optional object)
  (sub-gc)
  object)

;;; GC -- Exported
;;;
;;; The user advertised garbage collection function.
;;;
#-gencgc
(defun gc (&optional (verbose-p *gc-verbose*))
  "Initiate a garbage collection.

   If $verbose-p is true, then invoke *gc-notify-before* before GC'ing and
   *gc-notify-after* afterwards."
  (sub-gc :verbose-p verbose-p :force-p t))
;;;
#+gencgc
(defun gc (&key (verbose *gc-verbose*) (gen 0) (full nil))
  "Initiate a garbage collection of $gen generations.

   If $verbose is true, then invoke *gc-notify-before* before GC'ing and
   *gc-notify-after* afterwards."
  (sub-gc :verbose-p verbose :force-p t :gen (if full 6 gen)))


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
  "Enable automatic garbage collection.

   If the system would have GC'ed while automatic GC was off, then call
   `gc'."
  (setq *gc-inhibit* nil)
  (when *need-to-collect-garbage*
    (sub-gc))
  ())

(defun gc-off ()
  "Turn off automatic garbage collection.

   On return, the system will only GC via `gc' or `gc-on'."
  (setq *gc-inhibit* t)
  ())


;;;; Initialization stuff.

(defun gc-init ()
  (when *gc-trigger*
    (if (< *gc-trigger* (dynamic-usage))
	(sub-gc)
	(set-auto-gc-trigger *gc-trigger*))))
