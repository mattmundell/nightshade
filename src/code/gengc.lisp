;;; Lisp level interface to the Generational Garbage Collector.

(in-package "EXTENSIONS")
(export '(*before-gc-hooks* *after-gc-hooks* gc purify
	  *gc-verbose* *gc-notify-before* *gc-notify-after*))

(in-package "LISP")


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

;;; *GC-VERBOSE* -- interface
;;;
(defvar *gc-verbose* ()
  "When true, causes the functions bound to *GC-NOTIFY-BEFORE* and
   *GC-NOTIFY-AFTER* to be called before and after a garbage collection
   occurs respectively.  If :BEEP, causes the default notify functions to
   beep annoyingly.")

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

;;; CAREFULLY-FUNCALL -- Internal
;;;
;;; Used to carefully invoke hooks.
;;;
(defmacro carefully-funcall (function &rest args)
  `(handler-case (funcall ,function ,@args)
     (error (cond)
       (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
       nil)))

;;; DO-BEFORE-GC-STUFF -- interface.
;;;
;;; Called by the C code just before doing a GC.
;;;
(defun do-before-gc-stuff ()
  (when *gc-verbose*
    (carefully-funcall *gc-notify-before* 0))
  (dolist (hook *before-gc-hooks*)
    (carefully-funcall hook))
  nil)

;;; DO-AFTER-GC-STUFF -- interface.
;;;
;;; Called by the C code just after doing a GC.
;;;
(defun do-after-gc-stuff ()
  (dolist (hook *after-gc-hooks*)
    (carefully-funcall hook))
  (when *gc-verbose*
    (carefully-funcall *gc-notify-after* 0 0 0))
  nil)


;;;; Interface to GC routines

(alien:def-alien-routine ("collect_garbage" gc) c-call:void
  "Force a garbage collection.")

(defun purify (&key root-structures constants)
  (declare (ignore root-structures constants))
  (gc))

(defun gc-init ()
  ;; Nothing to do.
  (undefined-value))
