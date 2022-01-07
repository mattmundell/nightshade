;;; Some initialization hacks to get the type system started up enough to
;;; define the types used to define types.

(in-package "C")

(deftype inlinep ()
  '(member :inline :maybe-inline :notinline nil))

(deftype boolean ()
  '(member t nil))

(in-package "KERNEL")

;;; Define this so that we can copy type-class structures before the
;;; defstruct for type-class runs.
;;;
(defun copy-type-class (tc)
  (let ((new (make-type-class)))
    (dotimes (i (%instance-length tc))
      (declare (type index i))
      (setf (%instance-ref new i)
	    (%instance-ref tc i)))
    new))

;;; Define the STRUCTURE-OBJECT class as a subclass of INSTANCE.  This must
;;; be the first DEFSTRUCT executed.
;;;
(defstruct (structure-object (:alternate-metaclass instance)))


(in-package "LISP")

;;;; Documentation boot.
;;;
;;; Define an initial simple version of add-documentation that stores the
;;; documentation for insertion in the *documentation* table later (when
;;; more of the system is loaded).

(defvar *pre-doc* ())

(defun add-documentation (string file position)
  "Add STRING as a node of documentation.  The first line of STRING is the
   title of the node."
  (setq *pre-doc* (cons (list string file position) *pre-doc*)))
