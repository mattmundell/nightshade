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
