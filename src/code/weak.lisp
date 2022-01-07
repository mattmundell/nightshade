;;; Weak pointer support.

(in-package "EXTENSIONS")

(export '(weak-pointer weak-pointer-p make-weak-pointer weak-pointer-value))

#[ Weak Pointers

A weak pointer provides a way to maintain a reference to an object without
preventing an object from being garbage collected.  If the garbage collector
discovers that the only pointers to an object are weak pointers, then it
breaks the weak pointers and deallocates the object.

{function:make-weak-pointer}
{function:weak-pointer-value}
]#

(defun make-weak-pointer (object)
  "Allocate and return a weak pointer to $object."
  (declare (values weak-pointer))
  (make-weak-pointer object))

(declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  "If $weak-pointer is valid, returns the value of $weak-pointer and t.  If
   the referent of $weak-pointer has been garbage collected, return the
   values () and ()."
  (declare (type weak-pointer weak-pointer)
	   (values t (member t nil)))
  ;; We don't need to wrap this with a without-gcing, because once we have
  ;; extracted the value, our reference to it will keep the weak pointer
  ;; from becoming broken.  We just have to make sure the compiler won't
  ;; reorder these primitives.
  (let ((value (c::%weak-pointer-value weak-pointer))
	(broken (c::%weak-pointer-broken weak-pointer)))
    (values value (not broken))))

;;; For the interpreter..

(defun c::%weak-pointer-value (w)
  (declare (type weak-pointer w))
  (c::%weak-pointer-value w))

(defun c::%weak-pointer-broken (w)
  (declare (type weak-pointer w))
  (c::%weak-pointer-broken w))
