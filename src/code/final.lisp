;;; Finalization based on weak pointers.

(in-package "EXTENSIONS")

(export '(finalize cancel-finalization))

#[ Finalization

Finalization provides a "hook" that is triggered when the garbage collector
reclaims an object.  It is usually used to recover non-Lisp resources that
were allocated to implement the finalized Lisp object.  For example, when a
unix file-descriptor stream is collected, finalization is used to close the
underlying file descriptor.

{function:finalize}
{function:cancel-finalization}
]#

(defvar *objects-pending-finalization* ())

(defun finalize (object function)
  "Register $object for [finalization].

   $function is called when $object is reclaimed.  Normally $function is a
   closure over the underlying state that needs to be freed, e.g. the unix
   file descriptor in the fd-stream case.  Care may be required: if
   $function closes over $object then $object remains allocated."
  (declare (type function function))
  (system:without-gcing
   (push (cons (make-weak-pointer object) function)
	 *objects-pending-finalization*))
  object)

(defun cancel-finalization (object)
  "Cancel any finalization registered for $object."
  (if object
      ;; We check to make sure object isn't nil because if there are any
      ;; broken weak pointers, their value will show up as nil.  Therefore,
      ;; they would be deleted from the list, but not finalized.  Broken
      ;; weak pointers shouldn't be left in the list, but why take chances?
      (system:without-gcing
       (setf *objects-pending-finalization*
	     (delete object *objects-pending-finalization*
		     :key #'(lambda (pair)
			      (values (weak-pointer-value (car pair))))))))
  ())

(defun finalize-corpses ()
  (setf *objects-pending-finalization*
	(delete-if #'(lambda (pair)
		       (multiple-value-bind
			   (object valid)
			   (weak-pointer-value (car pair))
			 (declare (ignore object))
			 (unless valid
			   (funcall (cdr pair))
			   t)))
		   *objects-pending-finalization*))
  ())

(pushnew 'finalize-corpses *after-gc-hooks*)
