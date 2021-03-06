;;; LOAD-TIME-VALUE implementation.

(in-package "C")

(in-package "LISP")
(export 'load-time-value)

(in-package "C")

(defknown %load-time-value (t) t (flushable movable))

(def-ir1-translator load-time-value ((form &optional read-only-p) start cont)
  "Arrange for FORM to be evaluated at load-time and use the value produced
   as if it were a constant.  If READ-ONLY-P is true, then the resultant
   object is guaranteed to never be modified, so it can be put in read-only
   storage."
  (if (producing-fasl-file)
      (multiple-value-bind
	  (handle type)
	  (compile-load-time-value (if read-only-p
				       form
				       `(make-value-cell ,form)))
	(declare (ignore type))
	(ir1-convert start cont
		     (if read-only-p
			 `(%load-time-value ',handle)
			 `(value-cell-ref (%load-time-value ',handle)))))
      (let ((value
	     (handler-case (eval form)
	       (error (condition)
		 (compiler-error "(during EVAL of LOAD-TIME-VALUE)~%~A"
				 condition)))))
	(ir1-convert start cont
		     (if read-only-p
			 `',value
			 `(value-cell-ref ',(make-value-cell value)))))))

(defoptimizer (%load-time-value ir2-convert) ((handle) node block)
  (assert (constant-continuation-p handle))
  (let ((cont (node-cont node))
	(tn (make-load-time-value-tn (continuation-value handle)
				     *universal-type*)))
    (move-continuation-result node block (list tn) cont)))
