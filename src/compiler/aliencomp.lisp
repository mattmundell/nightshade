;;; Transforms and other stuff used to compile Alien operations.

(in-package "C")
(use-package "ALIEN")
(use-package "SYSTEM")

(export '(%alien-funcall))


;;;; defknowns

(defknown %sap-alien (system-area-pointer alien-type) alien-value
  (flushable movable))
(defknown alien-sap (alien-value) system-area-pointer
  (flushable movable))

(defknown slot (alien-value symbol) t
  (flushable recursive))
(defknown %set-slot (alien-value symbol t) t
  (recursive))
(defknown %slot-addr (alien-value symbol) (alien (* t))
  (flushable movable recursive))

(defknown deref (alien-value &rest index) t
  (flushable))
(defknown %set-deref (alien-value t &rest index) t
  ())
(defknown %deref-addr (alien-value &rest index) (alien (* t))
  (flushable movable))

(defknown %heap-alien (heap-alien-info) t
  (flushable))
(defknown %set-heap-alien (heap-alien-info t) t
  ())
(defknown %heap-alien-addr (heap-alien-info) (alien (* t))
  (flushable movable))

(defknown make-local-alien (local-alien-info) t
  ())
(defknown note-local-alien-type (local-alien-info t) null
  ())
(defknown local-alien (local-alien-info t) t
  (flushable))
(defknown %local-alien-forced-to-memory-p (local-alien-info) (member t nil)
  (movable))
(defknown %set-local-alien (local-alien-info t t) t
  ())
(defknown %local-alien-addr (local-alien-info t) (alien (* t))
  (flushable movable))
(defknown dispose-local-alien (local-alien-info t) t
  ())

(defknown %cast (alien-value alien-type) alien
  (flushable movable))

(defknown naturalize (t alien-type) alien
  (flushable movable))
(defknown deport (alien alien-type) t
  (flushable movable))
(defknown extract-alien-value (system-area-pointer index alien-type) t
  (flushable))
(defknown deposit-alien-value (system-area-pointer index alien-type t) t
  ())

(defknown alien-funcall (alien-value &rest *) *
  (any recursive))
(defknown %alien-funcall (system-area-pointer alien-type &rest *) *)


;;;; Cosmetic transforms.

(deftransform slot ((object slot)
		    ((alien (* t)) symbol))
  '(slot (deref object) slot))

(deftransform %set-slot ((object slot value)
			 ((alien (* t)) symbol t))
  '(%set-slot (deref object) slot value))

(deftransform %slot-addr ((object slot)
			  ((alien (* t)) symbol))
  '(%slot-addr (deref object) slot))


;;;; SLOT support.

(defun find-slot-offset-and-type (alien slot)
  (or (constant-continuation-p slot)
      (give-up "Slot is not constant, so cannot open code access."))
  (let ((type (continuation-type alien)))
    (or (alien-type-type-p type)
	(give-up))
    (let ((alien-type (alien-type-type-alien-type type)))
      (or (alien-record-type-p alien-type)
	  (give-up))
      (let* ((slot-name (continuation-value slot))
	     (field (find slot-name (alien-record-type-fields alien-type)
			  :key #'alien-record-field-name)))
	(or field
	    (abort-transform "~S doesn't have a slot named ~S" alien slot-name))
	(values (alien-record-field-offset field)
		(alien-record-field-type field))))))

#+nil ;; Shouldn't be necessary.
(defoptimizer (slot derive-type) ((alien slot))
  (block nil
    (catch 'give-up
      (multiple-value-bind (slot-offset slot-type)
			   (find-slot-offset-and-type alien slot)
	(declare (ignore slot-offset))
	(return (make-alien-type-type slot-type))))
    *wild-type*))

(deftransform slot ((alien slot) * * :important t)
  (multiple-value-bind (slot-offset slot-type)
		       (find-slot-offset-and-type alien slot)
    `(extract-alien-value (alien-sap alien)
			  ,slot-offset
			  ',slot-type)))

#+nil ;; ### But what about coersions?
(defoptimizer (%set-slot derive-type) ((alien slot value))
  (block nil
    (catch 'give-up
      (multiple-value-bind (slot-offset slot-type)
			   (find-slot-offset-and-type alien slot)
	(declare (ignore slot-offset))
	(let ((type (make-alien-type-type slot-type)))
	  (assert-continuation-type value type)
	  (return type))))
    *wild-type*))

(deftransform %set-slot ((alien slot value) * * :important t)
  (multiple-value-bind (slot-offset slot-type)
		       (find-slot-offset-and-type alien slot)
    `(deposit-alien-value (alien-sap alien)
			  ,slot-offset
			  ',slot-type
			  value)))

(defoptimizer (%slot-addr derive-type) ((alien slot))
  (block nil
    (catch 'give-up
      (multiple-value-bind (slot-offset slot-type)
			   (find-slot-offset-and-type alien slot)
	(declare (ignore slot-offset))
	(return (make-alien-type-type
		 (make-alien-pointer-type :to slot-type)))))
    *wild-type*))

(deftransform %slot-addr ((alien slot) * * :important t)
  (multiple-value-bind (slot-offset slot-type)
		       (find-slot-offset-and-type alien slot)
    `(%sap-alien (sap+ (alien-sap alien) (/ ,slot-offset vm:byte-bits))
		 ',(make-alien-pointer-type :to slot-type))))


;;;; DEREF support.

(defun find-deref-alien-type (alien)
  (let ((alien-type (continuation-type alien)))
    (or (alien-type-type-p alien-type)
	(give-up))
    (let ((alien-type (alien-type-type-alien-type alien-type)))
      (if (alien-type-p alien-type)
	  alien-type
	  (give-up)))))

(defun find-deref-element-type (alien)
  (let ((alien-type (find-deref-alien-type alien)))
    (typecase alien-type
      (alien-pointer-type
       (alien-pointer-type-to alien-type))
      (alien-array-type
       (alien-array-type-element-type alien-type))
      (t
       (give-up)))))

(defun compute-deref-guts (alien indices)
  (let ((alien-type (find-deref-alien-type alien)))
    (typecase alien-type
      (alien-pointer-type
       (when (cdr indices)
	 (abort-transform "Too many indices for pointer deref: ~D"
			  (length indices)))
       (let ((element-type (alien-pointer-type-to alien-type)))
	 (if indices
	     (let ((bits (alien-type-bits element-type))
		   (alignment (alien-type-alignment element-type)))
	       (or bits
		   (abort-transform "Unknown element size."))
	       (or alignment
		   (abort-transform "Unknown element alignment."))
	       (values '(offset)
		       `(* offset
			   ,(align-offset bits alignment))
		       element-type))
	     (values nil 0 element-type))))
      (alien-array-type
       (let* ((element-type (alien-array-type-element-type alien-type))
	      (bits (alien-type-bits element-type))
	      (alignment (alien-type-alignment element-type))
	      (dims (alien-array-type-dimensions alien-type)))
	 (or (= (length indices) (length dims))
	     (give-up "Incorrect number of indices."))
	 (or bits
	     (give-up "Element size unknown."))
	 (or alignment
	     (give-up "Element alignment unknown."))
	 (if (null dims)
	     (values nil 0 element-type)
	     (let* ((arg (gensym))
		    (args (list arg))
		    (offsetexpr arg))
	       (dolist (dim (cdr dims))
		 (let ((arg (gensym)))
		   (push arg args)
		   (setf offsetexpr `(+ (* ,offsetexpr ,dim) ,arg))))
	       (values (reverse args)
		       `(* ,offsetexpr
			   ,(align-offset bits alignment))
		       element-type)))))
      (t
       (abort-transform "~S not either a pointer or array type."
			alien-type)))))

#+nil ;; Shouldn't be necessary.
(defoptimizer (deref derive-type) ((alien &rest noise))
  (declare (ignore noise))
  (block nil
    (catch 'give-up
      (return (make-alien-type-type (find-deref-element-type alien))))
    *wild-type*))

(deftransform deref ((alien &rest indices) * * :important t)
  (multiple-value-bind
      (indices-args offset-expr element-type)
      (compute-deref-guts alien indices)
    `(lambda (alien ,@indices-args)
       (extract-alien-value (alien-sap alien)
			    ,offset-expr
			    ',element-type))))

#+nil ;; ### Again, the value might be coerced.
(defoptimizer (%set-deref derive-type) ((alien value &rest noise))
  (declare (ignore noise))
  (block nil
    (catch 'give-up
      (let ((type (make-alien-type-type
		   (make-alien-pointer-type
		    :to (find-deref-element-type alien)))))
	(assert-continuation-type value type)
	(return type)))
    *wild-type*))

(deftransform %set-deref ((alien value &rest indices) * * :important t)
  (multiple-value-bind
      (indices-args offset-expr element-type)
      (compute-deref-guts alien indices)
    `(lambda (alien value ,@indices-args)
       (deposit-alien-value (alien-sap alien)
			    ,offset-expr
			    ',element-type
			    value))))

(defoptimizer (%deref-addr derive-type) ((alien &rest noise))
  (declare (ignore noise))
  (block nil
    (catch 'give-up
      (return (make-alien-type-type
	       (make-alien-pointer-type
		:to (find-deref-element-type alien)))))
    *wild-type*))

(deftransform %deref-addr ((alien &rest indices) * * :important t)
  (multiple-value-bind
      (indices-args offset-expr element-type)
      (compute-deref-guts alien indices)
    `(lambda (alien ,@indices-args)
       (%sap-alien (sap+ (alien-sap alien) (/ ,offset-expr vm:byte-bits))
		   ',(make-alien-pointer-type :to element-type)))))


;;;; Heap Alien Support.

(defun heap-alien-sap-and-type (info)
  (or (constant-continuation-p info)
      (give-up "Info not constant; can't open code."))
  (let ((info (continuation-value info)))
    (values (heap-alien-info-sap-form info)
	    (heap-alien-info-type info))))

#+nil ;; Shouldn't be necessary.
(defoptimizer (%heap-alien derive-type) ((info))
  (block nil
    (catch 'give-up
      (multiple-value-bind (sap type)
			   (heap-alien-sap-and-type info)
	(declare (ignore sap))
	(return (make-alien-type-type type))))
    *wild-type*))

(deftransform %heap-alien ((info) * * :important t)
  (multiple-value-bind (sap type)
		       (heap-alien-sap-and-type info)
    `(extract-alien-value ,sap 0 ',type)))

#+nil ;; ### Again, deposit value might change the type.
(defoptimizer (%set-heap-alien derive-type) ((info value))
  (block nil
    (catch 'give-up
      (multiple-value-bind (sap type)
			   (heap-alien-sap-and-type info)
	(declare (ignore sap))
	(let ((type (make-alien-type-type type)))
	  (assert-continuation-type value type)
	  (return type))))
    *wild-type*))

(deftransform %set-heap-alien ((info value) (heap-alien-info *) * :important t)
  (multiple-value-bind (sap type)
		       (heap-alien-sap-and-type info)
    `(deposit-alien-value ,sap 0 ',type value)))

(defoptimizer (%heap-alien-addr derive-type) ((info))
  (block nil
    (catch 'give-up
      (multiple-value-bind (sap type)
			   (heap-alien-sap-and-type info)
	(declare (ignore sap))
	(return (make-alien-type-type (make-alien-pointer-type :to type)))))
    *wild-type*))

(deftransform %heap-alien-addr ((info) * * :important t)
  (multiple-value-bind (sap type)
		       (heap-alien-sap-and-type info)
    `(%sap-alien ,sap ',type)))


;;;; Local (stack or register) alien support.

(deftransform make-local-alien ((info) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let* ((info (continuation-value info))
	 (alien-type (local-alien-info-type info))
	 (bits (alien-type-bits alien-type)))
    (or bits
	(abort-transform "Unknown size: ~S" (unparse-alien-type alien-type)))
    (if (local-alien-info-force-to-memory-p info)
	(if (backend-featurep :x86)
	    `(truly-the system-area-pointer
			(%primitive alloc-alien-stack-space
				    ,(ceiling (alien-type-bits alien-type)
					      vm:byte-bits)))
	  `(truly-the system-area-pointer
		      (%primitive alloc-number-stack-space
				  ,(ceiling (alien-type-bits alien-type)
					    vm:byte-bits))))
	(let* ((alien-rep-type-spec (compute-alien-rep-type alien-type))
	       (alien-rep-type (specifier-type alien-rep-type-spec)))
	  (cond ((csubtypep (specifier-type 'system-area-pointer)
			    alien-rep-type)
		 '(int-sap 0))
		((ctypep 0 alien-rep-type) 0)
		((ctypep 0.0f0 alien-rep-type) 0.0f0)
		((ctypep 0.0d0 alien-rep-type) 0.0d0)
		(t
		 (compiler-error
		  "Aliens of type ~S cannot be represented immediately."
		  (unparse-alien-type alien-type))))))))

(deftransform note-local-alien-type ((info var) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let ((info (continuation-value info)))
    (or (local-alien-info-force-to-memory-p info)
	(let ((var-node (continuation-use var)))
	  (when (ref-p var-node)
	    (propagate-to-refs (ref-leaf var-node)
			       (specifier-type
				(compute-alien-rep-type
				 (local-alien-info-type info))))))))
  'nil)

(deftransform local-alien ((info var) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let* ((info (continuation-value info))
	 (alien-type (local-alien-info-type info)))
    (if (local-alien-info-force-to-memory-p info)
	`(extract-alien-value var 0 ',alien-type)
	`(naturalize var ',alien-type))))

(deftransform %local-alien-forced-to-memory-p ((info) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let ((info (continuation-value info)))
    (local-alien-info-force-to-memory-p info)))

(deftransform %set-local-alien ((info var value) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let* ((info (continuation-value info))
	 (alien-type (local-alien-info-type info)))
    (if (local-alien-info-force-to-memory-p info)
	`(deposit-alien-value var 0 ',alien-type value)
	'(error "This should be dead-code eleminated."))))

(defoptimizer (%local-alien-addr derive-type) ((info var))
  (if (constant-continuation-p info)
      (let* ((info (continuation-value info))
	     (alien-type (local-alien-info-type info)))
	(make-alien-type-type (make-alien-pointer-type :to alien-type)))
      *wild-type*))

(deftransform %local-alien-addr ((info var) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let* ((info (continuation-value info))
	 (alien-type (local-alien-info-type info)))
    (if (local-alien-info-force-to-memory-p info)
	`(%sap-alien var ',(make-alien-pointer-type :to alien-type))
	(error "This shouldn't happen."))))

(deftransform dispose-local-alien ((info var) * * :important t)
  (or (constant-continuation-p info)
      (abort-transform "Local Alien Info isn't constant?"))
  (let* ((info (continuation-value info))
	 (alien-type (local-alien-info-type info)))
    (if (local-alien-info-force-to-memory-p info)
	(if (backend-featurep :x86)
	    `(%primitive dealloc-alien-stack-space
			 ,(ceiling (alien-type-bits alien-type)
				   vm:byte-bits))
	  `(%primitive dealloc-number-stack-space
		       ,(ceiling (alien-type-bits alien-type)
				 vm:byte-bits)))
      nil)))


;;;; %CAST

(defoptimizer (%cast derive-type) ((alien type))
  (or (when (constant-continuation-p type)
	(let ((alien-type (continuation-value type)))
	  (when (alien-type-p alien-type)
	    (make-alien-type-type alien-type))))
      *wild-type*))

(deftransform %cast ((alien target-type) * * :important t)
  (or (constant-continuation-p target-type)
      (give-up "Alien type not constant; cannot open code."))
  (let ((target-type (continuation-value target-type)))
    (cond ((or (alien-pointer-type-p target-type)
	       (alien-array-type-p target-type)
	       (alien-function-type-p target-type))
	   `(naturalize (alien-sap alien) ',target-type))
	  (t
	   (abort-transform "Cannot cast to alien type ~S" target-type)))))


;;;; alien-sap, %sap-alien, %addr, etc

(deftransform alien-sap ((alien) * * :important t)
  (let ((alien-node (continuation-use alien)))
    (typecase alien-node
      (combination
       (extract-function-args alien '%sap-alien 2)
       '(lambda (sap type)
	  (declare (ignore type))
	  sap))
      (t
       (give-up)))))

(defoptimizer (%sap-alien derive-type) ((sap type))
  (declare (ignore sap))
  (if (constant-continuation-p type)
      (make-alien-type-type (continuation-value type))
      *wild-type*))

(deftransform %sap-alien ((sap type) * * :important t)
  (give-up "Could not optimize away %SAP-ALIEN: forced to do runtime ~@
	    allocation of alien-value structure."))


;;;; Extract/deposit magic

(eval-when (compile eval)
  (defmacro compiler-error-if-loses (form)
    `(handler-case
	 ,form
       (error (condition)
	 (compiler-error "~A" condition)))))

(deftransform naturalize ((object type) * * :important t)
  (or (constant-continuation-p type)
      (give-up "Type not constant at compile time; can't open code."))
  (compiler-error-if-loses
   (compute-naturalize-lambda (continuation-value type))))

(deftransform deport ((alien type) * * :important t)
  (or (constant-continuation-p type)
      (give-up "Type not constant at compile time; can't open code."))
  (compiler-error-if-loses
   (compute-deport-lambda (continuation-value type))))

(deftransform extract-alien-value ((sap offset type) * * :important t)
  (or (constant-continuation-p type)
      (give-up "Type not constant at compile time; can't open code."))
  (compiler-error-if-loses
   (compute-extract-lambda (continuation-value type))))

(deftransform deposit-alien-value ((sap offset type value) * * :important t)
  (or (constant-continuation-p type)
      (give-up "Type not constant at compile time; can't open code."))
  (compiler-error-if-loses
   (compute-deposit-lambda (continuation-value type))))


;;;; Hack to clean up divisions.

(defun count-low-order-zeros (thing)
  (typecase thing
    (continuation
     (if (constant-continuation-p thing)
	 (count-low-order-zeros (continuation-value thing))
	 (count-low-order-zeros (continuation-use thing))))
    (combination
     (case (continuation-function-name (combination-fun thing))
       ((+ -)
	(let ((min most-positive-fixnum)
	      (itype (specifier-type 'integer)))
	  (dolist (arg (combination-args thing) min)
	    (if (csubtypep (continuation-type arg) itype)
		(setf min (min min (count-low-order-zeros arg)))
		(return 0)))))
       (*
	(let ((result 0)
	      (itype (specifier-type 'integer)))
	  (dolist (arg (combination-args thing) result)
	    (if (csubtypep (continuation-type arg) itype)
		(setf result (+ result (count-low-order-zeros arg)))
		(return 0)))))
       (ash
	(let ((args (combination-args thing)))
	  (if (= (length args) 2)
	      (let ((amount (second args)))
		(if (constant-continuation-p amount)
		    (max (+ (count-low-order-zeros (first args))
			    (continuation-value amount))
			 0)
		    0))
	      0)))
       (t
	0)))
    (integer
     (if (zerop thing)
	 most-positive-fixnum
	 (do ((result 0 (1+ result))
	      (num thing (ash num -1)))
	     ((logbitp 0 num) result))))
    (t
     0)))

(deftransform / ((numerator denominator) (integer integer))
  (or (constant-continuation-p denominator)
      (give-up))
  (let* ((denominator (continuation-value denominator))
	 (bits (1- (integer-length denominator))))
    (or (= (ash 1 bits) denominator)
	(give-up))
    (let ((alignment (count-low-order-zeros numerator)))
      (or (>= alignment bits)
	  (give-up))
      `(ash numerator ,(- bits)))))

(deftransform ash ((value amount))
  (let ((value-node (continuation-use value)))
    (or (and (combination-p value-node)
	     (eq (continuation-function-name (combination-fun value-node))
		 'ash))
	(give-up))
    (let ((inside-args (combination-args value-node)))
      (or (= (length inside-args) 2)
	  (give-up))
      (let ((inside-amount (second inside-args)))
	(or (and (constant-continuation-p inside-amount)
		 (not (minusp (continuation-value inside-amount))))
	    (give-up)))))
  (extract-function-args value 'ash 2)
  '(lambda (value amount1 amount2)
     (ash value (+ amount1 amount2))))


;;;; ALIEN-FUNCALL support.

(deftransform alien-funcall ((function &rest args)
			     ((alien (* t)) &rest *) *
			     :important t)
  (let ((names (loop repeat (length args) collect (gensym))))
    `(lambda (function ,@names)
       (alien-funcall (deref function) ,@names))))

(deftransform alien-funcall ((function &rest args) * * :important t)
  (let ((type (continuation-type function)))
    (or (alien-type-type-p type)
	(give-up "Can't tell function type at compile time."))
    (let ((alien-type (alien-type-type-alien-type type)))
      (or (alien-function-type-p alien-type)
	  (give-up))
      (let ((arg-types (alien-function-type-arg-types alien-type)))
	(or (= (length args) (length arg-types))
	    (abort-transform "Wrong number of arguments.  Expected ~D, got ~D."
			     (length arg-types) (length args)))
	(collect ((params) (deports))
	  (dolist (arg-type arg-types)
	    (let ((param (gensym)))
	      (params param)
	      (deports `(deport ,param ',arg-type))))
	  (let ((return-type (alien-function-type-result-type alien-type))
		(body `(%alien-funcall (deport function ',alien-type)
				       ',alien-type
				       ,@(deports))))
	    (if (alien-values-type-p return-type)
		(collect ((temps) (results))
		  (dolist (type (alien-values-type-values return-type))
		    (let ((temp (gensym)))
		      (temps temp)
		      (results `(naturalize ,temp ',type))))
		  (setf body
			`(multiple-value-bind
			     ,(temps)
			     ,body
			   (values ,@(results)))))
		(setf body `(naturalize ,body ',return-type)))
	    `(lambda (function ,@(params))
	       ,body)))))))

(defoptimizer (%alien-funcall derive-type) ((function type &rest args))
  (declare (ignore function args))
  (or (constant-continuation-p type)
      (error "Something is broken."))
  (let ((type (continuation-value type)))
    (or (alien-function-type-p type)
	(error "Something is broken."))
    (specifier-type
     (compute-alien-rep-type
      (alien-function-type-result-type type)))))

(defoptimizer (%alien-funcall ltn-annotate)
	      ((function type &rest args) node policy)
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  (annotate-ordinary-continuation function policy)
  (dolist (arg args)
    (annotate-ordinary-continuation arg policy)))

(defoptimizer (%alien-funcall ir2-convert)
	      ((function type &rest args) call block)
  (let ((type (if (constant-continuation-p type)
		  (continuation-value type)
		  (error "Something is broken.")))
	(cont (node-cont call))
	(args args))
    (multiple-value-bind (nsp stack-frame-size arg-tns result-tns)
			 (make-call-out-tns type)
      (vop alloc-number-stack-space call block stack-frame-size nsp)
      (dolist (tn arg-tns)
	(let* ((arg (pop args))
	       (sc (tn-sc tn))
	       (scn (sc-number sc))
	       (temp-tn (make-representation-tn (tn-primitive-type tn) scn))
	       (move-arg-vops (svref (sc-move-arg-vops sc) scn)))
	  (assert arg)
	  (assert (= (length move-arg-vops) 1) ()
		  "No unique move-arg-vop for moves in SC ~S."
		  (sc-name sc))
	  (emit-move call block (continuation-tn call block arg) temp-tn)
	  (emit-move-arg-template call block (first move-arg-vops)
				  temp-tn nsp tn)))
      (assert (null args))
      (or (listp result-tns)
	  (setf result-tns (list result-tns)))
      (vop* call-out call block
	    ((continuation-tn call block function)
	     (reference-tn-list arg-tns nil))
	    ((reference-tn-list result-tns t)))
      (vop dealloc-number-stack-space call block stack-frame-size)
      (move-continuation-result call block result-tns cont))))
