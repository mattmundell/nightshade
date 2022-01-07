;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ir2tran.lisp,v 1.69 2001/09/25 21:24:39 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the virtual machine independent parts of the code
;;; which does the actual translation of nodes to VOPs.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")
(in-package "KERNEL")
(export '(%caller-frame-and-pc))
(in-package "C")

(export '(safe-fdefn-function return-single instance-ref instance-set
			      funcallable-instance-lexenv))


#+sparc
(defvar *always-clear-stack* nil
  "Always perform stack clearing if non-NIL, independent of the
compilation policy")

#+sparc
(defvar *enable-stack-clearing* t
  "If non-NIL and the compilation policy allows, stack clearing is enabled.")


;;;; Moves and type checks:

;;; Emit-Move  --  Internal
;;;
;;;    Move X to Y unless they are EQ.
;;;
(defun emit-move (node block x y)
  (declare (type node node) (type ir2-block block) (type tn x y))
  (unless (eq x y)
    (vop move node block x y))
  (undefined-value))


;;; Type-Check-Template  --  Interface
;;;
;;;    If there is any CHECK-xxx template for Type, then return it, otherwise
;;; return NIL.
;;;
(defun type-check-template (type)
  (declare (type ctype type))
  (multiple-value-bind (check-ptype exact)
		       (primitive-type type)
    (if exact
	(primitive-type-check check-ptype)
	(let ((name (hairy-type-check-template type)))
	  (if name
	      (template-or-lose name *backend*)
	      nil)))))


;;; Emit-Type-Check  --  Internal
;;;
;;;    Emit code in Block to check that Value is of the specified Type,
;;; yielding the checked result in Result.  Value and result may be of any
;;; primitive type.  There must be CHECK-xxx VOP for Type.  Any other type
;;; checks should have been converted to an explicit type test.
;;;
(defun emit-type-check (node block value result type)
  (declare (type tn value result) (type node node) (type ir2-block block)
	   (type ctype type))
  (emit-move-template node block (type-check-template type) value result)
  (undefined-value))

;;; MAKE-VALUE-CELL  --  Internal
;;;
;;;    Allocate an indirect value cell.  Maybe do some clever stack allocation
;;; someday.
;;;
(defevent make-value-cell "Allocate heap value cell for lexical var.")
(defun do-make-value-cell (node block value res)
  (event make-value-cell node)
  (vop make-value-cell node block value res))


;;;; Leaf reference:

;;; Find-In-Environment  --  Internal
;;;
;;;    Return the TN that holds the value of Thing in the environment Env.
;;;
(defun find-in-environment (thing env)
  (declare (type (or nlx-info lambda-var) thing) (type environment env)
	   (values tn))
  (or (cdr (assoc thing (ir2-environment-environment (environment-info env))))
      (etypecase thing
	(lambda-var
	 (assert (eq env (lambda-environment (lambda-var-home thing))))
	 (leaf-info thing))
	(nlx-info
	 (assert (eq env (block-environment (nlx-info-target thing))))
	 (ir2-nlx-info-home (nlx-info-info thing))))))


;;; Constant-TN  --  Internal
;;;
;;;    If Leaf already has a constant TN, return that, otherwise make a TN for
;;; it.
;;;
(defun constant-tn (leaf)
  (declare (type constant leaf))
  (or (leaf-info leaf)
      (setf (leaf-info leaf)
	    (make-constant-tn leaf))))

  
;;; Leaf-TN  --  Internal
;;;
;;;    Return a TN that represents the value of Leaf, or NIL if Leaf isn't
;;; directly represented by a TN.  Env is the environment that the reference is
;;; done in.
;;;
(defun leaf-tn (leaf env)
  (declare (type leaf leaf) (type environment env))
  (typecase leaf
    (lambda-var
     (unless (lambda-var-indirect leaf)
       (find-in-environment leaf env)))
    (constant (constant-tn leaf))
    (t nil)))


;;; Emit-Constant  --  Internal
;;;
;;;    Used to conveniently get a handle on a constant TN during IR2
;;; conversion.  Returns a constant TN representing the Lisp object Value.
;;;
(defun emit-constant (value)
  (constant-tn (find-constant value)))


;;; IR2-Convert-Ref  --  Internal
;;;
;;;    Convert a Ref node.  The reference must not be delayed.
;;;
(defun ir2-convert-ref (node block)
  (declare (type ref node) (type ir2-block block))
  (let* ((cont (node-cont node))
	 (leaf (ref-leaf node))
	 (name (leaf-name leaf))
	 (locs (continuation-result-tns
		cont (list (primitive-type (leaf-type leaf)))))
	 (res (first locs)))
    (etypecase leaf
      (lambda-var
       (let ((tn (find-in-environment leaf (node-environment node))))
	 (if (lambda-var-indirect leaf)
	     (vop value-cell-ref node block tn res)
	     (emit-move node block tn res))))
      (constant
       (if (legal-immediate-constant-p leaf)
	   (emit-move node block (constant-tn leaf) res)
	   (let ((name-tn (emit-constant name)))
	     (if (policy node (zerop safety))
		 (vop fast-symbol-value node block name-tn res)
		 (vop symbol-value node block name-tn res)))))
      (functional
       (ir2-convert-closure node block leaf res))
      (global-var
       (let ((unsafe (policy node (zerop safety))))
	 (ecase (global-var-kind leaf)
	   ((:special :global :constant)
	    (assert (symbolp name))
	    (let ((name-tn (emit-constant name)))
	      (if unsafe
		  (vop fast-symbol-value node block name-tn res)
		  (vop symbol-value node block name-tn res))))
	   (:global-function
	    (let ((fdefn-tn (make-load-time-constant-tn :fdefinition name)))
	      (if unsafe
		  (vop fdefn-function node block fdefn-tn res)
		  (vop safe-fdefn-function node block fdefn-tn res))))))))

    (move-continuation-result node block locs cont))
  (undefined-value))


;;; IR2-Convert-Closure  --  Internal
;;;
;;;    Emit code to load a function object representing Leaf into Res.  This
;;; gets interesting when the referenced function is a closure: we must make
;;; the closure and move the closed over values into it.
;;;
;;; Leaf is either a :TOP-LEVEL-XEP functional or the XEP lambda for the called
;;; function, since local call analysis converts all closure references.  If a
;;; TL-XEP, we know it is not a closure.
;;;
;;; If a closed-over lambda-var has no refs (is deleted), then we don't
;;; initialize that slot.  This can happen with closures over top-level
;;; variables, where optimization of the closure deleted the variable.  Since
;;; we committed to the closure format when we pre-analyzed the top-level code,
;;; we just leva an empty slot.
;;;
(defun ir2-convert-closure (node block leaf res)
  (declare (type ref node) (type ir2-block block)
	   (type functional leaf) (type tn res))
  (unless (leaf-info leaf)
    (setf (leaf-info leaf) (make-entry-info)))
  (let ((entry (make-load-time-constant-tn :entry leaf))
	(closure (etypecase leaf
		   (clambda
		    (environment-closure (get-lambda-environment leaf)))
		   (functional
		    (assert (eq (functional-kind leaf) :top-level-xep))
		    nil))))
    (cond (closure
	   (let ((this-env (node-environment node)))
	     (vop make-closure node block entry (length closure) res)
	     (loop for what in closure and n from 0 do
	       (unless (and (lambda-var-p what)
			    (null (leaf-refs what)))
		 (vop closure-init node block
		      res
		      (find-in-environment what this-env)
		      n)))))
	  (t
	   (emit-move node block entry res))))
  (undefined-value))


;;; IR2-Convert-Set  --  Internal
;;;
;;;    Convert a Set node.  If the node's cont is annotated, then we also
;;; deliver the value to that continuation.  If the var is a lexical variable
;;; with no refs, then we don't actually set anything, since the variable has
;;; been deleted.
;;;
(defun ir2-convert-set (node block)
  (declare (type cset node) (type ir2-block block))
  (let* ((cont (node-cont node))
	 (leaf (set-var node))
	 (val (continuation-tn node block (set-value node)))
	 (locs (if (continuation-info cont)
		   (continuation-result-tns
		    cont (list (primitive-type (leaf-type leaf))))
		   nil)))
    (etypecase leaf
      (lambda-var
       (when (leaf-refs leaf)
	 (let ((tn (find-in-environment leaf (node-environment node))))
	   (if (lambda-var-indirect leaf)
	       (vop value-cell-set node block tn val)
	       (emit-move node block val tn)))))
      (global-var
       (ecase (global-var-kind leaf)
	 ((:special :global)
	  (assert (symbolp (leaf-name leaf)))
	  (vop set node block (emit-constant (leaf-name leaf)) val)))))

    (when locs
      (emit-move node block val (first locs))
      (move-continuation-result node block locs cont)))
  (undefined-value))


;;;; Utilities for receiving fixed values:

;;; Continuation-TN  --  Internal
;;;
;;;    Return a TN that can be referenced to get the value of Cont.  Cont must
;;; be LTN-Annotated either as a delayed leaf ref or as a fixed, single-value
;;; continuation.  If a type check is called for, do it.
;;;
;;;    The primitive-type of the result will always be the same as the
;;; ir2-continuation-primitive-type, ensuring that VOPs are always called with
;;; TNs that satisfy the operand primitive-type restriction.  We may have to
;;; make a temporary of the desired type and move the actual continuation TN
;;; into it.  This happens when we delete a type check in unsafe code or when
;;; we locally know something about the type of an argument variable.
;;;
(defun continuation-tn (node block cont)
  (declare (type node node) (type ir2-block block) (type continuation cont))
  (let* ((2cont (continuation-info cont))
	 (cont-tn 
	  (ecase (ir2-continuation-kind 2cont)
	    (:delayed
	     (let ((ref (continuation-use cont)))
	       (leaf-tn (ref-leaf ref) (node-environment ref))))
	    (:fixed
	     (assert (= (length (ir2-continuation-locs 2cont)) 1))
	     (first (ir2-continuation-locs 2cont)))))
	 (ptype (ir2-continuation-primitive-type 2cont)))
    
    (cond ((and (eq (continuation-type-check cont) t)
		(multiple-value-bind (check types)
		    (continuation-check-types cont)
		  (assert (eq check :simple))
		  ;; If the proven type is a subtype of the possibly
		  ;; weakened type check then it's always True and is
		  ;; flushed.
		  (unless (values-subtypep (continuation-proven-type cont)
					   (first types))
		    (let ((temp (make-normal-tn ptype)))
		      (emit-type-check node block cont-tn temp
				       (first types))
		      temp)))))
	  ((eq (tn-primitive-type cont-tn) ptype) cont-tn)
	  (t
	   (let ((temp (make-normal-tn ptype)))
	     (emit-move node block cont-tn temp)
	     temp)))))


;;; CONTINUATION-TNS  --  Internal
;;;
;;;    Similar to CONTINUATION-TN, but hacks multiple values.  We return
;;; continuations holding the values of Cont with Ptypes as their primitive
;;; types.  Cont must be annotated for the same number of fixed values are
;;; there are Ptypes.
;;;
;;;    If the continuation has a type check, check the values into temps and
;;; return the temps.  When we have more values than assertions, we move the
;;; extra values with no check.
;;; 
(defun continuation-tns (node block cont ptypes)
  (declare (type node node) (type ir2-block block)
	   (type continuation cont) (list ptypes))
  (let* ((locs (ir2-continuation-locs (continuation-info cont)))
	 (nlocs (length locs)))
    (assert (= nlocs (length ptypes)))
    (if (eq (continuation-type-check cont) t)
	(multiple-value-bind (check types)
			     (continuation-check-types cont)
	  (assert (eq check :simple))
	  (let ((ntypes (length types)))
	    (mapcar #'(lambda (from to-type assertion)
			(let ((temp (make-normal-tn to-type)))
			  (if assertion
			      (emit-type-check node block from temp assertion)
			      (emit-move node block from temp))
			  temp))
		    locs ptypes
		    (if (< ntypes nlocs)
			(append types (make-list (- nlocs ntypes)
						 :initial-element nil))
			types))))
	(mapcar #'(lambda (from to-type)
		    (if (eq (tn-primitive-type from) to-type)
			from
			(let ((temp (make-normal-tn to-type)))
			  (emit-move node block from temp)
			  temp)))
		locs ptypes))))


;;;; Utilities for delivering values to continuations:

;;; Continuation-Result-TNs  --  Internal
;;;
;;;    Return a list of TNs with the specifier Types that can be used as result
;;; TNs to evaluate an expression into the continuation Cont.  This is used
;;; together with Move-Continuation-Result to deliver fixed values to a
;;; continuation.
;;;
;;;    If the continuation isn't annotated (meaning the values are discarded)
;;; or is unknown-values, the then we make temporaries for each supplied value,
;;; providing a place to compute the result in until we decide what to do with
;;; it (if anything.)
;;;
;;;    If the continuation is fixed-values, and wants the same number of values
;;; as the user wants to deliver, then we just return the
;;; IR2-Continuation-Locs.  Otherwise we make a new list padded as necessary by
;;; discarded TNs.  We always return a TN of the specified type, using the
;;; continuation locs only when they are of the correct type.
;;;
(defun continuation-result-tns (cont types)
  (declare (type continuation cont) (type list types))
  (let ((2cont (continuation-info cont)))
    (if (not 2cont)
	(mapcar #'make-normal-tn types)
	(ecase (ir2-continuation-kind 2cont)
	  (:fixed
	   (let* ((locs (ir2-continuation-locs 2cont))
		  (nlocs (length locs))
		  (ntypes (length types)))
	     (if (and (= nlocs ntypes)
		      (do ((loc locs (cdr loc))
			   (type types (cdr type)))
			  ((null loc) t)
			(unless (eq (tn-primitive-type (car loc)) (car type))
			  (return nil))))
		 locs
		 (mapcar #'(lambda (loc type)
			     (if (eq (tn-primitive-type loc) type)
				 loc
				 (make-normal-tn type)))
			 (if (< nlocs ntypes)
			     (append locs
				     (mapcar #'make-normal-tn
					     (subseq types nlocs)))
			     locs)
			 types))))
	  (:unknown
	   (mapcar #'make-normal-tn types))))))


;;; Make-Standard-Value-Tns  --  Internal
;;;
;;;    Make the first N standard value TNs, returning them in a list.
;;;
(defun make-standard-value-tns (n)
  (declare (type unsigned-byte n))
  (collect ((res))
    (dotimes (i n)
      (res (standard-argument-location i)))
    (res)))


;;; Standard-Result-TNs  --  Internal
;;;
;;;    Return a list of TNs wired to the standard value passing conventions
;;; that can be used to receive values according to the unknown-values
;;; convention.  This is used with together Move-Continuation-Result for
;;; delivering unknown values to a fixed values continuation.
;;;
;;;    If the continuation isn't annotated, then we treat as 0-values,
;;; returning an empty list of temporaries.
;;;
;;;    If the continuation is annotated, then it must be :Fixed.
;;;
(defun standard-result-tns (cont)
  (declare (type continuation cont))
  (let ((2cont (continuation-info cont)))
    (if 2cont
	(ecase (ir2-continuation-kind 2cont)
	  (:fixed
	   (make-standard-value-tns (length (ir2-continuation-locs 2cont)))))
	())))


;;; Move-Results-Coerced  --  Internal
;;;
;;;    Just move each Src TN into the corresponding Dest TN, defaulting any
;;; unsupplied source values to NIL.  We let Emit-Move worry about doing the
;;; appropriate coercions.
;;;
(defun move-results-coerced (node block src dest)
  (declare (type node node) (type ir2-block block) (list src dest))
  (let ((nsrc (length src))
	(ndest (length dest)))
    (mapc #'(lambda (from to)
	      (unless (eq from to)
		(emit-move node block from to)))
	  (if (> ndest nsrc)
	      (append src (make-list (- ndest nsrc)
				     :initial-element (emit-constant nil)))
	      src)
	  dest))
  (undefined-value))


;;; Move-Continuation-Result  --  Internal
;;;
;;;    If necessary, emit coercion code needed to deliver the
;;; Results to the specified continuation.  Node and block provide context for
;;; emitting code.  Although usually obtained from Standard-Result-TNs or
;;; Continuation-Result-TNs, Results my be a list of any type or number of TNs.
;;;
;;;    If the continuation is fixed values, then move the results into the
;;; continuation locations.  If the continuation is unknown values, then do the
;;; moves into the standard value locations, and use Push-Values to put the
;;; values on the stack.
;;;
(defun move-continuation-result (node block results cont)
  (declare (type node node) (type ir2-block block)
	   (list results) (type continuation cont))
  (let* ((2cont (continuation-info cont)))
    (when 2cont
      (ecase (ir2-continuation-kind 2cont)
	(:fixed
	 (let ((locs (ir2-continuation-locs 2cont)))
	   (unless (eq locs results)
	     (move-results-coerced node block results locs))))
	(:unknown
	 (let* ((nvals (length results))
		(locs (make-standard-value-tns nvals)))
	   (move-results-coerced node block results locs)
	   (vop* push-values node block
		 ((reference-tn-list locs nil))
		 ((reference-tn-list (ir2-continuation-locs 2cont) t))
		 nvals))))))
  (undefined-value))


;;;; Template conversion:


;;; Reference-Arguments  --  Internal
;;;
;;;    Build a TN-Refs list that represents access to the values of the
;;; specified list of continuations Args for Template.  Any :CONSTANT arguments
;;; are returned in the second value as a list rather than being accessed as a
;;; normal argument.  Node and Block provide the context for emitting any
;;; necessary type-checking code.
;;;
(defun reference-arguments (node block args template)
  (declare (type node node) (type ir2-block block) (list args)
	   (type template template))
  (collect ((info-args))
    (let ((last nil)
	  (first nil))
      (do ((args args (cdr args))
	   (types (template-arg-types template) (cdr types)))
	  ((null args))
	(let ((type (first types))
	      (arg (first args)))
	  (if (and (consp type) (eq (car type) ':constant))
	      (info-args (continuation-value arg))
	      (let ((ref (reference-tn (continuation-tn node block arg) nil)))
		(if last
		    (setf (tn-ref-across last) ref)
		    (setf first ref))
		(setq last ref)))))

      (values (the (or tn-ref null) first) (info-args)))))


;;; IR2-Convert-Conditional  --  Internal
;;;
;;;    Convert a conditional template.  We try to exploit any drop-through, but
;;; emit an unconditional branch afterward if we fail.  Not-P is true if the
;;; sense of the Template's test should be negated.
;;;
(defun ir2-convert-conditional (node block template args info-args if not-p)
  (declare (type node node) (type ir2-block block)
	   (type template template) (type (or tn-ref null) args)
	   (list info-args) (type cif if) (type boolean not-p))
  (assert (= (template-info-arg-count template) (+ (length info-args) 2)))
  (let ((consequent (if-consequent if))
	(alternative (if-alternative if)))
    (cond ((drop-thru-p if consequent)
	   (emit-template node block template args nil
			  (list* (block-label alternative) (not not-p)
				 info-args)))
	  (t
	   (emit-template node block template args nil
			  (list* (block-label consequent) not-p info-args))
	   (unless (drop-thru-p if alternative)
	     (vop branch node block (block-label alternative)))))))


;;; IR2-Convert-IF  --  Internal
;;;
;;;    Convert an IF that isn't the DEST of a conditional template.
;;;
(defun ir2-convert-if (node block)
  (declare (type ir2-block block) (type cif node))
  (let* ((test (if-test node))
	 (test-ref (reference-tn (continuation-tn node block test) nil))
	 (nil-ref (reference-tn (emit-constant nil) nil)))
    (setf (tn-ref-across test-ref) nil-ref)
    (ir2-convert-conditional node block (template-or-lose 'if-eq *backend*)
			     test-ref () node t)))


;;; FIND-TEMPLATE-RESULT-TYPES  --  Internal
;;;
;;;    Return a list of primitive-types that we can pass to
;;; CONTINUATION-RESULT-TNS describing the result types we want for a template
;;; call.  We duplicate here the determination of output type that was done in
;;; initially selecting the template, so we know that the types we find are
;;; allowed by the template output type restrictions.
;;;
(defun find-template-result-types (call cont template rtypes)
  (declare (type combination call) (type continuation cont)
	   (type template template) (list rtypes))
  (let* ((dtype (node-derived-type call))
	 (type (if (and (or (eq (template-policy template) :safe)
			    (policy call (= safety 0)))
			(continuation-type-check cont))
		   (values-type-intersection
		    dtype
		    (continuation-asserted-type cont))
		   dtype))
	 (types (mapcar #'primitive-type
			(if (values-type-p type)
			    (append (values-type-required type)
				    (values-type-optional type))
			    (list type)))))
    (let ((nvals (length rtypes))
	  (ntypes (length types)))
      (cond ((< ntypes nvals)
	     (append types
		     (make-list (- nvals ntypes)
				:initial-element
				(backend-any-primitive-type *backend*))))
	    ((> ntypes nvals)
	     (subseq types 0 nvals))
	    (t
	     types)))))


;;; MAKE-TEMPLATE-RESULT-TNS  --  Internal
;;;
;;;    Return a list of TNs usable in a Call to Template delivering values to
;;; Cont.  As an efficiency hack, we pick off the common case where the
;;; continuation is fixed values and has locations that satisfy the result
;;; restrictions.  This can fail when there is a type check or a values count
;;; mismatch.
;;;
(defun make-template-result-tns (call cont template rtypes)
  (declare (type combination call) (type continuation cont)
	   (type template template) (list rtypes))
  (let ((2cont (continuation-info cont)))
    (if (and 2cont (eq (ir2-continuation-kind 2cont) :fixed))
	(let ((locs (ir2-continuation-locs 2cont)))
	  (if (and (= (length rtypes) (length locs))
		   (do ((loc locs (cdr loc))
			(rtype rtypes (cdr rtype)))
		       ((null loc) t)
		     (unless (operand-restriction-ok
			      (car rtype)
			      (tn-primitive-type (car loc))
			      :t-ok nil)
		       (return nil))))
	      locs
	      (continuation-result-tns
	       cont
	       (find-template-result-types call cont template rtypes))))
	(continuation-result-tns
	 cont
	 (find-template-result-types call cont template rtypes)))))


;;; IR2-Convert-Template  --  Internal
;;;
;;;    Get the operands into TNs, make TN-Refs for them, and then call the
;;; template emit function. 
;;;
(defun ir2-convert-template (call block)
  (declare (type combination call) (type ir2-block block))
  (let* ((template (combination-info call))
	 (cont (node-cont call))
	 (rtypes (template-result-types template)))
    (multiple-value-bind
	(args info-args)
	(reference-arguments call block (combination-args call) template)
      (assert (not (template-more-results-type template)))
      (if (eq rtypes :conditional)
	  (ir2-convert-conditional call block template args info-args
				   (continuation-dest cont) nil)
	  (let* ((results (make-template-result-tns call cont template rtypes))
		 (r-refs (reference-tn-list results t)))
	    (assert (= (length info-args)
		       (template-info-arg-count template)))
	    (if info-args
		(emit-template call block template args r-refs info-args)
		(emit-template call block template args r-refs))
	    (move-continuation-result call block results cont)))))
  (undefined-value))


;;; %%Primitive IR2 Convert  --  Internal
;;;
;;;    We don't have to do much because operand count checking is done by IR1
;;; conversion.  The only difference between this and the function case of
;;; IR2-Convert-Template is that there can be codegen-info arguments.
;;;
(defoptimizer (%%primitive ir2-convert) ((template info &rest args) call block)
  (let* ((template (continuation-value template))
	 (info (continuation-value info))
	 (cont (node-cont call))
	 (rtypes (template-result-types template))
	 (results (make-template-result-tns call cont template rtypes))
	 (r-refs (reference-tn-list results t)))
    (multiple-value-bind
	(args info-args)
	(reference-arguments call block (cddr (combination-args call))
			     template)
      (assert (not (template-more-results-type template)))
      (assert (not (eq rtypes :conditional)))
      (assert (null info-args))
      
      (if info
	  (emit-template call block template args r-refs info)
	  (emit-template call block template args r-refs))
      
      (move-continuation-result call block results cont)))
  (undefined-value))


;;;; Local call:

;;; IR2-Convert-Let  --  Internal
;;;
;;;    Convert a let by moving the argument values into the variables.  Since a
;;; a let doesn't have any passing locations, we move the arguments directly
;;; into the variables.  We must also allocate any indirect value cells, since
;;; there is no function prologue to do this.
;;;
(defun ir2-convert-let (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
  (mapc #'(lambda (var arg)
	    (when arg
	      (let ((src (continuation-tn node block arg))
		    (dest (leaf-info var)))
		(if (lambda-var-indirect var)
		    (do-make-value-cell node block src dest)
		    (emit-move node block src dest)))))
	(lambda-vars fun) (basic-combination-args node))
  (undefined-value))


;;; EMIT-PSETQ-MOVES  --  Internal
;;;
;;;    Emit any necessary moves into assignment temps for a local call to Fun.
;;; We return two lists of TNs: TNs holding the actual argument values, and
;;; (possibly EQ) TNs that are the actual destination of the arguments.  When
;;; necessary, we allocate temporaries for arguments to preserve paralell
;;; assignment semantics.   These lists exclude unused arguments and include
;;; implicit environment arguments, i.e. they exactly correspond to the
;;; arguments passed.
;;;
;;; OLD-FP is the TN currently holding the value we want to pass as OLD-FP.  If
;;; null, then the call is to the same environment (an :ASSIGNMENT), so we
;;; only move the arguments, and leave the environment alone.
;;;
(defun emit-psetq-moves (node block fun old-fp)
  (declare (type combination node) (type ir2-block block) (type clambda fun)
	   (type (or tn null) old-fp))
  (let* ((called-env (environment-info (lambda-environment fun)))
	 (this-1env (node-environment node))
	 (actuals (mapcar #'(lambda (x)
			     (when x
			       (continuation-tn node block x)))
			 (combination-args node))))
    (collect ((temps)
	      (locs))
      (dolist (var (lambda-vars fun))
	(let ((actual (pop actuals))
	      (loc (leaf-info var)))
	  (when actual
	    (cond
	     ((lambda-var-indirect var)
	      (let ((temp
		     (make-normal-tn (backend-any-primitive-type *backend*))))
		(do-make-value-cell node block actual temp)
		(temps temp)))
	     ((member actual (locs))
	      (let ((temp (make-normal-tn (tn-primitive-type loc))))
		(emit-move node block actual temp)
		(temps temp)))
	     (t
	      (temps actual)))
	    (locs loc))))

      (when old-fp
	(dolist (thing (ir2-environment-environment called-env))
	  (temps (find-in-environment (car thing) this-1env))
	  (locs (cdr thing)))
	
	(temps old-fp)
	(locs (ir2-environment-old-fp called-env)))

      (values (temps) (locs)))))


;;; IR2-Convert-Tail-Local-Call   --  Internal
;;;
;;;    A tail-recursive local call is done by emitting moves of stuff into the
;;; appropriate passing locations.  After setting up the args and environment,
;;; we just move our return-pc into the called function's passing
;;; location.
;;;
(defun ir2-convert-tail-local-call (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
  (let ((this-env (environment-info (node-environment node))))
    (multiple-value-bind
	(temps locs)
	(emit-psetq-moves node block fun (ir2-environment-old-fp this-env))
      
      (mapc #'(lambda (temp loc)
		(emit-move node block temp loc))
	    temps locs))
  
    (emit-move node block
	       (ir2-environment-return-pc this-env)
	       (ir2-environment-return-pc-pass
		(environment-info
		 (lambda-environment fun)))))
  
  (undefined-value))


;;; IR2-CONVERT-ASSIGNMENT  --  Internal
;;;
;;;    Convert an :ASSIGNMENT call.  This is just like a tail local call,
;;; except that the caller and callee environment are the same, so we don't
;;; need to mess with the environment locations, return PC, etc.
;;;
(defun ir2-convert-assignment (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
    (multiple-value-bind
	(temps locs)
	(emit-psetq-moves node block fun nil)
      
      (mapc #'(lambda (temp loc)
		(emit-move node block temp loc))
	    temps locs))
  (undefined-value))


;;; IR2-CONVERT-LOCAL-CALL-ARGS  --  Internal
;;;
;;;    Do stuff to set up the arguments to a non-tail local call (including
;;; implicit environment args.)  We allocate a frame (returning the FP and
;;; NFP), and also compute the TN-Refs list for the values to pass and the list
;;; of passing location TNs.
;;;
(defun ir2-convert-local-call-args (node block fun)
  (declare (type combination node) (type ir2-block block) (type clambda fun))
  (let ((fp (make-stack-pointer-tn))
	(nfp (make-number-stack-pointer-tn))
	(old-fp (make-stack-pointer-tn)))
    (multiple-value-bind (temps locs)
			 (emit-psetq-moves node block fun old-fp)
      (vop current-fp node block old-fp)
      (vop allocate-frame node block
	   (environment-info (lambda-environment fun))
	   #+sparc
	   (or *always-clear-stack*
	       (and *enable-stack-clearing*
		    (policy node (= speed 3) (>= space 2))))
	   fp nfp)
      (values fp nfp temps (mapcar #'make-alias-tn locs)))))


;;; IR2-Convert-Local-Known-Call  --  Internal
;;;
;;;    Handle a non-TR known-values local call.  We Emit the call, then move
;;; the results to the continuation's destination.
;;;
(defun ir2-convert-local-known-call (node block fun returns cont start)
  (declare (type node node) (type ir2-block block) (type clambda fun)
	   (type return-info returns) (type continuation cont)
	   (type label start))
  (multiple-value-bind (fp nfp temps arg-locs)
		       (ir2-convert-local-call-args node block fun)
    (let ((locs (return-info-locations returns)))
      (vop* known-call-local node block
	    (fp nfp (reference-tn-list temps nil))
	    ((reference-tn-list locs t))
	    arg-locs (environment-info (lambda-environment fun)) start)
      (move-continuation-result node block locs cont)))
  (undefined-value))


;;; IR2-Convert-Local-Unknown-Call  --  Internal
;;;
;;;    Handle a non-TR unknown-values local call.  We do different things
;;; depending on what kind of values the continuation wants.
;;;
;;;    If Cont is :Unknown, then we use the "Multiple-" variant, directly
;;; specifying the continuation's Locs as the VOP results so that we don't have
;;; to do anything after the call.
;;;
;;;    Otherwise, we use Standard-Result-Tns to get wired result TNs, and
;;; then call Move-Continuation-Result to do any necessary type checks or
;;; coercions.
;;;
(defun ir2-convert-local-unknown-call (node block fun cont start)
  (declare (type node node) (type ir2-block block) (type clambda fun)
	   (type continuation cont) (type label start))
  (multiple-value-bind (fp nfp temps arg-locs)
		       (ir2-convert-local-call-args node block fun)
    (let ((2cont (continuation-info cont))
	  (env (environment-info (lambda-environment fun)))
	  (temp-refs (reference-tn-list temps nil)))
      (if (and 2cont (eq (ir2-continuation-kind 2cont) :unknown))
	  (vop* multiple-call-local node block (fp nfp temp-refs)
		((reference-tn-list (ir2-continuation-locs 2cont) t))
		arg-locs env start)
	  (let ((locs (standard-result-tns cont)))
	    (vop* call-local node block
		  (fp nfp temp-refs)
		  ((reference-tn-list locs t))
		  arg-locs env start (length locs))
	    (move-continuation-result node block locs cont)))))
  (undefined-value))


;;; IR2-Convert-Local-Call  --  Internal
;;;
;;;    Dispatch to the appropriate function, depending on whether we have a
;;; let, tail or normal call.  If the function doesn't return, call it using
;;; the unknown-value convention.  We could compile it as a tail call, but that
;;; might seem confusing in the debugger.
;;;
(defun ir2-convert-local-call (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((fun (ref-leaf (continuation-use (basic-combination-fun node))))
	 (kind (functional-kind fun)))
    (cond ((eq kind :let)
	   (ir2-convert-let node block fun))
	  ((eq kind :assignment)
	   (ir2-convert-assignment node block fun))
	  ((node-tail-p node)
	   (ir2-convert-tail-local-call node block fun))
	  (t
	   (let ((start (block-label (node-block (lambda-bind fun))))
		 (returns (tail-set-info (lambda-tail-set fun)))
		 (cont (node-cont node)))
	     (ecase (if returns
			(return-info-kind returns)
			:unknown)
	       (:unknown
		(ir2-convert-local-unknown-call node block fun cont start))
	       (:fixed
		(ir2-convert-local-known-call node block fun returns
					      cont start)))))))
  (undefined-value))


;;;; Full call:


;;; Function-Continuation-TN  --  Internal
;;;
;;; Given a function continuation Fun, return as values a TN holding the
;;; thing that we call and true if the thing is named (false if it is a
;;; function).  There are two interesting non-named cases:
;;; -- Known to be a function, no check needed: return the continuation loc.
;;; -- Not known what it is.
;;;
(defun function-continuation-tn (node block cont)
  (declare (type continuation cont))
  (let ((2cont (continuation-info cont)))
    (if (eq (ir2-continuation-kind 2cont) :delayed)
	(let ((name (continuation-function-name cont t)))
	  (assert name)
	  (values (make-load-time-constant-tn :fdefinition name) t))
	(let* ((locs (ir2-continuation-locs 2cont))
	       (loc (first locs))
	       (check (continuation-type-check cont))
	       (function-ptype (primitive-type-or-lose 'function *backend*)))
	  (assert (and (eq (ir2-continuation-kind 2cont) :fixed)
		       (= (length locs) 1)))
	  (cond ((eq (tn-primitive-type loc) function-ptype)
		 (assert (not (eq check t)))
		 (values loc nil))
		(t
		 (let ((temp (make-normal-tn function-ptype)))
		   (assert (and (eq (ir2-continuation-primitive-type 2cont)
				    function-ptype)
				(eq check t)))
		   (emit-type-check node block loc temp
				    (specifier-type 'function))
		   (values temp nil))))))))


;;; MOVE-TAIL-FULL-CALL-ARGS  --  Internal
;;;
;;;    Set up the args to Node in the current frame, and return a tn-ref list
;;; for the passing locations.
;;;
(defun move-tail-full-call-args (node block)
  (declare (type combination node) (type ir2-block block))
  (let ((args (basic-combination-args node))
	(last nil)
	(first nil))
    (dotimes (num (length args))
      (let ((loc (standard-argument-location num)))
	(emit-move node block (continuation-tn node block (elt args num)) loc)
	(let ((ref (reference-tn loc nil)))
	  (if last
	      (setf (tn-ref-across last) ref)
	      (setf first ref))
	  (setq last ref))))
      first))


;;; IR2-Convert-Tail-Full-Call  --  Internal
;;;
;;;    Move the arguments into the passing locations and do a (possibly named)
;;; tail call.
;;;
(defun ir2-convert-tail-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((env (environment-info (node-environment node)))
	 (args (basic-combination-args node))
	 (nargs (length args))
	 (pass-refs (move-tail-full-call-args node block))
	 (old-fp (ir2-environment-old-fp env))
	 (return-pc (ir2-environment-return-pc env)))

    (multiple-value-bind
	(fun-tn named)
	(function-continuation-tn node block (basic-combination-fun node))
      (if named
	  (vop* tail-call-named node block
		(fun-tn old-fp return-pc pass-refs)
		(nil)
		nargs)
	  (vop* tail-call node block
		(fun-tn old-fp return-pc pass-refs)
		(nil)
		nargs))))

  (undefined-value))


;;; IR2-CONVERT-FULL-CALL-ARGS  --  Internal
;;;
;;;    Like IR2-CONVERT-LOCAL-CALL-ARGS, only different.
;;;
(defun ir2-convert-full-call-args (node block)
  (declare (type combination node) (type ir2-block block))
  (let* ((args (basic-combination-args node))
	 (fp (make-stack-pointer-tn))
	 (nargs (length args)))
    (vop allocate-full-call-frame node block nargs fp)
    (collect ((locs))
      (let ((last nil)
	    (first nil))
	(dotimes (num nargs)
	  (locs (standard-argument-location num))
	  (let ((ref (reference-tn (continuation-tn node block (elt args num))
				   nil)))
	    (if last
		(setf (tn-ref-across last) ref)
		(setf first ref))
	    (setq last ref)))
	
	(values fp first (locs) nargs)))))


;;; IR2-Convert-Fixed-Full-Call  --  Internal
;;;
;;;    Do full call when a fixed number of values are desired.  We make
;;; Standard-Result-TNs for our continuation, then deliver the result using
;;; Move-Continuation-Result.  We do named or normal call, as appropriate.
;;;
(defun ir2-convert-fixed-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (multiple-value-bind (fp args arg-locs nargs)
		       (ir2-convert-full-call-args node block)
    (let* ((cont (node-cont node))
	   (locs (standard-result-tns cont))
	   (loc-refs (reference-tn-list locs t))
	   (nvals (length locs)))
      (multiple-value-bind
	  (fun-tn named)
	  (function-continuation-tn node block (basic-combination-fun node))
	(if named
	    (vop* call-named node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs nvals)
	    (vop* call node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs nvals))
	(move-continuation-result node block locs cont))))
  (undefined-value))


;;; IR2-Convert-Multiple-Full-Call  --  Internal
;;;
;;;    Do full call when unknown values are desired.
;;;
(defun ir2-convert-multiple-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (multiple-value-bind (fp args arg-locs nargs)
		       (ir2-convert-full-call-args node block)
    (let* ((cont (node-cont node))
	   (locs (ir2-continuation-locs (continuation-info cont)))
	   (loc-refs (reference-tn-list locs t)))
      (multiple-value-bind
	  (fun-tn named)
	  (function-continuation-tn node block (basic-combination-fun node))
	(if named
	    (vop* multiple-call-named node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs)
	    (vop* multiple-call node block (fp fun-tn args) (loc-refs)
		  arg-locs nargs)))))
  (undefined-value))


;;; IR2-Convert-Full-Call  --  Internal
;;;
;;;    If the call is in a TR position and the return convention is standard,
;;; then do a tail full call.  If one or fewer values are desired, then use a
;;; single-value call, otherwise use a multiple-values call.
;;;
(defun ir2-convert-full-call (node block)
  (declare (type combination node) (type ir2-block block))
  (let ((2cont (continuation-info (node-cont node))))
    (cond ((node-tail-p node)
	   (ir2-convert-tail-full-call node block))
	  ((and 2cont
		(eq (ir2-continuation-kind 2cont) :unknown))
	   (ir2-convert-multiple-full-call node block))
	  (t
	   (ir2-convert-fixed-full-call node block))))
  (undefined-value))


;;;; Function entry:

;;; Init-XEP-Environment  --  Internal
;;;
;;;    Do all the stuff that needs to be done on XEP entry:
;;; -- Create frame
;;; -- Copy any more arg
;;; -- Set up the environment, accessing any closure variables
;;; -- Move args from the standard passing locations to their internal
;;;    locations.
;;; 
(defun init-xep-environment (node block fun)
  (declare (type bind node) (type ir2-block block) (type clambda fun))
  (let ((start-label (entry-info-offset (leaf-info fun)))
	(env (environment-info (node-environment node))))
    (let ((ef (functional-entry-function fun)))
      (cond ((and (optional-dispatch-p ef) (optional-dispatch-more-entry ef))
	     ;; Special case the xep-allocate-frame + copy-more-arg case.
	     (vop xep-allocate-frame node block start-label t
		  #+sparc
		  (or *always-clear-stack*
		      (and *enable-stack-clearing*
			   (policy node (= speed 3) (>= space 2)))))
	     (vop copy-more-arg node block (optional-dispatch-max-args ef)))
	    (t
	     ;; No more args, so normal entry.
	     (vop xep-allocate-frame node block start-label nil
		  #+sparc
		  (or *always-clear-stack*
		      (and *enable-stack-clearing*
			   (policy node (>= space 2) (= speed 3)))))))
      (if (ir2-environment-environment env)
	  (let ((closure
		 (make-normal-tn (backend-any-primitive-type *backend*))))
	    (vop setup-closure-environment node block start-label closure)
	    (when (getf (functional-plist ef) :fin-function)
	      (vop funcallable-instance-lexenv node block closure closure))
	    (let ((n -1))
	      (dolist (loc (ir2-environment-environment env))
		(vop closure-ref node block closure (incf n) (cdr loc)))))
	  (vop setup-environment node block start-label)))
    
    (unless (eq (functional-kind fun) :top-level)
      (let ((vars (lambda-vars fun))
	    (n 0))
	(when (leaf-refs (first vars))
	  (emit-move node block (make-argument-count-location)
		     (leaf-info (first vars))))
	(dolist (arg (rest vars))
	  (when (leaf-refs arg)
	    (let ((pass (standard-argument-location n))
		  (home (leaf-info arg)))
	      (if (lambda-var-indirect arg)
		  (do-make-value-cell node block pass home)
		  (emit-move node block pass home))))
	  (incf n))))
    
    (emit-move node block (make-old-fp-passing-location t)
	       (ir2-environment-old-fp env)))
  
  (undefined-value))


;;; IR2-Convert-Bind  --  Internal
;;;
;;;    Emit function prolog code.  This is only called on bind nodes for
;;; functions that allocate environments.  All semantics of let calls are
;;; handled by IR2-Convert-Let.
;;;
;;;    If not an XEP, all we do is move the return PC from its passing
;;; location, since in a local call, the caller allocates the frame and sets up
;;; the arguments.
;;;
(defun ir2-convert-bind (node block)
  (declare (type bind node) (type ir2-block block))
  (let* ((fun (bind-lambda node))
	 (env (environment-info (lambda-environment fun))))
    (assert (member (functional-kind fun)
		    '(nil :external :optional :top-level :cleanup)))

    (when (external-entry-point-p fun)
      (init-xep-environment node block fun)
      (when *collect-dynamic-statistics*
	(vop count-me node block *dynamic-counts-tn*
	     (block-number (ir2-block-block block)))))

    (emit-move node block (ir2-environment-return-pc-pass env)
	       (ir2-environment-return-pc env))

    (let ((lab (gen-label)))
      (setf (ir2-environment-environment-start env) lab)
      (vop note-environment-start node block lab)))
  
  (undefined-value))


;;;; Function return:

;;; IR2-Convert-Return  --  Internal
;;;
;;;    Do stuff to return from a function with the specified values and
;;; convention.  If the return convention is :Fixed and we aren't returning
;;; from an XEP, then we do a known return (letting representation selection
;;; insert the correct move-arg VOPs.)  Otherwise, we use the unknown-values
;;; convention.  If there is a fixed number of return values, then use Return,
;;; otherwise use Return-Multiple.
;;;
(defun ir2-convert-return (node block)
  (declare (type creturn node) (type ir2-block block))
  (let* ((cont (return-result node))
	 (2cont (continuation-info cont))
	 (cont-kind (ir2-continuation-kind 2cont))
	 (fun (return-lambda node))
	 (env (environment-info (lambda-environment fun)))
	 (old-fp (ir2-environment-old-fp env))
	 (return-pc (ir2-environment-return-pc env))
	 (returns (tail-set-info (lambda-tail-set fun))))
    (cond
     ((and (eq (return-info-kind returns) :fixed)
	   (not (external-entry-point-p fun)))
      (let ((locs (continuation-tns node block cont
				    (return-info-types returns))))
	(vop* known-return node block
	      (old-fp return-pc (reference-tn-list locs nil))
	      (nil)
	      (return-info-locations returns))))
     ((eq cont-kind :fixed)
      (let* ((types (mapcar #'tn-primitive-type (ir2-continuation-locs 2cont)))
	     (cont-locs (continuation-tns node block cont types))
	     (nvals (length cont-locs))
	     (locs (make-standard-value-tns nvals)))
	(mapc #'(lambda (val loc)
		  (emit-move node block val loc))
	      cont-locs
	      locs)
	(if (= nvals 1)
	    (vop return-single node block old-fp return-pc (car locs))
	    (vop* return node block
		  (old-fp return-pc (reference-tn-list locs nil))
		  (nil)
		  nvals))))
     (t
      (assert (eq cont-kind :unknown))
      (vop* return-multiple node block
	    (old-fp return-pc
		    (reference-tn-list (ir2-continuation-locs 2cont) nil))
	    (nil)))))

  (undefined-value))


;;;; Debugger hooks:

;;; This is used by the debugger to find the top function on the stack.  It
;;; returns the OLD-FP and RETURN-PC for the current function as multiple
;;; values.
;;;
(defoptimizer (kernel:%caller-frame-and-pc ir2-convert) (() node block)
  (let ((env (environment-info (node-environment node))))
    (move-continuation-result node block
			      (list (ir2-environment-old-fp env)
				    (ir2-environment-return-pc env))
			      (node-cont node))))
			    

;;;; Multiple values:

;;; IR2-Convert-MV-Bind  --  Internal
;;;
;;;    Almost identical to IR2-Convert-Let.  Since LTN annotates the
;;; continuation for the correct number of values (with the continuation user
;;; responsible for defaulting), we can just pick them up from the
;;; continuation.
;;;
(defun ir2-convert-mv-bind (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (let* ((cont (first (basic-combination-args node)))
	 (fun (ref-leaf (continuation-use (basic-combination-fun node))))
	 (vars (lambda-vars fun)))
    (assert (eq (functional-kind fun) :mv-let))
    (mapc #'(lambda (src var)
	      (when (leaf-refs var)
		(let ((dest (leaf-info var)))
		  (if (lambda-var-indirect var)
		      (do-make-value-cell node block src dest)
		      (emit-move node block src dest)))))
	  (continuation-tns node block cont
			    (mapcar #'(lambda (x)
					(primitive-type (leaf-type x)))
				    vars))
	  vars))
  (undefined-value))


;;; IR2-Convert-MV-Call  --  Internal
;;;
;;;    Emit the appropriate fixed value, unknown value or tail variant of
;;; Call-Variable.  Note that we only need to pass the values start for the
;;; first argument: all the other argument continuation TNs are ignored.  This
;;; is because we require all of the values globs to be contiguous and on stack
;;; top. 
;;;
(defun ir2-convert-mv-call (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (assert (basic-combination-args node))
  (let* ((start-cont (continuation-info (first (basic-combination-args node))))
	 (start (first (ir2-continuation-locs start-cont)))
	 (tails (and (node-tail-p node)
		     (lambda-tail-set (node-home-lambda node))))
	 (cont (node-cont node))
	 (2cont (continuation-info cont)))
    (multiple-value-bind
	(fun named)
	(function-continuation-tn node block (basic-combination-fun node))
      (assert (and (not named)
		   (eq (ir2-continuation-kind start-cont) :unknown)))
      (cond
       (tails
	(let ((env (environment-info (node-environment node))))
	  (vop tail-call-variable node block start fun
	       (ir2-environment-old-fp env)
	       (ir2-environment-return-pc env))))
       ((and 2cont
	     (eq (ir2-continuation-kind 2cont) :unknown))
	(vop* multiple-call-variable node block (start fun nil)
	      ((reference-tn-list (ir2-continuation-locs 2cont) t))))
       (t
	(let ((locs (standard-result-tns cont)))
	  (vop* call-variable node block (start fun nil)
		((reference-tn-list locs t)) (length locs))
	  (move-continuation-result node block locs cont)))))))


;;; %Pop-Values IR2 convert  --  Internal
;;;
;;;    Reset the stack pointer to the start of the specified unknown-values
;;; continuation (discarding it and all values globs on top of it.)
;;;
(defoptimizer (%pop-values ir2-convert) ((continuation) node block)
  (let ((2cont (continuation-info (continuation-value continuation))))
    (assert (eq (ir2-continuation-kind 2cont) :unknown))
    (vop reset-stack-pointer node block
	 (first (ir2-continuation-locs 2cont)))))


;;; Values IR2 convert  --  Internal
;;;
;;;    Deliver the values TNs to Cont using Move-Continuation-Result.
;;;
(defoptimizer (values ir2-convert) ((&rest values) node block)
  (let ((tns (mapcar #'(lambda (x)
			 (continuation-tn node block x))
		     values)))
    (move-continuation-result node block tns (node-cont node))))


;;; Values-List IR2 convert  --  Internal
;;;
;;;    In the normal case where unknown values are desired, we use the
;;; Values-List VOP.  In the relatively unimportant case of Values-List for a
;;; fixed number of values, we punt by doing a full call to the Values-List
;;; function.  This gets the full call VOP to deal with defaulting any
;;; unsupplied values.  It seems unworthwhile to optimize this case.
;;;
(defoptimizer (values-list ir2-convert) ((list) node block)
  (let* ((cont (node-cont node))
	 (2cont (continuation-info cont)))
    (when 2cont
      (ecase (ir2-continuation-kind 2cont)
	(:fixed (ir2-convert-full-call node block))
	(:unknown
	 (let ((locs (ir2-continuation-locs 2cont)))
	   (vop* values-list node block
		 ((continuation-tn node block list) nil)
		 ((reference-tn-list locs t)))))))))


(defoptimizer (%more-arg-values ir2-convert) ((context start count) node block)
  (let* ((cont (node-cont node))
	 (2cont (continuation-info cont)))
    (when 2cont
      (ecase (ir2-continuation-kind 2cont)
	(:fixed (ir2-convert-full-call node block))
	(:unknown
	 (let ((locs (ir2-continuation-locs 2cont)))
	   (vop* %more-arg-values node block
		 ((continuation-tn node block context)
		  (continuation-tn node block start)
		  (continuation-tn node block count)
		  nil)
		 ((reference-tn-list locs t)))))))))



;;;; Special binding:

;;; %Special-Bind, %Special-Unbind IR2 convert  --  Internal
;;;
;;;    Trivial, given our assumption of a shallow-binding implementation.
;;;
(defoptimizer (%special-bind ir2-convert) ((var value) node block)
  (let ((name (leaf-name (continuation-value var))))
    (vop bind node block (continuation-tn node block value)
	 (emit-constant name))))
;;;
(defoptimizer (%special-unbind ir2-convert) ((var) node block)
  (vop unbind node block))


;;; PROGV IR1 convert  --  Internal
;;;
;;; ### Not clear that this really belongs in this file, or should really be
;;; done this way, but this is the least violation of abstraction in the
;;; current setup.  We don't want to wire shallow-binding assumptions into
;;; IR1tran.
;;;
(def-ir1-translator progv ((vars vals &body body) start cont)
  (ir1-convert
   start cont
   (if (or *converting-for-interpreter* (byte-compiling))
       `(%progv ,vars ,vals #'(lambda () ,@body))
       (once-only ((n-save-bs '(%primitive current-binding-pointer)))
	 `(unwind-protect
	      (progn
		(mapc #'(lambda (var val)
			  (%primitive bind val var))
		      ,vars
		      ,vals)
		,@body)
	    (%primitive unbind-to-here ,n-save-bs))))))


;;;; Non-local exit:

;;; IR2-Convert-Exit  --  Internal
;;;
;;;    Convert a non-local lexical exit.  First find the NLX-Info in our
;;; environment.  Note that this is never called on the escape exits for Catch
;;; and Unwind-Protect, since the escape functions aren't IR2 converted.
;;;
(defun ir2-convert-exit (node block)
  (declare (type exit node) (type ir2-block block))
  (let ((loc (find-in-environment (find-nlx-info (exit-entry node)
						 (node-cont node))
				  (node-environment node)))
	(temp (make-stack-pointer-tn))
	(value (exit-value node)))
    (vop value-cell-ref node block loc temp)
    (if value
	(let ((locs (ir2-continuation-locs (continuation-info value))))
	  (vop unwind node block temp (first locs) (second locs)))
	(let ((0-tn (emit-constant 0)))
	  (vop unwind node block temp 0-tn 0-tn))))

  (undefined-value))


;;; Cleanup-point doesn't to anything except prevent the body from being
;;; entirely deleted.
;;;
(defoptimizer (%cleanup-point ir2-convert) (() node block) node block)

  
;;; This function invalidates a lexical exit on exiting from the dynamic
;;; extent.  This is done by storing 0 into the indirect value cell that holds
;;; the closed unwind block.
;;;
(defoptimizer (%lexical-exit-breakup ir2-convert) ((info) node block)
  (vop value-cell-set node block
       (find-in-environment (continuation-value info) (node-environment node))
       (emit-constant 0)))


;;; IR2-Convert-Throw  --  Internal
;;;
;;;    We have to do a spurious move of no values to the result continuation so
;;; that lifetime analysis won't get confused.
;;;
(defun ir2-convert-throw (node block)
  (declare (type mv-combination node) (type ir2-block block))
  (let ((args (basic-combination-args node)))
    (vop* throw node block
	  ((continuation-tn node block (first args))
	   (reference-tn-list
	    (ir2-continuation-locs (continuation-info (second args)))
	    nil))
	  (nil)))

  (move-continuation-result node block () (node-cont node))
  (undefined-value))


;;; Emit-NLX-Start  --  Internal
;;;
;;;    Emit code to set up a non-local-exit.  Info is the NLX-Info for the
;;; exit, and Tag is the continuation for the catch tag (if any.)  We get at
;;; the target PC by passing in the label to the vop.  The vop is responsible
;;; for building a return-PC object.
;;;
(defun emit-nlx-start (node block info tag)
  (declare (type node node) (type ir2-block block) (type nlx-info info)
	   (type (or continuation null) tag))
  (let* ((2info (nlx-info-info info))
	 (kind (cleanup-kind (nlx-info-cleanup info)))
	 (block-tn (environment-live-tn
		    (make-normal-tn (primitive-type-or-lose 'catch-block
							    *backend*))
		    (node-environment node)))
	 (res (make-stack-pointer-tn))
	 (target-label (ir2-nlx-info-target 2info)))

    (vop current-binding-pointer node block
	 (car (ir2-nlx-info-dynamic-state 2info)))
    (vop* save-dynamic-state node block
	  (nil)
	  ((reference-tn-list (cdr (ir2-nlx-info-dynamic-state 2info)) t)))
    (vop current-stack-pointer node block (ir2-nlx-info-save-sp 2info))

    (ecase kind
      (:catch
       (vop make-catch-block node block block-tn
	    (continuation-tn node block tag) target-label res))
      ((:unwind-protect :block :tagbody)
       (vop make-unwind-block node block block-tn target-label res)))

    (ecase kind
      ((:block :tagbody)
       (do-make-value-cell node block res (ir2-nlx-info-home 2info)))
      (:unwind-protect
       (vop set-unwind-protect node block block-tn))
      (:catch)))

  (undefined-value))


;;; IR2-Convert-Entry  --  Internal
;;;
;;;    Scan each of Entry's exits, setting up the exit for each lexical exit.
;;;
(defun ir2-convert-entry (node block)
  (declare (type entry node) (type ir2-block block))
  (dolist (exit (entry-exits node))
    (let ((info (find-nlx-info node (node-cont exit))))
      (when (and info
		 (member (cleanup-kind (nlx-info-cleanup info))
			 '(:block :tagbody)))
	(emit-nlx-start node block info nil))))
  (undefined-value))


;;; %Catch, %Unwind-Protect IR2 convert  --  Internal
;;;
;;;    Set up the unwind block for these guys.
;;;
(defoptimizer (%catch ir2-convert) ((info-cont tag) node block)
  (emit-nlx-start node block (continuation-value info-cont) tag))
;;;
(defoptimizer (%unwind-protect ir2-convert) ((info-cont cleanup) node block)
  (emit-nlx-start node block (continuation-value info-cont) nil))


;;; %NLX-Entry IR2 convert  --  Internal
;;;
;;; Emit the entry code for a non-local exit.  We receive values and restore
;;; dynamic state.
;;;
;;; In the case of a lexical exit or Catch, we look at the exit continuation's
;;; kind to determine which flavor of entry VOP to emit.  If unknown values,
;;; emit the xxx-MULTIPLE variant to the continuation locs.  If fixed values,
;;; make the appropriate number of temps in the standard values locations and
;;; use the other variant, delivering the temps to the continuation using
;;; Move-Continuation-Result.
;;;
;;; In the Unwind-Protect case, we deliver the first register argument, the
;;; argument count and the argument pointer to our continuation as multiple
;;; values.  These values are the block exited to and the values start and
;;; count.
;;;
;;; After receiving values, we restore dynamic state.  Except in the
;;; Unwind-Protect case, the values receiving restores the stack pointer.  In
;;; an Unwind-Protect cleanup, we want to leave the stack pointer alone, since
;;; the thrown values are still out there.
;;;
(defoptimizer (%nlx-entry ir2-convert) ((info-cont) node block)
  (let* ((info (continuation-value info-cont))
	 (cont (nlx-info-continuation info))
	 (2cont (continuation-info cont))
	 (2info (nlx-info-info info))
	 (top-loc (ir2-nlx-info-save-sp 2info))
	 (start-loc (make-nlx-entry-argument-start-location))
	 (count-loc (make-argument-count-location))
	 (target (ir2-nlx-info-target 2info)))

    (ecase (cleanup-kind (nlx-info-cleanup info))
      ((:catch :block :tagbody)
       (if (and 2cont (eq (ir2-continuation-kind 2cont) :unknown))
	   (vop* nlx-entry-multiple node block
		 (top-loc start-loc count-loc nil)
		 ((reference-tn-list (ir2-continuation-locs 2cont) t))
		 target)
	   (let ((locs (standard-result-tns cont)))
	     (vop* nlx-entry node block
		   (top-loc start-loc count-loc nil)
		   ((reference-tn-list locs t))
		   target
		   (length locs))
	     (move-continuation-result node block locs cont))))
      (:unwind-protect
       (let ((block-loc (standard-argument-location 0)))
	 (vop uwp-entry node block target block-loc start-loc count-loc)
	 (move-continuation-result
	  node block
	  (list block-loc start-loc count-loc)
	  cont))))

    (when *collect-dynamic-statistics*
      (vop count-me node block *dynamic-counts-tn*
	   (block-number (ir2-block-block block))))

    (vop* restore-dynamic-state node block
	  ((reference-tn-list (cdr (ir2-nlx-info-dynamic-state 2info)) nil))
	  (nil))
    (vop unbind-to-here node block
	 (car (ir2-nlx-info-dynamic-state 2info)))))


;;;; N-arg functions:

(macrolet ((frob (name)
	     `(defoptimizer (,name ir2-convert) ((&rest args) node block)
		(let* ((refs (move-tail-full-call-args node block))
		       (cont (node-cont node))
		       (res (continuation-result-tns
			     cont
			     (list (primitive-type (specifier-type 'list))))))
		  (vop* ,name node block (refs) ((first res) nil)
			(length args))
		  (move-continuation-result node block res cont)))))
  (frob list)
  (frob list*))


;;;; Structure accessors:
;;;
;;;    These guys have to bizarrely determine the slot offset by looking at the
;;; called function.

(defoptimizer (%slot-accessor ir2-convert) ((str) node block)
  (let* ((cont (node-cont node))
	 (res (continuation-result-tns cont
				       (list (backend-any-primitive-type
					      *backend*)))))
    (vop instance-ref node block
	 (continuation-tn node block str)
	 (dsd-index
	  (slot-accessor-slot
	   (ref-leaf
	    (continuation-use
	     (combination-fun node)))))
	 (first res))
    (move-continuation-result node block res cont)))

(defoptimizer (%slot-setter ir2-convert) ((value str) node block)
  (let ((val (continuation-tn node block value)))
    (vop instance-set node block
	 (continuation-tn node block str)
	 val
	 (dsd-index
	  (slot-accessor-slot
	   (ref-leaf
	    (continuation-use
	     (combination-fun node))))))
  
    (move-continuation-result node block (list val) (node-cont node))))


;;; IR2-Convert  --  Interface
;;;
;;;    Convert the code in a component into VOPs.
;;;
(defun ir2-convert (component)
  (declare (type component component))
  (let ((*dynamic-counts-tn*
	 (when *collect-dynamic-statistics*
	   (let* ((blocks
		   (block-number (block-next (component-head component))))
		  (counts (make-array blocks
				      :element-type '(unsigned-byte 32)
				      :initial-element 0))
		  (info (make-dyncount-info
			 :for (component-name component)
			 :costs (make-array blocks
					    :element-type '(unsigned-byte 32)
					    :initial-element 0)
			 :counts counts)))
	     (setf (ir2-component-dyncount-info (component-info component))
		   info)
	     (emit-constant info)
	     (emit-constant counts)))))
    (let ((num 0))
      (declare (type index num))
      (do-ir2-blocks (2block component)
	(let ((block (ir2-block-block 2block)))
	  (when (block-start block)
	    (setf (block-number block) num)
	    (when *collect-dynamic-statistics*
	      (let ((first-node (continuation-next (block-start block))))
		(unless (or (and (bind-p first-node)
				 (external-entry-point-p
				  (bind-lambda first-node)))
			    (eq (continuation-function-name
				 (node-cont first-node))
				'%nlx-entry))
		  (vop count-me first-node 2block *dynamic-counts-tn* num))))
	    (ir2-convert-block block)
	    (incf num))))))
  (undefined-value))


;;; Finish-IR2-Block  --  Internal
;;;
;;;    If necessary, emit a terminal unconditional branch to go to the
;;; successor block.  If the successor is the component tail, then there isn't
;;; really any successor, but if the end is an unknown, non-tail call, then we
;;; emit an error trap just in case the function really does return.
;;;
(defun finish-ir2-block (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
	 (last (block-last block))
	 (succ (block-succ block)))
    (unless (if-p last)
      (assert (and succ (null (rest succ))))
      (let ((target (first succ)))
	(cond ((eq target (component-tail (block-component block)))
	       (when (and (basic-combination-p last)
			  (eq (basic-combination-kind last) :full))
		 (let* ((fun (basic-combination-fun last))
			(use (continuation-use fun))
			(name (and (ref-p use) (leaf-name (ref-leaf use)))))
		   (unless (or (node-tail-p last)
			       (info function info name)
			       (policy last (zerop safety)))
		     (vop nil-function-returned-error last 2block
			  (if name
			      (emit-constant name)
			      (multiple-value-bind
				  (tn named)
				  (function-continuation-tn last 2block fun)
				(assert (not named))
				tn)))))))
	      ((not (eq (ir2-block-next 2block) (block-info target)))
	       (vop branch last 2block (block-label target)))))))
  
  (undefined-value))


;;; IR2-Convert-Block  --  Internal
;;;
;;;    Convert the code in a block into VOPs.
;;;
(defun ir2-convert-block (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (do-nodes (node cont block)
      (etypecase node
	(ref
	 (let ((2cont (continuation-info cont)))
	   (when (and 2cont
		      (not (eq (ir2-continuation-kind 2cont) :delayed)))
	     (ir2-convert-ref node 2block))))
	(combination
	 (let ((kind (basic-combination-kind node)))
	   (case kind
	     (:local
	      (ir2-convert-local-call node 2block))
	     (:full
	      (ir2-convert-full-call node 2block))
	     (t
	      (let ((fun (function-info-ir2-convert kind)))
		(cond (fun
		       (funcall fun node 2block))
		      ((eq (basic-combination-info node) :full)
		       (ir2-convert-full-call node 2block))
		      (t
		       (ir2-convert-template node 2block))))))))
	(cif
	 (when (continuation-info (if-test node))
	   (ir2-convert-if node 2block)))
	(bind
	 (let ((fun (bind-lambda node)))
	   (when (eq (lambda-home fun) fun)
	     (ir2-convert-bind node 2block))))
	(creturn
	 (ir2-convert-return node 2block))
	(cset
	 (ir2-convert-set node 2block))
	(mv-combination
	 (cond
	  ((eq (basic-combination-kind node) :local)
	   (ir2-convert-mv-bind node 2block))
	  ((eq (continuation-function-name (basic-combination-fun node))
	       '%throw)
	   (ir2-convert-throw node 2block))
	  (t
	   (ir2-convert-mv-call node 2block))))
	(exit
	 (when (exit-entry node)
	   (ir2-convert-exit node 2block)))
	(entry
	 (ir2-convert-entry node 2block)))))

  (finish-ir2-block block)

  (undefined-value))
