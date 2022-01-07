;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/c-call.lisp,v 1.15 2001/01/23 04:40:15 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (register-args 0)
  ;; No matter what we have to allocate at least 7 stack frame slots.  One
  ;; because the C call convention requries it, and 6 because whoever we call
  ;; is going to expect to be able to save his 6 register arguments there.
  (stack-frame-size 7))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-register-args state)))
    (cond ((< reg-args 6)
	   (setf (arg-state-register-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc (+ frame-size 16)))))))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-reg)
	    (values 'unsigned-byte-32 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))
  
(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg 0))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 0))

#+long-float
(def-alien-type-method (long-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'long-float 'long-reg 0))

(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    values)))

(def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-function-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-function-type-result-type type)
	       (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    ;; We need to do something special for the following argument
    ;; types: single-float, double-float, and 64-bit integers.  For
    ;; results, we need something special for 64-bit integer results.
    (if (or (some #'alien-single-float-type-p arg-types)
	    (some #'alien-double-float-type-p arg-types)
	    (some #'(lambda (type)
		      (and (alien-integer-type-p type)
			   (> (alien::alien-integer-type-bits type) 32)))
		  arg-types)
	    #+long-float (some #'alien-long-float-type-p arg-types)
	    (and (alien-integer-type-p result-type)
		 (> (alien::alien-integer-type-bits result-type) 32)))
	(collect ((new-args) (lambda-vars) (new-arg-types))
		 (dolist (type arg-types)
		   (let ((arg (gensym)))
		     (lambda-vars arg)
		     (cond ((and (alien-integer-type-p type)
				 (> (alien::alien-integer-type-bits type) 32))
			    ;; 64-bit long long types are stored in
			    ;; consecutive locations, most significant word
			    ;; first (big-endian).
			    (new-args `(ash ,arg -32))
			    (new-args `(logand ,arg #xffffffff))
			    (if (alien-integer-type-signed type)
				(new-arg-types (parse-alien-type '(signed 32)))
				(new-arg-types (parse-alien-type '(unsigned 32))))
			    (new-arg-types (parse-alien-type '(unsigned 32))))
			   ((alien-single-float-type-p type)
			    (new-args `(single-float-bits ,arg))
			    (new-arg-types (parse-alien-type '(signed 32))))
			   ((alien-double-float-type-p type)
			    (new-args `(double-float-high-bits ,arg))
			    (new-args `(double-float-low-bits ,arg))
			    (new-arg-types (parse-alien-type '(signed 32)))
			    (new-arg-types (parse-alien-type '(unsigned 32))))
			   #+long-float
			   ((alien-long-float-type-p type)
			    (new-args `(long-float-exp-bits ,arg))
			    (new-args `(long-float-high-bits ,arg))
			    (new-args `(long-float-mid-bits ,arg))
			    (new-args `(long-float-low-bits ,arg))
			    (new-arg-types (parse-alien-type '(signed 32)))
			    (new-arg-types (parse-alien-type '(unsigned 32)))
			    (new-arg-types (parse-alien-type '(unsigned 32)))
			    (new-arg-types (parse-alien-type '(unsigned 32))))
			   (t
			    (new-args arg)
			    (new-arg-types type)))))
		 (cond ((and (alien-integer-type-p result-type)
			     (> (alien::alien-integer-type-bits result-type) 32))
			(let ((new-result-type
			       (let ((alien::*values-type-okay* t))
				 (parse-alien-type
				  (if (alien-integer-type-signed result-type)
				      '(values (signed 32) (unsigned 32))
				      '(values (unsigned 32) (unsigned 32)))))))
			  `(lambda (function type ,@(lambda-vars))
			    (declare (ignore type))
			    (multiple-value-bind (high low)
				(%alien-funcall function
						',(make-alien-function-type
						   :arg-types (new-arg-types)
						   :result-type new-result-type)
						,@(new-args))
			      (logior low (ash high 32))))))
		       (t
			`(lambda (function type ,@(lambda-vars))
			  (declare (ignore type))
			  (%alien-funcall function
			   ',(make-alien-function-type
			      :arg-types (new-arg-types)
			      :result-type result-type)
			   ,@(new-args))))))
	(c::give-up))))


(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign))))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:scs (any-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (move cfunc function)
      (inst li temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst jal lip temp)
      (inst nop)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
	(cond ((< delta (ash 1 12))
	       (inst sub nsp-tn delta))
	      (t
	       (inst li temp delta)
	       (inst sub nsp-tn temp)))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result tn was allocated by
      ;; make-call-out-tns above, which takes the number-stack-displacement
      ;; into account itself.
      (inst add result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
	(cond ((< delta (ash 1 12))
	       (inst add nsp-tn delta))
	      (t
	       (inst li temp delta)
	       (inst add nsp-tn temp)))))))
