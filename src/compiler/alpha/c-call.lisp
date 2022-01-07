;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/compiler/alpha/c-call.lisp,v 1.2 1994/10/31 04:39:51 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;; Converted by Sean Hallgren.
;;;
(in-package "ALPHA")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (stack-frame-size 0))

(def-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind
	(ptype reg-sc stack-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-64 'signed-reg 'signed-stack)
	    (values 'unsigned-byte-64 'unsigned-reg 'unsigned-stack))
      (if (< stack-frame-size 4)
	  (my-make-wired-tn ptype reg-sc (+ stack-frame-size nl0-offset))
	  (my-make-wired-tn ptype stack-sc (* 2 (- stack-frame-size 4)))))))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 4)
	(my-make-wired-tn 'system-area-pointer
			  'sap-reg
			  (+ stack-frame-size nl0-offset))
	(my-make-wired-tn 'system-area-pointer
			  'sap-stack
			  (* 2 (- stack-frame-size 4))))))

(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 6)
	(my-make-wired-tn 'double-float
			  'double-reg
			  (+ stack-frame-size nl0-offset))
	(my-make-wired-tn 'double-float
			  'double-stack
			  (* 2 (- stack-frame-size 6))))))

(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (if (< stack-frame-size 6)
	(my-make-wired-tn 'single-float
			  'single-reg
			  (+ stack-frame-size nl0-offset))
	(my-make-wired-tn 'single-float
			  'single-stack
			  (* 2 (- stack-frame-size 6))))))



(def-alien-type-method (integer :result-tn) (type state)
  (declare (ignore state))
  (multiple-value-bind
      (ptype reg-sc)
      (if (alien-integer-type-signed type)
	  (values 'signed-byte-64 'signed-reg)
	  (values 'unsigned-byte-64 'unsigned-reg))
    (my-make-wired-tn ptype reg-sc lip-offset)))

(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'system-area-pointer 'sap-reg lip-offset))
    
(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg lip-offset))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg lip-offset))

(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (cdr values)
      (error "Too many result values from c-call."))
    (when values
      (invoke-alien-type-method :result-tn (car values) state))))

(def-vm-support-routine make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
	      (* (max (arg-state-stack-frame-size arg-state) 4) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method :result-tn
					(alien-function-type-result-type type)
					nil)))))


(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li (make-fixup foreign-symbol :foreign) res)))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (move function cfunc)
      (inst li (make-fixup "call_into_c" :foreign) temp)
      (inst jsr lip-tn temp (make-fixup "call_into_c" :foreign))
      (when cur-nfp
	(maybe-load-stack-nfp-tn cur-nfp nfp-save temp)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
	(cond ((< delta (ash 1 15))
	       (inst lda nsp-tn (- delta) nsp-tn))
	      (t
	       (inst li delta temp)
	       (inst subq nsp-tn temp nsp-tn)))))
    (move nsp-tn result)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
	(cond ((< delta (ash 1 15))
	       (inst lda nsp-tn delta nsp-tn))
	      (t
	       (inst li delta temp)
	       (inst addq nsp-tn temp nsp-tn)))))))
