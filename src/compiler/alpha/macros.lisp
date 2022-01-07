;;; -*- Package: ALPHA; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/alpha/macros.lisp,v 1.3 1998/01/21 22:30:44 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains various useful macros for generating Alpha code.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; Alpha conversion by Sean Hallgren.
;;; 

(in-package :alpha)

;;; Handy macro for defining top-level forms that depend on the compile
;;; environment.

(defmacro expand (expr)
  (let ((gensym (gensym)))
    `(macrolet
	 ((,gensym ()
	    ,expr))
       (,gensym))))


;;; Instruction-like macros.

(defmacro move (src dst)
  "Move SRC into DST unless they are location=."
  (once-only ((src src) (dst dst))
    `(unless (location= ,src ,dst)
       (inst move ,src ,dst))))

(defmacro loadw (result base &optional (offset 0) (lowtag 0))
  (once-only ((result result) (base base))
    `(inst ldl ,result (- (ash ,offset word-shift) ,lowtag) ,base)))

(defmacro loadq (result base &optional (offset 0) (lowtag 0))
  (once-only ((result result) (base base))
    `(inst ldq ,result (- (ash ,offset word-shift) ,lowtag) ,base)))

(defmacro storew (value base &optional (offset 0) (lowtag 0))
  (once-only ((value value) (base base) (offset offset) (lowtag lowtag))
    `(inst stl ,value (- (ash ,offset word-shift) ,lowtag) ,base)))

(defmacro storeq (value base &optional (offset 0) (lowtag 0))
  (once-only ((value value) (base base) (offset offset) (lowtag lowtag))
    `(inst stq ,value (- (ash ,offset word-shift) ,lowtag) ,base)))

(defmacro load-symbol (reg symbol)
  (once-only ((reg reg) (symbol symbol))
    `(inst lda ,reg (static-symbol-offset ,symbol) null-tn)))

(defmacro load-symbol-value (reg symbol)
  `(inst ldl ,reg
	 (+ (static-symbol-offset ',symbol)
	    (ash symbol-value-slot word-shift)
	    (- other-pointer-type))
	 null-tn))

(defmacro store-symbol-value (reg symbol)
  `(inst stl ,reg
	 (+ (static-symbol-offset ',symbol)
	    (ash symbol-value-slot word-shift)
	    (- other-pointer-type))
	 null-tn))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
     `(progn
	(inst ldl ,n-target ,n-offset ,n-source)
	(inst and ,n-target #xff ,n-target))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst lda ,lip (- (ash vm:function-code-offset vm:word-shift)
			     vm:function-pointer-type)
	    ,function)
     (move ,function code-tn)
     (inst jsr zero-tn ,lip 1)))

(defmacro lisp-return (return-pc lip &key (offset 0) (frob-code t))
  "Return to RETURN-PC.  LIP is an interior-reg temporary."
  `(progn
     (inst lda ,lip  
	   (- (* (1+ ,offset) word-bytes) other-pointer-type)
	    ,return-pc)
     ,@(when frob-code
	 `((move ,return-pc code-tn)))
     (inst ret zero-tn ,lip 1)))


(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this
   return-pc."
  `(progn
     (align lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
	 (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (loadw reg cfp-tn offset))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
	 (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (storew reg cfp-tn offset))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg)
	   (move ,n-stack ,n-reg))
	  ((control-stack)
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))

;;; MAYBE-LOAD-STACK-NFP-TN -- Interface
;;;
(defmacro maybe-load-stack-nfp-tn (reg reg-or-stack temp)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
     `(when ,reg
	(sc-case ,n-reg
	 ((any-reg descriptor-reg)
	  (sc-case ,n-stack
	   ((any-reg descriptor-reg)
	    (move ,n-stack ,n-reg))
	   ((control-stack)
	    (loadw ,n-reg cfp-tn (tn-offset ,n-stack))
	    (inst mskll nsp-tn 0 ,temp)
	    (inst bis ,temp ,n-reg ,n-reg))))))))



;;;; Storage allocation:

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
   word header having the specified Type-Code.  The result is placed in
   Result-TN, Flag-Tn must be wired to NL3-OFFSET, and Temp-TN is a non-
   descriptor temp (which may be randomly used by the body.)  The body is
   placed inside the PSEUDO-ATOMIC, and presumably initializes the object."
  `(pseudo-atomic (:extra (pad-data-block ,size))
     (inst bis alloc-tn other-pointer-type ,result-tn)
     (inst li (logior (ash (1- ,size) type-bits) ,type-code) ,temp-tn)
     (storew ,temp-tn ,result-tn 0 other-pointer-type)
     ,@body))



;;;; Error Code


(defvar *adjustable-vectors* nil)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
		   (make-array 16
			       :element-type '(unsigned-byte 8)
			       :fill-pointer 0
			       :adjustable t))))
     (setf (fill-pointer ,var) 0)
     (unwind-protect
	 (progn
	   ,@body)
       (push ,var *adjustable-vectors*))))

(eval-when (compile load eval)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((let ((vop ,vop))
	  (when vop
	    (note-this-location vop :internal-error)))
	(inst gentrap ,kind)
	(with-adjustable-vector (,vector)
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer (make-sc-offset (sc-number
							       (tn-sc tn))
							      (tn-offset tn))
					      ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))
	(align word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     (inst br zero-tn ,label)
     ,@(emit-error-break vop cerror-trap error-code values)))

(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (let ((continue (gensym "CONTINUE-LABEL-"))
	(error (gensym "ERROR-LABEL-")))
    `(let ((,continue (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (let ((,error (gen-label)))
	   (emit-label ,error)
	   (cerror-call ,vop ,continue ,error-code ,@values)
	   ,error)))))


;;; PSEUDO-ATOMIC -- Handy macro for making sequences look atomic.
;;;
(defmacro pseudo-atomic ((&key (extra 0)) &rest forms)
  `(progn
     (inst addq alloc-tn 1 alloc-tn)
     ,@forms
     (inst lda alloc-tn (1- ,extra) alloc-tn)
     (inst stl zero-tn 0 alloc-tn)))



;;;; Memory accessor vop generators

(deftype load/store-index (scale lowtag min-offset
				 &optional (max-offset min-offset))
  `(integer ,(- (truncate (+ (ash 1 16)
			     (* min-offset word-bytes)
			     (- lowtag))
			  scale))
	    ,(truncate (- (+ (1- (ash 1 16)) lowtag)
			  (* max-offset word-bytes))
		       scale)))

(defmacro define-full-reffer (name type offset lowtag scs el-type
				   &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:temporary (:scs (interior-reg)) lip)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
	 (inst addq object index lip)
	 (inst ldl value (- (* ,offset word-bytes) ,lowtag) lip)
	 ,@(when (equal scs '(unsigned-reg))
	     '((inst mskll value 4 value)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
		   (:constant (load/store-index ,word-bytes ,(eval lowtag)
						,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 4
	 (inst ldl value (- (* (+ ,offset index) word-bytes) ,lowtag)
	       object)
	 ,@(when (equal scs '(unsigned-reg))
	     '((inst mskll value 4 value)))))))

(defmacro define-full-setter (name type offset lowtag scs el-type
				   &optional translate #+gengc (remember t))
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg))
	      (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:temporary (:scs (interior-reg)) lip)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 2
	 (inst addq index object lip)
	 (inst stl value (- (* ,offset word-bytes) ,lowtag) lip)
	 (move value result)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (value :scs ,scs))
       (:info index)
       (:arg-types ,type
		   (:constant (load/store-index ,word-bytes ,(eval lowtag)
						,(eval offset)))
		   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 1
	 (inst stl value (- (* (+ ,offset index) word-bytes) ,lowtag)
	       object)
	 (move value result)))))


(defmacro define-partial-reffer (name type size signed offset lowtag scs
				      el-type &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs ,scs))
	 (:result-types ,el-type)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:sc non-descriptor-reg) temp)
	 (:temporary (:sc non-descriptor-reg) temp1)
	 (:generator 5
	   (inst addq object index lip)
	   ,@(when (eq size :short)
	       '((inst addq index lip lip)))
	   ,@(ecase size
	       (:byte
		(if signed
		    `((inst ldq_u temp (- (* ,offset word-bytes) ,lowtag)
			    lip)
		      (inst lda temp1 (1+ (- (* ,offset word-bytes) ,lowtag))
			    lip)
		      (inst extqh temp temp1 temp)
		      (inst sra temp 56 value))
		    `((inst ldq_u temp (- (* ,offset word-bytes) ,lowtag) lip)
		      (inst lda temp1 (- (* ,offset word-bytes) ,lowtag)
					  lip)
		      (inst extbl temp temp1 value))))
	       (:short
		(if signed
		    `((inst ldq_u temp (- (* ,offset word-bytes) ,lowtag)
			    lip)
		      (inst lda temp1 (- (* ,offset word-bytes) ,lowtag)
			    lip)
		      (inst extwl temp temp1 temp)
		      (inst sll temp 48 temp)
		      (inst sra temp 48 value))
		    `((inst ldq_u temp (- (* ,offset word-bytes) ,lowtag)
			    lip)
		      (inst lda temp1 (- (* ,offset word-bytes) ,lowtag) lip)
		      (inst extwl temp temp1 value)))))))
       (define-vop (,(symbolicate name "-C"))
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:info index)
	 (:arg-types ,type
		     (:constant (load/store-index ,scale
						  ,(eval lowtag)
						  ,(eval offset))))
	 (:results (value :scs ,scs))
	 (:result-types ,el-type)
	 (:temporary (:sc non-descriptor-reg) temp)
	 (:temporary (:sc non-descriptor-reg) temp1)
	 (:generator 5
	   ,@(ecase size
	       (:byte
		(if signed
		    `((inst ldq_u temp (- (+ (* ,offset word-bytes)
					     (* index ,scale)) ,lowtag)
			    object)
		      (inst lda temp1 (1+ (- (+ (* ,offset word-bytes)
						(* index ,scale)) ,lowtag))
			    object)
		      (inst extqh temp temp1 temp)
		      (inst sra temp 56 value))
		    `((inst ldq_u temp (- (+ (* ,offset word-bytes)
					     (* index ,scale)) ,lowtag)
			    object)
		      (inst lda temp1 (- (+ (* ,offset word-bytes)
					    (* index ,scale)) ,lowtag)
			    object)
		      (inst extbl temp temp1 value))))
	       (:short
		(if signed
		    `((inst ldq_u temp (- (+ (* ,offset word-bytes)
					     (* index ,scale)) ,lowtag)
			    object)
		      (inst lda temp1 (- (+ (* ,offset word-bytes)
					    (* index ,scale)) ,lowtag)
			    object)
		      (inst extwl temp temp1 temp)
		      (inst sll temp 48 temp)
		      (inst sra temp 48 value))
		    `((inst ldq_u temp (- (+ (* ,offset word-bytes)
					     (* index ,scale)) ,lowtag)
			    object)
		      (inst lda temp1 (- (+ (* ,offset word-bytes)
					    (* index ,scale)) ,lowtag)
			    object)
		      (inst extwl temp temp1 value))))))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
				      &optional translate)
  (let ((scale (ecase size (:byte 1) (:short 2))))
    `(progn
       (define-vop (,name)
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg))
		(value :scs ,scs :target result))
	 (:arg-types ,type positive-fixnum ,el-type)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:sc non-descriptor-reg) temp)
	 (:temporary (:sc non-descriptor-reg) temp1)
	 (:temporary (:sc non-descriptor-reg) temp2)
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst addq object index lip)
	   ,@(when (eq size :short)
	       '((inst addq lip index lip)))
	   ,@(ecase size
	       (:byte
		`((inst lda temp (- (* ,offset word-bytes) ,lowtag) lip)
		  (inst ldq_u temp1 (- (* ,offset word-bytes) ,lowtag) lip)
		  (inst insbl value  temp temp2)
		  (inst mskbl temp1 temp temp1)
		  (inst bis temp1 temp2 temp1)
		  (inst stq_u temp1 (- (* ,offset word-bytes) ,lowtag) lip)))
	       (:short
		`((inst lda temp (- (* ,offset word-bytes) ,lowtag) lip)
		  (inst ldq_u temp1 (- (* ,offset word-bytes) ,lowtag) lip)
		  (inst mskwl temp1 temp temp1)
		  (inst inswl value temp temp2)
		  (inst bis temp1 temp2 temp)
		  (inst stq_u temp (- (* ,offset word-bytes) ,lowtag) lip))))
	   (move value result)))
       (define-vop (,(symbolicate name "-C"))
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs ,scs :target result))
	 (:info index)
	 (:arg-types ,type
		     (:constant (load/store-index ,scale
						  ,(eval lowtag)
						  ,(eval offset)))
		     ,el-type)
	 (:temporary (:sc non-descriptor-reg) temp)
	 (:temporary (:sc non-descriptor-reg) temp1)
	 (:temporary (:sc non-descriptor-reg) temp2)
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   ,@(ecase size
	       (:byte
		`((inst lda temp (- (* ,offset word-bytes)
				    (* index ,scale) ,lowtag)
			object)
		  (inst ldq_u temp1 (- (* ,offset word-bytes) 
				       (* index ,scale) ,lowtag)
			object)
		  (inst insbl value temp temp2)
		  (inst mskbl temp1 temp temp1)
		  (inst bis temp1 temp2 temp1)
		  (inst stq_u temp1 (- (* ,offset word-bytes)
				       (* index ,scale) ,lowtag) object)))
	       (:short
		`((inst lda temp (- (* ,offset word-bytes)
				    (* index ,scale) ,lowtag)
			object)
		  (inst ldq_u temp1 (- (* ,offset word-bytes)
				       (* index ,scale) ,lowtag)
			object)
		  (inst mskwl temp1 temp temp1)
		  (inst inswl value temp temp2)
		  (inst bis temp1 temp2 temp)
		  (inst stq_u temp (- (* ,offset word-bytes)
				      (* index ,scale) ,lowtag) object))))
	   (move value result))))))
