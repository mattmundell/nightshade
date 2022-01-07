;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/compiler/mips/macros.lisp,v 1.51 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains various useful macros for generating MIPS code.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 

(in-package "MIPS")

;;; Handy macro for defining top-level forms that depend on the compile
;;; environment.

(defmacro expand (expr)
  (let ((gensym (gensym)))
    `(macrolet
	 ((,gensym ()
	    ,expr))
       (,gensym))))


;;; Instruction-like macros.

(defmacro move (dst src &optional (always-emit-code-p nil))
  "Move SRC into DST (unless they are location= and ALWAYS-EMIT-CODE-P
  is nil)."
  (once-only ((n-dst dst)
	      (n-src src))
    (if always-emit-code-p
	`(inst move ,n-dst ,n-src)
	`(unless (location= ,n-dst ,n-src)
	   (inst move ,n-dst ,n-src)))))

(defmacro def-mem-op (op inst shift load)
  `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
     `(progn
	(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag))
	,,@(when load '('(inst nop))))))
;;; 
(def-mem-op loadw lw word-shift t)
(def-mem-op storew sw word-shift nil)
#+gengc
(def-mem-op storew-and-remember-slot sw-and-remember-slot word-shift nil)
#+gengc
(def-mem-op storew-and-remember-object sw-and-remember-object word-shift nil)

(defmacro load-symbol (reg symbol)
  `(inst addu ,reg null-tn (static-symbol-offset ,symbol)))

#-gengc
(defmacro load-symbol-value (reg symbol)
  `(progn
     (inst lw ,reg null-tn
	   (+ (static-symbol-offset ',symbol)
	      (ash symbol-value-slot word-shift)
	      (- other-pointer-type)))
     (inst nop)))
#-gengc
(defmacro store-symbol-value (reg symbol)
  `(inst sw ,reg null-tn
	 (+ (static-symbol-offset ',symbol)
	    (ash symbol-value-slot word-shift)
	    (- other-pointer-type))))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *target-backend*)
      (:little-endian
       `(inst lbu ,n-target ,n-source ,n-offset ))
      (:big-endian
       `(inst lbu ,n-target ,n-source (+ ,n-offset 3))))))


;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

#-gengc
(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst addu ,lip ,function (- (ash vm:function-code-offset vm:word-shift)
				   vm:function-pointer-type))
     (inst j ,lip)
     (move code-tn ,function)))

#-gengc
(defmacro lisp-return (return-pc lip &key (offset 0) (frob-code t))
  "Return to RETURN-PC.  LIP is an interior-reg temporary."
  `(progn
     (inst addu ,lip ,return-pc
	   (- (* (1+ ,offset) word-bytes) other-pointer-type))
     (inst j ,lip)
     ,(if frob-code
	  `(move code-tn ,return-pc)
	  '(inst nop))))


(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     #-gengc
     (align lowtag-bits)
     (emit-label ,label)
     #-gengc
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
	   (move ,n-reg ,n-stack))
	  ((control-stack)
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

#-gengc
(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
   word header having the specified Type-Code.  The result is placed in
   Result-TN, Flag-Tn must be wired to NL3-OFFSET, and Temp-TN is a non-
   descriptor temp (which may be randomly used by the body.)  The body is
   placed inside the PSEUDO-ATOMIC, and presumably initializes the object."
  `(pseudo-atomic (,flag-tn :extra (pad-data-block ,size))
     (inst or ,result-tn alloc-tn other-pointer-type)
     (inst li ,temp-tn (logior (ash (1- ,size) type-bits) ,type-code))
     (storew ,temp-tn ,result-tn 0 other-pointer-type)
     ,@body))

#+gengc
(defun fixed-allocate (result temp type-code size)
  (assemble ()
    (without-scheduling ()
      (inst li temp (logior (ash (1- size) type-bits) type-code))
      (inst or result alloc-tn other-pointer-type)
      (inst sw temp alloc-tn)
      (inst addu alloc-tn alloc-tn (pad-data-block size)))))

#+gengc
(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code size)
				 &body body)
  (declare (ignore flag-tn))
  `(progn
     (fixed-allocate ,result-tn ,temp-tn ,type-code ,size)
     ,@body))


;;;; Three Way Comparison

(defun three-way-comparison (x y condition flavor not-p target temp)
  (ecase condition
    (:eq
     (if not-p
	 (inst bne x y target)
	 (inst beq x y target)))
    (:lt
     (ecase flavor
       (:unsigned
	(inst sltu temp x y))
       (:signed
	(inst slt temp x y)))
     (if not-p
	 (inst beq temp zero-tn target)
	 (inst bne temp zero-tn target)))
    (:gt
     (ecase flavor
       (:unsigned
	(inst sltu temp y x))
       (:signed
	(inst slt temp y x)))
     (if not-p
	 (inst beq temp zero-tn target)
	 (inst bne temp zero-tn target))))
  (inst nop))



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
	(inst break ,kind)
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
     (inst b ,label)
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
(defmacro pseudo-atomic ((flag-tn &key (extra 0)) &rest forms)
  #+gengc
  (unless (eql extra 0)
    (error "Can't allocate any extra with pseudo-atomic in the gengc system."))
  `(progn
     (assert (= (tn-offset ,flag-tn) nl4-offset))
     (without-scheduling ()
       (inst li ,flag-tn #-gengc (1- ,extra) #+gengc -1)
       (inst addu alloc-tn 1))
     ,@forms
     (without-scheduling ()
       (inst add alloc-tn ,flag-tn))))



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
	 (inst add lip object index)
	 (inst lw value lip (- (* ,offset word-bytes) ,lowtag))
	 (inst nop)))
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
	 (inst lw value object (- (* (+ ,offset index) word-bytes) ,lowtag))
	 (inst nop)))))

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
	 (inst add lip object index)
	 (inst #+gengc ,(if remember 'sw-and-remember-slot 'sw) #-gengc sw
	       value lip (- (* ,offset word-bytes) ,lowtag))
	 (move result value)))
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
	 (inst #+gengc ,(if remember 'sw-and-remember-slot 'sw) #-gengc sw
	       value object (- (* (+ ,offset index) word-bytes) ,lowtag))
	 (move result value)))))


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
	 (:generator 5
	   (inst addu lip object index)
	   ,@(when (eq size :short)
	       '((inst addu lip index)))
	   (inst ,(ecase size
		    (:byte (if signed 'lb 'lbu))
		    (:short (if signed 'lh 'lhu)))
		 value lip (- (* ,offset word-bytes) ,lowtag))
	   (inst nop)))
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
	 (:generator 5
	   (inst ,(ecase size
		    (:byte (if signed 'lb 'lbu))
		    (:short (if signed 'lh 'lhu)))
		 value object
		 (- (+ (* ,offset word-bytes) (* index ,scale)) ,lowtag))
	   (inst nop))))))

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
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst addu lip object index)
	   ,@(when (eq size :short)
	       '((inst addu lip index)))
	   (inst ,(ecase size (:byte 'sb) (:short 'sh))
		 value lip (- (* ,offset word-bytes) ,lowtag))
	   (move result value)))
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
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst ,(ecase size (:byte 'sb) (:short 'sh))
		 value object
		 (- (* ,offset word-bytes) (* index ,scale) ,lowtag))
	   (move result value))))))

