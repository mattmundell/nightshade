;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/array.lisp,v 1.52 1999/06/17 15:18:14 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the MIPS definitions for array operations.
;;;
;;; Written by William Lott
;;; Complex-float support by Douglas Crosher 1998.
;;;
(in-package "MIPS")


;;;; Allocator for the array header.

#-gengc
(define-vop (make-array-header)
  (:policy :fast-safe)
  (:translate make-array-header)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 13
    (inst addu bytes rank (+ (* array-dimensions-offset word-bytes)
			     lowtag-mask))
    (inst li header (lognot lowtag-mask))
    (inst and bytes header)
    (inst addu header rank (fixnum (1- array-dimensions-offset)))
    (inst sll header type-bits)
    (inst or header header type)
    (inst srl header 2)
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn other-pointer-type)
      (storew header result 0 other-pointer-type)
      (inst addu alloc-tn bytes))))

#+gengc
(define-vop (make-array-header)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:sc non-descriptor-reg :offset nl0-offset) nl0)
  (:temporary (:sc non-descriptor-reg :offset nl1-offset) nl1)
  (:temporary (:sc non-descriptor-reg :offset nl2-offset) nl2)
  (:temporary (:sc descriptor-reg :offset a0-offset :target result
	       :from (:argument 0) :to (:result 0))
	      a0)
  (:results (result :scs (descriptor-reg)))
  (:ignore nl2)
  (:generator 13
    (inst addu a0 rank (fixnum (1+ array-dimensions-offset)))
    (inst addu nl1 rank (fixnum (1- array-dimensions-offset)))
    (inst sll nl1 type-bits)
    (inst or nl1 type)
    (inst srl nl1 2)
    (inst jal (make-fixup 'var-alloc :assembly-routine))
    (inst li nl0 other-pointer-type)
    (move result a0)))



;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t index) index
  (flushable))
(defknown lisp::%set-array-dimension (t index index) index
  ())

(define-full-reffer %array-dimension *
  array-dimensions-offset other-pointer-type
  (any-reg) positive-fixnum lisp::%array-dimension)

(define-full-setter %set-array-dimension *
  array-dimensions-offset other-pointer-type
  (any-reg) positive-fixnum lisp::%set-array-dimension #+gengc nil)


(defknown lisp::%array-rank (t) index (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 other-pointer-type)
    (inst sra temp type-bits)
    (inst subu temp (1- array-dimensions-offset))
    (inst sll res temp 2)))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index)))
      (inst sltu temp index bound)
      (inst beq temp zero-tn error)
      (inst nop)
      (move result index))))



;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(eval-when (compile eval)

(defmacro def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-type
       ,(remove-if #'(lambda (x) (member x '(null zero))) scs)
       ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-type ,scs ,element-type
       data-vector-set #+gengc ,(if (member 'descriptor-reg scs) t nil))))

(defmacro def-partial-data-vector-frobs
	  (type element-type size signed &rest scs)
  `(progn
     (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       ,size ,signed vector-data-offset other-pointer-type ,scs
       ,element-type data-vector-ref)
     (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       ,size vector-data-offset other-pointer-type ,scs
       ,element-type data-vector-set)))

); eval-when (compile eval)

(def-full-data-vector-frobs simple-vector *
  descriptor-reg any-reg null zero)

(def-partial-data-vector-frobs simple-string base-char :byte nil
  base-char-reg)

(def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
  :byte nil unsigned-reg signed-reg)

(def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
  :short nil unsigned-reg signed-reg)

(def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
  unsigned-reg)

(def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num
  :byte t signed-reg)

(def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num
  :short t signed-reg)

(def-full-data-vector-frobs simple-array-signed-byte-30 tagged-num
  any-reg)

(def-full-data-vector-frobs simple-array-signed-byte-32 signed-num
  signed-reg)



;;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;; 

(defmacro def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor word-bits bits))
	 (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:note "inline array access")
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs (any-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
	 (:generator 20
	   (inst srl temp index ,bit-shift)
	   (inst sll temp 2)
	   (inst addu lip object temp)
	   (inst lw result lip
		 (- (* vector-data-offset word-bytes)
		    other-pointer-type))
	   (inst and temp index ,(1- elements-per-word))
	   ,@(when (eq (backend-byte-order *target-backend*) :big-endian)
	       `((inst xor temp ,(1- elements-per-word))))
	   ,@(unless (= bits 1)
	       `((inst sll temp ,(1- (integer-length bits)))))
	   (inst srl result temp)
	   (inst and result ,(1- (ash 1 bits)))
	   (inst sll value result 2)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type
		     (:constant
		      (integer 0
			       ,(1- (* (1+ (- (floor (+ #x7fff
							other-pointer-type)
						     word-bytes)
					      vector-data-offset))
				       elements-per-word)))))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     ,@(when (eq (backend-byte-order *target-backend*) :big-endian)
		 `((setf extra (logxor extra (1- ,elements-per-word)))))
	     (loadw result object (+ word vector-data-offset) 
		    other-pointer-type)
	     (unless (zerop extra)
	       (inst srl result (* extra ,bits)))
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg) :target shift)
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:scs (non-descriptor-reg)) temp old)
	 (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
	 (:generator 25
	   (inst srl temp index ,bit-shift)
	   (inst sll temp 2)
	   (inst addu lip object temp)
	   (inst lw old lip
		 (- (* vector-data-offset word-bytes)
		    other-pointer-type))
	   (inst and shift index ,(1- elements-per-word))
	   ,@(when (eq (backend-byte-order *target-backend*) :big-endian)
	       `((inst xor shift ,(1- elements-per-word))))
	   ,@(unless (= bits 1)
	       `((inst sll shift ,(1- (integer-length bits)))))
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst li temp ,(1- (ash 1 bits)))
	     (inst sll temp shift)
	     (inst nor temp temp zero-tn)
	     (inst and old temp))
	   (unless (sc-is value zero)
	     (sc-case value
	       (immediate
		(inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
	       (unsigned-reg
		(inst and temp value ,(1- (ash 1 bits)))))
	     (inst sll temp shift)
	     (inst or old temp))
	   (inst sw old lip
		 (- (* vector-data-offset word-bytes)
		    other-pointer-type))
	   (sc-case value
	     (immediate
	      (inst li result (tn-value value)))
	     (zero
	      (move result zero-tn))
	     (unsigned-reg
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type
		     (:constant
		      (integer 0
			       ,(1- (* (1+ (- (floor (+ #x7fff
							other-pointer-type)
						     word-bytes)
					      vector-data-offset))
				       elements-per-word))))
		     positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp old)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     ,@(when (eq (backend-byte-order *target-backend*) :big-endian)
		 `((setf extra (logxor extra (1- ,elements-per-word)))))
	     (inst lw old object
		   (- (* (+ word vector-data-offset) word-bytes)
		      other-pointer-type))
	     (unless (and (sc-is value immediate)
			  (= (tn-value value) ,(1- (ash 1 bits))))
	       (cond ((= extra ,(1- elements-per-word))
		      (inst sll old ,bits)
		      (inst srl old ,bits))
		     (t
		      (inst li temp
			    (lognot (ash ,(1- (ash 1 bits)) (* extra ,bits))))
		      (inst and old temp))))
	     (sc-case value
	       (zero)
	       (immediate
		(let ((value (ash (logand (tn-value value) ,(1- (ash 1 bits)))
				  (* extra ,bits))))
		  (cond ((< value #x10000)
			 (inst or old value))
			(t
			 (inst li temp value)
			 (inst or old temp)))))
	       (unsigned-reg
		(inst sll temp value (* extra ,bits))
		(inst or old temp)))
	     (inst sw old object
		   (- (* (+ word vector-data-offset) word-bytes)
		      other-pointer-type))
	     (sc-case value
	       (immediate
		(inst li result (tn-value value)))
	       (zero
		(move result zero-tn))
	       (unsigned-reg
		(move result value)))))))))

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)


;;; And the float variants.
;;; 

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst lwc1 value lip
	  (- (* vector-data-offset word-bytes)
	     other-pointer-type))
    (inst nop)))

(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst swc1 value lip
	  (- (* vector-data-offset word-bytes)
	     other-pointer-type))
    (unless (location= result value)
      (inst fmove :single result value))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst addu lip index)
    (ecase (backend-byte-order *backend*)
      (:big-endian
       (inst lwc1 value lip
	     (+ (- (* vector-data-offset word-bytes)
		   other-pointer-type)
		word-bytes))
       (inst lwc1-odd value lip
	     (- (* vector-data-offset word-bytes)
		other-pointer-type)))
      (:little-endian
       (inst lwc1 value lip
	     (- (* vector-data-offset word-bytes)
		other-pointer-type))
       (inst lwc1-odd value lip
	     (+ (- (* vector-data-offset word-bytes)
		   other-pointer-type)
		word-bytes))))
    (inst nop)))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst addu lip object index)
    (inst addu lip index)
    (ecase (backend-byte-order *backend*)
      (:big-endian
       (inst swc1 value lip
	     (+ (- (* vector-data-offset word-bytes)
		   other-pointer-type)
		word-bytes))
       (inst swc1-odd value lip
	     (- (* vector-data-offset word-bytes)
		other-pointer-type)))
      (:little-endian
       (inst swc1 value lip
	     (- (* vector-data-offset word-bytes)
		other-pointer-type))
       (inst swc1-odd value lip
	     (+ (- (* vector-data-offset word-bytes)
		   other-pointer-type)
		word-bytes))))
    (unless (location= result value)
      (inst fmove :double result value))))


;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (inst addu lip object index)
    (inst addu lip index)
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst lwc1 real-tn lip (- (* vector-data-offset word-bytes)
				other-pointer-type)))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst lwc1 imag-tn lip (- (* (1+ vector-data-offset) word-bytes)
				other-pointer-type)))
    (inst nop)))


(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst addu lip object index)
    (inst addu lip index)
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (inst swc1 value-real lip (- (* vector-data-offset word-bytes)
				   other-pointer-type))
      (unless (location= result-real value-real)
	(inst fmove :single result-real value-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst swc1 value-imag lip (- (* (1+ vector-data-offset) word-bytes)
				   other-pointer-type))
      (unless (location= result-imag value-imag)
	(inst fmove :single result-imag value-imag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg) :target shift))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (any-reg) :from (:argument 1)) shift)
  (:generator 6
    (inst sll shift index 2)
    (inst addu lip object shift)
    (let ((real-tn (complex-double-reg-real-tn value)))
      (ld-double real-tn lip (- (* vector-data-offset word-bytes)
				other-pointer-type)))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (ld-double imag-tn lip (- (* (+ vector-data-offset 2) word-bytes)
				other-pointer-type)))
    (inst nop)))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg) :target shift)
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (any-reg) :from (:argument 1)) shift)
  (:generator 6
    (inst sll shift index 2)
    (inst addu lip object shift)  
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (str-double value-real lip (- (* vector-data-offset word-bytes)
				    other-pointer-type))
      (unless (location= result-real value-real)
	(inst fmove :double result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (str-double value-imag lip (- (* (+ vector-data-offset 2) word-bytes)
				    other-pointer-type))
      (unless (location= result-imag value-imag)
	(inst fmove :double result-imag value-imag)))))


;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-32 vector.
;;;
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum single-float))
;;;
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum double-float))

(define-vop (raw-ref-complex-single
	     data-vector-ref/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-single
	     data-vector-set/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-single-float))
;;;
(define-vop (raw-ref-complex-double
	     data-vector-ref/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-double
	     data-vector-set/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-double-float))

;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;; 

(define-full-reffer raw-bits * 0 other-pointer-type (unsigned-reg) unsigned-num
  %raw-bits)
(define-full-setter set-raw-bits * 0 other-pointer-type (unsigned-reg)
  unsigned-num %set-raw-bits #+gengc nil)



;;;; Misc. Array VOPs.

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))

