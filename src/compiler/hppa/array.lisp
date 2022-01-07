;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/compiler/hppa/array.lisp,v 1.5.2.2 2000/05/23 16:37:35 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the SPARC definitions for array operations.
;;;
;;; Written by William Lott
;;; Signed-array and Complex-float support by Douglas Crosher 1998.
;;;
(in-package "HPPA")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic ()
      (inst move alloc-tn header)
      (inst dep other-pointer-type 31 3 header)
      (inst addi (* (1+ array-dimensions-offset) word-bytes) rank ndescr)
      (inst dep 0 31 3 ndescr)
      (inst add alloc-tn ndescr alloc-tn)
      (inst addi (fixnum (1- array-dimensions-offset)) rank ndescr)
      (inst sll ndescr type-bits ndescr)
      (inst or ndescr type ndescr)
      (inst srl ndescr 2 ndescr)
      (storew ndescr header 0 vm:other-pointer-type))
    (move header result)))


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
  (any-reg) positive-fixnum lisp::%set-array-dimension)


(defknown lisp::%array-rank (t) index (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:other-pointer-type)
    (inst srl res type-bits res)
    (inst addi (- (1- vm:array-dimensions-offset)) res res)))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index)))
      (inst bc :>= nil index bound error))
    (move index result)))


;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(eval-when (compile eval)

(defmacro def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-type ,scs ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-type ,scs ,element-type
       data-vector-set)))

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

(def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)

(def-partial-data-vector-frobs simple-string base-char :byte nil base-char-reg)

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

(def-full-data-vector-frobs simple-array-signed-byte-30 tagged-num any-reg)

(def-full-data-vector-frobs simple-array-signed-byte-32 signed-num signed-reg)


;;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;; 

(eval-when (compile eval)

(defmacro def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor vm:word-bits bits))
	 (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:note "inline array access")
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (result :scs (unsigned-reg) :from (:argument 0)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp)
	 (:temporary (:scs (interior-reg)) lip)
	 (:generator 20
	   (inst srl index ,bit-shift temp)
	   (inst sh2add temp object lip)
	   (loadw result lip vector-data-offset other-pointer-type)
	   (inst zdep index ,(- 32 (integer-length bits)) ,bit-shift temp)
	   ,@(unless (= bits 1)
	       `((inst addi ,(1- bits) temp temp)))
	   (inst mtctl temp :sar)
	   (inst extru result :variable ,bits result)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       (cond ((typep offset '(signed-byte 14))
		      (inst ldw offset object result))
		     (t
		      (inst ldil (ldb (byte 21 11) offset) temp)
		      (inst ldw (ldb (byte 11 0) offset) temp result))))
	     (inst extru result (+ (* extra ,bits) ,(1- bits)) ,bits result))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg))
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp old)
	 (:temporary (:scs (interior-reg)) lip)
	 (:generator 25
	   (inst srl index ,bit-shift temp)
	   (inst sh2add temp object lip)
	   (loadw old lip vector-data-offset other-pointer-type)
	   (inst zdep index ,(- 32 (integer-length bits)) ,bit-shift temp)
	   ,@(unless (= bits 1)
	       `((inst addi ,(1- bits) temp temp)))
	   (inst mtctl temp :sar)
	   (inst dep (sc-case value (immediate (tn-value value)) (t value))
		 :variable ,bits old)
	   (storew old lip vector-data-offset other-pointer-type)
	   (sc-case value
	     (immediate
	      (inst li (tn-value value) result))
	     (t
	      (move value result)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type
		     (:constant index)
		     positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) old)
	 (:temporary (:scs (interior-reg)) lip)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (let ((offset (- (* (+ word vector-data-offset) word-bytes)
			      other-pointer-type)))
	       (cond ((typep offset '(signed-byte 14))
		      (inst ldw offset object old))
		     (t
		      (inst move object lip)
		      (inst addil (ldb (byte 21 11) offset) lip)
		      (inst ldw (ldb (byte 11 0) offset) lip old)))
	       (inst dep (sc-case value
			   (immediate (tn-value value))
			   (t value))
		     (+ (* extra ,bits) ,(1- bits))
		     ,bits
		     old)
	       (if (typep offset '(signed-byte 14))
		   (inst stw old offset object)
		   (inst stw old (ldb (byte 11 0) offset) lip)))
	     (sc-case value
	       (immediate
		(inst li (tn-value value) result))
	       (t
		(move value result)))))))))

); eval-when (compile eval)

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)

;;; And the float variants.
;;; 

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:argument 1))
	 (index :scs (any-reg) :to (:argument 0) :target offset))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) offset)
  (:result-types single-float)
  (:generator 5
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  index offset)
    (inst fldx offset object value)))

(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:argument 1))
	 (index :scs (any-reg) :to (:argument 0) :target offset)
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) offset)
  (:generator 5
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  index offset)
    (inst fstx value offset object)
    (unless (location= result value)
      (inst funop :copy value result))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:argument 1))
	 (index :scs (any-reg) :to (:argument 0) :target offset))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) offset)
  (:generator 7
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  offset offset)
    (inst fldx offset object value)))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:argument 1))
	 (index :scs (any-reg) :to (:argument 0) :target offset)
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) offset)
  (:generator 20
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  offset offset)
    (inst fstx value offset object)
    (unless (location= result value)
      (inst funop :copy value result))))


;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  offset offset)
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst fldx offset object real-tn))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst addi word-bytes offset offset)
      (inst fldx offset object imag-tn))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (inst sll index 1 offset)
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  offset offset)
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (inst fstx value-real offset object)
      (unless (location= result-real value-real)
	(inst funop :copy value-real result-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst addi word-bytes offset offset)
      (inst fstx value-imag offset object)
      (unless (location= result-imag value-imag)
	(inst funop :copy value-imag result-imag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (inst sll index 2 offset)
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  offset offset)
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst fldx offset object real-tn))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst addi (* 2 word-bytes) offset offset)
      (inst fldx offset object imag-tn))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (inst sll index 2 offset)
    (inst addi (- (* vector-data-offset word-bytes) other-pointer-type)
	  offset offset)
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (inst fstx value-real offset object)
      (unless (location= result-real value-real)
	(inst funop :copy value-real result-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst addi (* 2 word-bytes) offset offset)
      (inst fstx value-imag offset object)
      (unless (location= result-imag value-imag)
	(inst funop :copy value-imag result-imag)))))


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
  unsigned-num %set-raw-bits)



;;;; Misc. Array VOPs.

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))

