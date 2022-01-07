;;; The x86 definitions for array operations.

(in-package "X86")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :to :eval) bytes)
  (:temporary (:sc any-reg :to :result) header)
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 13
    (inst lea bytes
	  (make-ea :dword :base rank
		   :disp (+ (* (1+ array-dimensions-offset) word-bytes)
			    lowtag-mask)))
    (inst and bytes (lognot lowtag-mask))
    (inst lea header (make-ea :dword :base rank
			      :disp (fixnum (1- array-dimensions-offset))))
    (inst shl header type-bits)
    (inst or  header type)
    (inst shr header 2)
    (pseudo-atomic
     (allocation result bytes node)
     (inst lea result (make-ea :dword :base result :disp other-pointer-type))
     (storew header result 0 other-pointer-type))))


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
    (loadw res x 0 other-pointer-type)
    (inst shr res type-bits)
    (inst sub res (1- array-dimensions-offset))))


;;;; Bounds checking routine.

;;; Note that the immediate SC for the index argument is disabled
;;; because it is not possible to generate a valid error code SC for
;;; an immediate value.
(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg #+nil immediate) :target result))
  (:arg-types * positive-fixnum tagged-num)
  (:results (result :scs (any-reg descriptor-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index))
	  (index (if (sc-is index immediate) (fixnum (tn-value index)) index)))
      (inst cmp bound index)
      ;; We use below-or-equal even though it's an unsigned test, because
      ;; negative indexes appear as real large unsigned numbers.  Therefore,
      ;; we get the < 0 and >= bound test all rolled into one.
      (inst jmp :be error)
      (unless (and (tn-p index) (location= result index))
        (inst mov result index)))))


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

); eval-when (compile eval)

(def-full-data-vector-frobs simple-vector * descriptor-reg any-reg)
(def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
  unsigned-reg)
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
	 (:temporary (:sc unsigned-reg :offset ecx-offset) ecx)
	 (:generator 20
	   (move ecx index)
	   (inst shr ecx ,bit-shift)
	   (inst mov result
		 (make-ea :dword :base object :index ecx :scale 4
			  :disp (- (* vector-data-offset word-bytes)
				   other-pointer-type)))
	   (move ecx index)
	   (inst and ecx ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst shl ecx ,(1- (integer-length bits)))))
	   (inst shr result :cl)
	   (inst and result ,(1- (ash 1 bits)))))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (loadw result object (+ word vector-data-offset)
		    other-pointer-type)
	     (unless (zerop extra)
	       (inst shr result (* extra ,bits)))
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg) :target ptr)
		(index :scs (unsigned-reg) :target ecx)
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc unsigned-reg) word-index)
	 (:temporary (:sc unsigned-reg :from (:argument 0)) ptr old)
	 (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1))
		     ecx)
	 (:generator 25
	   (move word-index index)
	   (inst shr word-index ,bit-shift)
	   (inst lea ptr
		 (make-ea :dword :base object :index word-index :scale 4
			  :disp (- (* vector-data-offset word-bytes)
				   other-pointer-type)))
	   (loadw old ptr)
	   (move ecx index)
	   (inst and ecx ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst shl ecx ,(1- (integer-length bits)))))
	   (inst ror old :cl)
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst and old ,(lognot (1- (ash 1 bits)))))
	   (sc-case value
	     (immediate
	      (unless (zerop (tn-value value))
		(inst or old (logand (tn-value value) ,(1- (ash 1 bits))))))
	     (unsigned-reg
	      (inst or old value)))
	   (inst rol old :cl)
	   (storew old ptr)
	   (sc-case value
	     (immediate
	      (inst mov result (tn-value value)))
	     (unsigned-reg
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type (:constant index) positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:sc unsigned-reg :to (:result 0)) old)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (inst mov old
		   (make-ea :dword :base object
			    :disp (- (* (+ word vector-data-offset) word-bytes)
				     other-pointer-type)))
	     (sc-case value
	       (immediate
		(let* ((value (tn-value value))
		       (mask ,(1- (ash 1 bits)))
		       (shift (* extra ,bits)))
		  (unless (= value mask)
		    (inst and old (lognot (ash mask shift))))
		  (unless (zerop value)
		    (inst or old (ash value shift)))))
	       (unsigned-reg
		(let ((shift (* extra ,bits)))
		  (unless (zerop shift)
		    (inst ror old shift)
		    (inst and old (lognot ,(1- (ash 1 bits))))
		    (inst or old value)
		    (inst rol old shift)))))
	     (inst mov (make-ea :dword :base object
				:disp (- (* (+ word vector-data-offset) word-bytes)
					 other-pointer-type))
		   old)
	     (sc-case value
	       (immediate
		(inst mov result (tn-value value)))
	       (unsigned-reg
		(move result value)))))))))

); eval-when (compile eval)

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)

;;; And the float variants.

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
   (with-empty-tn@fp-top(value)
     (inst fld (make-ea	:dword :base object :index index :scale 1
			:disp (- (* vm:vector-data-offset vm:word-bytes)
				 vm:other-pointer-type))))))

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 30)))
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
   (with-empty-tn@fp-top(value)
     (inst fld (make-ea	:dword :base object
			:disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				    (* 4 index))
				 vm:other-pointer-type))))))

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
  (:generator 5
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base object :index index :scale 1
			      :disp (- (* vm:vector-data-offset vm:word-bytes)
				       vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base object :index index :scale 1
			      :disp (- (* vm:vector-data-offset vm:word-bytes)
				       vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

(define-vop (data-vector-set-c/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (single-reg) :target result))
  (:info index)
  (:arg-types simple-array-single-float (:constant (signed-byte 30))
	      single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base object
			      :disp (- (+ (* vm:vector-data-offset
					     vm:word-bytes)
					  (* 4 index))
				       vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base object
			      :disp (- (+ (* vm:vector-data-offset
					     vm:word-bytes)
					  (* 4 index))
				       vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 7
   (with-empty-tn@fp-top(value)
     (inst fldd (make-ea :dword :base object :index index :scale 2
			 :disp (- (* vm:vector-data-offset vm:word-bytes)
				  vm:other-pointer-type))))))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 30)))
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 6
   (with-empty-tn@fp-top(value)
     (inst fldd (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     (* 8 index))
				  vm:other-pointer-type))))))

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
  (:generator 20
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fstd (make-ea :dword :base object :index index :scale 2
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base object :index index :scale 2
			       :disp (- (* vm:vector-data-offset vm:word-bytes)
					vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))

(define-vop (data-vector-set-c/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (double-reg) :target result))
  (:info index)
  (:arg-types simple-array-double-float (:constant (signed-byte 30))
	      double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 19
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fstd (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset
					      vm:word-bytes)
					   (* 8 index))
					vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base object
			       :disp (- (+ (* vm:vector-data-offset
					      vm:word-bytes)
					   (* 8 index))
					vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))


#+long-float
(define-vop (data-vector-ref/simple-array-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-long-float positive-fixnum)
  (:temporary (:sc any-reg :from :eval :to :result) temp)
  (:results (value :scs (long-reg)))
  (:result-types long-float)
  (:generator 7
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (with-empty-tn@fp-top(value)
      (inst fldl (make-ea :dword :base object :index temp :scale 1
			  :disp (- (* vm:vector-data-offset vm:word-bytes)
				   vm:other-pointer-type))))))

#+long-float
(define-vop (data-vector-ref-c/simple-array-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-long-float (:constant (signed-byte 30)))
  (:results (value :scs (long-reg)))
  (:result-types long-float)
  (:generator 6
   (with-empty-tn@fp-top(value)
     (inst fldl (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     (* 12 index))
				  vm:other-pointer-type))))))

#+long-float
(define-vop (data-vector-set/simple-array-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (long-reg) :target result))
  (:arg-types simple-array-long-float positive-fixnum long-float)
  (:temporary (:sc any-reg :from (:argument 1) :to :result) temp)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:generator 20
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (store-long-float
	    (make-ea :dword :base object :index temp :scale 1
		     :disp (- (* vm:vector-data-offset vm:word-bytes)
			      vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (store-long-float
	    (make-ea :dword :base object :index temp :scale 1
		     :disp (- (* vm:vector-data-offset vm:word-bytes)
			      vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
		    (inst fstd result))
		  (inst fxch value)))))))

#+long-float
(define-vop (data-vector-set-c/simple-array-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (long-reg) :target result))
  (:info index)
  (:arg-types simple-array-long-float (:constant (signed-byte 30)) long-float)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:generator 19
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (store-long-float (make-ea :dword :base object
				      :disp (- (+ (* vm:vector-data-offset
						     vm:word-bytes)
						  (* 12 index))
					       vm:other-pointer-type)))
	   (unless (zerop (tn-offset result))
	     ;; Value is in ST0 but not result.
	     (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (store-long-float (make-ea :dword :base object
				      :disp (- (+ (* vm:vector-data-offset
						     vm:word-bytes)
						  (* 12 index))
					       vm:other-pointer-type)))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
		    (inst fstd result))
		  (inst fxch value)))))))

;;; Complex float variants.
(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fld (make-ea :dword :base object :index index :scale 2
			   :disp (- (* vm:vector-data-offset vm:word-bytes)
				    vm:other-pointer-type)))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fld (make-ea :dword :base object :index index :scale 2
			   :disp (- (* (1+ vm:vector-data-offset)
				       vm:word-bytes)
				    vm:other-pointer-type)))))))

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fld (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 8 index))
				    vm:other-pointer-type)))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fld (make-ea :dword :base object
			   :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				       (* 8 index) 4)
				    vm:other-pointer-type)))))))

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
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fst (make-ea :dword :base object :index index :scale 2
				:disp (- (* vm:vector-data-offset
					    vm:word-bytes)
					 vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fst result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fst (make-ea :dword :base object :index index :scale 2
				:disp (- (* vm:vector-data-offset
					    vm:word-bytes)
					 vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fst value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fst result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fst (make-ea :dword :base object :index index :scale 2
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     4)
				  vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fst result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-set-c/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-single-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-single-float (:constant (signed-byte 30))
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fst (make-ea :dword :base object
				:disp (- (+ (* vm:vector-data-offset
					       vm:word-bytes)
					    (* 8 index))
					 vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fst result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fst (make-ea :dword :base object
				:disp (- (+ (* vm:vector-data-offset
					       vm:word-bytes)
					    (* 8 index))
					 vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fst value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fst result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fst (make-ea :dword :base object
			 :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				     (* 8 index) 4)
				  vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fst result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object :index index :scale 4
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					8)
				     vm:other-pointer-type)))))))

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 6
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 16 index))
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldd (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 16 index) 8)
				     vm:other-pointer-type)))))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object :index index :scale 4
				 :disp (- (* vm:vector-data-offset
					     vm:word-bytes)
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object :index index :scale 4
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      8)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

(define-vop (data-vector-set-c/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-double-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-double-float (:constant (signed-byte 30))
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 19
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 16 index))
					  vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (inst fstd (make-ea :dword :base object
				 :disp (- (+ (* vm:vector-data-offset
						vm:word-bytes)
					     (* 16 index))
					  vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea :dword :base object
			  :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				      (* 16 index) 8)
				   vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

#+long-float
(define-vop (data-vector-ref/simple-array-complex-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-long-float positive-fixnum)
  (:temporary (:sc any-reg :from :eval :to :result) temp)
  (:results (value :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 7
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (let ((real-tn (complex-long-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldl (make-ea :dword :base object :index temp :scale 2
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-long-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldl (make-ea :dword :base object :index temp :scale 2
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					12)
				     vm:other-pointer-type)))))))

#+long-float
(define-vop (data-vector-ref-c/simple-array-complex-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-complex-long-float (:constant (signed-byte 30)))
  (:results (value :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 6
    (let ((real-tn (complex-long-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
	(inst fldl (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 24 index))
				     vm:other-pointer-type)))))
    (let ((imag-tn (complex-long-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
	(inst fldl (make-ea :dword :base object
			    :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
					(* 24 index) 12)
				     vm:other-pointer-type)))))))

#+long-float
(define-vop (data-vector-set/simple-array-complex-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-long-reg) :target result))
  (:arg-types simple-array-complex-long-float positive-fixnum
	      complex-long-float)
  (:temporary (:sc any-reg :from (:argument 1) :to :result) temp)
  (:results (result :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 20
    ;; temp = 3 * index
    (inst lea temp (make-ea :dword :base index :index index :scale 2))
    (let ((value-real (complex-long-reg-real-tn value))
	  (result-real (complex-long-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (store-long-float
	      (make-ea :dword :base object :index temp :scale 2
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (store-long-float
	      (make-ea :dword :base object :index temp :scale 2
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-long-reg-imag-tn value))
	  (result-imag (complex-long-reg-imag-tn result)))
      (inst fxch value-imag)
      (store-long-float
       (make-ea :dword :base object :index temp :scale 2
		:disp (- (+ (* vm:vector-data-offset vm:word-bytes) 12)
			 vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))

#+long-float
(define-vop (data-vector-set-c/simple-array-complex-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (complex-long-reg) :target result))
  (:info index)
  (:arg-types simple-array-complex-long-float (:constant (signed-byte 30))
	      complex-long-float)
  (:results (result :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:generator 19
    (let ((value-real (complex-long-reg-real-tn value))
	  (result-real (complex-long-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
	     ;; Value is in ST0
	     (store-long-float
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 24 index))
				vm:other-pointer-type)))
	     (unless (zerop (tn-offset result-real))
	       ;; Value is in ST0 but not result.
	       (inst fstd result-real)))
	    (t
	     ;; Value is not in ST0.
	     (inst fxch value-real)
	     (store-long-float
	      (make-ea :dword :base object
		       :disp (- (+ (* vm:vector-data-offset vm:word-bytes)
				   (* 24 index))
				vm:other-pointer-type)))
	     (cond ((zerop (tn-offset result-real))
		    ;; The result is in ST0.
		    (inst fstd value-real))
		   (t
		    ;; Neither value or result are in ST0
		    (unless (location= value-real result-real)
		      (inst fstd result-real))
		    (inst fxch value-real))))))
    (let ((value-imag (complex-long-reg-imag-tn value))
	  (result-imag (complex-long-reg-imag-tn result)))
      (inst fxch value-imag)
      (store-long-float
       (make-ea :dword :base object
		:disp (- (+ (* vm:vector-data-offset vm:word-bytes)
			    (* 24 index) 12)
			 vm:other-pointer-type)))
      (unless (location= value-imag result-imag)
	(inst fstd result-imag))
      (inst fxch value-imag))))


;;; unsigned-byte-8

(define-vop (data-vector-ref/simple-array-unsigned-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum)
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (inst movzx value
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-unsigned-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-unsigned-byte-8 (:constant (signed-byte 30)))
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst movzx value
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset word-bytes) index)
			    other-pointer-type)))))

(define-vop (data-vector-set/simple-array-unsigned-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum positive-fixnum)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  al-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-unsigned-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-unsigned-byte-8 (:constant (signed-byte 30))
	      positive-fixnum)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (move eax value)
    (inst mov (make-ea :byte :base object
		       :disp (- (+ (* vector-data-offset word-bytes) index)
				other-pointer-type))
	  al-tn)
    (move result eax)))

;;; unsigned-byte-16

(define-vop (data-vector-ref/simple-array-unsigned-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-unsigned-byte-16 positive-fixnum)
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (inst movzx value
	  (make-ea :word :base object :index index :scale 2
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-unsigned-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-unsigned-byte-16 (:constant (signed-byte 30)))
  (:results (value :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst movzx value
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes) (* 2 index))
			    other-pointer-type)))))

(define-vop (data-vector-set/simple-array-unsigned-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:arg-types simple-array-unsigned-byte-16 positive-fixnum positive-fixnum)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :word :base object :index index :scale 2
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  ax-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-unsigned-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (unsigned-reg signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-unsigned-byte-16 (:constant (signed-byte 30))
	      positive-fixnum)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (unsigned-reg signed-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (move eax value)
    (inst mov (make-ea :word :base object
		       :disp (- (+ (* vector-data-offset word-bytes)
				   (* 2 index))
				other-pointer-type))
	  ax-tn)
    (move result eax)))

;;; simple-string

(define-vop (data-vector-ref/simple-string)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-string positive-fixnum)
  (:temporary (:sc unsigned-reg ; byte-reg
		   :offset eax-offset ; al-offset
		   :target value
		   :from (:eval 0) :to (:result 0))
	      eax)
  (:ignore eax)
  (:results (value :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 5
    (inst mov al-tn
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))
    (move value al-tn)))

(define-vop (data-vector-ref-c/simple-string)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-string (:constant (signed-byte 30)))
  (:temporary (:sc unsigned-reg :offset eax-offset :target value
		   :from (:eval 0) :to (:result 0))
	      eax)
  (:ignore eax)
  (:results (value :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 4
    (inst mov al-tn
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset word-bytes) index)
			    other-pointer-type)))
    (move value al-tn)))

(define-vop (data-vector-set/simple-string)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (base-char-reg)))
  (:arg-types simple-string positive-fixnum base-char)
  (:results (result :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 5
    (inst mov (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  value)
    (move result value)))

(define-vop (data-vector-set/simple-string-c)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (base-char-reg)))
  (:info index)
  (:arg-types simple-string (:constant (signed-byte 30)) base-char)
  (:results (result :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 4
   (inst mov (make-ea :byte :base object
		      :disp (- (+ (* vector-data-offset word-bytes) index)
			       other-pointer-type))
	 value)
   (move result value)))

;;; signed-byte-8

(define-vop (data-vector-ref/simple-array-signed-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-signed-byte-8 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsx value
	  (make-ea :byte :base object :index index :scale 1
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-signed-byte-8)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-signed-byte-8 (:constant (signed-byte 30)))
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (inst movsx value
	  (make-ea :byte :base object
		   :disp (- (+ (* vector-data-offset word-bytes) index)
			    other-pointer-type)))))

(define-vop (data-vector-set/simple-array-signed-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:arg-types simple-array-signed-byte-8 positive-fixnum tagged-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :byte :base object :index index :scale 1
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  al-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-signed-byte-8)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-signed-byte-8 (:constant (signed-byte 30))
	      tagged-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (move eax value)
    (inst mov (make-ea :byte :base object
		       :disp (- (+ (* vector-data-offset word-bytes) index)
				other-pointer-type))
	  al-tn)
    (move result eax)))

;;; signed-byte-16

(define-vop (data-vector-ref/simple-array-signed-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (unsigned-reg)))
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (inst movsx value
	  (make-ea :word :base object :index index :scale 2
		   :disp (- (* vector-data-offset word-bytes)
			    other-pointer-type)))))

(define-vop (data-vector-ref-c/simple-array-signed-byte-16)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types simple-array-signed-byte-16 (:constant (signed-byte 30)))
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (inst movsx value
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes)
			       (* 2 index))
			    other-pointer-type)))))

(define-vop (data-vector-set/simple-array-signed-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (index :scs (unsigned-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target result
		   :from (:argument 2) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 5
    (move eax value)
    (inst mov (make-ea :word :base object :index index :scale 2
		       :disp (- (* vector-data-offset word-bytes)
				other-pointer-type))
	  ax-tn)
    (move result eax)))

(define-vop (data-vector-set-c/simple-array-signed-byte-16)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 0))
	 (value :scs (signed-reg) :target eax))
  (:info index)
  (:arg-types simple-array-signed-byte-16 (:constant (signed-byte 30)) tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target result
		   :from (:argument 1) :to (:result 0))
	      eax)
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num)
  (:generator 4
    (move eax value)
    (inst mov
	  (make-ea :word :base object
		   :disp (- (+ (* vector-data-offset word-bytes)
			       (* 2 index))
			    other-pointer-type))
	  ax-tn)
    (move result eax)))


;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-32 vector.
;;;
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
(define-vop (raw-ref-single-c data-vector-ref-c/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum single-float))
(define-vop (raw-set-single-c data-vector-set-c/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      single-float))
;;;
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
(define-vop (raw-ref-double-c data-vector-ref-c/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum double-float))
(define-vop (raw-set-double-c data-vector-set-c/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      double-float))
;;;
#+long-float
(define-vop (raw-ref-long data-vector-ref/simple-array-long-float)
  (:translate %raw-ref-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
#+long-float
(define-vop (raw-ref-long-c data-vector-ref-c/simple-array-long-float)
  (:translate %raw-ref-long)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
#+long-float
(define-vop (raw-set-double data-vector-set/simple-array-long-float)
  (:translate %raw-set-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum long-float))
#+long-float
(define-vop (raw-set-long-c data-vector-set-c/simple-array-long-float)
  (:translate %raw-set-long)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      long-float))

;;;; Complex-float raw structure slot accessors.

(define-vop (raw-ref-complex-single
	     data-vector-ref/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
(define-vop (raw-ref-complex-single-c
	     data-vector-ref-c/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
(define-vop (raw-set-complex-single
	     data-vector-set/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum complex-single-float))
(define-vop (raw-set-complex-single-c
	     data-vector-set-c/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      complex-single-float))
;;;
(define-vop (raw-ref-complex-double
	     data-vector-ref/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
(define-vop (raw-ref-complex-double-c
	     data-vector-ref-c/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
(define-vop (raw-set-complex-double
	     data-vector-set/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-double-float))
(define-vop (raw-set-complex-double-c
	     data-vector-set-c/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      complex-double-float))
;;;
#+long-float
(define-vop (raw-ref-complex-long
	     data-vector-ref/simple-array-complex-long-float)
  (:translate %raw-ref-complex-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
#+long-float
(define-vop (raw-ref-complex-long-c
	     data-vector-ref-c/simple-array-complex-long-float)
  (:translate %raw-ref-complex-long)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))))
;;;
#+long-float
(define-vop (raw-set-complex-long
	     data-vector-set/simple-array-complex-long-float)
  (:translate %raw-set-complex-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-long-float))
#+long-float
(define-vop (raw-set-complex-long-c
	     data-vector-set-c/simple-array-complex-long-float)
  (:translate %raw-set-complex-long)
  (:arg-types simple-array-unsigned-byte-32 (:constant (signed-byte 30))
	      complex-long-float))

;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;;
(define-full-reffer raw-bits * 0 other-pointer-type (unsigned-reg)
  unsigned-num %raw-bits)
(define-full-setter set-raw-bits * 0 other-pointer-type (unsigned-reg)
  unsigned-num %set-raw-bits)


;;;; Conditional setters.

(export 'kernel::data-vector-set-conditional "KERNEL")
(defknown data-vector-set-conditional (array index t t) t
  (unsafe c::explicit-check))

(define-full-conditional-setter data-vector-set-conditional/simple-vector
  simple-vector vector-data-offset other-pointer-type
  (descriptor-reg any-reg) *
  data-vector-set-conditional)


;;;; Misc. Array VOPs.

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))
