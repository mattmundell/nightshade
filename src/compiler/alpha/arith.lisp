;;; VM definition arithmetic VOPs.

(in-package "ALPHA")


;;;; Unary operations.

(define-vop (fixnum-unop)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num)
  (:policy :fast-safe))

(define-vop (signed-unop)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num)
  (:policy :fast-safe))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst subq zero-tn x res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst subq zero-tn x res)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (inst eqv x zero-tn res)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst not x res)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg.

(define-vop (fast-fixnum-binop)
  (:args (x :target r :scs (any-reg))
	 (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-unsigned-binop)
  (:args (x :target r :scs (unsigned-reg))
	 (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-signed-binop)
  (:args (x :target r :scs (signed-reg))
	 (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-fixnum-c-binop fast-fixnum-binop)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num (:constant integer)))

(define-vop (fast-signed-c-binop fast-signed-binop)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types tagged-num (:constant integer)))

(define-vop (fast-unsigned-c-binop fast-unsigned-binop)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types tagged-num (:constant integer)))

(defmacro define-binop (translate cost untagged-cost op
				  tagged-type untagged-type)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
		  fast-fixnum-binop)
       (:args (x :target r :scs (any-reg))
	      (y :target r :scs (any-reg)))
       (:translate ,translate)
       (:generator ,(1+ cost)
	 (inst ,op x y r)))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:args (x :target r :scs (signed-reg))
	      (y :target r :scs (signed-reg)))
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
	 (inst ,op x y r)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:args (x :target r :scs (unsigned-reg))
	      (y :target r :scs (unsigned-reg)))
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
	 (inst ,op x y r)))
     ,@(when tagged-type
	 `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
			fast-fixnum-c-binop)
		       (:arg-types tagged-num (:constant ,tagged-type))
	     (:translate ,translate)
	     (:generator ,cost
			 (inst ,op x (fixnum y) r)))))
     ,@(when untagged-type
	 `((define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
			fast-signed-c-binop)
		       (:arg-types signed-num (:constant ,untagged-type))
	     (:translate ,translate)
	     (:generator ,untagged-cost
			 (inst ,op x y r)))
	   (define-vop (,(symbolicate "FAST-" translate
				      "-C/UNSIGNED=>UNSIGNED")
			fast-unsigned-c-binop)
		       (:arg-types unsigned-num (:constant ,untagged-type))
	     (:translate ,translate)
	     (:generator ,untagged-cost
			 (inst ,op x y r)))))))

(define-binop + 1 5 addq (unsigned-byte 6) (unsigned-byte 8))
(define-binop - 1 5 subq (unsigned-byte 6) (unsigned-byte 8))
(define-binop logior 1 3 bis (unsigned-byte 6) (unsigned-byte 8))
(define-binop lognor 1 3 ornot (unsigned-byte 6) (unsigned-byte 8))
(define-binop logand 1 3 and (unsigned-byte 6) (unsigned-byte 8))
(define-binop logxor 1 3 xor (unsigned-byte 6) (unsigned-byte 8))

;;; Shifting

(define-vop (fast-ash/unsigned=>unsigned)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg) :to :save)
	 (amount :scs (signed-reg)))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:temporary (:sc non-descriptor-reg :to :eval) temp)
  (:generator 3
    (inst bge amount positive)
    (inst subq zero-tn amount ndesc)
    (inst cmplt ndesc 31 temp)
    (inst srl number ndesc result)
    (inst bne temp done)
    (inst srl number 31 result)
    (inst br zero-tn done)

    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll number amount result)

    DONE))

(define-vop (fast-ash/signed=>signed)
  (:note "inline ASH")
  (:args (number :scs (signed-reg) :to :save)
	 (amount :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:temporary (:sc non-descriptor-reg :to :eval) temp)
  (:generator 3
    (inst bge amount positive)
    (inst subq zero-tn amount ndesc)
    (inst cmplt ndesc 31 temp)
    (inst sra number ndesc result)
    (inst bne temp done)
    (inst sra number 31 result)
    (inst br zero-tn done)

    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll number amount result)

    DONE))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:policy :fast-safe)
  (:translate ash)
  (:note nil)
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (cond ((< count 0)
	   ;; It is a right shift.
	   (inst srl number (- count) result))
	  ((> count 0)
	   ;; It is a left shift.
	   (inst sll number count result))
	  (t
	   ;; Count=0?  Shouldn't happen, but it's easy:
	   (move number result)))))

(define-vop (fast-ash-c/signed=>signed)
  (:policy :fast-safe)
  (:translate ash)
  (:note nil)
  (:args (number :scs (signed-reg)))
  (:info count)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (cond ((< count 0)
	   ;; It is a right shift.
	   (inst sra number (- count) result))
	  ((> count 0)
	   ;; It is a left shift.
	   (inst sll number count result))
	  (t
	   ;; Count=0?  Shouldn't happen, but it's easy:
	   (move number result)))))

(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :to (:argument 1)))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (inst not arg shift)
    (inst cmovge arg arg shift)
    (inst subq zero-tn 4 res)
    (inst sll shift 1 shift)
    LOOP
    (inst addq res (fixnum 1) res)
    (inst srl shift 1 shift)
    (inst bne shift loop)))

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target num))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0) :to (:result 0)
		    :target res) num)
  (:temporary (:scs (non-descriptor-reg)) mask temp)
  (:generator 30
    (inst li #x55555555 mask)
    (inst srl arg 1 temp)
    (inst and arg mask num)
    (inst and temp mask temp)
    (inst addq num temp num)
    (inst li #x33333333 mask)
    (inst srl num 2 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst addq num temp num)
    (inst li #x0f0f0f0f mask)
    (inst srl num 4 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst addq num temp num)
    (inst li #x00ff00ff mask)
    (inst srl num 8 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst addq num temp num)
    (inst li #x0000ffff mask)
    (inst srl num 16 temp)
    (inst and num mask num)
    (inst and temp mask temp)
    (inst addq num temp res)))

;;; Multiply

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:generator 4
    (inst sra y 2 temp)
    (inst mulq x temp r)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mulq x y r)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst mulq x y r)))


;;;; Binary conditional VOPs.

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:policy :fast-safe))

(deftype integer-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 1))
	 (let ((bound (ash 1 s)))
	   `(integer 0 ,(- bound bite 1))))
	(t
	 (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (integer-with-a-bite-out 6 4)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (integer-with-a-bite-out 8 1)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (integer-with-a-bite-out 8 1)))
  (:info target not-p y))


(defmacro define-conditional-vop (translate &rest generator)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed)
		   (unless (and (member suffix '(/fixnum -c/fixnum))
				(eq translate 'eql))
		     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
						    translate suffix))
				   ,(intern
				     (format nil "~:@(FAST-CONDITIONAL~A~)"
					     suffix)))
			(:translate ,translate)
			(:generator ,cost
			  (let* ((signed ,signed)
				 (-c/fixnum ,(eq suffix '-c/fixnum))
				 (y (if -c/fixnum (fixnum y) y)))
			    (declare (optimize (inhibit-warnings 3)))
			    ,@generator)))))
	       '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
	       '(3 2 5 4 5 4)
	       '(t t t t nil nil))))

(define-conditional-vop <
  (cond ((and signed (eql y 0))
	 (if not-p
	     (inst bge x target)
	     (inst blt x target)))
	(t
	 (if signed
	     (inst cmplt x y temp)
	     (inst cmpult x y temp))
	 (if not-p
	     (inst beq temp target)
	     (inst bne temp target)))))

(define-conditional-vop >
  (cond ((and signed (eql y 0))
	 (if not-p
	     (inst ble x target)
	     (inst bgt x target)))
	((integerp y)
	 (let ((y (+ y (if -c/fixnum (fixnum 1) 1))))
	   (if signed
	       (inst cmplt x y temp)
	       (inst cmpult x y temp))
	   (if not-p
	       (inst bne temp target)
	       (inst beq temp target))))
	(t
	 (if signed
	     (inst cmplt y x temp)
	     (inst cmpult y x temp))
	 (if not-p
	     (inst beq temp target)
	     (inst bne temp target)))))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

(define-conditional-vop eql
  (declare (ignore signed))
  (when (integerp y)
    (inst li y temp)
    (setf y temp))
  (inst cmpeq x y temp)
  (if not-p
      (inst beq temp target)
      (inst bne temp target)))

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;
(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 3
    (cond ((equal y zero-tn)
	   (if not-p
	       (inst bne x target)
	       (inst beq x target)))
	  (t
	   (inst cmpeq x y temp)
	   (if not-p
	       (inst beq temp target)
	       (inst bne temp target))))))

;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte 6)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (let ((y (cond ((eql y 0) zero-tn)
		   (t
		    (inst li (fixnum y) temp)
		    temp))))
      (inst cmpeq x y temp)
      (if not-p
	  (inst beq temp target)
	  (inst bne temp target)))))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 6)))
  (:variant-cost 6))


;;;; 32-bit logical operations.

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 4
    (let ((done (gen-label)))
      (inst srl next shift res)
      (inst beq shift done)
      (inst subq zero-tn shift temp)
      (inst sll prev temp temp)
      (inst bis res temp res)
      (emit-label done)
      (move res result))))

(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:generator 2
    (inst not x r)
    (inst mskll r 4 r)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (inst and x y r)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (inst bis x y r)))

(define-vop (32bit-logical-nor 32bit-logical)
  (:translate 32bit-logical-nor)
  (:generator 2
    (inst ornot x y r)
    (inst mskll r 4 r)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (inst xor x y r)))

(deftransform 32bit-logical-eqv ((x y) (* *))
  '(32bit-logical-not (32bit-logical-xor x y)))

(deftransform 32bit-logical-andc1 ((x y) (* *))
  '(32bit-logical-and (32bit-logical-not x) y))

(deftransform 32bit-logical-andc2 ((x y) (* *))
  '(32bit-logical-and x (32bit-logical-not y)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-or (32bit-logical-not x) y))

(deftransform 32bit-logical-orc2 ((x y) (* *))
  '(32bit-logical-or x (32bit-logical-not y)))

(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
	 (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "SHIFT-TOWARDS-START")
  (:temporary (:sc non-descriptor-reg) temp)
  (:generator 1
    (inst and amount #x1f temp)
    (inst srl num temp r)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:temporary (:sc non-descriptor-reg) temp)
  (:generator 1
    (inst and amount #x1f temp)
    (inst sll num temp r)))


;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-type
  (unsigned-reg) unsigned-num bignum::%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-type
  (unsigned-reg) unsigned-num bignum::%bignum-set #+gengc nil)

(define-vop (digit-0-or-plus)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:temporary (:sc non-descriptor-reg) temp)
  (:conditional)
  (:info target not-p)
  (:generator 2
    (inst sll digit 32 temp)
    (if not-p
	(inst blt temp target)
	(inst bge temp target))))

(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from :load)
	    (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 5
    (inst addq a b result)
    (inst addq result c result)
    (inst sra result 32 carry)
    (inst mskll result 4 result)))

(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from :load)
	    (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst xor c 1 result)
    (inst subq a result result)
    (inst subq result b result)
    (inst srl result 63 borrow)
    (inst xor borrow 1 borrow)
    (inst mskll result 4 result)))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 6
    (inst mulq x y lo)
    (inst addq lo carry-in lo)
    (inst srl lo 32 hi)
    (inst mskll lo 4 lo)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 9
    (inst mulq x y lo)
    (inst addq lo prev lo)
    (inst addq lo carry-in lo)
    (inst srl lo 32 hi)
    (inst mskll lo 4 lo)))

(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 3
    (inst mulq x y lo)
    (inst srl lo 32 hi)
    (inst mskll lo 4 lo)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst not x r)
    (inst mskll r 4 r)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra fixnum 2 digit)))

(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (num-high :scs (unsigned-reg))
	 (num-low :scs (unsigned-reg))
	 (denom-arg :scs (unsigned-reg) :target denom))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) denom)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:results (quo :scs (unsigned-reg) :from (:eval 0))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 325 ; number of inst assuming targeting works.
    (inst sll num-high 32 rem)
    (inst bis rem num-low rem)
    (inst sll denom-arg 32 denom)
    (inst cmpule denom rem quo)
    (inst beq quo shift1)
    (inst subq rem denom rem)
    SHIFT1
    (dotimes (i 32)
      (let ((shift2 (gen-label)))
	(inst srl denom 1 denom)
	(inst cmpule denom rem temp)
	(inst sll quo 1 quo)
	(inst beq temp shift2)
	(inst subq rem denom rem)
	(inst bis quo 1 quo)
	(emit-label shift2)))))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 2
    (sc-case res
      (any-reg
       (inst sll digit 34 res)
       (inst sra res 32 res))
      (signed-reg
       (inst sll digit 32 res)
       (inst sra res 32 res)))))

(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sll digit 32 result)
    (inst sra result count result)
    (inst srl result 32 result)))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
    (inst srl digit count result)))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 1
    (inst sll digit count result)))


;;;; Static functions.

(define-static-function two-arg-gcd (x y) :translate gcd)
(define-static-function two-arg-lcm (x y) :translate lcm)

(define-static-function two-arg-+ (x y) :translate +)
(define-static-function two-arg-- (x y) :translate -)
(define-static-function two-arg-* (x y) :translate *)
(define-static-function two-arg-/ (x y) :translate /)

(define-static-function two-arg-< (x y) :translate <)
(define-static-function two-arg-<= (x y) :translate <=)
(define-static-function two-arg-> (x y) :translate >)
(define-static-function two-arg->= (x y) :translate >=)
(define-static-function two-arg-= (x y) :translate =)
(define-static-function two-arg-/= (x y) :translate /=)

(define-static-function %negate (x) :translate %negate)

(define-static-function two-arg-and (x y) :translate logand)
(define-static-function two-arg-ior (x y) :translate logior)
(define-static-function two-arg-xor (x y) :translate logxor)
