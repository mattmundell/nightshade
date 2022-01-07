;;; The VM definition arithmetic VOPs for the MIPS.

(in-package "MIPS")


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
    (inst subu res zero-tn x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst subu res zero-tn x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0))
	      temp)
  (:translate lognot)
  (:generator 2
    (inst li temp (fixnum -1))
    (inst xor res x temp)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst nor res x zero-tn)))


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
	 (inst ,op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:args (x :target r :scs (signed-reg))
	      (y :target r :scs (signed-reg)))
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
	 (inst ,op r x y)))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:args (x :target r :scs (unsigned-reg))
	      (y :target r :scs (unsigned-reg)))
       (:translate ,translate)
       (:generator ,(1+ untagged-cost)
	 (inst ,op r x y)))
     ,@(when tagged-type
	 `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
			fast-fixnum-c-binop)
		       (:arg-types tagged-num (:constant ,tagged-type))
	     (:translate ,translate)
	     (:generator ,cost
			 (inst ,op r x (fixnum y))))))
     ,@(when untagged-type
	 `((define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
			fast-signed-c-binop)
		       (:arg-types signed-num (:constant ,untagged-type))
	     (:translate ,translate)
	     (:generator ,untagged-cost
			 (inst ,op r x y)))
	   (define-vop (,(symbolicate "FAST-" translate
				      "-C/UNSIGNED=>UNSIGNED")
			fast-unsigned-c-binop)
		       (:arg-types unsigned-num (:constant ,untagged-type))
	     (:translate ,translate)
	     (:generator ,untagged-cost
			 (inst ,op r x y)))))))

(define-binop + 1 5 addu (signed-byte 14) (signed-byte 16))
(define-binop - 1 5 subu
  (integer #.(- (1- (ash 1 14))) #.(ash 1 14))
  (integer #.(- (1- (ash 1 16))) #.(ash 1 16)))
(define-binop logior 1 3 or (unsigned-byte 14) (unsigned-byte 16))
(define-binop lognor 1 3 nor nil nil)
(define-binop logand 1 3 and (unsigned-byte 14) (unsigned-byte 16))
(define-binop logxor 1 3 xor (unsigned-byte 14) (unsigned-byte 16))

;;; Special case fixnum + and - that trap on overflow.  Useful when we don't
;;; know that the result is going to be a fixnum.

(define-vop (fast-+/fixnum fast-+/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 4
    (inst add r x y)))

(define-vop (fast-+-c/fixnum fast-+-c/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 3
    (inst add r x (fixnum y))))

(define-vop (fast--/fixnum fast--/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 4
    (inst sub r x y)))

(define-vop (fast---c/fixnum fast---c/fixnum=>fixnum)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:note nil)
  (:generator 3
    (inst sub r x (fixnum y))))

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
    (inst bgez amount positive)
    (inst subu ndesc zero-tn amount)
    (inst slt temp ndesc 31)
    (inst bne temp zero-tn done)
    (inst srl result number ndesc)
    (inst b done)
    (inst srl result number 31)

    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll result number amount)

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
    (inst bgez amount positive)
    (inst subu ndesc zero-tn amount)
    (inst slt temp ndesc 31)
    (inst bne temp zero-tn done)
    (inst sra result number ndesc)
    (inst b done)
    (inst sra result number 31)

    POSITIVE
    ;; The result-type assures us that this shift will not overflow.
    (inst sll result number amount)

    DONE))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:policy :fast-safe)
  (:translate ash)
  (:note "inline ASH")
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (cond ((< count 0)
	   ;; It is a right shift.
	   (inst srl result number (min (- count) 31)))
	  ((> count 0)
	   ;; It is a left shift.
	   (inst sll result number (min count 31)))
	  (t
	   ;; Count=0?  Shouldn't happen, but it's easy:
	   (move result number)))))

(define-vop (fast-ash-c/signed=>signed)
  (:policy :fast-safe)
  (:translate ash)
  (:note "inline ASH")
  (:args (number :scs (signed-reg)))
  (:info count)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (cond ((< count 0)
	   ;; It is a right shift.
	   (inst sra result number (min (- count) 31)))
	  ((> count 0)
	   ;; It is a left shift.
	   (inst sll result number (min count 31)))
	  (t
	   ;; Count=0?  Shouldn't happen, but it's easy:
	   (move result number)))))

(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
	  (test (gen-label)))
      (move shift arg)
      (inst bgez shift test)
      (move res zero-tn)
      (inst b test)
      (inst nor shift shift)

      (emit-label loop)
      (inst add res (fixnum 1))

      (emit-label test)
      (inst bne shift loop)
      (inst srl shift 1))))

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
    (inst li mask #x55555555)
    (inst srl temp arg 1)
    (inst and num arg mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x33333333)
    (inst srl temp num 2)
    (inst and num mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x0f0f0f0f)
    (inst srl temp num 4)
    (inst and num mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x00ff00ff)
    (inst srl temp num 8)
    (inst and num mask)
    (inst and temp mask)
    (inst addu num temp)
    (inst li mask #x0000ffff)
    (inst srl temp num 16)
    (inst and num mask)
    (inst and temp mask)
    (inst addu res num temp)))

;;; Multiply and Divide.

(define-vop (fast-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:generator 4
    (inst sra temp y 2)
    (inst mult x temp)
    (inst mflo r)))

(define-vop (fast-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:generator 3
    (inst mult x y)
    (inst mflo r)))

(define-vop (fast-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:generator 3
    (inst multu x y)
    (inst mflo r)))

(define-vop (fast-truncate/fixnum fast-fixnum-binop)
  (:translate truncate)
  (:results (q :scs (any-reg))
	    (r :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:temporary (:scs (non-descriptor-reg) :to :eval) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 11
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst nop)
    (inst div x y)
    (inst mflo temp)
    (inst sll q temp 2)
    (inst mfhi r)))

(define-vop (fast-truncate/unsigned fast-unsigned-binop)
  (:translate truncate)
  (:results (q :scs (unsigned-reg))
	    (r :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst nop)
    (inst divu x y)
    (inst mflo q)
    (inst mfhi r)))

(define-vop (fast-truncate/signed fast-signed-binop)
  (:translate truncate)
  (:results (q :scs (signed-reg))
	    (r :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 12
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst beq y zero-tn zero))
    (inst nop)
    (inst div x y)
    (inst mflo q)
    (inst mfhi r)))


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
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(- bound bite 1))))
	(t
	 (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (integer-with-a-bite-out 14 4)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (integer-with-a-bite-out 16 1)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (and (integer-with-a-bite-out 16 1)
					   unsigned-byte)))
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
	     (inst bgez x target)
	     (inst bltz x target)))
	(t
	 (if signed
	     (inst slt temp x y)
	     (inst sltu temp x y))
	 (if not-p
	     (inst beq temp zero-tn target)
	     (inst bne temp zero-tn target))))
  (inst nop))

(define-conditional-vop >
  (cond ((and signed (eql y 0))
	 (if not-p
	     (inst blez x target)
	     (inst bgtz x target)))
	((integerp y)
	 (let ((y (+ y (if -c/fixnum (fixnum 1) 1))))
	   (if signed
	       (inst slt temp x y)
	       (inst sltu temp x y))
	   (if not-p
	       (inst bne temp zero-tn target)
	       (inst beq temp zero-tn target))))
	(t
	 (if signed
	     (inst slt temp y x)
	     (inst sltu temp y x))
	 (if not-p
	     (inst beq temp zero-tn target)
	     (inst bne temp zero-tn target))))
  (inst nop))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

(define-conditional-vop eql
  (declare (ignore signed))
  (when (integerp y)
    (inst li temp y)
    (setf y temp))
  (if not-p
      (inst bne x y target)
      (inst beq x y target))
  (inst nop))

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
  (:ignore temp)
  (:generator 3
    (if not-p
	(inst bne x y target)
	(inst beq x y target))
    (inst nop)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (let ((y (cond ((eql y 0) zero-tn)
		   (t
		    (inst li temp (fixnum y))
		    temp))))
      (if not-p
	  (inst bne x y target)
	  (inst beq x y target))
      (inst nop))))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 14)))
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
      (inst beq shift done)
      (inst srl res next shift)
      (inst subu temp zero-tn shift)
      (inst sll temp prev temp)
      (inst or res res temp)
      (emit-label done)
      (move result res))))

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
  (:generator 1
    (inst nor r x zero-tn)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (inst and r x y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (inst or r x y)))

(define-vop (32bit-logical-nor 32bit-logical)
  (:translate 32bit-logical-nor)
  (:generator 1
    (inst nor r x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (inst xor r x y)))

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
  (:generator 1
    (ecase (backend-byte-order *backend*)
      (:big-endian
       (inst sll r num amount))
      (:little-endian
       (inst srl r num amount)))))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (ecase (backend-byte-order *backend*)
      (:big-endian
       (inst srl r num amount))
      (:little-endian
       (inst sll r num amount)))))


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
  (:conditional)
  (:info target not-p)
  (:generator 2
    (if not-p
	(inst bltz digit target)
	(inst bgez digit target))
    (inst nop)))

(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
	    (carry :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let ((carry-in (gen-label))
	  (done (gen-label)))
      (inst bne c carry-in)
      (inst addu res a b)

      (inst b done)
      (inst sltu carry res b)

      (emit-label carry-in)
      (inst addu res 1)
      (inst nor temp a zero-tn)
      (inst sltu carry b temp)
      (inst xor carry 1)

      (emit-label done)
      (move result res))))

(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg))
	    (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (let ((no-borrow-in (gen-label))
	  (done (gen-label)))

      (inst bne c no-borrow-in)
      (inst subu res a b)

      (inst subu res 1)
      (inst b done)
      (inst sltu borrow b a)

      (emit-label no-borrow-in)
      (inst sltu borrow a b)
      (inst xor borrow 1)

      (emit-label done)
      (move result res))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) temp)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 6
    (inst multu x y)
    (inst mflo temp)
    (inst addu lo temp carry-in)
    (inst sltu temp lo carry-in)
    (inst mfhi hi)
    (inst addu hi temp)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg) :to :save))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 2)) temp)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 9
    (inst multu x y)
    (inst addu lo prev carry-in)
    (inst sltu temp lo carry-in)
    (inst mfhi hi)
    (inst addu hi temp)
    (inst mflo temp)
    (inst addu lo temp)
    (inst sltu temp lo temp)
    (inst addu hi temp)))

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
    (inst multu x y)
    (inst mflo lo)
    (inst mfhi hi)))

(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst nor r x zero-tn)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra digit fixnum 2)))

(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (num-high :scs (unsigned-reg) :target rem)
	 (num-low :scs (unsigned-reg) :target rem-low)
	 (denom :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:argument 1)) rem-low)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:results (quo :scs (unsigned-reg) :from (:eval 0))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 325 ; number of inst assuming targeting works.
    (move rem num-high)
    (move rem-low num-low)
    (flet ((maybe-subtract (&optional (guess temp))
	     (inst subu temp guess 1)
	     (inst and temp denom)
	     (inst subu rem temp)))
      (inst sltu quo rem denom)
      (maybe-subtract quo)
      (dotimes (i 32)
	(inst sll rem 1)
	(inst srl temp rem-low 31)
	(inst or rem temp)
	(inst sll rem-low 1)
	(inst sltu temp rem denom)
	(inst sll quo 1)
	(inst or quo temp)
	(maybe-subtract)))
    (inst nor quo zero-tn)))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst sll res digit 2))
      (signed-reg
       (move res digit)))))

(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (unsigned-reg)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sra result digit count)))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
    (inst srl result digit count)))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 1
    (inst sll result digit count)))


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
