;;; The VM definition of operand loading/saving and the Move VOP.

(in-package "ALPHA")

(define-move-function (load-immediate 1) (vop x y)
  ((null zero immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst li (fixnum val) y))
      (null
       (move null-tn y))
      (symbol
       (load-symbol y val))
      (character
       (inst li (logior (ash (char-code val) type-bits) base-char-type)
	     y)))))

(define-move-function (load-number 1) (vop x y)
  ((zero immediate)
   (signed-reg unsigned-reg))
  (inst li (tn-value x) y))

(define-move-function (load-base-char 1) (vop x y)
  ((immediate) (base-char-reg))
  (inst li (char-code (tn-value x)) y))

(define-move-function (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst li (sap-int (tn-value x)) y))

(define-move-function (load-constant 5) (vop x y)
  ((constant) (descriptor-reg any-reg))
  (loadw y code-tn (tn-offset x) other-pointer-type))

(define-move-function (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-function (load-number-stack 5) (vop x y)
  ((base-char-stack) (base-char-reg))
  (let ((nfp (current-nfp-tn vop)))
    (loadw y nfp (tn-offset x))))

(define-move-function (load-number-stack-64 5) (vop x y)
  ((sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (let ((nfp (current-nfp-tn vop)))
    (loadq y nfp (tn-offset x))))

(define-move-function (store-stack 5) (vop x y)
  ((any-reg descriptor-reg null zero) (control-stack))
  (store-stack-tn y x))

(define-move-function (store-number-stack 5) (vop x y)
  ((base-char-reg) (base-char-stack))
  (let ((nfp (current-nfp-tn vop)))
    (storew x nfp (tn-offset y))))

(define-move-function (store-number-stack-64 5) (vop x y)
  ((sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (let ((nfp (current-nfp-tn vop)))
    (storeq x nfp (tn-offset y))))


;;;; The Move VOP.
;;;
(define-vop (move)
  (:args (x :target y
	    :scs (any-reg descriptor-reg zero null)
	    :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg control-stack)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (unless (location= x y)
      (sc-case y
	((any-reg descriptor-reg)
	 (inst move x y))
	(control-stack
	 (store-stack-tn y x))))))

(define-move-vop move :move
  (any-reg descriptor-reg zero null)
  (any-reg descriptor-reg))

;;; Make Move the check VOP for T so that type check generation doesn't think
;;; it is a hairy type.  This also allows checking of a few of the values in a
;;; continuation to fall out.
;;;
(primitive-type-vop move (:check) t)

;;;    The Move-Argument VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
;;;
(define-vop (move-argument)
  (:args (x :target y
	    :scs (any-reg descriptor-reg null zero))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move x y))
      (control-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-argument :move-argument
  (any-reg descriptor-reg null zero)
  (any-reg descriptor-reg))


;;;; ILLEGAL-MOVE

;;; This VOP exists just to begin the lifetime of a TN that couldn't be written
;;; legally due to a type error.  An error is signalled before this VOP is
;;; so we don't need to do anything (not that there would be anything sensible
;;; to do anyway.)
;;;
(define-vop (illegal-move)
  (:args (x) (type))
  (:results (y))
  (:ignore y)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 666
    (error-call vop object-not-type-error x type)))


;;;; Moves and coercions.

;;; These MOVE-TO-WORD VOPs move a tagged integer to a raw full-word
;;; representation.  Similarly, the MOVE-FROM-WORD VOPs converts a raw integer
;;; to a tagged bignum or fixnum.

;;; Arg is a fixnum, so just shift it.  We need a type restriction because some
;;; possible arg SCs (control-stack) overlap with possible bignum arg SCs.
;;;
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (inst sra x 2 y)))
;;;
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; Arg is a non-immediate constant, load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (inst li (tn-value x) y)))
;;;
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; Arg is a fixnum or bignum, figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:sc non-descriptor-reg) header)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 3
    (inst and x 3 temp)
    (inst sra x 2 y)
    (inst beq temp done)

    (loadw header x 0 other-pointer-type)
    (inst srl header (1+ type-bits) header)
    (loadw y x bignum-digits-offset other-pointer-type)
    (inst beq header one)

    (loadw header x (1+ bignum-digits-offset) other-pointer-type)
    (inst sll header 32 header)
    (inst mskll y 4 y)
    (inst bis header y y)
    (inst br zero-tn done)
    ONE
    (when (sc-is y unsigned-reg)
      (inst mskll y 4 y))
    DONE))
;;;
(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))

;;; Result is a fixnum, so we can just shift.  We need the result type
;;; restriction because of the control-stack ambiguity noted above.
;;;
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (inst sll x 2 y)))
;;;
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; Result may be a bignum, so we have to check.  Use a worst-case cost to make
;;; sure people know they may be number consing.
;;;
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg) header)
  (:note "signed word to integer coercion")
  (:generator 18
    (move arg x)
    (inst sra x 29 temp)
    (inst sll x 2 y)
    (inst beq temp done)
    (inst not temp temp)
    (inst beq temp done)

    (inst li 2 header)
    (inst sra x 31 temp)
    (inst cmoveq temp 1 header)
    (inst not temp temp)
    (inst cmoveq temp 1 header)
    (inst sll header type-bits header)
    (inst bis header bignum-type header)

    (pseudo-atomic (:extra (pad-data-block (+ bignum-digits-offset 3)))
      (inst bis alloc-tn other-pointer-type y)
      (storew header y 0 other-pointer-type)
      (storew x y bignum-digits-offset other-pointer-type)
      (inst srl x 32 temp)
      (storew temp y (1+ bignum-digits-offset) other-pointer-type))
    DONE))

;;;
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

;;; Check for fixnum, and possibly allocate one or two word bignum result.  Use
;;; a worst-case cost to make sure people know they may be number consing.
;;;
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg) temp1)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move arg x)
    (inst srl x 29 temp)
    (inst sll x 2 y)
    (inst beq temp done)

    (inst li 3 temp)
    (inst cmovge x 2 temp)
    (inst srl x 31 temp1)
    (inst cmoveq temp1 1 temp)
    (inst sll temp type-bits temp)
    (inst bis temp bignum-type temp)

    (pseudo-atomic (:extra (pad-data-block (+ bignum-digits-offset 3)))
      (inst bis alloc-tn other-pointer-type y)
      (storew temp y 0 other-pointer-type)
      (storew x y bignum-digits-offset other-pointer-type)
      (inst srl x 32 temp)
      (storew temp y (1+ bignum-digits-offset) other-pointer-type))
    DONE))

;;;
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))

;;; Move untagged numbers.
;;;
(define-vop (word-move)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:note "word integer move")
  (:generator 0
    (move x y)))
;;;
(define-move-vop word-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Move untagged number arguments/return-values.
;;;
(define-vop (move-word-argument)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move x y))
      ((signed-stack unsigned-stack)
       (storeq x fp (tn-offset y))))))
;;;
(define-move-vop move-word-argument :move-argument
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARGUMENT + coercion to move an untagged number to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
