;;; The x86 VM definition of character operations.

(in-package "X86")


;;;; Moves and coercions.

;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-char)
  (:args (x :scs (any-reg control-stack) :target al))
  (:temporary (:sc byte-reg :offset al-offset
		   :from (:argument 0) :to (:eval 0)) al)
  (:ignore al)
  (:temporary (:sc byte-reg :offset ah-offset :target y
		   :from (:argument 0) :to (:result 0)) ah)
  (:results (y :scs (base-char-reg base-char-stack)))
  (:note "character untagging")
  (:generator 1
    (move eax-tn x)
    (move y ah)))
;;;
(define-move-vop move-to-base-char :move
  (any-reg control-stack) (base-char-reg base-char-stack))

;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-char)
  (:args (x :scs (base-char-reg base-char-stack) :target ah))
  (:temporary (:sc byte-reg :offset al-offset :target y
		   :from (:argument 0) :to (:result 0)) al)
  (:temporary (:sc byte-reg :offset ah-offset
		   :from (:argument 0) :to (:result 0)) ah)
  (:results (y :scs (any-reg descriptor-reg control-stack)))
  (:note "character tagging")
  (:generator 1
    (move ah x)				; maybe move char byte
    (inst mov al base-char-type)	; #x86 to type bits
    (inst and eax-tn #xffff)		; remove any junk bits
    (move y eax-tn)))

;;;
(define-move-vop move-from-base-char :move
  (base-char-reg base-char-stack) (any-reg descriptor-reg control-stack))

;;; Move untagged base-char values.
;;;
(define-vop (base-char-move)
  (:args (x :target y
	    :scs (base-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-char-reg base-char-stack)
	       :load-if (not (location= x y))))
  (:note "character move")
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop base-char-move :move
  (base-char-reg) (base-char-reg base-char-stack))

;;; Move untagged base-char arguments/return-values.
;;;
(define-vop (move-base-char-argument)
  (:args (x :target y
	    :scs (base-char-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y base-char-reg))))
  (:results (y))
  (:note "character arg move")
  (:generator 0
    (sc-case y
      (base-char-reg
       (move y x))
      (base-char-stack
       (inst mov
	     (make-ea :byte :base fp :disp (- (* (1+ (tn-offset y)) 4)))
	     x)))))
;;;
(define-move-vop move-base-char-argument :move-argument
  (any-reg base-char-reg) (base-char-reg))

;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-char
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-char-reg) (any-reg descriptor-reg))


;;;; Other operations.

(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-char-reg base-char-stack)))
  (:arg-types base-char)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst movzx res ch)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target eax))
  (:arg-types positive-fixnum)
  (:temporary (:sc unsigned-reg :offset eax-offset :target res
		   :from (:argument 0) :to (:result 0))
	      eax)
  (:results (res :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 1
    (move eax code)
    (move res al-tn)))


;;; Comparison of base-chars.
;;;
(define-vop (base-char-compare)
  (:args (x :scs (base-char-reg base-char-stack))
	 (y :scs (base-char-reg)
	    :load-if (not (and (sc-is x base-char-reg)
			       (sc-is y base-char-stack)))))
  (:arg-types base-char base-char)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition not-condition)
  (:generator 3
    (inst cmp x y)
    (inst jmp (if not-p not-condition condition) target)))

(define-vop (fast-char=/base-char base-char-compare)
  (:translate char=)
  (:variant :e :ne))

(define-vop (fast-char</base-char base-char-compare)
  (:translate char<)
  (:variant :b :nb))

(define-vop (fast-char>/base-char base-char-compare)
  (:translate char>)
  (:variant :a :na))
