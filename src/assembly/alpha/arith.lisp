;;; Stuff to handle simple cases for generic arithmetic.

(in-package "ALPHA")

(define-assembly-routine (generic-+
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate +)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp temp2 non-descriptor-reg nl1-offset)
			  (:temp temp3 non-descriptor-reg nl2-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and x 3 temp)
  (inst bne temp DO-STATIC-FUN)
  (inst and y 3 temp)
  (inst bne temp DO-STATIC-FUN)
  (inst addq x y res)

  ; Check to see if we need a bignum
  (inst sra res 31 temp)
  (inst beq temp DONE)
  (inst not temp temp)
  (inst beq temp DONE)
  (inst sra res 2 temp3)

  ; From move-from-signed
  (inst li 2 temp2)
  (inst sra temp3 31 temp)
  (inst cmoveq temp 1 temp2)
  (inst not temp temp)
  (inst cmoveq temp 1 temp2)
  (inst sll temp2 type-bits temp2)
  (inst bis temp2 bignum-type temp2)

  (pseudo-atomic (:extra (pad-data-block (+ bignum-digits-offset 3)))
    (inst bis alloc-tn other-pointer-type res)
    (storew temp2 res 0 other-pointer-type)
    (storew temp3 res bignum-digits-offset other-pointer-type)
    (inst srl temp3 32 temp)
    (storew temp res (1+ bignum-digits-offset) other-pointer-type))
  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst ldl lip (static-function-offset 'two-arg-+) null-tn)
  (inst li (fixnum 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip))

(define-assembly-routine (generic--
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp temp2 non-descriptor-reg nl1-offset)
			  (:temp temp3 non-descriptor-reg nl2-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and x 3 temp)
  (inst bne temp DO-STATIC-FUN)
  (inst and y 3 temp)
  (inst bne temp DO-STATIC-FUN)
  (inst subq x y res)

  ; Check to see if we need a bignum
  (inst sra res 31 temp)
  (inst beq temp DONE)
  (inst not temp temp)
  (inst beq temp DONE)
  (inst sra res 2 temp3)

  ; From move-from-signed
  (inst li 2 temp2)
  (inst sra temp3 31 temp)
  (inst cmoveq temp 1 temp2)
  (inst not temp temp)
  (inst cmoveq temp 1 temp2)
  (inst sll temp2 type-bits temp2)
  (inst bis temp2 bignum-type temp2)

  (pseudo-atomic (:extra (pad-data-block (+ bignum-digits-offset 3)))
    (inst bis alloc-tn other-pointer-type res)
    (storew temp2 res 0 other-pointer-type)
    (storew temp3 res bignum-digits-offset other-pointer-type)
    (inst srl temp3 32 temp)
    (storew temp res (1+ bignum-digits-offset) other-pointer-type))
  DONE
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst ldl lip (static-function-offset 'two-arg--) null-tn)
  (inst li (fixnum 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip))

(define-assembly-routine (generic-*
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate *)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lo non-descriptor-reg nl1-offset)
			  (:temp hi non-descriptor-reg nl2-offset)
			  (:temp temp2 non-descriptor-reg nl3-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (inst and x 3 temp)
  (inst bne temp DO-STATIC-FUN)
  (inst and y 3 temp)
  (inst bne temp DO-STATIC-FUN)

  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum tag.
  (inst sra x 2 temp)
  (inst mulq temp y lo)
  (inst sra lo 32 hi)
  (inst sll lo 32 res)
  (inst sra res 32 res)
  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word
  ;; is just 32 copies of the sign bit of the low word).
  (inst sra res 31 temp)
  (inst xor hi temp temp)
  (inst beq temp DONE)
  ;; Shift the double word hi:res down two bits into hi:low to get rid of the
  ;; fixnum tag.
  (inst sra lo 2 lo)
  (inst sra lo 32 hi)

  ;; Do we need one word or two?  Assume two.
  (inst li (logior (ash 2 type-bits) bignum-type) temp2)
  (inst sra lo 31 temp)
  (inst xor temp hi temp)
  (inst bne temp two-words)

  ;; Only need one word, fix the header.
  (inst li (logior (ash 1 type-bits) bignum-type) temp2)
  ;; Allocate one word.
  (pseudo-atomic (:extra (pad-data-block (1+ bignum-digits-offset)))
    (inst bis alloc-tn other-pointer-type res)
    (storew temp2 res 0 other-pointer-type))
  ;; Store one word
  (storew lo res bignum-digits-offset other-pointer-type)
  ;; Out of here
  (lisp-return lra lip :offset 2)

  TWO-WORDS
  ;; Allocate two words.
  (pseudo-atomic (:extra (pad-data-block (+ 2 bignum-digits-offset)))
    (inst bis alloc-tn other-pointer-type res)
    (storew temp2 res 0 other-pointer-type))
  ;; Store two words.
  (storew lo res bignum-digits-offset other-pointer-type)
  (storew hi res (1+ bignum-digits-offset) other-pointer-type)
  ;; Out of here
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst ldl lip (static-function-offset 'two-arg-*) null-tn)
  (inst li (fixnum 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip)

  DONE)


;;;; Division.

(define-assembly-routine (signed-truncate
			  (:note "(signed-byte 32) truncate")
			  (:cost 60)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types signed-num signed-num)
			  (:result-types signed-num signed-num))

			 ((:arg dividend signed-reg nl0-offset)
			  (:arg divisor signed-reg nl1-offset)

			  (:res quo signed-reg nl2-offset)
			  (:res rem signed-reg nl3-offset)

			  (:temp quo-sign signed-reg nl5-offset)
			  (:temp rem-sign signed-reg nargs-offset)
			  (:temp temp1 non-descriptor-reg nl4-offset))

  (let ((error (generate-error-code nil division-by-zero-error
				    dividend divisor)))
    (inst beq divisor error))

  (inst xor dividend divisor quo-sign)
  (inst move dividend rem-sign)
  (let ((label (gen-label)))
    (inst bge dividend label)
    (inst subq zero-tn dividend dividend)
    (emit-label label))
  (let ((label (gen-label)))
    (inst bge divisor label)
    (inst subq zero-tn divisor divisor)
    (emit-label label))
  (inst move zero-tn rem)
  (inst move zero-tn quo)
  (inst sll dividend 32 dividend)

  (dotimes (i 32)
    (inst srl dividend 63 temp1)
    (inst sll rem 1 rem)
    (inst bis temp1 rem rem)
    (inst cmple divisor rem temp1)
    (inst sll quo 1 quo)
    (inst bis temp1 quo quo)
    (inst sll dividend 1 dividend)
    (inst subq temp1 1 temp1)
    (inst zap divisor temp1 temp1)
    (inst subq rem temp1 rem))

  (let ((label (gen-label)))
    ;; If the quo-sign is negative, we need to negate quo.
    (inst bge quo-sign label)
    (inst subq zero-tn quo quo)
    (emit-label label))
  (let ((label (gen-label)))
    ;; If the rem-sign is negative, we need to negate rem.
    (inst bge rem-sign label)
    (inst subq zero-tn rem rem)
    (emit-label label)))


;;;; Comparison routines.

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp not-p)
       `(define-assembly-routine (,name
				  (:cost 10)
				  (:return-style :full-call)
				  (:policy :safe)
				  (:translate ,translate)
				  (:save-p t))
				 ((:arg x (descriptor-reg any-reg) a0-offset)
				  (:arg y (descriptor-reg any-reg) a1-offset)

				  (:res res descriptor-reg a0-offset)

				  (:temp temp non-descriptor-reg nl0-offset)
				  (:temp lip interior-reg lip-offset)
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (inst and x 3 temp)
	  (inst bne temp DO-STATIC-FN)
	  (inst and y 3 temp)
	  (inst beq temp DO-COMPARE)

	  DO-STATIC-FN
	  (inst ldl lip (static-function-offset ',static-fn) null-tn)
	  (inst li (fixnum 2) nargs)
	  (inst move cfp-tn ocfp)
	  (inst move csp-tn cfp-tn)
	  (inst jmp zero-tn lip)

	  DO-COMPARE
	  ,cmp
	  (inst move null-tn res)
	  (inst ,(if not-p 'bne 'beq) temp done)
	  (load-symbol res t)
	  DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< (inst cmplt x y temp) nil)
  (define-cond-assem-rtn generic-> > two-arg-> (inst cmplt y x temp) nil))

(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst cmpeq x y temp)
  (inst bne temp RETURN-T)
  (inst and x 3 temp)
  (inst beq temp RETURN-NIL)
  (inst and y 3 temp)
  (inst bne temp DO-STATIC-FN)

  RETURN-NIL
  (inst move null-tn res)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst ldl lip (static-function-offset 'eql) null-tn)
  (inst li (fixnum 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and x 3 temp)
  (inst bne temp DO-STATIC-FN)
  (inst and y 3 temp)
  (inst bne temp DO-STATIC-FN)
  (inst cmpeq x y temp)
  (inst bne temp RETURN-T)

  (inst move null-tn res)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst ldl lip (static-function-offset 'two-arg-=) null-tn)
  (inst li (fixnum 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-/=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate /=)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res descriptor-reg a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and x 3 temp)
  (inst bne temp DO-STATIC-FN)
  (inst and y 3 temp)
  (inst bne temp DO-STATIC-FN)
  (inst cmpeq x y temp)
  (inst bne temp RETURN-NIL)

  (load-symbol res t)
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst ldl lip (static-function-offset 'two-arg-=) null-tn)
  (inst li (fixnum 2) nargs)
  (inst move cfp-tn ocfp)
  (inst move csp-tn cfp-tn)
  (inst jmp zero-tn lip)

  RETURN-NIL
  (inst move null-tn res))
