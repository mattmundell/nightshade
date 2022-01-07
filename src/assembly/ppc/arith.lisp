;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/ppc/arith.lisp,v 1.1 2001/02/11 14:21:52 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;;

(in-package "PPC")



;;;; Addition and subtraction.

(define-assembly-routine 
  (generic-+
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
   (:temp flag non-descriptor-reg nl3-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))
  
  ; Clear the damned "sticky overflow" bit in :cr0 and :xer
  (inst mcrxr :cr0)
  (inst or temp x y)
  (inst andi. temp temp 3)
  (inst bne DO-STATIC-FUN)
  (inst addo. temp x y)
  (inst bns done)
 
  (inst srawi temp x 2)
  (inst srawi temp2 y 2)
  (inst add temp2 temp2 temp)
  (with-fixed-allocation (res flag temp bignum-type (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (inst lwz code-tn null-tn (static-function-offset 'two-arg-+))
  (inst li nargs (fixnum 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))

  DONE
  (move res temp))


(define-assembly-routine 
  (generic--
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
   (:temp flag non-descriptor-reg nl3-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))

  ; Clear the damned "sticky overflow" bit in :cr0
  (inst mcrxr :cr0)

  (inst or temp x y)
  (inst andi. temp temp 3)
  (inst bne DO-STATIC-FUN)

  (inst subo. temp x y)
  (inst bns done)

  (inst srawi temp x 2)
  (inst srawi temp2 y 2)
  (inst sub temp2 temp temp2)
  (with-fixed-allocation (res flag temp bignum-type (1+ bignum-digits-offset))
    (storew temp2 res bignum-digits-offset other-pointer-type))
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (inst lwz code-tn null-tn (static-function-offset 'two-arg--))
  (inst li nargs (fixnum 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))

  DONE
  (move res temp))



;;;; Multiplication


(define-assembly-routine 
  (generic-*
   (:cost 50)
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
   (:temp pa-flag non-descriptor-reg nl3-offset)
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))

  ;; If either arg is not a fixnum, call the static function.  But first ...
  (inst mcrxr :cr0)

  (inst or temp x y)
  (inst andi. temp temp 3)
  ;; Remove the tag from both args, so I don't get so confused.
  (inst srawi temp x 2)
  (inst srawi nargs y 2)
  (inst bne DO-STATIC-FUN)


  (inst mullwo. lo nargs temp)
  (inst srawi hi lo 31)                 ; hi = 32 copies of lo's sign bit
  (inst bns ONE-WORD-ANSWER)
  (inst mulhw hi nargs temp)
  (inst b CONS-BIGNUM)
  
  ONE-WORD-ANSWER                       ; We know that all of the overflow bits are clear.
  (inst addo temp lo lo)
  (inst addo. res temp temp)
  (inst bns GO-HOME)

  CONS-BIGNUM
  ;; Allocate a BIGNUM for the result.
  (pseudo-atomic (pa-flag :extra (pad-data-block (1+ bignum-digits-offset)))
    (let ((one-word (gen-label)))
      (inst ori res alloc-tn other-pointer-type)
      ;; We start out assuming that we need one word.  Is that correct?
      (inst srawi temp lo 31)
      (inst xor. temp temp hi)
      (inst li temp (logior (ash 1 type-bits) bignum-type))
      (inst beq one-word)
      ;; Nope, we need two, so allocate the additional space.
      (inst addi alloc-tn alloc-tn (- (pad-data-block (+ 2 bignum-digits-offset))
			              (pad-data-block (1+ bignum-digits-offset))))
      (inst li temp (logior (ash 2 type-bits) bignum-type))
      (storew hi res (1+ bignum-digits-offset) other-pointer-type)
      (emit-label one-word)
      (storew temp res 0 other-pointer-type)
      (storew lo res bignum-digits-offset other-pointer-type)))
  ;; Out of here
  GO-HOME
  (lisp-return lra :offset 2)

  DO-STATIC-FUN
  (inst lwz code-tn null-tn (static-function-offset 'two-arg-*))
  (inst li nargs (fixnum 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))

  LOW-FITS-IN-FIXNUM
  (move res lo))

(macrolet
    ((frob (name note cost type sc)
       `(define-assembly-routine (,name
				  (:note ,note)
				  (:cost ,cost)
				  (:translate *)
				  (:policy :fast-safe)
				  (:arg-types ,type ,type)
				  (:result-types ,type))
				 ((:arg x ,sc nl0-offset)
				  (:arg y ,sc nl1-offset)
				  (:res res ,sc nl0-offset))
	  ,@(when (eq type 'tagged-num)
	      `((inst srawi x x 2)))
          (inst mullw res x y))))
  (frob unsigned-* "unsigned *" 40 unsigned-num unsigned-reg)
  (frob signed-* "unsigned *" 41 signed-num signed-reg)
  (frob fixnum-* "fixnum *" 30 tagged-num any-reg))



;;;; Division.


(define-assembly-routine (positive-fixnum-truncate
			  (:note "unsigned fixnum truncate")
			  (:cost 45)
			  (:translate truncate)
			  (:policy :fast-safe)
			  (:arg-types positive-fixnum positive-fixnum)
			  (:result-types positive-fixnum positive-fixnum))
			 ((:arg dividend any-reg nl0-offset)
			  (:arg divisor any-reg nl1-offset)

			  (:res quo any-reg nl2-offset)
			  (:res rem any-reg nl0-offset))
  (assert (location= rem dividend))
  (let ((error (generate-error-code nil division-by-zero-error
				    dividend divisor)))
    (inst cmpwi divisor 0)
    (inst beq error))
    (inst divwu quo dividend divisor)
    (inst mullw divisor quo divisor)
    (inst sub rem dividend divisor)
    (inst slwi quo quo 2))



(define-assembly-routine (fixnum-truncate
			  (:note "fixnum truncate")
			  (:cost 50)
			  (:policy :fast-safe)
			  (:translate truncate)
			  (:arg-types tagged-num tagged-num)
			  (:result-types tagged-num tagged-num))
			 ((:arg dividend any-reg nl0-offset)
			  (:arg divisor any-reg nl1-offset)

			  (:res quo any-reg nl2-offset)
			  (:res rem any-reg nl0-offset))
  
  (assert (location= rem dividend))
  (let ((error (generate-error-code nil division-by-zero-error
				    dividend divisor)))
    (inst cmpwi divisor 0)
    (inst beq error))

    (inst divw quo dividend divisor)
    (inst mullw divisor quo divisor)
    (inst subf rem divisor dividend)
    (inst slwi quo quo 2))


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
			  (:res rem signed-reg nl0-offset))
  
  (let ((error (generate-error-code nil division-by-zero-error
				    dividend divisor)))
    (inst cmpwi divisor 0)
    (inst beq error))

    (inst divw quo dividend divisor)
    (inst mullw divisor quo divisor)
    (inst subf rem divisor dividend))


;;;; Comparison

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp)
       `(define-assembly-routine 
          (,name
           (:cost 10)
           (:return-style :full-call)
           (:policy :safe)
           (:translate ,translate)
           (:save-p t))
          ((:arg x (descriptor-reg any-reg) a0-offset)
           (:arg y (descriptor-reg any-reg) a1-offset)
           
           (:res res descriptor-reg a0-offset)
           
           (:temp nargs any-reg nargs-offset)
           (:temp ocfp any-reg ocfp-offset))
                          
          (inst or nargs x y)
          (inst andi. nargs nargs 3)
          (inst cmpw :cr1 x y)
          (inst beq DO-COMPARE)
	  
	  DO-STATIC-FN
	  (inst lwz code-tn null-tn (static-function-offset ',static-fn))
	  (inst li nargs (fixnum 2))
	  (inst mr ocfp cfp-tn)
	  (inst mr cfp-tn csp-tn)
	  (inst j code-tn
		(- (* function-code-offset word-bytes) function-pointer-type))
	  
	  DO-COMPARE
	  (load-symbol res t)
	  (inst b? :cr1 ,cmp done)
	  (inst mr res null-tn)
	  DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< :lt)
  (define-cond-assem-rtn generic-<= <= two-arg-<= :le)
  (define-cond-assem-rtn generic-> > two-arg-> :gt)
  (define-cond-assem-rtn generic->= >= two-arg->= :ge))


(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)

			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst cmpw :cr1 x y)
  (inst andi. nargs x 3)
  (inst beq :cr1 RETURN-T)
  (inst beq RETURN-NIL)                 ; x was fixnum, not eq y
  (inst andi. nargs y 3)
  (inst bne DO-STATIC-FN)

  RETURN-NIL
  (inst mr res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (inst lwz code-tn null-tn (static-function-offset 'eql))
  (inst li nargs (fixnum 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))

  RETURN-T
  (load-symbol res t))

(define-assembly-routine 
  (generic-=
   (:cost 10)
   (:return-style :full-call)
   (:policy :safe)
   (:translate =)
   (:save-p t))
  ((:arg x (descriptor-reg any-reg) a0-offset)
   (:arg y (descriptor-reg any-reg) a1-offset)
   
   (:res res descriptor-reg a0-offset)
   
   (:temp lra descriptor-reg lra-offset)
   (:temp nargs any-reg nargs-offset)
   (:temp ocfp any-reg ocfp-offset))

  (inst or nargs x y)
  (inst andi. nargs nargs 3)
  (inst cmpw :cr1 x y)
  (inst bne DO-STATIC-FN)
  (inst beq :cr1 RETURN-T)

  (inst mr res null-tn)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (inst lwz code-tn null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnum 2))
  (inst mr ocfp cfp-tn)
  (inst mr cfp-tn csp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))

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

			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst or nargs x y)
  (inst andi. nargs nargs 3)
  (inst cmpw :cr1 x y)
  (inst bne DO-STATIC-FN)
  (inst beq :cr1 RETURN-NIL)

  (load-symbol res t)
  (lisp-return lra :offset 2)

  DO-STATIC-FN
  (inst lwz code-tn null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnum 2))
  (inst mr ocfp cfp-tn)
  (inst j code-tn
	(- (* function-code-offset word-bytes) function-pointer-type))
  (inst mr cfp-tn csp-tn)

  RETURN-NIL
  (inst mr res null-tn))
