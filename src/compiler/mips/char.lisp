;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/char.lisp,v 1.14 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/mips/char.lisp,v 1.14 1994/10/31 04:44:16 ram Exp $
;;; 
;;; This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan
;;; Converted for the MIPS R2000 by Christopher Hoover.
;;;
(in-package "MIPS")



;;;; Moves and coercions:

;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-char)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (base-char-reg)))
  (:generator 1
    (inst srl y x vm:type-bits)))
;;;
(define-move-vop move-to-base-char :move
  (any-reg descriptor-reg) (base-char-reg))


;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-char)
  (:args (x :scs (base-char-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst sll y x vm:type-bits)
    (inst or y y vm:base-char-type)))
;;;
(define-move-vop move-from-base-char :move
  (base-char-reg) (any-reg descriptor-reg))

;;; Move untagged base-char values.
;;;
(define-vop (base-char-move)
  (:args (x :target y
	    :scs (base-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-char-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop base-char-move :move
  (base-char-reg) (base-char-reg))


;;; Move untagged base-char arguments/return-values.
;;;
(define-vop (move-base-char-argument)
  (:args (x :target y
	    :scs (base-char-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y base-char-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (base-char-reg
       (move y x))
      (base-char-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-base-char-argument :move-argument
  (any-reg base-char-reg) (base-char-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-char
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-char-reg) (any-reg descriptor-reg))



;;;; Other operations:

(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-char-reg) :target res))
  (:arg-types base-char)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst sll res ch 2)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 1
    (inst srl res code 2)))


;;; Comparison of base-chars.
;;;
(define-vop (base-char-compare pointer-compare)
  (:args (x :scs (base-char-reg))
	 (y :scs (base-char-reg)))
  (:arg-types base-char base-char))

(define-vop (fast-char=/base-char base-char-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</base-char base-char-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/base-char base-char-compare)
  (:translate char>)
  (:variant :gt))

