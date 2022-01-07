;;; The VM definition of predicate VOPs for the MIPS.

(in-package "MIPS")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the
;;; desired destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest)
    (inst nop)))


;;;; Conditional VOPs.

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
	 (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (if not-p
	(inst bne x y target)
	(inst beq x y target))
    (inst nop)))
