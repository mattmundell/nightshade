;;; The VM definition of predicate VOPs for the HPPA

(in-package "HPPA")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the
;;; desired destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest :nullify t)))


;;;; Conditional VOPs.

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
	 (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst bc := not-p x y target)))
