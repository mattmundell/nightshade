;;; The VM definition of predicate VOPs for the IBM RT.

(in-package "RT")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the
;;; desired destination.  Dest is the label where we want to be.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest)))


;;;; Conditional VOPs.

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg null))
	 (y :scs (any-reg descriptor-reg null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (let ((x-prime (sc-case x
		     ((any-reg descriptor-reg) x)
		     (null null-tn)))
	  (y-prime (sc-case y
		     ((any-reg descriptor-reg) y)
		     (null null-tn))))
      (inst c x-prime y-prime)
      (if not-p
	  (inst bnc :eq target)
	  (inst bc :eq target)))))
