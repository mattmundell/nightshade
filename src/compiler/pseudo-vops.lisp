;;; Definitions of VOPs used as internal markers by the compiler.  Since
;;; they don't emit any code, they should be portable.

(in-package "C")

;;; Notes the place at which the environment is properly initialized, for
;;; debug-info purposes.
;;;
(define-vop (note-environment-start)
  (:info start-lab)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (emit-label start-lab)
    (note-debug-location vop start-lab :non-local-entry)))

;;; Call a move function.  Used for register save/restore and spilling.
;;;
(define-vop (move-operand)
  (:args (x))
  (:results (y))
  (:info name)
  (:vop-var vop)
  (:generator 0
    (funcall (symbol-function name) vop x y)))
