;;; The print VOP, which is used while booting the kernel core to keep the
;;; user entertained.

(in-package "X86")

(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from :eval :to :result) eax)
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:generator 100
    (inst push object)
    (inst lea eax (make-fixup (extern-alien-name "debug_print") :foreign))
    (inst call (make-fixup (extern-alien-name "call_into_c") :foreign))
    (inst add esp-tn word-bytes)
    (move result eax)))
