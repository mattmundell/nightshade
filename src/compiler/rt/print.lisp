;;; Temporary printing utilities and similar noise.

(in-package "RT")

(define-vop (print)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset) nl0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:scs (sap-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((lra-label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (inst cal nsp-tn nsp-tn -16)
      (storew object nsp-tn)
      (inst compute-lra-from-code lra code-tn lra-label)
      (inst cai nl0 (make-fixup "_debug_print" :foreign))
      (inst cai temp (make-fixup "call_into_c" :foreign))
      (inst b temp)

      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (inst cal nsp-tn nsp-tn 16)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
