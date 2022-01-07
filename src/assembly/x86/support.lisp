;;; Machine specific support routines needed by the file assembler.

(in-package :x86)

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values
      `((inst call (make-fixup ',name :assembly-routine)))
      nil))
    (:full-call
     (values
      `((note-this-location ,vop :call-site)
	(inst call (make-fixup ',name :assembly-routine))
	(note-this-location ,vop :single-value-return)
	(move esp-tn ebx-tn))
      '((:save-p :compute-only))))
    (:none
     (values
      `((inst jmp (make-fixup ',name :assembly-routine)))
      nil))))

(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `(inst ret))
    (:full-call
     `(
       (inst pop eax-tn)

       (inst add eax-tn 2)
       (inst jmp eax-tn)))
    (:none)))
