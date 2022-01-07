;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/alpha/support.lisp,v 1.2 1994/10/31 04:55:55 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;
(in-package "ALPHA")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values
      `((inst li (make-fixup ',name :assembly-routine) temp)
	(inst jsr lip-tn temp))
      '((:temporary (:sc non-descriptor-reg) temp))
     nil))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
	   (nfp-save (make-symbol "NFP-SAVE"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn ,nfp-save cur-nfp))
	    (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
	    (note-next-instruction ,vop :call-site)
	    ; here
	    (inst li (make-fixup ',name :assembly-routine) temp1)
	    (inst jsr lip-tn temp1 (make-fixup ',name :assembly-routine))
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :single-value-return)
	    (without-scheduling ()
	      (move ocfp-tn csp-tn)
	      (inst nop))
	    (inst compute-code-from-lra code-tn code-tn
		  lra-label ,temp)
	    (when cur-nfp
	      (maybe-load-stack-nfp-tn cur-nfp ,nfp-save temp1))))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  (:temporary (:sc descriptor-reg :offset lra-offset
		       :from (:eval 0) :to (:eval 1))
		      ,lra)
	  (:temporary (:scs (control-stack) :offset nfp-save-offset)
		      ,nfp-save)
	  (:temporary (:scs (non-descriptor-reg)) temp1)
	  (:save-p t)))))
    (:none
     (values
      `((inst li (make-fixup ',name :assembly-routine) temp)
	(inst jsr lip-tn temp (make-fixup ',name :assembly-routine)))
      '((:temporary (:scs (non-descriptor-reg)) temp))
      nil))))


(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret zero-tn lip-tn)))
    (:full-call
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose
					 'descriptor-reg)
				    :offset lra-offset)
		    lip-tn :offset 2)))
    (:none)))
