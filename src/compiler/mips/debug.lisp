;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/debug.lisp,v 1.16 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; 
(in-package "MIPS")


(define-vop (debug-cur-sp)
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res cfp-tn)))

(define-vop (read-control-stack)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:temporary (:scs (sap-reg) :from :eval) sap)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst add sap object offset)
    (inst lw result sap 0)
    (inst nop)))

(define-vop (read-control-stack-c)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 14)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 4
    (inst lw result object (* offset word-bytes))
    (inst nop)))

(define-vop (write-control-stack)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
  (:generator 2
    (inst add sap object offset)
    (inst sw value sap 0)
    (move result value)))

(define-vop (write-control-stack-c)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (value :scs (descriptor-reg) :target result))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 14)) *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 1
    (inst sw value sap (* offset word-bytes))
    (move result value)))


(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
	  (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst srl temp vm:type-bits)
      (inst beq temp bogus)
      (inst sll temp (1- (integer-length vm:word-bytes)))
      (unless (= lowtag vm:other-pointer-type)
	(inst addu temp (- lowtag vm:other-pointer-type)))
      (inst subu code thing temp)
      (emit-label done)
      (assemble (*elsewhere*)
	(emit-label bogus)
	(inst b done)
	(move code null-tn)))))

#-gengc
(define-vop (code-from-lra code-from-mumble)
  (:translate lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate function-code-header)
  (:variant vm:function-pointer-type))

(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))

(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 function-pointer-type)
    (inst srl res vm:type-bits)))
