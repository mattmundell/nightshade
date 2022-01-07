;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/alpha/assem-rtns.lisp,v 1.3 1994/10/31 04:55:55 ram Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "ALPHA")


;;;; Return-multiple with other than one value

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer
     (:temp lip interior-reg lip-offset)
     (:temp count any-reg nl2-offset)
     (:temp dst any-reg nl4-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))

  ;; Note, because of the way the return-multiple vop is written, we can
  ;; assume that we are never called with nvals == 1 and that a0 has already
  ;; been loaded.
  (inst ble nvals default-a0-and-on)
  (inst ldl a1 (* 1 vm:word-bytes) vals)
  (inst subq nvals (fixnum 2) count)
  (inst ble count default-a2-and-on)
  (inst ldl a2 (* 2 vm:word-bytes) vals)
  (inst subq nvals (fixnum 3) count)
  (inst ble count default-a3-and-on)
  (inst ldl a3 (* 3 vm:word-bytes) vals)
  (inst subq nvals (fixnum 4) count)
  (inst ble count default-a4-and-on)
  (inst ldl a4 (* 4 vm:word-bytes) vals)
  (inst subq nvals (fixnum 5) count)
  (inst ble count default-a5-and-on)
  (inst ldl a5 (* 5 vm:word-bytes) vals)
  (inst subq nvals (fixnum 6) count)
  (inst ble count done)

  ;; Copy the remaining args to the top of the stack.
  (inst addq vals (* 6 vm:word-bytes) vals)
  (inst addq cfp-tn (* 6 vm:word-bytes) dst)

  LOOP
  (inst ldl temp 0 vals)
  (inst addq vals vm:word-bytes vals)
  (inst stl temp 0 dst)
  (inst subq count (fixnum 1) count)
  (inst addq dst vm:word-bytes dst)
  (inst bne count loop)
		
  (inst br zero-tn done)

  DEFAULT-A0-AND-ON
  (inst move null-tn a0)
  (inst move null-tn a1)
  DEFAULT-A2-AND-ON
  (inst move null-tn a2)
  DEFAULT-A3-AND-ON
  (inst move null-tn a3)
  DEFAULT-A4-AND-ON
  (inst move null-tn a4)
  DEFAULT-A5-AND-ON
  (inst move null-tn a5)
  DONE
  
  ;; Clear the stack.
  (move cfp-tn ocfp-tn)
  (move ocfp cfp-tn)
  (inst addq ocfp-tn nvals csp-tn)
  
  ;; Return.
  (lisp-return lra lip))


;;;; tail-call-variable.

#+assembler ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp count any-reg cfunc-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; Needed for the jump
     (:temp lip interior-reg lip-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst subq csp-tn args nargs)
     
  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (inst ldl a0 (* 0 vm:word-bytes) args)
  (inst ldl a1 (* 1 vm:word-bytes) args)
  (inst ldl a2 (* 2 vm:word-bytes) args)
  (inst ldl a3 (* 3 vm:word-bytes) args)
  (inst ldl a4 (* 4 vm:word-bytes) args)
  (inst ldl a5 (* 5 vm:word-bytes) args)

  ;; Calc SRC, DST, and COUNT
  (inst subq nargs (fixnum register-arg-count) count)
  (inst addq args (* vm:word-bytes register-arg-count) src)
  (inst ble count done)
  (inst addq cfp-tn (* vm:word-bytes register-arg-count) dst)
	
  LOOP
  ;; Copy one arg.
  (inst ldl temp 0 src)
  (inst addq src vm:word-bytes src)
  (inst stl temp 0 dst)
  (inst subq count (fixnum 1) count)
  (inst addq dst vm:word-bytes dst)
  (inst bgt count loop)
	
  DONE
  ;; We are done.  Do the jump.
  (progn
    (loadw temp lexenv vm:closure-function-slot vm:function-pointer-type)
    (lisp-jump temp lip)))


;;;; Non-local exit noise.

(define-assembly-routine
    (unwind
     (:translate %continue-unwind)
     (:policy :fast-safe))
    ((:arg block (any-reg descriptor-reg) a0-offset)
     (:arg start (any-reg descriptor-reg) ocfp-offset)
     (:arg count (any-reg descriptor-reg) nargs-offset)
     (:temp lip interior-reg lip-offset)
     (:temp lra descriptor-reg lra-offset)
     (:temp cur-uwp any-reg nl0-offset)
     (:temp next-uwp any-reg nl1-offset)
     (:temp target-uwp any-reg nl2-offset)
     (:temp temp1 non-descriptor-reg nl3-offset))
  (declare (ignore start count))

  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst beq block error))
  
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst cmpeq cur-uwp target-uwp temp1)
  (inst beq temp1 do-uwp)
      
  (move block cur-uwp)

  do-exit
      
  (loadw cfp-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  (progn
    (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
    (lisp-return lra lip :frob-code nil))

  do-uwp

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block*)
  (inst br zero-tn do-exit))


(define-assembly-routine
    throw
    ((:arg target descriptor-reg a0-offset)
     (:arg start any-reg ocfp-offset)
     (:arg count any-reg nargs-offset)
     (:temp catch any-reg a1-offset)
     (:temp tag descriptor-reg a2-offset)
     (:temp temp1 non-descriptor-reg nl0-offset))
  
  (progn start count) ; We just need them in the registers.

  (load-symbol-value catch lisp::*current-catch-block*)
  
  loop
  
  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst beq catch error))
  
  (loadw tag catch vm:catch-block-tag-slot)
  (inst cmpeq tag target temp1)
  (inst bne temp1 exit)
  (loadw catch catch vm:catch-block-previous-catch-slot)
  (inst br zero-tn loop)
  
  exit
  
  (move catch target)
  (inst li (make-fixup 'unwind :assembly-routine) temp1)
  (inst jmp zero-tn temp1 (make-fixup 'unwind :assembly-routine)))
