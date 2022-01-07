;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /home/CVS-cmucl/src/compiler/x86/system.lisp,v 1.3.2.2 2000/05/23 16:38:03 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; x86 VM definitions of various system hacking operations.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,1998,1999.
;;;

(in-package :x86)


;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg control-stack)
		 :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (move result object)
    (inst and result lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset :to (:result 0)) eax)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst mov eax object)
    (inst and al-tn lowtag-mask)
    (inst cmp al-tn other-pointer-type)
    (inst jmp :e other-ptr)
    (inst cmp al-tn function-pointer-type)
    (inst jmp :e function-ptr)

    ;; pick off structures and list pointers
    (inst test al-tn 1)
    (inst jmp :ne done)

    ;; pick off fixnums
    (inst and al-tn 3)
    (inst jmp :e done)

    ;; must be an other immediate
    (inst mov eax object)
    (inst jmp done)
    
    FUNCTION-PTR
    (load-type al-tn object (- vm:function-pointer-type))
    (inst jmp done)
    
    OTHER-PTR
    (load-type al-tn object (- vm:other-pointer-type))
    
    DONE
    (inst movzx result al-tn)))

(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:temporary (:sc byte-reg :from (:eval 0) :to (:eval 1)) temp)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type temp function (- vm:function-pointer-type))
    (inst movzx result temp)))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target eax)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 0) :target result)
	      eax)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (move eax type)
    (inst mov
	  (make-ea :byte :base function :disp (- function-pointer-type))
	  al-tn)
    (move result eax)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-type)
    (inst shr res type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 function-pointer-type)
    (inst shr res type-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res :to (:result 0))
	 (data :scs (any-reg) :target eax))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from (:argument 1) :to (:result 0)) eax)
  (:generator 6
    (move eax data)
    (inst shl eax (- type-bits 2))
    (inst mov al-tn (make-ea :byte :base x :disp (- other-pointer-type)))
    (storew eax x 0 other-pointer-type)
    (move res x)))

(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (move res ptr)
    (inst shl res 3)
    (inst shr res 1)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg) :target res)
	 (type :scs (unsigned-reg immediate)))
  (:results (res :scs (any-reg descriptor-reg) :from (:argument 0)))
  (:generator 2
    (move res val)
    (inst shl res (- type-bits 2))
    (inst or res (sc-case type
		   (unsigned-reg type)
		   (immediate (tn-value type))))))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (load-symbol-value int *binding-stack-pointer*)))

(defknown (setf binding-stack-pointer-sap)
    (system-area-pointer) system-area-pointer ())

(define-vop (set-binding-stack-pointer-sap)
  (:args (new-value :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate (setf binding-stack-pointer-sap))
  (:policy :fast-safe)
  (:generator 1
    (store-symbol-value new-value *binding-stack-pointer*)
    (move int new-value)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int esp-tn)))


;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :to (:result 0)))
  (:results (sap :scs (sap-reg) :from (:argument 0)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw sap code 0 other-pointer-type)
    (inst shr sap type-bits)
    (inst lea sap (make-ea :byte :base code :index sap :scale 4
			   :disp (- other-pointer-type)))))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg) :to (:result 0))
	 (offset :scs (signed-reg unsigned-reg) :to (:result 0)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg) :from (:argument 0)))
  (:generator 10
    (loadw func code 0 other-pointer-type)
    (inst shr func type-bits)
    (inst lea func
	  (make-ea :byte :base offset :index func :scale 4
		   :disp (- function-pointer-type other-pointer-type)))
    (inst add func code)))

(defknown %function-self (function) function (flushable))

(define-vop (%function-self)
  (:policy :fast-safe)
  (:translate %function-self)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (loadw result function function-self-slot function-pointer-type)
    (inst lea result
	  (make-ea :byte :base result 
		   :disp (- function-pointer-type
			    (* function-code-offset word-bytes))))))

;;; Closure function slot is a pointer to raw code on X86 instead of
;;; a pointer to the code function object itself. This VOP is used
;;; to reference the function object given the closure object.
(def-source-transform %closure-function (closure)
  `(%function-self ,closure))

(def-source-transform %funcallable-instance-function (fin)
  `(%function-self ,fin))

(defknown (setf %function-self) (function function) function  (unsafe))

(define-vop (%set-function-self)
  (:policy :fast-safe)
  (:translate (setf %function-self))
  (:args (new-self :scs (descriptor-reg) :target result :to :result)
	 (function :scs (descriptor-reg) :to :result))
  (:temporary (:sc any-reg :from (:argument 0) :to :result) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (inst lea temp
	  (make-ea :byte :base new-self
		   :disp (- (ash function-code-offset word-shift)
			    function-pointer-type)))
    (storew temp function function-self-slot function-pointer-type)
    (move result new-self)))

;; Would have really liked to use a source-transform for this, but they
;; don't work with setf functions.
;; 
(defknown ((setf %funcallable-instance-function)) (function function) function
  (unsafe))
(deftransform (setf %funcallable-instance-function) ((value fin))
  '(setf (%function-self fin) value))



;;;; Other random VOPs.

(defknown unix::do-pending-interrupt () (values))
(define-vop (unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate unix::do-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))

(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))

(defknown float-wait () (values))
(define-vop (float-wait)
  (:policy :fast-safe)
  (:translate float-wait)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (note-next-instruction vop :internal-error)
    (inst wait)))

;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:generator 0
    (inst inc (make-ea :dword :base count-vector
		       :disp (- (* (+ vector-data-offset index) word-bytes)
				other-pointer-type)))))



(defknown sys:scrub-control-stack () (values))

;;; Scrub the control stack.
;;;
;;; On the x86 port the stack grows downwards, and to support grow on
;;; demand stacks the stack must be decreased as it is scrubbed.
;;;
(define-vop (scrub-control-stack)
  (:policy :fast-safe)
  (:translate sys:scrub-control-stack)
  (:args)
  (:results)
  (:temporary (:sc unsigned-reg) count)
  (:temporary (:sc any-reg) stack-save zero)
  (:generator 25
    (inst mov stack-save esp-tn)
    (inst mov zero 0)
    (inst push zero)
    ;; Scrub the stack.
    SCRUB
    (inst add esp-tn 4)
    (inst mov count 2048)
    SCRUB-LOOP
    (inst dec count)
    (inst push zero)
    (inst jmp :nz SCRUB-LOOP)
    ;; Look for a clear stack unit.
    (inst mov count 2048)
    LOOK-LOOP
    (inst sub esp-tn 4)
    (inst cmp (make-ea :dword :base esp-tn) zero)
    (inst jmp :ne SCRUB)
    (inst dec count)
    (inst jmp :nz LOOK-LOOP)
    ;; Done, restore the stack pointer.
    (inst mov esp-tn stack-save)))


;;;; Primitive multi-thread support.

(export 'control-stack-fork)
(defknown control-stack-fork ((simple-array (unsigned-byte 32) (*)) t)
  (member t nil))

(define-vop (control-stack-fork)
  (:policy :fast-safe)
  (:translate control-stack-fork)
  (:args (save-stack :scs (descriptor-reg) :to :result)
	 (inherit :scs (descriptor-reg)))
  (:arg-types simple-array-unsigned-byte-32 *)
  (:results (child :scs (descriptor-reg)))
  (:result-types t)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) index)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) stack)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) temp)
  (:save-p t)
  (:generator 25
    (inst cmp inherit nil-value)
    (inst jmp :e FRESH-STACK)
    
    ;; Child inherits the stack of the parent.
    
    ;; Setup the return context.
    (inst push (make-fixup nil :code-object return))
    (inst push ebp-tn)
    ;; Save the stack.
    (inst xor index index)
    ;; First the stack-pointer.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type))
	  esp-tn)
    (inst inc index)
    (inst mov stack (make-fixup (extern-alien-name "control_stack_end")
				:foreign))
    (inst jmp-short LOOP)

    FRESH-STACK
    ;; Child has a fresh control stack.

    ;; Setup the return context.
    (inst push (make-fixup nil :code-object return))
    (inst mov stack (make-fixup (extern-alien-name "control_stack_end")
				:foreign))
    ;; New FP is the Top of the stack.
    (inst push stack)
    ;; Save the stack.
    (inst xor index index)
    ;; First save the adjusted stack-pointer.
    (inst sub stack ebp-tn)
    (inst add stack esp-tn)
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type))
	  stack)
    ;; Save the current frame, replacing the OCFP and RA by 0.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* (+ vm:vector-data-offset 1) vm:word-bytes)
				vm:other-pointer-type))
	  0)
    ;; Save 0 for the OCFP.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* (+ vm:vector-data-offset 2) vm:word-bytes)
				vm:other-pointer-type))
	  0)
    (inst add index 3)
    ;; Copy the remainder of the frame, skiping the OCFP and RA which
    ;; are saved above.
    (inst lea stack (make-ea :byte :base ebp-tn :disp -8))

    LOOP
    (inst cmp stack esp-tn)
    (inst jmp :le stack-save-done)
    (inst sub stack 4)
    (inst mov temp (make-ea :dword :base stack))
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type))
	  temp)
    (inst inc index)
    (inst jmp-short LOOP)
    
    RETURN
    ;; Stack already clean if it reaches here. Parent returns NIL.
    (inst mov child nil-value)
    (inst jmp-short DONE)
    
    STACK-SAVE-DONE
    ;; Cleanup the stack
    (inst add esp-tn 8)
    ;; Child returns T.
    (load-symbol child t)
    DONE))

(export 'control-stack-resume)
(defknown control-stack-resume ((simple-array (unsigned-byte 32) (*))
				(simple-array (unsigned-byte 32) (*)))
  (values))

(define-vop (control-stack-resume)
  (:policy :fast-safe)
  (:translate control-stack-resume)
  (:args (save-stack :scs (descriptor-reg) :to :result)
	 (new-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32 simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) index)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) stack)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) temp)
  (:save-p t)
  (:generator 25
    ;; Setup the return context.
    (inst push (make-fixup nil :code-object RETURN))
    (inst push ebp-tn)
    ;; Save the stack.
    (inst xor index index)
    ;; First the stack-pointer.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type))
	  esp-tn)
    (inst inc index)
    (inst mov stack (make-fixup (extern-alien-name "control_stack_end")
				:foreign))
    LOOP
    (inst cmp stack esp-tn)
    (inst jmp :le STACK-SAVE-DONE)
    (inst sub stack 4)
    (inst mov temp (make-ea :dword :base stack))
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
		       :disp (- (* vm:vector-data-offset vm:word-bytes)
				vm:other-pointer-type))
	  temp)
    (inst inc index)
    (inst jmp-short LOOP)

    STACK-SAVE-DONE
    ;; Cleanup the stack
    (inst add esp-tn 8)

    ;; Restore the new-stack.
    (inst xor index index)
    ;; First the stack-pointer.
    (inst mov esp-tn
	  (make-ea :dword :base new-stack :index index :scale 4
		   :disp (- (* vm:vector-data-offset vm:word-bytes)
			    vm:other-pointer-type)))
    (inst inc index)
    (inst mov stack (make-fixup (extern-alien-name "control_stack_end")
				:foreign))
    LOOP2
    (inst cmp stack esp-tn)
    (inst jmp :le STACK-RESTORE-DONE)
    (inst sub stack 4)
    (inst mov temp (make-ea :dword :base new-stack :index index :scale 4
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))
    (inst mov (make-ea :dword :base stack) temp)
    (inst inc index)
    (inst jmp-short LOOP2)
    STACK-RESTORE-DONE
    ;; Pop the frame pointer, and resume at the return address.
    (inst pop ebp-tn)
    (inst ret)
    
    ;; Original thread resumes, stack has been cleaned up.
    RETURN))


(export 'control-stack-return)
(defknown control-stack-return ((simple-array (unsigned-byte 32) (*)))
  (values))

(define-vop (control-stack-return)
  (:policy :fast-safe)
  (:translate control-stack-return)
  (:args (new-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) index)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) stack)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) temp)
  (:save-p t)
  (:generator 25
    ;; Restore the new-stack.
    (inst xor index index)
    ;; First the stack-pointer.
    (inst mov esp-tn
	  (make-ea :dword :base new-stack :index index :scale 4
		   :disp (- (* vm:vector-data-offset vm:word-bytes)
			    vm:other-pointer-type)))
    (inst inc index)
    (inst mov stack (make-fixup (extern-alien-name "control_stack_end")
				:foreign))
    LOOP
    (inst cmp stack esp-tn)
    (inst jmp :le STACK-RESTORE-DONE)
    (inst sub stack 4)
    (inst mov temp (make-ea :dword :base new-stack :index index :scale 4
			    :disp (- (* vm:vector-data-offset vm:word-bytes)
				     vm:other-pointer-type)))
    (inst mov (make-ea :dword :base stack) temp)
    (inst inc index)
    (inst jmp-short LOOP)
    STACK-RESTORE-DONE
    ;; Pop the frame pointer, and resume at the return address.
    (inst pop ebp-tn)
    (inst ret)))
