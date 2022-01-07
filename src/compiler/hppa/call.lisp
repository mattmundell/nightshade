;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/call.lisp,v 1.11 1998/03/04 14:53:33 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition of function call for the HPPA.
;;;
;;; Written by William Lott.
;;;
(in-package "HPPA")


;;;; Interfaces to IR2 conversion:

;;; Standard-Argument-Location  --  Interface
;;;
;;;    Return a wired TN describing the N'th full call argument passing
;;; location.
;;;
(def-vm-support-routine standard-argument-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *any-primitive-type*
		     register-arg-scn
		     (elt register-arg-offsets n))
      (make-wired-tn *any-primitive-type*
		     control-stack-arg-scn n)))


;;; Make-Return-PC-Passing-Location  --  Interface
;;;
;;;    Make a passing location TN for a local call return PC.  If standard is
;;; true, then use the standard (full call) location, otherwise use any legal
;;; location.  Even in the non-standard case, this may be restricted by a
;;; desire to use a subroutine call instruction.
;;;
(def-vm-support-routine make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn lra-offset)
      (make-restricted-tn *any-primitive-type* register-arg-scn)))

;;; Make-Old-FP-Passing-Location  --  Interface
;;;
;;;    Similar to Make-Return-PC-Passing-Location, but makes a location to pass
;;; Old-FP in.  This is (obviously) wired in the standard convention, but is
;;; totally unrestricted in non-standard conventions, since we can always fetch
;;; it off of the stack using the arg pointer.
;;;
(def-vm-support-routine make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make-Old-FP-Save-Location, Make-Return-PC-Save-Location  --  Interface
;;;
;;;    Make the TNs used to hold Old-FP and Return-PC within the current
;;; function.  We treat these specially so that the debugger can find them at a
;;; known location.
;;;
(def-vm-support-routine make-old-fp-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type*
		  control-stack-arg-scn
		  ocfp-save-offset)))
;;;
(def-vm-support-routine make-return-pc-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *any-primitive-type*) env)
   (make-wired-tn *any-primitive-type*
		  control-stack-arg-scn
		  lra-save-offset)))

;;; Make-Argument-Count-Location  --  Interface
;;;
;;;    Make a TN for the standard argument count passing location.  We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
;;;
(def-vm-support-routine make-argument-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))


;;; MAKE-NFP-TN  --  Interface
;;;
;;;    Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
;;;
(def-vm-support-routine make-nfp-tn ()
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset)))

;;; MAKE-STACK-POINTER-TN ()
;;; 
(def-vm-support-routine make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; MAKE-NUMBER-STACK-POINTER-TN ()
;;; 
(def-vm-support-routine make-number-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; Make-Unknown-Values-Locations  --  Interface
;;;
;;;    Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
;;;
(def-vm-support-routine make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
	(make-normal-tn *fixnum-primitive-type*)))


;;; Select-Component-Format  --  Interface
;;;
;;;    This function is called by the Entry-Analyze phase, allowing
;;; VM-dependent initialization of the IR2-Component structure.  We push
;;; placeholder entries in the Constants to leave room for additional
;;; noise in the code object header.
;;;
(def-vm-support-routine select-component-format (component)
  (declare (type component component))
  (dotimes (i code-constants-offset)
    (vector-push-extend nil
			(ir2-component-constants (component-info component))))
  (undefined-value))


;;;; Frame hackery:

;;; BYTES-NEEDED-FOR-NON-DESCRIPTOR-STACK-FRAME -- internal
;;;
;;; Return the number of bytes needed for the current non-descriptor stack.
;;; We have to allocate multiples of 64 bytes.
;;; 
(defun bytes-needed-for-non-descriptor-stack-frame ()
  (logandc2 (+ (* (sb-allocated-size 'non-descriptor-stack) word-bytes) 63)
	    63))

;;; Used for setting up the Old-FP in local call.
;;;
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (move cfp-tn val)))

;;; Used for computing the caller's NFP for use in known-values return.  Only
;;; works assuming there is no variable size stuff on the nstack.
;;;
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
	(inst addi (- (bytes-needed-for-non-descriptor-stack-frame))
	      nfp val)))))

(define-vop (xep-allocate-frame)
  (:info start-lab copy-more-arg-follows)
  (:ignore copy-more-arg-follows)
  (:vop-var vop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (align lowtag-bits)
    (trace-table-entry trace-table-function-prologue)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst function-header-word)
    (dotimes (i (1- function-code-offset))
      (inst word 0))
    ;; The start of the actual code.
    ;; Fix CODE, cause the function object was passed in.
    (let ((entry-point (gen-label)))
      (emit-label entry-point)
      (inst compute-code-from-fn lip-tn entry-point temp code-tn))
    ;; Build our stack frames.
    (inst addi (* vm:word-bytes (sb-allocated-size 'control-stack))
	  cfp-tn csp-tn)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
	(move nsp-tn nfp)
	(inst addi (bytes-needed-for-non-descriptor-stack-frame)
	      nsp-tn nsp-tn)))
    (trace-table-entry trace-table-normal)))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
	    (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2
    (move csp-tn res)
    (inst addi (* vm:word-bytes (sb-allocated-size 'control-stack))
	  csp-tn csp-tn)
    (when (ir2-environment-number-stack-p callee)
      (move nsp-tn nfp)
      (inst addi (bytes-needed-for-non-descriptor-stack-frame)
	    nsp-tn nsp-tn))))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
;;;
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (when (> nargs register-arg-count)
      (move csp-tn res)
      (inst addi (* nargs vm:word-bytes) csp-tn csp-tn))))


;;; Default-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values.  Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into.  Nvals is the number of
;;; values that are to be received (should equal the length of Values).
;;;
;;;    Move-Temp is a Descriptor-Reg TN used as a temporary.
;;;
;;;    This code exploits the fact that in the unknown-values convention, a
;;; single value return returns at the return PC + 8, whereas a return of other
;;; than one value returns directly at the return PC.
;;;
;;;    If 0 or 1 values are expected, then we just emit an instruction to reset
;;; the SP (which will only be executed when other than 1 value is returned.)
;;;
;;; In the general case, we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     called in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
;;;
;;; The general-case code looks like this:
#|
	b regs-defaulted		; Skip if MVs
	nop

	move a1 null-tn			; Default register values
	...
	loadi nargs 1			; Force defaulting of stack values
	move old-fp csp			; Set up args for SP resetting

regs-defaulted
	subu temp nargs register-arg-count

	bltz temp default-value-7	; jump to default code
        addu temp temp -1
	loadw move-temp old-fp-tn 6	; Move value to correct location.
	store-stack-tn val4-tn move-temp

	bltz temp default-value-8
        addu temp temp -1
	loadw move-temp old-fp-tn 7
	store-stack-tn val5-tn move-temp

	...

defaulting-done
	move sp old-fp			; Reset SP.
<end of code>

<elsewhere>
default-value-7
	store-stack-tn val4-tn null-tn	; Nil out 7'th value. (first on stack)

default-value-8
	store-stack-tn val5-tn null-tn	; Nil out 8'th value.

	...

	br defaulting-done
        nop
|#
;;;
(defun default-unknown-values (vop values nvals move-temp temp lra-label)
  (declare (type (or tn-ref null) values)
	   (type unsigned-byte nvals) (type tn move-temp temp))
  (cond
   ((<= nvals 1)
    (assemble ()
      ;; Note that this is a single-value return point.  This is actually
      ;; the multiple-value entry point for a single desired value, but
      ;; the code location has to be here, or the debugger backtrace
      ;; gets confused.
      (note-this-location vop :single-value-return)
      (move ocfp-tn csp-tn)
      (inst compute-code-from-lra code-tn lra-label temp code-tn)))
   ((<= nvals register-arg-count)
    (assemble ()
      ;; Note that this is an unknown-values return point.
      (note-this-location vop :unknown-return)
      ;; Branch off to the MV case.
      (inst b regs-defaulted :nullify t)
	
      ;; Default any unsupplied values.
      (do ((val (tn-ref-across values) (tn-ref-across val)))
	  ((null val))
	(inst move null-tn (tn-ref-tn val)
	      (if (tn-ref-across val)
		  :never
		  :tr)))

      REGS-DEFAULTED

      ;; Clear the stack.  Note: the last move in the single value reg
      ;; defaulting nullifies this, so this only happens in the mv case.
      (move ocfp-tn csp-tn)

      ;; Fix CODE.
      (inst compute-code-from-lra code-tn lra-label temp code-tn)))
   (t
    (collect ((defaults))
      (assemble (nil nil :labels (default-stack-vals))
	;; Note that this is an unknown-values return point.
	(note-this-location vop :unknown-return)
	;; Branch off to the MV case.
	(inst b regs-defaulted :nullify t)
	
	;; Default any unsupplied register values.
	(do ((i 1 (1+ i))
	     (val (tn-ref-across values) (tn-ref-across val)))
	    ((= i register-arg-count))
	  (inst move null-tn (tn-ref-tn val)))
	(inst b default-stack-vals)
	(move ocfp-tn csp-tn)
	
	REGS-DEFAULTED
	
	(do ((i register-arg-count (1+ i))
	     (val (do ((i 0 (1+ i))
		       (val values (tn-ref-across val)))
		      ((= i register-arg-count) val))
		  (tn-ref-across val)))
	    ((null val))
	  
	  (let ((default-lab (gen-label))
		(tn (tn-ref-tn val)))
	    (defaults (cons default-lab tn))
	    (inst bci :>= nil (fixnum i) nargs-tn default-lab)
	    (loadw move-temp ocfp-tn i)
	    (store-stack-tn tn move-temp)))
	
	DEFAULTING-DONE
	(move ocfp-tn csp-tn)
	(inst compute-code-from-lra code-tn lra-label temp code-tn)
	
	(let ((defaults (defaults)))
	  (assert defaults)
	  (assemble (*elsewhere*)
	    (trace-table-entry trace-table-call-site)
	    DEFAULT-STACK-VALS
	    (do ((remaining defaults (cdr remaining)))
		((null remaining))
	      (let ((def (car remaining)))
		(emit-label (car def))
		(when (null (cdr remaining))
		  (inst b defaulting-done))
		(store-stack-tn (cdr def) null-tn)))
	    (trace-table-entry trace-table-normal)))))))
  (undefined-value))


;;;; Unknown values receiving:

;;; Receive-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return point for an unknown-values call for an
;;; arbitrary number of values.
;;;
;;;    We do the single and non-single cases with no shared code: there doesn't
;;; seem to be any potential overlap, and receiving a single value is more
;;; important efficiency-wise.
;;;
;;;    When there is a single value, we just push it on the stack, returning
;;; the old SP and 1.
;;;
;;;    When there is a variable number of values, we move all of the argument
;;; registers onto the stack, and return Args and Nargs.
;;;
;;;    Args and Nargs are TNs wired to the named locations.  We must
;;; explicitly allocate these TNs, since their lifetimes overlap with the
;;; results Start and Count (also, it's nice to be able to target them).
;;;
(defun receive-unknown-values (args nargs start count lra-label temp)
  (declare (type tn args nargs start count temp))
  (assemble (nil nil :labels (variable-values))
    (inst b variable-values :nullify t)
      
    (inst compute-code-from-lra code-tn lra-label temp code-tn)
    (inst move csp-tn start)
    (inst stwm (first register-arg-tns) word-bytes csp-tn)
    (inst li (fixnum 1) count)
    
    DONE
    
    (assemble (*elsewhere*)
      (trace-table-entry trace-table-call-site)
      VARIABLE-VALUES
      (inst compute-code-from-lra code-tn lra-label temp code-tn)
      (do ((arg register-arg-tns (rest arg))
	   (i 0 (1+ i)))
	  ((null arg))
	(storew (first arg) args i))
      (move args start)
      (move nargs count)
      (inst b done :nullify t)
      (trace-table-entry trace-table-normal)))
  (undefined-value))

;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:results (start :scs (any-reg))
	    (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset ocfp-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset nargs-offset
	       :from :eval :to (:result 1))
	      nvals)
  (:temporary (:scs (non-descriptor-reg)) temp))



;;;; Local call with unknown values convention return:

;;; Non-TR local call for a fixed number of values passed according to the
;;; unknown values convention.
;;;
;;; Args are the argument passing locations, which are specified only to
;;; terminate their lifetimes in the caller.
;;;
;;; Values are the return value locations (wired to the standard passing
;;; locations).
;;;
;;; Save is the save info, which we can ignore since saving has been done.
;;; Return-PC is the TN that the return PC should be passed in.
;;; Target is a continuation pointing to the start of the called function.
;;; Nvals is the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (call-local)
  (:args (cfp)
	 (nfp)
	 (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:temporary (:scs (descriptor-reg) :from :eval) move-temp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset ocfp-offset :from :eval) ocfp)
  (:ignore arg-locs args ocfp)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn cfp)
      (inst compute-lra-from-code code-tn label temp
	    (callee-return-pc-tn callee))
      (note-this-location vop :call-site)
      (inst b target :nullify t)
      (emit-return-pc label)
      (default-unknown-values vop values nvals move-temp temp label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))
    (trace-table-entry trace-table-normal)))

;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (cfp)
	 (nfp)
	 (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 20
    (trace-table-entry trace-table-call-site)
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn cfp)
      (inst compute-lra-from-code code-tn label temp
	    (callee-return-pc-tn callee))
      (note-this-location vop :call-site)
      (inst b target :nullify t)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label temp)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))
    (trace-table-entry trace-table-normal)))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-call-local)
  (:args (cfp)
	 (nfp)
	 (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn cfp)
      (inst compute-lra-from-code code-tn label temp
	    (callee-return-pc-tn callee))
      (note-this-location vop :call-site)
      (inst b target :nullify t)
      (emit-return-pc label)
      (note-this-location vop :known-return)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))
    (trace-table-entry trace-table-normal)))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-return)
  (:args (old-fp :target old-fp-temp)
	 (return-pc :target return-pc-temp)
	 (vals :more t))
  (:temporary (:sc any-reg :from (:argument 0)) old-fp-temp)
  (:temporary (:sc descriptor-reg :from (:argument 1)) return-pc-temp)
  (:temporary (:scs (interior-reg)) lip)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (maybe-load-stack-tn old-fp-temp old-fp)
    (maybe-load-stack-tn return-pc-temp return-pc)
    (move cfp-tn csp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move cur-nfp nsp-tn)))
    (inst addi (- word-bytes other-pointer-type) return-pc-temp lip)
    (inst bv lip)
    (move old-fp-temp cfp-tn)
    (trace-table-entry trace-table-normal)))


;;;; Full call:
;;;
;;;    There is something of a cross-product effect with full calls.  Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on the
;;; stack top and storing stack arguments into that frame.  On entry to the
;;; callee, this partial frame is pointed to by FP.  If there are no stack
;;; arguments, we don't bother allocating a partial frame, and instead set FP
;;; to SP just before the call.

;;; Define-Full-Call  --  Internal
;;;
;;;    This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;; 
;;; Named is true if the first argument is a symbol whose global function
;;; definition is to be called.
;;;
;;; Return is either :Fixed, :Unknown or :Tail:
;;; -- If :Fixed, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :Unknown, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :Tail, then do a tail-recursive call.  No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as the last
;;; fixed argument.  If Variable is false, then the passing locations are
;;; passed as a more arg.  Variable is true if there are a variable number of
;;; arguments passed on the stack.  Variable cannot be specified with :Tail
;;; return.  TR variable argument call is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
;;;
(eval-when (compile eval)
(defmacro define-full-call (name named return variable)
  (assert (not (and variable (eq return :tail))))
  `(define-vop (,name
		,@(when (eq return :unknown)
		    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
	  '((new-fp :scs (any-reg) :to :eval)))

      ,(if named
	   '(fdefn :target fdefn-pass)
	   '(arg-fun :target lexenv))
      
      ,@(when (eq return :tail)
	  '((ocfp :target ocfp-pass)
	    (lra :target lra-pass)))
      
      ,@(unless variable '((args :more t :scs (descriptor-reg)))))

     ,@(when (eq return :fixed)
	 '((:results (values :more t))))
   
     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail) variable)
	 '((:move-args :full-call)))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
	    ,@(unless variable '(nargs))
	    ,@(when (eq return :fixed) '(nvals)))

     (:ignore
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
		  :offset ocfp-offset
		  ,@(when (eq return :tail)
		      '(:from (:argument 1)))
		  ,@(unless (eq return :fixed)
		      '(:to :eval)))
		 ocfp-pass)

     (:temporary (:sc descriptor-reg
		  :offset lra-offset
		  ,@(when (eq return :tail)
		      '(:from (:argument 2)))
		  :to :eval)
		 lra-pass)

     ,@(if named
	 `((:temporary (:sc descriptor-reg :offset fdefn-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       fdefn-pass))

	 `((:temporary (:sc descriptor-reg :offset lexenv-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       lexenv)
	   (:temporary (:scs (descriptor-reg)
			     :from (:argument ,(if (eq return :tail) 2 1))
			     :to :eval)
		       function)))

     (:temporary (:sc any-reg :offset nargs-offset :to :eval)
		 nargs-pass)

     ,@(when variable
	 (mapcar #'(lambda (name offset)
		     `(:temporary (:sc descriptor-reg
				   :offset ,offset
				   :to :eval)
			 ,name))
		 register-arg-names register-arg-offsets))
     ,@(when (eq return :fixed)
	 '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)))

     ,@(unless (eq return :tail)
	 '((:temporary (:scs (non-descriptor-reg)) temp)
	   (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:temporary (:scs (interior-reg) :type interior) lip)

     (:generator ,(+ (if named 5 0)
		     (if variable 19 1)
		     (if (eq return :tail) 0 10)
		     15
		     (if (eq return :unknown) 25 0))
       (trace-table-entry trace-table-call-site)
       (let* ((cur-nfp (current-nfp-tn vop))
	      ,@(unless (eq return :tail)
		  '((lra-label (gen-label))))
	      (filler
	       (list :load-nargs
		     ,@(if (eq return :tail)
			   '((unless (location= ocfp ocfp-pass)
			       :load-ocfp)
			     (unless (location= lra lra-pass)
			       :load-lra)
			     (when cur-nfp
			       :frob-nfp))
			   '((when cur-nfp
			       :frob-nfp)
			     :comp-lra
			     :save-fp
			     :load-fp)))))
	 (labels
	     ((do-next-filler ()
		(when filler
		  (ecase (pop filler)
		    ((nil) (do-next-filler))
		    (:load-nargs
		     ,@(if variable
			   `((inst sub csp-tn new-fp nargs-pass)
			     ,@(let ((index -1))
				 (mapcar #'(lambda (name)
					     `(loadw ,name new-fp
						     ,(incf index)))
					 register-arg-names)))
			   '((inst li (fixnum nargs) nargs-pass))))
		    ,@(if (eq return :tail)
			  '((:load-ocfp
			     (sc-case ocfp
			       (any-reg
				(inst move ocfp ocfp-pass))
			       (control-stack
				(loadw ocfp-pass cfp-tn (tn-offset ocfp)))))
			    (:load-lra
			     (sc-case lra
			       (descriptor-reg
				(inst move lra lra-pass))
			       (control-stack
				(loadw lra-pass cfp-tn (tn-offset lra)))))
			    (:frob-nfp
			     (inst move cur-nfp nsp-tn)))
			  `((:frob-nfp
			     (store-stack-tn nfp-save cur-nfp))
			    (:comp-lra
			     (inst compute-lra-from-code
				   code-tn lra-label temp lra-pass))
			    (:save-fp
			     (inst move cfp-tn ocfp-pass))
			    (:load-fp
			     ,(if variable
				  '(move new-fp cfp-tn)
				  '(if (> nargs register-arg-count)
				       (move new-fp cfp-tn)
				       (move csp-tn cfp-tn))))))))))

	   ,@(if named
		 `((sc-case fdefn
		     (descriptor-reg (move fdefn fdefn-pass))
		     (control-stack
		      (loadw fdefn-pass cfp-tn (tn-offset fdefn))
		      (do-next-filler))
		     (constant
		      (loadw fdefn-pass code-tn (tn-offset fdefn)
			     other-pointer-type)
		      (do-next-filler)))
		   (loadw lip fdefn-pass fdefn-raw-addr-slot
			  other-pointer-type)
		   (do-next-filler))
		 `((sc-case arg-fun
		     (descriptor-reg (move arg-fun lexenv))
		     (control-stack
		      (loadw lexenv cfp-tn (tn-offset arg-fun))
		      (do-next-filler))
		     (constant
		      (loadw lexenv code-tn (tn-offset arg-fun)
			     other-pointer-type)
		      (do-next-filler)))
		   (loadw function lexenv closure-function-slot
			  function-pointer-type)
		   (do-next-filler)
		   (inst addi (- (ash function-code-offset word-shift)
				 function-pointer-type)
			 function lip)))
	   (loop
	     (cond ((null filler)
		    (return))
		   ((null (car filler))
		    (pop filler))
		   ((null (cdr filler))
		    (return))
		   (t
		    (do-next-filler))))
	   
	   (note-this-location vop :call-site)
	   (inst bv lip :nullify (null filler))
	   (do-next-filler))

	 ,@(ecase return
	     (:fixed
	      '((emit-return-pc lra-label)
		(default-unknown-values vop values nvals
					move-temp temp lra-label)
		(when cur-nfp
		  (load-stack-tn cur-nfp nfp-save))))
	     (:unknown
	      '((emit-return-pc lra-label)
		(note-this-location vop :unknown-return)
		(receive-unknown-values values-start nvals start count
					lra-label temp)
		(when cur-nfp
		  (load-stack-tn cur-nfp nfp-save))))
	     (:tail)))
       (trace-table-entry trace-table-normal))))
);eval-when

(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)


;;; Defined separately, since needs special code that BLT's the arguments
;;; down.
;;;
(define-vop (tail-call-variable)
  (:args (args-arg :scs (any-reg) :target args)
	 (function-arg :scs (descriptor-reg) :target lexenv)
	 (old-fp-arg :scs (any-reg) :target old-fp)
	 (lra-arg :scs (descriptor-reg) :target lra))

  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) old-fp)
  (:temporary (:sc any-reg :offset lra-offset :from (:argument 3)) lra)
  (:temporary (:scs (any-reg) :from (:argument 3)) tmp)

  (:vop-var vop)

  (:generator 75

    ;; Move these into the passing locations if they are not already there.
    (move args-arg args)
    (move function-arg lexenv)
    (move old-fp-arg old-fp)
    (move lra-arg lra)

    ;; Clear the number stack if anything is there.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst move cur-nfp nsp-tn)))

    ;; And jump to the assembly-routine that does the bliting.
    (let ((fixup (make-fixup 'tail-call-variable :assembly-routine)))
      (inst ldil fixup tmp)
      (inst be fixup lisp-heap-space tmp :nullify t))))


;;;; Unknown values return:

;;; Return a single value using the unknown-values convention.
;;; 
(define-vop (return-single)
  (:args (old-fp :scs (any-reg))
	 (return-pc :scs (descriptor-reg))
	 (value))
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (trace-table-entry trace-table-function-epilogue)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst move cur-nfp nsp-tn)))
    ;; Clear the control stack, and restore the frame pointer.
    (move cfp-tn csp-tn)
    (move old-fp cfp-tn)
    ;; Out of here.
    (lisp-return return-pc :offset 1)
    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of a fixed number of values.  The Values are
;;; required to be set up in the standard passing locations.  Nvals is the
;;; number of values returned.
;;;
;;; If returning a single value, then deallocate the current frame, restore
;;; FP and jump to the single-value entry at Return-PC + 8.
;;;
;;; If returning other than one value, then load the number of values returned,
;;; NIL out unsupplied values registers, restore FP and return at Return-PC.
;;; When there are stack values, we must initialize the argument pointer to
;;; point to the beginning of the values block (which is the beginning of the
;;; current frame.)
;;;
(define-vop (return)
  (:args
   (old-fp :scs (any-reg))
   (return-pc :scs (descriptor-reg) :to (:eval 1))
   (values :more t))
  (:ignore values)
  (:info nvals)
  (:temporary (:sc descriptor-reg :offset a0-offset :from (:eval 0)) a0)
  (:temporary (:sc descriptor-reg :offset a1-offset :from (:eval 0)) a1)
  (:temporary (:sc descriptor-reg :offset a2-offset :from (:eval 0)) a2)
  (:temporary (:sc descriptor-reg :offset a3-offset :from (:eval 0)) a3)
  (:temporary (:sc descriptor-reg :offset a4-offset :from (:eval 0)) a4)
  (:temporary (:sc descriptor-reg :offset a5-offset :from (:eval 0)) a5)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (trace-table-entry trace-table-function-epilogue)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst move cur-nfp nsp-tn)))
    ;; Establish the values pointer and values count.
    (move cfp-tn val-ptr)
    (inst li (fixnum nvals) nargs)
    ;; restore the frame pointer and clear as much of the control
    ;; stack as possible.
    (move old-fp cfp-tn)
    (inst addi (* nvals word-bytes) val-ptr csp-tn)
    ;; pre-default any argument register that need it.
    (when (< nvals register-arg-count)
      (dolist (reg (subseq (list a0 a1 a2 a3 a4 a5) nvals))
	(move null-tn reg)))
    ;; And away we go.
    (lisp-return return-pc)
    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls an assembly-routine.
;;;
(define-vop (return-multiple)
  (:args
   (old-fp-arg :scs (any-reg) :to (:eval 1))
   (lra-arg :scs (descriptor-reg) :to (:eval 1))
   (vals-arg :scs (any-reg) :target vals)
   (nvals-arg :scs (any-reg) :target nvals))

  (:temporary (:sc any-reg :offset nl1-offset :from (:argument 0)) old-fp)
  (:temporary (:sc descriptor-reg :offset lra-offset :from (:argument 1)) lra)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:sc descriptor-reg :offset a0-offset) a0)
  (:temporary (:scs (any-reg) :from (:eval 0)) tmp)

  (:vop-var vop)
  (:node-var node)

  (:generator 13
    (trace-table-entry trace-table-function-epilogue)
    ;; Clear the number stack.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst move cur-nfp nsp-tn)))

    (unless (policy node (> space speed))
      ;; Check for the single case.
      (inst comib :<> (fixnum 1) nvals-arg not-single)
      (loadw a0 vals-arg)

      ;; Return with one value.
      (move cfp-tn csp-tn)
      (move old-fp-arg cfp-tn)
      (lisp-return lra-arg :offset 1))
		
    ;; Nope, not the single case.
    NOT-SINGLE
    (move old-fp-arg old-fp)
    (move lra-arg lra)
    (move vals-arg vals)
    (move nvals-arg nvals)
    (let ((fixup (make-fixup 'return-multiple :assembly-routine)))
      (inst ldil fixup tmp)
      (inst be fixup lisp-heap-space tmp :nullify t))
    (trace-table-entry trace-table-normal)))



;;;; XEP hackery:

;;; We don't need to do anything special for regular functions.
;;;
(define-vop (setup-environment)
  (:info label)
  (:ignore label)
  (:generator 0
    ;; Don't bother doing anything.
    ))

;;; Get the lexical environment from it's passing location.
;;;
(define-vop (setup-closure-environment)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :target closure
	       :to (:result 0))
	      lexenv)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move lexenv closure)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments. 
;;;
(define-vop (copy-more-arg)
  (:temporary (:sc any-reg :offset nl0-offset) result)
  (:temporary (:sc any-reg :offset nl1-offset) count)
  (:temporary (:sc any-reg :offset nl2-offset) src)
  (:temporary (:sc any-reg :offset nl3-offset) dst)
  (:temporary (:sc descriptor-reg :offset l0-offset) temp)
  (:info fixed)
  (:generator 20
    ;; Figure out how many things we are going to copy.
    (unless (zerop fixed)
      (inst addi (- (fixnum fixed)) nargs-tn count))

    ;; Blow out of here if is nothing to copy.
    (inst comb :<= (if (zerop fixed) nargs-tn count) zero-tn done :nullify t)

    (when (< fixed register-arg-count)
      ;; Save a pointer to the results so we can fill in register args.
      ;; We don't need this if there are more fixed args than reg args.
      (move csp-tn result))

    ;; Allocate the space on the stack.
    (inst add csp-tn (if (zerop fixed) nargs-tn count) csp-tn)

    (when (< fixed register-arg-count)
      ;; We must stop when we run out of stack args, not when we run out of
      ;; args in general.
      (inst addi (fixnum (- register-arg-count)) nargs-tn count)
      ;; Everything of interest in registers.
      (inst comb :<= count zero-tn do-regs))
    ;; Initialize dst to be end of stack.
    (move csp-tn dst)

    ;; Initialize src to be end of args.
    (inst add cfp-tn nargs-tn src)

    LOOP
    ;; *--dst = *--src, --count
    (inst ldwm (- word-bytes) src temp)
    (inst addib :> (fixnum -1) count loop)
    (inst stwm temp (- word-bytes) dst)

    DO-REGS
    (when (< fixed register-arg-count)
      ;; Now we have to deposit any more args that showed up in registers.
      ;; We know there is at least one more arg, otherwise we would have
      ;; branched to done up at the top.
      (inst addi (fixnum (- fixed)) nargs-tn count)
      (do ((i fixed (1+ i)))
	  ((>= i register-arg-count))
	;; Is this the last one?
	(inst addib :<= (fixnum -1) count done)
	;; Store it relative to the pointer saved at the start.
	(storew (nth i register-arg-tns) result (- i fixed))))
    DONE))

;;; More args are stored consequtively on the stack, starting immediately at
;;; the context pointer.  The context pointer is not typed, so the lowtag is 0.
;;;
(define-full-reffer more-arg * 0 0 (descriptor-reg any-reg) * %more-arg)


;;; Turn more arg (context, count) into a list.
;;;
(define-vop (listify-rest-args)
  (:args (context-arg :target context :scs (descriptor-reg))
	 (count-arg :target count :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg) :from (:argument 0)) context)
  (:temporary (:scs (any-reg) :from (:argument 1)) count)
  (:temporary (:scs (descriptor-reg) :from :eval) temp)
  (:temporary (:scs (non-descriptor-reg) :from :eval) dst)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:generator 20
    (move context-arg context)
    (move count-arg count)
    ;; Check to see if there are any arguments.
    (inst comb := count zero-tn done)
    (move null-tn result)

    ;; We need to do this atomically.
    (pseudo-atomic ()
      (assemble ()
	;; Allocate a cons (2 words) for each item.
	(inst move alloc-tn result)
	(inst dep list-pointer-type 31 3 result)
	(move result dst)
	(inst sll count 1 temp)
	(inst add alloc-tn temp alloc-tn)
	
	LOOP
	;; Grab one value and stash it in the car of this cons.
	(inst ldwm word-bytes context temp)
	(storew temp dst 0 vm:list-pointer-type)
	
	;; Dec count, and if != zero, go back for more.
	(inst addi (* 2 vm:word-bytes) dst dst)
	(inst addib :> (fixnum -1) count loop :nullify t)
	(storew dst dst -1 list-pointer-type)
	
	;; NIL out the last cons.
	(storew null-tn dst -1 list-pointer-type)
	;; Clear out dst, because it points past the last cons.
	(move null-tn dst)))
    DONE))

;;; Return the location and size of the more arg glob created by Copy-More-Arg.
;;; Supplied is the total number of arguments supplied (originally passed in
;;; NARGS.)  Fixed is the number of non-rest arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at that
;;; time the environment is in a pretty brain-damaged state, preventing this
;;; info from being returned as values.  What we do is compute
;;; supplied - fixed, and return a pointer that many words below the current
;;; stack top.
;;;
(setf (info function source-transform 'c::%more-arg-context) nil)
;;;
(define-vop (more-arg-context)
  (:policy :fast-safe)
  (:translate c::%more-arg-context)
  (:args (supplied :scs (any-reg)))
  (:arg-types tagged-num (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
	    (count :scs (any-reg)))
  (:result-types t tagged-num)
  (:note "more-arg-context")
  (:generator 5
    (inst addi (fixnum (- fixed)) supplied count)
    (inst sub csp-tn count context)))


;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:policy :fast-safe)
  (:translate c::%verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t))
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
	   (generate-error-code vop invalid-argument-count-error nargs)))
      (cond ((zerop count)
	     (inst bc :<> nil nargs zero-tn err-lab))
	    (t
	     (inst bci :<> nil (fixnum count) nargs err-lab))))))

;;; Signal an argument count error.
;;;
(macrolet ((frob (name error translate &rest args)
	     `(define-vop (,name)
		,@(when translate
		    `((:policy :fast-safe)
		      (:translate ,translate)))
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1000
		  (error-call vop ,error ,@args)))))
  (frob argument-count-error invalid-argument-count-error
    c::%argument-count-error nargs)
  (frob type-check-error object-not-type-error c::%type-check-error
    object type)
  (frob layout-invalid-error layout-invalid-error c::%layout-invalid-error
    object layout)
  (frob odd-keyword-arguments-error odd-keyword-arguments-error
    c::%odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error unknown-keyword-argument-error
    c::%unknown-keyword-argument-error key)
  (frob nil-function-returned-error nil-function-returned-error nil fun))
