;;; The VM definition of function call for the Alpha.

(in-package "ALPHA")


;;;; Interfaces to IR2 conversion.

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
  #+gengc (declare (ignore standard))
  #-gengc
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn lra-offset)
      (make-restricted-tn *any-primitive-type* register-arg-scn))
  #+gengc
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ra-offset))

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
  (let ((ptype #-gengc *any-primitive-type*
	       #+gengc *fixnum-primitive-type*))
    (specify-save-tn
     (environment-debug-live-tn (make-normal-tn ptype) env)
     (make-wired-tn ptype control-stack-arg-scn
		    #-gengc lra-save-offset #+gengc ra-save-offset))))

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


;;;; Frame hackery.

;;; BYTES-NEEDED-FOR-NON-DESCRIPTOR-STACK-FRAME -- internal
;;;
;;; Return the number of bytes needed for the current non-descriptor stack
;;; frame.  Non-descriptor stack frames must be multiples of 8 bytes on
;;; the PMAX.
;;;
(defun bytes-needed-for-non-descriptor-stack-frame ()
  (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
     word-bytes))

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
	(inst addq nfp (bytes-needed-for-non-descriptor-stack-frame) val)))))

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
      (inst lword 0))
    ;; The start of the actual code.
    ;; Compute CODE from the address of this entry point.
    (let ((entry-point (gen-label)))
      (emit-label entry-point)
      (inst compute-code-from-fn code-tn lip-tn entry-point temp)
      ;; ### We should also save it on the stack so that the garbage collector
      ;; won't forget about us if we call anyone else.
      )
    ;; Build our stack frames.
    (inst lda csp-tn (* word-bytes (sb-allocated-size 'control-stack)) cfp-tn)
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
	(inst subq nsp-tn (bytes-needed-for-non-descriptor-stack-frame)
	      nsp-tn)
	(move nsp-tn nfp)))
    (trace-table-entry trace-table-normal)))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
	    (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2
    (trace-table-entry trace-table-function-prologue)
    (move csp-tn res)
    (inst lda csp-tn (* word-bytes (sb-allocated-size 'control-stack)) csp-tn)
    (when (ir2-environment-number-stack-p callee)
      (inst subq nsp-tn (bytes-needed-for-non-descriptor-stack-frame)
	    nsp-tn)
      (move nsp-tn nfp))
    (trace-table-entry trace-table-normal)))

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
      (inst lda csp-tn (* nargs word-bytes) csp-tn))))


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
	move ocfp csp			; Set up args for SP resetting

regs-defaulted
	subu temp nargs register-arg-count

	bltz temp default-value-7	; jump to default code
        addu temp temp -1
	loadw move-temp ocfp-tn 6	; Move value to correct location.
	store-stack-tn val4-tn move-temp

	bltz temp default-value-8
        addu temp temp -1
	loadw move-temp ocfp-tn 7
	store-stack-tn val5-tn move-temp

	...

defaulting-done
	move sp ocfp			; Reset SP.
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
  (if (<= nvals 1)
      (progn
	;; Note that this is a single-value return point.  This is actually
	;; the multiple-value entry point for a single desired value, but
	;; the code location has to be here, or the debugger backtrace
	;; gets confused.
	(without-scheduling ()
	  (note-this-location vop :single-value-return)
	  (move ocfp-tn csp-tn)
	  (inst nop))
	(when lra-label
	  #-gengc (inst compute-code-from-lra code-tn code-tn lra-label temp)
	  #+gengc (inst compute-code-from-ra code-tn ra-tn lra-label temp)))
      (let ((regs-defaulted (gen-label))
	    (defaulting-done (gen-label))
	    (default-stack-vals (gen-label)))
	(without-scheduling ()
	  ;; Note that this is an unknown-values return point.
	  (note-this-location vop :unknown-return)
	  ;; If there are no stack results, clear the stack now.
	  (if (> nvals register-arg-count)
	      (inst subq nargs-tn (fixnum register-arg-count) temp)
	      (move ocfp-tn csp-tn))
	  ;; Branch off to the MV case.
	  (inst br zero-tn regs-defaulted))

	;; Do the single value calse.
	(do ((i 1 (1+ i))
	     (val (tn-ref-across values) (tn-ref-across val)))
	    ((= i (min nvals register-arg-count)))
	  (move null-tn (tn-ref-tn val)))
	(when (> nvals register-arg-count)
	  (move csp-tn ocfp-tn)
	  (inst br zero-tn default-stack-vals))

	(emit-label regs-defaulted)

	(when (> nvals register-arg-count)
	  ;; If there are stack results, we have to default them
	  ;; and clear the stack.
	  (collect ((defaults))
	    (do ((i register-arg-count (1+ i))
		 (val (do ((i 0 (1+ i))
			   (val values (tn-ref-across val)))
			  ((= i register-arg-count) val))
		      (tn-ref-across val)))
		((null val))

	      (let ((default-lab (gen-label))
		    (tn (tn-ref-tn val)))
		(defaults (cons default-lab tn))

		(inst blt temp default-lab)
		(inst ldl move-temp (* i word-bytes) ocfp-tn)
		(inst subq temp (fixnum 1) temp)
		(store-stack-tn tn move-temp)))

	    (emit-label defaulting-done)
	    (move ocfp-tn csp-tn)

	    (let ((defaults (defaults)))
	      (assert defaults)
	      (assemble (*elsewhere*)
		(emit-label default-stack-vals)
		(do ((remaining defaults (cdr remaining)))
		    ((null remaining))
		  (let ((def (car remaining)))
		    (emit-label (car def))
		    (when (null (cdr remaining))
		      (inst br zero-tn defaulting-done))
		    (store-stack-tn (cdr def) null-tn)))))))

	(when lra-label
	  #-gengc (inst compute-code-from-lra code-tn code-tn lra-label temp)
	  #+gengc (inst compute-code-from-ra code-tn ra-tn lra-label temp))))
  (undefined-value))


;;;; Unknown values receiving.

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
  (let ((variable-values (gen-label))
	(done (gen-label)))
    (without-scheduling ()
      (inst br zero-tn variable-values)
      (inst nop))

    (when lra-label
      #-gengc (inst compute-code-from-lra code-tn code-tn lra-label temp)
      #+gengc (inst compute-code-from-ra code-tn ra-tn lra-label temp))
    (inst addq csp-tn 4 csp-tn)
    (storew (first register-arg-tns) csp-tn -1)
    (inst subq csp-tn 4 start)
    (inst li (fixnum 1) count)

    (emit-label done)

    (assemble (*elsewhere*)
      (emit-label variable-values)
      (when lra-label
	#-gengc (inst compute-code-from-lra code-tn code-tn lra-label temp)
	#+gengc (inst compute-code-from-ra code-tn ra-tn lra-label temp))
      (do ((arg register-arg-tns (rest arg))
	   (i 0 (1+ i)))
	  ((null arg))
	(storew (first arg) args i))
      (move args start)
      (move nargs count)
      (inst br zero-tn done)))
  (undefined-value))

;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset ocfp-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset nargs-offset
	       :from :eval :to (:result 1))
	      nvals)
  (:temporary (:scs (non-descriptor-reg)) temp))


;;;; Local call with unknown values convention return.

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
  (:args (fp)
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
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(maybe-load-stack-nfp-tn callee-nfp nfp temp))
      (maybe-load-stack-tn cfp-tn fp)
      (trace-table-entry trace-table-call-site)
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label temp)
      (note-this-location vop :call-site)
      (inst br zero-tn target)
      (trace-table-entry trace-table-normal)
      (emit-return-pc label)
      (default-unknown-values vop values nvals move-temp temp label)
      (maybe-load-stack-nfp-tn cur-nfp nfp-save temp))))

;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 20
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(maybe-load-stack-nfp-tn callee-nfp nfp temp))
      (maybe-load-stack-tn cfp-tn fp)
      (trace-table-entry trace-table-call-site)
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label temp)
      (note-this-location vop :call-site)
      (inst bsr zero-tn target)
      (trace-table-entry trace-table-normal)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label temp)
      (maybe-load-stack-nfp-tn cur-nfp nfp-save temp))))


;;;; Local call with known values return.

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-call-local)
  (:args (fp)
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
    (let (#-gengc (label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(maybe-load-stack-nfp-tn callee-nfp nfp temp))
      (maybe-load-stack-tn cfp-tn fp)
      (trace-table-entry trace-table-call-site)
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label temp)
      (note-this-location vop :call-site)
      (inst bsr zero-tn target)
      (trace-table-entry trace-table-normal)
      (emit-return-pc label)
      (note-this-location vop :known-return)
      (maybe-load-stack-nfp-tn cur-nfp nfp-save temp))))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-return)
  (:args (ocfp :target ocfp-temp)
	 (return-pc :target return-pc-temp)
	 (vals :more t))
  (:temporary (:sc any-reg :from (:argument 0)) ocfp-temp)
  (:temporary (:sc #-gengc descriptor-reg #+gengc any-reg :from (:argument 1))
	      return-pc-temp)
  #-gengc (:temporary (:scs (interior-reg)) lip)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (maybe-load-stack-tn ocfp-temp ocfp)
    (maybe-load-stack-tn return-pc-temp return-pc)
    (move cfp-tn csp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst addq cur-nfp (bytes-needed-for-non-descriptor-stack-frame)
	      nsp-tn)))
    (inst subq return-pc-temp (- other-pointer-type word-bytes) lip)
    (move ocfp-temp cfp-tn)
    (inst ret zero-tn lip 1)
    (trace-table-entry trace-table-normal)))


;;;; Full call.
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
;;;    The Ocfp and Return-PC are passed as the second and third arguments.
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
(defmacro define-full-call (name named return variable)
  (assert (not (and variable (eq return :tail))))
  `(define-vop (,name
		,@(when (eq return :unknown)
		    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
	  '((new-fp :scs (any-reg) :to :eval)))

      ,(if named
	   '(name :target name-pass)
	   '(arg-fun :target lexenv))

      ,@(when (eq return :tail)
	  '((ocfp :target ocfp-pass)
	    (return-pc :target return-pc-pass)))

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

     (:ignore #+gengc ,@(unless (eq return :tail) '(return-pc-pass))
	      ,@(unless (or variable (eq return :tail)) '(arg-locs))
	      ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
		  :offset ocfp-offset
		  :from (:argument 1)
		  ,@(unless (eq return :fixed)
		      '(:to :eval)))
		 ocfp-pass)

     (:temporary (:sc descriptor-reg
		  :offset #-gengc lra-offset #+gengc ra-offset
		  :from (:argument ,(if (eq return :tail) 2 1))
		  :to :eval)
		 return-pc-pass)

     ,@(if named
	 `((:temporary (:sc descriptor-reg :offset fdefn-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       name-pass))

	 `((:temporary (:sc descriptor-reg :offset lexenv-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       lexenv)
	   #-gengc
	   (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
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

     (:temporary (:sc interior-reg :offset lip-offset) entry-point)

     (:generator ,(+ (if named 5 0)
		     (if variable 19 1)
		     (if (eq return :tail) 0 10)
		     15
		     (if (eq return :unknown) 25 0))
       (let* ((cur-nfp (current-nfp-tn vop))
	      ,@(unless (eq return :tail)
		  '((lra-label (gen-label))))
	      (filler
	       (remove nil
		       (list :load-nargs
			     ,@(if (eq return :tail)
				   '((unless (location= ocfp ocfp-pass)
				       :load-ocfp)
				     (unless (location= return-pc
							return-pc-pass)
				       :load-return-pc)
				     (when cur-nfp
				       :frob-nfp))
				   '(#-gengc
				     :comp-lra
				     (when cur-nfp
				       :frob-nfp)
				     :save-fp
				     :load-fp))))))
	 (flet ((do-next-filler ()
		  (let* ((next (pop filler))
			 (what (if (consp next) (car next) next)))
		    (ecase what
		      (:load-nargs
		       ,@(if variable
			     `((inst subq csp-tn new-fp nargs-pass)
			       ,@(let ((index -1))
				   (mapcar #'(lambda (name)
					       `(inst ldl ,name
						      ,(ash (incf index)
							    word-shift)
						      new-fp))
					   register-arg-names)))
			     '((inst li (fixnum nargs) nargs-pass))))
		      ,@(if (eq return :tail)
			    '((:load-ocfp
			       (sc-case ocfp
				 (any-reg
				  (inst move ocfp ocfp-pass))
				 (control-stack
				  (inst ldl ocfp-pass
					(ash (tn-offset ocfp)
					     word-shift)
					cfp-tn))))
			      (:load-return-pc
			       (sc-case return-pc
				 (#-gengc descriptor-reg #+gengc any-reg
				  (inst move return-pc return-pc-pass))
				 (control-stack
				  (inst ldl return-pc-pass
					(ash (tn-offset return-pc)
					     word-shift)
					 cfp-tn))))
			      (:frob-nfp
			       (inst addq cur-nfp
				     (bytes-needed-for-non-descriptor-stack-frame)
				     nsp-tn)))
			    `(#-gengc
			      (:comp-lra
			       (inst compute-lra-from-code
				     return-pc-pass code-tn lra-label temp))
			      (:frob-nfp
			       (store-stack-tn nfp-save cur-nfp))
			      (:save-fp
			       (inst move cfp-tn ocfp-pass))
			      (:load-fp
			       ,(if variable
				    '(move new-fp cfp-tn)
				    '(if (> nargs register-arg-count)
					 (move new-fp cfp-tn)
					 (move csp-tn cfp-tn)))
			       (trace-table-entry trace-table-call-site))))
		      ((nil))))))

	   ,@(if named
		 `((sc-case name
		     (descriptor-reg (move name name-pass))
		     (control-stack
		      (inst ldl name-pass
			    (ash (tn-offset name) word-shift) cfp-tn)
		      (do-next-filler))
		     (constant
		      (inst ldl name-pass
			    (- (ash (tn-offset name) word-shift)
			       other-pointer-type) code-tn)
		      (do-next-filler)))
		   (inst ldl entry-point
			 (- (ash fdefn-raw-addr-slot word-shift)
			    other-pointer-type) name-pass)
		   (do-next-filler))
		 `((sc-case arg-fun
		     (descriptor-reg (move arg-fun lexenv))
		     (control-stack
		      (inst ldl lexenv
			    (ash (tn-offset arg-fun) word-shift) cfp-tn)
		      (do-next-filler))
		     (constant
		      (inst ldl lexenv
			    (- (ash (tn-offset arg-fun) word-shift)
			       other-pointer-type) code-tn)
		      (do-next-filler)))
		   #-gengc
		   (inst ldl function
			 (- (ash closure-function-slot word-shift)
			    function-pointer-type) lexenv)
		   #-gengc
		   (do-next-filler)
		   #-gengc
		   (inst addq function
			 (- (ash function-code-offset word-shift)
			    function-pointer-type) entry-point)
		   #+gengc
		   (inst ldl entry-point
			 (- (ash closure-entry-point-slot word-shift)
			    function-pointer-type) lexenv)
		   #+gengc
		   (do-next-filler)))
	   (loop
	     (if (cdr filler)
		 (do-next-filler)
		 (return)))

	   (note-this-location vop :call-site)
	   (do-next-filler)
	   (inst jsr zero-tn entry-point))

	 ,@(ecase return
	     (:fixed
	      '((trace-table-entry trace-table-normal)
		(emit-return-pc lra-label)
		(default-unknown-values vop values nvals
					move-temp temp lra-label)
		(maybe-load-stack-nfp-tn cur-nfp nfp-save temp)))
	     (:unknown
	      '((trace-table-entry trace-table-normal)
		(emit-return-pc lra-label)
		(note-this-location vop :unknown-return)
		(receive-unknown-values values-start nvals start count
					lra-label temp)
		(maybe-load-stack-nfp-tn cur-nfp nfp-save temp)))
	     (:tail))))))

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
  (:args
   (args-arg :scs (any-reg) :target args)
   (function-arg :scs (descriptor-reg) :target lexenv)
   (ocfp-arg :scs (any-reg) :target ocfp)
   (lra-arg :scs (#-gengc descriptor-reg #+gengc any-reg) :target lra))

  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) ocfp)
  (:temporary (:sc any-reg :offset #-gengc lra-offset #+gengc ra-offset
		   :from (:argument 3)) lra)
  (:temporary (:scs (non-descriptor-reg)) temp)

  (:vop-var vop)

  (:generator 75

    ;; Move these into the passing locations if they are not already there.
    (move args-arg args)
    (move function-arg lexenv)
    (move ocfp-arg ocfp)
    (move lra-arg lra)

    ;; Clear the number stack if anything is there.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst addq cur-nfp (bytes-needed-for-non-descriptor-stack-frame)
	      nsp-tn)))

    ;; And jump to the assembly-routine that does the bliting.
    (inst li (make-fixup 'tail-call-variable :assembly-routine) temp)
    (inst jmp zero-tn temp)))


;;;; Unknown values return.

;;; Return a single value using the unknown-values convention.
;;;
(define-vop (return-single)
  (:args (ocfp :scs (any-reg))
	 #-gengc (return-pc :scs (descriptor-reg))
	 #+gengc (return-pc :scs (any-reg) :target ra)
	 (value))
  (:ignore value)
  #-gengc (:temporary (:scs (interior-reg)) lip)
  #+gengc (:temporary (:sc any-reg :offset ra-offset :from (:argument 1)) ra)
  #+gengc (:temporary (:scs (any-reg) :from (:argument 1)) temp)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (trace-table-entry trace-table-function-epilogue)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst addq cur-nfp (bytes-needed-for-non-descriptor-stack-frame)
	      nsp-tn)))
    ;; Clear the control stack, and restore the frame pointer.
    (move cfp-tn csp-tn)
    (move ocfp cfp-tn)
    ;; Out of here.
    #-gengc (lisp-return return-pc lip :offset 2)
    #+gengc
    (progn
      (inst addq return-pc (* 2 word-bytes) temp)
      (unless (location= ra return-pc)
	(inst move ra return-pc))
      (inst ret zero-tn temp 1))
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
  (:args (ocfp :scs (any-reg))
	 (return-pc :scs (#-gengc descriptor-reg #+gengc any-reg) :to (:eval 1)
		    #+gengc :target #+gengc ra)
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
  #-gengc (:temporary (:scs (interior-reg)) lip)
  #+gengc (:temporary (:sc any-reg :offset ra-offset :from (:eval 1)) ra)
  (:vop-var vop)
  (:generator 6
    ;; Clear the number stack.
    (trace-table-entry trace-table-function-epilogue)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst addq cur-nfp (bytes-needed-for-non-descriptor-stack-frame)
	      nsp-tn)))
    ;; Establish the values pointer and values count.
    (move cfp-tn val-ptr)
    (inst li (fixnum nvals) nargs)
    ;; restore the frame pointer and clear as much of the control
    ;; stack as possible.
    (move ocfp cfp-tn)
    (inst addq val-ptr (* nvals word-bytes) csp-tn)
    ;; pre-default any argument register that need it.
    (when (< nvals register-arg-count)
      (dolist (reg (subseq (list a0 a1 a2 a3 a4 a5) nvals))
	(move null-tn reg)))
    ;; And away we go.
    (lisp-return return-pc lip)
    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls an assembly-routine.
;;;
(define-vop (return-multiple)
  (:args (ocfp-arg :scs (any-reg) :target ocfp)
	 #-gengc (lra-arg :scs (descriptor-reg) :target lra)
	 #+gengc (return-pc :scs (any-reg) :target ra)
	 (vals-arg :scs (any-reg) :target vals)
	 (nvals-arg :scs (any-reg) :target nvals))

  (:temporary (:sc any-reg :offset nl1-offset :from (:argument 0)) ocfp)
  #-gengc
  (:temporary (:sc descriptor-reg :offset lra-offset :from (:argument 1)) lra)
  #+gengc
  (:temporary (:sc any-reg :offset ra-offset :from (:argument 1)) ra)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:sc descriptor-reg :offset a0-offset) a0)
  (:temporary (:scs (non-descriptor-reg)) temp)
  #-gengc (:temporary (:scs (interior-reg)) lip)
  #+gengc (:temporary (:scs (any-reg) :from (:argument 0)) temp)

  (:vop-var vop)

  (:generator 13
    (trace-table-entry trace-table-function-epilogue)
    (let ((not-single (gen-label)))
      ;; Clear the number stack.
      (let ((cur-nfp (current-nfp-tn vop)))
	(when cur-nfp
	  (inst addq cur-nfp (bytes-needed-for-non-descriptor-stack-frame)
		nsp-tn)))

      ;; Check for the single case.
      (inst li (fixnum 1) a0)
      (inst cmpeq nvals-arg a0 temp)
      (inst ldl a0 0 vals-arg)
      (inst beq temp not-single)

      ;; Return with one value.
      (move cfp-tn csp-tn)
      (move ocfp-arg cfp-tn)
      (lisp-return lra-arg lip :offset 2)

      ;; Nope, not the single case.
      (emit-label not-single)
      (move ocfp-arg ocfp)
      (move lra-arg lra)
      (move vals-arg vals)
      (move nvals-arg nvals)
      (inst li (make-fixup 'return-multiple :assembly-routine) temp)
      (inst jmp zero-tn temp))
    (trace-table-entry trace-table-normal)))


;;;; XEP hackery.

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
  (:temporary (:sc any-reg :offset nl4-offset) dst)
  (:temporary (:sc descriptor-reg :offset l0-offset) temp)
  (:info fixed)
  (:generator 20
    (let ((loop (gen-label))
	  (do-regs (gen-label))
	  (done (gen-label)))
      (when (< fixed register-arg-count)
	;; Save a pointer to the results so we can fill in register args.
	;; We don't need this if there are more fixed args than reg args.
	(move csp-tn result))
      ;; Allocate the space on the stack.
      (cond ((zerop fixed)
	     (inst addq csp-tn nargs-tn csp-tn)
	     (inst beq nargs-tn done))
	    (t
	     (inst subq nargs-tn (fixnum fixed) count)
	     (inst ble count done)
	     (inst addq csp-tn count csp-tn)))
      (when (< fixed register-arg-count)
	;; We must stop when we run out of stack args, not when we run out of
	;; more args.
	(inst subq nargs-tn (fixnum register-arg-count) count))
      ;; Initialize dst to be end of stack.
      (move csp-tn dst)
      ;; Everything of interest in registers.
      (inst ble count do-regs)
      ;; Initialize src to be end of args.
      (inst addq cfp-tn nargs-tn src)

      (emit-label loop)
      ;; *--dst = *--src, --count
      (inst subq src word-bytes src)
      (inst subq count (fixnum 1) count)
      (loadw temp src)
      (inst subq dst word-bytes dst)
      (storew temp dst)
      (inst bgt count loop)

      (emit-label do-regs)
      (when (< fixed register-arg-count)
	;; Now we have to deposit any more args that showed up in registers.
	;; We know there is at least one more arg, otherwise we would have
	;; branched to done up at the top.
	(inst subq nargs-tn (fixnum (1+ fixed)) count)
	(do ((i fixed (1+ i)))
	    ((>= i register-arg-count))
	  ;; Store it relative to the pointer saved at the start.
	  (storew (nth i register-arg-tns) result (- i fixed))
	  ;; Is this the last one?
	  (inst beq count done)
	  ;; Decrement count.
	  (inst subq count (fixnum 1) count)))
      (emit-label done))))

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
  (:temporary (:scs (descriptor-reg) :from :eval) temp dst)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:generator 20
    (let ((enter (gen-label))
	  (loop (gen-label))
	  (done (gen-label)))
      (move context-arg context)
      (move count-arg count)
      ;; Check to see if there are any arguments.
      (move null-tn result)
      (inst beq count done)

      ;; We need to do this atomically.
      (pseudo-atomic ()
	;; Allocate a cons (2 words) for each item.
	(inst bis alloc-tn list-pointer-type result)
	(move result dst)
	(inst sll count 1 temp)
	(inst addq alloc-tn temp alloc-tn)
	(inst br zero-tn enter)

	;; Store the current cons in the cdr of the previous cons.
	(emit-label loop)
	(inst addq dst (* 2 word-bytes) dst)
	(storew dst dst -1 list-pointer-type)

	(emit-label enter)
	;; Grab one value.
	(loadw temp context)
	(inst addq context word-bytes context)

	;; Store the value in the car (in delay slot)
	(storew temp dst 0 list-pointer-type)

	;; Dec count, and if != zero, go back for more.
	(inst subq count (fixnum 1) count)
	(inst bne count loop)

	;; NIL out the last cons.
	(storew null-tn dst 1 list-pointer-type))
      (emit-label done))))

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
    (inst subq supplied (fixnum fixed) count)
    (inst subq csp-tn count context)))

;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:policy :fast-safe)
  (:translate c::%verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t))
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
	   (generate-error-code vop invalid-argument-count-error nargs)))
      (cond ((zerop count)
	     (inst bne nargs err-lab))
	    (t
	     (inst subq nargs (fixnum count) temp)
	     (inst bne temp err-lab))))))

;;; Various other error signalers.
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
