;;; The definitions of VOPs used for non-local exit (throw, lexical exit,
;;; etc.)

(in-package "HPPA")

;;; MAKE-NLX-SP-TN  --  Interface
;;;
;;; Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(def-vm-support-routine make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *fixnum-primitive-type* immediate-arg-scn)
   env))

;;; Make-NLX-Entry-Argument-Start-Location  --  Interface
;;;
;;; Make a TN for the argument count passing location for a non-local
;;; entry.
;;;
(def-vm-support-routine make-nlx-entry-argument-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))


;;; Save and restore dynamic environment.
;;;
;;; These VOPs are used in the reentered function to restore the
;;; appropriate dynamic environment.  Currently we only save the
;;; Current-Catch and binding stack pointer.  We don't need to save/restore
;;; the current unwind-protect, since unwind-protects are implicitly
;;; processed during unwinding.  If there were any additional stacks, then
;;; this would be the place to restore the top pointers.

;;; Make-Dynamic-State-TNs  --  Interface
;;;
;;; Return a list of TNs that can be used to snapshot the dynamic state for
;;; use with the Save/Restore-Dynamic-Environment VOPs.
;;;
(def-vm-support-routine make-dynamic-state-tns ()
  (make-n-tns 4 *any-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (nfp :scs (descriptor-reg))
	    (nsp :scs (descriptor-reg))
	    (eval :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    (load-symbol-value catch lisp::*current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move cur-nfp nfp)))
    (move nsp-tn nsp)
    (load-symbol-value eval lisp::*eval-stack-top*)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
	 (nfp :scs (descriptor-reg))
	 (nsp :scs (descriptor-reg))
	 (eval :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    (store-symbol-value catch lisp::*current-catch-block*)
    (store-symbol-value eval lisp::*eval-stack-top*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move nfp cur-nfp)))
    (move nsp nsp-tn)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move csp-tn res)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move bsp-tn res)))


;;;; Unwind block hackery.

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 22
    (inst addi (* (tn-offset tn) word-bytes) cfp-tn block)
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp block unwind-block-current-uwp-slot)
    (storew cfp-tn block unwind-block-current-cont-slot)
    (storew code-tn block unwind-block-current-code-slot)
    (inst compute-lra-from-code code-tn entry-label ndescr temp)
    (storew temp block catch-block-entry-pc-slot)))

;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg) :from (:argument 0)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 44
    (inst addi (* (tn-offset tn) word-bytes) cfp-tn block)
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp block catch-block-current-uwp-slot)
    (storew cfp-tn block catch-block-current-cont-slot)
    (storew code-tn block catch-block-current-code-slot)
    (inst compute-lra-from-code code-tn entry-label ndescr temp)
    (storew temp block catch-block-entry-pc-slot)

    (storew tag block catch-block-tag-slot)
    (load-symbol-value temp lisp::*current-catch-block*)
    (storew temp block catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block*)))

;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst addi (* (tn-offset tn) word-bytes) cfp-tn new-uwp)
    (store-symbol-value new-uwp lisp::*current-unwind-protect-block*)))

(define-vop (unlink-catch-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-catch-block*)
    (loadw block block catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block*)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-unwind-protect-block*)
    (loadw block block unwind-block-current-uwp-slot)
    (store-symbol-value block lisp::*current-unwind-protect-block*)))


;;;; NLX entry VOPs.

(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
	      ; would be inserted before the LRA.
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (inst comclr count zero-tn zero-tn :<>)
	   (inst move null-tn (tn-ref-tn values) :tr)
	   (loadw (tn-ref-tn values) start))
	  (t
	   (collect ((defaults))
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))

		 (inst bci := nil (fixnum i) count default-lab)
		 (sc-case tn
		   ((descriptor-reg any-reg)
		    (loadw tn start i))
		   (control-stack
		    (loadw move-temp start i)
		    (store-stack-tn tn move-temp)))))

	     (let ((defaulting-done (gen-label)))
	       (emit-label defaulting-done)

	       (assemble (*elsewhere*)
		 (do ((defs (defaults) (cdr defs)))
		     ((null defs))
		   (let ((def (car defs)))
		     (emit-label (car def))
		     (unless (cdr defs)
		       (inst b defaulting-done))
		     (let ((tn (cdr def)))
		       (sc-case tn
			 ((descriptor-reg any-reg)
			  (move null-tn tn))
			 (control-stack
			  (store-stack-tn tn null-tn)))))))))))
    (load-stack-tn csp-tn sp)))

(define-vop (nlx-entry-multiple)
  (:args (top :target dst) (start :target src) (count :target num))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg) :from (:argument 0)) dst)
  (:temporary (:scs (any-reg) :from (:argument 1)) src)
  (:temporary (:scs (any-reg) :from (:argument 2)) num)
  (:temporary (:scs (descriptor-reg)) temp)
  (:results (new-start) (new-count))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)

    ;; Copy args.
    (load-stack-tn dst top)
    (move start src)
    (move count num)

    ;; Establish results.
    (sc-case new-start
      (any-reg (move dst new-start))
      (control-stack (store-stack-tn new-start dst)))
    (inst comb := num zero-tn done)
    (sc-case new-count
      (any-reg (inst move num new-count))
      (control-stack (store-stack-tn new-count num)))
    ;; Load the first word.
    (inst ldwm word-bytes src temp)

    ;; Copy stuff on stack.
    LOOP
    (inst stwm temp word-bytes dst)
    (inst addib :<> (fixnum -1) num loop :nullify t)
    (inst ldwm word-bytes src temp)

    DONE
    (inst move dst csp-tn)))

;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)))
