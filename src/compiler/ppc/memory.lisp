;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/memory.lisp,v 1.1 2001/02/11 14:22:05 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the PPC definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "PPC")

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))

;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the standard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (storew value object (+ base offset) lowtag)))



;;;; Indexed references:

;;; Define-Indexer  --  Internal
;;;
;;;    Define some VOPs for indexed memory reference.
;;;
(defmacro define-indexer (name write-p ri-op rr-op shift &optional sign-extend-byte)
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg))
	    (index :scs (any-reg zero immediate))
	    ,@(when write-p
		'((value :scs (any-reg descriptor-reg) :target result))))
     (:arg-types * tagged-num ,@(when write-p '(*)))
     (:temporary (:scs (non-descriptor-reg)) temp)
     (:results (,(if write-p 'result 'value)
		:scs (any-reg descriptor-reg)))
     (:result-types *)
     (:variant-vars offset lowtag)
     (:policy :fast-safe)
     (:generator 5
       (sc-case index
	 ((immediate zero)
	  (let ((offset (- (+ (if (sc-is index zero)
				  0
				  (ash (tn-value index)
				       (- vm:word-shift ,shift)))
			      (ash offset vm:word-shift))
			   lowtag)))
	    (etypecase offset
	      ((signed-byte 16)
	       (inst ,ri-op value object offset))
	      ((or (unsigned-byte 32) (signed-byte 32))
	       (inst lr temp offset)
	       (inst ,rr-op value object temp)))))
	 (t
	  ,@(unless (zerop shift)
	      `((inst srwi temp index ,shift)))
	  (inst addi temp ,(if (zerop shift) 'index 'temp)
		(- (ash offset vm:word-shift) lowtag))
	  (inst ,rr-op value object temp)))
       ,@(when sign-extend-byte
           `((inst extsb value value)))
       ,@(when write-p
	   '((move result value))))))

(define-indexer word-index-ref nil lwz lwzx 0)
(define-indexer word-index-set t stw stwx 0)
(define-indexer halfword-index-ref nil lhz lhzx 1)
(define-indexer signed-halfword-index-ref nil lha lhax 1)
(define-indexer halfword-index-set t sth sthx 1)
(define-indexer byte-index-ref nil lbz lbzx 2)
(define-indexer signed-byte-index-ref nil lbz lbzx 2 t)
(define-indexer byte-index-set t stb stbx 2)

