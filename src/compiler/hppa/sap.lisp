;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/sap.lisp,v 1.5 2001/02/16 23:39:10 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the HP-PA VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;
(in-package "HPPA")


;;;; Moves and coercions:

;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "system area pointer indirection")
  (:generator 1
    (loadw y x sap-pointer-slot other-pointer-type)))

;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; Move an untagged SAP to a tagged representation.
;;;
(define-vop (move-from-sap)
  (:args (x :scs (sap-reg) :to (:eval 1)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (y :scs (descriptor-reg) :from (:eval 0)))
  (:note "system area pointer allocation")
  (:generator 20
    (with-fixed-allocation (y ndescr sap-type sap-size)
      (storew x y sap-pointer-slot other-pointer-type))))
;;;
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))


;;; Move untagged sap values.
;;;
(define-vop (sap-move)
  (:args (x :target y
	    :scs (sap-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move x y)))
;;;
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))


;;; Move untagged sap arguments/return-values.
;;;
(define-vop (move-sap-argument)
  (:args (x :target y
	    :scs (sap-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (sap-reg
       (move x y))
      (sap-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-sap-argument :move-argument
  (descriptor-reg sap-reg) (sap-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged sap to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (sap-reg) (descriptor-reg))



;;;; SAP-INT and INT-SAP

(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int sap)))



;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg) :target res)
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (inst add ptr offset res)))

(define-vop (pointer+-c)
  (:translate sap+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 11)))
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 1
    (inst addi offset ptr res)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg))
	 (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (inst sub ptr1 ptr2 res)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(eval-when (compile eval)

(defmacro def-system-ref-and-set
	  (ref-name set-name sc type size &optional signed)
  (let ((ref-name-c (symbolicate ref-name "-C"))
	(set-name-c (symbolicate set-name "-C")))
    `(progn
       (define-vop (,ref-name)
	 (:translate ,ref-name)
	 (:policy :fast-safe)
	 (:args (object :scs (sap-reg))
		(offset :scs (signed-reg)))
	 (:arg-types system-area-pointer signed-num)
	 (:results (result :scs (,sc)))
	 (:result-types ,type)
	 (:generator 5
	   (inst ,(ecase size
		    (:byte 'ldbx)
		    (:short 'ldhx)
		    (:long 'ldwx)
		    (:float 'fldx))
		 offset object result)
	   ,@(when (and signed (not (eq size :long)))
	       `((inst extrs result 31 ,(ecase size
			  (:byte 8)
			  (:short 16))
		       result)))))
       (define-vop (,ref-name-c)
	 (:translate ,ref-name)
	 (:policy :fast-safe)
	 (:args (object :scs (sap-reg)))
	 (:arg-types system-area-pointer
		     (:constant ,(if (eq size :float)
				     '(signed-byte 5)
				     '(signed-byte 14))))
	 (:info offset)
	 (:results (result :scs (,sc)))
	 (:result-types ,type)
	 (:generator 4
	   (inst ,(ecase size
		    (:byte 'ldb)
		    (:short 'ldh)
		    (:long 'ldw)
		    (:float 'flds))
		 offset object result)
	   ,@(when (and signed (not (eq size :long)))
	       `((inst extrs result 31 ,(ecase size
			  (:byte 8)
			  (:short 16))
		       result)))))
       (define-vop (,set-name)
	 (:translate ,set-name)
	 (:policy :fast-safe)
	 (:args (object :scs (sap-reg)
			,@(unless (eq size :float) '(:target sap)))
		(offset :scs (signed-reg))
		(value :scs (,sc) :target result))
	 (:arg-types system-area-pointer signed-num ,type)
	 (:results (result :scs (,sc)))
	 (:result-types ,type)
	 ,@(unless (eq size :float)
	     '((:temporary (:scs (sap-reg) :from (:argument 0)) sap)))
	 (:generator 5
	   ,@(if (eq size :float)
		 `((inst fstx value offset object)
		   (unless (location= value result)
		     (inst funop :copy value result)))
		 `((inst add object offset sap)
		   (inst ,(ecase size (:byte 'stb) (:short 'sth) (:long 'stw))
			 value 0 sap)
		   (move value result)))))
       (define-vop (,set-name-c)
	 (:translate ,set-name)
	 (:policy :fast-safe)
	 (:args (object :scs (sap-reg))
		(value :scs (,sc) :target result))
	 (:arg-types system-area-pointer
		     (:constant ,(if (eq size :float)
				     '(signed-byte 5)
				     '(signed-byte 14)))
		     ,type)
	 (:info offset)
	 (:results (result :scs (,sc)))
	 (:result-types ,type)
	 (:generator 5
	   ,@(if (eq size :float)
		 `((inst fsts value offset object)
		   (unless (location= value result)
		     (inst funop :copy value result)))
		 `((inst ,(ecase size (:byte 'stb) (:short 'sth) (:long 'stw))
			 value offset object)
		   (move value result))))))))

); eval-when (compile eval)

(def-system-ref-and-set sap-ref-8 %set-sap-ref-8
  unsigned-reg positive-fixnum :byte nil)
(def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
  signed-reg tagged-num :byte t)
(def-system-ref-and-set sap-ref-16 %set-sap-ref-16
  unsigned-reg positive-fixnum :short nil)
(def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
  signed-reg tagged-num :short t)
(def-system-ref-and-set sap-ref-32 %set-sap-ref-32
  unsigned-reg unsigned-num :long nil)
(def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
  signed-reg signed-num :long t)
(def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
  sap-reg system-area-pointer :long)
(def-system-ref-and-set sap-ref-single %set-sap-ref-single
  single-reg single-float :float)
(def-system-ref-and-set sap-ref-double %set-sap-ref-double
  double-reg double-float :float)


;;; Noise to convert normal lisp data objects into SAPs.

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst addi
	  (- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type)
	  vector
	  sap)))


;;; Transforms for 64-bit SAP accessors.

(deftransform sap-ref-64 ((sap offset) (* *))
  '(logior (ash (sap-ref-32 sap offset) 32)
	   (sap-ref-32 sap (+ offset 4))))

(deftransform signed-sap-ref-64 ((sap offset) (* *))
  '(logior (ash (signed-sap-ref-32 sap offset) 32)
	   (sap-ref-32 sap (+ 4 offset))))

(deftransform %set-sap-ref-64 ((sap offset value) (* * *))
  '(progn
     (%set-sap-ref-32 sap offset (ash value -32))
     (%set-sap-ref-32 sap (+ offset 4) (logand value #xffffffff))))

(deftransform %set-signed-sap-ref-64 ((sap offset value) (* * *))
  '(progn
     (%set-signed-sap-ref-32 sap offset (ash value -32))
     (%set-sap-ref-32 sap (+ 4 offset) (logand value #xffffffff))))
