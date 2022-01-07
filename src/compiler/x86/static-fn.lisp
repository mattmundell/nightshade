;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /home/CVS-cmucl/src/compiler/x86/static-fn.lisp,v 1.1.2.1 1998/06/23 11:24:12 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and macro magic necessary to call static
;;; functions.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997.
;;;
(in-package :x86)


(define-vop (static-function-template)
  (:save-p t)
  (:policy :safe)
  (:variant-vars function)
  (:vop-var vop)
  (:node-var node)
  (:temporary (:sc unsigned-reg :offset ebx-offset
		   :from (:eval 0) :to (:eval 2)) ebx)
  (:temporary (:sc unsigned-reg :offset ecx-offset
		   :from (:eval 0) :to (:eval 2)) ecx))

(eval-when (compile load eval)

(defun static-function-template-name (num-args num-results)
  (intern (format nil "~:@(~R-arg-~R-result-static-function~)"
		  num-args num-results)))

(defun moves (dst src)
  (collect ((moves))
    (do ((dst dst (cdr dst))
	 (src src (cdr src)))
	((or (null dst) (null src)))
      (moves `(move ,(car dst) ,(car src))))
    (moves)))

(defun static-function-template-vop (num-args num-results)
  (assert (and (<= num-args register-arg-count)
	       (<= num-results register-arg-count))
	  (num-args num-results)
	  "Either too many args (~D) or too many results (~D).  Max = ~D"
	  num-args num-results register-arg-count)
  (let ((num-temps (max num-args num-results)))
    (collect ((temp-names) (temps) (arg-names) (args) (result-names) (results))
      (dotimes (i num-results)
	(let ((result-name (intern (format nil "RESULT-~D" i))))
	  (result-names result-name)
	  (results `(,result-name :scs (any-reg descriptor-reg)))))
      (dotimes (i num-temps)
	(let ((temp-name (intern (format nil "TEMP-~D" i))))
	  (temp-names temp-name)
	  (temps `(:temporary (:sc descriptor-reg
			       :offset ,(nth i register-arg-offsets)
			       :from ,(if (< i num-args)
					  `(:argument ,i)
					  '(:eval 1))
			       :to ,(if (< i num-results)
					`(:result ,i)
					'(:eval 1))
			       ,@(when (< i num-results)
				   `(:target ,(nth i (result-names)))))
			      ,temp-name))))
      (dotimes (i num-args)
	(let ((arg-name (intern (format nil "ARG-~D" i))))
	  (arg-names arg-name)
	  (args `(,arg-name
		  :scs (any-reg descriptor-reg)
		  :target ,(nth i (temp-names))))))
      `(define-vop (,(static-function-template-name num-args num-results)
		    static-function-template)
	(:args ,@(args))
	,@(temps)
	(:results ,@(results))
	(:generator ,(+ 50 num-args num-results)
	 ,@(moves (temp-names) (arg-names))

	 ;; If speed not more important than size, duplicate the
	 ;; effect of the ENTER with discrete instructions.  Takes
	 ;; 2+1+3+2=8 bytes as opposed to 4+3=7 bytes.
	 (cond ((policy node (>= speed space))
		(inst mov ebx esp-tn)
		;; Save the old-fp
		(inst push ebp-tn)
		;; Ensure that at least three slots are available; one
		;; above, two more needed.
		(inst sub esp-tn (fixnum 2))
		(inst mov ebp-tn ebx))
	       (t
		(inst enter (fixnum 2))
		;; The enter instruction pushes EBP and then copies
		;; ESP into EBP.  We want the new EBP to be the
		;; original ESP, so we fix it up afterwards.
		(inst add ebp-tn (fixnum 1))))
	 
	 ,(if (zerop num-args)
	      '(inst xor ecx ecx)
	      `(inst mov ecx (fixnum ,num-args)))
	 
	 (note-this-location vop :call-site)
	 ;; Static-function-offset gives the offset from the start of
	 ;; the nil object to the static function fdefn and has the
	 ;; low tag of 1 added.  When the nil symbol value with its
	 ;; low tag of 3 is added the resulting value points to the
	 ;; raw address slot of the fdefn (at +4).
	 (inst call (make-ea :dword
			     :disp (+ nil-value
				      (static-function-offset function))))
	 ,(collect ((bindings) (links))
		   (do ((temp (temp-names) (cdr temp))
			(name 'values (gensym))
			(prev nil name)
			(i 0 (1+ i)))
		       ((= i num-results))
		     (bindings `(,name
				 (make-tn-ref ,(car temp) nil)))
		     (when prev
		       (links `(setf (tn-ref-across ,prev) ,name))))
		   `(let ,(bindings)
		     ,@(links)
		     (default-unknown-values
			 vop
			 ,(if (zerop num-results) nil 'values)
		       ,num-results)))
	 ,@(moves (result-names) (temp-names)))))))


) ; eval-when (compile load eval)


(macrolet ((frob (num-args num-res)
	     (static-function-template-vop (eval num-args) (eval num-res))))
  (frob 0 1)
  (frob 1 1)
  (frob 2 1)
  (frob 3 1))


(defmacro define-static-function (name args &key (results '(x)) translate
				       policy cost arg-types result-types)
  `(define-vop (,name
		,(static-function-template-name (length args)
						(length results)))
     (:variant ',name)
     (:note ,(format nil "static-function ~@(~S~)" name))
     ,@(when translate
	 `((:translate ,translate)))
     ,@(when policy
	 `((:policy ,policy)))
     ,@(when cost
	 `((:generator-cost ,cost)))
     ,@(when arg-types
	 `((:arg-types ,@arg-types)))
     ,@(when result-types
	 `((:result-types ,@result-types)))))
