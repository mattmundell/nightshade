;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/hppa/array.lisp,v 1.5 1994/10/31 04:56:18 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;;
(in-package "HPPA")

(define-assembly-routine
    (allocate-vector
     (:policy :fast-safe)
     (:translate allocate-vector)
     (:arg-types positive-fixnum
		 positive-fixnum
		 positive-fixnum))
    ((:arg type any-reg a0-offset)
     (:arg length any-reg a1-offset)
     (:arg words any-reg a2-offset)
     (:res result descriptor-reg a0-offset)
     
     (:temp ndescr non-descriptor-reg nl0-offset)
     (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic ()
    (move alloc-tn vector)
    (inst dep other-pointer-type 31 3 vector)
    (inst addi (* (1+ vector-data-offset) word-bytes) words ndescr)
    (inst dep 0 31 3 ndescr)
    (inst add ndescr alloc-tn alloc-tn)
    (inst srl type word-shift ndescr)
    (storew ndescr vector 0 other-pointer-type)
    (storew length vector vector-length-slot other-pointer-type))
  (move vector result))



;;;; Hash primitives

#+assembler
(defparameter *sxhash-simple-substring-entry* (gen-label))

(define-assembly-routine
    (sxhash-simple-string
     (:translate %sxhash-simple-string)
     (:policy :fast-safe)
     (:result-types positive-fixnum))
    ((:arg string descriptor-reg a0-offset)
     (:res result any-reg a0-offset)

     (:temp length any-reg a1-offset)
     (:temp accum non-descriptor-reg nl0-offset)
     (:temp data non-descriptor-reg nl1-offset)
     (:temp offset non-descriptor-reg nl2-offset))

  (declare (ignore result accum data offset))

  ;; Save the return address.
  (inst b *sxhash-simple-substring-entry*)
  (loadw length string vector-length-slot other-pointer-type))

(define-assembly-routine
    (sxhash-simple-substring
     (:translate %sxhash-simple-substring)
     (:policy :fast-safe)
     (:arg-types * positive-fixnum)
     (:result-types positive-fixnum))
    
    ((:arg string descriptor-reg a0-offset)
     (:arg length any-reg a1-offset)
     (:res result any-reg a0-offset)

     (:temp accum non-descriptor-reg nl0-offset)
     (:temp data non-descriptor-reg nl1-offset)
     (:temp offset non-descriptor-reg nl2-offset))

  (emit-label *sxhash-simple-substring-entry*)

  (inst li (- (* vector-data-offset word-bytes) other-pointer-type) offset)
  (inst b test)
  (move zero-tn accum)

  LOOP
  (inst xor accum data accum)
  (inst shd accum accum 5 accum)

  TEST
  (inst ldwx offset string data)
  (inst addib :>= (fixnum -4) length loop)
  (inst addi (fixnum 1) offset offset)

  (inst addi (fixnum 4) length length)
  (inst comb := zero-tn length done :nullify t)
  (inst sub zero-tn length length)
  (inst sll length 1 length)
  (inst mtctl length :sar)
  (inst shd zero-tn data :variable data)
  (inst xor accum data accum)

  DONE

  (inst sll accum 5 result)
  (inst srl result 3 result))
