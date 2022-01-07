;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/hppa/alloc.lisp,v 1.2 1994/10/31 04:56:18 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle allocation of stuff we don't want to do inline.
;;;
;;; Written by William Lott.
;;;

(in-package "HPPA")

;;; Given that the pseudo-atomic sequence is so short, there is
;;; nothing that qualifies.  But we want to keep the file around
;;; in case we decide to add something later.

