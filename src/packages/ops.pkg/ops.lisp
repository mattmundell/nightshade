;;; -*- Package: OPS -*-

(defpackage "OPS"
  (:version 0)
  (:use "LISP" "EXT")
  (:documentation "OPS."))

;************************************************************************
;
;	VPS2 -- Interpreter for OPS5
;
;
;
; This Common Lisp version of OPS5 is in the public domain.  It is based
; in part on based on a Franz Lisp implementation done by Charles L. Forgy
; at Carnegie-Mellon University, which was placed in the public domain by
; the author in accordance with CMU policies.  This version has been
; modified by George Wood, Dario Giuse, Skef Wholey, Michael Parzen,
; and Dan Kuokka.
;
; This code is made available is, and without warranty of any kind by the
; authors or by Carnegie-Mellon University.
;

;;;; This file performs the necessary initialization of the OPS interpreter.

(in-package "OPS")

#[ OPS

To compile for CMU Common Lisp, (load "library:contrib/ops/compile-ops").
After OPS has been compiled, you can (load "library:contrib/ops/ops.fasl").
Then go into the OPS package with (in-package :ops).  Now you can load your
OPS5 code or start typing in productions.

There are a number of demos and sample programs; particularly amusing is the
Haunt adventure game.  Do (load "<name>.ops"), then "(run)".  Many systems
require an initial "(make start)" before the "(run)" --- if this is missing,
"(run)" will do nothing.  Set *ptrace* to NIL to eliminate production tracing.

See the OPS5 User's Manual, July 1981, by Forgy, CMU CSD.
]#

(defun ops-init ()
  ; Allows ^ , { , and } operators to be right next to another symbol.
  (set-macro-character #\{ #'(lambda (s c)
			       (declare (ignore s c))
			       '\{))
  (set-macro-character #\} #'(lambda (s c)
			       (declare (ignore s c))
			       '\}))
  (set-macro-character #\^ #'(lambda (s c)
			       (declare (ignore s c))
			       '\^))
  (backup-init)
  (compile-init)
  (main-init)
  (match-init)
  (io-init)
  (rhs-init))

(ops-init)
