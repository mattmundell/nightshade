;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/code/generic-site.lisp,v 1.11.2.2 2000/08/20 14:43:11 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Generic site specific initialization.  This can be used as a template
;;; for "library:site-init" files.
;;;
(in-package "SYSTEM")

(setq *short-site-name* "Short site name")
(setq *long-site-name* "Long site name")

(rplaca
 (cdr (member :subsystems *herald-items*))
 '("Loaded subsystems:" terpri))

;;; If you have sources installed on your system, un-comment the following
;;; form and change it to point to the source location.  This will allow
;;; the editor "Edit Definition" command and the debugger to find sources
;;; for functions in the core.
;(setf (search-list "target:") "<the source tree root>/")
