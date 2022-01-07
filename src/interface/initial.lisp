;;; -*- Package: User -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /home/CVS-cmucl/src/interface/initial.lisp,v 1.5 1994/10/31 04:53:18 ram Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "USER")

(pushnew :motif *features*)

(setf (getf ext:*herald-items* :motif)
      `("    Motif toolkit and graphical debugger 1.0"))

(defpackage "INTERFACE"
  (:use "TOOLKIT" "LISP" "EXTENSIONS" "KERNEL")
  (:shadow "CLASS-DIRECT-SUPERCLASSES")
  (:export "*HEADER-FONT*" "*ITALIC-FONT*" "*ENTRY-FONT*" "*INTERFACE-STYLE*"
	   "USE-GRAPHICS-INTERFACE" "VERIFY-SYSTEM-SERVER-EXISTS"
	   "CREATE-INTERFACE-SHELL" "POPUP-INTERFACE-PANE"
	   "CREATE-INTERFACE-PANE-SHELL" "FIND-INTERFACE-PANE"
	   "DESTROY-INTERFACE-PANE" "CREATE-HIGHLIGHT-BUTTON" "CREATE-VALUE-BOX"
	   "SET-VALUE-BOX" "WITH-WIDGET-CHILDREN" "INTERFACE-ERROR"
	   "PRINT-FOR-WIDGET-DISPLAY" "WITH-BUSY-CURSOR"
	   "CREATE-INTERFACE-MENU" "CREATE-CACHED-MENU"
	   "GRAB-OUTPUT-AS-STRING" "*ALL-FONTS*" "LISP-CONTROL-PANEL"))
