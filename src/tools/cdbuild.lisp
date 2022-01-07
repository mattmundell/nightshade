;;; Build the boot CD core from the boot .assem files.

(in-package "LISP")

(or (fboundp 'genesis)
    (load "target:compiler/generic/new-genesis"))

(defparameter lisp-files
  `(,@(when (c:backend-featurep :pmax)
	'("target:assembly/mips/boot.assem"))
    ,@(when (c:backend-featurep :sparc)
	'("target:assembly/sparc/boot.assem"))
    ,@(when (c:backend-featurep :rt)
	'("target:assembly/rt/boot.assem"))
    ,@(when (c:backend-featurep :hppa)
	'("target:assembly/hppa/boot.assem"))
    ,@(when (c:backend-featurep :x86)
	'("target:assembly/x86/boot.assem"))
    ,@(when (c:backend-featurep :alpha)
	'("target:assembly/alpha/boot.assem"))
    ,@(when (c:backend-featurep :sgi)
	'("target:assembly/mips/boot.assem"))))

(when (boundp '*target-page-size*)
  (locally (declare (optimize (inhibit-warnings 3)))
    (setf *target-page-size*
	  (c:backend-page-size c:*backend*))))

(build-cd-boot-image lisp-files)
