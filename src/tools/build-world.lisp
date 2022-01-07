;;; Build all the Lisp code.
;;;
;;; Intended for a command like
;;;
;;;    lisp -load src/tools/build-world.lisp
;;;
;;; or
;;;
;;;    build-small/lisp/lisp -core build-small/lisp/lisp.core \
;;;        -eval "(defvar user::src \"modified-src/\")" \
;;;        -load src/tools/build-world.lisp
;;;
;;; The source and target directories (variables src and target) are either
;;; supplied or set to "./src" and "./build/".  Similarly the systems to
;;; compile can be set via the systems variable.

(in-package "USER")

(defvar src "./src/"
  "Location of the source.")

(defvar target (if src (format nil "~A/../build/" src) "./build/")
  "Destination directory for the build.")

(defvar systems '(:lisp :compiler :ed :kernel)
  "A list of systems to build.  Members of the list can be any of :code,
   :compiler, :ed and :kernel (the lisp kernel core), in any order")

(defun preserve-logs (dir)
  "Backup previous log files."
  (format t "Preserving log files in ~A as .OLD...~%" dir)
  (dolist (file (lisp::enumerate-names (format nil "~A/*.log" dir)
				       nil nil nil nil))
    ;; FIX original appended the log to any existing old log
    (let ((old (format nil "~A.OLD" file)))
      (if (probe-file old)
	  (delete-file old))
      (rename-file file old))))

(declaim (special *interactive*))

(defun build-world (src target systems)
  "Build Systems from Src into Target."
  (if src
      (or (eq (char src (1- (length src))) #\/)
	  (setq src (format nil "~A/" src)))
      ;; FIX set to ../../src relative to location of current script
      (setq src "./src/"))
  (or (probe-file src)
      (progn
	(format t "Source directory must exist.")
	(quit)))
  (format t "Source: ~A~%" src)

  (if target
      (or (eq (char target (1- (length target))) #\/)
	  (setq target (format nil "~A/" target)))
      (setq target "./build/"))
  (or (probe-file target)
      (progn
	(format t "Target directory must exist.")
	(quit)))
  (format t "Target: ~A~%" target)

  (preserve-logs target)
  (when (find-package "INTERFACE")
    (set (intern "*INTERFACE-STYLE*" "INTERFACE") :tty))

  (setf (search-list "target:") `(,target ,src))

  (format t "*features*: ~A~%" *features*)
  (format t "Loading features...~%")
  (load (open "target:features.lisp"))
  (format t "*features*: ~A~%" *features*)

  (format t "Loading setup...~%")
  (setq *compile-verbose* nil *compile-print* nil)
  (load "target:tools/setup" :if-source-newer :load-source)
  (setf *interactive* nil *gc-verbose* nil)
  ;; FIX quiet warning about def of comf
  (comf "target:tools/setup" :load t)

  (format t "Systems: ~A~%" systems)

  (when (probe-file "bootstrap.lisp")
    (format t "Loading bootstrap...")
    (comf "target:bootstrap" :load t))

  (when (position :lisp systems)
    (format t "Compiling Lisp code...")
    (load "target:tools/worldcom"))

  (when (position :compiler systems)
    (format t "Compiling compiler...")
    (load "target:tools/comcom"))

  (when (position :ed systems)
    (format t "Compiling Editor...")
    (load "target:tools/edcom"))

  (when (position :kernel systems)
    (format t "Compiling kernel core...")
    (load "target:tools/worldbuild")))

(build-world src target systems)
