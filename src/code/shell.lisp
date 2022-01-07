;;; Unix-like shell for the REPL.

(in-package "SHELL")

(export '(~ cd #| dir |# ls mv pwd rm touch))

(in-package "LISP")

(import '(~ cd #| dir |# ls mv pwd rm touch)
	"SHELL")

(in-package "SHELL")

(use-package "EXTENSIONS")

(declaim (inline cd dir ls mv pwd rm touch))

(defvar ~ #p"home:")

(defun pwd ()
  "Print the Working Directory (the current directory)."
  (current-directory))

#|
(defun ls (&rest args)
  "LiSt the files in the current directory."
  (fi args
      (print-directory "./")
      (collect ((names))
	(dolist (arg args)
	  (etypecase arg
	    (string (names arg))
	    ;; FIX turn these commands into macros, share arg processing
	    ;; (ls -la)  (ls --list --all)  (ls :list :all) (ls :l :a)
	    (keyword)
	     ))
	(dolist (name (names))
	  (print-directory name)))))
|#

(defmacro with-args ((args) &body body)
  `(collect ((vars) (opts))
     (dolist (arg ,args)
       (typecase arg
	 (symbol
	  (let ((string (symbol-name arg)))
	    (if (and (> (length string) 1)
		     (char= (char string 0) #\-))
		;; FIX check for long args
		(until ((ind 1 (1+ ind)))
		       ((eq ind (length string)))
		  (opts (aref string ind)))
		(vars arg))))
	 (t (vars arg))))
     ,@body))

(defmacro ls (&rest args)
  "LiSt the files in the current directory."
  (fi args
      `(print-directory "")
      `(with-args (',args)
	 (if (vars)
	     (dolist (var (vars))
	       (print-directory var t
				:all     (member #\A (opts))
				:verbose (member #\L (opts))
				:recurse (member #\R (opts))))
	     (print-directory "" t
			      :all     (member #\A (opts))
			      :verbose (member #\L (opts))
			      :recurse (member #\R (opts)))))))

(defun dir ()
  "List the files in the current DIRectory."
  (directory "./"))

(defun cd (&optional pathname)
  "Change Directory to $pathname.  That is, set the current directory to
   $pathname."
  (setf (current-directory) (or pathname "home:")))

(defun rm (pathname)
  "ReMove the file $pathname."
  (delete-file pathname))

(defun mv (src dest)
  "MoVe pathname $src to $dest."
  (rename-file src dest))

(defun touch (directory)
  "Touch file or directory $pathname."
  (touch-file directory))
