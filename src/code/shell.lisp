;;; Unix-like shell for the REPL.

(in-package "SHELL")

(export '(~ && ||
	  ] ]] >> [ ; > < are used already!
	  cat cd #| dir |# ech ls mv pwd rm she touch))

;(import '(~ cd #| dir |# ls mv pwd rm touch) "LISP")

(use-package "EXTENSIONS")

(declaim (inline cd dir mv pwd rm touch))

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

(defun cat (&rest pathnames)
  "If $pathnames are given \"conCATenate\" the files in $pathnames: write
   them sequentially to *standard-output*, otherwise `transfer'
   *standard-input* to *standard-output*."
  (if pathnames
      (dolist (pathname pathnames)
	(from-file (in pathname) (transfer in *standard-output*)))
      (transfer *standard-input* *standard-output*)))

(defun she (command)
  "Run the string $command in a subshell."
  (let ((process))
    (unwind-protect
	(setq process
	      (run-program "/bin/sh" (list "-c" command)
			   :pty t
			   :wait ()
			   :output *standard-output*))
      (if process
	  (progn
	    ;; This is to ensure the process is killed after QUIT in the
	    ;; RTI, as the RTI is invoked on control-c.
	    ;; FIX control-c should just quit?
	    (catch 'lisp::top-level-catcher
	      (ext:process-wait process))
	    (prog1
		(ext:process-exit-code process)
	      (if (ext:process-alive-p process)
		  (ext:process-kill process :sigkill
				    :pty-process-group))))))))

(defmacro ] (destination &body body)
  "Run $body with *standard-output* bound to a stream open on the file
   $destination.  Truncate $destination beforehand if it exists."
  `(to-file (*standard-output* ,destination)
     ,@body))

(defmacro ]] (destination &body body)
  "Run $body with *standard-output* bound to a stream open on the file
   $destination.  Append to $destination if it exists."
  `(to-file (*standard-output* ,destination :if-exists :append)
     ,@body))

(defmacro >> (destination &body body)
  "Run $body with *standard-output* bound to a stream open on the file
   $destination.  Append to $destination if it exists."
  `(to-file (*standard-output* ,destination :if-exists :append)
     ,@body))

(defmacro [ (destination &body body)
  "Run $body with *standard-input* bound to a stream open on the file
   $destination."
  `(from-file (*standard-input* ,destination)
     ,@body))

(defun ech (&rest args)
  "Print the args to *standard-output* with spaces inbetween, followed by a
   newline.  Print with `princ'.  Evaluate the arguments."
  (when args
    (princ (car args) *standard-output*)
    (dolist (arg (cdr args))
      (princ #\space *standard-output*)
      (princ arg *standard-output*))
    (princ #\newline *standard-output*)))

(defmacro && (&body body)
  "Evaluate the elements of $body, exiting if an element returns ()."
  `(and ,@body))

(defmacro || (&body body)
  "Evaluate the elements of $body, exiting if an element returns t."
  `(or ,@body))
