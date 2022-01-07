;; Build system.
;;
;; FIX need finer file modification time than 1 sec

(in-package "BUILD")

(export '(*first-target* *phonies* *targets*
	  build build-target deftarget with-build))

(defvar *targets* ()
  "String table of the targets in scope during a build.")

(defvar *phonies* ()
  "List of phony targets, set during a build.  A phony target is always
   rebuilt, even if there is a file with the name of the target and all
   requirements are older than this file.  Typically this will be targets
   such as \"all\" and \"clean\".")

(defvar *first-target* ()
  "The first target defined during a build.")

(defun build-targets (targs &optional force)
  "Build the target TARGS."
  (loop for target in targs do
    (etypecase target
      (list
       (loop for target2 in target do (build-target target2 force)))
      (string
       (build-target target force)))))

(defun build-target (target &optional force)
  "Build TARGET."
  (when (getstring target *targets*)
    (funcall (getstring target *targets*) force)))

;; FIX when symbols are case sensitive, update this to allow
;; (deftarget file (req1 req2) ...)
(defmacro deftarget (target (&rest requirements) &body body)
  "deftarget target (requirement*) body"
  (let ((fun-name (gensym)))
    `(progn
       (or *first-target* (setq *first-target* ,target))
       (defun ,fun-name (&optional force)
	 ,(fi requirements '(declare (ignore force)))
	 ,(if requirements
	      `(if (probe-file ,target)
		   (let ((date (file-write-date ,target))
			 (buildp))
		     (loop for req in (list ,@requirements) do
		       (etypecase req
			 (list
			  (loop for req2 in req do
			    (when (or force
				      (member ,target *phonies* :test #'equal)
				      (<= date (file-write-date req2)))
			      (build-target req2)
			      (setq buildp t))))
			 (string
			  (when (or force
				    (member ,target *phonies* :test #'equal)
				    (<= date (file-write-date req)))
			    (build-target req)
			    (setq buildp t)))))
		     (if buildp (progn ,@body)))
		   (progn
		     (build-targets (list ,@requirements) force)
		     (progn
		       ,@body)))
	      `(progn
		 ,@body)))
       (setf (getstring ,target *targets*) #',fun-name))))

(defun build (pathname target)
  "In directory PATHNAME build TARGET.  Targets are defined in Build.lisp
   in PATHNAME."
  (let ((*targets* (make-string-table))
	(*first-target*)
	(*phonies*)
	(package (symbol-name (gensym))))
    (in-directory pathname
      (let ((package (make-package package-name :nicknames () :use ())))
	(use-package '("LISP" "BUILD" "EXT") package-name)
	(defpackage package-name
	  (:documentation
	   "Temporary package for a particular build."))
	(unwind-protect
	    (let ((*package* package))
	      (load "Build.lisp" :verbose ())
	      (build-target target))
	  (delete-package package))))))

(defmacro with-build (pathname &body body)
  "WITH-BUILD PATHNAME BODY.  Execute body in a build context for PATHNAME.
   Build rules are read from Build.lisp in PATHNAME."
  `(let ((*targets* (make-string-table))
	 (*first-target*)
	 (*phonies*)
	 (package-name (symbol-name (gensym))))
     (in-directory ,pathname
       (let ((package (make-package package-name
				    :nicknames () :use ())))
	 (use-package '("LISP" "BUILD" "EXT") package-name)
	 (defpackage package-name
	   (:documentation
	    "Temporary package for a particular build."))
	 (unwind-protect
	     (let ((*package* package))
	       (load "Build.lisp" :verbose ())
	       ,@body)
	   (delete-package package))))))
