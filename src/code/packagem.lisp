;;; -*- Package: Package -*-
;;;
;;; Package management.

(in-package "PACKAGE")

(export '(commit
	  do-packages
	  ensure-meta-loaded
	  flush
	  install installed-p
	  load-package loaded-p
	  local-file local-requires local-version
	  meta-version
	  test-dir
	  update-meta-from-server))

(defvar *package-directory* "n:src/packages/"
  "Directory holding packages.")

(defvar *meta-data* ()
  "Meta-information for all packages.")

;;; Public
;;;
(defun ensure-meta-loaded ()
  "Ensure package meta-information is loaded."
  (or *meta-data* (load-meta)))

(defun ensure-meta-exists ()
  "Ensure package meta-information file exists."
  (or (probe-file "library:packages/packages.lbytef")
      (update-meta-from-server)))

(defun update-meta-from-server ()
  "Sync the local package meta-information with the server."
  (copy-file (merge-pathnames "packages.lbytef" *package-directory*)
	     "library:packages/")
  (load-meta))

(defun read-package-info (file)
  "Return a pkg-info describing the package in file.  Read the package info
   from the first form in $file, which should be a call to `defpackage'."
  (let* ((lisp::*define-packages* ())
	 (meta (with-open-file (stream file :direction :input)
		 (eval (read stream)))))
    (or (typep meta 'lisp::pkg-info)
	(error "Failed to read package information from ~A" file))
    meta))

;;; Public
;;;
(defun update-meta-from-source ()
  "Update the database of meta-information for all packages on the server,
   reading the information from the server source files."
  (setq *meta-data* (make-hash-table :test #'equal))
  (do-files (file *package-directory*
		  :recurse () :backups () :check-for-subdirs ())
    (or (string= (pathname-name file) "packages")
	(string= (pathname-type file) "test")
	(progn
	  (if (gethash (pathname-name file) *meta-data*)
	      (error "Package ~A defined twice."
		     (pathname-name file)))
	  (setf (gethash (pathname-name file) *meta-data*)
		(if (directoryp file)
		    (read-package-info
		     (merge-pathnames (ensure-trailing-slash file)
				      (concat (pathname-name file)
					      ".lisp")))
		    (read-package-info file)))))))

(defun package-pathname (file)
  "Return $file merged with the package directory."
  (merge-pathnames file *package-directory*))

(defun save-meta ()
  "Save the database of meta-information to n:src/packages/packages.lisp,
   and byte compile that file."
  (when *meta-data*
    (touch-file "n:src/packages/packages.lisp")
    (copy-file "n:src/packages/packages.lisp"
	       "n:src/packages/packages.lisp.BAK")
    (with-open-file (file "n:src/packages/packages.lisp.TEM"
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
      (let ((*package* (or (find-package "PACKAGE")
			   (error "Failed to find LISP package."))))
	(write '(in-package "PACKAGE") :stream file :readably t)
	(terpri file)
	;; FIX should be able to ~ print everything readably
	(write `(setq *meta-data* (make-hash-table :test #'equal))
	       :stream file :readably t)
	(terpri file)
	(with-hash-table-iterator (next *meta-data*)
	  (iterate hash ()
	    (multiple-value-bind (more key value)
				 (next)
	      (when more
		(write `(setf (gethash ,key *meta-data*)
			      (lisp::make-pkg-info
			       :name ,(lisp::pkg-info-name value)
			       :doc ,(lisp::pkg-info-doc value)))
		       :stream file :readably t)
		(terpri file)
		(hash)))))))
    (delete-file "n:src/packages/packages.lisp")
    (lisp:copy-file "n:src/packages/packages.lisp.TEM"
		    "n:src/packages/packages.lisp")
    (delete-file "n:src/packages/packages.lisp.TEM")
    (compile-file "n:src/packages/packages.lisp"
		  :verbose () :print () :byte-compile t)))

(defun load-meta ()
  "Load the database of meta-information for all packages.  Return true if
   the load succeeds."
  (ensure-meta-exists)
  (load "library:packages/packages.lbytef"
	:verbose ()
	:if-does-not-exist ()))

;;; Public
;;;
(defmacro do-packages ((meta) &body body)
  "do-packages (meta) body

   Evaluate $body for each package, with $meta bound to the package meta
   information."
  (let ((next (gensym))
	(more (gensym))
	(key (gensym)))
    `(progn
       (ensure-meta-loaded)
       (with-hash-table-iterator (,next *meta-data*)
         (iterate do-next ()
	   (multiple-value-bind (,more ,key ,meta)
				(,next)
	     (declare (ignore ,key))
	     (when ,more
	       ,@body
	       (do-next))))))))

;;; Public
;;;
(defun installed-p (name)
  "Return true if package $name is installed."
  (let ((pathname (merge-pathnames (concat (string-downcase name)
					   ".lisp")
				   "library:packages/")))
    (or (probe-file pathname)
	(let ((pathname (merge-pathnames (concat (string-downcase name)
						 ".pkg/")
					 "library:packages/")))
	  (probe-file pathname)))))

;;; Public
;;;
(defun install (name)
  "Install package $name."
  (let ((source (merge-pathnames (concat (string-downcase name)
					 ".lisp")
				 *package-directory*))
	(test (merge-pathnames (concat (string-downcase name)
				       ".test/")
			       *package-directory*)))
    (if (probe-file source)
	(let ((dest (merge-pathnames (concat (string-downcase name)
					     ".lisp")
				     "library:packages/")))
	  (ensure-directories-exist dest)
	  (if (probe-file dest)
	      (rename-file dest (merge-pathnames dest ".bak")))
	  (copy-file source dest))
	(let ((source (merge-pathnames (concat (string-downcase name)
					       ".pkg/")
				       *package-directory*)))
	  (fi (probe-file source)
	      (error "Failed to find package \"~A\"." name)
	      (fi (directoryp source)
		  (error "Package \"~A\" should be a directory." source)
		  (let ((dest (merge-pathnames (concat (string-downcase name)
						       ".pkg")
					       "library:packages/")))
		    (if (probe-file dest)
			(rename-file dest (concat dest ".bak")))
		    (ensure-directories-exist dest)
		    (dired::copy-file source dest :update () :clobber ()
				     :check-for-links t))))))
    (if (eq (file-kind test) :directory)
	(let ((dest (merge-pathnames (concat (string-downcase name)
					     ".test")
				     "library:packages/")))
	  (if (probe-file dest)
	      (rename-file dest (concat dest ".bak")))
	  (ensure-directories-exist dest)
	  (dired::copy-file test dest :update () :clobber ()
			    :check-for-links t)))))

;;; Public
;;;
(declaim (inline loaded))
;;;
(defun loaded-p (name)
  "Return true if package $name is loaded."
  (find-package name))

;;; Public
;;;
(defun load-package (name)
  "Load package $name."
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  (let ((pathname (merge-pathnames (concat (string-downcase name)
					   ".lisp")
				   "library:packages/")))
    (if (probe-file pathname)
	(load pathname)
	(let ((pathname (merge-pathnames (concat (string-downcase name)
						 ".pkg/")
					 "library:packages/")))
	  (fi (probe-file pathname)
	      (error "Failed to find package \"~A\"." name)
	      (fi (directoryp pathname)
		  (error "Package \"~A\" should be a directory." name)
		  (do-files (file pathname)
		    (if (string= (string-upcase (pathname-type file)) "LISP")
			(load pathname)))))))))

;;; Public
;;;
(defun flush (name)
  "Flush package $name from the directory of local packages."
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  (let ((pathname (merge-pathnames (concat (string-downcase name)
					   ".lisp")
				   "library:packages/")))
    (if (probe-file pathname)
	(delete-file pathname)
	(let ((pathname (merge-pathnames (concat (string-downcase name)
						 ".pkg")
					 "library:packages/")))
	  (fi (probe-file pathname)
	      (error "Failed to find package \"~A\"." name)
	      (if (directoryp pathname)
		  (dired::delete-file pathname :recurse t :clobber t)
		  (error "Package \"~A\" should be a directory." name)))))
    (let ((test (merge-pathnames (concat (string-downcase name)
					 ".test")
				 "library:packages/")))
      (if (eq (file-kind test) :directory)
	  (dired::delete-file test :recurse t :clobber t)))))

;;; Public
;;;
(defun commit (name)
  "Copy the local version of package $name to the server if the version on
   the server is older than the local version."
  ;; FIX afterwards server packages.lbytef must be rebuilt
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  ;; FIX update from server first
  (if (<= (local-version name)
	  (meta-version name))
      (error "Package ~A is up-to-date or newer on the server." name))
  (let ((pathname (merge-pathnames (concat (string-downcase name)
					   ".lisp")
				   "library:packages/")))
    (if (probe-file pathname)
	(copy-file pathname *package-directory*)
	(let ((pathname (merge-pathnames (concat (string-downcase name)
						 ".pkg")
					 "library:packages/")))
	  (fi (probe-file pathname)
	      (error "Failed to find package \"~A\"." name)
	      (if (directoryp pathname)
		  (dired::copy-file pathname
				    *package-directory*
				    :clobber t)
		  (error "Package \"~A\" should be a directory." name)))))))

;;; Public
;;;
(defun meta-version (name)
  "Return the version of package $name, according to the package
   meta-information."
  (let ((entry (gethash (string-downcase name) *meta-data*)))
    (if entry (or (lisp::pkg-info-version entry) 0))))

;;; Public
;;;
(defun local-file (name)
  "Return the pathname of the local source file representing $name.  If
   $name is in a subdirectory of the package tree then return the file in
   the subdirectory that is named like the package."
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  (let ((file (merge-pathnames (concat (string-downcase name)
				       ".lisp")
			       "library:packages/")))
    (if (probe-file file)
	(if (directoryp file)
	    (merge-pathnames (ensure-trailing-slash file)
			     (concat (pathname-name file)
				     ".lisp"))
	    file))))

;;; Public
;;;
(defun local-version (name)
  "Return the local version of package $name."
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  (let ((file (local-file name)))
    (if file
	(let ((info (read-package-info file)))
	  (if info (or (lisp::pkg-info-version info) 0))))))

;;; Public
;;;
(defun local-requires (name)
  "Return the local requirements of package $name."
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  (let ((file (local-file name)))
    (if file
	(let ((info (read-package-info file)))
	  (if info (lisp::pkg-info-requires info))))))

;;; Public
;;;
(defun test-dir (name)
  "Return the test directory of package $name."
  (if (string= name "packages")
      (error "Package \"packages\" is reserved for the package meta information."))
  (let ((dir (ensure-trailing-slash
	       (merge-pathnames (concat (string-downcase name)
					".test")
				"library:packages/"))))
    (if (probe-file dir) dir)))
