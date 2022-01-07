;;; Tests of lisp:current-directory.

(in-package "LISP")

(import '(deftest:deftest))

;; FIX remote directories
;; (setf (current-directory) "dir/name")

(deftest current-directory (t current-directory-1)
  "Test `current-directory'."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (string= (namestring (current-directory))
		   (namestring dir)))
      (delete-dir dir))))


;;;; `setf'.

(deftest current-directory (t current-directory-10)
  "Test setting the `current-directory' to an absolute pathname."
  (let ((dir (pick-new-dir))
	(old (current-directory)))
    (unwind-protect
	(progn
	  (setf (current-directory) dir)
	  (string= (namestring (current-directory))
		   (namestring dir)))
      (setf (current-directory) old)
      (delete-dir dir))))

(deftest current-directory (t current-directory-11)
  "Test setting the `current-directory' to a relative pathname."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory (namify dir)
	  (setf (current-directory) (file-namestring (namify dir)))
	  (string= (namestring (current-directory))
		   (namestring dir)))
      (delete-dir dir))))

(deftest current-directory (t current-directory-12)
  "Test setting the `current-directory' to \"./\"."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (setf (current-directory) "./")
	  (string= (namestring (truename (current-directory)))
		   (namestring dir)))
      (delete-dir dir))))

(deftest current-directory (t current-directory-13)
  "Test setting the `current-directory' to \"../\"."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/")
	  (in-directory "a/"
	    (setf (current-directory) "../")
	    (string= (namestring (truename (current-directory)))
		     (namestring dir))))
      (dired:delete-file dir :recurse t))))

(deftest current-directory (t current-directory-14)
  "Test setting the `current-directory' to the empty string."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (setf (current-directory) "")
	  (string= (namestring (truename (current-directory)))
		   (namestring dir)))
      (dired:delete-file dir :recurse t))))


;;;; Errors.

(deftest current-directory (t current-directory-20)
  "Test setting the `current-directory' to false."
  (let ((dir (pick-new-dir))
	ret)
    (unwind-protect
	(in-directory dir
	  (handler-case
	      (setf (current-directory) ())
	    (error () (setq ret t))))
      (delete-dir dir))
    ret))

(deftest current-directory (t current-directory-21)
  "Test setting the `current-directory' to a missing directory."
  (let ((dir (pick-new-dir))
	ret)
    (delete-dir dir)
    (handler-case
	(setf (current-directory) dir)
      (error () (setq ret t)))
    ret))

(deftest current-directory (t current-directory-22)
  "Test setting the `current-directory' to a file."
  (let ((dir (pick-new-file))
	ret)
    (unwind-protect
	(handler-case
	    (setf (current-directory) dir)
	  (error () (setq ret t)))
      (delete-file dir))
    ret))
