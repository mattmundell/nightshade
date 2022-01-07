;;; Tests of lisp:file-stats.

(in-package "LISP")

(import '(deftest:deftest))

(defun verbose (format &rest args)
  "Print a formatted message and a newline."
  (apply #'format t format args)
  (terpri))

;; FIX set and check for mode

(deftest file-stats (t file-stats-0)
  "Test `file-stats' with an empty directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(multiple-value-bind
	    (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
	    (file-stats dir)
	  (declare (ignore dev-or-err ino mode uid gid rdev size))
	  (and pass
	       (equal nlink 2)
	       (equal atime mtime)
	       (equal name dir)))
      (delete-dir dir))))

(deftest file-stats (t file-stats-1)
  "Test `file-stats' with an empty file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(multiple-value-bind
	    (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
	    (file-stats file)
	  (declare (ignore dev-or-err ino mode uid gid rdev))
	  (and pass
	       (equal nlink 1)
	       (equal atime mtime)
	       (equal name file)
	       (zerop size)))
      (delete-file file))))

(deftest file-stats (t file-stats-2)
  "Test `file-stats' on an empty file via a symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (unwind-protect
	      (multiple-value-bind
		  (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
		  (file-stats link)
		(declare (ignore dev-or-err ino mode uid gid rdev))
		(and pass
		     (equal nlink 1)
		     (zerop size)
		     (equal atime mtime)
		     (equal name link)))
	    (delete-file link)))
      (delete-file file))))

(deftest file-stats (t file-stats-3)
  "Test `file-stats' on a symlink to an empty file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (unwind-protect
	      (multiple-value-bind
		  (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
		  (file-stats link :check-for-links t)
		(declare (ignore dev-or-err ino mode uid gid rdev size))
		(and pass
		     (equal nlink 1)
		     (equal atime mtime)
		     (equal name link)))
	    (delete-file link)))
      (delete-file file))))

(deftest file-stats (t file-stats-4)
  "Test `file-stats' on an empty directory via a symlink."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (unwind-protect
	      (multiple-value-bind
		  (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
		  (file-stats link)
		(declare (ignore dev-or-err ino mode uid gid rdev size))
		(and pass
		     (equal nlink 2)
		     (equal atime mtime)
		     (equal name link)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest file-stats (t file-stats-5)
  "Test `file-stats' on a symlink to an empty directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (unwind-protect
	      (multiple-value-bind
		  (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
		  (file-stats link :check-for-links t)
		(declare (ignore dev-or-err ino mode uid gid rdev size))
		(and pass
		     (equal nlink 1)
		     (equal atime mtime)
		     (equal name link)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest file-stats (t file-stats-6)
  "Test `file-stats' on a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(multiple-value-bind
	    (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
	    (file-stats link :check-for-links t)
	  (declare (ignore dev-or-err ino mode uid gid rdev size))
	  (and pass
	       (equal nlink 1)
	       (equal atime mtime)
	       (equal name link)))
      (delete-file link))))

(deftest file-stats (() file-stats-7)
  "Test `file-stats' on the destination of a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(file-stats link)
      (delete-file link))))

(deftest file-stats (t file-stats-8)
  "Test `file-stats' with a special file."
  (multiple-value-bind
      (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
      (file-stats "/dev/null")
    (declare (ignore dev-or-err ino mode uid gid rdev size))
    (and pass
	 (equal nlink 1)
	 (equal atime mtime)
	 (equal name "/dev/null"))))

(defconstant file-stats-9-string "12345")

(deftest file-stats (t file-stats-9)
  "Test `file-stats' on a file with content."
  (let ((file (pick-new-file)))
    (with-open-file (stream file :direction :output)
      (write-string file-stats-9-string stream))
    (unwind-protect
	(multiple-value-bind
	    (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
	    (file-stats file)
	  (declare (ignore dev-or-err ino mode uid gid rdev))
	  (and pass
	       (equal nlink 1)
	       (equal atime mtime)
	       (equal name file)
	       (equal size (length file-stats-9-string))))
      (delete-file file))))

(defconstant file-stats-10-string "asd;lhgoei2-39hbal;dkfj
")

(deftest file-stats (t file-stats-10)
  "Test `file-stats' on a symlink to a file with content."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (with-open-file (stream file :direction :output)
      (write-string file-stats-10-string stream))
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (unwind-protect
	      (multiple-value-bind
		  (pass dev-or-err ino mode nlink uid gid rdev size atime mtime name)
		  (file-stats link)
		(declare (ignore dev-or-err ino mode uid gid rdev))
		(and pass
		     (equal nlink 1)
		     (equal atime mtime)
		     (equal name link)
		     (equal size (length file-stats-10-string))))
	    (delete-file link)))
      (delete-file file))))
