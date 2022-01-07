;;; Tests of lisp:file-write-date.

(in-package "LISP")

(import '(deftest:deftest))

(deftest file-write-date (t file-write-date-0)
  "Test `file-write-date' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((time (get-universal-time)))
	  (setf (file-write-date dir) time)
	  (equal (file-write-date dir) time))
      (delete-dir dir))))

(deftest file-write-date (t file-write-date-1)
  "Test `file-write-date' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(let ((time (get-universal-time)))
	  (setf (file-write-date file) time)
	  (equal (file-write-date file) time))
      (delete-file file))))

(deftest file-write-date (t file-write-date-3)
  "Test `file-write-date' on a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (let ((time (get-universal-time))
		(file-time (file-write-date file)))
	    ;; FIX
	    (lisp::set-file-write-date link time :check-for-links t)
	    (and (equal (file-write-date file) file-time)
		 (equal (file-write-date link) time))))
      (delete-file link)
      (delete-file file))))

(deftest file-write-date (t file-write-date-4)
  "Test `file-write-date' on a file via a symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (let ((time (get-universal-time))
		(link-time (file-write-date link)))
	    (setf (file-write-date link) time)
	    (and (equal (file-write-date file) time)
		 (equal (file-write-date link) link-time))))
      (delete-file link)
      (delete-file file))))
