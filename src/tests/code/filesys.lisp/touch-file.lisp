;;; Tests of lisp:touch-file.

(in-package "LISP")

(import '(deftest:deftest))

(deftest touch-file (t touch-file-0)
  "Test `touch-file' with an empty directory."
  (let* ((dir (pick-new-dir))
	 (time (1- (get-universal-time))))
    (unwind-protect
	(progn
	  (setf (file-write-date dir) time)
	  (> (touch-file dir) time))
      (delete-dir dir))))

(deftest touch-file (t touch-file-1)
  "Test `touch-file' with an empty file."
  (let* ((file (pick-new-file))
	 (time (1- (get-universal-time))))
    (unwind-protect
	(progn
	  (setf (file-write-date file) time)
	  (> (touch-file file) time))
      (delete-file file))))

(deftest touch-file (t touch-file-2)
  "Test `touch-file' with a symlink to an empty file."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(time (1- (get-universal-time))))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (unwind-protect
	      (progn
		(setf (file-write-date file) time)
		(setf (file-write-date link) time)
		(and (> (touch-file link) time)
		     (> (file-write-date file) time)
		     (> (file-write-date link) time)))
	    (delete-file link)))
      (delete-file file))))

(deftest touch-file (t touch-file-3)
  "Test `touch-file' with a symlink to an empty file."
  (let ((dir (pick-new-dir))
	(link (pick-new-file))
	(time (1- (get-universal-time))))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (unwind-protect
	      (progn
		(setf (file-write-date dir) time)
		(setf (file-write-date link) time)
		(and (> (touch-file link) time)
		     (> (file-write-date dir) time)
 		     (> (file-write-date link :check-for-links t)
			time)))
	    (delete-file link)))
      (delete-dir dir))))

(deftest touch-file (t touch-file-4)
  "Test `touch-file' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(time (1- (get-universal-time))))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  ;; FIX
	  (lisp::set-file-write-date link time :check-for-links t)
	  (touch-file link)
	  (and (> (file-write-date link :check-for-links t) time)
	       (> (file-write-date link) time)))
      (if (probe-file file) (delete-file file))
      (delete-file link))))

#|
(deftest touch-file touch-file-5 0
  "Test `touch-file' on a special file."
  (touch-file "/dev/null"))
|#

(defconstant touch-file-6-string "12345")

(deftest touch-file (t touch-file-6)
  "Test `touch-file' on a file with content."
  (let ((file (pick-new-file))
	(time (1- (get-universal-time))))
    (with-open-file (stream file :direction :output)
      (write-string touch-file-6-string stream))
    (unwind-protect
	(progn
	  (setf (file-write-date file) time)
	  (and (> (touch-file file) time)
	       (> (file-write-date file) time)
	       (equal (file-size file) (length touch-file-6-string))))
      (delete-file file))))

(deftest touch-file (t touch-file-7)
  "Test `touch-file' where the file is created."
  (let ((file (pick-new-file)))
    (delete-file file)
    (touch-file file)
    (prog1 (if (probe-file file) t)
      (if (probe-file file) (delete-file file)))))
