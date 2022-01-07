;;; Tests of lisp:truncate-file.

(in-package "LISP")

(import '(deftest:deftest))

(deftest truncate-file (t truncate-file-0)
  "Test `truncate-file' with an empty directory."
  (let* ((dir (pick-new-dir))
	 (size (file-size dir)))
    (unwind-protect
	(progn
	  (ignore-errors (truncate-file dir))
	  (equal size (file-size dir)))
      (delete-dir dir))))

(deftest truncate-file (0 truncate-file-1)
  "Test `truncate-file' with an empty file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(progn
	  (truncate-file file)
	  (file-size file))
      (delete-file file))))

(deftest truncate-file (0 truncate-file-2)
  "Test `truncate-file' with a symlink to an empty file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (truncate-file link)
	  (file-size link))
      (delete-file file)
      (delete-file link))))

(deftest truncate-file (t truncate-file-3)
  "Test `truncate-file' with a symlink to an empty directory."
  (let* ((dir (pick-new-dir))
	 (link (pick-new-file))
	 (size (file-size dir)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (ignore-errors (truncate-file link))
	  (equal size (file-size dir)))
      (delete-file link)
      (delete-dir dir))))

(deftest truncate-file (() truncate-file-4)
  "Test `truncate-file' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (ignore-errors (truncate-file link))
	  (file-size link))
      (delete-file link))))

#|
(deftest truncate-file (0 truncate-file-5)
  "Test `truncate-file' on a special file."
  (truncate-file "/dev/null"))
|#

(defconstant truncate-file-6-string "12345")

(deftest truncate-file (0 truncate-file-6)
  "Test `truncate-file' on a file with content."
  (let ((file (pick-new-file)))
    (with-open-file (stream file :direction :output)
      (write-string truncate-file-6-string stream))
    (unwind-protect
	(progn
	  (truncate-file file)
	  (file-size file))
      (delete-file file))))

(defconstant truncate-file-7-string "asd;lhgoei2-39hbal;dkfj
")

(deftest truncate-file (0 truncate-file-7)
  "Test `truncate-file' on a symlink to a file with content."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output)
	    (write-string truncate-file-7-string stream))
	  (symlink-file link file)
	  (truncate-file link)
	  (file-size link))
      (delete-file file)
      (delete-file link))))
