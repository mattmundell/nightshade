;;; Tests of lisp:file-size.

(in-package "LISP")

(import '(deftest:deftest))

#| FIX How to tell size that empty dir should be?
(deftest file-size (0 file-size-0)
  "Test `file-size' with an empty directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(file-size dir)
      (delete-dir dir))))
|#

(deftest file-size (0 file-size-1)
  "Test `file-size' with an empty file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(file-size file)
      (delete-file file))))

(deftest file-size (0 file-size-2)
  "Test `file-size' with a symlink to an empty file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-size link))
      (delete-file file)
      (delete-file link))))

#| FIX How to tell size that symlink should be?
(deftest file-size (0 file-size-3)
  "Test `file-size' with a symlink to an empty file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-size link :check-for-links t))
      (delete-file file)
      (delete-file link))))
|#

#| FIX How to tell size that empty dir should be?
(deftest file-size (0 file-size-4)
  "Test `file-size' with a symlink to an empty directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-size link))
      (delete-dir dir))))
|#

#| FIX How to tell size that symlink should be?
(deftest file-size (0 file-size-5)
  "Test `file-size' with a symlink to an empty directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-size link :check-for-links t))
      (delete-dir dir))))
|#

#| FIX How to tell size that symlink should be?
(deftest file-size (0 file-size-6)
  "Test `file-size' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-size link :check-for-links t))
      (delete-file link))))
|#

(deftest file-size (() file-size-7)
  "Test `file-size' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-size link))
      (delete-file link))))

(deftest file-size (0 file-size-8)
  "Test `file-size' with a special file."
  (file-size "/dev/null"))

(defconstant file-size-9-string "12345")

(deftest file-size ((length file-size-9-string) file-size-9)
  "Test `file-size' on a file with content."
  (let ((file (pick-new-file)))
    (with-open-file (stream file :direction :output)
      (write-string file-size-9-string stream))
    (unwind-protect
	(file-size file)
      (delete-file file))))

(defconstant file-size-10-string "asd;lhgoei2-39hbal;dkfj
")

(deftest file-size ((length file-size-10-string) file-size-10)
  "Test `file-size' on a symlink to a file with content."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output)
	    (write-string file-size-10-string stream))
	  (symlink-file link file)
	  (file-size link))
      (delete-file file)
      (delete-file link))))
