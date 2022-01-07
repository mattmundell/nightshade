;;; Tests of lisp:file-name-p.

(in-package "LISP")

(import '(deftest:deftest))

(deftest file-name-p (() file-name-p-0)
  "Test `file-name-p' with a directory name."
  (file-name-p "/a/b/c/"))

(deftest file-name-p (t file-name-p-1)
  "Test `file-name-p' with a file name."
  (file-name-p "/a/b/c"))

(deftest file-name-p (t file-name-p-2)
  "Test `file-name-p' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(and (file-name-p (namify dir))
	     (fi (file-name-p (directorify dir))))
      (delete-dir dir))))

(deftest file-name-p (t file-name-p-3)
  "Test `file-name-p' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(file-name-p file)
      (delete-file file))))

(deftest file-name-p (t file-name-p-4)
  "Test `file-name-p' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (file-name-p link)
	    (delete-file link)))
      (delete-file file))))

(deftest file-name-p (t file-name-p-5)
  "Test `file-name-p' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (file-name-p (namify link))
		   (fi (file-name-p (directorify link))))
	    (delete-file link)))
      (delete-dir dir))))

(deftest file-name-p (t file-name-p-6)
  "Test `file-name-p' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(file-name-p (directorify link))
      (delete-file link))))
