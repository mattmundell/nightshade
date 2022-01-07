;;; Tests of lisp:directory-name-p.

(in-package "LISP")

(import '(deftest:deftest))

(deftest directory-name-p (t directory-name-p-0)
  "Test `directory-name-p' with a directory name."
  (directory-name-p "/a/b/c/"))

(deftest directory-name-p (() directory-name-p-1)
  "Test `directory-name-p' with a file name."
  (directory-name-p "/a/b/c"))

(deftest directory-name-p (t directory-name-p-2)
  "Test `directory-name-p' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(and (directory-name-p (directorify dir))
	     (fi (directory-name-p (namify dir))))
      (delete-dir dir))))

(deftest directory-name-p (() directory-name-p-3)
  "Test `directory-name-p' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(directory-name-p file)
      (delete-file file))))

(deftest directory-name-p (() directory-name-p-4)
  "Test `directory-name-p' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (directory-name-p link)
	    (delete-file link)))
      (delete-file file))))

(deftest directory-name-p (t directory-name-p-5)
  "Test `directory-name-p' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (and (directory-name-p (directorify link))
		   (fi (directory-name-p (namify link))))
	    (delete-file link)))
      (delete-dir dir))))

(deftest directory-name-p (() directory-name-p-6)
  "Test `directory-name-p' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(directory-name-p (directorify link))
      (delete-file link))))

(deftest directory-name-p (t directory-name-p-7)
  "Test `directory-name-p' with a directory name with a version."
  (directory-name-p (make-pathname :version :newest)))
