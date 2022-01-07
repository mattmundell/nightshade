;;; Tests of lisp:namify.

(in-package "LISP")

(import '(deftest:deftest))

(deftest namify ("/a/b/c" namify-0)
  "Test `namify' with a directory name."
  (namify "/a/b/c/"))

(deftest namify ("/a/b/c" namify-1)
  "Test `namify' with a file name."
  (namify "/a/b/c"))

(deftest namify (() namify-2)
  "Test `namify' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((name (namify dir)))
	  (char= (aref name (1- (length name))) #\/))
      (delete-dir dir))))

(deftest namify (() namify-3)
  "Test `namify' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(let ((name (namify file)))
	  (char= (aref name (1- (length name))) #\/))
      (delete-file file))))

(deftest namify (() namify-4)
  "Test `namify' with a symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (let ((name (namify link)))
		(char= (aref name (1- (length name))) #\/))
	    (delete-file link)))
      (delete-file file))))

(deftest namify (() namify-5)
  "Test `namify' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (unwind-protect
	(let ((name (namify link)))
	  (char= (aref name (1- (length name))) #\/))
      (delete-file link))))

(deftest namify ("" namify-6)
  "Test `namify' with an empty string."
  (namify ""))
