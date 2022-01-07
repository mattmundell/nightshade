;;; Tests of lisp:directorify.

(in-package "LISP")

(import '(deftest:deftest))

(deftest directorify (#\/ directorify-0)
  "Test `directorify' with a directory."
  (let* ((dir (pick-new-dir))
	 (name (namestring (directorify dir))))
    (unwind-protect
	(aref name (1- (length name)))
      (delete-dir dir))))

(deftest directorify (() directorify-1)
  "Test `directorify' with a name."
  (let ((dir (pick-new-dir)))
    (delete-dir dir)
    (let ((name (namestring (directorify (namify dir)))))
      (char= (aref name (1- (length name))) #\/))))

(deftest directorify (() directorify-2)
  "Test `directorify' with a file."
  (let* ((file (pick-new-file))
	 (name (namestring (directorify file))))
    (unwind-protect
	(char= (aref name (1- (length name))) #\/)
      (delete-file file))))

(deftest directorify (#\/ directorify-3)
  "Test `directorify' with a symlink."
  (let ((dir (pick-new-dir))
	(file (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file file)
	  (symlink-file file dir)
	  (let ((name (namestring (directorify file))))
	    (aref name (1- (length name)))))
      (delete-dir dir)
      (delete-file file))))

(deftest directorify (() directorify-4)
  "Test `directorify' with a broken symlink."
  (let ((dir (pick-new-dir))
	(file (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file file)
	  (symlink-file file dir)
	  (delete-dir dir)
	  (let ((name (namestring (directorify file))))
	    (char= (aref name (1- (length name))) #\/)))
      (delete-file file))))
