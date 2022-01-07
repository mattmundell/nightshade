;;; Tests of sync:build-server-pathname.

(in-package "UNIX")

(use-package "DEFTEST")

(deftest unix-resolve-links ("/dev/null" unix-resolve-links-0)
  "Test unix-resolve-links with a special file."
  (unix-resolve-links "/dev/null"))

(deftest unix-resolve-links (t unix-resolve-links-1)
  "Test unix-resolve-links with a file."
  (let ((file (pick-new-file)))
    (prog1
	(string= file (unix-resolve-links file))
      (delete-file file))))

(deftest unix-resolve-links (t unix-resolve-links-2)
  "Test unix-resolve-links with a directory."
  (let ((dir (pick-new-dir)))
    (prog1
	(string= dir (unix-resolve-links dir))
      (delete-dir dir))))

(deftest unix-resolve-links (t unix-resolve-links-3)
  "Test unix-resolve-links with a relative symlink to a file."
  (let ((dir (pick-new-dir)))
    (prog1
	(in-directory dir
	  (touch-file "f")
	  (symlink-file "s" "f")
	  (prog1
	      (string= "f" (unix-resolve-links "s"))
	    (delete-file "s")
	    (delete-file "f")))
      (delete-dir dir))))

(deftest unix-resolve-links (t unix-resolve-links-4)
  "Test unix-resolve-links with a relative symlink to a dir."
  (let ((dir (pick-new-dir)))
    (prog1
	(in-directory dir
	  (ensure-directories-exist "d/")
	  (symlink-file "s" "d")
	  (prog1
	      (string= "d" (unix-resolve-links "s"))
	    (delete-file "s")
	    (delete-dir "d")))
      (delete-dir dir))))

(deftest unix-resolve-links (t unix-resolve-links-5)
  "Test unix-resolve-links with a self-referencing symlink."
  (let ((dir (pick-new-dir)))
    (prog1
	(in-directory dir
	  (symlink-file "s" "s")
	  (prog1
	      (string= "s" (unix-resolve-links "s"))
	    (delete-file "s")))
      (delete-dir dir))))

(deftest unix-resolve-links (() unix-resolve-links-6)
  "Test unix-resolve-links with a broken symlink."
  (let ((dir (pick-new-dir)))
    (prog1
	(in-directory dir
	  (symlink-file "s" "x")
	  (prog1
	      (unix-resolve-links "s")
	    (delete-file "s")))
      (delete-dir dir))))
