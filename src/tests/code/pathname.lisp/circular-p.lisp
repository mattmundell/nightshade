;;; Tests of lisp:circular-p.

(in-package "LISP")

(import '(deftest:deftest))

(deftest circular-p (() circular-p-0)
  "Test `circular-p' with a dir."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(circular-p dir)
      (delete-dir dir))))

(deftest circular-p (() circular-p-1)
  "Test `circular-p' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(circular-p file)
      (delete-file file))))

(deftest circular-p (t circular-p-2)
  "Test `circular-p' with an absolute circular link, giving an absolute
   pathname."
  (let* ((dir (pick-new-dir))
	 (link (merge-pathnames dir "a")))
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (circular-p link))
      (delete-file link)
      (delete-dir dir))))

(deftest circular-p (t circular-p-3)
  "Test `circular-p' with a relative circular link, giving an absolute
   pathname."
  (let* ((dir (pick-new-dir))
	 (link (merge-pathnames dir "a")))
    (unwind-protect
	(progn
	  (in-directory dir
	    (symlink-file "a" ".."))
	  (circular-p link))
      (delete-file link)
      (delete-dir dir))))

(deftest circular-p (t circular-p-4)
  "Test `circular-p' with an absolute circular link, giving a relative
   pathname."
  (let* ((dir (pick-new-dir))
	 (link (merge-pathnames dir "a")))
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (in-directory dir
	    (circular-p "a")))
      (delete-file link)
      (delete-dir dir))))

(deftest circular-p (t circular-p-5)
  "Test `circular-p' with a relative circular link, giving a relative
   pathname."
  (let* ((dir (pick-new-dir))
	 (link (merge-pathnames dir "a")))
    (unwind-protect
	(in-directory dir
	  (symlink-file "a" "..")
	  (circular-p "a"))
      (delete-file link)
      (delete-dir dir))))

(deftest circular-p (() circular-p-6)
  "Test `circular-p' with a relative circular link, giving a relative
   pathname with a trailing slash."
  (let* ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/b/")
	  (in-directory "a/"
	    (symlink-file "l" "b/"))
	  (circular-p "a/l/"))
      (let ((dired:*report-function* #'t))
	(dired:delete-file dir :recurse t)))))
