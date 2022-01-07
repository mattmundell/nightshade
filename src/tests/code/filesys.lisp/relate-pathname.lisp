;;; Tests of lisp::relate-pathname.

(in-package "LISP")

(import '(deftest:deftest))

;; FIX dired:delete-file
(defun flush-tree (dir)
  "Flush dir recursively."
  (do-files (file dir :recurse t)
    (if (directoryp file)
	(flush-tree file)
	(delete-file file)))
  (delete-dir dir))


;;;; Normal cases.

(deftest relate-pathname (#p"b" relate-pathname-0)
  "Test `relate-pathname' with two leaf files."
  (relate-pathname "b" "d"))

(deftest relate-pathname (#p"a/b" relate-pathname-1)
  "Test `relate-pathname' with absolute pathnames."
  (relate-pathname "a/b" "d"))

;; FIX clean up dirs

(deftest relate-pathname (#p"a/b" relate-pathname-2)
  "Test `relate-pathname' where the source is one up."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/")
	  (in-directory "a/"
	    (relate-pathname "b" "../d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"a1/a2/b" relate-pathname-3)
  "Test `relate-pathname' where the source is two up."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/a2/")
	  (in-directory "a1/a2/"
	    (relate-pathname "b" "../../d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../b" relate-pathname-4)
  "Test `relate-pathname' where the source is one down."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/")
	  (relate-pathname "b" "a1/d"))
      (flush-tree dir))))

(deftest relate-pathname (#p"../../b" relate-pathname-5)
  "Test `relate-pathname' where the source is two down."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/a2/")
	  (relate-pathname "b" "a1/a2/d"))
      (flush-tree dir))))

(deftest relate-pathname (#p"../b1/b" relate-pathname-6)
  "Test `relate-pathname' where the source is up and down, by one."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "b1/")
	  (ensure-directories-exist "a1/")
	  (in-directory "b1/"
	    (relate-pathname "b" "../a1/d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../../b1/b2/b" relate-pathname-7)
  "Test `relate-pathname' where the source is up and down, by two."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/a2/")
	  (ensure-directories-exist "b1/b2/")
	  (in-directory "b1/b2/"
	    (relate-pathname "b" "../../a1/a2/d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../../b1/b" relate-pathname-8)
  "Test `relate-pathname' where the source is up one and down two."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/a2/")
	  (ensure-directories-exist "b1/")
	  (in-directory "b1/"
	    (relate-pathname "b" "../a1/a2/d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../b" relate-pathname-9)
  "Test `relate-pathname' where the source is up between two downs."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "b1/a1/")
	  (ensure-directories-exist "b1/a2/")
	  (in-directory "b1/"
	    (relate-pathname "b" "a1/../a2/d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../a2/b" relate-pathname-10)
  "Test `relate-pathname' where the source is up between two downs and the
   destination is down one."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "b1/a1/")
	  (ensure-directories-exist "b1/a2/")
	  (in-directory "b1/"
	    (relate-pathname "a2/b" "a1/../a2/d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../../b1/b2/b" relate-pathname-11)
  "Test `relate-pathname' where the source is up one and down two, and the
   destination is down one."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/a2/")
	  (ensure-directories-exist "b1/b2/")
	  (in-directory "b1/"
	    (relate-pathname "b2/b" "../a1/a2/d")))
      (flush-tree dir))))

(deftest relate-pathname (#p"../b" relate-pathname-12)
  "Test `relate-pathname' where the source is two down then one up."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a1/a2/")
	  (ensure-directories-exist "b1/")
	  (in-directory "b1/"
	    (relate-pathname "b" "a1/a2/../d")))
      (flush-tree dir))))

(deftest relate-pathname ("/a/b" relate-pathname-13)
  "Test `relate-pathname' with absolute source."
  (relate-pathname "/a/b" "d"))

(deftest relate-pathname ("/a/b" relate-pathname-14)
  "Test `relate-pathname' with absolute source and destination."
  (relate-pathname "/a/b" "/d"))


;;;; Error case.

(deftest relate-pathname (t relate-pathname-15)
  "Test `relate-pathname' with absolute destination."
  (handler-case
      (progn (relate-pathname "a/b" "/d") ())
    (error () t)))
