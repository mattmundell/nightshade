;;; Tests of lisp:copy-dir.

#|
(in-package "LISP")

(import '(deftest:deftest))

(deftest copy-dir copy-dir-0 t
  "Test `copy-dir' with an empty directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/")
	  (copy-dir "a" "b")
	  (prog1
	      (>= (file-write-date "a") (file-write-date "b"))
	    (ignore-errors (delete-dir "a/"))
	    (ignore-errors (delete-dir "b/"))))
      (delete-dir dir))))
|#
