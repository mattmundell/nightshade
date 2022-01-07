;;; Tests of lisp:add-dir.

(in-package "LISP")

(import '(deftest:deftest))

(deftest add-dir (t add-dir-0)
  "Test `add-dir'."
  (let ((dir (pick-new-dir)))
    (delete-dir dir)
    (add-dir dir)
    (unwind-protect
	(if (probe-file dir) t)
      (delete-dir dir))))

(deftest add-dir (t add-dir-1)
  "Test `add-dir', where the directory exists already."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(handler-case
	    (progn
	      (add-dir dir)
	      ())
	  (file-error ()
		      (return-from add-dir-1 t)))
      (delete-dir dir))))
