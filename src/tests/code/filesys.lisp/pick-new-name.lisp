;;; Tests of `lisp:pick-new-name'.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))


;;;; Auto argument.

(deftest pick-new-name (() pick-new-name-1)
  "Test `pick-new-name', picking a single name."
  (probe-file (pick-new-name)))

(deftest pick-new-name (t pick-new-name-2)
  "Test `pick-new-name', picking many names."
  (collect ((names))
    (dotimes (num 400)
      (names (pick-new-name)))
    (fi (duplicatesp (names))
	(and (eq (length (names)) 400)
	     (dolist (file (names) t)
	       (if (probe-file file) (return ())))))))


;;;; Given an argument.

(deftest pick-new-name (() pick-new-name-20)
  "Test `pick-new-name', passing an absolute file name arg."
  (probe-file (pick-new-name "/tmp/pick-new-name-test-~D-~D")))

(deftest pick-new-name (() pick-new-name-21)
  "Test `pick-new-name', passing a relative file name arg."
  (with-test-dir (dir)
    (in-directory dir
      (probe-file (pick-new-name "pick-new-name-test-~D-~D")))))

(deftest pick-new-name (() pick-new-name-22)
  "Test `pick-new-name', picking many times, passing the same absolute file
   name arg each time."
  (collect ((names))
    (dotimes (num 80)
      (names (pick-new-name "/tmp/pick-new-name-test-~D-~D")))
    (and (duplicatesp (names))
	 (dolist (file (names) t)
	   (if (probe-file file) (return t))))))

(deftest pick-new-name (t pick-new-name-23)
  "Test `pick-new-name', picking many times, passing the same relative file
   name arg each time."
  (with-test-dir (dir)
    (collect ((names))
      (in-directory dir
	(dotimes (num 80)
	  (names (pick-new-name "/tmp/pick-new-name-test-~D-~D")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (if (probe-file file) (return ())))))))))

(deftest pick-new-name (() pick-new-name-24)
  "Test `pick-new-name', passing an absolute directory name arg."
  (probe-file (pick-new-name "/tmp/pick-new-test-~D-~D/")))

(deftest pick-new-name (() pick-new-name-25)
  "Test `pick-new-name', passing a relative directory name arg."
  (with-test-dir (dir)
    (in-directory dir
      (probe-file (pick-new-name "pick-new-name-test-~D-~D/")))))

(deftest pick-new-name (t pick-new-name-26)
  "Test `pick-new-name', picking many times, passing the same absolute
   directory name arg each time."
  (collect ((names))
    (dotimes (num 80)
      (names (pick-new-name "/tmp/pick-new-name-test-~D-~D/")))
    (fi (duplicatesp (names))
	(and (eq (length (names)) 80)
	     (dolist (file (names) t)
	       (if (probe-file file) (return ())))))))

(deftest pick-new-name (t pick-new-name-27)
  "Test `pick-new-name', picking many times, passing the same relative
   directory name arg each time."
  (with-test-dir (dir)
    (collect ((names))
      (in-directory dir
	(dotimes (num 80)
	  (names (pick-new-name "pick-new-name-test-~D-~D/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (if (probe-file file) (return t)))))))))

(deftest pick-new-name (() pick-new-name-28)
  "Test `pick-new-name' on an absolute missing dir."
  (deftest:with-test-dir (dir)
    (probe-file (pick-new-name
		 (namestring (merge-pathnames "a/b/cde/~D~D"
					      dir))))))

(deftest pick-new-name (() pick-new-name-29)
  "Test `pick-new-name' on an absolute read-blocked dir."
  (deftest:with-test-dir (dir)
    (setf (file-mode dir) "a-rwx")
    (unwind-protect
	(probe-file
	 (pick-new-name
	  (namestring (merge-pathnames "a/b/cde~D~D" dir))))
      (setf (file-mode dir) "a+rwx"))))


;;;; Errors.

(deftest pick-new-name (t pick-new-name-40)
  "Test `pick-new-name', passing too few directives."
  (let (ret)
    (handler-case
	(pick-new-name "~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-name (t pick-new-name-41)
  "Test `pick-new-name', passing too many directives."
  (let (ret)
    (handler-case
	(pick-new-name "~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-name (t pick-new-name-42)
  "Test `pick-new-name', passing too many directives."
  (let (ret)
    (handler-case
	(pick-new-name "~D~D~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-name (t pick-new-name-43)
  "Test `pick-new-name', passing the wrong type of directive."
  (let (ret)
    (handler-case
	(pick-new-name "~A~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-name (t pick-new-name-44)
  "Test `pick-new-name', passing a pathname with wildcards."
  (let (ret)
    (handler-case
	(pick-new-name "~D~D*")
      (error () (setq ret t)))
    ret))

(deftest pick-new-name (t pick-new-name-45)
  "Test `pick-new-name', passing an empty pathname."
  (let (ret)
    (handler-case
	(pick-new-name ())
      (error () (setq ret t)))
    ret))
