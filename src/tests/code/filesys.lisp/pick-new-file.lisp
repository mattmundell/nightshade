;;; Tests of `lisp:pick-new-file'.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))


;;;; Auto argument.

(deftest pick-new-file (t pick-new-file-1)
  "Test `pick-new-file', picking a single name."
  (let ((file (pick-new-file)))
    (prog1 (if (probe-file file) t)
      (delete-file file))))

(deftest pick-new-file (t pick-new-file-2)
  "Test `pick-new-file', picking many names."
  (collect ((names))
    (unwind-protect
	(progn
	  (dotimes (num 40)
	    (names (pick-new-file)))
	  (fi (duplicatesp (names))
	      (and (eq (length (names)) 40)
		   (dolist (file (names) t)
		     (or (probe-file file)
			 (return-from pick-new-file-2 ()))))))
      (dolist (name (names))
	(delete-file name)))))


;;;; File argument.

(deftest pick-new-file (t pick-new-file-20)
  "Test `pick-new-file', passing an absolute file name arg."
  (with-test-dir (dir)
    (let ((file (pick-new-file
		 (namestring (merge-pathnames "pick-new-~D-~D"
					      dir)))))
      (prog1 (if (probe-file file) t)
	(delete-file file)))))

(deftest pick-new-file (t pick-new-file-21)
  "Test `pick-new-file', passing a relative file name arg."
  (with-test-dir (dir "eg")
    (in-directory dir
      (let ((file (pick-new-file "pick-new-file-test-~D-~D.c")))
	(prog1 (if (probe-file file) t)
	  (delete-file file))))))

(deftest pick-new-file (t pick-new-file-22)
  "Test `pick-new-file', picking many times, passing the same absolute file
   name arg each time."
  (with-test-dir (dir "zz/zz/" "zz/zz/zz")
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-file
		(namestring (merge-pathnames "pick test ~D-~D"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-file (t pick-new-file-23)
  "Test `pick-new-file', picking many times, passing the same relative file
   name arg each time."
  (with-test-dir (dir)
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file "~D~D")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Hidden file argument.

(deftest pick-new-file (t pick-new-file-30)
  "Test `pick-new-file', passing an absolute hidden file name arg."
  (with-test-dir (dir)
    (let ((file (pick-new-file
		 (namestring (merge-pathnames ".pick-new-~D-~D" dir)))))
      (prog1 (if (probe-file file) t)
	(delete-file file)))))

(deftest pick-new-file (t pick-new-file-31)
  "Test `pick-new-file', passing a relative hidden file name arg."
  (with-test-dir (dir "abc.Z")
    (in-directory dir
      (let ((file (pick-new-file ".pick-new-file-test-~D-~D")))
	(prog1 (if (probe-file file) t)
	  (delete-file file))))))

(deftest pick-new-file (t pick-new-file-32)
  "Test `pick-new-file', picking many times, passing the same absolute
   hidden file name arg each time."
  (with-test-dir (dir ".pick-test-1-1")
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-file
		(namestring (merge-pathnames ".pick-test-~D-~D"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-file (t pick-new-file-33)
  "Test `pick-new-file', picking many times, passing the same relative
   hidden file name arg each time."
  (with-test-dir (dir)
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file ".~D~D")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Backup file argument.

(deftest pick-new-file (t pick-new-file-40)
  "Test `pick-new-file', passing an absolute file backup name arg."
  (with-test-dir (dir)
    (let ((file (pick-new-file
		 (namestring (merge-pathnames "pick-new-~D-~D~~" dir)))))
      (prog1 (if (probe-file file) t)
	(delete-file file)))))

(deftest pick-new-file (t pick-new-file-41)
  "Test `pick-new-file', passing a relative backup file name arg."
  (with-test-dir (dir "pick-1000-1000.BAK" "a/b/")
    (in-directory dir
      (let ((file (pick-new-file "pick-~D-~D.BAK")))
	(prog1 (if (probe-file file) t)
	  (delete-file file))))))

(deftest pick-new-file (t pick-new-file-42)
  "Test `pick-new-file', picking many times, passing the same absolute file
   name arg each time."
  (with-test-dir (dir)
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-file
		(namestring (merge-pathnames "pick-test-~D-~D.CKP"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-file (t pick-new-file-43)
  "Test `pick-new-file', picking many times, passing the same relative file
   name arg each time."
  (with-test-dir (dir)
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file ",pick-test-~D-~D.BAK")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Search list argument.

(deftest pick-new-file (t pick-new-file-50)
  "Test `pick-new-file', passing a search list file name arg."
  (with-test-dir (dir "a" "b" "c/d/")
    (with-test-search-list ("a" dir)
      (let ((file (pick-new-file
		   (namestring (merge-pathnames "a:pick-new-~D-~D" dir)))))
	(prog1 (if (probe-file file) t)
	  (delete-file file))))))

(deftest pick-new-file (t pick-new-file-51)
  "Test `pick-new-file', picking many times, passing the same search list
   file name arg each time."
  (with-test-dir (dir)
    (with-test-search-list ("a" dir)
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file
		  (namestring (merge-pathnames "a:pick-test-~D-~D"
					       dir)))))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Symlink argument.

(deftest pick-new-file (t pick-new-file-60)
  "Test `pick-new-file', passing an absolute file name arg with a symlink
   in the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (let ((file (pick-new-file
		 (namestring (merge-pathnames "l/pick-new-~D-~D" dir)))))
      (prog1 (if (probe-file file) t)
	(delete-file file)))))

(deftest pick-new-file (t pick-new-file-61)
  "Test `pick-new-file', passing a relative file name arg with a symlink in
   the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (in-directory dir
      (let ((file (pick-new-file "l/pick-new-file-test-~D-~D")))
	(prog1 (if (probe-file file) t)
	  (delete-file file))))))

(deftest pick-new-file (t pick-new-file-62)
  "Test `pick-new-file', picking many times, passing the same absolute file
   name arg with a symlink in the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-file
		(namestring (merge-pathnames "l/pick-test-~D-~D"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-file (t pick-new-file-63)
  "Test `pick-new-file', picking many times, passing the same relative
   search list file name arg with a symlink in the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file "l/pick-test-~D-~D")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Relative pathname argument.

(deftest pick-new-file (t pick-new-file-71)
  "Test `pick-new-file', passing a relative file name arg with a relative
   pathname."
  (with-test-dir (dir "a/b/")
    (in-directory dir
      (in-directory "a/b/"
	(let ((file (pick-new-file "../../pick-new-file-test-~D-~D")))
	  (prog1 (if (probe-file file)
		     (string= (namestring
			       (truename (directory-namestring file)))
			      (namestring dir)))
	    (delete-file file)))))))

(deftest pick-new-file (t pick-new-file-72)
  "Test `pick-new-file', picking many times, passing the same relative
   search list file name arg with a relative pathname."
  (with-test-dir (dir "a/b/")
    (in-directory (merge-pathnames "a/b/" dir)
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file "../pick-test-~D-~D")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ()))
		   (or (string= (namestring
				 (truename (directory-namestring file)))
				(namestring (merge-pathnames "a/" dir)))
		       (return ())))))))))

(deftest pick-new-file (t pick-new-file-73)
  "Test `pick-new-file', passing a relative file name arg with a relative
   pathname."
  (with-test-dir (dir "a/b/")
    (in-directory dir
      (let ((file (pick-new-file "./pick-new-file-test-~D-~D")))
	(prog1 (if (probe-file file)
		   (string= (namestring
			     (truename (directory-namestring file)))
			    (namestring dir)))
	  (delete-file file))))))

(deftest pick-new-file (t pick-new-file-74)
  "Test `pick-new-file', picking many times, passing the same relative
   search list file name arg with a relative pathname."
  (with-test-dir (dir "a/b/")
    (in-directory (merge-pathnames "a/b/" dir)
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-file ".././pick-test-~D-~D")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ()))
		   (or (string= (namestring
				 (truename (directory-namestring file)))
				(namestring (merge-pathnames "a/" dir)))
		       (return ())))))))))


;;;; Errors.

(deftest pick-new-file (t pick-new-file-100)
  "Test `pick-new-file', passing too few directives."
  (let (ret)
    (handler-case
	(pick-new-file "~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-file (t pick-new-file-101)
  "Test `pick-new-file', passing an empty string."
  (let (ret)
    (handler-case
	(pick-new-file "")
      (error () (setq ret t)))
    ret))

(deftest pick-new-file (t pick-new-file-102)
  "Test `pick-new-file', passing too many directives."
  (let (ret)
    (handler-case
	(pick-new-file "~D~D~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-file (t pick-new-file-103)
  "Test `pick-new-file', passing the wrong type of directive."
  (let (ret)
    (handler-case
	(pick-new-file "~A~D")
      (error () (setq ret t)))
    ret))

(deftest pick-new-file (t pick-new-file-104)
  "Test `pick-new-file', passing a pathname with wildcards."
  (let (ret)
    (handler-case
	(pick-new-file "~D~D*")
      (error () (setq ret t)))
    ret))

(deftest pick-new-file (t pick-new-file-105)
  "Test `pick-new-file', passing an empty pathname."
  (let (ret)
    (handler-case
	(pick-new-file ())
      (error () (setq ret t)))
    ret))

(deftest pick-new-file (t pick-new-file-106)
  "Test `pick-new-file', passing a directory name arg."
  (let (ret)
    (deftest:with-test-dir (dir)
      (handler-case
	  (pick-new-file (merge-pathnames "pick-test-~D-~D/" dir))
	(error () (setq ret t))))
    ret))

(deftest pick-new-file (t pick-new-file-107)
  "Test `pick-new-file' on a missing dir."
  (deftest:with-test-dir (dir)
    (let (ret)
      (handler-case
	  (pick-new-file (merge-pathnames "pick-new/file-~D-~D" dir))
	(error () (setq ret t)))
      ret)))

(deftest pick-new-file (t pick-new-file-108)
  "Test `pick-new-file' on a read-blocked dir."
  (deftest:with-test-dir (dir)
    (setf (file-mode dir) "a-rwx")
    (let (ret)
      (unwind-protect
	  (handler-case
	      (pick-new-file
	       (namestring (merge-pathnames "cde~D~D" dir)))
	    (error () (setq ret t)))
	(setf (file-mode dir) "a+rwx"))
      ret)))

(deftest pick-new-file (t pick-new-file-109)
  "Test `pick-new-file' on a symlink to a file."
  (deftest:with-test-dir (dir "a" ("l" "a"))
    (let (ret)
      (handler-case
	  (pick-new-file
	   (namestring (merge-pathnames "l/~D~D" dir)))
	(error () (setq ret t)))
      ret)))
