;;; Tests of `lisp:pick-new-dir'.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))


;;;; Auto argument.

(deftest pick-new-dir (t pick-new-dir-1)
  "Test `pick-new-dir', picking a single name."
  (let ((file (pick-new-dir)))
    (prog1 (if (probe-file file) t)
      (delete-dir file))))

(deftest pick-new-dir (t pick-new-dir-2)
  "Test `pick-new-dir', picking many names."
  (collect ((names))
    (unwind-protect
	(progn
	  (dotimes (num 40)
	    (names (pick-new-dir)))
	  (fi (duplicatesp (names))
	      (and (eq (length (names)) 40)
		   (dolist (file (names) t)
		     (or (probe-file file)
			 (return-from pick-new-dir-2 ()))))))
      (dolist (name (names))
	(delete-dir name)))))


;;;; File argument.

(deftest pick-new-dir (t pick-new-dir-20)
  "Test `pick-new-dir', passing an absolute file name arg."
  (with-test-dir (dir)
    (let ((file (pick-new-dir
		 (namestring (merge-pathnames "pick-new-~D-~D/"
					      dir)))))
      (prog1 (if (probe-file file) t)
	(delete-dir file)))))

(deftest pick-new-dir (t pick-new-dir-21)
  "Test `pick-new-dir', passing a relative file name arg."
  (with-test-dir (dir "eg")
    (in-directory dir
      (let ((file (pick-new-dir "pick-new-dir-test-~D-~D.c/")))
	(prog1 (if (probe-file file) t)
	  (delete-dir file))))))

(deftest pick-new-dir (t pick-new-dir-22)
  "Test `pick-new-dir', picking many times, passing the same absolute file
   name arg each time."
  (with-test-dir (dir "zz/zz/" "zz/zz/zz")
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-dir
		(namestring (merge-pathnames "pick test ~D-~D/"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-dir (t pick-new-dir-23)
  "Test `pick-new-dir', picking many times, passing the same relative file
   name arg each time."
  (with-test-dir (dir)
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir "~D~D/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Hidden file argument.

(deftest pick-new-dir (t pick-new-dir-30)
  "Test `pick-new-dir', passing an absolute hidden file name arg."
  (with-test-dir (dir)
    (let ((file (pick-new-dir
		 (namestring (merge-pathnames ".pick-new-~D-~D/" dir)))))
      (prog1 (if (probe-file file) t)
	(delete-dir file)))))

(deftest pick-new-dir (t pick-new-dir-31)
  "Test `pick-new-dir', passing a relative hidden file name arg."
  (with-test-dir (dir "abc.Z")
    (in-directory dir
      (let ((file (pick-new-dir ".pick-new-dir-test-~D-~D/")))
	(prog1 (if (probe-file file) t)
	  (delete-dir file))))))

(deftest pick-new-dir (t pick-new-dir-32)
  "Test `pick-new-dir', picking many times, passing the same absolute
   hidden file name arg each time."
  (with-test-dir (dir ".pick-test-1-1")
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-dir
		(namestring (merge-pathnames ".pick-test-~D-~D/"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-dir (t pick-new-dir-33)
  "Test `pick-new-dir', picking many times, passing the same relative
   hidden file name arg each time."
  (with-test-dir (dir)
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir ".~D~D/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Backup file argument.

(deftest pick-new-dir (t pick-new-dir-40)
  "Test `pick-new-dir', passing an absolute file backup name arg."
  (with-test-dir (dir)
    (let ((file (pick-new-dir
		 (namestring (merge-pathnames "pick-new-~D-~D~~/" dir)))))
      (prog1 (if (probe-file file) t)
	(delete-dir file)))))

(deftest pick-new-dir (t pick-new-dir-41)
  "Test `pick-new-dir', passing a relative backup file name arg."
  (with-test-dir (dir "pick-1000-1000.BAK/" "a/b/")
    (in-directory dir
      (let ((file (pick-new-dir "pick-~D-~D.BAK/")))
	(prog1 (if (probe-file file) t)
	  (delete-dir file))))))

(deftest pick-new-dir (t pick-new-dir-42)
  "Test `pick-new-dir', picking many times, passing the same absolute file
   name arg each time."
  (with-test-dir (dir)
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-dir
		(namestring (merge-pathnames "pick-test-~D-~D.CKP/"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-dir (t pick-new-dir-43)
  "Test `pick-new-dir', picking many times, passing the same relative file
   name arg each time."
  (with-test-dir (dir)
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir ",pick-test-~D-~D.BAK/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Search list argument.

(deftest pick-new-dir (t pick-new-dir-50)
  "Test `pick-new-dir', passing a search list file name arg."
  (with-test-dir (dir "a" "b" "c/d/")
    (with-test-search-list ("a" dir)
      (let ((file (pick-new-dir
		   (namestring (merge-pathnames "a:pick-new-~D-~D/" dir)))))
	(prog1 (if (probe-file file) t)
	  (delete-dir file))))))

(deftest pick-new-dir (t pick-new-dir-51)
  "Test `pick-new-dir', picking many times, passing the same search list
   file name arg each time."
  (with-test-dir (dir)
    (with-test-search-list ("a" dir)
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir
		  (namestring (merge-pathnames "a:pick-test-~D-~D/"
					       dir)))))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Symlink argument.

(deftest pick-new-dir (t pick-new-dir-60)
  "Test `pick-new-dir', passing an absolute file name arg with a symlink
   in the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (let ((file (pick-new-dir
		 (namestring (merge-pathnames "l/pick-new-~D-~D/" dir)))))
      (prog1 (if (probe-file file) t)
	(delete-dir file)))))

(deftest pick-new-dir (t pick-new-dir-61)
  "Test `pick-new-dir', passing a relative file name arg with a symlink in
   the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (in-directory dir
      (let ((file (pick-new-dir "l/pick-new-dir-test-~D-~D/")))
	(prog1 (if (probe-file file) t)
	  (delete-dir file))))))

(deftest pick-new-dir (t pick-new-dir-62)
  "Test `pick-new-dir', picking many times, passing the same absolute file
   name arg with a symlink in the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (collect ((names))
      (dotimes (num 80)
	(names (pick-new-dir
		(namestring (merge-pathnames "l/pick-test-~D-~D/"
					     dir)))))
      (fi (duplicatesp (names))
	  (and (eq (length (names)) 80)
	       (dolist (file (names) t)
		 (or (probe-file file)
		     (return ()))))))))

(deftest pick-new-dir (t pick-new-dir-63)
  "Test `pick-new-dir', picking many times, passing the same relative
   search list file name arg with a symlink in the path."
  (with-test-dir (dir "b/" ("l" "b/"))
    (in-directory dir
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir "l/pick-test-~D-~D/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ())))))))))


;;;; Relative pathname argument.

(deftest pick-new-dir (t pick-new-dir-71)
  "Test `pick-new-dir', passing a relative file name arg with a relative
   pathname."
  (with-test-dir (dir "a/b/")
    (in-directory dir
      (in-directory "a/b/"
	(let ((file (pick-new-dir "../../pick-new-dir-test-~D-~D/")))
	  (prog1 (if (probe-file file)
		     (string= (namestring
			       (truename (directory-namestring
					  (namify file))))
			      (namestring dir)))
	    (delete-dir file)))))))

(deftest pick-new-dir (t pick-new-dir-72)
  "Test `pick-new-dir', picking many times, passing the same relative
   search list file name arg with a relative pathname."
  (with-test-dir (dir "a/b/")
    (in-directory (merge-pathnames "a/b/" dir)
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir "../pick-test-~D-~D/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ()))
		   (or (string= (namestring
				 (truename (directory-namestring
					    (namify file))))
				(namestring (merge-pathnames "a/" dir)))
		       (return ())))))))))

(deftest pick-new-dir (t pick-new-dir-73)
  "Test `pick-new-dir', passing a relative file name arg with a relative
   pathname."
  (with-test-dir (dir "a/b/")
    (in-directory dir
      (let ((file (pick-new-dir "./pick-new-dir-test-~D-~D/")))
	(prog1 (if (probe-file file)
		   (string= (namestring
			     (truename (directory-namestring
					(namify file))))
			    (namestring dir)))
	  (delete-dir file))))))

(deftest pick-new-dir (t pick-new-dir-74)
  "Test `pick-new-dir', picking many times, passing the same relative
   search list file name arg with a relative pathname."
  (with-test-dir (dir "a/b/")
    (in-directory (merge-pathnames "a/b/" dir)
      (collect ((names))
	(dotimes (num 80)
	  (names (pick-new-dir ".././pick-test-~D-~D/")))
	(fi (duplicatesp (names))
	    (and (eq (length (names)) 80)
		 (dolist (file (names) t)
		   (or (probe-file file)
		       (return ()))
		   (or (string= (namestring
				 (truename (directory-namestring
					    (namify file))))
				(namestring (merge-pathnames "a/" dir)))
		       (return ())))))))))


;;;; Errors.

(deftest pick-new-dir (t pick-new-dir-100)
  "Test `pick-new-dir', passing too few directives."
  (let (ret)
    (handler-case
	(pick-new-dir "~D/")
      (error () (setq ret t)))
    ret))

(deftest pick-new-dir (t pick-new-dir-101)
  "Test `pick-new-dir', passing an empty string."
  (let (ret)
    (handler-case
	(pick-new-dir "")
      (error () (setq ret t)))
    ret))

(deftest pick-new-dir (t pick-new-dir-102)
  "Test `pick-new-dir', passing too many directives."
  (let (ret)
    (handler-case
	(pick-new-dir "~D~D~D/")
      (error () (setq ret t)))
    ret))

(deftest pick-new-dir (t pick-new-dir-103)
  "Test `pick-new-dir', passing the wrong type of directive."
  (let (ret)
    (handler-case
	(pick-new-dir "~A~D/")
      (error () (setq ret t)))
    ret))

(deftest pick-new-dir (t pick-new-dir-104)
  "Test `pick-new-dir', passing a pathname with wildcards."
  (let (ret)
    (handler-case
	(pick-new-dir "~D~D*/")
      (error () (setq ret t)))
    ret))

(deftest pick-new-dir (t pick-new-dir-105)
  "Test `pick-new-dir', passing an empty pathname."
  (let (ret)
    (handler-case
	(pick-new-dir ())
      (error () (setq ret t)))
    ret))

(deftest pick-new-dir (t pick-new-dir-106)
  "Test `pick-new-dir', passing a file name arg."
  (let (ret)
    (deftest:with-test-dir (dir)
      (handler-case
	  (pick-new-dir (merge-pathnames "pick-test-~D-~D/" dir))
	(error () (setq ret t))))
    ret))

(deftest pick-new-dir (t pick-new-dir-107)
  "Test `pick-new-dir' on a missing dir."
  (deftest:with-test-dir (dir)
    (let (ret)
      (handler-case
	  (pick-new-dir (merge-pathnames "pick-new/file-~D-~D/" dir))
	(error () (setq ret t)))
      ret)))

(deftest pick-new-dir (t pick-new-dir-108)
  "Test `pick-new-dir' on a read-blocked dir."
  (deftest:with-test-dir (dir)
    (setf (file-mode dir) "a-rwx")
    (let (ret)
      (unwind-protect
	  (handler-case
	      (pick-new-dir
	       (namestring (merge-pathnames "cde~D~D/" dir)))
	    (error () (setq ret t)))
	(setf (file-mode dir) "a+rwx"))
      ret)))

(deftest pick-new-dir (t pick-new-dir-109)
  "Test `pick-new-dir' on a symlink to a file."
  (deftest:with-test-dir (dir "a" ("l" "a"))
    (let (ret)
      (handler-case
	  (pick-new-dir
	   (namestring (merge-pathnames "l/~D~D/" dir)))
	(error () (setq ret t)))
      ret)))
