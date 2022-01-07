;;; Tests of lisp:print-files.
;;;
;;; Similar to some of print-directory.lisp.
;;;
;;; These tests depend on the formatting of print-files staying constant,
;;; in particular, FIX on (line-length *standard-output*) being 80.

(in-package "LISP")

(import '(deftest:deftest deftest:check-verbose-list))

;; FIX file extensions
;; FIX remote directories
;; FIX :verbose with :coldefs


;;;; Files and directories.

(deftest print-files (t print-files-1)
  "Test `print-files' with an empty absolute dir."
  (let ((dir (deftest:make-test-dir)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-files dir '() stream))
		 (format () "Directory of ~A :~%" dir))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-2)
  "Test `print-files' with a single file, passing an empty dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-files "" '("a") stream))
		   (format () "Directory of  :~%a~%")))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-3)
  "Test `print-files' with a single directory, passing an absolute dir."
  (let ((dir (deftest:make-test-dir "a/")))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-files dir
				  (list (pathname "a/"))
				  stream))
		   (format () "Directory of ~A :~%a/~%" dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-4)
  "Test `print-files' with files and directories, passing an absolute dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-files dir
				(sort spec #'string>)
				stream))
		 (format () "Directory of ~A :~%dir2/ dir1/ c     b     a~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-5)
  "Test `print-files' with a single file, passing a relative dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (let ((files (list (merge-pathnames
					 "a"
					 (ensure-trailing-slash
					  (file-namestring
					   (namify dir)))))))
		       (print-files (directory-namestring (namify dir))
				    files
				    stream)))
		   (format () "Directory of ~A :~%~A/a~%"
			   (directory-namestring (namify dir))
			   (file-namestring (namify dir)))))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-6)
  "Test `print-files' with a single directory, passing a relative dir."
  (let ((dir (namify (deftest:make-test-dir "a/"))))
    (unwind-protect
	(in-directory dir
	  (let ((rel-dir (directorify (file-namestring dir))))
	    (string= (with-output-to-string (stream)
		       (print-files rel-dir
				    '("a/")
				    stream))
		     (format () "Directory of ~A :~%a/~%"
			     rel-dir
			     dir))))
      (dired:delete-file (directorify dir) :recurse t))))

(deftest print-files (t print-files-7)
  "Test `print-files' with files and directories, passing a relative dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
      (unwind-protect
	  (in-directory dir
	    (in-directory "dir2/"
	      (string= (with-output-to-string (stream)
			 (print-files "../"
				      (sort spec #'string>)
				      stream))
		       (format () "Directory of ../ :~%dir2/ dir1/ c     b     a~%"))))
	(dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-8)
  "Test `print-files' with files and directories, passing an absolute dir
   and absolute files in the list."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((files (sort spec #'string>)))
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files dir files stream :verbose t))
	   files
	   dir))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-9)
  "Test `print-files' with files and directories, passing an absolute dir
   and a mix of absolute and relative files in the list."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec))
	 (count 0))
    (unwind-protect
	(let ((files (mapcar (lambda (ele)
			       (incf count)
			       (if (oddp count)
				   (merge-pathnames ele dir)
				   ele))
			     (sort spec #'string>))))
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files dir files stream :verbose t))
	   files
	   dir))
      (dired:delete-file dir :recurse t))))


;;;; Files and directories, verbosely.

(deftest print-files (t print-files-11)
  "Test verbose `print-files' with an empty absolute dir."
  (let ((dir (deftest:make-test-dir)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-files dir
				()
				stream
				:verbose t))
		 (format () "v Directory of ~A :~%" dir))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-12)
  "Test verbose `print-files' with a single file, passing an absolute dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (let ((files (list (namestring
			      (merge-pathnames "a" dir)))))
	    (check-verbose-list
	     (with-output-to-string (stream)
	       (print-files dir files stream :verbose t))
	     files
	     dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-13)
  "Test verbose `print-files' with a single directory, passing an absolute
   dir."
  (let ((dir (deftest:make-test-dir "a/")))
    (unwind-protect
	(in-directory dir
	  (let ((files (list (merge-pathnames "a/" dir)))
		(expect (list (namestring (merge-pathnames "a/" dir)))))
	    (check-verbose-list
	     (with-output-to-string (stream)
	       (print-files dir files stream :verbose t))
	     expect
	     dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-14)
  "Test verbose `print-files' with files and directories, passing an
   absolute dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((files (sort spec #'string>)))
	    (check-verbose-list
	     (with-output-to-string (stream)
	       (print-files dir files stream :verbose t))
	     files
	     dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-15)
  "Test verbose `print-files' with a single file, passing a relative dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files "./" '("a") stream :verbose t))
	   '("a")
	   "./"))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-16)
  "Test verbose `print-files' with a single directory, passing a relative
   dir."
  (let ((dir (namify (deftest:make-test-dir "a/"))))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files (directorify (file-namestring dir))
			  '("a/")
			  stream
			  :verbose t))
	   '("a/")
	   (directorify (file-namestring dir))))
      (dired:delete-file (directorify dir) :recurse t))))

(deftest print-files (t print-files-17)
  "Test verbose `print-files' with files and directories, passing a
   relative dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (in-directory "dir2/"
	    (let ((files (sort spec #'string>)))
	      (check-verbose-list
	       (with-output-to-string (stream)
		 (print-files "../" files stream :verbose t))
	       files
	       "../"))))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-18)
  "Test verbose `print-files' with files and directories, passing an
   absolute dir and absolute files in the list."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((files (mapcar (lambda (ele)
			       (merge-pathnames ele dir))
			     (sort spec #'string>))))
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files dir files stream :verbose t))
	   files
	   dir))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-19)
  "Test verbose `print-files' with files and directories, passing an
   absolute dir and a mix of absolute and relative files in the list."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (count 0)
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((files (mapcar (lambda (ele)
			       (incf count)
			       (if (evenp count)
				   (merge-pathnames ele dir)
				   ele))
			     (sort spec #'string>))))
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files dir files stream :verbose t))
	   files
	   dir))
      (dired:delete-file dir :recurse t))))


;;;; Symlinks.

(deftest print-files (t print-files-30)
  "Test `print-files' with files, directories and symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (files '("a" "b" "c" "l" "dir1/" "dir1/end"
		  "dir2/" "dl/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-files "./" files stream))
		   (format () "Directory of ./ :
a        b        c        l        dir1/    dir1/end dir2/    dl/~%")))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-31)
  "Test verbose `print-files' with files, directories and symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (files '("a" "dir1/" "dir1/end" "dir2/" "dl/"
		 "c" "l"))
	 (expect '("a" "dir1/" "dir1/end" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-files "./" files stream :verbose t))
	   expect
	   "./"))
      (dired:delete-file dir :recurse t))))


;;;; Key argument :return-list.

(deftest print-files (t print-files-40)
  "Test `print-files' with files, directories and symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (files '("a" "b" "c" "l" "dir1/" "dir1/end"
		  "dir2/" "dl/"))
	 (dir (apply #'deftest:make-test-dir spec))
	 return)
    (unwind-protect
	(in-directory dir
	  (and (string= (with-output-to-string (stream)
			  (setq return
				(print-files "./" files stream
					     :return-list t)))
			(format () "Directory of ./ :
a        b        c        l        dir1/    dir1/end dir2/    dl/~%"))
	       (equal return files)))
      (dired:delete-file dir :recurse t))))

(deftest print-files (t print-files-41)
  "Test verbose `print-files' with files, directories and symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (files '("a" "dir1/" "dir1/end" "dir2/" "dl/"
		  "c" "l"))
	 (expect '("a" "dir1/" "dir1/end" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec))
	 return)
    (unwind-protect
	(in-directory dir
	  (and (check-verbose-list
		(with-output-to-string (stream)
		  (setq return
			(print-files "./" files stream :verbose t
				     :return-list t)))
		expect
		"./")
	       (equal return files)))
      (dired:delete-file dir :recurse t))))


;;;; Implicit stream argument.

(deftest print-files (t print-files-50)
  "Test `print-files' with a relative pathname, on a directory with files
   and subdirectories, including hidden files and directories, with the
   stream argument implicit."
  (let* ((spec '("b" "ba.b" ".b" ".b.b" "bdir.b/" "bdir.b/suba"
		 "dir2.BAK/" ".bdir3/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (let* ((*standard-output* (out-synonym-of stream)))
		       (print-files "" spec)))
		   "Directory of  :
b           .b          bdir.b/     dir2.BAK/   .dir3/
ba.b        .b.b        bdir.b/suba .bdir3/
"))
      (dired:delete-file dir :recurse t))))


;;; Errors.

(deftest print-files (t print-files-61)
  "Test `print-files' on an absolute missing directory."
  (let ((dir (pick-new-dir)) ret)
    (delete-dir dir)
    (handler-case
	(with-output-to-string (stream)
	  (print-files dir '() stream))
      (error () (setq ret t)))
    ret))

(deftest print-files (t print-files-62)
  "Test `print-files' on an relative missing directory."
  (let ((dir (pick-new-dir)) ret)
    (unwind-protect
	(handler-case
	    (in-directory dir
	      (with-output-to-string (stream)
		(print-files "xxx/" '() stream)))
	  (error () (setq ret t)))
      (delete-dir dir))
    ret))

(deftest print-files (t print-files-63)
  "Test verbose `print-files' on an absolute missing directory."
  (let ((dir (pick-new-dir)) ret)
    (delete-dir dir)
    (handler-case
	(with-output-to-string (stream)
	  (print-files dir '() stream :verbose t))
      (error () (setq ret t)))
    ret))

(deftest print-files (t print-files-64)
  "Test verbose `print-files' on an relative missing directory."
  (let ((dir (pick-new-dir)) ret)
    (unwind-protect
	(handler-case
	    (in-directory dir
	      (with-output-to-string (stream)
		(print-files "xxx/" '() stream :verbose t)))
	  (error () (setq ret t)))
      (delete-dir dir))
    ret))
