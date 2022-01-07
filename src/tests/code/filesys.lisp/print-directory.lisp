;;; Tests of lisp:print-directory.
;;;
;;; Some of this similar to print-files.lisp.
;;;
;;; These tests depend on the formatting of print-directory staying
;;; constant, in particular, on (line-length *standard-output*) being 80.

(in-package "LISP")

(import '(deftest:deftest deftest:check-verbose-list))

;; FIX file extensions
;; FIX remote directories
;; FIX :verbose with :coldefs


;;;; Files and directories.

(deftest print-directory (t print-directory-1)
  "Test `print-directory' with an empty absolute dir."
  (let ((dir (deftest:make-test-dir)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream))
		 (format () "Directory of ~A :~%" dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-2)
  "Test `print-directory' with a single file, implicitly passing an
   absolute dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory () stream))
		   (format () "Directory of ~A :~%a~%" dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-3)
  "Test `print-directory' with a single directory, passing an absolute
   dir."
  (let ((dir (deftest:make-test-dir "a/")))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory () stream))
		   (format () "Directory of ~A :~%a/~%" dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-4)
  "Test `print-directory' with files and directories, passing an absolute
   dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream))
		 (format () "Directory of ~A :~%a     b     c     dir1/ dir2/~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-5)
  "Test `print-directory' with a single file, passing a relative dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "./" stream))
		   (format () "Directory of ./ :~%a~%" dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-6)
  "Test `print-directory' with a single directory, passing a relative dir."
  (let ((dir (namify (deftest:make-test-dir "a/"))))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory (directorify (file-namestring dir))
				      stream))
		   (format () "Directory of ~A :~%a/~%"
			   (directorify (file-namestring dir))
			   dir)))
      (dired:delete-file (directorify dir) :recurse t))))

(deftest print-directory (t print-directory-7)
  "Test `print-directory' with files and directories, passing a relative
   dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (in-directory "dir2/"
	    (string= (with-output-to-string (stream)
		       (print-directory "../" stream))
		     (format () "Directory of ../ :~%a     b     c     dir1/ dir2/~%"))))
      (dired:delete-file dir :recurse t))))


;;;; Files and directories, verbosely.

(deftest print-directory (t print-directory-11)
  "Test verbose `print-directory' with an empty absolute dir."
  (let ((dir (deftest:make-test-dir)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream :verbose t))
		 (format () "v Directory of ~A :~%" dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-12)
  "Test verbose `print-directory' with a single file, implicitly passing an
   absolute dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list (with-output-to-string (stream)
				(print-directory () stream :verbose t))
			      '("a")
			      dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-13)
  "Test verbose `print-directory' with a single directory, passing an
   absolute dir."
  (let ((dir (deftest:make-test-dir "a/")))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list (with-output-to-string (stream)
				(print-directory () stream :verbose t))
			      '("a/")
			      dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-14)
  "Test verbose `print-directory' with files and directories, passing an
   absolute dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (expect '("a" "b" "c" "dir1/" "dir2/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(check-verbose-list (with-output-to-string (stream)
			      (print-directory dir stream
					       :verbose t))
			    expect
			    dir)
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-15)
  "Test verbose `print-directory' with a single file, passing a relative
   dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list (with-output-to-string (stream)
				(print-directory "./" stream
						 :verbose t))
			      '("a")
			      "./"))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-16)
  "Test verbose `print-directory' with a single directory, passing a
   relative dir."
  (let ((dir (namify (deftest:make-test-dir "a/"))))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory (directorify (file-namestring dir))
			      stream
			      :verbose t))
	   '("a/")
	   (directorify (file-namestring dir))))
      (dired:delete-file (directorify dir) :recurse t))))

(deftest print-directory (t print-directory-17)
  "Test verbose `print-directory' with files and directories, passing a
   relative dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (in-directory "dir2/"
	    (check-verbose-list (with-output-to-string (stream)
				  (print-directory "../" stream
						   :verbose t))
				(sort spec #'string<)
				"../")))
      (dired:delete-file dir :recurse t))))


;;;; Symlinks, limited to a single directory (vs recursing).

(deftest print-directory (t print-directory-30)
  "Test `print-directory' with files, directories and symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "./" stream))
		   (format () "Directory of ./ :
a     b     c     dir1/ dir2/ dl/   l~%")))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-31)
  "Test verbose `print-directory' with files, directories and symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "b" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory "./" stream :verbose t))
	   (sort expect
		 (lambda (a b)
		   (string< (if (listp a) (car a) a)
			    (if (listp b) (car b) b))))
	   "./"))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "recurse".

(deftest print-directory (t print-directory-40)
  "Test `print-directory' with files and recursive subdirectories."
  (let* ((spec '("a" "dir1/" "dir1/sub1/" "dir1/sub/" "dir1/sub/subsub/"
		 "dir1/sub/subsub/b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream))
		 (format () "Directory of ~A :
a     c     dir1/ dir2/~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-41)
  "Test verbose `print-directory' with files and recursive subdirectories."
  (let* ((spec '("a" "dir1/" "dir1/sub1/" "dir1/sub/"
		 "dir1/sub/subsub/" "dir1/sub/subsub/b" "dir2/" "c"))
	 (expect '("a" "dir1/" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(check-verbose-list
	 (with-output-to-string (stream)
	   (print-directory dir stream :verbose t))
	 (sort expect
	       (lambda (a b)
		 (string< (if (listp a) (car a) a)
			  (if (listp b) (car b) b))))
	 dir)
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-42)
  "Test recursive `print-directory' with files and recursive subdirectories."
  (let* ((spec '("a" "dir1/" "dir1/sub1/" "dir1/sub/" "dir1/sub/subsub/"
		 "dir1/sub/subsub/b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream :recurse t))
		 (format () "Directory of ~A :
a                 dir1/             dir1/sub/subsub/  dir1/sub1/
c                 dir1/sub/         dir1/sub/subsub/b dir2/~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-43)
  "Test verbose recursive `print-directory' with files and recursive subdirectories."
  (let* ((spec '("a" "dir1/" "dir1/sub1/" "dir1/sub/" "dir1/sub/subsub/"
		 "dir1/sub/subsub/b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(check-verbose-list
	 (with-output-to-string (stream)
	   (print-directory dir stream :verbose t :recurse t))
	 (sort spec
	       (lambda (a b)
		 (string< (if (listp a) (car a) a)
			  (if (listp b) (car b) b))))
	 dir)
      (dired:delete-file dir :recurse t))))


;;;; Symlinks, recursing into subdirs.

(deftest print-directory (t print-directory-50)
  "Test recursive `print-directory' with files, directories and symlinks,
   ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "./" stream :recurse t))
		   (format () "Directory of ./ :
a        c        dir1/end dl/      l
b        dir1/    dir2/    dl/end~%")))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-51)
  "Test verbose recursive `print-directory' with files, directories and
   symlinks, ."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		   "dl/end" "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory "./" stream :verbose t :recurse t))
	   (sort expect
		 (lambda (a b)
		   (string< (if (listp a) (car a) a)
			    (if (listp b) (car b) b))))
	   "./"))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "backups".

(deftest print-directory (t print-directory-60)
  "Test `print-directory' with files and subdirectories, including backups."
  (let* ((spec '("z.bak" "dir/" "a~" "dir0~/" "b.BAK" "dir2.BAK/"
		 "c.CKP" "dir2.CKP/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "./" stream :recurse t))
		   "Directory of ./ :
a~        b.BAK     c.CKP     dir/      dir0~/    dir2.BAK/ dir2.CKP/ z.bak
"))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-61)
  "Test verbose `print-directory' with files and subdirectories, including
   backups."
  (let* ((spec '("z.bak" "dir/" "a~" "dir0~/" "b.BAK" "dir2.BAK/"
		 "c.CKP" "dir2.CKP/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory "./" stream :verbose t :recurse t))
	   (sort spec
		 (lambda (a b)
		   (string< (if (listp a) (car a) a)
			    (if (listp b) (car b) b))))
	   "./"))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-62)
  "Test `print-directory' with :backups () on files and subdirectories, both of
   which include backups."
  (let* ((spec '("aa.bak" "dir/" "b" "a~" "dir0~/" "b.BAK" "dir2.BAK/"
		 "c.CKP" "dir2.CKP/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "./" stream :recurse t :backups ()))
		   "Directory of ./ :
aa.bak b      dir/
"))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "all".

(deftest print-directory (t print-directory-70)
  "Test `print-directory' on a directory with files and subdirectories,
   including hidden files and directories."
  (let* ((spec '("a" ".a" "dir/" ".dir0/" ".b.BAK" ".dir2.BAK/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream :recurse t))
		 (format () "Directory of ~A :
a    dir/~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-71)
  "Test verbose `print-directory' on a directory with files and
   subdirectories, including hidden files and directories."
  (let* ((spec '("a" ".a" "dir/" ".dir0/" ".b.BAK" ".dir2.BAK/"))
	 (expect '("a" "dir/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(check-verbose-list
	 (with-output-to-string (stream)
	   (print-directory dir stream :verbose t :recurse t))
	 (sort expect
	       (lambda (a b)
		 (string< (if (listp a) (car a) a)
			  (if (listp b) (car b) b))))
	 dir)
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-73)
  "Test `print-directory' with :all t on a directory with files and
   subdirectories, including hidden files and directories."
  (let* ((spec '("a" ".a" "dir/" ".dir0/" ".b.BAK" ".dir2.BAK/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream :all t :recurse t))
		 (format () "Directory of ~A :
.a         .b.BAK     .dir0/     .dir2.BAK/ a          dir/~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-74)
  "Test verbose `print-directory' with :all t on a directory with files
   and subdirectories, including hidden files and directories."
  (let* ((spec '("a" ".a" "dir/" ".dir0/" ".b.BAK" ".dir2.BAK/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(check-verbose-list
	 (with-output-to-string (stream)
	   (print-directory dir stream :all t :verbose t :recurse t))
	 (sort spec
	       (lambda (a b)
		 (string< (if (listp a) (car a) a)
			  (if (listp b) (car b) b))))
	 dir)
      (dired:delete-file dir :recurse t))))


;;;; Key argument "check-for-subdirs".

(deftest print-directory (t print-directory-80)
  "Test `print-directory' on a directory with files and subdirectories,
   including hidden files and directories, with :check-for-subdirs ()."
  (let* ((spec '("a" ".b" "dir/" "dir2.BAK/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(string= (with-output-to-string (stream)
		   (print-directory dir stream :recurse t
				    :check-for-subdirs ()))
		 (format () "Directory of ~A :
a        dir      dir2.BAK~%"
			 dir))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-81)
  "Test `print-directory' with :check-for-subdirs () on a directory with
   files and subdirectories, including hidden files and directories."
  (let* ((spec '("a" ".b" "dir/" "dir2.BAK/" ".dir3/"))
	 (expect '("a" "dir" "dir2.BAK"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(check-verbose-list
	 (with-output-to-string (stream)
	   (print-directory dir stream :check-for-subdirs () :verbose t))
	 (sort expect
	       (lambda (a b)
		 (string< (if (listp a) (car a) a)
			  (if (listp b) (car b) b))))
	 dir)
      (dired:delete-file dir :recurse t))))


;;;; Wildcards.

(deftest print-directory (t print-directory-90)
  "Test `print-directory' with a relative pathname with name and type
   wildcards, on a directory with files and subdirectories, including
   hidden files and directories."
  (let* ((spec '("a" ".b" "dir/" "dir/suba" "dir2.BAK/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "*.*" stream :all t :recurse t))
		   (format () "Directory of *.* :
.b        .dir3/    a         dir/      dir/suba  dir2.BAK/~%"
			   dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-91)
  "Test `print-directory' with a relative pathname with name and type
   wildcards, on a directory with files and subdirectories, including
   hidden files and directories."
  (let* ((spec '("a" ".b" "dir/" "dir/suba" "dir2.BAK/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory "*.*" stream
			      :all t :recurse t :verbose t))
	   (sort spec
		 (lambda (a b)
		   (string< (if (listp a) (car a) a)
			    (if (listp b) (car b) b))))
	   "*.*"))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-92)
  "Test `print-directory' with a relative pathname with a name limiting
   wildcard, on a directory with files and subdirectories, including hidden
   files and directories."
  (let* ((spec '("ba" ".b" "bdir/" "bdir/suba" "dir2.BAK/" ".bdir3/"
		 ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "b*.*" stream :all t :recurse t))
		   (format () "Directory of b*.* :
ba        bdir/     bdir/suba~%"
			   dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-93)
  "Test `print-directory' with a relative pathname with a name limiting
   wildcard, on a directory with files and subdirectories, including hidden
   files and directories."
  (let* ((spec '("ba" ".b" "bdir/" "bdir/suba" "dir2.BAK/" ".bdir3/"
		 ".dir3/"))
	 (expect '("ba" "bdir/" "bdir/suba"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory "b*.*" stream
			      :all t :recurse t :verbose t))
	   (sort expect
		 (lambda (a b)
		   (string< (if (listp a) (car a) a)
			    (if (listp b) (car b) b))))
	   "b*.*"))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-94)
  "Test `print-directory' with a relative pathname with a type limiting
   wildcard, on a directory with files and subdirectories, including hidden
   files and directories."
  (let* ((spec '("b" "ba.b" ".b" ".b.b" "bdir.b/" "bdir.b/suba"
		 "dir2.BAK/" ".bdir3/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (print-directory "*.b" stream :all t :recurse t))
		   (format () "Directory of *.b :
.b.b        ba.b        bdir.b/     bdir.b/suba~%"
			   dir)))
      (dired:delete-file dir :recurse t))))

(deftest print-directory (t print-directory-95)
  "Test `print-directory' with a relative pathname with a type limiting
   wildcard, on a directory with files and subdirectories, including hidden
   files and directories."
  (let* ((spec '("b" "ba.b" ".b" ".b.b" "bdir.b/" "bdir.b/suba"
		 "dir2.BAK/" ".bdir3/" ".dir3/"))
	 (expect '(".b.b" "ba.b" "bdir.b/" "bdir.b/suba"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (check-verbose-list
	   (with-output-to-string (stream)
	     (print-directory "*.b" stream
			      :all t :recurse t :verbose t))
	   (sort expect
		 (lambda (a b)
		   (string< (if (listp a) (car a) a)
			    (if (listp b) (car b) b))))
	   "*.b"))
      (dired:delete-file dir :recurse t))))


;;;; Implicit stream argument.

(deftest print-directory (t print-directory-96)
  "Test `print-directory' with a relative pathname with a type limiting
   wildcard, on a directory with files and subdirectories, including hidden
   files and directories, with the stream argument implicit."
  (let* ((spec '("b" "ba.b" ".b" ".b.b" "bdir.b/" "bdir.b/suba"
		 "dir2.BAK/" ".bdir3/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (string= (with-output-to-string (stream)
		     (let* ((*standard-output* (out-synonym-of stream)))
		       (print-directory "b*.b")))
		   "Directory of b*.b :
ba.b    bdir.b/
"))
      (dired:delete-file dir :recurse t))))
