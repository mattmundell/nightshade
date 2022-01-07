;;; Tests of `lisp:in-directory'.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))

(defmacro def-in-directory-test (number type files pathname expect)
  "Define a test of `in-directory'.

   $number   unique number for test, appended to name
   $type     :absolute or :relative
   $files    list of file names, created as test directory
                if element is a list then set the mode to the cadr
                if element is a list and car is a list then a symlink
   $pathname  first arg to function under test
   $expect   expected pathname return from `current-directory' inside
             `in-directory' (merged with test dir and `namestring'ed)
   $args     list of args to function."
  (let* ((dir (gensym))
	 (body (ecase type
		 (:absolute
		  ;; FIX if pathname () merge-pathname errs instd of in-dir
		  `(in-directory (merge-pathnames ,pathname ,dir)
		     (current-directory)))
		 (:relative
		  `(in-directory ,dir
		     (in-directory ,pathname
		       (current-directory)))))))
    `(deftest in-directory
	      (,t
	       ,(read-from-string (concatenate 'string
					       "in-directory-"
					       (string (eval number)))))
       ,(format () "Test number ~A of `in-directory'."
		(eval number))
       (with-test-dir (,dir ,@(mapcar (lambda (file)
					(etypecase file
					  (string file)
					  (list (car file))))
				      files))
         (in-directory ,dir
	   ,@(mapcar (lambda (file)
		       (etypecase file
			 (string)
			 (list
			  (when (cadr file)
			    ;; FIX this should :check-for-links t
			    (list 'setf (list 'file-mode
					      (etypecase (car file)
						(string (car file))
						(list (caar file))))
				  (cadr file))))))
		     files))
	 (string= (namestring ,body)
		  (namestring (merge-pathnames ,expect ,dir)))))))

(defmacro def-in-directory-tests (number files pathname expect)
  "Define an absolute and a relative test of `in-directory' using
   `def-in-directory-test'.  The absolute ones are test $number and the
   relative ones are test (1+ $number)."
  `(progn
     (def-in-directory-test ,number :absolute ,files ,pathname ,expect)
     (def-in-directory-test ,(1+ number) :relative ,files
       ,pathname ,expect)))


;;;; Examples.

(deftest in-directory (t in-directory-0)
  "Test `in-directory' with the absolute name of a writable directory."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (string= (in-directory dir (current-directory))
	     (namestring dir))))

(deftest in-directory (t in-directory-1)
  "Test `in-directory' with the relative name of a writable directory."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (in-directory dir
      (string= (in-directory "a/" (current-directory))
	       (namestring (merge-pathnames "a/" dir))))))

(deftest in-directory (t in-directory-2)
  "Test `in-directory' with the absolute name of a file in a writable
   directory."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (string= (in-directory (merge-pathnames "c" dir) (current-directory))
	     (namestring dir))))

(deftest in-directory (t in-directory-3)
  "Test `in-directory' with the relative name of a file in a writable
   directory."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (in-directory dir
      (string= (in-directory "c" (current-directory))
	       (namestring dir)))))


;;;; Directory.

(def-in-directory-tests 11
  ()
  ""
  "")

(def-in-directory-tests 13
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  "")

(def-in-directory-tests 15
  ("a/" "b/D/e/")
  "b/D/e/"
  "b/D/e/")

(def-in-directory-tests 17
  ("a/" "a/file")
  "a/file"
  "a/")

(def-in-directory-tests 19
  ("a b/" "a b/file")
  "a b/"
  "a b/")

(def-in-directory-tests 21
  ("a b/" "a b/file")
  "a b/file"
  "a b/")


;;;; Symlink to directory.

(def-in-directory-tests 31
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  "l/"
  "l/")

(def-in-directory-tests 33
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  "l/e"
  "l/")

(def-in-directory-tests 35
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  "l/e/"
  "l/e/")

(def-in-directory-tests 37
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  "l"
  "")


;;;; Symlink to file.

(def-in-directory-tests 121
  ("a/" "a/c" "a/c.CKP" "a/c~" (("a/l" "a/c~")) (("a/l-to-c" "a/c")))
  "a/l"
  "a/")


;;;; Symlink into subdirectory.

(def-in-directory-tests 141
  ("C/ab/d/" (("l" "C/ab/")) "b/" "C/ab/.a")
  "l/"
  "l/")

(def-in-directory-tests 143
  ((("l" "C/ab/")) "b/" "C/ab/" "C/ab/.a")
  "l/e"
  "l/")

(def-in-directory-tests 145
  ((("l" "C/ab/")) "b/" "C/ab/" "C/ab/.a")
  "l"
  "")


;;;; Hidden file.

(def-in-directory-tests 171
  (".c.lisp")
  ".c.lisp"
  "")

(def-in-directory-tests 173
  ()
  ".c.lisp"
  "")

(def-in-directory-tests 175
  (".c.lisp/")
  ".c.lisp/"
  ".c.lisp/")


;;;; Backup file.

(def-in-directory-tests 201
  ("c.lisp~")
  "c.lisp~"
  "")

(def-in-directory-tests 203
  ("c.lisp.BAK/")
  "c.lisp.BAK/"
  "c.lisp.BAK/")

(def-in-directory-tests 205
  ("c.lisp.BAK/" "c.lisp.BAK/file.ext")
  "c.lisp.BAK/file.ext"
  "c.lisp.BAK/")

(def-in-directory-tests 207
  ("c.lisp.CKP/")
  "c.lisp.CKP/"
  "c.lisp.CKP/")

(def-in-directory-tests 209
  ("c.lisp~/" "c.lisp~/file.ext")
  "c.lisp~/file.ext"
  "c.lisp~/")


;;;; Search list.

(deftest in-directory (t in-directory-230)
  "Test `in-directory' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (string= (namestring (in-directory "a:" (current-directory)))
	       (namestring (truename "a:"))))))

(deftest in-directory (t in-directory-231)
  "Test `in-directory' with a search list bound to a mixed directory, with
   a file name."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (string= (in-directory "a:abc" (current-directory))
	       (namestring (truename "a:"))))))


;;;; Current directory.

(def-in-directory-tests 261
  ("a/" "b/" "c.lisp~" ".a")
  ; This means a file named ".".
  "."
  "")

(def-in-directory-tests 263
  ("a/" "b/" "c.lisp~" ".a")
  "./"
  "./")

(def-in-directory-tests 265
  ("a/z/" "a/b/" "a/c.lisp~" "a/.a")
  "a/z/../"
  "a/z/../")


;;;; Wildcards.

(def-in-directory-tests 290
  ("abc.lisp")
  "*.*"
  "")

(def-in-directory-tests 293
  ("abc.lisp/")
  "*.*"
  "")

(def-in-directory-tests 295
  ("a/b/c/")
  "a/*/c/"
  "a/")

(def-in-directory-tests 297
  ("a/b/c/")
  "a/*/"
  "a/")

(def-in-directory-tests 299
  ("a/b/c/")
  "a/*"
  "a/")

(def-in-directory-tests 301
  ("a/c/" "a/c/d" "a/c.CKP" "a/c~" (("a/l" "a/")) (("a/l-to-c" "a/c")))
  "a/*.*"
  "a/")


;;;; Errors.

(deftest in-directory (t in-directory-340)
  "Test `in-directory' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory () (current-directory))
	(error () (setq ret t))))
    ret))

(deftest in-directory (t in-directory-341)
  "Test `in-directory' with a missing directory."
  (let (ret)
    (with-test-dir (dir)
      (handler-case
	  (in-directory dir
	    (in-directory "b/" (current-directory)))
	(error () (setq ret t))))
    ret))

(deftest in-directory (t in-directory-342)
  "Test `in-directory' on a read-blocked directory."
  (let (ret)
    (with-test-dir (dir "a/b/c/")
      (setf (file-mode (merge-pathnames "a/" dir)) "a-rx")
      (in-directory dir
	(handler-case
	    (in-directory "a/b/"
	      (current-directory))
	  (error () (setq ret t)))
	(setf (file-mode (merge-pathnames "a/" dir)) "a+rx")))
    ret))

(deftest in-directory (t in-directory-345)
  "Test `in-directory', treating a file as a directory."
  (let (ret)
    (with-test-dir (dir "b" "c/")
      (in-directory dir
	(handler-case
	    (in-directory "b/" (current-directory))
	  (error () (setq ret t)))))
    ret))


;;;; Broken symlink.

(def-in-directory-tests 361
  ("a" "b/" (("l" "broken/")) ".a")
  "l"
  "")

(deftest in-directory (t in-directory-363)
  "Test `in-directory' with a broken symlink."
  (let (ret)
    (with-test-dir (dir "b" "c/" ("l" "broken/"))
      (in-directory dir
	(handler-case
	    (in-directory "l/" (current-directory))
	  (error () (setq ret t)))))
    ret))


;;;; Self-referencing symlink.

(def-in-directory-tests 391
  ("a" "b/" (("l" "l")) ".a")
  "l"
  "")

(deftest in-directory (t in-directory-393)
  "Test `in-directory' with a self-referencing symlink."
  (let (ret)
    (with-test-dir (dir "b" "c/" ("l" "broken/"))
      (in-directory dir
	(handler-case
	    (in-directory "l/" (current-directory))
	  (error () (setq ret t)))))
    ret))


;;;; Device.

(deftest in-directory ("/dev/" in-directory-400)
  "Test `in-directory' with an absolute device pathname."
  (in-directory "/dev/null" (current-directory)))
