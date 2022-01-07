;;; Tests of lisp:os-namestring.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))

(defmacro def-os-namestring-test (number return type files
					 pathname expected-pathname
					 args)
  "Define a test of `os-namestring'.

   $number  unique number for test, appended to name
   $return  if true, string compare the return to $expected-pathname
   $type    :absolute or :relative
   $files   list of file names, created as test directory
                if element is a list then set the mode to the cadr
                if element is a list and car is a list then a symlink
   $pathname  first arg to `os-namestring'
   $expected-pathname  expected return from `os-namestring'
   $args    list of values of second and third arg to `os-namestring'"
  (let* ((dir (gensym))
	 (body (ecase type
		 (:relative
		  (if return
		      `(string= (in-directory ,dir
				  (os-namestring ,pathname ,@args))
				(namestring ,expected-pathname))
		      `(in-directory ,dir (os-namestring ,pathname
							 ,@args))))
		 (:absolute
		  (if return
		     `(string= (os-namestring (merge-pathnames ,pathname
							       ,dir)
					      ,@args)
			       (namestring (merge-pathnames
					    ,expected-pathname
					    ,dir)))
		     `(os-namestring (merge-pathnames ,pathname
						      ,dir)
				     ,@args))))))
    `(deftest os-namestring
	      (,return
	       ,(read-from-string (concatenate 'string
					       "os-namestring-"
					       (string (eval number)))))
       ,(format () "Test ~A of `os-namestring'." (eval number))
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
	 ,body))))

(defmacro def-os-namestring-tests (number return files
					  pathname expected-pathname
					  args)
  "Define a absolute and a relative test with `def-os-namestring-test' and
   the given args.  The absolute one is test $number and the relative one
   is test (1+ $number)."
  `(progn
     (def-os-namestring-test ,number ,return :absolute ,files
       ,pathname ,expected-pathname ,args)
     (def-os-namestring-test ,(1+ number) ,return :relative ,files
       ,pathname ,expected-pathname ,args)))

(deftest os-namestring (t os-namestring-0)
  "Test `os-namestring' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/" "c" ".a")
    (string= (os-namestring (merge-pathnames "a/" dir))
	     (namestring (merge-pathnames "a/" dir)))))


;;;; Directory, directory name form.

(def-os-namestring-tests 1 t
  ("a/" "b/" "c" ".a")
  "a/"
  "a/"
  (:for-input ()))

(def-os-namestring-tests 3 ()
  ()
  "a/"
  ()
  (:for-input ()))

(def-os-namestring-tests 5 t
  ("a/" "b/" "c" ".a")
  "a/"
  "a/"
  (() ()))

(def-os-namestring-tests 7 t
  ()
  "a/"
  "a/"
  (() ()))

(def-os-namestring-tests 9 ()
  ("a/" "b/" "c" ".a")
  "a/"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 11 ()
  (("a/" "a+x") "b/" "c" ".a")
  "a/"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 13 ()
  ("a/" "b/" "c" ".a")
  "a/"
  ()
  (() :executable-only))

(def-os-namestring-tests 15 ()
  (("a/" "a+x") "b/" "c" ".a")
  "a/"
  ()
  (() :executable-only))


;;;; Directory, file name form.

(def-os-namestring-tests 21 t
  ("a/" "b/" "c" ".a")
  "a"
  "a/"
  (:for-input ()))

(def-os-namestring-tests 23 ()
  ()
  "a"
  ()
  (:for-input ()))

(def-os-namestring-tests 25 t
  ("a/")
  "a"
  "a/"
  (() ()))

(def-os-namestring-tests 27 t
  ()
  "a"
  "a"
  (() ()))

(def-os-namestring-tests 29 ()
  ("a/" "b/" "c" ".a")
  "a"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 31 ()
  (("a/" "a+x") "b/" "c" ".a")
  "a"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 33 ()
  ("a/")
  "a"
  ()
  (() :executable-only))

(def-os-namestring-tests 35 ()
  (("a/" "a+x") "b/" "c" ".a")
  "a"
  ()
  (() :executable-only))


;;;; File.

(def-os-namestring-tests 41 t
  ("a/" "b/" "c.lisp" ".a")
  "c.lisp"
  "c.lisp"
  (:for-input ()))

(def-os-namestring-tests 43 ()
  ()
  "c.lisp"
  ()
  (:for-input ()))

(def-os-namestring-tests 45 t
  ("a/" "b/" "ccc" ".a")
  "ccc"
  "ccc"
  (() ()))

(def-os-namestring-tests 47 t
  ()
  "ccc"
  "ccc"
  (() ()))

(def-os-namestring-tests 49 ()
  ("a/" "b/" "c" ".a")
  "c"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 51 t
  (("a/" "a+x") "b/" ("c" "a+x") ".a")
  "c"
  "c"
  (:for-input :executable-only))

(def-os-namestring-tests 53 ()
  ("a/" "b/" "c.bin" ".a")
  "c.bin"
  ()
  (() :executable-only))

(def-os-namestring-tests 55 t
  (("a/" "a+x") "b/" ("C" "a+x") ".a")
  "C"
  "C"
  (() :executable-only))

(def-os-namestring-tests 57 t
  ("a/" "b/" "a file.lisp" ".a")
  "a file.lisp"
  "a file.lisp"
  (:for-input ()))


;;;; Symlink to file.

(def-os-namestring-tests 60 t
  ((("l" "C")) "b/" "C" ".a")
  "l"
  "l"
  (:for-input ()))

(def-os-namestring-tests 62 t
  ("a/" "b/" "ccc" (("ccc.link" "ccc")) ".a")
  "ccc.link"
  "ccc.link"
  (() ()))

(def-os-namestring-tests 64 ()
  ("a/" "b/" "c" (("L" "c")) ".a")
  "L"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 66 t
  (("a/" "a+x") "b/" "c" (("l" "c") "a+x") ".a")
  "l"
  "l"
  (:for-input :executable-only))

(def-os-namestring-tests 68 t
  (("a/" "a+x") "b/" ("c" "a+x") (("l" "c") "a+x") ".a")
  "l"
  "l"
  (:for-input :executable-only))

(def-os-namestring-tests 70 t
  (("a/" "a+x") "b/" ("c" "a+x") (("l" "c")) ".a")
  "l"
  "l"
  (:for-input :executable-only))

(def-os-namestring-tests 72 ()
  ("a/" "b/" "c" (("L" "c")) ".a")
  "L"
  ()
  (() :executable-only))

(def-os-namestring-tests 74 t
  (("a/" "a+x") "b/" "c" (("l" "c") "a+x") ".a")
  "l"
  "l"
  (() :executable-only))

(def-os-namestring-tests 76 t
  (("a/" "a+x") "b/" ("c" "a+x") (("l" "c") "a+x") ".a")
  "l"
  "l"
  (() :executable-only))

(def-os-namestring-tests 78 t
  (("a/" "a+x") "b/" ("c" "a+x") (("l" "c")) ".a")
  "l"
  "l"
  (() :executable-only))


;;;; Symlink to directory, either file or directory style name.

(def-os-namestring-tests 80 t
  ((("l" "C/")) "b/" "C/" ".a")
  "l/"
  "l/"
  (:for-input ()))

(def-os-namestring-tests 82 t
  ("a/" "b/" "ccc/" (("ccc.link" "ccc/")) ".a")
  "ccc.link"
  "ccc.link"
  (() ()))

(def-os-namestring-tests 84 ()
  ("a/" "b/" "c/" (("L" "c/")) ".a")
  "L/"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 86 ()
  (("a/" "a+x") "b/" "c/" (("l" "c/") "a+x") ".a")
  "l"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 88 ()
  (("a/" "a+x") "b/" ("c/" "a+x") (("l" "c/") "a+x") ".a")
  "l/"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 90 ()
  (("a/" "a+x") "b/" ("c/" "a+x") (("l" "c/")) ".a")
  "l"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 92 ()
  ("a/" "b/" "c/" (("L" "c/")) ".a")
  "L/"
  ()
  (() :executable-only))

(def-os-namestring-tests 94 ()
  (("a/" "a+x") "b/" "c/" (("l" "c/") "a+x") ".a")
  "l"
  ()
  (() :executable-only))

(def-os-namestring-tests 96 ()
  (("a/" "a+x") "b/" ("c/" "a+x") (("l" "c/") "a+x") ".a")
  "l/"
  ()
  (() :executable-only))

(def-os-namestring-tests 98 ()
  (("a/" "a+x") "b/" ("c/" "a+x") (("l" "c/")) ".a")
  "l"
  ()
  (() :executable-only))


;;;; Symlink into subdirectory.

(def-os-namestring-tests 110 t
  ((("l" "C/ab/")) "b/" "C/ab/" ".a")
  "l/"
  "l/"
  (:for-input ()))

(def-os-namestring-tests 112 t
  ("a/" "b/" "ccc/c/cc/" (("ccc.link" "ccc/c/cc/")) ".a")
  "ccc.link"
  "ccc.link"
  (() ()))

(def-os-namestring-tests 114 ()
  (("a/" "a+x") "b/" "c/d.d/" (("l" "c/d.d/") "a+x") ".a")
  "l"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 116 ()
  (("a/" "a+x") "b/" ("cz/ZC/" "a+x") (("l" "cz/ZC/") "a+x") ".a")
  "l/"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 118 ()
  ("a/" "b/" "c/a/" (("L" "c/a/")) ".a")
  "L/"
  ()
  (() :executable-only))

(def-os-namestring-tests 120 ()
  (("a/" "a+x") "b/" ("c/d/" "a+x") (("l" "c/d/") "a+x") ".a")
  "l/"
  ()
  (() :executable-only))


;;;; Symlink into parent directory.

(def-os-namestring-tests 142 t
  ("a" "b/" (("b/l" "b/") "a+x") ".a")
  "b/l/"
  "b/l/"
  (:for-input ()))

(def-os-namestring-tests 144 t
  ("a" "b/" (("b/zz" "b/") "a+x") ".a")
  "b/zz/"
  "b/zz/"
  (:for-input ()))

(def-os-namestring-tests 146 t
  ("a" "b/" (("b/zz" "b/") "a+x") ".a")
  "b/zz/"
  "b/zz/"
  (() ()))

(def-os-namestring-tests 148 ()
  ("a" "b/" (("b/l" "b/") "a+x") ".a")
  "b/l/"
  ()
  (() :executable-only))

(def-os-namestring-tests 150 t
  ("b/c/" "b/a" (("b/c/l" "b/a") "a+x") ".a")
  "b/c/l"
  "b/c/l"
  (:for-input :executable-only))


;;;; Hidden file.

(def-os-namestring-tests 171 t
  (".c.lisp")
  ".c.lisp"
  ".c.lisp"
  (:for-input ()))

(def-os-namestring-tests 173 ()
  ()
  ".c.lisp"
  ()
  (:for-input ()))

(def-os-namestring-tests 175 t
  ("a/" "b/" "ccc" ".a")
  ".a"
  ".a"
  (() ()))

(def-os-namestring-tests 177 t
  ()
  ".a"
  ".a"
  (() ()))

(def-os-namestring-tests 179 ()
  ("a/" "b/" ".C" ".a")
  ".C"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 181 t
  (("a/" "a+x") "b/" (".c" "a+x") ".a")
  ".c"
  ".c"
  (:for-input :executable-only))

(def-os-namestring-tests 183 ()
  ("a/" "b/" ".c.bin" ".a")
  ".c.bin"
  ()
  (() :executable-only))

(def-os-namestring-tests 185 t
  (("a/" "a+x") "b/" (".C" "a+x") ".a")
  ".C"
  ".C"
  (() :executable-only))


;;;; Backup file.

(def-os-namestring-tests 201 t
  ("a/" "b/" "c.lisp~" ".a")
  "c.lisp~"
  "c.lisp~"
  (:for-input ()))

(def-os-namestring-tests 203 ()
  ()
  "c.lisp~"
  ()
  (:for-input ()))

(def-os-namestring-tests 205 t
  ("a/" "b/" "ccc" ".a.BAK")
  ".a.BAK"
  ".a.BAK"
  (() ()))

(def-os-namestring-tests 207 t
  ()
  ".a.BAK"
  ".a.BAK"
  (() ()))

(def-os-namestring-tests 209 ()
  ("a/" "b/" "C.CKP" ".a")
  "C.CKP"
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 211 t
  (("a/" "a+x") "b/" (".c.CKP" "a+x") ".a")
  ".c.CKP"
  ".c.CKP"
  (:for-input :executable-only))

(def-os-namestring-tests 213 ()
  ("a/" "b/" ".c.bin~" ".a")
  ".c.bin~"
  ()
  (() :executable-only))

(def-os-namestring-tests 215 t
  (("a/" "a+x") "b/" ("C.CKP" "a+x") ".a")
  "C.CKP"
  "C.CKP"
  (() :executable-only))


;;;; Search list.

(deftest os-namestring (t os-namestring-230)
  "Test `os-namestring' with a search list bound to a writable directory of
   files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (string= (os-namestring "a:") dir))))

(deftest os-namestring (() os-namestring-231)
  "Test `os-namestring' with a search list bound to a writable directory of
   files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (os-namestring "a:" :for-input :executable-only))))

(deftest os-namestring (t os-namestring-232)
  "Test `os-namestring' with a search list bound to an empty directory."
  (with-test-dir (dir)
    (with-test-search-list ("a" dir)
      (string= (os-namestring "a:") dir))))

(deftest os-namestring (t os-namestring-233)
  "Test `os-namestring' with a search list bound to a writable directory of
   files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (setf (file-mode "a:abc.b") "a+x")
      (string= (os-namestring "a:abc.b" :for-input :executable-only)
	       (namestring (merge-pathnames "abc.b" dir))))))

(deftest os-namestring (() os-namestring-234)
  "Test `os-namestring' with a search list bound to a writable directory of
   files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (os-namestring "a:abc.b" :for-input :executable-only))))


;;;; Current directory.

(def-os-namestring-tests 261 t
  ("a/" "b/" "c.lisp~" ".a")
  "."
  "./"
  (:for-input ()))

(def-os-namestring-tests 263 t
  ()
  "."
  "./"
  (:for-input ()))

(def-os-namestring-tests 265 t
  ("a/" "b/" "ccc" ".a.BAK")
  "."
  "./"
  (() ()))

(def-os-namestring-tests 267 t
  ()
  "."
  "./"
  (() ()))

(def-os-namestring-tests 269 ()
  ("a/" "b/" "C.CKP" ".a")
  "."
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 271 ()
  (("a/" "a+x") "b/" (".c.CKP" "a+x") ".a")
  "."
  ()
  (:for-input :executable-only))

(def-os-namestring-tests 273 ()
  ("a/" "b/" ".c.bin~" ".a")
  "."
  ()
  (() :executable-only))

(def-os-namestring-tests 275 ()
  (("a/" "a+x") "b/" ("C.CKP" "a+x") ".a")
  "."
  ()
  (() :executable-only))

(def-os-namestring-tests 277 t
  ()
  ""
  ""
  (:for-input ()))


;;;; Wildcards.

(def-os-namestring-tests 290 t
  ("abc.lisp")
  "*.*"
  "abc.lisp"
  (:for-input ()))

(def-os-namestring-tests 293 ()
  ()
  "*.*"
  ()
  (:for-input ()))

(def-os-namestring-tests 295 t
  ("abc.lisp")
  "*.*"
  "abc.lisp"
  (() ()))

(def-os-namestring-tests 297 ()
  ()
  "*.*"
  ()
  (() ()))

(def-os-namestring-tests 299 t
  (("abc.lisp" "a+x"))
  "*.*"
  "abc.lisp"
  (() :executable-only))

(def-os-namestring-tests 301 ()
  ("abc.lisp")
  "*.*"
  "abc.lisp"
  (() :executable-only))

(def-os-namestring-tests 303 t
  (("abc.lisp" "a+x"))
  "*.*"
  "abc.lisp"
  (:for-input :executable-only))

(def-os-namestring-tests 305 t
  (("abc.lisp" "a+x"))
  "*.*"
  "abc.lisp"
  (:for-input :executable-only))


;;;; Errors.

(deftest os-namestring (t os-namestring-340)
  "Test `os-namestring' with ambiguous wildcards."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (os-namestring "*.*"))
	(error () (setq ret t))))
    ret))

(deftest os-namestring (() os-namestring-341)
  "Test `os-namestring' with a missing directory."
  (with-test-dir (dir)
    (in-directory dir (os-namestring "aa/"))))

(deftest os-namestring (() os-namestring-342)
  "Test `os-namestring' with a missing file."
  (with-test-dir (dir "b" "c/")
    (in-directory dir (os-namestring "a"))))


;;;; Broken symlink.

(def-os-namestring-tests 360 t
  ("a" "b/" (("l" "broken/")) ".a")
  "l"
  "l"
  (() ()))

(def-os-namestring-tests 364 ()
  ("a" "b/" (("b/zz" "broken")) ".a")
  "b/zz/"
  ()
  (:for-input ()))

(def-os-namestring-tests 368 ()
  ("a" "b/" (("b/l" "b/") "a+x") ".a")
  "b/l/"
  ()
  (() :executable-only))

(def-os-namestring-tests 370 ()
  ("b/c/" (("b/c/l" "b/a")) ".a")
  "b/c/l"
  ()
  (:for-input :executable-only))


;;;; Self-referencing symlink.

(def-os-namestring-tests 380 t
  ("a" "b/" (("l" "l")) ".a")
  "l"
  "l"
  (() ()))

(def-os-namestring-tests 382 t
  ("a" "b/" (("zz" "zz")) ".a")
  "zz"
  "zz"
  (:for-input ()))

(def-os-namestring-tests 384 ()
  ("a" "b/" (("zz" "zz")) ".a")
  "zz"
  ()
  (() :executable-only))

(def-os-namestring-tests 386 ()
  ("b/c/" (("link" "link")) ".a")
  "b/c/l"
  ()
  (:for-input :executable-only))


;;;; Device.

(deftest os-namestring ("/dev/null" os-namestring-400)
  "Test `os-namestring' with an absolute device pathname."
  (os-namestring "/dev/null"))

(deftest os-namestring ("null" os-namestring-401)
  "Test `os-namestring' with a relative device pathname."
  (in-directory "/dev/"
    (os-namestring "null")))
