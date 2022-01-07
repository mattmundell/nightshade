;;; Tests of file iteration functions in package lisp.
;;;
;;; These include lisp:map-files, lisp:do-files, lisp:list-files,
;;; lisp:map-dirs, lisp:do-dirs and list:list-dirs.
;;;
;;; FIX Test list-* with more restrictive predicates.

;;; FIX /.a/b/c when all ()  similar for hidden...

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))

(defmacro def-map-files-test (number type files
					 pathname expect-files
					 &optional args dirp)
  "Define a test of `map-files' or `map-dirs'.

   $number   unique number for test, appended to name
   $type     :absolute or :relative
   $files    list of file names, created as test directory
                if element is a list then set the mode to the cadr
                if element is a list and car is a list then a symlink
   $pathname  first arg to function under test
   $expect-files  expect function to produce these
   $args     list of args to function
   $dirp     if true test `map-files', else test `map-dirs'."
  (let* ((dir (gensym))
	 (body (ecase type
		 (:absolute
		  `(,(if dirp 'map-dirs 'map-files)
		    (if ,pathname (merge-pathnames ,pathname ,dir))
		    (lambda (file)
		      (files (namestring file)))
		    ,@args))
		 (:relative
		  `(in-directory ,dir
		     (,(if dirp 'map-dirs 'map-files)
		      ,pathname
		      (lambda (file)
			(files (namestring file)))
		      ,@args))))))
    `(deftest ,(if dirp 'map-dirs 'map-files)
	      (,t
	       ,(read-from-string (concatenate 'string
					      (if dirp
						  "map-dirs-"
						  "map-files-")
					      (string (eval number)))))
       ,(format () "Test number ~A of `~:[map-files~;map-dirs~]'."
		(eval number)
		dirp)
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
	 (collect ((files))
	   ,body
	   (equal (sort (files) #'string<)
		  (mapcar (lambda (file)
			    (namestring
			     ,(if (eq type :relative)
				  'file
				  (list 'merge-pathnames 'file dir))))
			  ,expect-files)))))))

(defmacro def-do-files-test (number type files
					 pathname expect-files
					 &optional args dirp)
  "Define a test of `do-files' or `do-dirs'.

   $number    unique number for test, appended to name
   $type      :absolute or :relative
   $files     list of file names, created as test directory
                if element is a list then set the mode to the cadr
                if element is a list and car is a list then a symlink
   $pathname  first arg to function under test
   $expect-files  expect function to produce these
   $args      list of args to function
   $dirp      if true test `do-files', else test `do-dirs'."
  (let* ((dir (gensym))
	 (body (ecase type
		 (:absolute
		  `(,(if dirp 'do-dirs 'do-files)
		    (file (if ,pathname
			      (merge-pathnames ,pathname
					       ,dir))
			  ,@args)
		    (files (namestring file))))
		 (:relative
		  `(in-directory ,dir
		     (,(if dirp 'do-dirs 'do-files)
		      (file ,pathname ,@args)
		      (files (namestring file))))))))
    `(deftest ,(if dirp 'do-dirs 'do-files)
	      (,t
	       ,(read-from-string (concatenate 'string
					       (if dirp
						   "do-dirs-"
						   "do-files-")
					       (string (eval number)))))
       ,(format () "Test number ~A of `~:[do-files~;do-dirs~]'."
		(eval number) dirp)
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
	 (collect ((files))
	   ,body
	   (equal (sort (files) #'string<)
		  (mapcar (lambda (file)
			    (namestring
			     ,(if (eq type :relative)
				  'file
				  (list 'merge-pathnames 'file dir))))
			  ,expect-files)))))))

(defmacro def-list-files-test (number type files
					 pathname expect-files
					 &optional args dirp)
  "Define a test of `list-files' or `list-dirs'.

   $dirp    generate `list-dirs' if true, else `list-files'
   $number  unique number for test, appended to name
   $type    :absolute or :relative
   $files   list of file names, created as test directory
                if element is a list then set the mode to the cadr
                if element is a list and car is a list then a symlink
   $pathname  first arg to function under test
   $expect-files  expect function to produce these
   $args    list of args to function."
  (let* ((dir (gensym))
	 (body (ecase type
		 (:absolute
		  `(,(if dirp 'list-dirs 'list-files)
		    (if ,pathname (merge-pathnames ,pathname ,dir))
		    #'t
		    ,@args))
		 (:relative
		  `(in-directory ,dir
		     (,(if dirp 'list-dirs 'list-files)
		      ,pathname #'t ,@args))))))
    `(deftest ,(if dirp 'list-dirs 'list-files)
	      (,t
	       ,(read-from-string (concatenate 'string
					       (if dirp
						   "list-dirs-"
						   "list-files-")
					       (string (eval number)))))
       ,(format () "Test number ~A of `~:[list-files~;list-dirs~]."
		(eval number) dirp)
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
	 (equal (sort ,body #'string<)
		(mapcar (lambda (file)
			  (namestring
			   ,(if (eq type :relative)
				'file
				(list 'merge-pathnames 'file dir))))
			,expect-files))))))

(defmacro def-file-iterator-tests (number files pathname
					  expect-files expect-dirs
					  &optional args)
  "Define an absolute and a relative test for each of `map-files' and
   `do-files', using `def-map-files-test', `def-do-files-test',
   `def-list-files-test' and the given args.  The absolute ones are test
   $number and the relative ones are test (1+ $number)."
  `(progn
     ;; map-files
     (def-map-files-test ,number :absolute ,files
       ,pathname ',expect-files ,args)
     (def-map-files-test ,(1+ number) :relative ,files
       ,pathname ',expect-files ,args)
     ;; map-dirs
     (def-map-files-test ,number :absolute ,files
       ,pathname ',expect-dirs ,args t)
     (def-map-files-test ,(1+ number) :relative ,files
       ,pathname ',expect-dirs ,args t)
     ;; do-files
     (def-do-files-test ,number :absolute ,files
       ,pathname ',expect-files ,args)
     (def-do-files-test ,(1+ number) :relative ,files
       ,pathname ',expect-files ,args)
     ;; do-dirs
     (def-do-files-test ,number :absolute ,files
       ,pathname ',expect-dirs ,args t)
     (def-do-files-test ,(1+ number) :relative ,files
       ,pathname ',expect-dirs ,args t)
     ;; list-files
     (def-list-files-test ,number :absolute ,files
       ,pathname ',expect-files ,args)
     (def-list-files-test ,(1+ number) :relative ,files
       ,pathname ',expect-files ,args)
     ;; list-dirs
     (def-list-files-test ,number :absolute ,files
       ,pathname ',expect-dirs ,args t)
     (def-list-files-test ,(1+ number) :relative ,files
       ,pathname ',expect-dirs ,args t)))


;;;; Examples.

(deftest map-files (t map-files-0)
  "Test `map-files' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (collect ((files))
      (map-files dir (lambda (file) (files (namestring file))))
      (equal (sort (files) #'string<)
	     (mapcar (lambda (ele) (namestring (merge-pathnames ele dir)))
		     '(".a" "a/" "b/" "c"))))))

(deftest map-dirs (t map-dirs-0)
  "Test `map-dirs' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (collect ((files))
      (map-dirs dir (lambda (file) (files (namestring file))))
      (equal (sort (files) #'string<)
	     (mapcar (lambda (ele) (namestring (merge-pathnames ele dir)))
		     '("a/" "b/"))))))

(deftest do-files (t do-files-0)
  "Test `do-files' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (collect ((files))
      (do-files (file dir)
	(files (namestring file)))
      (equal (sort (files) #'string<)
	     (mapcar (lambda (ele) (namestring (merge-pathnames ele dir)))
		     '(".a" "a/" "b/" "c"))))))

(deftest do-dirs (t do-dirs-0)
  "Test `do-dirs' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (collect ((files))
      (do-dirs (file dir)
	(files (namestring file)))
      (equal (sort (files) #'string<)
	     (mapcar (lambda (ele) (namestring (merge-pathnames ele dir)))
		     '("a/" "b/"))))))

(deftest list-dirs (t list-dirs-0)
  "Test `list-dirs' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/c/" "c" ".a")
    (equal (sort (list-dirs dir) #'string<)
	   (mapcar (lambda (ele) (namestring (merge-pathnames ele dir)))
		   '("a/" "b/")))))


;;;; A mix of files, directories and symlinks.

(def-file-iterator-tests 1
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "c" "c.BAK" "c.CKP" "c.c" "c~"
   ;; FIX "l" because follow-links is () even though check-for-subdirs is t
   "l-to-c" "l")
  ("a.dir/" "b/" "l"))

(def-file-iterator-tests 3
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l" "l-to-c")
  ("a.dir" "b" "l")
  (:check-for-subdirs ()))

(def-file-iterator-tests 5
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.BAK"
   "c.CKP" "c.c" "c~" "l-to-c" "l")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l")
  (:recurse t))

(def-file-iterator-tests 7
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "b/D" "b/D/e" "c" "c.BAK" "c.CKP"
   "c.c" "c~" "l" "l-to-c")
  ("a.dir" "b" "b/D" "b/D/e" "l")
  (:check-for-subdirs () :recurse t))

;; :all

(def-file-iterator-tests 9
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l-to-c" "l")
  ("a.dir/" "b/" "l")
  (:all ()))

(def-file-iterator-tests 11
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l" "l-to-c")
  ("a.dir" "b" "l")
  (:all () :check-for-subdirs ()))

(def-file-iterator-tests 13
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.BAK" "c.CKP" "c.c"
   "c~" "l-to-c" "l")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l")
  (:all () :recurse t))

(def-file-iterator-tests 15
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "b/D" "b/D/e" "c" "c.BAK" "c.CKP" "c.c"
   "c~" "l" "l-to-c")
  ("a.dir" "b" "b/D" "b/D/e" "l")
  (:all () :check-for-subdirs () :recurse t))

;; :backups

(def-file-iterator-tests 17
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "c" "c.c"
   "l-to-c" "l")
  ("a.dir/" "b/" "l")
  (:backups ()))

(def-file-iterator-tests 19
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "c" "c.c"
   "l" "l-to-c")
  ("a.dir" "b" "l")
  (:check-for-subdirs () :backups ()))

(def-file-iterator-tests 21
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.c"
   "l-to-c" "l")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l")
  (:recurse t :backups ()))

(def-file-iterator-tests 23
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "b/D" "b/D/e" "c"
   "c.c" "l" "l-to-c")
  ("a.dir" "b" "b/D" "b/D/e" "l")
  (:check-for-subdirs () :recurse t :backups ()))

;; :backups, :all

(def-file-iterator-tests 25
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "c" "c.c" "l-to-c" "l/")
  ("a.dir/" "b/" "l")
  (:all () :backups ()))

(def-file-iterator-tests 27
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "c" "c.c" "l" "l-to-c")
  ("a.dir" "b" "l")
  (:all () :check-for-subdirs () :backups ()))

(def-file-iterator-tests 29
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.c" "l-to-c" "l")
  ;; FIX surely "l" is a file if the / is being left off
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l")
  (:all () :recurse t :backups ()))

(def-file-iterator-tests 31
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "b/D" "b/D/e" "c" "c.c" "l" "l-to-c")
  ("a.dir" "b" "b/D" "b/D/e" "l")
  (:all () :check-for-subdirs () :recurse t :backups ()))

;; :follow-links

(def-file-iterator-tests 33
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l-to-c" "l/")
  ("a.dir/" "b/" "l/")
  (:follow-links t))

(def-file-iterator-tests 35
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l" "l-to-c")
  ("a.dir" "b" "l")
  (:check-for-subdirs () :follow-links t))

(def-file-iterator-tests 37
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.BAK"
   "c.CKP" "c.c" "c~" "l-to-c" "l/" "l/e/")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l/" "l/e/")
  (:recurse t :follow-links t))

(def-file-iterator-tests 39
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "b/D" "b/D/e" "c" "c.BAK" "c.CKP"
   "c.c" "c~" "l" "l-to-c" "l/e")
  ("a.dir" "b" "b/D" "b/D/e" "l" "l/e")
  (:check-for-subdirs () :recurse t :follow-links t))

;; :follow-links, :all

(def-file-iterator-tests 41
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l-to-c" "l/")
  ("a.dir/" "b/" "l/")
  (:all () :follow-links t))

(def-file-iterator-tests 43
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "c" "c.BAK" "c.CKP" "c.c" "c~"
   "l" "l-to-c")
  ("a.dir" "b" "l")
  (:all () :check-for-subdirs () :follow-links t))

(def-file-iterator-tests 45
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.BAK"
   "c.CKP" "c.c" "c~" "l-to-c" "l/" "l/e/")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l/" "l/e/")
  (:all () :recurse t :follow-links t))

(def-file-iterator-tests 47
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "b/D" "b/D/e" "c" "c.BAK" "c.CKP" "c.c"
   "c~" "l" "l-to-c" "l/e")
  ("a.dir" "b" "b/D" "b/D/e" "l" "l/e")
  (:all () :check-for-subdirs () :recurse t :follow-links t))

;; :follow-links, :backups

(def-file-iterator-tests 49
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "c" "c.c" "l-to-c" "l/")
  ("a.dir/" "b/" "l/")
  (:backups () :follow-links t))

(def-file-iterator-tests 51
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   "c~" (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "c" "c.c" "l" "l-to-c")
  ("a.dir" "b" "l")
  (:check-for-subdirs () :backups () :follow-links t))

(def-file-iterator-tests 53
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.c"
   "l-to-c" "l/" "l/e/")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l/" "l/e/")
  (:recurse t :backups () :follow-links t))

(def-file-iterator-tests 55
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  (".a" ".a.ext" "Ccccc" "a.dir" "b" "b/D" "b/D/e" "c"
   "c.c" "l" "l-to-c" "l/e")
  ("a.dir" "b" "b/D" "b/D/e" "l" "l/e")
  (:check-for-subdirs () :recurse t :backups () :follow-links t))

;; :all, :backups, :follow-links

(def-file-iterator-tests 57
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "c" "c.c" "l-to-c" "l/")
  ("a.dir/" "b/" "l/")
  (:all () :backups () :follow-links t))

(def-file-iterator-tests 59
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "c" "c.c" "l" "l-to-c")
  ("a.dir" "b" "l")
  (:all () :check-for-subdirs () :backups () :follow-links t))

(def-file-iterator-tests 61
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir/" "b/" "b/D/" "b/D/e/" "c" "c.c" "l-to-c" "l/" "l/e/")
  ("a.dir/" "b/" "b/D/" "b/D/e/" "l/" "l/e/")
  (:all () :recurse t :backups () :follow-links t))

(def-file-iterator-tests 63
  ("a.dir/" "b/D/e/" "c" "Ccccc" "c.c" ".a" ".a.ext" "c.BAK" "c.CKP"
   (("l" "b/D")) (("l-to-c" "c")))
  ""
  ("Ccccc" "a.dir" "b" "b/D" "b/D/e" "c" "c.c" "l" "l-to-c" "l/e")
  ("a.dir" "b" "b/D" "b/D/e" "l" "l/e")
  (:all () :check-for-subdirs () :recurse t
   :backups () :follow-links t))


;;;; Directory in file name form.

(def-file-iterator-tests 101
  ("a/a.dir/" "a/b/D/e/" "a/c" "a/Ccccc" "a/c.c" "a/.a" "a/.a.ext" "a/c.BAK" "a/c.CKP"
   "a/c~" (("a/l" "a/b/D")) (("a/l-to-c" "a/c")))
  "a"
  ("a/")
  ("a/"))


;;;; File.

(def-file-iterator-tests 111
  ("a/a.dir/" "a/b/D/e/" "a/c" "a/Ccccc" "a/c.c" "a/.a" "a/.a.ext"
   "a/c.BAK" "a/c.CKP"
   "a/c~" (("a/l" "a/b/D")) (("a/l-to-c" "a/c")))
  "a/c"
  ;; "a/c~" is a file on its own.
  ("a/c" "a/c.BAK" "a/c.CKP" "a/c.c")
  ("a/a.dir/" "a/b/" "a/l"))

(def-file-iterator-tests 113
  ("a/a.dir/" "a/b/D/e/" "a/c" "a/Ccccc" "a/c.c" "a/.a" "a/.a.ext"
   "a/c.BAK" "a/c.CKP" "a/c~" (("a/l" "a/b/D")) (("a/l-to-c" "a/c")))
  "a/c"
  ;; "a/c~" is a file on its own.
  ("a/c" "a/c.c")
  ("a/a.dir/" "a/b/" "a/l")
  (:backups ()))

(def-file-iterator-tests 115
  ("a/a.dir/" "a/b/D/e/" "a/.c" "a/Ccccc" "a/c.c" "a/.a" "a/.a.ext"
   "a/c.BAK" "a/.c.CKP" "a/.c~" (("a/l" "a/b/D")) (("a/l-to-c" "a/c")))
  "a/c"
  ;; "a/.c~" is a file on its own.
  ("a/c.BAK" "a/c.c")
  ("a/a.dir/" "a/b/" "a/l")
  (:all ()))


;;;; Symlink to file.

(def-file-iterator-tests 121
  ("a/" "a/c" "a/c.CKP" "a/c~" (("a/l" "a/c~")) (("a/l-to-c" "a/c")))
  "a/c~"
  ("a/c~")
  ())

(def-file-iterator-tests 123
  ("a/" "a/c" "a/c.CKP" "a/c~" (("a/l" "a/c~")) (("a/l-to-c" "a/c")))
  "a/c~"
  ()
  ()
  (:backups ()))


;;;; Symlink to directory, either file or directory style name.

(def-file-iterator-tests 131
  ("a/" "a/c" "a/c.CKP" "a/c~" (("a/l" "a/")) (("a/l-to-c" "a/c")))
  "a/l"
  ("a/l")
  ("a/l"))

(def-file-iterator-tests 133
  ("a/" "a/c" "a/c.CKP" "a/c~" (("a/l" "a/")) (("a/l-to-c" "a/c")))
  "a/l/"
  ("a/l/c" "a/l/c.CKP" "a/l/c~" "a/l/l-to-c" "a/l/l")
  ;; FIX surely a file
  ("a/l/l")
  (:follow-links ()))

(def-file-iterator-tests 135
  ("a/" "a/.b/" "a/c.CKP" "a/c~" (("a/l" "a/.b/")))
  "a/l"
  ("a/l")
  ;; FIX fails expecting "a/l", which is a file.
  ("a/.b/" "a/l/")
  (:all ()))

(def-file-iterator-tests 137
  ("a/" "a/c" "a/c.CKP" "a/c~" (("a/l" "a/")) (("a/l-to-c" "a/c")))
  "a/l"
  ("a/l")
  ("a/l/")
  (:follow-links t))


;;;; Symlink into subdirectory.

(def-file-iterator-tests 141
  ((("l" "C/ab/")) "b/" "C/ab/" "C/ab/.a")
  "l/"
  ("l/.a")
  ())

(def-file-iterator-tests 143
  ((("l" "C/ab/")) "b/" "C/ab/" "C/ab/.a")
  "l/"
  ("l/.a")
  ()
  ;; Only matters when recursing.
  (:follow-links ()))


;;;; Hidden file.

(def-file-iterator-tests 171
  (".c.lisp")
  ".c.lisp"
  (".c.lisp")
  ())

(def-file-iterator-tests 173
  ()
  ".c.lisp"
  ()
  ())

(def-file-iterator-tests 175
  (".c.lisp" ".c.lisp.BAK")
  ".c.lisp"
  (".c.lisp")
  ()
  (:backups ()))


;;;; Backup file.

(def-file-iterator-tests 201
  ("c.lisp~")
  "c.lisp~"
  ("c.lisp~")
  ())


;;;; Search list.

(deftest map-files (t map-files-230)
  "Test `map-files' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(map-files "a:" (lambda (file) (push file files)))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '(".abc.c.BAK" ".li" "abc.b" "link")))))))

(deftest map-dirs (t map-dirs-230)
  "Test `map-dirs' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b/" ("link" "abc.b/"))
    (with-test-search-list ("a" dir)
      (let (files)
	(map-dirs "a:" (lambda (file) (push file files)))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '("abc.b/" "link/")))))))

(deftest do-files (t do-files-230)
  "Test `do-files' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(do-files (file "a:")
	  (push file files))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '(".abc.c.BAK" ".li" "abc.b" "link")))))))

(deftest do-dirs (t do-dirs-230)
  "Test `do-dirs' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK/" (".li" ".abc.c.BAK/")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(do-dirs (file "a:")
	  (push file files))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '(".abc.c.BAK/" ".li/")))))))

(deftest list-files (t list-files-230)
  "Test `list-files' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (equal (sort (list-files "a:") #'string<)
	     (mapcar (lambda (file)
		       (namestring
			(merge-pathnames file dir)))
		     '(".abc.c.BAK" ".li" "abc.b" "link"))))))

(deftest list-dirs (t list-dirs-230)
  "Test `list-dirs' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b/" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (equal (sort (list-dirs "a:") #'string<)
	     (mapcar (lambda (file)
		       (namestring
			(merge-pathnames file dir)))
		     '("abc.b/" "link/"))))))

(deftest map-files (t map-files-231)
  "Test `map-files' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(map-files "a:abc" (lambda (file) (push file files)))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '("abc.b")))))))

(deftest map-dirs (t map-dirs-231)
  "Test `map-dirs' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b/" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(map-dirs "a:abc" (lambda (file) (push file files)))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '("abc.b/" "link/")))))))

(deftest do-files (t do-files-231)
  "Test `do-files' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(do-files (file "a:abc")
	  (push file files))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '("abc.b")))))))

(deftest do-dirs (t do-dirs-231)
  "Test `do-dirs' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c/" (".li" ".abc.c/")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (let (files)
	(do-dirs (file "a:.abc")
	  (push file files))
	(equal (sort files #'string<)
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '(".abc.c/" ".li/")))))))

(deftest list-files (t list-files-231)
  "Test `list-files' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (equal (sort (list-files "a:abc") #'string<)
	     (mapcar (lambda (file)
		       (namestring
			(merge-pathnames file dir)))
		     '("abc.b"))))))

(deftest list-dirs (t list-dirs-231)
  "Test `list-dirs' with a search list bound to a mixed directory."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b/" ("link" "abc.b/"))
    (with-test-search-list ("a" dir)
      (equal (sort (list-dirs "a:abc") #'string<)
	     (mapcar (lambda (file)
		       (namestring
			(merge-pathnames file dir)))
		     '("abc.b/" "link/"))))))


;;;; Current directory.

(def-file-iterator-tests 261
  ("a/" "b/" "c.lisp~" ".a")
  ; This means a file named ".".
  "."
  ()
  ("a/" "b/"))

(def-file-iterator-tests 263
  ("a/" "b/" "c.lisp~" ".a")
  "./"
  ("./.a" "./a/" "./b/" "./c.lisp~")
  ("./a/" "./b/"))

(def-file-iterator-tests 265
  ("a/z/" "a/b/" "a/c.lisp~" "a/.a")
  "a/z/../"
  ("a/z/../.a" "a/z/../b/" "a/z/../c.lisp~" "a/z/../z/")
  ("a/z/../b/" "a/z/../z/"))


;;;; Wildcards.

(def-file-iterator-tests 290
  ("abc.lisp")
  "*.*"
  ("abc.lisp")
  ())

(def-file-iterator-tests 293
  ()
  "*.*"
  ()
  ())

(def-file-iterator-tests 295
  ("a/c/" "a/c/d" "a/c.CKP" "a/c~" (("a/l" "a/")) (("a/l-to-c" "a/c")))
  "a/*.*"
  ("a/c" "a/c.CKP" "a/c/d" "a/c~" "a/l" "a/l-to-c")
  ("a/c" "a/l" "a/l-to-c")
  (:follow-links () :recurse t :check-for-subdirs ()))


;;;; Errors.

(deftest map-files (t map-files-340)
  "Test `map-files' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (map-files () #'t))
	(error () (setq ret t))))
    ret))

(deftest map-dirs (t map-dirs-340)
  "Test `map-dirs' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc/" (".li" ".abc/")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (map-dirs () #'t))
	(error () (setq ret t))))
    ret))

(deftest do-files (t do-files-340)
  "Test `do-files' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (do-files (file ())))
	(error () (setq ret t))))
    ret))

(deftest do-dirs (t do-dirs-340)
  "Test `do-dirs' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK/" (".li" ".abc.c.BAK/")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (do-dirs (file ())))
	(error () (setq ret t))))
    ret))

(deftest list-files (t list-files-340)
  "Test `list-files' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (list-files ()))
	(error () (setq ret t))))
    ret))

(deftest list-dirs (t list-dirs-340)
  "Test `list-dirs' with an empty pathname."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b/" (".link/" "abc.b"))
      (handler-case
	  (in-directory dir (list-dirs ()))
	(error () (setq ret t))))
    ret))

(deftest map-files (t map-files-341)
  "Test `map-files' with too few args."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (map-files dir))
	(error () (setq ret t))))
    ret))

(deftest map-dirs (t map-dirs-341)
  "Test `map-dirs' with too few args."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b/" (".link" "abc.b/"))
      (handler-case
	  (in-directory dir (map-dirs dir))
	(error () (setq ret t))))
    ret))

(deftest do-files (t do-files-341)
  "Test `do-files' with too few args."
  (let (ret)
    (handler-case
	(do-files (file))
      (error () (setq ret t)))
    ret))

(deftest do-dirs (t do-dirs-341)
  "Test `do-dirs' with too few args."
  (let (ret)
    (handler-case
	(do-dirs (file))
      (error () (setq ret t)))
    ret))

(deftest list-files (t list-files-341)
  "Test `list-files' with too few args."
  (let (ret)
    (handler-case
	(list-files)
      (error () (setq ret t)))
    ret))

(deftest list-dirs (t list-dirs-341)
  "Test `list-dirs' with too few args."
  (let (ret)
    (handler-case
	(list-dirs)
      (error () (setq ret t)))
    ret))

(deftest map-files (t map-files-342)
  "Test `map-files' on a read-blocked directory."
  (with-test-dir (dir "a/b/c/")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-r")
    (collect ((files))
      (in-directory dir (map-files dir
				   (lambda (file)
				     (files (namestring file)))
				   :recurse t))
      (prog1
	  (equal (sort (files) #'string<)
		 (mapcar (lambda (file)
			   (namestring
			    (merge-pathnames file dir)))
			 '("a/" "a/b/")))
	(setf (file-mode (merge-pathnames "a/b" dir)) "a+r")))))

(deftest map-dirs (t map-dirs-342)
  "Test `map-dirs' on a read-blocked directory."
  (with-test-dir (dir "a/b/c/")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-r")
    (collect ((files))
      (in-directory dir (map-dirs dir
				   (lambda (file)
				     (files (namestring file)))
				   :recurse t))
      (prog1
	  (equal (sort (files) #'string<)
		 (mapcar (lambda (file)
			   (namestring
			    (merge-pathnames file dir)))
			 '("a/" "a/b/")))
	(setf (file-mode (merge-pathnames "a/b" dir)) "a+r")))))

(deftest do-files (t do-files-342)
  "Test `do-files' on a read-blocked directory."
  (with-test-dir (dir "a/b/c/")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-r")
    (collect ((files))
      (in-directory dir (do-files (file dir :recurse t)
			  (files (namestring file))))
      (prog1
	  (equal (sort (files) #'string<)
		 (mapcar (lambda (file)
			   (namestring
			    (merge-pathnames file dir)))
			 '("a/" "a/b/")))
	(setf (file-mode (merge-pathnames "a/b" dir)) "a+r")))))

(deftest do-dirs (t do-dirs-342)
  "Test `do-dirs' on a read-blocked directory."
  (with-test-dir (dir "a/b/c/")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-r")
    (collect ((files))
      (in-directory dir (do-dirs (file dir :recurse t)
			  (files (namestring file))))
      (prog1
	  (equal (sort (files) #'string<)
		 (mapcar (lambda (file)
			   (namestring
			    (merge-pathnames file dir)))
			 '("a/" "a/b/")))
	(setf (file-mode (merge-pathnames "a/b" dir)) "a+r")))))

(deftest list-files (t list-files-342)
  "Test `list-files' on a read-blocked directory."
  (with-test-dir (dir "a/b/c/")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-r")
    (prog1
	(equal (in-directory dir
		 (sort (list-files dir #'t :recurse t) #'string<))
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '("a/" "a/b/")))
      (setf (file-mode (merge-pathnames "a/b" dir)) "a+r"))))

(deftest list-dirs (t list-dirs-342)
  "Test `list-dirs' on a read-blocked directory."
  (with-test-dir (dir "a/b/c/")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-r")
    (prog1
	(equal (in-directory dir
		 (sort (list-dirs dir #'t :recurse t) #'string<))
	       (mapcar (lambda (file)
			 (namestring
			  (merge-pathnames file dir)))
		       '("a/" "a/b/")))
      (setf (file-mode (merge-pathnames "a/b" dir)) "a+r"))))

(deftest map-files (() map-files-343)
  "Test `map-files' with a missing relative directory."
  (with-test-dir (dir)
    (collect ((files))
      (in-directory dir
	(map-files "aa/" (lambda (file) (files file)))
	(files)))))

(deftest map-dirs (() map-dirs-343)
  "Test `map-dirs' with a missing relative directory."
  (with-test-dir (dir)
    (collect ((files))
      (in-directory dir
	(map-dirs "aa/" (lambda (file) (files file)))
	(files)))))

(deftest do-files (() do-files-343)
  "Test `do-files' with a missing relative directory."
  (with-test-dir (dir)
    (collect ((files))
      (in-directory dir
	(do-files (file "aa/") (files file))
	(files)))))

(deftest do-dirs (() do-dirs-343)
  "Test `do-dirs' with a missing relative directory."
  (with-test-dir (dir)
    (collect ((files))
      (in-directory dir
	(do-dirs (file "aa/") (files file))
	(files)))))

(deftest list-files (() list-files-343)
  "Test `list-files' with a missing relative directory."
  (with-test-dir (dir)
    (in-directory dir
      (list-files "aa/"))))

(deftest list-dirs (() list-dirs-343)
  "Test `list-dirs' with a missing relative directory."
  (with-test-dir (dir)
    (in-directory dir
      (list-dirs "aa/"))))

(deftest map-files (() map-files-344)
  "Test `map-files' with a missing file."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(map-files "a" (lambda (file) (files file))))
      (files))))

(deftest map-dirs ('("c/") map-dirs-344)
  "Test `map-dirs' with a missing file."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(map-dirs "a" (lambda (file) (files file))))
      (files))))

(deftest do-files (() do-files-344)
  "Test `do-files' with a missing file."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(do-files (file "a")
	  (files file)))
      (files))))

(deftest do-dirs ('("c/") do-dirs-344)
  "Test `do-dirs' with a missing file."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(do-dirs (file "a")
	  (files file)))
      (files))))

(deftest list-files (() list-files-344)
  "Test `list-files' with a missing file."
  (with-test-dir (dir "b" "c/")
    (in-directory dir (list-files "a"))))

(deftest list-dirs ('("c/") list-dirs-344)
  "Test `list-dirs' with a missing file."
  (with-test-dir (dir "b" "c/")
    (in-directory dir (list-dirs "a"))))

(deftest map-files (() map-files-345)
  "Test `map-files', treating a file as a directory."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(map-files "b/" (lambda (file) (files file)))
	(files)))))

(deftest map-dirs (() map-dirs-345)
  "Test `map-dirs', treating a file as a directory."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(map-dirs "b/" (lambda (file) (files file)))
	(files)))))

(deftest do-files (() do-files-345)
  "Test `do-files', treating a file as a directory."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(do-files (file "b/")
	  (files file))
	(files)))))

(deftest do-dirs (() do-dirs-345)
  "Test `do-dirs', treating a file as a directory."
  (with-test-dir (dir "b" "c/")
    (collect ((files))
      (in-directory dir
	(do-dirs (file "b/")
	  (files file))
	(files)))))

(deftest list-files (() list-files-345)
  "Test `list-files', treating a file as a directory."
  (with-test-dir (dir "b" "c/")
    (in-directory dir (list-files "b/"))))

(deftest list-dirs (() list-dirs-345)
  "Test `list-dirs', treating a file as a directory."
  (with-test-dir (dir "b" "c/")
    (in-directory dir (list-dirs "b/"))))


;;;; Broken symlink.

(def-file-iterator-tests 361
  ("a" "b/" (("l" "broken/")) ".a")
  "l"
  ("l")
  ("b/"))

(def-file-iterator-tests 363
  ("a" "b/" (("l" "broken/")) ".a")
  "l"
  ("l")
  ("b/")
  (:follow-links t))

(def-file-iterator-tests 365
  ("a" "b/" (("l" "broken/")) ".a")
  "l"
  ("l")
  ("b/")
  (:follow-links t :recurse t))

(def-file-iterator-tests 367
  ("a" "b/" (("l" "broken/")) ".a")
  "l/"
  ()
  ())

(def-file-iterator-tests 369
  ("a" "b/" (("l" "broken/")) ".a")
  "l/"
  ()
  ()
  (:follow-links t))

(def-file-iterator-tests 371
  ("a" "b/" (("l" "broken/")) ".a")
  "l/"
  ()
  ()
  (:follow-links t :recurse t))


;;;; Self-referencing symlink.

(def-file-iterator-tests 391
  ("a" "b/" (("l" "l")) ".a")
  "l"
  ("l")
  ("b/"))

(def-file-iterator-tests 393
  ("a" "b/" (("l" "l")) ".a")
  "l/"
  ()
  ())


;;;; Device.

(deftest map-files ('("/dev/null") map-files-400)
  "Test `map-files' with an absolute device pathname."
  (collect ((files))
    (map-files "/dev/null" (lambda (file) (files file)))
    (files)))

(deftest do-files ('("/dev/null") do-files-400)
  "Test `do-files' with an absolute device pathname."
  (collect ((files))
    (do-files (file "/dev/null") (files file))
    (files)))

(deftest list-files ('("/dev/null") list-files-400)
  "Test `list-files' with an absolute device pathname."
  (list-files "/dev/null"))

(deftest map-files ('("/dev/null") map-files-401)
  "Test `map-files' with an absolute device pathname."
  (collect ((files))
    (in-directory "/dev/"
      (map-files "/dev/null" (lambda (file) (files file))))
    (files)))

(deftest do-files ('("/dev/null") do-files-401)
  "Test `do-files' with an absolute device pathname."
  (collect ((files))
    (in-directory "/dev/"
      (do-files (file "/dev/null") (files file)))
    (files)))

(deftest list-files ('("/dev/null") list-files-401)
  "Test `list-files' with an absolute device pathname."
  (in-directory "/dev/" (list-files "/dev/null")))
