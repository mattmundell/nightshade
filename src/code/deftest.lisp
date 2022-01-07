;;;; Test suite.

(in-package "DEFTEST")

(export '(*tests*
	  check-verbose-list
	  create-tree
	  deftest do-test-symbols
	  make-test-dir
	  random-pathname random-string
	  test test-from-dir
	  test-string-1 test-string-2 test-string-3 test-string-4
	  with-test-dir with-test-search-list))


#[ Test Suite

The `deftest' macro provides a simple test suite.

{function:deftest}
{function:test}
{function:test-from-dir}

== Test Helper Functions ==

{function:with-test-dir}
{function:with-test-search-list}

== Else where ==

[ Running System Tests ]  The predefined system tests.
[ Writing System Tests ]  Notes on writing these tests.
[ Editor Test Commands ]
]#


(defvar *tests* (make-string-table))

(defmacro deftest (symbol (result &optional name &key (test #'equal))
		   doc &body body)
  "deftest name (symbol result [keys]) doc body

   Define test $name with $doc which is associated with $symbol.  The test
   succeeds if the result of $body equals $result (according to predicate
   $test)."
  (let ((name (or name (gensym)))
	(ret (gensym))
	(fun (gensym))
	(expected (gensym)))
    `(progn
       (defun ,name ()
	 ,(or doc "A test defined by `deftest'.")
	 (flet ((,fun ()
		  (block ()
		    ,@body)))
	   (let ((,expected ,result)
		 (,ret (,fun)))
	     (values (funcall ,test ,ret ,expected) ,ret ,expected))))
       (if (member ',name
		   (getstring (symbol-name ',symbol) *tests*))
	   (warn "Replacing ~A on ~A." ',name ',symbol))
       (setf (getstring (symbol-name ',symbol) *tests*)
	     (append (delete ',name (getstring (symbol-name ',symbol) *tests*))
		     (list ',name))))))

(defmacro test (symbol)
  "Run the tests associated with $symbol, returning #t on success, else ()
   and the symbol of the test function that failed, the value returned from
   the test and the expected return."
  (let ((test (gensym)) (pass (gensym))
	(value (gensym)) (expected (gensym)))
    `(progn
       (or (getstring (symbol-name ,symbol) *tests*)
	   (error "Failed to find any tests for ~A." ,symbol))
       (loop for ,test in (getstring (symbol-name ,symbol) *tests*)
	 finally return t do
	 (format t "    ~A~%" ,test)
	 (multiple-value-bind (,pass ,value ,expected)
			      (funcall ,test)
	   (or ,pass
	       (return (values () ,test ,value ,expected))))))))

(defun test-from-dir (dir)
  "Run the tests in $dir."
  (let ((*tests* (make-string-table))
	(fails 0))
    (format t "Testing ~A...~%~%" dir)
    (format t "Loading tests...~%")
    (do-files (file dir :recurse t :backups ())
      (or (directoryp file)
	  (symlinkp file)
	  (progn
	    (format t "  ~A~%" file)
	    (load file :verbose ()))))
    (format t "~%Running tests...~%")
    (do-test-symbols (symbol)
      (format t "  ~A~%" symbol)
      (multiple-value-bind (pass test value expected)
			   (test symbol)
	(if pass
	    (format t "~%")
	    (progn
	      (format t "      fail on ~A: returned ~A, expected ~A~%~%"
		      test value expected)
	      (incf fails)))))
    (if (zerop fails)
	(format t "All tests passed.~%")
	(format t "~A test~:P failed.~%" fails))))

(defmacro do-test-symbols ((symbol) &body body)
  "do-test-symbols ($symbol)

   Evaluate $body with $symbol bound to each symbol for which tests are defined."
  (let ((name (gensym)) (value (gensym)))
    `(do-strings (,name ,value *tests*)
       (declare (ignore ,value))
       (let ((,symbol (read-from-string ,name)))
	 ,@body))))


;;;; Common data used in system tests.

(defconstant test-string-1 "abcdefghijklmnopqrstuvwxyz")
(defconstant test-string-2 "0123456789")
(defconstant test-string-3 "0123456789
abcdefghijklmnopqrstuvwxyz
.,/;:][{}()_-=+|\/!#$%^&*
")
(defconstant test-string-4 "0123456789
abcdefghijklmnopqrstuvwxyz
.,/;:][{}()_-=+|\/!#$%^&*")


;;;; Common functions used in system tests.

(declaim (inline random-filename))
(defun random-pathname (&key (min 2) (max 14))
  "Return a random pathname string, at least $min and at most $max
   characters long."
  ;; FIX could add more range to the predicate
  (random-string :min min
		 :max max
		 :predicate (lambda (ch) (or (alphanumericp ch)
					     (char= ch #\.)))))

(defun random-string (&key (min 5) (max 500) (predicate #'standard-char-p))
  "Return a random string, at least $min and at most $max characters long,
   where the characters satisfy $predicate."
  (let* ((len (+ (random (- (1+ max) min)) min))
	 (string (make-string len)))
    (loop for i from 0 to (1- len) do
      (loop for code = (random #xff) do
	(when (funcall predicate (code-char code))
	  (setf (char string i) (code-char code))
	  (return ()))))
    string))

(defun pick-dir (root &optional (num (random 100)))
  "Return a relative directory in directory $root, including the trailing
   slash."
  (let (found)
    (do-files (file root :recurse t)
      (when (directoryp file)
	(if (zerop num)
	    (return-from pick-dir
			 (setq found
			       (ensure-trailing-slash file)))
	    (decf num))))
    (or found (pick-dir root num))))

(defun link-tree (root)
  "Add links into tree $root."
  (do-files (file root :recurse t)
    (when (zerop (random 3))
      (if (plusp (random 2))
	  ;; Absolute.
	  (symlink-file (concat (pick-dir root)
				(random-pathname))
			(truename file))
	  ;; Relative.  Must be in the root directory for
	  ;; `relate-pathname'.
	  (let ((link (lisp::relate-pathname (concat (pick-dir root)
						     (random-pathname))
					     file)))
	    (in-directory file
	      (symlink-file link (file-namestring file))))))))

(defun create-tree ()
  "Return the root pathname of a newly created tree of files and
   directories."
  (let ((top (pick-new-dir)))
    (labels
	((pick-new-dir-name ()
	   "Return a directory namestring."
	   (ensure-trailing-slash (random-pathname)))
	 (tree (size root)
	   "Create a tree up to $size deep at $root."
	   (ensure-directories-exist root)
	   (in-directory root
	     ;; Create files.
	     (loop repeat (random 3) do
	       (with-open-file (s (random-pathname)
				  :direction :output
				  :if-exists :append
				  :if-does-not-exist :create)
		 (write-string (random-string :min 0) s)
		 (if (plusp (random 3)) (terpri s))))
	     ;; Create subdirs.
	     (if (plusp size)
		 (loop repeat (1+ (random 3)) do
		   (tree (1- size) (pick-new-dir-name)))))))
      (in-directory top
	(tree (+ 2 (random 2)) "")
	;; Link some of the files and directories.
	(link-tree "")
	top))))

(defun make-test-dir (&rest pathnames)
  "Create $pathnames in a new temporary directory.  Return the pathname of
   the temporary directory.

   All $pathnames must be relative."
  (let ((dir (pick-new-dir)))
    (in-directory dir
      (dolist (pathname pathnames)
	(etypecase pathname
	  (list
	   (or (relativep (car pathname))
	       (error "pathname must be relative: ~A" pathname))
	   (symlink-file (car pathname) (cadr pathname)))
	  (string
	   (or (relativep pathname)
	       (error "pathname must be relative: ~A" pathname))
	   (if (directory-name-p pathname)
	       (ensure-directories-exist pathname)
	       (touch-file pathname))))))
    dir))

(defmacro with-test-dir ((dir &rest pathnames) &body body)
  "with-test-dir ($dir pathname*) $body

   Create a temporary directory with `make-test-dir' according to
   $pathnames.  Evaluate $body with the name of the directory bound to
   $dir.  Remove the temporary directory."
  `(let ((,dir (apply #'make-test-dir ',pathnames)))
     (unwind-protect
	 (progn
	   ,@body)
       (let ((dired:*report-function* #'t))
	 (dired:delete-file ,dir :recurse t)))))

(defmacro with-test-search-list ((name &rest pathnames) &body body)
  "with-test-search-list ($name pathname*) $body

   Evaluate $body with $pathnames set as search list $name, restoring the
   old value of search list $name afterwards."
  (let ((old (gensym)) (list-name (gensym)) (defined-p (gensym)))
    `(let* ((,list-name (concatenate 'string ,name ":"))
	    (,defined-p (search-list-defined-p ,list-name))
	    (,old (if ,defined-p (search-list ,list-name))))
       (setf (search-list ,list-name) (list ,@pathnames))
       (unwind-protect
	   (progn
	     ,@body)
	 (if ,defined-p
	     (setf (search-list ,list-name) ,old)
	     (clear-search-list ,list-name))))))


;;;; File system functions.

(defun check-verbose-list (string spec dir)
  "Return true if $string is the verbose output for the directory defined
   by $spec."
  ;; -rw-rw-rw-  1 guest           0 Mar 29 19:06 a
  (with-input-from-string (stream string)
    (let ((heading (read-line stream ())))
      (or (and heading
	       (string= heading (format () "v Directory of ~A :" dir)))
	  (return-from check-verbose-list ()))
      (macrolet ((test (exp)
		   `(or ,exp
			(progn
			  ;(format t "~A failed" ',exp)
			  (return-from check-verbose-list ())))))
	(iterate iter ((line (read-line stream ()))
		       (spec spec))
	  (when line
	    (test spec)
	    (test (> (length line) 22))
	    (test (integerp (parse-mode-string line)))
 	    (multiple-value-bind (val end)
				 (read-from-string line () () :start 10)
	      ;; Number of links.
 	      (test (integerp val))
	      (multiple-value-bind (val end)
				   (read-from-string line () () :start end)
 		(test (and (symbolp val)
 			   (string= (string-downcase (symbol-name val))
 				    (user-name))))
		(multiple-value-bind (val end)
				     (read-from-string line () ()
						       :start end)
		  ;; File size.
		  (test (integerp val))
		  (test (parse-time line :start end :end (+ end 12)))
		  (if (listp (car spec))
		      ;; Symlink.
		      (progn
			(test (string= line (caar spec)
				       :start1 (+ end 13)
				       :end1 (+ end 13
						(length (caar spec)))))
			(test (string= line "-->"
				       :start1 (+ end 14
						  (length (caar spec)))
				       :end1 (+ end 14
						(length (caar spec))
						3)))
			;; FIX Could also test dest.
			)
		      (test (string= line (namestring (car spec))
				     :start1 (+ end 13)))))))
	    (iter (read-line stream ()) (cdr spec)))))
      t)))


#[ Writing System Tests

full coverage

    each test must test a single case
    --
    code coverage
    functionality coverage
    input coverage
    context coverage (i.e. state of system while function running)
    --
    many gaps in existing tests

assume that that the support for the test works
	ie only check the feature of the function under test
	and leave as little after failure as psbl (eg temp files)
also test error cases
test extreme cases
test interp,byte,native

cleanup after test
    eg temp files
	also pkgs must be left in orig state
		temp test pkgs?

would like to test speed
    eg test mh:read-field with many-lined field
	could add time estimate param to tests  then could fail on time too

== Testing File System Functions ==

    spaces in filenames
    punctuation in filenames

    absolute
    relative
    wildcards
        :newest :wild
    version numbers
    . .. "" ./ ../
    search list  empty
    file types

    context  lone, siblings, files with content

    file
    directory  empty
    symlink
        to file, dir, empty dir, symlink
        to subdir, ancestor, direct parent, sibling, self
        broken symlink

    backup
    hidden
    special

    parents,subdirs
    permissions?

    errors

]#
