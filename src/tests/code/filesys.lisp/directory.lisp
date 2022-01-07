;;; Tests of lisp:directory.
;;
;; FIX file extensions
;; FIX consider integrating with map-files

(in-package "LISP")

(import '(deftest:deftest))


;;;; Files and directories, absolute return.

(deftest directory (() directory-1)
  "Test `directory' with an empty absolute dir."
  (let ((dir (deftest:make-test-dir)))
    (unwind-protect
	(directory dir)
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-2)
  "Test `directory' with a single file, implicitly passing an absolute
   dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory)))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring (merge-pathnames "a" dir))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-3)
  "Test `directory' with a single directory, passing an absolute dir."
  (let ((dir (deftest:make-test-dir "a/")))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory ())))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring (merge-pathnames "a/" dir))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-4)
  "Test `directory' with files and directories, passing an absolute dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir)))
	  (and (eq (length names) (length spec))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length spec) t)
		   (or (member (namestring (merge-pathnames (nth index spec)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-5)
  "Test `directory' with a single file, passing a relative dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./")))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring (merge-pathnames "a" dir))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-6)
  "Test `directory' with a single directory, passing a relative dir."
  (let ((dir (namify (deftest:make-test-dir "a/"))))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory (directorify (file-namestring dir)))))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring
			   (merge-pathnames "a/"
					    (directorify dir)))))))
      (dired:delete-file (directorify dir) :recurse t))))

(deftest directory (t directory-7)
  "Test `directory' with files and directories, passing a relative dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (in-directory "dir2/"
	    (let ((names (directory "../")))
	      (and (eq (length names) (length spec))
		   (let ((names (mapcar #'namestring names)))
		     (dotimes (index (length spec) t)
		       (or (member (namestring (merge-pathnames (nth index spec)
								dir))
				   names
				   :test #'string=)
			   (return))))))))
      (dired:delete-file dir :recurse t))))


;;;; Files and directories, relative return.

;; FIX these should return relative pathnames, `directory' needs updating

(deftest directory (() directory-11)
  "Test `directory' with an empty absolute dir, with :absolute ()."
  (let ((dir (deftest:make-test-dir)))
    (unwind-protect
	(directory dir :absolute ())
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-12)
  "Test `directory' with a single file, implicitly passing an absolute
   dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory () :absolute ())))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring (merge-pathnames "a" dir))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-13)
  "Test `directory' with a single directory, passing an absolute dir."
  (let ((dir (deftest:make-test-dir "a/")))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory () :absolute ())))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring (merge-pathnames "a/" dir))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-14)
  "Test `directory' with files and directories, passing an absolute dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir :absolute ())))
	  (and (eq (length names) (length spec))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length spec) t)
		   (or (member (namestring (merge-pathnames (nth index spec)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-15)
  "Test `directory' with a single file, passing a relative dir."
  (let ((dir (deftest:make-test-dir "a")))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./" :absolute ())))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring (merge-pathnames "a" dir))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-16)
  "Test `directory' with a single directory, passing a relative dir."
  (let ((dir (namify (deftest:make-test-dir "a/"))))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory (directorify (file-namestring dir))
				  :absolute ())))
	    (and (eq (length names) 1)
		 (string= (namestring (car names))
			  (namestring
			   (merge-pathnames "a/"
					    (directorify dir)))))))
      (dired:delete-file (directorify dir) :recurse t))))

(deftest directory (t directory-17)
  "Test `directory' with files and directories, passing a relative dir."
  (let* ((spec '("a" "dir1/" "b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (in-directory "dir2/"
	    (let ((names (directory "../" :absolute ())))
	      (and (eq (length names) (length spec))
		   (let ((names (mapcar #'namestring names)))
		     (dotimes (index (length spec) t)
		       (or (member (namestring (merge-pathnames (nth index spec)
								dir))
				   names
				   :test #'string=)
			   (return))))))))
      (dired:delete-file dir :recurse t))))


;;;; Symlinks, limited to a single directory (vs recursing).

; * (directory "./" :truenamep t  :follow-links () :absolute ())
; * (directory "./" :truenamep () :follow-links () :absolute ())
; * (directory "./" :truenamep t  :follow-links t  :absolute ())
; * (directory "./" :truenamep () :follow-links t  :absolute ())
; * (directory "./" :truenamep t  :follow-links () :absolute t)
; * (directory "./" :truenamep t  :follow-links t  :absolute t)
; * (directory "./" :truenamep () :follow-links () :absolute t)
; * (directory "./" :truenamep () :follow-links t  :absolute t)

(deftest directory (t directory-30)
  "Test `directory' with files, directories and symlinks, with :truenamep
   t, :follow-links () and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "b" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep t
				  :follow-links ()
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :truenamep t overrides :absolute ().
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are expanded.
				     (list (cadr entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-31)
  "Test `directory' with files, directories and symlinks, with :truenamep
   (), :follow-links () and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 ;; ./dl instead of ./dl/ because follow-links is ().
	 (expect '("./a" "./dir1/" "./b" "./dir2/" ("./dl" "./dir1/")
		   "./c" ("./l" "./c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep ()
				  :follow-links ()
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   (dotimes (index (length expect) t)
		     (or (member
			  (let ((entry (nth index expect)))
			    (etypecase entry
			      ;; Link names are preserved by :truenamep ().
			      (list (car entry))
			      (string entry)))
			  names
			  :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-32)
  "Test `directory' with files, directories and symlinks, with :truenamep
   t, :follow-links t and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "b" "dir2/" ("dl/" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep t
				  :follow-links t
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :truenamep t overrides :absolute ().
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are expanded.
				     (list (cadr entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-33)
  "Test `directory' with files, directories and symlinks, with :truenamep
   (), :follow-links t and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("./a" "./dir1/" "./b" "./dir2/" ("./dl/" "./dir1/")
		   "./c" ("./l" "./c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep ()
				  :follow-links t
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   (dotimes (index (length expect) t)
		     (or (member
			  (let ((entry (nth index expect)))
			    (etypecase entry
			      ;; Link names are preserved by :truenamep ().
			      (list (car entry))
			      (string entry)))
			  names
			  :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-34)
  "Test `directory' with files, directories and symlinks, with :truenamep
   t, :follow-links () and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "b" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep t
				  :follow-links ()
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :truenamep t includes :absolute t.
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are expanded.
				     (list (cadr entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-35)
  "Test `directory' with files, directories and symlinks, with :truenamep
   t, :follow-links t and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "b" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep t
				  :follow-links t
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :truenamep t includes :absolute t.
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are expanded.
				     (list (cadr entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-36)
  "Test `directory' with files, directories and symlinks, with :truenamep
   (), :follow-links () and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("./a" "./dir1/" "./b" "./dir2/" ("./dl" "dir1/")
		   "./c" ("./l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep ()
				  :follow-links ()
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are preserved due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "./dir1/"
								dir))
				   names
				   :test #'equal)
			    1)
			(eq (count (namestring (merge-pathnames "./c" dir))
				   names
				   :test #'equal)
			    1)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :absolute t.
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are preserved.
				     (list (car entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-37)
  "Test `directory' with files, directories and symlinks, with :truenamep
   (), :follow-links t and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("./a" "./dir1/" "./b" "./dir2/" ("./dl/" "dir1/")
		   "./c" ("./l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :truenamep ()
				  :follow-links t
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are preserved due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "./dir1/"
								dir))
				   names
				   :test #'equal)
			    1)
			(eq (count (namestring (merge-pathnames "./c" dir))
				   names
				   :test #'equal)
			    1)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :absolute t.
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are preserved.
				     (list (car entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "recurse".

(deftest directory (t directory-40)
  "Test `directory' with files and recursive subdirectories."
  (let* ((spec '("a" "dir1/" "dir1/sub1/" "dir1/sub/"
		 "dir1/sub/subsub/" "dir1/sub/subsub/b" "dir2/" "c"))
	 (expect '("a" "dir1/" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir)))
	  (and (eq (length names) (length expect))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length expect) t)
		   (or (member (namestring (merge-pathnames (nth index expect)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-41)
  "Test recursive `directory' with files and recursive subdirectories."
  (let* ((spec '("a" "dir1/" "dir1/sub1/" "dir1/sub/" "dir1/sub/subsub/"
		 "dir1/sub/subsub/b" "dir2/" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir :recurse t)))
	  (and (eq (length names) (length spec))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length spec) t)
		   (or (member (namestring (merge-pathnames (nth index spec)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))


;;;; Symlinks, recursing into subdirs.

; * (directory "./" :truenamep t  :follow-links () :absolute ())
; * (directory "./" :truenamep () :follow-links () :absolute ())
; * (directory "./" :truenamep t  :follow-links t  :absolute ())
; * (directory "./" :truenamep () :follow-links t  :absolute ())
; * (directory "./" :truenamep t  :follow-links () :absolute t)
; * (directory "./" :truenamep t  :follow-links t  :absolute t)
; * (directory "./" :truenamep () :follow-links () :absolute t)
; * (directory "./" :truenamep () :follow-links t  :absolute t)

(deftest directory (t directory-50)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep t, :follow-links () and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		   "c" ("l" "c")))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep t
				  :follow-links ()
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			;; :follow-links ()
			(eq (count (namestring (merge-pathnames "dir1/end" dir))
				   names
				   :test #'equal)
			    1)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :truenamep t overrides :absolute ().
				(merge-pathnames
				 (let ((entry (nth index expect)))
				   (etypecase entry
				     ;; Link names are expanded.
				     (list (cadr entry))
				     (string entry)))
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-51)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep (), :follow-links () and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("./a" "./dir1/" "./dir1/end" "./b" "./dir2/" "./dl/"
		   "./c" "./l"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep ()
				  :follow-links ()
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   (dotimes (index (length expect) t)
		     (or (member
			  (nth index expect)
			  names
			  :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-52)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep t, :follow-links t and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "dir1/end" "b" "dir2/" "dir1/" "dir1/end"
		   "c" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep t
				  :follow-links t
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :truenamep t overrides :absolute ().
				(merge-pathnames (nth index expect)
						 dir))
				names
				:test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-53)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep (), :follow-links t and :absolute ()."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 ;; Link names are preserved by :truenamep ().
	 (expect '("./a" "./dir1/" "./dir1/end" "./b" "./dir2/" "./dl/"
		   "./dl/end" "./c" "./c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep ()
				  :follow-links t
				  :absolute ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   (dotimes (index (length expect) t)
		     (or (member
			  (nth index expect)
			  names
			  :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-54)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep t, :follow-links () and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("a" "dir1/" "dir1/end" "b" "dir2/" "dir1/"
		   "c" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep t
				  :follow-links ()
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				(merge-pathnames
				 (nth index expect)
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-55)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep t, :follow-links t and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 ;; Link names are expanded by :truenamep t.
	 (expect '("a" "dir1/" "dir1/end" "b" "dir2/" "dir1/"
		   "dir1/end" "c" "c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep t
				  :follow-links t
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are expanded due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "dir1/"
								dir))
				   names
				   :test #'equal)
			    2)
			(eq (count (namestring (merge-pathnames "c" dir))
				   names
				   :test #'equal)
			    2)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				(merge-pathnames
				 (nth index expect)
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-56)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep (), :follow-links () and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 ;; Link names are preserved.
	 (expect '("./a" "./dir1/" "./dir1/end" "./b" "./dir2/" "./dl/"
		   "./c" "./c"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep ()
				  :follow-links ()
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are preserved due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "./dir1/"
								dir))
				   names
				   :test #'equal)
			    1)
			(eq (count (namestring (merge-pathnames "./c" dir))
				   names
				   :test #'equal)
			    1)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :absolute t.
				(merge-pathnames
				 (nth index expect)
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-57)
  "Test recursive `directory' with files, directories and symlinks, with
   :truenamep (), :follow-links t and :absolute t."
  (let* ((spec '("a" "dir1/" "dir1/end" "b" "dir2/" ("dl" "dir1/")
		 "c" ("l" "c")))
	 (expect '("./a" "./dir1/" "./dir1/end" "./b" "./dir2/" "./dl/"
		   "./dl/end" "./c" "./l"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./"
				  :recurse t
				  :truenamep ()
				  :follow-links t
				  :absolute t)))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   ;; Links are preserved due to :truenamep t.
		   (and (eq (count (namestring (merge-pathnames "./dir1/"
								dir))
				   names
				   :test #'equal)
			    1)
			(eq (count (namestring (merge-pathnames "./c" dir))
				   names
				   :test #'equal)
			    1)
			(dotimes (index (length expect) t)
			  (or (member
			       (namestring
				;; :absolute t.
				(merge-pathnames
				 (nth index expect)
				 dir))
			       names
			       :test #'string=)
			      (return))))))))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "backups".

(deftest directory (t directory-60)
  "Test `directory' with files and subdirectories, including backups."
  (let* ((spec '("z.bak" "dir/" "a~" "dir0~/" "b.BAK" "dir2.BAK/"
		 "c.CKP" "dir2.CKP/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir)))
	  (and (eq (length names) (length spec))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length spec) t)
		   (or (member (namestring (merge-pathnames (nth index spec)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-61)
  "Test `directory' with :backups () on files and subdirectories, both of
   which include backups."
  (let* ((spec '("aa.bak" "dir/" "b" "a~" "dir0~/" "b.BAK" "dir2.BAK/"
		 "c.CKP" "dir2.CKP/"))
	 (expect '("aa.bak" "b" "dir/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir :backups ())))
	  (and (eq (length names) (length expect))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length expect) t)
		   (or (member (namestring (merge-pathnames (nth index expect)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "all".

(deftest directory (t directory-70)
  "Test `directory' on a directory with files and subdirectories, including
   hidden files and directories."
  (let* ((spec '("a" ".a" "dir/" ".dir0/" ".b.BAK" ".dir2.BAK/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir)))
	  (and (eq (length names) (length spec))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length spec) t)
		   (or (member (namestring (merge-pathnames (nth index spec)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-71)
  "Test `directory' with :all () on files and subdirectories which both
   include hidden files."
  (let* ((spec '("a" ".a" "dir/" ".dir0/" ".b.BAK" ".dir2.BAK/" "c.BAK"))
	 (expect '("a" "dir/" "c.BAK"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir :all ())))
	  (and (eq (length names) (length expect))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length expect) t)
		   (or (member (namestring (merge-pathnames (nth index expect)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))


;;;; Key argument "check-for-subdirs".

(deftest directory (t directory-80)
  "Test `directory' on a directory with files and subdirectories, including
   hidden files and directories, where directories will be appended with
   slashes."
  (let* ((spec '("a" ".b" "dir/" "dir2.BAK/" ".dir3/"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(let ((names (directory dir)))
	  (and (eq (length names) (length spec))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length spec) t)
		   (or (member (namestring (merge-pathnames (nth index spec)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-81)
  "Test `directory' with :check-for-subdirs () on a directory with
   files and subdirectories, including hidden files and directories."
  (let* ((spec '("a" ".b" "dir/" "dir2.BAK/" ".dir3/"))
	 (expect '("a" ".b" "dir" "dir2.BAK" ".dir3"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	;; :truenamep t would add the trailing slash.
	(let ((names (directory dir :check-for-subdirs () :truenamep ())))
	  (and (eq (length names) (length expect))
	       (let ((names (mapcar #'namestring names)))
		 (dotimes (index (length expect) t)
		   (or (member (namestring (merge-pathnames (nth index expect)
							    dir))
			       names
			       :test #'string=)
		       (return))))))
      (dired:delete-file dir :recurse t))))


;;;; Wildcards.

(deftest directory (t directory-90)
  "Test `directory' with a relative pathname with wildcards, on a directory
   with files and subdirectories, including hidden files and directories."
  (let* ((spec '("a" ".b" "dir/" "dir2.BAK/" ".dir3/"))
	 (expect '("a" ".b" "dir" "dir2.BAK" ".dir3"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./*.*"
				  :check-for-subdirs ()
				  ;; `truename' adds trailing slashes.
				  :truenamep ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names)))
		   (dotimes (index (length expect) t)
		     (or (member (namestring
				  (merge-pathnames
				   (nth index expect)
				   (merge-pathnames "./" dir)))
				 names
				 :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-91)
  "Test `directory' with a relative pathname with limiting wildcards, on a
   directory with files and subdirectories, including hidden files and
   directories."
  (let* ((spec '("b" ".b" "dir/" "bdir2.BAK/" ".dir3/"))
	 (expect '("b" "bdir2.BAK"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./b*.*"
				  :check-for-subdirs ()
				  ;; `truename' adds trailing slashes.
				  :truenamep ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names))
		       (dir (merge-pathnames "./" dir)))
		   (dotimes (index (length expect) t)
		     (or (member (namestring
				  (merge-pathnames (nth index expect)
						    dir))
				 names
				 :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-92)
  "Test `directory' with a relative pathname with a limiting name and type
   wildcard, on a directory with files and subdirectories, including hidden
   files and directories."
  (let* ((spec '("b1.a" "b.b" "dir/" "bdir2.BAK/" "bdir3.a/"))
	 (expect '("b1.a" "bdir3.a"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./b*.a"
				  :check-for-subdirs ()
				  ;; `truename' adds trailing slashes.
				  :truenamep ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names))
		       (dir (merge-pathnames "./" dir)))
		   (dotimes (index (length expect) t)
		     (or (member (namestring
				  (merge-pathnames (nth index expect)
						   dir))
				 names
				 :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))

(deftest directory (t directory-93)
  "Test `directory' with a relative pathname with a name wildcard, on a
   directory with files and subdirectories, including hidden files and
   directories.

   Call with :truenamep () and :absolute t.  This shows an error in
   `directory' which was affecting `Complete Field'."
  (let* ((spec '("b1.a" "b.b" "dir/" "bdir2.BAK/" "bdir3.a/"))
	 (expect '("b.b"))
	 (dir (apply #'deftest:make-test-dir spec)))
    (unwind-protect
	(in-directory dir
	  (let ((names (directory "./b.*"
				  :truenamep ()
				  :absolute t
				  :check-for-subdirs ())))
	    (and (eq (length names) (length expect))
		 (let ((names (mapcar #'namestring names))
		       (dir (merge-pathnames "./" dir)))
		   (dotimes (index (length expect) t)
		     (or (member (namestring
				  (merge-pathnames (nth index expect)
						   dir))
				 names
				 :test #'string=)
			 (return)))))))
      (dired:delete-file dir :recurse t))))


;;;; Errors.

(deftest directory (t directory-100)
  "Test `directory' with a missing directory."
  (let ((dir (ensure-trailing-slash (pick-new-name)))
	ret)
    (handler-case
	(directory dir)
      (file-error () (setq ret t)))
    ret))


;;;; Passing a subdir.

(deftest directory (t directory-110)
  "Test `directory' on a subdirectory."
  (deftest:with-test-dir (dir "a/b/" "a/b/c.y" "a/b/d" "xx/")
    (let ((expect '("c.y" "d"))
	  (names (in-directory dir (directory "a/b/"))))
      (and (eq (length names) (length expect))
	   (let ((names (mapcar #'namestring names))
		 (dir (merge-pathnames "a/b/" dir)))
	     (dotimes (index (length expect) t)
	       (or (member (namestring (merge-pathnames (nth index expect)
							dir))
			   names
			   :test #'string=)
		   (return))))))))

(deftest directory (t directory-111)
  "Test `directory' on a subdirectory, with :truenamep ()."
  (deftest:with-test-dir (dir "a/b/" "a/b/c.y" "a/b/d" "xx/")
    (let ((expect '("a/b/c.y" "a/b/d"))
	  (names (in-directory dir (directory "a/b/" :truenamep ()))))
      (and (eq (length names) (length expect))
	   (let ((names (mapcar #'namestring names)))
	     (dotimes (index (length expect) t)
	       (or (member (namestring (merge-pathnames (nth index expect)
							dir))
			   names
			   :test #'string=)
		   (return))))))))

(deftest directory (t directory-112)
  "Test `directory' on a subdirectory, with :truenamep () and :absolute
   ()."
  (deftest:with-test-dir (dir "a/b/" "a/b/c.y" "a/b/d" "xx/")
    (let ((expect '("a/b/c.y" "a/b/d"))
	  (names (in-directory dir (directory "a/b/"
					      :absolute ()
					      :truenamep ()))))
      (and (eq (length names) (length expect))
	   (let ((names (mapcar #'namestring names)))
	     (dotimes (index (length expect) t)
	       (or (member (namestring (nth index expect))
			   names
			   :test #'string=)
		   (return))))))))
