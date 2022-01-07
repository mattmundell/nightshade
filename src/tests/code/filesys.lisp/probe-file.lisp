;;; Tests of lisp:probe-file.
;;
;; Very similar to truename.lisp.
;;
;; The first sections below cover calls with files, directorys and
;; symlinks, varying over:
;;
;;   - relative and absolute pathnames
;;   - (for symlinks) following and checking for symlinks
;;   - (for symlinks) relative and absolute symlink

(in-package "LISP")

(import '(deftest:deftest))


;;;; File, absolute pathname.

(deftest probe-file (t probe-file-1)
  "Test `probe-file' on a file with an absolute pathname."
  (let ((file (pick-new-file)))
    (unwind-protect
	(multiple-value-bind (pathname type)
			     (probe-file file)
	  (and (string= (namestring pathname) (namestring file))
	       (eq type :file)))
      (delete-file file))))

(deftest probe-file (t probe-file-2)
  "Test `probe-file' on a typed file with an absolute pathname."
  (let ((file (pick-new-file "/tmp/tmp-~D-~D.lisp")))
    (unwind-protect
	(multiple-value-bind (pathname type)
			     (probe-file file)
	  (and (string= (namestring pathname) (namestring file))
	       (eq type :file)))
      (delete-file file))))

(deftest probe-file ('(()) probe-file-3)
  "Test `probe-file' on a missing file with an absolute pathname."
  (let ((file (pick-new-file)))
    (delete-file file)
    (multiple-value-list (probe-file file))))


;;;; File, relative pathname.

(deftest probe-file (t probe-file-10)
  "Test `probe-file' on a file with a relative pathname."
  (let ((file (pick-new-file)))
    (unwind-protect
	(in-directory file
	  (multiple-value-bind (pathname type)
			       (probe-file (file-namestring file))
	    (and (string= (namestring pathname) (namestring file))
		 (eq type :file))))
      (delete-file file))))

(deftest probe-file (t probe-file-11)
  "Test `probe-file' on a typed file with a relative pathname."
  (let ((file (pick-new-file "/tmp/tmp-~D-~D.lisp")))
    (unwind-protect
	(in-directory file
	  (multiple-value-bind (pathname type)
			       (probe-file (file-namestring file))
	    (and (string= (namestring pathname) (namestring file))
		 (eq type :file))))
      (delete-file file))))

(deftest probe-file ('(()) probe-file-12)
  "Test `probe-file' on a missing file with an relative pathname."
  (let ((file (pick-new-file)))
    (delete-file file)
    (in-directory file
      (multiple-value-list (probe-file (file-namestring file))))))


;;;; Directory, absolute pathname.

(deftest probe-file (t probe-file-20)
  "Test `probe-file' on a directory with an absolute pathname."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(multiple-value-bind (pathname type)
			     (probe-file dir)
	  (and (string= (namestring pathname) (namestring dir))
	       (eq type :directory)))
      (delete-dir dir))))

(deftest probe-file (t probe-file-21)
  "Test `probe-file' on a \"typed\" directory with an absolute pathname."
  (let ((dir (pick-new-dir "/tmp/tmp-~D-~D.lisp/")))
    (unwind-protect
	(multiple-value-bind (pathname type)
			     (probe-file dir)
	  (and (string= (namestring pathname) (namestring dir))
	       (eq type :directory)))
      (delete-dir dir))))

(deftest probe-file ((values () ()) probe-file-22)
  "Test `probe-file' on a missing directory with an absolute pathname."
  (let ((dir (pick-new-dir)))
    (delete-dir dir)
    (probe-file dir)))


;;;; Directory, relative pathname.

(deftest probe-file (t probe-file-30)
  "Test `probe-file' on a directory with a relative pathname."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (multiple-value-bind (pathname type)
			       (probe-file ".")
	    (and (string= (namestring pathname) (namestring dir))
		 (eq type :directory))))
      (delete-dir dir))))

(deftest probe-file (t probe-file-31)
  "Test `probe-file' on a \"typed\" directory with a relative pathname."
  (let ((dir (pick-new-dir "/tmp/tmp-~D-~D.lisp/")))
    (unwind-protect
	(in-directory (namify dir)
	  (multiple-value-bind (pathname type)
			       (probe-file (file-namestring (namify dir)))
	    (and (string= (namestring pathname) (namestring (namify dir)))
		 (eq type :directory))))
      (delete-dir dir))))

(deftest probe-file ((values () ()) probe-file-32)
  "Test `probe-file' on a missing directory with an relative pathname."
  (let ((dir (pick-new-dir)))
    (delete-dir dir)
    (in-directory (namify dir)
      (probe-file (file-namestring (namify dir))))))


;;;; Symlink to file, absolute pathname.

(deftest probe-file (t probe-file-40)
  "Test `probe-file' on an absolute symlink to a file using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (probe-file link)
	    (and (string= (namestring pathname) (namestring file))
		 ;; Follow links.
		 (eq type :file))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-41)
  "Test `probe-file' on an absolute symlink to a file using an absolute
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (probe-file link :check-for-links t)
	    (and (string= (namestring pathname) (namestring file))
		 ;; Check for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-42)
  "Test `probe-file' on a relative symlink to a file using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (probe-file (merge-pathnames "l" dir))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Follow links.
		 (eq type :file))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-43)
  "Test `probe-file' on a relative symlink to a file using an absolute
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (probe-file (merge-pathnames "l" dir))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Checking for links.
		 (eq type :file))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to file, relative pathname.

(deftest probe-file (t probe-file-50)
  "Test `probe-file' on an absolute symlink to a file using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l"))
	      (and (string= (namestring pathname) (namestring file))
		   ;; Follow links.
		   (eq type :file))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-51)
  "Test `probe-file' on a relative symlink to a file using a relative
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l" :check-for-links t))
	    (and (string= (namestring pathname) (namestring file))
		 ;; Check for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-52)
  "Test `probe-file' on a relative symlink to a file using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l"))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Follow links.
		 (eq type :file))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-53)
  "Test `probe-file' on a relative symlink to a file using a relative
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l" :check-for-links t))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Checking for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to directory, absolute pathname.

(deftest probe-file (t probe-file-60)
  "Test `probe-file' on an absolute symlink to a directory using an
   absolute pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (probe-file link)
	    (and (string= (namestring pathname) (namestring file))
		 ;; Follow links.
		 (eq type :directory))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-61)
  "Test `probe-file' on an absolute symlink to a directory using an
   absolute pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (probe-file link :check-for-links t)
	    (and (string= (namestring pathname) (namestring file))
		 ;; Check for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-62)
  "Test `probe-file' on a relative symlink to a directory using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (probe-file (merge-pathnames "l" dir))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Follow links.
		 (eq type :directory))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-63)
  "Test `probe-file' on a relative symlink to a directory using an absolute
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (probe-file (merge-pathnames "l" dir)
					   :check-for-links t)
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Checking for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to directory, relative pathname.

(deftest probe-file (t probe-file-70)
  "Test `probe-file' on a absolute symlink to a file using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l"))
	      (and (string= (namestring pathname) (namestring file))
		   ;; Follow links.
		   (eq type :directory))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-71)
  "Test `probe-file' on a relative symlink to a directory using a relative
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l" :check-for-links t))
	    (and (string= (namestring pathname) (namestring file))
		 ;; Check for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-72)
  "Test `probe-file' on a relative symlink to a directory using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l"))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Follow links.
		 (eq type :directory))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-73)
  "Test `probe-file' on a relative symlink to a directory using a relative
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "l" "a"))
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l" :check-for-links t))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Checking for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to symlink, absolute pathname.

(deftest probe-file (t probe-file-80)
  "Test `probe-file' on an absolute symlink to a symlink using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (dest (merge-pathnames "d" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file dest file)
	  (symlink-file link dest)
	  (multiple-value-bind (pathname type)
			       (probe-file link)
	    (and (string= (namestring pathname) (namestring file))
		 ;; Follow links.
		 (eq type :file))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-81)
  "Test `probe-file' on an absolute symlink to a symlink using an
   absolute pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (dest (merge-pathnames "d" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file dest file)
	  (symlink-file link dest)
	  (multiple-value-bind (pathname type)
			       (probe-file link :check-for-links t)
	    (and (string= (namestring pathname) (namestring file))
		 ;; Check for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-82)
  "Test `probe-file' on a relative symlink to a symlink using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "d" "a")
	    (symlink-file "l" "d"))
	  (multiple-value-bind (pathname type)
			       (probe-file (merge-pathnames "l" dir))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Follow links.
		 (eq type :directory))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-83)
  "Test `probe-file' on a relative symlink to a symlink using an absolute
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "d" "a")
	    (symlink-file "l" "d"))
	  (multiple-value-bind (pathname type)
			       (probe-file (merge-pathnames "l" dir)
					   :check-for-links t)
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Checking for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to symlink, relative pathname.

(deftest probe-file (t probe-file-90)
  "Test `probe-file' on a absolute symlink to a symlink using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l"))
	      (and (string= (namestring pathname) (namestring file))
		   ;; Follow links.
		   (eq type :directory))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-91)
  "Test `probe-file' on a relative symlink to a directory using a relative
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (dest (merge-pathnames "d" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file dest file)
	  (symlink-file link dest)
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l" :check-for-links t))
	    (and (string= (namestring pathname) (namestring file))
		 ;; Check for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-92)
  "Test `probe-file' on a relative symlink to a symlink using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "d" "a")
	    (symlink-file "l" "d"))
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l"))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Follow links.
		 (eq type :file))))
      (dired:delete-file dir :recurse t))))

(deftest probe-file (t probe-file-93)
  "Test `probe-file' on a relative symlink to a file using a relative
   pathname, checking for links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "d" "a")
	    (symlink-file "l" "d"))
	  (multiple-value-bind (pathname type)
			       (in-directory dir
				 (probe-file "l" :check-for-links t))
	    (and (string= (namestring pathname)
			  (namestring (merge-pathnames "a" dir)))
		 ;; Checking for links.
		 (eq type :link))))
      (dired:delete-file dir :recurse t))))


;;;; Wildcard.

(deftest probe-file (t probe-file-100)
  "Test `probe-file' with a file wildcard."
  (let (ret)
    (handler-case
	(probe-file "*.lisp")
      (file-error () (setq ret t)))
    ret))

(deftest probe-file (t probe-file-101)
  "Test `probe-file' with a type wildcard."
  (let (ret)
    (handler-case
	(probe-file "a.*")
      (file-error () (setq ret t)))
    ret))

(deftest probe-file (t probe-file-102)
  "Test `probe-file' with a file and a type wildcard."
  (let (ret)
    (handler-case
	(probe-file "*.*")
      (file-error () (setq ret t)))
    ret))

(deftest probe-file (t probe-file-103)
  "Test `probe-file' with a directory wildcard."
  (let (ret)
    (handler-case
	(probe-file "/*/a.lisp")
      (file-error () (setq ret t)))
    ret))

(deftest probe-file (t probe-file-104)
  "Test `probe-file' with directory, file and type wildcards."
  (let (ret)
    (handler-case
	(probe-file "/*/a.lisp")
      (file-error () (setq ret t)))
    ret))


;;;; Device.

(deftest probe-file ('(#p"/dev/null" :special) probe-file-110)
  "Test `probe-file' with a device."
  (multiple-value-list (probe-file "/dev/null")))
