;;; Tests of lisp:truename.
;;
;; Quite similar to probe-file.lisp.
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

(deftest truename (t truename-1)
  "Test `truename' on a file with an absolute pathname."
  (let ((file (pick-new-file)))
    (unwind-protect
	(string= (namestring (truename file)) (namestring file))
      (delete-file file))))

(deftest truename (t truename-2)
  "Test `truename' on a typed file with an absolute pathname."
  (let ((file (pick-new-file "/tmp/tmp-~D-~D.lisp")))
    (unwind-protect
	(string= (namestring (truename file)) (namestring file))
      (delete-file file))))

(deftest truename (t truename-3)
  "Test `truename' on a missing file with an absolute pathname."
  (let ((file (pick-new-file))
	ret)
    (delete-file file)
    (handler-case
	(truename file)
      (file-error () (setq ret t)))
    ret))


;;;; File, relative pathname.

(deftest truename (t truename-10)
  "Test `truename' on a file with a relative pathname."
  (let ((file (pick-new-file)))
    (unwind-protect
	(in-directory file
	  (string= (namestring (truename (file-namestring file)))
		   (namestring file)))
      (delete-file file))))

(deftest truename (t truename-11)
  "Test `truename' on a typed file with a relative pathname."
  (let ((file (pick-new-file "/tmp/tmp-~D-~D.lisp")))
    (unwind-protect
	(in-directory file
	  (string= (namestring (truename (file-namestring file)))
		   (namestring file)))
      (delete-file file))))

(deftest truename (t truename-12)
  "Test `truename' on a missing file with an relative pathname."
  (let ((file (pick-new-file))
	ret)
    (delete-file file)
    (in-directory file
      (handler-case
	  (truename (file-namestring file))
	(file-error () (setq ret t))))))


;;;; Directory, absolute pathname.

(deftest truename (t truename-20)
  "Test `truename' on a directory with an absolute pathname."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(string= (namestring (truename dir))
		 (namestring dir))
      (delete-dir dir))))

(deftest truename (t truename-21)
  "Test `truename' on a \"typed\" directory with an absolute pathname."
  (let ((dir (pick-new-dir "/tmp/tmp-~D-~D.lisp/")))
    (unwind-protect
	(string= (namestring (truename dir)) (namestring dir))
      (delete-dir dir))))

(deftest truename (t truename-22)
  "Test `truename' on a missing directory with an absolute pathname."
  (let ((dir (pick-new-dir))
	ret)
    (delete-dir dir)
    (handler-case
	(truename dir)
      (file-error () (setq ret t)))))


;;;; Directory, relative pathname.

(deftest truename (t truename-30)
  "Test `truename' on a directory with a relative pathname."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (string= (namestring (truename ".")) (namestring dir)))
      (delete-dir dir))))

(deftest truename (t truename-31)
  "Test `truename' on a \"typed\" directory with a relative pathname."
  (let ((dir (pick-new-dir "/tmp/tmp-~D-~D.lisp/")))
    (unwind-protect
	(in-directory (namify dir)
	  (string= (namestring (truename (file-namestring (namify dir))))
		   (namestring (namify dir))))
      (delete-dir dir))))

(deftest truename (t truename-32)
  "Test `truename' on a missing directory with an relative pathname."
  (let ((dir (pick-new-dir))
	ret)
    (delete-dir dir)
    (in-directory (namify dir)
      (handler-case
	  (truename (file-namestring (namify dir)))
	(file-error () (setq ret t))))))


;;;; Symlink to file, absolute pathname.

(deftest truename (t truename-40)
  "Test `truename' on an absolute symlink to a file using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file link file)
	  (string= (namestring (truename link)) (namestring file)))
      (dired:delete-file dir :recurse t))))

(deftest truename (t truename-42)
  "Test `truename' on a relative symlink to a file using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "l" "a"))
	  (string= (namestring (truename (merge-pathnames "l" dir)))
		   (namestring (merge-pathnames "a" dir))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to file, relative pathname.

(deftest truename (t truename-50)
  "Test `truename' on an absolute symlink to a file using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file link file)
	  (string= (namestring (in-directory dir (truename "l")))
		   (namestring file)))
      (dired:delete-file dir :recurse t))))

(deftest truename (t truename-52)
  "Test `truename' on a relative symlink to a file using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "l" "a"))
	  (string= (namestring (in-directory dir (truename "l")))
		   (namestring (merge-pathnames "a" dir))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to directory, absolute pathname.

(deftest truename (t truename-60)
  "Test `truename' on an absolute symlink to a directory using an
   absolute pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (string= (namestring (truename link)) (namestring file)))
      (dired:delete-file dir :recurse t))))

(deftest truename (t truename-62)
  "Test `truename' on a relative symlink to a directory using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "l" "a"))
	  (string= (namestring (truename (merge-pathnames "l" dir)))
		   (namestring (merge-pathnames "a" dir))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to directory, relative pathname.

(deftest truename (t truename-70)
  "Test `truename' on a absolute symlink to a file using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (string= (namestring (in-directory dir (truename "l")))
		   (namestring file)))
      (dired:delete-file dir :recurse t))))

(deftest truename (t truename-72)
  "Test `truename' on a relative symlink to a directory using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "l" "a"))
	  (string= (namestring (in-directory dir (truename "l")))
		   (namestring (merge-pathnames "a" dir))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to symlink, absolute pathname.

(deftest truename (t truename-80)
  "Test `truename' on an absolute symlink to a symlink using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a" dir))
	      (dest (merge-pathnames "d" dir))
	      (link (merge-pathnames "l" dir)))
	  (touch-file file)
	  (symlink-file dest file)
	  (symlink-file link dest)
	  (string= (namestring (truename link))
		   (namestring file)))
      (dired:delete-file dir :recurse t))))

(deftest truename (t truename-82)
  "Test `truename' on a relative symlink to a symlink using an absolute
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "a/")
	    (symlink-file "d" "a")
	    (symlink-file "l" "d"))
	  (string= (namestring (truename (merge-pathnames "l" dir)))
		   (namestring (merge-pathnames "a" dir))))
      (dired:delete-file dir :recurse t))))


;;;; Symlink to symlink, relative pathname.

(deftest truename (t truename-90)
  "Test `truename' on a absolute symlink to a symlink using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames "a/" dir))
	      (link (merge-pathnames "l" dir)))
	  (ensure-directories-exist file)
	  (symlink-file link file)
	  (string= (namestring (in-directory dir (truename "l")))
		   (namestring file)))
      (dired:delete-file dir :recurse t))))

(deftest truename (t truename-92)
  "Test `truename' on a relative symlink to a symlink using a relative
   pathname, following links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (touch-file "a")
	    (symlink-file "d" "a")
	    (symlink-file "l" "d"))
	  (string= (namestring (in-directory dir (truename "l")))
		   (namestring (merge-pathnames "a" dir))))
      (dired:delete-file dir :recurse t))))


;;;; Wildcard.

(deftest truename (t truename-100)
  "Test `truename' with a file wildcard."
  (let (ret)
    (handler-case
	(truename "*.lisp")
      (file-error () (setq ret t)))
    ret))

(deftest truename (t truename-101)
  "Test `truename' with a type wildcard."
  (let (ret)
    (handler-case
	(truename "a.*")
      (file-error () (setq ret t)))
    ret))

(deftest truename (t truename-102)
  "Test `truename' with a file and a type wildcard."
  (let (ret)
    (handler-case
	(truename "*.*")
      (file-error () (setq ret t)))
    ret))

(deftest truename (t truename-103)
  "Test `truename' with a directory wildcard."
  (let (ret)
    (handler-case
	(truename "/*/a.lisp")
      (file-error () (setq ret t)))
    ret))

(deftest truename (t truename-104)
  "Test `truename' with directory, file and type wildcards."
  (let (ret)
    (handler-case
	(truename "/*/a.lisp")
      (file-error () (setq ret t)))
    ret))


;;;; Device.

(deftest truename (#p"/dev/null" truename-110)
  "Test `truename' with a device."
  (truename "/dev/null"))
