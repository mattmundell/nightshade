;;; Tests of lisp:symlink-file.

(in-package "LISP")

(import '(deftest:deftest))

(deftest symlink-file (t symlink-file-0)
  "Test `symlink-file' with an empty directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link dir)
	  (unwind-protect
	      (equal (probe-file link) (truename dir))
	    (delete-file link)))
      (delete-dir dir))))

(defconstant symlink-file-1-string "abcde")

(deftest symlink-file (t symlink-file-1)
  "Test `symlink-file' with a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output)
	    (write-string symlink-file-1-string stream))
	  (delete-file link)
	  (symlink-file link file)
	  (unwind-protect
	      (and (equal (probe-file link) (truename file))
		   (equal (file-size file)
			  (length symlink-file-1-string)))
	    (delete-file link)))
      (delete-file file))))

(deftest symlink-file (t symlink-file-2)
  "Test `symlink-file' where the destination is a directory in a
   subdirectory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (when (ensure-directories-exist "sub/subsub/")
	    (unwind-protect
		(progn
		  (symlink-file "link" "sub/subsub")
		  (unwind-protect
		      (equal (probe-file "link") (truename "sub/subsub"))
		    (delete-file "link")))
	      (delete-dir "sub/subsub/")
	      (delete-dir "sub/"))))
      (delete-dir dir))))

(deftest symlink-file (t symlink-file-3)
  "Test `symlink-file' where the link is in one subdir and the destination
   is a directory in another."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (when (ensure-directories-exist "sub2/subsub/")
	    (unwind-protect
		(when (ensure-directories-exist "sub1/")
		  (unwind-protect
		      (progn
			(symlink-file "sub1/link" "sub2/subsub/")
			(unwind-protect
			    (equal (probe-file "sub1/link")
				   (truename "sub2/subsub/"))
			  (delete-file "sub1/link")))
		    (delete-dir "sub1/")))
	      (delete-dir "sub2/subsub/")
	      (delete-dir "sub2/"))))
      (delete-dir dir))))

(deftest symlink-file (t symlink-file-4)
  "Test `symlink-file' where the link is in a subdir and the destination is
   a file in the parent."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (when (ensure-directories-exist "parent/current/sub/")
	    (unwind-protect
		(progn
		  (touch-file "parent/dest")
		  (in-directory "parent/current/"
		    (symlink-file "sub/link" "../dest")
		    (unwind-protect
			(equal (probe-file "sub/link")
			       (truename "../dest"))
		      (delete-file "sub/link"))))
	      (if (probe-file "parent/dest") (delete-file "parent/dest"))
	      (delete-dir "parent/current/sub/")
	      (delete-dir "parent/current/")
	      (delete-dir "parent/"))))
      (delete-dir dir))))

(deftest symlink-file (t symlink-file-5)
  "Test `symlink-file' where the destination is a file in a subdir and the
   link is in the parent."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (when (ensure-directories-exist "parent/current/sub/")
	    (unwind-protect
		(progn
		  (touch-file "parent/current/sub/dest")
		  (in-directory "parent/current/"
		    (symlink-file "../link" "sub/dest")
		    (unwind-protect
			(equal (probe-file "../link")
			       (truename "sub/dest"))
		      (delete-file "../link"))))
	      (if (probe-file "parent/current/sub/dest")
		  (delete-file "parent/current/sub/dest"))
	      (delete-dir "parent/current/sub/")
	      (delete-dir "parent/current/")
	      (delete-dir "parent/"))))
      (delete-dir dir))))

;; FIX test absolute pn's.


;;;; Error cases.

;;; FIX
