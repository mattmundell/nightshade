;;; Tests of lisp:rename-file.

(in-package "LISP")

(import '(deftest:deftest deftest:random-string deftest:random-pathname))


;;;; Files.

(deftest rename-file (deftest:test-string-1 rename-file-1)
  "Test `rename-file' with a file of text."
  (let ((file (pick-new-file))
	(new (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file new)
	  (with-open-file (stream file :direction :output)
	    (write-string deftest:test-string-1 stream))
	  (rename-file file new)
	  (fi (probe-file file)
	      (with-output-to-string (out)
		(with-open-file (in new)
		  (transfer in out)))))
      (delete-file new))))

(deftest rename-file (deftest:test-string-2 rename-file-2)
  "Test `rename-file' by renaming a file of text into a subdir."
  (let* ((file (pick-new-file))
	 (dir (pick-new-dir))
	 (new (merge-pathnames (random-pathname) dir)))
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output)
	    (write-string deftest:test-string-2 stream))
	  (rename-file file new)
	  (fi (probe-file file)
	      (with-output-to-string (out)
		(with-open-file (in new)
		  (transfer in out)))))
      (delete-file new))))

(deftest rename-file (deftest:test-string-3 rename-file-3)
  "Test `rename-file' by renaming a file of text from a subdir."
  (let* ((new (pick-new-file))
	 (dir (pick-new-dir))
	 (file (merge-pathnames "a" dir)))
    (unwind-protect
	(progn
	  (delete-file new)
	  (with-open-file (stream file :direction :output)
	    (write-string deftest:test-string-3 stream))
	  (rename-file file new)
	  (fi (probe-file file)
	      (with-output-to-string (out)
		(with-open-file (in new)
		  (transfer in out)))))
      (delete-file new))))

(deftest rename-file (deftest:test-string-4 rename-file-4)
  "Test `rename-file' by renaming a file of text from a directory to
   another directory."
  (let* ((file-dir (pick-new-dir))
	 (new-dir (pick-new-dir))
	 (file (merge-pathnames "a" file-dir))
	 (new (merge-pathnames "b" new-dir)))
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output)
	    (write-string deftest:test-string-4 stream))
	  (rename-file file new)
	  (fi (probe-file file)
	      (with-output-to-string (out)
		(with-open-file (in new)
		  (transfer in out)))))
      (delete-file new))))

(deftest rename-file (deftest:test-string-3 rename-file-5)
  "Test `rename-file' by copying a file of text from a subdir to a parent
   directory."
  (let* ((new-dir (pick-new-dir)))
    (unwind-protect
	(in-directory new-dir
	  (ensure-directories-exist "dir/subdir/")
	  (in-directory "dir/"
	    (in-directory "subdir/"
	      (with-open-file (stream "a" :direction :output)
		(write-string deftest:test-string-3 stream)))
	    (rename-file "subdir/a" "../b"))
	  (fi (probe-file "dir/subdir/a")
	      (with-output-to-string (out)
		(with-open-file (in "b")
		  (transfer in out)))))
      (dired:delete-file new-dir :recurse t))))


;;;; Symlinks.

(deftest rename-file (deftest:test-string-2 rename-file-6)
  "Test `rename-file' on a file of text via an absolute symlink."
  (let* ((dir (pick-new-dir))
	 (file (merge-pathnames "a" dir))
	 (link (merge-pathnames "l" dir))
	 (new (merge-pathnames "b" dir)))
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-2 stream))
	  (symlink-file link file)
	  (rename-file link new)
	  (fi (and (probe-file file) (symlinkp new))
	      (and
	       ;; Link destination must have been adjusted.
	       (pathname= (truename link) (truename new))
	       (with-output-to-string (out)
		 (with-open-file (in new)
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-2 rename-file-7)
  "Test `rename-file' on a file of text via a relative symlink."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (with-open-file (stream "a" :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-2 stream))
	  (symlink-file "l" "a")
	  (rename-file "l" "b")
	  (fi (and (probe-file "a") (symlinkp "b"))
	      (and
	       ;; Link destination must have been adjusted.
	       (pathname= (truename "l") (truename "b"))
	       (with-output-to-string (out)
		 (with-open-file (in "b")
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-3 rename-file-8)
  "Test `rename-file' on an absolute symlink to a file of text."
  (let* ((dir (pick-new-dir))
	 (file (merge-pathnames "a" dir))
	 (link (merge-pathnames "l" dir))
	 (new (merge-pathnames "b" dir)))
    (unwind-protect
	(progn
	  (with-open-file (stream file :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-3 stream))
	  (symlink-file link file)
	  (rename-file link new :check-for-links t)
	  (fi (probe-file link)
	      (and (symlinkp new)
		   ;; Destination must be as before `rename-file'.
		   (pathname= (truename (symlink-dest new))
			      (truename file))
		   (with-output-to-string (out)
		     (with-open-file (in new)
		       (transfer in out))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-1 rename-file-9)
  "Test `rename-file' by renaming a file of text with a new file name into
   a subdir, via an absolute symlink."
  (let* ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir
	    (ensure-directories-exist "subdir/"))
	  (let ((file (merge-pathnames "a" dir))
		(link (merge-pathnames "l" dir))
		(new (merge-pathnames "subdir/b" dir)))
	    (with-open-file (stream file :direction :output
				    :if-does-not-exist :create)
	      (write-string deftest:test-string-1 stream))
	    (symlink-file link file)
	    (rename-file link new)
	    (fi (probe-file file)
		(and
		 (eq (file-kind new) :file)
		 ;; Link destination must have been adjusted.
		 (pathname= (truename link) (truename new))
		 (with-output-to-string (out)
		   (with-open-file (in new)
		     (transfer in out)))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-2 rename-file-10)
  "Test `rename-file' by renaming a file of text with a new file name into
   a subdir, via a relative symlink."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "subdir/")
	  (with-open-file (stream "a" :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-2 stream))
	  (symlink-file "l" "a")
	  (rename-file "l" "subdir/b")
	  (fi (probe-file "a")
	      (and
	       (eq (file-kind "subdir/b") :file)
	       ;; Link destination must have been adjusted.
	       (pathname= (truename "l") (truename "subdir/b"))
	       (with-output-to-string (out)
		 (with-open-file (in "subdir/b")
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-1 rename-file-11)
  "Test `rename-file' by renaming into a subdir an absolute symlink, with a
   new file name, to a file of text."
  (let* ((dir (pick-new-dir)))
    (unwind-protect
	(progn
	  (in-directory dir (ensure-directories-exist "subdir/"))
	  (let ((file (merge-pathnames "a" dir))
		(link (merge-pathnames "l" dir))
		(new (merge-pathnames "subdir/l2" dir)))
	    (with-open-file (stream file :direction :output
				    :if-does-not-exist :create)
	      (write-string deftest:test-string-1 stream))
	    (symlink-file link file)
	    (rename-file link new :check-for-links t)
	    (fi (probe-file link)
		(and
		 (eq (file-kind new :check-for-links t) :link)
		 ;; Link destination must have been adjusted.
		 (pathname= (truename file) (truename new))
		 (with-output-to-string (out)
		   (with-open-file (in new)
		     (transfer in out)))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-4 rename-file-12)
  "Test `rename-file' by renaming a relative file symlink from a subdir,
   with a file name, to another subdir."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "subdir/")
	  (with-open-file (stream "a" :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-4 stream))
	  (symlink-file "l" "a")
	  (rename-file "l" "subdir/b" :check-for-links t)
	  (fi (or (probe-file "l")
		  ;; The relative link is broken by the `rename-file'.
		  (probe-file "subdir/b"))
	      (and
	       (eq (file-kind "subdir/b" :check-for-links t) :link)
	       ;; "a" must stay intact.
	       (with-output-to-string (out)
		 (with-open-file (in "a")
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-4 rename-file-13)
  "Test `rename-file' by renaming a relative file symlink from a subdir,
   with a new file name, to another subdir."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "subdir0/")
	  (ensure-directories-exist "subdir/")
	  (with-open-file (stream "subdir0/a" :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-4 stream))
	  (in-directory "subdir0/"
	    (symlink-file "l" "a"))
	  (rename-file "subdir0/l" "subdir/b" :check-for-links t)
	  (fi (or (probe-file "l")
		  ;; The relative link is broken by the `rename-file'.
		  (probe-file "subdir/b"))
	      (and
	       (eq (file-kind "subdir/b" :check-for-links t) :link)
	       ;; "a" must stay intact.
	       (with-output-to-string (out)
		 (with-open-file (in "subdir0/a")
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))

(deftest rename-file (deftest:test-string-4 rename-file-14)
  "Test `rename-file' by renaming a relative file symlink from a subdir,
   with a new file name, to another subdir, preserving links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "subdir0/")
	  (ensure-directories-exist "subdir/")
	  (with-open-file (stream "subdir0/a" :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-4 stream))
	  (in-directory "subdir0/"
	    (symlink-file "l" "a"))
	  (rename-file "subdir0/l" "subdir/b")
	  (fi (probe-file "subdir0/a")
	      (and
	       (eq (file-kind "subdir0/l" :check-for-links t) :link)
	       (eq (file-kind "subdir/b" :check-for-links t) :file)
	       (with-output-to-string (out)
		 (with-open-file (in "subdir/b")
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))


;;;; Destination a directory.

(deftest rename-file (deftest:test-string-4 rename-file-15)
  "Test `rename-file' by renaming a relative file symlink from a subdir
   into another subdir."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "subdir0/")
	  (ensure-directories-exist "subdir/")
	  (with-open-file (stream "subdir0/a" :direction :output
				  :if-does-not-exist :create)
	    (write-string deftest:test-string-4 stream))
	  (in-directory "subdir0/"
	    (symlink-file "l" "a"))
	  (rename-file "subdir0/l" "subdir/")
	  (fi (probe-file "subdir0/a")
	      (and
	       (eq (file-kind "subdir0/l" :check-for-links t) :link)
	       (eq (file-kind "subdir/a" :check-for-links t) :file)
	       (with-output-to-string (out)
		 (with-open-file (in "subdir/a")
		   (transfer in out))))))
      (dired:delete-file dir :recurse t))))


;;;; FIX Destination merged with source.

;(rename "a/b" "a/b.c")
;(rename "a/b.c" "a/b")
;(rename "a/sub/b.c" "a/b")


;;;; Error cases.

;; FIX source a dir
;; FIX source names a dir
