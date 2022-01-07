;;; Tests of lisp:copy-file.

(in-package "LISP")

(import '(deftest:deftest deftest:random-string deftest:random-pathname))


;;;; Files.

(deftest copy-file (t copy-file-1)
  "Test `copy-file' with a randomly sized file."
  (let ((file (pick-new-file))
	(copy (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file copy)
	  (with-open-file (stream file :direction :output)
	    (write-string (random-string) stream))
	  (copy-file file copy)
	  (ed::compare-files file copy))
      (ignore-errors (delete-file copy))
      (delete-file file))))

(deftest copy-file (t copy-file-2)
  "Test `copy-file' by copying a randomly sized file to a subdir."
  (let ((file (pick-new-file))
	(dir (pick-new-dir)))
    (unwind-protect
	(let ((copy (merge-pathnames (random-pathname) dir)))
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(copy-file file copy)
		(ed::compare-files file copy))
	    (ignore-errors (delete-file copy))))
      (delete-file file)
      (delete-dir dir))))

(deftest copy-file (t copy-file-3)
  "Test `copy-file' by copying a randomly sized file from a subdir."
  (let ((copy (pick-new-file))
	(dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames (random-pathname) dir)))
	  (delete-file copy)
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(copy-file file copy)
		(ed::compare-files file copy))
	    (ignore-errors (delete-file file))))
      (ignore-errors (delete-file copy))
      (delete-dir dir))))

(deftest copy-file (t copy-file-4)
  "Test `copy-file' by copying a randomly sized file from a directory to
   another directory."
  (let ((copy-dir (pick-new-dir))
	(file-dir (pick-new-dir)))
    (unwind-protect
	(let ((file (merge-pathnames (random-pathname) file-dir))
	      (copy (merge-pathnames (random-pathname) copy-dir)))
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(copy-file file copy)
		(ed::compare-files file copy))
	    (ignore-errors (delete-file copy))
	    (ignore-errors (delete-file file))))
      (delete-dir file-dir)
      (delete-dir copy-dir))))

(deftest copy-file (t copy-file-5)
  "Test `copy-file' by copying a randomly sized file from a subdir to a
   parent subdir."
  (let ((copy-dir (pick-new-dir)))
    (unwind-protect
	(let* ((current-dir (random-pathname))
	       (file-dir (concat current-dir "/" (random-pathname) "/"))
	       (file (concat file-dir "/" (random-pathname)))
	       (copy (concat copy-dir "/" (random-pathname))))
	  (unwind-protect
	      (in-directory current-dir
		(ensure-directories-exist file-dir)
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(copy-file file copy)
		(ed::compare-files file copy))
	    (ignore-errors (delete-file copy))
	    (ignore-errors (delete-file file))
	    (ignore-errors (delete-dir file-dir))
	    (ignore-errors (delete-dir current-dir))))
      (delete-dir copy-dir))))


;;;; Symlinks.

(deftest copy-file (t copy-file-6)
  "Test `copy-file' with a randomly sized file via an absolute symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(copy (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file copy)
	  (delete-file link)
	  (with-open-file (stream file :direction :output)
	    (write-string (random-string) stream))
	  (symlink-file link file)
	  (copy-file link copy :check-for-links t)
	  (and (symlinkp copy)
	       (ed::compare-files link copy)))
      (delete-file link)
      (delete-file copy)
      (delete-file file))))

(deftest copy-file (t copy-file-7)
  "Test `copy-file' with a randomly sized file via a relative symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(copy (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file copy)
	  (delete-file link)
	  (with-open-file (stream file :direction :output)
	    (write-string (random-string) stream))
	  (in-directory (directory-namestring link)
	    (symlink-file (file-namestring link)
			  (file-namestring file)))
	  (copy-file link copy :check-for-links t)
	  (and (symlinkp copy)
	       ;; The link destination is in the same directory as copy, so
	       ;; the copy still points to the destination.
	       (ed::compare-files link copy)))
      (delete-file link)
      (ignore-errors (delete-file copy))
      (delete-file file))))

(deftest copy-file (t copy-file-8)
  "Test `copy-file' with an absolute symlink to a randomly sized file."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(copy (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file copy)
	  (delete-file link)
	  (with-open-file (stream file :direction :output)
	    (write-string (random-string) stream))
	  (symlink-file link file)
	  (copy-file link copy)
	  (and (eq (file-kind copy) :file)
	       (ed::compare-files file copy)))
      (delete-file link)
      (ignore-errors (delete-file copy))
      (delete-file file))))

(deftest copy-file (t copy-file-9)
  "Test `copy-file' by copying to a subdir a randomly sized file, via an
   absolute symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(dir (pick-new-dir)))
    (unwind-protect
	(let ((copy (merge-pathnames (random-pathname) dir)))
	  (delete-file link)
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(symlink-file link file)
		(copy-file link copy)
		(and (eq (file-kind copy) :file)
		     (ed::compare-files file copy)))
	    (ignore-errors (delete-file link))
	    (ignore-errors (delete-file copy))))
      (delete-file file)
      (delete-dir dir))))

(deftest copy-file (t copy-file-10)
  "Test `copy-file' by copying to a subdir a randomly sized file, via a
   relative symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(dir (pick-new-dir)))
    (unwind-protect
	(let ((copy (random-pathname)))
	  (delete-file link)
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(symlink-file link file)
		(in-directory dir
		  (copy-file link copy)
		  (and (eq (file-kind copy) :file)
		       (ed::compare-files file copy))))
	    (ignore-errors (delete-file link))
	    (ignore-errors (delete-file (merge-pathnames copy dir)))))
      (delete-file file)
      (delete-dir dir))))

(deftest copy-file (t copy-file-11)
  "Test `copy-file' by copying to a subdir an absolute symlink to a
   randomly sized file."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(dir (pick-new-dir)))
    (unwind-protect
	(let ((copy (merge-pathnames (random-pathname) dir)))
	  (delete-file link)
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(symlink-file link file)
		(copy-file link copy :check-for-links t)
		(and (symlinkp copy)
		     (ed::compare-files link copy)))
	    (ignore-errors (delete-file link))
	    (ignore-errors (delete-file copy))))
      (delete-file file)
      (delete-dir dir))))

(deftest copy-file (t copy-file-12)
  "Test `copy-file' by copying to a subdir a relative symlink to a randomly
   sized file."
  (let ((file (pick-new-file))
	(link (pick-new-file))
	(dir (pick-new-dir)))
    (unwind-protect
	(let ((copy (merge-pathnames (random-pathname) dir)))
	  (delete-file link)
	  (unwind-protect
	      (progn
		(with-open-file (stream file :direction :output)
		  (write-string (random-string) stream))
		(in-directory (directory-namestring file)
		  (symlink-file (file-namestring link)
				(file-namestring file)))
		(copy-file link copy :check-for-links t)
		(and (symlinkp copy)
		     (in-directory copy
		       (fi (probe-file (symlink-dest copy))))))
	    (ignore-errors (delete-file link))
	    (ignore-errors (delete-file (merge-pathnames copy dir)))))
      (delete-file file)
      (delete-dir dir))))

(deftest copy-file (t copy-file-13)
  "Test `copy-file' by copying a relative file symlink from a subdir to
   another subdir."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/")
	  (ensure-directories-exist "b/")
	  (in-directory "a/"
	    (touch-file "f")
	    (symlink-file "l" "f"))
	  (in-directory "b/"
	    (with-open-file (stream "f" :direction :output)
	      (write-string deftest:test-string-1 stream)))
	  (copy-file "a/l" "b/")
	  (ed::compare-files "b/l" "a/f"))
      (let ((dired:*report-function* #'t))
	(dired:delete-file dir :recurse t)))))

(deftest copy-file (t copy-file-14)
  "Test `copy-file' by copying a relative file symlink from a subdir to
   another subdir, preserving links."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/")
	  (ensure-directories-exist "b/")
	  (in-directory "a/"
	    (touch-file "f")
	    (symlink-file "l" "f"))
	  (in-directory "b/"
	    (with-open-file (stream "f" :direction :output)
	      (write-string deftest:test-string-1 stream)))
	  (copy-file "a/l" "b/" :check-for-links t)
	  (ed::compare-files "b/l" "b/f"))
      (let ((dired:*report-function* #'t))
	(dired:delete-file dir :recurse t)))))

(deftest copy-file (t copy-file-15)
  "Test `copy-file' by copying a relative file symlink from a subdir to
   another subdir."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(in-directory dir
	  (ensure-directories-exist "a/d/")
	  (ensure-directories-exist "b/d/")
	  (in-directory "a/"
	    (symlink-file "l" "d/"))
	  (copy-file "a/l" "b/" :check-for-links t)
	  (ed::compare-files "b/l" "b/d/"))
      (let ((dired:*report-function* #'t))
	(dired:delete-file dir :recurse t)))))


;;;; FIX Destination a directory.


;;;; FIX Destination merged with source.

;(rename "a/b" "a/b.c")
;(rename "a/b.c" "a/b")
;(rename "a/sub/b.c" "a/b")


;;;; Error cases.

;; FIX source a dir
;; FIX source names a dir
