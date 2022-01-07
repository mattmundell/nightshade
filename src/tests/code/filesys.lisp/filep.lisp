;;; Tests of lisp:filep.

(in-package "LISP")

(import '(deftest:deftest))

(deftest filep (() filep-0)
  "Test `filep' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(filep dir)
      (delete-dir dir))))

(deftest filep (t filep-1)
  "Test `filep' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(filep file)
      (delete-file file))))

(deftest filep (t filep-2)
  "Test `filep' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (and (filep file)
	       (filep link)
	       (filep file :check-for-links ())
	       (filep link :check-for-links ())
	       (filep file :check-for-links t)
	       (filep link :check-for-links t)))
      (delete-file file)
      (delete-file link))))

(deftest filep (() filep-3)
  "Test `filep' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (or (filep dir)
	      (filep link)
	      (filep dir :check-for-links ())
	      (filep link :check-for-links ())
	      (filep dir :check-for-links t)
	      (fi (filep link :check-for-links t))))
      (delete-dir dir)
      (delete-file link))))

(deftest filep (() filep-4)
  "Test `filep' with a broken symlink."
  (let ((file (pick-new-file))
	(file2 (pick-new-file)))
    (delete-file file2)
    (unwind-protect
	(progn
	  (symlink-file file2 file)
	  (delete-file file)
	  (or (filep file2)
	      (filep file2 :check-for-links ())
	      (fi (filep file2 :check-for-links t))))
      (delete-file file2))))
