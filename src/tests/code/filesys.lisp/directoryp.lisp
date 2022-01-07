;;; Tests of lisp:directoryp.

(in-package "LISP")

(import '(deftest:deftest))

(deftest directoryp (t directoryp-0)
  "Test `directoryp' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(directoryp dir)
      (delete-dir dir))))

(deftest directoryp (() directoryp-1)
  "Test `directoryp' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(directoryp file)
      (delete-file file))))

(deftest directoryp (t directoryp-2)
  "Test `directoryp' with a symlink."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (and (directoryp dir)
	       (directoryp link)
	       (directoryp dir :check-for-links ())
	       (directoryp link :check-for-links ())
	       (directoryp dir :check-for-links t)
	       (fi (directoryp link :check-for-links t))))
      (delete-dir dir)
      (delete-file link))))

(deftest directoryp (() directoryp-3)
  "Test `directoryp' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (or (directoryp link) ; FIX should err?
	      (directoryp link :check-for-links ()) ; FIX should err?
	      (directoryp link :check-for-links t)))
      (delete-file link))))
