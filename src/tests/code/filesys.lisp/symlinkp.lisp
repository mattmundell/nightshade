;;; Tests of lisp:symlinkp.

(in-package "LISP")

(import '(deftest:deftest))

(deftest symlinkp (() symlinkp-0)
  "Test `symlinkp' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(symlinkp dir)
      (delete-dir dir))))

(deftest symlinkp (() symlinkp-1)
  "Test `symlinkp' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(symlinkp file)
      (delete-file file))))

(deftest symlinkp (t symlinkp-2)
  "Test `symlinkp' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (symlinkp link))
      (delete-file file)
      (delete-file link))))

(deftest symlinkp (t symlinkp-3)
  "Test `symlinkp' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (symlinkp link))
      (delete-dir dir)
      (delete-file link))))

(deftest symlinkp (t symlinkp-4)
  "Test `symlinkp' with a broken symlink."
  (let ((file (pick-new-file))
	(file2 (pick-new-file)))
    (delete-file file2)
    (unwind-protect
	(progn
	  (symlink-file file2 file)
	  (delete-file file)
	  (symlinkp file2))
      (delete-file file2))))
