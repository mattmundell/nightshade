;; Tests of lisp:delete-dir.

(in-package "LISP")

(import '(deftest:deftest))

(deftest delete-dir (() delete-dir-0)
  "Test `delete-dir' with a directory."
  (let ((dir (pick-new-dir)))
    (delete-dir dir)
    (probe-file dir)))

(deftest delete-dir (t delete-dir-1)
  "Test `delete-dir' with a file."
  (let ((file (pick-new-file)))
    (ignore-errors (delete-dir file))
    (prog1 (if (probe-file file) t)
      (delete-file file))))

(deftest delete-dir (t delete-dir-2)
  "Test `delete-dir' with a symlink."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link dir)
    (unwind-protect
	(progn
	  (ignore-errors (delete-dir link))
	  (if (and (probe-file dir) (probe-file link)) t))
      (delete-dir dir)
      (delete-file link))))

(deftest delete-dir (() delete-dir-3)
  "Test `delete-dir' on a broken symlink."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link dir)
    (unwind-protect
	(progn
	  (delete-dir dir)
	  (ignore-errors (delete-dir link))
	  (values (probe-file dir)
		  (probe-file link)))
      (delete-file link))))
