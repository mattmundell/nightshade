;;; Tests of lisp:delete-file.

(in-package "LISP")

(import '(deftest:deftest))

(deftest delete-file (() delete-file-0)
  "Test `delete-file' with a file."
  (let ((file (pick-new-file)))
    (delete-file file)
    (probe-file file)))

(deftest delete-file (t delete-file-1)
  "Test `delete-file' with a file."
  (let ((dir (pick-new-dir)))
    (ignore-errors (delete-file dir))
    (prog1 (if (probe-file dir) t)
      (delete-dir dir))))

(deftest delete-file (() delete-file-2)
  "Test `delete-file' with a symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (unwind-protect
	(progn
	  (delete-file link)
	  (symlink-file link file)
	  (delete-file link)
	  (probe-file link))
    (delete-file file))))

(deftest delete-file (() delete-file-3)
  "Test `delete-file' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (symlink-file link file)
    (delete-file file)
    (delete-file link)
    (or (probe-file link) (probe-file link))))
