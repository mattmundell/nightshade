;;; Tests of lisp:file-kind.

(in-package "LISP")

(import '(deftest:deftest))

(deftest file-kind (:directory file-kind-0)
  "Test `file-kind' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(file-kind dir)
      (delete-dir dir))))

(deftest file-kind (:file file-kind-1)
  "Test `file-kind' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(file-kind file)
      (delete-file file))))

(deftest file-kind (:file file-kind-2)
  "Test `file-kind' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-kind link))
      (ignore-errors (delete-file link))
      (delete-file file))))

(deftest file-kind (:link file-kind-3)
  "Test `file-kind' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (file-kind link :check-for-links t))
      (ignore-errors (delete-file link))
      (delete-file file))))

(deftest file-kind (:directory file-kind-4)
  "Test `file-kind' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-kind link))
      (ignore-errors (delete-file link))
      (delete-dir dir))))

(deftest file-kind (:link file-kind-5)
  "Test `file-kind' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (file-kind link :check-for-links t))
      (ignore-errors (delete-file link))
      (delete-dir dir))))

(deftest file-kind (:link file-kind-6)
  "Test `file-kind' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-kind link :check-for-links t))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-file file)))))

(deftest file-kind (() file-kind-7)
  "Test `file-kind' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (file-kind link))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-file file)))))

(deftest file-kind (:special file-kind-8)
  "Test `file-kind' with a special file."
  (file-kind "/dev/null"))

(deftest file-kind (:link file-kind-9)
  "Test `file-kind' with a symlink to the current directory."
  (let ((dir (pick-new-dir)))
    (in-directory dir
      (symlink-file "a" dir)
      (prog1
	  (file-kind "a" :check-for-links t)
	(delete-file "a")
	(delete-dir dir)))))

(deftest file-kind (:directory file-kind-10)
  "Test `file-kind' with the destination of a symlink to the current
   directory."
  (let ((dir (pick-new-dir)))
    (in-directory dir
      (symlink-file "a" dir)
      (prog1
	  (file-kind "a")
	(delete-file "a")
	(delete-dir dir)))))

(deftest file-kind (:link file-kind-11)
  "Test `file-kind' with a self-referencing symlink."
  (let ((dir (pick-new-dir)))
    (in-directory dir
      (symlink-file "a" "a")
      (prog1
	  (file-kind "a" :check-for-links t)
	(delete-file "a")
	(delete-dir dir)))))

(deftest file-kind (:directory file-kind-12)
  "Test `file-kind' with the destination of a self-referencing symlink."
  (let ((dir (pick-new-dir)))
    (in-directory dir
      (symlink-file "a" "a")
      (prog1
	  (file-kind "a")
	(delete-file "a")
	(delete-dir dir)))))
