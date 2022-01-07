;;; Tests of lisp:symlink-dest.

(in-package "LISP")

(import '(deftest:deftest))

(deftest symlink-dest (() symlink-dest-0)
  "Test `symlink-dest' with a directory."
  (let ((dir (pick-new-dir)))
    (unwind-protect
	(symlink-dest dir)
      (delete-dir dir))))

(deftest symlink-dest (() symlink-dest-1)
  "Test `symlink-dest' with a file."
  (let ((file (pick-new-file)))
    (unwind-protect
	(symlink-dest file)
      (delete-file file))))

(deftest symlink-dest (t symlink-dest-2)
  "Test `symlink-dest' with a symlink to a file."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (equal (symlink-dest link) file))
      (delete-file file)
      (delete-file link))))

(deftest symlink-dest (t symlink-dest-3)
  "Test `symlink-dest' with a symlink to a directory."
  (let ((dir (pick-new-dir))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link dir)
	  (equal (symlink-dest link) dir))
      (delete-dir dir)
      (delete-file link))))

(deftest symlink-dest (t symlink-dest-4)
  "Test `symlink-dest' with a broken symlink."
  (let ((file (pick-new-file))
	(link (pick-new-file)))
    (delete-file link)
    (unwind-protect
	(progn
	  (symlink-file link file)
	  (delete-file file)
	  (equal (symlink-dest link) file)
      (delete-file link)))))
