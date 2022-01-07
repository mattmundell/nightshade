;;; Tests of lisp:file-author.

(in-package "LISP")

(import '(deftest:deftest))

(deftest file-author (t file-author-0)
  "Test `file-author' with a directory."
  (let ((dir (pick-new-dir))
	(file (pick-new-file)))
    (unwind-protect
	(eq (file-author dir) (file-author file))
      (delete-file file)
      (delete-dir dir))))
