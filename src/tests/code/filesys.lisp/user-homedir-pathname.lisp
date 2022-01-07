;;; Tests of lisp:user-homedir-pathname.

(in-package "LISP")

(import '(deftest:deftest))

(deftest user-homedir-pathname (#p"home:" user-homedir-pathname-1)
  "Test `user-homedir-pathname-1'."
  (user-homedir-pathname))
