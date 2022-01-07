;;; Tests of lisp:hidden-name-p.

(in-package "LISP")

(import '(deftest:deftest))


;;; File and directory names.

(deftest hidden-name-p (() hidden-name-p-0)
  "Test `hidden-name-p' with a directory name."
  (hidden-name-p "/a/b/c/"))

(deftest hidden-name-p (() hidden-name-p-1)
  "Test `hidden-name-p' with a visible file name."
  (hidden-name-p "/a/b/c"))

(deftest hidden-name-p (() hidden-name-p-2)
  "Test `hidden-name-p' with a visible file name with an extension."
  (hidden-name-p "/a/b/c.d"))

(deftest hidden-name-p (t hidden-name-p-3)
  "Test `hidden-name-p' with a hidden file name."
  (hidden-name-p "/a/b/.c"))

(deftest hidden-name-p (t hidden-name-p-4)
  "Test `hidden-name-p' with a hidden file name with an extension."
  (hidden-name-p "/a/b/.c.d"))


;;; Current directory, and "up".

(deftest hidden-name-p (t hidden-name-p-40)
  "Test `hiddenp' with the name for the current directory."
  (hidden-name-p "."))

(deftest hidden-name-p (t hidden-name-p-41)
  "Test `hiddenp' with the name for the parent directory."
  (hidden-name-p ".."))
