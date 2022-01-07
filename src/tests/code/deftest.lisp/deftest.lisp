;;; Tests of deftest:deftest.

(in-package "DEFTEST")

(import '(deftest:deftest))


(deftest deftest ('(1 2) deftest-1)
  "Test `deftest' with a multiple-value return."
  (multiple-value-list (values 1 2)))

(deftest deftest ('(1 2) deftest-2)
  "Test `deftest' failing with a multiple-value return."
  (multiple-value-list (values 1 3)))
