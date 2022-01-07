;;; Tests of `csv:parse-buffer'.

(in-package "CSV")

(import '(deftest:deftest))

(deftest parse-bufer parse-buffer-0 '((("a" . "1")
				       ("b" . "2")
				       ("c" . "3"))
				      (("a" . "5")
				       ("b" . "6")
				       ("c" . "4")))
  "Test `parse-buffer'."
  (with-temp-buffer (buffer)
    (let ((point (buffer-point buffer)))
      (insert-string point "a,b,c")
      (insert-string point "1,2,3")
      (insert-string point "5,6,4")
      (parse-buffer buffer))))

(deftest parse-bufer parse-buffer-1 '()
  "Test `parse-buffer' with an empty buffer."
  (with-temp-buffer (buffer)
    (let ((point (buffer-point buffer)))
      (parse-buffer buffer))))

(deftest parse-bufer parse-buffer-2 '()
  "Test `parse-buffer' with just a header."
  (with-temp-buffer (buffer)
    (let ((point (buffer-point buffer)))
      (insert-string point "a,b,c")
      (parse-buffer buffer))))

(deftest parse-bufer parse-buffer-3 '((("a" . "x
y")
				       ("b" . "0")
				       ("c" . "0"))
				      (("a" . "1")
				       ("b" . "2,a")
				       ("c" . "3"))
				      (("a" . "5")
				       ("b" . "6")
				       ("c" . "4,
")))
  "Test `parse-buffer' with quoted elements."
  (with-temp-buffer (buffer)
    (let ((point (buffer-point buffer)))
      (insert-string point "a,b,c")
      (insert-string point "\"x
y\",0,0")
      (insert-string point "1,\"2,a\",,3")
      (insert-string point "5,6,\"4,
\"")
      (parse-buffer buffer))))
