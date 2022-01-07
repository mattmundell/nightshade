(declaim (optimize speed))

(defun foo (x)
  (declare (type (simple-string 3) x))
  (char x 2))
