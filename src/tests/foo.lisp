(in-package "C")

(declaim (block-start foo))
(defun bar () 'yow)
(defun foo () (bar))
