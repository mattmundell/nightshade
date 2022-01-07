;;; Tests of `lisp:parse-mode-string'.

(in-package "LISP")

(import '(deftest:deftest))

(deftest parse-mode-string ("d---------" parse-mode-string-0)
  "Test `parse-mode-string'."
  (with-output-to-string (*standard-output*)
    (lisp::print-mode (parse-mode-string "d---------"))))

(deftest parse-mode-string (t parse-mode-string-1)
  "Test `parse-mode-string' on every possible mode, confirming that
   `lisp::print-mode' on the returned mode produces the same string."
  ; drwxrwxrwx
  (let ((mode-string (make-string 10 :initial-element #\-)))
    (setf (char mode-string 0) #\d)
    (labels ((iter (index)
	       (if (eq index 9)
		   (progn
		     (setf (char mode-string 9) #\-)
		     ;(format t "~A~%" mode-string)
		     (or (string= mode-string
				  (with-output-to-string (*standard-output*)
				    (lisp::print-mode (parse-mode-string mode-string))))
			 (return ()))
		     (setf (char mode-string 9) #\x)
		     ;(format t "~A~%" mode-string)
		     (or (string= mode-string
				  (with-output-to-string (*standard-output*)
				    (lisp::print-mode (parse-mode-string mode-string))))
			 (return ())))
		   (progn
		     (setf (char mode-string index) #\-)
		     (or (iter (1+ index)) (return ()))
		     ;; s
		     (when (or (eq index 3) (eq index 6))
		       (setf (char mode-string index) #\s)
		       (or (iter (1+ index)) (return ())))
		     (setf (char mode-string index)
			   (aref (vector #\x #\r #\w)
				 (mod index 3)))
		     (or (iter (1+ index)) (return ()))))))
      ;; Files.
      (iter 1)
      ;; Directories.
      (setf (char mode-string 0) #\d)
      (iter 1))))


;;;; Errors.

(deftest parse-mode-string ("d---------" parse-mode-string-10)
  "Test `parse-mode-string' with trailing junk."
  (with-output-to-string (*standard-output*)
    (lisp::print-mode (parse-mode-string "d---------xxxxxxx"))))

(deftest parse-mode-string (t parse-mode-string-11)
  "Test `parse-mode-string' with a string too short."
  (let (ret)
    (handler-case
	(parse-mode-string "d--------")
      (error () (setq ret t)))
    ret))

(deftest parse-mode-string (t parse-mode-string-12)
  "Test `parse-mode-string' with an empty string."
  (let (ret)
    (handler-case
	(parse-mode-string "")
      (error () (setq ret t)))
    ret))

(deftest parse-mode-string (t parse-mode-string-13)
  "Test `parse-mode-string' with erroneous characters in successive
   positions in the string."
  (dotimes (index 9 t)
    (let ((string "----------"))
      (setf (char string index) #\f)
      (handler-case
	  (progn
	    (parse-mode-string string)
	    (return ()))
	(error ())))))

(deftest parse-mode-string (t parse-mode-string-14)
  "Test `parse-mode-string' with ()."
  (let (ret)
    (handler-case
	(parse-mode-string ())
      (error () (setq ret t)))
    ret))

(deftest parse-mode-string (t parse-mode-string-15)
  "Test `parse-mode-string' with too few args."
  (let (ret)
    (handler-case
	(parse-mode-string)
      (error () (setq ret t)))
    ret))

(deftest parse-mode-string (t parse-mode-string-16)
  "Test `parse-mode-string' with too many args."
  (let (ret)
    (handler-case
	(parse-mode-string "----------" t)
      (error () (setq ret t)))
    ret))
