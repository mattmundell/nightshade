;;; Tests of lisp:read.
;;;
;;; Tests of sharp macros (#|, #x...) are in ../sharpm.lisp/.

(in-package "LISP")

(import '(deftest:deftest))

(deftest read (t read-0)
  "Test `read'."
  (with-input-from-string (in "t") (read in)))

(deftest read (3 read-1)
  "Test `read' at EOF, with error signalling."
  (handler-case
      (with-input-from-string (in "") (read in))
    (error (c) (if (eq (type-of c) 'end-of-file) 3))))

(deftest read (:eof read-2)
  "Test `read' at EOF, with EOF returning."
  (with-input-from-string (in "") (read in () :eof)))

(deftest read (:eof read-3)
  "Test `read' on an open list, with error signalling."
  (handler-case
      (with-input-from-string (in "(") (read in))
    (error (c) (if (eq (type-of c) 'end-of-file) :eof))))

(deftest read (:eof read-4)
  "Test `read' on an open list, with EOF returning."
  (with-input-from-string (in "(") (read in () :eof)))


;;;; Line comment (;).

(deftest read (:eof read-10)
  "Test `read' on a comment at EOF, with error signalling."
  (handler-case
      (with-input-from-string (in "(") (read in))
    (error (c) (if (eq (type-of c) 'end-of-file) :eof))))

(deftest read (:eof read-11)
  "Test `read' on a comment at EOF, with EOF returning."
  (with-input-from-string (in ";") (read in () :eof)))

(deftest read (3 read-12)
  "Test `read' on a form after a comment, with error signalling."
  (handler-case
      (with-input-from-string (in (format () ";~%3")) (read in))
    (error (c) (if (eq (type-of c) 'end-of-file) :eof))))

(deftest read (3 read-13)
  "Test `read' on a form after a comment, with EOF returning."
  (with-input-from-string (in (format () ";~%3")) (read in () :eof)))

(deftest read (() read-14)
  "Test `read' on a comment at EOF, with EOF returning where EOF is ()."
  (with-input-from-string (in ";") (read in () ())))

(deftest read (3 read-15)
  "Test `read' on a form after a comment, with EOF returning where EOF is ()."
  (with-input-from-string (in (format () ";~%3")) (read in () ())))


;;;; (())

(deftest read ('(()) read-20)
  "Test `read' on (()), with error signalling."
  (with-input-from-string (in "(())") (read in)))

(deftest read ('(()) read-21)
  "Test `read' on (())."
  (with-input-from-string (in "(())") (read in () :eof)))

(deftest read ('(()) read-22)
  "Test `read' on (())."
  (with-input-from-string (in "(())") (read in () ())))

