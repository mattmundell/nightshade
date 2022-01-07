;; Static code analysis.
;;
;; This file is public domain.

(defpackage "ANALYSE"
  (:version 0)
  (:use "LISP" "EXTENSIONS")
  (:export "SUMMARISE")
  (:documentation "Static code analysis."))

(in-package "ANALYSE")

(defun count-lines (file &key skip-blanks)
  "Return the number of lines in $file.

   If $skip-blanks then skip over any blank lines."
  (from-file (in file)
    (let ((count 0))
      ;; FIX note above `transfer'
      (if skip-blanks
	  (multiple-value-bind (line eofp)
			       (read-line in ())
	    (iterate iter ((line line) (eofp eofp))
	      (when line
		(or (every (lambda (char) (< (char-code char) 33)) line)
		    (incf count))
		(or eofp
		    (multiple-value-bind (line eofp)
					 (read-line in ())
		      (iter line eofp))))))
	  (multiple-value-bind (line eofp)
			       (read-line in ())
	    (iterate iter ((line line) (eofp eofp))
	      (when line
		(incf count)
		(or eofp
		    (multiple-value-bind (line eofp)
					 (read-line in ())
		      (iter line eofp)))))))
      count)))

(defun loc (file)
  "Return the number of lines of code in $file, including comments and
   documentation strings."
  (count-lines file :skip-blanks t))

#|
(defun count-in (path)
  (let ((total 0))
    (do-files (file path :recurse t)
      (let ((loc (loc file)))
	(incf total loc)
	(ed::msg "~A: ~A" file loc)))
    (ed::msg "TOTAL: ~A" total)))

(count-in "c:*.lisp")     57841
(count-in "code:*.lisp")  97350
(count-in "e:*.lisp")     66276
(count-in "tools:*.lisp")  2161
|#
