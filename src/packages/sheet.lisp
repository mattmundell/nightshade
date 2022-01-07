;; Spread sheet.                        -*- Package: SHEET -*-
;;
;; This file is public domain.

(defpackage "SHEET"
  (:version 0)
  (:use "LISP" "EXTENSIONS")
  (:documentation "Spread sheet."))

(in-package "SHEET")


;;;; Structure.

(defun make-cells ()
  (make-hash-table))

(defun getcell (cell cells)
  "Get the value of $cell in $cells."
  (gethash cell cells))

(defun %setcell (cell cells value)
  (setf (gethash cell cells) value))
;;
(defsetf getcell %setcell
  "Set the value of the cell to $value.")

(defstruct (sheet
	    (:constructor %make-sheet))
  init-form
  cells)

(defun make-sheet ()
  "Make a sheet structure with the cells slot initialized."
  (let ((sheet (%make-sheet)))
    (setf (sheet-cells sheet) (make-cells))
    sheet))


;;;; Function.

(defun find-sheet (pathname)
  "Read in the spreadsheet in $pathname."
  (from-file (in pathname)
    (let* ((sheet (make-sheet))
	   (line (read-line in))
	   (cells (sheet-cells sheet)))
      (or (string= line "NSS") (return))
      (setf (sheet-init-form sheet) (read in))
      (while ((cell (read in ()) (read in ())))
	     (cell sheet)
	(if (cdr cell)
	    (setf (getcell (car cell) cells)
		  (cadr cell)))))))

#|
(find-sheet ":tmp/eg.nss")
|#

(defvar *current-sheet* ())

(defun cell (cell &optional (sheet *current-sheet*))
  "Return the value of $cell in $sheet."
  (declare (symbol cell))
  (let ((value (getcell cell (sheet-cells sheet))))
    (typecase value
      (string value)
      (null "")
      (t (eval value)))))

;; FIX mv this to ed

(defun write-sheet (sheet &optional
			  (stream *standard-output*)
			  (height 10))
  "Write $sheet to $stream."
  (let ((package (find-package "SHEET"))
	(*current-sheet* sheet))
    (dotimes (row height)
      (format stream "~3<~A~>~10<~A~>~%"
	      row
	      (cell (intern (format () "A~D" row)
			    package))))))

#|
(write-sheet *sheet*)

(maphash (lambda (k v)
	   (ed::message "k ~A t ~A v ~A" k (type-of k) v))
	 (sheet-cells *sheet*))
|#

