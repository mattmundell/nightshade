;;; -*- Package: ASCII -*-
;;;
;;; ASCII table printing.

(defpackage "ASCII"
  (:export "print-ascii")
  (:use "LISP" "EXT")
  (:version 1)
  (:documentation "ASCII table printing."))

(in-package "ASCII")

(export '(print-ascii))

(defun print-ascii (&key (stream *standard-output*) (separator "     ")
			 (cols 4))
  "Print an ASCII table to $stream, returning #t."
  (dotimes (x cols) (format stream "~A  D   H   O   C" separator))
  (terpri stream)
  (dotimes (x cols) (format stream "~A ==  ==  ==  ==" separator))
  (terpri stream)
  (flet ((print-row (code)
	   (format stream "~3<~D~> ~3<~X~> ~3<~O~> ~3<~A~>"
		   code code code
		   (if (= code (char-code #\newline))
		       ""
		       (if (< code 32)
			   (format () "^~C" (code-char (+ (char-code #\@) code)))
			   (string (code-char code)))))))
    (let ((end (multiple-value-bind (end rem)
				    (truncate 128 cols)
		 (if (zerop rem)
		     end
		     (truncate 128 (1+ cols))))))
      (iterate table ((code 0))
	(when (< code end)
	  (dotimes (col cols)
	    (write-string separator stream)
	    (print-row (+ code (* end col))))
	  (terpri stream)
	  (table (1+ code))))))
  #t)


;;;; Editor interface.

(in-package "ED")

(defvar *ascii-buffer* ()
  "Buffer for printing the ASCII table.")

(defmode "ASCII" :major-p t)

(defcommand "ASCII" ()
  "Switch to a buffer containing a table of the ASCII characters."
  (if *ascii-buffer*
      (switch-to-buffer-command () *ascii-buffer*)
      (let ((buffer (setq *ascii-buffer*
			  (make-unique-buffer "ASCII"
					      :modes '("ASCII")))))
	(with-writable-buffer (buffer)
	  (with-output-to-mark (stream (buffer-point buffer))
	    (ascii:print-ascii :stream stream))
	  (buffer-start (buffer-point buffer)))
	(switch-to-buffer-command () buffer))))


;;;; Bindings.

(bind-key "Scroll Window Down"     #k"space"  :mode "ASCII")
(bind-key "Scroll Window Up"       #k"delete" :mode "ASCII")
(bind-key "Next Line"              #k"n"      :mode "ASCII")
(bind-key "Previous Line"          #k"p"      :mode "ASCII")
(bind-key "Rotate Buffers Forward" #k"q"      :mode "ASCII")
