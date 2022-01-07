;; -*- Package: ED -*-

(defpackage "DABBREV"
  (:use "COMMON-LISP" "ED")
  (:export "DABBREV-EXPAND")
  (:documentation
   "Completion of the word at point according to the current context."))

(in-package "ED")

(defcommand "Dabbrev Expand" (&optional p)
  "Expand previous word \"dynamically\".
   Expands to the most recent, preceding word for which this is a prefix.
   If no suitable preceding word is found, words following point are
   considered.
   Repeated calls continue by finding new expansions."
  "Expand previous word \"dynamically\".
   Expands to the most recent, preceding word for which this is a prefix.
   If no suitable preceding word is found, words following point are
   considered.
   Repeated calls continue by finding new expansions."
  (declare (ignore p))
  (dabbrev:dabbrev-expand))

(in-package "DABBREV")


;;;; Internal state for continuing expansions.

(defvar *expanded-suffix-length* nil
  "Length of previous expanded suffix, if an expansion has been made.  This
   is needed to undo old expansions.")

(defvar *seen-expansions* nil
  "List of expansions that have already been offered.  These will be
   skipped in future.")

(defvar *continuation* nil
  "No-args closure to replace the current match with a new one.")

(defvar *buffers* ()
  "List of buffers to search for expansions.")

(defun dabbrev-expand ()
  "Main entry function.  Try to dynamically expand the word at point."
  (if (eq (last-command-type) :dabbrev-expand)
      (funcall *continuation*)
      (new-search)))

(defun new-search ()
  (let ((mark (copy-mark (current-point) :temporary)))
    (when (move-to-prefix-start mark)
      (let ((prefix (region-to-string (region mark (current-point)))))
	(if (string= prefix "")
	    (editor-error "No possible abbreviation preceding point")
	    (progn
	      (setq *buffers* (mapcar #'window-buffer *window-list*))
	      (setq *buffers*
		    (append *buffers*
			    (set-difference *buffer-list* *buffers*)))
	      (expand mark :backward prefix)))))))

(defun move-to-prefix-start (mark)
  "Move Mark to the beginning of the word containing it.  Returns Nil if
   there is no matching word."
  (or (reverse-find-attribute mark :lisp-syntax #'not-constituent-p)
      (line-start mark)))


;;;; Main searching engine.

(defun expand (mark direction string)
  "Find and insert an expansion of String.  Searches from Mark for the
   match, starting in Direction."
  (cond ((find-pattern mark
		       (new-search-pattern :string-sensitive direction string))
	 (let* ((suffix (extract-suffix mark string))
		(match-pos (copy-mark mark :temporary)))
	   (advance-search-pos mark direction string)
	   (if (good-suffix-p match-pos suffix)
	       (apply-expansion mark direction string suffix)
	       (expand mark direction string))))
	(t
	 (continue-failed-expansion mark direction string))))

(defun advance-search-pos (mark dir string)
  "Advance Mark, so the next search won't match the same place.  Mark
   points to the beginning of a match for String."
  (when (eq dir :forward)
    (character-offset mark (length string))))

(defun good-suffix-p (mark suffix)
  "Is the Suffix of the match at Mark usable?"
  (and (> (length suffix) 0)
       (at-beginning-of-word-p mark)
       (not (member suffix *seen-expansions* :test #'string=))))

(defun extract-suffix (match-mark string)
  "Get the suffix after String at Match-Mark.
   Example: if Match-Mark is \"quux /\\foobar baz\" and String is
   \"foo\", then the result is \"bar\"."
  (let ((start (copy-mark match-mark :temporary))
	(end   (copy-mark match-mark :temporary)))
    (character-offset start (length string))
    (move-mark end start)
    (or (find-attribute end :lisp-syntax #'not-constituent-p)
	(line-end end))
    (region-to-string (region start end))))

(defun continue-failed-expansion (mark dir string)
  "Continue or end the search, after one avenue has failed."
  (cond ((eq dir :backward)
	 ;; Turn around -- now try forwards from Point
	 (expand (copy-mark (buffer-point (line-buffer (mark-line mark)))
			    :temporary)
		 :forward string))
	(t
	 ;; Try next buffer.
	 (expand (copy-mark
		  (buffer-point
		   (or (pop *buffers*)
		       (progn
			 (undo-previous-expansion)
			 (editor-error (if *seen-expansions*
					   "Expanding `~A' wrapped"
					   "Failed to expand `~A'")
				       string))))
		  :temporary)
		 :backward string))))


(defun apply-expansion (mark dir string suffix)
  "Apply the expansion Suffix to the buffer.  Sets up a continuation to
   search for the next expansion of String in Direction, starting from
   Mark."
  (undo-previous-expansion)
  (insert-string (current-point) suffix)
  (setup-continuation mark dir string suffix))

(defun undo-previous-expansion ()
  (when *expanded-suffix-length*
    (delete-characters (current-point) (- *expanded-suffix-length*))))

(defun setup-continuation (mark dir string suffix)
  "Setup a continuation for the next call to Dabbrev Expand.  The
   continuation is a closure that will undo the current match and apply the
   next one."
  (setf (last-command-type) :dabbrev-expand)
  (let ((len (length suffix))
	(seen (cons suffix *seen-expansions*)))
    (setq *continuation*
	  (lambda ()
	    (let ((*expanded-suffix-length* len)
		  (*seen-expansions* seen))
	      (expand mark dir string))))))


;;;; Little helpers.

(defun at-beginning-of-word-p (mark)
  (or (start-line-p mark)
      (eq (character-attribute
	   :word-delimiter
	   (previous-character mark))
	  1)))

(defun not-constituent-p (property)
  (not (eq property :constituent)))
