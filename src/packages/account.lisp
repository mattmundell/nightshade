;; Simple bank statement parsing and summary.
;;
;; This file is public domain.

(defpackage "ACCOUNT"
  (:version 0)
  (:use "LISP" "EXTENSIONS" "ED")
  (:export "SUMMARISE")
  (:documentation "Simple bank statement parsing and summary.

Command `Summarise Account' prompts for a bank statement in comma
seperated values (CSV) format, then prompts for a section name for each
row in the statement, and finally presents a short summary of the
statement in buffer \"Account Summary\".

Bank statement format:

29/08/2003,CPT,30-96-96,11853968,READING WOKING . CD 2723 29AUG03   ,20.00,,8519.16
01/09/2003,CPT,30-96-96,11853968,READING WOKING . CD 2723 30AUG03   ,100.00,,8419.16"))

(in-package "ACCOUNT")

(defvar header-text
  "date,type,sort code,account number,description,paid out,paid in,balance"
  "Column headings in CSV file.")

(defvar summary-padding 2
  "Number of spaces between columns in the summary.")

(defun insert-descr-groups (contentlist)
  "Inserts account description, paid in and paid out fields."
  (let ((lens (list 0 0 0)))
    ;; get length of longest entry for each of the three field
    (mapcar (lambda (line)
	      (let ((len (length (cdr (nth 4 line)))))
		(if (> len (car lens))
		    (setf (car lens) len))

		(setq len (length
			   (format nil
				   "~,2f"
				   (string-to-number
				    (cdr (nth 5 line))))))
		(if (> len (nth 1 lens))
		    (setf (car (cdr lens)) len))

		(setq len (length
			   (format nil
				   "~,2f"
				   (string-to-number
				    (cdr (nth 6 line))))))
		(if (> len (nth 2 lens))
		    (setf (car (cddr lens)) len))))
	    contentlist)
    (ed::message "FIX lens ~A" lens)
    ;; insert all entries
    (mapcar (lambda (line)
	      (let ((out (string-to-number (cdr (nth 5 line))))
		    (in (string-to-number (cdr (nth 6 line)))))
		(insert-string
		 (current-point)
		 (format nil
			 (format nil
				 ;; "%%-%ds%%%d.2%s%%%d.2%s\n"
				 "~~-~dA~~~d~A~~~d~A~%"
				 (+ (car lens)
				    summary-padding)
				 (nth 1 lens)
				 (if (equal out 0) "A" "f")
				 (+ (nth 2 lens)
				    summary-padding)
				 (if (equal in 0) "A" "f"))
			 (cdr (nth 4 line))
			 (if (equal out 0) "" out)
			 (if (equal in 0) "" in)))))
	    contentlist)))

(defun make-csv-list-unique (csv-list)
  "Return a unique version of CSV-LIST.
   Returns a list like CSV-LIST in which elements with the same
   description are combined.  Only respects the in and out fields."
  (let (unique-list)
    (mapcar
     (lambda (line)
       (catch 'found
	 (let ((descr (cdr (nth 4 line))))
	   (mapcar
	    (lambda (unique-line)
	      (if (equal descr (cdr (nth 4 unique-line)))
		  (let ((unique-in (nth 5 unique-line))
			(unique-out (nth 6 unique-line)))
		    (setf (cdr unique-in)
			  (format nil "~A"
				  (+ (string-to-number (cdr unique-in))
				     (string-to-number
				      (cdr (nth 5 line))))))
		    (setf (cdr unique-out)
			  (format nil "~A"
				  (+ (string-to-number (cdr unique-out))
				     (string-to-number
				      (cdr (nth 6 line))))))
		    (throw 'found t))))
	    unique-list))
;;;	 (setq unique-list (append unique-list (list line)))))
;;;	 (setq unique-list (nconc unique-list (list line)))))
	 (if unique-list
	     (nconc unique-list (list line))
	     (setq unique-list (list line)))))
     csv-list)
    unique-list))

(defvar *summary-history* (make-ring 40)
  "History list of account summary sections.")

(defvar *summary-history-pointer* 0
  "Current position during a historical exploration.")

(defun make-summary-list (csv-list)
  "FIX, name."
  (let (unique-list)
    (mapcar
     (lambda (line)
       (let ((section (prompt-for-string
		       :prompt (format nil
				       "Section for ~s, ~s ~s: "
				       (cdr (car line))
				       (cdr (nth 4 line))
				       (let ((out (cdr (nth 5 line))))
					 (if (equal out "")
					     (concatenate 'simple-string
							  "+£" (cdr (nth 6 line)))
					     (concatenate 'simple-string
							  "-£" out))))
		       :trim t
		       :default (if (> (ring-length *summary-history*) 0)
				    (ring-ref *summary-history* 0))
		       :history *summary-history*
		       :history-pointer
		       '*summary-history-pointer*)))
	 (setf (cdr (nth 4 line)) section)
	 (catch 'found
	   (let ((descr (cdr (nth 4 line))))
	     (mapcar
	      (lambda (unique-line)
		(if (equal descr (cdr (nth 4 unique-line)))
		    (let ((unique-in (nth 5 unique-line))
			  (unique-out (nth 6 unique-line)))
		      (setf (cdr unique-in)
			    (format nil
				    "~A"
				    (+ (string-to-number
					(cdr unique-in))
				       (string-to-number
					(cdr (nth 5 line))))))
		      (setf (cdr unique-out)
			    (format nil
				    "~A"
				    (+ (string-to-number
					(cdr unique-out))
				       (string-to-number
					(cdr (nth 6 line))))))
		      (throw 'found t))))
	      unique-list))
	   (if unique-list
	       (nconc unique-list (list line))
	       (setq unique-list (list line))))))
     csv-list)
    unique-list))

(defun string-to-number (string)
  (if (or (eq string nil) (string= string ""))
      0
      (read-from-string string)))

(defun total (lines)
  "Total 'in' and 'out' in Lines, returning values in and out."
  (let ((in 0.0)
	(out 0.0))
    (mapcar (lambda (line)
	      (incf in (string-to-number (cdr (nth 6 line))))
	      (incf out (string-to-number (cdr (nth 5 line)))))
	    lines)
    (values in out)))

(defun summarise (file)
  "Summarise CSV account $file into a buffer.  Prompt for a file if $file
   is ()."
  (let ((b (or (getstring "Account Summary" *buffer-names*) ; FIX ~unique
	       (make-buffer "Account Summary")))
	(pn (or file
		(prompt-for-file
		 :prompt "CSV statement to summarise: "
		 :help "File name of CSV account statement to summarise."
		 :default (or (buffer-pathname (current-buffer))
			      (value ed::pathname-defaults))
		 :must-exist t))))
    ;; FIX prompt-for-file could catch this.
    (if (directoryp pn) (editor-error "Given statement is a directory."))
    (change-to-buffer b)
    (delete-region (buffer-region b))
    (let ((point (current-point)))
      (insert-string point header-text)
      (insert-character point #\newline)
      (ed::insert-file-command nil pn) ; FIX
      (break)
      (let* ((statement (csv:parse-buffer b))
	     (statement2 (csv:parse-buffer b))
	     statement-unique summary)
	(delete-region (buffer-region b))
	;;(total statement)
	(multiple-value-bind (in out)
			     (total statement)
	  (insert-string point
			 (format nil "out: ~f~% in: ~f~%~%" out in))  ;; FIX both .2f
	  (setq summary (make-summary-list statement2))
	  (insert-descr-groups summary)
	  (setq statement-unique (make-csv-list-unique statement))
	  (setq in 0.0 out 0.0)
	  (multiple-value-bind (in out)
			       (total statement-unique)
	    (insert-string point
			   (format nil
				   "~%~%-------------------------------~%"))
	    (insert-string point
			   (format nil
				   "Per-description Totals:~%~%   out: ~f~%   in:  ~f~%~%"
				   out in))
	    (insert-descr-groups statement-unique))
	  ;;(print summary))
	  ;;(csv-insert-contents statement-unique))
	  ;;(switch-to-buffer b)
	  )))))

(in-package "ED")

(defcommand "Summarise Account" (p file)
  "Summarise prompted CSV account into a buffer."
  "Summarise CSV account File into a buffer.  Prompt for a file if File is
   nil."
  (declare (ignore p))
  (account:summarise file))
