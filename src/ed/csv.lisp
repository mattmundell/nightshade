;;; Comma Separated Value (CSV) file parser.

#|
(defpackage "CSV"
  (:use    "ED" "EDI")
  (:export csv-parse-buffer csv-parse-buffer-into-list
	   csv-parse-buffer-into-node))

(in-package "CSV")
|#

(in-package "ED")

(eval-when (compile eval load)

(ed::defparser
    `((:csv         (or (many (or #\newline :row)) :epsilon))
      (:row         :field (or (many #\, :field) :epsilon) #\newline)
      (:field       (or (group (many :field-char)) :epsilon))
      (:field-char  (cond (if (or (eq ch #\,) (eq ch #\newline)) nil t)))
      (:epsilon     "")))

) ; eval-when

(defun csv-parse-buffer (buffer)
  "Return the comma separated values in Buffer as a list, one element per
   line.  The first line in the buffer lists the heading for each column.
   The elements of the returned list are alists (heading . field), where
   heading and field are both strings."
  (with-input-from-region (*stream* (buffer-region buffer))
    (let* ((*streams* `((,*stream* 0)))
	   (node (parse-csv)))
      (when node
	(let* ((content (node-content node))
	       (headings (do ((line content (node-next line)))
			     ((eq line nil)
			      (setq content line))
			   (etypecase line
			     (epsilon-node)
			     (char-node)
			     (row-node
			      (setq content line)
			      (return-from nil
					   (csv-row-node-to-list line)))))))
 	  (do ((line (node-next content) (node-next line))
	       (out-lines '()))
	      ((eq line nil)
	       (when out-lines
		 (nreverse out-lines)))
	    (etypecase line
	      (epsilon-node)
	      (char-node)
	      (row-node
	       (do ((field (node-content line) (node-next field))
		    (out-line '())
		    (count 0))
		   ((eq field nil)
		    (when out-line
		      (push (nreverse out-line) out-lines)))
		 (etypecase field
		   (epsilon-node)
		   (char-node)
		   (field-node
		    (let ((content (node-content field)))
		      (etypecase content
			(epsilon-node
			 (push (cons (nth count headings) "") out-line)
			 (incf count))
			(region-node
			 (push (cons (nth count headings)
				     (region-to-string (node-content content)))
			       out-line)
			 (incf count)))))))))))))))

(defun csv-row-node-to-list (line)
  "Return a list of the fields strings in node line."
  (etypecase line
    (epsilon-node)
    (char-node)
    (row-node
     (do ((field (node-content line) (node-next field))
	  (out-line '()))
	 ((eq field nil)
	  (when out-line
	    (nreverse out-line)))
       (etypecase field
	 (epsilon-node)
	 (char-node)
	 (field-node
	  (let ((content (node-content field)))
	    (etypecase content
	      (epsilon-node)
	      (region-node
	       (push (region-to-string (node-content content))
		     out-line))))))))))

(defun csv-parse-buffer-into-list (buffer)
  "Return the comma separated values in Buffer as a list.  Each returned
   element lists as strings the fields on successive lines of the file."
  (with-input-from-region (*stream* (buffer-region buffer))
    (let* ((*streams* `((,*stream* 0)))
	   (node (parse-csv)))
      (when node
	(do ((line (node-content node) (node-next line))
	     (out-lines '()))
	    ((eq line nil)
	     (when out-lines
	       (nreverse out-lines)))
	  (let ((out-line (csv-row-node-to-list line)))
	    (when out-line
	      (push out-line out-lines))))))))

(defun csv-parse-buffer-into-node (buffer)
  "Return a node of the comma separated values in Buffer."
  (with-input-from-region (*stream* (buffer-region buffer))
    (let* ((*streams* `((,*stream* 0))))
      (parse-csv))))
