;;; -*- Package: CSV -*-

(defpackage "CSV"
  (:version 1)
  (:use    "LISP" "ED" "EDI")
  (:export "parse-buffer" "parse-buffer-into-list"
	   "parse-buffer-into-node")
  (:documentation "Comma Separated Value (CSV) file parser."))

;; FIX flag for header
;; FIX implement,use more-general "SV" where delimiter can be specified
;;         could approach awk?

(in-package "CSV")

(export '(parse-buffer parser-buffer-into-list parser-buffer-into-node))

(parse:defparser
 `((:csv           (or (many (or #\newline :row)) :epsilon))
   (:row           (or :quoted-field :field :empty)
		   (or (many #\, (or :quoted-field :field :empty)) :epsilon)
		   #\newline)
   (:field         (group (many :field-char)))
   (:quoted-field  #\" (or (group (many :quoted-char)) :epsilon) #\")
   (:field-char    (cond (fi (member parse:ch '(#\, #\newline)))))
   (:quoted-char   (cond (fi (char= parse:ch #\"))))
   (:empty         "")
   (:epsilon       "")))

(defun parse-buffer (buffer)
  "Return the comma separated values in $buffer as a list, one element per
   line.  The first line in the buffer lists the heading for each column.
   The elements of the returned list are alists (heading . field), where
   heading and field are both strings."
  (with-input-from-region (parse:*stream* (buffer-region buffer))
    (let* ((parse:*streams* `((,parse:*stream* 0)))
	   (node (parse-csv)))
      (when node
	(let* ((content (parse:node-content node))
	       (headings (do ((line content (parse:node-next line)))
			     ((eq line nil)
			      (setq content line))
			   (etypecase line
			     (epsilon-node)
			     (parse:char-node)
			     (row-node
			      (setq content line)
			      (return-from nil
					   (row-node-to-list line)))))))
	  (when content
	    (do ((line (parse:node-next content) (parse:node-next line))
		 (out-lines '()))
		((eq line nil)
		 (when out-lines (nreverse out-lines)))
	      (etypecase line
		(epsilon-node)
		(parse:char-node)
		(row-node
		 (do ((field (parse:node-content line) (parse:node-next field))
		      (out-line '())
		      (count 0))
		     ((eq field nil)
		      (if out-line (push (nreverse out-line) out-lines)))
		   (flet ((element-from-field (content)
			    (etypecase content
			      (epsilon-node
			       (push (cons (nth count headings) "") out-line)
			       (incf count))
			      (parse:region-node
			       (push (cons (nth count headings)
					   (parse:node-content content))
				     out-line)
			       (incf count)))))
		     (etypecase field
		       (epsilon-node)
		       (empty-node
			(push (cons (nth count headings) "") out-line)
			(incf count))
		       (parse:char-node)
		       (quoted-field-node
			(let ((content (parse:node-next (parse:node-content field))))
			  (element-from-field content)))
		       (field-node
			(let ((content (parse:node-content field)))
			  (element-from-field content)))))))))))))))

(defun row-node-to-list (line)
  "Return a list of the fields strings in node line."
  (etypecase line
    (epsilon-node)
    (parse:char-node)
    (row-node
     (do ((field (parse:node-content line) (parse:node-next field))
	  (out-line '()))
	 ((eq field nil)
	  (when out-line
	    (nreverse out-line)))
       (etypecase field
	 (epsilon-node)
	 (parse:char-node)
	 (field-node
	  (let ((content (parse:node-content field)))
	    (etypecase content
	      (epsilon-node)
	      (parse:region-node
	       (push (parse:node-content content)
		     out-line))))))))))

(defun parse-buffer-into-list (buffer)
  "Return the comma separated values in Buffer as a list.  Each returned
   element lists as strings the fields on successive lines of the file."
  (with-input-from-region (parse:*stream* (buffer-region buffer))
    (let* ((parse:*streams* `((,parse:*stream* 0)))
	   (node (parse-csv)))
      (when node
	(do ((line (parse:node-content node) (parse:node-next line))
	     (out-lines '()))
	    ((eq line nil)
	     (when out-lines
	       (nreverse out-lines)))
	  (let ((out-line (row-node-to-list line)))
	    (when out-line
	      (push out-line out-lines))))))))

(defun parse-buffer-into-node (buffer)
  "Return a node of the comma separated values in Buffer."
  (with-input-from-region (parse:*stream* (buffer-region buffer))
    (let* ((parse:*streams* `((,parse:*stream* 0))))
      (parse-csv))))
