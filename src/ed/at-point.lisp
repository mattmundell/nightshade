;;; Functions that return various entities from point or a mark.

(in-package "EDI")

(export '(word-at-point number-at-point symbol-at-point
	  pathname-at-point url-at-point manual-name-at-point))

(defun word-at-point (&optional (mark (current-point)) more-word-chars)
  "If there is a word at MARK return the word, else return nil.
   MORE-WORD-CHARS lists chars that can occur in a word, in addition to the
   chars that are word parts by attribute."
  (let ((ch (next-character mark)))
    (if (or (zerop (character-attribute :word-delimiter ch))
	    (and more-word-chars (member ch more-word-chars)))
	(let ((mark-1 (copy-mark mark :temporary)))
	  (find-attribute mark-1 :word-delimiter)
	  (when more-word-chars
	    (loop while (and (next-character mark-1)
			     (member (next-character mark-1)
				     more-word-chars)) do
	      (if (mark-after mark-1)
		  (find-attribute mark-1 :word-delimiter))))
	  (let ((mark-2 (copy-mark mark-1)))
	    (reverse-find-attribute (mark-before mark-2) :word-delimiter)
	    (when more-word-chars
	      (loop while (and (previous-character mark-2)
			       (member (previous-character mark-2)
				       more-word-chars)) do
		(reverse-find-attribute (mark-before mark-2) :word-delimiter)))
	    (region-to-string (region mark-2 mark-1)))))))

(defun number-at-point (&optional (mark (current-point)))
  "If there is a number at Mark return the number, else return nil."
  (let ((ch (next-character mark)))
    (if (or (eq ch #\.) (zerop (character-attribute :word-delimiter ch)))
	(let* ((mark-1 (find-attribute (copy-mark mark :temporary)
				       :word-delimiter))
	       (mark-2 (copy-mark mark-1))
	       dot-after-p)
	  (when (eq (next-character mark-1) #\.)
	    (setq dot-after-p t)
	    (mark-after mark-1)
	    (setq mark-1 (find-attribute mark-1 :word-delimiter)))
	  (reverse-find-attribute (mark-before mark-2) :word-delimiter)
	  (or dot-after-p
	      (when (eq (previous-character mark-2) #\.)
		(setq mark-2
		      (reverse-find-attribute (mark-before mark-2)
					      :word-delimiter))))
	  (let ((object (read-from-string (region-to-string (region mark-2
								    mark-1)))))
	    (if (numberp object) object))))))

(defun number-at-mark (mark)
  "If there is a number at mark or a space at mark and a number before the
   space then return the number.  (This is used by Tally-column.  Function
   Number-at-point requires a number at the given mark.)"
  (if (or (eq (next-character mark) #\ )
	  (eq (next-character mark) #\newline))
      (progn
	(mark-before mark)
	(prog1 (number-at-point mark)
	  (mark-after mark)))
      (number-at-point mark)))

;; FIX a bit fuzzy, reads inside strings as symbols
(defun symbol-at-point ()
  "Return the word at point as a symbol."
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    ;; FIX search across -
    ;; Move to end of word.
    (and (find-attribute mark2 :word-delimiter #'zerop)
	 (find-attribute mark2 :word-delimiter))
    (with-input-from-region (s (region mark1 mark2))
      (ed::in-lisp
       (let ((thing (read s)))
	 (if (symbolp thing)
	     thing
	     (if (and (consp thing)
		      (or (eq (car thing) 'quote)
			  (eq (car thing) 'function))
		      (symbolp (cadr thing)))
		 (cadr thing)
		 (editor-error "~S is not a symbol, or 'symbol, or #'symbol."
			       thing))))))))

;; FIX handle ~
(defun pathname-at-point (&optional (mark (current-point)))
  "If point is on the name of a file return the name, else return nil.
   This might be slow for long filenames (> 150 characters)."
  ;; FIX mv to tests
  ;; Tests: /home/ /home/] [/home] /home/. /home/x
  ;;        / xx /xxx xxx/xxx xxxx/x x/xxxx //
  ;;        : target: x: x:x xx:x x:xx x/x:x
  ;;        \\\"target:tools/worldload.lisp\\
  (let ((ch (next-character mark)))
    (if (zerop (character-attribute :whitespace ch))
	(let ((mark-1 (copy-mark mark))
	      (mark-2 (find-attribute (copy-mark mark :temporary)
				      :whitespace)))
	  (when mark-2
	    (mark-before mark-1)
	    (or (reverse-find-attribute mark-1 :whitespace)
		(buffer-start mark-1))
	    (let ((str (region-to-string (region mark-1 mark-2))))
	      (when str
		(macrolet ((parse-name (end)
			     `(let* ((substr (subseq str start ,end))
				     (cpos (position #\: substr))
				     (spos (position #\/ substr)))
				(if (if cpos
					(or (and spos (> cpos spos))
					    ;; Errors can come from weird
					    ;; backslashing.
					    (ignore-errors
					     (search-list-defined-p substr ())))
					t)
				    (parse-namestring substr
						      ()  ;; FIX host
						      *default-pathname-defaults*
						      :junk-allowed t)))))
		  (do* ((pos (count-characters (region mark-1 mark)))
			(start 0 (incf start))
			(end (length str))
			(name (parse-name end) (parse-name end)))
		       ((and name
			     ;; Errors can come from weird backslashing.
			     (ignore-errors (probe-file name)))
			name)
		    (do* ((end2 end (decf end2))
			  (name (parse-name end2) (parse-name end2)))
			 (nil)
		      (when (and name
				 ;; Errors can come from weird
				 ;; backslashing.
				 (ignore-errors (probe-file name)))
			(return-from pathname-at-point name))
		      (if (eq end2 (1+ pos))
			  (return nil)))
		    (if (eq start pos) (return nil)))))))))))

(defun url-at-point (&optional (point (current-point)))
  "If Point is on an URL return the URL, else return nil."
  ;; Tests: http://localhost/ http://a.b.c/dir/file.ext https://a.b.c/dir/
  (let ((mark (copy-mark point)))
    (or (reverse-find-attribute mark :whitespace) (buffer-start mark))
    (let ((end (copy-mark mark)))
      (when (and (character-offset end 7)
		 (let ((prefix (nstring-downcase
				(region-to-string (region mark end)))))
		   (or (string= prefix "http://")
		       (and (string= prefix "https:/")
			    (eq (char prefix 6) #\/)))))
	(or (find-attribute end :whitespace) (buffer-end end))
	(region-to-string (region mark end))))))

(defun manual-name-at-point (&optional (mark (current-point)))
  "If there's a Unix manual page name at point return it, else return ().
   Prepend any trailing section number onto the return, so \"man(1)\"
   becomes \"1 man\"."
  ;; Tests: pdksh pdksh() pdksh(1) pdksh(a) pdksh(1a) pdksh(1)1)
  ;;        pdksh (1).
  ;;        pthread_create(7)
  (let* ((name (word-at-point mark '(#\- #\_)))
	 (point (and name
		     (copy-mark mark :temporary)))
	 (other-word-chars '(#\- #\_)))
    (fi point
	name
	(progn
	  (find-attribute point :word-delimiter)
	  (loop while (and (next-character point)
			   (member (next-character point) other-word-chars)) do
	    (if (mark-after point)
		(find-attribute point :word-delimiter)))
	  (with-mark ((word-end point))
	    (when (or (char= (next-character word-end) #\ )
		      (char= (next-character word-end) #\tab))
	      (mark-after point)
	      (mark-after word-end))
	    (if (and (char= (next-character word-end) #\()
		     (mark-after point)
		     (find-attribute point :digit #'zerop)
		     (char= (next-character point) #\)))
		(if (char= (previous-character point) #\()
		    name
		    (concatenate 'simple-string
				 (region-to-string (region (mark-after word-end)
							   point))
				 " "
				 name))
		name))))))
