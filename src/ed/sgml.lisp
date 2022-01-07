;;; SGML mode.

(in-package "ED")

(defun setup-sgml-mode (buffer)
  (highlight-visible-sgml-buffer buffer)
  (pushnew '("SGML" t highlight-visible-sgml-buffer) *mode-highlighters*))

(defmode "SGML" :major-p t
  :setup-function 'setup-sgml-mode)

(defcommand "SGML Mode" ()
  "Put the current buffer into \"SGML\" mode."
  (setf (buffer-major-mode (current-buffer)) "SGML"))

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "SGML" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "SGML" :value "<!--")

(defevar "Comment End"
  "String that ends comments.  () indicates #\newline termination."
  :mode "SGML" :value "-->")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "SGML" :value "// ")

(defevar "SGML Indent"
  "Number of spaces to indent a tag under the enclosing tag."
  :mode "SGML" :value 2)


;;;; Indentation.

(defcommand "SGML Insert >" ()
  "Insert a right angle bracket and indent the current line."
  (self-insert-command)
  (with-mark ((mark (current-point)))
    (line-start mark)
    (find-attribute mark :whitespace #'zerop)
    (let ((current-line (mark-line (current-point))))
      (if (and (equal (mark-line mark) current-line)
	       (char= (next-character mark) #\<))
	  (or (and (find-character (mark-after mark) #\<)
		   (equal (mark-line mark) current-line))
	      (indent-command))))))

(defun sgml-indentation (mark)
  "Compute number of spaces which mark should be indented according to
   local context and SGML conventions.  This assumes mark is at the
   beginning of the line to be indented."
  ;; Search backwards for the last open tag.
  (let ((depth 0)
	(mark1 (copy-mark mark)))
    (if (and (char= (next-character mark) #\<)
	     (let ((mark1 (copy-mark mark)))
	       (and (mark-after mark1)
		    (char= (next-character mark1) #\!)))) ; FIX assume comment
	;; Line starts with a comment.
	(return-from sgml-indentation 0))
    (let ((chi-info (getf (line-plist (mark-line mark)) 'chi-info)))
      (when chi-info
	(if (eq (chi-info-end-context chi-info) :comment)
	    ;; Line continues a comment.
	    (return-from sgml-indentation (value sgml-indent)))))
    (or (mark-before mark1)
	;; First character.
	(return-from sgml-indentation 0))
    ;; FIX better to indent according to previous line?
    ;; FIX skip over tags inside comments
    (loop while (find-character mark1 #\< :backward t) do
      (mark-after mark1)
      (case (next-character mark1)
	(#\/ (incf depth))
	(#\!) ;; Assume comment. FIX
	      ;;(fi (char= (next-character (mark-after mark)) #\-)
	(t   (when (zerop depth)
	       (flet ((content-after-mark-p (mark)
			"Return true iff there is tag content after mark."
			(fi* (end-line-p mark)
			  (let ((mark1 (copy-mark mark)))
			    (loop while (find-attribute mark1 :whitespace #'zerop) do
			      (if (equal (mark-line mark) (mark-line mark1))
				  (or (and (at* "<!--" mark1)
					   (find-string mark1 "-->")
					   (if (equal (mark-line mark) (mark-line mark1))
					       (character-offset mark1 3)))
				      (return))
				  (return-from content-after-mark-p ())))
			    t))))
		 (return-from sgml-indentation
			      (if (and (char= (next-character mark) #\<)
				       (let ((mark1 (copy-mark mark)))
					 (and (mark-after mark1)
					      (char= (next-character mark1) #\/))))
				  ;; Close tag starts the line.
				  (1- (mark-column mark1))
				  (progn
				    ;; Move mark to tag end if text follows tag.
				    (let ((mark2 (copy-mark mark1)))
				      (when (find-character mark2 #\>)
					(mark-after mark2)
					(if (content-after-mark-p mark2)
					    (move-mark mark1 (mark-before mark2)))))
				    (+ (mark-column mark1)
				       (1- (value sgml-indent))))))))
	     (decf depth)))
      (mark-before mark1))
    0))

(defun indent-for-sgml (mark)
  (line-start mark)
  (delete-horizontal-space mark)
  (funcall (value tab-indenter) mark (sgml-indentation mark)))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'indent-for-sgml
  :mode "SGML")


;;;; Highlighting.

(defun search-for-sgml-angle (string &optional (char #\<) (start 0))
  "Return position of first \< in String if there are any, else ().  Skip
   any quoted angle brackets (like \\\< or '\<')." ; FIX
  (do ((string-start (position char string :start start)
		     (position char string :start (1+ string-start))))
      ((or (eq string-start nil)
	   (zerop string-start)
	   (and (plusp string-start)
		(if (and (eq (aref string (1- string-start)) #\')
			 (> (1- (length string)) string-start)
			 (eq (aref string (1+ string-start)) #\'))
		    ()
		    (oddp (loop
			    for start downfrom (1- string-start) count start
			    while (eq (aref string start) #\\))))))
       string-start)))

(defun highlight-sgml-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line))
	  (comment-end (value comment-end))
	  (pos 0))
      (case *context*
	(:angle
	 (chi-mark line 0 *preprocessor-font*
		   :preprocessor chi-info)
	 (setq pos (1+ (or (search-for-sgml-angle chars #\>)
			   (return-from highlight-sgml-line))))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ()))
	(:comment
	 (chi-mark line 0 *comment-font* :comment chi-info)
	 (setq pos (+ (or (search comment-end chars)
			  (return-from highlight-sgml-line))
		      (length comment-end)))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ())))
      (loop
	(let ((angle (or (search-for-sgml-angle chars #\< pos)
			 most-positive-fixnum)))
	  #|
	  ;; Highlight keywords.
	  (let ((mark (mark line pos)))
	    (loop
	      while (find-attribute mark :whitespace #'zerop) do
	      (let ((start (mark-charpos mark))
		    (end (find-attribute mark :whitespace)))
		(or (and end
			 (eq (mark-line end) line)
			 (< (mark-charpos end)
			    (min string preproc comment multic)))
		    (return-from nil))
		(when (gethash (subseq chars start (mark-charpos end))
			       (value c-special-forms))
		  (chi-mark line start *special-form-font*
			    :special-form chi-info)
		  (chi-mark line (mark-charpos end) *original-font*
			    :window-foreground chi-info)))))
	  |#
	  ;; Highlight the rest.
	  (cond ((< angle most-positive-fixnum)
		 (if (and (> (length chars) (+ angle 3))
			  (string= chars "!--" :start1 (1+ angle)
				   :end1 (+ angle 4)))
		     (progn
		       (chi-mark line angle *comment-font*
				 :comment chi-info)
		       (setq pos (search comment-end chars
					 :start2 (+ angle 2)))
		       (if pos
			   (chi-mark line (incf pos
						(length comment-end))
				     *original-font* :window-foreground
				     chi-info)
			   (progn
			     (setq *context* :comment)
			     (return-from highlight-sgml-line))))
		     (progn
		       (chi-mark line angle *preprocessor-font*
				 :preprocessor chi-info)
		       (setq pos (search-for-sgml-angle chars #\> (1+ angle)))
		       (if pos
			   (chi-mark line (incf pos) *original-font*
				     :window-foreground chi-info)
			   (progn
			     (setq *context* :angle)
			     (return-from highlight-sgml-line))))))
		(t
		 (return-from highlight-sgml-line))))))))

(defun highlight-sgml-buffer (buffer)
  (highlight-chi-buffer buffer highlight-sgml-line))

(defun highlight-visible-sgml-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-sgml-line))
