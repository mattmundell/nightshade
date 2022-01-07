;;; Makefile mode.

(in-package "ED")

(defvar make-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of special form names in Makefiles.")

(setf (gethash "ifdef" make-special-forms) t)
(setf (gethash "ifndef" make-special-forms) t)
(setf (gethash "else" make-special-forms) t)
(setf (gethash "endif" make-special-forms) t)
(setf (gethash "include" make-special-forms) t)

(defun highlight-make-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line))
	  (comment-end (value comment-end))
	  (pos 0))
      (case *context*
	(:string
	 (chi-mark line 0 *string-font* :string chi-info)
	 (setq pos (1+ (or (search-for-qmark chars)
			   (return-from highlight-make-line))))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ()))
	(:comment
	 (chi-mark line 0 *comment-font* :comment chi-info)
	 (setq pos (+ (or (search comment-end chars)
			  (return-from highlight-make-line))
		      (length comment-end)))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ())))
      (let ((comment-start (value comment-start)))
	(loop
	  (let ((string (or (search-for-qmark chars pos)
			    most-positive-fixnum))
		(colon (or (position #\: chars :start pos)
			   most-positive-fixnum))
		(dollar (or (position #\$ chars :start pos)
			    most-positive-fixnum))
		(multic (or (and comment-start
				 (search comment-start chars
					 :start2 pos))
			    most-positive-fixnum)))
	    ;; Highlight keywords.
	    (let ((mark (mark line pos)))
	      (loop
		while (find-attribute mark :whitespace #'zerop) do
		(let ((start (mark-charpos mark))
		      (end (find-attribute mark :whitespace)))
		  (or (and end
			   (eq (mark-line end) line)
			   (< (mark-charpos end)
			      (min string colon multic)))
		      (return-from nil))
		  (when (gethash (subseq chars start (mark-charpos end))
				 make-special-forms)
		    (chi-mark line start *special-form-font*
			      :special-form chi-info)
		    (chi-mark line (mark-charpos end) *original-font*
			      :window-foreground chi-info)))))
	    ;; Highlight the rest.
	    (cond ((< string (min colon dollar multic))
		   (chi-mark line string *string-font*
			     :string chi-info)
		   (setq pos (search-for-qmark chars (1+ string)))
		   (if pos
		       (chi-mark line (incf pos) *original-font*
				 :window-foreground chi-info)
		       (progn
			 (setq *context* :string)
			 (return-from highlight-make-line))))

		  ((< colon (min string dollar multic))
		   (setq pos (1+ colon))
		   (when (zerop (character-attribute
				 :whitespace
				 (char (line-string line) 0)))
		     (chi-mark line 0 *preprocessor-font*
			       :preprocessor chi-info)
		     (chi-mark line pos *original-font*
			       :window-foreground chi-info)))

		  ((< dollar (min string colon multic))
		   (cond ((zerop (character-attribute
				  :whitespace
				  (char (line-string line) 0)))
			  (setq pos (1+ dollar)))
			 ((and (> (line-length line) (1+ dollar))
			       (eq (char (line-string line) (1+ dollar)) #\$))
			  (setq pos (+ dollar 2)))
			 (t
			  (chi-mark line dollar *variable-name-font*
				    *variable-name-font* chi-info)
			  (let* ((mark (mark line dollar))
				 (end (find-attribute mark :whitespace)))
			    (or end (return-from highlight-make-line))
			    (chi-mark line (mark-charpos end)
				      *original-font* :window-foreground
				      chi-info)
			    (setq pos (mark-charpos end))))))

		  ((< multic (min string colon dollar))
		   (chi-mark line multic *comment-font* :comment
			     chi-info)
		   (or comment-end (return-from highlight-make-line))
		   (setq pos
			 (search comment-end chars
				 :start2 (+ multic (length comment-start))))
		   (if pos
		       (chi-mark line (incf pos (length comment-end))
				 *original-font* :window-foreground
				 chi-info)
		       (progn
			 (setq *context* :comment)
			 (return-from highlight-make-line))))

		  (t
		   (return-from highlight-make-line)))))))))

(defun highlight-visible-make-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-make-line))

(defun setup-make-mode (buffer)
  (highlight-visible-make-buffer buffer)
  (pushnew '("Make" t highlight-visible-make-buffer) *mode-highlighters*))

(defmode "Make" :major-p t
  :setup-function 'setup-make-mode)

(defcommand "Make Mode" ()
  "Put the current buffer into \"Make\" mode."
  (setf (buffer-major-mode (current-buffer)) "Make"))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'tab-to-tab-stop
  :mode "Make")

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Make" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Make" :value "#")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Make")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Make" :value "# ")

(shadow-attribute :scribe-syntax #\< nil "Make")
