;;; Python mode.

(in-package "ED")

(defvar python-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of Python special form names.")

(setf (gethash "def" python-special-forms) t)

;; FIX special-vars? same for NULL in c
;;(setf (gethash "False" python-special-forms) t)
;;(setf (gethash "True" python-special-forms) t)
;;(setf (gethash "None" python-special-forms) t)

(setf (gethash "if" python-special-forms) t)
(setf (gethash "else" python-special-forms) t)
(setf (gethash "switch" python-special-forms) t)
(setf (gethash "case" python-special-forms) t)
(setf (gethash "default" python-special-forms) t)

(setf (gethash "while" python-special-forms) t)
(setf (gethash "for" python-special-forms) t)
(setf (gethash "do" python-special-forms) t)

(setf (gethash "return" python-special-forms) t)
(setf (gethash "goto" python-special-forms) t)
(setf (gethash "exit" python-special-forms) t)

(setf (gethash "cast" python-special-forms) t)
(setf (gethash "typedef" python-special-forms) t)

(setf (gethash "from" python-special-forms) t)
(setf (gethash "import" python-special-forms) t)

(declaim (inline highlight-python-line))
(declaim (special *context*))

(defun highlight-python-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line))
	  (comment-end (value comment-end))
	  (pos 0))
      (case *context*
       (:string
	(chi-mark line 0 *string-font* :string chi-info)
	(setq pos (1+ (or (search-for-qmark chars)
			  (return-from highlight-python-line))))
	(chi-mark line pos *original-font* :window-foreground chi-info)
	(setq *context* ()))
       (:comment
	(chi-mark line 0 *comment-font* :comment chi-info)
	(setq pos (+ (or (search comment-end chars)
			 (return-from highlight-python-line))
		     (length comment-end)))
	(chi-mark line pos *original-font* :window-foreground chi-info)
	(setq *context* ())))
      (let ((comment-start (value comment-start))
	    (pp-start (value preprocessor-start)))
	(loop
	  (let ((string (or (search-for-qmark chars pos)
			    most-positive-fixnum))
		(preproc (or (and pp-start
				  (search pp-start chars :start2 pos))
			     most-positive-fixnum))
		;; FIX (value ~single-line-comment-start)
		(comment (or (search "//" chars :start2 pos)
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
			      (min string preproc comment multic)))
		      (return-from nil))
		  (let ((end-pos (if (eq (previous-character end) #\:)
				     (1- (mark-charpos end))
				     (mark-charpos end))))
		    (when (gethash (subseq chars start end-pos)
				   python-special-forms)
		      (chi-mark line start *special-form-font*
				*special-form-font* chi-info)
		      (chi-mark line end-pos *original-font*
				:window-foreground chi-info))))))
	    ;; Highlight the rest.
	    (cond ((< string (min preproc comment multic))
		   (chi-mark line string *string-font* :string
			     chi-info)
		   (setq pos (search-for-qmark chars (1+ string)))
		   (if pos
		       (chi-mark line (incf pos) *original-font*
				 :window-foreground chi-info)
		       (progn
			 (setq *context* :string)
			 (return-from highlight-python-line))))

		  ((< preproc (min string comment multic))
		   ; FIX handle run-on lines (lines ending in \)
		   (chi-mark line preproc *preprocessor-font*
			     :preprocessor chi-info)
		   (return-from highlight-python-line))

		  ((< comment (min string preproc multic))
		   (chi-mark line comment *comment-font* :comment
			     chi-info)
		   (return-from highlight-python-line))

		  ((< multic (min string preproc comment))
		   (chi-mark line multic *comment-font* :comment
			     chi-info)
		   (or comment-end (return-from highlight-python-line))
		   (setq pos
			 (search comment-end chars
				 :start2 (+ multic (length comment-start))))
		   (if pos
		       (chi-mark line (incf pos (length comment-end))
				 *original-font* :window-foreground
				 chi-info)
		       (progn
			 (setq *context* :comment)
			 (return-from highlight-python-line))))

		  (t
		   (return-from highlight-python-line)))))))))

(defun highlight-python-buffer (buffer)
  (highlight-chi-buffer buffer highlight-python-line))

(defun highlight-visible-python-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-python-line))

(defun setup-python-mode (buffer)
  (highlight-visible-python-buffer buffer)
  (pushnew '("Python" t highlight-visible-python-buffer) *mode-highlighters*))

(defmode "Python" :major-p t
  :setup-function 'setup-python-mode)

(defcommand "Python Mode" ()
  "Put the current buffer into \"Python\" mode."
  (setf (buffer-major-mode (current-buffer)) "Python"))

;; FIX add a big test, handle comments,strings (end begin inbetween)
;; FIX handle function name line and following line
;; FIX indent cbrace according to obrace
;;     eg if obrace starts on if line then indent cbrace in line w i of if
;;         maybe when generic brace highlighter
(defun python-indentation (mark)
  "Compute number of spaces which mark should be indented according to
   local context and Python conventions."
  (let ((pp-start (value preprocessor-start)))
    (if (and pp-start (at* pp-start mark))
	(return-from python-indentation 0))
    (with-mark ((tem mark))
      ;; Calculate indentation.
      ;; FIX finish fun call arg calc
      (when (reverse-find-attribute tem :whitespace #'zerop)
	(let ((ch (previous-character tem)))
	  (cond ((eq ch #\:)
		 (line-start tem)
		 (find-attribute tem :whitespace #'zerop)
		 (+ (mark-column tem)
		    (if (eq (next-character mark) #\{) 2 4)))
		((eq ch #\,)
		 (reverse-find-character tem #\()
		 (1+ (mark-column tem)))
		(t
		 (+ (- (mark-column mark)
		       (mod (mark-column mark) (value spaces-per-tab)))
		    (value spaces-per-tab))
#|
		 (with-mark ((tem2 tem))
		   (line-start tem2)
		   (let ((prev))
		     (when (reverse-find-attribute tem2 :whitespace #'zerop)
		       (setq prev (previous-character tem2)))
		     (cond
		      ((eq prev #\,)
		       (line-start tem2)
		       (find-attribute tem2 :whitespace #'zerop)
		       (mark-column tem2))
		      ((eq prev #\:)
		       (line-start tem2)
		       (find-attribute tem2 :whitespace #'zerop)
		       (if (eq (next-character mark) #\})
			   (- (mark-column tem2) 2)
			   (mark-column tem2)))
		      (t
		       (line-start tem)
		       (find-attribute tem :whitespace #'zerop)
		       (if (eq (next-character mark) #\})
			   (- (mark-column tem) 2)
			   (mark-column tem))))))
|#
		 )))))))

(defun indent-for-python (mark)
  (insert-python-indentation mark))

(defun insert-python-indentation (mark)
  (let ((chars (python-indentation mark)))
    (line-start mark)
    (delete-horizontal-space mark)
    (funcall (value indent-with-tabs) mark chars)))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'indent-for-python
  :mode "Python")

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of
   `New Line'."
  :mode "Python" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Python" :value "#")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Python")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Python" :value "# ")

(shadow-attribute :scribe-syntax #\< () "Python")
