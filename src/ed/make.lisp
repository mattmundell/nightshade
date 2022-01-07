;;; Makefile mode.

(in-package "HEMLOCK")

(defvar make-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of special form names in Makefiles.")

(setf (gethash "ifdef" make-special-forms) t)
(setf (gethash "ifndef" make-special-forms) t)
(setf (gethash "endif" make-special-forms) t)
(setf (gethash "include" make-special-forms) t)

(defun highlight-make-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line))
	  (comment-end (value comment-end))
	  (pos 0))
      (if *in-string*
	  (progn
	    (chi-mark line 0 string-font chi-info)
	    (setq pos (1+ (or (search-for-qmark chars)
			      (return-from highlight-make-line))))
	    (chi-mark line pos original-font chi-info)
	    (setq *in-string* nil))
	  (when *in-comment*
	    (chi-mark line 0 comment-font chi-info)
	    (setq pos (+ (or (search comment-end chars)
			     (return-from highlight-make-line))
			 (length comment-end)))
	    (chi-mark line pos original-font chi-info)
	    (setq *in-comment* nil)))
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
		    (chi-mark line start special-form-font chi-info)
		    (chi-mark line (mark-charpos end) original-font chi-info)))))
	    ;; Highlight the rest.
	    (cond ((< string (min colon dollar multic))
		   (chi-mark line string string-font chi-info)
		   (setq pos (search-for-qmark chars (1+ string)))
		   (if pos
		       (chi-mark line (incf pos) original-font chi-info)
		       (progn
			 (setq *in-string* t)
			 (return-from highlight-make-line))))

		  ((< colon (min string dollar multic))
		   (setq pos (1+ colon))
		   (when (zerop (character-attribute :whitespace (char (line-string line) 0)))
		     (chi-mark line 0 *preprocessor-font* chi-info)
		     (chi-mark line pos original-font chi-info)))

		  ((< dollar (min string colon multic))
		   (cond ((zerop (character-attribute :whitespace
						      (char (line-string line) 0)))
			  (setq pos (1+ dollar)))
			 ((eq (char (line-string line) (1+ dollar)) #\$)
			  (setq pos (+ dollar 2)))
			 (t
			  (chi-mark line dollar variable-name-font chi-info)
			  (let* ((mark (mark line dollar))
				 (end (find-attribute mark :whitespace)))
			    (or end (return-from highlight-make-line))
			    (chi-mark line (mark-charpos end) original-font chi-info)
			    (setq pos (mark-charpos end))))))

		  ((< multic (min string colon dollar))
		   (chi-mark line multic comment-font chi-info)
		   (or comment-end (return-from highlight-make-line))
		   (setq pos
			 (search comment-end chars
				 :start2 (+ multic (length comment-start))))
		   (if pos
		       (chi-mark line (incf pos (length comment-end))
				 original-font chi-info)
		       (progn
			 (setq *in-comment* t)
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

(defcommand "Make Mode" (p)
  "Put the current buffer into \"Make\" mode."
  "Put the current buffer into \"Make\" mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Make"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'tab-to-tab-stop
  :mode "Make")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Make" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Make" :value "#")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Make" :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Make" :value "# ")

(shadow-attribute :scribe-syntax #\< nil "Make")
