;;; C-like language modes.

(in-package "ED")

#[ Editing Other Languages

The editor's programming language modes are currently fairly crude.  Most
modes provide syntax highlighting, at least of comments.  Many modes also
provide parenthesis highlighting and basic syntax-aware indenting.

FIX lisp [ Editing Lisp ]
    C-like: C, C++, Java, Perl
    Python, m4
    dylan, roff

{mode:Pascal}
]#

(defevar "Preprocessor Start"
  "String that indicates the start of a comment."
  :value ())

(defevar "C Special Forms"
  "Hashtable of C special forms."
  :value ())

(defevar "Indent Step"
  "Number of spaces per indentation level, for programming languages."
  :value 2)


;;;; C.

(defvar c-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of C special form names.")

(defun setup-c-mode (buffer)
  (if (editor-bound-p 'c-special-forms)
      (setv c-special-forms c-special-forms)
      (defevar "C Special Forms"
	"Hashtable of C special forms."
	:buffer buffer
	:value c-special-forms))
  (highlight-visible-c-buffer buffer)
  (pushnew '("C" t highlight-visible-c-buffer) *mode-highlighters*))

(defmode "C" :major-p t
  :setup-function 'setup-c-mode)

;; FIX add a big test, handle comments,strings (end begin inbetween)
;; FIX handle function name line and following line
;; FIX indent cbrace according to obrace
;;     eg if obrace starts on if line then indent cbrace in line w i of if
;;         maybe when generic brace highlighter
(defun c-indentation (mark)
  "Compute number of spaces which mark should be indented according to
   local context and C conventions.  This assumes mark is at the beginning
   of the line to be indented."
  (let ((pp-start (value preprocessor-start)))
    (if (and pp-start (at* pp-start mark))
	(return-from c-indentation 0))
    (with-mark ((tem mark))
      ;; Move up onto the first of any preceding preprocessor directive
      ;; lines.
      ;; FIX handle multip pp lines
      (when (and pp-start (line-offset tem -1))
	(when (find-attribute tem :whitespace #'zerop)
	  (if (at* pp-start tem)
	      (line-start tem)
	      (move-mark tem mark))))
      ;; Calculate indentation.
      ;; FIX finish fun call arg calc
      ;; FIX single expr if,else  multiple line expr
      (when (reverse-find-attribute tem :whitespace #'zerop)
	(let ((ch (previous-character tem)))
	  (cond ((eq ch #\})
		 (- (mark-column tem)
		    (if (eq (next-character mark) #\}) 5 3)))
		((eq ch #\))
		 (line-start tem)
		 (find-attribute tem :whitespace #'zerop)
		 (+ (mark-column tem)
		    ;(if (eq (next-character mark) #\{) 2 4)
		    (value indent-step)))
		((eq ch #\{)
		 (line-start tem)
		 (find-attribute tem :whitespace #'zerop)
		 (+ (mark-column tem)
		    ;(if (eq (next-character tem) #\{) 2 4)
		    (value indent-step)))
		((eq ch #\,)
		 (reverse-find-character tem #\()
		 (1+ (mark-column tem)))
		(t
		 (with-mark ((tem2 tem))
		   (line-start tem2)
		   ;; FIX check for preprocessor lines
		   (let ((prev))
		     (when (reverse-find-attribute tem2 :whitespace #'zerop)
		       (setq prev (previous-character tem2)))
		     (cond
		      ((eq prev #\,)
		       (line-start tem2)
		       (find-attribute tem2 :whitespace #'zerop)
		       (mark-column tem2))
		      ((eq prev #\))
		       (line-start tem2)
		       (find-attribute tem2 :whitespace #'zerop)
		       (if (eq (next-character mark) #\})
			   (- (mark-column tem2) (value indent-step))
			   (mark-column tem2)))
		      (t
		       (line-start tem)
		       (find-attribute tem :whitespace #'zerop)
		       (if (eq (next-character mark) #\})
			   (- (mark-column tem) (value indent-step))
			   (mark-column tem)))))))))))))

(defevar "Indent with Tabs"
  "Whether `indent-for-c' indents with tabs."
  :value t)

(defun indent-for-c (mark)
  (line-start mark)
  (insert-c-indentation mark))

(defun insert-c-indentation (mark)
  (let ((length (c-indentation mark)))
    (if (maybe-delete-horizontal-space mark length)
	(funcall (if (value indent-with-tabs)
		     (value tab-indenter)
		     (value space-indenter))
		 mark length))))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :mode "C" :value #'indent-for-c)

(defevar "Indent with Tabs"
  "When true, uses `Tab Indenter' instead of `Space Indenter' to indent
   lines."
  :mode "C" :value t)

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "C" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "C" :value "/*")

(defevar "Comment End"
  "String that ends comments.  () indicates #\newline termination."
  :mode "C" :value "*/")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "C" :value "// ")

(defevar "Preprocessor Start"
  "String that indicates the start of a preprocessor directive."
  :mode "C" :value "#")

(shadow-attribute :scribe-syntax #\< () "C")

;;; Context highlighting.

(setf (gethash "if" c-special-forms) t)
(setf (gethash "else" c-special-forms) t)
(setf (gethash "switch" c-special-forms) t)
(setf (gethash "case" c-special-forms) t)
(setf (gethash "default" c-special-forms) t)

(setf (gethash "while" c-special-forms) t)
(setf (gethash "for" c-special-forms) t)
(setf (gethash "do" c-special-forms) t)

(setf (gethash "return" c-special-forms) t)
(setf (gethash "goto" c-special-forms) t)
(setf (gethash "exit" c-special-forms) t)
(setf (gethash "break" c-special-forms) t)
(setf (gethash "continue" c-special-forms) t)

(setf (gethash "cast" c-special-forms) t)
(setf (gethash "typedef" c-special-forms) t)

(declaim (inline highlight-c-line))
(declaim (special *context*))

(defun search-for-c-qmark (string &optional (start 0))
  "Return position of first \" in String if there are any, else nil.  Skip
   any quoted quotation marks (like \\\" or '\"')."
  (do ((string-start (position #\" string :start start)
		     (position #\" string :start (1+ string-start))))
      ((or (eq string-start nil)
	   (zerop string-start)
	   (and (plusp string-start)
		(if (and (eq (aref string (1- string-start)) #\')
			 (> (1- (length string)) string-start)
			 (eq (aref string (1+ string-start)) #\'))
		    nil
		    (oddp (loop
			    for start downfrom (1- string-start) count start
			    while (eq (aref string start) #\\))))))
       string-start)))

;;; FIX return;  '#'

(defun highlight-c-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line))
	  (comment-end (value comment-end))
	  (pos 0))
      (case *context*
	(:string
	 (chi-mark line 0 *string-font* :string chi-info)
	 (setq pos (1+ (or (search-for-c-qmark chars)
			   (return-from highlight-c-line))))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ()))
	(:comment
	 (chi-mark line 0 *comment-font* :comment chi-info)
	 (setq pos (+ (or (search comment-end chars)
			  (return-from highlight-c-line))
		      (length comment-end)))
	 (chi-mark line pos *original-font* :comment chi-info)
	 (setq *context* ())))
      (let ((comment-start (value comment-start))
	    (pp-start (value preprocessor-start)))
	(loop
	  (let ((string (or (search-for-c-qmark chars pos)
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
		  (when (gethash (subseq chars start (mark-charpos end))
				 (value c-special-forms))
		    (chi-mark line start *special-form-font*
			      :special-form chi-info)
		    (chi-mark line (mark-charpos end) *original-font*
			      :window-foreground chi-info)))))
	    ;; Highlight the rest.
	    (cond ((< string (min preproc comment multic))
		   (chi-mark line string *string-font*
			     :string chi-info)
		   (setq pos (search-for-c-qmark chars (1+ string)))
		   (if pos
		       (chi-mark line (incf pos) *original-font*
				 :window-foreground chi-info)
		       (progn
			 (setq *context* :string)
			 (return-from highlight-c-line))))

		  ((< preproc (min string comment multic))
		   ; FIX handle run-on lines (lines ending in \)
		   (chi-mark line preproc *preprocessor-font*
			     :preprocessor chi-info)
		   (return-from highlight-c-line))

		  ((< comment (min string preproc multic))
		   (chi-mark line comment *comment-font*
			     :comment chi-info)
		   (return-from highlight-c-line))

		  ((< multic (min string preproc comment))
		   (chi-mark line multic *comment-font*
			     :comment chi-info)
		   (or comment-end (return-from highlight-c-line))
		   (setq pos
			 (search comment-end chars
				 :start2 (+ multic (length comment-start))))
		   (if pos
		       (chi-mark line (incf pos (length comment-end))
				 *original-font* :window-foreground
				 chi-info)
		       (progn
			 (setq *context* :comment)
			 (return-from highlight-c-line))))

		  (t
		   (return-from highlight-c-line)))))))))

(defun highlight-c-buffer (buffer)
  (highlight-chi-buffer buffer highlight-c-line))

(defun highlight-visible-c-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-c-line))


;;;; C++.

(defvar c++-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of C special form names.")

;; Copy the C special forms.
(do-hash (key value c-special-forms)
  (setf (gethash key c++-special-forms) value))

(setf (gethash "class" c++-special-forms) t)
(setf (gethash "namespace" c++-special-forms) t)
(setf (gethash "private" c++-special-forms) t)
(setf (gethash "protected" c++-special-forms) t)
(setf (gethash "public" c++-special-forms) t)
(setf (gethash "using" c++-special-forms) t)
(setf (gethash "virtual" c++-special-forms) t)

(defun setup-c++-mode (buffer)
  (if (editor-bound-p 'c-special-forms)
      (setv c-special-forms c++-special-forms)
      (defevar "C Special Forms"
	"Hashtable of C special forms."
	:buffer buffer
	:value c-special-forms))
  ;; FIX use c++ special forms
  (highlight-visible-c-buffer buffer)
  (pushnew '("C++" t highlight-visible-c-buffer) *mode-highlighters*))

(defmode "C++" :major-p t
  :setup-function 'setup-c++-mode)

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'indent-for-c
  :mode "C++")

(defevar "Indent with Tabs"
  "When true, uses `Tab Indenter' instead of `Space Indenter' to indent
   lines."
  :mode "C++" :value t)

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of
   `New Line'."
  :mode "C++" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "C++" :value "/*")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "C++" :value "*/")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "C++" :value "/* ")

(defevar "Preprocessor Start"
  "String that indicates the start of a preprocessor directive."
  :mode "C++" :value "#")

(shadow-attribute :scribe-syntax #\< nil "C++")


;;;; Perl.

(defun setup-perl-mode (buffer)
  (if (editor-bound-p 'c-special-forms)
      (setv c-special-forms c-special-forms)
      (defevar "C Special Forms"
	"Hashtable of C special forms."
	:buffer buffer
	:value c-special-forms))
  (highlight-visible-c-buffer buffer)
  (pushnew '("Perl" t highlight-visible-c-buffer) *mode-highlighters*))

(defmode "Perl" :major-p t
  :setup-function 'setup-perl-mode)

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'indent-for-c
  :mode "Perl")

(defevar "Indent with Tabs"
  "When true, uses `Tab Indenter' instead of `Space Indenter' to indent
   lines."
  :mode "Perl" :value t)

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of
   `New Line'."
  :mode "Perl" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Perl" :value "#")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Perl")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Perl" :value "# ")

(shadow-attribute :scribe-syntax #\< nil "Perl")


;;;; Java.

(defun setup-java-mode (buffer)
  (if (editor-bound-p 'c-special-forms)
      (setv c-special-forms c-special-forms)
      (defevar "C Special Forms"
	"Hashtable of C special forms."
	:buffer buffer
	:value c-special-forms))
  (highlight-visible-c-buffer buffer)
  (pushnew '("Java" t highlight-visible-c-buffer) *mode-highlighters*))

(defmode "Java" :major-p t
  :setup-function 'setup-java-mode)

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'indent-for-c
  :mode "Java")

(defevar "Indent with Tabs"
  "When true, uses `Tab Indenter' instead of `Space Indenter' to indent
   lines."
  :mode "Java" :value t)

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of
   `New Line'."
  :mode "Java" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Java" :value "#")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Java")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Java" :value "# ")

(shadow-attribute :scribe-syntax #\< nil "Java")
