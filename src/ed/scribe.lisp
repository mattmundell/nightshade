;;; Scribe mode.

(in-package "ED")


#[ Scribe Mode

`Scribe' mode provides a number of facilities useful for editing Scribe
documents.  It is also sufficiently parameterizable to be adapted to other
similar syntaxes.

{mode:Scribe}
{command:Insert Scribe Directive}
{command:Add Scribe Directive}
{command:Add Scribe Paragraph Delimiter}
{command:List Scribe Paragraph Delimiters}

The variables `Escape Character', `Close Paren Character' and `Open Paren
Character' determine the characters used when a Scribe directive is
inserted.

{command:Scribe Insert Bracket}
{evariable:Scribe Bracket Table}
]#


;;;; Variables.

(defvar *scribe-para-break-table* (make-hash-table :test #'equal)
  "A table of the Scribe commands that should be paragraph delimiters.")
;;;
(dolist (todo '("begin" "newpage" "make" "device" "caption" "tag" "end"
		"chapter" "section" "appendix" "subsection" "paragraph"
		"unnumbered" "appendixsection" "prefacesection" "heading"
		"majorheading" "subheading"))
  (setf (gethash todo *scribe-para-break-table*) t))

(defevar "Open Paren Character"
  "The open bracket inserted by Scribe commands."
  :value #\[)

(defevar "Close Paren Character"
  "The close bracket inserted by Scribe commands."
  :value #\])

(defevar "Escape Character"
  "The escape character inserted by Scribe commands."
  :value #\@)

(defevar "Scribe Bracket Table"
  "A table that maps Scribe brackets, open and close, to their opposing
   brackets.  If a character is a bracket, then the entry for its char-code
   is the opposite bracket otherwise the entry is ()."
  :value (make-array char-code-limit))
;;;
(mapc #'(lambda (x y)
	  (setf (svref (value scribe-bracket-table) (char-code x)) y)
	  (setf (svref (value scribe-bracket-table) (char-code y)) x))
      '(#\( #\[ #\{ #\<) '(#\) #\] #\} #\>))
;;;
(eval-when (compile eval)
  (defmacro opposing-bracket (bracket)
    `(svref (value scribe-bracket-table) (char-code ,bracket)))
) ;eval-when


;;;; "Scribe Syntax" Attribute.

(defattribute "Scribe Syntax"
  "For Scribe Syntax, Possible types are:
   :ESCAPE           ; basically #\@.
   :OPEN-PAREN       ; Characters that open a Scribe paren:  #\[, #\{, #\(, #\<.
   :CLOSE-PAREN      ; Characters that close a Scribe paren:  #\], #\}, #\), #\>.
   :SPACE            ; Delimits end of a Scribe command.
   :NEWLINE          ; Delimits end of a Scribe command."
  'symbol nil)

(setf (character-attribute :SCRIBE-SYNTAX #\)) :CLOSE-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\]) :CLOSE-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\}) :CLOSE-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\>) :CLOSE-PAREN)

(setf (character-attribute :SCRIBE-SYNTAX #\() :OPEN-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\[) :OPEN-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\{) :OPEN-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\<) :OPEN-PAREN)

(setf (character-attribute :SCRIBE-SYNTAX #\Space)   :SPACE)
(setf (character-attribute :SCRIBE-SYNTAX #\Newline) :NEWLINE)
(setf (character-attribute :SCRIBE-SYNTAX #\@)       :ESCAPE)


;;;; "Scribe" mode and setup.

(defmode "Scribe" :major-p t
  :documentation
  "`Scribe' mode is like `Text' mode with modified rules for determining
   paragraph breaks.  In `Scribe' mode, paragraphs delimited by Scribe
   commands are normally placed on their own line, in addition to the
   normal paragraph breaks.  The main reason for doing this is that it
   prevents `Fill Paragraph' from mashing these commands into the body of a
   paragraph.")

(shadow-attribute :paragraph-delimiter #\@ 1 "Scribe")
(shadow-attribute :word-delimiter #\' 0 "Scribe")		;from Text Mode
(shadow-attribute :word-delimiter #\backspace 0 "Scribe")	;from Text Mode
(shadow-attribute :word-delimiter #\_ 0 "Scribe")		;from Text Mode

(define-file-type-hook ("mss") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Scribe"))


;;;; Commands.

(defcommand "Scribe Mode" ()
  "Puts buffer in Scribe mode.  Sets up comment variables and has delimiter
   matching.  The definition of paragraphs is changed to know about scribe
   commands."
  "Puts buffer in Scribe mode."
  (setf (buffer-major-mode (current-buffer)) "Scribe"))

(defcommand "Select Scribe Warnings" ()
  "Go to the Scribe Warnings buffer if it exists."
  (let ((buffer (getstring "Scribe Warnings" *buffer-names*)))
    (if buffer
	(change-to-buffer buffer)
	(editor-error "There is no Scribe Warnings buffer."))))

(defcommand "Add Scribe Paragraph Delimiter"
	    (p &optional
	       (word (prompt-for-string
		      :prompt "Scribe command: "
		      :help "Name of Scribe command to make delimit paragraphs."
		      :trim t)))
  "Add a prompted string to the list of formatting commands that delimit
   paragraphs in `Scribe' mode.  If the user supplies a prefix argument,
   then this command removes the string as a delimiter."
  "Add Word in the *scribe-para-break-table* if P is () else remove Word."
  (setf (gethash word *scribe-para-break-table*) (not p)))

(defcommand "List Scribe Paragraph Delimiters" ()
  "Pop up a display of the Scribe commands that delimit paragraphs."
  (let (result)
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k result))
	     *scribe-para-break-table*)
    (setf result (sort result #'string<))
    (with-pop-up-display (s :height (length result))
      (dolist (ele result) (write-line ele s)))))

(defcommand "Scribe Insert Bracket" ()
  "Insert the bracket (>, }, ), or ]), that caused its invocation, and then
   show the matching bracket."
  (scribe-insert-paren (current-point)
		       (ext:key-event-char *last-key-event-typed*)))

(defevar "Scribe Command Table"
  "This is a character dispatching table indicating which Scribe command or
   environment to use."
  :value (make-hash-table)
  :mode "Scribe")

(defvar *scribe-directive-type-table*
  (make-string-table :initial-contents
		     '(("Command" . :command)
		       ("Environment" . :environment))))

(defcommand "Add Scribe Directive" (p (command-name nil command-name-p)
				      type key-event mode)
  "Add to the database of directives recognized by `Insert Scribe
   Directive'.  Prompt for the directive name, the kind of directive
   (environment or command) and the key-event."
  (declare (ignore p))
  (let ((command-name (if command-name-p
			  command-name
			  (or command-name
			      (prompt-for-string :help "Directive Name"
						 :prompt "Directive: ")))))
    (multiple-value-bind (ignore type)
			 (if type
			     (values nil type)
			     (prompt-for-keyword
			      (list *scribe-directive-type-table*)
			      :help "Enter Command or Environment."
			      :prompt "Command or Environment: "))
      (declare (ignore ignore))
      (let ((key-event (or key-event
			   (prompt-for-key-event :prompt
						 "Dispatch Character: "))))
	(setf (gethash key-event
		       (cond (mode
			      (variable-value 'scribe-command-table :mode mode))
			     ((editor-bound-p 'scribe-command-table)
			      (value scribe-command-table))
			     (t (editor-error
				 "Could not find \"Scribe Command Table\"."))))
	      (cons type command-name))))))

(defcommand "Insert Scribe Directive" ()
  "Insert a Scribe directive according to a prompted key-event.  Insert
   directives according to their kind:

     environment
	Enclose the current or next paragraph a begin-end pair:
	@begin[directive] paragraph @end[directive].  If the current region
	is active, then enclose the region instead of the paragraph.

     command
	Enclose the previous word by @directive[word].  If the previous
	word is already enclosed by the same command, then extend the
	beginning of the command backward by one word.

   If help (usually ? or control-h) is given to the prompt then display a
   list of all the defined key-events."
  (loop
    (let ((key-event (prompt-for-key-event :prompt "Dispatch Character: ")))
      (if (logical-key-event-p key-event :help)
	  (directive-help)
	  (let ((table-entry (gethash key-event (value scribe-command-table))))
	    (ecase (car table-entry)
	      (:command
	       (insert-scribe-directive (current-point) (cdr table-entry))
	       (return))
	      (:environment
	       (enclose-with-environment (current-point) (cdr table-entry))
	       (return))
	      ((nil) (editor-error "Unknown dispatch character."))))))))


;;;; "Insert Scribe Directive" support.

(defun directive-help ()
  (let ((commands ())
	(environments ()))
    (declare (list commands environments))
    (maphash #'(lambda (k v)
		 (if (eql (car v) :command)
		     (push (cons k (cdr v)) commands)
		     (push (cons k (cdr v)) environments)))
	     (value scribe-command-table))
    (setf commands (sort commands #'string< :key #'cdr))
    (setf environments (sort environments #'string< :key #'cdr))
    (with-pop-up-display (s :height (1+ (max (length commands)
					     (length environments))))
      (format s "~2TCommands~47TEnvironments~%")
      (do ((commands commands (rest commands))
	   (environments environments (rest environments)))
	   ((and (endp commands) (endp environments)))
	(let* ((command (first commands))
	       (environment (first environments))
	       (cmd-char (first command))
	       (cmd-name (rest command))
	       (env-char (first environment))
	       (env-name (rest environment)))
	  (write-string "  " s)
	  (when cmd-char
	    (ext:print-pretty-key-event cmd-char s)
	    (format s "~7T")
	    (write-string (or cmd-name "<prompts for command name>") s))
	  (when env-char
	    (format s "~47T")
	    (ext:print-pretty-key-event env-char s)
	    (format s "~51T")
	    (write-string (or env-name "<prompts for command name>") s))
	  (terpri s))))))

;;;
;;; Inserting and extending :command directives.
;;;

(defevar "Insert Scribe Directive Function"
  "`Insert Scribe Directive' calls this function when the directive type is
   :command.  The function takes four arguments: a mark pointing to the
   word start, the formatting command string, the open-paren character to
   use, and a mark pointing to the word end."
  :value 'scribe-insert-scribe-directive-fun
  :mode "Scribe")

(defun scribe-insert-scribe-directive-fun (word-start command-string
					   open-paren-char word-end)
  (insert-character word-start (value escape-character))
  (insert-string word-start command-string)
  (insert-character word-start open-paren-char)
  (insert-character word-end (value close-paren-character)))

(defevar "Extend Scribe Directive Function"
  "`Insert Scribe Directive' calls this function when the directive type is
   :command to extend the the commands effect.  This function takes a
   string and three marks: the first on pointing before the open-paren
   character for the directive.  The string is the command-string to
   selected by the user which this function uses to determine if it is
   actually extending a command or inserting a new one.  The function must
   move the first mark before any command text for the directive and the
   second mark to the end of any command text.  It moves the third mark to
   the previous word's start where the command region should be.  If this
   returns true `Insert Scribe Directive' moves the command region previous
   one word, and otherwise it inserts the directive."
  :value 'scribe-extend-scribe-directive-fun
  :mode "Scribe")

(defun scribe-extend-scribe-directive-fun (command-string
					   command-end command-start word-start)
  (word-offset (move-mark command-start command-end) -1)
  (when (string= (the simple-string (region-to-string
				     (region command-start command-end)))
		 command-string)
    (mark-before command-start)
    (mark-after command-end)
    (word-offset (move-mark word-start command-start) -1)))

;;; INSERT-SCRIBE-DIRECTIVE first looks for the current or previous word at
;;; mark.  Word-p says if we found one.  If mark is immediately before a word,
;;; we use that word instead of the previous.  This is because if mark
;;; corresponds to the CURRENT-POINT, the editor cursor is displayed on the
;;; first character of the word making users think the mark is in the word
;;; instead of before it.  If we find a word, then we see if it already has
;;; the given command-string, and if it does, we extend the use of the command-
;;; string to the previous word.  At the end, if we hadn't found a word, we
;;; backup the mark one character to put it between the command brackets.
;;;
(defun insert-scribe-directive (mark &optional command-string)
  (with-mark ((word-start mark :left-inserting)
	      (word-end mark :left-inserting))
    (let ((open-paren-char (value open-paren-character))
	  (word-p (if (and (zerop (character-attribute
				   :word-delimiter
				   (next-character word-start)))
			   (= (character-attribute
			       :word-delimiter
			       (previous-character word-start))
			      1))
		      word-start
		      (word-offset word-start -1)))
	  (command-string (or command-string
			      (prompt-for-string
			       :trim t :prompt "Environment: "
			       :help "Name of environment to enclose with."))))
      (declare (simple-string command-string))
      (cond
       (word-p
	(word-offset (move-mark word-end word-start) 1)
	(if (test-char (next-character word-end) :scribe-syntax
		       :close-paren)
	    (with-mark ((command-start word-start :left-inserting)
			(command-end word-end :left-inserting))
	      ;; Move command-end from word-end to open-paren of command.
	      (balance-paren (mark-after command-end))
	      (if (funcall (value extend-scribe-directive-function)
			   command-string command-end command-start word-start)
		  (let ((region (delete-and-save-region
				 (region command-start command-end))))
		    (word-offset (move-mark word-start command-start) -1)
		    (ninsert-region word-start region))
		  (funcall (value insert-scribe-directive-function)
			   word-start command-string open-paren-char
			   word-end)))
	    (funcall (value insert-scribe-directive-function)
		     word-start command-string open-paren-char word-end)))
	(t
	 (funcall (value insert-scribe-directive-function)
		  word-start command-string open-paren-char word-end)
	 (mark-before mark))))))

;;;
;;; Inserting :environment directives.
;;;

(defun enclose-with-environment (mark &optional environment)
  (if (region-active-p)
      (let ((region (current-region)))
	(with-mark ((top (region-start region) :left-inserting)
		    (bottom (region-end region) :left-inserting))
	  (get-and-insert-environment top bottom environment)))
      (with-mark ((bottom-mark mark :left-inserting))
	(let ((paragraphp (paragraph-offset bottom-mark 1)))
	  (unless (or paragraphp
		      (and (last-line-p bottom-mark)
			   (end-line-p bottom-mark)
			   (not (blank-line-p (mark-line bottom-mark)))))
	    (editor-error "No paragraph to enclose."))
	  (with-mark ((top-mark bottom-mark :left-inserting))
	    (paragraph-offset top-mark -1)
	    (cond ((not (blank-line-p (mark-line top-mark)))
		   (insert-character top-mark #\Newline)
		   (mark-before top-mark))
		  (t
		   (insert-character top-mark #\Newline)))
	    (cond ((and (last-line-p bottom-mark)
			(not (blank-line-p (mark-line bottom-mark))))
		   (insert-character bottom-mark #\Newline))
		  (t
		   (insert-character bottom-mark #\Newline)
		   (mark-before bottom-mark)))
	    (get-and-insert-environment top-mark bottom-mark environment))))))

(defun get-and-insert-environment (top-mark bottom-mark environment)
  (let ((environment (or environment
			 (prompt-for-string
			  :trim t :prompt "Environment: "
			  :help "Name of environment to enclose with."))))
    (insert-environment top-mark "begin" environment)
    (insert-environment bottom-mark "end" environment)))

(defun insert-environment (mark command environment)
  (let ((esc-char (value escape-character))
	(open-paren (value open-paren-character))
	(close-paren (value close-paren-character)))
      (insert-character mark esc-char)
      (insert-string mark command)
      (insert-character mark open-paren)
      (insert-string mark environment)
      (insert-character mark close-paren)))

(add-scribe-directive-command nil nil :Environment #k"Control-l" "Scribe")
(add-scribe-directive-command nil nil :Command #k"Control-w" "Scribe")
(add-scribe-directive-command nil "Begin" :Command #k"b" "Scribe")
(add-scribe-directive-command nil "End" :Command #k"e" "Scribe")
(add-scribe-directive-command nil "Center" :Environment #k"c" "Scribe")
(add-scribe-directive-command nil "Description" :Environment #k"d" "Scribe")
(add-scribe-directive-command nil "Display" :Environment #k"Control-d" "Scribe")
(add-scribe-directive-command nil "Enumerate" :Environment #k"n" "Scribe")
(add-scribe-directive-command nil "Example" :Environment #k"x" "Scribe")
(add-scribe-directive-command nil "FileExample" :Environment #k"y" "Scribe")
(add-scribe-directive-command nil "FlushLeft" :Environment #k"l" "Scribe")
(add-scribe-directive-command nil "FlushRight" :Environment #k"r" "Scribe")
(add-scribe-directive-command nil "Format" :Environment #k"f" "Scribe")
(add-scribe-directive-command nil "Group" :Environment #k"g" "Scribe")
(add-scribe-directive-command nil "Itemize" :Environment #k"Control-i" "Scribe")
(add-scribe-directive-command nil "Multiple" :Environment #k"m" "Scribe")
(add-scribe-directive-command nil "ProgramExample" :Environment #k"p" "Scribe")
(add-scribe-directive-command nil "Quotation" :Environment #k"q" "Scribe")
(add-scribe-directive-command nil "Text" :Environment #k"t" "Scribe")
(add-scribe-directive-command nil "i" :Command #k"i" "Scribe")
(add-scribe-directive-command nil "b" :Command #k"Control-b" "Scribe")
(add-scribe-directive-command nil "-" :Command #k"\-" "Scribe")
(add-scribe-directive-command nil "+" :Command #k"+" "Scribe")
(add-scribe-directive-command nil "u" :Command #k"Control-j" "Scribe")
(add-scribe-directive-command nil "p" :Command #k"Control-p" "Scribe")
(add-scribe-directive-command nil "r" :Command #k"Control-r" "Scribe")
(add-scribe-directive-command nil "t" :Command #k"Control-t" "Scribe")
(add-scribe-directive-command nil "g" :Command #k"Control-a" "Scribe")
(add-scribe-directive-command nil "un" :Command #k"Control-n" "Scribe")
(add-scribe-directive-command nil "ux" :Command #k"Control-x" "Scribe")
(add-scribe-directive-command nil "c" :Command #k"Control-k" "Scribe")


;;;; Scribe paragraph delimiter function.

(defevar "Paragraph Delimiter Function"
  "Scribe Mode's way of delimiting paragraphs."
  :mode "Scribe"
  :value 'scribe-delim-para-function)

(defun scribe-delim-para-function (mark)
  "Returns whether there is a paragraph delimiting Scribe command on the
   current line.  Add or remove commands for this purpose with the command
   `Add Scribe Paragraph Delimiter'."
  (let ((next-char (next-character mark)))
    (when (paragraph-delimiter-attribute-p next-char)
      (if (eq (character-attribute :scribe-syntax next-char) :escape)
	  (with-mark ((begin mark)
		      (end mark))
	    (mark-after begin)
	    (if (scan-char end :scribe-syntax (or :space :newline :open-paren))
		(gethash (nstring-downcase (region-to-string (region begin end)))
			 *scribe-para-break-table*)
		(editor-error "Failed to find Scribe command ending.")))
	  t))))


;;;; Bracket matching.

(defun scribe-insert-paren (mark bracket-char)
  (insert-character mark bracket-char)
  (with-mark ((m mark))
    (if (balance-paren m)
	(when (value paren-pause-period)
	  (unless (show-mark m (current-window) (value paren-pause-period))
	    (clear-echo-area)
	    (message "~A" (line-string (mark-line m)))))
	(editor-error "Failed to match paren."))))

;;; BALANCE-PAREN moves the mark to the matching open paren character, or
;;; returns nil.  The mark must be after the closing paren.
;;;
(defun balance-paren (mark)
  (with-mark ((m mark))
    (when (rev-scan-char m :scribe-syntax (or :open-paren :close-paren))
      (mark-before m)
      (let ((paren-count 1)
	    (first-paren (next-character m)))
	(loop
	  (unless (rev-scan-char m :scribe-syntax (or :open-paren :close-paren))
	    (return nil))
	  (if (test-char (previous-character m) :scribe-syntax :open-paren)
	      (setq paren-count (1- paren-count))
	      (setq paren-count (1+ paren-count)))
	  (when (< paren-count 0) (return nil))
	  (when (= paren-count 0)
	    ;; OPPOSING-BRACKET calls VALUE (each time around the loop)
	    (cond ((char= (opposing-bracket (previous-character m)) first-paren)
		   (mark-before (move-mark mark m))
		   (return t))
		  (t (editor-error "Scribe paren mismatch."))))
	  (mark-before m))))))
