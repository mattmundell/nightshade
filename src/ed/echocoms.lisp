;;; Echo area commands.

(in-package "ED")

(defevar "Raise Echo Area When Modified"
  "When set, the editor raises the echo area window when output appears
   there.")

(defevar "Message Pause" "The number of seconds to pause after a Message."
  :value 0.5s0)

#[ Control of Parsing Behavior

{evariable:Beep On Ambiguity}
{evariable:Help On Ambiguity}
]#

(defevar "Beep on Ambiguity"
  "If true, beep when completion of a parse is ambiguous."
  :value t)

(defevar "Help on Ambiguity"
  "If true, help when completion of a parse is ambiguous.")

(defevar "Ignore File Types"
  "A list of file types (i.e. extensions), represented as a string without
   the dot, e.g. \"fasl\".  Files having any of the specified types will be
   skipped for completion purposes, making an unique completion more
   likely.  The initial value contains most common binary and output file
   types."
  :value
  (list "fasl" "pmaxf" "sparcf" "rtf" "hpf" "axpf" "sgif" "err"
	"x86f" "lbytef"	"core" "trace"	    ; Lisp
	"BAK" "CKP"			    ; Backups & Checkpoints
	"PS" "ps" "press" "otl" "dvi" "toc" ; Formatting
	"bbl" "lof" "idx" "lot" "aux"	    ; Formatting
	"mo" "elc"			    ; Other editors
	"bin" "lbin"			    ; Obvious binary extensions.
	"o" "a" "aout" "out"		    ; Unixy stuff
	"bm" "onx" "snf"		    ; X stuff
	"UU" "uu" "arc" "Z" "gz" "tar"	    ; Binary encoded files
	))

;;; Field separator characters separate fields for TOPS-20 ^F style
;;; completion.
;;;
(defattribute "Parse Field Separator"
  "A value of 1 for this attribute indicates that the corresponding character
  should be considered to be a field separator by the prompting commands.")
(setf (character-attribute :parse-field-separator #\space) 1)


#[ Some Echo Area Commands

These are some of the `Echo Area' commands that coordinate with the
prompting routines.  The editor binds other commands specific to the `Echo
Area', but they are uninteresting to mention here, such as deleting to the
beginning of the line or deleting backwards a word.

{command:Help On Parse}
{command:Complete Keyword}
{command:Complete Field}
{command:Confirm Parse}
]#

;;; Find-All-Completions  --  Internal
;;;
;;; Return as a list all the possible completions of String in the list of
;;; string-tables Tables.
;;;
(defun find-all-completions (string tables)
  (do ((table tables (cdr table))
       (res ()
	    (merge 'list (find-ambiguous string (car table))
		   res #'string-lessp)))
      ((null table) res)))

(defcommand "Help on Parse" ()
  "Display the help text for the parse currently in progress.  If there are
   a limited number of options then display them."
  "Display the *parse-help* and any possibly completions of the current
   input."
  (let ((help (typecase *parse-help*
		(list (or *parse-help* (error "There is no parse help."))
		      (apply #'format nil *parse-help*))
		(string *parse-help*)
		(t (error "Parse help is not a string or list: ~S" *parse-help*))))
	(input (region-to-string *parse-input-region*)))
    (cond
     ((eq *parse-type* :keyword)
      (let ((strings (find-all-completions input *parse-string-tables*)))
	(with-pop-up-display (s :height (+ (length strings) 2))
	  (write-line help s)
	  (cond (strings
		 (write-line "Possible completions:" s)
		 (dolist (string strings)
		   (write-line string s)))
		(t
		 (write-line
 "There are no possible completions." s))))))
     ((and (eq *parse-type* :file) (not (zerop (length input))))
      (let ((pns (ambiguous-files (region-to-string *parse-input-region*)
				  (or *parse-default* "") #| FIX |#)))
	(declare (list pns))
	(with-pop-up-display (s :height (+ (length pns) 2))
	  (write-line help s)
	  (cond (pns
		 (write-line "Possible completions:" s)
		 (let ((width (- (window-width (current-window)) 27)))
		   (dolist (pn pns)
		     (let* ((dir (directory-namestring pn))
			    (len (length dir)))
		       (unless (<= len width)
			 (let ((slash (position #\/ dir
						:start (+ (- len width) 3))))
			   (setf dir
				 (if slash
				     (concatenate 'string "..."
						  (subseq dir slash))
				     "..."))))
		       (format s " ~A~25T ~A~%"
			       (file-namestring pn) dir)))))
		(t
		 (write-line
 "There are no possible completions." s))))))
     (t
      (with-mark ((m (buffer-start-mark *echo-area-buffer*) :left-inserting))
	(insert-string m help)
	(insert-character m #\newline))))))

(defun file-completion-action (typein)
  (declare (simple-string typein))
  (if (zerop (length typein))
      (editor-error "Parse region is empty."))
  (fi (probe-file (directory-namestring
		   (common-prefix (filter-tildes typein))))
      (when (value help-on-ambiguity)
	(force-output *echo-area-stream*)
	(help-on-parse-command))
      (multiple-value-bind
	  (result win)
	  (complete-file (filter-tildes typein)
			 :directory (if *parse-default* ;; FIX dir-namestring
					(directory-namestring
					 *parse-default*)
					"")
			 :ignore-types (value ignore-file-types))
	(when result
	  (delete-region *parse-input-region*)
	  (insert-string (region-start *parse-input-region*)
			 (namestring
			  (if (and win (directoryp result))
			      (ensure-trailing-slash result)
			      result))))
	(or win
	    (progn
	      (when (value help-on-ambiguity)
		(force-output *echo-area-stream*)
		(help-on-parse-command))
	      (if (and result (value beep-on-ambiguity))
		  (editor-error "Ambiguous.")))))))

(defcommand "Complete Keyword" ()
  "Attempt to complete the text being read in the echo area as a string in
   *parse-string-tables*.  Signal an editor-error if the input is ambiguous
   or incorrect and `Beep on Ambiquity' is true."
  "Complete the keyword being parsed as far as possible.  If it is
   ambiguous and `Beep On Ambiguity' true beep."
  (let ((typein (region-to-string *parse-input-region*)))
    (declare (simple-string typein))
    (case *parse-type*
      (:keyword
       (multiple-value-bind
	   (prefix key value field ambig)
	   (complete-string typein *parse-string-tables*)
	 (declare (ignore value field))
	 (when prefix
	   (delete-region *parse-input-region*)
	   (insert-string (region-start *parse-input-region*) prefix)
	   (when (eq key :ambiguous)
	     (let ((point (current-point)))
	       (move-mark point (region-start *parse-input-region*))
	       (unless (character-offset point ambig)
		 (buffer-end point)))))
	 (when (and (or (eq key :ambiguous) (eq key :complete))
		    (value help-on-ambiguity))
	   (force-output *echo-area-stream*)
	   (help-on-parse-command))
	 (and (or (eq key :ambiguous) (eq key :none))
	      (value beep-on-ambiguity)
	      (editor-error "Ambiguous."))))
      (:file
       (file-completion-action typein))
      (t
       (editor-error "Cannot complete input for this prompt.")))))

(defun field-separator-p (x)
  (plusp (character-attribute :parse-field-separator x)))

(defcommand "Complete Field" (p)
  ;; FIX check
  "Complete a field in a parse.  Fields are separated by characters having
   the :parse-field-separator attribute.  If the field separator is () then
   attempt to complete the entire keyword, otherwise just self-insert."
  "Complete a field in a keyword.  If it is ambiguous and `Beep On
   Ambiguity' is true beep.  Fields are separated by characters having a
   non-zero :parse-field-separator attribute, and this command should only
   be bound to characters having that attribute."
  (let ((typein (region-to-string *parse-input-region*)))
    (declare (simple-string typein))
    (case *parse-type*
      (:string
       (self-insert-command p))
      (:file
       (file-completion-action typein))
      (:keyword
       (let ((last-key-char (ext:key-event-char *last-key-event-typed*)))
	 (let ((point (current-point)))
	   (or (blank-after-p point)
	       (insert-character point last-key-char)))
	 (multiple-value-bind
	     (prefix key value field ambig last-field-p)
	     (complete-string typein *parse-string-tables*)
	   (declare (ignore value))
	   (when (eq key :none)
	     (setq *last-parse-input-string* typein)
	     (editor-error "No possible completion."))
	   (if ambig
	       (progn
		 (when (and (value help-on-ambiguity)
			    (or (string= *last-parse-input-string*
					 typein)
				(eq (value help-on-ambiguity) :always)))
		   (help-on-parse-command))
		 (setq *last-parse-input-string* typein)))
	   (delete-region *parse-input-region*)
	   (let ((new-typein (if (or (and (eq key :unique) (null field))
				     last-field-p)
				 prefix
				 (concatenate 'string
					      prefix
					      (string last-key-char)))))
	     (insert-string (region-start *parse-input-region*) new-typein)
	     (force-output *echo-area-stream*)
	     (when (and (value help-on-ambiguity)
			(or (eq key :complete)
			    (and ambig (string= new-typein typein))))
	       (help-on-parse-command))))))
      (t
       (editor-error "Cannot complete input for this prompt.")))
    (setq *last-parse-input-string* typein)))


(defcommand "Confirm Parse" ()
  ;; FIX check
  "Verify and terminate echo-area input.

   Call `parse-verification-function' with the current input.  If it
   returns a true value then return that as value of the parse.  A parse
   may return () if the verification function returns a true second value."
  "If input has been given call the verification function, otherwise exit
   the recursive edit with the suggested value."
  (let* ((string (region-to-string *parse-input-region*)))
    (declare (simple-string string))
    (if (zerop (length string))
	(when *parse-default* (setq string *parse-default*))
	(progn
	  (if (and (eq *parse-type* :keyword)
		   *parse-value-must-exist*)
	      (multiple-value-bind
		  (prefix key)
		  (complete-string string *parse-string-tables*)
		(or (eq key :none) (setq string prefix))))
	  (if *parse-history*
	      (or (and (plusp (ring-length *parse-history*))
		       (string= string (ring-ref *parse-history* 0)))
		  (ring-push string *parse-history*)))))
    (multiple-value-bind (res flag)
			 (funcall *parse-verification-function* string)
      (if (or res flag) (exit-recursive-edit res)))))

(defcommand "Previous Parse" (p)
  "Rotate the current history forward.

   If the current input is non-empty and different from what is on the top
   of the ring then push it on the ring before inserting the new input."
  "Pop the current history ring buffer."
  (or *parse-history*
      (editor-error "Parse history missing."))
  (let* ((length (ring-length *parse-history*))
	 (p (or p 1)))
    (when (zerop length) (editor-error "Input history empty."))
    (cond
     ((eq (last-command-type) :echo-history)
      (let ((base (mod (+ (symbol-value *parse-history-pointer*) p)
		       length)))
	(delete-region *parse-input-region*)
	(insert-string (region-end *parse-input-region*)
		       (ring-ref *parse-history* base))
	(set *parse-history-pointer* base)))
     (t
      (let ((current (region-to-string *parse-input-region*))
	    (base (mod (if (minusp p) p (1- p)) length)))
	(delete-region *parse-input-region*)
	(insert-string (region-end *parse-input-region*)
		       (ring-ref *parse-history* base))
	(when (and (plusp (length current))
		   (string/= (ring-ref *parse-history* 0) current))
	  (ring-push current *parse-history*)
	  (incf base))
	(set *parse-history-pointer* base))))
    (setf (last-command-type) :echo-history)))

(defcommand "Next Parse" (p)
  "Rotate the current history backward.
   If current input is non-empty and different from what is on the top
   of the ring then push it on the ring before inserting the new input."
  "Push the current history ring buffer."
  (previous-parse-command (- (or p 1))))


(defcommand "Editor Error" ()
  "Signal an editor-error."
  (editor-error "Editor error."))

(add-hook window-buffer-hook
	  #'(lambda (window new-buff)
	      (when (and (eq window *echo-area-window*)
			 (not (eq new-buff *echo-area-buffer*)))
		(editor-error "Can't change echo area window."))))

(defcommand "Beginning Of Parse" ()
  "Moves to immediately after the prompt when in the echo area."
  "Move the point of the echo area buffer to *parse-starting-mark*."
  (move-mark (buffer-point *echo-area-buffer*) *parse-starting-mark*))

(defcommand "Echo Area Delete Previous Character" (p)
  "Delete the previous character, as long as it is after the prompt."
  "Signal an editor-error if the previous character is in the prompt,
   otherwise do a normal delete."
  (with-mark ((tem (buffer-point *echo-area-buffer*)))
    (or (character-offset tem (- (or p 1)))
	(editor-error "Too few characters."))
    (if (mark< tem *parse-starting-mark*)
	(editor-error "Beginning of input area."))
    (delete-previous-character-command p)))

(defcommand "Echo Area Kill Previous Word" (p)
  "Kill as much of the the previous word that is after the prompt."
  "Signal an editor-error if we would mangle the prompt, otherwise
  do a normal kill-previous-word."
  (with-mark ((tem (buffer-point *echo-area-buffer*)))
    (or (word-offset tem (- (or p 1)))
	(editor-error "Too few words."))
    (if (mark< tem *parse-starting-mark*)
	(editor-error "Beginning of input area."))
    (kill-previous-word-command p)))

(proclaim '(special *kill-ring*))

(defcommand "Kill Parse" ()
  "Kills any input so far."
  "Kills *parse-input-region*."
  (if (end-line-p (current-point))
      (kill-region *parse-input-region* :kill-backward)
      (push-kill (delete-and-save-region *parse-input-region*))))

(defcommand "Insert Parse Default" ()
  "Inserts the default for the parse in progress.
  The text is inserted at the point."
  "Inserts *parse-default* at the point of the *echo-area-buffer*.
  If there is no default an editor-error is signalled."
  (if *parse-default*
      (insert-string (buffer-point *echo-area-buffer*)
		     *parse-default*)))

(defcommand "Echo Area Backward Character" (p)
  "Go back one character if the resulting position is after the prompt."
  "Signal an editor-error if we try to go into the prompt, otherwise do a
   backward-character command."
  (backward-character-command p)
  (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
    (beginning-of-parse-command)
    (editor-error "Beginning of input area.")))

(defcommand "Echo Area Backward Word" (p)
  "Go back one word, stopping before the end of the prompt."
  "Signal an editor-error if we try to go into the prompt, otherwise do a
   backward-word command."
  (backward-word-command p)
  (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
    (beginning-of-parse-command)
    (editor-error "Beginning of input area.")))
