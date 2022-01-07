;;; Documentation and help commands.

(in-package "HEMLOCK")


;;;; Help.

(defun switch-to-messages ()
  "Switch to message history buffer."
  (or hi::*message-buffer*
      (setq hi::*message-buffer* (or (getstring "Messages" *buffer-names*)
				     (make-buffer "Messages"))))
  (change-to-buffer hi::*message-buffer*))

(defun edit-editor-todo ()
  "Edit the editor TODO file."
  (let ((file (find-file-buffer "e:TODO")))
    (change-to-buffer file)))

(defcommand "Help" (p)
  "Give helpful information.
  This command dispatches to a number of other documentation commands,
  on the basis of a character command."
  "Prompt for a single character command to dispatch to another helping
  function."
  (declare (ignore p))
  (command-case (:prompt "Doc (help for Help): "
		 :help "Choose a form of Help by typing one of the following:")
    (#\a "List all commands, variables and attributes Apropos a keyword."
     (apropos-command nil))
    (#\b "Describe all the Bindings for the current buffer."
     (describe-bindings-command nil))
    (#\c "Describe a Command, given its name."
     (describe-command-command nil))
    (#\d "Generic Describe, any editor thing (e.g., variable, key, attribute)."
     (generic-describe-command nil))
    (#\e "Switch to message history buffer."
     (switch-to-messages))
    (#\f "Describe a Function."
     (editor-describe-function-command nil))
    (#\i "Switch to the Info buffer."
     (info-command nil))
    (#\k "Describe the command bound to a Character."
     (describe-key-command nil))
    (#\l "List the last 60 characters typed."
     (what-lossage-command nil))
    (#\m "Describe a Mode."
     (describe-mode-command nil))
    (#\o "Describe a Lisp object."
     (editor-describe-command nil))
;     (#\p "Describe commands with mouse/Pointer bindings."
;      (describe-pointer-command nil))
    (#\p "Describe a Person or entity, from the .db database."
     (describe-record-command nil))
    (#\t "Edit the TODO file."
     (edit-editor-todo))
    (#\v "Describe a Variable and show its values."
     (describe-and-show-variable-command nil))
    (#\w "Find out Where a command is bound."
     (where-is-command nil))
    ((#\q :no) "Quit.")))

(defcommand "Where Is" (p)
  "Find what key a command is bound to.
   Prompts for the command to look for, and says what environment it is
   available in."
  "List places where a command is bound."
  (declare (ignore p))
  (multiple-value-bind (nam cmd)
		       (prompt-for-keyword (list *command-names*)
					   :prompt "Command: "
					   :help "Name of command to look for.")
    (let ((bindings (command-bindings cmd)))
      (with-pop-up-display (s)
	(cond
	 ((null bindings)
	  (format s "~S may only be invoked as an extended command.~%" nam))
	 (t
	  (format s "~S may be invoked in the following ways:~%" nam)
	  (print-command-bindings bindings s)))))))



;;;; Apropos.

(defcommand "Apropos" (p)
  "List things whose names contain a keyword."
  "List things whose names contain a keyword."
  (declare (ignore p))
  (let* ((str (prompt-for-string
		:prompt "Apropos keyword: "
		:help
 "String to look for in command, variable and attribute names."))
	 (coms (find-containing str *command-names*))
	 (vars (mapcar #'(lambda (table)
			   (let ((res (find-containing str table)))
			     (if res (cons table res))))
		       (current-variable-tables)))
	 (attr (find-containing str *character-attribute-names*)))
    (if (or coms vars attr)
	(apropos-command-output str coms vars attr)
	(with-pop-up-display (s :height 1)
	  (format s "No command, attribute or variable name contains ~S."
		  str)))))

(defun apropos-command-output (str coms vars attr)
  (declare (list coms vars attr))
  (with-pop-up-display (s)
    (when coms
      (format s "Commands with ~S in their names:~%" str)
      (dolist (com coms)
	(let ((obj (getstring com *command-names*)))
	  (write-string com s)
	  (write-string "   " s)
	  (print-command-bindings (command-bindings obj) s)
	  (terpri s)
	  (print-short-doc (command-documentation obj) s))))
    (when vars
      (when coms (terpri s))
      (format s "Variables with ~S in their names:~%" str)
      (dolist (stuff vars)
	(let ((table (car stuff)))
	  (dolist (var (cdr stuff))
	    (let ((obj (getstring var table)))
	      (write-string var s)
	      (write-string "   " s)
	      (let ((*print-level* 2) (*print-length* 3))
		(prin1 (variable-value obj) s))
	      (terpri s)
	      (print-short-doc (variable-documentation obj) s))))))
    (when attr
      (when (or coms vars) (terpri s))
      (format s "Attributes with ~S in their names:~%" str)
      (dolist (att attr)
	(let ((obj (getstring att *character-attribute-names*)))
	  (write-line att s)
	  (print-short-doc (character-attribute-documentation obj) s))))))

;;; PRINT-SHORT-DOC takes doc, a function or string, and gets it out on stream.
;;; If doc is a string, this only outputs up to the first newline.  All output
;;; is preceded by two spaces.
;;;
(defun print-short-doc (doc stream)
  (let ((str (typecase doc
	       (function (funcall doc :short))
	       (simple-string
		(let ((nl (position #\newline (the simple-string doc))))
		  (subseq doc 0 (or nl (length doc)))))
	       (t
		(error "Bad documentation: ~S" doc)))))
    (write-string "  " stream)
    (write-line str stream)))



;;;; Describe command, key, pointer.

(defcommand "Describe Command" (p)
  "Describe a command.
  Prompts for a command and then prints out its full documentation."
  "Print out the command documentation for a command which is prompted for."
  (declare (ignore p))
  (multiple-value-bind (nam com)
		       (prompt-for-keyword
			(list *command-names*)
			:prompt "Describe command: "
			:help "Name of a command to document.")
    (let ((bindings (command-bindings com)))
      (with-pop-up-display (s :reference
			      (list (fun-defined-from-pathname
				     (command-function com))
				    :function
				    (concatenate 'simple-string
						 nam
						 "-COMMAND")))
	(format s "Documentation for ~S:~%   ~A~%"
		nam (command-documentation com))
	(cond ((not bindings)
	       (write-line
		"This can only be invoked as an extended command." s))
	      (t
	       (write-line
		"This can be invoked in the following ways:" s)
	       (write-string "   " s)
	       (print-command-bindings bindings s)
	       (terpri s)))))))

(defcommand "Describe Key" (p)
  "Prompt for a sequence of characters.  When the first character is typed that
   terminates a key binding in the current context, describe the command bound
   to it.  When the first character is typed that no longer allows a correct
   key to be entered, tell the user that this sequence is not bound to
   anything."
  "Print out the command documentation for a key
  which is prompted for."
  (declare (ignore p))
  (let ((old-window (current-window)))
    (unwind-protect
	(progn
	  (setf (current-window) hi::*echo-area-window*)
	  (hi::display-prompt-nicely "Describe key: " nil)
	  (setf (fill-pointer hi::*prompt-key*) 0)
	  (loop
	    (let ((key-event (get-key-event hi::*editor-input*)))
	      (vector-push-extend key-event hi::*prompt-key*)
	      (let ((res (get-command hi::*prompt-key* :current)))
		(ext:print-pretty-key-event key-event *echo-area-stream*)
		(write-char #\space *echo-area-stream*)
		(cond ((commandp res)
		       (setf (current-window) old-window)
		       (with-pop-up-display
			   (s :reference
			      (list (fun-defined-from-pathname
				     (command-function res))
				    :function
				    (concatenate 'simple-string
						 (command-name res)
						 "-COMMAND")))
			 (print-pretty-key (copy-seq hi::*prompt-key*) s)
			 (format s " is bound to ~S.~%" (command-name res))
			 (format s "Documentation for this command:~%   ~A"
				 (command-documentation res)))
		       (return))
		      ((not (eq res :prefix))
		       (with-pop-up-display (s :height 1)
			 (print-pretty-key (copy-seq hi::*prompt-key*) s)
			 (write-string " is not bound to anything." s))
		       (return)))))))
      (setf (current-window) old-window))))

(defcommand "Describe Pointer" (p)
  "Describe commands with any key binding that contains a \"mouse\" character
   (modified or not).  Does not describe the command \"Illegal\"."
  "Describe commands with any key binding that contains a \"mouse\" character
   (modified or not).  Does not describe the command \"Illegal\"."
  (declare (ignore p))
  (let ((illegal-command (getstring "Illegal" *command-names*)))
    (with-pop-up-display (s)
      (dolist (cmd (get-mouse-commands))
	(unless (eq cmd illegal-command)
	  (format s "Documentation for ~S:~%   ~A~%"
		  (command-name cmd)
		  (command-documentation cmd))
	  (write-line
	   "This can be invoked in the following ways:" s)
	  (write-string "   " s)
	  (print-command-bindings (command-bindings cmd) s)
	  (terpri s) (terpri s))))))

(defun get-mouse-commands ()
  (let ((result nil))
    (do-strings (name cmd *command-names* result)
      (declare (ignore name))
      (dolist (b (command-bindings cmd))
        (let ((key (car b)))
          (declare (simple-vector key))
	  (when (dotimes (i (length key) nil)
		  (when (member (ext:make-key-event (svref key i))
				(list #k"Leftdown" #k"Leftup" #k"Middledown"
				      #k"Middleup" #k"Rightdown" #k"Rightup"))
		    (push cmd result)
		    (return t)))
	    (return)))))))



;;;; Generic describe variable, command, key, attribute.

(defvar *generic-describe-kinds*
  (list (make-string-table :initial-contents
			   '(("Variable" . :variable)
			     ("Command" . :command)
			     ("Key" . :key)
			     ("Attribute" . :attribute)))))

(defcommand "Generic Describe" (p)
  "Describe some editor thing.
  First prompt for the kind of thing, then prompt for the thing to describe.
  Currently supported kinds of things are variables, commands, keys and
  character attributes."
  "Prompt for some editor thing to describe."
  (declare (ignore p))
  (multiple-value-bind (ignore kwd)
		       (prompt-for-keyword *generic-describe-kinds*
					   :default "Variable"
					   :help "Kind of thing to describe."
					   :prompt "Kind: ")
    (declare (ignore ignore))
    (case kwd
      (:variable
       (describe-and-show-variable-command nil))
      (:command (describe-command-command ()))
      (:key (describe-key-command ()))
      (:attribute
       (multiple-value-bind (name attr)
			    (prompt-for-keyword
			     (list *character-attribute-names*)
			     :help "Name of character attribute to describe."
			     :prompt "Attribute: ")
	 (print-full-doc name (character-attribute-documentation attr)))))))

;;; PRINT-FULL-DOC displays whole documentation string in a pop-up window.
;;; Doc may be a function that takes at least one arg, :short or :full.
;;;
(defun print-full-doc (nam doc)
  (typecase doc
    (function (funcall doc :full))
    (simple-string
     (with-pop-up-display (s)
       (format s "Documentation for ~S:~%  ~A" nam doc)))
    (t (error "Bad documentation: ~S" doc))))



;;;; Describing and show variables.

(defcommand "Show Variable" (p)
  "Display the values of an editor variable."
  "Display the values of an editor variable."
  (declare (ignore p))
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:help "Name of variable to describe."
			:prompt "Variable: ")
    (with-pop-up-display (s)
      (show-variable s name var))))

(defcommand "Describe and Show Variable" (p)
  "Describe in full and show all of variable's value.
   Variable is prompted for."
  "Describe in full and show all of variable's value."
  (declare (ignore p))
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:help "Name of variable to describe."
			:prompt "Variable: ")
    (with-pop-up-display (s)
      (format s "Documentation for ~S:~%  ~A~&~%"
	      name (variable-documentation var))
      (show-variable s name var))))

(defun show-variable (s name var)
  (when (editor-bound-p var :global)
    (format s "Global value of ~S:~%  ~S~%"
	    name (variable-value var :global)))
  (let ((buffer (current-buffer)))
    (when (editor-bound-p var :buffer (current-buffer))
      (format s "Value of ~S in buffer ~A:~%  ~S~%"
	      name (buffer-name buffer)
	      (variable-value var :buffer buffer))))
  (do-strings (mode-name val *mode-names*)
    (declare (ignore val))
    (when (editor-bound-p var :mode mode-name)
      (format s "Value of ~S in ~S Mode:~%  ~S~%"
	      name mode-name
	      (variable-value var :mode mode-name)))))



;;;; Describing modes.

(defvar *describe-mode-ignore* (list "Illegal" "Do Nothing"))

(defcommand "Describe Mode" (p &optional name)
  "Describe a mode showing special bindings for that mode."
  "Describe a mode showing special bindings for that mode."
  (declare (ignore p))
  (let ((name (or name
		  (prompt-for-keyword (list *mode-names*)
				      :prompt "Mode: "
				      :help "Enter mode to describe."
				      :default
				      (car (buffer-modes (current-buffer)))))))
    (with-pop-up-display (*stdout*)
      (format *stdout* "~A mode description:~%" name)
      (let ((doc (mode-documentation name)))
	(when doc
	  (write-line doc *stdout*)
	  (terpri *stdout*)))
      (map-bindings #'print-binding-doc :mode name))))

(defun key-to-string (key)
  (with-output-to-string (s)
    (print-pretty-key key s)))

(defun print-binding-doc (key cmd)
  (or (member (command-name cmd)
	      *describe-mode-ignore*
	      :test #'string-equal)
      (let ((str (key-to-string key)))
	(cond ((= (length str) 1)
	       (write-string str *stdout*)
	       (write-string "  - " *stdout*))
	      (t (write-line str *stdout*)
		 (write-string "   - " *stdout*)))
	(print-short-doc (command-documentation cmd) *stdout*))))

(defun print-binding (key cmd)
  (or (member (command-name cmd)
	      *describe-mode-ignore*
	      :test #'string-equal)
      (let ((str (key-to-string key)))
	(cond ((< (length str) 15)
	       (format *stdout* "~15A" str))
	      (t (write-line str *stdout*)
		 (write-string "               " *stdout*)))
	(write-line (command-name cmd) *stdout*))))

(defcommand "Describe Bindings" (p &optional (buffer (current-buffer)))
  "Describe all current bindings."
  "Describe all current bindings."
  (declare (ignore p))
  (with-pop-up-display (*stdout*)
    (let ((name (buffer-major-mode buffer)))
      (format *stdout* "~A major mode:~%" name)
      (map-bindings #'print-binding :mode name)
      (terpri *stdout*))
    ;; FIX maybe order by precedence?
    (do-strings (name value *mode-names*)
      (declare (ignore value))
      (or (mode-major-p name)
	  (when (buffer-minor-mode buffer name)
	    (format *stdout* "~A minor mode:~%" name)
	    (map-bindings #'print-binding :mode name)
	    (terpri *stdout*))))
    (format *stdout* "Global bindings:~%")
    (map-bindings #'print-binding :global)
    (terpri *stdout*)))


;;;; Printing bindings and last N characters typed.

(defcommand "What Lossage" (p)
  "Display the last 60 characters typed."
  "Display the last 60 characters typed."
  (declare (ignore p))
  (with-pop-up-display (s :height 7)
    (let ((num (ring-length *key-event-history*)))
      (format s "The last ~D characters typed:~%" num)
      (do ((i (1- num) (1- i)))
	  ((minusp i))
	(ext:print-pretty-key-event (ring-ref *key-event-history* i) s)
	(write-char #\space s)))))

(defun print-command-bindings (bindings stream)
  (let ((buffer ())
	(mode ())
	(global ()))
    (dolist (b bindings)
      (case (second b)
	(:global (push (first b) global))
	(:mode
	 (let ((m (assoc (third b) mode :test #'string=)))
	   (if m
	       (push (first b) (cdr m))
	       (push (list (third b) (first b)) mode))))
	(t
	 (let ((f (assq (third b) buffer)))
	   (if f
	       (push (first b) (cdr f))
	       (push (list (third b) (first b)) buffer))))))
    (when global
      (print-some-keys global stream)
      (write-string "; " stream))
    (dolist (b buffer)
      (format stream "Buffer ~S: " (buffer-name (car b)))
      (print-some-keys (cdr b) stream)
      (write-string "; " stream))
    (dolist (m mode)
      (write-string (car m) stream)
      (write-string ": " stream)
      (print-some-keys (cdr m) stream)
      (write-string "; " stream))))

;;; PRINT-SOME-KEYS prints the list of keys onto Stream.
;;;
(defun print-some-keys (keys stream)
  (do ((key keys (cdr key)))
      ((null (cdr key))
       (print-pretty-key (car key) stream))
    (print-pretty-key (car key) stream)
    (write-string ", " stream)))
