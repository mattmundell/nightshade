;;; Documentation and help commands.

(in-package "ED")


#[ Online Help

The editor has a fairly good online documentation facility.  You can get
brief documentation for every command, variable, character attribute, and
key by typing a key.

{command:Help}
{command:Apropos}
{command:Describe Command}
{command:Describe Key}
{command:Describe Mode}
{command:Show Variable}
{command:Describe and Show Variable}
{command:What Lossage}
{command:Describe Pointer}
{command:Where Is}
{command:Generic Describe}
{command:Welcome}
]#


;;;; Welcome.

(defevar "Show Welcome on Start"
  "If true, show the welcome message when first entering the editor."
  :value t)

(defcommand "Welcome" ()
  "Echo the welcome message."
  (info-command () "Welcome"))


;;;; Help.

(defun switch-to-messages ()
  "Switch to message history buffer."
  (or edi::*message-buffer*
      (setq edi::*message-buffer* (or (getstring "Messages" *buffer-names*)
				      (make-buffer "Messages"))))
  (change-to-buffer edi::*message-buffer*))

(defcommand "Edit TODO" ()
  "Edit the TODO file."
  (let ((file (find-file-buffer "etc:TODO")))
    (change-to-buffer file)))

(defcommand "Tutorial" ()
  "Switch to the tutorial documentation node."
  (info-command)
  (show-node "Editor Tutorial"))

;; FIX these should just be bindings
(defcommand "Help" (p)
  "Invoke a prompted help command.  The following are valid responses at the prompt:

     a  list all commands, variables and attributes Apropos (related to) a keyword.

     b  describe all the Bindings for the current buffer.

     c  describe a prompted Command.

     d  switch to the Documentation system.

     e  switch to Editor message history.

     f  describe a Function.

     i  switch to the GNU Info directory.

     k  describe the command bound to a Character.

     l  list the last sixty key-events typed.

     m  describe a mode.

     o  describe a Lisp Object.

     p  describe a Package.

     r  describe the Record for a person or entity, from the contacts database.

     t  edit the TODO file.

     u  view a Unix manual page.

     v  describe a Variable and show its values.

     w  list Where a command is bound.

     q  Quit.

     Home, C-_, ?, h
	list all of the options and what they do.

   With a prefix pass the prefix on to the associated command if the key is
   associated with a command."
  (command-case (:prompt "Doc (type h for options): "
		 :help "Choose a form of Help by typing one of the following:")
    (#\a "list all commands, variables and attributes Apropos (About) a keyword."
     (apropos-command p))
    (#\b "describe all the Bindings for the current buffer."
     (describe-bindings-command p))
    (#\c "describe a Command, given its name."
     (describe-command-command p))
    (#\d "switch to the Documentation system."
     (info-command p))
;   (#\d "generic Describe, any editor thing (e.g. variable, key, attribute)."
;    (generic-describe-command p))
    (#\e "describe an Editor variable and show its values."
     (describe-and-show-variable-command p))
    (#\f "describe a Function."
     (editor-describe-function-command p))
    (#\h "display this help message."
     (help "Choose a form of help by typing one of:")
     (reprompt))
    (#\i "switch to the GNU Info directory buffer."
     (ginfo-command p))
;   (#\j )
    (#\k "describe the command bound to a Character."
     (describe-key-command p))
    (#\l "list the Last 60 characters typed."
     (what-lossage-command p))
    (#\m "describe a Mode."
     (describe-mode-command p))
;   (#\n exit)
    (#\o "describe a Lisp object."
     (editor-describe-command p))
;   (#\p "Describe commands with mouse/Pointer bindings."
;    (describe-pointer-command p))
    (#\p "describe a Package."
     (describe-package-command p))
;   (#\q quit, below)
    (#\r "describe the Record for a person or entity, from the .db database."
     (describe-record-command p))
    (#\s "Switch to the message history buffer."
     (switch-to-messages))
    (#\t "enter the Tutorial."
     (tutorial-command))
    (#\u "view a Unix Manual page."
     (manual-page-command p))
    (#\v "describe a Variable and show its values."
     (editor-describe-variable-command p))
    (#\w "list Where a command is bound."
     (where-is-command p))
;   (#\x )
;   (#\y )
;   (#\z )
    ((#\q :no) "Quit.")))

(defcommand "Where Is" ()
  "List the keys that a prompted command is bound to, including what
   environment the binding is available in."
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
  "Print brief documentation for all commands, variables, and character
   attributes whose names match a prompted string.  Perform a prefix match
   on each supplied word separately, intersecting the names in each word's
   result.  For example, given \"f m\" tersely describe the following
   commands and variables:

  - `Auto Fill Mode'

  - `Fundamental Mode'

  - `Mark Form'

  - `Default Modeline Fields'

  - `Fill Mode Hook'

  - `Fundamental Mode Hook'

   Notice `Mark Form' demonstrates that the words may come in any order.

   The bindings of commands and values of variables are printed with the
   documentation."
  (if p
      (system-apropos-command)
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
		      str))))))

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

(defcommand "Describe Command" ()
  "Describe a prompted command.  Prints the full command documentation and
   all the keys bound to it."
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

(defcommand "Describe Key" ()
  "Prompt for a sequence of characters.  When the first character is typed that
   terminates a key binding in the current context, describe the command bound
   to it.  When the first character is typed that no longer allows a correct
   key to be entered, tell the user that this sequence is not bound to
   anything."
  (let ((old-window (current-window)))
    (unwind-protect
	(progn
	  (setf (current-window) edi::*echo-area-window*)
	  (edi::display-prompt-nicely "Describe key: " nil)
	  (setf (fill-pointer edi::*prompt-key*) 0)
	  (loop
	    (let ((key-event (get-key-event edi::*editor-input*)))
	      (vector-push-extend key-event edi::*prompt-key*)
	      (let ((res (get-command edi::*prompt-key* :current)))
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
			 (print-pretty-key (copy-seq edi::*prompt-key*) s)
			 (format s " is bound to ~S.~%" (command-name res))
			 (format s "Documentation for this command:~%   ~A"
				 (command-documentation res)))
		       (return))
		      ((not (eq res :prefix))
		       (with-pop-up-display (s :height 1)
			 (print-pretty-key (copy-seq edi::*prompt-key*) s)
			 (write-string " is not bound to anything." s))
		       (return)))))))
      (setf (current-window) old-window))))

(defcommand "Describe Pointer" ()
  "Describe commands with any key binding that contains a mouse/pointer
   key-event.  Skip the command `Editor Error'."
  (let ((error-command (getstring "Editor Error" *command-names*)))
    (with-pop-up-display (s)
      (dolist (cmd (get-mouse-commands))
	(unless (eq cmd error-command)
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

(defcommand "Generic Describe" ()
  "Print full documentation for any editor thing that has documentation.
   It first prompts for the kind of the thing, the following options being
   available:

  attribute
     Describe a character attribute, given its name.

  command
     Describe a command, given its name.

  key
     Describe a command, given a key to which it is bound.

  variable
     Describe a variable, given its name.  This is the default."
  (multiple-value-bind (ignore kwd)
		       (prompt-for-keyword *generic-describe-kinds*
					   :default "Variable"
					   :help "Kind of thing to describe."
					   :prompt "Kind: ")
    (declare (ignore ignore))
    (case kwd
      (:variable
       (describe-and-show-variable-command))
      (:command (describe-command-command))
      (:key (describe-key-command))
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


;;;; Describing symbols.

(defcommand "Find All Symbols" ()
  "Display the values of an editor variable."
  (let* ((name (string-upcase (prompt-for-string
			       :prompt "Symbol: "
			       :default (word-at-point))))
	 (symbols (find-all-symbols name)))
    (or symbols	(editor-error "Failed to find any symbol named ~A." name))
    (message "Found in:~A"
	     (with-output-to-string (out)
	       (dolist (symbol symbols)
		 (format out " ~A" (package-name (symbol-package symbol))))))))


;;;; Describing and showing variables.

(defcommand "Show Variable" ()
  "Prompt for the name of a variable and display the global value of the
   variable, the value local to the current buffer (if any), and the value
   of the variable in all defined modes that have it as a local variable."
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:help "Name of variable to describe."
			:prompt "Variable: ")
    (with-pop-up-display (s)
      (show-variable s name var))))

(defcommand "Describe and Show Variable" ()
  "Prompt for the name of a variable and display the variable's
   documentation, as well as the global value of the variable, the value
   local to the current buffer (if any), and the value of the variable in
   all defined modes that have it as a local variable."
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:help "Name of variable to describe."
			:prompt "Editor Variable: ")
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

(defcommand "Describe Mode" (p name)
  "Describe a mode, showing special bindings for that mode."
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

(defcommand "Describe Bindings" (p (buffer (current-buffer)))
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


;;;; Describing packages.

(defun describe-package (package stream)
  (let ((nicks (package-nicknames package))
	(package-name (package-name package))
	(doc (lisp::package-doc-string package))
	(istream (make-indenting-stream stream))
	(value-width (- (value fill-column) 20)))
    (format stream "The ~A package~%~%" package-name)
    (when doc
      (write-string "    " istream)
      (setf (lisp::indenting-stream-indentation istream) 4)
      ;; FIX why does it indent blank lines?
      (write-string doc istream)
      (terpri stream)
      (terpri stream))
    ;; FIX for all of these write only to width (use format?)
    (let ((*print-right-margin* (value fill-column))
	  (*print-miser-width* 10))
      (format stream "         Nicknames:")
      (write-string (mapconcat #'identity nicks " " t) stream)
      (terpri stream)
      (format stream "              Uses:")
      (flet ((string-or-name (package)
	       (if (packagep package)
		   (package-name package)
		   package)))
	(write-string (mapconcat #'string-or-name (package-use-list package) " " t)
		      stream)
	(terpri stream)
	(format stream "           Used by:")
	(write-string (mapconcat #'string-or-name (package-used-by-list package) " " t)
		      stream)))
    (terpri stream)
    (format stream "           Shadows:")
    (write-string (mapconcat #'symbol-name
			     (package-shadowing-symbols package)
			     " " t)
		  stream)
    (terpri stream)
    (multiple-value-bind (iu it) (lisp::internal-symbol-count package)
      (multiple-value-bind (eu et) (lisp::external-symbol-count package)
	(format stream
		"  Internal symbols: ~D/~D~%  External symbols: ~D/~D~%"
		iu it eu et)))
    ;; FIX how to tell if var special?
    ;; FIX var vs fun needs work
    (format stream "Exported variables:")
    (let ((width 0))
      (do-external-symbols (symbol package)
	(if (boundp symbol)
	    (let ((len (length (symbol-name symbol))))
	      (incf width (1+ len))
	      (when (> width value-width)
		(format stream "~%                   ")
		(setq width len))
	      (format stream " ~A" symbol)))))
    (terpri stream)
    (format stream "Exported functions:")
    (let ((width 0))
      (do-external-symbols (symbol package)
	(or (boundp symbol)
	    (let ((len (length (symbol-name symbol))))
	      (incf width (1+ len))
	      (when (> width value-width)
		(format stream "~%                   ")
		(setq width len))
	      (format stream " ~A" symbol)))))
    (terpri stream)))

(defun describe-package-from-meta (package stream)
  "Describe package from meta-inforation $package, to $stream."
  (let ((nicks (lisp::pkg-info-nicknames package))
	(package-name (lisp::pkg-info-name package))
	(doc (lisp::pkg-info-doc package))
	(istream (make-indenting-stream stream))
	;(value-width (- (value fill-column) 20))
	)
    (format stream "The ~A package~%~%" package-name)
    (when doc
      (write-string "    " istream)
      (setf (lisp::indenting-stream-indentation istream) 4)
      ;; FIX why does it indent blank lines?
      (write-string doc istream)
      (terpri stream)
      (terpri stream))
    ;; FIX for all of these write only to width (use format?)
    (let ((*print-right-margin* (value fill-column))
	  (*print-miser-width* 10))
      (format stream "         Nicknames:")
      (write-string (mapconcat #'identity nicks " " t) stream)
      (terpri stream)
      (format stream "              Uses:")
      (flet ((string-or-name (package)
	       (if (packagep package)
		   (package-name package)
		   package)))
	(write-string (mapconcat #'string-or-name
				 (lisp::pkg-info-use package)
				 " " t)
		      stream)
	(terpri stream)))
    (format stream "           Shadows:")
    (write-string (mapconcat #'symbol-name
			     (lisp::pkg-info-shadows package)
			     " " t)
		  stream)
    (terpri stream)
#|
    (multiple-value-bind (iu it) (lisp::internal-symbol-count package)
      (multiple-value-bind (eu et) (lisp::external-symbol-count package)
	(format stream
		"  Internal symbols: ~D/~D~%  External symbols: ~D/~D~%"
		iu it eu et)))
    ;; FIX how to tell if var special?
    ;; FIX var vs fun needs work
    (format stream "Exported variables:")
    (let ((width 0))
      (do-external-symbols (symbol package)
	(if (boundp symbol)
	    (let ((len (length (symbol-name symbol))))
	      (incf width (1+ len))
	      (when (> width value-width)
		(format stream "~%                   ")
		(setq width len))
	      (format stream " ~A" symbol)))))
    (terpri stream)
    (format stream "Exported functions:")
    (let ((width 0))
      (do-external-symbols (symbol package)
	(or (boundp symbol)
	    (let ((len (length (symbol-name symbol))))
	      (incf width (1+ len))
	      (when (> width value-width)
		(format stream "~%                   ")
		(setq width len))
	      (format stream " ~A" symbol)))))
    (terpri stream)
|#
    ))

(defun table-all-packages ()
  "Return a string table of the names of all packages."
  (let ((table (make-string-table)))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (setf (getstring (package-name value) table) value))
	     lisp::*package-names*)
    table))

(defcommand "Describe Package" ()
  "Describe a prompted package."
  (let ((package (prompt-for-keyword (list (table-all-packages))
				     ;; FIX why always cl?
				     :default (package-name (lisp::current-package))
				     :prompt "Describe package: "
				     :help "Name of package to describe.")))
    (with-pop-up-display (stream)
      (describe-package (find-package package) stream))))


;;;; Printing bindings and last N characters typed.

;; FIX rename  Show Last Keys?
(defcommand "What Lossage" ()
  "Display the last sixty key-events typed.  This can be useful if, for
   example, for find the bindings for a command typed by accident."
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


;;;; Messages.

(defmode "Messages" :major-p t)


#[ Helpful Information

This section contains assorted helpful information which may be useful in
staying out of trouble or getting out of trouble.


  - It is possible to get some sort of help nearly everywhere by typing
     Home or C-_.

  - Various commands take over the keyboard and insist that you type the key-events
     that they want as input.  If you get in such a situation and want to get out,
     you can usually do so by typing C-g some small number of times.  If this
     fails you can try typing C-x C-z to exit the editor and then "(ed)"
     to re-enter it.

  - Before you quit, make sure you have saved all your changes.  C-u C-x C-b
     will display a list of all modified buffers.  If you exit using C-x
     C-z, then the editor will save all modified buffers with associated files.

  - If you lose changes to a file due to a crash or accidental failure to save,
     look for backup ("file.BAK") or checkpoint ("file.CKP") files
     in the same directory where the file was.

  - If the screen changes unexpectedly, you may have accidentally typed an
     incorrect command.  Use Home l to see what it was.  If you are
     not familiar with the command, use Home c to see what it is so that
     you know what damage has been done.  Many interesting commands can be found
     in this fashion.

  - If you accidentally type a "killing" command such as C-w, you can
     get the lost text back using C-y.  The `Undo' command is also
     useful for recovering from this sort of problem.

{evariable:Region Query Size}
{command:Undo}
]#


#[ Welcome

Welcome to the Nightshade text editor.

Some useful commands:

    control-x control-z    Save All Files and Exit

        ?  "control-x control-z" means press "x" while holding down
           "control", then press "z" while holding down "control".

    control-h              Help
    control-h t            Tutorial
    control-h d            Info

    control-x control-f    Find File  (open a file)

This buffer is part of the documentation system, so type

    q  to  drop the buffer, exiting this message,
    d  to  go to the [documentation] contents,
    i  to  summon the index prompt, and
    e  to  edit this text.
]#
#|

To turn off this message, clear *Show Welcome on Start*, for example by
adding

    (setv show-welcome-on-start ())

to the file conf:ed.lisp.
|#


#[ Editor Tutorial

This tutorial introduces the editor to new users.  It is intended to be
read from inside the editor.  It is accessible at any time via "control-h
t", which runs the `Tutorial' command.

Typing a space moves a screenfull forward in this text and typing a
backspace moves a screenfull backward.  It may be a good idea to try those
now, to practice moving around.

The idea of this tutorial is to try out the commands while reading about
them.  The editor is much easier to use after a bit of background, so it is
usually a good idea to go through the whole tutorial.

== Buffers of Text ==

This text, like most text in the editor, is in a buffer.  Using the editor
involves moving around within buffers, and reading or modifying the text in
the buffers.

Every buffer has a special mark, called a point, which is the current focus
of attention.  The highlighted character in the tutorial shows the position
of the point in the tutorial buffer.

== A note on European keyboards ==

On many American keyboards there are two Alt keys, one on each side of the
space bar.  The editor key bindings depend on these two keys to be
effective.  On European keyboards the equivalent of the right hand Alt key
is usually AltGr.  It is recommended to bind the AltGr key to Alt on these
keyboards, as described in [Local Setup].

== Commands ==

Commands act on the editor.  Typing "meta-x", then typing the name of the
command, and then typing the return key, will invoke a command.  "meta-x"
means to press the "x" key while holding down the Alt key.  Similar key
sequences are mentioned throughout this tutorial, for example, "control-x
l" means to press "x" while holding down a control key (the key usually
labeled Ctrl), then to release both keys and then to type the "l" key.

At any time, "control-g" cancels a command.  FIX try "meta-x" "control-g".

As an example of a command, entering the key sequence "meta-x f o r w a r d
space c return" invokes the `Forward Character' command.  Typing that key
sequence now will move the point one character forward.

Only part of the `Forward Character' command name needed to be typed
because it is the only command that begins with "forward ch", so the editor
automatically fills in the rest.

Commands that are likely to be typed often are "bound" to shorter key
strokes.  For example, the `Forward Character' command used above is bound
to "control-f".  Typing "control-f" now will move the point one character
forward, as if the `Forward Character' command had been entered directly.

The "f" in "control-f" is for "Forward".  The letters in most key bindings
are chosen to match the name of the associated command, to ease remembering
the bindings.

Typing "control-b" will run the command `Backward Character', which moves
the point backward one character.

== Modes ==

Every buffer is in one or more modes.  The modes determine which command is
associated with which key sequence.  For example, this buffer is in View
mode, which is why typing "space" or "backspace" moves forward or backwards
in the text.

The most basic mode is called Fundamental mode.  The editor initially puts
newly created buffers in Fundamental mode.

== Windows ==

A window is a view onto a buffer.  Every window contains a buffer.  At the
bottom of every window is a modeline, a line of information about the
buffer in the window.  For example, the modeline of the tutorial window
shows the list of modes for the tutorial buffer, which should include Info
and View.

The window at the very bottom of the editor is the Echo Area window, which
is always present and is always positioned at the bottom.  It is a special
window, only being used to display messages and prompt for input.

The modeline of the Echo Area window is called the Status Line.  The Status
Line shows general information, such as the date and the name of the
computer.

Windows in the editor are somewhat similar to the windows common in
windowing systems such as X Windows.  However, all editor windows are
always entirely visible within the editor, whereas in other windowing
systems the windows usually overlap.  The use of the word "window" is from
the same usage in Emacs, which predates the overlapping use in the more
recent windowing systems.

== More Point Movement Commands ==

Typing "control-l" centers the current point in the window.

Typing "meta-f" moves forward one word, while typing "meta-b" moves
backward one word.  These letters used in these bindings are similar to the
character movement bindings, "control-f" and "control-b", to ease learning
and remembering them.

Typing "control-n" moves to the next line of the buffer.

If the text is too long for the window, the line of text is wrapped across
two lines of the screen.  In this case the line movement commands move over
both of the screen lines.

Typing "control-p" moves to the previous line of the buffer.

Typing "control-a" moves to the beginning of the current line, while
"control-e" moves to the end of the current line.  The "a" in "control-a"
can be remembered by its position at the beginning of the row on a qwerty
keyboard.

Typing "control-v" moves down one screenfull of text, and typing "meta-v"
moves up one screenfull.

The arrow keys and the page up and page down keys can also be used to move
around.  However, it is worth learning the control and meta versions when
doing any substantial amount of work in the editor, as these keys are much
faster.

== Creating and Killing Buffers ==

Typing "control-x k" runs the command `Kill Buffer', which prompts for a
buffer and then kills that buffer.  The prompt suggests the current buffer.
Entering an empty value (by pressing "return") accepts the suggestion.  FIX
example

Typing "control-x meta-control-b" runs the command `Create Buffer', which
prompts for a buffer, then creates the buffer and switches to the new
buffer in the current window..

== Multiple Windows ==

The editor can be split into more windows than the initial two.

Typing "control-x 2" runs `Split Window', which splits the current window
into two windows.

Typing "control-x 0" runs `Delete Window', which closes the current window.
If the current window is the last window then an error message is printed
in the Echo Area.

Typing "control-o" runs `Next Window', which switches the focus to the next
window.  The next window is the one below the current one, or the one at
the top if the current one is the bottom window.  As a shortcut, if there
is only one window then `Next Window' splits that window, as if `Split
Window' has been called.

Typing "control-x 1" runs `Go to One Window', which makes the current
window the sole window.

== Switching Buffers ==

Each window contains a buffer.  There are a few ways to switch the buffer
between in a window to another buffer.

The `Switch to Buffer' command prompts for the name of a buffer, and then
swaps the buffer in the current window to the given buffer.  Typing
"control-x b" invokes `Switch to Buffer'.  The prompt suggests a value in
square brackets.  Entering an empty value with "return" accepts the
suggestion.

The `Bufed' command switches to an editable list of all existing buffers.
It may be better now to split the window with "control-o" and then to type
"meta-x b u f e d" to run `Bufed', so that the tutorial can be read at the
same time.  As mentioned above "control-o" will move the focus between

While in the Bufed buffer, pressing "enter" switches to the buffer listed
on the current line.  Typing "control-n" and "control-p" moves to the next
and previous line of the buffer list, as usual, and "space" an "delete"
move a screenfull up and down.

When working with many buffers it is often useful to rotate between
multiple buffers.  For this the `Rotate Buffers Foward' and `Rotate Buffers
Backward' commands were created.  These commands affect the list of all
existing buffers.  Typing "control-x control-b" runs `Rotate Buffers
Forward', which moves the front of the list, the current buffer, to the end
of the list.  Typing "control-x control-v" runs `Rotate Buffers Backward',
which does the reverse, moving the buffer at the end of the list to the
front of the list, making it the current buffer.

== Modifying Text ==

Now the commands that modify the text in a buffer.  To try these commands
split the window (with "control-o") and switch the buffer in one of the
windows to the "Main" buffer (with "control-x b m a i n return"), then try
the commands with the focus in the "Main" buffer.

In an editing mode such as Fundamental mode, most keys insert the
associated character in the buffer.  This includes the alphanumeric keys
(letters and numbers) and the punctuation keys.

On modifying the buffer, the first two characters of the modeline change
from two spaces to "**", to indicate that the buffer has been modified.  In
contrast, the first to characters of the tutorial buffer are "%%", to
indicate that the buffer is read-only.

Typing "control-d" deletes the next character, typing "meta-d" deletes the
next word and typing "control-k" deletes the rest of the current line.

Typing "backspace" deletes the previous character and typing
"meta-backspace" deletes the previous word.

== Marks, Regions and the Kill Ring ==

There is a secondary focus in every buffer, a mark called "the mark".  The
text between the point and the mark is called the region.

Typing "control-space" runs the command `Set/Pop Mark', which moves the
mark to the position of point.  Point can then be moved with the usual
movement commands like "control-n" to extend the current region.

The Kill Ring is a list of saved regions.  Every time a buffer is changed
by more than a FIX few characters, the characters are saved to the Kill
Ring.

Typing "control-w" runs `Kill Region', which moves the current region to
the Kill Ring.

Typing "meta-w" runs `Save Region', which copies the current region to the
Kill Ring, leaving the version in the buffer.

Typing "control-y" runs `Yank', which copies the region from the top of the
Kill Ring at the point of the current buffer.

The `Rotate Kill Ring' command, when invoked immediately after `Yank' or
itself, moves the inserted region to the end of the Kill Ring, replacing it
with the region at the front of the Kill Ring.  Typing "meta-y" runs
`Rotate Kill Ring'.  When invoked after any other command, `Rotate Kill
Ring' behaves like `Yank'.

== Undo ==

There is a basic single-level "undo" facility for certain buffer editing
commands, such as `Yank'.  Typing "control-/" runs the command `Undo',
which reverts the last change.

== Files ==

Reading and modifiying files involves opening them into a buffer, then
reading the buffer as usual.  Typing "control-x control-f" invokes the
command `Find', which prompts for a file, then reads the file into the
buffer.

If the file given to `Find' is a directory then the directory is opened in
a buffer in a special mode called "Dired", which lists the directory and
provides for modifying and browsing the directory.

Any modifications to a buffer must be saved to be permanent.  Typing
"control-x s" runs the command `Save File', which saves the current buffer
to file.

== Further Help ==

Typing "control-h" brings up the help system, which prompts for a further
key for specific help.  For example, "control-h d" brings up the
documentation system, and "control-h b" lists the bindings available in the
current buffer.  Typing "h" after "control-h" lists the available help
options.

Within the documentation system, the node [Editor] describes the use of the
editor more fully.

Higher level applications are being added to the editor.  For example there
is a mail reader (FIX command `Mail') and a calendar (command `Calendar').
A menu system has been introduced to make it easy to browse the available
applications.  The command `Menu' switches to the menu in the current
window.

== Exiting the Editor ==

Typing "control-x control-z" runs `Save All Files and Exit', which saves
any modified files and exits.
]#


;;;; Initialization.

(after-editor-initializations
 (if (value show-welcome-on-start) (welcome-command)))
