;;; Keyboard macros.

(in-package "ED")

(export '(interactive))

#[ Keyboard Macros

Keyboard macros provide a facility to turn a sequence of commands into one
command.

{command:Define Keyboard Macro}
{command:End Keyboard Macro}
{command:Last Keyboard Macro}
{command:Define Keyboard Macro Key}
{evariable:Define Keyboard Macro Key Confirm}
{command:Keyboard Macro Query}
{command:Name Keyboard Macro}
{command:Free Command}

Many keyboard macros are not for customization, but rather for one-shot
use, a typical example being performing some operation on each line of a file.
To add "del " to the beginning and ".*" to the end of every line in
in a buffer, one could do this:

    C-x ( d e l Space C-e . * C-n C-a C-x ) C-u 9 9 9 C-x e

First a keyboard macro is defined which performs the desired operation on
one line, and then the keyboard macro is invoked with a large prefix
argument.  The keyboard macro will not actually execute that many times;
when the end of the buffer is reached the C-n will get an error
and abort the execution.
]#

;;; We have "Keyboard Macro Transforms" that help in making a keyboard
;;; macro.  What they do is turn the sequence of commands into equivalent
;;; lisp code.  They operate under the following principles:
;;;
;;;    They are passed two arguments:
;;; 1] The command invoked.
;;; 2] A keyword, either :invoke, :start or :finish
;;;
;;; If the keyword is :invoke, then the transform is expected to invoke the
;;; command and do whatever is necessary to make the same thing happen
;;; again when the macro is invoked.  The method does this by pushing forms
;;; on the list *current-kbdmac* and characters to simulate input of on
;;; *kbdmac-input*.  *current-kbdmac* is kept in reverse order.  Each form
;;; must be a function call, and none of the arguments are evaluated.  If
;;; the transform is unwound, presumably due to an error in the invoked
;;; command, then nothing should be done at invocation time.
;;;
;;; If the keyword is :finish, then nothing need be done.  This is to
;;; facilitate compaction of repetitions of the same command into one call.
;;; The transform is called with :finish when a run is broken.  Similarly,
;;; the transform is called with :start before the first occurrence in a
;;; run.

(defvar *kbdmac-transcript* (make-array 100  :fill-pointer 0 :adjustable t)
  "The thing we bind *input-transcript* to during keyboard macro definition.")

(defvar *kbdmac-input* (make-array 100  :fill-pointer 0  :adjustable t)
  "Place where we stick input that will need to be simulated during
   keyboard macro execution.")

(defvar *current-kbdmac* () "Body of keyboard macro we are building.")

(defvar *kbdmac-transforms* (make-hash-table :test #'eq)
  "Hashtable of function that know how to do things.")

(defvar *old-invoke-hook* () "Bound to *invoke-hook* by kbdmac-command-loop.")

(defmacro define-kbdmac-transform (command function)
  `(setf (gethash (getstring ,command *command-names*)
		  *kbdmac-transforms*)
	 ,function))

(defmacro kbdmac-emit (form)
  `(push ,form *current-kbdmac*))

(defun trash-character ()
  "Throw away a character on *editor-input*."
  (get-key-event *editor-input*))

;;; Save-Kbdmac-Input  --  Internal
;;;
;;; Pushes any input read within the body on *kbdmac-input* so that it is
;;; read again at macro invocation time.  It uses the (input-waiting)
;;; function which is a non-standard hook into the stream system.
;;;
(defmacro save-kbdmac-input (&body forms)
  (let ((slen (gensym)))
    `(let ((,slen (- (length *kbdmac-transcript*) (if (input-waiting) 1 0))))
       (multiple-value-prog1
	(progn ,@forms)
	(do ((i ,slen (1+ i))
	     (elen (length *kbdmac-transcript*)))
	    ((= i elen)
	     (when (input-waiting)
	       (kbdmac-emit '(trash-character))))
	  (vector-push-extend (aref *kbdmac-transcript* i)
			      *kbdmac-input*))))))

;;;; The default transform
;;;
;;; This transform is called when none is defined for a command.
;;;
(defun default-kbdmac-transform (command key)
  (case key
    (:invoke
     (let ((fun (command-function command))
	   (arg (prefix-argument))
	   (lastc *last-key-event-typed*))
       (save-kbdmac-input
	 (let ((*invoke-hook* *old-invoke-hook*))
	   (funcall fun arg))
	 (kbdmac-emit `(set *last-key-event-typed* ,lastc))
	 (kbdmac-emit `(,fun ,arg)))))))

;;;; Self insert transform:
;;;
;;; For self insert we accumulate the text in a string and then insert it
;;; all at once.

(defvar *kbdmac-text* (make-array 100 :fill-pointer 0 :adjustable t))

(defun insert-string-at-point (string)
  (insert-string (buffer-point (current-buffer)) string))
(defun insert-character-at-point (character)
  (insert-character (buffer-point (current-buffer)) character))

(defun key-vector-to-string (key-vector)
  (let ((string (make-array (length key-vector) :element-type 'base-char)))
    (dotimes (i (length key-vector) string)
      (setf (aref string i) (key-event-char (aref key-vector i))))))

(defun self-insert-kbdmac-transform (command key)
  (case key
    (:start
     (setf (fill-pointer *kbdmac-text*) 0))
    (:invoke
     (let ((p (or (prefix-argument) 1)))
       (funcall (command-function command) p)
       (dotimes (i p)
	 (vector-push-extend *last-key-event-typed* *kbdmac-text*))))
    (:finish
     (if (> (length *kbdmac-text*) 1)
	 (kbdmac-emit `(insert-string-at-point
			,(key-vector-to-string *kbdmac-text*)))
	 (kbdmac-emit `(insert-character-at-point
			,(key-event-char (aref *kbdmac-text* 0))))))))
;;;
(define-kbdmac-transform "Self Insert" #'self-insert-kbdmac-transform)
(define-kbdmac-transform "Lisp Insert )" #'self-insert-kbdmac-transform)

;;;; Do-Nothing transform:
;;;
;;; These are useful for prefix-argument setting commands, since they have
;;; no semantics at macro-time.
;;;
(defun do-nothing-kbdmac-transform (command key)
  (case key
    (:invoke
     (funcall (command-function command) (prefix-argument)))))
;;;
(define-kbdmac-transform "Argument Digit" #'do-nothing-kbdmac-transform)
(define-kbdmac-transform "Negative Argument" #'do-nothing-kbdmac-transform)
(define-kbdmac-transform "Universal Argument" #'do-nothing-kbdmac-transform)

;;;; Multiplicative transform
;;;
;;; Repititions of many commands can be turned into a call with an
;;; argument.
;;;
(defvar *kbdmac-count* 0
  "The number of occurrences we have counted of a given command.")

(defun multiplicative-kbdmac-transform (command key)
  (case key
    (:start
     (setq *kbdmac-count* 0))
    (:invoke
     (let ((p (or (prefix-argument) 1)))
       (funcall (command-function command) p)
       (incf *kbdmac-count* p)))
    (:finish
     (kbdmac-emit `(,(command-function command) ,*kbdmac-count*)))))
;;;
(define-kbdmac-transform "Forward Character" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Character" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Forward Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Uppercase Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Lowercase Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Capitalize Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Kill Next Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Kill Previous Word" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Forward Kill Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Kill Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Forward Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Backward Form" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Delete Next Character"
  #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Delete Previous Character"
   #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Delete Previous Character Expanding Tabs"
   #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Next Line" #'multiplicative-kbdmac-transform)
(define-kbdmac-transform "Previous Line" #'multiplicative-kbdmac-transform)

;;;; Vanilla transform
;;;
;;; These commands neither read input nor look at random silly variables.
;;;
(defun vanilla-kbdmac-transform (command key)
  (case key
    (:invoke
     (let ((fun (command-function command))
	   (p (prefix-argument)))
       (funcall fun p)
       (kbdmac-emit `(,fun ,p))))))
;;;
(define-kbdmac-transform "Beginning of Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "End of Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Beginning of Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Indent for Lisp" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Delete Horizontal Space" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Kill Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Backward Kill Line" #'vanilla-kbdmac-transform)
(define-kbdmac-transform "Yank" #'vanilla-kbdmac-transform)

;;;; MAKE-KBDMAC, INTERACTIVE, and kbdmac command loop.

;;; Kbdmac-Command-Loop  --  Internal
;;;
;;; Bind *invoke-hook* to call kbdmac transforms.
;;;
(defun kbdmac-command-loop ()
  (let* ((last-transform nil)
	 (last-command)
	 (last-ctype nil)
	 (*old-invoke-hook* *invoke-hook*)
	 (*invoke-hook*
	  #'(lambda (res p)
	      (declare (ignore p))
	      (when (and (not (eq last-command res)) last-transform)
		(funcall last-transform last-command :finish))
	      (if (last-command-type)
		  (setq last-ctype t)
		  (when last-ctype
		    (kbdmac-emit '(clear-command-type))
		    (setq last-ctype nil)))
	      (setq last-transform
		    (gethash res *kbdmac-transforms* #'default-kbdmac-transform))
	      (or (eq last-command res)
		  (funcall last-transform res :start))
	      (funcall last-transform res :invoke)
	      (setq last-command res))))
    (declare (special *invoke-hook*))
    (setf (last-command-type) nil)
    (recursive-edit nil)))

(defun clear-command-type ()
  (setf (last-command-type) nil))

(defvar *defining-a-keyboard-macro* ())
(defvar *kbdmac-stream* (make-kbdmac-stream))
(defvar *in-a-keyboard-macro* ()
  "True if we are currently executing a keyboard macro.")

#[ Interactive

The editor supports keyboard macros.  A user may enter a mode where the editor
records his actions, and when the user exits this mode, the command `Last
Keyboard Macro' plays back the actions.  Some commands behave differently when
invoked as part of the definition of a keyboard macro.  For example, when used
in a keyboard macro, a command that message's useless user confirmation
will slow down the repeated invocations of `Last Keyboard Macro' because
the command will pause on each execution to make sure the user sees the
message.  This can be eliminated with the use of interactive.  As another
example, some commands conditionally signal an editor-error versus simply
beeping the device depending on whether it executes on behalf of the user or a
keyboard macro.

{function:ed:interactive}
]#

;;; Interactive  --  Public
;;;
;;; See whether we are in a keyboard macro.
;;;
(defun interactive ()
  "Return true if in a command invoked by the user.  This is primarily
   useful for commands which want to know whether to do something when an
   error happens or just signal an editor-error."
  (fi *in-a-keyboard-macro*))

(defvar *kbdmac-done* ()
  "Setting this causes the keyboard macro being executed to terminate after
   the current iteration.")

(defvar *kbdmac-dont-ask* ()
  "Setting this inhibits `Keyboard Macro Query's querying.")

;;; Make-Kbdmac  --  Internal
;;;
;;; Grab the stuff lying around in *current-kbdmac* etc and make a lexical
;;; closure that can be used as the definition of a command.  The prefix
;;; argument is a repetition count.
;;;
(defun make-kbdmac ()
  (let ((code (reverse *current-kbdmac*))
	(input (copy-seq *kbdmac-input*)))
    (if (zerop (length input))
	#'(lambda (p)
	    (let ((*in-a-keyboard-macro* t)
		  (*kbdmac-done* nil)
		  (*kbdmac-dont-ask* nil))
	      (setf (last-command-type) nil)
	      (catch 'exit-kbdmac
		(dotimes (i (or p 1))
		  (catch 'abort-kbdmac-iteration
		    (dolist (form code)
		      (apply (car form) (cdr form))))
		  (when *kbdmac-done* (return nil))))))
	#'(lambda (p)
	    (let* ((stream (or *kbdmac-stream* (make-kbdmac-stream)))
		   (*kbdmac-stream* nil)
		   (*editor-input* stream)
		   (*in-a-keyboard-macro* t)
		   (*kbdmac-done* nil)
		   (*kbdmac-dont-ask* nil))
	      (setf (last-command-type) nil)
	      (catch 'exit-kbdmac
		(dotimes (i (or p 1))
		  (setq stream (modify-kbdmac-stream stream input))
		  (catch 'abort-kbdmac-iteration
		    (dolist (form code)
		      (apply (car form) (cdr form))))
		  (when *kbdmac-done* (return nil)))))))))


;;;; Commands.

(defmode "Def" :major-p nil)

(defcommand "Define Keyboard Macro" ()
  "Start the definition of a keyboard macro.  The commands which are
   invoked up to an `End Keyboard Macro' become the definition for the
   keyboard macro.  Replaying the keyboard macro is then synonymous with
   invoking that sequence of commands."
  (if *defining-a-keyboard-macro*
      (editor-error "Already defining a keyboard macro."))
  (define-keyboard-macro))

(defevar "Define Keyboard Macro Key Confirm"
  "When set, `Define Keyboard Macro Key' asks for confirmation before
   clobbering an existing key binding."
  :value t)

(defcommand "Define Keyboard Macro Key" ()
  "Prompt for a key before going into a mode for defining keyboard macros.
   Bind the macro definition to the key.  if the key is already bound and
   `Define Keyboard Macro Key Confirm' is set then ask for confirmation
   before clobbering the binding."
  (when *defining-a-keyboard-macro*
    (editor-error "Already defining a keyboard macro."))
  (multiple-value-bind (key kind where)
		       (get-keyboard-macro-key)
    (when key
      (setf (buffer-minor-mode (current-buffer) "Def") t)
      (let ((name (format nil "Keyboard Macro ~S" (gensym))))
	(make-command name "This is a user-defined keyboard macro."
		      (define-keyboard-macro))
	(bind-key name key kind where)
	(message "~A bound to ~A."
		 (with-output-to-string (s) (print-pretty-key key s))
		 name)))))

;;; GET-KEYBOARD-MACRO-KEY gets a key from the user and confirms clobbering it
;;; if it is already bound to a command, or it is a :prefix.  This returns nil
;;; if the user "aborts", otherwise it returns the key and location (kind
;;; where) of the binding.
;;;
(defun get-keyboard-macro-key ()
  (let* ((key (prompt-for-key :prompt "Bind keyboard macro to key: "
			      :must-exist nil)))
    (multiple-value-bind (kind where)
			 (prompt-for-place "Kind of binding: "
					   "The kind of binding to make.")
      (let* ((cmd (get-command key kind where)))
	(cond ((not cmd) (values key kind where))
	      ((commandp cmd)
	       (if (prompt-for-y-or-n
		    :prompt `("~A is bound to ~A.  Rebind it? "
			      ,(with-output-to-string (s)
				 (print-pretty-key key s))
			      ,(command-name cmd))
		    :default nil)
		   (values key kind where)
		   nil))
	      ((eq cmd :prefix)
	       (if (prompt-for-y-or-n
		    :prompt `("~A is a prefix for more than one command.  ~
			       Clobber it? "
			      ,(with-output-to-string (s)
				 (print-pretty-key key s)))
		    :default nil)
		   (values key kind where)
		   nil)))))))

;;; DEFINE-KEYBOARD-MACRO gets input from the user and clobbers the function
;;; for the "Last Keyboard Macro" command.  This returns the new function.
;;;
(defun define-keyboard-macro ()
  (setf (buffer-minor-mode (current-buffer) "Def") t)
  (unwind-protect
    (let* ((in *kbdmac-transcript*)
	   (*input-transcript* in)
	   (*defining-a-keyboard-macro* t))
      (setf (fill-pointer in) 0)
      (setf (fill-pointer *kbdmac-input*) 0)
      (setq *current-kbdmac* ())
      (catch 'punt-kbdmac
	(kbdmac-command-loop))
      (setf (command-function (getstring "Last Keyboard Macro" *command-names*))
	    (make-kbdmac)))
    (setf (buffer-minor-mode (current-buffer) "Def") nil)))

(defcommand "End Keyboard Macro" ()
  "End the definition of a keyboard macro."
  (or *defining-a-keyboard-macro*
      (editor-error "Not defining a keyboard macro."))
  (throw 'punt-kbdmac ()))
;;;
(define-kbdmac-transform "End Keyboard Macro" #'do-nothing-kbdmac-transform)

(defcommand "Last Keyboard Macro" ()
  "Execute the last keyboard macro defined.  With a prefix argument execute
   it that many times."
  "Execute the last keyboard macro defined.  If P is true execute it P
   times."
  (editor-error "Need a keyboard macro defined."))

(defvar *named-kbdmacs* ()
  "List of named keyboard macros.")

(defcommand "Name Keyboard Macro" (p name)
  "Name the `Last Keyboard Macro'.  The last defined keboard macro is made
   into a named command, which is kept when another keyboard macro is
   defined so that several keyboard macros can be kept around at once.  The
   resulting command may also be bound to a key using `Bind Key', in the
   same way any other command is.

   The command can be free'd up afterwards with `Free Command'."
  (declare (ignore p))
  (or name
      (setq name
	    (prompt-for-string
	     :prompt "Macro name: "
	     :help
	     "String name of command to make from keyboard macro.")))
  (make-command
   name "This is a named keyboard macro."
   (command-function (getstring "Last Keyboard Macro"
				*command-names*)))
  ;; FIX another above, what happens when command overwritten?
  (setq *named-kbdmacs*
	(cons (list name
		    *current-kbdmac*
		    *kbdmac-input*
		    *kbdmac-transcript*)
	      (delete name *named-kbdmacs*
		      :test #'string= :key #'car))))

(defcommand "Free Command" ()
  "Free up a prompted command."
  (let ((name
	 (prompt-for-keyword (list *command-names*)
			     :must-exist t
			     :prompt "Command: "
			     :help "Enter the name of an editor command."
			     :history *extended-command-history*
			     :history-pointer
			     '*extended-command-history-pointer*)))
    (free-command name)
    (setq *named-kbdmacs*
	  (delete name *named-kbdmacs*
		  :test #'string= :key #'car))))

(defcommand "Keyboard Macro Query" ()
  "Conditionalize the execution of a keyboard macro.  Return immediately
   when invoked during the definition of a macro.  When the macro replays,
   prompt for a key-event indicating what action to take, accepting the
   following responses:

    Escape
	Exit all repetitions of this keyboard macro.  More than one may
	have been specified using a prefix argument.

    Space, y
	Proceed with the execution of the keyboard macro.

    Delete, Backspace, n
	Skip the remainder of the keyboard macro and go on to the next
	repetition, if any.

    !
	Do all remaining repetitions of the keyboard macro without
	prompting.

    .
	Complete this repetition of the macro and then exit without doing
	any of the remaining repetitions.

    C-r
	Do a recursive edit and then prompt again."
  (fi (or (interactive) *kbdmac-dont-ask*)
      (let ((*editor-input* *real-editor-input*))
	(command-case (:prompt "Keyboard Macro Query: "
			       :help "Type one of these characters to say what to do:"
			       :change-window nil
			       :bind key-event)
	  (:exit
	   "Exit this keyboard macro immediately."
	   (throw 'exit-kbdmac nil))
	  (:yes
	   "Proceed with this iteration of the keyboard macro.")
	  (:no
	   "Skip to the next iteration of the keyboard macro."
	   (throw 'abort-kbdmac-iteration nil))
	  (:do-all
	   "Do all remaining repetitions of the keyboard macro without prompting."
	   (setq *kbdmac-dont-ask* t))
	  (:do-once
	   "Do this iteration of the keyboard macro and then exit."
	   (setq *kbdmac-done* t))
	  (:recursive-edit
	   "Do a recursive edit, then ask again."
	   (do-recursive-edit)
	   (reprompt))
	  (t
	   (unget-key-event key-event *editor-input*)
	   (throw 'exit-kbdmac nil))))))


;;;; Saving the keyboard macros between sessions.

;;; FIX Perhaps the written file should be more easily human editable.

(defconstant kbdmacs-save-name "keyboard-macros")

(defun write-keyboard-macros-file (pathname)
  "Save the currently defined keyboard macros to $pathname."
  (with-open-file (stream pathname
			  :if-exists :new-version
			  :direction :output)
    (dolist (named *named-kbdmacs*)
      (let ((*package* (find-package "ED")))
	(write (list (car named)
		     (cadr named)
		     (cons 'list (map 'list #'eval (caddr named)))
		     (cons 'list (map 'list #'eval (cadddr named))))
	       :stream stream
	       :readably t))
      (terpri stream))
    ;; Write it last so that `read-keyboard-macro-file' reads it last.
    (write (list t *current-kbdmac*
		 (cons 'list (map 'list #'eval *kbdmac-input*))
		 (cons 'list (map 'list #'eval *kbdmac-transcript*)))
	   :stream stream
	   :readably t)
    (terpri stream)))

(defun read-keyboard-macros-file (pathname)
  "Read in the keyboard macros stored in $pathname.

   Replace the current keyboard macro from the one in $pathname, and
   recreate any named macros in $pathname."
  (with-open-file (stream pathname
			  :if-does-not-exist ())
    (when stream
      (loop
	(let ((list (let ((*package* (find-package "ED")))
		      (read stream ()))))
	  (or list (return))
	  (destructuring-bind (name current input transcript)
			      list
	    (setf (fill-pointer *kbdmac-input*) 0)
	    (dolist (key (eval input))
	      (vector-push key *kbdmac-input*))
	    (setq *current-kbdmac* current)
	    (setf (fill-pointer *kbdmac-transcript*) 0)
	    (dolist (key (eval transcript))
	      (vector-push key *kbdmac-transcript*))
	    (setf (command-function (getstring "Last Keyboard Macro"
					       *command-names*))
		  (make-kbdmac))
	    (or (eq name t)
		(name-keyboard-macro-command () name))))))))

(defevar "Save Keyboard Macros"
  "Configuration of keyboard macro saving.  Possible values:

     t               always save
     :prompt         prompt for confirmation if any macros exist
     :named-prompt   prompt for confirmation if any named macros exist
     ()              always forego saving."
  :value t)

(defun save-keyboard-macros ()
  ;; FIX prompt to save keyboard macros defined this session, if so add
  ;;     them to the others
  (if (ecase (value save-keyboard-macros)
	(:named-prompt
	 (when *named-kbdmacs*
	   (prompt-for-y-or-n
	    :prompt "Named keyboard macros exist.  Save all keyboard macros? "
	    :default-string "Y"
	    :default-string t)))
	(:prompt
	 (when (or *named-kbdmacs* *current-kbdmac*)
	   (prompt-for-y-or-n
	    :prompt "Keyboard macros exist.  Save all keyboard macros? "
	    :default-string "Y"
	    :default-string t)))
	((t) t)
	((()) ()))
      (write-keyboard-macros-file
       (config:config-pathname kbdmacs-save-name))
      (if (probe-file (config:config-pathname kbdmacs-save-name))
	  (delete-file (config:config-pathname kbdmacs-save-name)))))

(add-hook exit-hook 'save-keyboard-macros)

(after-editor-initializations
 (or (ext:get-command-line-switch "noinit")
     (if (config:probe-config-file kbdmacs-save-name)
	 (read-keyboard-macros-file
	  (config:config-pathname kbdmacs-save-name)))))
