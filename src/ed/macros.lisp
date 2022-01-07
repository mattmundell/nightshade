;;; Most of the junk that needs to be in the compiler to compile commands.

(in-package "EDI")

(export '(invoke-hook value setv elet string-to-variable
	  add-hook remove-hook
	  defcommand defevar with-mark use-buffer editor-error
	  editor-error-format-string editor-error-format-arguments
	  do-region-lines do-buffer-lines do-lines-from-mark
	  do-processes
	  command-case reprompt help with-output-to-mark with-input-from-region
	  with-temp-buffer handle-lisp-errors with-output-to-window
	  with-pop-up-display with-pop-up-window in-pop-up-window
	  *random-typeout-buffers* complf in-lisp))


;;;; Macros used for manipulating editor variables.

(defmacro invoke-hook (place &rest args)
  "Call the functions in $place with $args.  If place is a symbol, then
   interpret it as an editor variable rather than a Lisp variable, using
   its current value as the list of functions."
  (let ((f (gensym)))
    `(dolist (,f ,(if (symbolp place) `(%value ',place) place))
       (funcall ,f ,@args))))

; FIX
; (defmacro invoke-hook (place &rest args)
;   "Call the functions in place with args.  If place is a symbol, then this
;    interprets it as an editor variable rather than a Lisp variable, using its
;    current value as the list of functions."
;   (let ((f (gensym))
; 	(return (gensym)))
;     `(let ((,return t))
;        (dolist (,f ,(if (symbolp place) `(%value ',place) place) ,return)
; 	 (setq return (and (funcall ,f ,@args) ,return))))))

(defmacro value (name)
  "Return the current value of the editor variable $name.  Use $name
   directly as a symbol.

   Settable with `setf'."
  `(%value ',name))

(defmacro setv (name new-value)
  "Set the current value of editor variable $name to $new-value, calling
   any hook functions with $new-value before setting the value.  Use $name
   directly as a symbol."
  `(%set-value ',name ,new-value))

;;; WITH-VARIABLE-OBJECT  --  Internal
;;;
;;; Look up the variable object for name and bind it to obj, giving error
;;; if there is no such variable.
;;;
(defmacro with-variable-object (name &body forms)
  `(let ((obj (get ,name 'editor-variable-value)))
     (fi obj (undefined-variable-error ,name))
     ,@forms))

(defmacro elet (binds &rest forms)
  "elet ({var value}*) {form}*

   `let' for editor variables.  Bind editor variables for the duration of
   $forms, returning the value of the last form.

   Skip calling any editor variable hook functions." ; FIX should call hooks?
  (let ((lets ())
	(sets ())
	(unsets ()))
    (dolist (bind binds)
      (let ((n-obj (gensym))
	    (n-val (gensym))
	    (n-old (gensym)))
	(push `(,n-val ,(second bind)) lets)
	(push `(,n-old (variable-object-value ,n-obj)) lets)
	(push `(,n-obj (with-variable-object ',(first bind) obj)) lets)
	(push `(setf (variable-object-value ,n-obj) ,n-val) sets)
	(push `(setf (variable-object-value ,n-obj) ,n-old) unsets)))
    `(let* ,lets
       (unwind-protect
	 (progn ,@sets nil ,@forms)
	 ,@unsets))))



;;;; A couple funs to hack strings to symbols.

(eval-when (compile load eval)

(defun bash-string-to-symbol (name suffix)
  (intern (nsubstitute #\- #\space
		       (nstring-upcase
			(concatenate 'simple-string
				     name (symbol-name suffix))))))

;;; string-to-variable  --  Exported
;;;
(defun string-to-variable (string)
  "Return the symbol which corresponds to the string name $string."
  (intern (nsubstitute #\- #\space
		       (the simple-string (string-upcase string)))
	  (find-package "ED")))

); eval-when (compile load eval)

;;; string-to-keyword  --  Internal
;;;
(defun string-to-keyword (string)
  "Mash $string into a Keyword."
  (intern (nsubstitute #\- #\space
		       (the simple-string (string-upcase string)))
	  (find-package "KEYWORD")))



;;;; Macros to add and delete hook functions.

#[ Hooks

Editor actions such as setting variables, changing buffers, changing windows,
turning modes on and off, etc., often have hooks associated with them.  A hook
is a list of functions called before the system performs the action.  The
manual describes the object specific hooks with the rest of the operations
defined on these objects.

Often hooks are stored in editor variables, `Delete Buffer Hook' and
`Set Window Hook' for example.  This leads to a minor point of confusion
because these variables have hooks that the system executes when someone
changes their values.  These hook functions the editor invokes when someone sets
a variable are an example of a hook stored in an object instead of an editor
variable.  These are all hooks for editor activity, but the editor keeps them in
different kinds of locations.  This is why some of the routines in this section
have a special interpretation of the hook place argument.

{function:ed:add-hook}
{function:ed:remove-hook}
{function:ed:invoke-hook}
]#

;;; add-hook  --  Exported
;;;
;;; Add a hook function to a hook, defining a variable if necessary.
;;;
(defmacro add-hook (place hook-fun &key end)
  "add-hook $place $hook-fun

   Add hook function $hook-fun in some $place.  If $hook-fun already exists
   in $place, then leave it so.  If $place is a symbol, then it is an editor
   variable; otherwise, it is a generalized variable or storage location.

   Here are two examples:

	(add-hook delete-buffer-hook 'remove-buffer-from-menu)

	(add-hook (variable-hooks 'check-mail-interval)
		  'reschedule-mail-check)

   If $end is true then add $hook-fun to the end of the list."
  (if end
      (if (symbolp place)
	  `(or (memq ,hook-fun (value ,place))
	       (setf (value ,place) (append (value ,place) (list ,hook-fun))))
	  `(or (memq ,hook-fun ,place)
	       (setf ,place (append ,place (list ,hook-fun)))))
      (if (symbolp place)
	  `(pushnew ,hook-fun (value ,place))
	  `(pushnew ,hook-fun ,place))))

;;; remove-hook  --  Public
;;;
;;; Delete a hook-function from somewhere.
;;;
(defmacro remove-hook (place hook-fun)
  "remove-hook $place $hook-fun

   Remove hook function $hook-fun from the list in $place.  If $place is a
   symbol, then it is an editor variable; otherwise, it is a generalized
   variable or storage location."
  (if (symbolp place)
      `(setf (value ,place) (delete ,hook-fun (value ,place)))
      `(setf ,place (delete ,hook-fun ,place))))



;;;; DEFCOMMAND.

#[ Commands

Nearly everything that can be done in the editor is done using a command.
Since there are many things worth doing, the editor provides many commands,
currently nearly two hundred.  Most of the editor documentation is a
description of what commands exist, how they are invoked, and what they do.
This is the format of a command's documentation:

{command:Forward Character}

The command is named `Forward Character', and it is bound to "control-f",
meaning that typing "control-f" runs the command.  The description is the
command documentation.
]#

#[ Defining Commands

{function:ed:defcommand}
{function:ed:make-command}
{function:ed:commandp}
{function:ed:command-documentation}
{function:ed:command-function}
{function:ed:command-name}
]#

#[ Command Documentation

Command documentation is a description of what the command does
when it is invoked as an extended command or from a key.  Command
documentation may be either a string or a function.  If the
documentation is a string then the first line should briefly summarize
the command, with remaining lines filling the details.  Example:

    (defcommand "Forward Character" (p)
      "Move the point forward one character.
       With prefix argument move that many characters, with negative
       argument go backwards."
      "Move the point of the current buffer forward p characters."
       . . .)

Command documentation may also be a function of one argument.  The
function is called with either :short or :full, indicating
that the function should return a short documentation string or do
something to document the command fully.
]#

;;; Defcommand  --  Public
;;;
(defmacro defcommand (name lambda-list command-doc function-doc
			   &body forms)
  "Define a new editor command $name.

   Create a function to implement the command from a derived name,
   $lambda-list (the arguments to the function) and $forms.  Set
   $command-doc as the documentation for the command.  If $function-doc is
   a string set it as the documentation for the function, otherwise set
   $command-doc as the function documentation (i.e. $function-doc is
   optional).

   Set the body of the function to $forms.  $forms implement the command.
   The first argument to the function is the prefix argument.  If
   $lambda-list is () a prefix argument and an ignore declaration are
   automatically added.  All arguments to the function are made optional,
   even if there is an &optional symbol in $lambda-list.

   Derive the name of the function by replacing all spaces in $name with
   hyphens and appending \"-command\".  To preserve a direct mapping
   between command names and function names, signal an error if $name
   contains any hyphens.

   An example:
     (defcommand \"Forward Character\" (p)
       \"Move the point forward one character.
	With prefix argument move that many characters, with negative argument
	go backwards.\"
       \"Move the point of the current buffer forward p characters.\"
       (or (character-offset (buffer-point (current-buffer)) (or p 1))
	   (editor-error \"Too few characters.\")))"
  (or (stringp command-doc)
      (error "Command documentation must be a string: ~S." command-doc))
  (if (position #\- name)
      (error "Command name contains a hyphen: ~S." name))
  (and lambda-list (atom lambda-list)
       (error "Command argument list must be a list: ~S." lambda-list))
  (let (command-name function-name)
    (cond ((listp name)
	   (setq command-name (car name)  function-name (cadr name))
	   (or (symbolp function-name)
	       (error "Function name must be a symbol: ~S" function-name)))
	  (t
	   (setq command-name name
		 function-name (bash-string-to-symbol name '-COMMAND))))
    (or (stringp command-name)
	(error "Command name must be a string: ~S." name))
    `(eval-when (load eval)
       (defun ,function-name ,(cons '&optional (if lambda-list
						   (delete '&optional lambda-list)
						   '(p)))
	 ,(if (stringp function-doc) function-doc command-doc)
	 ;; FIX test with start of body a declare
	 ,@(if lambda-list
	       (if (stringp function-doc)
		   forms
		   (cons function-doc forms))
	       (cons '(declare (ignore p))
		     (if (stringp function-doc)
			 forms
			 (cons function-doc forms)))))
       (make-command ',name ,command-doc ',function-name)
       ',function-name)))


;;;; DEFEVAR.

;;; DEFEVAR  --  Public
;;;
;;; Define an editor variable somewhere.
;;;
(defmacro defevar (name documentation &key mode buffer hooks value source)
  "Define an editor variable.

     $name
       The string name of the variable to define.

     $documentation
       The documentation string for the variable.


     $mode, $buffer

       If $buffer is supplied, set the variable as local to that buffer.  If $mode
       is supplied, set it local to that mode.  If both are (), set it global.

     $value
       The initial value for the variable.

     $hooks

       The initial list of functions to call when the variable's value is
       set.  These functions execute before the new value is established.

       FIX See `variable-value' for the arguments passed to the hook functions.

     $source
       The file from which the definition of the variable was read.

   If a variable with the same name already exists in the same place, then
   set its hooks and value from $hooks and $value if these keywords are
   set."
  `(%defevar ,name ,documentation ,mode ,buffer ,hooks ,value ,source))


;;;; PARSE-FORMS.

;;; Parse-Forms  --  Internal
;;;
;;; Used for various macros to get the declarations out of a list of forms.
;;;
(eval-when (compile load eval)
(defmacro parse-forms ((decls-var forms-var forms) &body gorms)
  "Parse-Forms (Decls-Var Forms-Var Forms) {Form}*
  Binds Decls-Var to leading declarations off of Forms and Forms-Var
  to what is left."
  `(do ((,forms-var ,forms (cdr ,forms-var))
	(,decls-var ()))
       ((or (atom ,forms-var) (atom (car ,forms-var))
	    (not (eq (caar ,forms-var) 'declare)))
	,@gorms)
     (push (car ,forms-var) ,decls-var)))
)



;;;; WITH-MARK and USE-BUFFER.

(defmacro with-mark (mark-bindings &rest forms)
  "with-mark ({(mark pos [kind])}*) {declaration}* {form}*

   Evaluate forms with the variable $mark bound to a copy of the mark
   specified by $pos, deleting $mark on exit if mark is permanent.  $mark
   is :temporary, or of kind $kind.  Return the value of the last form in
   $forms."
  (do ((bindings mark-bindings (cdr bindings))
       (let-slots ())
       (cleanup ()))
      ((null bindings)
       (if cleanup
	   (parse-forms (decls forms forms)
	     `(let ,(nreverse let-slots)
		,@decls
		(unwind-protect
		  (progn ,@forms)
		  ,@cleanup)))
	   `(let ,(nreverse let-slots) ,@forms)))
    (let ((name (caar bindings))
	  (pos (cadar bindings))
	  (type (or (caddar bindings) :temporary)))
      (cond ((not (eq type :temporary))
	     (push `(,name (copy-mark ,pos ,type)) let-slots)
	     (push `(delete-mark ,name) cleanup))
	    (t
	     (push `(,name (copy-mark ,pos :temporary)) let-slots))))))

#|Save this shit in case we want WITH-MARK to no longer cons marks.
(defconstant with-mark-total 50)
(defvar *with-mark-free-marks* (make-array with-mark-total))
(defvar *with-mark-next* 0)

(defmacro with-mark (mark-bindings &rest forms)
  "WITH-MARK ({(Mark Pos [Kind])}*) {declaration}* {form}*
   WITH-MARK evaluates each form with each Mark variable bound to a mark
   specified by the respective Pos, a mark.  The created marks are of kind
   :temporary, or of kind Kind."
  (do ((bindings mark-bindings (cdr bindings))
       (let-slots ())
       (cleanup ()))
      ((null bindings)
       (let ((old-next (gensym)))
	 (parse-forms (decls forms forms)
	   `(let ((*with-mark-next* *with-mark-next*)
		  (,old-next *with-mark-next*))
	      (let ,(nreverse let-slots)
		,@decls
		(unwind-protect
		    (progn ,@forms)
		  ,@cleanup))))))
       (let ((name (caar bindings))
	     (pos (cadar bindings))
	     (type (or (caddar bindings) :temporary)))
	 (push `(,name (mark-for-with-mark ,pos ,type)) let-slots)
	 (if (eq type :temporary)
	     (push `(delete-mark ,name) cleanup)
	     ;; Assume mark is on free list and drop its hold on data.
	     (push `(setf (mark-line ,name) nil) cleanup)))))

;;; MARK-FOR-WITH-MARK -- Internal.
;;;
;;; At run time of a WITH-MARK form, this returns an appropriate mark at the
;;; position mark of type kind.  First it uses one from the vector of free
;;; marks, possibly storing one in the vector if we need more marks than we
;;; have before, and that need is still less than the total free marks we are
;;; willing to hold onto.  If we're over the free limit, just make one for
;;; throwing away.
;;;
(defun mark-for-with-mark (mark kind)
  (let* ((line (mark-line mark))
	 (charpos (mark-charpos mark))
	 (mark (cond ((< *with-mark-next* with-mark-total)
		      (let ((m (svref *with-mark-free-marks* *with-mark-next*)))
			(cond ((markp m)
			       (setf (mark-line m) line)
			       (setf (mark-charpos m) charpos)
			       (setf (mark-%kind m) kind))
			      (t
			       (setf m (internal-make-mark line charpos kind))
			       (setf (svref *with-mark-free-marks*
					    *with-mark-next*)
				     m)))
			(incf *with-mark-next*)
			m))
		     (t (internal-make-mark line charpos kind)))))
    (unless (eq kind :temporary)
      (push mark (line-marks (mark-line mark))))
    mark))
|#

(defmacro use-buffer (buffer &body forms)
  "use-buffer buffer {form}*

   Execute $forms in a context which has buffer bound to current buffer
   with certain restrictions.

   In particular, the value of any global binding of an editor variable
   which is also a mode local variable is ill-defined; if the variable has
   a global binding the value may be that of the mode-local binding.

   It is also an error to nest `use-buffer's in more than one buffer.

   The reason for using this function is that it may be significantly
   faster than changing current-buffer to $buffer and back."
  (let ((gensym (gensym)))
    `(let ((,gensym *current-buffer*)
	   (*current-buffer* ,buffer))
       (unwind-protect
	   (progn
	     (use-buffer-set-up ,gensym)
	     ,@forms)
	 (use-buffer-clean-up ,gensym)))))


#[ User Errors

When in the course of editing, the editor is unable to do what it thinks
you want to do, then it brings this to your attention by a beep or a screen
flash (possibly accompanied by an explanatory echo area message such as
"Last line.".)  Although the exact attention-getting mechanism may vary on
the output device and variable settings, this is always called beeping.

Whatever the circumstances, you had best try something else since the editor,
being far more stupid than you, is far more stubborn.  the editor is an
extensible editor, so it is always possible to change the command that
complained to do what you wanted it to do.
]#

#[ Interface to the Error System

The error system interface is minimal.  There is a simple editor-error
condition which is a subtype of error and a convenient means for signaling
them.  The editor also provides a standard handler for error conditions while in
the editor.

{function:editor-error-format-string}
{function:editor-error-format-arguments}
{function:editor-error}
{function:ed:handle-lisp-errors}
]#


;;;; EDITOR-ERROR.

(defun print-editor-error (condx s)
  (apply #'format s (or (editor-error-format-string condx) "")
	 (editor-error-format-arguments condx)))

(define-condition editor-error (error)
  ((format-string :initform "" :initarg :format-string
		  :reader editor-error-format-string)
   (format-arguments :initform '() :initarg :format-arguments
		     :reader editor-error-format-arguments))
  (:report print-editor-error))
;;;
(setf (documentation 'editor-error-format-string 'function)
  "Return the `format' control string of the given editor-error condition.")
(setf (documentation 'editor-error-format-arguments 'function)
  "Return the `format' arguments for the given editor-error condition.")

;; FIX this is a file of macros?
(defun editor-error (&rest args)
  "Signal a minor error within the editor.  These are errors that a normal
   user could encounter in the course of editing such as a search failing
   or an attempt to move past the end of the buffer.

   Signal an editor-error condition formed from $args, which is () or a
   format string possibly followed by format arguments.

   `ed' invokes commands in a dynamic context with an editor-error
   condition handler bound.  The handler beeps or flashes (or both) the
   display.  If the condition passed to the handler has a string slot set,
   the handler also invokes message on it.  The command in progress is
   always terminated immediately.  Only ever return if invoked inside an
   exit hook."
  ; FIX will it? (handler returns from block)
  (let ((condx (make-condition 'editor-error
			       :format-string (car args)
			       :format-arguments (cdr args))))
    (signal condx)))



;;;; Do-*

(defmacro do-region-lines ((line-var region &optional result
				     &key backwards)
			   &body forms)
  "do-region-lines (line-var region {result {:backwards t}}) {declaration}* {form}*

   Evaluate $forms for each line in $region up to the one before the last
   with $line-var bound to the line.  Evaluate $result with $line-var bound
   to the last line in $region.

   If $backwards is true then do the lines from the last up to the one
   before the first, evaluating $result with $line-var bound to the first."
  (let ((end-line (gensym)) (call-forms (gensym)))
    ;; The block is to catch an returns from $forms.
    `(block ()
       (flet ((,call-forms (,line-var)
		,@forms))
	 (close-line)
	 (if ,backwards
	     (do ((,end-line (mark-line (region-start ,region)))
		  (,line-var (mark-line (region-end ,region))
			     (line-previous ,line-var)))
		 ((eq ,line-var ,end-line)
		  ,result)
	       (,call-forms ,line-var))
	     (do ((,end-line (mark-line (region-end ,region)))
		  (,line-var (mark-line (region-start ,region))
			     (line-next ,line-var)))
		 ((eq ,line-var ,end-line)
		  ,result)
	       (,call-forms ,line-var)))))))

(defmacro do-lines-from-mark ((line-var mark &key backwards) &body forms)
  "do-lines-from-mark (line-var mark) {declaration}* {form}*.

   Execute $forms for each line in $buffer with the line bound to
   $line-var, starting from the line the buffer point is on.  If $backwards
   is true do from the buffer point to the first line in the buffer, else
   do from the point to the last line."
  (let ((call-forms (gensym)))
    ;; The block is to catch any returns from $forms.
    `(block ()
       (flet ((,call-forms (,line-var) ,@forms))
	 (do-region-lines (,line-var (if ,backwards
					 (region (buffer-start-mark
						  (mark-buffer ,mark))
						 ,mark)
					 (region ,mark
						 (buffer-end-mark
						  (mark-buffer ,mark))))
				     (,call-forms ,line-var)
				     :backwards ,backwards)
	   (,call-forms ,line-var))))))

(defmacro do-buffer-lines ((line-var buffer &key backwards) &body forms)
  "do-buffer-lines (line-var buffer {:backwards t}) {declaration}* {form}*

   Execute $forms for each line in $buffer with the line bound to
   $line-var, starting from the first line in the buffer.

   If backwards is true execute from the last to the first line instead."
  (let ((call-forms (gensym)))
    ;; The block is to catch any returns from $forms.
    `(block ()
       (flet ((,call-forms (,line-var) ,@forms))
	 (do-region-lines (,line-var (buffer-region ,buffer)
				     (,call-forms ,line-var)
				     :backwards ,backwards)
	   (,call-forms ,line-var))))))

; (defmacro do-processes ((process buffer) &body body)
;   `(loop for ,buffer in *buffer-list* do
;      (when (buffer-minor-mode ,buffer "Process")
;        (let ((,process (variable-value 'ed::process :buffer ,buffer)))
; 	 ,@body))))
(defmacro do-processes ((process buffer) &body (body decls))
  "Do-Processes (Process Buffer) {declaration}* {form}*
   Iterate over the processes active in any buffer.  Buffer is always
   used."
  (let ((buffer-list (gensym)))
    `(let ((,buffer-list *buffer-list*))
       (loop while ,buffer-list do
	 (let ((,buffer (car ,buffer-list)))
	   (when (buffer-minor-mode ,buffer "Process")
	     (let ((,process (variable-value 'ed::process :buffer ,buffer)))
	       ,@decls
	       ,@body)))
	 (setq ,buffer-list (cdr ,buffer-list))))))



;;;; COMMAND-CASE

;;; COMMAND-CASE  --  Public
;;;
;;; Grovel the awful thing and spit out the corresponding Cond.  See Echo
;;; for the definition of COMMAND-CASE-HELP and logical char stuff.
;;;
(eval-when (compile load eval)
(defun command-case-tag (tag key-event char)
  (cond ((and (characterp tag) (standard-char-p tag))
	 `(and ,char (char= ,char ,tag)))
	((and (symbolp tag) (keywordp tag))
	 `(logical-key-event-p ,key-event ,tag))
	(t
	 (error "Tag in COMMAND-CASE is not a standard character or keyword: ~S"
		tag))))
); eval-when (compile load eval)
;;;
(defmacro command-case ((&key (change-window t)
			      (prompt "Command character: ")
			      (help "Choose one of the following characters:")
			      (bind (gensym)))
			&body forms)
  "Analogous to the `case' macro.  Get a key-event, translate it to a
   character, and then to invoke some behaviour depending on the character.
   In addition to character dispatching, this supports [logical key-events]
   by using the input key-event directly.  Commands such as `Query Replace'
   use this macro.

   The syntax is as follows:
      (COMMAND-CASE ( {key value}* )
        {( {( {tag}* )  |  tag}  help  {form}* )}* )

   Prompt for a key-event and then executes the code in the first branch
   with a logical key-event or a character (called tags) matching the
   input.  Each character must be a standard-character, one that satisfies
   the `standard-char-p' predicate.  The dispatching mechanism compares the
   input key-event to any character tags by mapping the key-event to a
   character with `ext:key-event-char'.  If the tag is a logical key-event,
   then the search for an appropriate case compares the key-event read with
   the tag using `logical-key-event-p'.

   Within the body of command-case, there is a defined `reprompt' macro.
   It immediately exits the current branch and repeats the prompt.

   All uses of `command-case' have two built in cases, :help and :abort.
   Any give branches that include these logical key-event tags override
   these the build in branches.  The :help branch displays in a pop-up
   window a description of the valid responses using the variously
   specified help strings.  The :abort branch signals an editor-error.

   The key/value arguments control the prompting.  The following are valid
   values:

     :help
	 The automatic :help case displays this string in a pop-up window.
	 In addition it formats a description of the valid input including
	 each case's help string.

     :prompt
	 This is the prompt used when reading the key-event.

     :change-window
	 If this is true, then the echo area window becomes the current
	 window while the prompting mechanism reads a key-event.  Sometimes
	 it is desirable to maintain the current window since it may be
	 easier for users to answer the question if they can see where the
	 current point is.

     :bind
	 This specifies a variable to which the prompting mechanism binds
	 the input key-event.  Any case may reference this variable.
	 ext:key-event-char returns the character corresponding to the
	 key-event.

   True may be specified instead of a tag or list of tags for a branch.
   This then becomes the fallback branch, and it is considered after all
   other branches, including the built in :help and :abort cases.  This
   option is always left off the help description.  Every command-case has
   a fallback branch; the macro includes a built in one that beep's and
   reprompts."
  (do* ((forms forms (cdr forms))
	(form (car forms) (car forms))
	(cases ())
	(bname (gensym))
	(again (gensym))
	(n-prompt (gensym))
	(n-change (gensym))
	(bind-char (gensym))
	(docs ())
	(t-case `(t (beep) (reprompt))))
       ((atom forms)
	`(macrolet ((reprompt ()
		      `(progn
			 (setf ,',bind
			       (prompt-for-key-event* ,',n-prompt ,',n-change))
			 (setf ,',bind-char (ext:key-event-char ,',bind))
			 (go ,',AGAIN))))
	   (declaim (inline help))
	   (flet ((help (message)
		    (command-case-help message ',(reverse docs))))
	     (block ,bname
	     (let* ((,n-prompt ,prompt)
		    (,n-change ,change-window)
		    (,bind (prompt-for-key-event* ,n-prompt ,n-change))
		    (,bind-char (ext:key-event-char ,bind)))
	       (tagbody
		,AGAIN
		(return-from
		 ,bname
		 (cond ,@(nreverse cases)
		       ((logical-key-event-p ,bind :abort)
			(editor-error "Help exited."))
		       ((logical-key-event-p ,bind :help)
			(command-case-help ,help ',(nreverse docs))
			(reprompt))
		       ,t-case))))))))

    (cond ((atom form)
	   (error "Malformed Command-Case clause: ~S" form))
	  ((eq (car form) t)
	   (setq t-case form))
	  ((or (< (length form) 2)
	       (not (stringp (second form))))
	   (error "Malformed Command-Case clause: ~S" form))
	  (t
	   (let ((tag (car form))
		 (rest (cddr form)))
	     (cond ((atom tag)
		    (push (cons (command-case-tag tag bind bind-char) rest)
			  cases)
		    (setq tag (list tag)))
		   (t
		    (do ((tag tag (cdr tag))
			 (res ()
			      (cons (command-case-tag (car tag) bind bind-char)
				    res)))
			((null tag)
			 (push `((or ,@res) . ,rest) cases)))))
	     (push (cons tag (second form)) docs))))))



;;;; Some random macros used everywhere.

(defmacro strlen (str) `(length (the simple-string ,str)))
;; FIX rename comf (like logcom) when comf in setup.lisp renamed
(defmacro complf (a) `(setf ,a (fi ,a)))

(defmacro in-lisp (&body body)
  "Evaluate $body inside a call to `handle-lisp-errors'.  Bind *package* to
   the package named by *Current Package* if it is true.  This is for
   evaluating Lisp code."
  (let ((name (gensym)) (package (gensym)))
    `(handle-lisp-errors
      (let* ((,name (value ed::current-package))
	     (,package (and ,name (find-package ,name))))
	(progv (if ,package '(*package*)) (if ,package (list ,package))
	  ,@body)))))


;;;; with-temp-buffer

(defmacro with-temp-buffer ((buffer &optional pathname) &body (body decls))
  "$buffer the file named $pathname for the duration of $body."
  `(let ((,buffer (make-unique-buffer (symbol-name (gensym)))))
     ,(if pathname
	  `(elet ((ed::read-file-hook))
	     (ed::read-buffer-file ,pathname ,buffer)))
     (unwind-protect
	 (progn
	   ,@decls
	   ,@body)
       ,(if pathname () `(setf (buffer-modified ,buffer) ()))
       (ed::kill-buffer-command () (buffer-name ,buffer)))))


;;;; Stuff from here on is implementation dependant.


;;;; WITH-INPUT & WITH-OUTPUT macros.

(defvar *free-editor-output-streams* ()
  "This variable contains a list of free editor output streams.")

;; FIX rename to-mark?
(defmacro with-output-to-mark ((var mark &optional (buffered ':line))
			       &body gorms)
  "with-output-to-mark (var mark [buffered]) {declaration}* {form}*

   Bind $var to a stream which inserts output at the permanent mark $mark
   for the duration of $gorms.  Buffered is the same as for
   `make-editor-output-stream'."
  (parse-forms (decls forms gorms)
    `(let ((,var (pop *free-editor-output-streams*)))
       ,@decls
       (if ,var
	   (modify-editor-output-stream ,var ,mark ,buffered)
	   (setq ,var (make-editor-output-stream ,mark ,buffered)))
       (unwind-protect
	   (progn ,@forms)
	 (setf (editor-output-stream-mark ,var) nil)
	 (push ,var *free-editor-output-streams*)))))

(defvar *free-editor-region-streams* ()
  "This variable contains a list of free editor input streams.")

;; FIX rename from-region?
(defmacro with-input-from-region ((var region) &body gorms)
  "with-input-from-region (var region) {declaration}* {form}*

   Bind $var to a stream which returns input from $region for the duration
   of $gorms."
  (parse-forms (decls forms gorms)
    `(let ((,var (pop *free-editor-region-streams*)))
       ,@decls
       (if ,var
	   (setq ,var (modify-editor-region-stream ,var ,region))
	   (setq ,var (make-editor-region-stream ,region)))
       (unwind-protect
	 (progn ,@forms)
	 (delete-mark (editor-region-stream-mark ,var))
	 (push ,var *free-editor-region-streams*)))))

(defmacro with-output-to-window ((stream name) &body forms)
  "With-Output-To-Window (Stream Name) {Form}*
   Bind Stream to a stream that writes into the buffer named Name a la
   With-Output-To-Mark.  The buffer is created if necessary and the buffer
   is displayed in the next window (which is created if necessary) if it is
   not already displayed.  For the duration of the evaluation this window
   is made the current window."
  (let ((nam (gensym)) (buffer (gensym)) (point (gensym))
	(window (gensym)) (old-window (gensym)))
    `(let* ((,nam ,name)
	    (,buffer (or (getstring ,nam *buffer-names*) (make-buffer ,nam)))
	    (,point (buffer-end (buffer-point ,buffer)))
	    (,window (or (car (buffer-windows ,buffer))
			 (if (<= (length *window-list*) 2)
			     (make-window ,point)
			     (prog1
				 (next-window (current-window))
			       (setf (window-buffer
				      (next-window (current-window)))
				     ,buffer)))))
	    (,old-window (current-window)))
       (unwind-protect
	 (progn (setf (current-window) ,window)
		(buffer-end ,point)
		(with-output-to-mark (,stream ,point) ,@forms))
	 (setf (current-window) ,old-window)))))



;;;; Pop-ups.

(defvar *random-typeout-switch* ()
  "Flag for switching to reference buffer in Random Typout.")

(defmacro with-pop-up-display ((var &key height (buffer-name "Random Typeout")
				         (reference nil))
			       &body (body decls))
  ;; FIX doc reference
  "Execute $body in a context with $var bound to a stream.

   Collect output to this stream and try to pop up a display of the
   appropriate height containing all the output.  When $height is supplied,
   create the pop-up display immediately, forcing output on line breaks.

   Save the output in a buffer named $name.  When the window is too small
   for the output, the display mechanism proveds FIX more-style scrolling.
   This is useful for displaying information of temporary interest.

   When a buffer with name name $already exists and was created by some
   other function signal an error."
  (and (numberp height) (zerop height)
       (editor-error "A pop-up window must have a height"))
  (let ((cleanup-p (gensym))
	(stream (gensym)))
    `(let ((,cleanup-p nil)
	   (*random-typeout-switch* nil)
	   (,stream (get-random-typeout-info ,buffer-name ,height)))
       (clearf *busy*)
       (if (buffer-modeline-field-p *echo-area-buffer* :busy)
	   (update-modeline-field *echo-area-buffer* *echo-area-window*
				  (modeline-field :busy)))
       (if (buffer-modeline-field-p *echo-area-buffer* :busy-or-menu)
	   (update-modeline-field *echo-area-buffer* *echo-area-window*
				  (modeline-field :busy-or-menu)))
       (redisplay)
       (unwind-protect
	   (progn
	     (catch 'more-punt
	       ,(when height
		  ;; Test height since it may be supplied, but evaluate
		  ;; to nil.
		  `(when ,height
		     (prepare-for-random-typeout ,stream ,height)
		     (setf ,cleanup-p t)))
	       (let ((,var ,stream))
		 ,@decls
		 (multiple-value-prog1
		     (progn ,@body)
		   (unless ,height
		     (prepare-for-random-typeout ,stream nil)
		     (setf ,cleanup-p t)
		     (funcall (device-random-typeout-full-more
			       (device-hunk-device
				(window-hunk
				 (random-typeout-stream-window ,stream))))
			      ,stream))
		   (end-random-typeout ,var ,reference))))
	     (setf ,cleanup-p nil))
	 (when ,cleanup-p (random-typeout-cleanup ,stream))))))

#[ Pop-Up Windows

Some commands print out information that is of little permanent value, and
these commands use a pop-up window to display the information.  It is known
as a pop-up window because it temporarily appears on the screen overlaying
text already displayed.  Most commands of this nature can generate their
output quickly, but in case there is a lot of output, or the user wants to
repeatedly refer to the same output while editing, the editor saves the
output in a buffer.  Different commands may use different buffers to save
their output, and we refer to these as random typeout buffers.

If the amount of output exceeds the size of the pop-up window, the editor
displays the message "--More--" after each window full.  The following are
valid responses to this prompt:

  % Space, y

    Display the next window full of text.

  % Delete, Backspace, n

    Abort any further output.

  % Escape, !

    Remove the window and continue saving any further output in the buffer.

  % k

    This is the same as ! or escape, but the editor makes a normal window
    over the pop-up window.  This only works on bitmap devices.

Any other input causes the system to abort using the key-event to determine
the next command to execute.

When the output is complete, the editor displays the string "--Flush--" in
the pop-up window's modeline, indicating that the user may flush the
temporary display.  Typing any of the key-events described above removes
the pop-up window, but typing k still produces a window suitable for normal
editing.  Any other input also flushes the display, but the editor uses the
key-event to determine the next command to invoke.

{command:Select Random Typeout Buffer}

Random typeout buffers are always in `Fundamental' mode.
]#

;; FIX may need to get-random-typeout-info
;; FIX handle when in echo area
(defmacro with-pop-up-window ((buffer-var window-var
					  &key
					  (buffer-name "Random Typeout")
					  modes
					  modeline-fields
					  delete-hook)
			      &body (body decls))
  (let ((new-window (gensym))
	(old-buffer (gensym)))
    `(let* ((,buffer-var)
	    (,old-buffer (current-buffer))
	    (,new-window
	     (if (eq (next-window (current-window)) (current-window))
		 (make-window (window-display-start (current-window))
			      :error ()))))
       (unwind-protect
	   (let ((,window-var (or ,new-window (current-window))))
	     ,@decls
	     (setq ,buffer-var (make-unique-buffer
				,buffer-name
				:modes (or ,modes (value ed::default-modes))
				:modeline-fields (or ,modeline-fields
						     (value ed::default-modeline-fields))
				:delete-hook ,delete-hook))
	     (setf (window-buffer ,window-var) ,buffer-var)
	     (if ,new-window (setf (current-window) ,new-window))
	     (setf (current-buffer) ,buffer-var)
	     ,@body)
	 (and ,new-window
	      (> (length *window-list*) 2)
	      (delete-window ,new-window))
	 (when ,buffer-var
	   (when (member ,buffer-var *buffer-list*)
	     (let ((new (or (and ,old-buffer
				 (member ,old-buffer *buffer-list*)
				 ,old-buffer)
			    (let ((b (car *buffer-list*)))
			      (if (eq b ,old-buffer)
				  (cadr *buffer-list*)
				  b)))))
	       (dolist (window *window-list*)
		 (if (equal (window-buffer window) ,buffer-var)
		     (setf (window-buffer window) new)))
	       (if (eq (current-buffer) ,buffer-var)
		   (setf (current-buffer) new))))
	   (delete-buffer ,buffer-var))))))

;; FIX window should auto-close if original buffer is q'd
;;       eg initial diary entry popup win should close on keypress q
;;       if the window was created
(defmacro in-pop-up-window ((buffer-var window-var
					  &key
					  (buffer-name "Random Typeout")
					  modes
					  modeline-fields
					  delete-hook)
			      &body (body decls))
  (let ((new-window (gensym))
	(old-buffer (gensym)))
    `(let* ((,buffer-var)
	    (,new-window
	     (if (eq (next-window (current-window)) (current-window))
		 (make-window (window-display-start (current-window))
			      :error ()))))
       (let ((,window-var (or ,new-window (current-window))))
	 ,@decls
	 (setq ,buffer-var (make-unique-buffer
			    ,buffer-name
			    :modes (or ,modes (value ed::default-modes))
			    :modeline-fields (or ,modeline-fields
						 (value ed::default-modeline-fields))
			    :delete-hook ,delete-hook))
	 (setf (window-buffer ,window-var) ,buffer-var)
	 (if ,new-window (setf (current-window) ,new-window))
	 (setf (current-buffer) ,buffer-var)
	 ,@body))))

(proclaim '(special *random-typeout-ml-fields* *buffer-names*))

(defvar *random-typeout-buffers* ()
  "An association list mapping random typeout buffers to the streams that
   operate on the buffers.")

(defun get-random-typeout-info (buffer-name line-buffered-p)
  (let* ((buffer (getstring buffer-name *buffer-names*))
	 (stream
	  (cond
	   ((not buffer)
	    (let* ((buf (make-buffer
			 buffer-name
			 :modes '("Fundamental")
			 :modeline-fields *random-typeout-ml-fields*
			 :delete-hook
			 (list #'(lambda (buffer)
				   (setq *random-typeout-buffers*
					 (delete buffer *random-typeout-buffers*
						 :key #'car))))))
		   (point (buffer-point buf))
		   (stream (make-random-typeout-stream
			    (copy-mark point :left-inserting))))
	      (setf (random-typeout-stream-more-mark stream)
		    (copy-mark point :right-inserting))
	      (push (cons buf stream) *random-typeout-buffers*)
	      stream))
	   ((member buffer *random-typeout-buffers* :key #'car)
	    (delete-region (buffer-region buffer))
	    (let* ((pair (assoc buffer *random-typeout-buffers*))
		   (stream (cdr pair)))
	      (setf *random-typeout-buffers*
		    (cons pair (delete pair *random-typeout-buffers*)))
	      (setf (random-typeout-stream-first-more-p stream) t)
	      (setf (random-typeout-stream-no-prompt stream) nil)
	      stream))
	   (t
	    (error "~A is not a random typeout buffer."
		   (buffer-name buffer))))))
    (if line-buffered-p
	(setf (random-typeout-stream-out stream) #'random-typeout-line-out
	      (random-typeout-stream-sout stream) #'random-typeout-line-sout
	      (random-typeout-stream-misc stream) #'random-typeout-line-misc)
	(setf (random-typeout-stream-out stream) #'random-typeout-full-out
	      (random-typeout-stream-sout stream) #'random-typeout-full-sout
	      (random-typeout-stream-misc stream) #'random-typeout-full-misc))
    stream))


#[ Internal Errors

A message of this form may appear in the echo
area, accompanied by a beep:

Internal error:
Wrong type argument, NIL, should have been of type SIMPLE-VECTOR.

If the error message is a file related error such as the following, then
you have probably done something illegal which the editor did not catch,
but was detected by the file system:

Internal error:
No access to "/lisp2/emacs/teco.mid"

Otherwise, you have found a bug.  Try to avoid the behavior that resulted
in the error and report the problem to your system maintainer.  Since Lisp
has fairly robust error recovery mechanisms, probably no damage has been
done.

If a truly abominable error from which the editor cannot recover occurs,
then you will be thrown into the Lisp debugger.  At this point it would be
a good idea to save any changes with save-all-buffers and then start
a new Lisp.

The Lisp function save-all-buffers may
be used to save modified buffers in a seriously broken the editor.  To use this,
type "(save-all-buffers)" to the top-level ("* ") or debugger
("1] ") prompt and confirm saving of each buffer that should be saved.
Since this function will prompt in the "Lisp" window, it isn't very useful
when called inside the editor.
]#


;;;; Error handling stuff.

#[ Error Handling

When an error happens inside the editor, the editor will trap the error and
display the error message in the echo area, possibly along with the
"Internal error:" prefix.  If you want to debug the error, type ?.
This causes the prompt "Debug:" to appear in the echo area.  The following
commands are recognized:

  d
     Enter a break-loop so that you can use the Lisp debugger.
     Proceeding with "go" will reenter the editor and give the "Debug:"
     prompt again.

  e
     Display the original error message in a pop-up window.

  b
     Show a stack backtrace in a pop-up window.

  q, Escape
     Quit from this error to the nearest command loop.

  r
     Display a list of the restart cases and prompt for the number of a
     restart-case with which to continue.  Restarting may result in prompting in
     the window in which Lisp started.

Only errors within the editor process are handled in this way.  Errors during
eval server operations are handled using normal terminal I/O on a typescript in
the eval server's slave buffer or background buffer (see page
pagerefoperations).  Errors due to interaction in a slave buffer will cause
the debugger to be entered in the slave buffer.
]#

(proclaim '(special *echo-area-stream*))

;;; LISP-ERROR-ERROR-HANDLER is in macros.lisp instead of rompsite.lisp because
;;; it uses WITH-POP-UP-DISPLAY, and macros is compiled after rompsite.  It
;;; binds an error condition handler to get us out of here on a recursive error
;;; (we are already handling one if we are here).  Since COMMAND-CASE uses
;;; EDITOR-ERROR for logical :abort characters, and this is a subtype of ERROR,
;;; we bind an editor-error condition handler just inside of the error handler.
;;; This keeps us from being thrown out into the debugger with supposedly
;;; recursive errors occuring.  What we really want in this case is to simply
;;; get back to the command loop and forget about the error we are currently
;;; handling.
;;;
(defun lisp-error-error-handler (condition &optional internalp)
  (handler-bind ((editor-error #'(lambda (condx)
				   (declare (ignore condx))
				   (beep)
				   (throw 'command-loop-catcher nil)))
		 (error #'(lambda (condition)
;			    (declare (ignore condition))
			    (with-screen
			     (invoke-debugger condition)))))
;  			    (invoke-debugger
;  			     (make-condition
;  			      'simple-condition
;  			      :format-control
;  			      "Error in error handler; editor broken.")))))
    (clear-echo-area)
    (clear-editor-input *editor-input*)

    (beep)
    (if internalp (write-string "Internal error: " *echo-area-stream*))
    (princ condition *echo-area-stream*)
    (log-message (if internalp "Internal error: ~A" "~A") condition)
    (if (or (value ed::inspect-on-error)
	    (let* ((*editor-input* *real-editor-input*)
		   (key-event (get-key-event *editor-input* t)))
	      (if (eq key-event #k"?")
		  t
		  (progn
		    (unget-key-event key-event *editor-input*)
		    ()))))
	(with-pop-up-window (buffer window :buffer-name "Inspect")
	  (setf (window-display-recentering window) t)
	  (inspect buffer condition)))
    (signal 'editor-top-level-catcher ())))

(defmacro handle-lisp-errors (&body body)
  "handle-lisp-errors {form}*

   Setup a context where any Lisp errors that occur are handled gracefully.

   This macro should be wrapped around code in which the user input may
   produce an error -- for example, code that evaluate given lisp forms.

   Using this in a command allows the command to shadow the editor-error
   handler.  Outside this context commands must take care to signal editor
   errors for errors caused by user input."
  `(handler-bind ((error #'lisp-error-error-handler))
     ,@body))
