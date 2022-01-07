;;; Routines which define commands, and the command interpreter.

(in-package "EDI")

(export '(bind-key delete-key-binding get-command map-bindings
	  make-command free-command command-name command-bindings
	  visible-command-bindings last-command-type
	  prefix-argument exit *invoke-hook* key-translation))

(defun %print-hcommand (obj stream depth)
  (declare (ignore depth))
  (write-string "#<Editor Command \"" stream)
  (write-string (command-name obj) stream)
  (write-string "\">" stream))


;;;; Key Tables.
;;;
;;; A key table provides a way to translate a sequence of characters to
;;; some lisp object.  It is currently represented by a tree of
;;; hash-tables, where each level is a hashing from a key to either another
;;; hash-table or a value.

;;; GET-TABLE-ENTRY returns the value at the end of a series of hashings.  For
;;; our purposes it is presently used to look up commands and key-translations.
;;;
(defun get-table-entry (table key)
  (let ((foo nil))
    (dotimes (i (length key) foo)
      (let ((key-event (aref key i)))
	(setf foo (gethash key-event table))
	(unless (hash-table-p foo) (return foo))
	(setf table foo)))))

;;; SET-TABLE-ENTRY sets the entry for key in table to val, creating new
;;; tables as needed.  If val is nil, then use REMHASH to remove this element
;;; from the hash-table.
;;;
(defun set-table-entry (table key val)
  (dotimes (i (1- (length key)))
    (let* ((key-event (aref key i))
	   (foo (gethash key-event table)))
      (if (hash-table-p foo)
	  (setf table foo)
	  (let ((new-table (make-hash-table)))
	    (setf (gethash key-event table) new-table)
	    (setf table new-table)))))
  (if (null val)
      (remhash (aref key (1- (length key))) table)
      (setf (gethash (aref key (1- (length key))) table) val)))


;;;; Key Translation.
;;;
;;; Key translations are maintained using a key table.  If a value is an
;;; integer, then it is prefix bits to be OR'ed with the next character.
;;; If it is a key, then we translate to that key.

#[ Key Translation

Key translation is a process that the command interpreter applies to keys
before doing anything else.  There are two kinds of key translations:
substitution and bit-prefix.  In either case, the command interpreter
translates a key when a specified key-event sequence appears in a key.

In a substitution translation, the system replaces the matched subsequence with
another key-event sequence.  Key translation is not recursively applied to the
substituted key-events.

In a bit-prefix translation, the system removes the matched subsequence and
effectively sets the specified bits in the next key-event in the key.

While translating a key, if the system encounters an incomplete final
subsequence of key-events, it aborts the translation process.  This happens
when those last key-events form a prefix of some translation.  It also happens
when they translate to a bit-prefix, but there is no following key-event to
which the system can apply the indicated modifier.  If there is a binding for
this partially untranslated key, then the command interpreter will invoke that
command; otherwise, it will wait for the user to type more key-events.

{function:ed:key-translation}
]#

(defvar *key-translations* (make-hash-table))
(defvar *translate-key-temp* (make-array 10 :fill-pointer 0 :adjustable t))

;;; TRANSLATE-KEY  --  Internal
;;;
;;; This is used internally to do key translations when we want the
;;; canonical representation for Key.  Result, if supplied, is an
;;; adjustable vector with a fill pointer.  We compute the output in this
;;; vector.  If the key ends in the prefix of a translation, we just return
;;; that part untranslated and return the second value true.
;;;
(defun translate-key (key &optional (result (make-array (length key)
							:fill-pointer 0
							:adjustable t)))
  (let ((key-len (length key))
	(temp *translate-key-temp*)
	(start 0)
	(try-pos 0)
	(prefix 0))
    (setf (fill-pointer temp) 0)
    (setf (fill-pointer result) 0)
    (loop
      (when (= try-pos key-len) (return))

      ;; FIX If at the end of a hyper prefix set `prefix' 0 and check whether
      ;; the next key begins a hyper prefix.  If so carry on (the following
      ;; hyper can do the prefixing), otherwise revert `prefix' and
      ;; create the key event with the reverted value (the event will be
      ;; the actual hyper-modified key).

      (let ((key-event (aref key try-pos)))
	(vector-push-extend
	 (ext:make-key-event key-event (logior (ext:key-event-bits key-event)
					       prefix))
	 temp)
	(setf prefix 0))
      (let ((entry (get-table-entry *key-translations* temp)))
	(cond ((hash-table-p entry)
	       (incf try-pos))
	      (t
	       (etypecase entry
		 (null
		  (vector-push-extend (aref temp 0) result)
		  (incf start))
		 (simple-vector
		  (dotimes (i (length entry))
		    (vector-push-extend (aref entry i) result))
		  (setf start (1+ try-pos)))
		 (integer
		  (setf start (1+ try-pos))
		  (when (= start key-len) (return))
		  ;; FIX why respect existing prefix if existing prefix set 0 above?
		  (setf prefix (logior entry prefix))))
	       (setq try-pos start)
	       (setf (fill-pointer temp) 0)))))
    (dotimes (i (length temp))
      (vector-push-extend (aref temp i) result))
    (values result (not (zerop (length temp))))))

;;; KEY-TRANSLATION -- Public.
;;;
(defun key-translation (key)
  "Return the key translation for $key if there is one, else ().

   $key is either a key-event or a sequence of key-events.  If $key is a
   prefix of a translation, then return :prefix.

   A key translation is either a key or modifier bit specification.  A bit
   translation is a list (:bits {bit-name}*).  In this case, the named bits
   will be set in the next character in the key being translated.

   Whenever $key appears as a subsequence of a key argument to the binding
   manipulation functions, that portion will be replaced with the
   translation.

   This form is setf'able.  Setting the form registers a key translation
   that the command interpreter will use on receiving key-events."
  (let ((entry (get-table-entry *key-translations* (crunch-key key))))
    (etypecase entry
      (hash-table :prefix)
      ((or simple-vector null) entry)
      (integer
       (cons :bits (ext:key-event-bits-modifiers entry))))))

;;; %SET-KEY-TRANSLATION  --  Internal
;;;
(defun %set-key-translation (key new-value)
  (let ((entry (cond ((and (consp new-value) (eq (car new-value) :bits))
		      (apply #'ext:make-key-event-bits (cdr new-value)))
		     (new-value (crunch-key new-value))
		     (t new-value))))
    (set-table-entry *key-translations* (crunch-key key) entry)
    new-value))
;;;
(defsetf key-translation %set-key-translation
  "Set the key translation for a key.  If set to null, deletes any
  translation.")


;;;; Interface Utility Functions.

(defvar *global-command-table* (make-hash-table)
  "The command table for global key bindings.")

;;; GET-RIGHT-TABLE  --  Internal
;;;
;;; Return a hash-table depending on "kind" and checking for errors.
;;;
(defun get-right-table (kind where)
  (case kind
     (:global
      (when where
	(error "Where argument ~S is meaningless for :global bindings."
	       where))
      *global-command-table*)
     (:mode (let ((mode (getstring where *mode-names*)))
	      (unless mode
		(error "~S is not a defined mode." where))
	      (mode-object-bindings mode)))
     (:buffer (unless (bufferp where)
		(error "~S is not a buffer." where))
	      (buffer-bindings where))
     (t (error "~S is not a valid binding type." kind))))

;;; CRUNCH-KEY  --  Internal.
;;;
;;; Take a key in one of the various specifications and turn it into the
;;; standard one: a simple-vector of characters.
;;;
(defun crunch-key (key)
  (typecase key
    (ext:key-event (vector key))
    ((or list vector) ; List thrown in gratuitously.
     (if (zerop (length key))
	 (error "Key must have length."))
     (or (every #'ext:key-event-p key)
	 (error "A Key ~S must contain only key-events." key))
     (coerce key 'simple-vector))
    (t
     (error "Key must be either a key-event or sequence of key-events: ~S"
	    key))))


;;;; Exported Primitives.

(defvar *command-names* (make-string-table)
  "A string-table (as in [string tables]) associating command names to
   commands.  Whenever a new command is defined it is entered in this
   table.")

#[ Binding Commands to Keys

The command interpreter determines which command to invoke on the basis of
key bindings.  A key binding is an association between a command and a
sequence of key-events (see section refkey-events-intro.  A sequence of
key-events is called a key and is represented by a single key-event or a
sequence (list or vector) of key-events.

Since key bindings may be local to a mode or buffer, [the current
environment] determines the set of key
bindings in effect at any given time.  When the command interpreter tries
to find the binding for a key, it first checks if there is a local binding
in the `current-buffer', then if there is a binding in each of the minor
modes and the major mode for the current buffer (page pagerefmodes), and
finally checks to see if there is a global binding.  If no binding is
found, then the command interpreter beeps or flashes the screen to indicate
this.

{function:ed:bind-key}
{function:ed:command-bindings}
{function:ed:delete-key-binding}
{function:ed:get-command}
{function:ed:map-bindings}
]#

#[ Transparent Key Bindings

Key bindings local to a mode may be transparent.  A transparent key
binding does not shadow less local key bindings, but rather indicates that
the bound command should be invoked before the first normal key binding.
Transparent key bindings are primarily useful for implementing minor modes
such as auto fill and word abbreviation.  There may be several transparent
key bindings for a given key, in which case all of the commands bound are
invoked in the order they were found.  If there no normal key binding for a
key typed, then the command interpreter acts as though the key is unbound
even if there are transparent key bindings.

The :transparent-p argument to `defmode' determines whether the key
bindings in a mode are transparent.
]#

;;; BIND-KEY  --  Public.
;;;
(defun bind-key (name key &optional (kind :global) where)
  "Bind (associate) command $name and $key in some environment.

   $key is either a key-event or a sequence of key-events.  Processes $key
   for [key translation]s before establishing the binding.  If $key is some
   prefix of a key binding which already exists in the specified place,
   then override the old one with the new one.

   There are three possible values of $kind:

     :global
	 Make a global key binding.

     :mode
	 Make a mode specific key binding in the mode whose name is $where.

     :buffer
	 Make a binding which is local to buffer $where.

   `ext:do-alpha-key-events' is useful for setting up bindings in certain
   new modes."
  (let ((cmd (getstring name *command-names*))
	(table (get-right-table kind where))
	(key (copy-seq (translate-key (crunch-key key)))))
    (cond (cmd
	   (set-table-entry table key cmd)
	   (push (list key kind where) (command-%bindings cmd))
	   cmd)
	  (t
	   (with-simple-restart (continue "Go on, ignoring binding attempt.")
	     (error "~S is not a defined command." name))))))

;;; DELETE-KEY-BINDING  --  Public
;;;
;;; Stick () in the key table specified.
;;;
(defun delete-key-binding (key &optional (kind :global) where)
  "Remove the binding of $key in some place.

   $key is either a key-event or a sequence of key-events.  Signal an error
   if the binding of $key is empty.  Processes key for [key translation]s
   before removing the binding.

   $kind is the kind of binding to delete, one of :global, :mode or
   :buffer.  If kind is :mode, then $where is the mode name, and if kind is
   :buffer, then $where is the buffer."
  (set-table-entry (get-right-table kind where)
		   (translate-key (crunch-key key))
		   nil))

;;; GET-CURRENT-BINDING  --  Internal
;;;
;;; Look up a key in the current environment.
;;;
(defun get-current-binding (key)
  (let ((res (get-table-entry (buffer-bindings *current-buffer*) key)))
    (cond
     (res (values res nil))
     (t
      (do ((mode (buffer-mode-objects *current-buffer*) (cdr mode))
	   (t-bindings ()))
	  ((null mode)
	   (values (get-table-entry *global-command-table* key)
		   (nreverse t-bindings)))
	(declare (list t-bindings))
	(let ((res (get-table-entry (mode-object-bindings (car mode)) key)))
	  (when res
	    (if (mode-object-transparent-p (car mode))
		(push res t-bindings)
		(return (values res (nreverse t-bindings)))))))))))

;;; GET-COMMAND -- Public.
;;;
(defun get-command (key &optional (kind :global) where)
  "Return the command bound to $key if there is one, else return ().

   $key is either a key-event or a sequence of key-events.  If $key is an
   initial subsequence of some keys, then return the keyword :prefix.
   Process $key for [key translation]s before looking for any binding.

   There are four cases of kind:

     :current
	Return the current binding of $key using the current buffer's
	search list.  If there are any transparent key bindings for $key,
	then they are returned in a list as a second value.

     :global
	 Return the global binding of $key.

     :mode
	 Return the binding of $key in the mode named $where.

     :buffer
	 Return the binding of $key local to the buffer $where."
  (multiple-value-bind (key prefix-p)
		       (translate-key (crunch-key key))
    (let ((entry (if (eq kind :current)
		     (get-current-binding key)
		     (get-table-entry (get-right-table kind where) key))))
      (etypecase entry
	(null (if prefix-p :prefix nil))
	(command entry)
	(hash-table :prefix)))))

(defvar *map-bindings-key* (make-array 5 :adjustable t :fill-pointer 0))

;;; MAP-BINDINGS -- Public.
;;;
(defun map-bindings (function kind &optional where)
  "Map $function over the key bindings in some place.

   For each binding, pass $function with the key and command bound to the
   binding.

   There are three possible values of $kind:

     :global
	 Make a global key binding.

     :mode
	 Make a mode specific key binding in the mode whose name is $where.

     :buffer
	 Make a binding which is local to buffer $where.

  The key passed to the function may be reused after a given iteration."
  (labels ((mapping-fun (hash-key hash-value)
	     (vector-push-extend hash-key *map-bindings-key*)
	     (etypecase hash-value
	       (command (funcall function *map-bindings-key* hash-value))
	       (hash-table (maphash #'mapping-fun hash-value)))
	     (decf (fill-pointer *map-bindings-key*))))
    (setf (fill-pointer *map-bindings-key*) 0)
    (maphash #'mapping-fun (get-right-table kind where))))

;;; MAKE-COMMAND -- Public.
;;;
;;; If the command is already defined, then alter the command object;
;;; otherwise, make a new command object and enter it into the *command-names*.
;;;
(defun make-command (name documentation function)
  "Create a new editor command with NAME and DOCUMENTATION which is
   implemented by calling the function-value of the symbol FUNCTION.  Enter
   the command in the string-table *command-names*, with the command object
   as its value.  This permits access to the command definition mechanism
   at a lower level, which is occasionally useful.  Normally the defcommand
   macro implements commands."
  (let ((entry (getstring name *command-names*)))
    (cond
     (entry
      (setf (command-name entry) name)
      (setf (command-documentation entry) documentation)
      (setf (command-function entry) function))
     (t
      (setf (getstring name *command-names*)
	    (internal-make-command name documentation function))))))

;;; FREE-COMMAND -- Public.
;;;
(defun free-command (name)
  "Free the editor command $name, including any bindings."
  (let ((command (getstring name *command-names*)))
    (dolist (binding (command-bindings command))
      (apply #'delete-key-binding binding))
    (delete-string name *command-names*)))

;;; COMMAND-NAME, %SET-COMMAND-NAME -- Public.
;;;
(defun command-name (command)
  "Returns the string which is the name of Command."
  (command-%name command))
;;;
(defun %set-command-name (command new-name)
  (check-type command command)
  (check-type new-name string)
  (setq new-name (coerce new-name 'simple-string))
  (delete-string (command-%name command) *command-names*)
  (setf (getstring new-name *command-names*) command)
  (setf (command-%name command) new-name))

;;; COMMAND-BINDINGS -- Public.
;;;
;;; Check that all the supposed bindings really exists.  Bindings which
;;; were once made may have been overwritten.  It is easier to filter
;;; out bogus bindings here than to catch all the cases that can make a
;;; binding go away.
;;;
(defun command-bindings (command)
  "Return a list of all the places where $command is bound.

   A place is specified as a list (key kind where) in whcih key is always a
   vector and where is either the mode or buffer to which the binding is
   local, or () if the binding is a global."
  (check-type command command)
  (let (result)
    (declare (list result))
    (dolist (place (command-%bindings command))
      (let ((table (case (cadr place)
		   (:global *global-command-table*)
		   (:mode
		    (let ((m (getstring (caddr place) *mode-names*)))
		      (when m (mode-object-bindings m))))
		   (t
		    (when (memq (caddr place) *buffer-list*)
		      (buffer-bindings (caddr place)))))))
	(when (and table
		   (eq (get-table-entry table (car place)) command)
		   (not (member place result :test #'equalp)))
	  (push place result))))
    result))

;;; VISIBLE-COMMAND-BINDINGS -- Public.
;;;
(defun visible-command-bindings (command)
  "Return a list of lists of the form (key kind where) describing all the
   visible places where Command is bound, i.e. the global bindings, the
   bindings in the current modes and the bindings in the current buffer."
  (check-type command command)
  (let ((buffer (current-buffer)) result)
    (declare (list result))
    (dolist (place (command-%bindings command))
      (let ((table (case (cadr place)
		     (:global *global-command-table*)
		     (:mode
		      (let* ((name (caddr place))
			     (m (getstring name *mode-names*)))
			(when (and m
				   (or (string= (string-downcase (buffer-major-mode buffer))
						(string-downcase name))
				       (if (mode-major-p name)
					   nil
					   (buffer-minor-mode buffer name))))
			  (mode-object-bindings m))))
		     (t
		      (when (eq (caddr place) buffer)
			(buffer-bindings (caddr place)))))))
	(when (and table
		   (eq (get-table-entry table (car place)) command)
		   (not (member place result :test #'equalp)))
	  (push place result))))
    result))

#[ Command Types

In many editors the behavior of a command depends on the kind of command
invoked before it.  The editor provides a mechanism to support this, known
as command type.

{function:ed:last-command-type}
]#

(defvar *last-command-type* ()
  "The command-type of the last command invoked.")
(defvar *command-type-set* ()
  "True if the last command set the command-type.")

;;; LAST-COMMAND-TYPE  --  Public
;;;
(defun last-command-type ()
  "Return the command type of the last command invoked.

   If the previous interactively invoked command set last-command-type,
   then that is the value of last command type, otherwise the last command
   type is ().  Normally a command type is a keyword.  The command type is
   left intact after a command is invoked due to a transparent key binding.

   If this is set with setf, the supplied value becomes the value of
   last-command-type until the next command completes."  ; FIX setf doc
  *last-command-type*)

;;; %SET-LAST-COMMAND-TYPE  --  Internal
;;;
;;; Set the flag so we know not to clear the command-type.
;;;
(defun %set-last-command-type (type)
  (setq *last-command-type* type *command-type-set* t))

#[ Command Arguments

There are three ways in which a command may be invoked: It may be bound to a
key which has been typed, it may be invoked as an extended command, or it may
be called as a Lisp function.  Ideally commands should be written in such a way
that they will behave sensibly no matter which way they are invoked.  The
functions which implement commands must obey certain conventions about argument
passing if the command is to function properly.

[ The Prefix Argument ]
[ Lisp Arguments      ]
]#

#[ The Prefix Argument

Whenever a command is invoked it is passed as its first argument what
is known as the prefix argument.  The prefix argument is always
either an integer or nil.  When a command uses this value it is
usually as a repeat count, or some conceptually similar function.

{function:ed:prefix-argument}

If the prefix argument is not set by the previous command then the
prefix argument for a command is ().  The prefix argument is not cleared
after a command is invoked due to a transparent key binding.
]#

#[ Lisp Arguments

It is often desirable to call commands from Lisp code, in which case
arguments which would otherwise be prompted for are passed as optional
arguments following the prefix argument.  A command should prompt for
any remaining arguments.
]#

(defvar *prefix-argument* () "The prefix argument or ().")
(defvar *prefix-argument-supplied* ()
  "Should be set by functions which supply a prefix argument.")

;;; PREFIX-ARGUMENT  --  Public
;;;
(defun prefix-argument ()
  "Return the current value of the prefix argument.  When set with setf,
   the new value becomes the prefix argument for the next command." ; FIX  setf doc
  *prefix-argument*)

;;; %SET-PREFIX-ARGUMENT  --  Internal
;;;
(defun %set-prefix-argument (argument)
  "Set the prefix argument for the next command to Argument."
  (or (null argument) (integerp argument)
      (error "Prefix argument ~S is neither an integer nor Nil." argument))
  (setq *prefix-argument* argument  *prefix-argument-supplied* t))


;;;; The Command Loop.

#[ Commands

[ Commands Introduction   ]
[ The Command Interpreter ]
[ Command Types           ]
[ Command Arguments       ]
[ Recursive Edits         ]
]#

#[ Commands Introduction

Interaction with the editor centers around invoking commands.  Commands
have three attributes:

  % name

    A command's name provides a way to refer to it.  Command names are
    usually capitalized words separated by spaces, such as `Forward Word'.

  % documentation

    The documentation for a command is used by on-line help facilities.

  % function

    A command is implemented by a Lisp function, which is callable from
    Lisp.

{variable:ed:*command-names*}

[ Defining Commands     ]
[ Command Documentation ]
]#

#[ The Command Interpreter

The command interpreter is a function which reads key-events (see section
refkey-events-intro) from the keyboard and dispatches to different commands
on the basis of what the user types.  When the command interpreter executes a
command, we say it invokes the command.  The command interpreter also
provides facilities for communication between commands contiguously running
commands, such as a last command type register.  It also takes care of
resetting communication mechanisms, clearing the echo area, displaying partial
keys typed slowly by the user, etc.

{variable:ed:*invoke-hook*}
{evariable:Command Abort Hook}

When the editor initially starts the command interpreter is in control, but
commands may read from the keyboard themselves and assign whatever
interpretation they will to the key-events read.  Commands may call the
command interpreter recursively using the function `recursive-edit'.

[ Editor Input             ]
[ Binding Commands to Keys ]
[ Key Translation          ]
[ Transparent Key Bindings ]
[ Interactive              ]
]#

;;; Buffers we use to read and translate keys.
;;;
(defvar *current-command* (make-array 10 :fill-pointer 0 :adjustable t))
(defvar *current-translation* (make-array 10 :fill-pointer 0 :adjustable t))

(defvar *invoke-hook* #'(lambda (command p)
			  (funcall (command-function command) p))
  "A function the command interpreter calls when it wants to invoke a
   command.  The function receives the command and the prefix argument as
   arguments.  The initial value is a function which simply funcalls the
   command-function of the command with the supplied prefix argument.  This
   variable is useful for implementing keyboard macros and similar things.")

;;; %COMMAND-LOOP  --  Internal
;;;
(defun %command-loop ()
  "Read commands from the terminal and execute them, forever."
  (let  ((cmd *current-command*)
	 (trans *current-translation*)
	 *last-command-type*
	 *command-type-set*
	 *prefix-argument*
	 *prefix-argument-supplied*)
    (declare (special *last-command-type* *command-type-set*
		      *prefix-argument* *prefix-argument-supplied*))
    (setf (fill-pointer cmd) 0)
    (flet ((hyper-p (cmd)
	     (and (= (length cmd) 5)
		  ;; FIX only after `frob-hyper-key'
		  ;; This is probably keyboard-specific.
		  (eq (key-event-char (elt cmd 0) ) #\escape)
		  (eq (key-event-char (elt cmd 1) ) #\[)
		  (eq (key-event-char (elt cmd 2) ) #\2)
		  (eq (key-event-char (elt cmd 3) ) #\5)
		  (eq (key-event-char (elt cmd 4) ) #\~))))
      (handler-bind
	  ;; Bind this outside the invocation loop to save consing.
	  ((editor-error #'(lambda (condx)
			     (beep)
			     (let ((string (editor-error-format-string condx)))
			       (when string
				 (apply #'message string
					(editor-error-format-arguments condx)))
			       (throw 'command-loop-catcher nil)))))
	(clearf *busy*)
	(loop
	  (unless (eq *current-window* *echo-area-window*)
	    (if (buffer-modified *echo-area-buffer*) (clear-echo-area))
	    (or (zerop (length cmd))
		(when (value ed::key-echo-delay)
		  (editor-sleep (value ed::key-echo-delay))
		  (unless (listen-editor-input *editor-input*)
		    (clear-echo-area)
		    (if (hyper-p cmd)
			(progn
			  (write-string "Hyper-" *echo-area-stream*)
			  (redisplay))
			(dotimes (i (length cmd))
			  (ext:print-pretty-key (aref cmd i)
						*echo-area-stream*)
			  (write-char #\space *echo-area-stream*)))))))
	  (vector-push-extend (get-key-event *editor-input*) cmd)
	  (or (eq *current-buffer* *echo-area-buffer*)
	      (when (hyper-p cmd)
		(write-string "Hyper-" *echo-area-stream*)
		(redisplay)))
	  (multiple-value-bind (trans-result prefix-p)
			       (translate-key cmd trans)
	    (multiple-value-bind (res t-bindings)  ;; transparent-bindings
				 (get-current-binding trans-result)
	      (etypecase res
		(command
		 (let ((punt t))
		   (setq *busy* t)
		   (if (buffer-modeline-field-p *echo-area-buffer* :busy)
		       (update-modeline-field *echo-area-buffer*
					      *echo-area-window*
					      (modeline-field :busy)))
		   (if (buffer-modeline-field-p *echo-area-buffer*
						:busy-or-menu)
		       (update-modeline-field *echo-area-buffer*
					      *echo-area-window*
					      (modeline-field
					       :busy-or-menu)))
		   (internal-redisplay)
		   (catch 'command-loop-catcher
		     (let ((*changed-buffers* ()))
		       (dolist (c t-bindings)
			 (funcall *invoke-hook* c *prefix-argument*))
		       (funcall *invoke-hook* res *prefix-argument*)
		       (dolist (buffer *changed-buffers*)
			 (if (member buffer *buffer-list*)
			     (invoke-hook ed::after-change-hook buffer))))
		     (setf punt ()))
		   (clearf *busy*)
		   (invoke-hook ed::after-command-hook)
		   (if punt (invoke-hook ed::command-abort-hook)))
		 (if *command-type-set*
		     (setq *command-type-set* nil)
		     (setq *last-command-type* nil))
		 (if *prefix-argument-supplied*
		     (setq *prefix-argument-supplied* nil)
		     (setq *prefix-argument* nil))
		 (setf (fill-pointer cmd) 0))
		(null
		 (unless prefix-p
		   (beep)
		   (setq *prefix-argument* nil)
		   (setf (fill-pointer cmd) 0)))
		(hash-table)))))))))

;;; EXIT  --  Public
;;;
(defun exit (&optional (value t))
  "Invoke *Exit Hook*, then exit from the editor, returning $value."
  (throw 'editor-exit value))
