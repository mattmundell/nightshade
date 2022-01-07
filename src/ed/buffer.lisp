;;; Functions for changing modes and buffers.

(in-package "EDI")

(export '(*buffer-list*
	  buffer-modified buffer-region buffer-name buffer-pathname
	  buffer-major-mode buffer-minor-mode buffer-modeline-fields
	  buffer-modeline-field-p
	  current-buffer current-point current-line
	  in-recursive-edit exit-recursive-edit abort-recursive-edit
	  recursive-edit defmode mode-major-p mode-variables mode-documentation
	  unique-buffer-name make-unique-buffer
	  make-buffer delete-buffer copy-buffer with-writable-buffer
	  buffer-start-mark buffer-end-mark recursive-edit))


#[ Buffers

In addition to some text, a buffer has several other user-visible attributes:

  A name
     A buffer is identified by its name, which allows it to be selected, destroyed,
     or otherwise manipulated.

  A collection of modes
     The [modes] present in a buffer alter the set of commands available and
     otherwise alter the behavior of the editor.

  A modification flag
     This flag is set whenever the text in a buffer is modified.  It is often
     useful to know whether a buffer has been changed, since if it has it usually
     needs to be saved in its associated file eventually.

  A write-protect flag
     If this flag is true, then any attempt to modify the buffer will result in an
     error.

== Changing which buffer is current ==

{command:Select Buffer}
{command:Select Previous Buffer}
{command:Circulate Buffers}

`Select Previous Buffer' and `Circulate Buffers' are generally used
together.  Often `Select Previous Buffer' will take you where you want to
go.  If you don't end up there, then using `Circulate Buffers' will do the
trick.

{command:Rotate Buffers Forward}
{command:Rotate Buffers Backward}

== More commands ==

{command:Create Buffer}
{command:Kill Buffer}
{command:List Buffers}
{command:Insert Buffer}

== Changing buffer state ==

{command:Clear Buffer Modified}
{command:Check Buffer Modified}
{command:Set Buffer Read Only}
{command:Set Buffer Writable}
{command:Rename Buffer}

== Copying entire buffers ==

{command:Copy Buffer}
{command:Copy Buffer Next Window}
{command:Switch to Next Copy}
]#


#[ Buffer Display

If a line of text is too long to fit within the screen width it is wrapped,
with the editor displaying consecutive pieces of the text line on as many
screen lines as needed to hold the text.  the editor indicates a wrapped
line by placing a line-wrap character in the last column of each screen
line.  Currently, the line-wrap character is an exclamation point (!).  It
is possible for a line to wrap off the bottom of the screen or on to the
top.

The editor wraps screen lines when the line is completely full regardless
of the line-wrap character.  Most editors insert the line-wrap character
and wrap a single character when a screen line would be full if the editor
had avoided wrapping the line.  In this situation, the editor would leave
the screen line full.  This means there are always at least two characters
on the next screen line if the editor wraps a line of display.  When the
cursor is at the end of a line which is the full width of the screen, it is
displayed in the last column, since it cannot be displayed off the edge.

The editor displays most characters as themselves, but it treats some
specially:

  - Tabs are treated as tabs, with eight character tab-stops.

  - Characters corresponding to ASCII control characters are printed as
     ^char; for example, a formfeed is ^L.

  - Characters with the most-significant bit on are displayed as
     <hex-code>; for example, <E2>.

Since a character may be displayed using more than one printing character,
there are some positions on the screen which are in the middle of a
character.  When the cursor is on a character with a multiple-character
representation, the editor always displays the cursor on the first
character.
]#


#[ Buffer Functions

{function:ed:make-buffer}
{evariable:Make Buffer Hook}
{evariable:Default Modeline Fields}
{function:ed:bufferp}
{function:ed:buffer-name}
{evariable:Buffer Name Hook}
{function:ed:buffer-region}
{function:ed:buffer-pathname}
{evariable:Buffer Pathname Hook}
{function:ed:buffer-write-date}
{function:ed:buffer-point}
{function:ed:buffer-mark}
{function:ed:buffer-start-mark}
{function:ed:buffer-end-mark}
{function:ed:buffer-writable}
{evariable:Buffer Writable Hook}
{function:ed:buffer-modified}
{evariable:Buffer Modified Hook}
{function:ed:with-writable-buffer}
{function:ed:buffer-signature}
{function:ed:buffer-variables}
{function:ed:buffer-modes}
{function:ed:buffer-windows}
{function:ed:buffer-delete-hook}
{function:ed:delete-buffer}
{evariable:Delete Buffer Hook}
{function:ed:delete-buffer-if-possible}
]#


;;;; Some buffer structure support.

(defun buffer-writable (buffer)
  "Return true if buffer may be modified, else ().  Any attempt to alter
   text a buffer that is read-only results in an error.

   There is a setf method to change this value.  The setf method invokes
   the functions in *Buffer Writable Hook* on the buffer and new value
   before storing the new value."
  (buffer-%writable buffer))

(defun %set-buffer-writable (buffer value)
  (prog1 (setf (buffer-%writable buffer) value)
    (invoke-hook ed::buffer-writable-hook buffer value)))

;;; BUFFER-MODIFIED uses the buffer modification tick which is for redisplay.
;;; We can never set this down to "unmodify" a buffer, so we keep an
;;; unmodification tick.  The buffer is modified only if this is less than the
;;; modification tick.
;;;
(defun buffer-modified (buffer)
  "Return t if $buffer has been modified, else ().  This attribute is set
   whenever a text-altering operation is performed on a buffer.

   There is a setf method to change this value.

   The setf method invokes the functions in *Buffer Modified Hook* with the
   buffer whenever the value of the modified flag changes."
  (or (bufferp buffer) (error "Argument must be a buffer: ~S." buffer))
  (> (buffer-modified-tick buffer) (buffer-unmodified-tick buffer)))

(defun %set-buffer-modified (buffer sense)
  "If true make the buffer modified, if NIL unmodified."
  (or (bufferp buffer) (error "~S is not a buffer." buffer))
  (invoke-hook ed::buffer-modified-hook buffer sense)
  (if sense
      (setf (buffer-modified-tick buffer) (tick))
      (setf (buffer-unmodified-tick buffer) (tick)))
  sense)

(proclaim '(inline buffer-name buffer-pathname buffer-region))

(defun buffer-region (buffer)
  "Return the region which contains the text in $buffer.  This can be set
   with setf.

   The returned region contains all the text in $buffer.  `current-region'
   returns the current region."
  (buffer-%region buffer))

(defun %set-buffer-region (buffer new-region)
  (let ((old (buffer-region buffer)))
    (delete-region old)
    (ninsert-region (region-start old) new-region)
    old))

(defun buffer-name (buffer)
  "Return the name, which is a string, of $buffer.

   The corresponding `setf' method invokes *Buffer Name Hook* with buffer
   and the new name and then sets the buffer's name.  The `setf' method
   signals an error when a buffer of the given name already exists."
  (buffer-%name buffer))

(defvar *buffer-names* (make-string-table)
  "A string-table (as in [string-tables]) of all the names of the buffers
   in *buffer-list*.  The values of the entries are the corresponding
   buffers.")

(defun %set-buffer-name (buffer name)
  (multiple-value-bind (entry foundp) (getstring name *buffer-names*)
    (cond ((or (not foundp) (eq entry buffer))
	   (invoke-hook ed::buffer-name-hook buffer name)
	   (delete-string (buffer-%name buffer) *buffer-names*)
	   (setf (getstring name *buffer-names*) buffer)
	   (setf (buffer-%name buffer) name))
	  (t (error "Attempt to rename buffer ~S to a name already in use: ~S."
		    buffer name)))))

(defun buffer-pathname (buffer)
  "Return the pathname of the file associated with $buffer if there is one,
   else ().

   The buffer pathname is the truename of the file as of the most recent
   time it was read or written.

   There is a `setf' form to change the pathname.  When the pathname is
   changed the hook *Buffer Pathname Hook* is invoked with the buffer and
   new value."
  (buffer-%pathname buffer))

(defun %set-buffer-pathname (buffer pathname)
  (invoke-hook ed::buffer-pathname-hook buffer pathname)
  (setf (buffer-%pathname buffer) pathname))

(defun buffer-modeline-fields (buffer)
  "Return a copy of the list of modeline-fields of buffer.

   The allocation of this list can be modified without affecting display of
   buffer's modeline.  Modifying any particular field's components (for
   example, width or function) causes the changes to be reflected the next
   trip through redisplay in every modeline display that uses the modified
   modeline-field.

   When this is set with `setf', `update-modeline-fields' is called for
   each window into the buffer."
  (do ((finfos (buffer-%modeline-fields buffer) (cdr finfos))
       (result () (cons (ml-field-info-field (car finfos)) result)))
      ((null finfos) (nreverse result))))

(defun %set-buffer-modeline-fields (buffer fields)
  (check-type fields list)
  (check-type buffer buffer "an editor buffer")
  (sub-set-buffer-modeline-fields buffer fields)
  (dolist (w (buffer-windows buffer))
    (update-modeline-fields buffer w)))

(defun sub-set-buffer-modeline-fields (buffer modeline-fields)
  (or (every #'modeline-field-p modeline-fields)
      (error "Fields must be a list of modeline-field objects."))
  (setf (buffer-%modeline-fields buffer)
	(do ((fields modeline-fields (cdr fields))
	     (res nil (cons (make-ml-field-info (car fields))
			    res)))
	    ((null fields) (nreverse res)))))

(defun buffer-modeline-field-p (buffer field)
  "If $field, a modeline-field or the name of one, is in $buffer's list of
   modeline-fields, return $field; else, returns ()."
  (let ((finfo (internal-buffer-modeline-field-p buffer field)))
    (if finfo (ml-field-info-field finfo))))

(defun internal-buffer-modeline-field-p (buffer field)
  (let ((fields (buffer-%modeline-fields buffer)))
    (if (modeline-field-p field)
	(find field fields :test #'eq :key #'ml-field-info-field)
	(find field fields
	      :key #'(lambda (f)
		       (modeline-field-name (ml-field-info-field f)))))))


;;;; Variable binding -- winding and unwinding.

(eval-when (compile eval)

(defmacro unbind-variable-bindings (bindings)
  `(do ((binding ,bindings (binding-across binding)))
       ((null binding))
     (setf (car (binding-cons binding))
	   (variable-object-down (binding-object binding)))))

(defmacro bind-variable-bindings (bindings)
  `(do ((binding ,bindings (binding-across binding)))
       ((null binding))
     (let ((cons (binding-cons binding))
	   (object (binding-object binding)))
       (setf (variable-object-down object) (car cons)
	     (car cons) object))))

) ;eval-when

;;; UNWIND-BINDINGS  --  Internal
;;;
;;; Unwind buffer variable bindings and all mode bindings up to and
;;; including mode.  Return a list of the modes unwound in reverse order.
;;; (buffer-mode-objects *current-buffer*) is clobbered.  If "mode" is NIL
;;; unwind all bindings.
;;;
(defun unwind-bindings (mode)
  (unbind-variable-bindings (buffer-var-values *current-buffer*))
  (do ((curmode (buffer-mode-objects *current-buffer*))
       (unwound ()) cw)
      (())
    (setf cw curmode  curmode (cdr curmode)  (cdr cw) unwound  unwound cw)
    (unbind-variable-bindings (mode-object-var-values (car unwound)))
    (when (or (null curmode) (eq (car unwound) mode))
      (setf (buffer-mode-objects *current-buffer*) curmode)
      (return unwound))))

;;; WIND-BINDINGS  --  Internal
;;;
;;; Add "modes" to the mode bindings currently in effect.
;;;
(defun wind-bindings (modes)
  (do ((curmode (buffer-mode-objects *current-buffer*)) cw)
      ((null modes) (setf (buffer-mode-objects *current-buffer*) curmode))
    (bind-variable-bindings (mode-object-var-values (car modes)))
    (setf cw modes  modes (cdr modes)  (cdr cw) curmode  curmode cw))
  (bind-variable-bindings (buffer-var-values *current-buffer*)))


;;;; BUFFER-MAJOR-MODE.

(eval-when (compile eval)
(defmacro with-mode-and-buffer ((name major-p buffer) &body forms)
  `(let ((mode (get-mode-object name)))
    (setq ,name (mode-object-name mode))
    (,(if major-p 'unless 'when) (mode-object-major-p mode)
      (error "~S is not a ~:[Minor~;Major~] Mode." ,name ,major-p))
    (check-type ,buffer buffer)
    ,@forms))
) ;eval-when

;;; BUFFER-MAJOR-MODE  --  Public
;;;
;;; The major mode is the first on the list, so just return that.
;;;
(defun buffer-major-mode (buffer)
  "Return the name of $buffer's major mode.

   The major mode may be changed with `setf'; then *Buffer Major Mode Hook*
   is invoked with $buffer and the new mode."
  (check-type buffer buffer)
  (car (buffer-modes buffer)))

;;; %SET-BUFFER-MAJOR-MODE  --  Public
;;;
;;; Unwind all modes in effect and add the major mode specified.
;;;
;;; Note that BUFFER-MODE-OBJECTS is in order of invocation in buffers
;;; other than the current buffer, and in the reverse order in the
;;; current buffer.
;;;
(defun %set-buffer-major-mode (buffer name)
  "Set the major mode of some buffer to the Name'd mode."
  (with-mode-and-buffer (name t buffer)
    (invoke-hook ed::buffer-major-mode-hook buffer name)
    (cond
     ((eq buffer *current-buffer*)
      (let ((old-mode (car (last (buffer-mode-objects buffer)))))
	(invoke-hook (%value (mode-object-hook-name old-mode)) buffer nil)
	(funcall (mode-object-cleanup-function old-mode) buffer)
	(swap-char-attributes old-mode)
	(wind-bindings (cons mode (cdr (unwind-bindings old-mode))))
	(swap-char-attributes mode)))
     (t
      (let ((old-mode (car (buffer-mode-objects buffer))))
	(invoke-hook (%value (mode-object-hook-name old-mode)) buffer nil)
	(funcall (mode-object-cleanup-function old-mode) buffer))
      (setf (car (buffer-mode-objects buffer)) mode)))
    (setf (car (buffer-modes buffer)) name)
    (funcall (mode-object-setup-function mode) buffer)
    (invoke-hook (%value (mode-object-hook-name mode)) buffer t))
  ())


;;;; BUFFER-MINOR-MODE.

;;; BUFFER-MINOR-MODE  --  Public
;;;
;;; Check if the mode-object is in the buffer's mode-list.
;;;
(defun buffer-minor-mode (buffer name)
  "Return true if the minor mode name is active in $buffer, else ().

   A minor mode may be turned on or off by using `setf'; then *Buffer Minor
   Mode Hook* is invoked with $buffer, $name and the new value."
  (with-mode-and-buffer (name nil buffer)
    (not (null (memq mode (buffer-mode-objects buffer))))))

(defvar *mode-names* (make-string-table)
  "A string table of the names of all the modes.")

;;; %SET-BUFFER-MINOR-MODE  --  Public
;;;
;;; Turn a minor mode on or off, with due respect for bindings.
;;;
(defun %set-buffer-minor-mode (buffer name new-value)
  (let ((objects (buffer-mode-objects buffer)))
    (with-mode-and-buffer (name nil buffer)
      (invoke-hook ed::buffer-minor-mode-hook buffer name new-value)
      (cond
       ;; Already there or not there, nothing to do.
       ((if (memq mode (buffer-mode-objects buffer)) new-value (not new-value)))
       ;; Adding a new mode.
       (new-value
	(cond
	 ((eq buffer *current-buffer*)
	  ;;
	  ;; Unwind bindings having higher precedence, cons on the new
	  ;; mode and then wind them back on again.
	  (do ((m objects (cdr m))
	       (prev nil (car m)))
	      ((or (null (cdr m))
		   (< (mode-object-precedence (car m))
		      (mode-object-precedence mode)))
	       (wind-bindings
		(cons mode (if prev
			       (unwind-bindings prev)
			       (unbind-variable-bindings
				(buffer-var-values *current-buffer*))))))))
	 (t
	  (do ((m (cdr objects) (cdr m))
	       (prev objects m))
	      ((or (null m)
		   (>= (mode-object-precedence (car m))
		       (mode-object-precedence mode)))
	       (setf (cdr prev) (cons mode m))))))
	;;
	;; Add the mode name.
	(let ((bm (buffer-modes buffer)))
	  (setf (cdr bm)
		(merge 'list (cdr bm) (list name) #'<  :key
		       #'(lambda (x)
			   (mode-object-precedence (getstring x *mode-names*))))))

	(funcall (mode-object-setup-function mode) buffer)
	(invoke-hook (%value (mode-object-hook-name mode)) buffer t))
       (t
	;; Removing an active mode.
	(invoke-hook (%value (mode-object-hook-name mode)) buffer nil)
	(funcall (mode-object-cleanup-function mode) buffer)
	;; In the current buffer, unwind buffer and any mode bindings on top
	;; pop off the mode and wind the rest back on.
	(cond ((eq buffer *current-buffer*)
	       (wind-bindings (cdr (unwind-bindings mode))))
	      (t
	       (setf (buffer-mode-objects buffer)
		     (delq mode (buffer-mode-objects buffer)))))
	;; We always use the same string, so we can delq it (How Tense!)
	(setf (buffer-modes buffer) (delq name (buffer-modes buffer))))))
  new-value))


;;;; CURRENT-BUFFER, CURRENT-POINT, CURRENT-LINE, and buffer using setup
;;;; and cleanup.

(proclaim '(inline current-buffer))

(defun current-buffer ()
  "Return the current buffer.  Usually this is the buffer that the current
   window is displaying.

   This value may be changed with `setf', and the `setf' method invokes
   *Set Buffer Hook* before the change occurs with the new value.  After
   the change occurs, the method invokes *After Set Buffer Hook* with the
   old value."
  *current-buffer*)

(defun current-point ()
  "Return the buffer-point of the current buffer."
  (buffer-point *current-buffer*))

(defun current-line ()
  "Return the line of the Buffer-Point of the current buffer."
  (mark-line (buffer-point *current-buffer*)))

;;; %SET-CURRENT-BUFFER  --  Internal
;;;
(defun %set-current-buffer (buffer)
  "Undo previous buffer and mode specific variables and character
   attributes and set up the new ones.  Set *current-buffer*."
  (let ((old-buffer *current-buffer*))
    (check-type buffer buffer)
    (invoke-hook ed::set-buffer-hook buffer)
    ;; Undo old bindings.
    (setf (buffer-mode-objects *current-buffer*)
	  (unwind-bindings nil))
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))
    (let ((pathname (buffer-pathname buffer)))
      (if pathname
	  (if (or (remote-pathname-p pathname)
		  (probe-file (directory-namestring pathname)))
	      (setf (current-directory) (directory-namestring pathname)))
	  (message "Buffer ~A should have a pathname" buffer)))
    (setq *current-buffer* buffer)
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))
    ;; Make new bindings.
    (wind-bindings (shiftf (buffer-mode-objects *current-buffer*) nil))
    (invoke-hook ed::after-set-buffer-hook old-buffer))
  buffer)

;;; USE-BUFFER-SET-UP  --  Internal
;;;
;;; This function is called by the use-buffer macro to wind on the new
;;; buffer's variable and key bindings and character attributes.
;;;
(defun use-buffer-set-up (old-buffer)
  (unless (eq old-buffer *current-buffer*)
    ;; Let new char attributes overlay old ones.
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))
    ;; Wind on bindings of new current buffer.
    (wind-bindings (shiftf (buffer-mode-objects *current-buffer*) nil))))

;;; USE-BUFFER-CLEAN-UP  --  Internal
;;;
;;; This function is called by use-buffer to clean up after it is done.
;;;
(defun use-buffer-clean-up (old-buffer)
  (unless (eq old-buffer *current-buffer*)
    ;; When we leave, unwind the bindings,
    (setf (buffer-mode-objects *current-buffer*) (unwind-bindings nil))
    ;; Restore the character attributes,
    (swap-char-attributes (car (buffer-mode-objects *current-buffer*)))))


;;;; Recursive editing.

#[ Recursive Edits

{function:ed:use-buffer}
{function:ed:recursive-edit}
{evariable:Enter Recursive Edit Hook}
{function:ed:in-recursive-edit}
{function:ed:exit-recursive-edit}
{evariable:Exit Recursive Edit Hook}
{function:ed:abort-recursive-edit}
{evariable:Abort Recursive Edit Hook}
]#

(defvar *in-a-recursive-edit* nil "True if we are in a recursive edit.")

(proclaim '(inline in-recursive-edit))

(defun in-recursive-edit ()
  "Return true if the calling point is dynamically within a recursive edit
   context, else ()."
  *in-a-recursive-edit*)

;;; RECURSIVE-EDIT  --  Public
;;;
;;; Call the command interpreter recursively, winding on new state as
;;; necessary.
;;;
(defun recursive-edit (&optional (handle-abort t))
  "Invoke the command interpreter, which reads from the keyboard and
   invokes commands until it is terminated with either
   `exit-recursive-edit' or `abort-recursive-edit'.

   Normally, an `editor-error' or \"Interrupt\" interrupt (sigint, usually
   control-g control-G) exits the command in progress and returns control
   to the top-level command loop.  If $handle-abort true, then
   `editor-error' or Quit will only exit back to the start of the recursive
   command loop.

   Before entering the command interpreter invoke the hook *Enter Recursive
   Edit Hook*."
  (invoke-hook ed::enter-recursive-edit-hook)
  (multiple-value-bind (flag args)
		       (let ((*in-a-recursive-edit* t)
			     (*busy* *busy*))
			 (catch 'leave-recursive-edit
			   (if handle-abort
			       (loop
				 (block ed-command-loop
				   (handler-case
				       ;(handler-case
				       (handler-bind ((error #'lisp-error-error-handler))
					 (progn
					   (invoke-hook ed::abort-hook)  ; control-g
					   (%command-loop)))
				     ; FIX how to pass :internal to handler-bind?
				     ; 			   (error (condition)
				     ; 			     (lisp-error-error-handler
				     ; 			      condition :internal)))
				     (editor-top-level-catcher ()
				       (return-from ed-command-loop)))))
			       (%command-loop))))
    (case flag
      (:abort (apply #'editor-error args))
      (:exit (values-list args))
      (t (error "Bad thing ~S thrown out of recursive edit." flag)))))

;;; EXIT-RECURSIVE-EDIT is intended to be called within the dynamic context
;;; of RECURSIVE-EDIT, causing return from that function with values returned
;;; as multiple values.  When not in a recursive edit, signal an error.
;;;
(defun exit-recursive-edit (&optional values)
  "Exit a recursive edit, returning as multiple values each element of
   $values.

   Invoke *Exit Recursive Edit Hook* after exiting the command interpreter.

   Signal an error if the editor is already at the top level."
  (or *in-a-recursive-edit* (error "Already in the top-level edit."))
  (invoke-hook ed::exit-recursive-edit-hook values)
  (throw 'leave-recursive-edit (values :exit values)))

;;; ABORT-RECURSIVE-EDIT is intended to be called within the dynamic context
;;; of RECURSIVE-EDIT, causing EDITOR-ERROR to be called on args.  When not
;;; in a recursive edit, signal an error.
;;;
(defun abort-recursive-edit (&rest args)
  "Terminate a recursive edit by applying
  `editor-error' to $args after exiting the command interpreter.

   Invoke *Abort Recursive Edit Hook* with $args before terminating the
   recursive edit.

   Signal an error if the editor is already at the top level."
  (or *in-a-recursive-edit* (error "Already in the top-level edit."))
  (invoke-hook ed::abort-recursive-edit-hook args)
  (throw 'leave-recursive-edit (values :abort args)))


;;;; WITH-WRITABLE-BUFFER

;;; This list indicates recursive use of WITH-WRITABLE-BUFFER on the same
;;; buffer.
;;;
(defvar *writable-buffers* ())

(defmacro with-writable-buffer ((buffer) &body body)
  "Executes $body in a scope where $buffer is writable.  After $body
   executes, clear the modified and writable status flags of $buffer."
  (let ((buf (gensym))
	(no-unwind (gensym)))
    `(let* ((,buf ,buffer)
	    (,no-unwind (member ,buf *writable-buffers* :test #'eq))
	    (*writable-buffers* (if ,no-unwind
				    *writable-buffers*
				    (cons ,buf *writable-buffers*))))
       (unwind-protect
	   (progn
	     (setf (buffer-writable ,buf) t)
	     ,@body)
	 (unless ,no-unwind
	   (setf (buffer-modified ,buf) ())
	   (setf (buffer-writable ,buf) ()))))))


;;;; DEFMODE.

#[ Modes

A mode provides a way to change the behavior of the editor by specifying a
modification to current key bindings, values of variables, and other
things.  Modes are typically used to adjust the editor to suit a particular
editing task, e.g. `Lisp' mode is used for editing Lisp code.

Modes in the editor are not like modes in most text editors; the editor is
really a "modeless" editor.  There are two ways that the the editor mode
concept differs from the conventional one:

  * Modes do not usually alter the environment in a very big way, i.e. replace
    the set of commands bound with another totally disjoint one.  When a
    mode redefines what a key does, it is usually redefined to have a
    slightly different meaning, rather than a totally different one.  For
    this reason, typing a given key does pretty much the same thing no
    matter what modes are in effect.  This property is the distinguishing
    characteristic of a modeless editor.

  * Once the modes appropriate for editing a given file have been chosen, they
    are seldom, if ever, changed.  One of the advantages of modeless
    editors is that time is not wasted changing modes.

A major mode is used to make some big change in the editing environment.
Programming language modes such as `Pascal' mode are major modes.  A major
mode is usually turned on by invoking the command mode-name` Mode' as an
extended command.  There is only one major mode present at a time.  Turning
on a major mode turns off the one that is currently in effect.

A minor mode is used to make a small change in the environment, such as
automatically breaking lines if they get too long.  Unlike major modes, any
number of minor modes may be present at once.  Ideally minor modes should
do the "right thing" no matter what major and minor modes are in effect,
but this is may not be the case when key bindings conflict.

Modes can be envisioned as switches, the major mode corresponding to one
big switch which is thrown into the correct position for the type of
editing being done, and each minor mode corresponding to an on-off switch
which controls whether a certain characteristic is present.

{mode:Fundamental}
]#

#[ Modes (extension)

A mode is a collection of editor values which may be present in the
[current environment] depending on the editing task at hand.  Examples of
typical modes are `Lisp', for editing Lisp code, and `Echo Area', for
prompting in the echo area.

[ Mode Hooks            ]
[ Major and Minor Modes ]
[ Mode Functions        ]
]#

#[ Mode Hooks

When a mode is added to or removed from a buffer, its mode hook
is invoked.  The hook functions take two arguments, the buffer
involved and true if the mode is being added or nil if it is being
removed.

Mode hooks are typically used to make a mode do something additional to
what it usually does.  One might, for example, make a text mode hook
that turned on auto-fill mode when you entered.
]#

#[ Major and Minor Modes

There are two kinds of modes, major modes and minor modes.  A buffer
always has exactly one major mode, but it may have any number of minor modes.
Major modes may have mode character attributes while minor modes may not.

A major mode is usually used to change the environment in some major way, such
as to install special commands for editing some language.  Minor modes
generally change some small attribute of the environment, such as whether lines
are automatically broken when they get too long.  A minor mode should work
regardless of what major mode and minor modes are in effect.

{evariable:Default Modes}
{variable:ed:*mode-names*}
{command:Editor Error}

`Editor Error' is a useful command to bind in modes that wish to shadow
global bindings by making them effectively errors.  Also, although less
likely, minor modes may shadow major mode bindings with this.  This command
calls editor-error.
]#

#[ Mode Functions

{function:ed:defmode}
{function:ed:mode-documentation}
{function:ed:buffer-major-mode}
{evariable:Buffer Major Mode Hook}
{function:ed:buffer-minor-mode}
{evariable:Buffer Minor Mode Hook}
{function:ed:mode-variables}
{function:ed:mode-major-p}
]#

(defun defmode (name &key (setup-function #'identity)
		     (cleanup-function #'identity) major-p transparent-p
		     precedence documentation short-name)
  "Define a new mode named $name, and enter it in *mode-names*.

   If $major-p is true then the mode is a major mode; otherwise it is a
   minor mode.

   $setup-function and $cleanup-function are functions which are invoked
   with the buffer affected, after the mode is turned on, and before it is
   turned off, respectively.  These functions typically are used to make
   buffer-local key or variable bindings and to clear them when the mode is
   turned off.

   $precedence is only meaningful for a minor mode.  The precedence of a
   minor mode determines the order in which it comes in a buffer's list of
   modes.  When searching for values in the current environment, minor
   modes are searched in order, so the precedence of a minor mode
   determines which value is found when there are several definitions.

   $transparent-p determines whether key bindings local to the defined mode
   are transparent.  Transparent key bindings are invoked in addition to
   the first normal key binding found rather than shadowing less local key
   bindings.

   $documentation is some introductory text about the mode.  Commands such
   as `Describe Mode' use this."
  (let ((hook-str (concatenate 'string name " Mode Hook"))
	(mode (getstring name *mode-names*)))
    (cond
     (mode
      (when (if major-p
		(not (mode-object-major-p mode))
		(mode-object-major-p mode))
	(cerror "Let bad things happen"
		"Mode ~S is being redefined as a ~:[Minor~;Major~] mode ~
		 where it was ~%~
		 previously a ~:*~:[Major~;Minor~] mode." name major-p))
      (warn "Mode ~S is being redefined, variables and bindings will ~
	     be preserved." name)
      (setq name (mode-object-name mode)))
     (t
      (defevar hook-str
	       (concatenate 'string "This is the mode hook variable for "
			    name " Mode."))
      (if major-p
	  (eval `(defcommand ,(concatenate 'string name " Mode") ()
		   ,(format () "Put the current buffer into ~A (major) mode."
			    name)
		   (setf (buffer-major-mode (current-buffer)) ,name)))
	  (eval `(defcommand ,(concatenate 'string name " Mode") (p)
		   ,(format ()
  "Toggle ~A (minor) mode in the current buffer.  With a prefix if the
   prefix is positive turn the mode on, else turn the mode off."
                     name)
		   (setf (buffer-minor-mode (current-buffer) ,name)
			 (if p
			     (plusp p)
			     (fi (buffer-minor-mode (current-buffer) ,name)))))))
      (setq mode (make-mode-object
		  :variables (make-string-table)
		  :bindings (make-hash-table)
		  :hook-name (getstring hook-str *global-variable-names*)))
      (setf (getstring name *mode-names*) mode)))

    (if precedence
	(if major-p
	    (error "Precedence ~S is meaningless for a major mode." precedence)
	    (check-type precedence number))
	(setq precedence 0))

    (setf (mode-object-major-p mode) major-p
	  (mode-object-documentation mode) documentation
	  (mode-object-transparent-p mode) transparent-p
	  (mode-object-precedence mode) precedence
	  (mode-object-setup-function mode) setup-function
	  (mode-object-cleanup-function mode) cleanup-function
	  (mode-object-short-name mode) short-name
	  (mode-object-name mode) name))
  nil)

(defun mode-major-p (name)
  "Return t if $name names a major mode, and () if it names a minor mode.
   $name must name a mode, else signal an error."
  (mode-object-major-p (get-mode-object name)))

(defun mode-variables (name)
  "Return the string-table that contains the names of the mode-local
   variables."
  (mode-object-variables (get-mode-object name)))

(defun mode-documentation (name)
  "Return the documentation for mode with $name."
  (mode-object-documentation (get-mode-object name)))


;;;; Making and Deleting buffers.

(defvar *buffer-list* () "A list of all the buffers.")

#[ The Current Buffer

{function:ed:current-buffer}
{evariable:Set Buffer Hook}
{evariable:After Set Buffer Hook}
{function:ed:current-point}
{function:ed:current-mark}
{function:ed:pop-buffer-mark}
{function:ed:push-buffer-mark}
{variable:ed:*buffer-list*}
{variable:ed:*buffer-names*}
{variable:ed:*buffer-history*}
{function:ed:change-to-buffer}
{function:ed:previous-buffer}
]#

(defvar *current-buffer* ()
  "Internal variable which might contain the current buffer." )

(defvar *ring-marker-buffer* ()
  "The buffer that marks the end or start of the conceptual ring of
   buffers." )

(defvar *ring-marker-message*
  "This buffer marks the end or start of the buffer list, when treating the
   buffers as a ring, for example, via the Rotate Buffers commands.  Most
   new buffers are inserted into the buffer list after this buffer."
  "The Ring Marker buffer." )

(defun get-ring-marker ()
  "Return the ring marker buffer, ensuring that it exists."
  (flet ((make-ring-buffer ()
	   (setq *ring-marker-buffer*
		 (make-unique-buffer "Ring Marker"
				     :modes '("Fundamental")
				     :modeline-fields ()
				     :position :bottom))
	   ;(insert-string (buffer-point *ring-marker-buffer*) *ring-marker-message*)
	   ;(buffer-start (buffer-point *ring-marker-buffer*))
	   ;(setf (buffer-writable *ring-marker-buffer*) ())
	   ))
    (if *ring-marker-buffer*
	(or (find *ring-marker-buffer* *buffer-list*)
	    (make-ring-buffer))
	(make-ring-buffer)))
  *ring-marker-buffer*)

(defun make-buffer (name &key (modes (value ed::default-modes))
			      (modeline-fields
			       (value ed::default-modeline-fields))
			      delete-hook
			      (position :ring))
  "Create and return a buffer named $name, or return () if a buffer with
   that name already exists.

   $modes lists modes which should be in effect in the buffer, major mode
   first, followed by any minor modes.  If this is () then create the
   buffer with the list of modes contained in *Default Modes*.

   $modeline-fields lists modeline fields (as in [Modelines]), which may be
   ().

   $delete-hook lists delete hooks specific to this buffer, which
   `delete-buffer' invokes along with *Delete Buffer Hook*.

   Enter the created buffer into the list *buffer-list* at $position, :top
   for the front of the list, :bottom for the end of the list and :ring for
   after the ring marker buffer.

   Insert the buffer name into the string-table *buffer-names*.  When a
   buffer is created invoke the hook *Make Buffer Hook* with the new buffer
   after setting up the modes on the buffer and before setting the buffer
   pathname."
  (fi* (getstring name *buffer-names*)
    (or (listp delete-hook)
	(error ":delete-hook must be a list of functions -- ~S." delete-hook))
    (let* ((region (make-empty-region))
	   (object (getstring "Fundamental" *mode-names*))
	   (buffer (internal-make-buffer
		    :%name name
		    :%region region
		    :modes (list (mode-object-name object))
		    :mode-objects (list object)
		    :bindings (make-hash-table)
		    :point (copy-mark (region-end region))
		    :display-start (copy-mark (region-start region))
		    :delete-hook delete-hook
		    :variables (make-string-table))))
      (sub-set-buffer-modeline-fields buffer modeline-fields)
      (setf (line-%buffer (mark-line (region-start region))) buffer)
      (ecase position
	((() :ring)
	 (let ((marker (nthcdr (position (get-ring-marker) *buffer-list*)
			       *buffer-list*)))
	   (setf (cdr marker) (cons buffer (cdr marker)))))
	(:top
	 (push buffer *buffer-list*))
	(:bottom
	 (setq *buffer-list* (append *buffer-list* (list buffer)))))
      (setf (getstring name *buffer-names*) buffer)
      (unless (equalp modes '("Fundamental"))
	(setf (buffer-major-mode buffer) (car modes))
	(dolist (m (cdr modes))
	  (setf (buffer-minor-mode buffer m) t)))
      (invoke-hook ed::make-buffer-hook buffer)
      (setf (buffer-pathname buffer) (current-directory))
      buffer)))

(defun delete-buffer (buffer)
  "Remove $buffer from *buffer-list* and $buffer's name from
   *buffer-names*.  Before buffer is deleted, invoke the functions on
   $buffer returned by `buffer-delete-hook' and those found in *Delete
   Buffer Hook*.  If buffer is the `current-buffer', or displayed in any
   windows, then signal an error."
  (if (eq buffer *current-buffer*)
      (error "Attempt to delete current buffer ~S." buffer))
  (if (buffer-windows buffer)
      (error "Attempt to delete buffer ~S, which is displayed in ~R window~:P."
	     buffer (length (buffer-windows buffer))))
  (invoke-hook (buffer-delete-hook buffer) buffer)
  (invoke-hook ed::delete-buffer-hook buffer)
  (setq *buffer-list* (delq buffer *buffer-list*))
  (delete-string (buffer-name buffer) *buffer-names*)
  ())

(defun unique-buffer-name (name)
  "Return the next unique buffer name like Name.  If Name ends in a space
   and an integer then increment the integer until a new buffer name is
   found, otherwise append the lowest positive integer that makes a new
   buffer name."
  (if (getstring name *buffer-names*)
      (let* ((pos (position #\  name :from-end t))
	     (start (if pos
			(parse-integer name :start pos :junk-allowed t)))
	     (buffer-name (if start (subseq name 0 pos) name)))
	(loop
	  for num = (if start (1+ start) 1) then (1+ num)
	  for name = (format nil "~A ~D" buffer-name num)
	  then (format nil "~A ~D" buffer-name num)
	  while (getstring name *buffer-names*)
	  finally return name))
      name))

(defun copy-buffer (buffer)
  "Return a copy of Buffer."
  (let ((new-name (unique-buffer-name (buffer-name buffer)))
	(new-buffer (internal-copy-buffer buffer)))
    (setf (buffer-%name new-buffer) new-name)

    (let ((region (make-empty-region)))
      (setf (buffer-%region new-buffer) region)
      (setf (line-%buffer (mark-line (region-start region)))
	    new-buffer))
    (if (buffer-writable new-buffer)
	(setf (buffer-region new-buffer)
	      (copy-region (buffer-region buffer)))
	(with-writable-buffer (new-buffer)
	  (setf (buffer-region new-buffer)
		(copy-region (buffer-region buffer)))))

    ;; Copy the buffer point position and kind.
    (let ((mark (copy-mark (buffer-start-mark new-buffer) :left-inserting))
	  (buffer-point (buffer-point buffer)))
      (line-offset mark (1- (count-lines
			     (region (buffer-start-mark buffer)
				     buffer-point))))
      (setf (mark-charpos mark) (mark-charpos buffer-point))
      (setf (mark-kind mark) (mark-kind buffer-point))
      (setf (buffer-point new-buffer) mark))

    (setf (buffer-windows new-buffer) nil)

    ;; Copy the display start position and kind.
    (let ((mark (copy-mark (buffer-start-mark new-buffer) :left-inserting))
	  (buffer-ds (buffer-display-start buffer)))
      (line-offset mark (1- (count-lines
			     (region (buffer-start-mark buffer)
				     (buffer-display-start buffer)))))
      (setf (mark-charpos mark) (mark-charpos buffer-ds))
      (setf (mark-kind mark) (mark-kind buffer-ds))
      (setf (buffer-display-start new-buffer) mark))

    ;; Copy variables.
    (setf (buffer-variables new-buffer) (make-string-table))
    (setf (buffer-var-values new-buffer) nil)
    (loop for var = (buffer-var-values buffer)
                  then (aref var 2)
          while var do
      (let ((var-obj (aref var 1)))
	(defevar (variable-object-name var-obj)
	  (variable-object-documentation var-obj)
	  :buffer new-buffer
	  :value (variable-object-value var-obj))))

    ;; Setup modes.
    (let ((object (getstring "Fundamental" *mode-names*)))
      (setf (buffer-modes new-buffer) (list (mode-object-name object)))
      (setf (buffer-mode-objects new-buffer) (list object)))
    (setf (buffer-major-mode new-buffer) (buffer-major-mode buffer))
    (dolist (mode (cdr (buffer-modes buffer)))
      (setf (buffer-minor-mode new-buffer mode) t))

    ;; Copy marks.
    (do ((num 0 (1+ num))
	 (line (mark-line (buffer-start-mark buffer))
	       (line-next line))
	 (new-line (mark-line (buffer-start-mark new-buffer))
		   (line-next new-line))
	 (last-line (mark-line (buffer-end-mark buffer))))
	((eq line last-line))
      (dolist (mark (line-marks line))
	(if (fast-font-mark-p mark)
	    (font-mark new-line
		       (mark-charpos mark)
		       (font-mark-font mark)
		       (mark-kind mark))
	    (mark new-line (mark-charpos mark) (mark-kind mark)))))

    (push new-buffer *buffer-list*)
    (setf (getstring new-name *buffer-names*) new-buffer)
    (invoke-hook ed::make-buffer-hook new-buffer)
    new-buffer))

(defun make-unique-buffer (name &key (modes (value ed::default-modes))
				     (modeline-fields
				      (value ed::default-modeline-fields))
				     delete-hook
				     position)
  (loop
    for num = 1 then (1+ num)
    for buffer = (make-buffer name
			      :modes modes
			      :modeline-fields modeline-fields
			      :delete-hook delete-hook
			      :position position)
        then (make-buffer (format () "~A ~D" name num)
			  :modes modes
			  :modeline-fields modeline-fields
			  :delete-hook delete-hook
			  :position position)
    do
    (if buffer (return-from make-unique-buffer buffer))))


;;;; Buffer start and end marks.

(defun buffer-start-mark (buffer)
  "Return the start mark of $buffer's region:

    (buffer-start-mark buffer)  <==>
      (region-start (buffer-region buffer))"
  (region-start (buffer-region buffer)))

(defun buffer-end-mark (buffer)
  "Return the end mark of $buffer's region:

    (buffer-end-mark buffer)  <==>
      (region-end (buffer-region buffer))"
  (region-end (buffer-region buffer)))


;;;; Setting up initial buffer.

;;; SETUP-INITIAL-BUFFER  --  Internal
;;;
;;; Create the buffer "Main" and the mode "Fundamental".  We make a dummy
;;; fundamental mode before we make the buffer Main, because "make-buffer"
;;; wants fundamental to be defined when it is called, and we can't make
;;; the real fundamental mode until there is a current buffer because
;;; "defmode" wants to invoke it's mode definition hook.  Also, when
;;; creating the "Main" buffer, "Default Modeline Fields" is not yet
;;; defined, so we supply this argument to MAKE-BUFFER as nil.  This is
;;; fine since firing up the editor in a core must set the "Main" buffer's
;;; modeline according to this variable in case the user changed it in his
;;; init file.  After the main buffer is created we then define the real
;;; fundamental mode and bash it into the buffer.
;;;
(defun setup-initial-buffer ()
  ;; Make it look like the mode is there so make-buffer survives.
  (setf (getstring "Fundamental" *mode-names*)
	(make-mode-object :major-p t))
  ;; Make it look like there is a make-buffer-hook.
  (setf (get 'ed::make-buffer-hook 'editor-variable-value)
	(make-variable-object "foo" "bar"))
  ;; Make it look like there is a buffer-pathname-hook.
  (setf (get 'ed::buffer-pathname-hook 'editor-variable-value)
	(make-variable-object "foo1" "bar1"))
  (setq *current-buffer* (make-buffer "Main"
				      :modes '("Fundamental")
				      :modeline-fields ()
				      :position :top))
  (or *current-buffer* (error "Failed to init *current-buffer*."))
  ;; Create the ring marker buffer.
  (get-ring-marker)
  ;; Make the bogus variables go away.
  (remf (symbol-plist 'ed::make-buffer-hook) 'editor-variable-value)
  (remf (symbol-plist 'ed::buffer-pathname-hook) 'editor-variable-value)
  ;; Make the mode go away to please defmode.
  (setf (getstring "Fundamental" *mode-names*) ())
  (defmode "Fundamental" :major-p t
    :documentation "The most basic mode.")
  ;; Bash the real mode object into the buffers.
  (let ((obj (getstring "Fundamental" *mode-names*)))
    (setf (car (buffer-mode-objects *ring-marker-buffer*)) obj
	  (car (buffer-modes *ring-marker-buffer*)) (mode-object-name obj))
    (setf (car (buffer-mode-objects *current-buffer*)) obj
	  (car (buffer-modes *current-buffer*)) (mode-object-name obj))))


#[ The Point and The Cursor

The point is the current focus of editing activity.  Text typed in by the
user is inserted at the point.  Nearly all commands use the point as a
indication of what text to examine or modify.  Textual positions in the
editor are between characters.  This may seem a bit curious at first, but
it is necessary since text must be inserted between characters.  Although
the point points between characters, it is sometimes said to point at a
character, in which case the character after the point is referred to.

The cursor is the visible indication of the current focus of attention: a
rectangular blotch under X Windows, or the hardware cursor on a terminal.
The cursor is usually displayed on the character which is immediately after
the point, but it may be displayed in other places.  Wherever the cursor is
displayed it indicates the current focus of attention.  When input is being
prompted for in the echo area, the cursor is displayed where the input is
to go.  Under X Windows the cursor is only displayed when the editor is
waiting for input.
]#
