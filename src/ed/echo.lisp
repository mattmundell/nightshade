;; Echo Area stuff.

;; FIX replace :default* w :suggest*

(in-package "EDI")

(export '(*echo-area-buffer* *echo-area-stream* *echo-area-window*
	  *parse-starting-mark* *parse-input-region*
	  *last-parse-input-string*
	  *parse-verification-function* *parse-string-tables*
	  *parse-value-must-exist* *parse-default*
	  *parse-default-string*
	  *parse-prompt* *parse-help* *parse-history*
	  *parse-history-pointer* *parse-initial-string*
	  clear-echo-area message msg loud-message
	  prompt-for-buffer prompt-for-date prompt-for-file
	  prompt-for-integer
	  prompt-for-keyword prompt-for-expression prompt-for-string
	  prompt-for-variable prompt-for-yes-or-no prompt-for-y-or-n
	  prompt-for-key-event prompt-for-key prompt-for-mode
	  prompt-in-buffer
	  *logical-key-event-names* logical-key-event-p
	  logical-key-event-documentation logical-key-event-name
	  logical-key-event-key-events define-logical-key-event
	  *parse-type*
	  current-variable-tables
	  defhistory load-histories maybe-load-histories
	  maybe-save-histories save-histories))

#[ The Echo Area

The echo area is the region which occupies the bottom few lines on the
screen.  It is used for two purposes: displaying brief messages to the user
and prompting.

When a command needs some information from the user, it requests it by
displaying a prompt in the echo area.  The following is a typical prompt:

Select Buffer: [nightshade-ed.lisp /home/user/]

The general format of a prompt is a one or two word description of the
input requested, possibly followed by a default in brackets.  The default
is a standard response to the prompt that the editor uses if you type
Return without giving any other input.

There are four general kinds of prompts:

  key-event
      The response is a single key-event and no confirming Return is
     needed.

  keyword
      The response is a selection from one of a limited number of choices.
     Completion is available using Space and Escape, and you
     only need to supply enough of the keyword to distinguish it from any other
     choice.  In some cases a keyword prompt accepts unknown input, indicating the
     prompter should create a new entry.  If this is the case, then you must enter
     the keyword fully specified or completed using Escape; this
     distinguishes entering an old keyword from making a new keyword which is a
     prefix of an old one since the system completes partial input automatically.

  file
      The response is the name of a file, which may have to exist.  Unlike other
     prompts, the default has some effect even after the user supplies some input:
     the system merges the default with the input filename.  See page
     pagerefmerging for a description of filename merging.  Escape and
     Space complete the input for a file parse.

  string
      The response is a string which must satisfy some property, such as being the
     name of an existing file.

These key-events have special meanings when prompting:

  Return
      Confirm the current parse.  If no input has been entered, then use the
     default.  If for some reason the input is unacceptable, the editor does two
     things:

  1) beeps, if the variable `Beep on Ambiguity' set, and

  2) moves the point to the end of the first word requiring disambiguation.
     This allows you to add to the input before confirming the it again.

  Home, C-_
      Print some sort of help message.  If the parse is a keyword parse, then print
     all the possible completions of the current input in a pop-up window.

  Escape
      Attempt to complete the input to a keyword or file parse as far as possible,
     beeping if the result is ambiguous.  When the result is ambiguous, the editor
     moves the point to the first ambiguous field, which may be the end of the
     completed input.

  Space
      In a keyword parse, attempt to complete the input up to the next space.  This
     is useful for completing the names of the editor commands and similar things
     without beeping a lot, and you can continue entering fields while leaving
     previous fields ambiguous.  For example, you can invoke `Forward Word' as
     an extended command by typing M-X f Space w Return.  Each time the
     user enters space, the editor attempts to complete the current field and all
     previous fields.

  C-i, Tab
      In a string or keyword parse, insert the default so that it may be edited.

  C-p
      Retrieve the text of the last string input from a history of echo area inputs.
     Repeating this moves to successively earlier inputs.

  C-n
      Go the other way in the echo area history.

  C-q
      Quote the next key-event so that it is not interpreted as a command.

{evariable:Ignore File Types}
]#

#[ The Echo Area (extension)

The editor provides a number of facilities for displaying information and
prompting the user for it.  Most of these work through a small window displayed
at the bottom of the screen.  This is called the echo area and is supported by
a buffer and a window.  This buffer's modeline (see section refmodelines) is
referred to as the status line, which, unlike other buffers' modelines, is used
to show general status about the editor, Lisp, or world.

{evariable:Default Status Line Fields}
{evariable:Echo Area Height}

[ Echo Area Functions              ]
[ Prompting Functions              ]
[ Control of Parsing Behavior      ]
[ Defining New Prompting Functions ]
[ Some Echo Area Commands          ]
]#


(defmode "Echo Area" :major-p t)

(defvar *echo-area-buffer* (make-buffer "Echo Area"
					:modes '("Echo Area")
					:position :top)
  "Buffer used as the echo area.")

(defvar *echo-area-region* (buffer-region *echo-area-buffer*)
  "Internal thing that's the *echo-area-buffer*'s region.")

(defvar *echo-area-stream*
  (make-editor-output-stream (region-end *echo-area-region*) :full)
  "A buffered editor output stream created with `make-editor-output-stream'
   which inserts text written to it at the point of the echo area buffer.
   Since this stream is buffered a `force-output' must be done when output
   is complete to assure that it is displayed.")

(defvar *echo-area-window* ()
  "The window displaying *echo-area-buffer*, that is, the window used to
   display the echo area.  Its modeline is the status line.")

(defvar *echo-parse-starting-mark*
  (copy-mark (buffer-point *echo-area-buffer*) :right-inserting)
  "Mark that points to the beginning of the text that'll be parsed.")

(defvar *echo-parse-input-region*
  (region *echo-parse-starting-mark* (region-end *echo-area-region*))
  "Region that contains the text typed in.")

(defvar *last-parse-input-string* ""
  "The previous text typed in, as a string.")

(proclaim '(special *parse-starting-mark* *parse-input-region*))

(setf (documentation '*parse-starting-mark* 'variable)
  "A mark in the *echo-area-buffer*: the position at which the parse
   began.")

(setf (documentation '*parse-input-region* 'variable)
  "A region with `parse-starting-mark' as its start and the end of
   *echo-area-buffer* as its end.  When `Confirm Parse' is called, the text
   in this region is the text that will be parsed.")


#[ Defining New Prompting Functions

Prompting functions are implemented as a recursive edit in the
`Echo Area' buffer.  Completion, help, and other parsing features
are implemented by commands which are bound in `Echo Area Mode'.

A prompting function passes information down into the recursive edit
by binding a collection of special variables.

{variable:ed:*parse-verification-function*}
{variable:ed:*parse-string-tables*}
{variable:ed:*parse-value-must-exist*}
{variable:ed:*parse-default*}
{variable:ed:*parse-default-string*}
{variable:ed:*parse-type*}
{variable:ed:*parse-prompt*}
{variable:ed:*parse-help*}
{variable:ed:*parse-starting-mark*}
{variable:ed:*parse-input-region*}
]#


;;;; Variables that control parsing.

(defvar *parse-verification-function* '%not-inside-a-parse
  "This is bound to a function of one argument that `Confirm Parse' calls.
   It does most of the work when parsing prompted input.  The argument is
   the string that was in `parse-input-region' when the `Confirm Parse' was
   invoked.  The function should return a list of values which are to be
   the result of the recursive edit, or () to indicate that the parse
   failed.  In order to return zero values, a true second value may be
   returned along with a () first value.")

;;; %Not-Inside-A-Parse  --  Internal
;;;
;;; This function is called if someone does stuff in the echo area when we
;;; aren't inside a parse.  It tries to put them back in a reasonable
;;; place.
;;;
(defun %not-inside-a-parse (quaz)
  "Thing that's called when somehow we get called to confirm a parse that's
   not in progress."
  (declare (ignore quaz))
  (let* ((bufs (remove *echo-area-buffer* *buffer-list*))
	 (buf (or (find-if #'buffer-windows bufs)
		  (car bufs)
		  (make-buffer "Main"))))
    (setf (current-buffer) buf)
    (dolist (w *window-list*)
      (and (eq (window-buffer w) *echo-area-buffer*)
	   (or (eq w *echo-area-window*)
	       (setf (window-buffer w) buf))))
    (setf (current-window)
	  (or (car (buffer-windows buf))
	      (make-window (buffer-start-mark buf)))))
  (editor-error "Attempt to confirm a parse that wasn't in progress?"))

(defvar *parse-string-tables* ()
  "The list of string-tables, if any, that pertain to the current parse.")

(defvar *parse-value-must-exist* ()
  "True if a value must be entered at the prompt.")

(defvar *parse-default* ()
  "When prompting, this is bound to a string representing the fallback
   value, which is supplied as the :default argument.

   `Confirm Parse' supplies this to the parse verification function when
   the `parse-input-region' is empty.")

(defvar *parse-default-string* ()
  "String used to show the fallback value.  When prompting, if
   `parse-default' is (), displays this string as a representation of the
   fallback value; for example, when prompting for a buffer, this variable
   would be bound to the buffer name.  If this is () prompting uses
   *Parse-Default*.")

(defvar *parse-initial-string* ()
  "String inserted at the prompt initially.")

(defvar *parse-prompt* ()
  "The prompt for the current parse.")

(defvar *parse-help* ()
  "The help string or function being used for the current parse.")

(defvar *parse-type* :string
  "The kind of parse in progress, one of :file, :keyword or :string.

   This tells the completion commands how to do completion, with :string
   turning off completion.")

(defvar *parse-history* ()
  "History for the current parse.")

(defvar *parse-history-pointer* 0
  "History pointer for the current parse.")

(defvar *parse-guide-p* ()
  "True to enable guided parsing.")


#[ Echo Area Functions

The message function is the prefered was to perform text operations on the
echo area buffer to display messages.  A command must use this function or
clear buffer-modified for the `Echo Area' buffer to cause the editor to
leave text in the echo area after the command's execution.

{function:ed:clear-echo-area}
{function:ed:message}
{function:ed:loud-message}

{variable:ed:*echo-area-window*}
{variable:ed:*echo-area-buffer*}
{variable:ed:*echo-area-stream*}
]#


;;;; MESSAGE and CLEAR-ECHO-AREA.

(defvar *last-message-time* 0
  "Internal-Real-Time the last time we displayed a message.")

(defvar *message-buffer* ()
  "Buffer holding history of messages.")

(defun maybe-wait ()
  (let* ((now (get-internal-real-time))
	 (delta (/ (float (- now *last-message-time*))
		   (float internal-time-units-per-second)))
	 (pause (value ed::message-pause)))
    (when (< delta pause)
      (sleep (- pause delta)))))

(defun clear-echo-area ()
  "Clear the Echo Area."
  (maybe-wait)
  (delete-region *echo-area-region*)
  (setf (buffer-modified *echo-area-buffer*) nil))

(defun log-message (string &rest args)
  "Add String (formatted with Args) to the message history buffer."
  (or *message-buffer*
      (setq *message-buffer* (or (getstring "Messages" *buffer-names*)
				 (make-buffer "Messages"
					      :modes '("Messages")))))
  (with-writable-buffer (*message-buffer*)
    (with-output-to-mark (s (buffer-end-mark *message-buffer*))
      (if args
	  (apply #'format s string args)
	  (write-string string s))
      (terpri s))))

;;; Message  --  Public
;;;
;;; Display the stuff on *echo-area-stream* and then wait.  Editor-Sleep
;;; will do a redisplay if appropriate.
;;;
(defun message (string &rest args)
  "Nicely display a message in the Echo Area.

   Format the message onto a fresh line and wait for *Message Pause*
   seconds to make the message more noticeable.  $string and $args are the
   format control string and format arguments, respectively.

   Related to `loud-message'."
  (maybe-wait)
  (cond ((eq *current-window* *echo-area-window*)
	 (let ((point (buffer-point *echo-area-buffer*)))
	   (with-mark ((m point :left-inserting))
	     (line-start m)
	     (with-output-to-mark (s m :full)
	       (if args
		   (apply #'format s string args)
		   (write-string string s))
	       (fresh-line s)))))
	(t
	 (let ((mark (region-end *echo-area-region*)))
	   (cond ((buffer-modified *echo-area-buffer*)
		  (clear-echo-area))
		 ((not (zerop (mark-charpos mark)))
		  (insert-character mark #\newline)
		  (or (displayed-p mark *echo-area-window*)
		      (clear-echo-area))))
	   (if args
	       (apply #'format *echo-area-stream* string args)
	       (write-string string *echo-area-stream*))
	   (setf (buffer-modified *echo-area-buffer*) nil))))
  (force-output *echo-area-stream*)
  (setq *last-message-time* (get-internal-real-time))
  (apply #'log-message string args)
  nil)

;;; Msg  --  Public
;;;
(defun msg (string &rest args)
  "Short form of Message, for tracing."
  (apply #'message string args))

;;; LOUD-MESSAGE -- Public.
;;;
(defun loud-message (string &rest args)
  "Beep and pass an empasized version of $string with $args to `message'."
  (beep)
  (apply #'message (format () "** ~A **" string) args))

;;; RAISE-ECHO-AREA-WHEN-MODIFIED -- Internal.
;;;
;;; INIT-BITMAP-SCREEN-MANAGER in bit-screen.lisp adds this hook when
;;; initializing the bitmap screen manager.
;;;
#+clx
(defun raise-echo-area-when-modified (buffer modified)
  (when (and (value ed::raise-echo-area-when-modified)
	     (eq buffer *echo-area-buffer*)
	     modified)
    (let* ((hunk (window-hunk *echo-area-window*))
	   (win (window-group-xparent (bitmap-hunk-window-group hunk))))
      (xlib:map-window win)
      (setf (xlib:window-priority win) :above)
      (xlib:display-force-output
       (bitmap-device-display (device-hunk-device hunk))))))

#[ Prompting Functions

Most of the prompting functions accept the following keyword arguments:

  :must-exist
     If :must-exist has a true value then the user is prompted until a
     valid response is obtained.  If :must-exist is nil then return as a
     string whatever is input.  The default is true.

  :default
     If null input is given when the user is prompted
     then this value is returned.  If no default is given then
     some input must be given before anything interesting will happen.

  :default-string
     If a :default is given then this is a
     string to be printed to indicate what the default is.  The default is
     some representation of the value for :default, for example for a
     buffer it is the name of the buffer.

  :prompt
     This is the prompt string to display.

  :help

This is similar to :prompt, except that it is displayed when
the help command is typed during input.

This may also be a function.  When called with no arguments, it should either
return a string which is the help text or perform some action to help the user,
returning nil.

{function:ed:prompt-for-buffer}

Since the description of `command-case' is rather complex, here is an usage
example:

    (defcommand "Save All Buffers" ()
     "Offer to save each modified buffer."
     (dolist (b *buffer-list*)
       (select-buffer-command () b)
       (when (buffer-modified b)
	 (command-case (:prompt "Save this buffer: [Y] "
			:help "Save buffer, or do something else:")
	   ((:yes :confirm)
	    "Save this buffer and go on to the next."
	    (save-file-command () b))
	   (:no "Skip saving this buffer, and go on to the next.")
	   (:recursive-edit
	    "Go into a recursive edit in this buffer."
	    (do-recursive-edit) (reprompt))
	   ((:exit #\q) "Quit immediately."
	    (return ()))))))

{function:ed:command-case}
{function:ed:prompt-for-key-event}
{function:ed:prompt-for-key}
{function:ed:prompt-for-file}
{function:ed:prompt-for-integer}
{function:ed:prompt-for-keyword}
{function:ed:prompt-for-expression}
{function:ed:prompt-for-string}
{function:ed:prompt-for-variable}
{function:ed:prompt-for-y-or-n}
{function:ed:prompt-for-yes-or-no}
]#


;;;; DISPLAY-PROMPT-NICELY and PARSE-FOR-SOMETHING.

(defun display-prompt-nicely (&optional (prompt *parse-prompt*)
					(default (or *parse-default-string*
						     *parse-default*)))
  (clear-echo-area)
  (let ((point (buffer-point *echo-area-buffer*)))
    (if (listp prompt)
	(apply #'format *echo-area-stream* prompt)
	(insert-string point prompt))
    (if default
	(progn
	  (insert-character point #\[)
	  (insert-string point default)
	  (insert-string point "] "))
	(when (and *parse-history* (plusp (ring-length *parse-history*)))
	  (insert-character point #\[)
	  (insert-string point
			 (setq *parse-default*
			       (ring-ref *parse-history*
					 (symbol-value *parse-history-pointer*))))
	  (insert-string point "] ")))))

(declaim (special *parse-input-words* *parse-input-string*))

(defun highlight-completions-line (line chi-info)
  (let ((mark (mark line 0)))
    (if (zerop (character-attribute :whitespace (next-character mark)))
	(push (color-mark line 0 :comment)
	      (ed::ch-info-font-marks chi-info))
	(case *parse-type*
	  (:file)
	  (:keyword
	   (when (find-attribute mark :whitespace #'zerop)
	     ;; FIX bind input,words outside
	     (let* ((input *parse-input-string*)
		    (words *parse-input-words*)
		    (string (line-string line)))
	       (declare (simple-string input))
	       (multiple-value-bind
		   (prefix key)
		   (complete-string input *parse-string-tables*)
		 (if (and (plusp (length string))
			  (member key '(:complete :unique))
			  (string= string prefix :start1 1))
		     (push (color-mark line 1 :string)
			   (ed::ch-info-font-marks chi-info))
		     (while ((line-length (line-length line))
			     (words words (cdr words))
			     (start 0 (position #\space
						string
						:start (+ start 1))))
			    ((and words start))
		       (push (color-mark line (+ start 1) :special-form)
			     (ed::ch-info-font-marks chi-info))
		       (push (color-mark line
					 (+ start 1 (length (car words)))
					 :window-foreground)
			     (ed::ch-info-font-marks chi-info))
		       (if (>= start line-length) (return))))))))))))

(defun highlight-visible-completions-buffer (buffer)
  (ed::highlight-visible-chi-buffer buffer highlight-completions-line))

(defmode "Completions" :major-p t)

(declaim (special *completions-buffer* *completions-window*))

(defun refresh-completions ()
  (let* ((buffer *completions-buffer*)
	 (mark (progn
		 (delete-region (buffer-region buffer))
		 (copy-mark (buffer-point buffer)
			    :right-inserting))))
    (buffer-start (buffer-point buffer))
    (with-output-to-mark (s mark)
      (let ((help (typecase *parse-help*
		    (list (if *parse-help*
			      (apply #'format nil *parse-help*)
			      "There is no parse help."))
		    (string *parse-help*)
		    (t "Parse help is not a string or list: ~S" *parse-help*)))
	    (input (region-to-string *parse-input-region*)))
	(cond
	 ((eq *parse-type* :keyword)
	  (write-line help s)
	  (if (plusp (length (string-trim '(#\space #\tab #\newline)
					  input)))
	      (let ((strings (ed::find-all-completions input *parse-string-tables*)))
		(cond (strings
		       (write-line "Possible completions:" s)
		       (dolist (string strings)
			 (write-char #\space s)
			 (write-line string s)))
		      (t
		       (write-line
			"There are no possible completions." s))))))
	 ((and (eq *parse-type* :file) (plusp (length input)))
	  (let ((pns (ambiguous-files (region-to-string *parse-input-region*)
				      (or *parse-default* "") #| FIX |#)))
	    (declare (list pns))
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
		    "There are no possible completions." s)))))
	 (t
	  (insert-string mark help)
	  (insert-character mark #\newline)))))
    (buffer-start mark)
    (let* ((*parse-input-string*  (region-to-string *parse-input-region*))
	   (*parse-input-words* (split *parse-input-string* #\space)))
      (highlight-visible-completions-buffer buffer))))

(defun parse-for-something (&optional (initial
				       (or *parse-initial-string*
					   "")))
  (multiple-value-bind (*parse-default-string*
			*parse-default*
			*parse-history*)
		       (fi (or ed::*defining-a-keyboard-macro*
			       ed::*in-a-keyboard-macro*)
			   (values *parse-default-string*
				   *parse-default*
				   *parse-history*))
    (setq *last-parse-input-string* nil)
    (display-prompt-nicely)
    (let ((start-window (current-window))
	  (previous-mode (buffer-major-mode *echo-area-buffer*)))
      (if (string= previous-mode "Echo Area")
	  (setq previous-mode "Fundamental"))
      (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
      (if initial
	  (insert-string (buffer-point *echo-area-buffer*) initial))
      (if *parse-guide-p*
	  (with-pop-up-window (*completions-buffer*
			       *completions-window*
			       :buffer-name "Guide"
			       :modes '("Completions"))
	    (unwind-protect
		(progn
		  (add-hook ed::after-command-hook #'refresh-completions)
		  (refresh-completions)
		  (setf (current-window) *echo-area-window*)
		  (setf (buffer-major-mode *echo-area-buffer*) "Echo Area")
		  (unwind-protect
		      (use-buffer *echo-area-buffer*
				  (recursive-edit nil))
		    (setf (buffer-major-mode *echo-area-buffer*) previous-mode)
		    (setf (current-window) start-window)))
	      (remove-hook ed::after-command-hook #'refresh-completions)))
	  (progn
	    (setf (current-window) *echo-area-window*)
	    (setf (buffer-major-mode *echo-area-buffer*) "Echo Area")
	    (unwind-protect
		(use-buffer *echo-area-buffer*
			    (recursive-edit nil))
	      (setf (buffer-major-mode *echo-area-buffer*) previous-mode)
	      (setf (current-window) start-window)))))))


;;;; Histories.

;;; *histories*  --  Internal
;;;
;;; A list of all the histories.
;;;
(defvar *histories* ())

(defmacro defhistory (history-variable pointer-variable size)
  "defhistory $history-variable $pointer-variable $size

   Define a history of size $size.  Define a variable $history-variable
   holding the history.  Define a variable $pointer-variable with value 0."
  `(progn
     (defvar ,history-variable (make-ring ,size))
     (defvar ,pointer-variable 0)
     (push (list ',history-variable ',pointer-variable) *histories*)))

;;; load-histories  --  Public
;;;
(defun load-histories ()
  "Load any saved histories."
  (load (config:config-pathname "histories")
	:if-does-not-exist ()))

;;; maybe-save-histories  --  Public
;;;
(defun maybe-save-histories ()
  (if (value ed::load-and-save-histories) (save-histories)))

;;; maybe-load-histories  --  Public
;;;
(defun maybe-load-histories (init)
  (and init
       (value ed::load-and-save-histories)
       (prog1 (load-histories)
	 (add-hook ed::exit-hook 'maybe-save-histories))))

;;; save-histories  --  Public
;;;
(defun save-histories ()
  "Save all histories listed in *histories*.  `defhistory' creates a
   history and adds it to *histories*."
  (to-file (out (config:config-pathname "histories"))
    (format out
     ";;; Auto-generated file.  Usually overwritten when editor exits.~%~
      (in-package \"EDI\")~%")
    (let ((*package* (find-package "EDI")))
      (dolist (history *histories*)
	(let* ((ring-symbol (car history))
	       (ring (eval ring-symbol))
	       (pointer-symbol (cadr history))
	       (pointer (eval pointer-symbol))
	       (ring-symbol-string
		(with-output-to-string (out)
		  (prin1 ring-symbol out)))
	       (pointer-symbol-string
		(with-output-to-string (out)
		  (prin1 pointer-symbol out))))
	  (format out "(setf ~
                       ~A ~D~%      ~
                       (ring-first ~A) ~S~%      ~
                       (ring-bound ~A) ~S~%      ~
                       (ring-vector ~A)~%      ~S)~%"
		  pointer-symbol-string pointer
		  ring-symbol-string (edi::ring-first ring)
		  ring-symbol-string (edi::ring-bound ring)
		  ring-symbol-string (edi::ring-vector ring)))))))


;;;; Buffer prompting.

(defhistory *buffer-input-history* *buffer-input-history-pointer* 350)

(defun prompt-for-buffer (&key ((:must-exist *parse-value-must-exist*) t)
			       default
			       ((:default-string *parse-default-string*))
			       ((:prompt *parse-prompt*) "Buffer: ")
			       ((:help *parse-help*) "Type a buffer name.")
			       ((:history *parse-history*) *buffer-input-history*)
			       ((:history-pointer *parse-history-pointer*)
				'*buffer-input-history-pointer*))
  "Prompt for a buffer name and return the corresponding buffer.  If
   $must-exist is true then the input must be the name of an existing
   buffer, otherwise just return the input string.  Only accept an empty
   input string when $default is supplied.  If $default-string is given it
   names the buffer to use if $default is ()."
  (let ((*parse-string-tables* (list *buffer-names*))
	(*parse-type* :keyword)
	(*parse-default* (cond
			  (default (buffer-name default))
			  (*parse-default-string*
			   (when (and *parse-value-must-exist*
				      (not (getstring *parse-default-string*
						      *buffer-names*)))
			     (error "Default-string must name an existing ~
				     buffer when must-exist is true -- ~S."
				    *parse-default-string*))
			   *parse-default-string*)
			  (t nil)))
	(*parse-verification-function* #'buffer-verification-function)
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*)
	(*parse-guide-p* (and (value ed::prompt-guide)
			      *parse-value-must-exist*)))
    (parse-for-something)))

(defun buffer-verification-function (string)
  (declare (simple-string string))
  (cond ((string= string "") nil)
	(*parse-value-must-exist*
	 (multiple-value-bind
	     (prefix key value field ambig)
	     (complete-string string *parse-string-tables*)
	   (declare (ignore field))
	   (ecase key
	     (:none nil)
	     ((:unique :complete)
	      (list value))
	     (:ambiguous
	      (delete-region *parse-input-region*)
	      (insert-string (region-start *parse-input-region*) prefix)
	      (let ((point (current-point)))
		(move-mark point (region-start *parse-input-region*))
		(unless (character-offset point ambig)
		  (buffer-end point)))
	      nil))))
	(t
	 (list (or (getstring string *buffer-names*) string)))))


;;;; Date prompting.

(defhistory *date-input-history* *date-input-history-pointer* 50)

(defun prompt-for-date (&key default
			     ((:default-string *parse-default-string*))
			     ((:prompt *parse-prompt*) "Date: ")
			     ((:help *parse-help*) "Type a date.")
			     ((:history *parse-history*) *date-input-history*)
			     ((:history-pointer *parse-history-pointer*)
			      '*date-input-history-pointer*))
  "Prompt for a date and return the corresponding universal time.  Only
   accept an empty input string when $default is supplied.  If
   $default-string is given it names the date to use if $default is ()."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (let ((time (parse-time string)))
	       (if time (list time) ()))))
	(*parse-default* (or default *parse-default-string*))
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*))
    (parse-for-something)))


;;;; File Prompting.

(defhistory *file-input-history* *file-input-history-pointer* 350)

(defun prompt-for-file (&key ((:must-exist *parse-value-must-exist*) t)
			     default
			     ((:default-string *parse-default-string*))
			     ((:prompt *parse-prompt*) "Filename: ")
			     ((:help *parse-help*) "Type a file name.")
			     ((:history *parse-history*) *file-input-history*)
			     ((:history-pointer *parse-history-pointer*)
			      '*file-input-history-pointer*))
  "Prompt for an acceptable filename.  \"Acceptable\" means that it is a
   legal filename, and it exists if $must-exist is true.  Return a
   pathname.

   If the file exists as entered, then return it, otherwise merge it with
   $default as by `merge-pathnames'."
  (let ((*parse-verification-function* #'file-verification-function)
	(*parse-default* (if default (namestring default) #|FIX|# ""))
;; FIX Problem is "Find: [/a/b/c] /a/b/" initial string in way for input "e:"
;;	(*parse-initial-string* (if default (directory-namestring default) #|FIX|# ""))
	(*parse-type* :file)
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*)
	(*parse-guide-p* (value ed::prompt-guide)))
    (parse-for-something)))

(defun file-verification-function (string)
  (when string
    (let ((pn (pathname-or-lose (ed::filter-tildes string))))
      (when pn
	(if (fi lisp::*literal-pathnames* ; FIX ::
		(wild-pathname-p pn))
	    (list pn)
	    (let ((merge
		   (when *parse-default*
		     (cond ((directory-name-p pn) ; Was directoryp, wildcard errors.
			    (merge-pathnames pn *parse-default*))
			   (t
			    (merge-pathnames pn
					     (directory-namestring
					      *parse-default*)))))))
	      (cond ((probe-file pn) (list pn))
		    ((and merge (probe-file merge)) (list merge))
		    ((not *parse-value-must-exist*) (list (or merge pn)))
		    (t ()))))))))

;;; PATHNAME-OR-LOSE tries to convert string to a pathname using
;;; PARSE-NAMESTRING.  If it succeeds, this returns the pathname.  Otherwise,
;;; this deletes the offending characters from *parse-input-region* and signals
;;; an editor-error.
;;;
(defun pathname-or-lose (string)
  (declare (simple-string string))
  (multiple-value-bind (pn idx)
		       (parse-namestring string nil *default-pathname-defaults*
					 :junk-allowed t)
    (cond (pn)
	  (t (delete-characters (region-end *echo-area-region*)
				(- idx (length string)))
	     nil))))


;;;; Keyword and variable prompting.

(defun prompt-for-keyword (*parse-string-tables*
			   &key
			   ((:must-exist *parse-value-must-exist*) t)
			   ((:default *parse-default*))
			   ((:default-string *parse-default-string*))
			   ((:prompt *parse-prompt*) "Keyword: ")
			   ((:help *parse-help*) "Type a keyword.")
			   ((:history *parse-history*))
			   ((:history-pointer *parse-history-pointer*)))
  "Prompt for a keyword, using the string tables in the list
   $*parse-string-tables*.  If $must-exist is true, then the result must be
   an unique prefix of a string in one of the string tables, and the return
   the complete string even if only a prefix of the full string was typed.
   In addition, return the value of the corresponding entry in the string
   table as the second value.

   If $must-exist is (), then return the string exactly as entered.

   The input `prompt-for-keyword' with $must-exist () may be completed
   using the `Complete Parse' and `Complete Field' commands, whereas the
   input to `prompt-for-string' must FIX."
  (let ((*parse-verification-function* #'keyword-verification-function)
	(*parse-type* :keyword)
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*)
	(*parse-guide-p* (and (value ed::prompt-guide)
			      *parse-value-must-exist*)))
    (parse-for-something)))

(defhistory *variable-prompt-history*
	    *variable-prompt-history-pointer*
	    50)

;; FIX prompt-for-editor-variable
(defun prompt-for-variable (&key ((:must-exist *parse-value-must-exist*) t)
				 ((:default *parse-default*))
				 ((:default-string *parse-default-string*))
				 ((:prompt *parse-prompt*) "Variable: ")
				 ((:help *parse-help*)
				  "Type the name of a variable.")
				 ((:history *parse-history*)
				  *variable-prompt-history*)
				 ((:history-pointer *parse-history-pointer*)
				  '*variable-prompt-history-pointer*)
				 (where  :current))
  "Prompt for a variable name.  If $must-exist is true, then the string
   must be a variable defined in the current environment, in which case
   return the symbol name of the variable found as the second value."
  (let ((*parse-string-tables* (current-variable-tables where))
	(*parse-verification-function* #'keyword-verification-function)
	(*parse-type* :keyword)
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*)
	(*parse-guide-p* (and (value ed::prompt-guide)
			      *parse-value-must-exist*)))
    (parse-for-something)))

(defun current-variable-tables (&optional (where :current))
  "Return a list of all the variable tables currently established in
   context $where.

   $where can be :buffer for the current buffer, :mode for the current mode
   or :current for buffer, mode and global variables.

   Return a list suitable for use with `prompt-for-variable'."
  (case where
    (:buffer (list (buffer-variables *current-buffer*)))
    (:mode (list *global-variable-names*))
    (:current
     (do ((tables (list (buffer-variables *current-buffer*)
			*global-variable-names*)
		  (cons (mode-object-variables (car mode)) tables))
	  (mode (buffer-mode-objects *current-buffer*) (cdr mode)))
	 ((null mode) tables)))))

(defun keyword-verification-function (string)
  (declare (simple-string string))
  (multiple-value-bind
      (prefix key value field ambig)
      (complete-string string *parse-string-tables*)
    (declare (ignore field))
    (cond (*parse-value-must-exist*
	   (ecase key
	     (:none nil)
	     ((:unique :complete)
	      (list prefix value))
	     (:ambiguous
	      (if (ed::value ed::help-on-ambiguity)
		  (ed::help-on-parse-command))
	      (delete-region *parse-input-region*)
	      (insert-string (region-start *parse-input-region*) prefix)
	      (let ((point (current-point)))
		(move-mark point (region-start *parse-input-region*))
		(unless (character-offset point ambig)
		  (move-mark point (region-end *parse-input-region*))
;; FIX orig
;;		  (buffer-end point)
		  ))
	      nil)))
	  (t
	   ;; HACK: If it doesn't have to exist, and the completion does not
	   ;; add anything, then return the completion's capitalization,
	   ;; instead of the user's input.
	   (list (if (= (length string) (length prefix)) prefix string))))))


;;;; Integer, expression, and string prompting.

(defhistory *integer-prompt-history* *integer-prompt-history-pointer* 30)

(defun prompt-for-integer (&key ((:must-exist *parse-value-must-exist*) t)
				default
				((:default-string *parse-default-string*))
				((:prompt *parse-prompt*) "Integer: ")
				((:help *parse-help*) "Type an integer.")
				((:history *parse-history*) *integer-prompt-history*)
				((:history-pointer *parse-history-pointer*)
				 '*integer-prompt-history-pointer*))
  "Prompt for a possibly signed integer.  If $must-exist is (), then return
   the input if it is an integer, else return the value of $must-exist."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (let ((number (parse-integer string  :junk-allowed t)))
	       (if *parse-value-must-exist*
		   (if number (list number))
		   (list (or number string))))))
	(*parse-default* (if default (write-to-string default :base 10)))
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*))
    (parse-for-something)))

(defvar editor-eof '(())
  "An object that won't be EQ to anything read.")

(defhistory *expression-prompt-history*
	    *expression-prompt-history-pointer*
	    350)

(defun prompt-for-expression (&key ((:must-exist *parse-value-must-exist*) t)
				   (default nil defaultp)
				   ((:default-string *parse-default-string*))
				   ((:prompt *parse-prompt*) "Expression: ")
				   ((:help *parse-help*)
				    "Type a Lisp expression.")
				   ((:history *parse-history*) *expression-prompt-history*)
				   ((:history-pointer *parse-history-pointer*)
				    '*expression-prompt-history-pointer*))
  "Prompt for a Lisp expression.  If $must-exist is () then return the
   string typed when a read error occurs."
  (let ((*parse-verification-function*
         #'(lambda (string)
	     (let ((expr (with-input-from-string (stream string)
			   (handler-case (read stream nil editor-eof)
			     (error () editor-eof)))))
	       (if *parse-value-must-exist*
		   (fi (eq expr editor-eof) (values (list expr) t))
		   (if (eq expr editor-eof)
		       (list string) (values (list expr) t))))))
	(*parse-default* (if defaultp (prin1-to-string default)))
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*))
      (parse-for-something)))

(defhistory *string-prompt-history* *string-prompt-history-pointer* 350)

(defun prompt-for-string (&key ((:default *parse-default*))
			       ((:default-string *parse-default-string*))
			       (initial)
			       (trim ())
			       ((:prompt *parse-prompt*) "String: ")
			       ((:help *parse-help*) "Type a string.")
			       ((:history *parse-history*)
				*string-prompt-history*)
			       ((:history-pointer *parse-history-pointer*)
				'*string-prompt-history-pointer*))
  "Prompt for a string.  If $trim is t, then trim leading and trailing
   whitespace from the input, otherwise trim the input with $trim
   interpreted as the `char-bag' argument to `string-trim'."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (list (string-trim (if (eq trim t) '(#\space #\tab) trim)
				string))))
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*))
    (parse-for-something initial)))

(defhistory *mode-prompt-history* *mode-prompt-history-pointer* 35)

(defun prompt-for-mode (&key ((:default *parse-default*))
			     ((:default-string *parse-default-string*))
			     ((:prompt *parse-prompt*) "File mode: ")
			     ((:help *parse-help*) "Enter a file mode, e.g. a+w.")
			     ((:history *parse-history*)
			      *mode-prompt-history*)
			     ((:history-pointer *parse-history-pointer*)
			      '*mode-prompt-history-pointer*))
  "Prompt for a mode."
  (let ((*parse-verification-function*
	 #'(lambda (mode)
	     (if (ignore-errors (parse-integer mode))
		 (list (parse-integer mode))
		 (let ((split (split mode '(#\+ #\-))))
		   (if (and split
			    (eq (length split) 2)
			    (every (lambda (char) (find char "augo"))
				   (car split))
			    (every (lambda (char) (find char "rwx"))
				   (cadr split)))
		       (list mode)
		       (let ((mode (ignore-errors
				    (eval (read-from-string mode)))))
			 (if (integerp mode) (list mode))))))))
	(*parse-input-region* *echo-parse-input-region*)
	(*parse-starting-mark* *echo-parse-starting-mark*))
    (parse-for-something)))


;;;; Yes-or-no and y-or-n prompting.

(defhistory *yes-or-no-history* *yes-or-no-history-pointer* 2)

(defvar *yes-or-no-string-table*
  (make-string-table :initial-contents '(("Yes" . t) ("No" . nil))))

(defun prompt-for-yes-or-no (&key ((:must-exist *parse-value-must-exist*) t)
				  (default nil defaultp)
				  ((:default-string *parse-default-string*))
				  ((:prompt *parse-prompt*) "Yes or No? ")
				  ((:help *parse-help*) "Type Yes or No.")
				  ((:history *parse-history*) *yes-or-no-history*)
				  ((:history-pointer *parse-history-pointer*)
				   '*yes-or-no-history-pointer*))
  "Prompt for a case-folded \"yes\" or \"no\", returning t or (),
   respectively.  If $default is supplied suggest \"Yes\" at the prompt if
   $default is true and \"No\" if $default is ().  If $must-exist is (),
   return the input string; however, if the input string is t or () return
   t or (), respectively.

   This is analogous to the function `yes-or-no-p'."
  (let* ((*parse-string-tables* (list *yes-or-no-string-table*))
	 (*parse-default* (if defaultp (if default "Yes" "No")))
	 (*parse-verification-function*
	  #'(lambda (string)
	      (multiple-value-bind
		  (prefix key value field ambig)
		  (complete-string string *parse-string-tables*)
		(declare (ignore prefix field ambig))
		(let ((won (or (eq key :complete) (eq key :unique))))
		  (if *parse-value-must-exist*
		      (if won (values (list value) t))
		      (list (if won (values value t) string)))))))
	 (*parse-type* :keyword)
	 (*parse-input-region* *echo-parse-input-region*)
	 (*parse-starting-mark* *echo-parse-starting-mark*))
    (parse-for-something)))

(defun prompt-for-y-or-n (&key ((:must-exist must-exist) t)
			       (default nil defaultp)
			       default-string
			       ((:prompt prompt) "Y or N? ")
			       ((:help *parse-help*) "Type Y or N."))
  "Prompt for y, Y, n, or N.  Return T for y and Y or () for n and N.  If
   $default is supplied suggest Y if $default is true, N if $default if ().
   If $must-exist is (), return the input key-event; however, if the input
   is one of y, Y, n or N, return true or () accordingly.

   This is analogous to `y-or-n-p'."
  (let ((old-window (current-window)))
    (unwind-protect
	(progn
	  (setf (current-window) *echo-area-window*)
	  (display-prompt-nicely prompt
				 (or default-string
				     (if defaultp
					 (if default "Y" "N"))))
	  (loop
	    (let ((key-event (get-key-event *editor-input*)))
	      (cond ((or (eq key-event #k"y")
			 (eq key-event #k"Y"))
		     (return t))
		    ((or (eq key-event #k"n")
			 (eq key-event #k"N"))
		     (return nil))
		    ((logical-key-event-p key-event :confirm)
		     (if defaultp
			 (return default)
			 (beep)))
		    ((logical-key-event-p key-event :help)
		     (ed::help-on-parse-command))
		    (t
		     (unless must-exist (return key-event))
		     (beep))))))
      (setf (current-window) old-window))))


;;;; Key-event and key prompting.

(defun prompt-for-key-event (&key (prompt "Key-event: ") (change-window t))
  "Prompt for a key-event, returning immediately after the next key-event.
   `command-case' is more useful for most purposes."
  (prompt-for-key-event* prompt change-window))

(defun prompt-for-key-event* (prompt change-window)
  (let ((old-window (current-window)))
    (unwind-protect
	(progn
	  (when change-window
	    (setf (current-window) *echo-area-window*))
	  (display-prompt-nicely prompt ())
	  (get-key-event *editor-input* t))
      (when change-window (setf (current-window) old-window)))))

(defvar *prompt-key* (make-array 10 :adjustable t :fill-pointer 0))

(defun prompt-for-key (&key ((:must-exist must-exist) t)
			    default default-string
			    (prompt "Key: ")
			    ((:help *parse-help*) "Type a key."))
  "Prompt for a key, a vector of key-events, suitable for passing to any of
   the functions that manipulate [key bindings].  If $must-exist is true,
   then the key must be bound in the current environment, and the command
   currently bound is returned as the second value."
  (let ((old-window (current-window))
	(string (if default
		    (or default-string
			(let ((l (coerce default 'list)))
			  (format nil "~:C~{ ~:C~}" (car l) (cdr l)))))))

    (unwind-protect
	(progn
	  (setf (current-window) *echo-area-window*)
	  (display-prompt-nicely prompt string)
	  (setf (fill-pointer *prompt-key*) 0)
	  (prog ((key *prompt-key*) key-event)
		(declare (vector key))
		TOP
		(setf key-event (get-key-event *editor-input*))
		(cond ((logical-key-event-p key-event :quote)
		       (setf key-event (get-key-event *editor-input* t)))
		      ((logical-key-event-p key-event :confirm)
		       (cond ((and default (zerop (length key)))
			      (let ((res (get-command default :current)))
				(unless (commandp res) (go FLAME))
				(return (values default res))))
			     ((and (not must-exist) (plusp (length key)))
			      (return (copy-seq key)))
			     (t
			      (go FLAME))))
		      ((logical-key-event-p key-event :help)
		       (ed::help-on-parse-command)
		       (go TOP)))
		(vector-push-extend key-event key)
		(when must-exist
		  (let ((res (get-command key :current)))
		    (cond ((commandp res)
			   (ext:print-pretty-key-event key-event
						       *echo-area-stream*
						       t)
			   (write-char #\space *echo-area-stream*)
			   (return (values (copy-seq key) res)))
			  ((not (eq res :prefix))
			   (vector-pop key)
			   (go FLAME)))))
		(print-pretty-key key-event *echo-area-stream* t)
		(write-char #\space *echo-area-stream*)
		(go TOP)
		FLAME
		(beep)
		(go TOP)))
      (force-output *echo-area-stream*)
      (setf (current-window) old-window))))


;;;; Prompting with a buffer.

(defhistory *buffer-input-history* *buffer-input-history-pointer* 350)

(defun prompt-in-buffer (buffer
			 &key ((:default *parse-default*))
			       ((:default-string *parse-default-string*))
			       (trim ())
			       ((:prompt *parse-prompt*))
			       ((:help *parse-help*) "Type a string.")
			       ((:history *parse-history*) *buffer-input-history*)
			       ((:history-pointer *parse-history-pointer*)
				'*buffer-input-history-pointer*))
  "\"Prompt\" via a recursive Buffer edit in the current window."
  (ed::change-to-buffer buffer)
  (let* ((*parse-start-mark* (copy-mark (buffer-point buffer)
					:right-inserting))
	 (*parse-input-region* (region *parse-start-mark*
				       (region-end (buffer-region buffer))))
	 (point (buffer-point buffer)))
    (if *parse-default-string*
	(insert-string point *parse-default-string*)
	(if *parse-default* (insert-string point
					   (format nil "~S" *parse-default*))))
    (if *parse-prompt* (message *parse-prompt*))
    (ed::do-recursive-edit)
    (let ((string (region-to-string (buffer-region buffer))))
      (or (and (plusp (ring-length *parse-history*))
	       (string= string (ring-ref *parse-history* 0)))
	  (ring-push string *parse-history*))
      (if trim (string-trim '(#\space #\tab) string) string))))


;;;; Logical key-event stuff.

#[ Logical Key-Events

Some functions such as `prompt-for-key' and commands such as query replace
read key-events directly from the keyboard instead of using the command
interpreter.  To encourage consistency between these commands and to make
them portable and easy to customize, there is a mechanism for defining
logical key-events.

A logical key-event is a keyword which stands for some set of key-events.  The
system globally interprets these key-events as indicators a particular action.
For example, the :help logical key-event represents the set of key-events
that request help in a given editor implementation.  This mapping is a
many-to-many mapping, not one-to-one, so a given logical key-event may have
multiple corresponding actual key-events.  Also, any key-event may represent
different logical key-events.

[ Logical Key-Event Functions       ]
[ System Defined Logical Key-Events ]
]#

(defvar *logical-key-event-names* (make-string-table)
  "A string-table mapping all logical key-event names to the keyword
   identifying the logical key-event.")

(defvar *real-to-logical-key-events* (make-hash-table :test #'eql)
  "A hashtable from real key-events to their corresponding logical
   key-event keywords.")

(defvar *logical-key-event-descriptors* (make-hash-table :test #'eq)
  "A hashtable from logical-key-events to logical-key-event-descriptors.")

(defstruct (logical-key-event-descriptor
	    (:constructor make-logical-key-event-descriptor ()))
  name
  key-events
  documentation)

#[ Logical Key-Event Functions

{variable:ed:*logical-key-event-names*}

{function:ed:define-logical-key-event}
{function:ed:logical-key-event-key-events}
{function:ed:logical-key-event-name}
{function:ed:logical-key-event-documentation}
{function:ed:logical-key-event-p}
]#

;;; LOGICAL-KEY-EVENT-P  --  Public
;;;
(defun logical-key-event-p (key-event keyword)
  "Return true if $key-event has been defined to have $keyword as its
   logical key-event.  The relation between logical and real key-events is
   defined by using `setf' on `logical-key-event-p'.  If it is set to true
   then calling `logical-key-event-p' with the same key-event and $keyword,
   will result in truth.  Setting to false produces the opposite result."
  (not (null (memq keyword (gethash key-event *real-to-logical-key-events*)))))

;;; GET-LOGICAL-KEY-EVENT-DESC  --  Internal
;;;
;;; Return the descriptor for the logical key-event keyword, or signal an
;;; error if it isn't defined.
;;;
(defun get-logical-key-event-desc (keyword)
  (let ((res (gethash keyword *logical-key-event-descriptors*)))
    (unless res
      (error "~S is not a defined logical-key-event keyword." keyword))
    res))

;;; %SET-LOGICAL-KEY-EVENT-P  --  Internal
;;;
;;; Add or remove a logical key-event link by adding to or deleting from
;;; the list in the from-char hashtable and the descriptor.
;;;
(defun %set-logical-key-event-p (key-event keyword new-value)
  (let ((entry (get-logical-key-event-desc keyword)))
    (cond
     (new-value
      (pushnew keyword (gethash key-event *real-to-logical-key-events*))
      (pushnew key-event (logical-key-event-descriptor-key-events entry)))
     (t
      (setf (gethash key-event *real-to-logical-key-events*)
	    (delete keyword (gethash key-event *real-to-logical-key-events*)))
      (setf (logical-key-event-descriptor-key-events entry)
	    (delete keyword (logical-key-event-descriptor-key-events entry))))))
  new-value)

;;; LOGICAL-KEY-EVENT-DOCUMENTATION, NAME, KEY-EVENTS  --  Public
;;;
;;; Grab the right field out of the descriptor and return it.
;;;
(defun logical-key-event-documentation (keyword)
  "Return the documentation for the logical key-event $keyword."
  (logical-key-event-descriptor-documentation
   (get-logical-key-event-desc keyword)))
;;;
(defun logical-key-event-name (keyword)
  "Return the string name for the logical key-event $keyword."
  (logical-key-event-descriptor-name (get-logical-key-event-desc keyword)))
;;;
(defun logical-key-event-key-events (keyword)
  "Return the list of key-events representing the logical key-event
   $keyword."
  (logical-key-event-descriptor-key-events
   (get-logical-key-event-desc keyword)))

;;; DEFINE-LOGICAL-KEY-EVENT  --  Public
;;;
;;; Make the entries in the two hashtables and the string-table.
;;;
(defun define-logical-key-event (name documentation)
  "Define a new logical key-event with $name, a simple-string.  Logical
   key-event operations take logical key-events arguments as a keyword
   whose name is string-name uppercased with spaces replaced by hyphens.

   $Documentation describes the action indicated by the logical key-event."
  (check-type name string)
  (check-type documentation (or string function))
  (let* ((keyword (string-to-keyword name))
	 (entry (or (gethash keyword *logical-key-event-descriptors*)
		    (setf (gethash keyword *logical-key-event-descriptors*)
			  (make-logical-key-event-descriptor)))))
    (setf (logical-key-event-descriptor-name entry) name)
    (setf (logical-key-event-descriptor-documentation entry) documentation)
    (setf (getstring name *logical-key-event-names*) keyword)))


;;;; Some standard logical-key-events.

(define-logical-key-event "Forward Search"
  "This key-event is used to indicate that a forward search should be made.")
(define-logical-key-event "Backward Search"
  "This key-event is used to indicate that a backward search should be made.")
(define-logical-key-event "Recursive Edit"
  "This key-event indicates that a recursive edit should be entered.")
(define-logical-key-event "Cancel"
  "This key-event is used  to cancel a previous key-event of input.")
(define-logical-key-event "Abort"
  "This key-event is used to abort the command in progress.")
(define-logical-key-event "Exit"
  "This key-event is used to exit normally the command in progress.")
(define-logical-key-event "Yes"
  "This key-event is used to indicate a positive response.")
(define-logical-key-event "No"
  "This key-event is used to indicate a negative response.")
(define-logical-key-event "Do All"
  "This key-event means do it as many times as possible.")
(define-logical-key-event "Do Once"
  "This key-event means, do it this time, then exit.")
(define-logical-key-event "Help"
  "This key-event is used to ask for help.")
(define-logical-key-event "Confirm"
  "This key-event is used to confirm some choice.")
(define-logical-key-event "Quote"
  "This key-event is used to quote the next key-event of input.")
(define-logical-key-event "Keep"
  "This key-event means exit but keep something around.")
(define-logical-key-event "Switch To Reference"
  "This key-event is used to switch to a reference buffer.")


;;;; COMMAND-CASE help message printing.

(defvar *my-string-output-stream* (make-string-output-stream))

(defun chars-to-string (chars)
  (do ((s *my-string-output-stream*)
       (chars chars (cdr chars)))
      ((null chars)
       (get-output-stream-string s))
    (let ((char (car chars)))
      (if (characterp char)
	  (write-char char s)
	  (do ((key-events
		(logical-key-event-key-events char)
		(cdr key-events)))
	      ((null key-events))
	    (ext:print-pretty-key (car key-events) s)
	    (unless (null (cdr key-events))
	      (write-string ", " s))))
      (unless (null (cdr chars))
	(write-string ", " s)))))

;;; COMMAND-CASE-HELP  --  Internal
;;;
;;; Print out a help message derived from the options in a random-typeout
;;; window.
;;;
(defun command-case-help (help options)
  (let ((help (if (listp help)
		  (apply #'format nil help) help)))
    (with-pop-up-display (s)
      (write-string help s)
      (fresh-line s)
      (do ((o options (cdr o)))
	  ((null o))
	(let ((string (chars-to-string (caar o))))
	  (declare (simple-string string))
	  (if (= (length string) 1)
	      (write-char (char string 0) s)
	      (write-line string s))
	  (write-string "  - " s)
	  (write-line (cdar o) s))))))
