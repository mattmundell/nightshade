;;; Basic commands.

(in-package "ED")

(export '(word-offset))


#[ Invoking Commands

In order to get a command to do its thing, it must be invoked.  The user can do
this two ways, by typing the key to which the command is bound or by
using an extended command.  Commonly used commands are invoked via their
key bindings since they are faster to type, while less used commands are
invoked as extended commands since they are easier to remember.

[ Key Bindings      ]
[ Extended Commands ]
]#

#[ Basic Commands

[ Motion Commands         ]
[ The Mark and The Region ]
[ Modification Commands   ]
[ Filtering               ]
[ Searching and Replacing ]
[ Page Commands           ]
[ Counting Commands       ]
[ Registers               ]
]#

#[ Modification Commands

There is a wide variety of basic text-modification commands, but once again
the simplest ones are the most often used.

[ Inserting Characters       ]
[ Deleting Characters        ]
[ Killing and Deleting       ]
[ Kill Ring Manipulation     ]
[ Killing Commands           ]
[ Case Modification Commands ]
[ Transposition Commands     ]
[ Whitespace Manipulation    ]
]#

;;; Make a mark for buffers as they're consed.

(defun hcmd-new-buffer-hook-fun (buff)
  (let ((ring (make-ring 10 #'delete-mark)))
    (defevar "Buffer Mark Ring"
      "This variable holds this buffer's mark ring."
      :buffer buff
      :value ring)
    (ring-push (copy-mark (buffer-point buff) :right-inserting) ring)))

(add-hook make-buffer-hook #'hcmd-new-buffer-hook-fun)
(dolist (buff *buffer-list*) (hcmd-new-buffer-hook-fun buff))

#[ Entering and Exiting

The editor is entered by using the Lisp ed function.  Simply typing (ed)
will enter the editor, leaving you in the state that you were in when you
left it.  The first time the editor is entered the current buffer will be
`Main' or `Welcome'.  The -edit command-line switch may also be used to
enter the editor, as described in [ Command Line Options ].

ed may optionally be given a file name or a symbol argument.  Typing (ed
filename) will cause the specified file to be read into the editor, as
though by `Find File'.  Typing (ed symbol) will pretty-print the definition
of the symbol into a buffer whose name is obtained by adding "Edit " to the
beginning of the symbol's name.

{command:Exit}
{command:Pause}
]#

(defcommand "Exit" ()
  "Exit the editor, returning t.  Simply exit, leaving modified buffer as
   they are.  Thereafter (ed) returns to the editor in the same state.
   Quiting, for example via (quit), loses any modifications."
  (exit))

(defcommand "Pause" ()
  "Pause the editor process, returning to the process that invoked it.
   When resumed, the process is still running the editor."
  (pause))


#[ Inserting Characters

In the editor, you can insert characters with graphic representations by typing
the corresponding key-event which you normally generate with the obvious
keyboard key.  You can only insert characters whose codes correspond to ASCII
codes.  To insert those without graphic representations, use `Quoted
Insert'.

{command:Self Insert}
{command:New Line}

Many key-events that have corresponding ASCII characters are bound to
commands other than `Self Insert'.  Sometimes they are otherwise encumbered
such as with C-g.  `Quoted Insert' prompts for a key-event, without any
command interpretation semantics, and inserts the corresponding character.
A common use for this command is inserting a Formfeed by typing C-q C-l.

{command:Quoted Insert}
{command:Open Line}

`Open Line' is useful to open up some room to work in in the middle of a
screen of text.  Related to `Delete Blank Lines'.
]#


;;;; Simple character manipulation.

(defcommand "Self Insert" (p)
  "Insert into the buffer the character corresponding to the key-event
   typed to invoke the command.  If a prefix argument is supplied, then
   insert the character that many times."
  "Insert into the buffer the last key event typed.  If a prefix argument
   is supplied, then insert the character that many times."
  (let ((char (ext:key-event-char *last-key-event-typed*)))
    (or char (editor-error "Failed to insert last character."))
    (if (and p (> p 1))
	(insert-string
	 (current-point)
	 (make-string p :initial-element char))
	(insert-character (current-point) char))))

(defcommand "Quoted Insert" (p)
  "Read a character from the terminal and insert it.  With prefix argument,
   insert the character that many times.  If the appropriate character has
   some code other than an ASCII code, beep and signal an editor error."
  "Read a key-event from *editor-input* and insert it at the point."
  (let ((char (ext:key-event-char (get-key-event *editor-input* t)))
	(point (current-point)))
    (or char (editor-error "Can't insert that character."))
    (if (and p (> p 1))
	(insert-string point (make-string p :initial-element char))
	(insert-character point char))))

(defcommand "Forward Character" (p)
  "Move the point forward one character.  With a prefix argument move that
   many characters, with a negative argument go backwards."
  "Move the point of the current buffer forward $p characters."
  (let ((p (or p 1)))
    (cond ((character-offset (current-point) p))
	  ((= p 1)
	   (editor-error "Last character."))
	  ((= p -1)
	   (editor-error "First character."))
	  (t
	   (if (plusp p)
	       (buffer-end (current-point))
	       (buffer-start (current-point)))
	   (editor-error "Too few characters.")))))

(defcommand "Backward Character" (p)
  "Move the point backward one character.  With a prefix argument move that
   many characters backward, with a negative argument go forwards."
  "Move the point p characters backward."
  (forward-character-command (if p (- p) -1)))

#[ Deleting Characters

There are a number of commands for deleting characters as well.

{evariable:Character Deletion Threshold}
{command:Delete Next Character}
{command:Delete Previous Character}
{command:Delete Previous Character Expanding Tabs}

Various language modes that use tabs for indentation bind Delete to `Delete
Previous Character Expanding Tabs'.
]#

#|
(defcommand "Delete Next Character" (p)
  "Deletes the character to the right of the point.
  With prefix argument, delete that many characters to the right
  (or left if prefix is negative)."
  "Deletes p characters to the right of the point."
  (unless (delete-characters (current-point) (or p 1))
    (buffer-end (current-point))
    (editor-error "No next character.")))

(defcommand "Delete Previous Character" (p)
  "Deletes the character to the left of the point.
  With prefix argument, delete that many characters to the left
  (or right if prefix is negative)."
  "Deletes p characters to the left of the point."
  (unless (delete-characters (current-point) (if p (- p) -1))
    (editor-error "No previous character.")))
|#

(defcommand "Delete Next Character" (p)
  "Delete the character immediately following the point, that is, the
   character which appears under the cursor.  With a prefix argument,
   delete that many characters after the point.  With a negative prefix
   delete characters before the point."
  "Delete p characters following point."
  (cond ((kill-characters (current-point) (or p 1)))
	((and p (minusp p))
	 (editor-error "Too few previous characters."))
	(t
	 (editor-error "Too few following characters."))))

(defcommand "Delete Previous Character" (p)
  "Delete the character immediately before the point, that is, the
   character which appears before the cursor.  With a prefix argument,
   delete that many characters before the point.  With a negative prefix
   delete characters after the point."
  "Delete p characters before point."
  (delete-next-character-command (- (or p 1))))

(defcommand "Transpose Characters" (p)
  "Transpose (swap) the characters on either side of the point and move
   forward With a prefix argument, transpose that many times.  With a
   negative prefix move point backwards instead of forwards."
  (let ((arg (or p 1))
	(point (current-point)))
    (dotimes (i (abs arg))
      (when (or (minusp arg) (end-line-p point)) (mark-before point))
      (let ((prev (previous-character point))
	    (next (next-character point)))
	(cond ((not prev) (editor-error "First character."))
	      ((not next) (editor-error "Last character."))
	      (t
	       (setf (previous-character point) next)
	       (setf (next-character point) prev))))
      (when (plusp arg) (mark-after point)))))


;;;; Character inserters for numpad.

(defun self-insert (p char)
  (if (and p (> p 1))
      (insert-string
       (current-point)
       (make-string p :initial-element char))
      (insert-character (current-point) char)))

(defcommand "Insert /" (p)
  "Insert a backslash."
  (self-insert p #\/))

(defcommand "Insert *" (p)
  "Insert a star."
  (self-insert p #\*))

(defcommand "Insert Minus" (p)
  "Insert a minus."
  (self-insert p #\-))

(defcommand "Insert +" (p)
  "Insert a plus."
  (self-insert p #\+))


;;;; Word hacking commands.

(defun word-offset (mark &optional (offset 1))
  "Move MARK OFFSET words forward (if positive) or backwards (if
   negative).  If MARK is in the middle of a word, that counts as one.  If
   there were OFFSET (-OFFSET if negative) words in the appropriate
   direction, return MARK, otherwise ().  Always move MARK.  A word lies
   between two characters whose `Word Delimiter' attribute value is 1 (as in
   [System Defined Character Attributes])."
  (if (minusp offset)
      (do ((cnt offset (1+ cnt)))
	  ((zerop cnt) mark)
	(cond
	 ((null (reverse-find-attribute mark :word-delimiter #'zerop))
	  (return nil))
	 ((reverse-find-attribute mark :word-delimiter))
	 (t
	  (move-mark
	   mark (buffer-start-mark (line-buffer (mark-line mark)))))))
      (do ((cnt offset (1- cnt)))
	  ((zerop cnt) mark)
	(cond
	 ((null (find-attribute mark :word-delimiter #'zerop))
	  (return nil))
	 ((null (find-attribute mark :word-delimiter))
	  (return nil))))))

(defcommand "Forward Word" (p)
  "Moves forward one word.
  With prefix argument, moves the point forward over that many words."
  "Moves the point forward p words."
  (cond ((word-offset (current-point) (or p 1)))
	((and p (minusp p))
	 (buffer-start (current-point))
	 (editor-error "No previous word."))
	(t
	 (buffer-end (current-point))
	 (editor-error "No next word."))))

(defcommand "Backward Word" (p)
  "Moves backward one word.
  With prefix argument, moves the point back over that many words."
  "Moves the point backward p words."
  (forward-word-command (- (or p 1))))


#[ Motion Commands

There is a fairly small number of
basic commands for moving around in the buffer.  While there are many other
more complex motion commands, these are by far the most commonly used and
the easiest to learn.

{command:Forward Character}
{command:Backward Character}
{command:Forward Word}
{command:Backward Word}

In `Forward Word' and `Backward Word' the point is always left between the
last word and first non-word character in the direction of motion.  This
means that after moving backward the cursor appears on the first character
of the word, while after moving forward, the cursor appears on the
delimiting character.

{command:Next Line}
{command:Previous Line}
{command:Goto Absolute Line}
{command:End of Line}
{command:Beginning of Line}
{command:Scroll Window Down}
{command:Scroll Window Up}
{evariable:Scroll Overlap}
{command:End of Buffer}
{command:Beginning of Buffer}
{command:Top of Window}
{command:Bottom of Window}
]#


;;;; Moving around.

(defvar *target-column* 0)

(defun set-target-column (mark)
  (if (eq (last-command-type) :line-motion)
      *target-column*
      (setq *target-column* (mark-column mark))))

(defevar "Next Line Insert"
  "If a true value then `Next Line' inserts lines apon reaching the buffer
   end."
  :value t)

(defcommand "Next Line" (p)
  "Moves the point to the next line, while remaining the same distance
   within a line.  This motion is by logical lines, each of which may take
   up many lines on the screen if it wraps.  argument is supplied, then the
   point is moved by that many lines.  With a prefix argument, move the
   point that many lines down (or up if the prefix is negative).

   If the new line is shorter than the current line, then leave the point
   at the end of the new line."
  "Moves the point down $p logical lines."
  (let* ((point (current-point))
	 (target (set-target-column point)))
    (unless (line-offset point (or p 1))
      (cond ((not p)
	     (when (same-line-p point (buffer-end-mark (current-buffer)))
	       (line-end point))
	     (if (value next-line-insert)
		 (insert-character point #\newline)
		 (editor-error "Last line.")))
	    ((minusp p)
	     (buffer-start point)
	     (editor-error "First line."))
	    (t
	     (buffer-end point)
	     (if (value next-line-insert)
		 (dotimes (i p)
		   (insert-character point #\newline))
		 (when p (editor-error "Last line."))))))
    (or (move-to-column point target) (line-end point))
    (setf (last-command-type) :line-motion)))

(defcommand "Previous Line" (p)
  "Moves the point to the previous line, while remaining the same distance
   within a line.  This motion is by logical lines, each of which may take
   up many lines on the screen if it wraps.  argument is supplied, then the
   point is moved by that many lines.  With a prefix argument, move the
   point that many lines up (or down if the prefix is negative).

   If the new line is shorter than the current line, then leave the point
   at the end of the new line."
  "Moves the point up p lines."
  (next-line-command (- (or p 1))))

(defcommand "Mark to End of Buffer" ()
  "Set the current region to be from point to the end of the buffer."
  (push-buffer-mark (buffer-end (copy-mark (current-point))) t))

(defcommand "Mark to Beginning of Buffer" ()
  "Set the current region to be from the beginning of the buffer to point."
  (push-buffer-mark (buffer-start (copy-mark (current-point))) t))

(defcommand "Beginning of Buffer" ()
  "Move the point to the very beginning of the current buffer.  Before
   moving the point, save its position by pushing it on the mark stack"
  (let ((point (current-point)))
    (push-buffer-mark (copy-mark point))
    (buffer-start point)))

(defcommand "End of Buffer" ()
  "Move the point to the very end of the current buffer.  Before moving the
   point, save its position by pushing it on the mark stack"
  (let ((point (current-point)))
    (push-buffer-mark (copy-mark point))
    (buffer-end point)))

(defcommand "Beginning of Line" (p)
  "Move the point to the beginning of the current line.  With prefix
   argument, move the point to the beginning of the prefix'th next line."
  "Move the point down p lines and then to the beginning of the line."
  (let ((point (current-point)))
    (unless (line-offset point (if p p 0)) (editor-error "No such line."))
    (line-start point)))

(defcommand "End of Line" (p)
  "Move the point to the end of the current line.  With prefix argument,
   move the point to the end of the prefix'th next line."
  "Move the point down p lines and then to the end of the line."
  (let ((point (current-point)))
    (or (line-offset point (if p p 0)) (editor-error "Out of buffer."))
    (line-end point)))

(defevar "Scroll Overlap"
  "The commands `Scroll Window Down' and `Scroll Window Up' leave this much
   overlap between screens."
  :value 2)

(defevar "Scroll Redraw Ratio"
  "This is a ratio of \"inserted\" lines to the size of a window.  When
   this ratio is exceeded, insert/delete line terminal optimization is
   aborted, and every altered line is simply redrawn as efficiently as
   possible.  For example, setting this to 1/4 will cause scrolling
   commands to redraw the entire window instead of moving the bottom two
   lines of the window to the top (typically 3/4 of the window is being
   deleted upward and inserted downward, hence a redraw); however, commands
   like `New Line' and `Open Line' will still efficiently insert a line,
   moving the rest of the window's text downward."
  :value 1/4)

(defcommand "Scroll Window Down" (p (window (current-window)))
  "Move forward in the buffer by one screenful of text, determining the
   exact amount from the size of the window.  With prefix argument move
   forward that many lines.  When this action scrolls the line with the
   point off the screen, move the point to the vertical center of the
   window."
  "If $p is supplied then scroll $window that many lines, else scroll
   $window down one screenfull.  "
  (if p
      (scroll-window window p)
      (let ((height (window-height window))
	    (overlap (value scroll-overlap)))
	(scroll-window window (if (<= height overlap)
				  height (- height overlap))))))

(defcommand "Scroll Window Up" (p (window (current-window)))
  "Move forward in the buffer by one screenful of text, determining the
   exact amount from the size of the window.  With prefix argument move
   forward that many lines.  When this action scrolls the line with the
   point off the screen, move the point to the vertical center of the
   window."
  "If $p is supplied then scroll $window that many lines, else scroll
   $window down one screenfull.  "
  (if p
      (scroll-window window (- p))
      (let ((height (- (window-height window)))
	    (overlap (- (value scroll-overlap))))
	(scroll-window window (if (>= height overlap)
				  height (- height overlap))))))

(defcommand "Scroll Next Window Down" (p)
  "Do a `Scroll Window Down' on the next window."
  (let ((win (next-window (current-window))))
    (if (eq win (current-window)) (editor-error "Only one window."))
    (scroll-window-down-command p win)))

(defcommand "Scroll Next Window Up" (p)
  "Do a `Scroll Window Up' on the next window."
  (let ((win (next-window (current-window))))
    (if (eq win (current-window)) (editor-error "Only one window."))
    (scroll-window-up-command p win)))

(defcommand "Top of Window" ()
  "Move the point to the beginning of the first line displayed in the
   current window."
  "Move the point to the top of the current window."
  (move-mark (current-point) (window-display-start (current-window))))

(defcommand "Bottom of Window" ()
  "Move the point to the beginning of the last line displayed in the
   current window."
  "Move the point to the bottom of the current window."
  (line-start (current-point)
	      (mark-line (window-display-end (current-window)))))


;;;; Kind of miscellaneous commands.

;;; "Refresh Screen" may not be right with respect to wrapping lines in
;;; the case where an argument is supplied due the use of
;;; WINDOW-DISPLAY-START instead of SCROLL-WINDOW, but using the latter
;;; messed with point and did other hard to predict stuff.
;;;
(defcommand "Refresh Screen" (p)
  "Refresh all windows, centering the current window about the current line.
   When the user supplies a positive argument, it scrolls that line to the
   top of the window.  When the argument is negative, the line that far
   from the bottom of the window is moved to the bottom of the window.  In
   either case when an argument is supplied, this command only refreshes
   the current window."
  (let ((window (current-window)))
    (cond ((not p) (center-window window (current-point)))
	  ((zerop p) (line-to-top-of-window-command))
	  ((line-offset (window-display-start window)
			(if (plusp p) (1- p) (1+ p))
			0))
	  (t (editor-error "Too few lines."))))
  (or p (redisplay-all)))

#[ Recentering Windows

When redisplaying the current window, the editor makes sure the current point is
visible.  This is the behavior you see when you are entering text near the
bottom of the window, and suddenly redisplay shifts your position to the
window's center.

Some buffers receive input from streams and other processes, and you might have
windows displaying these.  However, if those windows are not the current
window, the output will run off the bottom of the windows, and you won't be
able to see the output as it appears in the buffers.  You can change to a
window in which you want to track output and invoke the following command to
remedy this situation.

{command: Track Buffer Point}
]#

(defcommand "Track Buffer Point" ()
  "Make the current window track the buffer's point.  This means that each
   time the editor redisplays, it will make sure the buffer's point is
   visible in the window.  This is useful for windows into buffers that
   receive output from streams coming from other processes."
  "Make the current window track the buffer's point."
  (setf (window-display-recentering (current-window)) t))
;;;
(defun reset-window-display-recentering (window &optional buffer)
  (declare (ignore buffer))
  (setf (window-display-recentering window) nil))
;;;
(add-hook window-buffer-hook #'reset-window-display-recentering)

(defvar *extended-command-history* (make-ring 350)
  "This ring-buffer contains previously input extended commands.")

(defvar *extended-command-history-pointer* 0
  "Current position during a historical exploration.")

#[ Extended Commands

A command is invoked as an extended command by typing its name to the
`Extended Command' command, which is invoked using its key binding,
{command-binding: Extended Command}.

{command: Extended Command}
]#

(defun call-command (name function p)
  "Call command $name with prefix argument $p."
  (let ((function (command-function function))
	(bindings (visible-command-bindings function)))
    (unwind-protect
	(funcall function p)
      (if bindings
	  (let ((msg (with-output-to-string (stream)
		       (format stream "~A is bound to: " name)
		       (print-command-bindings bindings stream))))
	    (setf (aref msg (1- (length msg))) #\ )
	    (setf (aref msg (- (length msg) 2)) #\.)
	    (message msg))))))

(defcommand "Extended Command" (p)
  "Prompt in the echo area for the name of a command, and then invoke that
   command.  The prefix argument is passed through to the command invoked.
   The command name can be partially typed, as long as enough of its name
   is supplied to uniquely identify it.  Completion is available using
   Escape and Space, and a list of possible completions is given by Home or
   C-_."
  "Prompt for and execute an extended command.  Pass the prefix argument
   to the command."
  (multiple-value-bind (name function)
		       (prompt-for-keyword (list *command-names*)
					   :prompt "Command: "
					   :help "Name of an editor command"
					   :history *extended-command-history*
					   :history-pointer
					   '*extended-command-history-pointer*)
    (call-command name function p)))

#[ The Prefix Argument

The prefix argument is an integer argument which may be supplied to a
command.  It is known as the prefix argument because it is specified by
invoking some prefix argument setting command immediately before the
command to be given the argument.  The following statements about the
interpretation of the prefix argument are true:

  - When it is meaningful, most commands interpret the prefix argument as a
    repeat count, causing the same effect as invoking the command that many
    times.

  - When it is meaningful, most commands that use the prefix argument interpret
    a negative prefix argument as meaning the same thing as a positive
    argument, but the action is done in the opposite direction.

  - Most commands treat the absence of a prefix argument as meaning the same
    thing as a prefix argument of one.

  - Many commands ignore the prefix argument entirely.

  - Some commands do none of the above.

The following commands are used to set the prefix argument:

{command:Argument Digit}
{command:Negative Argument}
{command:Universal Argument}
{evariable:Universal Argument Fallback}
]#

(defevar "Universal Argument Fallback"
  "Fallback value and multiplier for the `Universal Argument' command."
  :value 4)

(defcommand "Universal Argument" ()
  "Set the prefix argument for the next command, or multiply it by four.
   Typing digits, regardless of any modifier keys, specifies the argument.
   Optionally, you may first type a sign (- or +).  While typing digits, if
   you type C-U or C-u, the digits following the C-U form a number this
   command multiplies by the digits preceding the C-U.  The default value
   for this command and any number following a C-U is the value of
   *Universal Argument Fallback*."
  "Set the prefix argument for the next command, or multiply it by four."
  (clear-echo-area)
  (write-string "C-U " *echo-area-stream*)
  (let* ((key-event (get-key-event *editor-input*))
	 (char (ext:key-event-char key-event)))
    (if char
	(case char
	  (#\-
	   (write-char #\- *echo-area-stream*)
	   (universal-argument-loop (get-key-event *editor-input*) -1))
	  (#\+
	   (write-char #\+ *echo-area-stream*)
	   (universal-argument-loop (get-key-event *editor-input*) -1))
	  (t
	   (universal-argument-loop key-event 1)))
	(universal-argument-loop key-event 1))))

(defcommand "Negative Argument" (p)
  "Negate the prefix argument.  Wait for more digits and a command to which
   to give the prefix argument.  Equivalent to invoking `Universal
   Argument' and typing a minus sign (-).  "
  "Negate the prefix argument.  Wait for more digits and a command to which
   to give the prefix argument.  Used for M--."
  (when p (editor-error "Must type minus sign first."))
  (clear-echo-area)
  (write-string "C-U -" *echo-area-stream*)
  (universal-argument-loop (get-key-event *editor-input*) -1))

(defcommand "Argument Digit" ()
  "Set the prefix argument to the last character typed and then read more
   digits and a command to call with the prefix argument.  Equivalent to
   invoking `Universal Argument' and typing a digit."
  "Set the prefix argument to the last character typed and then read more
   digits and a command to call with the prefix argument.  Used for keys
   such as M-1 as a short way to initiate the universal argument."
  (clear-echo-area)
  (write-string "C-U " *echo-area-stream*)
  (universal-argument-loop *last-key-event-typed* 1))

(defun universal-argument-loop (key-event sign &optional (multiplier 1))
  (flet ((prefix (sign multiplier read-some-digit-p result)
	   ;; read-some-digit-p and (zerop result) are not
	   ;; equivalent if the user invokes this and types 0.
	   (* sign multiplier
	      (if read-some-digit-p
		  result
		  (value universal-argument-fallback)))))
    (let* ((stripped-key-event (if key-event (ext:make-key-event key-event)))
	   (char (ext:key-event-char stripped-key-event))
	   (digit (if char (digit-char-p char)))
	   (result 0)
	   (read-some-digit-p nil))
      (loop
	(cond (digit
	       (setf read-some-digit-p t)
	       (write-char char *echo-area-stream*)
	       (setf result (+ digit (* 10 result)))
	       (setf key-event (get-key-event *editor-input*))
	       (setf stripped-key-event (if key-event
					    (ext:make-key-event key-event)))
	       (setf char (ext:key-event-char stripped-key-event))
	       (setf digit (if char (digit-char-p char))))
	      ((or (eq key-event #k"C-u") (eq key-event #k"C-U"))
	       (write-string " C-U " *echo-area-stream*)
	       (universal-argument-loop
		(get-key-event *editor-input*) 1
		(prefix sign multiplier read-some-digit-p result))
	       (return))
	      (t
	       (unget-key-event key-event *editor-input*)
	       (setf (prefix-argument)
		     (prefix sign multiplier read-some-digit-p result))
	       (return))))))
  (setf (last-command-type) (last-command-type)))
