;;; Even more commands...

(in-package "ED")

(export '(check-region-query-size display-page-directory goto-page
	  page-directory page-offset supply-generic-pointer-up-function))

(defevar "Region Query Size"
  "The region size, in lines, above which various commands ask for
   confirmation before modifying a region.  If (), then these commands
   refrain from asking, even if the region is very large."
  :value 30)

(defun check-region-query-size (region)
  "Count the lines in $region, and if their number exceeds *Region Query
   Size*, prompt for confirmation.  If the response is negative then signal
   an editor error."
  (let ((threshold (or (value region-query-size) 0)))
    (if (and (plusp threshold)
	     (>= (count-lines region) threshold))
	(or (prompt-for-y-or-n
	     :prompt "Region size exceeds *Region Query Size*.  Continue? "
	     :default ()
	     :must-exist t)
	    (editor-error "Command canceled.")))))


#[ Case Modification Commands

The editor provides a few case modification commands, which are often
useful for correcting typos.

{command:Capitalize Word}
{command:Lowercase Word}
{command:Uppercase Word}
{command:Lowercase Region}
{command:Uppercase Region}
]#


;;;; Casing commands.

(defcommand "Uppercase Word" (p)
  "Uppercase the word at point.  With a prefix argument, uppercase that
   many words.  With a negative argument uppercase words before the point,
   leaving the point where it was."
  (filter-words p (current-point) #'string-upcase))

(defcommand "Lowercase Word" (p)
  "Lowercase the word at point.  With a prefix argument, lowercase that
   many words.  With a negative argument lowercase words before the point,
   leaving the point where it was."
  (filter-words p (current-point) #'string-downcase))

;;; FILTER-WORDS implements "Uppercase Word" and "Lowercase Word".
;;;
(defun filter-words (p point function)
  (let ((arg (or p 1)))
    (with-mark ((mark point))
      (if (word-offset (if (minusp arg) mark point) arg)
	  (filter-region function (region mark point))
	  (editor-error "Too few words.")))))

;;; "Capitalize Word" is different than uppercasing and lowercasing because
;;; the differences between the editor's notion of what a word is and
;;; Lisp's notion are too annoying.
;;;
(defcommand "Capitalize Word" (p)
  "Lowercase a word then capitalize the first character.  With a prefix
   argument, capitalize that many words.  With a negative argument
   capitalize words before the point, leaving the point where it was."
  (let ((point (current-point))
	(arg (or p 1)))
    (with-mark ((start point :left-inserting)
		(end point))
      (if (minusp arg)
	  (or (word-offset start arg) (editor-error "First word.")))
      (do ((region (region start end))
	   (cnt (abs arg) (1- cnt)))
	  ((zerop cnt) (move-mark point end))
	(or (find-attribute start :word-delimiter #'zerop)
	    (editor-error "Last word."))
	(move-mark end start)
	(find-attribute end :word-delimiter)
	(loop
	  (when (mark= start end)
	    (move-mark point end)
	    (editor-error "No alphabetic characters in word."))
	  (if (alpha-char-p (next-character start)) (return))
	  (character-offset start 1))
	(setf (next-character start) (char-upcase (next-character start)))
	(mark-after start)
	(filter-region #'string-downcase region)))))

(defcommand "Uppercase Region" ()
  "Uppercase words from point to mark.  Ask for confirmation before
   modifying large regions."
  (twiddle-region (current-region) #'string-upcase "Uppercase Region"))

(defcommand "Lowercase Region" ()
  "Lowercase words from point to mark.  Ask for confirmation before
   modifying large regions."
  (twiddle-region (current-region) #'string-downcase "Lowercase Region"))

;;; TWIDDLE-REGION implements "Uppercase Region" and "Lowercase Region".
;;;
(defun twiddle-region (region function name)
  (let* (;; don't delete marks start and end since undo stuff will.
	 (start (copy-mark (region-start region) :left-inserting))
	 (end (copy-mark (region-end region) :left-inserting)))
    (let* ((region (region start end))
	   (undo-region (copy-region region)))
      (check-region-query-size region)
      (filter-region function region)
      (make-region-undo :twiddle name region undo-region))))


;;;; More stuff.

(defcommand "Delete Previous Character Expanding Tabs" (p)
  "Delete the character immediately before the point (that is, the
   character which appears before the cursor), treating tabs as the
   equivalent number of spaces.  With a prefix argument, delete that many
   characters before the point.  With a negative prefix delete characters
   after the point."
  "Delete $p characters before point, treating tabs as the equivalent number
   of spaces."
  (let ((point (current-point))
        (n (or p 1)))
    (if (minusp n)
	(editor-error "Delete Previous Character Expanding Tabs only accepts ~
		       positive arguments."))
    ;; Pre-calculate the number of characters that need to be deleted
    ;; and any remaining white space filling, allowing modification to
    ;; be avoided if there are not enough characters to delete.
    (let ((errorp nil)
	  (del 0)
	  (fill 0))
      (with-mark ((mark point))
	(dotimes (i n)
	  (if (> fill 0)
	      (decf fill)
	      (let ((prev (previous-character mark)))
		(cond ((and prev (char= prev #\tab))
		       (let ((pos (mark-column mark)))
			 (mark-before mark)
			 (incf fill (- pos (mark-column mark) 1)))
		       (incf del))
		      ((mark-before mark)
		       (incf del))
		      (t
		       (setq errorp t)
		       (return)))))))
      (cond ((and (not errorp) (kill-characters point (- del)))
	     (with-mark ((mark point :left-inserting))
	       (dotimes (i fill)
		 (insert-character mark #\space))))
	    (t
	     (editor-error "There were not ~D characters before point." n))))))


(defvar *scope-table*
  (list (make-string-table :initial-contents
			   '(("Global" . :global)
			     ("Buffer" . :buffer)
			     ("Mode" . :mode)))))

(defun prompt-for-place (prompt help)
  (multiple-value-bind (word val)
		       (prompt-for-keyword *scope-table* :prompt prompt
					   :help help :default "Global")
    (declare (ignore word))
    (case val
      (:buffer
       (values :buffer (prompt-for-buffer :help "Buffer to be local to."
					  :default (current-buffer))))
      (:mode
       (values :mode (prompt-for-keyword
		      (list *mode-names*)
		      :prompt "Mode: "
		      :help "Mode to be local to."
		      :default (buffer-major-mode (current-buffer)))))
      (:global :global))))

#[ Binding Keys

{command:Bind Key}
{command:Delete Key Binding}
]#

(defcommand "Bind Key" ()
  "Bind a command to a key in a prompted place.  The following places are
   available:

     buffer
	Make a key binding which is only present when a buffer is the
	current buffer.

     mode
	Makes a key binding which is only in present when a mode is active
	in the current buffer.

     global

	Makes a global key binding which is always in effect and can be
	shadowed by a buffer or mode binding."
  (multiple-value-call #'bind-key
    (values (prompt-for-keyword
	     (list *command-names*)
	     :prompt "Command to bind: "
	     :help "Name of command to bind to a key."))
    (values (prompt-for-key
	     :prompt "Bind to: "  :must-exist nil
	     :help "Key to bind command to, confirm to complete."))
    (prompt-for-place "Kind of binding: "
		      "The kind of binding to make.")))

(defcommand "Delete Key Binding" ()
  "Delete a key binding in a mode or buffer, or globally."
  (let ((key (prompt-for-key
	      :prompt "Delete binding: " :must-exist nil
	      :help "Key to delete binding from.")))
    (multiple-value-bind (kind where)
			 (prompt-for-place "Kind of binding: "
					   "The kind of binding to make.")
      (or (get-command key kind where)
	  (editor-error "No such binding: ~S" key))
      (delete-key-binding key kind where))))


#[ Editor Variables

A number of commands use the editor variables as flags to control their
behavior.  Often you can get a command to do what you want by setting a
variable.  Generally the default value for a variable is chosen to be the
safest value for novice users.

{command:Set Variable}
{command:Set Buffer Variable}
{command:Set Mode Variable}
{command:Clear Variable}
{command:Defevar}

`Defevar' is most useful for making mode or buffer local bindings of
variables.  Redefining a variable in a mode or buffer creates a
customization that takes effect only when in that mode or buffer.

{command:List Buffer Variable}
{command:List Mode Variable}
]#

(defcommand "Clear Variable" ()
  "Clear the value of an editor variable."
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:prompt "Variable: "
			:help "The name of a variable to clear.")
    (declare (ignore name))
    (setf (variable-value var) ())))

(defcommand "Set Variable" ()
  "Set the value of an editor variable to the result of the evaluation of a
   prompted expression."
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:prompt "Variable: "
			:help "The name of a variable to set.")
    (declare (ignore name))
    (setf (variable-value var)
	  (handle-lisp-errors
	   (eval (prompt-for-expression
		  :prompt "Value: "
		  :help "Expression to evaluate for new value."))))))

;; FIX Defevar does this?
(defcommand "Set Buffer Variable" ()
  "Set the value of an editor variable in the current buffer to the result
   of the evaluation of a prompted expression."
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:prompt "Buffer Variable: "
			:help "The name of a variable to set.")
    (if (editor-bound-p var :buffer (current-buffer))
	(setf (variable-value var :buffer (current-buffer))
	      (handle-lisp-errors
	       (eval (prompt-for-expression
		      :prompt "Value: "
		      :help "Expression to evaluate for new value."))))
	(defevar name
	  (documentation var 'function)
	  :buffer (current-buffer)
	  :value (handle-lisp-errors
		  (eval (prompt-for-expression
			 :prompt "Value: "
			 :help "Expression to evaluate for new value.")))))))

(defcommand "Set Mode Variable" ()
  "Set the value of an editor variable in the current major mode to the
   result of the evaluation of a prompted expression."
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:prompt "Mode Variable: "
			:help "The name of a variable to set.")
    (let ((mode (buffer-major-mode (current-buffer))))
      (if (editor-bound-p var :mode mode)
	(setf (variable-value var :mode mode)
	      (handle-lisp-errors
	       (eval (prompt-for-expression
		      :prompt "Value: "
		      :help "Expression to evaluate for new value."))))
	(defevar name
	  (documentation var 'function)
	  :mode mode
	  :value (handle-lisp-errors
		  (eval (prompt-for-expression
			 :prompt "Value: "
			 :help "Expression to evaluate for new value."))))))))

(defcommand "List Buffer Variables" ()
  "List all the buffer variables of the current buffer."
  (let ((buffer (current-buffer)))
    (with-pop-up-display (stream)
      (do-strings (string value (buffer-variables buffer))
	(format stream "~A: ~A~%"
		string
		(variable-value value :buffer buffer))))))

(defcommand "List Mode Variables" ()
  "List all the mode variables of the current major mode."
  (let ((mode (buffer-major-mode (current-buffer))))
    (with-pop-up-display (stream)
      (do-strings (string value (mode-variables mode))
	(format stream "~A: ~A~%"
		string
		(variable-value value :mode mode))))))

(defcommand "Defevar" ()
  "Define an editor variable with a value in some location (mode, buffer,
   globally).  The variable can be a new variable.  If the named variable
   exists currently, its documentation and hooks are propagated to the new
   instance."
  (let* ((name (nstring-capitalize (prompt-for-variable :must-exist nil)))
	 (var (string-to-variable name))
	 (doc (if (editor-bound-p var)
		  (variable-documentation var)
		  ""))
	 (hooks (if (editor-bound-p var) (variable-hooks var)))
	 (val (prompt-for-expression :prompt "Variable value: "
				     :help "Value for the variable.")))
    (multiple-value-bind
	(kind where)
	(prompt-for-place
	 "Kind of binding: "
	 "Whether the variable is global, mode, or buffer specific.")
      (case kind
	(:global
	 (defevar name doc :value val :hooks hooks))
; FIX orig (results in note)
; 	(t
; 	 (defevar name doc type where :value val :hooks hooks))
	(:mode
	 (defevar name doc :mode where :value val :hooks hooks))
	(:buffer
	 (defevar name doc :buffer where :value val :hooks hooks))))))


;; FIX Identity? T? Return Immediately?
(defcommand "Continue" ()
  "Continue processing, as before.")

(defcommand "List Buffers" (p)
  "Pop up a list of all existing buffers.  Display a * before the name of
   each modified buffer.  If buffer is associated with a file print the
   file name, type, device, directory and buffer name, otherwise print the
   buffer name followed by the number of lines in the buffer.

   If the buffer name matches the associated file name then just print the
   file name.

   When given a prefix argument, list only the modified buffers."
  (with-pop-up-display (s)
    (do-strings (n b *buffer-names*)
      (declare (simple-string n))
      (unless (or (eq b *echo-area-buffer*)
		  (assoc b *random-typeout-buffers* :test #'eq))
	(let ((modified (buffer-modified b))
	      (buffer-pathname (buffer-pathname b)))
	  (when (or (not p) modified)
	    (write-char (if modified #\* #\space) s)
	    (if (and buffer-pathname (file-namestring buffer-pathname))
		(format s "~A  ~25T~A~:[~68T~A~;~]~%"
			(file-namestring buffer-pathname)
			(directory-namestring buffer-pathname)
			(string= (pathname-to-buffer-name buffer-pathname) n)
			n)
		(format s "~A~68T~D Line~:P~%"
			n (count-lines (buffer-region b))))))))))

(defcommand "Select Random Typeout Buffer" ()
  "Make the most recently used random typeout buffer the current buffer in
   the current window."
  "Select last random typeout buffer."
  (if *random-typeout-buffers*
      (change-to-buffer (caar *random-typeout-buffers*))
      (editor-error "There are no random typeout buffers.")))

(defcommand "Room" (p)
  "Display stats on allocated storage.  With a positive prefix print the
   maximum amount of information, with a negative prefix print the
   minimum."
  "Run Room into a With-Random-Typeout window.  If P is positive pass room
   T, if P is negative pass (), else pass :default."
  (with-pop-up-display (*standard-output*)
    (room (if p (> p 0) :default))))

(defun gc-message-before (bytes-in-use)
  (message "~&[GC threshold exceeded with ~:D bytes in use.  ~
	    Commencing GC.]~%" bytes-in-use))

(defun gc-message-after (bytes-retained bytes-freed new-trigger)
  (message "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%"
	   bytes-retained bytes-freed)
  (message "[GC will next occur when at least ~:D bytes are in use.]~%"
	   new-trigger))

(defcommand "Garbage Collect" (p)
  "Call Collect Garbage"
  (collect-garbage-command p))
(defcommand "GC" (p)
  "Call Collect Garbage"
  (collect-garbage-command p))

#+gencgc
(defcommand "Collect Garbage" (p)
  "Initiate a garbage collection.  *gc-verbose* determines verbosity.  With
   a prefix collect prefix many generations, else collect all generations."
  (let ((*gc-notify-before* #'gc-message-before)
	(*gc-notify-after* #'gc-message-after))
    (if p (gc :gen p) (gc :full t))))
#-gencgc
(defcommand "Collect Garbage" (p)
  "Initiate a garbage collection.  *gc-verbose* determines verbosity."
  (declaim (ignore p))
  (gc))

(defcommand "List Fonts" ()
  "Pop-up an list of examples of each font."
  (let* ((buf-name "Font List")
	 (new-buffer (make-buffer buf-name
				  :modes '("Fundamental" "View")))
	 (buffer (or new-buffer (getstring buf-name *buffer-names*))))
    (change-to-buffer buffer)
    ;; FIX pick new buffer if exists and contains something else
    ;;     or perhaps confirm to overwrite (possibly saving first)
    (if new-buffer
	(let ((mark (current-point)))
	  (with-writable-buffer (buffer)
	    (dotimes (font edi::font-map-size)
	      (insert-string mark (format nil "~2D " font))
	      (font-mark (mark-line mark) (mark-charpos mark) font)
	      (insert-string mark (format nil "Font number ~A~%" font))
	      (font-mark (mark-line mark) (mark-charpos mark) 0)))))))

(defcommand "Clear Buffer Fonts" (p (buffer (current-buffer)))
  "Clear any font marks from the current buffer."
  "Clear any font marks from $buffer."
  (declare (ignore p))
  (loop for line = (mark-line (buffer-start-mark buffer))
              then (line-next line)
        while line do
    (setf (getf (line-plist line) 'chi-info) nil)
    (delete-line-font-marks line)))

(defcommand "Clear Line Fonts" (p (line (mark-line (current-point))))
  "Clear any font marks from the current line."
  "Clear any font marks from $line."
  (declare (ignore p))
  (delete-line-font-marks line))

#[ Recursive Edits

Some sophisticated commands, such as `Query Replace', can place you in a
recursive edit.  A recursive edit is simply a recursive invocation of the
editor, done within a command.  A recursive edit is useful because it
allows arbitrary editing to be done during the execution of a command while
maintaining the state of the command.  When the user exits a recursive
edit, the command that entered it proceeds as before.  The editor notes
recursive edits in the `Echo Area' modeline, or status line.  A counter
reflects the number of pending recursive edits.

{command:Exit Recursive Edit}
{command:Abort Recursive Edit}
]#

;;; This is used by the :edit-level modeline field which is defined in
;;; main.lisp.
;;;
(defvar *recursive-edit-count* 0)

(defun do-recursive-edit (&optional (handle-abort t))
  "Does a recursive edit, wrapping []'s around the modeline of the current
   window during its execution.  The current window and buffer are saved
   beforehand and restored afterward.  If they have been deleted by the
   time the edit is done then an editor-error is signalled."
  (let* ((win (current-window))
	 (buf (current-buffer)))
    (unwind-protect
	(let ((*recursive-edit-count* (1+ *recursive-edit-count*)))
	  (update-modeline-field *echo-area-buffer* *echo-area-window*
				 (modeline-field :edit-level))
	  (recursive-edit handle-abort))
      (update-modeline-field *echo-area-buffer* *echo-area-window*
			     (modeline-field :edit-level))
      (or (and (memq win *window-list*) (memq buf *buffer-list*))
	  (editor-error "Old window or buffer has been deleted."))
      (setf (current-window) win)
      (or (eq (window-buffer win) buf)
	  (setf (window-buffer win) buf))
      (setf (current-buffer) buf))))

(defcommand "Exit Recursive Edit" ()
  "Exit a level of recursive edit.  Signal an error when already at the
   top-level edit."
  (or (in-recursive-edit) (editor-error "Already in the top-level edit."))
  (exit-recursive-edit))

(defcommand "Abort Recursive Edit" ()
  "Send and error to the command which invoked the recursive edit.  Signal
   an error when already at the top-level edit."
  (or (in-recursive-edit) (editor-error "Already in the top-level edit."))
  (abort-recursive-edit "Recursive edit aborted."))

;;; TRANSPOSE REGIONS uses CURRENT-REGION to signal an error if the current
;;; region is not active and to get start2 and end2 in proper order.  Delete1,
;;; delete2, and delete3 are necessary since we are possibly ROTATEF'ing the
;;; locals end1/start1, start1/start2, and end1/end2, and we need to know which
;;; marks to dispose of at the end of all this stuff.  When we actually get to
;;; swapping the regions, we must delete both up front if they both are to be
;;; deleted since we don't know what kind of marks are in start1, start2, end1,
;;; and end2, and the marks will be moving around unpredictably as we insert
;;; text at them.  We copy point into ipoint for insertion purposes since one
;;; of our four marks is the point.
;;;
(defcommand "Transpose Regions" ()
  "Transpose (swap) two regions with endpoints defined by the mark stack
   and point.  When invoked immediately following a previous invocation put
   the regions back."
  (or (>= (ring-length (value buffer-mark-ring)) 3)
      (editor-error "Need two marked regions to do Transpose Regions."))
  (let* ((region (current-region))
	 (end2 (region-end region))
	 (start2 (region-start region))
	 (delete1 (pop-buffer-mark))
	 (end1 (pop-buffer-mark))
	 (delete2 end1)
	 (start1 (pop-buffer-mark))
	 (delete3 start1))
    ;;get marks in the right order, to simplify the code that follows
    (or (mark<= start1 end1) (rotatef start1 end1))
    (unless (mark<= start1 start2)
      (rotatef start1 start2)
      (rotatef end1 end2))
    ;;order now guaranteed:  <Buffer Start> start1 end1 start2 end2 <Buffer End>
    (or (mark<= end1 start2)
	(editor-error "Can't transpose overlapping regions."))
    (let* ((adjacent-p (mark= end1 start2))
	   (region1 (delete-and-save-region (region start1 end1)))
	   (region2 (fi adjacent-p
			(delete-and-save-region (region start2 end2))))
	   (point (current-point)))
      (with-mark ((ipoint point :left-inserting))
	(let ((save-end2-loc (push-buffer-mark (copy-mark end2))))
	  (ninsert-region (move-mark ipoint end2) region1)
	  (push-buffer-mark (copy-mark ipoint))
	  (cond (adjacent-p
		 (push-buffer-mark (copy-mark start2))
		 (move-mark point save-end2-loc))
		(t (push-buffer-mark (copy-mark end1))
		   (ninsert-region (move-mark ipoint end1) region2)
		   (move-mark point ipoint))))))
    (delete-mark delete1)
    (delete-mark delete2)
    (delete-mark delete3)))

(defcommand "Goto Absolute Line" (p)
  "Goes to the indicated line, as if counted from the beginning of the
   buffer with the number one.  If a prefix argument is supplied, that is
   the line number; otherwise, the user is prompted."
  "Go to a user perceived line number."
  (let ((p (or p (prompt-for-expression
		  :prompt "Line number: "
		  :help "Enter an absolute line number to goto."))))
    (or (and (integerp p) (plusp p))
	(editor-error "Must supply a positive integer."))
    (let ((point (current-point)))
      (with-mark ((m point))
	(or (line-offset (buffer-start m) (1- p) 0)
	    (editor-error "Too few lines in buffer."))
	(move-mark point m)))))

(defcommand "Goto Absolute Character" (p)
  "Goes to the prompted character, as counted from the beginning of the
   buffer starting with the number one.  With a prefix argument goes
   character number P."
  "Go to a user perceived character number."
  (let ((p (or p (prompt-for-expression
		  :prompt "Character number: "
		  :help "Enter an absolute character number to goto."))))
    (or (and (integerp p) (plusp p))
	(editor-error "Must supply a positive integer."))
    (let ((point (current-point)))
      (with-mark ((m point))
	(or (character-offset (buffer-start m) (1- p))
	    (editor-error "Too few characters in buffer."))
	(move-mark point m)))))


#[ Using The Mouse

It can be convenient to use the mouse to point to positions in
text, especially when moving large distances.  the editor defines several
commands for using the mouse.  These commands can only be used when running
under X Windows (see page pagerefusing-x.)

{command:Here to Top of Window}
{command:Top Line to Here}
{command:Point to Here}
{command:Generic Pointer Up}
{command:Insert Kill Buffer}
]#


;;;; Mouse Commands.

#[ Generic Pointer Up

`Generic Pointer Up' is an editor command bound to mouse up-clicks.  It
invokes a function supplied with the interface described below.  This
command allows various commands to be bound to the same down-click in
various modes with one command bound to the corresponding up-click.

{function:ed:supply-generic-pointer-up-function}
]#

(defcommand "Do Nothing" (p)
  "Do nothing.  With prefix argument, do it that many times."
  "Do nothing p times."
  (dotimes (i (or p 1)))
  (setf (last-command-type) (last-command-type)))

(defun maybe-change-window (window)
  (unless (eq window (current-window))
    (when (or (eq window *echo-area-window*)
	      (eq (current-window) *echo-area-window*)
	      (member window *random-typeout-buffers*
		      :key #'(lambda (cons)
			       (edi::random-typeout-stream-window (cdr cons)))))
      (supply-generic-pointer-up-function #'lisp::do-nothing)
      (editor-error "Attempt to make a special window current."))
    (setf (current-window) window)
    (let ((buffer (window-buffer window)))
      (unless (eq (current-buffer) buffer)
	(setf (current-buffer) buffer)))))

(defcommand "Top Line to Here" ()
  "Move the top line to the line the mouse is on.  If in the first two
   columns then scroll continuously upwards as long as the mouse button is
   pressed."
  "Move the top line to the line the mouse is on."
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (or y (editor-error "Failed to get cursor position."))
    (cond ((< x 2)
	   (loop
	     (when (listen-editor-input *editor-input*) (return))
	     (scroll-window window -1)
	     (redisplay)
	     (editor-finish-output window)))
	  (t
	   (scroll-window window (- y))))))

(defcommand "Here to Top of Window" ()
  "Move the line the mouse is on to the top of the window.  If the mouse is
   in the first two columns then scroll continuously downwards while the
   button is pressed."
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (or y (editor-error "Failed to get cursor position."))
    (cond ((< x 2)
	   (loop
	     (when (listen-editor-input *editor-input*) (return))
	     (scroll-window window 1)
	     (redisplay)
	     (editor-finish-output window)))
	  (t
	   (scroll-window window y)))))

(defvar *generic-pointer-up-fun* ()
  "This is the function for the `Generic Pointer Up' command that defines
   its action.  Other commands set this in preparation for the invocation
   of `Generic Pointer Up'.")
;;;
(defun supply-generic-pointer-up-function (fun)
  "Supply a function that `Generic Pointer Up' invokes the next time it
   executes."
  (check-type fun function)
  (setf *generic-pointer-up-fun* fun))

(defcommand "Generic Pointer Up" ()
  "Invoke the command in *generic-pointer-up-fun*."
  (or *generic-pointer-up-fun*
      (editor-error "*generic-pointer-up-fun* is empty."))
  (funcall *generic-pointer-up-fun*))

(defevar "Point to Here Return Skip"
  "A list of command names that `Point to Here' skips if they are bound to
   Return."
  :value '("New Line"))

(defcommand "Point to Here" ()
  "Move the point to the position of the mouse, moving to a new window if
   necessary.

   If the line at the new position has a primary-click-hook property then
   function call that property.  This is useful for the likes of clickable
   menus.

   When the mouse is in the first four characters of the echo window
   modeline, invoke `Menu'.  When the mouse is anywhere else in the echo
   window invoke `Extended Command'.

   When the mouse is in any other modeline, move the point of the window's
   buffer to the position that is the same percentage, start to end, as the
   horizontal position of the mouse within the modeline.  Also make the
   window current if necessary.

   Supply a function that `Generic Pointer Up' invokes if it runs before
   any intervening generic pointer up predecessors.  If the mouse has moved
   from the position of the current point, then the supplied function
   pushes a buffer mark at point and moves point to the mouse position.
   This allows the caller to mark off a region with the mouse."
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (or x (editor-error "Failed to get cursor position."))
    (if (eq window *echo-area-window*)
	(progn
	  (or (eq window (current-window))
	      (if (or y (> x 4))
		  (extended-command-command)
		  (menu-command)))
	  (supply-generic-pointer-up-function
	   #'continue-command))
	(progn
	  (maybe-change-window window)
	  (if y
	      (let* ((m (or (cursorpos-to-mark x y window)
			    (editor-error
			     "Cursor position out of view.")))
		     (hook (getf (line-plist (mark-line m))
				 'primary-click-hook)))
		(if hook
		    (progn
		      (supply-generic-pointer-up-function
		       #'continue-command)
		      (funcall hook m))
		    (progn
		      (move-mark (current-point) m)
		      (pacify-region)
		      (let ((bind (get-command #k"return" :current)))
			(supply-generic-pointer-up-function
			 (if (commandp bind)
			     (if (member (command-name bind)
					 (value point-to-here-return-skip)
					 :test #'string=)
				 #'point-to-here-up-action
				 (progn
				   (funcall (command-function bind))
				   #'continue-command))
			     #'point-to-here-up-action))))))
	      ;; In the modeline.
	      (let* ((buffer (window-buffer window))
		     (region (buffer-region buffer))
		     (point (buffer-point buffer)))
		(push-buffer-mark (copy-mark point))
		(move-mark point (region-start region))
		(line-offset point (round (* (1- (count-lines region)) x)
					  (1- (window-width window))))
		(supply-generic-pointer-up-function
		 #'continue-command)))))))

(defun point-to-here-up-action ()
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (or x (editor-error "Failed to get cursor position."))
    (when y
      (maybe-change-window window)
      (let ((m (cursorpos-to-mark x y window)))
	(or m (editor-error "Cursor position out of view."))
	(when (eq (line-buffer (mark-line (current-point)))
		  (line-buffer (mark-line m)))
	  (or (mark= m (current-point))
	      (push-buffer-mark (copy-mark (current-point)) t)))
	(move-mark (current-point) m)))))

(defcommand "Insert Kill Buffer" ()
  "Move the point to the mouse location and insert the most recently killed
   text."
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (or x (editor-error "Failed to get cursor position."))
    (maybe-change-window window)
    (if y
	(let ((m (cursorpos-to-mark x y window)))
	  (or m (editor-error "Cursor position out of view."))
	  (move-mark (current-point) m)
	  (yank-command))
	(editor-error "Attempt to insert kill buffer in modeline."))))


#[ Logical Pages

Logical pages are a way of dividing a file into coarse divisions.  This is
analogous to dividing a paper into sections, and the editor provides
primitives for moving between the pages of a file and listing a directory
of the page titles.  Pages are separated by `Page Delimiter' characters (as
in [System Defined Character Attributes]) that appear at the beginning of a
line.

{function:ed:goto-page}
{function:ed:page-offset}
{function:ed:page-directory}
{function:ed:display-page-directory}
]#

(defvar *goto-page-last-num* 0)
(defvar *goto-page-last-string* "")

(defun goto-page (mark i)
  "Move $mark to the absolute page numbered $i, returning $mark.

   If there are less than $i pages, signal an editor-error.

   Number pages starting with one for the page delimited by the beginning
   of the buffer and the first *Page Delimiter* (or the end of the
   buffer)."
  (with-mark ((m mark))
    (buffer-start m)
    (or (page-offset m (1- i))
	(editor-error "No page numbered ~D." i))
    (move-mark mark m)))

(defun display-page-directory (stream directory)
  "Write the list of strings, $directory, to $stream, enumerating them in a
   field three characters wide.  The number and string are separated by two
   spaces, and the first line contains headings for the page numbers and
   title strings."
  (write-line "Page  First Line With Text" stream)
  (do ((dir directory (cdr dir))
       (count 1 (1+ count)))
      ((null dir))
    (declare (fixnum count))
    (format stream "~4D  " count)
    (write-line (car dir) stream)))

(defun page-directory (buffer)
  "Return a list of each first line of size in buffer that follows a *Page
   Delimiter* character that is at the beginning of a line.  This includes
   the first line of the buffer as the first page title.  If a page is
   empty, then its title is the empty string."
  (with-mark ((m (buffer-point buffer)))
    (buffer-start m)
    (let ((end-of-buffer (buffer-end-mark buffer)) result)
      (loop ; Over pages.
	(loop ; For first line with text.
	  (cond ((not (blank-after-p m))
		 (let* ((str (line-string (mark-line m)))
			(len (length str)))
		   (declare (simple-string str))
		   (push (if (and (> len 1)
				  (= (character-attribute :page-delimiter
							  (schar str 0))
				     1))
			     (subseq str 1)
			     str)
			 result))
		 (or (page-offset m 1)
		     (return-from page-directory (nreverse result)))
		 (if (mark= m end-of-buffer)
		     (return-from page-directory (nreverse result)))
		 (return))
		((not (line-offset m 1 0))
		 (return-from page-directory (nreverse result)))
		((= (character-attribute :page-delimiter (next-character m))
		    1)
		 (push "" result)
		 (mark-after m)
		 (return))))))))

(defun page-offset (mark n)
  "Move mark forward $n (-$n backwards, if $n is negative) *Page Delimiter*
   characters that are at the beginning of a line.

   If a *Page Delimiter* is the immediately next character after $mark (or
   before $mark, if $n is negative), then skip it before starting.

   Always move mark.  If there are enough pages to move over, return mark;
   otherwise, returns ()."
  (cond ((plusp n)
	 (find-attribute mark :page-delimiter #'zerop)
	 (dotimes (i n mark)
	   (or (next-character mark) (return nil))
	   (loop
	     (or (find-attribute mark :page-delimiter)
		 (return-from page-offset nil))
	     (or (mark-after mark) (return (if (= i (1- n)) mark)))
	     (when (= (mark-charpos mark) 1) (return)))))
	(t
	 (reverse-find-attribute mark :page-delimiter #'zerop)
	 (prog1
	  (dotimes (i (- n) mark)
	    (or (previous-character mark) (return nil))
	    (loop
	      (or (reverse-find-attribute mark :page-delimiter)
		  (return-from page-offset nil))
	      (mark-before mark)
	      (when (= (mark-charpos mark) 0) (return))))
	  (let ((buffer (line-buffer (mark-line mark))))
	    (if buffer
		(or (mark= mark (buffer-start-mark buffer))
		    (mark-after mark))))))))


#[ Counting Commands

{command:Count Words}
{command:Count Lines}
{command:Count Lines Page}
{command:Count Occurrences}
]#


;;;; Counting some stuff

(defcommand "Count Lines Page" (p)
  "Display number of lines in the current page and the position within the
   current page.  With prefix argument count in the entire buffer."
  (let ((point (current-point)))
    (if p
	(let ((r (buffer-region (current-buffer))))
	  (count-lines-function "Buffer" (region-start r) point (region-end r)))
	(with-mark ((m1 point)
		    (m2 point))
	  (or (and (= (character-attribute :page-delimiter
					   (previous-character m1))
		      1)
		   (= (mark-charpos m1) 1))
	      (page-offset m1 -1))
	  (or (and (= (character-attribute :page-delimiter
					   (next-character m2))
		      1)
		   (= (mark-charpos m2) 0))
	      (page-offset m2 1))
	  (count-lines-function "Page" m1 point m2)))))

(defun count-lines-function (msg start mark end)
  (let ((before (1- (count-lines (region start mark))))
	(after (count-lines (region mark end))))
    (message "~A: ~D lines, ~D/~D" msg (+ before after) before after)))

(defcommand "Count Lines" ()
  "Count the number of lines from the current point to the end of the
   buffer, displaying the result in the echo area.  If the current region
   is active count in that region instead."
  (multiple-value-bind (region activep) (get-count-region)
    (message "~:[After point~;Active region~]: ~A lines"
	     activep (count-lines region))))

(defcommand "Count Words" ()
  "Prints in the Echo Area the number of words in the region between the
   point and the mark, using word-offset.  If the current region is active
   count in theat region instead."
  (multiple-value-bind (region activep) (get-count-region)
    (let ((end-mark (region-end region)))
      (with-mark ((beg-mark (region-start region)))
	(let ((word-count 0))
	  (loop
	    (if (mark>= beg-mark end-mark) (return))
	    (or (word-offset beg-mark 1) (return))
	    (incf word-count))
	  (message "~:[After point~;Active region~]: ~D Word~:P"
		   activep word-count))))))

(defcommand "Count Lines and Characters" ()
  "Print in the Echo Area the number of lines and characters in the current
   region, if it is active, else from point to the buffer end."
  "Print number of lines and characters in the region."
  (multiple-value-bind (region activep) (get-count-region)
    (message "~:[After point~;Active region~]: ~A lines, ~A characters"
	     activep
	     (count-lines region)
	     (count-characters region))))

(defcommand "Describe Cursor Position" ()
  "Print in the Echo Area the number of lines and characters in the current
   region, if it is active, else from point to the buffer end."
  "Print number of lines and characters in the region."
  (let* ((char (next-character (current-point)))
	 (buffer (current-buffer))
	 (point (current-point))
	 (pchars (count-characters (region (buffer-start-mark buffer)
					   point)))
	 (chars (count-characters (buffer-region buffer)))
	 (percent (* (/ pchars chars) 100))
	 (rest (format nil
		       "Point: ~D of ~D (~:[~4F~;~3D~]%); Column: ~D"
		       pchars chars (eq percent 100) percent
		       (mark-column point))))
    (if char
	(let ((code (char-code char)))
	  (message "Char: ~:C (0~O ~D 0x~X); ~A"
		   char code code code rest))
	(message "End of file; ~A" rest))))

;;; GET-COUNT-REGION -- Internal Interface.
;;;
;;; Returns the active region or the region between point and end-of-buffer.
;;; As a second value, it returns whether the region was active.
;;;
;;; Some searching commands use this routine.
;;;
(defun get-count-region ()
  (if (region-active-p)
      (values (current-region) t)
      (values (region (current-point) (buffer-end-mark (current-buffer)))
	      ())))

; FIX
; underground            55.00
; groceries	           56.17
; coventry interview     35.00
(defcommand "Tally Column" ()
  "Print the sum of the column of numbers at point.
   The numbers must line up where the decimal point is or would be."
  (let ((point (current-point)))
    (or (edi::number-at-mark point)(editor-error "Point must be on a number."))
    (message "~A" (tally-column))))

;; 3           .10
;; 3       5000
;; 3          5.        eol
;; 5         34.303
;; 2                 520.39
;;                             6.32
;; 7
(defun tally-column (&optional (point (current-point)))
  "Print the sum of the column of numbers at Point.
   The numbers must line up where the decimal point is or would be."
    (let ((total (edi::number-at-mark point)))
      (when total
	(let ((mark (copy-mark point)) start-col)
	  ;; Move point to before the decimal point.
	  (edi::word-start mark)
	  (if (eq (previous-character mark) #\.)
	      (mark-before mark)
	      (word-offset mark 1))
	  (setq start-col (mark-column mark))
	  ;; Tally downwards.
	  (let ((mark2 (copy-mark mark)))
	    (line-offset mark2 1)
	    (loop for n = (edi::number-at-mark mark2)
	      while (and n (eq start-col (mark-column mark2)))
	      do
	      (incf total n)
	      (line-offset mark2 1))
	    (move-mark mark2 mark)
	    ;; Tally upwards.
	    (line-offset mark2 -1)
	    (loop for n = (edi::number-at-mark mark2)
	      while (and n (eq start-col (mark-column mark2)))
	      do
	      (incf total n)
	      (line-offset mark2 -1)))
	  total))))


#[ Editing Documents

Although the editor is dedicated to editing text, it provides a number of
commands for word processing.  If the editor is used in conjunction with a
text-formatting program, then decent word processing is possible.

{mode:Text}

[ Sentence Commands   ]
[ Paragraph Commands  ]
[ Filling             ]
[ Scribe Mode         ]
[ Hex Mode            ]  Editing binary files.
[ Spelling Correction ]
]#


;;;; Some modes.

(defcommand "Fundamental Mode" ()
  "Put the current buffer into \"Fundamental\" mode.  Fundamental mode is
   the most basic major mode."
  "Put the current buffer into \"Fundamental\" mode."
  (setf (buffer-major-mode (current-buffer)) "Fundamental"))

;;; Text mode.
;;;
(defmode "Text" :major-p t
  :documentation "Word processing for text.")

#[ Caps Lock Mode

Caps Lock is a minor mode in which the editor inserts all alphabetic
characters as uppercase letters.

{mode:Caps Lock}
{command:Self Insert Caps Lock}
]#

;;; Caps Lock mode.
;;;
(defmode "Caps Lock"
  :short-name "CAPS"
  :documentation "Simulate a Caps Lock key.")

(defcommand "Self Insert Caps Lock" (p)
  "Insert the last character typed, or the argument number of them.  If the
   last character was an alphabetic character, then insert its capital
   form."
  (let ((char (char-upcase (ext:key-event-char *last-key-event-typed*))))
    (if (and p (> p 1))
	(insert-string (current-point) (make-string p :initial-element char))
	(insert-character (current-point) char))))

;;; CSV mode.
;;;

(declaim (special *mode-highlighters*))

(defun highlight-csv-line (line chi-info)
  (while* ((string (line-string line))
	   (pos (position #\, string :start 0)
		(position #\, string :start pos)))
	  (pos)
    (chi-mark line pos *original-font* :comment chi-info)
    (incf pos)
    (chi-mark line pos *original-font* :window-foreground chi-info)))

(defun highlight-csv-buffer (buffer)
  (highlight-chi-buffer buffer highlight-csv-line))

(defun highlight-visible-csv-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-csv-line))

(defun setup-csv-mode (buffer)
  (highlight-visible-csv-buffer buffer)
  (pushnew '("CSV" () highlight-visible-csv-buffer)
	   *mode-highlighters*))

(defmode "CSV" :major-p ()
  :documentation "Comma Separated Values mode."
  :setup-function #'setup-csv-mode)


;;;; File type handling.

(defvar *xdg-errors* #("success"
		       "command line syntax error"
		       "failed to find given file"
		       "failed to find required program"
		       "action failed"))

(defcommand "Screenshot" ()
  "Start a screenshot program."
  (or (run-program "gnome-screenshot" ()
		   :wait () :output t)
      (run-program "ksnapshot" ()
		   :wait () :output t)
      (message "Failed to start a screenshot program.")))

(defun %view (arg)
  (message "Opening ~A externally ..." arg)
  (let ((ret (run-program "xdg-open" (list arg)
			  :wait t :output t)))
    (if ret
	(case (process-exit-code ret)
	  (0 (return-from %view t))
	  ((1 2 3 4)
	   (message "xdg-open exited with ~A: ~A"
		    (process-exit-code ret)
		    (aref *xdg-errors* (process-exit-code ret))))
	  (t (message "xdg-open exited with ~A"
		      (process-exit-code ret))))
	(message "Failed to run xdg-open."))
    ()))

(defun view (pathname &optional type subtype params)
  (message "view . ~A ~A ~A" type subtype params)
  (in-directory pathname
    (case= (string-downcase type)
      ("text"
       (case= (string-downcase subtype)
	 ("html"
	  ;(www-command () pathname)
	  (view-url pathname))
	 ("plain" (find-file-command () pathname))
	 ("x-csrc"
	  (find-file-command () pathname)
	  (c-mode-command))
	 (("x-diff" "x-patch")
	  (find-file-command () pathname)
	  (vc-comparison-mode-command))
	 (t (find-file-command () pathname))))
      ("message"
       (case= (string-downcase subtype)
	 ("rfc822"
	  (let* ((buffer (make-unique-buffer
			 (format () "Message ~A" pathname)
			 :modes '("Message")))
		 (stream (make-editor-output-stream
			  (buffer-point buffer) :full)))
	    (defevar "Message Information"
	      "The information about the current message buffer."
	      :value (make-message-info :folder () :msgs ())
	      :buffer buffer)
	    ;; Normally this is cached by mh.
	    (defevar "Message Fields"
	      "The fields of the current message."
	      :value ()
	      :buffer buffer)
	    (setf (buffer-pathname buffer) pathname)
	    (multiple-value-bind
		(ret fields)
		(mh:write-file-message stream pathname
				       (value message-headers))
	      (or ret (error "Failed to write message: ~A" fields))
	      (buffer-start (buffer-point buffer))
	      (setf (buffer-writable buffer) ())
	      (setf (buffer-modified buffer) ())
	      (change-to-buffer buffer)
	      (setv message-fields fields))))))
      (("application")
       (case= (string-downcase subtype)
	 ("octet-stream"
	  (find-file-command () pathname))
	 (t
	  (%view (file-namestring pathname)))))
      (("audio" "image" "multipart" "video" "extension")
       (%view (file-namestring pathname)))
      (t
       (if (file-namestring pathname)
	   (%view (file-namestring pathname))
	   ;; View the directory.
	   (verbose-directory-command () pathname))))))

(defun view-url (url)
  (%view url))


;;;; Beeping.

(defcommand "Beep" (p)
  "Beep.  With a prefix beep that many times."
  (if p (dotimes (i p) (beep)) (beep)))


;;;; Break.

(defcommand "Break" ()
  "Call `break' to break out to the top level."
  (break))


;;;; Colors.

(defcommand "Set Window Foreground Color" ()
  "Set the foreground color of the current window to a prompted value."
  (set-window-foreground-color (current-window) '(1 1 1))
  (redisplay-all))

(defcommand "Set Window Background Color" ()
  "Set the background color of the current window to a prompted value."
  (set-window-background-color (current-window) '(0 0 0))
  (redisplay-all))

(defcommand "Set Window Foreground RGB" ()
  "Set the foreground color of the current window according to prompted
   red, green and blue values."
  (set-window-foreground-color
   (current-window)
   (list (prompt-for-expression :prompt "Red: ")
	 (prompt-for-expression :prompt "Green: ")
	 (prompt-for-expression :prompt "Blue: ")))
  (redisplay-all))

(defcommand "Set Window Background RGB" ()
  "Set the background color of the current window according to prompted
   red, green and blue values."
  (set-window-background-color
   (current-window)
   (list (prompt-for-expression :prompt "Red: ")
	 (prompt-for-expression :prompt "Green: ")
	 (prompt-for-expression :prompt "Blue: ")))
  (redisplay-all))

(defcommand "Set Echo Window Foreground Color" ()
  "Set the foreground color of the current window to a prompted value."
  (set-window-foreground-color *echo-area-window* '(1 1 1))
  (redisplay-all))

(defcommand "Set Echo Window Background Color" ()
  "Set the background color of the current window to a prompted value."
  (set-window-background-color *echo-area-window* '(0 0 0))
  (redisplay-all))

(defcommand "Set Echo Window Foreground RGB" ()
  "Set the foreground color of the current window according to prompted
   red, green and blue values."
  (set-window-foreground-color
   *echo-area-window*
   (list (prompt-for-expression :prompt "Red: ")
	 (prompt-for-expression :prompt "Green: ")
	 (prompt-for-expression :prompt "Blue: ")))
  (redisplay-all))

(defcommand "Set Echo Window Background RGB" ()
  "Set the background color of the current window according to prompted
   red, green and blue values."
  (set-window-background-color
   *echo-area-window*
   (list (prompt-for-expression :prompt "Red: ")
	 (prompt-for-expression :prompt "Green: ")
	 (prompt-for-expression :prompt "Blue: ")))
  (redisplay-all))

(declaim (special *mode-highlighters*))

(defun setup-colorlist-mode (buffer)
  (highlight-visible-colorlist-buffer buffer)
  (pushnew '("Colorlist" t highlight-visible-colorlist-buffer)
	   *mode-highlighters*))

(defmode "Colorlist" :major-p t
  :documentation "Mode for listing colors."
  :setup-function #'setup-colorlist-mode)

(defcommand "List X Colors" ()
  "Pop-up an list of examples of each color."
  (let* ((buf-name "X Color List")
	 (new-buffer (make-buffer buf-name
				  :modes '("Colorlist" "View")))
	 (buffer (or new-buffer (getstring buf-name *buffer-names*))))
    (change-to-buffer buffer)
    ;; FIX pick new buffer if exists and contains something else
    ;;     or perhaps confirm to overwrite (possibly saving first)
    (if new-buffer
	(let ((point (current-point)))
	  (with-writable-buffer (buffer)
	    (read-file "/etc/X11/rgb.txt" point)
	    (buffer-start point)
	    (let ((mark (copy-mark point)))
	      (line-end mark)
	      (mark-after mark)
	      (delete-region (region point mark))))))))

(defun highlight-colorlist-line (line chi-info)
  (with-input-from-string (stream (line-string line))
    (let ((red (read stream ()))
	  (green (read stream ()))
	  (blue (read stream ())))
      (and red green blue
	   (chi-mark line 0 *original-font*
		     (list (/ red 255)
			   (/ green 255)
			   (/ blue 255))
		     chi-info)))))

(defun highlight-colorlist-buffer (buffer)
  (highlight-chi-buffer buffer highlight-colorlist-line))

(defun highlight-visible-colorlist-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-colorlist-line))
