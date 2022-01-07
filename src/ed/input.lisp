;;; The code that handles input to the editor.

(in-package "EDI")

(export '(get-key-event unget-key-event clear-editor-input listen-editor-input
	  *last-key-event-typed* *key-event-history* *editor-input*
	  *real-editor-input* input-waiting last-key-event-cursorpos))
;;;
;;; INPUT-WAITING is exported solely as a hack for the kbdmac definition
;;; mechanism.

#[ Keyboard Input

Keyboard input interacts with a number of other parts of the editor.  Since
the command loop works by reading from the keyboard, keyboard input is the
initial cause of everything that happens.  Also, the editor redisplays in
the low-level input loop when there is no available input from the user.

{variable:ed:*editor-input*}
{variable:ed:*real-editor-input*}
{evariable:Input Hook}
{function:ed:get-key-event}
{function:ed:unget-key-event}
{function:ed:clear-editor-input}
{function:ed:listen-editor-input}
{function:ed:editor-sleep}
{variable:ed:*key-event-history*}
{variable:ed:*last-key-event-typed*}
{variable:ed:*input-transcript*}
]#

;;; These are public variables users hand to the four basic editor input
;;; routines for method dispatching:
;;;    GET-KEY-EVENT
;;;    UNGET-KEY-EVENT
;;;    LISTEN-EDITOR-INPUT
;;;    CLEAR-EDITOR-INPUT
;;;
(defvar *editor-input* ()
  "A structure on which the editor I/O routines operate.  There are
   functions to get input, clear input, return input, and listen for input.
   Input appears as key-events.")

(defvar *real-editor-input* ()
  "The initial value of *editor-input*.  This is useful for reading from
   the original input when *editor-input* is rebound (such as within a
   keyboard macro).")

;; FIX where should this go?
(define-condition editor-top-level-catcher ())


;;;; editor-input structure.

(defstruct (editor-input (:print-function
			  (lambda (s stream d)
			    (declare (ignore s d))
			    (write-string "#<Editor-Input stream>" stream))))
  get          ; A function that returns the next key-event in the queue.
  unget        ; A function that puts a key-event at the front of the queue.
  listen       ; A function that tells whether the queue is empty.
  clear        ; A function that empties the queue.
  ;;
  ;; Queue of events on this stream.  The queue always contains at least one
  ;; one element, which is the key-event most recently read.  If no event has
  ;; been read, the event is a dummy with a nil key-event.
  head
  tail)

;;; These are the elements of the editor-input event queue.
;;;
(defstruct (input-event (:constructor make-input-event ()))
  next		; Next queued event, or NIL if none.
  hunk		; Screen hunk event was read from.
  key-event     ; Key-event read.
  x		; X and Y character position of mouse cursor.
  y
  unread-p)

(defvar *free-input-events* ())

(defun new-event (key-event x y hunk next &optional unread-p)
  (let ((res (if *free-input-events*
		 (shiftf *free-input-events*
			 (input-event-next *free-input-events*))
		 (make-input-event))))
    (setf (input-event-key-event res) key-event)
    (setf (input-event-x res) x)
    (setf (input-event-y res) y)
    (setf (input-event-hunk res) hunk)
    (setf (input-event-next res) next)
    (setf (input-event-unread-p res) unread-p)
    res))

;;; This is a public variable.
;;;
(defvar *last-key-event-typed* ()
  "The last key-event that the command interpreter read.

   This variable is also maintained within keyboard macros allowing
   commands to behave the same on each repetition as they did in the
   recording invocation.")

;;; This is a public variable.  SITE-INIT initializes this.
;;;
(defvar *key-event-history* ()
  "A ring buffer ([Rings]) that holds the last 60 key-events read from the
   keyboard.")

(proclaim '(special *input-transcript*))

;;; DQ-EVENT is used in editor stream methods for popping off input.
;;; If there is an event not yet read in Stream, then pop the queue
;;; and return the character.  If there is none, return NIL.
;;;
(defun dq-event (stream)
  (block-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head)))
     (if next
	 (let ((key-event (input-event-key-event next)))
	   (setf (editor-input-head stream) next)
	   (shiftf (input-event-next head) *free-input-events* head)
	   (ring-push key-event *key-event-history*)
	   (setf *last-key-event-typed* key-event)
	   (when *input-transcript*
	     (vector-push-extend key-event *input-transcript*))
	   key-event)))))

;;; Q-EVENT is used in low level input fetching routines to add input to the
;;; editor stream.
;;;
(defun q-event (stream key-event &optional x y hunk)
  (block-interrupts
   (let ((new (new-event key-event x y hunk nil))
	 (tail (editor-input-tail stream)))
     (setf (input-event-next tail) new)
     (setf (editor-input-tail stream) new))))

(defun un-event (key-event stream)
  (block-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head))
	  (new (new-event key-event (input-event-x head) (input-event-y head)
			  (input-event-hunk head) next t)))
     (setf (input-event-next head) new)
     (unless next (setf (editor-input-tail stream) new)))))


;;;; Keyboard macro hacks.

(defvar *input-transcript* ()
  "If this variable is true then it should contain an adjustable vector
   with a fill pointer into which all keyboard input will be pushed.")

;;; INPUT-WAITING  --  Internal
;;;
;;; An Evil hack that tells us whether there is an unread key-event on
;;; *editor-input*.  Note that this is applied to the real *editor-input*
;;; rather than to a kbdmac stream.
;;;
(defun input-waiting ()
  "Return true if there is a key-event which has been unread-key-event'ed
   on *editor-input*.  Used by the keyboard macro stuff."
  (let ((next (input-event-next
	       (editor-input-head *real-editor-input*))))
    (and next (input-event-unread-p next))))


;;;; Input method macro.

(defvar *in-editor-stream-input-method* nil
  "This keeps us from undefined nasties like re-entering editor stream
   input methods from input hooks and scheduled events.")

(proclaim '(special *screen-image-trashed*))

;;; These are the characters GET-KEY-EVENT notices when it pays attention
;;; to aborting input.  This happens via EDITOR-INPUT-METHOD-MACRO.
;;;
(defparameter editor-abort-key-events (list #k"Control-g" #k"Control-G"))

(defmacro abort-key-event-p (key-event)
  `(member ,key-event editor-abort-key-events))

;;; EDITOR-INPUT-METHOD-MACRO  --  Internal.
;;;
;;; WINDOWED-GET-KEY-EVENT and TTY-GET-KEY-EVENT use this.  Somewhat odd
;;; stuff goes on here because this is the place where the editor waits, so
;;; this is where we redisplay, check the time for scheduled events, etc.
;;; In the loop, we call the input hook when we get a character and leave
;;; the loop.  If there isn't any input, invoke any scheduled events whose
;;; time is up.  Unless SERVE-EVENT returns immediately and did something,
;;; (serve-event 0), call redisplay, note that we are going into a read
;;; wait, and call SERVE-EVENT with a wait or infinite timeout.  Upon
;;; exiting the loop, turn off the read wait note and check for the abort
;;; character.  Return the key-event we got.  We bind an error condition
;;; handler here because the default editor error handler goes into a
;;; little debugging prompt loop, but if we got an error in getting input,
;;; we should prompt the user using the input method (recursively even).
;;;
(eval-when (compile eval)
(defmacro editor-input-method-macro ()
  `(handler-bind ((error #'(lambda (condition)
			     (let ((device (device-hunk-device
					    (window-hunk (current-window)))))
			       (funcall (device-exit device) device))
			     (invoke-debugger condition))))
;     (when *in-editor-stream-input-method*
;       (error "Entering editor stream input method recursively."))
     (let ((*in-editor-stream-input-method* t)
	   (nrw-fun (device-note-read-wait
		     (device-hunk-device (window-hunk (current-window)))))
	   key-event)
       (loop
	 (when (setf key-event (dq-event stream))
	   (dolist (f (variable-value 'ed::input-hook)) (funcall f))
	   (return))

	 (invoke-scheduled-events)
	 (or (system:serve-event 0)
	     (internal-redisplay)
	     (progn
	       (if nrw-fun (funcall nrw-fun t))
	       (let ((wait (next-scheduled-event-wait)))
		 (if wait
		     (system:serve-event wait)
		     (system:serve-event))))))
       (if nrw-fun (funcall nrw-fun ()))
       (if (abort-key-event-p key-event)
	   ;; ignore-abort-attempts-p must exist outside the macro.
	   ;; in this case it is bound in GET-KEY-EVENT.
	   (or ignore-abort-attempts-p
	       (progn
		 (beep)
		 (signal 'editor-top-level-catcher ()))))
       key-event)))
) ;eval-when


;;;; Editor input from windowing system.
#+clx
(defstruct (windowed-editor-input
	    (:include editor-input
		      (:get #'windowed-get-key-event)
		      (:unget #'windowed-unget-key-event)
		      (:listen #'windowed-listen)
		      (:clear #'windowed-clear-input))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (write-string "#<Editor-Window-Input stream>" stream)))
	    (:constructor make-windowed-editor-input
			  (&optional (head (make-input-event)) (tail head))))
  hunks)      ; List of bitmap-hunks which input to this stream.

#+clx
;;; It's actually the same as the TTY case...
(defun windowed-get-key-event (stream ignore-abort-attempts-p)
  (tty-get-key-event stream ignore-abort-attempts-p))

#+clx
(defun windowed-unget-key-event (key-event stream)
  (un-event key-event stream))

#+clx
(defun windowed-clear-input (stream)
  (loop (or (system:serve-event 0) (return)))
  (block-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
	       *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

#+clx
(defun windowed-listen (stream)
  (loop
    ;; Don't service anymore events if we just got some input.
    (when (input-event-next (editor-input-head stream))
      (return t))
    ;;
    ;; If nothing is pending, check the queued input.
    (or (system:serve-event 0)
	(return (not (null (input-event-next (editor-input-head stream))))))))


;;;; Editor input from a tty.

(defstruct (tty-editor-input
	    (:include editor-input
		      (:get #'tty-get-key-event)
		      (:unget #'tty-unget-key-event)
		      (:listen #'tty-listen)
		      (:clear #'tty-clear-input))
	    (:print-function
	     (lambda (obj stream n)
	       (declare (ignore obj n))
	       (write-string "#<Editor-Tty-Input stream>" stream)))
	    (:constructor make-tty-editor-input
			  (fd &optional (head (make-input-event)) (tail head))))
  fd)

(defun tty-get-key-event (stream ignore-abort-attempts-p)
  (editor-input-method-macro))

(defun tty-unget-key-event (key-event stream)
  (un-event key-event stream))

(defun tty-clear-input (stream)
  (block-interrupts
   (let* ((head (editor-input-head stream))
	  (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
	       *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

;;; Note that we never return NIL as long as there are events to be served with
;;; SERVE-EVENT.  Thus non-keyboard input (i.e. process output)
;;; effectively causes LISTEN to block until either all the non-keyboard input
;;; has happened, or there is some real keyboard input.
;;;
(defun tty-listen (stream)
  (loop
    ;; Don't service anymore events if we just got some input.
    (when (or (input-event-next (editor-input-head stream))
	      (editor-tty-listen stream))
      (return t))
    ;; If nothing is pending, check the queued input.
    (fi (system:serve-event 0)
	(return (not (null (input-event-next (editor-input-head stream))))))))


;;;; GET-KEY-EVENT, UNGET-KEY-EVENT, LISTEN-EDITOR-INPUT, CLEAR-EDITOR-INPUT.

(defvar *busy* ()
  "t during command processing.  For :busy status line field.")

;;; GET-KEY-EVENT -- Public.
;;;
(defun get-key-event (editor-input &optional ignore-abort-attempts-p)
  "Return a key-event as soon as it is available on $editor-input.

   $editor-input is either *editor-input* or *real-editor-input*.

   $ignore-abort-attempts-p indicates whether the \"Interrupt\" interrupt
   (sigint, usually control-g and control-G) throws to the top-level editor
   command loop.  When this is true, return the control-g/G key-events if
   they are input, otherwise, terminate from the editor's current state,
   returning to the command loop.  When performing this termination invoke
   the functions in *Abort Hook* and skip those in *Input Hook*."
  (prog1
      (let (*busy*)
	(if (buffer-modeline-field-p *echo-area-buffer* :busy)
	    (update-modeline-field *echo-area-buffer* *echo-area-window*
				   (modeline-field :busy)))
	(if (buffer-modeline-field-p *echo-area-buffer* :busy-or-menu)
	    (update-modeline-field *echo-area-buffer* *echo-area-window*
				   (modeline-field :busy-or-menu)))
	(internal-redisplay)
	(funcall (editor-input-get editor-input)
		 editor-input ignore-abort-attempts-p))
    (if (buffer-modeline-field-p *echo-area-buffer* :busy)
	(update-modeline-field *echo-area-buffer* *echo-area-window*
			       (modeline-field :busy)))
    (if (buffer-modeline-field-p *echo-area-buffer* :busy-or-menu)
	(update-modeline-field *echo-area-buffer* *echo-area-window*
			       (modeline-field :busy-or-menu)))
    (internal-redisplay)))

;;; UNGET-KEY-EVENT -- Public.
;;;
(defun unget-key-event (key-event editor-input)
  "Return $key-event to $editor-input, so the next invocation of
   `get-key-event' will return $key-event.

   If $key-event is #k\"control-g\" or #k\"control-G\", then whether
   `get-key-event' returns it depends on that function's second argument.

   $editor-input is either *editor-input* or *real-editor-input*."
  (funcall (editor-input-unget editor-input) key-event editor-input))

;;; CLEAR-EDITOR-INPUT -- Public.
;;;
(defun clear-editor-input (editor-input)
  "Flush any pending input on $editor-input.  $editor-input is either
   *editor-input* or *real-editor-input*."
  (funcall (editor-input-clear editor-input) editor-input))

;;; LISTEN-EDITOR-INPUT -- Public.
;;;
(defun listen-editor-input (editor-input)
  "Return whether there is any input available on $editor-input.
   $editor-input is either *editor-input* or *real-editor-input*."
  (funcall (editor-input-listen editor-input) editor-input))


;;;; LAST-KEY-EVENT-CURSORPOS and WINDOW-INPUT-HANDLER.

;;; LAST-KEY-EVENT-CURSORPOS  --  Public
;;;
;;; Just look up the saved info in the last read key event.
;;;
(defun last-key-event-cursorpos ()
  "Return as multiple values, the (X, Y) character position and window
   where the last key event happened, if possible, else return ().  If in
   the modeline, return a Y position of () and the correct X and window.
   Return () for terminal input."
  (let* ((ev (editor-input-head *real-editor-input*))
	 (hunk (input-event-hunk ev))
	 (window (and hunk (device-hunk-window hunk))))
    (when window
      (values (input-event-x ev) (input-event-y ev) window))))

;;; WINDOW-INPUT-HANDLER  --  Internal
;;;
;;; This is the input-handler function for hunks that implement windows.  It
;;; just queues the events on *real-editor-input*.
;;;
(defun window-input-handler (hunk char x y)
  (q-event *real-editor-input* char x y hunk))


;;;; Random typeout input routines.

(declaim (special *random-typeout-switch*))

(defun wait-for-more (stream)
  (let ((key-event (more-read-key-event)))
    (cond ((logical-key-event-p key-event :yes))
	  ((or (logical-key-event-p key-event :do-all)
	       (logical-key-event-p key-event :exit))
	   (setf (random-typeout-stream-no-prompt stream) t)
	   (random-typeout-cleanup stream))
	  ((logical-key-event-p key-event :keep)
	   (setf (random-typeout-stream-no-prompt stream) t)
	   (maybe-keep-random-typeout-window stream)
	   (random-typeout-cleanup stream))
	  ((logical-key-event-p key-event :no)
	   (random-typeout-cleanup stream)
	   (throw 'more-punt nil))
	  ((logical-key-event-p key-event :switch-to-reference)
	   (random-typeout-cleanup stream)
	   (setq *random-typeout-switch* t)
	   (throw 'more-punt nil))
	  (t
	   (unget-key-event key-event *editor-input*)
	   (random-typeout-cleanup stream)
	   (throw 'more-punt nil)))))

(proclaim '(special *more-prompt-action*))

(defun maybe-keep-random-typeout-window (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (buffer (window-buffer window))
	 (start (buffer-start-mark buffer)))
    (when (typep (edi::device-hunk-device (edi::window-hunk window))
		 'edi::bitmap-device)
      (let ((*more-prompt-action* :normal))
	(update-modeline-field buffer window :more-prompt)
	(random-typeout-redisplay window))
      (buffer-start (buffer-point buffer))
      (let* ((xwindow #+clx (make-xwindow-like-window window)
		      #-clx nil)
	     (window (make-window start :window xwindow)))
	(unless window
	  #+clx(xlib:destroy-window xwindow)
	  (editor-error "Could not create random typeout window."))))))

(defun end-random-typeout (stream &optional reference)
  (let ((*more-prompt-action* :flush)
	(window (random-typeout-stream-window stream)))
    (update-modeline-field (window-buffer window) window :more-prompt)
    (random-typeout-redisplay window))
  (unless (random-typeout-stream-no-prompt stream)
    (let* ((key-event (more-read-key-event))
	   (keep-p (logical-key-event-p key-event :keep)))
      (when keep-p (maybe-keep-random-typeout-window stream))
      (random-typeout-cleanup stream)
      (if (and reference
	       (logical-key-event-p key-event :switch-to-reference))
	  (if (integerp (cadr reference))
	      (progn
		(ed::change-to-buffer (ed::find-file-buffer (car reference)))
		(ed::goto-absolute-line-command (cadr reference)))
	      (apply 'ed::go-to-definition reference))
	  (unless (or (logical-key-event-p key-event :do-all)
		      (logical-key-event-p key-event :exit)
		      (logical-key-event-p key-event :no)
		      (logical-key-event-p key-event :yes)
		      keep-p)
	    (unget-key-event key-event *editor-input*))))))

;;; MORE-READ-KEY-EVENT -- Internal.
;;;
;;; This gets some input from the type of stream bound to *editor-input*.  Need
;;; to loop over SERVE-EVENT since it returns on any kind of event (not
;;; necessarily a key or button event).
;;;
;;; Currently this does not work for keyboard macro streams!
;;;
(defun more-read-key-event ()
  (clear-editor-input *editor-input*)
  (let ((key-event (loop
		     (let ((key-event (dq-event *editor-input*)))
		       (when key-event (return key-event))
		       (system:serve-event)))))
    (when (abort-key-event-p key-event)
      (beep)
      (signal 'editor-top-level-catcher ()))
    key-event))
