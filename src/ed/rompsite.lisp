;;; "Site dependent" stuff for the editor.        -*- Package: EDI -*-

;; FIX site-posix.lisp?

(in-package "SYSTEM")
(export '(%sp-byte-blt %sp-find-character
	  %sp-find-character-with-attribute
	  %sp-reverse-find-character-with-attribute))

(in-package "EDI")

(export '(show-mark editor-beep editor-sleep
	  *input-transcript* fun-defined-from-pathname
	  editor-describe-function pause
	  store-cut-region store-cut-string
	  store-selection-region store-selection-string store-clipboard
	  fetch-cut-string fetch-selection fetch-clipboard
	  schedule-event remove-scheduled-event
	  enter-window-autoraise
	  windowed-monitor-p
	  ;;
	  ;; Export default-font to prevent a name conflict that occurs due
	  ;; to the editor variable "Default Font" defined in SITE-INIT
	  ;; below.
	  ;;
	  default-font))



(declaim (special *with-screen-hooks* *caller-standar-input*))
;; FIX put in site-init below?
(push (cons #'(lambda ()
		(when *in-the-editor*
		  (or debug::*in-the-debugger*
		      (let ((device (device-hunk-device
				     (window-hunk (current-window)))))
			(funcall (device-exit device) device)))))
	    #'(lambda ()
		(when *in-the-editor*
		  (or debug::*in-the-debugger*
		      (let ((device (device-hunk-device
				     (window-hunk (current-window)))))
			(funcall (device-init device) device))))))
      system:*with-screen-hooks*)


;;;; I/O specials and initialization

;;; File descriptor for the terminal.
;;;
(defvar *editor-file-descriptor*)

;;; This is a hack, so screen can tell how to initialize screen management
;;; without re-opening the display.  It is set in INIT-RAW-IO and referenced
;;; in WINDOWED-MONITOR-P.
;;;
(defvar *editor-windowed-input* nil)

;;; These are used for selecting X events.
#+clx
(eval-when (compile load eval)
  (defconstant group-interesting-xevents
    '(:structure-notify :key-press)))
#+clx
(defconstant group-interesting-xevents-mask
  (apply #'xlib:make-event-mask group-interesting-xevents))

#+clx
(eval-when (compile load eval)
  (makunbound 'child-interesting-xevents) ; FIX for - :keymap-notify
  (defconstant child-interesting-xevents
    '(:key-press :button-press :button-release :structure-notify :exposure
		 :enter-window :leave-window :focus-in)))
#+clx
(defconstant child-interesting-xevents-mask
  (apply #'xlib:make-event-mask child-interesting-xevents))

#+clx
(eval-when (compile load eval)
  (defconstant random-typeout-xevents
    '(:key-press :button-press :button-release :enter-window :leave-window
		 :exposure)))
#+clx
(defconstant random-typeout-xevents-mask
  (apply #'xlib:make-event-mask random-typeout-xevents))

#+clx
(proclaim '(special ed::*open-paren-highlight-font*
		    ed::*active-region-highlight-font*))

#+clx
(defparameter lisp-fonts-pathnames '("library:fonts/"))

(proclaim '(special *editor-input* *real-editor-input*))

(proclaim '(special *editor-input* *real-editor-input*))

;;; INIT-RAW-IO  --  Internal
;;;
;;; This function should be called whenever the editor is entered in a new
;;; lisp.  It sets up process specific data structures.
;;;
(defun init-raw-io (display xoff)
  #-clx (declare (ignore display xoff))
  (setf *editor-windowed-input* ())
  (cond #+clx
	((and display (fi xoff))
	 (setf *editor-windowed-input* (ext:open-clx-display display))
	 (setf *editor-input* (make-windowed-editor-input))
	 (setup-font-family *editor-windowed-input*)
	 (add-hook ed::kill-ring-hook #'clipboard-on)
	 (add-hook ed::activate-region-hook #'selection-on))
	(t ;; The editor's file descriptor is Unix standard input (0).
	   ;; We don't need to affect system:*file-input-handlers* here
	   ;; because the init and exit methods for tty redisplay devices
	   ;; take care of this.
	   ;;
	 (setf *editor-file-descriptor* 0)
	 (setf *editor-input* (make-tty-editor-input 0))))
  (setf *real-editor-input* *editor-input*)
  *editor-windowed-input*)

;;; Stop flaming from compiler due to CLX macros expanding into illegal
;;; declarations.
;;;
(proclaim '(declaration values))
(proclaim '(special *default-font-family*))

;;; font-map-size should be defined in font.lisp, but SETUP-FONT-FAMILY would
;;; assume it to be special, issuing a nasty warning.
;;;
#+clx
(defconstant font-map-size 16
  "The number of possible fonts in a font-map.")
#-clx
(defconstant font-map-size 20)

;;; SETUP-FONT-FAMILY sets *default-font-family*, opening the three font names
;;; passed in.  The font family structure is filled in from the first argument.
;;; Actually, this ignores default-highlight-font and default-open-paren-font
;;; in lieu of "Active Region Highlighting Font" and "Open Paren Highlighting
;;; Font" when these are defined.
;;;
#+clx
(defun setup-font-family (display)
  (let* ((font-family (make-font-family :map (make-array font-map-size
							 :initial-element 0)
					:cursor-x-offset 0
					:cursor-y-offset 0))
	 (font-family-map (font-family-map font-family)))
    (declare (simple-vector font-family-map))
    (setf *default-font-family* font-family)
    (let ((font (xlib:open-font display (variable-value 'ed::default-font))))
      (or font
	  (error "Failed to open font -- ~S" (variable-value 'ed::default-font)))
      (fill font-family-map font)
      (let ((width (xlib:max-char-width font)))
	(setf (font-family-width font-family) width)
	(setf (font-family-cursor-width font-family) width))
      (let* ((baseline (xlib:font-ascent font))
	     (height (+ baseline (xlib:font-descent font))))
	(setf (font-family-height font-family) height)
	(setf (font-family-cursor-height font-family) height)
	(setf (font-family-baseline font-family) baseline)))
    (setup-one-font display
		    (variable-value 'ed::open-paren-highlighting-font)
		    font-family-map
		    ed::*open-paren-highlight-font*)
    (setup-one-font display
		    (variable-value 'ed::active-region-highlighting-font)
		    font-family-map
		    ed::*active-region-highlight-font*)))

;;; SETUP-ONE-FONT tries to open font-name for display, storing the result
;;; in font-family-map at index.  `xlib:open-font' will return font stuff
;;; regardless if the request is valid or not, so we finish the output to
;;; get synch'ed with the server which will cause any errors to get
;;; signaled.  At this level, we want to deal with this error here
;;; returning nil if the font couldn't be opened.
;;;
#+clx
(defun setup-one-font (display font-name font-family-map index)
  (handler-case (let ((font (xlib:open-font display (namestring font-name))))
		  (xlib:display-finish-output display)
		  (setf (svref font-family-map index) font))
    (xlib:name-error ()
     (warn "Failed to open font -- ~S" font-name)
     nil)))


;;;; EDITOR-BEEP.

#[ Beeping

{function:ed:editor-beep}
{evariable:Bell Style}
{evariable:Beep Border Width}
]#

(defvar *editor-bell* (make-string 1 :initial-element #\bell))

;;; TTY-BEEP is used in the editor for beeping when running under a
;;; terminal.  Send a #\bell to unix standard output.
;;;
(defun tty-beep (&optional device stream)
  (declare (ignore device stream))
  (when (variable-value 'ed::bell-style)
    ;; FIX add bell styles as in bitmap-beep
    (unix:unix-write 1 *editor-bell* 0 1)))

(proclaim '(special *current-window*))

;;; BITMAP-BEEP is used in the editor for beeping when running under
;;; windowed input.
;;;
#+clx
(defun bitmap-beep (device stream)
  (declare (ignore stream))
  (let ((display (bitmap-device-display device)))
    (ecase (variable-value 'ed::bell-style)
      (:border-flash
       (flash-window-border *current-window*))
      (:feep
       (xlib:bell display)
       (xlib:display-force-output display))
      (:border-flash-and-feep
       (xlib:bell display)
       (xlib:display-force-output display)
       (flash-window-border *current-window*))
      (:flash
       (flash-window *current-window*))
      (:flash-and-feep
       (xlib:bell display)
       (xlib:display-force-output display)
       (flash-window *current-window*))
      ((nil) ;Do nothing.
       ))))

#+clx
(defun flash-window-border (window)
  (let* ((hunk (window-hunk window))
	 (xwin (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (display (bitmap-device-display (device-hunk-device hunk)))
	 (border (variable-value 'ed::beep-border-width))
	 (h (or (bitmap-hunk-modeline-pos hunk) (bitmap-hunk-height hunk)))
	 (top-border (min (ash h -1) border))
	 (w (bitmap-hunk-width hunk))
	 (side-border (min (ash w -1) border))
	 (top-width (max 0 (- w (ash side-border 1))))
	 (right-x (- w side-border))
	 (bottom-y (- h top-border)))
    (xlib:with-gcontext (gcontext :function xlib:boole-invert)
      (flet ((zot ()
	       (xlib:draw-rectangle xwin gcontext 0 0 side-border h t)
	       (xlib:draw-rectangle xwin gcontext side-border bottom-y
				    top-width top-border t)
	       (xlib:draw-rectangle xwin gcontext right-x 0 side-border h t)
	       (xlib:draw-rectangle xwin gcontext side-border 0
				    top-width top-border t)))
	(zot)
	(xlib:display-force-output display)
	(sleep 0.1)
	(zot)
	(xlib:display-force-output display)))))

#+clx
(defun flash-window (window)
  (let* ((hunk (window-hunk window))
	 (xwin (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (display (bitmap-device-display (device-hunk-device hunk)))
	 (width (bitmap-hunk-width hunk))
	 (height (or (bitmap-hunk-modeline-pos hunk)
		     (bitmap-hunk-height hunk))))
    (xlib:with-gcontext (gcontext :function xlib:boole-invert)
      (xlib:draw-rectangle xwin gcontext 0 0 width height t)
      (xlib:display-force-output display)
      (sleep 0.1)
      (xlib:draw-rectangle xwin gcontext 0 0 width height t)
      (xlib:display-force-output display))))

(defun editor-beep (stream)
  "Using the current window, call the device's beep function on $stream.

   system:*beep-function* is bound to this function to beep the device.
   The implementation of the function depends on the device."
  (let ((device (device-hunk-device (window-hunk (current-window)))))
    (funcall (device-beep device) device stream)))


;;;; GC messages.

;;; EDITOR-GC-NOTIFY-BEFORE and EDITOR-GC-NOTIFY-AFTER both MESSAGE GC
;;; notifications when the editor is not running under X11.  It cannot
;;; affect its window's without using its display connection.  Since GC can
;;; occur inside CLX request functions, using the same display confuses
;;; CLX.
;;;

(defun editor-gc-notify-before (bytes-in-use)
  (if (value ed::notify-gc)
      (let ((control "~%[GC threshold exceeded with ~:D bytes in use.  ~
		      Commencing GC.]~%"))
	(cond ((not edi::*editor-windowed-input*)
	       (beep)
	       (message control bytes-in-use))
	      (t
	       ;; Can't call BEEP since it would use the editor's display
	       ;; connection.
	       (lisp::default-beep-function *standard-output*)
	       (format t control bytes-in-use)
	       (finish-output))))))

(defun editor-gc-notify-after (bytes-retained bytes-freed trigger)
  (if (value ed::notify-gc)
      (let ((control
	     "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%~
	      [GC will next occur when at least ~:D bytes are in use.]~%"))
	(cond ((not edi::*editor-windowed-input*)
	       (beep)
	       (message control bytes-retained bytes-freed))
	      (t
	       ;; Can't call BEEP since it would use the editor's display
	       ;; connection.
	       (lisp::default-beep-function *standard-output*)
	       (format t control bytes-retained bytes-freed trigger)
	       (finish-output))))))


;;;; Site-Wrapper-Macro and standard device init/exit functions.

(defun in-editor-standard-input-read (stream &rest ignore)
  (declare (ignore ignore))
  (error "Attempt to read off standard input while in the editor -- ~S"
	 stream))

(defvar *illegal-read-stream*
  (lisp::make-lisp-stream :in #'in-editor-standard-input-read))

(defmacro site-wrapper-macro (&body body)
  `(unwind-protect
       (progn
	 (if *editor-has-been-entered*
	     (let ((device (device-hunk-device (window-hunk (current-window)))))
	       (funcall (device-init device) device)))
	 (let ((*beep-function* #'editor-beep)
	       (*gc-notify-before* #'editor-gc-notify-before)
	       (*gc-notify-after* #'editor-gc-notify-after)
	       (*standard-input* *illegal-read-stream*)
	       (*query-io* *illegal-read-stream*))
	   (if *editor-windowed-input*
	       #+clx (ext:with-clx-event-handling
		      (*editor-windowed-input* #'ext:object-set-event-handler)
		      ,@body)
	       #-clx ()
	       ,@body)))
     (let ((device (device-hunk-device (window-hunk (current-window)))))
       (funcall (device-exit device) device))))

(defun standard-device-init ()
  (setup-input))

(defun standard-device-exit ()
  (reset-input))

(proclaim '(special *echo-area-window*))

;;; Maybe bury/unbury editor window when we go to and from Lisp.  This
;;; should do something more sophisticated when we know what that is.
;;;
#+clx
(defun default-editor-window-mngt (display on)
  (let ((xparent (window-group-xparent
		  (bitmap-hunk-window-group (window-hunk *current-window*))))
	(echo-xparent (window-group-xparent
		       (bitmap-hunk-window-group
			(window-hunk *echo-area-window*)))))
    (cond (on (setf (xlib:window-priority echo-xparent) :above)
	      (clear-editor-input *editor-input*)
	      (setf (xlib:window-priority xparent) :above))
	  (t (setf (xlib:window-priority echo-xparent) :below)
	     (setf (xlib:window-priority xparent) :below))))
  (xlib:display-force-output display))

(defvar *editor-window-mngt* nil ;#'default-editor-window-mngt
  "This function is called by EDITOR-WINDOW, passing its arguments.  This may
   be nil.")

(defun editor-window (display on)
  "Calls *editor-window-mngt* on the argument $on when *current-window* is
   bound.  This is called in the device init and exit methods for X bitmap
   devices."
  (and *editor-window-mngt*
       *current-window*
       (funcall *editor-window-mngt* display on)))


;;;; Line Wrap Char.

(defvar *line-wrap-char* #\!
  "The character to be displayed to indicate wrapped lines.")


;;;; Current terminal character translation.

(defvar termcap-file "/etc/termcap")


;;;; Event scheduling.

#[ Event Scheduling

The mechanism described in this chapter is only operative when the Lisp
process is actually running inside the editor, within the ed function.  The
designers intended its use to be associated with the editor, such as with
auto-saving files, reminding the user, etc.

{function:ed:schedule-event}
{function:ed:remove-scheduled-event}
]#

;;; The time queue provides a ROUGH mechanism for scheduling events to
;;; occur after a given amount of time has passed, optionally repeating
;;; using the given time as an interval for rescheduling.  When the input
;;; loop goes around, it will check the current time and process all events
;;; that should have happened before or at this time.  The function gets
;;; called on the number of seconds that have elapsed since it was last
;;; called.
;;;
;;; NEXT-SCHEDULED-EVENT-WAIT and INVOKE-SCHEDULED-EVENTS are used in the
;;; editor stream in methods.
;;;
;;; SCHEDULE-EVENT and REMOVE-SCHEDULED-EVENT are exported interfaces.

(defstruct (tq-event (:print-function print-tq-event)
		     (:constructor make-tq-event
				   (time last-time interval function)))
  time		; When the event should happen.
  last-time	; When the event was scheduled.
  interval	; When true, how often the event should happen.
  function)	; What to do.

(defun print-tq-event (obj stream n)
  (declare (ignore n))
  (format stream "#<Tq-Event ~S>" (tq-event-function obj)))

(defvar *time-queue* nil
  "This is the time priority queue used in editor input streams for event
   scheduling.")

(defvar *invoking-event* nil
  "True while running the function for an event.")

;;; QUEUE-TIME-EVENT inserts event into the time priority queue *time-queue*.
;;; Event is inserted before the first element that it is less than (which
;;; means that it gets inserted after elements that are the same).
;;; *time-queue* is returned.
;;;
(defun queue-time-event (event)
  (let ((time (tq-event-time event)))
    (if *time-queue*
	(if (< time (tq-event-time (car *time-queue*)))
	    (push event *time-queue*)
	    (do ((prev *time-queue* rest)
		 (rest (cdr *time-queue*) (cdr rest)))
		((or (null rest)
		     (< time (tq-event-time (car rest))))
		 (push event (cdr prev))
		 *time-queue*)))
	(push event *time-queue*))))

;;; NEXT-SCHEDULED-EVENT-WAIT returns nil or the number of seconds to wait for
;;; the next event to happen.
;;;
(defun next-scheduled-event-wait ()
  (if *time-queue*
      (let ((wait (round (- (tq-event-time (car *time-queue*))
			    (get-internal-real-time))
			 internal-time-units-per-second)))
	(if (plusp wait) wait 0))))

;;; INVOKE-SCHEDULED-EVENTS invokes all the functions in *time-queue* whose
;;; time has come.  If we run out of events, or there are none, then we get
;;; out.  If we popped an event whose time hasn't come, we push it back on
;;; the queue.  Each function is called on how many seconds, roughly, went
;;; by since the last time it was called (or scheduled).  If it has an
;;; interval, we re-queue it.  While invoking the function, bind
;;; *invoking-event* to t in case the event function tries to read off
;;; *editor-input*.
;;;
(defun invoke-scheduled-events ()
  (or *invoking-event*
      (let ((time (get-internal-real-time)))
	(loop
	  (or *time-queue* (return))
	  (let* ((event (car *time-queue*))
		 (event-time (tq-event-time event)))
	    (cond ((>= time event-time)
		   (let ((*invoking-event* t))
		     (funcall (tq-event-function event)
			      (round (- time (tq-event-last-time event))
				     internal-time-units-per-second)))
		   (block-interrupts
		    (pop *time-queue*)
		    (let ((interval (tq-event-interval event)))
		      (when interval
			(setf (tq-event-time event) (+ time interval))
			(setf (tq-event-last-time event) time)
			(queue-time-event event)))))
		  (t (return))))))))

(defun schedule-event (time function &optional (repeat t) (absolute nil))
  "Schedule $function to be called at some time.

   The editor runs `Schedule Event Hook' after the event is scheduled.

   If $absolute is true then call $function at universal time $time and, if
   $repeat is true, every $repeat seconds thereafter.  If $time is t call
   $function now.

   If $absolute is false call $function in $time seconds and, if $repeat is
   true, every $time seconds thereafter.

   This is a rough mechanism since is runs between commands and commands
   can take an arbitrary amount of time to run.  The $function will be
   called at the first possible moment after the event is due.  $function
   takes the time that has elapsed since the last time it was called (or
   since it was scheduled, for the first invocation)."
  (let* ((now (get-internal-real-time))
	 (itime (if absolute
		    (if (eq time t)
			now
			(universal-to-internal-real-time time))
		    (* internal-time-units-per-second time)))
	 (event (if absolute
		    (make-tq-event itime now
				   (if repeat
				       (* internal-time-units-per-second
					  repeat))
				   function)
		    (make-tq-event (+ itime now)
				   now
				   (if repeat itime)
				   function))))
    (queue-time-event event)
; FIX
;    (invoke-hook ed::schedule-event-hook event)
    event))

(defun remove-scheduled-event (function)
  "Removes FUNCTION, which was queued with SCHEDULE-EVENT."
  (setf *time-queue* (delete function *time-queue* :key
			     #'tq-event-function)))


;;;; Editor sleeping.

(defun editor-sleep (time)
  "Return after approximately $time seconds have elapsed or when input is
   available on *editor-input*."
  (or (zerop time) (listen-editor-input *editor-input*)
      (progn
	(internal-redisplay)
	(sleep-for-time time)
	())))

(defun sleep-for-time (time)
  (let ((nrw-fun (device-note-read-wait
		  (device-hunk-device (window-hunk (current-window)))))
	(end (+ (get-internal-real-time)
		(truncate (* time internal-time-units-per-second)))))
    (loop
      (when (listen-editor-input *editor-input*)
	(return))
      (let ((left (- end (get-internal-real-time))))
	(or (plusp left) (return nil))
	(when nrw-fun (funcall nrw-fun t))
	(system:serve-event (/ (float left)
			       (float internal-time-units-per-second)))))
    (when nrw-fun (funcall nrw-fun nil))))


;;;; Showing a mark.

(defun show-mark (mark window time)
  "Highlight the position of $mark within $window for $time seconds,
   possibly by moving the cursor there.  The wait may be cancelled if there
   is pending input.  If $mark is positioned outside the text displayed by
   $window, then return (), otherwise return true."
  (let* ((result t))
    (catch 'redisplay-catcher
      (redisplay-window window)
      (setf result
	    (multiple-value-bind (x y) (mark-to-cursorpos mark window)
	      (funcall (device-show-mark
			(device-hunk-device (window-hunk window)))
		       window x y time))))
    result))

(defun tty-show-mark (window x y time)
  (cond ((listen-editor-input *editor-input*))
	(x (internal-redisplay)
	   (let* ((hunk (window-hunk window))
		  (device (device-hunk-device hunk)))
	     (funcall (device-put-cursor device) hunk x y)
	     (when (device-force-output device)
	       (funcall (device-force-output device)))
	     (sleep-for-time time))
	   t)
	(t nil)))

#+clx
(defun bitmap-show-mark (window x y time)
  (cond ((listen-editor-input *editor-input*))
	(x (let* ((hunk (window-hunk window))
		  (display (bitmap-device-display (device-hunk-device hunk))))
	     (internal-redisplay)
	     (hunk-show-cursor hunk x y)
	     (drop-cursor)
	     (xlib:display-finish-output display)
	     (sleep-for-time time)
	     (lift-cursor)
	     t))
	(t nil)))


;;;; Function description and defined-from.

(defun fun-defined-from-pathname (function)
  "Return the pathname in which symbol or function $function was defined if
   it was defined in some file, else return ()."
  (if (eval:interpreted-function-p function)
      (eval::interpreted-function-source function)
      (flet ((frob (code)
	       (let ((info (kernel:%code-debug-info code)))
		 (when info
		   (let ((sources (c::debug-info-source info)))
		     (when sources
		       (let ((source (car sources)))
			 (when (eq (c::debug-source-from source) :file)
			   (c::debug-source-name source)))))))))
	(typecase function
	  (symbol (fun-defined-from-pathname (fdefinition function)))
	  (kernel:byte-closure
	   (fun-defined-from-pathname (kernel:byte-closure-function function)))
	  (kernel:byte-function
	   (frob (c::byte-function-component function)))
	  (function
	   (frob (kernel:function-code-header (kernel:%function-self function))))
	  (t ())))))

(defvar *editor-describe-stream*
  (system:make-indenting-stream *standard-output*))

;;; EDITOR-DESCRIBE-FUNCTION has to mess around to get indenting streams to
;;; work.  These apparently work fine for DESCRIBE, for which they were defined,
;;; but not in general.  It seems they don't indent initial text, only that
;;; following a newline, so inside our use of INDENTING-FURTHER, we need some
;;; form before the WRITE-STRING.  To get this to work, I had to remove the ~%
;;; from the FORMAT string, and use FRESH-LINE; simply using FRESH-LINE with
;;; the ~% caused an extra blank line.  Possibly I should not have glommed onto
;;; this hack whose interface comes from three different packages, but it did
;;; the right thing ....
;;;
;;; Also, we have set INDENTING-STREAM-STREAM to make sure the indenting stream
;;; is based on whatever *standard-output* is when we are called.
;;;
(defun editor-describe-function (fun sym)
  "Calls DESCRIBE on fun.  If fun is compiled, and its original name is not sym,
   then this also outputs any 'function documentation for sym to
   *standard-output*."
  (describe fun)
  (when (and (compiled-function-p fun)
	     (not (eq (kernel:%function-name (kernel:%closure-function fun))
		      sym)))
    (let ((doc (documentation sym 'function)))
      (when doc
	(format t "~&Function documentation for ~S:" sym)
	(setf (lisp::indenting-stream-stream *editor-describe-stream*)
	      *standard-output*)
	(ext:indenting-further *editor-describe-stream* 2
	  (fresh-line *editor-describe-stream*)
	  (write-string doc *editor-describe-stream*))))))


;;;; X Stuff.
;;; Setting window cursors ...

;;; COLOR-PIXEL  --  Internal
;;;
;;; Return the pixel associated with $color, which can be a color name
;;; (List Colors) or an RGB float list.
;;;
;;; FIX where should be?
;;;
#+clx
(defun color-pixel (display color)
  (xlib::color-pixel
   (etypecase color
     (list (xlib:make-color display
			    :red (car color)
			    :green (cadr color)
			    :blue (caddr color))))))

#+clx
(defvar *editor-cursor* nil "Holds cursor for editor windows.")

;;; DEFINE-WINDOW-CURSOR in shoved on the "Make Window Hook".
;;;
#+clx
(defun define-window-cursor (window)
  (setf (xlib:window-cursor (bitmap-hunk-xwindow (window-hunk window)))
	*editor-cursor*))

;;; These are set in INIT-BITMAP-SCREEN-MANAGER and REVERSE-VIDEO-HOOK-FUN.
;;;
#+clx
(defvar *cursor-foreground-color* nil)
#+clx
(defvar *cursor-background-color* nil)
#+clx
(defun make-white-color (display) (xlib:make-color display :red 1.0 :green 1.0 :blue 1.0))
#+clx
(defun make-black-color (display) (xlib:make-color display :red 0.0 :green 0.0 :blue 0.0))

;;; GET-EDITOR-CURSOR is used in INIT-BITMAP-SCREEN-MANAGER to load the
;;; cursor for DEFINE-WINDOW-CURSOR.
;;;
#+clx
(defun get-editor-cursor (display)
  (when *editor-cursor* (xlib:free-cursor *editor-cursor*))
  #| FIX
  (let* ((cursor-file (truename (variable-value 'ed::cursor-bitmap-file)))
	 (mask-file (probe-file (make-pathname :type "mask"
					       :defaults cursor-file)))
	 (root (xlib:screen-root (xlib:display-default-screen display)))
	 (mask-pixmap (if mask-file (get-cursor-pixmap root mask-file))))
    (multiple-value-bind (cursor-pixmap cursor-x-hot cursor-y-hot)
			 (get-cursor-pixmap root cursor-file)
      (setf *editor-cursor*
	    (xlib:create-cursor :source cursor-pixmap :mask mask-pixmap
				:x cursor-x-hot :y cursor-y-hot
				:foreground *cursor-foreground-color*
				:background *cursor-background-color*))
      (xlib:free-pixmap cursor-pixmap)
      (when mask-pixmap (xlib:free-pixmap mask-pixmap))))
  |#
  (setf *editor-cursor* (xlib:create-font-cursor display 2)))

#+clx
(defun get-cursor-pixmap (root pathname)
  (let* ((image (xlib:read-bitmap-file pathname))
	 (pixmap (xlib:create-pixmap :width 16 :height 16
				     :depth 1 :drawable root))
	 (display (xlib:drawable-display root))
	 (gc (xlib:create-gcontext
	      :drawable pixmap :function xlib:boole-1
	      :foreground
	      (color-pixel display
			   (or (value ed::initial-background-color)
			       '(1 1 1)))
	      :background
	      (color-pixel display
			   (or (value ed::initial-foreground-color)
			       '(0 0 0))))))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width 16 :height 16)
    (xlib:free-gcontext gc)
    (values pixmap (xlib:image-x-hot image) (xlib:image-y-hot image))))

;;; Setting up grey borders ...

#+clx
(defparameter editor-grey-bitmap-data
  '(#*10 #*01))

#+clx
(defun get-editor-grey-pixmap (display)
  (let* ((screen (xlib:display-default-screen display))
	 (depth (xlib:screen-root-depth screen))
	 (root (xlib:screen-root screen))
	 (height (length editor-grey-bitmap-data))
	 (width (length (car editor-grey-bitmap-data)))
	 (image (apply #'xlib:bitmap-image editor-grey-bitmap-data))
	 (pixmap (xlib:create-pixmap :width width :height height
				     :depth depth :drawable root))
	 (gc (xlib:create-gcontext :drawable pixmap
				   :function xlib:boole-1
				   :foreground
				   (color-pixel
				    display
				    (or (value ed::initial-foreground-color)
					'(0 0 0)))
				   :background
				   (color-pixel
				    display
				    (or (value ed::initial-background-color)
					'(1 1 1))))))
    (xlib:put-image pixmap gc image
		    :x 0 :y 0 :width width :height height :bitmap-p t)
    (xlib:free-gcontext gc)
    pixmap))

;;; Cut Buffer manipulation ...

#+clx
(defun store-cut-string (display string)
  (check-type string simple-string)
  (setf (xlib:cut-buffer display) string))

#+clx
(defun store-cut-region (&optional (region (ed::current-region () ())))
  (let ((string (region-to-string region)))
    (if (plusp (length string))
	(store-cut-string (bitmap-device-display
			   (device-hunk-device
			    (window-hunk (current-window))))
			  string))))

#+clx
(defun fetch-cut-string (display)
  (xlib:cut-buffer display))

#+clx
(defun fetch-selection ()
  (let ((hunk (edi::window-hunk (current-window))))
    (xlib:selection (edi::bitmap-device-display
		     (edi::device-hunk-device hunk))
		    (edi::bitmap-hunk-xwindow hunk))))

#+clx
(defun fetch-clipboard ()
  (let ((hunk (edi::window-hunk (current-window))))
    (xlib:clipboard (edi::bitmap-device-display
		     (edi::device-hunk-device hunk))
		    (edi::bitmap-hunk-xwindow hunk))))

#+clx
(defun store-selection-string (display window string &optional name)
  (check-type string simple-string)
  (xlib:set-selection display window string name))

#+clx
(defun store-selection-region (&optional (region (ed::current-region () ()))
					 name)
  (format t "ssr ~A~%" region name)
  (let ((string (region-to-string region))
	(hunk (window-hunk (current-window))))
    (if (plusp (length string))
	(store-selection-string (bitmap-device-display
				 (device-hunk-device hunk))
				(bitmap-hunk-xwindow hunk)
				string
				name))))

#+clx
(defun serve-selection (selection time)
  (declare (ignore selection time))
  (let ((region (ed::current-region () ())))
    (if region (region-to-string region))))

#+clx
(defun selection-on (&optional name)
  (let ((hunk (window-hunk (current-window))))
    (xlib:set-selection (bitmap-device-display
			 (device-hunk-device hunk))
			(bitmap-hunk-xwindow hunk)
			#'serve-selection
			name)))

#+clx
(defun store-clipboard (string)
  (let ((hunk (window-hunk (current-window))))
    (if (plusp (length string))
	(store-selection-string (bitmap-device-display
				 (device-hunk-device hunk))
				(bitmap-hunk-xwindow hunk)
				string
				"CLIPBOARD"))))

#+clx
(defun serve-clipboard (selection time)
  (declare (ignore selection time))
  (if (> (ring-length ed::*kill-ring*) 0)
      (let ((region (ring-ref ed::*kill-ring* 0)))
	(if region (region-to-string region)))))

#+clx
(defun clipboard-on (&optional name)
  (let ((hunk (window-hunk (current-window))))
    (xlib:set-selection (bitmap-device-display
			 (device-hunk-device hunk))
			(bitmap-hunk-xwindow hunk)
			#'serve-clipboard
			"CLIPBOARD")))


;;; Window naming ...

#+clx
(defun set-window-name-for-buffer-name (buffer new-name)
  (dolist (ele (buffer-windows buffer))
    (xlib:set-standard-properties (bitmap-hunk-xwindow (window-hunk ele))
				  :icon-name new-name)))

#+clx
(defun set-window-name-for-window-buffer (window new-buffer)
  (xlib:set-standard-properties (bitmap-hunk-xwindow (window-hunk window))
				:icon-name (buffer-name new-buffer)))


;;;; Some hacks for supporting the editor under Mach.

;;; WINDOWED-MONITOR-P is used by the reverse video variable's hook function
;;; to determine if it needs to go around fixing all the windows.
;;;
(defun windowed-monitor-p ()
  "Return whether the monitor is being used with a window system, by
   returning the console's CLX display structure."
  *editor-windowed-input*)

(defun get-terminal-name ()
  (cdr (assoc :term *environment-list* :test #'eq)))

(defun get-termcap-env-var ()
  (cdr (assoc :termcap *environment-list* :test #'eq)))

;;; GET-EDITOR-TTY-INPUT reads from stream's Unix file descriptor queuing events
;;; in the stream's queue.
;;;
(defun get-editor-tty-input (fd)
  (alien:with-alien ((buf (alien:array c-call:unsigned-char 256)))
    (multiple-value-bind
	(len errno)
	(unix:unix-read fd (alien:alien-sap buf) 256)
      (declare (type (or null fixnum) len))
      (or len
	  (error "Problem with tty input: ~S"
		 (unix:get-unix-error-msg errno)))
      (dotimes (i len t)
	(q-event *real-editor-input*
		 (ext:char-key-event (code-char (alien:deref buf i))))))))

(defun editor-tty-listen (stream)
  (alien:with-alien ((nc c-call:int))
    (and (unix:unix-ioctl (tty-editor-input-fd stream)
			  unix::FIONREAD
			  (alien:alien-sap (alien:addr nc)))
	 (> nc 0))))

(defvar old-flags)

(defvar old-tchars)

#-glibc2
(defvar old-ltchars)

#+(or hpux irix freebsd glibc2)
(progn
  (defvar old-c-iflag)
  (defvar old-c-oflag)
  (defvar old-c-cflag)
  (defvar old-c-lflag)
  (defvar old-c-cc))

(defun throw-to-top (&rest args)
  "Signal an editor-top-level-catcher condition."
  (declare (ignore args))
  (signal 'editor-top-level-catcher ()))

;; FIX maybe move to code:terminal.lisp

(defun setup-input ()
  (let ((fd *editor-file-descriptor*))
    (when (unix:unix-isatty 0)
      #+(or hpux irix freebsd glibc2)
      (alien:with-alien ((tios (alien:struct unix:termios)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-tcgetattr fd (alien:alien-sap tios))
	  (or val
	      (error "Failed to tcgetattr: ~S."
		     (unix:get-unix-error-msg err))))
	(setf old-c-iflag (alien:slot tios 'unix:c-iflag))
	(setf old-c-oflag (alien:slot tios 'unix:c-oflag))
	(setf old-c-cflag (alien:slot tios 'unix:c-cflag))
	(setf old-c-lflag (alien:slot tios 'unix:c-lflag))
	(setf old-c-cc
	      (vector (alien:deref (alien:slot tios 'unix:c-cc) unix:vdsusp)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:veof)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vintr)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vquit)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vstart)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vstop)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vsusp)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vmin)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vtime)))
	(setf (alien:slot tios 'unix:c-lflag)
	      (logand (alien:slot tios 'unix:c-lflag)
		      (lognot (logior unix:tty-echo unix:tty-icanon))))
	(setf (alien:slot tios 'unix:c-iflag)
	      (logand (alien:slot tios 'unix:c-iflag)
		      (lognot (logior unix:tty-icrnl unix:tty-ixon))))
	(setf (alien:slot tios 'unix:c-oflag)
	      (logand (alien:slot tios 'unix:c-oflag)
		      (lognot #-freebsd unix:tty-ocrnl
			      #+freebsd unix:tty-onlcr)))
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vdsusp) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:veof) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vintr)
	      (if *editor-windowed-input* #xff 7)) ; control-g
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vquit) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstart) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstop) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vsusp) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vmin) 1)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vtime) 0)
	(multiple-value-bind
	    (val err)
	    (unix:unix-tcsetattr fd unix:tcsaflush (alien:alien-sap tios))
	  (or val
	      (error "Failed to tcsetattr: ~S."
		     (unix:get-unix-error-msg err)))))
      #-(or hpux irix freebsd glibc2)
      (alien:with-alien ((sg (alien:struct unix:sgttyb)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCGETP (alien:alien-sap sg))
	  (or val
	      (error "Failed to get tty information: ~S."
		     (unix:get-unix-error-msg err))))
	(let ((flags (alien:slot sg 'unix:sg-flags)))
	  (setq old-flags flags)
	  (setf (alien:slot sg 'unix:sg-flags)
		(logand #-(or hpux irix freebsd glibc2) (logior flags unix:tty-cbreak)
			(lognot unix:tty-echo)
			(lognot unix:tty-crmod)))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCSETP (alien:alien-sap sg))
	    (if (null val)
		(error "Failed to set tty information: ~S."
		       (unix:get-unix-error-msg err))))))
      #-(or hpux irix freebsd glibc2)
      (alien:with-alien ((tc (alien:struct unix:tchars)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCGETC (alien:alien-sap tc))
	  (or val
	      (error "Failed to get tty tchars information: ~S."
		     (unix:get-unix-error-msg err))))
	(setq old-tchars
	      (vector (alien:slot tc 'unix:t-intrc)
		      (alien:slot tc 'unix:t-quitc)
		      (alien:slot tc 'unix:t-startc)
		      (alien:slot tc 'unix:t-stopc)
		      (alien:slot tc 'unix:t-eofc)
		      (alien:slot tc 'unix:t-brkc)))
	(setf (alien:slot tc 'unix:t-intrc)
	      (if *editor-windowed-input* -1 7))
	(setf (alien:slot tc 'unix:t-quitc) -1)
	(setf (alien:slot tc 'unix:t-startc) -1)
	(setf (alien:slot tc 'unix:t-stopc) -1)
	(setf (alien:slot tc 'unix:t-eofc) -1)
	(setf (alien:slot tc 'unix:t-brkc) -1)
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCSETC (alien:alien-sap tc))
	  (fi val
	      (error "Failed to set tchars: ~S."
		     (unix:get-unix-error-msg err)))))

      ;; Needed even under HpUx to suppress dsuspc.
      #-(or glibc2 irix)
      (alien:with-alien ((tc (alien:struct unix:ltchars)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCGLTC (alien:alien-sap tc))
	  (or val
	      (error "Failed to get tty ltchars information: ~S."
		     (unix:get-unix-error-msg err))))
	(setq old-ltchars
	      (vector (alien:slot tc 'unix:t-suspc)
		      (alien:slot tc 'unix:t-dsuspc)
		      (alien:slot tc 'unix:t-rprntc)
		      (alien:slot tc 'unix:t-flushc)
		      (alien:slot tc 'unix:t-werasc)
		      (alien:slot tc 'unix:t-lnextc)))
	(setf (alien:slot tc 'unix:t-suspc) -1)
	(setf (alien:slot tc 'unix:t-dsuspc) -1)
	(setf (alien:slot tc 'unix:t-rprntc) -1)
	(setf (alien:slot tc 'unix:t-flushc) -1)
	(setf (alien:slot tc 'unix:t-werasc) -1)
	(setf (alien:slot tc 'unix:t-lnextc) -1)
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCSLTC (alien:alien-sap tc))
	  (or val
	      (error "Failed to set ltchars, unix error ~S."
		     (unix:get-unix-error-msg err)))))
      (enable-interrupt :sigint #'throw-to-top))))

(defun reset-input ()
  (when (unix:unix-isatty 0)
    (let ((fd *editor-file-descriptor*))
      #+(or hpux irix freebsd glibc2)
      (when (boundp 'old-c-lflag)
	(alien:with-alien ((tios (alien:struct unix:termios)))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-tcgetattr fd (alien:alien-sap tios))
	    (or val
		(error "Failed to tcgetattr: ~S."
		       (unix:get-unix-error-msg err))))
	  (setf (alien:slot tios 'unix:c-iflag) old-c-iflag)
	  (setf (alien:slot tios 'unix:c-oflag) old-c-oflag)
	  (setf (alien:slot tios 'unix:c-cflag) old-c-cflag)
	  (setf (alien:slot tios 'unix:c-lflag) old-c-lflag)
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vdsusp)
		(svref old-c-cc 0))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:veof)
		(svref old-c-cc 1))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vintr)
		(svref old-c-cc 2))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vquit)
		(svref old-c-cc 3))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstart)
		(svref old-c-cc 4))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstop)
		(svref old-c-cc 5))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vsusp)
		(svref old-c-cc 6))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vmin)
		(svref old-c-cc 7))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vtime)
		(svref old-c-cc 8))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-tcsetattr fd unix:tcsaflush (alien:alien-sap tios))
	    (fi val
		(error "Failed to tcsetattr: ~S."
		       (unix:get-unix-error-msg err))))))
      #-(or hpux irix freebsd glibc2)
      (when (boundp 'old-flags)
	(alien:with-alien ((sg (alien:struct unix:sgttyb)))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCGETP (alien:alien-sap sg))
	    (or val
		(error "Failed to get tty information: ~S."
		       (unix:get-unix-error-msg err)))
	    (setf (alien:slot sg 'unix:sg-flags) old-flags)
	    (multiple-value-bind
		(val err)
		(unix:unix-ioctl fd unix:TIOCSETP (alien:alien-sap sg))
	      (fi val
		  (error "Failed to set tty information: ~S."
			 (unix:get-unix-error-msg err)))))))
      #-(or hpux irix freebsd glibc2)
      (when (and (boundp 'old-tchars)
		 (simple-vector-p old-tchars)
		 (eq (length old-tchars) 6))
	(alien:with-alien ((tc (alien:struct unix:tchars)))
	  (setf (alien:slot tc 'unix:t-intrc) (svref old-tchars 0))
	  (setf (alien:slot tc 'unix:t-quitc) (svref old-tchars 1))
	  (setf (alien:slot tc 'unix:t-startc) (svref old-tchars 2))
	  (setf (alien:slot tc 'unix:t-stopc) (svref old-tchars 3))
	  (setf (alien:slot tc 'unix:t-eofc) (svref old-tchars 4))
	  (setf (alien:slot tc 'unix:t-brkc) (svref old-tchars 5))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCSETC (alien:alien-sap tc))
	    (fi val
		(error "Failed to set tchars, unix error ~S."
		       (unix:get-unix-error-msg err))))))
      #-glibc2
      (when (and (boundp 'old-ltchars)
		 (simple-vector-p old-ltchars)
		 (eq (length old-ltchars) 6))
	(alien:with-alien ((tc (alien:struct unix:ltchars)))
	  (setf (alien:slot tc 'unix:t-suspc) (svref old-ltchars 0))
	  (setf (alien:slot tc 'unix:t-dsuspc) (svref old-ltchars 1))
	  (setf (alien:slot tc 'unix:t-rprntc) (svref old-ltchars 2))
	  (setf (alien:slot tc 'unix:t-flushc) (svref old-ltchars 3))
	  (setf (alien:slot tc 'unix:t-werasc) (svref old-ltchars 4))
	  (setf (alien:slot tc 'unix:t-lnextc) (svref old-ltchars 5))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCSLTC (alien:alien-sap tc))
	    (fi val
		(error "Failed to set ltchars, unix error ~S."
		       (unix:get-unix-error-msg err)))))))))

(defun pause ()
  "Suspend the editor process and return control to the shell."
  (system:with-screen
   (unix:unix-kill (unix:unix-getpid) :sigstop))
  ; FIX in x hide wins
  t)
