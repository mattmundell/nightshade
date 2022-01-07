;;; Device independent screen management functions.

(in-package "EDI")

(export '(make-window delete-window next-window previous-window
	  set-window-foreground-color set-window-background-color))

#[ Display Conventions

There are two ways that the editor displays information on the screen; one
is normal buffer display, in which the text being edited is shown on the
screen, and the other is a pop-up window.

[ Pop-Up Windows      ]
[ Buffer Display      ]
[ Recentering Windows ]
[ Modelines           ]
]#

#[ Controlling the Display

[ Windows            ]
[ The Current Window ]
[ Window Functions   ]
[ Cursor Positions   ]
[ Redisplay          ]
]#

#[ Windows

A window is a mechanism for displaying part of a buffer on some physical
device.  A window is a way to view a buffer but is not synonymous with one; a
buffer may be viewed in any number of windows.  A window may have a
modeline which is a line of text displayed across the bottom of a window to
indicate status information, typically related to the buffer displayed.
]#


;;;; Screen management initialization.

(proclaim '(special *echo-area-buffer*))

(defvar *devices* ()
  "List of all devices.")

;;; %INIT-SCREEN-MANAGER creates the initial windows and sets up the data
;;; structures used by the screen manager.  The "Main", "Echo Area" and
;;; "Ring Marker" buffer modelines are set here in case these editor
;;; variables are modified in an init file.  Since these buffers don't have
;;; windows yet, these sets won't cause any updates to occur.  This is
;;; called from %INIT-REDISPLAY.
;;;
(defun %init-screen-manager (display)
  #-clx
  (declare (ignore display))
  (setf (buffer-modeline-fields *current-buffer*)
	(value ed::default-modeline-fields))
  (setf (buffer-modeline-fields *echo-area-buffer*)
	(value ed::default-status-line-fields))
  (setf (buffer-modeline-fields *ring-marker-buffer*)
	(value ed::default-modeline-fields))
  #+clx
  (if (windowed-monitor-p)
      (init-bitmap-screen-manager display)
      (init-tty-screen-manager (get-terminal-name)))
  #-clx
  (init-tty-screen-manager (get-terminal-name))
  )


;;;; Window operations.

#[ Window Functions

{function:ed:make-window}
{evariable:Default Window Width}
{evariable:Default Window Height}
{evariable:Make Window Hook}
{function:ed:windowp}
{function:ed:delete-window}
{evariable:Delete Window Hook}
{function:ed:window-buffer}
{evariable:Window Buffer Hook}
{function:ed:window-display-start}
{function:ed:window-display-end}
{function:ed:window-display-recentering}
{function:ed:window-point}
{function:ed:center-window}
{function:ed:scroll-window}
{function:ed:displayed-p}
{function:ed:window-height}
{function:ed:window-width}
{function:ed:next-window}
{function:ed:previous-window}
]#

(defun make-window (start &key (modelinep t) (device nil) window
			  (proportion .5)
			  (font-family *default-font-family*)
			  (error t)
			  (ask-user nil) x y
			  (width (value ed::default-window-width))
			  (height (value ed::default-window-height)))
  "Return a window displaying text starting at the mark $start, which must
   point into a buffer.  On failure to make a window if $error is true
   signal an error otherwise return ().

   Make the current window a proportion of its current height to make room
   for the new window

   Set the window to display buffer modelines if $modelinep is true.

   $window is a device dependent window (e.g. an X window) to be used with
   the editor window.  Some devices support this argument.  $window becomes
   the parent window for a new group of windows that behave in a stack
   orientation as windows do on the terminal.

   $font-family is the font-family used for displaying text in the window.

   If $ask-user is true, prompt for any missing dimensions ($x, $y, $width,
   and $height) to make a new group of windows, as with the $window
   argument.  Some devices support this argument.  True values other than t
   may have device dependent meanings.  $x and $y are in pixel units,
   $width and $height are characters units.

   $proportion determines what proportion of the current window's height
   the new window will use.  The current window retains whatever space left
   after accommodating the new one.

   Invoke *Make Window Hook* with the new window."
  (if (eq (current-window) *echo-area-window*)
      (editor-error "Attempt to make a window in the echo area."))
  (let* ((device (or device (device-hunk-device (window-hunk (current-window)))))
	 (window (funcall (device-make-window device)
			  device start modelinep window font-family
			  ask-user x y width height proportion)))
    (if window
	(progn
	  (invoke-hook ed::make-window-hook window)
	  window)
	(if error (editor-error "Failed to make a window.")))))

(defun delete-window (window)
  "Release $window after invoking *Delete Window Hook* with $window.  Use
   edi::*delete-window-hook* to get rid of parent windows on a bitmap
   device deleting the last editor window in a group."
  (if (<= (length *window-list*) 2)
      (error "Cannot kill the only window."))
  (invoke-hook ed::delete-window-hook window)
  (setq *window-list* (delq window *window-list*))
  (funcall (device-delete-window (device-hunk-device (window-hunk window)))
	   window)
  ;;
  ;; Since the programmer's interface fails to allow users to determine if
  ;; they're commands delete the current window, this primitive needs to
  ;; make sure the editor doesn't get screwed.  This inadequacy comes from
  ;; the bitmap window groups and the vague descriptions of PREVIOUS-WINDOW
  ;; and NEXT-WINDOW.
  (when (eq window *current-window*)
    (let ((window (find-if-not #'(lambda (w) (eq w *echo-area-window*))
			       *window-list*)))
      (setf (current-buffer) (window-buffer window)
	    (current-window) window))))

(defun next-window (window)
  "Return the next window of $window, wrapping around if $window is the
   bottom window.

   The exact meaning of next depends on the device displaying the window.
   Repeated calls should eventually cycle through all the windows displayed
   on a device"
  (check-type window window)
  (funcall (device-next-window (device-hunk-device (window-hunk window)))
	   window))

(defun previous-window (window)
  "Return the previous window of $window, wrapping around if $window is the
   top window.

   The exact meaning of previous depends on the device displaying the
   window.  Repeated calls should eventually cycle through all the windows
   displayed on a device"
  (check-type window window)
  (funcall (device-previous-window (device-hunk-device (window-hunk window)))
	   window))

(defun set-window-foreground-color (window color)
  "Set the foreground color of $window to $color."
  (funcall (device-set-foreground-color (device-hunk-device (window-hunk window)))
	   window
	   color))

(defun set-window-background-color (window color)
  "Set the background color of $window to $color."
  (funcall (device-set-background-color (device-hunk-device (window-hunk window)))
	   window
	   color))


;;;; Random typeout support.

;;; PREPARE-FOR-RANDOM-TYPEOUT  --  Internal
;;;
;;; The WITH-POP-UP-DISPLAY macro calls this just before displaying output
;;; for the user.  This goes to some effor to compute the height of the window
;;; in text lines if it is not supplied.  Whether it is supplied or not, we
;;; add one to the height for the modeline, and we subtract one line if the
;;; last line is empty.  Just before using the height, make sure it is at
;;; least two -- one for the modeline and one for text, so window making
;;; primitives survive.
;;;
(defun prepare-for-random-typeout (stream height)
  (let* ((buffer (line-buffer (mark-line (random-typeout-stream-mark stream))))
	 (real-height (1+ (or height (rt-count-lines buffer))))
	 (device (device-hunk-device (window-hunk (current-window)))))
    (funcall (device-random-typeout-setup device) device stream
	     (max (if (and (empty-line-p (buffer-end-mark buffer)) (not height))
		      (1- real-height)
		      real-height)
		  2))))

;;; RT-COUNT-LINES computes the correct height for a window.  This includes
;;; taking wrapping line characters into account.  Take the MARK-COLUMN at
;;; the end of each line.  This is how many characters long the editor
;;; thinks the line is.  When it is displayed, however, end of line
;;; characters are added to the end of each line that wraps.  The second
;;; INCF form adds these to the current line length.  Then INCF the current
;;; height by the CEILING of the width of the random typeout window and the
;;; line length (with added line-end chars).  Use CEILING because there is
;;; always at least one line.  Finally, jump out of the loop if we're at
;;; the end of the buffer.
;;;
(defun rt-count-lines (buffer)
  (with-mark ((mark (buffer-start-mark buffer)))
    (let ((width (window-width (current-window)))
	  (count 0))
	(loop
	  (let* ((column (mark-column (line-end mark)))
		 (temp (ceiling (incf column (floor (1- column) width))
				width)))
	    ;; Lines with no characters yield zero temp.
	    (incf count (if (zerop temp) 1 temp))
	    (unless (line-offset mark 1) (return count)))))))


;;; RANDOM-TYPEOUT-CLEANUP  --  Internal
;;;
;;; Clean up after random typeout.  This clears the area where the random
;;; typeout was and redisplays any affected windows.
;;;
(defun random-typeout-cleanup (stream &optional (degree t))
  (let* ((window (random-typeout-stream-window stream))
	 (buffer (window-buffer window))
	 (device (device-hunk-device (window-hunk window)))
	 (*more-prompt-action* :normal))
    (update-modeline-field buffer window :more-prompt)
    (random-typeout-redisplay window)
    (setf (buffer-windows buffer) (delete window (buffer-windows buffer)))
    (funcall (device-random-typeout-cleanup device) stream degree)
    (when (device-force-output device)
      (funcall (device-force-output device)))))

;;; *more-prompt-action* is bound in random typeout streams before
;;; redisplaying.
;;;
(defvar *more-prompt-action* :normal)
(defvar *random-typeout-ml-fields*
  (list (make-modeline-field
	 :name :more-prompt
	 :function #'(lambda (buffer window)
		       (declare (ignore window))
		       (ecase *more-prompt-action*
			 (:more "--More--")
			 (:flush "--End--")
			 (:empty "")
			 (:normal
			  (concatenate 'simple-string
				       "Random Typeout Buffer          ["
				       (buffer-name buffer)
				       "]")))))))
