;;; Implementation independent code which implements the window primitives
;;; and most of the code which defines other aspects of the interface to
;;; redisplay.

(in-package "EDI")

(export '(current-window window-buffer modeline-field-width
	  modeline-field-function make-modeline-field update-modeline-fields
	  update-modeline-field modeline-field-name modeline-field
	  update-modeline-column-field update-modeline-position-field
	  update-modeline-buffer-state-field
	  update-all-modeline-buffer-state-fields
	  update-all-modeline-fields update-line-number update-line-numbers
	  editor-finish-output *window-list*))


#[ Windows

Editor windows display a portion of a buffer's text.  The section on window
groups, [Groups], discusses managing windows on a bitmap device.

{command:New Window}
{command:Split Window}
{command:Next Window}
{command:Previous Window}
{command:Delete Window}
{command:Delete Next Window}

On bitmap devices, if there is only one window in the group, both `Delete
Window' and `Delete Next Window' delete the group, making some window in
another group the current window.  If it is the only gropu, they signal a
user error.

{command:Go to One Window}
{command:Line to Top of Window}
{command:Line to Center of Window}
{command:Scroll Next Window Down}
{command:Scroll Next Window Up}
{command:Refresh Screen}

`Refresh Screen' is useful if the screen gets trashed.
]#


;;;; CURRENT-WINDOW.

#[ The Current Window

{function:edi:current-window}
{evariable:Set Window Hook}
{variable:ed:*window-list*}
]#

(defvar *current-window* () "The current window object.")
(defvar *window-list* () "A list of all window objects.")

(proclaim '(inline current-window))

(defun current-window ()
  "Return the window currently displaying the cursor.

   The cursor always tracks the buffer-point of the corresponding buffer.
   If the point is moved to a position which would be off the screen the
   recentering process is invoked.  Recentering shifts the starting point
   of the window so that the point is once again displayed.

   The current window may be changed with `setf'.  Before the current
   window is changed, the hook *Set Window Hook* is invoked with the new
   value."
  *current-window*)

(defun %set-current-window (new-window)
  (invoke-hook ed::set-window-hook new-window)
  (move-mark (window-point *current-window*)
	     (buffer-point (window-buffer *current-window*)))
  (move-mark (buffer-point (window-buffer new-window))
	     (window-point new-window))
  (setq *current-window* new-window))


;;;; Window structure support.

(defun %print-hwindow (obj stream depth)
  (declare (ignore depth))
  (write-string "#<Editor Window \"" stream)
  (write-string (buffer-name (window-buffer obj)) stream)
  (write-string "\">" stream))

(defun window-buffer (window)
  "Return the buffer from which the $window displays text.

   This may be changed with `setf', in which case the hook *Window Buffer
   Hook* is invoked beforehand with the window and the new buffer."
  (window-%buffer window))

(defun %set-window-buffer (window new-buffer)
  (or (bufferp new-buffer) (error "~S is not a buffer." new-buffer))
  (or (windowp window) (error "~S is not a window." window))
  (unless (eq new-buffer (window-buffer window))
    (invoke-hook ed::window-buffer-hook window new-buffer)
    ;;
    ;; Move the window's marks to the new start.
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer)))
      (move-mark (buffer-display-start buffer) (window-display-start window))
      (push window (buffer-windows new-buffer))
      (move-mark (window-point window) (buffer-point new-buffer))
      (move-mark (window-display-start window) (buffer-display-start new-buffer))
      (move-mark (window-display-end window) (buffer-display-start new-buffer)))
    ;;
    ;; Delete all the dis-lines, and nil out the line and chars so they get
    ;; gc'ed.
    (let ((first (window-first-line window))
	  (last (window-last-line window))
	  (free (window-spare-lines window)))
      (unless (eq (cdr first) the-sentinel)
	(shiftf (cdr last) free (cdr first) the-sentinel))
      (dolist (dl free)
	(setf (dis-line-line dl) nil  (dis-line-old-chars dl) nil))
      (setf (window-spare-lines window) free))
    ;;
    ;; Set the last line and first&last changed so we know there's nothing there.
    (setf (window-last-line window) the-sentinel
	  (window-first-changed window) the-sentinel
	  (window-last-changed window) the-sentinel)
    ;;
    ;; Make sure the window gets updated, and set the buffer.
    (setf (window-tick window) -3)
    (setf (window-%buffer window) new-buffer)))


;;; %INIT-REDISPLAY sets up redisplay's internal data structures.  We create
;;; initial windows, setup some hooks to cause modeline recomputation, and call
;;; any device init necessary.  This is called from ED.
;;;
(defun %init-redisplay (display)
  (%init-screen-manager display)
  (add-hook ed::buffer-major-mode-hook 'queue-buffer-change)
  (add-hook ed::buffer-minor-mode-hook 'queue-buffer-change)
  (add-hook ed::buffer-name-hook 'queue-buffer-change)
  (add-hook ed::buffer-pathname-hook 'queue-buffer-change)
  (add-hook ed::buffer-modified-hook 'queue-buffer-change)
  (add-hook ed::window-buffer-hook 'queue-window-change)

  ;; FIX rename :normal/standard (original sounds like tty original pair)
  (defcolor :original     '(0.0 0.0 0.0) "Normal text.")
  (defcolor :comment      '(1.0 0.0 0.0) "Comments.")
  (defcolor :string       '(0.0 0.7 0.0) "Strings.")
  (defcolor :variable     '(1.0 1.0 0.0) "Variable names.")
  (defcolor :function     '(0.0 0.0 1.0) "Function names.")
  (defcolor :preprocessor '(1.0 0.0 1.0)
    "Preprocessor and reader macro directives.")
  (defcolor :special-form '(0.0 1.0 1.0) "Special forms.")
  (defcolor :error        '(1.0 0.5 1.0) "Errors.")

  (let ((device (device-hunk-device (window-hunk (current-window)))))
    (funcall (device-init device) device))
  (center-window *current-window* (current-point)))


#[ Modelines

A modeline is the line displayed at the bottom of each window where the
editor shows information about the buffer displayed in that window.  Here
is a typical modeline:

    EDI: window.lisp (Lisp Save)   /src/nightshade/src/ed/

This indicates that the file associated with this buffer is
"/src/nightshade/src/ed/window.lisp", and the `Current Package' for Lisp
interaction commands is the "ED" package.  The modes currently present are
`Lisp' and `Save'; the major mode is always displayed first, followed by
any minor modes.  If the buffer has no associated file, then the buffer
name will be present instead:

    PLAY: (Lisp)  Silly:

In this case, the buffer is named `Silly' and is in `Lisp' mode.  The
user has set `Current Package' for this buffer to "PLAY".

{evariable:Maximum Modeline Pathname Length}
{evariable:Maximum Modeline Short Name Length}

The `defevar' command can be used to establish these variables buffer
locally.

If the user has modified the buffer since the last time it was read from or
save to a file, then the modeline starts with two asterisks (**)

    ** EDI: window.lisp (Lisp Save)   /src/nightshade/src/ed/

This serves as a reminder that the buffer should be saved eventually.

There is a special modeline known as the status line which appears as the
`Echo Area''s modeline.  This area displays general information --
recursive edits, whether new mail has arrived, etc.
]#


;;;; Modelines-field structure support.

(defun print-modeline-field (obj stream ignore)
  (declare (ignore ignore))
  (write-string "#<Editor Modeline-field " stream)
  (prin1 (modeline-field-%name obj) stream)
  (write-string ">" stream))

(defun print-modeline-field-info (obj stream ignore)
  (declare (ignore ignore))
  (write-string "#<Editor Modeline-field-info " stream)
  (prin1 (modeline-field-%name (ml-field-info-field obj)) stream)
  (write-string ">" stream))

(defvar *modeline-field-names* (make-hash-table))

(defun make-modeline-field (&key name width function (replace nil))
  "Return a modeline-field with $name, $width, and $function.

   If width is () the field is of variable width.

   $function must take a buffer and window as arguments and return a
   simple-string containing only standard characters.

   If $name already names a modeline-field, then signal an error."
  (if width
      (or (and (integerp width) (plusp width))
	  (error "Width must be () or a positive integer.")))
  (or replace
      (if (gethash name *modeline-field-names*)
	  (with-simple-restart (continue
				   "Use the new definition for this modeline field.")
	    (error "Modeline field ~S already exists."
		   (gethash name *modeline-field-names*)))))
  (setf (gethash name *modeline-field-names*)
	(%make-modeline-field name function width)))

(defun modeline-field (name)
  "Return the modeline-field named $name if such a field exists, else
   return ()."
  (gethash name *modeline-field-names*))

(proclaim '(inline modeline-field-name modeline-field-width
		   modeline-field-function))

(defun modeline-field-name (ml-field)
  "Return the name field of modeline-field $ml-field.

   If this is set with `setf', and the new name already names a
   modeline-field, then the `setf' method signals an error."
  (modeline-field-%name ml-field))

(defun %set-modeline-field-name (ml-field name)
  (check-type ml-field modeline-field)
  (when (gethash name *modeline-field-names*)
    (error "Modeline field ~S already exists."
	   (gethash name *modeline-field-names*)))
  (remhash (modeline-field-%name ml-field) *modeline-field-names*)
  (setf (modeline-field-%name ml-field) name)
  (setf (gethash name *modeline-field-names*) ml-field))

(defun modeline-field-width (ml-field)
  "Return the width to which modeline-field $ml-field is constrained, or
   () to indicate that it is of variable width.

   When this is set with `setf', the `setf' method updates all
   modeline-fields for all windows on all buffers that contain the given
   field, so the next trip through redisplay will reflect the change.  All
   the fields for any such modeline display must be updated."
  (modeline-field-%width ml-field))

(proclaim '(special *buffer-list*))

(defun %set-modeline-field-width (ml-field width)
  (check-type ml-field modeline-field)
  (or (eq width nil) (and (integerp width) (plusp width))
      (error "Width must be nil or a positive integer."))
  (unless (eql width (modeline-field-%width ml-field))
    (setf (modeline-field-%width ml-field) width)
    (dolist (b *buffer-list*)
      (when (buffer-modeline-field-p b ml-field)
	(dolist (w (buffer-windows b))
	  (update-modeline-fields b w)))))
  width)

(defun modeline-field-function (ml-field)
  "Return the function called when updating modeline-field $ml-field.

   When this is set with `setf', the `setf' method updates modeline-field
   for all windows on all buffers that contain the given field, so the next
   trip through redisplay will reflect the change.

   All modeline-field functions must return simple strings with standard
   characters, and take a buffer and a window as arguments."
  (modeline-field-%function ml-field))

(defun %set-modeline-field-function (ml-field function)
  (check-type ml-field modeline-field)
  (check-type function (or symbol function))
  (setf (modeline-field-%function ml-field) function)
  (dolist (b *buffer-list*)
    (when (buffer-modeline-field-p b ml-field)
      (dolist (w (buffer-windows b))
	(update-modeline-field b w ml-field))))
  function)


;;;; Modelines maintenance.

;;; Each window stores a modeline-buffer which is a string hunk-width-limit
;;; long.  Whenever a field is updated, we must maintain a maximally long
;;; representation of the modeline in case the window is resized.  Updating
;;; then first gets the modeline-buffer setup, and second blasts the necessary
;;; portion into the window's modeline-dis-line, setting the dis-line's changed
;;; flag.

(defparameter hunk-width-limit 256)  ;; Was in hacks.lisp.

(defun update-modeline-fields (buffer window)
  "Invoke each modeline-field function from $buffer's list, passing $buffer
   and $window.  Collect the results regarding each modeline-field's width
   as appropriate, and mark the window so the next trip through redisplay
   will reflect the changes."
  (let ((ml-buffer (window-modeline-buffer window)))
    (declare (simple-string ml-buffer))
    (when ml-buffer
      (let* ((ml-buffer-len
	      (do ((finfos (buffer-%modeline-fields buffer) (cdr finfos))
		   (start 0 (blt-modeline-field-buffer
			     ml-buffer (car finfos) buffer window start)))
		  ((null finfos) start)))
	     (dis-line (window-modeline-dis-line window))
	     (len (min (window-width window) ml-buffer-len)))
	(replace (the simple-string (dis-line-chars dis-line)) ml-buffer
		 :end1 len :end2 len)
	;; TODO Sortof tentative, for future fancy modeline.  For now the
	;; tty version is reading the first font change only, and the
	;; bitmap version is reading window-modeline-*-color directly.
	(setf (dis-line-font-changes dis-line)
	      (alloc-font-change 0 ; Position.
				 0 ; Font.
				 (window-modeline-fore-color window)
				 (window-modeline-back-color window)
				 t)) ; Dummy mark.
	(setf (window-modeline-buffer-len window) ml-buffer-len)
	(setf (dis-line-length dis-line) len)
	(setf (dis-line-flags dis-line) changed-bit)))))

;;; UPDATE-MODELINE-FIELD must replace the entire dis-line-chars with ml-buffer
;;; after blt'ing into buffer.  Otherwise it has to do all the work
;;; BLT-MODELINE-FIELD-BUFFER to figure out how to adjust dis-line-chars.  It
;;; isn't worth it.  Since things could have shifted around, after calling
;;; BLT-MODELINE-FIELD-BUFFER, we get the last field's end to know how long
;;; the buffer is now.
;;;
(defun update-modeline-field (buffer window field)
  "Invoke the modeline-field's update function of $field, which is a
   modeline-field or the name of one, for $buffer.  Pass $buffer and
   $window to the function.

   Apply the result to the window's modeline display using the
   modeline-field's width, and mark the window so the next trip through
   redisplay reflects the changes.

   If $field is missing from buffer's list of modeline-field, then signal
   an error."
  (let ((finfo (internal-buffer-modeline-field-p buffer field)))
    (or finfo
	(error "~S must be a modeline-field or the name of one for buffer ~S."
	       field buffer))
    (let ((ml-buffer (window-modeline-buffer window))
	  (dis-line (window-modeline-dis-line window)))
      (declare (simple-string ml-buffer))
      (blt-modeline-field-buffer ml-buffer finfo buffer window
				 (ml-field-info-start finfo) t)
      (let* ((ml-buffer-len (ml-field-info-end
			     (car (last (buffer-%modeline-fields buffer)))))
	     (dis-len (min (window-width window) ml-buffer-len)))
	(replace (the simple-string (dis-line-chars dis-line)) ml-buffer
		 :end1 dis-len :end2 dis-len)
	(setf (window-modeline-buffer-len window) ml-buffer-len)
	(setf (dis-line-length dis-line) dis-len)
	(setf (dis-line-flags dis-line) changed-bit)))))

(defvar *truncated-field-char* #\!)

;;; BLT-MODELINE-FIELD-BUFFER takes an editor buffer, editor window, the
;;; window's modeline buffer, a modeline-field-info object, a start in the
;;; modeline buffer, and an optional indicating whether a variable width
;;; field should be handled carefully.  When the field is fixed-width, this
;;; is simple.  When it is variable, we possibly have to shift all the text
;;; in the buffer right or left before storing the new string, updating all
;;; the finfo's after the one we're updating.  It is an error for the
;;; modeline-field-function to return anything but a simple-string with
;;; standard-chars.  This returns the end of the field blasted into
;;; ml-buffer.
;;;
(defun blt-modeline-field-buffer (ml-buffer finfo buffer window start
					    &optional fix-other-fields-p)
  (declare (simple-string ml-buffer))
  (let* ((f (ml-field-info-field finfo))
	 (width (modeline-field-width f))
	 (string (funcall (modeline-field-function f) buffer window))
	 (str-len (length string)))
    (declare (simple-string string))
    ;; FIX The (or ... 0) is a ~ guess hack, for when this is called before
    ;; the modeline is initialised, eg directly after make-buffer.
    (or start (setq start 0))
    (setf (ml-field-info-start finfo) start)
    (setf (ml-field-info-end finfo)
	  (cond
	   ((not width)
	    (let ((end (min (+ start str-len) hunk-width-limit))
		  ;; FIX Guess hack, as above.
		  (last-end (or (ml-field-info-end finfo) 0)))
	      (when (and fix-other-fields-p (/= end last-end))
		(blt-ml-field-buffer-fix ml-buffer finfo buffer window
					 end last-end))
	      (replace ml-buffer string :start1 start :end1 end :end2 str-len)
	      end))
	   ((= str-len width)
	    (let ((end (min (+ start width) hunk-width-limit)))
	      (replace ml-buffer string :start1 start :end1 end :end2 width)
	      end))
	   ((> str-len width)
	    (let* ((end (min (+ start width) hunk-width-limit))
		   (end-1 (1- end)))
	      (replace ml-buffer string :start1 start :end1 end-1 :end2 width)
	      (setf (schar ml-buffer end-1) *truncated-field-char*)
	      end))
	   (t
	    (let ((buf-replace-end (min (+ start str-len) hunk-width-limit))
		  (buf-field-end (min (+ start width) hunk-width-limit)))
	      (replace ml-buffer string
		       :start1 start :end1 buf-replace-end :end2 str-len)
	      (fill ml-buffer #\space :start buf-replace-end :end buf-field-end)
	      buf-field-end))))))

;;; BLT-ML-FIELD-BUFFER-FIX shifts the contents of ml-buffer in the direction
;;; of last-end to end.  finfo is a modeline-field-info structure in buffer's
;;; list of these.  If there are none following finfo, then we simply store the
;;; new end of the buffer.  After blt'ing the text around, we have to update
;;; all the finfos' starts and ends making sure nobody gets to stick out over
;;; the ml-buffer's end.
;;;
(defun blt-ml-field-buffer-fix (ml-buffer finfo buffer window end last-end)
  (declare (simple-string ml-buffer))
  (let ((finfos (do ((f (buffer-%modeline-fields buffer) (cdr f)))
		    ((null f) (error "This field must be here."))
		  (if (eq (car f) finfo)
		      (return (cdr f))))))
    (cond
     ((not finfos)
      (setf (window-modeline-buffer-len window) (min end hunk-width-limit)))
     (t
      (let ((buffer-len (window-modeline-buffer-len window)))
	(replace ml-buffer ml-buffer
		 :start1 end
		 :end1 (min (+ end (- buffer-len last-end)) hunk-width-limit)
		 :start2 last-end :end2 buffer-len)
	(let ((diff (- end last-end)))
	  (macrolet ((frob (f)
		       ;; FIX the (or ... 0) is a hack guess, as in blt-m-f-b above.
		       `(setf ,f (min (+ (or ,f 0) diff) hunk-width-limit))))
	    (dolist (f finfos)
	      (frob (ml-field-info-start f))
	      (frob (ml-field-info-end f)))
	    (frob (window-modeline-buffer-len window)))))))))


;;;; Pre-defined modeline and update hooks.

(make-modeline-field
 :name :package :replace t
 :function #'(lambda (buffer window)
	       "Return the value of $buffer's *Current Package* followed by
	        a colon and two spaces, or a string with one space."
	       (declare (ignore window))
	       (if (editor-bound-p 'ed::current-package :buffer buffer)
		   (let ((val (variable-value 'ed::current-package
					      :buffer buffer)))
		     (if val
			 (format nil "~A:  " val)
			 " "))
		   " ")))

(make-modeline-field
 :name :modes :replace t
 :function #'(lambda (buffer window)
	       "Return $buffer's modes."
	       (declare (ignore window))
	       (format nil "~A"
		       (mapcar (lambda (mode)
				 (let ((mode (getstring mode
							*mode-names*)))
				   (or (mode-object-short-name mode)
				       (mode-object-name mode)
				       "")))
			       (buffer-modes buffer)))))

(make-modeline-field
 :name :modifiedp :replace t
 :function #'(lambda (buffer window)
	       "Return \"* \" if $buffer is modified, or the empty string."
	       (declare (ignore window))
	       (let ((modifiedp (buffer-modified buffer)))
		 (if modifiedp
		     "* "
		     ""))))

(make-modeline-field
 :name :buffer-name :replace t
 :function #'(lambda (buffer window)
	       "Return $buffer's name followed by a colon and a space if
	        the name is not derived from the buffer's pathname, or the
	        empty string."
	       (declare (ignore window))
	       (let ((pn (buffer-pathname buffer))
		     (name (buffer-name buffer)))
		 (cond ((not pn)
			(format nil "~A: " name))
		       ((string/= (ed::pathname-to-buffer-name pn) name)
			(format nil "~A: " name))
		       (t "")))))

;;; MAXIMUM-MODELINE-PATHNAME-LENGTH-HOOK is called whenever "Maximum Modeline
;;; Pathname Length" is set.
;;;
(defun maximum-modeline-pathname-length-hook (name kind where new-value)
  (declare (ignore name new-value))
  (if (eq kind :buffer)
      (edi::queue-buffer-change where)
      (dolist (buffer *buffer-list*)
	(when (and (buffer-modeline-field-p buffer :buffer-pathname)
		   (buffer-windows buffer))
	  (edi::queue-buffer-change buffer)))))

(defun buffer-pathname-ml-field-fun (buffer window)
  "Return the namestring of buffer's pathname if there is one.  When
   *Maximum Modeline Pathname Length* is set, and the namestring is too
   long, return a truncated namestring chopping off leading directory
   specifications."
  (declare (ignore window))
  (let ((pn (buffer-pathname buffer)))
    (if pn
	(let* ((name (namestring pn))
	       (length (length name))
	       ;; FIX?
	       ;; Prefer a buffer local value over the global one.
	       ;; Because variables don't work right, blow off looking for
	       ;; a value in the buffer's modes.  In the future this will
	       ;; be able to get the "current" value as if buffer were current.
	       (max (if (editor-bound-p 'ed::maximum-modeline-pathname-length
					  :buffer buffer)
			 (variable-value 'ed::maximum-modeline-pathname-length
					 :buffer buffer)
			 (variable-value 'ed::maximum-modeline-pathname-length
					 :global))))
	  (declare (simple-string name))
	  (if (or (not max) (<= length max))
	      name
	      (let* ((extra-chars (+ (- length max) 3))
		     (slash (or (position #\/ name :start extra-chars)
				;; If no slash, then file-namestring is very
				;; long, and we should include all of it:
				(position #\/ name :from-end t
					  :end extra-chars))))
		(if slash
		    (concatenate 'simple-string "..." (subseq name slash))
		    name))))
	"")))

(make-modeline-field
 :name :buffer-pathname :replace t
 :function 'buffer-pathname-ml-field-fun)

(make-modeline-field
 :replace t
 :name :buffer-state
 :function #'(lambda (buffer window)
	       "Return \"* \" if $buffer is modified, else \"  \"."
	       (declare (ignore window))
	       (if (buffer-writable buffer)
		   (if (buffer-modified buffer)
		       "** "
		       "   ")
		   (if (buffer-modified buffer)
		       "%* "
		       "%% "))))

(make-modeline-field
 :replace t
 :name :buffer-short-name
 :function #'(lambda (buffer window)
	       "Return a short version of $buffer's name followed by a space."
	       (declare (ignore window))
	       (let* ((pn (buffer-pathname buffer))
		      (name (buffer-name buffer))
		      ;; FIX should ml-name be a flet, macrolet or variable
		      ;;     ~ is there any generic way to tell?
		      ;;     ~~ should there be only one way to do it?
		      (ml-name (if (and pn
					(string= (ed::pathname-to-buffer-name pn)
						 name))
				   (let ((type (pathname-type pn)))
				     (format () "~A~A~A"
					     (pathname-name pn)
					     (if type "." "")
					     (if type type "")))
				   (format () "~A" name)))
		      ;; FIX as in buffer-pathname-ml-field-fun above
		      (max (if (editor-bound-p 'ed::maximum-modeline-short-name-length
					       :buffer buffer)
			       (variable-value 'ed::maximum-modeline-short-name-length
					       :buffer buffer)
			       (variable-value 'ed::maximum-modeline-short-name-length
					       :global))))
		 (if (and max (> (length ml-name) max))
		     (concat (string-right-trim '(#\space #\tab)
						(subseq ml-name 0 max))
			     (string *truncated-field-char*) " ")
		     (concat ml-name " ")))))

(make-modeline-field
 :replace t
 :name :position
 :function #'(lambda (buffer window)
	       "Return the point positions \"(<column>,<$buffer line $number>)\"."
	       (let ((point (buffer-point buffer)))
		 (format nil "(~2,'0D,~D)"
			 (mark-column point)
			 (window-line-number window)))))

(make-modeline-field
 :replace t
 :name :column
 :function #'(lambda (buffer window)
	       "Return the column $buffer's point is on, prefixed with a 'C'."
	       (declare (ignore window))
	       (format nil "C~2,'0D" (mark-column (buffer-point buffer)))))

(make-modeline-field
 :replace t
 :name :line
 :function #'(lambda (buffer window)
	       "Return the line $buffer's point is on, prefixed with an 'L'."
	       (declare (ignore buffer))
	       (format nil "L~A" (window-line-number window))))

(make-modeline-field
 :replace t
 :name :%
 :function #'(lambda (buffer window)
	       "Return the percent of the $buffer before point, suffixed
		with a '%'."
	       (format nil "~A%"
		       (if (zerop (buffer-line-count buffer))
			   100
			   (truncate
			    (* (/ (window-line-number window)
				  (buffer-line-count buffer))
			       100))))))

(make-modeline-field
 :replace t
 :name :space
 :function #'(lambda (buffer window)
	       "Return a space."
	       (declare (ignore buffer window))
	       " "))

(make-modeline-field
 :replace t
 :name :doublespace
 :function #'(lambda (buffer window)
	       "Return two spaces."
	       (declare (ignore buffer window))
	       "  "))

(defun update-modeline-column-field (window)
  (or (eq window *echo-area-window*)
      (update-modeline-field (window-buffer window)
			     window
			     (modeline-field :column))))

(defun update-modeline-position-field (window)
  "Update position field in modeline of Window."
  (or (eq window *echo-area-window*)
      (let ((buffer (window-buffer window))
	    (field (modeline-field :position)))
	(when (buffer-modeline-field-p buffer field)
	  (update-modeline-field buffer window field)))))

(defun update-modeline-buffer-state-field (window)
  "Update buffer state field in modeline of Window."
  (or (eq window *echo-area-window*)
      (let ((buffer (window-buffer window)))
	(or (eq buffer (window-buffer *echo-area-window*))
	    (let ((field (modeline-field :buffer-state)))
	      (when (buffer-modeline-field-p buffer field)
		(update-modeline-field buffer window field)))))))

(defun update-all-modeline-buffer-state-fields (buffer flag)
  (declare (ignore flag))
  (let ((field (modeline-field :buffer-state)))
    (dolist (window (buffer-windows buffer))
      (or (eq window *echo-area-window*)
	  (when (buffer-modeline-field-p buffer field)
	    (update-modeline-field buffer window field))))))

(defun update-all-modeline-fields (window)
  "Update all modeline fields in Window."
  (update-modeline-fields (window-buffer window) window))

(defun update-line-number (buffer &optional exist)
  "Update the line-count field of Buffer and the line-number field of all
   windows into Buffer."
  (declare (ignore exist))
  (setf (buffer-line-count buffer)
	(count-lines (buffer-region buffer)))
  (dolist (window (buffer-windows buffer))
    (setf (window-line-number window)
	  (count-lines (region (buffer-start-mark buffer)
			       (window-point window))))))

(defun update-line-numbers ()
  "Update the line-number field of every window and the line-count field of
   every visible buffer."
  (dolist (window *window-list*)
    (or (equal window *echo-area-window*)
	(let ((buffer (window-buffer window)))
	  (setf (window-line-number window)
		(count-lines (region (buffer-start-mark buffer)
				     (window-point window))))
	  (update-all-modeline-fields window)))))

;;; QUEUE-BUFFER-CHANGE is used for various buffer hooks (e.g., mode changes,
;;; name changes, etc.), so it takes some arguments to ignore.  These hooks are
;;; invoked at a bad time to update the actual modeline-field, and user's may
;;; have fields that change as a function of the changes this function handles.
;;; This makes his update easier.  It doesn't cost much to update the entire line
;;; anyway.
;;;
(defun queue-buffer-change (buffer &optional something-else another-else)
  (declare (ignore something-else another-else))
  (push (list #'update-modelines-for-buffer buffer) *things-to-do-once*))

(defun update-modelines-for-buffer (buffer)
  (unless (eq buffer *echo-area-buffer*)
    (dolist (w (buffer-windows buffer))
      (update-modeline-fields buffer w))))

;;; QUEUE-WINDOW-CHANGE is used for the "Window Buffer Hook".  We ignore the
;;; argument since this hook function is invoked before any changes are made,
;;; and the changes must be made before the fields can be set according to the
;;; window's buffer's properties.  Therefore, we must queue the change to
;;; happen sometime before redisplay but after the change takes effect.
;;;
(defun queue-window-change (window &optional something-else)
  (declare (ignore something-else))
  (push (list #'update-modeline-for-window window) *things-to-do-once*))

(defun update-modeline-for-window (window)
  (update-modeline-fields (window-buffer window) window))


;;;; Modeline fields and update functions intended for the status line.

(make-modeline-field
 :replace t
 :name :user
 :function #'(lambda (buffer window)
	       "Return the user name associated with the current process."
	       (declare (ignore buffer window))
	       (user-name)))

(make-modeline-field
 :replace t
 :name :machine-instance
 :function #'(lambda (buffer window)
	       "Return the machine instance (the hostname)."
	       (declare (ignore buffer window))
	       (machine-instance)))

(make-modeline-field
 :replace t
 :name :user-at-machine
 :function #'(lambda (buffer window)
	       "Return the current user name at the machine name, e.g.
	        name@host."
	       (declare (ignore buffer window))
	       (format () "~A@~A" (user-name) (machine-instance))))

(make-modeline-field
 :replace t
 :name :busy
 :width 4
 :function #'(lambda (buffer window)
	       "Return \"BUSY\" if *busy*."
	       (declare (ignore buffer window))
	       (if *busy* "BUSY" "    ")))

(make-modeline-field
 :replace t
 :name :busy-or-menu
 :width 4
 :function #'(lambda (buffer window)
	       "Return \"BUSY\" if *busy*, else \"Menu\"."
	       (declare (ignore buffer window))
	       (if *busy* "BUSY" "Menu")))

(make-modeline-field
 :replace t
 :name :time
 :function #'(lambda (buffer window)
	       "Return the time in \"21h03\" format."
	       (declare (ignore buffer window))
	       (multiple-value-bind (sec min hr date month yr day ds tz)
				    (get-decoded-time)
		 (declare (ignore sec min hr date month yr day ds))
		 (multiple-value-bind (sec min hr date month yr day ds tz)
				      (get-decoded-time (- (* tz 60)))
		   (declare (ignore sec date month yr day ds tz))
		   (format () "~2Dh~2,'0D" hr min)))))

(make-modeline-field
 :replace t
 :name :date
 :function #'(lambda (buffer window)
	       "Return a date string in \"Thu May 02 \" format."
	       (declare (ignore buffer window))
	       (multiple-value-bind (sec min hr date month yr day ds tz)
				    (get-decoded-time)
		 (declare (ignore sec min hr yr ds tz))
		 (format nil
			 "~A ~A ~A~A "
			 (svref extensions::abbrev-weekday-table day)
			 (svref extensions::abbrev-month-table (1- month))
			 (if (> date 9) "" "0") date))))

(declaim (special ed::*new-mail-p*))

(make-modeline-field
 :replace t
 :name :mail
 :width 4
 :function #'(lambda (buffer window)
	       "Return \"Mail\" if there is new mail."
	       (declare (ignore buffer window))
	       (if ed::*new-mail-p* "Mail" "    ")))


;;;; Bitmap setting up new windows and modifying old.

(defvar dummy-line (make-window-dis-line "")
  "Dummy dis-line that we put at the head of window's dis-lines")
(setf (dis-line-position dummy-line) -1)

;;; WINDOW-FOR-HUNK makes an editor window and sets up its dis-lines and
;;; marks to display starting at start.
;;;
(defun window-for-hunk (hunk start modelinep)
  (check-type start mark)
  (setf (bitmap-hunk-changed-handler hunk) #'window-changed)
  (let ((buffer (line-buffer (mark-line start)))
	(first (cons dummy-line the-sentinel))
	(width (bitmap-hunk-char-width hunk))
	(height (bitmap-hunk-char-height hunk)))
    (when (or (< height minimum-window-lines)
	      (< width minimum-window-columns))
      (error "Window too small."))
    (or buffer (error "Window start must be in a buffer."))
    (let ((window
	   (internal-make-window
	    :hunk hunk
	    :display-start (copy-mark start :right-inserting)
	    :old-start (copy-mark start :temporary)
	    :display-end (copy-mark start :right-inserting)
	    :%buffer buffer
	    :point (copy-mark (buffer-point buffer))
	    :height height
	    :width width
	    :first-line first
	    :last-line the-sentinel
	    :first-changed the-sentinel
	    :last-changed first
	    :display-recentering t
	    :tick -1)))
      (push window *window-list*)
      (push window (buffer-windows buffer))
      ;;
      ;; Make the dis-lines.
      (do ((i (- height) (1+ i))
	   (res ()
		(cons (make-window-dis-line (make-string width)) res)))
	  ((= i height) (setf (window-spare-lines window) res)))
      ;;
      ;; Make the image up to date.
      (update-window-image window)
      (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
      ;;
      ;; If there is a modeline, set it up.
      (when modelinep
	(setup-modeline-image buffer window)
	(setf (bitmap-hunk-modeline-dis-line hunk)
	      (window-modeline-dis-line window)))
      window)))

;;; SETUP-MODELINE-IMAGE sets up the modeline-dis-line for window using the
;;; modeline-fields list.  This is used by tty redisplay too.
;;;
(defun setup-modeline-image (buffer window)
  (setf (window-modeline-buffer window) (make-string hunk-width-limit))
  (setf (window-modeline-dis-line window)
	(make-window-dis-line (make-string (window-width window))))
  (setf (window-modeline-fore-color window)
	(color (or (value ed::initial-modeline-foreground-color)
		   (value ed::initial-background-color)
		   '(0 0 0)))) ; Black.
  (setf (window-modeline-back-color window)
	(color (or (value ed::initial-modeline-background-color)
		   (value ed::initial-foreground-color)
		   '(1 1 1)))) ; White.
  (update-modeline-fields buffer window))

;;; Window-Changed  --  Internal
;;;
;;; The bitmap-hunk changed handler for windows.  This is only called if
;;; the hunk is not locked.  We invalidate the window image and change its
;;; size, then do a full redisplay.
;;;
(defun window-changed (hunk)
  (let ((window (bitmap-hunk-window hunk)))
    ;;
    ;; Nuke all the lines in the window image.
    (unless (eq (cdr (window-first-line window)) the-sentinel)
      (shiftf (cdr (window-last-line window))
	      (window-spare-lines window)
	      (cdr (window-first-line window))
	      the-sentinel))
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
    ;;
    ;; Add some new spare lines if needed.  If width is greater,
    ;; reallocate the dis-line-chars.
    (let* ((res (window-spare-lines window))
	   (new-width (bitmap-hunk-char-width hunk))
	   (new-height (bitmap-hunk-char-height hunk))
	   (width (length (the simple-string (dis-line-chars (car res))))))
      (declare (list res))
      (when (> new-width width)
	(setq width new-width)
	(dolist (dl res)
	  (setf (dis-line-chars dl) (make-string new-width))))
      (setf (window-height window) new-height (window-width window) new-width)
      (do ((i (- (* new-height 2) (length res)) (1- i)))
	  ((minusp i))
	(push (make-window-dis-line (make-string width)) res))
      (setf (window-spare-lines window) res)
      ;;
      ;; Force modeline update.
      (let ((ml-buffer (window-modeline-buffer window)))
	(when ml-buffer
	  (let ((dl (window-modeline-dis-line window))
		(chars (make-string new-width))
		(len (min new-width (window-modeline-buffer-len window))))
	    (setf (dis-line-old-chars dl) nil)
	    (setf (dis-line-chars dl) chars)
	    (replace chars ml-buffer :end1 len :end2 len)
	    (setf (dis-line-length dl) len)
	    (setf (dis-line-flags dl) changed-bit)))))
    ;;
    ;; Prepare for redisplay.
    (setf (window-tick window) (tick))
    (update-window-image window)
    (when (eq window *current-window*) (maybe-recenter-window window))
    hunk))


;;; EDITOR-FINISH-OUTPUT is used to synch output to a window with the rest of the
;;; system.
;;;
(defun editor-finish-output (window)
  "Ensure the editor is synchronized with respect to redisplay output to
   $WINDOW."
  (let* ((device (device-hunk-device (window-hunk window)))
	 (finish-output (device-finish-output device)))
    (if finish-output
	(funcall finish-output device window))))


;;;; Tty setting up new windows and modifying old.

;;; setup-window-image  --  Internal
;;;
;;; Set up the dis-lines and marks for Window to display starting at Start.
;;; Height and Width are the number of lines and columns in the window.
;;;
(defun setup-window-image (start window height width)
  (check-type start mark)
  (let ((buffer (line-buffer (mark-line start)))
	(first (cons dummy-line the-sentinel)))
    (unless buffer (error "Window start is not in a buffer."))
    (setf (window-display-start window) (copy-mark start :right-inserting)
	  (window-old-start window) (copy-mark start :temporary)
	  (window-display-end window) (copy-mark start :right-inserting)
	  (window-%buffer window) buffer
	  (window-point window) (copy-mark (buffer-point buffer))
	  (window-height window) height
	  (window-width window) width
	  (window-first-line window) first
	  (window-last-line window) the-sentinel
	  (window-first-changed window) the-sentinel
	  (window-last-changed window) first
	  (window-tick window) -1)
    (push window *window-list*)
    (push window (buffer-windows buffer))
    ;;
    ;; Make the dis-lines.
    (do ((i (- height) (1+ i))
	 (res ()
	      (cons (make-window-dis-line (make-string width)) res)))
	((= i height) (setf (window-spare-lines window) res)))
    ;;
    ;; Make the image up to date.
    (update-window-image window)))

;;; change-window-image-height  --  Internal
;;;
;;;    (Soya)Milkshake.
;;;
(defun change-window-image-height (window new-height)
  ;; Nuke all the lines in the window image.
  (unless (eq (cdr (window-first-line window)) the-sentinel)
    (shiftf (cdr (window-last-line window))
	    (window-spare-lines window)
	    (cdr (window-first-line window))
	    the-sentinel))
  ;; Add some new spare lines if needed.
  (let* ((res (window-spare-lines window))
	 (width (length (the simple-string (dis-line-chars (car res))))))
    (declare (list res))
    (setf (window-height window) new-height)
    (do ((i (- (* new-height 2) (length res)) (1- i)))
	((minusp i))
      (push (make-window-dis-line (make-string width)) res))
    (setf (window-spare-lines window) res)))
