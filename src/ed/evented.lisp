;;; Event editor.
;;;
;;; FIX also update evented buffer when an event runs maybe via hook in
;;; invoke-scheduled-events

(in-package "ED")


;;;; Listing events.

(defun list-events (events stream)
  (multiple-value-bind (sec min hr date month yr day ds tz)
		       (get-decoded-time)
    (declare (ignore sec min hr date month yr day ds))
    (flet ((format-event-time (event)
	     ; FIX if today just print time, if this year just print month and day
	     (format-universal-time ; FIX format-internal-time?
	      ()
	      (internal-real-to-universal-time
	       (edi::tq-event-time event))
	      :timezone (- (* tz 60))
	      :style :rfc1123
	      :print-weekday nil
	      :print-timezone nil)))
      (mapc (lambda (event)
	      (let ((period (edi::tq-event-interval event)))
		(format stream "~:[Once at ~A~;Every ~A secs from ~A~]: ~A"
			period
			(if period
			    (/ period internal-time-units-per-second)
			    (format-event-time event))
			(if period
			    (format-event-time event)
			    (edi::tq-event-function event))
			(edi::tq-event-function event)))
	      (write-char #\newline stream))
	    events))))

(defcommand "List Events" ()
  "List events."
  (with-pop-up-display (stream)
    (list-events edi::*time-queue* stream)))


;;;; Editing events.

(defvar *evented-buffer* nil
  "The Evented buffer.")
(defvar *evented-events* nil
  "The events listed in the Evented buffer.  All events for now.")

(defmode "Evented" :major-p t
  :documentation
  "Evented is a mode for editing the current events.")

(defun delete-evented-buffers (buffer)
  (when (eq buffer *evented-buffer*)
    (setf *evented-buffer* nil)
    (setf *evented-events* nil)))

(defcommand "Edit Events" (p)
  "Create or switch to the event editing buffer."
  (evented-command p))

(defcommand "Evented" ()
  "Create or switch to the event editing buffer."
  (let ((buf (or *evented-buffer*
		 (make-buffer "Evented" :modes '("Evented")
			      :delete-hook (list #'delete-evented-buffers)))))
    (or *evented-buffer*
	(progn
	  (setq *evented-buffer* buf)
	  (setq *evented-events* edi::*time-queue*)
	  (setf (buffer-writable buf) t)
	  (with-output-to-mark (stream (buffer-point buf))
	    (list-events *evented-events* stream))
	  (setf (buffer-writable buf) nil)
	  (setf (buffer-modified buf) nil)
	  (let ((fields (buffer-modeline-fields *evented-buffer*)))
	    (setf (cdr (last fields))
		  (list (or (modeline-field :evented-cmds)
			    (make-modeline-field
			     :name :evented-cmds :width 18
			     :function
			     #'(lambda (buffer window)
				 (declare (ignore buffer window))
				 "  Type ? for help.")))))
	    (setf (buffer-modeline-fields *evented-buffer*) fields))
	  (buffer-start (buffer-point buf))))
    (change-to-buffer buf)))

;; FIX rotate instead? (same in pack*ed)
(defcommand "Evented Quit" ()
  "Kill the Evented buffer."
  (when *evented-buffer* (delete-buffer-if-possible *evented-buffer*)))

(defcommand "Evented Help" ()
  "Show Evented mode help."
  (describe-mode-command nil "Evented"))

(defcommand "Evented Refresh" (p (buffer (current-buffer)))
  "Refresh the current buffer."
  (declare (ignore p))
  (when (string= (buffer-major-mode buffer) "Evented")
    (setf (buffer-writable buffer) t)
    (setq *evented-events* edi::*time-queue*)
    (delete-region (buffer-region buffer))
    (with-output-to-mark (stream (buffer-point buffer))
      (list-events *evented-events* stream))
    (setf (buffer-writable buffer) nil)
    (setf (buffer-modified buffer) nil)
    (buffer-start (buffer-point buffer))))

(defun event-at-mark (mark)
  (when (blank-line-p (mark-line mark))
    (editor-error "Point must be on an event line."))
  (nth (1- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark)))
				mark)))
       *evented-events*))

(defcommand "Evented Cancel Event" ()
  "Cancel the event at point."
  (when (prompt-for-y-or-n
       :prompt "Cancel event? "
       :default nil :must-exist t)
    (remove-scheduled-event
     (event-at-mark (current-point)))
    (evented-refresh-command)))

(defun refresh-evented-buffer (&optional event)
  (declare (ignore event))
  (if *evented-buffer*
      (evented-refresh-command nil *evented-buffer*)))

(add-hook schedule-event-hook #'refresh-evented-buffer)
