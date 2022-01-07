;; Periodic break reminding.

(in-package "ED")

(defvar *break-timer* nil)

(defhvar "Work Period"
  "Minutes between break reminders."
  :value 60)

(defhvar "Break Reremind Period"
  "Minutes between break reminders after first reminder."
  :value 5)

(defcommand "Reset Break Reminder" (mins)
  "Reset the break timer.  With a prefix remind in prefix minutes."
  "Reset the break timer."
  (cancel-break-reminder-command nil)
  (setq *break-timer*
	(schedule-event
	 (+ (get-universal-time) (* 60 (or mins (value work-period))))
	 (lambda (secs)
	   (declare (ignore secs))
	   (message "Break.")
	   (if (value break-reremind-period)
	       (reset-break-reminder-command
		(value break-reremind-period))))
	 nil t)))

(defcommand "Cancel Break Reminder" (p)
  "Cancel periodic break reminding."
  "Cancel periodic break reminding."
  (declare (ignore p))
  (when *break-timer*
    (remove-scheduled-event (hi::tq-event-function *break-timer*))
    (setq *break-timer* nil)))

(defcommand "Break" (p)
  "Initiate a break."
  "Initiate a break."
  (declare (ignore p))
  (clear-echo-area)
  (redisplay)
  (with-pop-up-display (stream)
    (dotimes (x 20)
      (terpri stream))
    (format stream "break")
    (dotimes (x 20)
      (terpri stream)))
  (reset-break-reminder-command nil))

(defcommand "Break Status" (p)
  "Print the break status."
  "Print the break status."
  (declare (ignore p))
  (message "Next break: ~A"
	   (format-universal-time
	    nil
	    (internal-real-to-universal-time (hi::tq-event-time *break-timer*))
	    :print-seconds nil
	    :print-meridian nil
	    :print-timezone nil
	    :print-weekday nil)))
