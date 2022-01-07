;; Periodic rest reminding.

(in-package "ED")

(defvar *rest-timer* ())
(defvar *rest-micro-timer* ())


;;;; Variables.

(defevar "Rest Work Period"
  "Minutes between rest reminders.  If () rest reminding is off."
  :value 50)

(defevar "Rest Reremind Period"
  "Minutes between rest reminders after first reminder."
  :value 5)

(defevar "Rest Work Subperiod"
  "Minutes between micro-rest reminders.  If () micro-rest reminding is off."
  :value 5)

(defevar "Rest Warmup"
  "If true prompt with a warmup reminder on startup.")

(defevar "Rest Beep"
  "If true beep when reminding and promptin about rests."
  :value t)

(defevar "Rest Subreminder Prompt"
  "The reminder prompt for subrests."
  :value
  "Flex hands.  Sit up straight.  Look away.  Done? ")

(defevar "Rest Initial Prompt"
  "The initial reminder prompt."
  :value
  "Warmup hands.  Done? ")

(defevar "Rest Prompt"
  "The prompt for rests."
  :value
  "Rest now? (y/n) ")

(defevar "Rest Message"
  "The message for the rest screen."
  :value
  "  Stand up and move away from the screen.")

(defvar *rest-retry-period* 5
  "The number of seconds between reminder reattempts when point is in the
   Echo window.")


;;;; Commands.

;;; cancel-subrest-reminder  --  Internal
;;;
(defun cancel-subrest-reminder ()
  (when *rest-micro-timer*
    (remove-scheduled-event *rest-micro-timer*)
    (setq *rest-micro-timer* ())))

(defcommand "Reset Rest Reminder" (secs (subrest t))
  "Reset the rest timer.  With a prefix remind in prefix seconds."
  "Reset the rest timer."
  (let ((secs (or secs (if (value rest-work-period)
			   (* 60 (value rest-work-period))))))
    (cancel-rest-reminder-command)
    (when secs
      (setq *rest-timer*
	    (schedule-event
	     secs
	     (lambda (secs-since)
	       (declare (ignore secs-since))
	       (cancel-subrest-reminder)
	       (if (or (eq (current-window) *echo-area-window*)
		       (plusp (length edi::*current-command*))
		       *defining-a-keyboard-macro*)
		   (reset-rest-reminder-command *rest-retry-period* ())
		   (progn
		     (if (value rest-beep) (beep))
		     (if (let (response)
			   (unwind-protect
			       (prog1
				   (prompt-for-y-or-n
				    :prompt (value rest-prompt)
				    :default t)
				 (setq response t))
			     (or response
				 (reset-rest-reminder-command 60 ()))))
			 (rest-command)
			 (if (value rest-reremind-period)
			     (reset-rest-reminder-command
			      (* 60 (value rest-reremind-period))
			      ())
			     (message "OK.  `Rest' initiates the rest later."))))))
	     () ; repeat
	     ())) ; absolute
      (if subrest (add-subrest-reminder)))))

;;; add-subrest-reminder  --  Internal
;;;
;;; Schedule a subrest reminder event for *Rest Work Subperiod* minutes
;;; time.
;;;
(defun add-subrest-reminder (&optional secs)
  (let ((secs (or secs (if (value rest-work-subperiod)
			   (* 60 (value rest-work-subperiod))))))
    (when secs
      (setq *rest-micro-timer*
	    (schedule-event
	     secs
	     (lambda (secs-since)
	       (declare (ignore secs-since))
	       (let ((secs))
		 (unwind-protect
		     (if (or (eq (current-window) *echo-area-window*)
			     (plusp (length edi::*current-command*))
			     *defining-a-keyboard-macro*)
			 ; Try again in a short while.
			 (setq secs *rest-retry-period*)
			 (progn
			   (if (value rest-beep) (beep))
			   (until ()
				  ((catch-cancel
				    (prompt-for-y-or-n
				     :prompt
				     (value rest-subreminder-prompt)
				     :default-string "Y"
				     :default t
				     :help "Rest, then type Y."))))))
		   (add-subrest-reminder secs))))
	     () ; repeat
	     ()))))) ; absolute

(defcommand "Cancel Rest Reminder" ()
  "Cancel periodic rest reminding."
  (when *rest-timer*
    (remove-scheduled-event *rest-timer*)
    (setq *rest-timer* ()))
  (cancel-subrest-reminder))

(defcommand "Rest" ()
  "Initiate a rest."
  (clear-echo-area)
  (redisplay)
  (with-pop-up-display (stream)
    (dotimes (x 20)
      (terpri stream))
    (format stream (value rest-message))
    (dotimes (x 20)
      (terpri stream)))
  (reset-rest-reminder-command))

(defcommand "Rest Status" ()
  "Print the rest status."
  (if *rest-timer*
      (message "Next rest: ~A"
	       (multiple-value-bind (sec min hr date month yr day ds tz)
				    (get-decoded-time)
		 (declare (ignore sec min hr date month yr day ds))
		 (format-universal-time
		  nil
		  (internal-real-to-universal-time (edi::tq-event-time *rest-timer*))
		  :style :rfc1123
		  :timezone (- (* tz 60))
		  :print-seconds nil
		  :print-meridian nil
		  :print-timezone nil
		  :print-weekday nil)))
      (message "Rest reminding is off.  Enable with M-x Rest.")))


;;;; Initialization.

(after-editor-initializations
 (when (value rest-warmup)
   (let ((on (prog1 (or *rest-timer* *rest-micro-timer*)
	       (cancel-rest-reminder-command))))
     (unwind-protect
	 (until () ((catch-cancel
		     (prompt-for-y-or-n
		      :prompt (value rest-initial-prompt)
		      :default t
		      :default-string "Y"))))
       (if on (reset-rest-reminder-command))))))

(add-hook resume-hook 'reset-rest-reminder-command)
