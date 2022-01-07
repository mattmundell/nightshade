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
  "Flex hands.  Sit up straight.  Look around.  Done? ")

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


;;;; Commands.

;;; cancel-subrest-reminder  --  Internal
;;;
(defun cancel-subrest-reminder ()
  (when *rest-micro-timer*
    (remove-scheduled-event (edi::tq-event-function *rest-micro-timer*))
    (setq *rest-micro-timer* ())))

(defcommand "Reset Rest Reminder" (mins (subrest t))
  "Reset the rest timer.  With a prefix remind in prefix minutes."
  "Reset the rest timer."
  (cancel-rest-reminder-command)
  (when (value rest-work-period)
    (setq *rest-timer*
	  (schedule-event
	   (* 60 (or mins (value rest-work-period)))
	   (lambda (secs)
	     (declare (ignore secs))
	     (cancel-subrest-reminder)
	     (if (value rest-beep) (beep))
	     (if (let (response)
		   (unwind-protect
		       (prog1
			 (prompt-for-y-or-n
			  :prompt (value rest-prompt)
			  :default t)
			 (setq response t))
		     (or response
			 (reset-rest-reminder-command 3 ()))))
		 (rest-command)
		 (if (value rest-reremind-period)
		     (reset-rest-reminder-command
		      (value rest-reremind-period)
		      ())
		     (message "OK.  M-x Rest to initiate the rest later."))))
	   () ; repeat
	   ()))) ; absolute
  (if subrest (add-subrest-reminder)))

;;; add-subrest-reminder  --  Internal
;;;
;;; Schedule a subrest reminder event for *Rest Work Subperiod* minutes
;;; time.
;;;
(defun add-subrest-reminder ()
  (when (value rest-work-subperiod)
    (setq *rest-micro-timer*
	  (schedule-event
	   (* 60 (value rest-work-subperiod))
	   (lambda (secs)
	     (declare (ignore secs))
	     (if (value rest-beep) (beep))
	     (unwind-protect
		 (until ()
			((prompt-for-y-or-n
			  :prompt (value rest-subreminder-prompt)
			  :default-string "Y"
			  :default t)))
	       (add-subrest-reminder)))
	   () ; repeat
	   ())))) ; absolute

(defcommand "Cancel Rest Reminder" ()
  "Cancel periodic rest reminding."
  (when *rest-timer*
    (remove-scheduled-event (edi::tq-event-function *rest-timer*))
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
	 (until () ((prompt-for-y-or-n
		     :prompt (value rest-initial-prompt)
		     :default t
		     :default-string "Y")))
       (if on (reset-rest-reminder-command))))))
