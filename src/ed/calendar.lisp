;;; Calendar, diary and reminders.

(in-package "ED")


;;;; Highlighting.

(defun highlight-calendar-line (line chi-info)
  (let ((mark (mark line 0)))
    (when (next-character mark)
      (if (char= (next-character mark) #\M)
	  ;; Day of week heading.
	  (chi-mark line 0 *comment-font* :comment chi-info)
	  (progn
	    (find-attribute mark :whitespace #'zerop)
	    (let ((char (next-character mark)))
	      (cond ((digit-char-p char))
		    (t
		     ;; Month and year heading.
		     (chi-mark line 0 *string-font*
			       :string chi-info)))))))))

(defun highlight-calendar (buffer)
  (highlight-chi-buffer buffer highlight-calendar-line))

(defun highlight-visible-calendar (buffer)
  (highlight-visible-chi-buffer buffer highlight-calendar-line))


;;;; Structure.

(defevar "Diary File"
  "Full name of diary file."
  :value "home:diary")

(defevar "Show Diary on Start"
  "If true pop-up the diary initially.")

(defun setup-calendar-mode (buffer)
  (highlight-visible-calendar buffer)
  (pushnew '("Calendar" t highlight-visible-calendar)
	   *mode-highlighters*))

(defmode "Calendar" :major-p t
  :setup-function #'setup-calendar-mode
  :documentation "Calendar mode.")

(defun setup-diary-mode (buffer)
  "Setup $buffer for diary mode."
  (refresh-diary-reminders buffer)
  (highlight-visible-diary buffer)
  (pushnew '("Diary" () highlight-visible-diary)
	   *mode-highlighters*)
  (add-hook write-file-hook
	    (lambda (buffer)
	      (if (eq (buffer-minor-mode buffer "Diary") t)
		  (refresh-diary-reminders buffer)))))

(defmode "Diary" :major-p nil
  :setup-function #'setup-diary-mode
  :documentation "Diary mode.")

(defmode "Diary Entry" :major-p t
  :short-name "Diary-Entry"
  :documentation "Mode for showing one or more diary entries.")

(defconstant *calendar-short-weekday-names*
  #("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))

(defconstant week-header "Mo Tu We Th Fr Sa Su")

(defvar *calendar-month-names*
  (make-string-table :initial-contents '(("January" . nil)
					 ("February" . nil)
					 ("March" . nil)
					 ("April" . nil)
					 ("May" . nil)
					 ("June" . nil)
					 ("July" . nil)
					 ("August" . nil)
					 ("September" . nil)
					 ("October" . nil)
					 ("November" . nil)
					 ("December" . nil))))

(defvar *calendar-columns* 3)

(defvar *last-calendar-search-pattern*
  (new-search-pattern :string-sensitive :forward "September 2006")
  "Cached search pattern.")

(defvar *calendar-mo-back-pattern*
  (new-search-pattern :string-sensitive :backward "Mo")
  "Pattern to find \"Mo\" backwards.")

(defvar *calendar-nlnl-fore-pattern*
  (new-search-pattern :string-sensitive :forward "

") ;"
  "Pattern to find two newlines forward.")

(defvar *reminders* '()
  "List of scheduled reminders.")

(defvar *diary-reminders* '()
  "List of scheduled diary reminders.")


;;;; Helper functions.

(defun cancel-reminder (reminder)
  "Cancel *reminders* entry Reminder."
  (setq *reminders* (delq reminder *reminders*))
  (remove-scheduled-event (cadr reminder)))

(defun cancel-diary-reminder (reminder)
  "Cancel *diary-reminders* entry Reminder."
  (setq *diary-reminders* (delq reminder *diary-reminders*))
  (remove-scheduled-event (cadr reminder)))

(defun universal-time-to-diary-date (time)
  (multiple-value-bind
      (secs mins hours day month year weekday dst tz)
      (decode-universal-time time)
    (declare (ignore secs mins hours weekday dst tz))
    (format () "~A ~A, ~A"
	    (aref ext::abbrev-month-table (1- month)) day year)))

(defun make-cal-search-pattern (kind direction pattern)
  (setq *last-calendar-search-pattern*
	(new-search-pattern kind direction pattern
			    *last-calendar-search-pattern*)))

(defun calendar-time-at-point ()
  "Return the universal time at point."
  (multiple-value-bind (day month year)
		       (calendar-date-at-point)
    (encode-universal-time
     1 1 1 ; sec min hr
     (parse-integer day)
     (month-number month)
     (parse-integer year))))

(defun calendar-date-at-point ()
  "Return day, month, year if there's a calendar date at point, else invoke
   an editor error."
  (let ((mark (copy-mark (current-point)))
	day month)
    (or (digit-char-p (next-character mark))
	(editor-error "Must be on a day to get the date at point."))

    ;; Parse the day number.
    (reverse-find-attribute mark :whitespace)
    (setq day
	  (copy-seq
	   (region-to-string
	    (region (mark (mark-line mark)
			  (mark-charpos mark))
		    (progn
		      (mark-after mark)
		      (or (find-attribute mark :whitespace)
			  (error "Failed to find whitespace after day."))
		      mark)))))
    (mark-before mark)

    ;; Move up to the day name heading.
    (loop
      (or (line-offset mark -1)
	  (error "Failed to move up to day name heading."))
      (if (find (next-character mark) "MoTuWehFrSau")
	  (return)))
    (if (char= (next-character mark) #\o)
	(mark-before mark)
	(or (char= (next-character mark) #\M)
	    (find-pattern mark *calendar-mo-back-pattern*)))

    ;; Parse the month and year.
    (line-offset mark -1)
    (mark-after mark)
    (find-attribute mark :whitespace #'zerop)
    (setq month
	  (region-to-string
	   (region (mark (mark-line mark)
			 (mark-charpos mark))
		   (progn
		     (mark-after mark)
		     (or (find-attribute mark :whitespace)
			 (error "Failed to find end of month name."))
		     mark))))
    (find-attribute mark :whitespace #'zerop)
    (values day
	    month
	    (region-to-string
	     (region (mark (mark-line mark)
			   (mark-charpos mark))
		     (progn
		       (mark-after mark)
		       (or (find-attribute mark :whitespace)
			   (error "Failed to find end of year name."))
		       mark))))))

(defun insert-calendar (mark month year months)
  "Insert at Mark Months months starting from Month (January is 1) in
   Year."
  (or (<= 1 month 12) (error "Month must be >= 1 and <= 12."))
  (let ((last-month (+ month (1- months)))
	(row-month month))
    (loop for first = t then () do
      (if (> row-month last-month) (return))
      (if (> row-month 12)
	  (setq row-month (- row-month 12)
		last-month (- last-month 12)
		year (1+ year)))
      (or first (insert-character mark #\newline))
      (let ((top-line (mark-line mark))
	    (end-line (mark-line mark))
	    (last-col-month (min (+ row-month (1- *calendar-columns*))
				 last-month))
	    (col-month row-month))
	(loop
	  for column = (mark-charpos mark) then (+ column 22)
	  for adjacent = () then t
	  do
	  (if (> col-month last-col-month) (return))
	  (assert (<= 1 col-month 13))
	  (if (eq col-month 13)
	      (setq col-month 1
		    last-col-month (- last-col-month 12)
		    last-month (- last-month 12)
		    row-month (- row-month 12)
		    year (1+ year)))
	  (move-mark mark (mark top-line 0))
	  (line-end mark)
	  (fill-to-column mark column)
	  (insert-month mark col-month year :adjacent adjacent)
	  (or adjacent
	      (progn
		(insert-character mark #\newline)
		(setq end-line (mark-line mark))))
	  (incf col-month))
	(move-mark mark (mark end-line 0))
	(line-end mark)
	(or (zerop (mark-charpos mark))
	    (insert-character mark #\newline)))
      (incf row-month *calendar-columns*))))

(defun fill-to-column (mark column)
  "Insert spaces at Mark up to Column."
  (let ((col (mark-charpos mark)))
    (if (> column col)
	(insert-string mark (make-string (- column col)
					 :initial-element #\ )))))

(defun insert-month (mark month year &key (fill t) (adjacent nil))
  "Insert Month (January is 1) of Year at Mark."
  (with-output-to-mark (stream mark)
    (or (<= 1 month 12)
	(error "Month must be >= 1 and <= 12."))
    (let ((month-name (month-name month))
	  (column (mark-charpos mark)))
      (fill-to-column mark column)
      (let ((head (format nil "~A ~A" month-name year)))
	(format stream "~A~A"
		(make-string (truncate (/ (- 20 (length head)) 2))
			     :initial-element #\ )
		head))
      (if adjacent
	  (progn
	    (line-offset mark 1)
	    (line-end mark))
	  (terpri stream))
      (if fill (fill-to-column mark column))
      (insert-string mark week-header)
      (multiple-value-bind
	  (secs mins hours day month year first-weekday dst tz)
	  (decode-universal-time (encode-universal-time
				  0 0 0 1
				  (month-number month-name)
				  year))
	(declare (ignore secs mins hours day dst tz))
	(let ((day 1)
	      (first-week t)
	      (last-day (last-day-of-julian-month month year)))
	  (if adjacent
	      (progn
		(or (line-offset mark 1) (terpri stream))
		(line-end mark))
	      (terpri stream))
	  (if fill (fill-to-column mark column))
	  ;; Insert leading space for first week.
	  (loop for i downfrom first-weekday to 1 do
	    (insert-string mark "   "))
	  ;; Insert weeks.
	  (loop while
	    ;; Insert week.
	    (progn
	      (loop
		for d from day
		to (setq day
			 (min last-day
			      (+ day
				 (if first-week
				     (progn
				       (setq first-week nil)
				       (- 6 first-weekday))
				     (progn
				       (if adjacent
					   (progn
					     (or (line-offset mark 1)
						 (terpri stream))
					     (line-end mark))
					   (terpri stream))
				       (if fill
					   (fill-to-column mark
							   column))
				       6)))))
		do
		(format stream "~3A" d))
	      (prog1
		  (if (eq day last-day) nil t)
		(incf day)))))))))

(defvar *reminder-buffer-len* 10)
(defvar *reminder-buffer* (make-string *reminder-buffer-len*))

(declaim (inline diary-reminder-at-mark))

;; Would need updating for dates with *'s.
; (defun diary-date-at-mark-p (mark)
;   "Return a true value if there is a diary date at mark, else nil."
;   (let ((chars (subseq (line-string (mark-line mark))
; 		       (mark-charpos mark))))
;     (when (> (length chars) 11)
;       (and (short-month-number (subseq chars 0 3))
; 	   (eq (char chars 3) #\ )
; 	   (parse-integer chars :start 4 :end 6)
; 	   (eq (char chars 6) #\,)
; 	   (parse-integer chars :start 7 :end 12)))))

(defun long-diary-entry-at-mark-p (mark)
  "Return a true value if there is a long entry at mark, else nil."
  (let* ((chars (subseq (line-string (mark-line mark))
			(mark-charpos mark)))
	 (length (length chars)))
    (if (eq length 11)
	(and (short-month-number (subseq chars 0 3))
	     (eq (char chars 3) #\ )
	     (parse-integer chars :start 4 :end 5)
	     (eq (char chars 5) #\,)
	     (parse-integer chars :start 6 :end 11))
	(if (eq length 12)
	    (and (short-month-number (subseq chars 0 3))
		 (eq (char chars 3) #\ )
		 (parse-integer chars :start 4 :end 6)
		 (eq (char chars 6) #\,)
		 (parse-integer chars :start 7 :end 12))))))

(declaim (inline word-from-point))

(defun word-from-point (mark)
  "If there is a word starting at Mark return the word and move Mark to the
   end of the word, else return nil."
  (let ((ch (next-character mark)))
    (if (zerop (character-attribute :word-delimiter ch))
	(let ((mark-1 (find-attribute (copy-mark mark :temporary)
				      :word-delimiter)))
	  (prog1
	      (region-to-string (region mark mark-1))
	    (move-mark mark mark-1))))))

(declaim (inline diary-date-at-mark))

(defun diary-date-at-mark (mark line-end)
  "Return a mark and the visibility, day, month and year if there is a
   diary entry at point, else nil.  The returned mark is at the end of the
   date."
  ;; Sep 13, 2006
  ;; &Oct 3, 2006
  ;; &* *, *
  (let ((visible t)
	(mark2 (copy-mark mark)))
    (when (eq (next-character mark2) #\&)
      (mark-after mark2)
      (setq visible nil))
    (let* ((star (eq (next-character mark2) #\*))
	   (month (if star #\* (word-from-point mark2))))
      (when month
	(if star (mark-after mark2))
	(with-input-from-region (stream (region mark2 line-end))
;; FIX add time parsing
; 	  (when (and (position month ext::abbrev-weekday-table
; 			       :test 'string=)
; 		     (eq (next-character mark2) #\space))
; 	    ;; Sun 19h00 text
; 	    (read-char stream) ; #\space
; 	    (return-from diary-reminder-at-mark
; 			 (values (parse-time month)
; 				 (read-line stream nil)
; 				 visible)))
	  (setq month
		(if star
		    (nth-value 4 (get-decoded-time))
		    (short-month-number month)))
	  (when month
	    (handler-case
		(let ((day (read stream nil)))
		  (when (if (eq day '*)
			    (setq day (nth-value 3 (get-decoded-time)))
			    (integerp day))
		    (when (eq (read-char stream nil) #\,)
		      (let ((year (read stream nil)))
			(when (if (eq year '*)
				  (setq year (nth-value 5 (get-decoded-time)))
				  (integerp year))
			  (character-offset mark2 (file-position stream))
			  (values mark2 visible day month year))))))
	      (reader-error ()))))))))

(defun diary-reminder-at-mark (mark tz dst line-end)
  "Return the universal time, text and visibility of any diary reminder at
   Mark, else nil."
  ;; Sep 13, 2006 23:00 text
  ;; &Oct 3, 2006 23h15 text
  ;; Oct 3, 2006 23h50 sss
  ;; &* *, * *h15 text
  ;;     FIX This could go all the way to *h**; may need recurring reminders.
  ;; (let ((e (copy-mark (current-point)))) (line-end e) (diary-reminder-at-mark (current-point) 0 t e))
  (multiple-value-bind (mark2 visible day month year)
		       (diary-date-at-mark mark line-end)
    (when mark2
      (with-input-from-region (stream (region mark2 line-end))
	(let ((chars 0))
	  (loop
	    for i from 0 to (1- *reminder-buffer-len*)
	    for ch = (read-char stream nil)
	    then (read-char stream nil)
	    while ch do
	    (incf chars)
	    (if (or (eq ch #\:) (eq ch #\h)) (return))
	    (setf (aref *reminder-buffer* i) ch))
	  (when (> chars 1)
	    (let ((hour
		   (if (eq (char *reminder-buffer* 0) #\*)
		       (if (eq chars 2)
			   (nth-value 2 (get-decoded-time)))
		       (parse-integer *reminder-buffer*
				      :junk-allowed t))))
	      (when hour
		(handler-case
		    (let ((min (read stream nil)))
		      (when (and min (integerp min))
			(let ((text (read-line stream nil)))
			  (values
			   (encode-universal-time 0 min hour day
						  month
						  year tz dst)
			   text
			   visible))))
		  (reader-error ()))))))))))

(defun refresh-diary-reminders (buffer)
  "Re-schedule the *diary-reminders* according to diary $buffer."
  (let ((mark (copy-mark (buffer-mark buffer))))
    (dolist (reminder *diary-reminders*)
      (cancel-diary-reminder reminder))
    (move-mark mark (buffer-start-mark buffer))
    (multiple-value-bind
	(tz dst)
	(unix:get-timezone (nth-value 1 (unix:unix-gettimeofday)))
      (let ((line-end (copy-mark mark))
	    ;; FIX is this correct? result is off, maybe should incl dst?
	    ;(now (+ 30 (- (get-universal-time) (* tz 60)))))
	    (now (+ 30 (get-universal-time))))
	;; FIX encode-u-time via d-r-a-m seems to be off when tz is given
	;;(setq tz (/ tz 60))
	(setq tz 0)
	(loop
	  (when (and (next-character mark)
		     (or (alpha-char-p (next-character mark))
			 (eq (next-character mark) #\*)
			 (eq (next-character mark) #\&)))
	    (line-end line-end)
	    (multiple-value-bind (time text)
				 (diary-reminder-at-mark
				  mark tz dst line-end)
	      (when (and time (> time now))
		(push (list text
			    (schedule-event
			     time
			     (lambda (secs)
			       (declare (ignore secs))
			       (let ((rem (assoc text
						 *diary-reminders*)))
				 (when rem
				   (cancel-diary-reminder rem)))
			       (message "Diary reminder: ~A" text)
			       (with-pop-up-display (stream)
				 (format stream "Diary reminder: ~A"
					 text)))
			     nil ; repeat
			     t)) ; absolute
		      *diary-reminders*))))
	  (let ((line (line-next (mark-line mark))))
	    (or line (return))
	    (edi::change-line mark line)
	    (edi::change-line line-end line)
	    (setf (mark-charpos mark) 0)))))))


;;;; Calendar and Diary Commands.

(defvar *calendar-buffer* ())

(defun refresh-calendar-buffer (buffer date)
  "Refresh the calendar in $buffer."
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (multiple-value-bind
	(secs mins hours day month year)
	(decode-universal-time date)
      (declare (ignore secs mins hours day))
      (insert-calendar (buffer-point buffer)
		       month year 3)
      (elet ((flush-trailing-whitespace t))
	(flush-trailing-whitespace buffer)))))

(defun go-to-date (date buffer)
  (or (string= (buffer-major-mode buffer) "Calendar")
      (editor-error "Buffer must be in Calendar mode."))
  (let ((point (buffer-point buffer)))
    (buffer-start point)
    (multiple-value-bind
	(secs mins hours day month year weekday dst tz)
	(decode-universal-time date)
      (declare (ignore secs mins hours dst tz))
      (let ((pattern (make-cal-search-pattern
		      :string-sensitive :forward
		      (format () "~A ~A" (month-name month) year))))
	(when (find-pattern point pattern)
	  (line-offset point 1)
	  (when (find-pattern point *calendar-mo-back-pattern*)
	    (or (eq weekday 0)
		(let ((pattern (make-cal-search-pattern
				:string-sensitive :forward
				(aref *calendar-short-weekday-names*
				      weekday))))
		  (or (find-pattern point pattern)
		      (error "Failed to find weekday name."))))
	    (loop
	      (or (line-offset point 1)
		  (editor-error
		   "Reached end of buffer while looking for day."))
	      (let ((ch (next-character point)))
		(or (eq ch #\ )
		    (if (eq day
			    (with-input-from-region
				(stream (region
					 point
					 (mark (mark-line point)
					       (+ (mark-charpos point)
						  2))))
			      (handler-case
				  (read stream)
				(reader-error ()))))
			(return-from ())))))))))))

(defcommand "Calendar" (p)
  "Switch to the calendar buffer."
  (let* ((buffer (or *calendar-buffer*
		     (setq *calendar-buffer*
			   (make-unique-buffer
			    "Calendar"
			    :modes '("Calendar")
			    :delete-hook
			    (list (lambda (buffer)
				    (declare (ignore buffer))
				    (setq *calendar-buffer* ())))))))
	 (date (if p (prompt-for-date) (get-universal-time))))
    (defevar "Calendar Months"
      "The number of months in the buffer."
      :buffer buffer
      :value 3)
    (go-to-date-command () date buffer)
    (change-to-buffer buffer)))

(defcommand "Go To Date" (p (date (prompt-for-date))
			   (buffer (current-buffer)))
  "Center the calendar buffer around a prompted date."
  "Center the calendar buffer around $date."
  (declare (ignore p))
  (or (string= (buffer-major-mode buffer) "Calendar")
      (editor-error "Buffer must be in Calendar mode."))
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (multiple-value-bind
	(secs mins hours day month year)
	(decode-universal-time date)
      (declare (ignore secs mins hours day))
      (let ((start-month-index (- month
				  (floor *calendar-columns* 2)
				  1)))
	(insert-calendar (buffer-point buffer)
			 (1+ (mod start-month-index 12))
			 (+ year (floor start-month-index 12))
			 (variable-value 'calendar-months
					 :buffer buffer))
	(elet ((flush-trailing-whitespace t))
	  (flush-trailing-whitespace buffer)))))
  (go-to-date date buffer))

(defcommand "Refresh Calendar" (p (buffer (current-buffer)))
  "Refresh the calendar buffer."
  "Refresh the calendar buffer Buffer."
  (go-to-date-command p (calendar-time-at-point) buffer))

(defcommand "Go To Today" (p (buffer (current-buffer)))
  "If in the calendar buffer move point to today."
  "Move point to today in calendar buffer Buffer."
  (go-to-date-command p (get-universal-time) buffer))

(defcommand "Calendar Show Year" (p (buffer (current-buffer)))
  "Show the whole year."
  "Show the whole year in calendar buffer Buffer."
  (declare (ignore p))
  (or (string= (buffer-major-mode buffer) "Calendar")
      (editor-error "Buffer must be in Calendar mode."))
  (setv calendar-months 12)
  (refresh-calendar-command))

(defcommand "Calendar Show 3 Months" (p (buffer (current-buffer)))
  "Show three months."
  "Show three months in calendar buffer Buffer."
  (declare (ignore p))
  (or (string= (buffer-major-mode buffer) "Calendar")
      (editor-error "Buffer must be in Calendar mode."))
  (setv calendar-months 3)
  (refresh-calendar-command))

#|
(profile::profile diary-date-at-mark diary-command word-from-point)
(profile::unprofile diary-date-at-mark diary-command word-from-point)
|#

(defun write-diary (&optional (date (get-universal-time))
			      (stream *standard-output*))
  "Write the diary for $date to $stream."
  ;; FIX prhps filter-tildes elsewhere (more below)
  (let ((pathname (filter-tildes
		   (or (value diary-file)
		       (editor-error
			"Filename required in variable *Diary File*.")))))
    (or (probe-file pathname) (touch-file pathname))
    (let* ((buffer (find-file-buffer pathname))
	   (mark (copy-mark (buffer-start-mark buffer)))
	   (dow (nth-value 6 (get-decoded-time))))
      (multiple-value-bind
	  (secs mins hours day month year weekday dst tz)
	  (decode-universal-time date)
	(declare (ignore secs mins hours weekday dst tz))
	(format stream "Diary entries:~%~%")
	(loop
	  for line = (mark-line mark) then (line-next line)
	  while line do
	  (let ((mark (mark line 0)))
	    (if (eq (next-character mark) #\&)
		(mark-after mark))
	    (let ((sl-day "abc"))
	      (setf (char sl-day 0) (or (next-character mark)
					(return)))
	      (mark-after mark)
	      (setf (char sl-day 1) (or (next-character mark)
					(return)))
	      (mark-after mark)
	      (setf (char sl-day 2) (or (next-character mark)
					(return)))
	      (mark-after mark)
	      (if (and (eq (position sl-day ext::abbrev-weekday-table
				     :test 'string=)
			   dow)
		       (eq (next-character mark) #\space))
		  ;; Insert single line entry.
		  (format stream "~A~%" (line-string line))
		  ;; Look for multiple line entry.
		  (let* ((mark (mark line 0))
			 (end (copy-mark mark)))
		    (line-end end)
		    (multiple-value-bind
			(date-end visible dday dmonth dyear)
			(diary-date-at-mark mark end)
		      (declare (ignore visible))
		      (when (and (eq dday day)
				 (eq dmonth month)
				 (eq dyear year))
			(if (eq (next-character date-end) #\newline)
			    (format stream
				    "~A"
				    (region-to-string
				     (region
				      (mark (mark-line mark)
					    (mark-charpos mark))
				      (progn
					(or (find-pattern
					     mark
					     *calendar-nlnl-fore-pattern*)
					    (move-mark
					     mark
					     (buffer-end-mark buffer)))
					(mark-after (mark-after mark))
					mark))))
			    (progn
			      (line-end mark)
			      (format stream
				      "~A~%"
				      (region-to-string
				       (region (mark (mark-line mark) 0)
					       mark)))
			      (line-offset mark 1)))
			(progn
			  (line-start mark)
			  (line-offset mark 1)))))))))))))

(defcommand "Diary" (p (date (get-universal-time)))
  "Pop-up any diary entries for today."
  "Pop-up any diary entries for universal time $date."
  (declare (ignore p))
  (in-pop-up-window (buffer window
			    :buffer-name "Diary"
			    :modes '("Diary Entry"))
    (with-writable-buffer (buffer)
      (with-output-to-mark (stream (buffer-point buffer))
	(write-diary date stream))
      (buffer-start (buffer-point buffer)))))

(defcommand "Show Diary Entries" ()
  "Pop-up diary entry for day at point if in Calendar, else for today."
  (if (string= (buffer-major-mode (current-buffer)) "Calendar")
      (multiple-value-bind (day month year)
			   (calendar-date-at-point)
	(diary-command ()
		       (encode-universal-time
			1 1 1 ; sec min hr
			(parse-integer day)
			(1+ (month-number month))
			(parse-integer year))))
      (diary-command)))

(defcommand "Insert Diary Entry" ()
  "In a calendar buffer insert a diary entry for the day at point."
  (or (string= (buffer-major-mode (current-buffer)) "Calendar")
      (editor-error "Current buffer must be in Calendar mode."))
  (multiple-value-bind (day month year)
		       (calendar-date-at-point)
    (let ((buffer (find-file-buffer
		   (filter-tildes
		    (or (value diary-file)
			(editor-error
		 "Filename required in variable *Diary File*."))))))
      (or (buffer-writable buffer)
	  (editor-error "Diary buffer is read-only."))
      (change-to-buffer buffer)
      (let ((point (current-point)))
	(move-mark point (buffer-end-mark buffer))
	(or (zerop (mark-charpos point))
	    (insert-character point #\newline))
	(with-output-to-mark (stream point)
	  (format stream "~A ~A, ~A "
		  (aref ext::abbrev-month-table
			(1- (month-number month)))
		  day year)
	  (insert-character point #\newline)
	  (mark-before point))))))

(defcommand "Go To Today in Diary"
	    (&optional p (date (universal-time-to-diary-date
				(get-universal-time))))
  "Move point to the end of the last diary entry for today."
  (declare (ignore p))
  (let ((buffer (find-file-buffer
		 ;; FIX prhps filter-tildes elsewhere (more below)
		 (filter-tildes
		  (or (value diary-file)
		      (editor-error
	       "Filename required in variable `Diary File'."))))))
    (change-to-buffer buffer)
    (let* ((point (current-point))
	   (pattern (make-cal-search-pattern :string-sensitive
					     :backward
					     date)))
      (move-mark point (buffer-end-mark (current-buffer)))
      (if (find-pattern point pattern)
	  (when (find-pattern point *calendar-nlnl-fore-pattern*)
	    (mark-after point))
	  (insert-today-command)))))

(defcommand "Go To Date in Diary" ()
  "Switch to the diary with point at the end of the last diary entry for a
   prompted date."
  (go-to-today-in-diary-command () (universal-time-to-diary-date
				    (prompt-for-date))))

(defcommand "Go To Current Date in Diary" ()
  "Switch to the diary with point at the end of the last diary entry for
   the date under point."
  (or (string= (buffer-major-mode (current-buffer)) "Calendar")
      (editor-error "Current buffer must be in Calendar mode."))
  (go-to-today-in-diary-command () (calendar-date-at-point)))

(defcommand "Insert Today"
	    (&optional p (date (universal-time-to-diary-date
				(get-universal-time))))
  "Start a long diary entry for today after the last long entry."
  (declare (ignore p))
  (let ((buffer (find-file-buffer
		 ;; FIX prhps filter-tildes elsewhere (more below)
		 (filter-tildes
		  (or (value diary-file)
	      (editor-error
	       "Filename required in variable `Diary File'."))))))
    (change-to-buffer buffer)
    (let ((mark (current-point)))
      (move-mark mark (buffer-end-mark (current-buffer)))
      ;; Find the last entry in the buffer.
      (loop
	(when (and (next-character mark)
		   (alpha-char-p (next-character mark)))
	  (if (long-diary-entry-at-mark-p mark) (return)))
	(let ((line (line-previous (mark-line mark))))
	  (or line (return)) ;; FIX
	  (edi::change-line mark line)
	  (setf (mark-charpos mark) 0)))
      ;; Move to the end of the entry.
      (loop
	(or (line-offset mark 1 0) (return))
	(let ((ch (next-character mark)))
	  (or ch (return))
	  (if (or (eq ch #\newline)
		  (alpha-char-p ch))
	      (return))))
      (insert-character mark #\newline)
      (insert-string mark date)
      (insert-character mark #\newline)
      (insert-character mark #\tab)
      (insert-character mark #\newline)
      (mark-before mark))))

#|
(profile::profile refresh-diary-reminders-command diary-reminder-at-mark copy-mark next-character alpha-char-p line-end line-offset setq change-line setf line-next mark-line edi::change-line)
(profile::unprofile refresh-diary-reminders-command diary-reminder-at-mark copy-mark next-character alpha-char-p line-end line-offset setq change-line setf line-next mark-line)
(profile::reset-time)
(profile::report-time
)
|#

(defcommand "Refresh Diary Reminders" ()
  "Refresh the diary reminders."
  (let ((pathname (or (value diary-file)
		      (editor-error
		       "Filename required in variable `Diary File'."))))
    (if (probe-file pathname)
	(refresh-diary-reminders (find-file-buffer pathname)))))

(defun insert-diary-reminders (stream)
  "Insert diary reminders at Stream."
  (let ((i 0))
    (dolist (reminder *diary-reminders*)
      (format stream "~A) " (incf i))
      (format-universal-time stream
			     (internal-real-to-universal-time
			      (edi::tq-event-time (cadr reminder)))
			     :style :rfc1123
			     :print-seconds nil
			     :print-timezone nil
			     :print-meridian nil)
      (format stream ": ~A~%" (car reminder)))))

(defcommand "List Diary Reminders" ()
  "List reminders scheduled from the diary."
  (with-pop-up-display (stream)
    (format stream "Diary Reminders:~%")
    (insert-diary-reminders stream)))


;;;; Reminder Commands.

(defcommand "Add Reminder" ()
  "Add a reminder."
  (multiple-value-bind
      (secs mins hours day month year weekday dst tz)
      (get-decoded-time)
    (declare (ignore secs weekday))
    (let ((year (prompt-for-integer :prompt "Year: "
				    :must-exist t
				    :help "Year of the reminder."
				    :default year))
	  (month (prompt-for-keyword (list *calendar-month-names*)
				     :prompt "Month: "
				     :help "Month of the reminder."
				     :default (month-name month)))
	  ;; FIX could ensure > 0 and < 28,29,30,31
	  ;;     prompt-for-day?
	  (day (prompt-for-integer :prompt "Day: "
				   :must-exist t
				   :help "Day of the reminder."
				   :default day))
	  ;; FIX ensure in range (:max and :min?)
	  (hours (prompt-for-integer :prompt "Hour (0 to 23): "
				     :help "Hour of the reminder."
				     :default hours))
	  ;; FIX ensure in range
	  (mins (prompt-for-integer :prompt "Minute: "
				    :help "Minute of the reminder."
				    :default mins))
	  (text (prompt-for-string
		 :prompt "Reminder text: "
		 :help "Text to present in the reminder.")))
      (let ((time (encode-universal-time 0 mins hours day
					 (month-number month)
					 year tz dst)))
	(push (list text
		    (schedule-event
		     time
		     (lambda (secs)
		       (declare (ignore secs))
		       (let ((reminder (assoc text *reminders*)))
			 (if reminder
			     (cancel-reminder reminder)))
		       (message "Reminder: ~A" text)
		       (with-pop-up-display (stream)
			 (format stream "Reminder: ~A" text)))
		     nil t))
	      *reminders*)
	(message "Reminder scheduled for ~A."
		 (format-universal-time nil time :style :rfc1123))))))

(defun insert-reminders (stream)
  "Insert reminders added with `Add Reminder' in Stream."
  (let ((i 0))
    (dolist (reminder *reminders*)
      (format stream "~A) " (incf i))
      (format-universal-time stream
			     (internal-real-to-universal-time
			      (edi::tq-event-time (cadr reminder)))
			     :style :rfc1123
			     :print-seconds nil
			     :print-timezone nil
			     :print-meridian nil)
      (format stream ": ~A~%" (car reminder)))))

(defcommand "List Reminders" ()
  "List reminders added with `Add Reminder'."
  (with-pop-up-display (stream)
    (format stream "Reminders:~%")
    (insert-reminders stream)))

(defcommand "List All Reminders" ()
  "List reminders scheduled from the diary or added with `Add Reminder'."
  (with-pop-up-display (stream)
    (format stream "Reminders:~%")
    (insert-diary-reminders stream)
    (format stream "~%Diary Reminders:~%")
    (insert-reminders stream)))

(defcommand "Cancel Reminder" ()
  "Cancel a reminder."
  (or *reminders*
      (editor-error "Reminder list empty."))
  (let* ((number (prompt-for-integer
		  :prompt "Reminder number: "
		  :must-exist t
		  :help "Number of reminder to cancel."
		  :default 1))
	 (reminder (nth (1- number) *reminders*)))
    (cancel-reminder reminder)
    (message "Cancelled \"~A\"" (car reminder))))


;;;; Highlighting.

(defevar "Diary Line Colors"
  "List of line beginning characters and associated colors."
  :value '((#\: :string)
	   (#\= :comment)
	   (#\| :preprocessor)))

(defmacro rehighlight-diary-line (info)
  `(progn
     (dolist (fmark (ch-info-font-marks ,info))
       (delete-font-mark fmark))
     (let ((mark (mark line 0)))
       (if (zerop (character-attribute :whitespace (next-character mark)))
	   (push (color-mark line 0 :variable)
		 (ch-info-font-marks ,info))
	   (when (find-attribute mark :whitespace #'zerop)
	     (if (eq (next-character mark) #\~) (mark-after mark))
	     (if (next-character mark)
		 (cond ((digit-char-p (next-character mark))
			(push (color-mark (mark-line mark)
					  (mark-charpos mark)
					  :special-form)
			      (ch-info-font-marks ,info)))
		       (t
			(let ((assoc (assoc (next-character mark)
					    (value diary-line-colors))))
			  (if assoc
			      (push (color-mark (mark-line mark)
						(mark-charpos mark)
						(cadr assoc))
				    (ch-info-font-marks ,info))))))))))))

;; FIX similar to, for instance, h-v-kill-browse
(defun highlight-visible-diary (buffer)
  (dolist (window (buffer-windows buffer))
    (or (eq (edi::window-first-changed window) edi::the-sentinel)
	(loop
	  for num from (window-height window) downto 1
	  for line = (mark-line (window-display-start window))
	  then (line-next line)
	  while line
	  do
	  (when (plusp (line-length line))
	    (let ((info (check-highlight-line line 'diary-ch-info)))
	      (when info
		(rehighlight-diary-line info))))))))

(defun highlight-diary (buffer)
  (do-buffer-lines (line buffer)
    (when (> (line-length line) 0)
      (let ((info (check-highlight-line line 'diary-ch-info)))
	(when info
	  (rehighlight-diary-line info))))))


;;;; Mailing diary entries.

(defevar "Diary SMTP Account"
  "SMTP account destination for `mail-diary-entries'."
  :value (internet:make-inet-account "localhost"))
; FIX home: error
;   :value (internet:fill-from-netrc
; 	  (internet:make-inet-account "localhost")))

;; FIX get mh to do the headers?
(defun write-diary-mail (stream to from date)
  (format stream "To: ~A~%From: ~A~%Subject: [Diary] Entries for ~A~%~%"
	  to
	  from
	  ; FIX merge time printing into format
	  (format-universal-time () date :style :abbreviated
				 :print-seconds ()
				 :print-meridian ()
				 :print-timezone ()))
  (write-diary date stream)
  t)

(defun mail-diary-entries ()
  "Mail the diary entries for today to the user.  Return true on success,
   else signal an editor error."
  (let ((date (get-universal-time))
	;; FIX
	(to (user-name)) ; mh::*address*)
	(from (user-name))) ; (user-name)))
    (multiple-value-bind
	(success error)
	(internet:smtp-mail to from
			    (value diary-smtp-account)
			    #'write-diary-mail
			    date)
      (or success
	  (editor-error "Failed to mail diary: ~A." error)))))

;; FIX should be auto  eg check if host online before connecting
(defevar "Online"
  "If true attempt network connections.")

(defun mail-diary-for-today (&optional seconds)
  "Mail diary for today if the diary was last mailed before today."
  (declare (ignore seconds))
  (let ((diary-mail-file (config:config-pathname "diary-date")))
    (let ((last (with-open-file (stream diary-mail-file :direction :input
					:if-does-not-exist ())
		  (if stream (read stream) 0)))
	  (midnight
	   (multiple-value-bind (sec min hr date month year)
				(get-decoded-time)
	     (declare (ignore sec min hr))
	     (parse-time (format () "~A ~A, ~A 0:00 am" month date year)))))
      (and last midnight
	   (> midnight last)
	   (when (mail-diary-entries)
	     (with-open-file (stream diary-mail-file
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
	       (write (get-universal-time) :stream stream)))))))


;;;; Initialization.

(after-editor-initializations
 (refresh-diary-reminders-command)
 (if (value show-diary-on-start) (diary-command)))
