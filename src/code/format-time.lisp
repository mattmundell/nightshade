;;; Time printing routines built upon the format function.

(in-package :extensions)

(export '(format-universal-time format-decoded-time format-time))


;;; Valid-Destination-P ensures the destination stream is okay for the
;;; Format function.

(defun valid-destination-p (destination)
  (if destination
      (or (eq destination 't)
	  (streamp destination)
	  (and (stringp destination)
	       (array-has-fill-pointer-p destination)))
      t))

(defun format-iso8601-time (destination time-value &optional include-timezone-p)
  "Formats to $destination a universal time $time-value in ISO 8601 format,
   with the time zone included if $include-timezone-p is true."
  (flet ((format-iso8601-timezone (zone dst)
	   (when dst (decf zone))
	   (if (zerop zone)
	       "Z"
	       (multiple-value-bind (h m) (truncate (abs zone) 1.0)
		 ;; Tricky.  Sign of time zone is reversed in ISO 8601
		 ;; relative to Lisp convention!
		 (format nil "~:[+~;-~]~2,'0D:~2,'0D"
			 (> zone 0) h (round m))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
			 (decode-universal-time time-value)
      (declare (ignore dow))
      (format destination
	      "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
	      year month day hour minute second
	      include-timezone-p (format-iso8601-timezone zone dst)))))

;;; Format-Universal-Time - External.

(defun format-universal-time (destination universal-time
			      &key (timezone nil)
			           (style :short)
				   (date-first t)
				   (print-seconds t)
				   (print-date t)
				   (print-meridian t)
				   (print-timezone t)
				   (print-weekday t))
  "Format and return a string containing the time and date expressed by
   $universal-time.  The destination is any destination which can be
   accepted by the Format function.

   Keyword arguments:

     $timezone
         an integer specifying the hours west of Greenwich.  Use the
         current time zone if this is ().

     $style
         specify the style to use in formatting the time.
            :short        a numeric date
                              Monday, 4/28/08 03:08:35 pm [+0:01]
            :long         months and weekdays as words instead of numbers
                              Monday, April 28, 2008 03:08:58 pm [+0:01]
            :abbreviated  like long, with abbreviated words
                              Mon, Apr 28, 2008 03:09:28 pm [+0:01]
            :government   like abbreviated, with \"day month year\"
                              Mon, 28 APR 8 03:09:55 pm [+0:01]
            :rfc1123      conforming to RFC 1123
                              Mon, 28 Apr 2008 15:10:15 -0001
            :iso8601      ISO 8601 format
                              2008-04-28T15:10:48
            :condensed    CCYYMMDDhhmm.ss
                              200804281511.06

     $date-first
         if true place the date first, otherwise, placed the time first.

     $print-*
         if true, include the associated component."
  (or (valid-destination-p destination)
      (error "~A: Not a valid format destination." destination))
  (or (integerp universal-time)
      (error "~A: Universal-Time should be an integer." universal-time))
  (when timezone
    (or (and (rationalp timezone) (<= -24 timezone 24))
	(error "~A: Timezone should be a rational between -24 and 24." timezone))
    (or (zerop (rem timezone 1/3600))
	(error "~A: Timezone is not a second (1/3600) multiple." timezone)))

  (case style
    (:iso8601
     (if timezone (error "FIX timezone with iso8601 style"))
     (format-iso8601-time destination universal-time))
    (:condensed
     (multiple-value-bind (secs mins hours day month year)
			  (if timezone
			      (decode-universal-time universal-time timezone)
			      (decode-universal-time universal-time))
       (declare (fixnum secs mins hours day month year))
       ;; FIX handle print-* args
       (format destination "~A~2,'0D~2,'0D~2,'0D~2,'0D.~2,'0D"
	       year month day hours mins secs)))
    (t
     (multiple-value-bind (secs mins hours day month year dow dst tz)
			  (if timezone
			      (decode-universal-time universal-time timezone)
			      (decode-universal-time universal-time))
       (declare (fixnum secs mins hours day month year dow))
       (let ((time-string "~2,'0D:~2,'0D")
	     (date-string
	      (case style
		(:short "~D/~D/~2,'0D")                 ;;  MM/DD/YY
		((:abbreviated :long) "~A ~D, ~D")      ;;  Month DD, YYYY
		(:rfc1123 "~2,'0D ~A ~4,'0D")           ;;  DD Mon YYYY
		(:government "~2,'0D ~:@(~A~) ~D")	;;  DD MON YY
		(t
		 (error "~A: Unrecognized :style keyword value." style))))
	     (time-args
	      (if (eq style :rfc1123)
		  (list mins hours)
		  (list mins (max (mod hours 12) (1+ (mod (1- hours) 12))))))
	     (date-args (case style
			  (:short
			   (list month day (mod year 100)))
			  (:abbreviated
			   (list (svref abbrev-month-table (1- month)) day year))
			  (:long
			   (list (svref long-month-table (1- month)) day year))
			  (:rfc1123
			   (list day (svref abbrev-month-table (1- month)) year))
			  (:government
			   (list day (svref abbrev-month-table (1- month))
				 (mod year 100)))))
	     (timezone-name (if (eq style :rfc1123)
				(timezone-rfc1123-name dst tz)
				(timezone-name dst tz))))
	 (declare (simple-string time-string date-string timezone-name))
	 (when print-weekday
	   (push (case style
		   ((:short :long) (svref long-weekday-table dow))
		   ((:abbreviated :rfc1123 :government)
		    (svref abbrev-weekday-table dow)))
		 date-args)
	   (setq date-string
		 (concatenate 'simple-string "~A, " date-string)))
	 (when (or print-seconds (eq style :government))
	   (push secs time-args)
	   (setq time-string
		 (concatenate 'simple-string time-string ":~2,'0D")))
	 (when (and print-meridian (not (eq style :rfc1123)))
	   (push (signum (floor hours 12)) time-args)
	   (setq time-string
		 (concatenate 'simple-string time-string " ~[am~;pm~]")))
	 (apply #'format destination
		(if print-date
		    (if date-first
			(concatenate 'simple-string date-string " " time-string
				     (if print-timezone " ~A"))
			(concatenate 'simple-string time-string " " date-string
				     (if print-timezone " ~A")))
		    (concatenate 'simple-string time-string
				 (if print-timezone " ~A")))
		(if print-date
		    (if date-first
			(nconc date-args (nreverse time-args)
			       (if print-timezone
				   (list timezone-name)))
			(nconc (nreverse time-args) date-args
			       (if print-timezone
				   (list timezone-name))))
			(nconc (nreverse time-args)
			       (if print-timezone
				   (list timezone-name))))))))))

(defun timezone-name (dst tz)
  (if (and (integerp tz)
	   (or (and (not dst) (= tz 0))
	       (<= 5 tz 8)))
      (svref (if dst daylight-table timezone-table) tz)
      (multiple-value-bind
	  (rest seconds)
	  (truncate (* tz 60 60) 60)
	(multiple-value-bind
	    (hours minutes)
	    (truncate rest 60)
	  (format () "[~C~D~@[~*:~2,'0D~@[~*:~2,'0D~]~]]"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (not (and (zerop minutes) (zerop seconds)))
		  (abs minutes)
		  (not (zerop seconds))
		  (abs seconds))))))

;;; RFC 1123 style timezone: GMT, +1000, -1000.
;;; Timezone is the negative of the CL timezone.
;;;
(defun timezone-rfc1123-name (dst tz)
  (let ((tz (- tz)))
    (if (and (integerp tz)
	     (or (and (not dst) (= tz 0))
		 (<= 5 tz 8)))
	(svref (if dst daylight-table timezone-table) tz)
	(multiple-value-bind
	      (hours minutes)
	    (truncate tz)
	  (format nil "~C~2,'0D~2,'0D"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (abs (truncate (* minutes 60))))))))

;;; Format-Decoded-Time - External.

(defun format-decoded-time (destination seconds minutes hours
			    day month year
			    &key (timezone nil)
			         (style :short)
				 (date-first t)
				 (print-seconds t)
				 (print-meridian t)
				 (print-timezone t)
				 (print-weekday t))
  "Format and return a string containing the time expressed by the
   component arguments.  The destination is any destination which can be
   accepted by the Format function.

   Keyword arguments:

     :TIMEZONE
         an integer specifying the hours west of Greenwich. Falls back to
         the current time zone.

     :STYLE
         specifies the style to use in formatting the time.
            :short        a numeric date
            :long         months and weekdays as words instead of numbers
                             \"month day, year\"
            :abbreviated  like long, with abbreviated words
            :government   like abbreviated, with \"day month year\"
            :rfc1123      conforming to RFC 1123
            :iso8601      ISO 8601 format.

     :DATE-FIRST
         if true place the date first, otherwise, placed the time first.

     :PRINT-*
         if true, include the associated component."
  (or (valid-destination-p destination)
      (error "~A: Not a valid format destination." destination))
  (or (and (integerp seconds) (<= 0 seconds 59))
      (error "~A: Seconds should be an integer between 0 and 59." seconds))
  (or (and (integerp minutes) (<= 0 minutes 59))
      (error "~A: Minutes should be an integer between 0 and 59." minutes))
  (or (and (integerp hours) (<= 0 hours 23))
      (error "~A: Hours should be an integer between 0 and 23." hours))
  (or (and (integerp day) (<= 1 day 31))
      (error "~A: Day should be an integer between 1 and 31." day))
  (or (and (integerp month) (<= 1 month 12))
      (error "~A: Month should be an integer between 1 and 12." month))
  (or (and (integerp year) (plusp year))
      (error "~A: Hours should be an non-negative integer." year))
  (when timezone
    (or (and (integerp timezone) (<= 0 timezone 32))
	(error "~A: Timezone should be an integer between 0 and 32."
	       timezone)))
  (format-universal-time destination
   (encode-universal-time seconds minutes hours day month year)
   :timezone timezone :style style :date-first date-first
   :print-seconds print-seconds :print-meridian print-meridian
   :print-timezone print-timezone :print-weekday print-weekday))

;;; Format-Time - External.

(defun format-time (&optional destination
		    &key (timezone nil)
		         (style :short)
			 (date-first t)
			 (print-seconds t)
			 (print-date t)
			 (print-meridian t)
			 (print-timezone t)
			 (print-weekday t))
  "Format the current time to $destination, as if by `format-universal-time'."
  (format-universal-time destination
			 (get-universal-time)
			 :timezone timezone :style style
			 :date-first date-first
			 :print-seconds print-seconds
			 :print-date print-date
			 :print-meridian print-meridian
			 :print-timezone print-timezone
			 :print-weekday print-weekday))
