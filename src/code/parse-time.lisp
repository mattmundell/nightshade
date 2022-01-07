;;; Parsing routines for time and date strings.

(in-package "EXTENSIONS")

(export '(parse-time parse-iso8601-time
	  *default-date-time-patterns* *http-date-time-patterns*))

(defconstant whitespace-chars '(#\space #\tab #\newline #\, #\' #\` #\"))
(defconstant time-dividers '(#\: #\.))
(defconstant date-dividers '(#\\ #\/ #\-))
(defconstant timezone-prefixes '(#\+ #\-))
(defconstant open-brackets '(#\())
(defconstant close-brackets '(#\)))

(defvar *error-on-mismatch* nil
  "If t, an error will be signalled if parse-time is unable to determine
   the time/date format of the string.")

;;; Set up hash tables for month, weekday, zone, and special strings.
;;; Provides quick, easy access to associated information for these items.

;;; Hashlist takes an association list and hashes each pair into the
;;; specified tables using the car of the pair as the key and the cdr as
;;; the data object.

(defmacro hashlist (list table)
  `(dolist (item ,list)
     (setf (gethash (car item) ,table) (cdr item))))

(defparameter weekday-table-size 23)
(defparameter month-table-size 31)
(defparameter zone-table-size 23)
(defparameter special-table-size 11)

(defvar *weekday-strings* (make-hash-table :test #'equal
					   :size weekday-table-size))

(defvar *month-strings* (make-hash-table :test #'equal
					 :size month-table-size))

(defvar *zone-strings* (make-hash-table :test #'equal
					:size zone-table-size))

(defvar *special-strings* (make-hash-table :test #'equal
					   :size special-table-size))

;;; Load-time creation of the hash tables.

(hashlist '(("monday" . 0)    ("mon" . 0)
	    ("tuesday" . 1)   ("tues" . 1)   ("tue" . 1)
	    ("wednesday" . 2) ("wednes" . 2) ("wed" . 2)
	    ("thursday" . 3)  ("thurs" . 3)  ("thu" . 3)
	    ("friday" . 4)    ("fri" . 4)
	    ("saturday" . 5)  ("sat" . 5)
	    ("sunday" . 6)    ("sun" . 6))
	  *weekday-strings*)

(hashlist '(("january" . 1)   ("jan" . 1)
	    ("february" . 2)  ("feb" . 2)
	    ("march" . 3)     ("mar" . 3)
	    ("april" . 4)     ("apr" . 4)
	    ("may" . 5)       ("june" . 6)
	    ("jun" . 6)       ("july" . 7)
	    ("jul" . 7)	      ("august" . 8)
	    ("aug" . 8)       ("september" . 9)
	    ("sept" . 9)      ("sep" . 9)
	    ("october" . 10)  ("oct" . 10)
	    ("november" . 11) ("nov" . 11)
	    ("december" . 12) ("dec" . 12))
	  *month-strings*)

(hashlist '(("brst" . -2) ("brdt" . -2)
	    ("art" . -3) ("brt" . -3) ("clst" . -3)
	    ("ast" . -4)
	    ("gmt" . 0) ("ut" . 0) ("utc" . 0)
	    ("lcl" . 0)  ;; Possibly for local, from an old mail.
	    ("bst" . 1) ("cet" . 1) ("met" . 1)
	    ("cest" . 2) ("eet" . 2) ("ist" . 2) ("sast" . 2)
	    ("edt" . 4)
	    ("cdt" . 5) ("est" . 5) ("yekt" . 5)
	    ("cst" . 6) ("mdt" . 6) ("yekst" . 6)
	    ("ict" . 7) ("mst" . 7) ("pdt" . 7)
	    ("myt" . 8) ("pst" . 8) ("hkt" . 8)
	    ("jst" . 9) ("kst" . 9)
	    ("nzdt" . 13))
	  *zone-strings*)

(hashlist '(("yesterday" . yesterday)  ("today" . today)
	    ("tomorrow" . tomorrow)   ("now" . now))
	  *special-strings*)

;;; Time/date format patterns are specified as lists of symbols repre-
;;; senting the elements.  Optional elements can be specified by enclosing
;;; them in parentheses.  Note that the order in which the patterns are
;;; specified below determines the order of search.

;;; Choices of pattern symbols are: second, minute, hour, day, month, year,
;;; time-divider, date-divider, am-pm, zone, izone, weekday, noon-midn, and
;;; any special symbol.

(defparameter *default-date-time-patterns*
  '(
    ;; Date formats.
    ((weekday) month (date-divider) day (date-divider) year (noon-midn))
    ((weekday) day (date-divider) month (date-divider) year (noon-midn))
    ((weekday) month (date-divider) day (noon-midn))
    (year (date-divider) month (date-divider) day (noon-midn))
    (month (date-divider) year (noon-midn))
    (year (date-divider) month (noon-midn))

    ((noon-midn) (weekday) month (date-divider) day (date-divider) year)
    ((noon-midn) (weekday) day (date-divider) month (date-divider) year)
    ((noon-midn) (weekday) month (date-divider) day)
    ((noon-midn) year (date-divider) month (date-divider) day)
    ((noon-midn) month (date-divider) year)
    ((noon-midn) year (date-divider) month)

    ;; Time formats.
    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone))
    (noon-midn)
    (hour (noon-midn))

    ;; Time/date combined formats.
    ((weekday) month (date-divider) day (date-divider) year
	   hour (time-divider) (minute) (time-divider) (secondp)
	   (am-pm) (date-divider) (zone))
    ((weekday) day (date-divider) month (date-divider) year
	 hour (time-divider) (minute) (time-divider) (secondp)
	 (am-pm) (date-divider) (zone))
    ((weekday) month (date-divider) day
	   hour (time-divider) (minute) (time-divider) (secondp)
	   (am-pm) (date-divider) (zone))
    (year (date-divider) month (date-divider) day
	  hour (time-divider) (minute) (time-divider) (secondp)
	  (am-pm) (date-divider) (zone))
    (month (date-divider) year
	   hour (time-divider) (minute) (time-divider) (secondp)
	   (am-pm) (date-divider) (zone))
    (year (date-divider) month
	  hour (time-divider) (minute) (time-divider) (secondp)
	  (am-pm) (date-divider) (zone))

    ;; FIX from mail messages
    ;; FIX check that these produce the correct times
    ; Mon, 11 Dec 2006 18:44:08 +0100 (CET)
    ; Mon, 06 Sep 1999 08:04:30 "GMT"
    ((weekday) day (date-divider) month (date-divider) year
         hour (time-divider) (minute) (time-divider) (secondp)
	 (am-pm) (date-divider) (timezone-prefix) (zone)
         (open-bracket) (zone) (close-bracket))
    ; Fri, 16 Jul 1999 15:20:35 GMT+0200
    ((weekday) day (date-divider) month (date-divider) year
	 hour (time-divider) (minute) (time-divider) (secondp)
	 (am-pm) (date-divider) (zone) (timezone-prefix) (izone))

    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone) (weekday) month (date-divider)
	  day (date-divider) year)
    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone) (weekday) day (date-divider)
	  month (date-divider) year)
    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone) (weekday) month (date-divider)
	  day)
    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone) year (date-divider) month
	  (date-divider) day)
    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone) month (date-divider) year)
    (hour (time-divider) (minute) (time-divider) (secondp) (am-pm)
	  (date-divider) (zone) year (date-divider) month)

    ;; Weird, non-standard formats.
    (weekday month day hour (time-divider) minute (time-divider)
	     secondp (am-pm)
	     (zone) year)
    ((weekday) day (date-divider) month (date-divider) year hour
     (time-divider) minute (time-divider) (secondp) (am-pm)
     (date-divider) (zone))
    ((weekday) month (date-divider) day (date-divider) year hour
     (time-divider) minute (time-divider) (secondp) (am-pm)
     (date-divider) (zone))

    ;; Special-string formats.
    (now (yesterday))
    ((yesterday) now)
    (now (today))
    ((today) now)
    (now (tomorrow))
    ((tomorrow) now)
    (yesterday (noon-midn))
    ((noon-midn) yesterday)
    (today (noon-midn))
    ((noon-midn) today)
    (tomorrow (noon-midn))
    ((noon-midn) tomorrow)

    (weekday)))

;;; HTTP header style date/time patterns: RFC1123/RFC822, RFC850, ANSI-C.
(defparameter *http-date-time-patterns*
  '(
    ;; RFC1123/RFC822 and RFC850.
    ((weekday) day (date-divider) month (date-divider) year
     hour time-divider minute (time-divider) (secondp) (timezone-prefix)
     izone)
    ((weekday) day (date-divider) month (date-divider) year
     hour time-divider minute (time-divider) (secondp) (zone))

    ;; ANSI-C.
    ((weekday) month day
     hour time-divider minute (time-divider) (secondp) year)))


;;; The decoded-time structure holds the time/date values which are
;;; eventually passed to 'encode-universal-time' after parsing.

;;; Note: Currently nothing is done with the day of the week.  It might
;;; be appropriate to add a function to see if it matches the date.

(defstruct decoded-time
  (second 0    :type integer)    ; Value between 0 and 59.
  (minute 0    :type integer)    ; Value between 0 and 59.
  (hour   0    :type integer)    ; Value between 0 and 24.
  (day    1    :type integer)    ; Value between 1 and 31.
  (month  1    :type integer)    ; Value between 1 and 12.
  (year   1900 :type integer)    ; Value above 1899 or between 0 and 99.
  (zone   0    :type rational)   ; Value between -24 and 24 inclusive.
  (dotw   0    :type integer))   ; Value between 0 and 6.

;;; Make-default-time returns a decoded-time structure with the default
;;; time values already set.  The default time is currently 00:00 on the
;;; current day, current month, current year, and current time-zone.

(defun make-default-time (def-sec def-min def-hour def-day
			   def-mon def-year def-zone def-dotw)
  (let ((default-time (make-decoded-time)))
    (multiple-value-bind (sec min hour day mon year dotw dst zone)
			 (get-decoded-time)
      (declare (ignore dst))
      (if def-sec
	  (if (eq def-sec :current)
	      (setf (decoded-time-second default-time) sec)
	      (setf (decoded-time-second default-time) def-sec))
	  (setf (decoded-time-second default-time) 0))
      (if def-min
	  (if (eq def-min :current)
	      (setf (decoded-time-minute default-time) min)
	      (setf (decoded-time-minute default-time) def-min))
	  (setf (decoded-time-minute default-time) 0))
      (if def-hour
	  (if (eq def-hour :current)
	      (setf (decoded-time-hour default-time) hour)
	      (setf (decoded-time-hour default-time) def-hour))
	  (setf (decoded-time-hour default-time) 0))
      (if def-day
	  (if (eq def-day :current)
	      (setf (decoded-time-day default-time) day)
	      (setf (decoded-time-day default-time) def-day))
	  (setf (decoded-time-day default-time) day))
      (if def-mon
	  (if (eq def-mon :current)
	      (setf (decoded-time-month default-time) mon)
	      (setf (decoded-time-month default-time) def-mon))
	  (setf (decoded-time-month default-time) mon))
      (if def-year
	  (if (eq def-year :current)
	      (setf (decoded-time-year default-time) year)
	      (setf (decoded-time-year default-time) def-year))
	  (setf (decoded-time-year default-time) year))
      (if def-zone
	  (if (eq def-zone :current)
	      (setf (decoded-time-zone default-time) zone)
	      (setf (decoded-time-zone default-time) def-zone))
	  (setf (decoded-time-zone default-time) zone))
      (if def-dotw
	  (if (eq def-dotw :current)
	      (setf (decoded-time-dotw default-time) dotw)
	      (setf (decoded-time-dotw default-time) def-dotw))
	  (setf (decoded-time-dotw default-time) dotw))
      default-time)))

;;; Converts the values in the decoded-time structure to universal time
;;; by calling encode-universal-time.
;;; If zone is in numerical form, tweeks it appropriately.

(defun convert-to-unitime (parsed-values)
  (let ((zone (decoded-time-zone parsed-values)))
    (encode-universal-time (decoded-time-second parsed-values)
			   (decoded-time-minute parsed-values)
			   (decoded-time-hour parsed-values)
			   (decoded-time-day parsed-values)
			   (decoded-time-month parsed-values)
			   (decoded-time-year parsed-values)
			   (if (or (> zone 24) (< zone -24))
			       (let ((new-zone (/ zone 100)))
				 (cond ((minusp new-zone) (- new-zone))
				       ((plusp new-zone) (- 24 new-zone))
				       ;; must be zero (GMT)
				       (t new-zone)))
			       zone))))

;;; Sets the current values for the time and/or date parts of the
;;; decoded time structure.

(defun set-current-value (values-structure &key (time nil) (date nil)
						(zone nil))
  (multiple-value-bind (sec min hour day mon year dotw dst tz)
      (get-decoded-time)
    (declare (ignore dst))
    (when time
      (setf (decoded-time-second values-structure) sec)
      (setf (decoded-time-minute values-structure) min)
      (setf (decoded-time-hour values-structure) hour))
    (when date
      (setf (decoded-time-day values-structure) day)
      (setf (decoded-time-month values-structure) mon)
      (setf (decoded-time-year values-structure) year)
      (setf (decoded-time-dotw values-structure) dotw))
    (when zone
      (setf (decoded-time-zone values-structure) tz))))

;;; Special function definitions.  To define a special substring, add
;;; a dotted pair consisting of the substring and a symbol in the
;;; *special-strings* hashlist statement above.  Then define a function
;;; here which takes one argument- the decoded time structure- and
;;; sets the values of the structure to whatever is necessary.  Also,
;;; add a some patterns to the patterns list using whatever combinations
;;; of special and pre-existing symbols desired.

(defun yesterday (parsed-values)
  (set-current-value parsed-values :date t :zone t)
  (setf (decoded-time-day parsed-values)
	(1- (decoded-time-day parsed-values))))

(defun today (parsed-values)
  (set-current-value parsed-values :date t :zone t))

(defun tomorrow (parsed-values)
  (set-current-value parsed-values :date t :zone t)
  (setf (decoded-time-day parsed-values)
	(1+ (decoded-time-day parsed-values))))

(defun now (parsed-values)
  (set-current-value parsed-values :time t))

;;; Predicates for symbols.  Each symbol has a corresponding function
;;; defined here which is applied to a part of the datum to see if
;;; it matches the qualifications.

(defun am-pm (string)
  (and (simple-string-p string)
       (cond ((string= string "am") 'am)
	     ((string= string "pm") 'pm)
	     (t nil))))

(defun noon-midn (string)
  (and (simple-string-p string)
       (cond ((string= string "noon") 'noon)
	     ((string= string "midnight") 'midn)
	     (t nil))))

(defun weekday (string)
  (and (simple-string-p string) (gethash string *weekday-strings*)))

(defun month (thing)
  (or (and (simple-string-p thing) (gethash thing *month-strings*))
      (and (integerp thing) (<= 1 thing 12))))

(defun zone (thing)
  (or (and (simple-string-p thing) (gethash thing *zone-strings*))
      (if (integerp thing)
	  (let ((zone (/ thing 100)))
	    (and (integerp zone) (<= -24 zone 24))))))

;;; Internet numerical time zone, e.g. RFC1123, in hours and minutes.
; (defun izone (thing)
;   (ed::msg "t ~A" thing)
;   (if (integerp thing)
;       (multiple-value-bind (hours mins)
; 	  (truncate thing 100)
; 	(and (<= -24 hours 24) (<= -59 mins 59)))))

(defun izone (thing)
  (if (or (integerp thing)
	  (and (stringp thing)
	       (eq (char thing 0) #\+)
	       (setq thing (parse-integer (subseq thing 1) :errorp ()))))
      (multiple-value-bind (hours mins)
	  (truncate thing 100)
	(<= -24 hours 24))))

(defun special-string-p (string)
  (and (simple-string-p string) (gethash string *special-strings*)))

(defun secondp (number)
  (and (integerp number) (<= 0 number 59)))

(defun minute (number)
  (and (integerp number) (<= 0 number 59)))

(defun hour (number)
  (and (integerp number) (<= 0 number 24)))

(defun day (number)
  (and (integerp number) (<= 1 number 31)))

(defun year (number)
  (and (integerp number)
       (or (<= 0 number 99)
	   (<= 1900 number))))

(defun time-divider (character)
  (and (characterp character)
       (member character time-dividers :test #'char=)))

(defun date-divider (character)
  (and (characterp character)
       (member character date-dividers :test #'char=)))

(defun timezone-prefix (character)
  (and (characterp character)
       (member character timezone-prefixes :test #'char=)))

(defun open-bracket (character)
  (and (characterp character)
       (member character open-brackets :test #'char=)))

(defun close-bracket (character)
  (and (characterp character)
       (member character close-brackets :test #'char=)))


;;; Match-substring takes a string argument and tries to match it with
;;; the strings in one of the four hash tables: *weekday-strings*, *month-
;;; strings*, *zone-strings*, *special-strings*.  It returns a specific
;;; keyword and/or the object it finds in the hash table.  If no match
;;; is made then it immediately signals an error.

(defun match-substring (substring)
  (let ((substring (nstring-downcase substring)))
    (or (let ((test-value (month substring)))
	  (if test-value (cons 'month test-value)))
	(let ((test-value (weekday substring)))
	  (if test-value (cons 'weekday test-value)))
	(let ((test-value (am-pm substring)))
	  (if test-value (cons 'am-pm test-value)))
	(let ((test-value (noon-midn substring)))
	  (if test-value (cons 'noon-midn test-value)))
	(let ((test-value (zone substring)))
	  (if test-value (cons 'zone test-value)))
	(let ((test-value (special-string-p substring)))
	  (if test-value  (cons 'special test-value)))
	(if *error-on-mismatch*
	    (error "\"~A\" is not a recognized word or abbreviation."
		   substring)
	    (return-from match-substring nil)))))

;;; Decompose-string takes the time/date string and decomposes it into a
;;; list of alphabetic substrings, numbers, and special divider characters.
;;; It matches whatever strings it can and replaces them with a dotted pair
;;; containing a symbol and value.

(defun decompose-string (string &key (start 0) (end (length string)) (radix 10))
  (do ((string-index start)
       (next-negative nil)
       (parts-list nil))
      ((eq string-index end) (nreverse parts-list))
    (let ((next-char (char string string-index))
	  (prev-char (if (= string-index start)
			 nil
			 (char string (1- string-index)))))
      (cond ((alpha-char-p next-char)
	     ;; Alphabetic character - scan to the end of the substring.
	     (do ((scan-index (1+ string-index) (1+ scan-index)))
		 ((or (eq scan-index end)
		      (not (alpha-char-p (char string scan-index))))
		  (let ((match-symbol (match-substring
				       (subseq string string-index scan-index))))
		    (if match-symbol
			(push match-symbol parts-list)
			(return-from decompose-string nil)))
		  (setf string-index scan-index))))
	    ((digit-char-p next-char radix)
	     ;; Numeric digit - convert digit-string to a decimal value.
	     (do ((scan-index string-index (1+ scan-index))
		  (numeric-value 0 (+ (* numeric-value radix)
				      (digit-char-p (char string scan-index) radix))))
		 ((or (eq scan-index end)
		      (not (digit-char-p (char string scan-index) radix)))
		  ;; If next-negative is t, set the numeric value to it's
		  ;; opposite and reset next-negative to nil.
		  (when next-negative
		    (setf next-negative nil)
		    (setf numeric-value (- numeric-value)))
		  (push numeric-value parts-list)
		  (setf string-index scan-index))))
	    ((and (char= next-char #\-)
		  (or (not prev-char)
		      (member prev-char whitespace-chars :test #'char=)))
	     ;; If we see a minus sign before a number, but not after one,
	     ;; it is not a date divider, but a negative offset from GMT, so
	     ;; set next-negative to t and continue.
	     (setf next-negative t)
	     (incf string-index))
	    ((member next-char time-dividers :test #'char=)
 	     ;; Time-divider - add it to the parts-list with symbol.
	     (push (cons 'time-divider next-char) parts-list)
	     (incf string-index))
	    ((member next-char date-dividers :test #'char=)
	     ;; Date-divider - add it to the parts-list with symbol.
	     (push (cons 'date-divider next-char) parts-list)
	     (incf string-index))
	    ((member next-char timezone-prefixes :test #'char=)
	     ;; Timezone-prefix - add it to the parts-list with symbol.
	     (push (cons 'timezone-prefix next-char) parts-list)
	     (incf string-index))
	    ((member next-char open-brackets :test #'char=)
	     ;; Open-bracket - add it to the parts-list with symbol.
	     (push (cons 'open-bracket next-char) parts-list)
	     (incf string-index))
	    ((member next-char close-brackets :test #'char=)
	     ;; Close-bracket - add it to the parts-list with symbol.
	     (push (cons 'close-bracket next-char) parts-list)
	     (incf string-index))
	    ((member next-char whitespace-chars :test #'char=)
	     ;; Whitespace character - ignore it completely.
	     (incf string-index))
	    ((char= next-char #\()
	     ;; Parenthesized string - scan to the end and ignore it.
	     (do ((scan-index string-index (1+ scan-index)))
		 ((or (eq scan-index end)
		      (char= (char string scan-index) #\)))
 		  (setf string-index (1+ scan-index)))))
	    (t
	     ;; Unrecognized character - barf voraciously.
	     (if *error-on-mismatch*
		 (error (concatenate 'simple-string ">>> " string
				     "~%~VT^-- Bogus character encountered here.")
			(+ string-index 4))
		 (return-from decompose-string nil)))))))

;;; Match-pattern-element tries to match a pattern element with a datum
;;; element and returns the symbol associated with the datum element if
;;; successful.  Otherwise nil is returned.

(defun match-pattern-element (pattern-element datum-element)
  (cond ((listp datum-element)
	 (let ((datum-type (if (eq (car datum-element) 'special)
			       (cdr datum-element)
			       (car datum-element))))
	   (if (eq datum-type pattern-element) datum-element)))
	((funcall pattern-element datum-element)
	 (cons pattern-element datum-element))
	(t nil)))

;;; Match-pattern matches a pattern against a datum, returning the
;;; pattern if successful and nil otherwise.

(defun match-pattern (pattern datum datum-length)
  (if (>= (length pattern) datum-length)
      (let ((form-list nil))
	(do ((pattern pattern (cdr pattern))
	     (datum datum (cdr datum)))
	    ((or (null pattern) (null datum))
	     (cond ((and (null pattern) (null datum))
		    (nreverse form-list))
		   ((null pattern) nil)
		   ((null datum) (dolist (element pattern
						  (nreverse form-list))
				   (if (not (listp element))
				       (return nil))))))
	  (let* ((pattern-element (car pattern))
		 (datum-element (car datum))
		 (optional (listp pattern-element))
		 (matching (match-pattern-element (if optional
						      (car pattern-element)
						      pattern-element)
						  datum-element)))
	    (cond (matching (let ((form-type (car matching)))
			      (or (eq form-type 'time-divider)
				  (eq form-type 'date-divider)
				  (eq form-type 'open-bracket)
				  (eq form-type 'close-bracket)
				  (eq form-type 'timezone-prefix)
				  (push matching form-list))))
		  (optional (push datum-element datum))
		  (t (return-from match-pattern nil))))))))

;;; Deal-with-noon-midn sets the decoded-time values to either noon
;;; or midnight depending on the argument form-value.  Form-value
;;; can be either 'noon or 'midn.

(defun deal-with-noon-midn (form-value parsed-values)
  (cond ((eq form-value 'noon)
	 (setf (decoded-time-hour parsed-values) 12))
	((eq form-value 'midn)
	 (setf (decoded-time-hour parsed-values) 0))
	(t (error "Unrecognized symbol: ~A" form-value)))
  (setf (decoded-time-minute parsed-values) 0)
  (setf (decoded-time-second parsed-values) 0))

;;; Deal-with-am-pm sets the decoded-time values to be in the am
;;; or pm depending on the argument form-value.  Form-value can
;;; be either 'am or 'pm.

(defun deal-with-am-pm (form-value parsed-values)
  (let ((hour (decoded-time-hour parsed-values)))
    (cond ((eq form-value 'am)
	   (cond ((eq hour 12)
		  (setf (decoded-time-hour parsed-values) 0))
		 ((not (<= 0 hour 12))
		  (if *error-on-mismatch*
		      (error "~D is not an AM hour." hour)))))
	  ((eq form-value 'pm)
	   (if (<= 0 hour 11)
	       (setf (decoded-time-hour parsed-values)
		     (mod (+ hour 12) 24))))
	  (t (error "~A isn't AM/PM - this shouldn't happen.")))))

;;; Internet numerical time zone, e.g. RFC1123, in hours and minutes.
; (defun deal-with-izone (form-value parsed-values)
;   (ed::msg "f ~A" form-value)
;   (multiple-value-bind (hours mins)
; 		       (truncate form-value 100)
;     (setf (decoded-time-zone parsed-values) (- (+ hours (/ mins 60))))))

(defun deal-with-izone (form-value parsed-values)
  (if (or (integerp form-value)
	  (and (stringp form-value)
	       (eq (char form-value 0) #\+)
	       (setq form-value (parse-integer (subseq form-value 1) :errorp ()))))
      (multiple-value-bind (hours mins)
			   (truncate form-value 100)
	(setf (decoded-time-zone parsed-values) (- (+ hours (/ mins 60)))))))

;;; Deal-with-dow sets the decoded-time values to match the day of week.

;;; FIX Can increase past the last day of the month.  (Can tomorrow do
;;; similar?)
(defun deal-with-dow (form-value parsed-values)
  (let ((dotw (decoded-time-dotw parsed-values)))
    (setf (decoded-time-dotw parsed-values) form-value)
    ;; Increase day to the required day of the week.
    (incf (decoded-time-day parsed-values)
	  (- (if (< form-value dotw)
		 (+ form-value 7)
		 form-value)
	     dotw))))

;;; Set-time-values uses the association list of symbols and values
;;; to set the time in the decoded-time structure.

(defun set-time-values (string-form parsed-values)
  (dolist (form-part string-form t)
    (let ((form-type (car form-part))
	  (form-value (cdr form-part)))
      (case form-type
	(secondp (setf (decoded-time-second parsed-values) form-value))
	(minute (setf (decoded-time-minute parsed-values) form-value))
	(hour (setf (decoded-time-hour parsed-values) (mod form-value 24)))
	(day (setf (decoded-time-day parsed-values) form-value))
	(month (setf (decoded-time-month parsed-values) form-value))
	(year (setf (decoded-time-year parsed-values) form-value))
	(zone (setf (decoded-time-zone parsed-values) form-value))
	(izone (deal-with-izone form-value parsed-values))
	(izone (deal-with-izone form-value parsed-values))
	(weekday (deal-with-dow form-value parsed-values))
	(am-pm (deal-with-am-pm form-value parsed-values))
	(noon-midn (deal-with-noon-midn form-value parsed-values))
	(special (funcall form-value parsed-values))
	(t (error "Unrecognized symbol in form list: ~A." form-type))))))

(defun parse-time (time-string &key (start 0) (end (length time-string))
			       (error-on-mismatch nil)
			       (patterns *default-date-time-patterns*)
			       (default-seconds nil) (default-minutes nil)
			       (default-hours nil) (default-day nil)
			       (default-month nil) (default-year nil)
			       (default-zone nil) (default-weekday nil))
  "Tries very hard to make sense out of the argument time-string and
   returns a single integer representing the universal time if successful.
   If not, it returns nil.  If the :error-on-mismatch keyword is true,
   parse-time will signal an error instead of returning nil.  Default
   values for each part of the time/date can be specified by the
   appropriate :default- keyword.  These keywords can be given a numeric
   value or the keyword :current to set them to the current value.  The
   default-default values are 00:00:00 on the current date, current
   time-zone."
  (setq *error-on-mismatch* error-on-mismatch)
  (let* ((string-parts (decompose-string time-string :start start :end end))
	 (parts-length (length string-parts))
	 (string-form (dolist (pattern patterns)
			(let ((match-result (match-pattern pattern
							   string-parts
							   parts-length)))
			  (if match-result (return match-result))))))
    (if string-form
	(let ((parsed-values (make-default-time default-seconds default-minutes
						default-hours default-day
						default-month default-year
						default-zone default-weekday)))
	  (set-time-values string-form parsed-values)
	  (convert-to-unitime parsed-values))
	(if *error-on-mismatch*
	    (error "\"~A\" is not a recognized time/date format." time-string)))))


;; FIX integrate into parse-time
(defun parse-iso8601-time (time-string)
  "Parse an ISO 8601 format string and return the universal time."
  (flet ((parse-delimited-string (string delimiter n)
           ;; Parses a delimited string and returns a list of n integers found in that string.
           (let ((answer (make-list n :initial-element 0)))
             (when (> (length string) 0)
               (loop for i upfrom 0
                     for start = 0 then (1+ end)
                     for end = (position delimiter string :start (1+ start))
                     do (setf (nth i answer)
                              (parse-integer (subseq string start end)))
                     when (null end) return t))
             (values-list answer)))
         (parse-fixed-field-string (string field-sizes)
           ;; Parses a string with fixed length fields and returns a list of integers found in that string.
           (let ((answer (make-list (length field-sizes) :initial-element 0)))
             (loop with len = (length string)
                   for start = 0 then (+ start field-size)
                   for field-size in field-sizes
                   for i upfrom 0
                   while (< start len)
                   do (setf (nth i answer)
                            (parse-integer (subseq string start (+ start field-size)))))
             (values-list answer))))
    (flet ((parse-iso8601-date (date-string)
             (let ((hyphen-pos (position #\- date-string)))
               (if hyphen-pos
                 (parse-delimited-string date-string #\- 3)
                 (parse-fixed-field-string date-string '(4 2 2)))))
           (parse-iso8601-timeonly (time-string)
             (let* ((colon-pos (position #\: time-string))
                    (zone-pos (or (position #\- time-string)
                                  (position #\+ time-string)
                                  (position #\Z time-string)))
                    (timeonly-string (subseq time-string 0 zone-pos))
                    (zone-string (when zone-pos (subseq time-string (1+ zone-pos))))
                    (time-zone nil))
               (when zone-pos
                 (multiple-value-bind (zone-h zone-m)
                                      (parse-delimited-string zone-string #\: 2)
                   (setq time-zone (+ zone-h (/ zone-m 60)))
                   (when (char= (char time-string zone-pos) #\-)
                     (setq time-zone (- time-zone)))))
               (multiple-value-bind (hh mm ss)
                                    (if colon-pos
                                      (parse-delimited-string timeonly-string #\: 3)
                                      (parse-fixed-field-string timeonly-string '(2 2 2)))
                   (values hh mm ss time-zone)))))
    (let ((time-separator (position #\T time-string)))
      (multiple-value-bind (year month date)
                           (parse-iso8601-date
                            (subseq time-string 0 time-separator))
        (if time-separator
          (multiple-value-bind (hh mm ss zone)
                               (parse-iso8601-timeonly
                                (subseq time-string (1+ time-separator)))
            (if zone
              ;; Tricky:  Sign of time zone is reversed in ISO 8601
              ;; relative to Common Lisp convention!
              (encode-universal-time ss mm hh date month year (- zone))
              (encode-universal-time ss mm hh date month year)))
          (encode-universal-time 0 0 0 date month year)))))))
