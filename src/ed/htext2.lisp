;;; Text-manipulation functions: regions and movement.

(in-package "EDI")

(export '(region-to-string string-to-region line-to-region
	  previous-character next-character count-lines count-characters
	  word-start word-end line-start line-end buffer-start
	  buffer-end move-mark mark-before mark-after character-offset
	  line-offset region-bounds set-region-bounds *print-region*))


(defun region-to-string (region)
  "Return a string containing the characters in $region.  Delimit lines
   with the newline character."
  (close-line)
  (let* ((dst-length (count-characters region))
	 (string (make-string dst-length))
	 (start-mark (region-start region))
	 (end-mark (region-end region))
	 (start-line (mark-line start-mark))
	 (end-line (mark-line end-mark))
	 (start-charpos (mark-charpos start-mark)))
    (declare (simple-string string))
    (if (eq start-line end-line)
	(%sp-byte-blt (line-chars start-line) start-charpos string 0
		      dst-length)
	(let ((index ()))
	  (let* ((line-chars (line-chars start-line))
		 (dst-end (- (length line-chars) start-charpos)))
	    (declare (simple-string line-chars))
	    (%sp-byte-blt line-chars start-charpos string 0 dst-end)
	    (setf (char string dst-end) #\newline)
	    (setq index (1+ dst-end)))
	  (do* ((line (line-next start-line) (line-next line))
		(chars (line-chars line) (line-chars line)))
	       ((eq line end-line)
		(%sp-byte-blt (line-chars line) 0 string index dst-length))
	    (declare (simple-string chars))
	    (%sp-byte-blt (line-chars line) 0 string index
			  (incf index (length chars)))
	    (setf (char string index) #\newline)
	    (setq index (1+ index)))))
    string))


(defun string-to-region (string)
  "Return a region containing the characters in $string."
  (let* ((string (if (simple-string-p string)
		     string (coerce string 'simple-string)))
	 (end (length string)))
    (declare (simple-string string))
    (do* ((index 0)
	  (buffer (incf *disembodied-buffer-counter*))
	  (previous-line)
	  (line (make-line :%buffer buffer))
	  (first-line line))
	 (())
      (let ((right-index (%sp-find-character string index end #\newline)))
	(cond (right-index
	       (let* ((length (- right-index index))
		      (chars (make-string length)))
		 (%sp-byte-blt string index chars 0 length)
		 (setf (line-chars line) chars))
	       (setq index (1+ right-index))
	       (setq previous-line line)
	       (setq line (make-line :%buffer buffer))
	       (setf (line-next previous-line) line)
	       (setf (line-previous line) previous-line))
	      (t
	       (let* ((length (- end index))
		      (chars (make-string length)))
		 (%sp-byte-blt string index chars 0 length)
		 (setf (line-chars line) chars))
	       (return (renumber-region
			(internal-make-region
			 (mark first-line 0 :right-inserting)
			 (mark line (length (line-chars line))
			       :left-inserting))))))))))

(defun line-to-region (line)
  "Return a region containing all the characters on $line.  The first mark
   is :right-inserting and the last is :left-inserting."
  (internal-make-region (mark line 0 :right-inserting)
			(mark line (line-length* line) :left-inserting)))


(defun previous-character (mark)
  "Return the character immediately before $mark, or () if $mark is before
   the first character."
  (let ((line (mark-line mark))
	(charpos (mark-charpos mark)))
    (if (= charpos 0)
	(if (line-previous line)
	    #\newline
	    nil)
	(if (eq line open-line)
	    (char (the simple-string open-chars)
		  (if (<= charpos left-open-pos)
		      (1- charpos)
		      (1- (+ right-open-pos (- charpos left-open-pos)))))
	    (schar (line-chars line) (1- charpos))))))

;; FIX consider n arg  eg (next-character mark 2) for char after next char
(defun next-character (mark)
  "Return the character immediately after $mark, or () if $mark is after
   the last character."
  (let ((line (mark-line mark))
	(charpos (mark-charpos mark)))
    (if (eq line open-line)
	(if (= charpos (- line-cache-length (- right-open-pos left-open-pos)))
	    (if (line-next line)
		#\newline
		nil)
	    (schar open-chars
		   (if (< charpos left-open-pos)
		       charpos
		       (+ right-open-pos (- charpos left-open-pos)))))
	(let ((chars (line-chars line)))
	  (if (= charpos (strlen chars))
	      (if (line-next line)
		  #\newline
		  nil)
	      (schar chars charpos))))))

;;; %Set-Next-Character  --  Internal
;;;
;;; This is the setf form for Next-Character.  Since we may change a
;;; character to or from a newline, we must be prepared to split and join
;;; lines.  We cannot just delete a character and insert the new one
;;; because the marks would not be right.
;;;
(defun %set-next-character (mark character)
  (let* ((line (mark-line mark))
	 (buffer (line-%buffer line))
	 (next (line-next line)))
    (modifying-buffer buffer
      (modifying-line line mark)
      (cond ((= right-open-pos line-cache-length)
	     ;; The mark is at the end of the line.
	     (or next
		 (error "~S has no next character, so it cannot be set." mark))
	     (unless (char= character #\newline)
	       ;; If the character is no longer a newline then mash two
	       ;; lines together.
	       (let ((chars (line-chars next)))
		 (declare (simple-string chars))
		 (setq right-open-pos (- line-cache-length (length chars)))
		 (when (<= right-open-pos left-open-pos)
		   (grow-open-chars (* (+ (length chars) left-open-pos 1) 2)))
		 (%sp-byte-blt chars 0 open-chars right-open-pos
			       line-cache-length)
		 (setf (schar open-chars left-open-pos) character)
		 (incf left-open-pos))
	       (move-some-marks (charpos next line)
				(+ charpos left-open-pos))
	       (setq next (line-next next))
	       (setf (line-next line) next)
	       (when next (setf (line-previous next) line))))
	    ((char= character #\newline)
	     ;; The char is being changed to a newline, so we must split lines.
	     (incf right-open-pos)
	     (let* ((len (- line-cache-length right-open-pos))
		    (chars (make-string len))
		    (new (make-line :chars chars  :previous line
				    :next next  :%buffer buffer)))
	       (%sp-byte-blt open-chars right-open-pos chars 0 len)
	       (maybe-move-some-marks* (charpos line new) left-open-pos
				       (- charpos left-open-pos 1))
	       (setf (line-next line) new)
	       (when next (setf (line-previous next) new))
	       (setq right-open-pos line-cache-length)
	       (number-line new)))
	    (t
	     (setf (char (the simple-string open-chars) right-open-pos)
		   character)))))
  character)

;;; %Set-Previous-Character  --  Internal
;;;
;;; The setf form for Previous-Character.  We just Temporarily move the
;;; mark back one and call %Set-Next-Character.
;;;
(defun %set-previous-character (mark character)
  (or (mark-before mark)
      (error "~S has no previous character, so it cannot be set." mark))
  (%set-next-character mark character)
  (mark-after mark)
  character)


(defun count-lines (region)
  "Return the number of lines in the $region, including the first and last
   lines.  Associate a newline with the line it follows, thus count a
   region containing some text followed by one newline is one line, and the
   same line prefixed with a newline as two lines."
  (do ((line (mark-line (region-start region)) (line-next line))
       (count 1 (1+ count))
       (last-line (mark-line (region-end region))))
      ((eq line last-line) count)))

(defun count-characters (region)
  "Return the number of characters in $region.  Count line breaks as one
   character."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end)))
    (if (eq first-line last-line)
	(- (mark-charpos end) (mark-charpos start))
	(do ((line (line-next first-line) (line-next line))
	     (count (1+ (- (line-length* first-line) (mark-charpos start)))))
	    ((eq line last-line)
	     (+ count (mark-charpos end)))
	  (setq count (+ 1 count (line-length* line)))))))


#[ Moving Marks

These functions destructively modify marks to point to new positions.  Other
sections of this document describe mark moving routines specific to higher
level text forms than characters and lines, such as words, sentences,
paragraphs, Lisp forms, etc.

{function:ed:move-to-position}
{function:ed:move-mark}
{function:ed:line-start}
{function:ed:line-end}
{function:ed:buffer-start}
{function:ed:buffer-end}
{function:ed:mark-before}
{function:ed:mark-after}
{function:ed:character-offset}
{function:ed:line-offset}
]#

(defun word-start (mark)
  "If $mark is on a word move $mark to the beginning of the word."
  (reverse-find-attribute mark :word-delimiter))

(defun word-end (mark)
  "If $mark is on a word move $mark to the end of the word."
  (find-attribute mark :word-delimiter))

(defun line-start (mark &optional (line (mark-line mark)))
  "Change $mark to point to the beginning of the current line, or the
   beginning of $line if line is given.  Return $mark."
  (if line (change-line mark line))
  (setf (mark-charpos mark) 0)
  mark)

(defun line-end (mark &optional line)
  "Change $mark to point to the end of the current line or the end of $line
   if $line is given.  Return $mark."
  (if line
      (change-line mark line)
      (setq line (mark-line mark)))
  (setf (mark-charpos mark) (line-length* line))
  mark)

(defun buffer-start (mark &optional (buffer (line-buffer (mark-line mark))))
  "Change $mark to point to the beginning of the current buffer, or of
   $buffer if $buffer is set."
  (or buffer (error "Mark ~S must point into a buffer." mark))
  (move-mark mark (buffer-start-mark buffer)))

(defun buffer-end (mark &optional (buffer (line-buffer (mark-line mark))))
  "Change $mark to point to the end of the current buffer, or of $buffer if
   $buffer is set."
  (or buffer (error "Mark ~S must point into a buffer." mark))
  (move-mark mark (buffer-end-mark buffer)))

(defun move-mark (mark new-position)
  "Change $mark to point to the same position as $new-position, and return
   $mark."
  (let ((line (mark-line new-position)))
    (change-line mark line))
  (setf (mark-charpos mark) (mark-charpos new-position))
  mark)


;; FIX these very similar character-offset?

(defun mark-before (mark)
  "Change $mark to point one character before the current position.  If
   mark points before the first character, then return () and leave mark as
   it is."
  (let ((charpos (mark-charpos mark)))
    (cond ((zerop charpos)
	   (let ((prev (line-previous (mark-line mark))))
	     (when prev
	       (always-change-line mark prev)
	       (setf (mark-charpos mark) (line-length* prev))
	       mark)))
	  (t
	   (setf (mark-charpos mark) (1- charpos))
	   mark))))

(defun mark-after (mark)
  "Change $mark to point one character after the current position.  If
   $mark points after the last character, then return nil and leave mark as
   it is."
  (let ((line (mark-line mark))
	(charpos (mark-charpos mark)))
    (cond ((= charpos (line-length* line))
	   (let ((next (line-next line)))
	     (when next
	       (always-change-line mark next)
	       (setf (mark-charpos mark) 0)
	       mark)))
	  (t
	   (setf (mark-charpos mark) (1+ charpos))
	   mark))))

;; FIX alias/rename char-offset?
(defun character-offset (mark n)
  "Change $mark to point $n characters after (or before if $n is negative)
   the current position.  If there are less than $n characters after (or
   before) the mark, then return () and leave mark as it is."
  (let ((charpos (mark-charpos mark)))
    (if (< n 0)
	(let ((n (- n)))
	  (if (< charpos n)
	      (do ((line (line-previous (mark-line mark)) (line-previous line))
		   (n (- n charpos 1)))
		  ((null line) nil)
		(let ((length (line-length* line)))
		  (cond ((<= n length)
			 (always-change-line mark line)
			 (setf (mark-charpos mark) (- length n))
			 (return mark))
			(t
			 (setq n (- n (1+ length)))))))
	      (progn (setf (mark-charpos mark) (- charpos n))
		     mark)))
	(let* ((line (mark-line mark))
	       (length (line-length* line)))
	  (if (> (+ charpos n) length)
	      (do ((line (line-next line) (line-next line))
		   (n (- n (1+ (- length charpos)))))
		  ((null line) nil)
		(let ((length (line-length* line)))
		  (cond ((<= n length)
			 (always-change-line mark line)
			 (setf (mark-charpos mark) n)
			 (return mark))
			(t
			 (setq n (- n (1+ length)))))))
	      (progn (setf (mark-charpos mark) (+ charpos n))
		     mark))))))

;; FIX (n 1)?  like (character-offset
(defun line-offset (mark n &optional charpos)
  "Change $mark to point $n lines after (or before if $n is negative) the
   current position.  The character position of the resulting mark is

         (min (line-length resulting-line) (mark-charpos mark))

   if charpos is (), or

         (min (line-length resulting-line) charpos)

   if it is specified.  As with `character-offset', if there are fewer than
   $n lines then return () and leave mark as it is."
  (declare (type integer n))
  (if (< n 0)
      (do ((line (mark-line mark) (line-previous line))
	   (n n (1+ n)))
	  ((null line) nil)
	(when (= n 0)
	  (always-change-line mark line)
	  (setf (mark-charpos mark)
		(if charpos
		    (min (line-length line) charpos)
		    (min (line-length line) (mark-charpos mark))))
	  (return mark)))
      (do ((line (mark-line mark) (line-next line))
	   (n n (1- n)))
	  ((null line) nil)
	(when (= n 0)
	  (change-line mark line)
	  (setf (mark-charpos mark)
		(if charpos
		    (min (line-length line) charpos)
		    (min (line-length line) (mark-charpos mark))))
	  (return mark)))))

;;; region-bounds  --  Public
;;;
(defun region-bounds (region)
  "Return as multiple-values the starting and ending marks of $region."
  (values (region-start region) (region-end region)))

(defun set-region-bounds (region start end)
  "Set the start and end of $region to the marks $start and $end.  Throw an
   error if $start is after $end or if the marks are in separate buffers."
  (let ((sl (mark-line start))
	(el (mark-line end)))
    (or (eq (line-%buffer sl) (line-%buffer el))
	(if (or (> (line-number sl) (line-number el))
		(and (eq sl el) (> (mark-charpos start) (mark-charpos end))))
	    (error "Marks ~S and ~S cannot be made into a region." start end)))
    (setf (region-start region) start  (region-end region) end))
  region)


;;;; Debugging stuff.

(defun slf (string)
  "For a good time, figure out what this function does, and why it was written."
  (delete #\linefeed (the simple-string string)))

(defun %print-whole-line (structure stream)
  (cond ((eq structure open-line)
	 (write-string open-chars stream :end left-open-pos)
	 (write-string open-chars stream :start right-open-pos
		       :end line-cache-length))
	(t
	 (write-string (line-chars structure) stream))))

(defun %print-before-mark (mark stream)
  (if (mark-line mark)
      (let* ((line (mark-line mark))
	     (chars (line-chars line))
	     (charpos (mark-charpos mark))
	     (length (line-length line)))
	(declare (simple-string chars))
	(cond ((or (> charpos length) (< charpos 0))
	       (write-string "{bad mark}" stream))
	      ((eq line open-line)
	       (cond ((< charpos left-open-pos)
		      (write-string open-chars stream :end charpos))
		     (t
		      (write-string open-chars stream :end left-open-pos)
		      (let ((p (+ charpos (- right-open-pos left-open-pos))))
			(write-string open-chars stream  :start right-open-pos
				      :end p)))))
	      (t
	       (write-string chars stream :end charpos))))
      (write-string "{deleted mark}" stream)))


(defun %print-after-mark (mark stream)
  (if (mark-line mark)
      (let* ((line (mark-line mark))
	     (chars (line-chars line))
	     (charpos (mark-charpos mark))
	     (length (line-length line)))
	(declare (simple-string chars))
	(cond ((or (> charpos length) (< charpos 0))
	       (write-string "{bad mark}" stream))
	      ((eq line open-line)
	       (cond ((< charpos left-open-pos)
		      (write-string open-chars stream  :start charpos
				    :end left-open-pos)
		      (write-string open-chars stream  :start right-open-pos
				    :end line-cache-length))
		     (t
		      (let ((p (+ charpos (- right-open-pos left-open-pos))))
			(write-string open-chars stream :start p
				      :end line-cache-length)))))
	      (t
	       (write-string chars stream  :start charpos  :end length))))
      (write-string "{deleted mark}" stream)))

(defun %print-hline (structure stream d)
  (declare (ignore d))
  (write-string "#<Editor Line \"" stream)
  (%print-whole-line structure stream)
  (write-string "\">" stream))

(defun %print-hmark (structure stream d)
  (declare (ignore d))
  (write-string "#<Editor Mark \"" stream)
  (%print-before-mark structure stream)
  (write-string "/\\" stream)
  (%print-after-mark structure stream)
  (write-string "\">" stream))

(defvar *print-region* 10
  "The number of lines to print out of a region, or NIL if none.")

(defun %print-hregion (region stream d)
  (declare (ignore d))
  (write-string "#<Editor Region \"" stream)
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end)))
    (cond
     ((not (and (linep first-line) (linep last-line)
		(eq (line-%buffer first-line) (line-%buffer last-line))
		(mark<= start end)))
      (write-string "{bad region}" stream))
     (*print-region*
      (cond ((eq first-line last-line)
	     (let ((cs (mark-charpos start))
		   (ce (mark-charpos end))
		   (len (line-length first-line)))
	       (cond
		((or (< cs 0) (> ce len))
		 (write-string "{bad region}" stream))
		((eq first-line open-line)
		 (let ((gap (- right-open-pos left-open-pos)))
		   (cond
		    ((<= ce left-open-pos)
		     (write-string open-chars stream  :start cs  :end ce))
		    ((>= cs left-open-pos)
		     (write-string open-chars stream  :start (+ cs gap)
				   :end (+ ce gap)))
		    (t
		     (write-string open-chars stream :start cs
				   :end left-open-pos)
		     (write-string open-chars stream :start right-open-pos
				   :end (+ gap ce))))))
		(t
		 (write-string (line-chars first-line) stream  :start cs
			       :end ce)))))
	    (t
	     (%print-after-mark start stream)
	     (write-char #\/ stream)
	     (do ((line (line-next first-line) (line-next line))
		  (last-line (mark-line end))
		  (cnt *print-region* (1- cnt)))
		 ((or (eq line last-line)
		      (when (zerop cnt) (write-string "..." stream) t))
		  (%print-before-mark end stream))
	       (%print-whole-line line stream)
	       (write-char #\/ stream)))))
     (t
      (write-string "{mumble}" stream))))
  (write-string "\">" stream))

(defun %print-hbuffer (structure stream d)
  (declare (ignore d))
  (write-string "#<Editor Buffer \"" stream)
  (write-string (buffer-name structure) stream)
  (write-string "\">" stream))
