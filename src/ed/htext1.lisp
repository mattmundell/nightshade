;;; Text-Manipulation functions for lines and marks.

;; FIX rename etext1.lisp or better

(in-package "EDI")

(export '(line-length line-buffer line-string line-character
	  mark mark-kind mark-buffer
	  copy-mark delete-mark move-to-position region make-empty-region
	  start-line-p end-line-p empty-line-p blank-line-p blank-before-p
	  blank-after-p same-line-p mark< mark<= mark> mark>= mark=
	  line< line<= line> line>= first-line-p last-line-p buffer-signature
	  lines-related))

#[ Altering and Searching Text

[ Altering Text                 ]
[ Text Predicates               ]
[ Kill Ring                     ]
[ Active Regions                ]
[ Searching and Replacing (ext) ]
]#


;;;; Representation of Text.

;;; Line cache mechanism.
;;;
;;; The "open line" is used when inserting and deleting characters from a line.
;;; It acts as a cache that provides a more flexible (but more expensive)
;;; representation of the line for multiple insertions and deletions.  When a
;;; line is open, it is represented as a vector of characters and two indices:
;;;
;;; +-----------------------------------------------------------+
;;; | F | O | O |   | B | x | x | x | x | x | x | x | x | A | R |
;;; +-----------------------------------------------------------+
;;;			  ^			          ^
;;;		      Left Pointer		     Right Pointer
;;;
;;; The open line is represented by 4 special variables:
;;;	Open-Line: the line object that is opened
;;;	Open-Chars: the vector of cached characters
;;;	Left-Open-Pos: index of first free character in the gap
;;;	Right-Open-Pos: index of first used character after the gap
;;;
;;; Note: Any modificiation of the line cache must be protected by
;;; Block-Interrupts.  This is done automatically by modifying-buffer;
;;; other users beware.

(defvar line-cache-length 200
  "Length of Open-Chars.")

(defvar open-line ()
  "Line open for hacking on.")

(defvar open-chars (make-string line-cache-length)
  "Vector of characters for hacking on.")

(defvar left-open-pos 0
  "Index to first free character to left of mark in Open-Chars.")

(defvar right-open-pos 0
  "Index to first used character to right of mark in Open-Chars.")

(defun grow-open-chars (&optional (new-length (* line-cache-length 2)))
  "Grows Open-Chars to twice its current length, or the New-Length if
  specified."
  (let ((new-chars (make-string new-length))
	(new-right (- new-length (- line-cache-length right-open-pos))))
    (%sp-byte-blt open-chars 0 new-chars 0 left-open-pos)
    (%sp-byte-blt open-chars right-open-pos new-chars new-right new-length)
    (setq right-open-pos new-right)
    (setq open-chars new-chars)
    (setq line-cache-length new-length)))

(defun close-line ()
  "Stuffs the characters in the currently open line back into the line they
   came from, and sets open-line to Nil."
  (when open-line
    (block-interrupts
      (let* ((length (+ left-open-pos (- line-cache-length right-open-pos)))
	     (string (make-string length)))
	(%sp-byte-blt open-chars 0 string 0 left-open-pos)
	(%sp-byte-blt open-chars right-open-pos string left-open-pos length)
	(setf (line-chars open-line) string)
	(setf open-line nil)))))

;;; We stick decrementing fixnums in the line-chars slot of the open line
;;; so that whenever the cache is changed the chars are no longer eq.
;;; They decrement so that they will be distinct from positive fixnums,
;;; which might mean something else.
;;;
(defvar *cache-modification-tick* -1
  "The counter for the fixnums we stick in the chars of the cached line.")

(defun open-line (line mark)
  "Closes the current Open-Line and opens the given Line at the Mark.
   Don't call this, use modifying-line instead."
  (cond ((eq line open-line)
	 (let ((charpos (mark-charpos mark)))
	   (cond ((< charpos left-open-pos)	; BLT 'em right!
		  (let ((right-start (- right-open-pos
					(- left-open-pos charpos))))
		    (%sp-byte-blt open-chars charpos
				  open-chars right-start
				  right-open-pos)
		    (setq left-open-pos charpos)
		    (setq right-open-pos right-start)))
		 ((> charpos left-open-pos)	; BLT 'em left!
		  (%sp-byte-blt open-chars right-open-pos
				open-chars left-open-pos
				charpos)
		  (setq right-open-pos
			(+ right-open-pos (- charpos left-open-pos)))
		  (setq left-open-pos charpos)))))

	(t
	 (close-line)
	 (let* ((chars (line-chars line))
		(len (length chars)))
	   (declare (simple-string chars))
	   (when (> len line-cache-length)
	     (setq line-cache-length (* len 2))
	     (setq open-chars (make-string line-cache-length)))
	   (setq open-line line)
	   (setq left-open-pos (mark-charpos mark))
	   (setq right-open-pos
		 (- line-cache-length (- (length chars) left-open-pos)))
	   (%sp-byte-blt chars 0 open-chars 0 left-open-pos)
	   (%sp-byte-blt chars left-open-pos open-chars right-open-pos
			 line-cache-length)))))

;;;; Some macros for Text hacking.

(defmacro modifying-line (line mark)
  "Checks to see if the Line is already opened at the Mark, and calls Open-Line
  if not.  Sticks a tick in the open-line's chars.  This must be called within
  the body of a Modifying-Buffer form."
  `(progn
    (unless (and (= (mark-charpos ,mark) left-open-pos) (eq ,line open-line))
      (open-line ,line ,mark))
    (setf (line-chars open-line) (decf *cache-modification-tick*))))

;;; Now-Tick tells us when now is and isn't.
;;;
(defvar now-tick 0 "Current tick.")

(defmacro tick ()
  "Increments the ``now'' tick."
  `(incf now-tick))

;;; Yeah, the following is kind of obscure, but at least it doesn't
;;; call Bufferp twice.  The block-interrupts is just to prevent
;;; people from being screwed by interrupting when the buffer structure
;;; is in an inconsistent state.
;;;
(defmacro modifying-buffer (buffer &body forms)
  "Does groovy stuff for modifying buffers." ; FIX
  `(progn
     (when (bufferp ,buffer)
       (or (buffer-writable ,buffer)
	   (editor-error "Buffer ~S is read only." (buffer-name ,buffer)))
       (when (< (buffer-modified-tick ,buffer)
		(buffer-unmodified-tick ,buffer))
	 ;; Check if file has changed on disk.
	 (let ((buffer-pn (buffer-pathname ,buffer)))
	   (when buffer-pn
	     (let* ((pathname (namestring buffer-pn))
		    (date (buffer-write-date ,buffer))
		    (file-date (when (probe-file pathname) (file-write-date pathname))))
	       (when (and date file-date
			  (equal (make-pathname :version nil :defaults buffer-pn)
				 (make-pathname :version nil :defaults pathname)))
		 (or (= date file-date)
		     (prompt-for-yes-or-no :prompt (list
 "File has changed on disk since it was read.~%Modify the buffer anyway? "
 pathname)
					   :help
 "Type No to cancel or Yes to modify the buffer."
                                           :default nil)
		     (editor-error "Modification cancelled."))))))
	 ;; Run hooks.
	 (invoke-hook ed::buffer-modified-hook ,buffer t))
       (setf (buffer-modified-tick ,buffer) (tick)))
     (prog1 (block-interrupts ,@forms)
       (when (bufferp ,buffer)
	 (push ,buffer *changed-buffers*)))))
;	 (invoke-hook ed::after-change-hook ,buffer)))))

(defmacro always-change-line (mark new-line)
  (let ((scan (gensym))
	(prev (gensym))
	(old-line (gensym)))
    `(let ((,old-line (mark-line ,mark)))
       (when (not (eq (mark-%kind ,mark) :temporary))
	 (do ((,scan (line-marks ,old-line) (cdr ,scan))
	      (,prev () ,scan))
	     ((eq (car ,scan) ,mark)
	      (if ,prev
		  (setf (cdr ,prev) (cdr ,scan))
		  (setf (line-marks ,old-line) (cdr ,scan)))
	      (setf (cdr ,scan) (line-marks ,new-line)
		    (line-marks ,new-line) ,scan))))
       (setf (mark-line ,mark) ,new-line))))

(defmacro change-line (mark new-line)
  (let ((scan (gensym))
	(prev (gensym))
	(old-line (gensym)))
    `(let ((,old-line (mark-line ,mark)))
       (unless (or (eq (mark-%kind ,mark) :temporary)
		   (eq ,old-line ,new-line))
	 (do ((,scan (line-marks ,old-line) (cdr ,scan))
	      (,prev () ,scan))
	     ((eq (car ,scan) ,mark)
	      (if ,prev
		  (setf (cdr ,prev) (cdr ,scan))
		  (setf (line-marks ,old-line) (cdr ,scan)))
	      (setf (cdr ,scan) (line-marks ,new-line)
		    (line-marks ,new-line) ,scan))))
       (setf (mark-line ,mark) ,new-line))))

;;; MOVE-SOME-MARKS  --  Internal
;;;
;;; Move all the marks from the line Old to New, performing some function
;;; on their charpos'es.  Charpos is bound to the charpos of the mark, and
;;; the result of the evaluation of the last form in the body should be the
;;; new charpos for the mark.  If New is not supplied then the marks are
;;; left on the old line.
;;;
(defmacro move-some-marks ((charpos old &optional new) &body body)
  (let ((last (gensym)) (mark (gensym)) (marks (gensym)))
    (if new
	`(let ((,marks (line-marks ,old)))
	   (do ((,mark ,marks (cdr ,mark))
		(,last nil ,mark))
	       ((null ,mark)
		(when ,last
		  (shiftf (cdr ,last) (line-marks ,new) ,marks))
		(setf (line-marks ,old) nil))
	     (setf (mark-line (car ,mark)) ,new)
	     (setf (mark-charpos (car ,mark))
		   (let ((,charpos (mark-charpos (car ,mark))))
		     ,@body))))
	`(dolist (,mark (line-marks ,old))
	   (setf (mark-charpos ,mark)
		 (let ((,charpos (mark-charpos ,mark)))
		   ,@body))))))

;;; Maybe-Move-Some-Marks  --  Internal
;;;
;;; Like Move-Some-Marks, but only moves the mark if the charpos is greater
;;; than the bound, OR the charpos equals the bound and the marks %kind is
;;; :left-inserting.
;;;
(defmacro maybe-move-some-marks ((charpos old &optional new) bound &body body)
  (let ((mark (gensym)) (marks (gensym)) (prev (gensym)))
    (if new
	`(do ((,mark (line-marks ,old))
	      (,marks (line-marks ,new))
	      (,prev ()))
	     ((null ,mark)
	      (setf (line-marks ,new) ,marks))
	   (let ((,charpos (mark-charpos (car ,mark))))
	     (cond
	       ((or (> ,charpos ,bound)
		    (and (= ,charpos ,bound)
			 (eq (mark-%kind (car ,mark)) :left-inserting)))
		(setf (mark-line (car ,mark)) ,new)
		(setf (mark-charpos (car ,mark)) (progn ,@body))
		(if ,prev
		    (setf (cdr ,prev) (cdr ,mark))
		    (setf (line-marks ,old) (cdr ,mark)))
		(rotatef (cdr ,mark) ,marks ,mark))
	       (t
		(setq ,prev ,mark  ,mark (cdr ,mark))))))
	`(dolist (,mark (line-marks ,old))
	   (let ((,charpos (mark-charpos ,mark)))
	     (when (or (> ,charpos ,bound)
		       (and (= ,charpos ,bound)
			    (eq (mark-%kind ,mark) :left-inserting)))
	       (setf (mark-charpos ,mark) (progn ,@body))))))))


;;; Maybe-Move-Some-Marks*  --  Internal
;;;
;;; Like Maybe-Move-Some-Marks, but ignores the mark %kind.
;;;
(defmacro maybe-move-some-marks* ((charpos old &optional new) bound &body body)
  (let ((mark (gensym)) (marks (gensym)) (prev (gensym)))
    (if new
	`(do ((,mark (line-marks ,old))
	      (,marks (line-marks ,new))
	      (,prev ()))
	     ((null ,mark)
	      (setf (line-marks ,new) ,marks))
	   (let ((,charpos (mark-charpos (car ,mark))))
	     (cond
	       ((> ,charpos ,bound)
		(setf (mark-line (car ,mark)) ,new)
		(setf (mark-charpos (car ,mark)) (progn ,@body))
		(if ,prev
		    (setf (cdr ,prev) (cdr ,mark))
		    (setf (line-marks ,old) (cdr ,mark)))
		(rotatef (cdr ,mark) ,marks ,mark))
	       (t
		(setq ,prev ,mark  ,mark (cdr ,mark))))))
	`(dolist (,mark (line-marks ,old))
	   (let ((,charpos (mark-charpos ,mark)))
	     (when (> ,charpos ,bound)
	       (setf (mark-charpos ,mark) (progn ,@body))))))))

;;;; Lines.

(defun line-length (line)
  "Return the number of characters in $line.  Newlines characters are left
   off lines when the lines are created."
  (if (linep line)
      (line-length* line)
      (error "~S must be a line." line)))

(defun line-buffer (line)
  "Return the buffer which contains $line if there is one, else ()."
  (let ((buffer (line-%buffer line)))
    (if (bufferp buffer) buffer)))

(defun line-string (line)
  "Return as a simple string the characters in $line.

   This is `setf'able to set the line-string to any string.  If the string
   contains a newline characters the setf form throws an error.

   It is an error to destructively modify the result of line-string or to
   destructively modify any string after the line-string of some line has
   been set to that string."
  (if (eq line open-line)
      (close-line))
  (line-chars line))

(defun %set-line-string (line string)
  (let ((buffer (line-%buffer line)))
    (modifying-buffer buffer
      (unless (simple-string-p string)
	(setq string (coerce string 'simple-string)))
      (when (eq line open-line) (setq open-line nil))
      (let ((length (length (the simple-string string))))
	(dolist (m (line-marks line))
	  (if (eq (mark-%kind m) :left-inserting)
	      (setf (mark-charpos m) length)
	      (setf (mark-charpos m) 0))))
      (setf (line-chars line) string))))

(defun line-character (line index)
  "Return the character at position $index within $line.

   Throw an error if index is greater than the length of $line or less than
   zero.  If index is equal to the length of the line, return a #\newline
   character."
  (if (eq line open-line)
      (if (< index left-open-pos)
	  (schar open-chars index)
	  (let ((index (+ index (- right-open-pos left-open-pos))))
	    (if (= index line-cache-length)
		#\newline
		(schar open-chars index))))
      (let ((chars (line-chars line)))
	(declare (simple-string chars))
	(if (= index (length chars))
	    #\newline
	    (schar chars index)))))

;;;; Marks.

#[ Mark Functions

{function:ed:markp}
{function:ed:mark-line}
{function:ed:mark-charpos}
{function:ed:mark-kind}
{function:ed:previous-character}
{function:ed:next-character}
]#

#[ Making Marks

{function:ed:mark}
{function:ed:copy-mark}
{function:ed:delete-mark}
{function:ed:with-mark}
{function:ed:mark-buffer}
]#

(defun mark (line charpos &optional (kind :temporary))
  "Return a mark that points to the $charpos'th character of $line.  $kind
   is the kind of mark to create, one of :temporary, :left-inserting, or
   :right-inserting."
  (let ((mark (internal-make-mark line charpos kind)))
    (if (not (eq kind :temporary))
	(push mark (line-marks line)))
    mark))

(defun mark-kind (mark)
  "Return the kind of $mark, :temporary, :left-inserting, or
   :right-inserting.  This may be set with Setf."
  (mark-%kind mark))

(defun %set-mark-kind (mark kind)
  (let ((line (mark-line mark)))
    (cond ((eq kind :temporary)
	   (setf (line-marks line) (delq mark (line-marks line)))
	   (setf (mark-%kind mark) kind))
	  ((or (eq kind :left-inserting) (eq kind :right-inserting))
	   (if (not (memq mark (line-marks line)))
	       (push mark (line-marks line)))
	   (setf (mark-%kind mark) kind))
	  (t
	   (error "~S is an invalid mark type." kind)))))

(defun mark-buffer (mark)
  "Return the buffer which contains $mark if there is one, else ()."
  (line-buffer (mark-line mark)))

(defun copy-mark (mark &optional (kind (mark-kind mark)))
  "Return a new mark of $kind pointing to the same position as $mark.  Any
   permanent mark should be deleted after use."
  (let ((mark (internal-make-mark (mark-line mark) (mark-charpos mark) kind)))
    (if (not (eq kind :temporary))
	(push mark (line-marks (mark-line mark))))
    mark))

(defun delete-mark (mark)
  "Deletes $mark.  Any permanent mark should be deleted after use."
  (if (not (eq (mark-%kind mark) :temporary))
      (let ((line (mark-line mark)))
	(when line
	  (setf (line-marks line) (delq mark (line-marks line))))
	nil))
  (setf (mark-line mark) nil))

(defun move-to-position (mark charpos &optional (line (mark-line mark)))
  "Change $mark to point to character position $charpos on $line."
  (change-line mark line)
  (setf (mark-charpos mark) charpos)
  mark)

;; FIX reconsider marks as streams

(defun mark-stream-read-char (mark eof-error eof-value)
  "Return the character at $mark.  If $mark is at the end of file, return
   $eof-value or signal an end-of-file error if $eof-error is true."
  (or (next-character mark)
      (if eof-error
	  (error 'end-of-file :stream mark) ;; FIX good error to use?
	  eof-value)))

(defun mark-stream-handle-misc (mark operation &optional arg1 arg2)
  "Handle stream $operation for $mark."
  (declare (ignore arg1 arg2))
  (case operation
    (:listen
     (or (next-character mark) :eof))
    (:unread
     (mark-before mark))
#|
    (:close)
    (:clear-input)
|#
    (:force-output
     (redisplay-windows-from-mark mark))
    (:finish-output
     (redisplay-windows-from-mark mark))
    (:element-type 'base-char)
    ;(:interactive-p)
    (:line-length
     (let* ((buffer (mark-buffer mark)))
       (when buffer
	 (do ((w (buffer-windows buffer) (cdr w))
	      (min most-positive-fixnum (min (window-width (car w)) min)))
	     ((null w)
	      (if (/= min most-positive-fixnum) min))))))
    (:charpos
     (mark-column mark))
    (:file-length
     (count-characters (buffer-region (mark-buffer mark))))
    (:file-position
     (count-characters (region (buffer-start-mark (mark-buffer mark))
			       mark)))))


;;;; Regions.

(defun region (start end)
  "Return a region constructed from the marks $start and $end.  Throw an
   error if the marks are in separate buffers or if $start comes after
   $end."
  (let ((l1 (mark-line start))
	(l2 (mark-line end)))
    (or (eq (line-%buffer l1) (line-%buffer l2))
	(error "Marks must be in the same buffer."))
    (or (if (eq l1 l2)
	    (<= (mark-charpos start) (mark-charpos end))
	    (< (line-number l1) (line-number l2)))
	(error "Start ~S is after end ~S." start end)))
  (internal-make-region start end))

;;; The *Disembodied-Buffer-Counter* exists to give lines in regions that are not
;;; in any buffer unique buffer slots.

(defvar *Disembodied-Buffer-Counter* 0
  "The \"Buffer\" given to lines in regions that are not in any buffer.")

(defun make-empty-region ()
  "Return a region with start and end marks pointing to the start of one
   empty line.  The start mark is right-inserting and the end mark is
   left-inserting."
  (let* ((line (make-line :chars ""  :number 0
			  :%buffer (incf *disembodied-buffer-counter*)))
	 (start (mark line 0 :right-inserting))
	 (end (mark line 0 :left-inserting)))
    (internal-make-region start end)))

;;; Line-Increment is the default difference for line numbers when we don't
;;; know any better.

(defconstant line-increment 256 "Default difference for line numbers.")

;;; Renumber-Region is used internally to keep line numbers in ascending order.
;;; The lines in the region are numbered starting with the given Start value
;;; by increments of the given Step value.  It returns the region.

(defun renumber-region (region &optional (start 0) (step line-increment))
  (do ((line (mark-line (region-start region)) (line-next line))
       (last-line (mark-line (region-end region)))
       (number start (+ number step)))
      ((eq line last-line)
       (setf (line-number line) number)
       region)
    (setf (line-number line) number))
  region)

;;; Renumber-Region-Containing renumbers the region containing the given line.

(defun renumber-region-containing (line)
  (cond ((line-buffer line)
	 (renumber-region (buffer-region (line-%buffer line))))
	(t
	 (do ((line line (line-previous line))
	      (number 0 (- number line-increment)))
	     ((null line))
	   (setf (line-number line) number))
	 (do ((line (line-next line) (line-next line))
	      (number line-increment (+ number line-increment)))
	     ((null line))
	   (setf (line-number line) number)))))


;;; Number-Line numbers a newly created line.  The line has to have a previous
;;; line.
(defun number-line (line)
  (let ((prev (line-number (line-previous line)))
	(next (line-next line)))
    (if (null next)
	(setf (line-number line) (+ prev line-increment))
	(let ((new (+ prev (truncate (- (line-number next) prev) 2))))
	  (if (= new prev)
	      (renumber-region-containing line)
	      (setf (line-number line) new))))))


;;;; Buffers.

;;; BUFFER-SIGNATURE is the exported interface to the internal function,
;;; BUFFER-MODIFIED-TICK
;;;
(defun buffer-signature (buffer)
  "Return an arbitrary number which reflects the buffers current
   \"signature\".  Return a value that is guaranteed to be `eql' to the
   value returned by a previous call if the buffer has remained the same
   between the calls."
  (or (bufferp buffer) (error "~S is not a buffer." buffer))
  (buffer-modified-tick buffer))


;;;; Predicates.

#[ Text Predicates

Some of these functions use character attributes, which are discussed in
[Character Attributes].

{function:ed:start-line-p}
{function:ed:end-line-p}
{function:ed:empty-line-p}
{function:ed:blank-line-p}
{function:ed:blank-before-p}
{function:ed:blank-after-p}
{function:ed:same-line-p}

{function:ed:lines-related}
{function:ed:first-line-p}
{function:ed:last-line-p}

== Mark Ordering Predicates ==

{function:ed:mark<}
{function:ed:mark<=}
{function:ed:mark=}
{function:ed:mark>=}
{function:ed:mark>}

== Line Ordering Predicates ==

{function:ed:line<}
{function:ed:line<=}
{function:ed:line>=}
{function:ed:line>}
]#

(defun start-line-p (mark)
  "Return t if $mark points before the first character in a line, else ()."
  (= (mark-charpos mark) 0))

(defun end-line-p (mark)
  "Return t if $mark points after the last character in a line, else ()."
  (= (mark-charpos mark) (line-length (mark-line mark))))

(defun empty-line-p (mark)
  "Return t if the line pointed to by $mark is empty, else ()."
  (let ((line (mark-line mark)))
    (if (eq line open-line)
	(and (= left-open-pos 0) (= right-open-pos line-cache-length))
	(= (length (line-chars line)) 0))))

;;; blank-between-positions  --  Internal
;;;
(eval-when (compile eval)
(defmacro check-range (chars start end)
  `(do ((i ,start (1+ i)))
       ((= i ,end) t)
     (when (zerop (character-attribute :whitespace (schar ,chars i)))
       (return nil)))))
;;;
(defun blank-between-positions (line start end)
  "Check if $line is blank between positions $start and $end.  Used by
   `blank-XXX-p'."
  (if (eq line open-line)
      (let ((gap (- right-open-pos left-open-pos)))
	(cond ((>= start left-open-pos)
	       (check-range open-chars (+ start gap) (+ end gap)))
	      ((<= end left-open-pos)
	       (check-range open-chars start end))
	      (t
	       (and (check-range open-chars start left-open-pos)
		    (check-range open-chars right-open-pos (+ end gap))))))
      (let ((chars (line-chars line)))
	(check-range chars start end))))

(defun blank-line-p (line)
  "Return true if $line contains only characters with a :whitespace
   attribute of 1."
  (blank-between-positions line 0 (line-length line)))

(defun blank-before-p (mark)
  "Return true if all the characters before $mark and on the same line have
   a :whitespace attribute of 1."
  (blank-between-positions (mark-line mark) 0 (mark-charpos mark)))

(defun blank-after-p (mark)
  "Return true if all the characters after $mark and on the same line have
   a :whitespace attribute of 1."
  (let ((line (mark-line mark)))
    (blank-between-positions line (mark-charpos mark)
			     (line-length line))))

(defun same-line-p (mark1 mark2)
  "Return true if $mark1 and $mark2 point to the same line, else ().  That
   is,

         (same-line-p a b) <==> (eq (mark-line a) (mark-line b))"
  (eq (mark-line mark1) (mark-line mark2)))

(defun mark< (mark1 mark2)
  "Return t if $mark1 points to a character before $mark2, else ().  If the
   marks are in seperate pieces of text (e.g. in seperate buffers) then
   throw an error."
  (or (eq (line-%buffer (mark-line mark1))
	  (line-%buffer (mark-line mark2)))
      (error "Marks must be in a contiguous piece of text."))
  (or (< (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
	   (< (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark<= (mark1 mark2)
  "Return t if $mark1 points to a character at or before $mark2, else ().
   If the marks are in seperate pieces of text (e.g. in seperate buffers)
   then throw an error."
  (or (eq (line-%buffer (mark-line mark1))
	  (line-%buffer (mark-line mark2)))
      (error "Marks must be in a contiguous piece of text."))
  (or (< (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
	   (<= (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark> (mark1 mark2)
  "Return t if $mark1 points to a character before $mark2, else ().  If the
   marks are in seperate pieces of text (e.g. in seperate buffers) then
   throw an error."
  (or (eq (line-%buffer (mark-line mark1))
	  (line-%buffer (mark-line mark2)))
      (error "Marks must be in a contiguous piece of text."))
  (or (> (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
	   (> (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark>= (mark1 mark2)
  "Return t if $mark1 points to a character at or after $mark2, else ().
   If the marks are in seperate pieces of text (e.g. in seperate buffers)
   then throw an error."
  (or (eq (line-%buffer (mark-line mark1))
	       (line-%buffer (mark-line mark2)))
      (error "Marks must be in a contiguous piece of text."))
  (or (> (line-number (mark-line mark1)) (line-number (mark-line mark2)))
      (and (= (line-number (mark-line mark1)) (line-number (mark-line mark2)))
	   (>= (mark-charpos mark1) (mark-charpos mark2)))))

(defun mark= (mark1 mark2)
  "Return t if $mark1 and $mark2 point to the same position, else ()."
  (and (eq (mark-line mark1) (mark-line mark2))
       (= (mark-charpos mark1) (mark-charpos mark2))))

(defun line< (line1 line2)
  "Return t if $line1 comes before $line2, else ().  Throw an error if the
   lines are in seperate pieces of text (e.g. in seperate buffers)."
  (or (eq (line-%buffer line1) (line-%buffer line2))
      (error "Lines must be in a contiguous piece of text."))
  (< (line-number line1) (line-number line2)))

(defun line<= (line1 line2)
  "Return t if $line1 comes before or is the same as $line2, else ().
   Throw an error if the lines are in seperate pieces of text (e.g. in
   seperate buffers)."
  (or (eq (line-%buffer line1) (line-%buffer line2))
      (error "Lines must be in a contiguous piece of text."))
  (<= (line-number line1) (line-number line2)))

(defun line>= (line1 line2)
  "Return t if $line1 comes after or is the same as $line2, else ().  Throw
   an error if the lines are in seperate pieces of text (e.g. in seperate
   buffers)."
  (or (eq (line-%buffer line1) (line-%buffer line2))
      (error "Lines must be in a contiguous piece of text."))
  (>= (line-number line1) (line-number line2)))

(defun line> (line1 line2)
  "Return t if $line1 comes after $line2, else ().  Throw an error if the
   lines are in seperate pieces of text (e.g. in seperate buffers)."
  (or (eq (line-%buffer line1) (line-%buffer line2))
      (error "Lines must be in a contiguous piece of text."))
  (> (line-number line1) (line-number line2)))

(defun lines-related (line1 line2)
  "Return true if $line1 and $line2 are in the same piece of text, else
   ()."
  (eq (line-%buffer line1) (line-%buffer line2)))

(defun first-line-p (mark)
  "Return t if the line pointed to by $mark is the first line in the text,
   else ()."
  (null (line-previous (mark-line mark))))

(defun last-line-p (mark)
  "Return t if the line pointed to by $mark is the last line in the text,
   else ()."
  (null (line-next (mark-line mark))))
