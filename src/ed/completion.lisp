;;; Completion mode.

(in-package "ED")


#[ Completion

This is a minor mode that saves words greater than three characters in length,
allowing later completion of those words.  This is very useful for the often
long identifiers used in Lisp programs.  As you type a word, such as a Lisp
symbol when in `Lisp' mode, and you progress to typing the third letter,
the editor displays a possible completion in the status line.  You can then
rotate through the possible completions or type some more letters to narrow
down the possibilities.  If you choose a completion, you can also rotate
through the possibilities in the buffer instead of in the status line.
Choosing a completion or inserting a character that delimits words moves the
word forward in the ring of possible completions, so the next time you enter
its initial characters, the editor will prefer it over less recently used
completions.

{mode:Completion}
{command:Completion Self Insert}

`Completion Self Insert' is bound to most of the key-events with
corresponding graphic characters.

{command:Completion Complete Word}
{command:Completion Rotate Completions}
{command:List Possible Completions}
{evariable:Completion Bucket Size}
{command:Save Completions}
{command:Read Completions}
{evariable:Completion Database Filename}
{command:Parse Buffer for Completions}
]#


;;;; The Completion Database.

;;; The top level structure here is an array that gets indexed with the
;;; first three characters of the word to be completed.  That will get us to
;;; a list of the strings with that prefix sorted in most-recently-used order.
;;; The number of strings in any given bucket will never exceed
;;; Completion-Bucket-Size-Limit.  Strings are stored in the database in
;;; lowercase form always.

(defconstant completion-table-size 991)

(defvar *completions* (make-array completion-table-size :initial-element nil))

(defevar "Completion Bucket Size"
  "Completions are stored in buckets determined by the first three letters
   of a word. This variable limits the number of completions saved for each
   combination of the first three letters of a word.  If there are many
   identifier in some module beginning with the same first three letters,
   this variable needs to increase to accommodate all the names."
  :value 20)

;;; Mapping strings into buckets.

;;; The characters that are considered parts of "words" change from mode
;;; to mode.
;;;
(defattribute "Completion Wordchar"
  "1 for characters we consider to be constituents of words.")

(defconstant default-other-wordchars
  '(#\- #\* #\' #\_))

(do-alpha-chars (char :both)
  (setf (character-attribute :completion-wordchar char) 1))

(dolist (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (setf (character-attribute :completion-wordchar char) 1))

(dolist (char default-other-wordchars)
  (setf (character-attribute :completion-wordchar char) 1))


;;; The difference between Lisp mode and the other modes is pretty radical in
;;; this respect.  These are interesting too, but they're on by default: #\*,
;;; #\-, and #\_.  #\' is on by default too, but it's uninteresting in "Lisp"
;;; mode.
;;;
(defconstant default-lisp-wordchars
  '(#\~ #\! #\@ #\$ #\% #\^ #\& #\+ #\= #\: #\< #\> #\. #\/ #\?))

(dolist (char default-lisp-wordchars)
  (shadow-attribute :completion-wordchar char 1 "Lisp"))

(shadow-attribute :completion-wordchar #\' 0 "Lisp")

(defmacro completion-char-p (char)
  `(= (the fixnum (character-attribute :completion-wordchar ,char)) 1))

;;; COMPLETION-BUCKET-FOR returns the Completion-Bucket that might hold a
;;; completion for the given String.  With optional Value, sets the bucket.
;;;
(defun completion-bucket-for (string length &optional (value nil value-p))
  (declare (simple-string string)
	   (fixnum length))
  (when (and (>= length 3)
	     (completion-char-p (char string 0))
	     (completion-char-p (char string 1))
	     (completion-char-p (char string 2)))
    (let ((index (mod (logxor (ash
			       (logxor
				(ash (edi::search-hash-code (schar string 0))
				     5)
				(edi::search-hash-code (schar string 1)))
			       3)
			      (edi::search-hash-code (schar string 2)))
		      completion-table-size)))
      (declare (fixnum index))
      (if value-p
	  (setf (svref *completions* index) value)
	  (svref *completions* index)))))

(defsetf completion-bucket-for completion-bucket-for)


;;; FIND-COMPLETION returns the most recent string matching the given
;;; Prefix, or Nil if nothing appropriate is in the database.  We assume
;;; the Prefix is passed to us in lowercase form so we can use String=.  If
;;; we find something appropriate, we bring it to the front of the list.
;;; Prefix-Length, if supplied restricts us to look at just the start of
;;; the string...
;;;
(defun find-completion (prefix &optional (prefix-length (length prefix)))
  (declare (simple-string prefix)
	   (fixnum prefix-length))
  (let ((bucket (completion-bucket-for prefix prefix-length)))
    (do ((list bucket (cdr list)))
	((null list))
      (let ((completion (car list)))
	(declare (simple-string completion))
	(when (and (>= (length completion) prefix-length)
		   (string= prefix completion
			    :end1 prefix-length
			    :end2 prefix-length))
	  (unless (eq list bucket)
	    (rotatef (car list) (car bucket)))
	  (return completion))))))

;;; RECORD-COMPLETION saves string in the completion database as the first item
;;; in the bucket, that's the most recently used completion.  If the bucket is
;;; full, drop the oldest item in the list.  If string is already in the
;;; bucket, simply move it to the front.  The way we move an element to the
;;; front requires a full bucket to be at least three elements long.
;;;
(defun record-completion (string)
  (declare (simple-string string))
  (let ((string-length (length string)))
    (declare (fixnum string-length))
    (when (> string-length 3)
      (let ((bucket (completion-bucket-for string string-length))
	    (limit (value completion-bucket-size)))
	(do ((list bucket (cdr list))
	     (last nil list)
	     (length 1 (1+ length)))
	    ((null list)
	     (setf (completion-bucket-for string string-length)
		   (cons string bucket)))
	  (cond ((= length limit)
		 (setf (car list) string)
		 (setf (completion-bucket-for string string-length) list)
		 (setf (cdr list) bucket)
		 (setf (cdr last) nil)
		 (return))
		((string= string (the simple-string (car list)))
		 (unless (eq list bucket)
		   (rotatef (car list) (car bucket)))
		 (return))))))))

;;; ROTATE-COMPLETIONS rotates the completion bucket for the given Prefix.
;;; We just search for the first thing in the bucket with the Prefix, then
;;; move that to the end of the list.  If there ain't no such thing there,
;;; or if it's already at the end, we do nothing.
;;;
(defun rotate-completions (prefix &optional (prefix-length (length prefix)))
  (declare (simple-string prefix))
  (let ((bucket (completion-bucket-for prefix prefix-length)))
    (do ((list bucket (cdr list))
	 (prev nil list))
	((null list))
      (let ((completion (car list)))
	(declare (simple-string completion))
	(when (and (>= (length completion) prefix-length)
		   (string= prefix completion
			    :end1 prefix-length :end2 prefix-length))
	  (when (cdr list)
	    (if prev
		(setf (cdr prev) (cdr list))
		(setf (completion-bucket-for prefix prefix-length) (cdr list)))
	    (setf (cdr (last list)) list)
	    (setf (cdr list) nil))
	  (return nil))))))


;;;; Editor interface.

(defmode "Completion" :transparent-p t :precedence 10.0
  :documentation
  "This is a minor mode that saves words greater than three characters in
   length, allowing later completion of those words.  This is very useful
   for often long identifiers used in Lisp code.  All words with the same
   first three letters are in one list sorted by most recently used.
   *Completion Bucket Size* limits the number of completions saved in each
   list.")

(defcommand "Completion Mode" ()
  "Toggles Completion Mode in the current buffer."
  (setf (buffer-minor-mode (current-buffer) "Completion")
	(not (buffer-minor-mode (current-buffer) "Completion"))))


;;; Consecutive alphanumeric keystrokes that start a word cause a possible
;;; completion to be displayed in the echo area's modeline, the status line.
;;; Since most insertion is building up a word that was already started, we
;;; keep track of the word in *completion-prefix* that the user is typing.  The
;;; length of the thing is kept in *completion-prefix-length*.
;;;
(defconstant completion-prefix-max-size 256)

(defvar *completion-prefix* (make-string completion-prefix-max-size))

(defvar *completion-prefix-length* 0)


;;; "Completion Self Insert" does different stuff depending on whether or
;;; not the thing to be inserted is Completion-Char-P.  If it is, then we
;;; try to come up with a possible completion, using Last-Command-Type to
;;; tense things up a bit.  Otherwise, if Last-Command-Type says we were
;;; just doing a word, then we record that word in the database.
;;;
(defcommand "Completion Self Insert" (p)
  "Insert the last character typed, showing possible completions.  With prefix
   argument insert the character that many times."
  (let ((char (ext:key-event-char *last-key-event-typed*)))
    (or char (editor-error "Can't insert that character."))
#| FIX somehow handle a word going over the limit
    (if (>= *completion-prefix-length* completion-prefix-max-size)
	(editor-error "Word too long for completion."))
|#
    (cond ((completion-char-p char)
	   ;; If start of word not already in *completion-prefix*, put it
	   ;; there.
	   (or (eq (last-command-type) :completion-self-insert)
	       (set-completion-prefix))
	   ;; Then add new stuff.
	   (cond ((and p (> p 1))
		  (fill *completion-prefix* (char-downcase char)
			:start *completion-prefix-length*
			:end (+ *completion-prefix-length* p))
		  (incf *completion-prefix-length* p))
		 (t
		  (setf (schar *completion-prefix* *completion-prefix-length*)
			(char-downcase char))
		  (incf *completion-prefix-length*)))
	   ;; Display possible completion, if any.
	   (display-possible-completion *completion-prefix*
					*completion-prefix-length*)
	   (setf (last-command-type) :completion-self-insert))
	  (t
	   (when (eq (last-command-type) :completion-self-insert)
	     (record-completion (subseq *completion-prefix*
					0 *completion-prefix-length*)))))))

;;; SET-COMPLETION-PREFIX grabs any completion-wordchars immediately before
;;; point and stores these into *completion-prefix*.
;;;
(defun set-completion-prefix ()
  (let* ((point (current-point))
	 (point-line (mark-line point)))
    (cond ((and (previous-character point)
		(completion-char-p (previous-character point)))
	   (with-mark ((mark point))
	     (reverse-find-attribute mark :completion-wordchar #'zerop)
	     (or (eq (mark-line mark) point-line)
		 (editor-error "Must have completion wordchars on this line."))
	     (let ((insert-string (nstring-downcase
				   (region-to-string
				    (region mark point)))))
	       (replace *completion-prefix* insert-string)
	       (setq *completion-prefix-length* (length insert-string)))))
	  (t
	   (setq *completion-prefix-length* 0)))))

;; FIX compare m-/
(defcommand "Completion Complete Word" ()
  "Selects the currently displayed completion if there is one, guessing the
   case of the inserted text as with `Query Replace'.  If invoked in
   immediate succession rotate through possible completions in the buffer.
   If there is no currently displayed completion, try to choose a
   completion from text immediately before the point and display the
   completion if found."
  "Complete the word if we've got a completion, fixing up the case."
  (let ((last-command-type (last-command-type)))
    ;; If the user has been cursoring around and then tries to complete,
    ;; let him.
    ;;
    (unless (member last-command-type '(:completion-self-insert :completion))
      (set-completion-prefix)
      (setf last-command-type :completion-self-insert))
    (case last-command-type
      (:completion-self-insert
       (do-completion))
      (:completion
       (rotate-completions *completion-prefix* *completion-prefix-length*)
       (do-completion))))
  (setf (last-command-type) :completion))

;; FIX consistent with kill ring browsing?
(defcommand "List Possible Completions" ()
  "List all possible completions of the text immediately before point in a
   pop-up display.  Sometimes this is more useful than rotating through
   several completions to see what is available."
  "List all possible completions of the text immediately before point in a
   pop-up display."
  (let ((last-command-type (last-command-type)))
    (unless (member last-command-type '(:completion-self-insert :completion))
      (set-completion-prefix))
    (let* ((prefix *completion-prefix*)
	   (prefix-length *completion-prefix-length*)
	   (bucket (completion-bucket-for prefix prefix-length)))
      (with-pop-up-display (s)
	(dolist (completion bucket)
	  (when (and (> (length completion) prefix-length)
		     (string= completion prefix
			      :end1 prefix-length
			      :end2 prefix-length))
	    (write-line completion s))))))
  ;; Keep the redisplay hook from clearing any possibly displayed completion.
  (setf (last-command-type) :completion-self-insert))

(defvar *last-completion-mark* nil)

(defun do-completion ()
  (let ((completion (find-completion *completion-prefix*
				     *completion-prefix-length*))
	(point (current-point)))
    (when completion
      (if *last-completion-mark*
	  (move-mark *last-completion-mark* point)
	  (setq *last-completion-mark* (copy-mark point :temporary)))
      (let ((mark *last-completion-mark*))
	(reverse-find-attribute mark :completion-wordchar #'zerop)
	(let* ((region (region mark point))
	       (string (region-to-string region)))
	  (declare (simple-string string))
	  (delete-region region)
	  (let* ((first (position-if #'alpha-char-p string))
		 (next (if first (position-if #'alpha-char-p string
					      :start (1+ first)))))
	    ;; Often completions start with asterisks when hacking on Lisp
	    ;; code, so we look for alphabetic characters.
	    (insert-string point
			   ;; Leave the cascading IF's alone.
			   ;; Writing this as a COND, using LOWER-CASE-P as
			   ;; the test is not equivalent to this code since
			   ;; numbers (and such) are nil for LOWER-CASE-P and
			   ;; UPPER-CASE-P.
			   (if (and first (upper-case-p (schar string first)))
			       (if (and next
					(upper-case-p (schar string next)))
				   (string-upcase completion)
				   (word-capitalize completion))
			       completion))))))))


;;; WORD-CAPITALIZE is like STRING-CAPITALIZE except that it treats apostrophes
;;; the Right Way.
;;;
(defun word-capitalize (string)
  (let* ((length (length string))
	 (strung (make-string length)))
    (do  ((i 0 (1+ i))
	  (new-word t))
	 ((= i length))
      (let ((char (schar string i)))
	(cond ((or (alphanumericp char)
		   (char= char #\'))
	       (setf (schar strung i)
		     (if new-word (char-upcase char) (char-downcase char)))
	       (setq new-word nil))
	      (t
	       (setf (schar strung i) char)
	       (setq new-word t)))))
    strung))

(defcommand "Completion Rotate Completions" (p)
  "Show the next possible completion in the status line, if there is one,
   otherwise try to choose a completion from text immediately before the
   point and display the completion if found.  With an argument, rotate the
   completion ring that many times."
  (unless (eq (last-command-type) :completion-self-insert)
    (set-completion-prefix)
    (setf (last-command-type) :completion-self-insert))
  (dotimes (i (or p 1))
    (rotate-completions *completion-prefix* *completion-prefix-length*))
  (display-possible-completion *completion-prefix* *completion-prefix-length*)
  (setf (last-command-type) :completion-self-insert))


;;;; Nifty database and parsing machanisms.

(defevar "Completion Database Filename"
  "The file that `Save Completions' and `Read Completions' respectively
   write and read the completion database to and from.")

(defvar *completion-default-default-database-filename*
  "ed-completions.txt"
  "The file that will be defaultly written to and read from by `Save
   Completions' and `Read Completions'.")

(defcommand "Save Completions" (p)
  "Write the current completion database to `Completion Database Filename'.
   With an argument, prompts for a filename.  Write completions such that
   `Read Completions' can read them back in preserving the
   most-recently-used order."
  (let ((filename (or (and (not p) (value completion-database-filename))
		      (prompt-for-file
		       :must-exist nil
		       :default *completion-default-default-database-filename*
		       :prompt "File to write completions to: "))))
    (with-open-file (s filename
		       :direction :output
		       :if-exists :rename-and-delete
		       :if-does-not-exist :create)
      (message "Saving completions...")
      (dotimes (i (length *completions*))
	(let ((bucket (svref *completions* i)))
	  (when bucket
	    (write i :stream s :base 10 :radix 10)
	    (write-char #\newline s)
	    (dolist (completion bucket)
	      (write-line completion s))
	    (terpri s))))
      (message "Done."))))

(defcommand "Read Completions" (p)
  "Read completions from *Completion Database File*.  With an argument,
   prompt for a filename.  Move any current completions to a
   less-recently-used status, and remove any in buckets that exceed the
   limit `Completion Bucket Size'."
  (let ((filename (or (and (not p) (value completion-database-filename))
		      (prompt-for-file
		       :must-exist nil
		       :default *completion-default-default-database-filename*
		       :prompt "File to read completions from: ")))
	(index nil)
	(completion nil))
    (with-open-file (s filename :if-does-not-exist :error)
      (message "Reading in completions...")
      (loop
	(let ((new-completions '()))
	  (unless (setf index (read-preserving-whitespace s nil nil))
	    (return))
	  ;; Zip past the newline that I know is directly after the number.
	  ;; All this to avoid consing.  I love it.
	  (read-char s)
	  (loop
	    (setf completion (read-line s))
	    (when (string= completion "") (return))
	    (unless (member completion (svref *completions* index))
	      (push completion new-completions)))
	  (let ((new-bucket (nconc (nreverse new-completions)
					    (svref *completions* index))))
	    (setf (svref *completions* index) new-bucket)
	    (do ((completion new-bucket (cdr completion))
		 (end (1- (value completion-bucket-size)))
		 (i 0 (1+ i)))
		((endp completion))
	      (when (= i end) (setf (cdr completion) nil))))))
      (message "Done."))))

(defcommand "Parse Buffer for Completions" ()
  "Pass over the current buffer putting each valid completion word into the
   database.  This is a good way of picking up many useful completions upon
   visiting a new file for which there are no saved completions."
  (let ((buffer (prompt-for-buffer :prompt "Buffer to parse: "
				   :must-exist t
				   :default (current-buffer)
				   :default-string (buffer-name
						    (current-buffer)))))
    (with-mark ((word-start (buffer-start-mark buffer) :right-inserting)
		(word-end (buffer-start-mark buffer) :left-inserting)
		(buffer-end-mark (buffer-start-mark buffer)))
      (message "Starting parse of ~S..." (buffer-name buffer))
      (loop
	(unless (find-attribute word-start :completion-wordchar) (return))
	(record-completion
	 (region-to-string (region word-start
				   (or (find-attribute
					(move-mark word-end word-start)
					:completion-wordchar #'zerop)
				       buffer-end-mark))))
	(move-mark word-start word-end))
      (message "Done."))))


;;;; Modeline hackery:

(defvar *completion-mode-possibility* "")

(defvar *completion-modeline-field* (modeline-field :completion))

(defun display-possible-completion (prefix
				    &optional (prefix-length (length prefix)))
  (let ((old *completion-mode-possibility*))
    (setq *completion-mode-possibility*
	  (or (find-completion prefix prefix-length) ""))
    (unless (eq old *completion-mode-possibility*)
      (update-modeline-field *echo-area-buffer* *echo-area-window*
			     *completion-modeline-field*))))

(defun clear-completion-display ()
  (unless (= (length (the simple-string *completion-mode-possibility*)) 0)
    (setq *completion-mode-possibility* "")
    (update-modeline-field *echo-area-buffer* *echo-area-window*
			   *completion-modeline-field*)))


;;; COMPLETION-REDISPLAY-FUN erases any completion displayed in the status line.
;;;
(defun completion-redisplay-fun (window)
  (declare (ignore window))
  (unless (eq (last-command-type) :completion-self-insert)
    (clear-completion-display)))
;;;
(add-hook redisplay-hook #'completion-redisplay-fun)
