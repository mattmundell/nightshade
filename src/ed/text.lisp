;;; Operations on units of texts, such as paragraphs, sentences, lines, and
;;; words.

(in-package "ED")

(export '(mark-paragraph paragraph-offset sentence-offset))

#[ English Text Buffers

This section describes some routines that understand basic English language
forms.

{function:ed:word-offset}
{function:ed:sentence-offset}
{function:ed:paragraph-offset}
{evariable:Paragraph Delimiter Function}
{function:ed:mark-paragraph}
]#

;;;; New Variables

(defevar "Paragraph Delimiter Function"
  "A function that takes a mark as its argument and returns true when the
   line it points to should break the paragraph."
  :value 'default-para-delim-function)

;;; The standard paragraph delimiting function is DEFAULT-PARA-DELIM-FUNCTION
(defun default-para-delim-function (mark)
  "Return whether or not to break on this line."
  (paragraph-delimiter-attribute-p (next-character mark)))


;;;; Paragraph Commands

#[ Paragraph Commands

A paragraph may be delimited by a blank line or a line beginning with "'"
or ".", in which case the delimiting line is not part of the paragraph.
Other characters may be paragraph delimiters in some modes.  A line with at
least one leading whitespace character may also introduce a paragraph and
is considered to be part of the paragraph.  Any fill-prefix which is
present on a line is skipped over for the purpose of locating a paragraph
boundary.

{command:Forward Paragraph}
{command:Backward Paragraph}
{command:Mark Paragraph}
{evariable:Paragraph Delimiter Function}
]#

(defcommand "Forward Paragraph" (p)
  "Move point to the end of the current or next paragraph.  With a prefix
   move that many paragraphs."
  (let ((point (current-point)))
    (unless (paragraph-offset point (or p 1))
      (buffer-end point)
      (editor-error "Too few paragraphs."))))

(defcommand "Backward Paragraph" (p)
  "Move point to the start of the current or previous paragraph.  With a
   prefix move that many paragraphs."
  (let ((point (current-point)))
    (unless (paragraph-offset point (- (or p 1)))
      (buffer-start point)
      (editor-error "Too few paragraphs."))))

(defcommand "Mark Paragraph" ()
  "Put mark and point around the current or next paragraph."
  (let* ((point (current-point))
	 (mark (copy-mark point :temporary)))
    (if (mark-paragraph point mark)
	(push-buffer-mark mark t)
	(editor-error "Point must be on or before a paragraph."))))

(defun mark-paragraph (mark1 mark2)
  "Mark the next or current paragraph, setting $mark1 to the beginning and
   $mark2 to the end.  Use *Fill Prefix* (as in [Filling]).  $mark1 is
   always on the first line of the paragraph, even if the previous line is
   blank.  $mark2 is typically at the beginning of the line after the line
   the paragraph ends on, this returns $mark2 on success.  If no paragraph
   is found, then the marks remain the same, and () is returned."
  (with-mark ((tmark1 mark1)
	      (tmark2 mark2))
    (let* ((prefix (value fill-prefix))
	   (prefix-len (length prefix))
	   (paragraphp (paragraph-offset tmark2 1)))
      (when (or paragraphp
		(and (last-line-p tmark2)
		     (end-line-p tmark2)
		     (not (blank-line-p (mark-line tmark2)))))
	(mark-before (move-mark tmark1 tmark2))
	(%fill-paragraph-start tmark1 prefix prefix-len)
	(move-mark mark1 tmark1)
	(move-mark mark2 tmark2)))))

(eval-when (compile eval)

;;; %MARK-TO-PARAGRAPH moves mark to next immediate (current) paragraph in
;;; the specified direction.  Nil is returned when no paragraph is found.
;;; NOTE: the order of the arguments to OR within the first branch of the
;;; COND must be as it is, and mark must be at the beginning of the line it
;;; is on.
(defmacro %mark-to-paragraph (mark prefix prefix-length
				   &optional (direction :forward))
  `(do ((skip-prefix-p)
	(paragraph-delim-function (value paragraph-delimiter-function)))
       (nil)
     (setf skip-prefix-p
	   (and ,prefix (%line-has-prefix-p ,mark ,prefix ,prefix-length)))
     (if skip-prefix-p (character-offset ,mark ,prefix-length))
     (let ((next-char (next-character ,mark)))
       (cond ((and (not (blank-after-p ,mark))
		   (or (whitespace-attribute-p next-char)
		       (not (funcall paragraph-delim-function ,mark))))
	      (return (if skip-prefix-p (line-start ,mark) ,mark)))
	     (,(if (eq direction :forward)
		   `(last-line-p ,mark)
		   `(first-line-p ,mark))
	      (if skip-prefix-p (line-start ,mark))
	      (return nil)))
       (line-offset ,mark ,(if (eq direction :forward) 1 -1) 0))))

;;; %PARAGRAPH-OFFSET-AUX is the inner loop of PARAGRAPH-OFFSET.  It moves
;;; over a paragraph to find the beginning or end depending on direction.
;;; Prefixes on a line are ignored or skipped over before it is determined
;;; if the line is a paragraph boundary.
(defmacro %paragraph-offset-aux (mark prefix prefix-length
				       &optional (direction :forward))
  `(do ((paragraph-delim-function (value paragraph-delimiter-function))
	(skip-prefix-p))
       (nil)
     (setf skip-prefix-p
	   (and ,prefix (%line-has-prefix-p ,mark ,prefix ,prefix-length)))
     (if skip-prefix-p (character-offset ,mark ,prefix-length))
     (cond ((or (blank-after-p ,mark)
		(funcall paragraph-delim-function ,mark))
	    (return (line-start ,mark)))
	   (,(if (eq direction :forward)
		 `(last-line-p ,mark)
		 `(first-line-p ,mark))
	    (return ,(if (eq direction :forward)
			 `(line-end ,mark)
			 `(line-start ,mark)))))
     (line-offset ,mark ,(if (eq direction :forward) 1 -1) 0)))

); (eval-when (compile eval)


;;; PARAGRAPH-OFFSET takes a mark and a number of paragraphs to move over.
;;; If the specified number of paragraphs does not exist in the direction
;;; indicated by the sign of number, then nil is returned, otherwise the
;;; mark is returned.
;;;
(defun paragraph-offset (mark number &optional (prefix (value fill-prefix)))
  "Move MARK COUNT paragraphs forward (if positive) or backwards (if
   negative).  If MARK is in the middle of a paragraph, that counts as one.
   If there were COUNT (-COUNT if negative) paragraphs in the appropriate
   direction, this returns MARK, otherwise nil.  This only moves MARK if
   there were enough paragraphs.

   `Paragraph Delimiter Function' holds a function that takes a MARK,
   typically at the beginning of a line, and returns whether or not the
   current line should break the paragraph.  default-para-delim-function
   returns true if the next character, the first on the line, has a
   `Paragraph Delimiter' attribute value of 1.  This is typically a space,
   for an indented paragraph, or a newline, for a block style.  Some modes
   require a more complicated determinant; for example, `Scribe' modes adds
   some characters to the set and special cases certain formatting
   commands.

   Prefix defaults to `Fill Prefix' (see section reffilling), and the right
   prefix is necessary to correctly skip paragraphs.  If prefix is true,
   and a line begins with prefix, then the scanning process skips the
   prefix before invoking the `Paragraph Delimiter Function'.  Note, when
   scanning for paragraph bounds, and prefix is true, lines are potentially
   part of the paragraph regardless of whether they contain the prefix;
   only the result of invoking the delimiter function matters.

   The programmer should be aware of an idiom for finding the end of the
   current paragraph.  Assume paragraphp is the result of moving MARK one
   paragraph, then the following correctly determines whether there
   actually is a current paragraph:

       (or paragraphp
	   (and (last-line-p mark)
		(end-line-p mark)
		(not (blank-line-p (mark-line mark)))))

   in this example mark is at the end of the last paragraph in the buffer,
   and there is no last newline character in the buffer.  paragraph-offset
   would have returned nil since it could not skip any paragraphs since
   mark was at the end of the current and last paragraph.  However, you
   still have found a current paragraph on which to operate.
   mark-paragraph understands this problem."
  (if (plusp number)
      (%paragraph-offset-forward mark number prefix)
      (%paragraph-offset-backward mark number prefix)))

;;; %PARAGRAPH-OFFSET-FORWARD moves mark forward over number paragraphs.
;;; The first branch of the COND is necessary for the side effect provided
;;; by LINE-OFFSET.  If %MARK-TO-PARAGRAPH left tmark at the beginning of
;;; some paragraph %PARAGRAPH-OFFSET-AUX will think it has moved mark past
;;; a paragraph, so we make sure tmark is inside the paragraph or after it.
;;;
(defun %paragraph-offset-forward (mark number prefix)
  (do* ((n number (1- n))
	(tmark (line-start (copy-mark mark :temporary)))
	(prefix-length (length prefix))
	(paragraphp (%mark-to-paragraph tmark prefix prefix-length)
		    (if (plusp n)
			(%mark-to-paragraph tmark prefix prefix-length))))
       ((zerop n) (move-mark mark tmark))
    (cond ((and paragraphp (not (line-offset tmark 1))) ;
	   (if (or (> n 1) (and (last-line-p mark) (end-line-p mark)))
	       (return nil))
	   (return (line-end (move-mark mark tmark))))
	  (paragraphp (%paragraph-offset-aux tmark prefix prefix-length))
	  (t (return nil)))))

(defun %paragraph-offset-backward (mark number prefix)
  (with-mark ((tmark1 mark)
	      (tmark2 mark))
    (do* ((n (abs number) (1- n))
	  (prefix-length (length prefix))
	  (paragraphp (%para-offset-back-find-para tmark1 prefix
						   prefix-length mark)
		      (if (plusp n)
			  (%para-offset-back-find-para tmark1 prefix
						       prefix-length tmark2))))
	 ((zerop n) (move-mark mark tmark1))
      (cond ((and paragraphp (first-line-p tmark1))
	     (if (and (first-line-p mark) (start-line-p mark))
		 (return nil)
		 (if (> n 1) (return nil))))
	    (paragraphp
	     (%paragraph-offset-aux tmark1 prefix prefix-length :backward)
	     (%para-offset-back-place-mark tmark1 prefix prefix-length))
	    (t (return nil))))))


;;; %PARA-OFFSET-BACK-PLACE-MARK makes sure that mark is in the right place
;;; when it has been moved backward over a paragraph.  The "right place" is
;;; defined to be where EMACS leaves it for a given situation or where it
;;; is necessary to ensure the mark's skipping backward over another
;;; paragraph if PARAGRAPH-OFFSET was given an argument with a greater
;;; magnitude than one.  I believe these two constraints are equivalent;
;;; that is, neither changes what the other would dictate.
;;;
(defun %para-offset-back-place-mark (mark prefix prefix-length)
  (skip-prefix-if-here mark prefix prefix-length)
  (cond ((text-blank-line-p mark) (line-start mark))
	((not (first-line-p mark))
	 (line-offset mark -1 0)
	 (skip-prefix-if-here mark prefix prefix-length)
	 (if (text-blank-line-p mark)
	     (line-start mark)
	     (line-offset mark 1 0)))))

(defun %para-offset-back-find-para (mark1 prefix prefix-length mark2)
  (move-mark mark2 mark1)
  (line-start mark1)
  (let ((para-p (%mark-to-paragraph mark1 prefix prefix-length :backward)))
    (cond ((and para-p (same-line-p mark1 mark2))
	   (skip-prefix-if-here mark1 prefix prefix-length)
	   (find-attribute mark1 :whitespace #'zerop)
	   (cond ((mark<= mark2 mark1)
		  (line-offset mark1 -1 0)
		  (%mark-to-paragraph mark1 prefix prefix-length :backward))
		 (t (line-start mark1))))
	  (t para-p))))


#[ Sentence Commands

A sentence is defined as a sequence of characters ending with a period,
question mark or exclamation point, followed by either two spaces or a newline.
A sentence may also be terminated by the end of a paragraph.  Any number of
closing delimiters, such as brackets or quotes, may be between the punctuation
and the whitespace.  This somewhat complex definition of a sentence is used so
that periods in abbreviations are not misinterpreted as sentence ends.

{command:Forward Sentence}
{command:Backward Sentence}
{command:Forward Kill Sentence}
{command:Backward Kill Sentence}
{command:Mark Sentence}
]#


;;;; Sentence Commands

(defcommand "Forward Sentence" (p)
  "Move point forward past the next sentence end.  With a prefix move that
   many sentence ends.

   A sentence terminates with a .,?, or ! followed by any number of closing
   delimiters (such as \",',),],>,|) which are followed by either two
   spaces or a newline."
  (or (sentence-offset (current-point) (or p 1))
      (editor-error "Too few sentences.")))

(defcommand "Backward Sentence" (p)
  "Move point backward past the beginning of the next sentence.  With a
   prefix move that many sentences.

   A sentence terminates with a .,?, or ! followed by any number of closing
   delimiters (such as \",',),],>,|) which are followed by either two
   spaces or a newline."
   (or (sentence-offset (current-point) (- (or p 1)))
       (editor-error "Too few sentences.")))

(defcommand "Mark Sentence" ()
  "Put point and mark at the beginning and end of the current or next
   sentence.

   A sentence terminates with a .,?, or ! followed by any number of closing
   delimiters (such as \",',),],>,|) which are followed by either two
   spaces or a newline."
  (let* ((point (current-point))
	 (end (copy-mark point :temporary)))
    (or (sentence-offset end 1)
	(editor-error "Point must be on or before a sentence."))
    (move-mark point end)
    (sentence-offset point -1)
    (push-buffer-mark end t)))

(defcommand "Forward Kill Sentence" (p)
  "Kill text from the point through to the end of the current sentence.
   With a prefix kill that many sentences."
  (let ((point (current-point))
	(offset (or p 1)))
    (with-mark ((mark point))
      (if (sentence-offset mark offset)
	  (if (plusp offset)
	      (kill-region (region point mark) :kill-forward)
	      (kill-region (region mark point) :kill-backward))
	  (editor-error "Too few sentences.")))))

(defcommand "Backward Kill Sentence" (p)
  "Kill text from the point to the beginning of the current sentence.  With
   a prefix kill that many sentences."
  (forward-kill-sentence-command (- (or p 1))))


;;; SENTENCE-OFFSET-END-P returns true if mark is at the end of a sentence.
;;; If that the end of a sentence, it leaves mark at an appropriate
;;; position with respect to the sentence-terminator character, the
;;; beginning of the next sentence, and direction.  See the commands
;;; "Forward Sentence" and "Backward Sentence" for a definition of a
;;; sentence.
;;;
(eval-when (compile eval)
(defmacro sentence-offset-end-p (mark &optional (direction :forward))
  `(let ((start (mark-charpos ,mark)))
     (do ()
	 ((not (sentence-closing-char-attribute-p (next-character ,mark))))
       (mark-after ,mark))
     (let ((next (next-character ,mark)))
       (cond ((or (not next)
		  (char= next #\newline))
	      ,(if (eq direction :forward) mark `(mark-after ,mark)))
	     ((and (char= next #\space)
		   (member (next-character (mark-after ,mark))
			   '(nil #\space #\newline)))
	      ,(if (eq direction :forward)
		   `(mark-before ,mark)
		   `(mark-after ,mark)))
	     (t (move-to-position ,mark start)
		nil)))))
); (eval-when (compile eval)


;;; SENTENCE-OFFSET-FIND-END moves in the direction direction stopping at
;;; sentence terminating characters until either there are not any more
;;; such characters or one is found that defines the end of a sentence.
;;; When looking backwards, we may be at the beginning of some sentence,
;;; and if we are, then we must move mark before the sentence terminator;
;;; otherwise, we would find the immediately preceding sentence terminator
;;; and end up right where we started.

(eval-when (compile eval)
(defmacro sentence-offset-find-end (mark &optional (direction :forward))
  `(progn
    ,@(if (eq direction :backward)
	  `((reverse-find-attribute ,mark :whitespace #'zerop)
	    (when (fill-region-insert-two-spaces-p ,mark)
	      (reverse-find-attribute ,mark :sentence-terminator)
	      (mark-before ,mark))))
    (do ((foundp) (endp)) (nil)
      (setf foundp ,(if (eq direction :forward)
			`(find-attribute ,mark :sentence-terminator)
			`(reverse-find-attribute ,mark :sentence-terminator)))
      (setf endp ,(if (eq direction :forward)
		      `(if foundp (progn (mark-after ,mark)
					 (sentence-offset-end-p ,mark)))
		      `(if foundp (sentence-offset-end-p ,mark :backward))))
      (if endp (return ,mark))
      ,(if (eq direction :forward)
	   `(unless foundp (return nil))
	   `(if foundp (mark-before ,mark) (return nil))))))
); (eval-when (compile eval)

;;; SENTENCE-OFFSET takes a mark and a number of paragraphs to move over.
;;; If the specified number of paragraphs does not exist in the direction
;;; indicated by the sign of the number, then nil is returned, otherwise
;;; the mark is returned.
;;;
(defun sentence-offset (mark count)
  "Move MARK COUNT sentences forward (if positive) or backwards (if
   negative).  If MARK is in the middle of a sentence, that counts as one.
   If there were COUNT (-COUNT if negative) sentences in the appropriate
   direction, this returns MARK, otherwise nil.  Always move MARK.

   A sentence ends with a character whose `Sentence Terminator' attribute
   is 1 followed by two spaces, a newline, or the end of the buffer.  The
   terminating character is optionally followed by any number of characters
   whose `Sentence Closing Char' attribute is 1.  A sentence begins after a
   previous sentence ends, at the beginning of a paragraph, or at the
   beginning of the buffer."
  (if (plusp count)
      (sentence-offset-forward mark count)
      (sentence-offset-backward mark (abs count))))



;;; SENTENCE-OFFSET-FORWARD tries to move mark forward over number
;;; sentences.  If it can, then mark is moved and returned; otherwise, mark
;;; remains unmoved, and nil is returned.  When tmark2 is moved to the end
;;; of a new paragraph, we reverse find for a non-whitespace character to
;;; bring tmark2 to the end of the previous line.  This is necessary to
;;; detect if tmark1 is at the end of the paragraph, in which case tmark2
;;; wants to be moved over another paragraph.
;;;
(defun sentence-offset-forward (mark number)
  (with-mark ((tmark1 mark)
	      (tmark2 mark))
    (do ((n number (1- n))
	 (found-paragraph-p))
	((zerop n) (move-mark mark tmark1))
      (when (and (mark<= tmark2 tmark1)
		 (setf found-paragraph-p (paragraph-offset tmark2 1)))
	(reverse-find-attribute tmark2 :whitespace #'zerop)
	(when (mark>= tmark1 tmark2)
	  (line-offset tmark2 1 0)
	  (setf found-paragraph-p (paragraph-offset tmark2 1))
	  (reverse-find-attribute tmark2 :whitespace #'zerop)))
      (cond ((sentence-offset-find-end tmark1)
	     (if (mark> tmark1 tmark2) (move-mark tmark1 tmark2)))
	    (found-paragraph-p (move-mark tmark1 tmark2))
	    (t (return nil))))))

(defun sentence-offset-backward (mark number)
  (with-mark ((tmark1 mark)
	      (tmark2 mark)
	      (tmark3 mark))
    (do* ((n number (1- n))
	  (prefix (value fill-prefix))
	  (prefix-length (length prefix))
	  (found-paragraph-p
	   (cond ((paragraph-offset tmark2 -1)
		  (sent-back-place-para-start tmark2 prefix prefix-length)
		  t))))
	 ((zerop n) (move-mark mark tmark1))
      (move-mark tmark3 tmark1)
      (when (and (sent-back-para-start-p tmark1 tmark3 prefix prefix-length)
		 (setf found-paragraph-p
		       (paragraph-offset (move-mark tmark2 tmark3) -1)))
	(paragraph-offset (move-mark tmark1 tmark2) 1)
	(sent-back-place-para-start tmark2 prefix prefix-length))
      (cond ((sentence-offset-find-end tmark1 :backward)
	     (if (mark< tmark1 tmark2) (move-mark tmark1 tmark2)))
	    (found-paragraph-p (move-mark tmark1 tmark2))
	    (t (return nil))))))

(defun sent-back-para-start-p (mark1 mark2 prefix prefix-length)
  (skip-prefix-if-here (line-start mark2) prefix prefix-length)
  (cond ((text-blank-line-p mark2)
	 (line-start mark2))
	((whitespace-attribute-p (next-character mark2))
	 (find-attribute mark2 :whitespace #'zerop)
	 (if (mark= mark1 mark2) (line-offset mark2 -1 0)))
	((and (mark= mark2 mark1) (line-offset mark2 -1 0))
	 (skip-prefix-if-here mark2 prefix prefix-length)
	 (if (text-blank-line-p mark2)
	     (line-start mark2)))))

(defun sent-back-place-para-start (mark2 prefix prefix-length)
  (skip-prefix-if-here mark2 prefix prefix-length)
  (when (text-blank-line-p mark2)
    (line-offset mark2 1 0)
    (skip-prefix-if-here mark2 prefix prefix-length))
  (find-attribute mark2 :whitespace #'zerop))


#[ Transposition Commands

the editor provides a
number of transposition commands.  A transposition command swaps the
"things" before and after the point and moves forward one "thing".  Just
how a "thing" is defined depends on the particular transposition command.
Transposition commands, particularly
`Transpose Characters' and `Transpose Words', are useful for
correcting typos.  More obscure transposition commands can be used to amaze
your friends and demonstrate your immense knowledge of exotic Emacs
commands.

To the uninitiated, the behavior of transposition commands may seem mysterious;
this has led some implementors to attempt to improve the definition of
transposition, but right-thinking people will accept no substitutes.  The
Emacs transposition definition used in the editor has two useful properties:

  1) Repeated applications of a transposition command have a useful effect.  The
     way to visualize this effect is that each use of the transposition command
     drags the previous thing over the next thing.  It is possible to correct
     double transpositions easily using `Transpose Characters'.

  2) Transposition commands move backward with a negative prefix argument, thus
     undoing the effect of the equivalent positive argument.

{command:Transpose Characters}
{command:Transpose Lines}
{command:Transpose Words}
{command:Transpose Regions}

To use `Transpose Regions', place three marks (in order) at the start and
end of the first region, and at the start of the second region, then place
the point at the end of the second region and call the command to swap the
two regions.
]#


;;;; Transposing.

(defcommand "Transpose Words" (p)
  "Transpose (swap) the words before and after point.

   With a positive argument transpose the words before and after the
   cursor, move a word right, and repeat the specified number of times,
   dragging the word to the left of the cursor right.

   With a negative argument, transpose the two words to the left of the
   cursor, move between them, and repeat the specified number of times,
   which would reverse a positive argument form of the same value."
  (let ((num (or p 1))
	(point (current-point)))
    (with-mark ((mark point :left-inserting)
		(start point :left-inserting))
      (let ((mark-prev (previous-character mark))
	    (mark-next (next-character mark)))
	(cond ((plusp num)
	       (let ((forwardp (word-offset point num))
		     (backwardp (if (or (word-delimiter-attribute-p mark-next)
					(word-delimiter-attribute-p mark-prev))
				    (word-offset mark -1)
				    (word-offset mark -2))))
		 (or forwardp
		     (editor-error "Too few words after point."))
		 (or backwardp
		     (editor-error "Too few words before point."))
		 (transpose-words-forward mark point start)))
	      ((minusp num)
	       (let ((enoughp (word-offset point (1- num))))
		 (if (word-delimiter-attribute-p mark-prev)
		     (reverse-find-attribute mark :word-delimiter #'zerop)
		     (find-attribute mark :word-delimiter))
		 (if enoughp
		     (transpose-words-backward point mark start)
		     (editor-error "Too few words before point."))))
	      (t (editor-error "Zero passed to `Transpose Words'.")))))))

(defun transpose-words-forward (mark1 end mark2)
  (with-mark ((tmark1 mark1 :left-inserting)
	      (tmark2 mark2 :left-inserting))
    (find-attribute tmark1 :word-delimiter)
    (do ((region1 (delete-and-save-region (region mark1 tmark1))))
	((mark= tmark2 end) (ninsert-region end region1))
      (word-offset tmark2 1)
      (reverse-find-attribute (move-mark tmark1 tmark2) :word-delimiter)
      (ninsert-region mark1 (delete-and-save-region (region tmark1 tmark2)))
      (move-mark mark1 tmark1))))

(defun transpose-words-backward (start mark1 mark2)
  (with-mark ((tmark1 mark1 :left-inserting)
	      (tmark2 mark2 :left-inserting))
    (reverse-find-attribute tmark1 :word-delimiter)
    (move-mark mark2 mark1)
    (do ((region1 (delete-and-save-region (region tmark1 mark1))))
	((mark= tmark1 start) (ninsert-region start region1))
      (word-offset tmark1 -1)
      (find-attribute (move-mark tmark2 tmark1) :word-delimiter)
      (ninsert-region mark1 (delete-and-save-region (region tmark1 tmark2)))
      (move-mark mark1 tmark1))))


(defcommand "Transpose Lines" (p)
  "Transpose (swap) the current line with the line before the cursor.

   With a positive argument transpose the current line with the one before,
   move down a line, and repeat the specified number of times, dragging the
   originally current line down.

   With a negative argument, transposes the two lines prior to the current,
   moves up a line, and repeats the specified number of times, exactly the
   reverse of the positive argument form.

   With a zero argument, transpose the lines at point and mark."
  (let ((num (or p 1))
	(point (current-point)))
    (flet ((check-end (point mark)
	     "Check for newline on line of point."
	     (let ((end (copy-mark (buffer-end-mark (current-buffer)))))
	       (unwind-protect
		   (progn
		     (setf (mark-kind point) :right-inserting)
		     (setf (mark-kind mark) :right-inserting)
		     (or (start-line-p end)
			 (and (equal (mark-line point)
				     (mark-line end))
			      (if (eq (value add-end-newline-on-writing-file)
				      :ask-user)
				  (prompt-for-y-or-n
				   :prompt
				   (list "~A~%Buffer missing final newline, add one? "
					 (buffer-name (current-buffer)))
				   :default t)
				  (value add-end-newline-on-writing-file))
			      (progn
				(insert-character end #\newline)
				(message "Added newline at end of buffer.")))))
		 (setf (mark-kind point) :left-inserting)
		 (setf (mark-kind mark) :left-inserting)
		 (delete-mark end)))))
      (with-mark ((mark point :left-inserting))
	(cond ((plusp num)
	       (check-end point mark)
	       (if (and (line-offset mark -1 0)
			(line-offset point num 0))
		   (transpose-lines mark point)
		   (editor-error "Too few lines around point.")))
	      ((minusp num)
	       (cond ((and (line-offset mark (1- num) 0)
			   (line-offset point -1 0))
		      (transpose-lines point mark)
		      (move-mark point mark))
		     (t (editor-error "Too few lines around point."))))
	      (t
	       (check-end point mark)
	       (rotatef (line-string (mark-line point))
			(line-string (mark-line (current-mark))))
	       (line-start point)))))))

(defun transpose-lines (mark1 mark2)
  (with-mark ((tmark1 mark1))
    (line-offset tmark1 1)
    (ninsert-region mark2 (delete-and-save-region (region mark1 tmark1)))))


;;;; Utilities

(defun skip-prefix-if-here (mark prefix prefix-length)
  (if (and prefix (%line-has-prefix-p mark prefix prefix-length))
      (character-offset mark prefix-length)))

(defun text-blank-line-p (mark)
  (let ((next-char (next-character mark)))
    (or (blank-after-p mark)
	(and (funcall (value paragraph-delimiter-function) mark)
	     (not (whitespace-attribute-p next-char))))))

(defun whitespace-attribute-p (char)
  (= (character-attribute :whitespace char) 1))

(defun sentence-terminator-attribute-p (char)
  (= (character-attribute :sentence-terminator char) 1))

(defun sentence-closing-char-attribute-p (char)
  (= (character-attribute :sentence-closing-char char) 1))

(defun paragraph-delimiter-attribute-p (char)
  (= (character-attribute :paragraph-delimiter char) 1))

(defun word-delimiter-attribute-p (char)
  (= (character-attribute :word-delimiter char) 1))
