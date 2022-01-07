;;; LISP Mode and commands.

(in-package "ED")

(export '(backward-up-list defindent forward-up-list form-offset
	  end-of-parse-block inside-defun-p mark-top-level-form
	  pre-command-parse-check start-defun-p start-of-parse-block
	  top-level-offset valid-spot))

#[ Editing Lisp

The editor provides a large number of commands for editing Lisp code.  It
is easier for a text editor to provide a higher level of support for
editing Lisp than ordinary programming languages, since its syntax is much
simpler.

[ Lisp Mode            ]
[ Form Manipulation    ]
[ List Manipulation    ]
[ Defun Manipulation   ]
[ Lisp Indentation     ]
[ Parenthesis Matching ]
[ Parsing Lisp         ]
]#

#[ Parsing Lisp

Lisp mode has a fairly complete knowledge of Lisp syntax, but since it does
not use the reader, and must work incrementally, it can be confused by legal
constructs.  Lisp mode totally ignores the read-table, so user-defined read
macros have no effect on the editor.  In some cases, the values the `Lisp
Syntax' character attribute can be changed to get a similar effect.

Lisp commands consistently treat semicolon (;) style comments as
whitespace when parsing, so a Lisp command used in a comment will affect the
next (or previous) form outside of the comment.  Since #| ... |# comments
are not recognized, they can used to comment out code, while still allowing
Lisp editing commands to be used.

Strings are parsed similarly to symbols.  When within a string, the next form
is after the end of the string, and the previous form is the beginning of the
string.

In order to save time, Lisp mode does not parse the entire buffer every
time a Lisp command is used.  Instead, it uses a heuristic to guess the
region of the buffer that is likely to be interesting.  The next three
variables control the heuristic.

Normally, parsing begins and ends on defun boundaries (an open parenthesis
at the beginning of a line).

{evariable:Defun Parse Goal}
{evariable:Maximum Lines Parsed}
{evariable:Minimum Lines Parsed}

When the heuristic fails, and does not parse enough of the buffer, then
commands usually act as though a syntax error was detected.  If the parse
starts in a bad place (such as in the middle of a string), then Lisp
commands will be totally confused.  Such problems can usually be eliminated
by increasing the values of some of these variables.

The functions in the following variables determine the region of the buffer
parsed.

{evariable:Parse Start Function}
{evariable:Parse End Function}
]#

(declaim (optimize (speed 2))) ; turn off byte compilation.


;;;; Variables and lisp-info structure.

;;; These routines are used to define, for standard LISP mode, the start and end
;;; of a block to parse.  If these need to be changed for a minor mode that sits
;;; on top of LISP mode, simply do a DEFEVAR with the minor mode and give the
;;; name of the function to use instead of START-OF-PARSE-BLOCK and
;;; END-OF-PARSE-BLOCK.
;;;
(defevar "Parse Start Function"
  "A function that takes a mark and moves it to the top of a block for
   paren parsing."
  :value 'start-of-parse-block)

(defevar "Parse End Function"
  "A function that takes a mark and moves it to the bottom of a block for
   paren parsing."
  :value 'end-of-parse-block)

;;; LISP-INFO is the structure used to store the data about the line in its
;;; Plist.
;;;
;;;     -> BEGINS-QUOTED, ENDING-QUOTED are both Boolean slots that tell whether
;;;        or not a line's begining and/or ending are quoted.
;;;
;;;     -> RANGES-TO-IGNORE is a list of cons cells, each having the form
;;;        ( [begining-charpos] [end-charpos] ) each of these cells indicating
;;;        a range to ignore.  End is exclusive.
;;;
;;;     -> NET-OPEN-PARENS, NET-CLOSE-PARENS integers that are the number of
;;;        unmatched opening and closing parens that there are on a line.
;;;
;;;     -> SIGNATURE-SLOT ...
;;;

(defstruct (lisp-info (:constructor make-lisp-info ()))
  (begins-quoted nil)		; (or t nil)
  (ending-quoted nil)		; (or t nil)
  (ranges-to-ignore nil)	; (or t nil)
  (net-open-parens 0 :type fixnum)
  (net-close-parens 0 :type fixnum)
  (signature-slot))


;;;; Macros.

;;; The following Macros exist to make it easy to access the Syntax primitives
;;; without uglifying the code.

(defmacro scan-char (mark attribute values)
  `(find-attribute ,mark ',attribute ,(attr-predicate values)))

(defmacro rev-scan-char (mark attribute values)
  `(reverse-find-attribute ,mark ',attribute ,(attr-predicate values)))

(defmacro test-char (char attribute values)
  `(let ((x (character-attribute ',attribute ,char)))
     ,(attr-predicate-aux values)))

(eval-when (compile load eval)
(defun attr-predicate (values)
  (cond ((eq values 't)
	 '#'plusp)
	((eq values 'nil)
	 '#'zerop)
	(t `#'(lambda (x) ,(attr-predicate-aux values)))))

(defun attr-predicate-aux (values)
  (cond ((eq values t)
	 '(plusp x))
	((eq values nil)
	 '(zerop x))
	((symbolp values)
	 `(eq x ',values))
	((and (listp values) (member (car values) '(and or not)))
	 (cons (car values) (mapcar #'attr-predicate-aux (cdr values))))
	(t (error "Illegal form in attribute pattern - ~S" values))))

); Eval-When (Compile Load Eval)

(defmacro find-lisp-char (mark)
  "Move MARK to next :LISP-SYNTAX character, if one isn't found, return NIL."
  `(find-attribute ,mark :lisp-syntax
		   #'(lambda (x)
		       (member x '(:open-paren :close-paren :newline :comment
				   :char-quote :string-quote)))))
(defmacro push-range (new-range info-struct)
  "Insert NEW-RANGE into the LISP-INFO-RANGES-TO-IGNORE slot of the INFO-STRUCT."
  `(when ,new-range
     (setf (lisp-info-ranges-to-ignore ,info-struct)
	   (cons ,new-range (lisp-info-ranges-to-ignore ,info-struct)))))

(defmacro scan-direction (mark forwardp &rest forms)
  "Expand to a form that scans either backward or forward according to Forwardp."
  (if forwardp
      `(scan-char ,mark ,@forms)
      `(rev-scan-char ,mark ,@forms)))

(defmacro direction-char (mark forwardp)
  "Expand to a form that returns either the previous or next character according
  to Forwardp."
  (if forwardp
      `(next-character ,mark)
      `(previous-character ,mark)))


(defmacro neighbor-mark (mark forwardp)
  "Expand to a form that moves MARK either backward or forward one character,
  depending on FORWARDP."
  (if forwardp
      `(mark-after ,mark)
      `(mark-before ,mark)))

(defmacro neighbor-line (line forwardp)
  "Expand to return the next or previous line, according to Forwardp."
  (if forwardp
      `(line-next ,line)
      `(line-previous ,line)))


#[ Lisp Text Buffers

The editor bases its Lisp primitives on parsing a block of the buffer and
annotating lines as to what kind of Lisp syntax occurs on the line or what kind
of form a mark might be in (for example, string, comment, list, etc.).  These
do not work well if the block of parsed forms is exceeded when moving marks
around these forms, but the block that gets parsed is somewhat programmable.

There is also a notion of a top level form which this documentation often
uses synonymously with defun, meaning a Lisp form occurring in a source
file delimited by parentheses with the opening parenthesis at the beginning of
some line.  The names of the functions include this inconsistency.

{function:ed:pre-command-parse-check}
{evariable:Parse Start Function}
{function:ed:start-of-parse-block}
{evariable:Parse End Function}
{function:ed:end-of-parse-block}
{evariable:Minimum Lines Parsed}
{evariable:Maximum Lines Parsed}
{evariable:Defun Parse Goal}

Every command that uses the following routines calls
`pre-command-parse-check' before doing so.

{function:ed:form-offset}
{function:ed:top-level-offset}
{function:ed:mark-top-level-form}
{function:ed:defun-region}

The Lisp Syntax refered to in the next few functions is a character
attribute, as described in [System Defined Character Attributes].

{function:ed:inside-defun-p}
{function:ed:start-defun-p}
{function:ed:forward-up-list}
{function:ed:backward-up-list}
{function:ed:valid-spot}
{function:ed:defindent}
]#


;;;; Parsing functions.

;;; Public.
;;;
(defun name-at-point (&optional (mark (current-point)))
  "If there is a variable name at Mark return the name, else return nil."
  (let ((ch (next-character mark)))
    (if (test-char ch :lisp-syntax :constituent)
	(let* ((mark-1 (scan-char (copy-mark mark :temporary)
				  :lisp-syntax (not :constituent)))
	       (mark-2 (copy-mark mark-1)))
	  (rev-scan-char (mark-before mark-2) :lisp-syntax (not :constituent))
	  (region-to-string (region mark-2 mark-1))))))

;;; PRE-COMMAND-PARSE-CHECK -- Public.
;;;
(defun pre-command-parse-check (mark &optional (for-sure-parse nil))
  "Call *Parse Start Function* and *Parse End Function* on $mark to get two
   marks.  Then parse all the lines between the marks including the
   complete lines they point into.  When $for-sure is true, parse the area
   regardless of any cached information about the lines."
  (with-mark ((top mark)
	      (bottom mark))
    (funcall (value parse-start-function) top)
    (funcall (value parse-end-function) bottom)
    (parse-over-block (mark-line top) (mark-line bottom) for-sure-parse)))

;;; PARSE-OVER-BLOCK
;;;
(defun parse-over-block (start-line end-line &optional (for-sure-parse nil))
  "Parse over an area indicated from END-LINE to START-LINE."
  (let ((test-line start-line)
	prev-line-info)

    (with-mark ((mark (mark test-line 0)))

      ; Set the pre-begining and post-ending lines to delimit the range
      ; of action any command will take.  This means set the lisp-info of the
      ; lines immediately before and after the block to Nil.

      (when (line-previous start-line)
	(setf (getf (line-plist (line-previous start-line)) 'lisp-info) nil))
      (when (line-next end-line)
	(setf (getf (line-plist (line-next end-line)) 'lisp-info) nil))

      (loop
       (let ((line-info (getf (line-plist test-line) 'lisp-info)))

	 ;;    Reparse the line when any of the following are true:
	 ;;
	 ;;      FOR-SURE-PARSE is T
	 ;;
	 ;;      LINE-INFO or PREV-LINE-INFO are Nil.
	 ;;
	 ;;      If the line begins quoted and the previous one wasn't
	 ;;      ended quoted.
	 ;;
	 ;;      The Line's signature slot is invalid (the line has changed).

	 (when (or for-sure-parse
		   (not line-info)
		   (not prev-line-info)

		   (not (eq (lisp-info-begins-quoted line-info)
			    (lisp-info-ending-quoted prev-line-info)))

		   (not (eql (line-signature test-line)
			     (lisp-info-signature-slot line-info))))

	   (move-to-position mark 0 test-line)

	   (unless line-info
	     (setf line-info (make-lisp-info))
	     (setf (getf (line-plist test-line) 'lisp-info) line-info))

	   (parse-lisp-line-info mark line-info prev-line-info))

	 (when (eq end-line test-line)
	   (return nil))

	 (setq prev-line-info line-info)

	 (setq test-line (line-next test-line)))))))


;;;; Parse block finders.

(defevar "Minimum Lines Parsed"
  "The minimum number of lines before and after the point parsed by Lisp mode."
  :value 50)

(defevar "Maximum Lines Parsed"
  "The maximum number of lines before and after the point parsed by Lisp mode."
  :value 500)

(defevar "Defun Parse Goal"
  "Lisp mode parses the region obtained by skipping this many defuns
   forward and backward from the point, as long as this falls inside the
   range specified by *Minimum Lines Parsed* and *Maximum Lines Parsed*."
  :value 2)

(macrolet ((frob (step end)
	     `(let ((min (value minimum-lines-parsed))
		    (max (value maximum-lines-parsed))
		    (goal (value defun-parse-goal))
		    (last-defun nil))
		(declare (fixnum min max goal))
		(do ((line (mark-line mark) (,step line))
		     (count 0 (1+ count)))
		    ((null line)
		     (,end mark))
		  (declare (fixnum count))
		  (when (char= (line-character line 0) #\()
		    (setq last-defun line)
		    (decf goal)
		    (when (and (<= goal 0) (>= count min))
		      (line-start mark line)
		      (return)))
		  (when (> count max)
		    (line-start mark (or last-defun line))
		    (return))))))

  (defun start-of-parse-block (mark)
    "Return the start of the parse block required around $mark.

     Always include at least *Minimum Lines Parsed* lines before $mark.
     Try to include *Defun Parse Goal* number of top level forms before
     $mark, and always return marks that include at most the *Maximum Lines
     Parsed* before $mark.

     Used by `pre-command-parse-check' via *Parse Start Function*."
    (frob line-previous buffer-start))

  (defun end-of-parse-block (mark)
    "Return the end of the parse block required around $mark.

     Always include at least *Minimum Lines Parsed* lines after $mark.  Try
     to include *Defun Parse Goal* number of top level forms after $mark,
     and always return marks that include at most the *Maximum Lines
     Parsed* after $mark.

     Used by `pre-command-parse-check' via *Parse End Function*."
    (frob line-next buffer-end)))

(defun start-of-search-line (line)
  "Set LINE to the begining line of the block of text to parse."
  (with-mark ((mark (mark line 0)))
    (funcall (value 'Parse-Start-Function) mark)
    (setq line (mark-line mark))))

(defun end-of-search-line (line)
  "Set LINE to the ending line of the block of text to parse."
  (with-mark ((mark (mark line 0)))
    (funcall (value 'Parse-End-Function) mark)
    (setq line (mark-line mark))))


;;;; PARSE-LISP-LINE-INFO.

;;; PARSE-LISP-LINE-INFO -- Internal.
;;;
;;; This parses through the line doing the following things:
;;;
;;;      Counting/Setting the NET-OPEN-PARENS & NET-CLOSE-PARENS.
;;;
;;;      Making all areas of the line that should be invalid (comments,
;;;      char-quotes, and the inside of strings) and such be in
;;;      RANGES-TO-IGNORE.
;;;
;;;      Set BEGINS-QUOTED and ENDING-QUOTED
;;;
(defun parse-lisp-line-info (mark line-info prev-line-info)
  "Parse line and set line information like NET-OPEN-PARENS, NET-CLOSE-PARENS,
   RANGES-TO-INGORE, and ENDING-QUOTED."
  (let ((net-open-parens 0)
	(net-close-parens 0))
    (declare (fixnum net-open-parens net-close-parens))

    ;; Re-set the slots necessary

    (setf (lisp-info-ranges-to-ignore line-info) nil)

    ;; The only way the current line begins quoted is when there
    ;; is a previous line and it's ending was quoted.

    (setf (lisp-info-begins-quoted line-info)
	  (and prev-line-info
	       (lisp-info-ending-quoted prev-line-info)))

    (if (lisp-info-begins-quoted line-info)
	(deal-with-string-quote mark line-info)
	(setf (lisp-info-ending-quoted line-info) nil))

    (unless (lisp-info-ending-quoted line-info)
      (loop
	(find-lisp-char mark)
	(ecase (character-attribute :lisp-syntax (next-character mark))

	  (:open-paren
	   (setq net-open-parens (1+ net-open-parens))
	   (mark-after mark))

	  (:close-paren
	   (if (zerop net-open-parens)
	       (setq net-close-parens (1+ net-close-parens))
	       (setq net-open-parens (1- net-open-parens)))
	   (mark-after mark))

	  (:newline
	   (setf (lisp-info-ending-quoted line-info) nil)
	   (return t))

	  (:comment
	   (push-range (cons (mark-charpos mark) (line-length (mark-line mark)))
		       line-info)
	   (setf (lisp-info-ending-quoted line-info) nil)
	   (return t))

	  (:char-quote
	   (mark-after mark)
	   (push-range (cons (mark-charpos mark) (1+ (mark-charpos mark)))
		       line-info)
	   (mark-after mark))

	  (:string-quote
	   (mark-after mark)
	   (unless (deal-with-string-quote mark line-info)
	     (setf (lisp-info-ending-quoted line-info) t)
	     (return t))))))

    (setf (lisp-info-net-open-parens line-info) net-open-parens)
    (setf (lisp-info-net-close-parens line-info) net-close-parens)
    (setf (lisp-info-signature-slot line-info)
	  (line-signature (mark-line mark)))))


;;;; String quote utilities.

;;; VALID-STRING-QUOTE-P
;;;
(defmacro valid-string-quote-p (mark forwardp)
  "Return T if the string-quote indicated by MARK is valid."
  (let ((test-mark (gensym)))
    `(with-mark ((,test-mark ,mark))
       ,(unless forwardp
	  ;; TEST-MARK should always be right before the String-quote to be
	  ;; checked.
	  `(mark-before ,test-mark))
       (when (test-char (next-character ,test-mark) :lisp-syntax :string-quote)
	 (let ((slash-count 0))
	   (loop
	     (mark-before ,test-mark)
	     (if (test-char (next-character ,test-mark) :lisp-syntax :char-quote)
		 (incf slash-count)
		 (return t)))
	   (not (oddp slash-count)))))))

(defmacro find-valid-string-quote (mark &key forwardp (cease-at-eol nil))
  "Expand to a form that will leave MARK before a valid string-quote character,
  in either a forward or backward direction, according to FORWARDP.  If
  CEASE-AT-EOL is T then it will return nil if encountering the EOL before a
  valid string-quote."
  (let ((e-mark (gensym)))
    `(with-mark ((,e-mark ,mark))

       (loop
	(unless (scan-direction ,e-mark ,forwardp :lisp-syntax
				,(if cease-at-eol
				     `(or :newline :string-quote)
				     `:string-quote))
	  (return nil))

	,@(if cease-at-eol
	      `((when (test-char (direction-char ,e-mark ,forwardp) :lisp-syntax
				 :newline)
		  (return nil))))

	(when (valid-string-quote-p ,e-mark ,forwardp)
	  (move-mark ,mark ,e-mark)
	  (return t))

	(neighbor-mark ,e-mark ,forwardp)))))


;;;; DEAL-WITH-STRING-QUOTE.

;;; DEAL-WITH-STRING-QUOTE
;;;
;;; Called when a string is begun (i.e. parse hits a #\").  It checks for a
;;; matching quote on the line that MARK points to, and puts the appropriate
;;; area in the RANGES-TO-IGNORE slot and leaves MARK pointing after this area.
;;; The "appropriate area" is from MARK to the end of the line or the matching
;;; string-quote, whichever comes first.
;;;
(defun deal-with-string-quote (mark info-struct)
  "Alter the current line's info struct as necessary as due to encountering a
   string quote character."
  (with-mark ((e-mark mark))
    (cond ((find-valid-string-quote e-mark :forwardp t :cease-at-eol t)
	   ;; If matching quote is on this line then mark the area between the
	   ;; first quote (MARK) and the matching quote as invalid by pushing
	   ;; its begining and ending into the IGNORE-RANGE.
	   (push-range (cons (mark-charpos mark) (mark-charpos e-mark))
		       info-struct)
	   (setf (lisp-info-ending-quoted info-struct) nil)
	   (mark-after e-mark)
	   (move-mark mark e-mark))
	  ;; If the EOL has been hit before the matching quote then mark the
	  ;; area from MARK to the EOL as invalid.
	  (t
	   (push-range (cons (mark-charpos mark)
			     (1+ (line-length (mark-line mark))))
		       info-struct)
	   ;; The Ending is marked as still being quoted.
	   (setf (lisp-info-ending-quoted info-struct) t)
	   (line-end mark)
	   nil))))


;;;; Character validity checking.

;;; Find-Ignore-Region  --  Internal
;;;
(defun find-ignore-region (mark forwardp)
  "If the character in the specified direction from Mark is in an ignore
   region, then return the region and the line that the region is in as
   values.  If there is no ignore region, then return NIL and the
   Mark-Line.  If the line is not parsed, or there is no character (because
   of being at the buffer beginning or end), then return both values NIL."
  (flet ((scan (line pos)
	   (declare (fixnum pos))
	   (let ((info (getf (line-plist line) 'lisp-info)))
	     (if info
		 (dolist (range (lisp-info-ranges-to-ignore info)
				(values nil line))
		   (let ((start (car range))
			 (end (cdr range)))
		     (declare (fixnum start end))
		     (when (and (>= pos start) (< pos end))
		       (return (values range line)))))
		 (values nil nil)))))
    (let ((pos (mark-charpos mark))
	  (line (mark-line mark)))
      (declare (fixnum pos))
      (cond (forwardp (scan line pos))
	    ((> pos 0) (scan line (1- pos)))
	    (t
	     (let ((prev (line-previous line)))
	       (if prev
		   (scan prev (line-length prev))
		   (values nil nil))))))))

;;; Valid-Spot  --  Public
;;;
(defun valid-spot (mark forwardp)
  "Return true if the character at $mark is a valid spot, else ().

   When $forwardp is set, use the character after mark and vice versa.

   A spot is valid if it is outside commented text, strings and character
   quoting."
  (multiple-value-bind (region line)
		       (find-ignore-region mark forwardp)
    (and line (not region))))

;;; Scan-Direction-Valid  --  Internal
;;;
;;; Like scan-direction, but only stop on valid characters.
;;;
(defmacro scan-direction-valid (mark forwardp &rest forms)
  (let ((n-mark (gensym))
	(n-line (gensym))
	(n-region (gensym))
	(n-won (gensym)))
    `(let ((,n-mark ,mark) (,n-won nil))
       (loop
	 (multiple-value-bind (,n-region ,n-line)
			      (find-ignore-region ,n-mark ,forwardp)
	   (unless ,n-line (return nil))
	   (if ,n-region
	       (move-to-position ,n-mark
				 ,(if forwardp
				      `(cdr ,n-region)
				      `(car ,n-region))
				 ,n-line)
	       (when ,n-won (return t)))
	   ;;
	   ;; Peculiar condition when a quoting character terminates a line.
	   ;; The ignore region is off the end of the line causing %FORM-OFFSET
	   ;; to infinitely loop.
	   (when (> (mark-charpos ,n-mark) (line-length ,n-line))
	     (line-offset ,n-mark 1 0))
	   (unless (scan-direction ,n-mark ,forwardp ,@forms)
	     (return nil))
	   (setq ,n-won t))))))


;;;; List offseting.

;;; %LIST-OFFSET allows for BACKWARD-LIST and FORWARD-LIST to be built
;;; with the same existing structure, with the altering of one variable.
;;; This one variable being FORWARDP.
;;;
(defmacro %list-offset (actual-mark forwardp &key (extra-parens 0) )
  "Expand to code that will go forward one list either backward or forward,
   according to the FORWARDP flag."
  (let ((mark (gensym)))
    `(let ((paren-count ,extra-parens))
       (declare (fixnum paren-count))
       (with-mark ((,mark ,actual-mark))
	 (loop
	   (scan-direction ,mark ,forwardp :lisp-syntax
			   (or :close-paren :open-paren :newline))
	   (let ((ch (direction-char ,mark ,forwardp)))
	     (unless ch (return nil))
	     (when (valid-spot ,mark ,forwardp)
	       (case (character-attribute :lisp-syntax ch)
		 (:close-paren
		  (decf paren-count)
		  ,(when forwardp
		     ;; When going forward, an unmatching close-paren means the
		     ;; end of list.
		     `(when (<= paren-count 0)
			(neighbor-mark ,mark ,forwardp)
			(move-mark ,actual-mark ,mark)
			(return t))))
		 (:open-paren
		  (incf paren-count)
		  ,(unless forwardp             ; Same as above only end of list
		     `(when (>= paren-count 0)  ; is opening parens.
			(neighbor-mark ,mark ,forwardp)
			(move-mark ,actual-mark ,mark)
			(return t))))

		 (:newline
		  ;; When a #\Newline is hit, then the matching paren must lie
		  ;; on some other line so drop down into the multiple line
		  ;; balancing function: QUEST-FOR-BALANCING-PAREN If no paren
		  ;; seen yet, keep going.
		  (cond ((zerop paren-count))
			((quest-for-balancing-paren ,mark paren-count ,forwardp)
			 (move-mark ,actual-mark ,mark)
			 (return t))
			(t
			 (return nil)))))))

	   (neighbor-mark ,mark ,forwardp))))))

(defmacro quest-for-balancing-paren (mark paren-count forwardp)
  "Expand to a form that finds the the balancing paren for however many
   opens or closes are registered by Paren-Count."
  `(let* ((line (mark-line ,mark)))
     (loop
       (setq line (neighbor-line line ,forwardp))
       (or line (return nil))
       (let ((line-info (getf (line-plist line) 'lisp-info))
	     (unbal-paren ,paren-count))
	 (or line-info (return nil))

	 ,(if forwardp
	      `(decf ,paren-count (lisp-info-net-close-parens line-info))
	      `(incf ,paren-count (lisp-info-net-open-parens line-info)))

	 (when ,(if forwardp
		    `(<= ,paren-count 0)
		    `(>= ,paren-count 0))
	   ,(if forwardp
		`(line-start ,mark line)
		`(line-end ,mark line))
	   (return (goto-correct-paren-char ,mark unbal-paren ,forwardp)))

	 ,(if forwardp
	      `(incf ,paren-count (lisp-info-net-open-parens line-info))
	      `(decf ,paren-count (lisp-info-net-close-parens line-info)))))))

(defmacro goto-correct-paren-char (mark paren-count forwardp)
  "Expand to a form that will leave MARK on the correct balancing paren matching
   however many are indicated by COUNT."
  `(with-mark ((m ,mark))
     (let ((count ,paren-count))
       (loop
	 (scan-direction m ,forwardp :lisp-syntax
			 (or :close-paren :open-paren :newline))
	 (when (valid-spot m ,forwardp)
	   (ecase (character-attribute :lisp-syntax (direction-char m ,forwardp))
	     (:close-paren
	      (decf count)
	      ,(when forwardp
		 `(when (zerop count)
		    (neighbor-mark m ,forwardp)
		    (move-mark ,mark m)
		    (return t))))

	     (:open-paren
	      (incf count)
	      ,(unless forwardp
		 `(when (zerop count)
		    (neighbor-mark m ,forwardp)
		    (move-mark ,mark m)
		    (return t))))))
	 (neighbor-mark m ,forwardp)))))

(defun list-offset (mark offset)
  (if (plusp offset)
      (dotimes (i offset t)
	(unless (%list-offset mark t) (return nil)))
      (dotimes (i (- offset) t)
	(unless (%list-offset mark nil) (return nil)))))

(defun forward-up-list (mark)
  "Move mark immediately past a character whose *Lisp Syntax* value is
   :closing-paren."
  (%list-offset mark t :extra-parens 1))

(defun backward-up-list (mark)
  "Moves mark immediately before a character whose *Lisp Syntax* value is
   :opening-paren."
  (%list-offset mark nil :extra-parens -1))


;;;; Top level form location hacks (open parens beginning lines).

;;; NEIGHBOR-TOP-LEVEL is used only in TOP-LEVEL-OFFSET.
;;;
(eval-when (compile eval)
(defmacro neighbor-top-level (line forwardp)
  `(loop
     (when (test-char (line-character ,line 0) :lisp-syntax :open-paren)
       (return t))
     (setf ,line ,(if forwardp `(line-next ,line) `(line-previous ,line)))
     (unless ,line (return nil))))
) ;eval-when

(defun top-level-offset (mark offset)
  "Try move mark $offset top level forms forward if $offset is positive or
   -$offset top level forms backwards if $offset is negative.  If there
   were enough top level forms in the appropriate direction, return $mark,
   else return ().  Only move $mark if there are enough forms."
  (declare (fixnum offset))
  (let* ((line (mark-line mark))
	 (at-start (test-char (line-character line 0) :lisp-syntax :open-paren)))
    (cond ((zerop offset) mark)
	  ((plusp offset)
	   (do ((offset (if at-start offset (1- offset))
			(1- offset)))
	       (nil)
	     (declare (fixnum offset))
	     (unless (neighbor-top-level line t) (return nil))
	     (when (zerop offset) (return (line-start mark line)))
	     (unless (setf line (line-next line)) (return nil))))
	  (t
	   (do ((offset (if (and at-start (start-line-p mark))
			    offset
			    (1+ offset))
			(1+ offset)))
		(nil)
	     (declare (fixnum offset))
	     (unless (neighbor-top-level line nil) (return nil))
	     (when (zerop offset) (return (line-start mark line)))
	     (unless (setf line (line-previous line)) (return nil)))))))

(defun mark-previous-sexp (mark1 mark2)
  "Moves mark2 to mark1 and mark1 to the beginning of the sexp before
   mark1.  If successful, return mark2, else return nil.  The marks may be
   altered in either case."
  (move-mark mark2 mark1)
  (do () ((form-offset mark1 -1))
    (or (line-previous (mark-line mark1))
	(return nil)))
  mark2)

(defun mark-top-level-form (mark1 mark2)
  "Move $mark1 and $mark2 to the beginning and end, respectively, of the
   current or next top level form.

   Use $mark1 is used as a reference to start looking.  Possibly alter the
   marks, even on failure.  If successful, return $mark2, else return ().

   Leave $mark2 at the beginning of the line following the top level form
   if possible, otherwise if the last line has text after the closing
   parenthesis, leave mark immediately after the form."
  (let ((winp (cond ((inside-defun-p mark1)
		     (cond ((not (top-level-offset mark1 -1)) nil)
			   ((not (form-offset (move-mark mark2 mark1) 1)) nil)
			   (t mark2)))
		    ((start-defun-p mark1)
		     (form-offset (move-mark mark2 mark1) 1))
		    ((and (top-level-offset (move-mark mark2 mark1) -1)
			  (start-defun-p mark2)
			  (form-offset mark2 1)
			  (same-line-p mark1 mark2))
		     (form-offset (move-mark mark1 mark2) -1)
		     mark2)
		    ((top-level-offset mark1 1)
		     (form-offset (move-mark mark2 mark1) 1)))))
    (when winp
      (if (blank-after-p mark2) (line-offset mark2 1 0))
      mark2)))

(defun inside-defun-p (mark)
  "Return whether $mark is inside a top level form."
  (with-mark ((m mark))
    (when (top-level-offset m -1)
      (form-offset m 1)
      (mark> m mark))))

(defun start-defun-p (mark)
  "Return whether $mark the beginning of a line immediately before a
   character whose *Lisp Syntax* value is :opening-paren."
  (and (start-line-p mark)
       (test-char (next-character mark) :lisp-syntax :open-paren)))


;;;; Form offseting.

(defmacro %form-offset (mark forwardp)
  `(with-mark ((m ,mark))
     (when (scan-direction-valid m ,forwardp :lisp-syntax
				 (or :open-paren :close-paren
				     :char-quote :string-quote
				     :constituent))
       (ecase (character-attribute :lisp-syntax (direction-char m ,forwardp))
	 (:open-paren
	  (when ,(if forwardp `(list-offset m 1) `(mark-before m))
	    ,(unless forwardp
	       '(scan-direction m nil :lisp-syntax (not :prefix)))
	    (move-mark ,mark m)
	    t))
	 (:close-paren
	  (when ,(if forwardp `(mark-after m) `(list-offset m -1))
	    ,(unless forwardp
	       '(scan-direction m nil :lisp-syntax (not :prefix)))
	    (move-mark ,mark m)
	    t))
	 ((:constituent :char-quote)
	  (scan-direction-valid m ,forwardp :lisp-syntax
				(not (or :constituent :char-quote)))
	  ,(if forwardp
	       `(scan-direction-valid m t :lisp-syntax
				      (not (or :constituent :char-quote)))
	       `(scan-direction-valid m nil :lisp-syntax
				      (not (or :constituent :char-quote
					       :prefix))))
	  (move-mark ,mark m)
	  t)
	 (:string-quote
	  (cond ((valid-spot m ,(not forwardp))
		 (neighbor-mark m ,forwardp)
		 (when (scan-direction-valid m ,forwardp :lisp-syntax
					     :string-quote)
		   (neighbor-mark m ,forwardp)
		   (move-mark ,mark m)
		   t))
		(t (neighbor-mark m ,forwardp)
		   (move-mark ,mark m)
		   t)))))))

(defun form-offset (mark offset)
  "Try move mark $offset forms forward if $offset is positive or -$offset
   forms backwards if $offset is negative.  Always move $mark.  If there
   are enough forms in the appropriate direction, return $mark, else return
   ()."
  (if (plusp offset)
      (dotimes (i offset t)
	(or (%form-offset mark t) (return nil)))
      (dotimes (i (- offset) t)
	(or (%form-offset mark nil) (return nil)))))


;;;; Table of special forms with special indenting requirements.

(defevar "Indent Defanything"
  "The number of special arguments implicitly assumed to be supplied in
   calls to functions whose names begin with \"def\".  If set to (), this
   feature is turned off."
  :value 2)

(defvar *special-forms* (make-hash-table :test #'equal))

(defun defindent (fname args)
  "Define the function with $fname to have $args special arguments.

   `indent-for-lisp', the value of *Indent Function* (as in [Indenting]) in
   `Lisp Mode', uses this definition to specially indent the number of
   given arguments.

   For example, `do' has two special arguments and `with-open-file' has
   one.  There are many of these defined by the system including
   definitions for special editor forms.

   $fname is a case insensitive simple-string that is always treated as
   text; therefore, \"with-a-mumble\" is separate from
   \"mumble:with-a-mumble\".

   If $args is () then clear any special arguments information."
  (check-type fname string)
  (let ((fname (string-upcase fname)))
    (cond ((null args) (remhash fname *special-forms*))
	  (t
	   (check-type args integer)
	   (setf (gethash fname *special-forms*) args)))))

;;; Editor forms.
;;;
(defindent "command-case" 1)
(defindent "defattribute" 1)
(defindent "defcommand" 2)
(defindent "defevar" 1)
(defindent "defmode" 1)
(defindent "defparser" 1)
(defindent "do-alpha-chars" 1)
(defindent "do-buffer-lines" 1)
(defindent "do-headers-buffers" 1)
(defindent "do-headers-lines" 1)
(defindent "do-lines-from-mark" 1)
(defindent "do-processes" 1)
(defindent "do-region-lines" 1)
(defindent "do-strings" 1)
(defindent "elet" 1)
(defindent "frob" 1) ; cover silly FLET and MACROLET names for Rob and Bill.
(defindent "in-directory" 1)
(defindent "save-for-undo" 1)
(defindent "with-headers-mark" 1)
(defindent "with-input-from-region" 1)
(defindent "with-mark" 1)
(defindent "with-output-to-mark" 1)
(defindent "with-output-to-window" 1)
(defindent "with-pop-up-display" 1)
(defindent "with-pop-up-window" 1)
(defindent "with-random-typeout" 1)
(defindent "with-temp-buffer" 1)
(defindent "with-writable-buffer" 1)

;;; Lisp forms.
;;;
(defindent "block" 1)
(defindent "case" 1)
(defindent "case=" 1)
(defindent "catch" 1)
(defindent "ccase" 1)
(defindent "ccase=" 1)
(defindent "compiler-let" 1)
(defindent "ctypecase" 1)
(defindent "collect" 1)
(defindent "defconstant" 1)
(defindent "define-setf-method" 2)
(defindent "destructuring-bind" 2)
(defindent "defmacro" 2)
(defindent "defpackage" 1)
(defindent "defparameter" 1)
(defindent "defstruct" 1)
(defindent "deftype" 2)
(defindent "defun" 2)
(defindent "defvar" 1)
(defindent "do" 2)
(defindent "do*" 2)
(defindent "do-all-symbols" 1)
(defindent "do-dirs" 1)
(defindent "do-external-symbols" 1)
(defindent "do-files" 1)
(defindent "do-hash" 1)
(defindent "do-symbols" 1)
(defindent "dolist" 1)
(defindent "dotimes" 1)
(defindent "ecase" 1)
(defindent "ecase=" 1)
(defindent "etypecase" 1)
(defindent "eval-when" 1)
(defindent "fi*" 1)
(defindent "flet" 1)
(defindent "from-file" 1)
(defindent "iterate" 1)
(defindent "labels" 1)
(defindent "lambda" 1)
(defindent "let" 1)
(defindent "let*" 1)
(defindent "letf" 1)
(defindent "letf*" 1)
(defindent "locally" 0)
(defindent "loop" 0)
(defindent "macrolet" 1)
(defindent "multiple-value-bind" 2)
(defindent "multiple-value-call" 1)
(defindent "multiple-value-prog1" 1)
(defindent "multiple-value-setq" 1)
(defindent "prog1" 1)
(defindent "progv" 2)
(defindent "progn" 0)
(defindent "typecase" 1)
(defindent "to-file" 1)
(defindent "unless" 1)
(defindent "until" 2)
(defindent "until*" 2)
(defindent "unwind-protect" 1)
(defindent "when" 1)
(defindent "while" 2)
(defindent "while*" 2)
(defindent "with-input-from-string" 1)
(defindent "with-open-file" 1)
(defindent "with-temp-file" 1)
(defindent "with-open-stream" 1)
(defindent "with-output-to-string" 1)
(defindent "with-package-iterator" 1)

;;; Library forms.  (FIX define in library)
;;;
(defindent "deftest" 2)

;;; Error/condition system forms.
;;;
(defindent "define-condition" 2)
(defindent "handler-bind" 1)
(defindent "handler-case" 1)
(defindent "restart-bind" 1)
(defindent "restart-case" 1)
(defindent "with-simple-restart" 1)
;;; These are for RESTART-CASE branch formatting.
(defindent "store-value" 1)
(defindent "use-value" 1)
(defindent "muffle-warning" 1)
(defindent "abort" 1)
(defindent "continue" 1)

;;; Debug-internals forms.
;;;
(defindent "do-debug-function-blocks" 1)
(defindent "di:do-debug-function-blocks" 1)
(defindent "do-debug-function-variables" 1)
(defindent "di:do-debug-function-variables" 1)
(defindent "do-debug-block-locations" 1)
(defindent "di:do-debug-block-locations" 1)
;;;
;;; Debug-internals conditions
;;; (define these to make uses of HANDLER-CASE indent branches correctly.)
;;;
(defindent "debug-condition" 1)
(defindent "di:debug-condition" 1)
(defindent "no-debug-info" 1)
(defindent "di:no-debug-info" 1)
(defindent "no-debug-function-returns" 1)
(defindent "di:no-debug-function-returns" 1)
(defindent "no-debug-blocks" 1)
(defindent "di:no-debug-blocks" 1)
(defindent "lambda-list-unavailable" 1)
(defindent "di:lambda-list-unavailable" 1)
(defindent "no-debug-variables" 1)
(defindent "di:no-debug-variables" 1)
(defindent "invalid-value" 1)
(defindent "di:invalid-value" 1)
(defindent "ambiguous-variable-name" 1)
(defindent "di:ambiguous-variable-name" 1)
(defindent "debug-error" 1)
(defindent "di:debug-error" 1)
(defindent "unhandled-condition" 1)
(defindent "di:unhandled-condition" 1)
(defindent "unknown-code-location" 1)
(defindent "di:unknown-code-location" 1)
(defindent "unknown-debug-variable" 1)
(defindent "di:unknown-debug-variable" 1)
(defindent "invalid-control-stack-pointer" 1)
(defindent "di:invalid-control-stack-pointer" 1)
(defindent "frame-function-mismatch" 1)
(defindent "di:frame-function-mismatch" 1)

;;; Xlib forms.
;;;
(defindent "with-gcontext" 1)
(defindent "xlib:with-gcontext" 1)
(defindent "with-state" 1)
(defindent "xlib:with-state" 1)
(defindent "with-display" 1)
(defindent "xlib:with-display" 1)
(defindent "with-event-queue" 1)
(defindent "xlib:with-event-queue" 1)
(defindent "with-server-grabbed" 1)
(defindent "xlib:with-server-grabbed" 1)
(defindent "event-case" 1)
(defindent "xlib:event-case" 1)

;;; CLOS forms.
;;;
(defindent "with-slots" 1)
(defindent "with-slots*" 2) ; obsolete
(defindent "with-accessors" 2)
(defindent "with-accessors*" 2) ; obsolete
(defindent "defclass" 2)
(defindent "print-unreadable-object" 1)

;;; System forms.
;;;
(defindent "alien-bind" 1)
(defindent "def-c-record" 1)
(defindent "defrecord" 1)
(defindent "add-fd-handler" 2)
(defindent "with-fd-handler" 1)

;;; Wire forms.
(defindent "remote" 1)
(defindent "wire:remote" 1)
(defindent "remote-value" 1)
(defindent "wire:remote-value" 1)
(defindent "remote-value-bind" 3)
(defindent "wire:remote-value-bind" 3)

;;; Multiprocessing forms.
(defindent "with-lock-held" 1)
(defindent "process-wait" 1)

;;; Alien forms.
(defindent "with-alien" 1)


;;;; Indentation.

;;; LISP-INDENTATION -- Internal Interface.
;;;
(defun lisp-indentation (mark)
  "Compute number of spaces which mark should be indented according to
   local context and lisp grinding conventions.  This assumes mark is at the
   beginning of the line to be indented."
  (with-mark ((m mark)
	      (temp mark))
    ;; See if we are in a quoted context.
    (or (valid-spot m nil)
	(return-from lisp-indentation (lisp-generic-indentation m)))
    ;; Look for the paren that opens the containing form.
    (or (backward-up-list m)
	(return-from lisp-indentation 0))
    ;; Move after the paren, save the start, and find the form name.
    (mark-after m)
    (with-mark ((start m))
      (or (and (scan-char m :lisp-syntax
			  (not (or :space :prefix :char-quote)))
	       (test-char (next-character m) :lisp-syntax :constituent))
	  (return-from lisp-indentation (mark-column start)))
      (with-mark ((fstart m))
	(scan-char m :lisp-syntax (not :constituent))
	(let* ((fname (nstring-upcase (region-to-string (region fstart m))))
	       (special-args (or (gethash fname *special-forms*)
				 (and (> (length fname) 2)
				      (string= fname "DEF" :end1 3)
				      (value indent-defanything)))))
	  (declare (simple-string fname))
	  ;; Now that we have the form name, did it have special syntax?
	  (cond (special-args
		 (with-mark ((spec m))
		   (cond ((and (form-offset spec special-args)
			       (mark<= spec mark))
			  (1+ (mark-column start)))
			 ((skip-valid-space m)
			  (mark-column m))
			 (t
			  (+ (mark-column start) 3)))))
		;; See if the user seems to have altered the editor's
		;; indentation, and if so, try to adhere to it.  This usually
		;; happens when you type in a quoted list constant that line
		;; wraps.  You want all the items on successive lines to fall
		;; under the first character after the opening paren, not as if
		;; you are calling a function.
		((and (form-offset temp -1)
		      (or (blank-before-p temp) (not (same-line-p temp fstart)))
		      (not (same-line-p temp mark)))
		 (unless (blank-before-p temp)
		   (line-start temp)
		   (find-attribute temp :space #'zerop))
		 (mark-column temp))
		;; Appears to be a normal form.  Is the first arg on the same
		;; line as the form name?
		((skip-valid-space m)
		 (or (lisp-indentation-check-for-local-def
		      mark temp fstart start t)
		     (mark-column m)))
		;; Okay, fall under the first character after the opening paren.
		(t
		 (or (lisp-indentation-check-for-local-def
		      mark temp fstart start nil)
		     (mark-column start)))))))))

(defevar "Lisp Indentation Local Definers"
  "Forms with syntax like LABELS, MACROLET, etc."
  :value '("LABELS" "MACROLET" "FLET"))

;;; LISP-INDENTATION-CHECK-FOR-LOCAL-DEF -- Internal.
;;;
;;; This is a temporary hack to see how it performs.  When we are indenting
;;; what appears to be a function call, let's look for FLET or MACROLET to see
;;; if we really are indenting a local definition.  If we are, return the
;;; indentation for a DEFUN; otherwise, nil
;;;
;;; Mark is the argument to LISP-INDENTATION.  Start is just inside the paren
;;; of what looks like a function call.  If we are in an FLET, arg-list
;;; indicates whether the local function's arg-list has been entered, that is,
;;; whether we need to normally indent for a DEFUN body or indent specially for
;;; the arg-list.
;;;
(defun lisp-indentation-check-for-local-def (mark temp1 temp2 start arg-list)
  ;; We know this succeeds from LISP-INDENTATION.
  (backward-up-list (move-mark temp1 mark)) ;Paren for local definition.
  (cond ((and (backward-up-list temp1)	    ;Paren opening the list of defs
	      (form-offset (move-mark temp2 temp1) -1)
	      (mark-before temp2)
	      (backward-up-list temp1)	    ;Paren for FLET or MACROLET.
	      (mark= temp1 temp2))	    ;Must be in first arg form.
	 ;; See if the containing form is named FLET or MACROLET.
	 (mark-after temp1)
	 (or (and (scan-char temp1 :lisp-syntax
			     (not (or :space :prefix :char-quote)))
		  (test-char (next-character temp1) :lisp-syntax
			     :constituent))
	     (return-from lisp-indentation-check-for-local-def nil))
	 (move-mark temp2 temp1)
	 (scan-char temp2 :lisp-syntax (not :constituent))
	 (let ((fname (nstring-upcase (region-to-string (region temp1 temp2)))))
	   (cond ((not (member fname (value lisp-indentation-local-definers)
			       :test #'string=))
		  nil)
		 (arg-list
		  (1+ (mark-column start)))
		 (t
		  (+ (mark-column start) 3)))))))

;;; LISP-GENERIC-INDENTATION -- Internal.
;;;
(defun lisp-generic-indentation (mark)
  "`lisp-indentation' calls this when mark is in a invalid spot, or quoted
   context.  If we are inside a string, we return the column one greater
   than the opening double quote.  Otherwise, we just use the indentation
   of the first preceding non-blank line."
  (with-mark ((m mark))
    (form-offset m -1)
    (cond ((eq (character-attribute :lisp-syntax (next-character m))
	       :string-quote)
	   (1+ (mark-column m)))
	  (t
	   (let* ((line (mark-line mark))
		  (prev (do ((line (line-previous line) (line-previous line)))
			    ((not (and line (blank-line-p line))) line))))
	     (cond (prev
		    (line-start mark prev)
		    (find-attribute mark :space #'zerop)
		    (mark-column mark))
		   (t 0)))))))

;;; Skip-Valid-Space  --  Internal
;;;
;;; Skip over any space on the line Mark is on, stopping at the first valid
;;; non-space character.  If there is none on the line, return nil.
;;;
(defun skip-valid-space (mark)
  (loop
    (scan-char mark :lisp-syntax (not :space))
    (let ((val (character-attribute :lisp-syntax
				    (next-character mark))))
      (cond ((eq val :newline) (return nil))
	    ((valid-spot mark t) (return mark))))
    (mark-after mark)))

(declaim (optimize (speed 0))); byte compile again


;;;; Indentation commands and hook functions.

(defcommand "Defindent" (p)
  "Define the Lisp indentation for the current function.  The indentation
   is a positive integer or zero, defining the number of special arguments
   for the form.  Examples: 2 for Do, 1 for Dolist.  If a prefix argument
   is supplied, then clear the indentation information."
  (with-mark ((m (current-point)))
    (pre-command-parse-check m)
    (or (backward-up-list m)
	(editor-error "Point must be enclosed by a list."))
    (mark-after m)
    (with-mark ((n m))
      (scan-char n :lisp-syntax (not :constituent))
      (let ((s (region-to-string (region m n))))
	(declare (simple-string s))
	(if (zerop (length s))
	    (editor-error "List must start with a name."))
	(if p
	    (defindent s nil)
	    (let ((i (prompt-for-integer
		      :prompt (format nil "Indentation for ~A: " s)
		      :help "Number of special arguments.")))
	      (if (minusp i)
		  (editor-error "Indentation must be non-negative."))
	      (defindent s i))))))
  (indent-command))

(defcommand "Indent Form" ()
  "Indent all the lines in the current form, leaving the point in place."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((m point))
      (or (form-offset m 1)
	  (editor-error "Point must be enclosed by a list."))
      (lisp-indent-region (region point m) "Indent Form"))))

;;; LISP-INDENT-REGION -- Internal.
;;;
;;; This indents a region of Lisp code without doing excessive redundant
;;; computation.  We parse the entire region once, then scan through doing
;;; indentation on each line.  We forcibly reparse each line that we indent so
;;; that the list operations done to determine indentation of subsequent lines
;;; will work.  This is done undoably with save1, save2, buf-region, and
;;; undo-region.
;;;
(defun lisp-indent-region (region &optional
				  (undo-text "Lisp region indenting"))
  (check-region-query-size region)
  (let ((start (region-start region))
	(end (region-end region)))
    (with-mark ((m1 start)
		(m2 end))
      (funcall (value parse-start-function) m1)
      (funcall (value parse-end-function) m2)
      (parse-over-block (mark-line m1) (mark-line m2)))
    (let* ((first-line (mark-line start))
	   (last-line (mark-line end))
	   (prev (line-previous first-line))
	   (prev-line-info
	    (and prev (getf (line-plist prev) 'lisp-info)))
	   (save1 (line-start (copy-mark start :right-inserting)))
	   (save2 (line-end (copy-mark end :left-inserting)))
	   (buf-region (region save1 save2))
	   (undo-region (copy-region buf-region)))
      (with-mark ((bol start :left-inserting))
	(do ((line first-line (line-next line)))
	    (nil)
	  (line-start bol line)
	  (insert-lisp-indentation bol)
	  (let ((line-info (getf (line-plist line) 'lisp-info)))
	    (parse-lisp-line-info bol line-info prev-line-info)
	    (setq prev-line-info line-info))
	  (when (eq line last-line) (return nil))))
      (make-region-undo :twiddle undo-text buf-region undo-region))))

;;; INDENT-FOR-LISP -- Internal.
;;;
;;; This is the value of "Indent Function" for "Lisp" mode.
;;;
(defun indent-for-lisp (mark)
  (line-start mark)
  (pre-command-parse-check mark)
  (insert-lisp-indentation mark))

(defun insert-lisp-indentation (m)
  (delete-horizontal-space m)
  (funcall (value indent-with-tabs) m (lisp-indentation m)))


#[ Form Manipulation

These commands manipulate Lisp forms, the printed representations of Lisp
objects.  A form is either an expression balanced with respect to parentheses
or an atom such as a symbol or string.

{command:Forward Form}
{command:Backward Form}
{command:Forward Kill Form}
{command:Backward Kill Form}
{command:Mark Form}
{command:Transpose Forms}
{command:Insert ()}
{command:Extract Form}

`Extract Form' and `Extract List' are similar.  `Exract Form' is more
generally useful since it works on any kind of form.
]#

#[ List Manipulation

List commands are similar to form commands, but
they only pay attention to lists, ignoring any atomic objects that may
appear.  These commands are useful because they can skip over many symbols
and move up and down in the list structure.

{command:Forward List}
{command:Backward List}
{command:Forward Up List}
{command:Backward Up List}
{command:Down List}
{command:Extract List}
]#

#[ Defun Manipulation

A defun is a list whose open parenthesis is
against the left margin.  It is called this because an occurrence of the
defun top level form usually satisfies this definition, but
other top level forms such as a defstruct and defmacro work just as
well.

{command:End of Defun}
{command:Beginning of Defun}
{command:Mark Defun}
]#

#[ Lisp Indentation

One of the most important features provided by `Lisp' mode is automatic
indentation of Lisp code.  Since unindented Lisp is unreadable, poorly
indented Lisp is hard to manage, and inconsistently indented Lisp is subtly
misleading.  See section [indentation] for a description of the
general-purpose indentation commands.  `Lisp' mode uses these indentation
rules:

  - If in a semicolon (;) comment, then use the standard comment indentation
     rules.  See page pagerefcomment-indentation.

  - If in a quoted string, then indent to the column one greater than the column
     containing the opening double quote.  This is exactly what you want in function
     documentation strings and wrapping error strings.

  - If there is no enclosing list, then use no indentation.

  - If enclosing list resembles a call to a known macro or special-form, then the
     first few arguments are given greater indentation and the first body form is
     indented two spaces.  If the first special argument is on the same line as the
     beginning of the form, then following special arguments will be indented to the
     start of the first special argument, otherwise all special arguments are
     indented four spaces.

  - If the previous form starts on its own line, then the indentation is copied
     from that form.  This rule allows the default indentation to be overridden:
     once a form has been manually indented to the user's satisfaction, subsequent
     forms will be indented in the same way.

  - If the enclosing list has some arguments on the same line as the form start,
     then subsequent arguments will be indented to the start of the first argument.

  - If the enclosing list has no argument on the same line as the form start, then
     arguments will be indented one space.

{command:Indent Form}
{command:Fill Lisp Comment Paragraph}
{evariable:Fill Lisp Comment Paragraph Confirm}
{command:Defindent}
{evariable:Indent Defanything}
{command:Move Over )}
]#


;;;; Most "Lisp" mode commands.

(defcommand "Beginning of Defun" (p)
  "Move the point to the beginning of the current or next top-level form.
   With an argument, move backward that many top-level forms.  With a
   negative argument, move forward instead of backward."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(end-of-defun-command (- count))
	(fi (top-level-offset point (- count))
	    (editor-error "Too few forms before point.")))))

;;; "End of Defun", with a positive p (the normal case), does something weird.
;;; Get a mark at the beginning of the defun, and then offset it forward one
;;; less top level form than we want.  This sets us up to use FORM-OFFSET which
;;; allows us to leave the point immediately after the defun.  If we used
;;; TOP-LEVEL-OFFSET one less than p on the mark at the end of the current
;;; defun, point would be left at the beginning of the p+1'st form instead of
;;; at the end of the p'th form.
;;;
(defcommand "End of Defun" (p)
  "Move the point to the end of the current or next top-level form.  With
   an argument, move forward that many top-level forms."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(beginning-of-defun-command (- count))
	(with-mark ((m point)
		    (dummy point))
	  (cond ((not (mark-top-level-form m dummy))
		 (editor-error "No current or next top level form."))
		(t
		 (or (top-level-offset m (1- count))
		     (editor-error "Not enough top level forms."))
		 ;; We might be one unparsed for away.
		 (pre-command-parse-check m)
		 (or (form-offset m 1)
		     (editor-error "Not enough top level forms."))
		 (when (blank-after-p m) (line-offset m 1 0))
		 (move-mark point m)))))))

(defcommand "Forward List" (p)
  "Move the point to immediately after the end of the next list at the
   current level of list structure.  If at the end of the current list
   level, then move point up past the end of the containing list.  With a
   prefix argument move that many lists."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (or (list-offset point count)
	(editor-error "Too few lists to move over."))))

(defcommand "Backward List" (p)
  "Move the point to immediately before the beginning of the next list at
   the current level of list structure.  If at the end of the current list
   level, then move point up past the beginning of the containing list.
   With a prefix argument move that many lists."
  (let ((point (current-point))
	(count (- (or p 1))))
    (pre-command-parse-check point)
    (or (list-offset point count)
	(editor-error "Too few lists to move over."))))

(defcommand "Forward Form" (p)
  "Move to the end of the current or next form.  With a prefix argument
   move forward that many forms."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (or (form-offset point count)
	(editor-error "Too few forms to move over."))))

(defcommand "Backward Form" (p)
  "Move to the beginning of the current or next form.  With a prefix
   argument move backward that many forms."
  (let ((point (current-point))
	(count (- (or p 1))))
    (pre-command-parse-check point)
    (or (form-offset point count)
	(editor-error "Too few forms to move over."))))

(defcommand "Mark Form" (p)
  "Set the mark at the end of the next form.  With a positive argument, set
   the mark after that many following forms. With a negative argument, set
   the mark before that many preceding forms."
  (with-mark ((m (current-point)))
    (pre-command-parse-check m)
    (let ((count (or p 1))
	  (mark (push-buffer-mark (copy-mark m) t)))
      (if (form-offset m count)
	  (move-mark mark m)
	  (editor-error "Too few forms to mark.")))))

(defcommand "Mark Defun" ()
  "Put the region around the next or containing top-level form.  Leave the
   point before the form and the mark immediately after it."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((start point)
		(end point))
      (or (mark-top-level-form start end)
	  (editor-error "Point must be on or before a top level form."))
      (move-mark point start)
      (move-mark (push-buffer-mark (copy-mark point) t) end))))

(defcommand "Forward Kill Form" (p)
  "Kill text from the point to the end of the current form.  If at the end
   of a list and inside the close parenthesis, then kill the close
   parenthesis.  With a prefix argument kill that many forms."
  (with-mark ((m1 (current-point))
	      (m2 (current-point)))
    (pre-command-parse-check m1)
    (let ((count (or p 1)))
      (or (form-offset m1 count)
	  (editor-error ""))
      (if (minusp count)
	  (kill-region (region m1 m2) :kill-backward)
	  (kill-region (region m2 m1) :kill-forward)))))

(defcommand "Backward Kill Form" (p)
  "Kill text from the point to the beginning of the current form.  If at
   the beginning of a list and inside the close parenthesis, then kill the
   close parenthesis.  With a prefix argument kill that many forms."
  (forward-kill-form-command (- (or p 1))))

(defcommand "Extract Form" (p)
  "Replace the current containing list with the next form, pushing the
   entire affected area onto the kill ring.  If an argument is supplied,
   replace that many upward levels of list nesting by the next form."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((form-start point :right-inserting)
		(form-end point))
      (or (form-offset form-end 1)
 	  (editor-error "A form must follow point."))
      (form-offset (move-mark form-start form-end) -1)
      (with-mark ((containing-start form-start :left-inserting)
		  (containing-end form-end :left-inserting))
	(dotimes (i (or p 1))
	  (or (and (forward-up-list containing-end)
		   (backward-up-list containing-start))
	      (editor-error "Too few enclosing lists.")))
	(let ((r (copy-region (region form-start form-end))))
	  (ring-push (delete-and-save-region
		      (region containing-start containing-end))
		     *kill-ring*)
	  (ninsert-region point r)
	  (move-mark point form-start))))))

(defcommand "Extract List" (p)
  "Extract the current list.  Replace the surrounding list with the current
   list.  Push the entire affected area is pushed onto the kill-ring.  With
   a prefix argument, replace that many surrounding lists."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((lstart point :right-inserting)
		(lend point))
      (if (eq (character-attribute :lisp-syntax (next-character lstart))
	      :open-paren)
	  (mark-after lend)
	  (or (backward-up-list lstart)
	      (editor-error "Point must be inside a list.")))
      (or (forward-up-list lend)
	  (editor-error "Current list must be closed."))
      (with-mark ((rstart lstart)
		  (rend lend))
	(dotimes (i (or p 1))
	  (or (and (forward-up-list rend) (backward-up-list rstart))
	      (editor-error "Too few enclosing lists.")))
	(let ((r (copy-region (region lstart lend))))
	  (ring-push (delete-and-save-region (region rstart rend))
		     *kill-ring*)
	  (ninsert-region point r)
	  (move-mark point lstart))))))

(defcommand "Transpose Forms" (p)
  "Transpose (swap) the forms immediately preceding and following the
   point.  With a zero argument, tranpose the forms at the point and the
   mark.  With a positive argument, transpose the form preceding the point
   with the form argument number of forms following it.  With a negative
   argument, transposes the form following the point with the argument-th
   one preceding it."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (zerop count)
	(let ((mark (current-mark)))
	  (with-mark ((s1 mark :left-inserting)
		      (s2 point :left-inserting))
	    (scan-char s1 :whitespace nil)
	    (scan-char s2 :whitespace nil)
	    (with-mark ((e1 s1 :right-inserting)
			(e2 s2 :right-inserting))
	      (or (form-offset e1 1)
		  (editor-error "There must be a form at the mark."))
	      (or (form-offset e2 1)
		  (editor-error "There must be a form at the point."))
	      (ninsert-region s1 (delete-and-save-region (region s2 e2)))
	      (ninsert-region s2 (delete-and-save-region (region s1 e1))))))
	(let ((fcount (if (plusp count) count 1))
	      (bcount (if (plusp count) 1 count)))
	  (with-mark ((s1 point :left-inserting)
		      (e2 point :right-inserting))
	    (dotimes (i bcount)
	      (or (form-offset s1 -1)
		  (editor-error "Too few forms before point.")))
	    (dotimes (i fcount)
	      (or (form-offset e2 1)
		  (editor-error "Too few forms after point.")))
	    (with-mark ((e1 s1 :right-inserting)
			(s2 e2 :left-inserting))
	      (or (form-offset e1 1)
		  (editor-error "First form must be closed."))
	      (or (form-offset s2 -1)
		  (editor-error "Second form must be opened."))
	      (ninsert-region s1 (delete-and-save-region (region s2 e2)))
	      (ninsert-region s2 (delete-and-save-region (region s1 e1)))
	      (move-mark point s2)))))))

(defcommand "Insert ()" (p)
  "Insert a pair of parentheses: ().  With positive argument, puts
   parentheses around that many following forms.  Position the point after
   the open parenthesis."
  (let ((point (current-point))
	(count (or p 0)))
    (pre-command-parse-check point)
    (if (minusp count)
	(editor-error
	 "Prefix argument to `Insert ()' must be zero or greater."))
    (insert-character point #\()
    (with-mark ((tmark point))
      (or (form-offset tmark count)
	  ;; FIX most other commands leave buffer same when too few
	  (message "Too few forms.  Adding closing paren anyway."))
      (cond ((mark= tmark point)
	     (insert-character point #\))
	     (mark-before point))
	    (t (insert-character tmark #\)))))))

(defcommand "Move Over )" ()
  "Move past the next close parenthesis, and start a new line.  Flush any
   indentation preceding the parenthesis, and indent the new line."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((m point :left-inserting))
      (cond ((scan-char m :lisp-syntax :close-paren)
	     (delete-horizontal-space m)
	     (mark-after m)
	     (move-mark point m)
	     (delete-mark m)
	     (indent-new-line-command 1))
	    (t
	     (delete-mark m)
	     (editor-error "Past last close paren."))))))

(defcommand "Forward Up List" (p)
  "Move point to after the end of the enclosing list.  With a prefix move
   that many lists."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(backward-up-list-command (- count))
	(with-mark ((m point))
	  (dotimes (i count (move-mark point m))
	    (fi (forward-up-list m)
		(editor-error "Too few enclosing lists.")))))))

(defcommand "Backward Up List" (p)
  "Move point to before the end of the enclosing list.  With a prefix move
   that many lists."
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (if (minusp count)
	(forward-up-list-command (- count))
	(with-mark ((m point))
	  (dotimes (i count (move-mark point m))
	    (fi (backward-up-list m)
		(editor-error "Too few enclosing lists.")))))))

(defcommand "Down List" (p)
  "Move down a level in list structure.  That is, move point just after the
   beginning of the next list.  With a prefix argument move down that many
   times."
  ;; FIX With negative p move up.
  (let ((point (current-point))
	(count (or p 1)))
    (pre-command-parse-check point)
    (with-mark ((m point))
      (dotimes (i count (move-mark point m))
	(or (and (scan-char m :lisp-syntax :open-paren)
		     (mark-after m))
	    (editor-error "Too few lists after point."))))))


;;;; Filling Lisp comments, strings, and indented text.

(defevar "Fill Lisp Comment Paragraph Confirm"
  "This determines whether `Fill Lisp Comment Paragraph' prompts for
   confirmation to fill contiguous lines with the same initial whitespace
   when it is invoked outside a comment or string."
  :value ())

(defcommand "Fill Lisp Comment Paragraph" (p)
  "Fill a flushleft or indented Lisp comment.  Also fill Lisp string
   literals using the proper indentation as a filling prefix.  When invoked
   outside a comment or string, try to fill all contiguous lines beginning
   with the same initial whitespace.  When filling a comment, use the
   current line to determine a fill prefix by taking all the initial
   whitespace on the line, the semicolons, and any whitespace following the
   semicolons.

   When invoked outside of comment or string and `Fill Lisp Comment
   Paragraph Confirm' is true, prompt for confirmation before filling.
   This can be useful for filling long export lists or other indented text
   or symbols."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((start point)
		(end point)
		(m point))
      (let ((commentp (fill-lisp-comment-paragraph-prefix start end)))
	(cond (commentp
	       (fill-lisp-comment-or-indented-text start end))
	      ((fi (valid-spot m nil)
		   (if p
		       (progn
			 (form-offset m -1)
			 (eq (character-attribute :lisp-syntax (next-character m))
			     :string-quote))
		       t))
	       ; FIX orig
	       ;(fill-lisp-comment-or-indented-text start end))
	       (if p
		   (fill-lisp-string m t)  ; Fill entire string.
		   ;; FIX special case first line for leading "
		   (fill-lisp-string-paragraph m)))
	      ((if (value fill-lisp-comment-paragraph-confirm)
		   (prompt-for-y-or-n
		    :prompt '("Not in a comment or string.  Fill contiguous ~
			       lines with the same initial whitespace? "))
		   t)
	       (if (mark= start end)
		   (fill-paragraph-command)
		   (fill-lisp-comment-or-indented-text start end))))))))

;;; FILL-LISP-STRING -- Internal.
;;;
(defun fill-lisp-string (mark &optional keep-paragraphs)
  "Fill the Lisp string containing $mark as if it had been entered using
   the editor's Lisp string indentation, `Indent Function' for \"Lisp\"
   mode.  Assume the area around $mark has already been passed to
   `pre-command-parse-check', and ensure the string ends before doing any
   filling.  `Undo'able."
  (break)
  (with-mark ((end mark))
    (or (form-offset end 1)
	(editor-error "Attempt to fill an open-ended Lisp string."))
    (let* ((mark (copy-mark mark :left-inserting))
	   (end (copy-mark end :left-inserting))
	   (string-region (region mark end))
	   (undo-region (copy-region string-region))
	   (hack (make-empty-region)))
      ;; Generate prefix.
      (funcall (value indent-with-tabs)
	       (region-end hack) (1+ (mark-column mark)))
      ;; Skip opening double quote and fill string starting on its own line.
      (mark-after mark)
      (insert-character mark #\newline)
      (line-start mark)
      (setf (mark-kind mark) :right-inserting)
      (fill-region string-region (region-to-string hack)
		   (value fill-column) keep-paragraphs)
      ;; Clean up inserted prefix on first line, delete inserted newline,
      ;; and move before the double quote for undo.
      (with-mark ((text mark :left-inserting))
	(find-attribute text :whitespace #'zerop)
	(delete-region (region mark text)))
      (delete-characters mark -1)
      (mark-before mark)
      ;; Save undo.
      (make-region-undo :twiddle "Fill Lisp Comment Paragraph"
			string-region undo-region))))

;;; FILL-LISP-STRING-PARAGRAPH -- Internal.
;;;
(defun fill-lisp-string-paragraph (mark)
  "Fill the Lisp string paragraph containing $mark as if it had been
   entered using the editor's Lisp string indentation, *Indent Function*
   for \"Lisp\" mode.  Assume the area around $mark has already been passed
   to `pre-command-parse-check', and ensure the string ends before doing
   any filling.  `Undo'able."
  (with-mark ((start mark)
	      (end mark))
    (or (form-offset start -1)
	(editor-error "Attempt to fill an open-ended Lisp string."))
    (or (form-offset end 1)
	(editor-error "Attempt to fill an open-ended Lisp string."))
    (let* ((mark (copy-mark mark :left-inserting))
	   (mark2 (copy-mark mark :left-inserting))
	   (start (copy-mark start :left-inserting))
	   (end (copy-mark end :left-inserting)))
      ;; Find paragraph.
      (do-lines-from-mark (line mark :backwards t)
	(when (blank-line-p line)
	  (move-mark mark2 (mark line 0))
	  (return)))
      (when (mark< start mark2)
	(move-mark start mark2)
	(mark-after start))
      (move-mark mark2 mark)
      (do-lines-from-mark (line mark)
	(when (blank-line-p line)
	  (move-mark mark2 (mark line 0))
	  (return)))
      (if (mark> end mark2) (move-mark end mark2))
      (let* ((string-region (region start end))
	     (undo-region (copy-region string-region))
	     (hack (make-empty-region)))
	;; Save the white region at the beginning of the first line.
	(line-start mark)
	(move-mark (region-start hack) mark)
	(find-attribute mark :whitespace #'zerop)
	(let ((start-p (char= (next-character mark) #\")))
	  (when start-p
	    ;; Temporarily replace the opening quotation mark with a space.
	    ;; FIX use equivalent space with maximum num tabs?
	    (delete-characters mark 1)
	    (insert-character mark #\space))
	  (move-mark (region-end hack) mark)
	  (setq hack (copy-region hack))
	  (fill-region string-region (region-to-string hack)
		       (value fill-column))
	  (when start-p
	    ;; Add back the opening quotation mark.
	    (mark-before mark)
	    (delete-characters mark 1)
	    (insert-character mark #\")))
	;; Save undo.
	(make-region-undo :twiddle "Fill Lisp Comment Paragraph"
			  string-region undo-region)))))

;;; FILL-LISP-COMMENT-OR-INDENTED-TEXT -- Internal.
;;;
;;; This fills all contiguous lines around start and end containing fill prefix
;;; designated by the region between start and end.  These marks can only be
;;; equal when there is no comment and no initial whitespace.  This is a bad
;;; situation since this function in that situation would fill the entire
;;; buffer into one paragraph.  This function is undo'able.
;;;
(defun fill-lisp-comment-or-indented-text (start end)
  (if (mark= start end)
      (editor-error "This command only fills Lisp comments, strings, or ~
		     indented text, but this line is flushleft."))
  ;;
  ;; Find comment block.
  (let* ((prefix (region-to-string (region start end)))
	 (length (length prefix)))
    (declare (simple-string prefix))
    (flet ((frob (mark direction)
	     (loop
	       (let* ((line (line-string (mark-line mark)))
		      (line-len (length line)))
		 (declare (simple-string line))
		 (unless (string= line prefix :end1 (min line-len length))
		   (when (= direction -1)
		     (or (same-line-p mark end) (line-offset mark 1 0)))
		   (return)))
	       (unless (line-offset mark direction 0)
		 (when (= direction 1) (line-end mark))
		 (return)))))
      (frob start -1)
      (frob end 1))
    ;;
    ;; Do it undoable.
    (let* ((start1 (copy-mark start :right-inserting))
	   (end2 (copy-mark end :left-inserting))
	   (region (region start1 end2))
	   (undo-region (copy-region region)))
      (fill-region region prefix)
      (make-region-undo :twiddle "Fill Lisp Comment Paragraph"
			region undo-region))))

;;; FILL-LISP-COMMENT-PARAGRAPH-PREFIX -- Internal.
;;;
;;; This sets start and end around the prefix to be used for filling.  We
;;; assume we are dealing with a comment.  If there is no ";", then we try to
;;; find some initial whitespace.  If there is a ";", we make sure the line is
;;; blank before it to eliminate ";"'s in the middle of a line of text.
;;; Finally, if we really have a comment instead of some indented text, we skip
;;; the ";"'s and any immediately following whitespace.  We allow initial
;;; whitespace, so we can fill strings with the same command.
;;;
(defun fill-lisp-comment-paragraph-prefix (start end)
  (line-start start)
  (let ((commentp t)) ; Assumes there's a comment.
    (unless (to-line-comment (line-start end) ";")
      (find-attribute end :whitespace #'zerop)
      #|(when (start-line-p end)
	(editor-error "No comment on line, and no initial whitespace."))|#
      (setf commentp nil))
    (when commentp
      (unless (blank-before-p end)
	(find-attribute (line-start end) :whitespace #'zerop)
	#|(when (start-line-p end)
	  (editor-error "Semicolon preceded by unindented text."))|#
	(setf commentp nil)))
    (when commentp
      (find-attribute end :lisp-syntax #'(lambda (x) (not (eq x :comment))))
      (find-attribute end :whitespace #'zerop))
    commentp))


;;;; "Lisp" mode.

#[ Lisp Mode

Lisp mode is a major mode used for editing Lisp code.  Although most Lisp
specific commands are globally bound, `Lisp' mode is necessary to enable
Lisp indentation, commenting, and parenthesis-matching.  Whenever the user
or some editor mechanism turns on `Lisp' mode, the mode's setup includes
locally setting `Current Package' (as in [Lisp package]) in that buffer if
its value is non-existent there; the value used is USER.

{mode:Lisp}
]#

(defmode "Lisp" :major-p t :setup-function 'setup-lisp-mode)

(declaim (special *mode-highlighters*))

(defun setup-lisp-mode (buffer)
  (or (editor-bound-p 'current-package :buffer buffer)
      (defevar "Current Package"
	"The package used for evaluation of Lisp in this buffer."
	:buffer buffer
	:value (let ((form (with-input-from-region (stream (buffer-region buffer))
			     (read stream ()))))
		 (if (and form (listp form)
			  (eq (car form) 'in-package))
		     (cadr form)
		     "USER"))))
  (highlight-visible-lisp-buffer buffer)
  (pushnew '("Lisp" t highlight-visible-lisp-buffer) *mode-highlighters*))


#[ Parenthesis Matching

Another very important facility provided by
`Lisp' mode is parenthesis matching.  Two different styles of
parenthesis matching are supported: highlighting and pausing.

{evariable:Highlight Open Parens}
{evariable:Open Paren Highlighting Font}
{command:Lisp Insert )}
{evariable:Paren Pause Period}

The initial values shown for `Highlight Open Parens' and `Paren Pause
Period' are only approximately correct.  Since paren highlighting is only
meaningful in Lisp mode, `Highlight Open Parens' is false globally, and
has a mode-local value of true in Lisp mode.  It it redundant to do both
kinds of paren matching, so there is also a binding of `Paren Pause Period'
to false in Lisp mode.

Paren highlighting is only supported under X Windows, so the above defaults are
conditional on the device type.  If the editor is started on a terminal, the
initialization code makes Lisp mode bindings of false and 0.5 for
`Highlight Open Parens' and `Paren Pause Period'.  Since these
alternate default bindings are made at initialization time, the only way to
affect them is to use the after-editor-initializations macro.
]#


;;;; Matching parenthesis display.

(defevar "Paren Pause Period"
  "The number of seconds that commands that deal with \"brackets\" show the
   cursor at the matching \"bracket\"."
  :value 0.5)

(defcommand "Lisp Insert )" ()
  "Insert a close parenthesis and then attempt to display the matching open
   parenthesis by placing the cursor on top of it for `Paren Pause Period'
   seconds.  If the matching parenthesis is off the top of the screen, then
   echo the line on which it appears instead.  If the search for a matching
   paren fails, then beep.  The paren highlighting may be turned off by
   clearing `Paren Pause Period'."
  (let ((point (current-point)))
    (insert-character point #\))
    (pre-command-parse-check point)
    (when (valid-spot point ())
      (with-mark ((m point))
	(if (list-offset m -1)
	    (let ((pause (value paren-pause-period))
		  (win (current-window)))
	      (if pause
		  (unless (show-mark m win pause)
		    (clear-echo-area)
		    (message "~A" (line-string (mark-line m))))
		  (unless (displayed-p m (current-window))
		    (clear-echo-area)
		    (message "~A" (line-string (mark-line m))))))
	    (editor-error "Added surplus close paren."))))))

;;; Since we use paren highlighting in Lisp mode, we do not want paren
;;; flashing too.
;;;
(defevar "Paren Pause Period"
  "This is how long commands that deal with \"brackets\" shows the cursor at
   the matching \"bracket\" for this number of seconds."
  :mode "Lisp")
;;;
(defevar "Highlight Open Parens"
  "When true, and a close paren is immediately before the point, then
   display the matching open paren in `Open Paren Highlighting Font'."
  :value t
  :mode "Lisp")

(defevar "Open Paren Finder Function"
  "Should be a function that takes a mark for input and returns either NIL
   if the mark is not after a close paren, or two (temporary) marks
   surrounding the corresponding open paren."
  :mode "Lisp"
  :value 'lisp-open-paren-finder-function)

(defun lisp-open-paren-finder-function (mark)
  (when (eq (character-attribute :lisp-syntax (previous-character mark))
	    :close-paren)
    (with-mark ((mark mark))
      (pre-command-parse-check mark)
      (if (not (and (valid-spot mark nil) (list-offset mark -1)))
	  (values nil nil)
	  (values mark (mark-after (copy-mark mark)))))))


;;;; Some mode variables to coordinate with other stuff.

(defevar "Auto Fill Space Indent"
  "When true, use `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Lisp" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Lisp" :value ";")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Lisp" :value "; ")

(defevar "Indent Function"
  "Indentation function invoked by the `Indent command.  The function takes
   a :left-inserting mark that may be moved, and indents the line that the
   mark is on."
  :value 'indent-for-lisp
  :mode "Lisp")


;;;; Context highlighting.

(defvar lisp-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of Lisp special form names.")

;; FIX maybe merge w indent definitions above?

(setf (gethash "let" lisp-special-forms) t)
(setf (gethash "let*" lisp-special-forms) t)
(setf (gethash "letf" lisp-special-forms) t)
(setf (gethash "letf*" lisp-special-forms) t)
(setf (gethash "elet" lisp-special-forms) t)
(setf (gethash "flet" lisp-special-forms) t)
(setf (gethash "macrolet" lisp-special-forms) t)

(setf (gethash "block" lisp-special-forms) t)
(setf (gethash "progn" lisp-special-forms) t)
(setf (gethash "progv" lisp-special-forms) t)
(setf (gethash "prog" lisp-special-forms) t)
(setf (gethash "prog1" lisp-special-forms) t)
(setf (gethash "prog2" lisp-special-forms) t)
(setf (gethash "prog*" lisp-special-forms) t)

(setf (gethash "return" lisp-special-forms) t)
(setf (gethash "return-from" lisp-special-forms) t)
(setf (gethash "go" lisp-special-forms) t)
(setf (gethash "if" lisp-special-forms) t)
(setf (gethash "fi" lisp-special-forms) t)
(setf (gethash "fi*" lisp-special-forms) t)
(setf (gethash "or" lisp-special-forms) t)
(setf (gethash "and" lisp-special-forms) t)
(setf (gethash "do" lisp-special-forms) t)
(setf (gethash "do*" lisp-special-forms) t)
(setf (gethash "until" lisp-special-forms) t)
(setf (gethash "until*" lisp-special-forms) t)
(setf (gethash "while" lisp-special-forms) t)
(setf (gethash "while*" lisp-special-forms) t)
(setf (gethash "iterate" lisp-special-forms) t)
(setf (gethash "labels" lisp-special-forms) t)
(setf (gethash "lambda" lisp-special-forms) t)
(setf (gethash "dotimes" lisp-special-forms) t)
(setf (gethash "dolist" lisp-special-forms) t)
(setf (gethash "dohash" lisp-special-forms) t)
(setf (gethash "loop" lisp-special-forms) t)
(setf (gethash "when" lisp-special-forms) t)
(setf (gethash "cond" lisp-special-forms) t)
(setf (gethash "unless" lisp-special-forms) t)
(setf (gethash "case" lisp-special-forms) t)
(setf (gethash "case=" lisp-special-forms) t)
(setf (gethash "ecase" lisp-special-forms) t)
(setf (gethash "ecase=" lisp-special-forms) t)
(setf (gethash "collect" lisp-special-forms) t)

(setf (gethash "typecase" lisp-special-forms) t)
(setf (gethash "ctypecase" lisp-special-forms) t)
(setf (gethash "etypecase" lisp-special-forms) t)

(setf (gethash "declare" lisp-special-forms) t)
(setf (gethash "declaim" lisp-special-forms) t)
(setf (gethash "defcommand" lisp-special-forms) t)
(setf (gethash "defconstant" lisp-special-forms) t)
(setf (gethash "defevar" lisp-special-forms) t)
(setf (gethash "defmacro" lisp-special-forms) t)
(setf (gethash "defmode" lisp-special-forms) t)
(setf (gethash "defpackage" lisp-special-forms) t)
(setf (gethash "defparameter" lisp-special-forms) t)
(setf (gethash "defprinter" lisp-special-forms) t)
(setf (gethash "defsetf" lisp-special-forms) t)
(setf (gethash "defstruct" lisp-special-forms) t)
(setf (gethash "deftarget" lisp-special-forms) t)
(setf (gethash "deftype" lisp-special-forms) t)
(setf (gethash "deftest" lisp-special-forms) t)
(setf (gethash "defun" lisp-special-forms) t)
(setf (gethash "defvar" lisp-special-forms) t)
(setf (gethash "proclaim" lisp-special-forms) t)

(declaim (inline highlight-lisp-line))
(declaim (special *context*))

;; FIX #\;
(defun highlight-lisp-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line))
	  (pos 0))
      (case *context*
	(:string
	 (chi-mark line 0 *string-font* :string chi-info)
	 (setq pos (1+ (or (search-for-qmark chars)
			   (return-from highlight-lisp-line))))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ()))
	(:comment
	 (chi-mark line 0 *comment-font* :comment chi-info)
	 (setq pos (+ (or (search "|#" chars)
			  (return-from highlight-lisp-line))
		      2))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ()))
	(:doc
	 (chi-mark line 0 *preprocessor-font*
		   :preprocessor chi-info)
	 (setq pos (+ (or (search "]#" chars)
			  (return-from highlight-lisp-line))
		      2))
	 (chi-mark line pos *original-font* :window-foreground chi-info)
	 (setq *context* ())))
      (loop
	(let ((string (or (search-for-qmark chars pos)
			  most-positive-fixnum))
	      (comment (or (let ((co (position #\; chars :start pos))
				 (ex (search "#!" chars :start2 pos)))
			     (if co
				 (if ex (min co ex) co)
				 ex))
			   most-positive-fixnum))
	      (multic (or (search "#|" chars :start2 pos)
			  most-positive-fixnum))
	      (doc (or (search "#[" chars :start2 pos)
		       most-positive-fixnum))
	      (oparen (or (position #\( chars :start pos)
			  most-positive-fixnum)))
	  (cond ((< string (min comment multic doc oparen))
		 (chi-mark line string *string-font*
			   :string chi-info)
		 (setq pos (search-for-qmark chars (1+ string)))
		 (if pos
		     (chi-mark line (incf pos) *original-font*
			       :window-foreground chi-info)
		     (progn
		       (setq *context* :string)
		       (return-from highlight-lisp-line))))

		((< comment (min string multic doc oparen))
		 (chi-mark line comment *comment-font*
			   :comment chi-info)
		 (return-from highlight-lisp-line))

		((< multic (min comment string doc oparen))
		 (chi-mark line multic *comment-font* :comment
			   chi-info)
		 (setq pos (search "|#" chars :start2 (+ multic 2)))
		 (if pos
		     (chi-mark line (incf pos 2) *original-font*
			       :window-foreground chi-info)
		     (progn
		       (setq *context* :comment)
		       (return-from highlight-lisp-line))))

		((< doc (min comment string multic oparen))
		 (chi-mark line doc *preprocessor-font*
			   :preprocessor chi-info)
		 (setq pos (search "]#" chars :start2 (+ doc 2)))
		 (if pos
		     (chi-mark line (incf pos 2) *original-font*
			       :window-foreground chi-info)
		     (progn
		       (setq *context* :doc)
		       (return-from highlight-lisp-line))))

		((< oparen (min multic comment string doc))
		 ;; FIX (return-from-many)
		 ;; FIX (cond ((fi p)
		 (if (>= oparen (1- (line-length line)))
		     (return-from highlight-lisp-line)
		     (let ((mark (mark line (1+ oparen))))
		       (if (member (next-character mark) '(#\" #\\))
			   (setq pos (1+ oparen))
			   (progn
			     (find-attribute mark :word-delimiter)
			     (or (memq (next-character mark)
				       '(#\  #\) #\newline))
				 (if (mark-after mark)
				     (find-attribute mark :word-delimiter)))
			     (setq pos (mark-charpos mark))
			     ;; This also highlights special form names in lists.
			     (when (gethash (subseq chars (1+ oparen) pos)
					    lisp-special-forms)
			       ;; FIX Highlight next word if last was defun, defvar, def...
			       (chi-mark line
					 (1+ oparen)
					 *special-form-font*
					 :special-form
					 chi-info)
			       (chi-mark line
					 pos
					 *original-font*
					 :window-foreground
					 chi-info)))))))

		(t
		 (return-from highlight-lisp-line))))))))

;; FIX maybe this should use find-pattern, or [a mod of] lispmode parsing
;; FIX can (,;,#|," be found using char attributes?  (,;,"  #| is a string
;; FIX (let (;; comment  (highlight first ;)
;; FIX highlight &optional...

(defun highlight-lisp-buffer (buffer)
  (highlight-chi-buffer buffer highlight-lisp-line))

(defun highlight-visible-lisp-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-lisp-line))
