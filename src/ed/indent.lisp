;;; Indentation commands.

(in-package "ED")

(export '(delete-horizontal-space indent-region indent-region-for-commands
          maybe-delete-horizontal-space))

#[ Indenting Text

{evariable:Indent Function}
{evariable:Tab Indenter}
{evariable:Space Indenter}
{evariable:Indent With Tabs}
{evariable:Spaces per Tab}
{function:ed:indent-region}
{function:ed:indent-region-for-commands}
{function:ed:delete-horizontal-space}
]#

(defun tab-to-tab-stop (mark)
  (insert-character mark #\tab))

(defevar "Indent with Tabs"
  "Whether `indent' indents with tabs or spaces."
  :value t)

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'tab-to-tab-stop)

(defun indent (mark length)
  "Indent the text by $length characters from $mark, heeding *Indent with
   Tabs*."
  (funcall (if (value indent-with-tabs)
	       (value tab-indenter)
	       (value space-indenter))
	   mark length))

(defun update-spaces-per-tab (&optional name kind where value)
  "Update display to a new Value of *Spaces per Tab*."
  (declare (ignore name kind where value))
  ;; FIX change only takes effect when buffer is switched
  (when edi::*in-the-editor*
    (setq edi::*screen-image-trashed* t)
    (redisplay-all)))

(defevar "Spaces per Tab"
  "The number of spaces a tab is equivalent to."
  :value 8
  :hooks '(update-spaces-per-tab))

(defun indent-using-tabs (mark column)
  "Inserts at MARK a maximum number of tabs and a minimum number of spaces to
   move mark to COLUMN.  This assumes MARK is at the beginning of a line."
  (multiple-value-bind (tabs spaces) (floor column (value spaces-per-tab))
    (dotimes (i tabs) (insert-character mark #\tab))
    (dotimes (i spaces) (insert-character mark #\space))))

(defevar "Tab Indenter"
  "A function that takes a mark and a number of columns.  The function
   inserts a maximum number of tabs and a minimum number of spaces at the
   mark to move the mark the specified number of columns."
  :value #'indent-using-tabs)

(defun indent-using-spaces (mark column)
  "Insert spaces at $mark to move mark to $column.  Assume $mark is at the
   beginning of a line."
  (dotimes (i column) (insert-character mark #\space)))

(defevar "Space Indenter"
  "A function that takes a mark and a number of columns.  The function
   inserts spaces at the mark mark to move the mark the specified number of
   columns."
  :value #'indent-using-spaces)

(defun indent-region-for-commands (region)
  "Invoke the function in *Indent Function* on every line in $region."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (undo-region (copy-region (region (line-start start) (line-end end)))))
    (indent-region region)
    (make-region-undo :twiddle "Indent"
		      (region (line-start (copy-mark start :left-inserting))
			      (line-end (copy-mark end :right-inserting)))
		      undo-region)))

(defun indent-region (region)
  "Invoke the function in *Indent Function* on every line of $region."
  (let ((indent-function (value indent-function)))
    (with-mark ((start (region-start region) :left-inserting)
		(end (region-end region)))
      (line-start start)
      (line-start end)
      (loop
	(when (mark= start end)
	  (funcall indent-function start)
	  (return))
	(funcall indent-function start)
	(line-offset start 1 0)))))

(defun generic-indent (mark)
  (let* ((line (mark-line mark))
	 (prev (while ((line (line-previous line) (line-previous line)))
		      ((and line (blank-line-p line)) line))))
    (or prev (editor-error "First line with text."))
    (line-start mark prev)
    (find-attribute mark :space #'zerop)
    (let ((indentation (mark-column mark)))
      (line-start mark line)
      (delete-horizontal-space mark)
      (indent mark indentation))))

(defattribute "Space"
  "This attribute is used by the indentation commands to determine which
  characters are treated as space."
  '(mod 2) 0)

(setf (character-attribute :space #\space) 1)
(setf (character-attribute :space #\tab) 1)

(defun count-horizontal-space (mark)
  "Return the number of characters with a :space attribute of 1 (as in
   [System Defined Character Attributes]) on either side of $mark."
  (with-mark ((start mark))
    (reverse-find-attribute start :space #'zerop)
    (find-attribute mark :space #'zerop)
    (count-characters (region start mark))))

(defun delete-horizontal-space (mark)
  "Delete all characters with a :space attribute of 1 (as in [System
   Defined Character Attributes]) on either side of $mark."
  (with-mark ((start mark))
    (reverse-find-attribute start :space #'zerop)
    (find-attribute mark :space #'zerop)
    (delete-region (region start mark))))

(defun maybe-delete-horizontal-space (mark length)
  "If there are $length characters with a :space attribute of 1 (as in
   [System Defined Character Attributes]) on either side of $mark then
   return (), otherwise delete all those space characters and return t."
  (with-mark ((start mark))
    (reverse-find-attribute start :space #'zerop)
    (find-attribute mark :space #'zerop)
    (fi* (= (- (mark-column mark) (mark-column start)) length)
      (delete-region (region start mark))
      t)))

#[ Indentation

Nearly all programming languages have conventions for indentation or
leading whitespace at the beginning of lines.  The editor indentation
facility is integrated into the command set so that it interacts well with
other features such as filling and commenting.

{evariable:Indent Function}
{command:Indent}
{command:Indent New Line}
{command:Indent Region}
{command:Back to Indentation}
{command:Delete Indentation}
{command:Quote Tab}
{command:Indent Rigidly}
{command:Center Line}
{evariable:Indent with Tabs}
{evariable:Spaces per Tab}
]#

(defcommand "Indent New Line" (p)
  "Start a new indented line.  Flush any whitespace before the point and
   inserts indentation on a blank line.  The effect of this is similar to
   Return followed by Tab.  The prefix argument is passed to `New Line',
   which is used to insert the blank line.  If there is a Fill Prefix then
   it is used for indentation, otherwise, the function in \"Indent
   Function\" is called to do the indenting."
  (let ((point (current-point))
	(prefix (value fill-prefix)))
    (delete-horizontal-space point)
    (new-line-command p)
    (if prefix
	(insert-string point prefix)
	(funcall (value indent-function) point))))

(defcommand "Indent" (p)
  "Invoke the function held in the editor variable *Indent Function*,
   moving point past the region if the prefix is set."
  (let ((point (current-point)))
    (with-mark ((mark point :left-inserting))
      (cond ((or (not p) (zerop p))
	     (funcall (value indent-function) mark))
	    (t
	     (if (plusp p)
		 (or (line-offset point (1- p))
		     (buffer-end point))
		 (or (line-offset mark (1+ p))
		     (buffer-start mark)))
	     (indent-region-for-commands (region mark point))
	     (find-attribute (line-start point) :whitespace #'zerop))))))

(defcommand "Indent Region" ()
  "Invokes function held by editor variable *Indent Function* on every line
   between point and mark, inclusively."
  (let ((region (current-region)))
    (with-mark ((start (region-start region) :left-inserting)
		(end (region-end region) :left-inserting))
      (indent-region-for-commands (region start end)))))

(defcommand "Center Line" (p)
  "Indent the current line so that it is centered between the left margin
   and Fill Column.  With a prefix use the prefix as the width instead of
   `Fill Column'."
  (let* ((region (if (region-active-p)
		     (current-region)
		     (region (current-point) (current-point))))
	 (end (region-end region)))
    (with-mark ((temp (region-start region) :left-inserting))
      (loop
	(when (mark> temp end) (return))
	(delete-horizontal-space (line-end temp))
	(delete-horizontal-space (line-start temp))
	(let* ((len (line-length (mark-line temp)))
	       (spaces (- (or p (value fill-column)) len)))
	  (if (and (plusp spaces)
		   (not (zerop len)))
	      (indent temp (ceiling spaces 2)))
	  (or (line-offset temp 1) (return))
	  (line-start temp))))))

(defcommand "Quote Tab" (p)
  "Insert a tab character."
  (if (and p (> p 1))
      (insert-string (current-point) (make-string p :initial-element #\tab))
      (insert-character (current-point) #\tab)))

(defcommand "Open Line" (p)
  "Insert a newline into the current buffer, leaving point in place.  With
   a prefix argument, insert that many newlines."
  "Insert a newline into the current buffer, leaving point in place.  If $p
   is true then insert that many newlines."
  (let ((point (current-point))
	(count (if p p 1)))
    (if (minusp count)
	(editor-error "Negative argument given to `Open Line'.")
	(dotimes (i count)
	  (insert-character point #\newline)
	  (mark-before point)))))

(defcommand "New Line" (p)
  "Moves the point to a new blank line.  A newline is inserted if either of
   the next two lines contain text.  With an argument, repeats p times."
  "Moves the point to a new blank line."
  (let ((point (current-point))
	(count (if p p 1)))
    (if (minusp count)
	(editor-error "Negative argument given to `New Line'.")
	(do* ((next (line-next (mark-line point))
		    (line-next (mark-line point)))
	      (i 1 (1+ i)))
	     ((> i count))
	  (cond ((and (blank-after-p point)
		      next (blank-line-p next)
		      (let ((after (line-next next)))
			(or (not after) (blank-line-p after))))
		 (line-start point next)
		 (let ((len (line-length next)))
		   (unless (zerop len)
		     (delete-characters point len))))
		(t
		 (insert-character point #\newline)))))))


#[ Whitespace Manipulation

These commands change the amount of space between words.  See also the
indentation commands in section [ indentation ].

{command:Just One Space}
{command:Delete Horizontal Space}
{command:Delete Blank Lines}
]#

(defcommand "Tabs to Spaces" (p)
  "Convert tabs to spaces.  With a prefix insert prefix spaces per tab,
   else insert *Spaces per Tab* spaces per tab."
  "Convert tabs to spaces.  If $p is true insert $p spaces per tab, else
   insert *Spaces per Tab* spaces per tab."
  (let ((p (or p (value spaces-per-tab)))
	(region (current-region)))
    (with-mark ((start (region-start region) :left-inserting)
		(end (region-end region) :left-inserting))
      (loop until (mark= start end) do
	(if (eq (next-character start) #\Tab)
	    (let* ((pos (mark-charpos start))
		   (num (- p (mod pos p))))
	      (delete-characters start)
	      (dotimes (x num)
		(insert-character start #\ )))
	    (mark-after start))))))

(defcommand "Delete Indentation" (p)
  "Join current line with the previous one, flushing any excess whitespace.
   Usually replace all whitespace with a single space. At the beginning of
   a line, immmediately following a \"(\", or immediately preceding a
   \")\", flush all whitespace.  If the preceeding character is a sentence
   terminator, leave two spaces instead of one.  If a prefix argument is
   given, join the following line with the current line."
  (with-mark ((m (current-point) :right-inserting))
    (when p (line-offset m 1))
    (line-start m)
    (unless (delete-characters m -1) (editor-error "No previous line."))
    (delete-horizontal-space m)
    (let ((prev (previous-character m)))
      (when (and prev (char/= prev #\newline))
	(cond ((not (zerop (character-attribute :sentence-terminator prev)))
	       (insert-string m "  "))
	      ((not (or (eq (character-attribute :lisp-syntax prev) :open-paren)
			(eq (character-attribute :lisp-syntax (next-character m))
			    :close-paren)))
	       (insert-character m #\space)))))))

(defcommand "Delete Horizontal Space" ()
  "Flush all spaces and tabs surrounding the point."
  (delete-horizontal-space (current-point)))

(defcommand "Just One Space" (p)
  "Flush all whitespace characters before and after the point and then
   insert one space.  With a prefix argument, insert that number of
   spaces."
  "Flush surrounding space and insert $p spaces, or 1 space if $p is ().."
  (let ((point (current-point)))
    (delete-horizontal-space point)
    (dotimes (i (or p 1)) (insert-character point #\space))))

(defcommand "Back to Indentation" ()
  "Move point past all leading whitespace characters on the current line."
  (let ((point (current-point)))
    (line-start point)
    (find-attribute point :whitespace #'zerop)))

(defcommand "Indent Rigidly" (p)
  "Indent the region rigidly by $p spaces.  Move each line in the region $p
   spaces to the right (left if $p is negative).  When moving a line to the
   left, convert tabs to spaces."
  (let ((p (or p 1))
	(region (current-region)))
    (with-mark ((mark1 (region-start region) :left-inserting)
		(mark2 (region-end region) :left-inserting))
      (line-start mark1)
      (line-start mark2)
      (do ()
	  ((mark= mark1 mark2))
	(cond ((empty-line-p mark1))
	      ((blank-after-p mark1)
	       (delete-characters mark1 (line-length (mark-line mark1))))
	      (t (find-attribute mark1 :whitespace #'zerop)
		 (let ((new-column (+ p (mark-column mark1))))
		   (delete-characters mark1 (- (mark-charpos mark1)))
		   (if (plusp new-column)
		       (indent mark1 new-column)))))
	(line-offset mark1 1 0)))))
