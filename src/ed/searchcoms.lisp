;;; Searching and replacing commands.

(in-package "ED")

(export '(*last-search-pattern* *last-search-string* get-search-pattern))


;;;; Some global state.

(defvar *last-search-string* ()
  "The last string searched for, as set by `get-search-pattern'.")
(defvar *last-search-switched* ()
  "Whether the last search switched to a case sensitive search.")
(defvar *last-search-pattern*
  (new-search-pattern :string-insensitive :forward "Foo")
  "Search pattern shared between the searching commands.")

#| FIX
(defvar *last-rule-search-rule* () "Rule from last search.")
(defvar *last-rule-search-pattern*
  (new-search-pattern :parser :forward '("Foo"))
  "Cached rule search pattern.")
|#

(defevar "String Search Fold Case"
  "When t, string searching compares upper and lower case as equal.
   When :initially searching switches to case sensitive as soon as an
   uppercase character is entered."
  :value t)

(defun get-search-pattern (string direction)
  "Return a search string and pattern that search and replacing commands
   can use.

   The search and replace commands share the pattern returned by
   `get-search-pattern', and save on creating a search pattern each time
   they execute."
  (declare (simple-string string))
  (if (zerop (length string))
      (editor-error "Attempt to search for empty string."))
  (setq *last-search-string* string)
  (setq *last-search-pattern*
	(new-search-pattern (if (value string-search-fold-case)
				:string-insensitive
				:string-sensitive)
			    direction string *last-search-pattern*)))

#| FIX
(defun get-rule-search-pattern (rule direction)
  (or rule (editor-error "Attempt to search by an empty rule."))
  (setq *last-rule-search-rule* rule)
  (setq *last-rule-search-pattern*
	(new-search-pattern :parser direction rule
			    *last-rule-search-pattern*)))
|#


;;;; Once-off searching.

(defcommand "Forward Search" (p string)
  "Search forward for a string.  On success push the starting position
   (before the search) onto the mark stack.  If the current region is
   active then forego pushing the mark."
  (declare (ignore p))
  (or string
      (setq string (prompt-for-string :prompt "Search: "
				      :default *last-search-string*
				      :help "String to search for")))
  (let* ((pattern (get-search-pattern string :forward))
	 (point (current-point))
	 (mark (copy-mark point))
	 (won (find-pattern point pattern)))
    (cond (won (character-offset point won)
	       (if (region-active-p)
		   (delete-mark mark)
		   (push-buffer-mark mark)))
	  (t (delete-mark mark)
	     (editor-error "Search reached end of buffer." string)))))

(defcommand "Reverse Search" (p string)
  "Search backward for a string.  On success push the starting position
   (before the search) onto the mark stack.  If the current region is
   active then forego pushing the mark."
  (declare (ignore p))
  (or string
      (setq string (prompt-for-string :prompt "Reverse Search: "
				      :default *last-search-string*
				      :help "String to search for")))
  (let* ((pattern (get-search-pattern string :backward))
	 (point (current-point))
	 (mark (copy-mark point))
	 (won (find-pattern point pattern)))
    (cond (won (if (region-active-p)
		   (delete-mark mark)
		   (push-buffer-mark mark)))
	  (t (delete-mark mark)
	     (editor-error "Search reached start of buffer.")))))

(defun read-list-from-string (string)
  (let ((list ())
	(offset 0)
	(string-len (length string)))
    (loop
      (multiple-value-bind
	  (item off)
	  (read-from-string string nil nil :start offset)
	(or item (return-from nil (nreverse list)))
;; Towards   ~BNF Isearch: (a :c d .)(m ab\\f)b(o c)
; 	(if (symbolp item)
; 	    (or (eq (symbol-package item) *keyword-package*)
; 		(setq item
; 		      (subseq string
; 			      (- off (length (symbol-name item)))
; 			      off))
; 		(if (eq (length item) 1)
; 		    (setq item (aref item 0)))))
	(setq offset off)
	(push item list)
	(if (>= offset string-len)
	    (return-from nil (nreverse list)))))))

#|
(defcommand "Forward Rule Search" (p rule)
  "Do a forward search for a match to a parser rule.
   Prompt for the rule and leave the point after the match."
  "Searches the current buffer for a match to the specified Rule."
  (declare (ignore p))
  (or rule
      (setq rule (prompt-for-string :prompt "Rule Search: "
				    ;; FIX this prints the surrounding parens
				    :default (format () "~S" *last-rule-search-rule*)
				    :help "Rule for which to find a match.")))
  (let* ((pattern (get-rule-search-pattern (cons 'group
						 (read-list-from-string rule))
					   :forward))
	 (point (current-point))
	 (mark (copy-mark point))
	 (len (find-pattern point pattern)))
    (cond (len (character-offset point len)
	       (if (region-active-p)
		   (delete-mark mark)
		   (push-buffer-mark mark)))
	  (t (delete-mark mark)
	     (editor-error "Search reached end of buffer.")))))

(defcommand "Reverse Rule Search" (p rule)
  "Do a forward search for a match to a parser rule.
   Prompt for the rule and leave the point after the match."
  "Searches the current buffer for a match to the specified Rule."
  (declare (ignore p))
  (or rule
      (setq rule (prompt-for-expression :prompt "Reverse Rule Search: "
					:default *last-rule-search-rule*
					:help "Rule for which to find a match.")))
  (let* ((pattern (get-rule-search-pattern rule :backward))
	 (point (current-point))
	 (mark (copy-mark point))
	 (won (find-pattern point pattern)))
    (cond (won (if (region-active-p)
		   (delete-mark mark)
		   (push-buffer-mark mark)))
	  (t (delete-mark mark)
	     (editor-error "Search reached start of buffer.")))))
|#


#[ Searching and Replacing

Searching for some string known to appear in the text is a commonly used
method of moving long distances in a file.  Replacing occurrences of one
pattern with another is a useful way to make many simple changes to text.
The editor provides commands for doing both of these operations.

{evariable:String Search Fold Case}

The incremental search commands searche for an occurrence of a string after
the current point.  They are known as incremental searches because they
read key-events from the keyboard one at a time and immediately search for
the pattern of corresponding characters.  This is useful because it is
possible to initially type in a very short pattern and then add more
characters if it turns out that this pattern has too many spurious matches.

{command:Incremental Search}
{command:Reverse Incremental Search}
{command:Forward Search}
{command:Reverse Search}

One reason for using the full string searches `Forward Search' and `Reverse
Search' is that it may be faster since it is possible to specify a long
search string from the very start.  Since the search is a Boyer--Moore
search algorithm, the speed of the search increases with the size of the
search string.

{command:Forward Rule Search}
{command:Reverse Rule Search}
{command:Query Replace}
{evariable:Case Replace}
{command:Replace String}
{command:List Matching Lines}
{command:Flush Lines}
{command:Flush Other Lines}
]#


;;;; Incremental searching.

(defun i-search-pattern (string direction)
  (setq *last-search-pattern*
	(new-search-pattern (if (value string-search-fold-case)
				:string-insensitive
				:string-sensitive)
			    direction string *last-search-pattern*)))

;;; %I-SEARCH-ECHO-REFRESH refreshes the echo buffer for incremental
;;; search.
;;;
(defun %i-search-echo-refresh (string direction failure)
  (when (interactive)
    (clear-echo-area)
    (format *echo-area-stream*
	    "~:[~;Failing ~]~:[Reverse I-Search~;I-Search~]: ~A"
	    failure (eq direction :forward) string)))

(defcommand "Incremental Search" ()
  "Search, initially forward, for an input string according to successively
   prompted characters.

   Dispatch on the following key-events as sub-commands:

     C-s
	Search forward for an occurrence of the current pattern.  This can
	be used repeatedly to skip from one occurrence of the pattern to
	the next, or it can be used to change the direction of the search
	if it is currently a reverse search.  If C-s is typed when the
	search string is empty, then a search is done for the string that
	was used by the last searching command.

     C-r
	Similar to C-s, only search backwards.

     Delete, Backspace
	If the last key-event simply added to the search pattern, then
	remove the key-event character from the pattern, moving back to the
	last match found before entering the removed character.  If the
	character was a C-s or C-r, then move back to the previous match
	and possibly reverse the search direction.

     C-g

	If the search is currently failing, meaning that there is no
	occurrence of the search pattern in the direction of search, then
	remove enough characters from the end of the pattern to make it
	successful.  If the search is currently successful, then exit the
	search, leaving the point where it was when the search started.
	Exiting the search forfeits the saving of the current search
	pattern as the last search string.

     Escape
	Exit at the current position in the text.  If the search string
	is empty, enter a non-incremental string search instead.

     C-q

	Search for the character corresponding to the next key-event,
	rather than treating it as a command.

   For example if C-a is given then exit the search and go to the beginning
   of the current line.  When any of the commands successfully exit, push
   the starting position (before the search) on the mark stack.  If the
   current region was active when the search started, forego pushing a
   mark."
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" :forward nil)
  (let* ((point (current-point))
	 (save-start (copy-mark point :temporary))
	 (previous-lss *last-search-pattern*))
    (with-mark ((here point))
      (handler-case
	    (when (eq (catch 'exit-i-search
			(%i-search "" point here :forward nil))
		      :control-g)
	      (setq *last-search-switched* previous-lss)
	      (move-mark point save-start)
	      (invoke-hook abort-hook)
	      (editor-error "Search exited."))
	;; Now signalled in handler of C-g sigint.
	(edi::editor-top-level-catcher ()
	  (setq *last-search-switched* previous-lss)
	  (move-mark point save-start)))
      (if (region-active-p)
	  (delete-mark save-start)
	  (push-buffer-mark save-start)))))

(defcommand "Reverse Incremental Search" ()
  "Search, initially backwards, for an input string according to
   successively prompted characters.

   Dispatch on the following key-events as sub-commands:

     C-s
	Search forward for an occurrence of the current pattern.  This can
	be used repeatedly to skip from one occurrence of the pattern to
	the next, or it can be used to change the direction of the search
	if it is currently a reverse search.  If C-s is typed when the
	search string is empty, then a search is done for the string that
	was used by the last searching command.

     C-r
	Similar to C-s, only search backwards.

     Delete, Backspace
	If the last key-event simply added to the search pattern, then
	remove the key-event character from the pattern, moving back to the
	last match found before entering the removed character.  If the
	character was a C-s or C-r, then move back to the previous match
	and possibly reverse the search direction.

     C-g

	If the search is currently failing, meaning that there is no
	occurrence of the search pattern in the direction of search, then
	remove enough characters from the end of the pattern to make it
	successful.  If the search is currently successful, then exit the
	search, leaving the point where it was when the search started.
	Exiting the search forfeits the saving of the current search
	pattern as the last search string.

     Escape
	Exit at the current position in the text.  If the search string
	is empty, enter a non-incremental string search instead.

     C-q

	Search for the character corresponding to the next key-event,
	rather than treating it as a command.

   For example if C-a is given then exit the search and go to the beginning
   of the current line.  When any of the commands successfully exit, push
   the starting position (before the search) on the mark stack.  If the
   current region was active when the search started, forego pushing a
   mark."
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" :backward nil)
  (let* ((point (current-point))
	 (save-start (copy-mark point :temporary))
	 (previous-lss *last-search-switched*))
    (with-mark ((here point))
      (handler-case
	  (when (eq (catch 'exit-i-search
		      (%i-search "" point here :backward nil))
		    :control-g)
	    (setq *last-search-switched* previous-lss)
	    (move-mark point save-start)
	    (invoke-hook abort-hook)
	    (editor-error "Search exited."))
	;; Now signalled in handler of C-g sigint.
	(edi::editor-top-level-catcher ()
	  (setq *last-search-switched* previous-lss)
	  (move-mark point save-start)))
      (if (region-active-p)
	  (delete-mark save-start)
	  (push-buffer-mark save-start)))))

;;; %I-SEARCH recursively (with support functions) searches to provide
;;; incremental searching.  There is a loop in case the recursion is ever
;;; unwound to some call.  curr-point must be saved since point is
;;; clobbered with each recursive call, and the point must be moved back
;;; before a different letter may be typed at a given call.  In the CASE at
;;; :cancel and :control-g, if the string is not null, an accurate pattern
;;; for this call must be provided when %I-SEARCH-CHAR-EVAL is called a
;;; second time since it is possible for ^S or ^R to be typed.
;;;
(defun %i-search (string point trailer direction failure)
  (elet ((string-search-fold-case (value string-search-fold-case)))
    (do* ((curr-point (copy-mark point :temporary))
	  (curr-trailer (copy-mark trailer :temporary)))
	 (nil)
      (let ((next-key-event (get-key-event *editor-input* t)))
	(case (%i-search-char-eval next-key-event string point trailer
				   direction failure)
	  (:cancel
	   (%i-search-echo-refresh string direction failure)
	   (unless (zerop (length string))
	     (i-search-pattern string direction)))
	  (:return-cancel
	   (unless (zerop (length string)) (return :cancel))
	   (beep))
	  (:control-g
	   (when failure (return :control-g))
	   (%i-search-echo-refresh string direction nil)
	   (unless (zerop (length string))
	     (i-search-pattern string direction))))
	(move-mark point curr-point)
	(move-mark trailer curr-trailer)))))

;;; %I-SEARCH-CHAR-EVAL evaluates the last character typed and takes
;;; necessary actions.
;;;
(defun %i-search-char-eval (key-event string point trailer direction failure)
  (declare (simple-string string))
  (cond ((let ((character (key-event-char key-event)))
	   (and character (standard-char-p character)))
	 (setq *last-search-switched* ())
	 (%i-search-printed-char key-event string point trailer
				 direction failure))
	((or (logical-key-event-p key-event :forward-search)
	     (logical-key-event-p key-event :backward-search))
	 (%i-search-control-s-or-r key-event string point trailer
				   direction failure))
	((logical-key-event-p key-event :cancel) :return-cancel)
	((logical-key-event-p key-event :abort)
	 (unless failure
	   (clear-echo-area)
	   (message "Search aborted.")
	   (throw 'exit-i-search :control-g))
	 :control-g)
	((logical-key-event-p key-event :quote)
	 (setq *last-search-switched* ())
	 (%i-search-printed-char (get-key-event *editor-input* t)
				 string point trailer direction failure))
	((equalp key-event #k"C-w")
	 (setq *last-search-switched* ())
	 (%i-search-copy-word string point trailer direction failure))
	((and (zerop (length string)) (logical-key-event-p key-event :exit))
	 (if (eq direction :forward)
	     (forward-search-command)
	     (reverse-search-command))
	 (throw 'exit-i-search nil))
	(t
	 (unless (logical-key-event-p key-event :exit)
	   (unget-key-event key-event *editor-input*))
	 (unless (zerop (length string))
	   (setf *last-search-string* string))
	 (throw 'exit-i-search nil))))

;;; %I-SEARCH-COPY-WORD handles the "take the next word of the current
;;; match" case.

(defun %i-search-copy-word (string point trailer direction failure)
  ;; Begin by finding the region starting at the end of the current search
  ;; and ending after the next word.
  (let ((word-region (%i-search-region-to-copy string point direction)))
    (when word-region
      (let* ((new-fragment (region-to-string word-region))
	     (new-string (concatenate 'simple-string
				      string new-fragment)))
	;; Update the status message.
	(when (interactive)
	  (insert-string (buffer-point *echo-area-buffer*) new-fragment)
	  (force-output *echo-area-stream*))
	(i-search-pattern new-string direction)
	(cond (failure (%i-search new-string point
				  trailer direction failure))
	      (t
	       (when (eq direction :backward)
		 (move-mark trailer (region-end word-region)))
	       (%i-search-find-pattern new-string point trailer direction)))))))

(defun %i-search-region-to-copy (string point direction)
  "Find the region containing the rest of the word to copy."
  (let ((start-mark (copy-mark point :temporary)))
    ;; When going backwards, the point is at the beginning of the search
    ;; string, so move start-mark to the end.
    (when (eq direction :backward)
      (character-offset start-mark (length string)))
    (let* ((end-mark (copy-mark start-mark :temporary)))
      ;; Advance end-mark to the end of the word.
      (when (and (find-attribute end-mark :word-delimiter #'zerop)
		 (find-attribute (mark-after end-mark) :word-delimiter))
	(region start-mark end-mark)))))

;;; %I-SEARCH-CONTROL-S-OR-R handles repetitions in the search.  Note that
;;; there cannot be failure in the last COND branch: since the direction
;;; has just been changed, there cannot be a failure before trying a new
;;; direction.
;;;
(defun %i-search-control-s-or-r (key-event string point trailer
					   direction failure)
  (let ((forward-direction-p (eq direction :forward))
	(forward-character-p (logical-key-event-p key-event :forward-search)))
    (cond ((zerop (length string))
	   (%i-search-empty-string point trailer direction forward-direction-p
				   forward-character-p))
	  ((eq forward-direction-p forward-character-p)
	   (if failure
	       (%i-search string point trailer direction failure)
	       (%i-search-find-pattern string point (move-mark trailer point)
				       direction)))
	  (t
	   (let ((new-direction (if forward-character-p :forward :backward)))
	     (%i-search-echo-refresh string new-direction nil)
	     (i-search-pattern string new-direction)
	     (%i-search-find-pattern string point (move-mark trailer point)
				     new-direction))))))


;;; %I-SEARCH-EMPTY-STRING handles the empty string case when a ^S or ^R is
;;; typed.  If the direction and character typed do not agree, then merely
;;; switch directions.  If there was a previous string, search for it, else
;;; flash at the guy.
;;;
(defun %i-search-empty-string (point trailer direction forward-direction-p
				     forward-character-p)
  (cond ((eq forward-direction-p (not forward-character-p))
	 (let ((direction (if forward-character-p :forward :backward)))
	   (%i-search-echo-refresh "" direction nil)
	   (%i-search "" point trailer direction nil)))
	(*last-search-string*
	 (elet ((string-search-fold-case (fi *last-search-switched*
					     (value string-search-fold-case))))
	   (%i-search-echo-refresh *last-search-string* direction nil)
	   (i-search-pattern *last-search-string* direction)
	   (%i-search-find-pattern *last-search-string* point trailer direction)))
	(t (beep))))


;;; %I-SEARCH-PRINTED-CHAR handles the case of standard character input.
;;; If the direction is backwards, we have to be careful not to MARK-AFTER
;;; the end of the buffer or to include the next character at the beginning
;;; of the search.
;;;
(defun %i-search-printed-char (key-event string point trailer direction failure)
  (let ((tchar (ext:key-event-char key-event)))
    (unless tchar
      (editor-error "Not a text character -- ~S" (key-event-char
						  key-event)))
    (when (interactive)
      (insert-character (buffer-point *echo-area-buffer*) tchar)
      (force-output *echo-area-stream*))
    (when (and (eq (value string-search-fold-case) :initially)
	       (upper-case-p tchar))
      (setq *last-search-switched* t)
      (setv string-search-fold-case nil))
    (let ((new-string (concatenate 'simple-string string (string tchar))))
      (i-search-pattern new-string direction)
      (cond (failure (%i-search new-string point trailer direction failure))
	    ((and (eq direction :backward) (next-character trailer))
	     (%i-search-find-pattern new-string point (mark-after trailer)
				     direction))
	    (t
	     (%i-search-find-pattern new-string point trailer direction))))))

;;; %I-SEARCH-FIND-PATTERN takes a pattern for a string and direction and
;;; finds it, updating necessary pointers for the next call to %I-SEARCH.
;;; If the search failed, tell the user and do not move any pointers.
;;;
(defun %i-search-find-pattern (string point trailer direction)
  (let ((found-offset (find-pattern trailer *last-search-pattern*)))
    (cond (found-offset
	    (cond ((eq direction :forward)
		   (character-offset (move-mark point trailer) found-offset))
		  (t
		   (move-mark point trailer)
		   (character-offset trailer found-offset)))
	    (%i-search string point trailer direction nil))
	  (t
	   (%i-search-echo-refresh string direction t)
	   (if (interactive)
	       (beep)
	       (editor-error "I-Search failed."))
	   (%i-search string point trailer direction t)))))


;;;; Replacement commands.

(defcommand "Replace String" (p (target (prompt-for-string
					 :prompt "Replace String: "
					 :help "Target string"
					 :default *last-search-string*))
				(replacement (prompt-for-string
					      :prompt "With: "
					      :help "Replacement string")))

  "Prompt for a target and replacement string, and replaces all occurrences
   of the target string following the point.  If a prefix argument is
   specified, then replace only that many occurrences.  When the current
   region is active, use the current region instead of the region from
   point to the end of the buffer."
  (multiple-value-bind (ignore count)
		       (query-replace-function p target replacement
					       "Replace String" t)
    (declare (ignore ignore))
    (message "~D Occurrences replaced." count)))

(defcommand "Query Replace" (p (target (prompt-for-string
					:prompt "Query Replace: "
					:help "Target string"
					:default *last-search-string*))
			       (replacement (prompt-for-string
					     :default ""
					     :prompt "With: "
					     :help "Replacement string")))
  "Interactively replace a string with another.  Prompt in the echo area
   for a target string and a replacement string, then search for an
   occurrence of the target after the point.  On finding a match, prompt
   for a key-event indicating what action to take.  The following are valid
   responses:

     Space, y
	 Replace this occurrence of the target with the replacement string,
	 and search again.

     Delete, Backspace, n
	 Leave this occurrence as it is, continuing the search.

     !
	 Replace this and all remaining occurrences.

     .
	 Replace this occurrence and exit.

      C-r
	 Go into a recursive edit ([Recursive Edits]) at the current
	 location.  Continue from wherever the point is left when the
	 recursive edit exits.  Useful for handling complicated replacement
	 cases.

     Escape
	 Exit immediately.

     Home, C-_, ?, h
	 Print a list of all the options available.

     Any other key-event
         Exit, returning the key-event to the editor input stream for
         interpretation as a normal command.

   When the current region is active, uses the current region instead of
   the region from point to the end of the buffer.  This can be especially
   useful if all remaining occurences are to be replaced (with !).

   If the replacement string is all lowercase, then a heuristic is used
   that attempts to make the case of the replacement the same as that of
   the particular occurrence of the target pattern.  If \"foo\" is being
   replaced with \"bar\" then \"Foo\" is replaced with \"Bar\" and \"FOO\"
   with \"BAR\".

   This command may be reverted with `Undo', but its undoing may not be
   undone.  On a successful exit from this command, the starting position
   (before the search) is pushed on the mark stack."
  (let ((mark (copy-mark (current-point))))
    (multiple-value-bind (ignore count)
			 (query-replace-function p target replacement
						 "Query Replace")
      (declare (ignore ignore))
      (message "~D Occurrences replaced." count))
    (push-buffer-mark mark)))

(defevar "Case Replace"
  "If this is true then \"Query Replace\" tries to preserve case when doing
   replacements, otherwise all replacements are done with the replacement
   string exactly as specified."
  :value t)

(defstruct (replace-undo (:constructor make-replace-undo (mark region)))
  mark
  region)

(setf (documentation 'replace-undo-mark 'function)
      "Return the mark where a replacement was made.")
(setf (documentation 'replace-undo-region 'function)
      "Return region deleted due to replacement.")

(defvar *query-replace-undo-data* nil)

;;; REPLACE-THAT-CASE replaces a string case-sensitively.  Lower, Cap and Upper
;;; are the original, capitalized and uppercase replacement strings.  Mark is a
;;; :left-inserting mark after the text to be replaced.  Length is the length
;;; of the target string.  If dumb, then do a simple replace.  This pushes
;;; an undo information structure into *query-replace-undo-data* which
;;; QUERY-REPLACE-FUNCTION uses.
;;;
(defun replace-that-case (lower cap upper mark length dumb)
  (character-offset mark (- length))
  (let ((insert (cond (dumb lower)
		      ((upper-case-p (next-character mark))
		       (mark-after mark)
		       (prog1 (if (upper-case-p (next-character mark)) upper cap)
			      (mark-before mark)))
		      (t lower))))
    (with-mark ((undo-mark1 mark :left-inserting)
		(undo-mark2 mark :left-inserting))
      (character-offset undo-mark2 length)
      (push (make-replace-undo
	     ;; Save :right-inserting, so the INSERT-STRING at mark below
	     ;; doesn't move the copied mark the past replacement.
	     (copy-mark mark :right-inserting)
	     (delete-and-save-region (region undo-mark1 undo-mark2)))
	    *query-replace-undo-data*))
    (insert-string mark insert)))

;;; QUERY-REPLACE-FUNCTION does the work for the main replacement commands:
;;; "Query Replace", "Replace String", "Group Query Replace", "Group Replace".
;;; Name is the name of the command for undoing purposes.  If doing-all? is
;;; true, this replaces all ocurrences for the non-querying commands.  This
;;; returns t if it completes successfully, and nil if it is aborted.  As a
;;; second value, it returns the number of replacements.
;;;
;;; The undo method, before undo'ing anything, makes all marks :left-inserting.
;;; There's a problem when two replacements are immediately adjacent, such as
;;;    foofoo
;;; replacing "foo" with "bar".  If the marks were still :right-inserting as
;;; REPLACE-THAT-CASE makes them, then undo'ing the first replacement would
;;; bring the two marks together due to the DELETE-CHARACTERS.  Then inserting
;;; the region would move the second replacement's mark to be before the first
;;; replacement.
;;;
(defun query-replace-function (count target replacement name
			       &optional (doing-all? nil))
  (declare (simple-string replacement))
  (let ((replacement-len (length replacement))
	(*query-replace-undo-data* nil))
    (and count (minusp count)
	 (editor-error "Replacement count is negative."))
    (get-search-pattern target :forward)
    (unwind-protect
	(query-replace-loop (get-count-region) (or count -1) target replacement
			    replacement-len (current-point) doing-all?)
      (let ((undo-data (nreverse *query-replace-undo-data*)))
	(save-for-undo name
	  #'(lambda ()
	      (dolist (ele undo-data)
		(setf (mark-kind (replace-undo-mark ele)) :left-inserting))
	      (dolist (ele undo-data)
		(let ((mark (replace-undo-mark ele)))
		  (delete-characters mark replacement-len)
		  (ninsert-region mark (replace-undo-region ele)))))
	  #'(lambda ()
	      (dolist (ele undo-data)
		(delete-mark (replace-undo-mark ele)))))))))

;;; QUERY-REPLACE-LOOP is the essence of QUERY-REPLACE-FUNCTION.  The first
;;; value is whether we completed all replacements, nil if we aborted.  The
;;; second value is how many replacements occurred.
;;;
(defun query-replace-loop (region count target replacement replacement-len
			   point doing-all?)
  (with-mark ((last-found point)
	      ;; Copy REGION-END before moving point to REGION-START in case
	      ;; the end is point.  Also, make it permanent in case we make
	      ;; replacements on the last line containing the end.
	      (stop-mark (region-end region) :left-inserting))
    (move-mark point (region-start region))
    (let ((length (length target))
	  (cap (string-capitalize replacement))
	  (upper (string-upcase replacement))
	  (dumb (not (and (every #'(lambda (ch) (or (not (both-case-p ch))
						    (lower-case-p ch)))
				 (the string replacement))
			  (value case-replace)))))
      (values
       (loop
	 (let ((won (find-pattern point *last-search-pattern*)))
	   (when (or (null won) (zerop count) (mark> point stop-mark))
	     (character-offset (move-mark point last-found) replacement-len)
	     (return t))
	   (decf count)
	   (move-mark last-found point)
	   (character-offset point length)
	   (if doing-all? ; FIX -p
	       (replace-that-case replacement cap upper point length dumb)
	       ;; FIX include c-l
	       (command-case
		 (:prompt "Query replace: "
		    :help "Type one of the following single-character commands:"
		    :change-window nil :bind key-event)
		 (:yes "Replace this occurrence."
		    (replace-that-case replacement cap upper point length
				       dumb))
		 (:no "Don't replace this occurrence, but continue.")
		 (:do-all "Replace this and all remaining occurrences."
			  (replace-that-case replacement cap upper point length
					     dumb)
			  (setq doing-all? t))
		 (:do-once "Replace this occurrence, then exit."
			   (replace-that-case replacement cap upper point length
					      dumb)
			   (return nil))
		 (:recursive-edit
		  "Go into a recursive edit at the current position."
		  (do-recursive-edit)
		  (get-search-pattern target :forward))
		 (:exit "Exit immediately."
			(return nil))
		 (t (unget-key-event key-event *editor-input*)
		    (return nil))))))
       (length (the list *query-replace-undo-data*))))))


;;;; Occurrence searching.

(defcommand "List Matching Lines" (p string)
  "Pop up a window of all the lines after point that contain a prompted
   string.  If a prefix argument is specified, then display that many lines
   before and after each matching line.  When the current region is active,
   use the current region instead of the region from point to the end of
   the buffer."
  (or string
      (setf string (prompt-for-string :prompt "List lines matching: "
				      :default *last-search-string*
				      :help "String to search for")))
  (let ((pattern (get-search-pattern string :forward))
	(matching-lines nil)
	(region (get-count-region)))
    (with-mark ((mark (region-start region))
		(end-mark (region-end region)))
      (loop
	(when (or (null (find-pattern mark pattern)) (mark> mark end-mark))
	  (return))
	(setf matching-lines
	      (nconc matching-lines (list-lines mark (or p 0))))
	(or (line-offset mark 1 0)
	    (return))))
    (with-pop-up-display (s :height (length matching-lines))
      (dolist (line matching-lines)
	(write-line line s)))))

;;; LIST-LINES creates a lists of strings containing (num) lines before the
;;; line that the point is on, the line that the point is on, and (num)
;;; lines after the line that the point is on. If (num) > 0, a string of
;;; dashes will be added to make life easier for List Matching Lines.
;;;
(defun list-lines (mark num)
  (if (<= num 0)
      (list (line-string (mark-line mark)))
      (with-mark ((mark mark)
		  (beg-mark mark))
	(or (line-offset beg-mark (- num)) (buffer-start beg-mark))
	(or (line-offset mark num) (buffer-end mark))
	(let ((lines (list "--------")))
	  (loop
	    (push (line-string (mark-line mark)) lines)
	    (if (same-line-p mark beg-mark) (return lines))
	    (line-offset mark -1))))))

(defcommand "Flush Lines" (p string)
  "Flush all lines that contain a prompted search string. If the current
   region is active, limit the search to that region."
  (declare (ignore p))
  (or string
      (setf string (prompt-for-string :prompt "Flush lines matching: "
				      :default *last-search-string*
				      :help "String to search for")))
  (let* ((region (get-count-region))
	 (pattern (get-search-pattern string :forward))
	 (start-mark (region-start region))
	 (end-mark (region-end region)))
    (with-mark ((bol-mark start-mark :left-inserting)
		(eol-mark start-mark :right-inserting))
      (loop
	(or (and (find-pattern bol-mark pattern) (mark< bol-mark end-mark))
	    (return))
	(move-mark eol-mark bol-mark)
	(line-start bol-mark)
	(or (line-offset eol-mark 1 0)
	    (buffer-end eol-mark))
	(delete-region (region bol-mark eol-mark))))))

(defcommand "Flush Other Lines" (p string)
  "Flush lines, leaving only those that match String.  Limit search to
   current region if current region is active."
  (declare (ignore p))
  (or string
      (setf string (prompt-for-string :prompt "Flush other lines: "
				      :default *last-search-string*
				      :help "String to search for")))
  (let* ((region (get-count-region))
	 (start-mark (region-start region))
	 (stop-mark (region-end region))
	 (pattern (get-search-pattern string :forward)))
    (with-mark ((beg-mark start-mark :left-inserting)
		(end-mark start-mark :right-inserting))
      (loop
	(move-mark end-mark beg-mark)
	(cond ((and (find-pattern end-mark pattern) (mark< end-mark stop-mark))
	       (line-start end-mark)
	       (delete-region (region beg-mark end-mark))
	       (unless (line-offset beg-mark 1 0)
		 (return)))
	      (t
	       (delete-region (region beg-mark stop-mark))
	       (return)))))))

#| FIX
(defun rule-flush-other-lines (region rule)
  (let ((start-mark (region-start region))
	(stop-mark (region-end region))
	(pattern (get-rule-search-pattern rule :forward)))
    (with-mark ((beg-mark start-mark :left-inserting)
		(end-mark start-mark :right-inserting))
      (loop
	(move-mark end-mark beg-mark)
	(cond ((and (find-pattern end-mark pattern) (mark< end-mark stop-mark))
	       (line-start end-mark)
	       (delete-region (region beg-mark end-mark))
	       (or (line-offset beg-mark 1 0) (return)))
	      (t
	       (delete-region (region beg-mark stop-mark))
	       (return)))))))

(defcommand "Rule Flush Other Lines" (p rule)
  "Flush lines, leaving those that match Rule.  Limit search to current
   region if current region is active."
  (declare (ignore p))
  (or rule
      (setf rule (prompt-for-expression :prompt "Rule: "
; FIX
;					:default *last-search-string*
					:help "Search rule.")))
  (rule-flush-other-lines (get-count-region) rule))
|#

(defcommand "Flush Duplicate Lines" ()
  "Flush any duplicate lines in the current buffer.  Limit search to
   current region if current region is active."
  (do-buffer-lines (line (current-buffer))
    (loop for next = (line-next line) then (line-next next)
          while next do
      (when (string= (line-string next) (line-string line))
	;; FIX (delete-line
	(let ((end (mark line 0)))
	  (line-end end)
	  (mark-after end)
	  (delete-region (region (mark line 0) end)))))))

(defcommand "Count Occurrences" (p string)
  "Count the number of occurrences of a prompted string in the text from
   the point to the end of the buffer.  Echo the count.  If the
   current-region is active count in the current region instead."
  (declare (ignore p))
  (message "~D occurrence~:P"
	   (count-occurrences-region (get-count-region)
				     (or string
					 (prompt-for-string
					  :prompt "Count Occurrences: "
					  :default *last-search-string*
					  :help "String to search for")))))

(defun count-occurrences-region (region string)
  (let ((pattern (get-search-pattern string :forward))
	(end-mark (region-end region)))
    (let ((occurrences 0))
      (with-mark ((mark (region-start region)))
	(loop
	  (let ((won (find-pattern mark pattern)))
	    (when (or (null won) (mark> mark end-mark))
	      (return))
	    (incf occurrences)
	    (character-offset mark won))))
      occurrences)))
