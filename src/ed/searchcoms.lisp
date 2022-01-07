;;; Searching and replacing commands.

(in-package "HEMLOCK")



;;;; Some global state.

(defvar *last-search-string* () "Last string searched for.")
(defvar *last-search-switched* nil
  "Whether the last search switched to a case sensitive.")
(defvar *last-search-pattern*
  (new-search-pattern :string-insensitive :forward "Foo")
  "Search pattern we keep around so we don't cons them all the time.")

(defvar *last-rule-search-rule* () "Rule from last search.")
(defvar *last-rule-search-pattern*
  (new-search-pattern :parser :forward '("Foo"))
  "Cached rule search pattern.")

(defhvar "String Search Fold Case"
  "When true, string searching compares upper and lower case as equal.
   When :initially searching switches to case sensitive as soon as an
   uppercase character is entered."
  :value t)

(defun get-search-pattern (string direction)
  (declare (simple-string string))
  (when (zerop (length string)) (editor-error))
  (setq *last-search-string* string)
  (setq *last-search-pattern*
	(new-search-pattern (if (value string-search-fold-case)
				:string-insensitive
				:string-sensitive)
			    direction string *last-search-pattern*)))

(defun get-rule-search-pattern (rule direction)
  (or rule (editor-error))
  (setq *last-rule-search-rule* rule)
  (setq *last-rule-search-pattern*
	(new-search-pattern :parser direction rule
			    *last-rule-search-pattern*)))



;;;; Once-off searching.

(defcommand "Forward Search" (p &optional string)
  "Do a forward search for a string.
  Prompt for the string and leave the point after where it is found."
  "Searches for the specified String in the current buffer."
  (declare (ignore p))
  (if (not string)
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
	     (editor-error)))))

(defcommand "Reverse Search" (p &optional string)
  "Do a backward search for a string.
  Prompt for the string and leave the point before where it is found."
  "Searches backwards for the specified String in the current buffer."
  (declare (ignore p))
  (if (not string)
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
	     (editor-error)))))

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

(defcommand "Forward Rule Search" (p &optional rule)
  "Do a forward search for a match to a parser rule.
   Prompt for the rule and leave the point after the match."
  "Searches the current buffer for a match to the specified Rule."
  (declare (ignore p))
  (or rule
      (setq rule (prompt-for-string :prompt "Rule Search: "
				    ;; FIX this prints the surrounding parens
				    :default (format nil "~S" *last-rule-search-rule*)
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
	     (editor-error)))))

(defcommand "Reverse Rule Search" (p &optional rule)
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
	     (editor-error)))))



;;;; Incremental searching.

(defun i-search-pattern (string direction)
  (setq *last-search-pattern*
	(new-search-pattern (if (value string-search-fold-case)
				:string-insensitive
				:string-sensitive)
			    direction string *last-search-pattern*)))

;;;      %I-SEARCH-ECHO-REFRESH refreshes the echo buffer for incremental
;;; search.
;;;
(defun %i-search-echo-refresh (string direction failure)
  (when (interactive)
    (clear-echo-area)
    (format *echo-area-stream*
	    "~:[~;Failing ~]~:[Reverse I-Search~;I-Search~]: ~A"
	    failure (eq direction :forward) string)))

(defcommand "Incremental Search" (p)
  "Searches for input string as characters are provided.
  These are the default I-Search command characters:  ^Q quotes the
  next character typed.  Backspace cancels the last character typed.  ^S
  repeats forward, and ^R repeats backward.  ^R or ^S with empty string
  either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with
  an empty search string calls the non-incremental search command.
  Other control characters cause exit and execution of the appropriate
  command.  If the search fails at some point, ^G and backspace may be
  used to backup to a non-failing point; also, ^S and ^R may be used to
  look the other way.  ^G during a successful search aborts and returns
  point to where it started."
  "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."
  (declare (ignore p))
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" :forward nil)
  (let* ((point (current-point))
	 (save-start (copy-mark point :temporary)))
    (with-mark ((here point))
      (handler-case
	  (when (eq (catch 'exit-i-search
		      (%i-search "" point here :forward nil))
		    :control-g)
	    (move-mark point save-start)
	    (invoke-hook abort-hook)
	    (editor-error))
	;; Now signalled in handler of C-g sigint.
	(hi::editor-top-level-catcher ()
	  (move-mark point save-start)))
      (if (region-active-p)
	  (delete-mark save-start)
	  (push-buffer-mark save-start)))))


(defcommand "Reverse Incremental Search" (p)
  "Searches for input string as characters are provided.
  These are the default I-Search command characters:  ^Q quotes the
  next character typed.  Backspace cancels the last character typed.  ^S
  repeats forward, and ^R repeats backward.  ^R or ^S with empty string
  either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with
  an empty search string calls the non-incremental search command.
  Other control characters cause exit and execution of the appropriate
  command.  If the search fails at some point, ^G and backspace may be
  used to backup to a non-failing point; also, ^S and ^R may be used to
  look the other way.  ^G during a successful search aborts and returns
  point to where it started."
  "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."
  (declare (ignore p))
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" :backward nil)
  (let* ((point (current-point))
	 (save-start (copy-mark point :temporary)))
    (with-mark ((here point))
      (handler-case
	  (when (eq (catch 'exit-i-search
		      (%i-search "" point here :backward nil))
		    :control-g)
	    (move-mark point save-start)
	    (invoke-hook abort-hook)
	    (editor-error))
	;; Now signalled in handler of C-g sigint.
	(hi::editor-top-level-catcher ()
	  (move-mark point save-start)))
      (if (region-active-p)
	  (delete-mark save-start)
	  (push-buffer-mark save-start)))))

;;;      %I-SEARCH recursively (with support functions) searches to provide
;;; incremental searching.  There is a loop in case the recursion is ever
;;; unwound to some call.  curr-point must be saved since point is clobbered
;;; with each recursive call, and the point must be moved back before a
;;; different letter may be typed at a given call.  In the CASE at :cancel
;;; and :control-g, if the string is not null, an accurate pattern for this
;;; call must be provided when %I-SEARCH-CHAR-EVAL is called a second time
;;; since it is possible for ^S or ^R to be typed.
;;;
(defun %i-search (string point trailer direction failure)
  (hlet ((string-search-fold-case (value string-search-fold-case)))
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

;;;      %I-SEARCH-CHAR-EVAL evaluates the last character typed and takes
;;; necessary actions.
;;;
(defun %i-search-char-eval (key-event string point trailer direction failure)
  (declare (simple-string string))
  (cond ((let ((character (key-event-char key-event)))
	   (and character (standard-char-p character)))
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
	 (%i-search-printed-char (get-key-event *editor-input* t)
				 string point trailer direction failure))
	((equalp key-event #k"C-w")
	 (%i-search-copy-word key-event
			      string point trailer direction failure))
	((and (zerop (length string)) (logical-key-event-p key-event :exit))
	 (if (eq direction :forward)
	     (forward-search-command nil)
	     (reverse-search-command nil))
	 (throw 'exit-i-search nil))
	(t
	 (unless (logical-key-event-p key-event :exit)
	   (unget-key-event key-event *editor-input*))
	 (unless (zerop (length string))
	   (setf *last-search-string* string))
	 (throw 'exit-i-search nil))))

;;;      %I-SEARCH-CONTROL-S-OR-R handles repetitions in the search.  Note
;;; that there cannot be failure in the last COND branch: since the direction
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


;;;      %I-SEARCH-EMPTY-STRING handles the empty string case when a ^S
;;; or ^R is typed.  If the direction and character typed do not agree,
;;; then merely switch directions.  If there was a previous string, search
;;; for it, else flash at the guy.
;;;
(defun %i-search-empty-string (point trailer direction forward-direction-p
				     forward-character-p)
  (cond ((eq forward-direction-p (not forward-character-p))
	 (let ((direction (if forward-character-p :forward :backward)))
	   (%i-search-echo-refresh "" direction nil)
	   (%i-search "" point trailer direction nil)))
	(*last-search-string*
	 (hlet ((string-search-fold-case (if *last-search-switched*
					     nil
					     (value string-search-fold-case))))
	   (%i-search-echo-refresh *last-search-string* direction nil)
	   (i-search-pattern *last-search-string* direction)
	   (%i-search-find-pattern *last-search-string* point trailer direction)))
	(t (beep))))


;;;      %I-SEARCH-PRINTED-CHAR handles the case of standard character input.
;;; If the direction is backwards, we have to be careful not to MARK-AFTER
;;; the end of the buffer or to include the next character at the beginning
;;; of the search.
;;;
(defun %i-search-printed-char (key-event string point trailer direction failure)
  (let ((tchar (ext:key-event-char key-event)))
    (unless tchar (editor-error "Not a text character -- ~S" (key-event-char
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

;;; FIX Presumably %i-search-copy-word, added 2003 (file from 2006 head),
;;; is pd like dabbrev.lisp which is also by Luke Gorrie.
;;;     check http://fresh.homeunix.net/~luke/hemlock/

;;;       %I-SEARCH-COPY-WORD handles the "take the next word of the current
;;; match" case, like C-w in GNU Emacs. By Luke Gorrie.

(defun %i-search-copy-word (key-event string point trailer direction failure)
  (declare (ignore key-event))
  ;; Begin by finding the region starting at the end of the current search
  ;; and ending after the next word.
  (let ((start-mark (copy-mark point :temporary)))
    ;; When going backwards, the point is at the beginning of the search
    ;; string, so move start-mark to the end.
    (when (eq direction :backward)
      (character-offset start-mark (length string)))
    (let* ((end-mark (copy-mark start-mark :temporary))
	   (word-region (region start-mark end-mark)))
      ;; Advance end-mark to the end of the word.
      (and (find-attribute end-mark :word-delimiter #'zerop)
	   (find-attribute (mark-after end-mark) :word-delimiter)
	   ;; The region is correct, now extract the text and update the
	   ;; search string.
	   (let ((new-fragment (region-to-string word-region)))
	     ;; Start End  start End  sTaRt End
	     (when (eq (value string-search-fold-case) :initially)
	       (nstring-downcase new-fragment))
	     (let ((new-string (concatenate 'simple-string string new-fragment)))
	       ;; Update the status message.
	       (when (interactive)
		 (insert-string (buffer-point *echo-area-buffer*) new-fragment)
		 (force-output *echo-area-stream*))
	       (i-search-pattern new-string direction)
	       (cond (failure (%i-search new-string point trailer direction failure))
		     (t
		      (when (eq direction :backward)
			(move-mark trailer end-mark))
		      (%i-search-find-pattern new-string point trailer direction)))))))))

;;;      %I-SEARCH-FIND-PATTERN takes a pattern for a string and direction
;;; and finds it, updating necessary pointers for the next call to %I-SEARCH.
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



;;;; Replacement commands:

(defcommand "Replace String" (p &optional
				(target (prompt-for-string
					 :prompt "Replace String: "
					 :help "Target string"
					 :default *last-search-string*))
				(replacement (prompt-for-string
					      :prompt "With: "
					      :help "Replacement string")))
  "Replaces the specified Target string with the specified Replacement
   string in the current buffer for all occurrences after the point or within
   the active region, depending on whether it is active."
  "Replaces the specified Target string with the specified Replacement
   string in the current buffer for all occurrences after the point or within
   the active region, depending on whether it is active.  The prefix argument
   may limit the number of replacements."
  (multiple-value-bind (ignore count)
		       (query-replace-function p target replacement
					       "Replace String" t)
    (declare (ignore ignore))
    (message "~D Occurrences replaced." count)))

(defcommand "Query Replace" (p &optional
			       (target (prompt-for-string
					:prompt "Query Replace: "
					:help "Target string"
					:default *last-search-string*))
			       (replacement (prompt-for-string
					     :prompt "With: "
					     :help "Replacement string")))
  "Replaces the Target string with the Replacement string if confirmation
   from the keyboard is given.  If the region is active, limit queries to
   occurrences that occur within it, otherwise use point to end of buffer."
  "Replaces the Target string with the Replacement string if confirmation
   from the keyboard is given.  If the region is active, limit queries to
   occurrences that occur within it, otherwise use point to end of buffer.
   A prefix argument may limit the number of queries."
  (let ((mark (copy-mark (current-point))))
    (multiple-value-bind (ignore count)
			 (query-replace-function p target replacement
						 "Query Replace")
      (declare (ignore ignore))
      (message "~D Occurrences replaced." count))
    (push-buffer-mark mark)))


(defhvar "Case Replace"
  "If this is true then \"Query Replace\" will try to preserve case when
  doing replacements."
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
    (when (and count (minusp count))
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
	   (if doing-all?
	       (replace-that-case replacement cap upper point length dumb)
	       (command-case
		   (:prompt
		    "Query replace: "
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

(defcommand "List Matching Lines" (p &optional string)
  "Prompts for a search string and lists all matching lines after the point or
   within the current-region, depending on whether it is active or not.
   With an argument, lists p lines before and after each matching line."
  "Prompts for a search string and lists all matching lines after the point or
   within the current-region, depending on whether it is active or not.
   With an argument, lists p lines before and after each matching line."
  (unless string
    (setf string (prompt-for-string :prompt "List Matching: "
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
	(unless (line-offset mark 1 0)
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
	(unless (line-offset beg-mark (- num))
	  (buffer-start beg-mark))
	(unless (line-offset mark num)
	  (buffer-end mark))
	(let ((lines (list "--------")))
	  (loop
	    (push (line-string (mark-line mark)) lines)
	    (when (same-line-p mark beg-mark)
	      (return lines))
	    (line-offset mark -1))))))

(defcommand "Delete Matching Lines" (p &optional string)
  "Deletes all lines that match the search pattern using delete-region. If
   the current region is active, limit the search to it. The argument is
   ignored."
  "Deletes all lines that match the search pattern using delete-region. If
   the current region is active, limit the search to it. The argument is
   ignored."
  (flush-matching-lines-command p string))

(defcommand "Flush Lines" (p &optional string)
  "Flush lines matching String.  Limit search to current region if current region is
   active."
  "Flush lines matching String.  Limit search to current region if current region is
   active."
  (declare (ignore p))
  (unless string
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
	(unless (and (find-pattern bol-mark pattern) (mark< bol-mark end-mark))
	  (return))
	(move-mark eol-mark bol-mark)
	(line-start bol-mark)
	(unless (line-offset eol-mark 1 0)
	  (buffer-end eol-mark))
	(delete-region (region bol-mark eol-mark))))))

(defcommand "Delete Non-Matching Lines" (p &optional string)
  "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."
  "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."
  (flush-non-matching-lines-command p string))

(defcommand "Flush Other Lines" (p &optional string)
  "Flush lines, leaving those that match String.  Limit search to current
   region if current region is active."
  "Flush lines, leaving those that match String.  Limit search to current
   region if current region is active."
  (declare (ignore p))
  (unless string
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

(defcommand "Rule Flush Other Lines" (p &optional rule)
  "Flush lines, leaving those that match Rule.  Limit search to current
   region if current region is active."
  "Flush lines, leaving those that match Rule.  Limit search to current
   region if current region is active."
  (declare (ignore p))
  (or rule
      (setf rule (prompt-for-expression :prompt "Rule: "
; FIX
;					:default *last-search-string*
					:help "Search rule.")))
  (rule-flush-other-lines (get-count-region) rule))

(defcommand "Flush Duplicate Lines" (p)
  "Flush any duplicate lines in the current buffer.  Limit search to
   current region if current region is active."
  "Flush any duplicate lines in the current buffer.  Limit search to
   current region if current region is active."
  (declare (ignore p))
  (do-lines (line (current-buffer))
    (loop for next = (line-next line) then (line-next next)
          while next do
      (when (string= (line-string next) (line-string line))
	;; FIX (delete-line
	(let ((end (mark line 0)))
	  (line-end end)
	  (mark-after end)
	  (delete-region (region (mark line 0) end)))))))

(defcommand "Delete Non-Matching Lines" (p &optional string)
  "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."
  "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."
  (flush-non-matching-lines-command p string))

(defcommand "Flush Other Lines" (p &optional string)
  "Flush lines, leaving those that match String.  Limit search to current
   region if current region is active."
  "Flush lines, leaving those that match String.  Limit search to current
   region if current region is active."
  (declare (ignore p))
  (unless string
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



(defcommand "Count Occurrences" (p &optional string)
  "Prompts for a search string and counts occurrences of it after the point or
   within the current-region, depending on whether it is active or not. The
   argument is ignored."
  "Prompts for a search string and counts occurrences of it after the point or
   within the current-region, depending on whether it is active or not. The
   argument is ignored."
  (declare (ignore p))
  (unless string
    (setf string (prompt-for-string
		  :prompt "Count Occurrences: "
		  :default *last-search-string*
		  :help "String to search for")))
  (message "~D occurrence~:P"
	   (count-occurrences-region (get-count-region) string)))

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
