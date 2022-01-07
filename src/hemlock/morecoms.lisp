;;; -*- Log: hemlock.log; Package: Hemlock; Mode: Editor -*-
;;;
;;; Even more commands...

(in-package "HEMLOCK")


(defhvar "Region Query Size"
  "A number-of-lines threshold that destructive, undoable region commands
   should ask the user about when the indicated region is too big."
  :value 30)

(defun check-region-query-size (region)
  "Checks the number of lines in region against \"Region Query Size\" and
   asks the user if the region crosses this threshold.  If the user responds
   negatively, then an editor error is signaled."
  (let ((threshold (or (value region-query-size) 0)))
    (if (and (plusp threshold)
	     (>= (count-lines region) threshold)
	     (not (prompt-for-y-or-n
		   :prompt "Region size exceeds \"Region Query Size\".  Confirm: "
		   :must-exist t)))
	(editor-error))))



;;;; Casing commands...

(defcommand "Uppercase Word" (p)
  "Uppercase a word at point.
   With prefix argument uppercase that many words."
  "Uppercase p words at the point."
  (filter-words p (current-point) #'string-upcase))

(defcommand "Lowercase Word" (p)
  "Uppercase a word at point.
   With prefix argument uppercase that many words."
  "Uppercase p words at the point."
  (filter-words p (current-point) #'string-downcase))

;;; FILTER-WORDS implements "Uppercase Word" and "Lowercase Word".
;;;
(defun filter-words (p point function)
  (let ((arg (or p 1)))
    (with-mark ((mark point))
      (if (word-offset (if (minusp arg) mark point) arg)
	  (filter-region function (region mark point))
	  (editor-error "Not enough words.")))))

;;; "Capitalize Word" is different than uppercasing and lowercasing because
;;; the differences between Hemlock's notion of what a word is and Common
;;; Lisp's notion are too annoying.
;;;
(defcommand "Capitalize Word" (p)
  "Lowercase a word capitalizing the first character.  With a prefix
  argument, capitalize that many words.  A negative argument capitalizes
  words before the point, but leaves the point where it was."
  "Capitalize p words at the point."
  (let ((point (current-point))
	(arg (or p 1)))
    (with-mark ((start point :left-inserting)
		(end point))
      (when (minusp arg)
	(unless (word-offset start arg) (editor-error "No previous word.")))
      (do ((region (region start end))
	   (cnt (abs arg) (1- cnt)))
	  ((zerop cnt) (move-mark point end))
	(unless (find-attribute start :word-delimiter #'zerop)
	  (editor-error "No next word."))
	(move-mark end start)
	(find-attribute end :word-delimiter)
	(loop
	  (when (mark= start end)
	    (move-mark point end)
	    (editor-error "No alphabetic characters in word."))
	  (when (alpha-char-p (next-character start)) (return))
	  (character-offset start 1))
	(setf (next-character start) (char-upcase (next-character start)))
	(mark-after start)
	(filter-region #'string-downcase region)))))

(defcommand "Uppercase Region" (p)
  "Uppercase words from point to mark."
  "Uppercase words from point to mark."
  (declare (ignore p))
  (twiddle-region (current-region) #'string-upcase "Uppercase Region"))

(defcommand "Lowercase Region" (p)
  "Lowercase words from point to mark."
  "Lowercase words from point to mark."
  (declare (ignore p))
  (twiddle-region (current-region) #'string-downcase "Lowercase Region"))

;;; TWIDDLE-REGION implements "Uppercase Region" and "Lowercase Region".
;;;
(defun twiddle-region (region function name)
  (let* (;; don't delete marks start and end since undo stuff will.
	 (start (copy-mark (region-start region) :left-inserting))
	 (end (copy-mark (region-end region) :left-inserting)))
    (let* ((region (region start end))
	   (undo-region (copy-region region)))
      (check-region-query-size region)
      (filter-region function region)
      (make-region-undo :twiddle name region undo-region))))



;;;; More stuff.

(defcommand "Delete Previous Character Expanding Tabs" (p)
  "Delete the previous character.
  When deleting a tab pretend it is the equivalent number of spaces.
  With prefix argument, do it that many times."
  "Delete the P previous characters, expanding tabs into spaces."
  (let ((point (current-point))
        (n (or p 1)))
    (when (minusp n)
      (editor-error "Delete Previous Character Expanding Tabs only accepts ~
                     positive arguments."))
    ;; Pre-calculate the number of characters that need to be deleted
    ;; and any remaining white space filling, allowing modification to
    ;; be avoided if there are not enough characters to delete.
    (let ((errorp nil)
	  (del 0)
	  (fill 0))
      (with-mark ((mark point))
	(dotimes (i n)
	  (if (> fill 0)
	      (decf fill)
	      (let ((prev (previous-character mark)))
		(cond ((and prev (char= prev #\tab))
		       (let ((pos (mark-column mark)))
			 (mark-before mark)
			 (incf fill (- pos (mark-column mark) 1)))
		       (incf del))
		      ((mark-before mark)
		       (incf del))
		      (t
		       (setq errorp t)
		       (return)))))))
      (cond ((and (not errorp) (kill-characters point (- del)))
	     (with-mark ((mark point :left-inserting))
	       (dotimes (i fill)
		 (insert-character mark #\space))))
	    (t
	     (editor-error "There were not ~D characters before point." n))))))


(defvar *scope-table*
  (list (make-string-table :initial-contents
			   '(("Global" . :global)
			     ("Buffer" . :buffer)
			     ("Mode" . :mode)))))

(defun prompt-for-place (prompt help)
  (multiple-value-bind (word val)
		       (prompt-for-keyword *scope-table* :prompt prompt
					   :help help :default "Global")
    (declare (ignore word))
    (case val
      (:buffer
       (values :buffer (prompt-for-buffer :help "Buffer to be local to."
					  :default (current-buffer))))
      (:mode
       (values :mode (prompt-for-keyword
		      (list *mode-names*)
		      :prompt "Mode: "
		      :help "Mode to be local to."
		      :default (buffer-major-mode (current-buffer)))))
      (:global :global))))

(defcommand "Bind Key" (p)
  "Bind a command to a key.
  The command, key and place to make the binding are prompted for."
  "Prompt for stuff to do a bind-key."
  (declare (ignore p))
  (multiple-value-call #'bind-key
    (values (prompt-for-keyword
	     (list *command-names*)
	     :prompt "Command to bind: "
	     :help "Name of command to bind to a key."))
    (values (prompt-for-key
	     :prompt "Bind to: "  :must-exist nil
	     :help "Key to bind command to, confirm to complete."))
    (prompt-for-place "Kind of binding: "
		      "The kind of binding to make.")))

(defcommand "Delete Key Binding" (p)
  "Delete a key binding.
  The key and place to remove the binding are prompted for."
  "Prompt for stuff to do a delete-key-binding."
  (declare (ignore p))
  (let ((key (prompt-for-key
	      :prompt "Delete binding: " :must-exist nil
	      :help "Key to delete binding from.")))
    (multiple-value-bind (kind where)
			 (prompt-for-place "Kind of binding: "
					   "The kind of binding to make.")
      (unless (get-command key kind where)
	(editor-error "No such binding: ~S" key))
      (delete-key-binding key kind where))))


(defcommand "Set Variable" (p)
  "Prompt for a Hemlock variable and a new value."
  "Prompt for a Hemlock variable and a new value."
  (declare (ignore p))
  (multiple-value-bind (name var)
		       (prompt-for-variable
			:prompt "Variable: "
			:help "The name of a variable to set.")
    (declare (ignore name))
    (setf (variable-value var)
	  (handle-lisp-errors
	   (eval (prompt-for-expression
		  :prompt "Value: "
		  :help "Expression to evaluate for new value."))))))

(defcommand "Defhvar" (p)
  "Define a hemlock variable in some location.  If the named variable exists
   currently, its documentation is propagated to the new instance, but this
   never prompts for documentation."
  "Define a hemlock variable in some location."
  (declare (ignore p))
  (let* ((name (nstring-capitalize (prompt-for-variable :must-exist nil)))
	 (var (string-to-variable name))
	 (doc (if (hemlock-bound-p var)
		  (variable-documentation var)
		  ""))
	 (hooks (if (hemlock-bound-p var) (variable-hooks var)))
	 (val (prompt-for-expression :prompt "Variable value: "
				     :help "Value for the variable.")))
    (multiple-value-bind
	(kind where)
	(prompt-for-place
	 "Kind of binding: "
	 "Whether the variable is global, mode, or buffer specific.")
      (case kind 
	(:global
	 (defhvar name doc :value val :hooks hooks))
; FIX orig (results in note)
; 	(t
; 	 (defhvar name doc type where :value val :hooks hooks))
	(:mode
	 (defhvar name doc :mode where :value val :hooks hooks))
	(:buffer
	 (defhvar name doc :buffer where :value val :hooks hooks))))))


(defcommand "List Buffers" (p)
  "Show a list of all buffers.
   If the buffer is modified then a * is displayed before the name.  If there
   is an associated file then it's name is displayed last.  With prefix
   argument, only list modified buffers."
  "Display the names of all buffers in a with-random-typeout window."
  (with-pop-up-display (s)
    (do-strings (n b *buffer-names*)
      (declare (simple-string n))
      (unless (or (eq b *echo-area-buffer*)
		  (assoc b *random-typeout-buffers* :test #'eq))
	(let ((modified (buffer-modified b))
	      (buffer-pathname (buffer-pathname b)))
	  (when (or (not p) modified)
	    (write-char (if modified #\* #\space) s)
	    (if buffer-pathname
		(format s "~A  ~25T~A~:[~68T~A~;~]~%"
			(file-namestring buffer-pathname)
			(directory-namestring buffer-pathname)
			(string= (pathname-to-buffer-name buffer-pathname) n)
			n)
		(format s "~A~68T~D Line~:P~%"
			n (count-lines (buffer-region b))))))))))

(defcommand "Select Random Typeout Buffer" (p)
  "Select last random typeout buffer."
  "Select last random typeout buffer."
  (declare (ignore p))
  (if *random-typeout-buffers*
      (change-to-buffer (caar *random-typeout-buffers*))
      (editor-error "There are no random typeout buffers.")))


(defcommand "Room" (p)
  "Display stats on allocated storage.  With a positive prefix print the
   maximum amount of information, with a negative prefix print the
   minimum."
  "Run Room into a With-Random-Typeout window.  If P is positive pass room
   T, if P is negative pass nil, else pass :default."
  (with-pop-up-display (*standard-output*)
    (room (if p (> p 0) :default))))

(defun gc-message-before (bytes-in-use)
  (message "~&[GC threshold exceeded with ~:D bytes in use.  ~
	    Commencing GC.]~%" bytes-in-use))

(defun gc-message-after (bytes-retained bytes-freed new-trigger)
  (message "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%"
	   bytes-retained bytes-freed)
  (message "[GC will next occur when at least ~:D bytes are in use.]~%"
	   new-trigger))

(defcommand "Garbage Collect" (p)
  "Call Collect Garbage"
  "Call Collect Garbage"
  (collect-garbage-command p))
(defcommand "GC" (p)
  "Call Collect Garbage"
  "Call Collect Garbage"
  (collect-garbage-command p))

#+gencgc
(defcommand "Collect Garbage" (p)
  "Initiate a garbage collection.  *gc-verbose* determines verbosity.  With
   a prefix collect prefix many generations, else collect all generations."
  "Initiate a garbage collection.  *gc-verbose* determines verbosity.  If P
   collect P generations, else collect all generations."
  (let ((*gc-notify-before* #'gc-message-before)
	(*gc-notify-after* #'gc-message-after))
    (if p (gc :gen p) (gc :full t))))
#-gencgc
(defcommand "Collect Garbage" (p)
  "Initiate a garbage collection.  *gc-verbose* determines verbosity."
  "Initiate a garbage collection.  *gc-verbose* determines verbosity."
  (declaim (ignore p))
  (gc))

(defcommand "List Fonts" (p)
  "Pop-up an list of examples of each font."
  "Pop-up an list of examples of each font."
  (declare (ignore p))
  (let* ((buf-name "Font List")
	 (new-buffer (make-buffer buf-name
				  :modes '("Fundamental" "View")))
	 (buffer (or new-buffer (getstring buf-name *buffer-names*))))
    (change-to-buffer buffer)
    ;; FIX pick new buffer if exists and contains something else
    ;;     or perhaps confirm to overwrite (possibly saving first)
    (if new-buffer
	(let ((mark (current-point)))
	  (with-writable-buffer (buffer)
	    (dotimes (font hi::font-map-size)
	      (insert-string mark (format nil "~A " font))
	      (font-mark (mark-line mark) (mark-charpos mark) font)
	      (insert-string mark (format nil "Font number ~A~%" font))
	      (font-mark (mark-line mark) (mark-charpos mark) 0)))))))

(defcommand "Clear Buffer Fonts" (p &optional (buffer (current-buffer)))
  "Clear any font marks from the current buffer."
  "Clear any font marks from Buffer."
  (declare (ignore p))
  (loop for line = (mark-line (buffer-start-mark buffer))
              then (line-next line)
        while line do
    (setf (getf (line-plist line) 'chi-info) nil)
    (delete-line-font-marks line)))

(defcommand "Clear Line Fonts" (p &optional (line (mark-line (current-point))))
  "Clear any font marks from the current line."
  "Clear any font marks from Line."
  (declare (ignore p))
  (delete-line-font-marks line))

;;; This is used by the :edit-level modeline field which is defined in Main.Lisp.
;;;
(defvar *recursive-edit-count* 0)

(defun do-recursive-edit ()
  "Does a recursive edit, wrapping []'s around the modeline of the current
  window during its execution.  The current window and buffer are saved
  beforehand and restored afterward.  If they have been deleted by the
  time the edit is done then an editor-error is signalled."
  (let* ((win (current-window))
	 (buf (current-buffer)))
    (unwind-protect
	(let ((*recursive-edit-count* (1+ *recursive-edit-count*)))
	  (update-modeline-field *echo-area-buffer* *echo-area-window*
				 (modeline-field :edit-level))
	  (recursive-edit))
      (update-modeline-field *echo-area-buffer* *echo-area-window*
			     (modeline-field :edit-level))
      (unless (and (memq win *window-list*) (memq buf *buffer-list*))
	(editor-error "Old window or buffer has been deleted."))
      (setf (current-window) win)
      (unless (eq (window-buffer win) buf)
	(setf (window-buffer win) buf))
      (setf (current-buffer) buf))))

(defcommand "Exit Recursive Edit" (p)
  "Exit a level of recursive edit.  Signals an error when not in a
   recursive edit."
  "Exit a level of recursive edit.  Signals an error when not in a
   recursive edit."
  (declare (ignore p))
  (unless (in-recursive-edit) (editor-error "Not in a recursive edit!"))
  (exit-recursive-edit ()))

(defcommand "Abort Recursive Edit" (p)
  "Abort the current recursive edit.  Signals an error when not in a
   recursive edit."
  "Abort the current recursive edit.  Signals an error when not in a
   recursive edit."
  (declare (ignore p))
  (unless (in-recursive-edit) (editor-error "Not in a recursive edit!"))
  (abort-recursive-edit "Recursive edit aborted."))


;;; TRANSPOSE REGIONS uses CURRENT-REGION to signal an error if the current
;;; region is not active and to get start2 and end2 in proper order.  Delete1,
;;; delete2, and delete3 are necessary since we are possibly ROTATEF'ing the
;;; locals end1/start1, start1/start2, and end1/end2, and we need to know which
;;; marks to dispose of at the end of all this stuff.  When we actually get to
;;; swapping the regions, we must delete both up front if they both are to be
;;; deleted since we don't know what kind of marks are in start1, start2, end1,
;;; and end2, and the marks will be moving around unpredictably as we insert
;;; text at them.  We copy point into ipoint for insertion purposes since one
;;; of our four marks is the point.
;;;
(defcommand "Transpose Regions" (p)
  "Transpose two regions with endpoints defined by the mark stack and point.
   To use:  mark start of region1, mark end of region1, mark start of region2,
   and place point at end of region2.  Invoking this immediately following
   one use will put the regions back, but you will have to activate the
   current region."
  "Transpose two regions with endpoints defined by the mark stack and point."
  (declare (ignore p))
  (unless (>= (ring-length (value buffer-mark-ring)) 3)
    (editor-error "Need two marked regions to do Transpose Regions."))
  (let* ((region (current-region))
	 (end2 (region-end region))
	 (start2 (region-start region))
	 (delete1 (pop-buffer-mark))
	 (end1 (pop-buffer-mark))
	 (delete2 end1)
	 (start1 (pop-buffer-mark))
	 (delete3 start1))
    ;;get marks in the right order, to simplify the code that follows
    (unless (mark<= start1 end1) (rotatef start1 end1))
    (unless (mark<= start1 start2)
      (rotatef start1 start2)
      (rotatef end1 end2))
    ;;order now guaranteed:  <Buffer Start> start1 end1 start2 end2 <Buffer End>
    (unless (mark<= end1 start2)
      (editor-error "Can't transpose overlapping regions."))
    (let* ((adjacent-p (mark= end1 start2))
	   (region1 (delete-and-save-region (region start1 end1)))
	   (region2 (unless adjacent-p
		      (delete-and-save-region (region start2 end2))))
	   (point (current-point)))
      (with-mark ((ipoint point :left-inserting))
	(let ((save-end2-loc (push-buffer-mark (copy-mark end2))))
	  (ninsert-region (move-mark ipoint end2) region1)
	  (push-buffer-mark (copy-mark ipoint))
	  (cond (adjacent-p
		 (push-buffer-mark (copy-mark start2))
		 (move-mark point save-end2-loc))
		(t (push-buffer-mark (copy-mark end1))
		   (ninsert-region (move-mark ipoint end1) region2)
		   (move-mark point ipoint))))))
    (delete-mark delete1)
    (delete-mark delete2)
    (delete-mark delete3)))

(defcommand "Goto Absolute Line" (p)
  "Goes to the indicated line, if you counted them starting at the beginning
   of the buffer with the number one.  If a prefix argument is supplied, that
   is the line number; otherwise, the user is prompted."
  "Go to a user perceived line number."
  (let ((p (or p (prompt-for-expression
		  :prompt "Line number: "
		  :help "Enter an absolute line number to goto."))))
    (unless (and (integerp p) (plusp p))
      (editor-error "Must supply a positive integer."))
    (let ((point (current-point)))
      (with-mark ((m point))
	(unless (line-offset (buffer-start m) (1- p) 0)
	  (editor-error "Too few lines in buffer."))
	(move-mark point m)))))

(defcommand "Goto Absolute Character" (p)
  "Goes to the prompted character, as counted from the beginning of the
   buffer starting with the number one.  With a prefix argument goes
   character number P."
  "Go to a user perceived character number."
  (let ((p (or p (prompt-for-expression
		  :prompt "Character number: "
		  :help "Enter an absolute character number to goto."))))
    (unless (and (integerp p) (plusp p))
      (editor-error "Must supply a positive integer."))
    (let ((point (current-point)))
      (with-mark ((m point))
	(unless (character-offset (buffer-start m) (1- p))
	  (editor-error "Too few characters in buffer."))
	(move-mark point m)))))



;;;; Mouse Commands.

(defcommand "Do Nothing" (p)
  "Do nothing.
  With prefix argument, do it that many times."
  "Do nothing p times."
  (dotimes (i (or p 1)))
  (setf (last-command-type) (last-command-type)))

(defun maybe-change-window (window)
  (unless (eq window (current-window))
    (when (or (eq window *echo-area-window*)
	      (eq (current-window) *echo-area-window*)
	      (member window *random-typeout-buffers*
		      :key #'(lambda (cons)
			       (hi::random-typeout-stream-window (cdr cons)))))
      (supply-generic-pointer-up-function #'lisp::do-nothing)
      (editor-error "I'm afraid I can't let you do that Dave."))
    (setf (current-window) window)
    (let ((buffer (window-buffer window)))
      (unless (eq (current-buffer) buffer)
	(setf (current-buffer) buffer)))))

(defcommand "Top Line to Here" (p)
  "Move the top line to the line the mouse is on.
  If in the first two columns then scroll continuously until the button is
  released."
  "Move the top line to the line the mouse is on."
  (declare (ignore p))
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (unless y (editor-error))
    (cond ((< x 2)
	   (loop
	     (when (listen-editor-input *editor-input*) (return))
	     (scroll-window window -1)
	     (redisplay)
	     (editor-finish-output window)))
	  (t
	   (scroll-window window (- y))))))

(defcommand "Here to Top of Window" (p)
  "Move the line the mouse is on to the top of the window.
  If in the first two columns then scroll continuously until the button is
  released."
  "Move the line the mouse is on to the top of the window."
  (declare (ignore p))
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (unless y (editor-error))
    (cond ((< x 2)
	   (loop
	     (when (listen-editor-input *editor-input*) (return))
	     (scroll-window window 1)
	     (redisplay)
	     (editor-finish-output window)))
	  (t
	   (scroll-window window y)))))


(defvar *generic-pointer-up-fun* nil
  "This is the function for the \"Generic Pointer Up\" command that defines
   its action.  Other commands set this in preparation for this command's
   invocation.")
;;;
(defun supply-generic-pointer-up-function (fun)
  "This provides the action \"Generic Pointer Up\" command performs."
  (check-type fun function)
  (setf *generic-pointer-up-fun* fun))

(defcommand "Generic Pointer Up" (p)
  "Other commands determine this command's action by supplying functions that
   this command invokes.  The following built-in commands supply the following
   generic up actions:
      \"Point to Here\"
         When the position of the pointer is different than the current
	 point, the action pushes a buffer mark at point and moves point
         to the pointer's position.
      \"Bufed Goto and Quit\"
         The action is a no-op."
  "Invoke whatever is on *generic-pointer-up-fun*."
  (declare (ignore p))
  (unless *generic-pointer-up-fun*
    (editor-error "No commands have supplied a \"Generic Pointer Up\" action."))
  (funcall *generic-pointer-up-fun*))


(defcommand "Point to Here" (p)
  "Move the point to the position of the mouse.
   If in the modeline, move to the absolute position in the file indicated by
   the position within the modeline, pushing the old position on the mark
   stack.  This supplies a function \"Generic Pointer Up\" invokes if it runs
   without any intervening generic pointer up predecessors running.  If the
   position of the pointer is different than the current point when the user
   invokes \"Generic Pointer Up\", then this function pushes a buffer mark at
   point and moves point to the pointer's position.  This allows the user to
   mark off a region with the mouse."
  "Move the point to the position of the mouse."
  (declare (ignore p))
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (unless x (editor-error))
    (maybe-change-window window)
    (if y
	(let ((m (cursorpos-to-mark x y window)))
	  (unless m (editor-error))
	  (move-mark (current-point) m))
	(let* ((buffer (window-buffer window))
	       (region (buffer-region buffer))
	       (point (buffer-point buffer)))
	  (push-buffer-mark (copy-mark point))
	  (move-mark point (region-start region))
	  (line-offset point (round (* (1- (count-lines region)) x)
				    (1- (window-width window)))))))
  (supply-generic-pointer-up-function #'point-to-here-up-action))

(defun point-to-here-up-action ()
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (unless x (editor-error))
    (when y
      (maybe-change-window window)
      (let ((m (cursorpos-to-mark x y window)))
	(unless m (editor-error))
	(when (eq (line-buffer (mark-line (current-point)))
		  (line-buffer (mark-line m)))
	  (unless (mark= m (current-point))
	    (push-buffer-mark (copy-mark (current-point)) t)))
	(move-mark (current-point) m)))))


(defcommand "Insert Kill Buffer" (p)
  "Move current point to the mouse location and insert the kill buffer."
  "Move current point to the mouse location and insert the kill buffer."
  (declare (ignore p))
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (unless x (editor-error))
    (maybe-change-window window)
    (if y
	(let ((m (cursorpos-to-mark x y window)))
	  (unless m (editor-error))
	  (move-mark (current-point) m)
	  (un-kill-command nil))
	(editor-error "Can't insert kill buffer in modeline."))))



;;;; Page commands & stuff.

(defvar *goto-page-last-num* 0)
(defvar *goto-page-last-string* "")

(defcommand "Goto Page" (p)
  "Go to an absolute page number (argument).  If no argument, then go to
  next page.  A negative argument moves back that many pages if possible.
  If argument is zero, prompt for string and goto page with substring
  in title."
  "Go to an absolute page number (argument).  If no argument, then go to
  next page.  A negative argument moves back that many pages if possible.
  If argument is zero, prompt for string and goto page with substring
  in title."
  (let ((point (current-point)))
    (cond ((not p)
	   (page-offset point 1))
	  ((zerop p)
	   (let* ((againp (eq (last-command-type) :goto-page-zero))
		  (name (prompt-for-string :prompt "Substring of page title: "
					   :default (if againp
							*goto-page-last-string*
							*parse-default*)))
		  (dir (page-directory (current-buffer)))
		  (i 1))
	     (declare (simple-string name))
	     (cond ((not againp)
		    (push-buffer-mark (copy-mark point)))
		   ((string-equal name *goto-page-last-string*)
		    (setf dir (nthcdr *goto-page-last-num* dir))
		    (setf i (1+ *goto-page-last-num*))))
	     (loop
	       (when (null dir)
		 (editor-error "No page title contains ~S." name))
	       (when (search name (the simple-string (car dir))
			     :test #'char-equal)
		 (goto-page point i)
		 (setf (last-command-type) :goto-page-zero)
		 (setf *goto-page-last-num* i)
		 (setf *goto-page-last-string* name)
		 (return t))
	       (incf i)
	       (setf dir (cdr dir)))))
	    ((minusp p)
	     (page-offset point p))
	    (t (goto-page point p)))
    (line-start (move-mark (window-display-start (current-window)) point))))

(defun goto-page (mark i)
  (with-mark ((m mark))
    (buffer-start m)
    (unless (page-offset m (1- i))
      (editor-error "No page numbered ~D." i))
    (move-mark mark m)))


(defcommand "View Page Directory" (p)
  "Print a listing of the first non-blank line after each page mark
   in a pop-up window."
  "Print a listing of the first non-blank line after each page mark
   in a pop-up window."
  (declare (ignore p))
  (let ((dir (page-directory (current-buffer))))
    (declare (list dir))
    (with-pop-up-display (s :height (1+ (the fixnum (length dir))))
      (display-page-directory s dir))))

(defcommand "Insert Page Directory" (p)
  "Insert a listing of the first non-blank line after each page mark at
   the beginning of the buffer.  A mark is dropped before going to the
   beginning of the buffer.  If an argument is supplied, insert the page
   directory at point."
  "Insert a listing of the first non-blank line after each page mark at
   the beginning of the buffer."
  (let ((point (current-point)))
    (unless p
      (push-buffer-mark (copy-mark point))
      (buffer-start point))
    (push-buffer-mark (copy-mark point))
    (display-page-directory (make-hemlock-output-stream point :full)
			    (page-directory (current-buffer))))
  (setf (last-command-type) :ephemerally-active))

(defun display-page-directory (stream directory)
  "This writes the list of strings, directory, to stream, enumerating them
   in a field of three characters.  The number and string are separated by
   two spaces, and the first line contains headings for the numbers and
   strings columns."
  (write-line "Page    First Non-blank Line" stream)
  (do ((dir directory (cdr dir))
       (count 1 (1+ count)))
      ((null dir))
    (declare (fixnum count))
    (format stream "~3D  " count)
    (write-line (car dir) stream)))

(defun page-directory (buffer)
  "Return a list of strings where each is the first non-blank line
   following a :page-delimiter in buffer."
  (with-mark ((m (buffer-point buffer)))
    (buffer-start m)
    (let ((end-of-buffer (buffer-end-mark buffer)) result)
      (loop ;over pages.
	(loop ;for first non-blank line.
	  (cond ((not (blank-after-p m))
		 (let* ((str (line-string (mark-line m)))
			(len (length str)))
		   (declare (simple-string str))
		   (push (if (and (> len 1)
				  (= (character-attribute :page-delimiter
							  (schar str 0))
				     1))
			     (subseq str 1)
			     str)
			 result))
		 (unless (page-offset m 1)
		   (return-from page-directory (nreverse result)))
		 (when (mark= m end-of-buffer)
		   (return-from page-directory (nreverse result)))
		 (return))
		((not (line-offset m 1 0))
		 (return-from page-directory (nreverse result)))
		((= (character-attribute :page-delimiter (next-character m))
		    1)
		 (push "" result)
		 (mark-after m)
		 (return))))))))


(defcommand "Previous Page" (p)
  "Move to the beginning of the current page.
  With prefix argument move that many pages."
  "Move backward P pages."
  (let ((point (current-point)))
    (unless (page-offset point (- (or p 1)))
      (editor-error "No such page."))
    (line-start (move-mark (window-display-start (current-window)) point))))

(defcommand "Next Page" (p)
  "Move to the beginning of the next page.
  With prefix argument move that many pages."
  "Move forward P pages."
  (let ((point (current-point)))
    (unless (page-offset point (or p 1))
      (editor-error "No such page."))
    (line-start (move-mark (window-display-start (current-window)) point))))

(defcommand "Mark Page" (p)
  "Put point at beginning, mark at end of current page.
   With prefix argument, mark the page that many pages after the current one."
  "Mark the P'th page after the current one."
  (let ((point (current-point)))
    (if p
	(unless (page-offset point (1+ p)) (editor-error "No such page."))
	(page-offset point 1)) ;If this loses, we're at buffer-end.
    (with-mark ((m point))
      (unless (page-offset point -1)
	(editor-error "No such page."))
      (push-buffer-mark (copy-mark m) t)
      (line-start (move-mark (window-display-start (current-window)) point)))))

(defun page-offset (mark n)
  "Move mark past n :page-delimiters that are in the zero'th line position.
   If a :page-delimiter is the immediately next character after mark in the
   appropriate direction, then skip it before starting."
  (cond ((plusp n)
	 (find-attribute mark :page-delimiter #'zerop)
	 (dotimes (i n mark)
	   (unless (next-character mark) (return nil))
	   (loop
	     (unless (find-attribute mark :page-delimiter)
	       (return-from page-offset nil))
	     (unless (mark-after mark)
	       (return (if (= i (1- n)) mark)))
	     (when (= (mark-charpos mark) 1) (return)))))
	(t
	 (reverse-find-attribute mark :page-delimiter #'zerop)
	 (prog1
	  (dotimes (i (- n) mark)
	    (unless (previous-character mark) (return nil))
	    (loop
	      (unless (reverse-find-attribute mark :page-delimiter)
		(return-from page-offset nil))
	      (mark-before mark)
	      (when (= (mark-charpos mark) 0) (return))))
	  (let ((buffer (line-buffer (mark-line mark))))
	    (unless (or (not buffer) (mark= mark (buffer-start-mark buffer)))
	      (mark-after mark)))))))



;;;; Counting some stuff

(defcommand "Count Lines Page" (p)
  "Display number of lines in current page and position within page.
   With prefix argument do on entire buffer."
  "Count some lines, Man."
  (let ((point (current-point)))
    (if p
	(let ((r (buffer-region (current-buffer))))
	  (count-lines-function "Buffer" (region-start r) point (region-end r)))
	(with-mark ((m1 point)
		    (m2 point))
	  (unless (and (= (character-attribute :page-delimiter
					       (previous-character m1))
			  1)
		       (= (mark-charpos m1) 1))
	    (page-offset m1 -1))
	  (unless (and (= (character-attribute :page-delimiter
					       (next-character m2))
			  1)
		       (= (mark-charpos m2) 0))
	    (page-offset m2 1))
	  (count-lines-function "Page" m1 point m2)))))

(defun count-lines-function (msg start mark end)
  (let ((before (1- (count-lines (region start mark))))
	(after (count-lines (region mark end))))
    (message "~A: ~D lines, ~D/~D" msg (+ before after) before after)))

(defcommand "Count Lines" (p)
  "Display number of lines in the region."
  "Display number of lines in the region."
  (declare (ignore p))
  (multiple-value-bind (region activep) (get-count-region)
    (message "~:[After point~;Active region~]: ~A lines"
	     activep (count-lines region))))

(defcommand "Count Words" (p)
  "Prints in the Echo Area the number of words in the region
   between the point and the mark by using word-offset. The
   argument is ignored."
  "Prints number of words in the region"
  (declare (ignore p))
  (multiple-value-bind (region activep) (get-count-region)
    (let ((end-mark (region-end region)))
      (with-mark ((beg-mark (region-start region)))
	(let ((word-count 0))
	  (loop
	    (when (mark>= beg-mark end-mark)
	      (return))
	    (unless (word-offset beg-mark 1)
	      (return))
	    (incf word-count))
	  (message "~:[After point~;Active region~]: ~D Word~:P"
		   activep word-count))))))

(defcommand "Count Lines and Characters" (p)
  "Print in the Echo Area the number of lines and characters in the current
   region, if it is active, else from point to the buffer end."
  "Print number of lines and characters in the region."
  (declare (ignore p))
  (multiple-value-bind (region activep) (get-count-region)
    (message "~:[After point~;Active region~]: ~A lines, ~A characters"
	     activep
	     (count-lines region)
	     (count-characters region))))

(defcommand "Describe Cursor Position" (p)
  "Print in the Echo Area the number of lines and characters in the current
   region, if it is active, else from point to the buffer end."
  "Print number of lines and characters in the region."
  (declare (ignore p))
  (let* ((char (next-character (current-point)))
	 (buffer (current-buffer))
	 (point (current-point))
	 (pchars (count-characters (region (buffer-start-mark buffer)
					   point)))
	 (chars (count-characters (buffer-region buffer)))
	 (percent (* (/ pchars chars) 100))
	 (rest (format nil
		       "Point: ~D of ~D (~:[~4F~;~3D~]%); Column: ~D"
		       pchars chars (eq percent 100) percent
		       (mark-column point))))
    (if char
	(let ((code (char-code char)))
	  (message "Char: ~:C (0~O ~D 0x~X); ~A"
		   char code code code rest))
	(message "End of file; ~A" rest))))

;;; GET-COUNT-REGION -- Internal Interface.
;;;
;;; Returns the active region or the region between point and end-of-buffer.
;;; As a second value, it returns whether the region was active.
;;;
;;; Some searching commands use this routine.
;;;
(defun get-count-region ()
  (if (region-active-p)
      (values (current-region) t)
      (values (region (current-point) (buffer-end-mark (current-buffer)))
	      nil)))

(defcommand "Tally Column" (p)
  "Print the sum of the column of numbers at point.
   The numbers must line up where the decimal point is or would be."
  "Print the sum of the column of numbers at point.
   The numbers must line up where the decimal point is or would be."
  (declare (ignore p))
  (let ((point (current-point)))
    (or (hi::number-at-mark point)(editor-error "Point must be on a number."))
    (message "~A" (tally-column))))

;; 3           .10
;; 3       5000
;; 3          5.        eol
;; 5         34.303
;; 2                 520.39
;;                             6.32
;; 7
(defun tally-column (&optional (point (current-point)))
  "Print the sum of the column of numbers at Point.
   The numbers must line up where the decimal point is or would be."
    (let ((total (hi::number-at-mark point)))
      (when total
	(let ((mark (copy-mark point)) start-col)
	  ;; Move point to before the decimal point.
	  (hi::word-start mark)
	  (if (eq (previous-character mark) #\.)
	      (mark-before mark)
	      (word-offset mark 1))
	  (setq start-col (mark-column mark))
	  ;; Tally downwards.
	  (let ((mark2 (copy-mark mark)))
	    (line-offset mark2 1)
	    (loop for n = (hi::number-at-mark mark2)
	      while (and n (eq start-col (mark-column mark2)))
	      do
	      (incf total n)
	      (line-offset mark2 1))
	    (move-mark mark2 mark)
	    ;; Tally upwards.
	    (line-offset mark2 -1)
	    (loop for n = (hi::number-at-mark mark2)
	      while (and n (eq start-col (mark-column mark2)))
	      do
	      (incf total n)
	      (line-offset mark2 -1)))
	  total))))


;;;; Some modes:

(defcommand "Fundamental Mode" (p)
  "Put the current buffer into \"Fundamental\" mode."
  "Put the current buffer into \"Fundamental\" mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Fundamental"))

;;;
;;; Text mode.
;;;

(defmode "Text" :major-p t)

(defcommand "Text Mode" (p)
  "Put the current buffer into \"Text\" mode."
  "Put the current buffer into \"Text\" mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Text"))

;;;
;;; Caps-lock mode.
;;;

(defmode "CAPS-LOCK")

(defcommand "Caps Lock Mode" (p)
  "Simulate having a CAPS LOCK key.  Toggle CAPS-LOCK mode.  Zero or a
   negative argument turns it off, while a positive argument turns it
   on."
  "Simulate having a CAPS LOCK key.  Toggle CAPS-LOCK mode.  Zero or a
   negative argument turns it off, while a positive argument turns it
   on."
  (setf (buffer-minor-mode (current-buffer) "CAPS-LOCK")
	(if p
	    (plusp p)
	    (not (buffer-minor-mode (current-buffer) "CAPS-LOCK")))))

(defcommand "Self Insert Caps Lock" (p)
  "Insert the last character typed, or the argument number of them.
   If the last character was an alphabetic character, then insert its
   capital form."
  "Insert the last character typed, or the argument number of them.
   If the last character was an alphabetic character, then insert its
   capital form."
  (let ((char (char-upcase (ext:key-event-char *last-key-event-typed*))))
    (if (and p (> p 1))
	(insert-string (current-point) (make-string p :initial-element char))
	(insert-character (current-point) char))))
