;;; Run-time program inspector interfaces.


;;;; Inspecting in the editor Lisp.

;; FIX ensure consistent w e:debug.lisp

(in-package "ED")


(defmode "Inspect" :major-p t
  :documentation
  "Run-time program inspector.")

(defcommand "Restart from Error" ()
  "Attempt to restart from an error."
  (when (editor-bound-p 'inspect-condition :buffer (current-buffer))
    (let ((cases (compute-restarts)))
      (declare (list cases))
#|
      (with-pop-up-display (s :height (1+ (length cases)))
	(debug::show-restarts cases s))
|#
      (invoke-restart-interactively
       (nth (prompt-for-integer :prompt "Restart number: ") cases)))))

(defcommand "Restart from Error Self" ()
  "Attempt to restart from the error numbered by the last key typed."
  (when (editor-bound-p 'inspect-condition :buffer (current-buffer))
    (let ((char (ext:key-event-char *last-key-event-typed*)))
      (or char (editor-error "Failed to get last character."))
      (let ((cases (compute-restarts))
	    (number (- (char-code char) (char-code #\0))))
	(declare (list cases))
	(or (plusp (length cases)) (editor-error "Too few restarts."))
	(or (and (>= number 0) (< number (length cases)))
	    (editor-error "Restart number out of range."))
#|
	(with-pop-up-display (s :height (1+ (length cases)))
	  (debug::show-restarts cases s))
|#
	(invoke-restart-interactively (nth number cases))))))

(defcommand "Enter Break Loop" ()
  "Enter a break loop."
  (when (editor-bound-p 'inspect-condition :buffer (current-buffer))
    (let ((condition (value inspect-condition)))
      (with-screen
	(unwind-protect
	    (with-simple-restart
		(continue "Return to editor.")
	      (invoke-debugger condition)))))))
#|
	  (device (edi::device-hunk-device
		   (edi::window-hunk (current-window)))))
      (when condition
	(funcall (edi::device-exit device) device)
	(unwind-protect
	    (with-simple-restart
		(continue "Return to editor debug loop.")
	      (invoke-debugger condition))
	  (funcall (device-init device) device))))))
|#

(defun inspect (buffer condition)
  "Inspect $condition in $buffer."
  (if (editor-bound-p 'inspect-frame :buffer (current-buffer))
      (setv inspect-frame (di:top-frame))
      (defevar "Inspect Frame"
	"Current frame of inspection."
	:buffer buffer
	:value (di:top-frame)))
  (if (editor-bound-p 'inspect-condition :buffer (current-buffer))
      (setv inspect-condition condition)
      (defevar "Inspect Condition"
	"The condition under inspection."
	:buffer buffer
	:value condition))
  (if (editor-bound-p 'inspect-show-backtrace :buffer (current-buffer))
      (setv inspect-show-backtrace ())
      (defevar "Inspect Show Backtrace"
	"If true inspection shows backtrace."
	:buffer buffer
	:value ()))
  (if (editor-bound-p 'inspect-show-locals :buffer (current-buffer))
      (setv inspect-show-locals ())
      (defevar "Inspect Show Locals"
	"If true inspection shows local variables."
	:buffer buffer
	:value ()))
  (refresh-inspection buffer)
  (ed::do-recursive-edit))

(defun refresh-inspection (&optional (buffer (current-buffer)))
  "Update the inspection in BUFFER."
  (with-writable-buffer (buffer)
    (let ((line-num (count-lines (region (buffer-start-mark buffer)
					 (buffer-point buffer)))))
      (delete-region (buffer-region buffer))
      (with-output-to-mark (*debug-io* (copy-mark (buffer-point buffer)))
	(setf (mark-kind (buffer-point buffer)) :right-inserting)
	(format *debug-io* "~A~%~%" (value inspect-condition))
	(format *debug-io* "Frame: ~A~%~%"
		(di:frame-number (value inspect-frame)))
	(let ((cases (compute-restarts)))
	  (declare (list cases))
	  (debug::show-restarts cases *debug-io*)
	  (terpri *debug-io*))
	(when (value inspect-show-locals)
	  (format *debug-io* "Local Variables:~%")
	  (let ((d-fun (di:frame-debug-function (value inspect-frame))))
	    (if (di:debug-variable-info-available d-fun)
		(let ((*print-level* (or debug:*debug-print-level* *print-level*))
		      (*print-length* (or debug:*debug-print-length* *print-length*))
		      (location (di:frame-code-location (value inspect-frame))))
		  (dolist (v (di:ambiguous-debug-variables d-fun ""))
		    (when (eq (di:debug-variable-validity v location) :valid)
		      (format *debug-io* "~S~:[#~D~;~*~]  =  ~S~%"
			      (di:debug-variable-symbol v)
			      (zerop (di:debug-variable-id v))
			      (di:debug-variable-id v)
			      (di:debug-variable-value v (value inspect-frame))))))))
	  (terpri *debug-io*))
	(if (value inspect-show-backtrace) (debug:backtrace)))
      (setf (buffer-major-mode buffer) "Inspect")
      (setf (buffer-writable buffer) ())
      (buffer-start (buffer-point buffer))
      (line-offset (buffer-point buffer) line-num))))

(defcommand "Forward Frame" ()
  "Move to the next (i.e. older) frame."
  (when (editor-bound-p 'inspect-frame :buffer (current-buffer))
    (setv inspect-frame (di:frame-down (value inspect-frame)))
    (refresh-inspection)))

(defcommand "Backward Frame" ()
  "Move to the next (i.e. older) frame."
  (when (editor-bound-p 'inspect-frame :buffer (current-buffer))
    (setv inspect-frame (di:frame-up (value inspect-frame)))
    (refresh-inspection)))

(defcommand "Edit Frame Source" ()
  "Edit the source of the frame at point."
  (when (editor-bound-p 'inspect-condition :buffer (current-buffer))
    (let ((frame-num (parse-integer (line-string (mark-line (current-point)))
				    :junk-allowed t)))
      (when frame-num
	(let ((frame (di:top-frame)))
	  (msg "fn ~A" frame-num)
	  (loop repeat (1+ frame-num) do (setq frame (di:frame-down frame))
	    (msg "."))
	  (msg "frame num ~A" (di:frame-number frame))
	  (msg "frame ~A" frame)
	  (setv inspect-frame frame)

;	  (refresh-inspection)

	  (let* ((location (debug::maybe-block-start-location
			    (di:frame-code-location frame)))
		 (d-source (di:code-location-debug-source location))
		 (name (di:debug-source-name d-source)))
	    (msg "from ~A" (di:debug-source-from d-source))
	    (msg "name ~A" name)
	    (case (di:debug-source-from d-source)
	      (:file
	       (let* ((tlf-offset (di:code-location-top-level-form-offset location))
		      (local-tlf-offset (- tlf-offset
					   (di:debug-source-root-number d-source)))
		      (char-offset (aref (or (di:debug-source-start-positions d-source)
					     (error "No start positions map."))
					 local-tlf-offset)))
		 (progn ;let ((*debug-editor-source-data*))
		   (msg "~A" (namestring name))
		   (ed::edit-source-location (namestring name)
					     (di:debug-source-created d-source)
					     tlf-offset local-tlf-offset char-offset
					     (di:code-location-form-number location)))))
	      ((:lisp :stream)
	       (message "Frame function defined from a stream or the eval loop."))
	      (t
	       (message "Weird frame function source."))))

	  )))))

(defcommand "Step Forward" (p)
  "Step forward in the program."
  (setf debug::*number-of-steps* (or p 1))
  (debug::set-step-breakpoint (value inspect-frame))
  (continue (value inspect-condition)))

(defcommand "Inspect Refresh" ()
  "Move to the next (i.e. older) frame."
  (when (editor-bound-p 'inspect-frame :buffer (current-buffer))
    (refresh-inspection)))

(defcommand "Inspect Show Backtrace" ()
  "Toggle display of the backtrace."
  (when (editor-bound-p 'inspect-frame :buffer (current-buffer))
    (setv inspect-show-backtrace (fi (value inspect-show-backtrace)))
    (refresh-inspection)))

(defcommand "Inspect Show Locals" ()
  "Toggle display of the local variables."
  (when (editor-bound-p 'inspect-frame :buffer (current-buffer))
    (setv inspect-show-locals (fi (value inspect-show-locals)))
    (refresh-inspection)))


;;;; Inspecting in a slave environment.


;;; Commands for sending inspector commands to slaves that are in the
;;; inspector.

#[ Inspecting Running Programs

These commands manipulate the slave when it is in the program inspector and
provide source editing based on the current inspector frame.  These all
affect the `Current Eval Server'.

[ Changing Frames              ]
[ Getting out of the Inspector ]
[ Getting Information          ]
[ Editing Sources              ]
[ Miscellaneous                ]
]#


;;;; DEFINE-INSPECTOR-COMMAND.

(defmacro define-inspector-command (name doc cmd &key uses-argument)
  `(defcommand ,(concatenate 'simple-string "Inspect " name) (p)
     ,doc ,doc
     ,@(if uses-argument
	   nil
	   '((declare (ignore p))))
     (let* ((server-info (get-current-eval-server t))
	    (wire (server-info-wire server-info)))
       (wire:remote wire
	 (ts-stream-accept-input
	  (ts-data-stream (server-info-slave-info server-info))
	  ,(if uses-argument
	       `(list ,cmd p)
	       cmd)))
       (wire:wire-force-output wire))))


;;;; Frame changing commands.

#[ Changing Frames

{command:Inspect Down}
{command:Inspect Up}
{command:Inspect Top}
{command:Inspect Bottom}
{command:Inspect Frame}
]#

(define-inspector-command "Up"
  "Move the *Current Eval Server* up one inspector frame."
  :up)

(define-inspector-command "Down"
  "Move the *Current Eval Server* down one inspector frame."
  :down)

(define-inspector-command "Top"
  "Move the *Current Eval Server* to the top of the inspection stack."
  :top)

(define-inspector-command "Bottom"
  "Move the *Current Eval Server* to the bottom of the inspection stack."
  :bottom)

(define-inspector-command "Frame"
  "Move the *Current Eval Server* to the absolute inspector frame number
   indicated by the prefix argument."
  :frame
  :uses-argument t)


;;;; In and Out commands.

#[ Getting out of the Inspector

{command:Inspect Quit}
{command:Inspect Go}
{command:Inspect Abort}
{command:Inspect Restart}
]#

(define-inspector-command "Quit"
  "In the *Current Eval Server*, throw to the top level out of the
   inspector."
  :quit)

(define-inspector-command "Go"
  "In the *Current Eval Server*, try the CONTINUE restart."
  :go)

(define-inspector-command "Abort"
  "In the *Current Eval Server*, execute the previous ABORT restart."
  :abort)

(define-inspector-command "Restart"
  "In the *Current Eval Server*, executes the restart indicated by the
   prefix argument.  The inspector enumerates the restart cases upon
   initially."
  :restart
  :uses-argument t)


;;;; Information commands.

#[ Getting Information

{command:Inspect Help}
{command:Inspect Error}
{command:Inspect Backtrace}
{command:Inspect Print}
{command:Inspect Verbose Print}
{command:Inspect Source}
{command:Inspect Verbose Source}
{command:Inspect List Locals}
]#

(define-inspector-command "Help"
  "In the *Current Eval Server*, print the inspector help text."
  :help)

(define-inspector-command "Error"
  "In the *Current Eval Server*, print the error condition and restart
   cases displayed upon entering the inspector."
  :error)

(define-inspector-command "Backtrace"
  "Execute the inspector BACKTRACE command."
  :backtrace)

(define-inspector-command "Print"
  "In the *Current Eval Server*, print a representation of the current
   inspector frame."
  :print)

(define-inspector-command "Verbose Print"
  "In the *Current Eval Server*, print a verbose representation of the
   current inspector frame."
  :vprint)

(define-inspector-command "List Locals"
  "In the *Current Eval Server*, print the local variables for the
   current inspector frame."
  :list-locals)

(define-inspector-command "Source"
  "In the *Current Eval Server*, print the source form for the current
   inspector frame."
  :source)

(define-inspector-command "Verbose Source"
  "In the *Current Eval Server*, print the source form for the current
   inspector frame with surrounding forms for context."
  :vsource)


;;;; Source editing.

#[ Editing Sources

{command:Inspect Edit Source}
]#

;;; "Inspect Edit Source" -- Command.
;;;
;;; The :edit-source command in the slave inspector initiates a synchronous RPC
;;; into the editor via the wire in *terminal-io*, a typescript stream.  This
;;; routine takes the necessary values, a file and source-path, and changes the
;;; editor's state to display that location.
;;;
;;; This command has to wait on SERVE-EVENT until some special is set by the
;;; RPC routine saying it is okay to return to the editor's top level.
;;;
(defvar *inspect-editor-source-data* ())
(defvar *in-inspect-edit-source* ())

(defcommand "Inspect Edit Source" ()
  "Given the current inspector frame in the *Current Eval Server*, go to
   the source of the frame location source in the editor, if possible, else
   beep."
  (let* ((server-info (get-current-eval-server t))
	 (wire (server-info-wire server-info)))
    ;;
    ;; Tell the slave to tell the editor some source info.
    (wire:remote wire
      (ts-stream-accept-input
       (ts-data-stream (server-info-slave-info server-info))
       :edit-source))
    (wire:wire-force-output wire)
    ;;
    ;; Wait for the source info.
    (let ((*inspect-editor-source-data* nil)
	  (*in-inspect-edit-source* t))
      (catch 'blow-inspect-edit-source
	(loop
	  (system:serve-event)
	  (when *inspect-editor-source-data* (return)))))))

;;; EDIT-SOURCE-LOCATION -- Internal Interface.
;;;
;;; The slave calls this in the editor when the inspector gets an
;;; :edit-source command.  This receives the information necessary to take
;;; the user in the editor to the source location, and does it.
;;;
(defun edit-source-location (name source-created-date tlf-offset
			     local-tlf-offset char-offset form-number)
  (let ((pn (pathname name)))
    (or (probe-file pn)
	(editor-error "Source file no longer exists: ~A." name))
    (multiple-value-bind (buffer newp) (find-file-buffer pn)
      (declare (ignore newp))
      (let ((date (buffer-write-date buffer))
	    (point (buffer-point buffer)))
#|
	(when newp
	  ;(push-buffer-mark (copy-mark point) nil))
	  ; FIX should be ~~
	  (setf (mark-kind mark) :right-inserting)
	  (ring-push mark (value buffer-mark-ring)))
|#
	(buffer-start point)
	;;
	;; Get to the top-level form in the buffer.
	(cond ((buffer-modified buffer)
	       (loud-message "Buffer has been modified.  Using form offset ~
			      instead of character position.")
	       (dotimes (i local-tlf-offset)
		 (pre-command-parse-check point)
		 (form-offset point 1)))
	      ((not date)
	       (loud-message "Cannot compare write dates.  Assuming source ~
			      has not been modified -- ~A."
			     name)
	       (character-offset point char-offset))
	      ((= source-created-date date)
	       (character-offset point char-offset))
	      (t
	       (loud-message "File has been modified since reading the source.  ~
			      Using form offset instead of character position.")
	       (dotimes (i local-tlf-offset)
		 (pre-command-parse-check point)
		 (form-offset point 1))))
	;;
	;; Read our form, get form-number translations, get the source-path,
	;; and make it usable.
	;;
	;; NOTE: Here READ is used in the editor lisp to look at a form
	;; that the compiler has digested in the slave lisp. The editor
	;; does not have the same environment as the slave so bad things
	;; can happen if READ hits a #. reader macro (like unknown package
	;; or undefined function errors) which can break the editor. This
	;; code basically inhibits the read-time eval. This doesn't always
	;; work right as the compiler may be seeing a different form structure
	;; and the compiler's version of PATH may not match the editor's.
	;; The main trouble seen in testing is that the 'form-number'
	;; supplied by the compiler was one more than what the vector
	;; returned by form-number-translations contained. For lack of a
	;; better solution, I (pw) just limit the form-number to legal range.
	;; This has worked ok on test code but may be off for some
	;; forms. At least the editor won't break.

	(let* ((vector (di:form-number-translations
			(with-input-from-region
			    (s (region point (buffer-end-mark buffer)))
			  (let ((*read-suppress* t))
			    (read s)))
			tlf-offset))
	       ;; Don't signal error on index overrun.It may be due
	       ;; to read-time eval getting form editing blind to
	       ;; editor
	       (index (min form-number (1- (length vector))))
	       (path (nreverse (butlast (cdr (svref vector index))))))
	  ;;
	  ;; Walk down to the form.  Change to buffer in case we get an error
	  ;; while finding the form.
	  (change-to-buffer buffer)
	  (mark-to-inspect-source-path point path)))))
  (setf *inspect-editor-source-data* t)
  ;;
  ;; While the editor was setting up the source edit, the user could have
  ;; typed while looking at a buffer no longer current when the commands
  ;; execute.
  (clear-editor-input *editor-input*))

;;; CANNOT-EDIT-SOURCE-LOCATION -- Interface.
;;;
;;; The slave calls this when the inspector command "EDIT-SOURCE" runs, and
;;; the slave cannot give the editor source information.
;;;
(defun cannot-edit-source-location ()
  (loud-message "Can't edit source.")
  (when *in-inspect-edit-source*
    (throw 'blow-inspect-edit-source nil)))


;;;; Breakpoints.

;;;
;;; Breakpoint information for editor management.
;;;

;;; This holds all the stuff we might want to know about a breakpoint in some
;;; slave.
;;;
(defstruct (breakpoint-info (:print-function print-breakpoint-info)
			    (:constructor make-breakpoint-info
					  (slave buffer remote-object name)))
  (slave nil :type server-info)
  (buffer nil :type buffer)
  (remote-object nil :type wire:remote-object)
  (name nil :type simple-string))
;;;
(defun print-breakpoint-info (obj str n)
  (declare (ignore n))
  (format str "#<Breakpoint-Info for ~S>" (breakpoint-info-name obj)))

(defvar *breakpoints* nil)

(macrolet ((frob (name accessor)
	     `(defun ,name (key)
		(let ((res nil))
		  (dolist (bpt-info *breakpoints* res)
		    (when (eq (,accessor bpt-info) key)
		      (push bpt-info res)))))))
  (frob slave-breakpoints breakpoint-info-slave)
  (frob buffer-breakpoints breakpoint-info-buffer))

(defun delete-breakpoints-buffer-hook (buffer)
  (let ((server-info (value current-eval-server)))
    (when server-info
      (let ((bpts (buffer-breakpoints buffer))
	    (wire (server-info-wire server-info)))
	  (dolist (b bpts)
	    (setf *breakpoints* (delete b *breakpoints*))
	    (when wire
	      (wire:remote wire
		(di:delete-breakpoint (breakpoint-info-remote-object b))))
	(when wire
	  (wire:wire-force-output wire)))))))
;;;
(add-hook delete-buffer-hook 'delete-breakpoints-buffer-hook)

;;;
;;; Setting breakpoints.
;;;

;;; "Inspect Breakpoint" uses this to prompt for :function-end and
;;; :function-start breakpoints.
;;;
(defvar *function-breakpoint-strings*
  (make-string-table :initial-contents
		     '(("Start" . :function-start) ("End" . :function-end))))
;;;
;;; Maybe this should use the wire level directly and hold onto
;;; remote-objects identifying the breakpoints.  Then we could write
;;; commands to show where the breakpoints were and to individually
;;; deactivate or delete them.  As it is now we probably have to delete all
;;; for a given function.  What about setting user supplied breakpoint
;;; hook-functions, or the editor supplying a nice set such as something to
;;; simply print all locals at a certain location.
;;;
(defcommand "Inspect Breakpoint" (p)
  "This tries to set a breakpoint in the *Current Eval Server* at the
   location designated by the current point.  If there is no known code
   location at the point, then this moves the point to the closest location
   before the point.  With an argument, this sets a breakpoint at the start
   or end of the function, prompting the user for which one to use."
  "This tries to set a breakpoint in the *Current Eval Server* at the
   location designated by the current point.  If there is no known code
   location at the point, then this moves the point to the closest location
   before the point.  With an argument, this sets a breakpoint at the start
   or end of the function, prompting the user for which one to use."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (let ((name (find-defun-for-breakpoint point)))
      (if p
	  (multiple-value-bind (str place)
			       (prompt-for-keyword
				(list *function-breakpoint-strings*)
				:prompt "Set breakpoint at function: "
				:default :start :default-string "Start")
	    (declare (ignore str))
	    (set-breakpoint-in-slave (get-current-eval-server t) name place))
	  (let* ((path (find-path-for-breakpoint point))
		 (server-info (get-current-eval-server t))
		 (res (set-breakpoint-in-slave server-info name path)))
	    (cond ((not res)
		   (message "No code locations correspond with point."))
		  ((wire:remote-object-p res)
		   (push (make-breakpoint-info server-info (current-buffer)
					       res name)
			 *breakpoints*)
		   (message "Breakpoint set."))
		  (t
		   (resolve-ambiguous-breakpoint-location server-info
							  name res))))))))

;;; FIND-PATH-FOR-BREAKPOINT -- Internal.
;;;
;;; This walks up from point to the beginning of its containing DEFUN to return
;;; the pseudo source-path (no form-number, no top-level form offset, and in
;;; descent order from start of the DEFUN).
;;;
(defun find-path-for-breakpoint (point)
  (with-mark ((m point)
	      (end point))
    (let ((path nil))
      (top-level-offset end -1)
      (with-mark ((containing-form m))
	(loop
	  (when (mark= m end) (return))
	  (backward-up-list containing-form)
	  (do ((count 0 (1+ count)))
	      ((mark= m containing-form)
	       ;; Count includes moving from the first form inside the
	       ;; containing-form paren to the outside of the containing-form
	       ;; paren -- one too many.
	       (push (1- count) path))
	    (form-offset m -1))))
      path)))

;;; SET-BREAKPOINT-IN-SLAVE -- Internal.
;;;
;;; This tells the slave to set a breakpoint for name.  Path is a modified
;;; source-path (with no form-number or top-level-form offset) or a symbol
;;; (:function-start or :function-end).  If the server dies while evaluating
;;; form, then this signals an editor-error.
;;;
(defun set-breakpoint-in-slave (server-info name path)
  (when (server-info-notes server-info)
    (editor-error "Server ~S is currently busy.  See `List Operations'."
		  (server-info-name server-info)))
  (multiple-value-bind (res error)
		       (wire:remote-value (server-info-wire server-info)
			 (di:set-breakpoint-for-editor (value current-package)
						       name path))
    (when error (editor-error "The server died before finishing."))
    res))

;;; RESOLVE-AMBIGUOUS-BREAKPOINT-LOCATION -- Internal.
;;;
;;; This helps the user select an ambiguous code location for "Inspect
;;; Breakpoint".
;;;
(defun resolve-ambiguous-breakpoint-location (server-info name locs)
  (declare (list locs))
  (let ((point (current-point))
	(loc-num (length locs))
	(count 1)
	(cur-loc locs))
    (flet ((show-loc ()
	     (top-level-offset point -1)
	     (mark-to-inspect-source-path point (cdar cur-loc))))
      (show-loc)
      (command-case (:prompt `("Ambiguous location ~D of ~D: " ,count ,loc-num)
		      :help "Pick a location to set a breakpoint."
		      :change-window nil)
	(#\space "Move point to next possible location."
	  (setf cur-loc (cdr cur-loc))
	  (cond (cur-loc
		 (incf count))
		(t
		 (setf cur-loc locs)
		 (setf count 1)))
	  (show-loc)
	  (reprompt))
	(:confirm "Choose the current location."
	  (let ((res (wire:remote-value (server-info-wire server-info)
		       (di:set-location-breakpoint-for-editor (caar cur-loc)))))
	    (or (wire:remote-object-p res)
		(editor-error "Couldn't set breakpoint from location?"))
	    (push (make-breakpoint-info server-info (current-buffer) res name)
		  *breakpoints*))
	  (message "Breakpoint set."))))))

;;; MARK-TO-INSPECT-SOURCE-PATH -- Internal.
;;;
;;; This takes a mark at the beginning of a top-level form and modified
;;; inspector source-path.  Path has no form number or top-level-form
;;; offset element, and it has been reversed to actually be usable.
;;;
(defun mark-to-inspect-source-path (mark path)
  (let ((quote-or-function nil))
    (pre-command-parse-check mark)
    (dolist (n path)
      (when quote-or-function
	(editor-error
	 "Apparently settled on the symbol QUOTE or FUNCTION via their ~
	  read macros, which is odd, but furthermore there seems to be ~
	  more source-path left."))
      (or (form-offset mark 1)
	  ;; Want to use the following and delete the next FORM-OFFSET -1.
	  ;; (scan-direction-valid mark t (or :open-paren :prefix))
	  (editor-error
	   "Ran out of text in buffer with more source-path remaining."))
      (form-offset mark -1)
      (ecase (next-character mark)
	(#\(
	 (mark-after mark)
	 (form-offset mark n))
	(#\'
	 (case n
	   (0 (setf quote-or-function t))
	   (1 (mark-after mark))
	   (t (editor-error "Next form is QUOTE, but source-path index ~
			     is other than zero or one."))))
	(#\#
	 (case (next-character (mark-after mark))
	   (#\'
	    (case n
	      (0 (setf quote-or-function t))
	      (1 (mark-after mark))
	      (t (editor-error "Next form is FUNCTION, but source-path ~
				index is other than zero or one."))))
	   (t (editor-error
	       "Can only parse ' and #' read macros."))))))
    ;; Get to the beginning of the form.
    (form-offset mark 1)
    (form-offset mark -1)))

;;;
;;; Deleting breakpoints.
;;;

(defevar "Delete Breakpoints Confirm"
  "This determines whether `Inspect Delete Breakpoints' should ask for
   confirmation before deleting breakpoints."
  :value t)

(defcommand "Inspect Delete Breakpoints" ()
  "Delet all breakpoints for the named DEFUN containing the point.  This
   affects the *Current Eval Server*."
  (let* ((server-info (get-current-eval-server t))
	 (wire (server-info-wire server-info))
	 (name (find-defun-for-breakpoint (current-point)))
	 (bpts (slave-breakpoints server-info)))
    (cond ((not bpts)
	   (message "No breakpoints recorded for ~A." name))
	  ((or (not (value delete-breakpoints-confirm))
	       (prompt-for-y-or-n :prompt `("Delete breakpoints for ~A? " ,name)
				  :default t
				  :default-string "Y"))
	   (dolist (b bpts)
	     (when (string= name (breakpoint-info-name b))
	       (setf *breakpoints* (delete b *breakpoints*))
	       (wire:remote wire
		 (di:delete-breakpoint-for-editor
		  (breakpoint-info-remote-object b)))))
	   (wire:wire-force-output wire)))))

;;;
;;; Breakpoint utilities.
;;;

;;; FIND-DEFUN-FOR-BREAKPOINT -- Internal.
;;;
;;; This returns as a string the name of the DEFUN containing point.  It
;;; signals any errors necessary to ensure "we are in good form".
;;;
(defun find-defun-for-breakpoint (point)
  (with-mark ((m1 point)
	      (m2 point))
    (or (top-level-offset m2 -1)
	(editor-error "Must be inside a DEFUN."))
    ;;
    ;; Check for DEFUN.
    (mark-after (move-mark m1 m2))
    (or (find-attribute m1 :whitespace #'zerop)
	(editor-error "Must be inside a DEFUN."))
    (word-offset (move-mark m2 m1) 1)
    (or (string-equal (region-to-string (region m1 m2)) "defun")
	(editor-error "Must be inside a DEFUN."))
    ;;
    ;; Find name.
    (or (find-attribute m2 :whitespace #'zerop)
	(editor-error "Function unnamed?"))
    (form-offset (move-mark m1 m2) 1)
    (region-to-string (region m2 m1))))


;;;; Miscellaneous commands.

#[ Miscellaneous

{command:Inspect Flush Errors}
]#

(define-inspector-command "Flush Errors"
  "In the *Current Eval Server*, toggle whether the inspector flushes
   errors or recursively enters itself."
  :flush)
