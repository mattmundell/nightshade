;;; Command level support for processes.

(in-package "ED")

(file-comment "Test file comment for e:shell.lisp.")

#[ Process Mode

`Process' mode allows the user to execute a Unix process within an editor
buffer.  These commands and default bindings cater to running Unix shells
yin buffers.  For example, `Stop Buffer Subprocess' is bound to H-z to stop
the process you are running in the shell instead of binding `Stop Main
Process' to this key which would stop the shell itself.

{command:Shell}
{evariable:Shell Utility}
{evariable:Shell Utility Switches}
{evariable:Current Shell}
{evariable:Ask about Old Shells}
{command:Shell Command Line in Buffer}
{command:Saving Shell Command Line in Buffer}
{command:Set Current Shell}
{command:Stop Main Process}
{command:Continue Main Process}
{command:Kill Main Process}
{evariable:Kill Process Confirm}
{command:Stop Buffer Subprocess}
{command:Kill Buffer Subprocess}
{command:Interrupt Buffer Subprocess}
{command:Quit Buffer Subprocess}
{command:Send EOF to Process}
{command:Confirm Process Input}

The user may edit process input using commands that are shared with
`Typescript' mode, as described in [typescripts].
]#

(defun setup-process-buffer (buffer)
  (let ((mark (copy-mark (buffer-point buffer) :right-inserting)))
    (defevar "Buffer Input Mark"
      "The buffer input mark for this buffer."
      :buffer buffer
      :value mark)
    (defevar "Process Output Stream"
      "The process structure for this buffer."
      :buffer buffer
      :value (make-editor-output-stream mark :full))
    (defevar "Interactive History"
      "A ring of the regions input to an interactive mode (Eval or Typescript)."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))
    (defevar "Interactive Pointer"
      "Pointer into *Interactive History*."
      :buffer buffer
      :value 0)
    (defevar "Searching Interactive Pointer"
      "Pointer into *Interactive History*."
      :buffer buffer
      :value 0)
    (fi (buffer-modeline-field-p buffer :process-status)
	(setf (buffer-modeline-fields buffer)
	      (nconc (buffer-modeline-fields buffer)
		     (list (modeline-field :space)
			   (modeline-field :process-status)))))))

(defmode "Process" :major-p nil :setup-function #'setup-process-buffer)


;;;; Shell-filter streams.

;;; We use shell-filter-streams to capture text going from the shell
;;; process to an editor output stream.  They pass character and misc
;;; operations through to the attached editor-output-stream.  The string
;;; output function scans the string for ^A_____^B, denoting a change of
;;; directory.
;;;
;;; The following aliases in a .cshrc file are required for using filename
;;; completion:
;;;    alias cd 'cd \!* ; echo ""`pwd`"/"'
;;;    alias popd 'popd \!* ; echo ""`pwd`"/"'
;;;    alias pushd 'pushd \!* ; echo ""`pwd`"/"'
;;;

(defstruct (shell-filter-stream
	    (:include sys:lisp-stream
		      (:out #'shell-filter-out)
		      (:sout #'shell-filter-string-out)
		      (:misc #'shell-filter-output-misc))
	    (:print-function print-shell-filter-stream)
	    (:constructor
	     make-shell-filter-stream (buffer editor-stream)))
  ;; The buffer where output will be going
  buffer
  ;; The editor stream to which output will be directed
  editor-stream)

;;; PRINT-SHELL-FILTER-STREAM  -- Internal
;;;
;;; Function for printing a shell-filter-stream.
;;;
(defun print-shell-filter-stream (s stream d)
  (declare (ignore d s))
  (write-string "#<Shell filter stream>" stream))

;;; SHELL-FILTER-OUT -- Internal
;;;
;;; This is the character-out handler for the shell-filter-stream.
;;; It writes the character it is given to the underlying
;;; editor-output-stream.
;;;
(defun shell-filter-out (stream character)
  (write-char character (shell-filter-stream-editor-stream stream)))

;;; SHELL-FILTER-OUTPUT-MISC -- Internal
;;;
;;; This will also simply pass the output request on the attached
;;; editor-output-stream.
;;;
(defun shell-filter-output-misc (stream operation &optional arg1 arg2)
  (let ((editor-stream (shell-filter-stream-editor-stream stream)))
    (funcall (edi::editor-output-stream-misc editor-stream)
	     editor-stream operation arg1 arg2)))

;;; CATCH-CD-STRING -- Internal
;;;
;;; Scans String for the sequence ^A...^B.  Returns as multiple values
;;; the breaks in the string.  If the second start/end pair is nil, there
;;; was no cd sequence.
;;;
(defun catch-cd-string (string start end)
  (declare (simple-string string))
  (let ((cd-start (position (code-char 1) string :start start :end end)))
    (if cd-start
	(let ((cd-end (position (code-char 2) string :start cd-start :end end)))
	  (if cd-end
	      (values start cd-start cd-end end)
	      (values start end nil nil)))
	(values start end nil nil))))

;;; SHELL-FILTER-STRING-OUT -- Internal
;;;
;;; The string output function for shell-filter-stream's.
;;; Any string containing a ^A...^B is caught and assumed to be
;;; the path-name of the new current working directory.  This is
;;; removed from the orginal string and the result is passed along
;;; to the editor stream.
;;;
(defun shell-filter-string-out (stream string start end)
  (declare (simple-string string))
  (let ((editor-stream (shell-filter-stream-editor-stream stream))
	(buffer (shell-filter-stream-buffer stream)))

    (multiple-value-bind (start1 end1 start2 end2)
			 (catch-cd-string string start end)
      (write-string string editor-stream :start start1 :end end1)
      (when start2
	(write-string string editor-stream :start (+ 2 start2) :end end2)
	(let ((cd-string (subseq string (1+ end1) start2)))
	  (setf (variable-value 'current-working-directory :buffer buffer)
		(pathname cd-string)))))))

;;; FILTER-TILDES -- Internal
;;;
;;; Since COMPLETE-FILE does not seem to deal with ~'s in the filename
;;; this function expands them to a full path name.
;;;
;;; FIX correctly deal with cases like "~ram/"
;;;
(defun filter-tildes (name)
  (declare (type (or simple-string null) name))
  (if (and name
	   (> (length name) 0)
	   (char= (schar name 0) #\~))
      (concatenate 'simple-string
		   (if (or (= (length name) 1)
			   (char= (schar name 1) #\/))
		       (cdr (assoc :home *environment-list*))
		       "/usr/")
		 (subseq name 1))
      name))


;;;; Support for handling input before the prompt in process buffers.

(defun unwedge-process-buffer ()
  (buffer-end (current-point))
  (deliver-signal-to-process :SIGINT (value process))
  (editor-error "Aborted."))

(defevar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-process-buffer
  :mode "Process")

(defevar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Interrupt and throw to end of buffer? "
  :mode "Process")


;;;; Some Global Variables.

(defevar "Current Shell"
  "The shell to which `Select Shell' goes.")

(defevar "Ask about Old Shells"
  "When set, prompt for an existing shell buffer in preference to making a
   new one when *Current Shell* is empty."
  :value t)

(defevar "Prompt for Shell Buffer Names"
  "When true prompt for buffer names for shell commands, otherwise generate
   name.")

(defevar "Clear Shell Buffers"
  "When true clear the destination buffer before running a shell command."
  :value t)

(defevar "Kill Process Confirm"
  "When set, the editor prompts for confirmation before killing a buffer's
   process."
  :value t)

(defevar "Shell Utility"
  "The `Shell' command uses this as the default command line."
  :value "/bin/csh")

(defevar "Shell Utility Switches"
  "This is a string containing the default command line arguments to the
   utility in *Shell Utility*.  This is a string since the utility is
   typically \"/bin/csh\", and this string can contain I/O redirection and
   other shell directives."
  :value "")


;;;; The Shell, New Shell, and Set Current Shell Commands.

(defvar *shell-names* (make-string-table)
  "A string-table of the string-name of all process buffers and corresponding
   buffer structures.")

(defcommand "Set Current Shell" ()
  "Set the value of *Current Shell*, which the `Shell' command uses."
  (set-current-shell))

;;; SET-CURRENT-SHELL -- Internal.
;;;
;;; This prompts for a known shell buffer to which it sets "Current Shell".
;;; It signals an error if there are none.
;;;
(defun set-current-shell ()
  (let ((old-buffer (value current-shell))
	(first-old-shell (do-strings (var val *shell-names* nil)
			   (declare (ignore val))
			   (return var))))
    (or old-buffer
	first-old-shell
	(editor-error "Need something to set current shell to."))
    (let ((default-shell (if old-buffer
			     (buffer-name old-buffer)
			     first-old-shell)))
      (multiple-value-bind
	  (new-buffer-name new-buffer)
	  (prompt-for-keyword (list *shell-names*)
			      :must-exist t
			      :default default-shell
			      :default-string default-shell
			      :prompt "Existing Shell: "
			      :help "Enter the name of an existing shell.")
	(declare (ignore new-buffer-name))
	(setf (value current-shell) new-buffer)))))

(defcommand "Shell" (p)
  "Switch to a shell, spawning one if required.

   `Current Shell' holds the current shell buffer.

   If `Current Shell' is set, go to that buffer.  Otherwise if there are
   shell buffers prompt for one of them make current.  Otherwise spawn the
   program in `Shell Utility' passing it `Shell Utility Switches' in a new
   buffer named \"Shell N\" where N is some distinguishing integer.

   With an argument create a new shell buffer in any case."
  (let ((shell (value current-shell))
	(no-shells-p (do-strings (var val *shell-names* t)
		       (declare (ignore var val))
		       (return ()))))
    (cond (p (make-new-shell () no-shells-p))
	  (shell (change-to-buffer shell))
	  ((and (value ask-about-old-shells) (fi no-shells-p))
	   (set-current-shell)
	   (change-to-buffer (value current-shell)))
	  (t (make-new-shell ())))))

(defvar *shell-command-in-buffer-history* (make-ring 350)
  "Shell commands previously put into Shell Command Line in Buffer.")

(defvar *shell-command-in-buffer-history-pointer* 0
  "Current position during a historical exploration.")

(defcommand "Shell Command Line in Buffer" (p)
  "Run a prompted shell command.  Prompt for a buffer for the output if
   Prompt for Shell Buffer Names is true.  With a prefix clear the
   destination buffer before running the command."
  (make-new-shell t () () :clear-buffer (if (value clear-shell-buffers)
					    (fi p) p)))

(defcommand "Saving Shell Command Line in Buffer" (p)
  "Save all files, then run a prompted shell command.  Prompt for a buffer
   for the output if Prompt for Shell Buffer Names is true.  With a prefix
   clear the destination buffer before running the command."
  (elet ((save-all-files-confirm ()))
    (save-all-files-command))
  (shell-command-line-in-buffer-command p))

;;; MAKE-NEW-SHELL -- Internal.
;;;
(defun make-new-shell (prompt-for-command-p &optional (set-current-shell-p t)
		       (command-line (get-command-line))
		       &key clear-buffer proposed-command)
  "Make a new shell, dealing with prompting for various things and setting
   *Current Shell*."
  (let* ((command (or command-line
		      (if prompt-for-command-p
			  (prompt-for-string
			   :default (or command-line proposed-command)
			   :trim t
			   :prompt "Command to execute: "
			   :help "Shell command line to execute."
			   :history *shell-command-in-buffer-history*
			   :history-pointer
			   '*shell-command-in-buffer-history-pointer*)
			  command-line)))
	 (buffer-name (if prompt-for-command-p
			  (let ((name (new-process-buffer-name command
#|
				       (if (> (length command) 30)
					   (concatenate 'simple-string
							(subseq command 0 26)
							"...")
					   command)
|#
				       )))
			    (if (value prompt-for-shell-buffer-names)
				(prompt-for-string
				 :default name
				 :trim t
				 :prompt `("Buffer in which to execute ~A? "
					   ,command)
				 :help "Where output from this process will appear.")
				name))
			  (new-shell-name)))
	 (temp (make-buffer
		  buffer-name
		  :modes '("Fundamental" "Process")
		  :delete-hook
		  (list #'(lambda (buffer)
			    (when (eq (value current-shell) buffer)
			      (setf (value current-shell) nil))
			    (delete-string (buffer-name buffer) *shell-names*)
			    (if (editor-bound-p 'process :buffer buffer)
				(let ((process (variable-value 'process
							       :buffer buffer)))
				  (when process
				    (kill-process process))))))))
	 (buffer (or temp (getstring buffer-name *buffer-names*)))
	 (stream (variable-value 'process-output-stream :buffer buffer))
	 (output-stream
	  ;; If we re-used an old shell buffer, this isn't necessary.
	  (if (editor-output-stream-p stream)
	      (setf (variable-value 'process-output-stream :buffer buffer)
		    (make-shell-filter-stream buffer stream))
	      stream)))
    (if clear-buffer (delete-region (buffer-region buffer)))
    (buffer-end (buffer-point buffer))
    ;; Check that the current directory matches the buffer directory.
    (or (let ((buffer-file
	       (if (editor-bound-p 'dired-information)
		   (dired-info-pathname (value dired-information))
		   (directory-namestring
		    (or (buffer-pathname (current-buffer))
			(value pathname-defaults))))))
	  (and (probe-file buffer-file)
	       (string= (namestring (truename (current-directory)))
			(namestring (truename buffer-file)))))
	(editor-error
	 "Safety assertion failed: Current directory: ~A~%                         Buffer directory : ~A"
	 (truename (current-directory))
	 (if (editor-bound-p 'dired-information)
	     (dired-info-pathname (value dired-information))
	     (directory-namestring
	      (or (buffer-pathname (current-buffer))
		  (value pathname-defaults))))))
    (in-directory (setf (buffer-pathname buffer)
			(directory-namestring
			 (or (current-directory)
			     (value pathname-defaults))))
      (defevar "Update Process Buffer Quietly"
	"If true suppress messages in `update-process-buffer'."
	:buffer buffer)
      (defevar "Process"
	"The process for Shell and Process buffers."
	:buffer buffer
	:value (ext::run-program "/bin/sh" (list "-c" command)
				 :wait nil
				 :pty output-stream
				 :env (frob-environment-list
				       (car (buffer-windows buffer))
				       (directory-namestring
					(buffer-pathname buffer)))
				 :status-hook #'(lambda (process)
						  (declare (ignore process))
						  (update-process-buffer buffer))
				 :input t :output t)))
    (let ((exp `(ext::run-program "/bin/sh" ',(list "-c" command)
				  :wait nil
				  :pty ,output-stream
				  :env ',(frob-environment-list
					  (car (buffer-windows buffer))
					  (directory-namestring
					   (buffer-pathname buffer)))
				  :status-hook #'(lambda (process)
						   (declare (ignore process))
						   (update-process-buffer ,buffer))
				  :input t :output t)))
      (if (editor-bound-p 'process-start-expression)
	  (setv process-start-expression exp)
	  (defevar "Process Start Expression"
	    "The expression called to start the process."
	    :buffer buffer
	    :value exp)))
    (defevar "Current Working Directory"
      "The pathname of the current working directory for this buffer."
      :buffer buffer
      :value (buffer-pathname buffer))
    (update-process-buffer buffer)
    (setf (getstring buffer-name *shell-names*) buffer)
    (or (value current-shell)
	(if set-current-shell-p (setf (value current-shell) buffer)))
    (change-to-buffer buffer)))

;;; GET-COMMAND-LINE -- Internal.
;;;
(defun get-command-line ()
  "Cons up a string to feed to the shell."
  (concatenate 'simple-string (value shell-utility) " "
	       (value shell-utility-switches)))

;;; FROB-ENVIRONMENT-LIST -- Internal.
;;;
(defun frob-environment-list (window pwd)
  "Set up some environment variables so the shell will be in the proper
   state when it comes up."
  (let ((l
	 (list* (cons :termcap  (concatenate 'simple-string
				      "emacs:co#"
				      (if window
					  (lisp::quick-integer-to-string
					   (window-width window))
					  "")
				      ":tc=unkown:"))
;		(cons :emacs "t") (cons :term "emacs")
		(cons :term "dumb")
		(remove-if #'(lambda (keyword)
			(member keyword '(:termcap :emacs :term)
				:test #'(lambda (cons keyword)
					  (eql (car cons) keyword))))
			   ext:*environment-list*))))
    (if pwd (setf (cdr (assoc :pwd l)) pwd))
    l))

(defvar l '((a . b) (c . d)))
(setf (cdr (assoc 'a l)) 'e)

;;; NEW-SHELL-NAME -- Internal.
;;;
;;; This returns a unique buffer name for a shell by incrementing the value of
;;; *process-number* until "Process <*process-number*> is not already the name
;;; of a buffer.  Perhaps this is being overly cautious, but FIX I've seen some
;;; really stupid users.
;;;
(defvar *process-number* 0)
;;;
(defun new-shell-name ()
  (loop
    (let ((buffer-name (format nil "Shell ~D" (incf *process-number*))))
      (unless (getstring buffer-name *buffer-names*) (return buffer-name)))))

;; FIX much like ~unique-buffer-name
(defun new-process-buffer-name (buffer-name &optional (number 0))
  "Return the name of the first available process buffer where the name
   starts with $buffer-name."
  (if (getstring buffer-name *buffer-names*)
      (let ((buffer (getstring buffer-name *buffer-names*)))
	(if (and (buffer-minor-mode buffer "Process")
		 (editor-bound-p 'process :buffer buffer)
		 (let ((process (variable-value 'process :buffer buffer)))
		   (eq (ext:process-status process) :running)))
	    (new-process-buffer-name (format () "~A ~D" buffer-name (1+ number)))
	    buffer-name))
      buffer-name))


;;;; Modeline support.

(defun modeline-process-status (buffer window)
  (declare (ignore window))
  (when (editor-bound-p 'process :buffer buffer)
    (let ((process (variable-value 'process :buffer buffer)))
      (ecase (ext:process-status process)
	(:running "running")
	(:stopped "stopped")
	(:signaled "killed by signal ~D" (unix:unix-signal-name
					  (ext:process-exit-code process)))
	(:exited (format nil "exited with status ~D"
			 (ext:process-exit-code process)))))))

(make-modeline-field :name :process-status :replace t
		     :function #'modeline-process-status)

(defvar *update-process-buffer-quietly* ()
  "If t suppress messages in `update-process-buffer'.")

(defun update-process-buffer (buffer)
  (if (buffer-modeline-field-p buffer :process-status)
      (dolist (window (buffer-windows buffer))
	(update-modeline-field buffer window :process-status)))
  (if (editor-bound-p 'process :buffer buffer)
      (let ((process (variable-value 'process :buffer buffer)))
	(unless (ext:process-alive-p process)
	  (or (variable-value 'update-process-buffer-quietly
			      :buffer buffer)
	      (let ((code (process-exit-code process)))
		(case code
		  (0 (message "~A finished." (buffer-name buffer)))
		  (t (message "~A exited with status ~A."
			      (buffer-name buffer)
			      code)))))
	  (ext:process-close process)
	  (when (eq (value current-shell) buffer)
	    (setf (value current-shell) ()))))))


;;;; Supporting Commands.

(defun update-shell-buffer (buffer &optional quiet)
  "Rerun the shell command running in $buffer."
  (delete-region (buffer-region buffer))
  (in-directory (buffer-pathname buffer)
    (setf (variable-value 'process :buffer buffer)
	  (eval (variable-value 'process-start-expression
				:buffer buffer)))))

(defcommand "Update Shell Buffer" ()
  "Update a `Process Mode' buffer."
  (let ((buffer (current-buffer)))
    (or (editor-bound-p 'process :buffer buffer)
	(editor-error "Current buffer must be in Process mode."))
    (when (prompt-for-y-or-n
	   :prompt (format nil "Rerun command? ")
	   :help "Y to turn rerun the compile."
	   :default ())
      (update-shell-buffer buffer))))

(defcommand "Confirm Process Input" ()
  "Send the text at the end of a process buffer (between the point and the
   last prompt) to the process in that buffer.  Insert the resulting output
   at the end of the process buffer."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (let* ((process (value process))
	 (stream (ext:process-pty process)))
    (case (ext:process-status process)
      (:running)
      (:stopped (editor-error "The process has stopped."))
      (t (editor-error "The process has ended.")))
    (let ((input-region (get-interactive-input)))
      (write-line (region-to-string input-region) stream)
      (force-output (ext:process-pty process))
      (insert-character (current-point) #\newline)
      ;; Move "Buffer Input Mark" to end of buffer.
      (move-mark (region-start input-region) (region-end input-region)))))

(defcommand "Shell Complete Filename" ()
  "Attempt to complete the filename immediately preceding the point.  Beep
   if the result of completion is ambiguous."
  (or (editor-bound-p 'current-working-directory)
      (editor-error "Shell filename completion only works in shells."))
  (let ((point (current-point)))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (or (form-offset start -1) (editor-error "Can't grab filename."))
      (when (member (next-character start) '(#\" #\' #\< #\>))
	(mark-after start))
      (let* ((name-region (region start point))
	     (fragment (filter-tildes (region-to-string name-region)))
	     (shell-dir (value current-working-directory)))
	(multiple-value-bind (filename unique)
			     (in-directory shell-dir
			       (complete-file fragment :directory shell-dir))
	  (cond (filename
		 (delete-region name-region)
		 (insert-string point (namestring filename))
		 (fi unique (editor-error "Ambiguous prefix.")))
		(t (editor-error "No such file exists."))))))))

(defcommand "Kill Main Process" ()
  "Prompt for confirmation and kills the process running in the current
   buffer."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (when (or (not (value kill-process-confirm))
	    (prompt-for-y-or-n :default nil
			       :prompt "Really blow away shell? "
			       :default nil
			       :default-string "no"))
    (kill-process (value process))))

(defcommand "Stop Main Process" (p)
  "Stop the process running in the current buffer by sending a :SIGTSTP to
   that process.  With an argument, stops the process using :SIGSTOP."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (deliver-signal-to-process (if p :SIGSTOP :SIGTSTP) (value process)))

(defcommand "Continue Main Process" ()
  "If the process in the current buffer is stopped, continue it."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (deliver-signal-to-process :SIGCONT (value process)))

(defun kill-process (process)
  "Kill $process."
  (deliver-signal-to-process :SIGKILL process))

(defun deliver-signal-to-process (signal process)
  "Deliver $signal to $process."
  (ext:process-kill process signal :process-group))

(defcommand "Send EOF to Process" ()
  "Send the end of file character to the process in the current buffer,
   similar to the effect of Control-D in a shell."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (let ((stream (ext:process-pty (value process))))
    (write-char (code-char 4) stream)
    (force-output stream)))

(defcommand "Interrupt Buffer Subprocess" ()
  "Interrupt the foreground subprocess of the process in the current
   buffer, similar to the effect of C-C in a shell."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (buffer-end (current-point))
  (buffer-end (value buffer-input-mark))
  (deliver-signal-to-subprocess :SIGINT (value process)))

(defcommand "Kill Buffer Subprocess" ()
  "Kill the foreground subprocess of the process in the current buffer."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (deliver-signal-to-subprocess :SIGKILL (value process)))

(defcommand "Quit Buffer Subprocess" ()
  "Dump the core of the foreground subprocess of the processs in the
   current buffer, similar to the effect of C-\ in a shell."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (deliver-signal-to-subprocess :SIGQUIT (value process)))

(defcommand "Stop Buffer Subprocess" (p)
  "Stop the foreground subprocess of the process in the current buffer,
   similar to the effect of C-Z in a shell."
  (or (editor-bound-p 'process :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (deliver-signal-to-subprocess (if p :SIGSTOP :SIGTSTP) (value process)))

(defun deliver-signal-to-subprocess (signal process)
  "Delivers a signal to a subprocess of a shell."
  (ext:process-kill process signal :pty-process-group))


;;; Proced.

(defun active-processes-p ()
  (do-processes (process buffer)
    (if (process-alive-p process)
	(return-from active-processes-p t))))

(defun list-processes (stream)
  (format stream "PID    Status    Buffer~%")
  (do-processes (process buffer)
    (format stream "~6A ~9A ~A~%"
	    (process-pid process)
	    (process-status process)
	    (buffer-name buffer))))

(defun active-process-p (buffer)
  "Return t if Buffer has an active process."
  (if (buffer-minor-mode buffer "Process")
      (let ((process (variable-value 'ed::process :buffer buffer)))
	(and process (process-alive-p process)))))

(defun list-active-processes (stream)
  (format stream "PID    Buffer~%")
  (do-processes (process buffer)
    (if (process-alive-p process)
	(format stream "~6A ~A~%"
		(process-pid process)
		(buffer-name buffer)))))

(defcommand "List Processes" ()
  "List all processes started in the editor."
  (with-pop-up-display (stream)
    (list-processes stream)))

(defcommand "List Active Processes" ()
  "List all active processes started in the editor."
  (with-pop-up-display (stream)
    (list-active-processes stream)))

(defun end-active-processes ()
  "End any active processes."
  (do-processes (process buffer)
    (process-kill process 9)))

(defun check-for-active-processes ()
  (when (active-processes-p)
    (with-pop-up-window (buffer window :buffer-name "Active Processes")
      (with-output-to-mark (stream (buffer-mark buffer))
	(list-active-processes stream))
      (or (prompt-for-y-or-n
	   :default nil
	   :prompt "End active process(es) and exit? ")
	  (editor-error "Exit cancelled."))
      (end-active-processes))))

(add-hook exit-hook 'check-for-active-processes)
