;;; Commands useful when running on a Unix box.

(in-package "ED")


;;;; Region and File printing commands.

#[ Printing

{command:Print Region}
{command:Print Buffer}
{command:Print File}
{evariable:Print Utility}
{evariable:Print Utility Switches}
]#

(defevar "Print Utility"
  "The Unix program the print commands use to send files to the printer.
   The program should act like lpr: if a filename is given as an argument,
   it should print that file, otherwise standard input should be assumed."
  :value "lpr")

(defevar "Print Utility Switches"
  "A list of strings specifying the options to pass to the `Print Utility'.")

;;; PRINT-SOMETHING calls RUN-PROGRAM on the utility-name and args.  Output
;;; and error output are done to the echo area, and errors are ignored for
;;; now.  Run-program-keys are other keywords to pass to RUN-PROGRAM in
;;; addition to :wait, :output, and :error.
;;;
(defmacro print-something (&optional (run-program-keys)
				     (utility-name '(value print-utility))
				     (args '(value print-utility-switches)))
  (let ((pid (gensym))
	(error-code (gensym)))
    `(multiple-value-bind (,pid ,error-code)
			  (ext:run-program ,utility-name ,args
					   ,@run-program-keys
					   :wait t
					   :output *echo-area-stream*
					   :error *echo-area-stream*)
       (declare (ignore ,pid ,error-code))
       (force-output *echo-area-stream*)
       ;; Keep the echo area from being cleared at the top of the command loop.
       (setf (buffer-modified *echo-area-buffer*) nil))))


;;; PRINT-REGION -- Interface
;;;
;;; Takes a region and outputs the text to the program defined by
;;; the hvar "Print Utility" with options form the hvar "Print
;;; Utility Options" using PRINT-SOMETHING.
;;;
(defun print-region (region)
  (with-input-from-region (s region)
    (print-something (:input s))))

(defcommand "Print Buffer" ()
  "Print the current buffer using the program defined by the `Print
   Utility' with the options from the `Print Utility Options'.  Put errors
   in the echo area."
  (message "Printing buffer...~%")
  (print-region (buffer-region (current-buffer))))

(defcommand "Print Region" ()
  "Print the current region using the program defined by `Print Utility'
   with the options from the `Print Utility Options'.  Put errors in the
   echo area."
  (message "Printing region...~%")
  (print-region (current-region)))

(defcommand "Print File" ()
  "Print a prompted file usings the program defined by the `Print Utility'
   with the options from the `Print Utility Options'.  Put errors in the
   echo area."
  (let* ((pn (prompt-for-file :prompt "File to print: "
			      :help "Name of file to print."
			      :default (buffer-default-pathname (current-buffer))
			      :must-exist t))
	 (ns (namestring (truename pn))))
    (message "Printing file...~%")
    (print-something () (value print-utility)
		     (append (value print-utility-switches) (list ns)))))


;;;; Scribe.

#[ Scribe

{command:Scribe Buffer File}
{evariable:Scribe Buffer File Confirm}
{command:Scribe File}
{evariable:Scribe Utility}
{evariable:Scribe Utility Switches}
{command:Select Scribe Warnings}
]#

(defcommand "Scribe File" ()
  "Scribe a prompted file with the default directory set to the directory
   of the specified file.  Send the output from running Scribe to the
   \"Scribe Warnings\" buffer.  Build the Scribe command from *Scribe
   Utility* and *Scribe Utility Switches*.  Before doing anything confirm
   saving and Scribe'ing the file if `Scribe Buffer File Confirm' is set."
  (scribe-file (prompt-for-file :prompt "Scribe file: "
				:default
				(buffer-default-pathname (current-buffer)))))

(defevar "Scribe Buffer File Confirm"
  "When set, `Scribe Buffer File' prompts for confirmation before doing
   anything."
  :value t)

(defcommand "Scribe Buffer File" ()
  "Scribe the file associated with the current buffer.  The default
   directory set to the directory of the file.  Send the output from
   running Scribe to the \"Scribe Warnings\" buffer.  Build the Scribe
   command from `Scribe Utility' and `Scribe Utility Switches'.  Before
   doing anything confirm saving and Scribe'ing the file if `Scribe Buffer
   File Confirm' is set."
  (let* ((buffer (current-buffer))
	 (pathname (buffer-pathname buffer))
	 (modified (buffer-modified buffer)))
    (when (or (not (value scribe-buffer-file-confirm))
	      (prompt-for-y-or-n
	       :default t :default-string "Y"
	       :prompt (list "~:[S~;Save and s~]cribe file ~A? "
			     modified (namestring pathname))))
      (when modified (write-buffer-file buffer pathname))
      (scribe-file pathname))))

(defevar "Scribe Utility"
  "The Unix program the Scribe commands use to compile the text
   formatting."
  :value "scribe")

(defevar "Scribe Utility Switches"
  "A list of strings whose contents would be contiguous characters, other
   than space, had the user invoked this program on a command line outside
   of the editor.  The Scribe commands supply the file to compile in
   addition to these switches.")

(defun scribe-file (pathname)
  (let* ((pathname (truename pathname))
	 (out-buffer (or (getstring "Scribe Warnings" *buffer-names*)
			 (make-buffer "Scribe Warnings")))
	 (out-point (buffer-end (buffer-point out-buffer)))
	 (stream (make-editor-output-stream out-point :line)))
    (buffer-end out-point)
    (insert-character out-point #\newline)
    (insert-character out-point #\newline)
    (in-directory (directory-namestring pathname)
      (ext:run-program (namestring (value scribe-utility))
		       (list* (namestring pathname)
			      (value scribe-utility-switches))
		       :output stream :error stream
		       :wait ()))))


#[ Miscellaneous

{command:Manual Page}
{command:Unix Filter Region}
]#


;;;; Unix Filter Region.

(defcommand "Unix Filter Region" ()
  "Pass the current region to a Unix program as standard input.  The
   standard output from the program is used to replace the region.  This
   command is undo-able."
  (let* ((region (current-region))
	 (filter-and-args (prompt-for-string
			   :prompt "Filter: "
			   :help "Unix program to filter the region through."))
	 (filter-and-args-list (listify-unix-filter-string filter-and-args))
	 (filter (car filter-and-args-list))
	 (args (cdr filter-and-args-list))
	 (new-region (unix-filter-region region filter args))
	 (start (copy-mark (region-start region) :right-inserting))
	 (end (copy-mark (region-end region) :left-inserting))
	 (old-region (region start end))
	 (undo-region (delete-and-save-region old-region)))
    (ninsert-region end new-region)
    (make-region-undo :twiddle "Unix Filter Region" old-region undo-region)))

(defun unix-filter-region (region command args)
  "Pass $region as standard input to the program $command with arguments
   $args and return the standard output as a freshly cons'ed region."
  (let ((new-region (make-empty-region)))
    (with-input-from-region (input region)
      (with-output-to-mark (output (region-end new-region) :full)
	(ext:run-program command args
			 :input input
			 :output output
			 :error output)))
    new-region))

(defun listify-unix-filter-string (str)
  (declare (simple-string str))
  (let ((result nil)
	(lastpos 0))
    (loop
      (let ((pos (position #\Space str :start lastpos :test #'char=)))
	(push (subseq str lastpos pos) result)
	(unless pos
	  (return))
	(setf lastpos (1+ pos))))
    (nreverse result)))


;;;; Environment variables.

(defcommand "Getenv" (p)
  "Print value of prompted environment variable."
  "Print value of prompted environment variable.  To access an environment
   variable in Lisp programs use (assoc :variable *environment-list*)."
  (print-environment-variable-command p))

(defcommand "Print Environment Variable" ()
  "Print value of prompted environment variable."
  (let ((v (prompt-for-string
	    :prompt "Environment variable: "
	    :help "Environment variable of which to print the value (e.g. PATH)."
	    :trim t)))
    (message "~A: ~A"
	     v
	     (cdr (assoc (lisp::make-keyword (intern v))
			 *environment-list*
			 :test #'eq)))))

(defcommand "Setenv" (p variable value)
  "Set value of prompted environment variable."
  (set-environment-variable-command p variable value))

(defcommand "Set Environment Variable" (p variable value)
  "Set value of prompted environment variable."
  (declare (ignore p))
  (let ((var (or variable
		 (prompt-for-string
		  :prompt "Environment variable: "
:help "Environment variable to be printed (e.g. PATH)."
		  :trim t)))
	(val (or value
		 (prompt-for-string
		  :prompt "Value: "
		  :help "Value for environment variable."))))
    (let ((assoc (assoc (lisp::make-keyword (intern var))
			*environment-list*
			:test #'eq)))
      (if assoc
	  (setf (cdr assoc) val)
	  (push (cons (lisp::make-keyword (intern var)) val)
		*environment-list*))
      (message "~A set to ~A." var val))))


;;;; Man pages.

(defun highlight-man-page-line (line chi-info)
  (let ((mark (mark line 0)))
    (if (next-character mark)
	(or (member (next-character mark) '(#\space #\tab))
	    (chi-mark line 0 *variable-font* :variable chi-info)))))

(defun highlight-man-page-buffer (buffer)
  (highlight-chi-buffer buffer highlight-man-page-line))

(defun highlight-visible-man-page-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-man-page-line))

(defun setup-man-page-mode (buffer)
  (highlight-visible-man-page-buffer buffer)
  (pushnew '("Manual" () highlight-visible-man-page-buffer)
	   *mode-highlighters*))

(defmode "Manual" :major-p ()
  :precedence 4.0
  :setup-function #'setup-man-page-mode
  :documentation
  "Manual Page Mode.")

(defcommand "Manual Page from Point" (p)
  "Read the Unix manual pages named at point in a View buffer.  If given an
   argument, put the man page in a Pop-up display."
  (manual-page-command p (or (manual-name-at-point) nil)))

(defcommand "Refresh Manual Page" ()
  "Refresh the Unix manual in the current buffer."
  (let ((topic (value manual-page))
	(local-p (value manual-page-local-p))
	(buffer (current-buffer)))
    (if topic
	(with-writable-buffer (buffer)
	  (let* ((point (current-point))
		 (pos (count-characters (region (buffer-start-mark buffer)
						point))))
	    (delete-region (buffer-region buffer))
	    (with-output-to-mark (s point :full)
	      (execute-man topic s local-p))
	    (buffer-start point buffer)
	    (character-offset point pos))))))

(defcommand "Manual Page" (p page-name)
  "Display a Unix manual page in a buffer in `Manual Page' mode.  When
   given an argument, pop-up the manual page."
  (let* ((page-name (if p
			(prompt-for-file :prompt "Man file: "
					 :must-exist t)
			page-name))
	 (topic (or page-name
		    (prompt-for-string :prompt "Man topic: "
				       :default (manual-name-at-point))))
	 (buf-name (format () "Man Page ~a" topic))
	 (new-buffer (make-buffer buf-name
				  :modes '("Fundamental" "View" "Manual")))
	 (buffer (or new-buffer (getstring buf-name *buffer-names*)))
	 (point (buffer-point buffer)))
    (change-to-buffer buffer)
    (when new-buffer
      (setf (value view-return-function) #'(lambda ()))
      (defevar "Manual Page"
	"The manual page in this buffer."
	:buffer buffer
	:value topic)
      (defevar "Manual Page Local P"
	"Whether this is a local page."
	:buffer buffer
	:value p)
      (with-writable-buffer (buffer)
	(with-output-to-mark (s point :full)
	  (execute-man topic s p))))
    (buffer-start point buffer)))

(defun execute-man (topic stream local-p)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format () "man ~A~A | ul -t adm3"
		 (if local-p "-l " "") topic))
   :output stream))

(defcommand "Next Manual Part" (p)
  "Move to the next part of the Manual Page in the current buffer.
   With a prefix argument move prefix number of parts forwards."
  (or (buffer-minor-mode (current-buffer) "Manual")
      (editor-error "Manual mode must be active in the current buffer."))
  (do ((point (line-start (current-point)))
       (offset (if (and p (minusp p)) -1 1))
       (times (if p (abs p) 1)))
      ((if (line-offset point offset)
	   (if (zerop (character-attribute :word-delimiter
					   (next-character point)))
	       (zerop (decf times)))
	   t))))

(defcommand "Previous Manual Part" (p)
  "Move to the previous part of the Manual Page in the current buffer.
   With a prefix argument move prefix number of parts backwards."
  (next-manual-part-command (if p (- p) -1)))

(defcommand "Next Manual Reference" ()
  "Move to the next reference to a Manual Page in the current buffer."
  (let ((mark (copy-mark (current-point))))
    (if (manual-name-at-point mark) (word-offset mark))
    (mark-after mark)
    (loop while (find-character mark #\() do
      (mark-before mark)
      (let ((name (manual-name-at-point mark)))
	(when name
	  (word-offset mark -1)
	  (move-mark (current-point) mark)
	  (return t)))
      (mark-after (mark-after mark)))))

(defcommand "Previous Manual Reference" ()
  "Move to the previous reference to a Manual Page in the current buffer."
  (let ((mark (copy-mark (current-point))))
    (loop while (reverse-find-character mark #\() do
      (mark-before mark)
      (let ((name (manual-name-at-point mark)))
	(when name
	  (word-offset mark -1)
	  (move-mark (current-point) mark)
	  (return t)))
      (mark-before mark))))


;;;; Apropos.

(defmode "SysApropos" :major-p nil
  :precedence 4.0
  :documentation
  "System Apropos Mode -- query the operating system for manual pages
   related to a word or phrase.")

(defcommand "System Apropos" (p page-name)
  "Unix apropos in a View buffer.
   If given an argument, put the apropos results in a Pop-up display."
  (let ((topic (or page-name
		   (prompt-for-string :prompt "System Apropos: "
				      :default (manual-name-at-point)))))
    (if p
	(with-pop-up-display (stream)
	  (execute-apropos topic stream))
	(let* ((buf-name (format nil "System Apropos ~a" topic))
	       (new-buffer (make-buffer buf-name
					:modes '("Fundamental" "View" "SysApropos")))
	       (buffer (or new-buffer (getstring buf-name *buffer-names*)))
	       (point (buffer-point buffer)))
	  (change-to-buffer buffer)
	  (when new-buffer
	    (setf (value view-return-function) #'(lambda ()))
	    (with-writable-buffer (buffer)
	      (with-output-to-mark (s point :full)
		(execute-apropos topic s))))
	  (buffer-start point buffer)))))

(defun execute-apropos (topic stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format nil "apropos ~a| ul -t adm3" topic))
   :output stream))


;;;; System process listing.

(defevar "List System Processes User"
  "User of which to list processes.")

(defmode "SysProc" :major-p nil
  :precedence 4.0
  :documentation
  "System Process List mode.")

(defun execute-header (stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format nil "uptime"))
   :output stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format nil "free -o"))
   :output stream))

(defun execute-ps (stream &optional user)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (if user
	     (format nil "ps -U ~A -u ~A u" user user)
	     "ps aux"))
   :output stream))

;; FIX do with lisp?
(defun execute-kill (pid signal)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format () "kill -~A ~A" signal pid))))

(defvar *sp-list-buffer* ()
  "The name of the buffer used to list system processes.")

(defun get-sp-list-buffer ()
  "Return the name of the buffer used to list system processes."
  (or *sp-list-buffer*
      (let ((buffer (make-unique-buffer
		     "System Processes"
		     :modes '("Fundamental" "View" "SysProc")
		     :delete-hook
		     (list (lambda ()
			     (setq *sp-list-buffer* ()))))))
	(setf (variable-value 'view-return-function
			      :buffer buffer)
	      #'(lambda ()))
	(defevar "User"
	  "User of processes to list; nil for all users."
	  :value (value list-system-processes-user)
	  :buffer buffer)
	(setq *sp-list-buffer* buffer))))

(defun refresh-system-processes (buffer)
  (let ((line-num (1- (count-lines (region (buffer-start-mark buffer)
					   (buffer-point buffer))))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (delete-line-font-marks (mark-line (current-point)))
      (with-output-to-mark (stream (buffer-point buffer) :full)
	(execute-header stream)
	(font-mark (mark-line (current-point)) 0 7)
	(execute-ps stream (variable-value 'user :current buffer)))
      (flush-trailing-whitespace buffer))
    (buffer-start (buffer-point buffer) buffer)
    (line-offset (buffer-point buffer) line-num)))

(defcommand "List System Processes" ()
  "Pop up a list of system processes."
  (with-pop-up-display (stream)
    (execute-header stream)
    (execute-ps stream (value list-system-processes-user))))

(defcommand "Edit System Processes" ()
  "Edit system processes."
  (let ((buffer (get-sp-list-buffer)))
    (change-to-buffer buffer)
    (refresh-system-processes buffer)))

(defcommand "System Processes" ()
  "Edit system processes."
  (edit-system-processes-command))

(defcommand "TOP" ()
  "Edit system processes."
  (edit-system-processes-command))

(defcommand "Refresh System Processes" ()
  "Refresh system process list."
  (or (buffer-minor-mode (current-buffer) "SysProc")
      (editor-error "SysProc mode must be active in the current buffer."))
  (refresh-system-processes (current-buffer)))

(defcommand "Set Current User" ()
  "Set the user of which to list processes."
  (or (buffer-minor-mode (current-buffer) "SysProc")
      (editor-error "SysProc mode must be active in the current buffer."))
  (setv user
	(prompt-for-string :prompt "User (* for all): "
			   :trim t
			   :default (user-name)))
  (if (string= (value user) "*") (setv user ()))
  (refresh-system-processes (current-buffer)))

(defcommand "Signal Process" ()
  "Signal the process at point."
  (if (or (blank-line-p (mark-line (current-point)))
	  (< (count-lines (region (buffer-start-mark (current-buffer))
				  (current-point)))
	     6))
      (editor-error "Must be on a process line."))
  (let ((mark (copy-mark (current-point))))
    (line-start mark)
    (find-attribute mark :whitespace)
    (find-attribute mark :whitespace #'zerop)
    (let ((end (copy-mark mark)))
      (find-attribute end :whitespace)
      (let ((pid (parse-integer (region-to-string (region mark end))))
	    (signal (prompt-for-integer :prompt "Signal: "
					:default 15)))
	(execute-kill pid signal)))
    (refresh-system-processes (current-buffer))))


;;;; Dictionary interface.

(defcommand "Describe Word" ()
  "Describe a prompted word in a pop-up display."
  (let ((word (prompt-for-string
	       :default (or (word-at-point) "")
	       :trim t
	       :prompt "Word: "
	       :help "Word to describe from dictionary.")))
    (with-pop-up-display (stream)
      (ext::run-program "dict"
			`("--pager" "-" ,word)
			:wait t
			:output stream))))


;;;; Halting, rebooting and suspending.

;; FIX mv bodies to code:

(defun run-halt ()
  "Sleep for a few seconds, then halt."
  (ext::run-program "sh" '("-c" "sleep 3 && sudo halt") :wait ()))

(defcommand "Halt" ()
  "Halt machine."
  (when (prompt-for-y-or-n
	 :prompt (format () "Halt ~A? " (machine-instance))
	 :help "Y to turn off the machine, N to cancel."
	 :default ())
    (elet ((save-all-files-confirm ()))
      (save-all-files-command ()))
    (add-hook exit-hook 'run-halt :end t)
    (handler-case
	(exit-command ())
      (editor-error () (remove-hook exit-hook 'run-halt)))))

(defun run-reboot ()
  "Sleep for a few seconds, then reboot."
  (ext::run-program "sh" '("-c" "sleep 3 && sudo reboot") :wait ()))

(defcommand "Reboot" ()
  "Reboot machine."
  (when (prompt-for-y-or-n
	 :prompt (format () "Reboot ~A? " (machine-instance))
	 :help "Y to reboot the machine, N to cancel."
	 :default ())
    (elet ((save-all-files-confirm ()))
      (save-all-files-command ()))
    (add-hook exit-hook 'run-reboot :end t)
    (handler-case
	(exit-command ())
      (editor-error () (remove-hook exit-hook 'run-reboot)))))

(defevar "Suspend Hook"
  "Hook called by `Suspend', before suspending.")

(defevar "Resume Hook"
  "Hook called by `Suspend', after resuming.")

(defcommand "Suspend" ()
  "Suspend to disk."
  (when (prompt-for-y-or-n
	 :prompt (format () "Suspend ~A? " (machine-instance))
	 :help "Y to suspend to disk, N to cancel."
	 :default ())
    (elet ((save-all-files-confirm ()))
      (save-all-files-command ()))
    (invoke-hook suspend-hook)
    (ext::run-program "sudo" '("s2disk"))
    (invoke-hook resume-hook)))
