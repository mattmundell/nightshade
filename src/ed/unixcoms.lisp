;;; Commands useful when running on a Unix box.

(in-package "ED")


;;;; Region and File printing commands.

(defhvar "Print Utility"
  "UNIX(tm) program to invoke (via EXT:RUN-PROGRAM) to do printing.
   The program should act like lpr: if a filename is given as an argument,
   it should print that file, and if no name appears, standard input should
   be assumed."
  :value "lpr")

(defhvar "Print Utility Switches"
  "Switches to pass to the \"Print Utility\" program.  This should be a list
   of strings."
  :value ())

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

(defcommand "Print Buffer" (p)
  "Prints the current buffer using the program defined by the hvar
   \"Print Utility\" with the options from the hvar \"Print Utility
   Options\".   Errors appear in the echo area."
  "Prints the contents of the buffer."
  (declare (ignore p))
  (message "Printing buffer...~%")
  (print-region (buffer-region (current-buffer))))

(defcommand "Print Region" (p)
  "Prints the current region using the program defined by the hvar
   \"Print Utility\" with the options from the hvar \"Print Utility
   Options\".  Errors appear in the echo area."
  "Prints the current region."
  (declare (ignore p))
  (message "Printing region...~%")
  (print-region (current-region)))

(defcommand "Print File" (p)
  "Prompts for a file and prints it usings the program defined by
   the hvar \"Print Utility\" with the options from the hvar \"Print
   Utility Options\".  Errors appear in the echo area."
  "Prints a file."
  (declare (ignore p))
  (let* ((pn (prompt-for-file :prompt "File to print: "
			      :help "Name of file to print."
			      :default (buffer-default-pathname (current-buffer))
			      :must-exist t))
	 (ns (namestring (truename pn))))
    (message "Printing file...~%")
    (print-something () (value print-utility)
		     (append (value print-utility-switches) (list ns)))))


;;;; Scribe.

(defcommand "Scribe File" (p)
  "Scribe a file with the default directory set to the directory of the
   specified file.  The output from running Scribe is sent to the
   \"Scribe Warnings\" buffer.  See \"Scribe Utility\" and \"Scribe Utility
   Switches\"."
  "Scribe a file with the default directory set to the directory of the
   specified file."
  (declare (ignore p))
  (scribe-file (prompt-for-file :prompt "Scribe file: "
				:default
				(buffer-default-pathname (current-buffer)))))

(defhvar "Scribe Buffer File Confirm"
  "When set, \"Scribe Buffer File\" prompts for confirmation before doing
   anything."
  :value t)

(defcommand "Scribe Buffer File" (p)
  "Scribe the file associated with the current buffer.  The default directory
   set to the directory of the file.  The output from running Scribe is sent to
   the \"Scribe Warnings\" buffer.  See \"Scribe Utility\" and \"Scribe Utility
   Switches\".  Before doing anything the user is asked to confirm saving and
   Scribe'ing the file.  This prompting can be inhibited by with \"Scribe Buffer
   File Confirm\"."
  "Scribe a file with the default directory set to the directory of the
   specified file."
  (declare (ignore p))
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

(defhvar "Scribe Utility"
  "Program name to invoke (via EXT:RUN-PROGRAM) to do text formatting."
  :value "scribe")

(defhvar "Scribe Utility Switches"
  "Switches to pass to the \"Scribe Utility\" program.  This should be a list
   of strings."
  :value ())

(defun scribe-file (pathname)
  (let* ((pathname (truename pathname))
	 (out-buffer (or (getstring "Scribe Warnings" *buffer-names*)
			 (make-buffer "Scribe Warnings")))
	 (out-point (buffer-end (buffer-point out-buffer)))
	 (stream (make-hemlock-output-stream out-point :line))
	 (orig-cwd (default-directory)))
    (buffer-end out-point)
    (insert-character out-point #\newline)
    (insert-character out-point #\newline)
    (unwind-protect
	(progn
	  (setf (default-directory) (directory-namestring pathname))
	  (ext:run-program (namestring (value scribe-utility))
			   (list* (namestring pathname)
				  (value scribe-utility-switches))
			   :output stream :error stream
			   :wait nil))
      (setf (default-directory) orig-cwd))))


;;;; UNIX Filter Region

(defcommand "Unix Filter Region" (p)
  "Unix Filter Region prompts for a UNIX program and then passes the
   current region to the program as standard input.  The standard output
   from the program is used to replace the region.  This command is
   undo-able."
  "UNIX-FILTER-REGION-COMMAND is not intended to be called from normal
   editor commands; use UNIX-FILTER-REGION instead."
  (declare (ignore p))
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
  "Passes the region REGION as standard input to the program COMMAND
  with arguments ARGS and returns the standard output as a freshly
  cons'ed region."
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

(defcommand "Print Environment Variable" (p)
  "Print value of prompted environment variable."
  "Print value of prompted environment variable."
  (declare (ignore p))
  (let ((v (prompt-for-string
	    :prompt "Environment variable: "
	    :help "Environment variable of which to print the value (e.g. PATH)."
	    :trim t)))
    (message "~A: ~A"
	     v
	     (cdr (assoc (lisp::make-keyword (intern v))
			 *environment-list*
			 :test #'eq)))))

(defcommand "Setenv" (p &optional variable value)
  "Set value of prompted environment variable."
  "Set value of prompted environment variable."
  (set-environment-variable-command p variable value))

(defcommand "Set Environment Variable" (p &optional variable value)
  "Set value of prompted environment variable."
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

(defmode "Manual" :major-p nil
  :precedence 4.0
  :documentation
  "Manual Page Mode.")

(defcommand "Manual Page from Point" (p)
  "Read the Unix manual pages named at point in a View buffer.
   If given an argument, this will put the man page in a Pop-up display."
  "Read the Unix manual pages named at point in a View buffer.
   If given an argument, this will put the man page in a Pop-up display."
  (manual-page-command p (or (manual-name-at-point) nil)))

(defcommand "Manual Page" (p &optional page-name)
  "Read the Unix manual pages in a View buffer.
   If given an argument, this will put the man page in a Pop-up display."
  "Read the Unix manual pages in a View buffer.
   If given an argument, this will put the man page in a Pop-up display."
  (let ((topic (or page-name
		   (prompt-for-string :prompt "Man topic: "
				      :default (manual-name-at-point)))))
    (if p
	(with-pop-up-display (stream)
	  (execute-man topic stream))
	(let* ((buf-name (format nil "Man Page ~a" topic))
	       (new-buffer (make-buffer buf-name
					:modes '("Fundamental" "View" "Manual")))
	       (buffer (or new-buffer (getstring buf-name *buffer-names*)))
	       (point (buffer-point buffer)))
	  (change-to-buffer buffer)
	  (when new-buffer
	    (setf (value view-return-function) #'(lambda ()))
	    (with-writable-buffer (buffer)
	      (with-output-to-mark (s point :full)
		(execute-man topic s))))
	  (buffer-start point buffer)))))

(defun execute-man (topic stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format nil "man ~a| ul -t adm3" topic))
   :output stream))

(defcommand "Next Manual Part" (p)
  "Move to the next part of the Manual Page in the current buffer.
   With a prefix argument move prefix number of parts forwards."
  "Move to the next part of the Manual Page in the current buffer."
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
  "Move to the previous part of the Manual Page in the current buffer."
  (next-manual-part-command (if p (- p) -1)))


;; System process listing

(defhvar "List System Processes User"
  "User of which to list processes."
  :value nil)

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
	 (format nil "kill -~A ~A" signal pid))))

(defvar *sp-list-buffer* nil
  "The name of the buffer used to list system processes.")

(defun get-sp-list-buffer ()
  "Return the name of the buffer used to list system processes."
  (or *sp-list-buffer*
      ;; FIX exported
      (edi::unique-buffer-name "System Processes")))

(defun refresh-system-processes (buffer)
  (let ((line-num (1- (count-lines (region (buffer-start-mark buffer)
					   (buffer-point buffer))))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (delete-line-font-marks (mark-line (current-point)))
      (with-output-to-mark (stream (buffer-point buffer) :full)
	(execute-header stream)
	(font-mark (mark-line (current-point)) 0 7)
	(execute-ps stream (variable-value 'user :current buffer))))
    (buffer-start (buffer-point buffer) buffer)
    (line-offset (buffer-point buffer) line-num)))

(defcommand "List System Processes" (p)
  "Pop up a list of system processes."
  "Pop up a list of system processes."
  (declare (ignore p))
  (with-pop-up-display (stream)
    (execute-header stream)
    (execute-ps stream (value list-system-processes-user))))

(defcommand "Edit System Processes" (p)
  "Edit system processes."
  "Edit system processes."
  (declare (ignore p))
  (let* ((new-buffer (make-buffer (get-sp-list-buffer)
				  :modes '("Fundamental" "View" "SysProc")))
	 (buffer (or new-buffer
		     (getstring (get-sp-list-buffer) *buffer-names*))))
    (change-to-buffer buffer)
    (when new-buffer
      (setf (value view-return-function) #'(lambda ()))
      (defhvar "User"
	"User of processes to list; nil for all users."
	:value (value list-system-processes-user)
	:buffer buffer)
      (refresh-system-processes buffer))))

(defcommand "Refresh System Processes" (p)
  "Refresh system process list."
  "Refresh system process list."
  (declare (ignore p))
  (or (buffer-minor-mode (current-buffer) "SysProc")
      (editor-error "SysProc mode must be active in the current buffer."))
  (refresh-system-processes (current-buffer)))

(defcommand "Set Current User" (p)
  "Set the user of which to list processes."
  "Set the user of which to list processes."
  (declare (ignore p))
  (or (buffer-minor-mode (current-buffer) "SysProc")
      (editor-error "SysProc mode must be active in the current buffer."))
  (setv user
	(prompt-for-string :prompt "User: "
			   :default (cdr (assoc (lisp::make-keyword
						 (intern "USER"))
						*environment-list*
						:test #'eq))))
  (refresh-system-processes (current-buffer)))

(defcommand "Kill Process" (p)
  "Kill the process at point."
  "Kill the process at point."
  (declare (ignore p))
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


;; Dictionary interface.

(defcommand "Describe Word" (p)
  "Describe a prompted word in a pop-up display."
  "Describe a prompted word in a pop-up display."
  (declare (ignore p))
  (let ((word (prompt-for-string
	       :default (word-at-point)
	       :trim t
	       :prompt "Word: "
	       :help "Word to describe from dictionary.")))
    (with-pop-up-display (stream)
      (ext::run-program "dict"
			`("--pager" "-" ,word)
			:wait t
			:output stream))))
