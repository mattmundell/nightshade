;;; Code for sending requests to eval servers and the commands based on
;;; that code.

(in-package "ED")

(export '(eval-form-in-server eval-form-in-server-1
	  string-eval region-eval region-compile file-compile))

;;; The note structure holds everything we need to know about an
;;; operation.  Not all operations use all the available fields.
;;;
(defstruct (note (:print-function %print-note))
  (state :unsent)	      ; :unsent, :pending, :running, :aborted or :dead.
  server		      ; Server-Info for the server this op is on.
  context		      ; Short string describing what this op is doing.
  quiet                       ; Suppress messages if true.
  kind			      ; Either :eval, :compile, or :compile-file
  buffer		      ; Buffer source came from.
  region		      ; Region of request
  package		      ; Package or NIL if none
  text			      ; string containing request
  input-file		      ; File to compile or where stuff was found
  net-input-file	      ; Net version of above.
  output-file		      ; Temporary output file for compiler fasl code.
  net-output-file	      ; Net version of above
  output-date		      ; Temp-file is created before calling compiler,
			      ;  and this is its write date.
  lap-file		      ; The lap file for compiles
  error-file		      ; The file to dump errors into
  load			      ; Load compiled file or not?
  (errors 0)		      ; Count of compiler errors.
  (warnings 0)		      ; Count of compiler warnings.
  (notes 0)    		      ; Count of compiler notes.
  complete-hook)              ; List of functions called on node after op.
;;;
(defun %print-note (note stream d)
  (declare (ignore d))
  (format stream "#<Eval-Server-Note for ~A [~A]>"
	  (note-context note)
	  (note-state note)))


;;;; Note support routines.

;;; QUEUE-NOTE -- Internal.
;;;
;;; This queues note for server.  SERVER-INFO-NOTES keeps notes in stack order,
;;; not queue order.  We also link the note to the server and try to send it
;;; to the server.  If we didn't send this note, we tell the user the server
;;; is busy and that we're queuing his note to be sent later.
;;;
(defun queue-note (note server)
  (push note (server-info-notes server))
  (setf (note-server note) server)
  (maybe-send-next-note server)
  (or (note-quiet note)
      (when (eq (note-state note) :unsent)
	(message "Server ~A busy, ~A queued."
		 (server-info-name server)
		 (note-context note)))))

;;; MAYBE-SEND-NEXT-NOTE -- Internal.
;;;
;;; Loop over all notes in server.  If we see any :pending or :running, then
;;; punt since we can't send one.  Otherwise, by the end of the list, we may
;;; have found an :unsent one, and if we did, next will be the last :unsent
;;; note.  Remember, SERVER-INFO-NOTES is kept in stack order not queue order.
;;;
(defun maybe-send-next-note (server)
  (let ((busy nil)
	(next nil))
    (dolist (note (server-info-notes server))
      (ecase (note-state note)
	((:pending :running)
	 (setf busy t)
	 (return))
	(:unsent
	 (setf next note))
	(:aborted :dead)))
    (when (and (not busy) next)
      (send-note next))))

(defun send-note (note)
  (let* ((remote (wire:make-remote-object note))
	 (server (note-server note))
	 (ts (server-info-slave-info server))
	 (bg (server-info-background-info server))
	 (wire (server-info-wire server)))
    (setf (note-state note) :pending)
    (or (note-quiet note)
	(message "Sending ~A." (note-context note)))
    (case (note-kind note)
      (:eval
       (wire:remote wire
	 (server-eval-text remote
			   (note-package note)
			   (note-text note)
			   (and ts (ts-data-stream ts)))))
      (:compile
       (wire:remote wire
	 (server-compile-text remote
			      (note-package note)
			      (note-text note)
			      (note-input-file note)
			      (and ts (ts-data-stream ts))
			      (and bg (ts-data-stream bg)))))
      (:compile-file
       (macrolet ((frob (x)
		    `(if (pathnamep ,x)
		       (namestring ,x)
		       ,x)))
	 (wire:remote wire
	   (server-compile-file remote
				(note-package note)
				(frob (or (note-net-input-file note)
					  (note-input-file note)))
				(frob (or (note-net-output-file note)
					  (note-output-file note)))
				(frob (note-error-file note))
				(frob (note-lap-file note))
				(note-load note)
				(and ts (ts-data-stream ts))
				(and bg (ts-data-stream bg))))))
      (t
       (error "Unknown note kind ~S" (note-kind note))))
    (wire:wire-force-output wire)))


;;;; Server Callbacks.

(defun operation-started (note)
  (let ((note (wire:remote-object-value note)))
    (setf (note-state note) :running)
    (or (note-quiet note)
	(message "The ~A started." (note-context note))))
  (values))

(defun eval-form-error (message)
  (editor-error message))

(defun lisp-error (note start end msg)
  (declare (ignore start end))
  (let ((note (wire:remote-object-value note)))
    (loud-message "During ~A: ~A"
		  (note-context note)
		  msg))
  (values))

(defun compiler-error (note start end function severity)
  (let* ((note (wire:remote-object-value note))
	 (server (note-server note))
	 (line (mark-line
		(buffer-end-mark
		 (server-info-background-buffer server))))
	 (message (format nil "~:(~A~) ~@[in ~A ~]during ~A."
			  severity
			  function
			  (note-context note)))
	 (error (make-error-info :buffer (note-buffer note)
				 :message message
				 :line line)))
    (message "~A" message)
    (case severity
      (:error (incf (note-errors note)))
      (:warning (incf (note-warnings note)))
      (:note (incf (note-notes note))))
    (let ((region (case (note-kind note)
		    (:compile
		     (note-region note))
		    (:compile-file
		     (let ((buff (note-buffer note)))
		       (and buff (buffer-region buff))))
		    (t
		     (error "Compiler error in ~S?" note)))))
      (when region
	(let* ((region-end (region-end region))
	       (m1 (copy-mark (region-start region) :left-inserting))
	       (m2 (copy-mark m1 :left-inserting)))
	  (when start
	    (character-offset m1 start)
	    (when (mark> m1 region-end)
	      (move-mark m1 region-end)))
	  (unless (and end (character-offset m2 end))
	    (move-mark m2 region-end))

	  (setf (error-info-region error)
		(region m1 m2)))))

    (vector-push-extend error (server-info-errors server)))

  (values))

(defun eval-text-result (note start end values)
  (declare (ignore start end))
  (let* ((note (wire:remote-object-value note)))
    (or (note-quiet note)
	(message "=> ~{~#[~;~A~:;~A, ~]~}" values)))
  (values))

(defun operation-completed (note abortp)
  (let* ((note (wire:remote-object-value note))
	 (server (note-server note))
	 (file (note-output-file note)))
    (wire:forget-remote-translation note)
    (setf (note-state note) :dead)
    (setf (server-info-notes server)
	  (delete note (server-info-notes server)
		  :test #'eq))
    (setf (note-server note) nil)

    (if abortp
	(loud-message "The ~A aborted." (note-context note))
	(or (note-quiet note)
	    (let ((errors (note-errors note))
		  (warnings (note-warnings note))
		  (notes (note-notes note)))
	      (message "The ~A complete.~
			~@[ ~D error~:P~]~@[ ~D warning~:P~]~@[ ~D note~:P~]"
		       (note-context note)
		       (and (plusp errors) errors)
		       (and (plusp warnings) warnings)
		       (and (plusp notes) notes)))))

    (let ((region (note-region note)))
      (when (regionp region)
	(delete-mark (region-start region))
	(delete-mark (region-end region))
	(setf (note-region note) nil)))

    (when (and (eq (note-kind note)
		   :compile-file)
	       (not (eq file t))
	       file)
      (if (> (file-write-date file)
	     (note-output-date note))
	  (let ((new-name (make-pathname :type "fasl"
					 :defaults (note-input-file note))))
	    (rename-file file new-name)
	    (unix:unix-chmod (namestring new-name) #o644))
	  (delete-file file)))

    (when (note-complete-hook note)
      (dolist (function (note-complete-hook note))
	(funcall function note)))

    (maybe-send-next-note server))
  (values))


;;;; Stuff to send noise to the server.

#[ Synchronous Operation Queuing

The routines in this section queue requests with an eval server and wait for
confirmation that the evaluation actually occurred.  Because of this, the user
cannot continue editing while the slave executes the request.  Note, these
usually execute in the slave immediately, but if the interactive buffer
connected to the slave is waiting for a form to return a value, the operation
requested must wait until the slave is free again.

{function:ed:eval-form-in-server}
{function:ed:eval-form-in-server-1}
]#

;;; EVAL-FORM-IN-SERVER -- Public.
;;;
(defun eval-form-in-server (server-info form
			    &optional
			    (package (value current-package)))
  "Queue the evaluation of $form in the server associated with $server-info
   and wait for the results.  The server read's the form from string with
   *package* bound to the package named by $package.  Return the results
   from the slave Lisp in a list of string values.

   $package falls back to `current-package'.  If $package is (), the server
   uses the value of *package* in the server.

   While the slave executes the form, it binds *terminal-io* to a stream
   that signals errors when read from and dumps output to a bit-bucket.
   This prevents the editor and slave from dead locking by waiting for each
   other to reply.

   If server is busy with other requests, signal an editor-error to prevent
   commands using this from hanging.  If the server dies while evaluating
   $form, then signal an editor-error."
  (declare (simple-string form))
  (if (server-info-notes server-info)
      (editor-error "Server ~S is currently busy.  See `List Operations'."
		    (server-info-name server-info)))
  (multiple-value-bind (values error)
		       (wire:remote-value (server-info-wire server-info)
			 (server-eval-form package form))
    (when error
      (editor-error "The server died before finishing"))
    values))

;;; EVAL-FORM-IN-SERVER-1 -- Public.
;;;
;;; Use VALUES to squelch the second value of READ-FROM-STRING.
;;;
(defun eval-form-in-server-1 (server-info form
			      &optional
			      (package (value current-package)))
  "Call `eval-form-in-server' and read the result in the first string it
   returns.  This result must be read'able in the editor's Lisp.  Return
   the value read."
  (values (read-from-string
	   (car (eval-form-in-server server-info form package)))))

#[ Asynchronous Operation Queuing

The routines in this section queue requests with an eval server.  Requests are
always satisfied in order, but these do not wait for notification that the
operation actually happened.  Because of this, the user can continue editing
while his evaluation or compilation occurs.  Note, these usually execute in the
slave immediately, but if the interactive buffer connected to the slave is
waiting for a form to return a value, the operation requested must wait until
the slave is free again.

{function:ed:string-eval}
{function:ed:region-eval}
{function:ed:region-compile}
{function:ed:file-compile}
{evariable:Remote Compile File}
]#

(defun string-eval (string
		    &key
		    (server (get-current-eval-server))
		    (package (value current-package))
		    (context (format nil
				     "evaluation of ~S"
				     string))
		    (complete-hook)
		    (quiet))
  "Queue the evaluation of the form read from $string on eval server
   $server.  $string is a simple-string.

   The evaluation occurs with *package* bound in the slave to the package
   named by $package.  If $package is the empty string the slave evaluates
   the form in its current package.  The slave reads the form in $string
   within this context as well.

   $context is a string to use when reporting start and end notifications
   in the Echo Area.

   $complete-hook is a list of functions to call when the server evaluation
   completes.  These functions must take a single argument."
  (declare (simple-string string))
  (queue-note (make-note :kind :eval
			 :context context
			 :package package
			 :text string
			 :complete-hook complete-hook
			 :quiet quiet)
	      server)
  (values))

(defun region-eval (region
		    &key
		    (server (get-current-eval-server))
		    (package (value current-package))
		    (context (region-context region "evaluation")))
  "Queue the evaluation of the form read from $region on eval server
   $server.  $region is an editor region.

   The evaluation occurs with *package* bound in the slave to the package
   named by $package.  If $package is the empty string the slave evaluates
   the form in its current package.  The slave reads the form in $string
   within this context as well.

   $context is a string to use when reporting start and end notifications
   in the Echo Area."
  (let ((region (region (copy-mark (region-start region) :left-inserting)
			(copy-mark (region-end region) :left-inserting))))
    (queue-note (make-note :kind :eval
			   :context context
			   :region region
			   :package package
			   :text (region-to-string region))
		server))
  (values))

(defun region-compile (region
		       &key
		       (server (get-current-eval-server))
		       (package (value current-package)))
  "Queue the compilation of $region in eval server $server.  $region is an
   editor region.

   The evaluation occurs with *package* bound in the slave to the package
   named by $package.  If $package is the empty string the slave evaluates
   the form in its current package.  The slave reads the form in $string
   within this context as well."
  (let* ((region (region (copy-mark (region-start region) :left-inserting)
			 (copy-mark (region-end region) :left-inserting)))
	 (buf (line-buffer (mark-line (region-start region))))
	 (pn (and buf (buffer-pathname buf)))
	 (defined-from (if pn (namestring pn) "unknown")))
    (queue-note (make-note :kind :compile
			   :context (region-context region "compilation")
			   :buffer (and region
					(region-start region)
					(mark-line (region-start region))
					(line-buffer (mark-line
						      (region-start region))))
			   :region region
			   :package package
			   :text (region-to-string region)
			   :input-file defined-from)
		server))
  (values))


;;;; File compiling noise.

(defevar "Remote Compile File"
  "When set, slave file compilations assume that the compilation is
   occurring on a remote machine.  This means the source file must be world
   readable.")

;;; FILE-COMPILE compiles files in a client Lisp.  Because of Unix file
;;; protection, one cannot write files over the net unless they are publicly
;;; writeable.  To get around this, we create a temporary file that is
;;; publicly writeable for compiler output.  This file is renamed to an
;;; ordinary output name if the compiler wrote anything to it, or deleted
;;; otherwise.  No temporary file is created when output-file is not t.
;;;
(defun file-compile (file
		     &key
		     buffer
		     (output-file t)
		     error-file
		     lap-file
		     load
		     (server (get-current-compile-server))
		     (package (value current-package)))
  "Compile $file in a slave Lisp.

   When $output-file is t and *Remote Compile File* is true, use a
   temporary output file that is publicly writable.  This is in case the
   client is on another machine, which allows for file systems that
   restrict remote write access.  After compilation either rename the
   temporary file to the appropriate binary name or delete it.  If
   $output-file is a true value other then t, write directly to the binary
   file named by $output-file.

   The compilation occurs with *package* bound in the slave to the package
   named by $package.  If $package is empty string the slave evaluates the
   form in its current package.

   $error-file is the file in which to record compiler output.  Only create
   the file if $error-file is true.

   If $load is true load the resulting binary file.

   If $server is () use the value returned from get-current-server."
  (let* ((file (truename file)) ; in case of search-list in pathname.
	 (namestring (namestring file))
	 (note (make-note
		:kind :compile-file
		:context (format nil "compilation of ~A" namestring)
		:buffer buffer
		:region nil
		:package package
		:input-file file
		:output-file output-file
		:error-file error-file
		:lap-file lap-file
		:load load)))

    (when (and (value remote-compile-file)
	       (eq output-file t))
      (multiple-value-bind (net-infile ofile net-ofile date)
			   (file-compile-temp-file file)
	(setf (note-net-input-file note) net-infile)
	(setf (note-output-file note) ofile)
	(setf (note-net-output-file note) net-ofile)
	(setf (note-output-date note) date)))

    (clear-server-errors server
			 #'(lambda (error)
			     (eq (error-info-buffer error)
				 buffer)))
    (queue-note note server)))

;;; FILE-COMPILE-TEMP-FILE creates a a temporary file that is publicly
;;; writable in the directory file is in and with a .fasl type.  Four values
;;; are returned -- a pathname suitable for referencing file remotely, the
;;; pathname of the temporary file created, a pathname suitable for referencing
;;; the temporary file remotely, and the write date of the temporary file.
;;;

(defun file-compile-temp-file (file)
  (let ((ofile (loop (let* ((sym (gensym))
			    (f (merge-pathnames
				(format nil "compile-file-~A.fasl" sym)
				file)))
		       (unless (probe-file f) (return f))))))
    (multiple-value-bind (fd err)
			 (unix:unix-open (namestring ofile)
					 unix:o_creat #o666)
      (unless fd
	(editor-error "Couldn't create compiler temporary output file:~%~
	~A" (unix:get-unix-error-msg err)))
      (unix:unix-fchmod fd #o666)
      (unix:unix-close fd))
    (let ((net-ofile (pathname-for-remote-access ofile)))
      (values (make-pathname :directory (pathname-directory net-ofile)
			     :defaults file)
	      ofile
	      net-ofile
	      (file-write-date ofile)))))

(defun pathname-for-remote-access (file)
  (let* ((machine (machine-instance))
	 (usable-name (nstring-downcase
		       (the simple-string
			    (subseq machine 0 (position #\. machine))))))
    (declare (simple-string machine usable-name))
    (make-pathname :directory (concatenate 'simple-string
					   "/../"
					   usable-name
					   (directory-namestring file))
		   :defaults file)))

;;; REGION-CONTEXT -- internal
;;;
;;; Return a string which describes the code in a region.  Thing is the
;;; thing being done to the region.  "compilation" or "evaluation"...

(defun region-context (region thing)
  (declare (simple-string thing))
  (pre-command-parse-check (region-start region))
  (let ((start (region-start region)))
    (with-mark ((m1 start))
      (unless (start-defun-p m1)
	(top-level-offset m1 1))
      (with-mark ((m2 m1))
	(mark-after m2)
	(form-offset m2 2)
	(format nil
		"~A of ~S"
		thing
		(if (eq (mark-line m1) (mark-line m2))
		  (region-to-string (region m1 m2))
		  (concatenate 'simple-string
			       (line-string (mark-line m1))
			       "...")))))))


;;;; Commands.

(defcommand "Editor Server Name" ()
  "Echo the editor server's name, the machine and port of the editor, which
   is suitable for use with the Lisp processes -slave switch, as described
   in [Slave Switch]."
  (if *editor-name*
      (message "This editor is named ~S." *editor-name*)
      (message "This editor is not currently named.")))

#[ The Current Package

The current package is the package which Lisp interaction commands use.  The
current package is specified on a per-buffer basis, and defaults to "USER".
If the current package does not exist in the eval server, then it is created.
If evaluation is being done in the editor process and the current package
doesn't exist, then the value of *package* is used.  The current package is
displayed in the modeline (see section [modelines].)  Normally the package
for each file is specified using the Package file option (see page
pagereffile-options.)

When in a slave buffer, the current package is controlled by the value of
`package' in that Lisp process.  Modeline display of the current package
is inhibited in this case.

{command:Set Buffer Package}
]#

(defcommand "Set Buffer Package" ()
  "This command prompts for the name of a package to make the current
   package in the current buffer.  If the current buffer is a slave,
   background, or eval buffer, then set the current package in the
   associated eval server or editor Lisp.  This command needs to be used in
   an interactive buffer instead of in-package."
  (let* ((name (string (prompt-for-expression
			:prompt "Package name: "
			:help "Name of package to associate with this buffer.")))
	 (buffer (current-buffer))
	 (info (value current-eval-server)))
    (cond ((and info
		(or (eq (server-info-slave-buffer info) buffer)
		    (eq (server-info-background-buffer info) buffer)))
	   (wire:remote (server-info-wire info)
	     (server-set-package name))
	   (wire:wire-force-output (server-info-wire info)))
	  ((eq buffer *selected-eval-buffer*)
	   (setf *package* (maybe-make-package name)))
	  (t
	   (defevar "Current Package"
	     "The package used for evaluation of Lisp in this buffer."
	     :buffer buffer  :value name)))
    (when (buffer-modeline-field-p buffer :package)
      (dolist (w (buffer-windows buffer))
	(update-modeline-field buffer w :package)))))

(defcommand "Current Compile Server" (p)
  "Echo the name of current compile server.  With a prefix argument, shows
   the global one."
  (let ((info (if p
		  (variable-value 'current-compile-server :global)
		  (value current-compile-server))))
    (if info
	(message "~A" (server-info-name info))
	(message "No ~:[current~;global~] compile server." p))))

(defcommand "Set Compile Server" ()
  "Specify the name of the server used globally for file compilation
   requests."
  (elet ((ask-about-old-servers t))
    (setf (variable-value 'current-compile-server :global)
	  (maybe-create-server))))

(defcommand "Set Buffer Compile Server" ()
  "Specify the name of the server used for file compilation requests in the
   current buffer."
  (elet ((ask-about-old-servers t))
    (defevar "Current Compile Server"
      "The Server-Info object for the server currently used for compilation requests."
      :buffer (current-buffer)
      :value (maybe-create-server))))

#[ The Current Eval Server

Although the editor can be connected to several eval servers
simultaneously, one eval server is designated as the current eval server.
This is the eval server used to handle evaluation and compilation requests.
Eval servers are referred to by name so that there is a convenient way to
tell between servers when the editor is connected to more than one.  The
current eval server is normally globally specified, but it may also be
shadowed locally in specific buffers.

{command:Set Eval Server}
{command:Set Buffer Eval Server}
{command:Current Eval Server}
]#

(defcommand "Current Eval Server" (p)
  "Echo the name of the current eval server.  With prefix argument, show
   the global server.  Set Compile Server is a similar command."
  (let ((info (if p
		  (variable-value 'current-eval-server :global)
		  (value current-eval-server))))
    (if info
	(message "~A" (server-info-name info))
	(message "No ~:[current~;global~] eval server." p))))

(defcommand "Set Eval Server" ()
  "Make a prompted server the current eval server."
  (elet ((ask-about-old-servers t))
    (setf (variable-value 'current-eval-server :global)
	  (maybe-create-server))))

(defcommand "Set Buffer Eval Server" ()
  "Make a prompted server the current eval server in the current buffer."
  (elet ((ask-about-old-servers t))
    (defevar "Current Eval Server"
      "The Server-Info for the eval server used in this buffer."
      :buffer (current-buffer)
      :value (maybe-create-server))))

#[ Compiling and Evaluating Lisp Code

These commands can greatly speed up
the edit/debug cycle since they enable incremental reevaluation or
recompilation of changed code, avoiding the need to compile and load an
entire file.

{command:Evaluate Expression}
{command:Evaluate Defun}
{command:Evaluate Region}
{command:Evaluate Buffer}
{command:Macroexpand Expression}
{command:Evaluate Defvar}
{command:Compile Defun}
{command:Compile Region}
{command:Load File}
{evariable:Load Pathname Defaults}
]#

(defcommand "Evaluate Defun" ()
  "Evaluates the current or next top-level form.  If the current region is
   active, then evaluate it."
  "Evaluates the current or next top-level form."
  (if (region-active-p)
      (evaluate-region-command)
      (region-eval (defun-region (current-point)))))

(defcommand "Evaluate Defvar" ()
  "If the current or next top-level form is a `defvar' then clear the
   variable binding and evaluate the form."
  (let* ((form (defun-region (current-point)))
	 (start (region-start form)))
    (with-mark ((var-start start)
		(var-end start))
      (mark-after var-start)
      (form-offset var-start 1)
      (form-offset (move-mark var-end var-start) 1)
      (let ((exp (concatenate 'simple-string
			      "(makunbound '"
			      (region-to-string (region var-start var-end))
			      ")")))
	(eval-form-in-server (get-current-eval-server) exp)))
    (region-eval form)))

;;; We use Prin1-To-String in the client so that the expansion gets pretty
;;; printed.  Since the expansion can contain unreadable stuff, we can't expect
;;; to be able to read that string back in the editor.  We shove the region
;;; at the client Lisp as a string, so it can read from the string with the
;;; right package environment.
;;;

(defcommand "Macroexpand Expression" (p)
  "Pop-up the macroexpansion of the current expression in the null
   environment.  With an argument, use MACROEXPAND instead of
   MACROEXPAND-1."
  (let ((point (current-point)))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (with-mark ((end start))
        (or (form-offset end 1)
	    (editor-error "Point must be followed by a form to expand."))
	(with-pop-up-display (s)
	  (write-string
	   (eval-form-in-server-1
	    (get-current-eval-server)
	    (format nil "(prin1-to-string (~S (read-from-string ~S)))"
		    (if p 'macroexpand 'macroexpand-1)
		    (region-to-string (region start end))))
	   s))))))

(defcommand "Evaluate Expression" ()
  "Prompt for an expression and print the result of its evaluation in the
   echo area.  If an error happens during evaluation, exit the evaluation
   instead of going into the inspector.  Return when the evaluation is
   complete."
  (let ((exp (prompt-for-string
	      :prompt "Eval: "
	      :help "Expression to evaluate.")))
    (message "=> ~{~#[~;~A~:;~A, ~]~}"
	     (eval-form-in-server (get-current-eval-server) exp))))

(defcommand "Compile Defun" ()
  "Compiles the current or next top-level form.  First the form is
   evaluated, then the result of this evaluation is passed to compile.  If
   the current region is active, compile the region."
  (if (region-active-p)
      (compile-region-command)
      (region-compile (defun-region (current-point)))))

(defcommand "Compile Region" ()
  "Compile the lisp forms between the point and the mark."
  (region-compile (current-region)))

(defcommand "Evaluate Region" ()
  "Evaluate the lisp forms between the point and the mark."
  (region-eval (current-region)))

(defcommand "Evaluate Buffer" ()
  "Evaluates the text in the current buffer."
  "Evaluates the text in the current buffer redirecting *Standard-Output*
   to the echo area."
  (let ((b (current-buffer)))
    (region-eval (buffer-region b)
		 :context (format nil
				  "evaluation of buffer ``~A''"
				  (buffer-name b)))))

(defcommand "Load File" ()
  "Load a prompted file into the current eval server.  FIX Heed the `Load
   Pathname Defaults' and `Remote Compile File' variable."
  (let ((name (truename (prompt-for-file
			 :default
			 (or (value load-pathname-defaults)
			     (buffer-default-pathname (current-buffer)))
			 :prompt "File to load: "
			 :help "The name of the file to load"))))
    (setv load-pathname-defaults name)
    (string-eval (format nil "(load ~S)"
			 (namestring
			  (if (value remote-compile-file)
			      (pathname-for-remote-access name)
			      name))))))

#[ Compiling Files

These commands are used to compile source (".lisp") files, producing binary
(".fasl") output files.  Note that unlike the other compiling and evalating
commands, this does not have the effect of placing the definitions in the
environment; to do so, the binary file must be loaded.

{command:Compile Buffer File}
{evariable:Compile Buffer File Confirm}
{command:Compile File}
{command:Compile Group}
{command:List Compile Group}

The next commands are analogous to `Set Eval Server', `Set Buffer Eval
Server' and `Current Eval Server', but they determine the eval server used
for file compilation requests.

If the user specifies a compile server, then the file compilation commands
send compilation requests to that server instead of the current eval
server.

Having a separate compile server makes it easy to do compilations in the
background while continuing to interact with your eval server and editor.
The compile server can also run on a remote machine relieving your active
development machine of the compilation effort.

{command:Set Compile Server}
{command:Set Buffer Compile Server}
{command:Current Compile Server}

The next two commands provides a convenient way to inspect compiler errors.

{command:Next Compiler Error}
{command:Previous Compiler Error}
{command:Flush Compiler Error Information}

`Flush Compiler Error Information' is convenient if you have been compiling
a lot, but you were ignoring errors and warnings.  You don't want to step
through all the old errors, so you can use this command immediately before
compiling a file whose errors you intend to edit.

{evariable:Remote Compile File}
]#

(defcommand "Compile File" ()
  "Prompts for file to compile.  Does not compare source and binary write
   dates.  Does not check any buffer for that file for whether the buffer
   needs to be saved."
  "Prompts for file to compile."
  (let ((pn (prompt-for-file :default
			     (buffer-default-pathname (current-buffer))
			     :prompt "File to compile: ")))
    (file-compile pn)))

(defevar "Compile Buffer File Confirm"
  "When set, `Compile Buffer File' prompts before doing anything."
  :value t)

(defcommand "Compile Buffer File" (p)
  "Ensure that the file in the current buffer has an up-to-date compiled
   version (a binary file, of type .fasl).  When the binary file is up to
   date, prompt whether to compile anyway.  When the prefix argument is
   supplied, compile the file even if the compiled version is up-to-date.

   If `Compile Buffer File Confirm' is set then ask for confirmation
   before compiling when the buffer is modified or the compiled version
   needs creating or updating."
  (let* ((buf (current-buffer))
	 (pn (buffer-pathname buf)))
    (or pn (editor-error "Buffer has no associated pathname."))
    (cond ((buffer-modified buf)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Save and compile file ~A? "
				    (namestring pn))))
	     (write-buffer-file buf pn)
	     (file-compile pn :buffer buf)))
	  ((older-or-non-existent-fasl-p pn p)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Compile file ~A? " (namestring pn))))
	     (file-compile pn :buffer buf)))
	  ((or p
	       (prompt-for-y-or-n
		:default t :default-string "Y"
		:prompt
		"Fasl file up to date, compile source anyway? "))
	   (file-compile pn :buffer buf)))))

(defcommand "Compile Group" (p)
  "`Save All Files' then ensure that there is an up-to-date compiled
   version of every \".lisp\" file in the current group.  Direct error
   output to the \"Compiler Warnings\" buffer.  If a prefix argument is
   provided, then compile every file even those with up-to-date compiled
   counterparts."
  (save-all-files-command)
  (or *active-file-group* (editor-error "No active file group."))
  (dolist (file *active-file-group*)
    (when (string-equal (pathname-type file) "lisp")
      (let ((tn (probe-file file)))
	(cond ((not tn)
	       (message "File ~A not found." (namestring file)))
	      ((older-or-non-existent-fasl-p tn p)
	       (file-compile tn)))))))

(defun older-or-non-existent-fasl-p (pathname &optional definitely)
  (let ((obj-pn (probe-file (compile-file-pathname pathname))))
    (or definitely
	(not obj-pn)
	(< (file-write-date obj-pn) (file-write-date pathname)))))


;;;; Error hacking stuff.

(defcommand "Flush Compiler Error Information" ()
  "Flush all infomation about errors encountered while compiling using the
   current server"
  (clear-server-errors (get-current-compile-server t)))

(defcommand "Next Compiler Error" (p)
  "First ensure there are at least two windows and position the current
   point in the first window at the erroneous source code for the next
   error.  Then in the second window, displays the error beginning at the
   top of the window.  Given an argument, this command skips that many
   errors."
  (let* ((server (get-current-compile-server t))
	 (errors (server-info-errors server))
	 (fp (fill-pointer errors)))
    (if (zerop fp)
	(editor-error "There are no compiler errors."))
    (let* ((old-index (server-info-error-index server))
	   (new-index (+ (or old-index -1) (or p 1))))
      (when (< new-index 0)
	(if old-index
	    (editor-error "Can't back up ~R, only at the ~:R compiler error."
			  (- p) (1+ old-index))
	    (editor-error "Not even at the first compiler error.")))
      (when (>= new-index fp)
	(if (= (1+ (or old-index -1)) fp)
	    (editor-error "No more compiler errors.")
	    (editor-error "Only ~R remaining compiler error~:P."
			  (- fp old-index 1))))
      (setf (server-info-error-index server) new-index)
      ;; Display the silly error.
      (let ((error (aref errors new-index)))
	(let ((region (error-info-region error)))
	  (if region
	      (let* ((start (region-start region))
		     (buffer (line-buffer (mark-line start))))
		(change-to-buffer buffer)
		(move-mark (buffer-point buffer) start))
	      (message "Hmm, no region for this error.")))
	(let* ((line (error-info-line error))
	       (buffer (line-buffer line)))
	  (if (and line (bufferp buffer))
	      (let ((mark (mark line 0)))
		(or (buffer-windows buffer)
		    (let ((window (find-if-not
				   #'(lambda (window)
				       (or (eq window (current-window))
					   (eq window *echo-area-window*)))
				   *window-list*)))
		      (if window
			  (setf (window-buffer window) buffer)
			  (make-window mark))))
		(move-mark (buffer-point buffer) mark)
		(dolist (window (buffer-windows buffer))
		  (move-mark (window-display-start window) mark)
		  (move-mark (window-point window) mark))
		(delete-mark mark))
	      (message "Hmm, no line for this error.")))))))

(defcommand "Previous Compiler Error" (p)
  "First ensure there are at least two windows and position the current
   point in the first window at the erroneous source code for the previous
   error.  Then in the second window, displays the error beginning at the
   top of the window.  Given an argument, this command skips that many
   previous errors."
  (next-compiler-error-command (- (or p 1))))


;;;; Operation management commands.

#[ Eval Server Operations

The editor handles requests for compilation or evaluation by queuing an
operation on the current eval server.  Any number of operations may be
queued.  Each eval server services one operation at a time.  Information
about the progress of operations is displayed in the echo area.

{command:Abort Operations}
{command:List Operations}
]#

(defcommand "Abort Operations" ()
  "End all operations on current eval server connection, either queued or
   in progress.  Flush any operations in the aborted state."
  (let* ((server (get-current-eval-server))
	 (wire (server-info-wire server)))
    ;; Tell the slave to abort the current operation and to ignore any further
    ;; operations.
    (dolist (note (server-info-notes server))
      (setf (note-state note) :aborted))
    (ext:send-character-out-of-band (wire:wire-fd wire) #\N)
    (wire:remote-value wire (server-accept-operations))
    ;; Synch'ing with server here, causes any operations queued at the socket or
    ;; in the server to be ignored, and the last thing evaluated is an
    ;; instruction to go on accepting operations.
    (wire:wire-force-output wire)
    (dolist (note (server-info-notes server))
      (when (eq (note-state note) :pending)
	;; The WIRE:REMOTE-VALUE call should have allowed a handshake to
	;; tell the editor anything :pending was aborted.
	(error "Operation ~S is still around after we aborted it?" note)))
    ;; Forget anything queued in the editor.
    (setf (server-info-notes server) nil)))

(defcommand "List Operations" ()
  "List all operations which have yet to complete.  Display the state and
   eval server along with a description of the operation.  The following
   states are used:

     Unsent
	The operation is in local queue in the editor, and has yet to be
	sent.

     Pending
	The operation has been sent and has yet to start execution.

     Running
	The operation is currently being processed.

     Aborted
	The operation has been aborted, and the eval server still has
	to indicate termination."
  (let ((notes nil))
    ;; Collect all notes, reversing them since they act like a queue but
    ;; are not in queue order.
    (do-strings (str val *server-names*)
      (declare (ignore str))
      (setq notes (nconc notes (reverse (server-info-notes val)))))
    (if notes
	(with-pop-up-display (s)
	  (dolist (note notes)
	    (format s "~@(~8A~) ~A on ~A.~%"
		    (note-state note)
		    (note-context note)
		    (server-info-name (note-server note)))))
	(message "No uncompleted operations.")))
  (values))


;;;; Describing in the client lisp.

#[ Querying the Environment

These commands are useful for obtaining various random information from the
Lisp environment.

{command:Describe Function Call}
{command:Describe Symbol}
]#

;;; "Describe Function Call" gets the function name from the current form
;;; as a string.  This string is used as the argument to a call to
;;; DESCRIBE-FUNCTION-CALL-AUX which is eval'ed in the client lisp.  The
;;; auxiliary function's name is qualified since it is read in the client
;;; Lisp with *package* bound to the buffer's package.  The result comes
;;; back as a list of strings, so we read the first string to get out the
;;; string value returned by DESCRIBE-FUNCTION-CALL-AUX in the client Lisp.
;;;
(defcommand "Describe Function Call" (p)
  "Pop up the description of the symbol found at the head of the currently
   enclosing list. If there is a valid current eval server then use that to
   find the description, otherwise use the editor Lisp environment."
  (let ((info (value current-eval-server)))
    (cond
     ((not info)
      (message "Describing from the editor Lisp ...")
      (editor-describe-function-call-command p))
     (t
      (with-mark ((mark1 (current-point))
		  (mark2 (current-point)))
	(pre-command-parse-check mark1)
	(or (backward-up-list mark1)
	    (editor-error
	     "There must be a function call before point to describe."))
	(form-offset (move-mark mark2 (mark-after mark1)) 1)
	(let* ((package (value current-package))
	       (package-exists
		(eval-form-in-server-1
		 info
		 (format nil
			 "(if (find-package ~S) t (package-name *package*))"
			 package)
		 nil)))
	  (or (eq package-exists t)
	      (message "Using package ~S in ~A since ~
			~:[there is no current package~;~:*~S does not exist~]."
		       package-exists (server-info-name info) package))
	  (with-pop-up-display (s)
	    (write-string (eval-form-in-server-1
			   info
			   (format nil "(ed::describe-function-call-aux ~S)"
				   (region-to-string (region mark1 mark2)))
			   (if (eq package-exists t) package nil))
			   s))))))))

;;; DESCRIBE-FUNCTION-CALL-AUX is always evaluated in a client Lisp to some
;;; editor, relying on the fact that the cores have the same functions.  String
;;; is the name of a function that is read (in the client Lisp).  The result is
;;; a string of all the output from EDITOR-DESCRIBE-FUNCTION.
;;;
(defun describe-function-call-aux (string)
  (let* ((sym (read-from-string string))
	 (fun (function-to-describe sym error)))
    (with-output-to-string (*standard-output*)
      (editor-describe-function fun sym))))

;;; "Describe Symbol" gets the symbol name and quotes it as the argument to a
;;; call to DESCRIBE-SYMBOL-AUX which is eval'ed in the client lisp.  The
;;; auxiliary function's name is qualified since it is read in the client Lisp
;;; with *package* bound to the buffer's package.  The result comes back as a
;;; list of strings, so we read the first string to get out the string value
;;; returned by DESCRIBE-SYMBOL-AUX in the client Lisp.
;;;
(defcommand "Describe Symbol" ()
  "Pop up the description of the symbol found before point. If there is a
   valid current eval server then use that to find the description,
   otherwise use the editor Lisp environment."
  (let ((info (value current-eval-server)))
    (cond
     ((not info)
      (message "Describing from the editor Lisp ...")
      (editor-describe-symbol-command))
     (t
      (with-mark ((mark1 (current-point))
		  (mark2 (current-point)))
	(mark-symbol mark1 mark2)
	(with-pop-up-display (s)
	  (write-string (eval-form-in-server-1
			 info
			 (format nil "(ed::describe-symbol-aux '~A)"
				 (region-to-string (region mark1 mark2))))
			s)))))))

(defun describe-symbol-aux (thing)
  (with-output-to-string (*standard-output*)
    (describe (if (and (consp thing)
		       (or (eq (car thing) 'quote)
			   (eq (car thing) 'function))
		       (symbolp (cadr thing)))
		  (cadr thing)
		  thing))))
