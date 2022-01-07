;;; Parsing the command line passed from the shell.

(in-package "EXTENSIONS")

(export '(*command-line-words* *command-line-switches*
	  *command-switch-demons* *command-line-utility-name*
	  *command-line-strings* *batch-mode*
	  cmd-switch-string command-line-switch-p
	  cmd-switch-name cmd-switch-value cmd-switch-words command-line-switch
	  defswitch cmd-switch-arg get-command-line-switch))


#[ Reading the Command Line

The shell parses the command line with which Nightshade is invoked, and
passes a data structure containing the parsed information to Nightshade.
This information is then extracted from that data structure and put into a
set of data structures.

{variable:ext:*command-line-strings*}
{variable:ext:*command-line-utility-name*}
{variable:ext:*command-line-words*}
{variable:ext:*command-line-switches*}
{function:ext:defswitch}

The following functions may be used to examine `command-line-switch'
structures.

{function:ext:cmd-switch-name}
{function:ext:cmd-switch-value}
{function:ext:cmd-switch-words}
{function:ext:cmd-switch-arg}
{function:ext:get-command-line-switch}
]#


(defvar *command-line-switches* ()
  "A list of command-line-switch structures, with a structure for each word
   on the command line starting with a hyphen.")

(defvar *command-line-utility-name* ""
  "The first word on the command line, i.e. the name of the program invoked
   (usually \"nightshade\").")

(defvar *command-line-words* ()
  "A list of all the command line words between the program name and the
   first switch")

(defvar *command-line-strings* ()
  "A list of strings that make up the command line, one word per string.")

(defvar *command-switch-demons* ()
  "An Alist of (\"argument-name\" . demon-function)")

(defvar *batch-mode* nil
  "When true run lisp with its input coming from standard-input and exit on
   reaching the end of the input.  If an error is detected return error
   code 1, otherwise 0.")

(defstruct (command-line-switch (:conc-name cmd-switch-)
				(:constructor make-cmd-switch
					      (name value words))
				(:print-function print-command-line-switch))
  name         		;the name of the switch
  value                 ;the value of that switch
  words                 ;random words dangling between switches assigned to the
                        ;preceeding switch
  )

(setf (documentation 'cmd-switch-name 'function)
  "Return the name of $switch between the preceding hyphen and any trailing
   equal sign.")

(setf (documentation 'cmd-switch-value 'function)
  "Return the value designated using an embedded equal sign of $switch if
   there is one, else ().")

(setf (documentation 'cmd-switch-words 'function)
  "Return a list of words between $switch and the next switch or the end of
   the command line.")

(defun print-command-line-switch (object stream n)
  (declare (ignore n))
  (write-string "#<Command Line Switch " stream)
  (prin1 (cmd-switch-name object) stream)
  (let ((value (cmd-switch-value object))
	(words (cmd-switch-words object)))
    (when (or value words) (write-string " -- " stream)
      (when value (prin1 value stream))
      (when words (prin1 words stream))))
  (write-string ">" stream))


;;;; Processing the command strings.

(defun process-command-strings ()
  (setq *command-line-words* nil)
  (setq *command-line-switches* nil)
  (let ((cmd-strings lisp::lisp-command-line-list)
	str)
    (declare (special lisp::lisp-command-line-list))
    ;; Set some initial variables.
    ;;
    (setf *command-line-strings* (copy-list lisp::lisp-command-line-list))
    (setf *command-line-utility-name* (pop cmd-strings))
    (setq str (pop cmd-strings))
    ;; Set initial command line words.
    ;;
    (loop
      (or str (return nil))
      (unless (zerop (length (the simple-string str)))
	(when (char= (schar str 0) #\-)
	  (setq *command-line-words* (reverse *command-line-words*))
	  (return nil))
	(push str *command-line-words*))
      (setq str (pop cmd-strings)))
    ;; Set command line switches.
    ;;
    (loop
      (or str
	  (return (setf *command-line-switches*
			(nreverse *command-line-switches*))))
      (let* ((position (position #\= (the simple-string str) :test #'char=))
	     (switch (subseq (the simple-string str) 1 position))
	     (value (if position
			(subseq (the simple-string str) (1+ position)
				(length (the simple-string str))))))
	(setq str (pop cmd-strings))
	;; Set this switch's words until the next switch.
	;;
	(let (word-list)
	  (loop
	    (unless str
	      (push (make-cmd-switch switch value (nreverse word-list))
		    *command-line-switches*)
	      (return nil))
	    (unless (zerop (length (the simple-string str)))
	      (when (char= #\- (schar str 0))
		(push (make-cmd-switch switch value (nreverse word-list))
		      *command-line-switches*)
		(return nil))
	      (push str word-list))
	    (setq str (pop cmd-strings))))))))

(defun get-command-line-switch (sname)
  "Return the value of the switch named by string $sname if the value was
   specified, otherwise any following words if there were any, otherwise t
   if the switch was specified, otherwise ()."
  (let* ((name (if (char= (schar sname 0) #\-) (subseq sname 1) sname))
	 (switch (find name *command-line-switches*
		       :test #'string-equal
		       :key #'cmd-switch-name)))
    (when switch
      (or (cmd-switch-value switch)
	  (cmd-switch-words switch)
	  t))))


;;;; Defining Switches and invoking demons.

(defvar *complain-about-illegal-switches* t
  "When set, invoking switch demons complains about illegal switches that have
   not been defined with DEFSWITCH.")

;;; This is a list of legal switch names.  DEFSWITCH sets this, and
;;; INVOKE-SWITCH-DEMONS makes sure all the switches it sees are on this
;;; list.
;;;
(defvar *legal-cmd-line-switches* nil)

;;; INVOKE-SWITCH-DEMONS cdrs down the list of *command-line-switches*.  For
;;; each switch, it checks to see if there is a switch demon with the same
;;; name.  If there is, then that demon is called as a function on the switch.
;;;
(defun invoke-switch-demons (&optional (switches *command-line-switches*)
					 (demons *command-switch-demons*))
  (dolist (switch switches t)
    (let* ((name (cmd-switch-name switch))
	   (demon (cdr (assoc name demons :test #'string-equal))))
      (cond (demon (funcall demon switch))
	    ((or (member name *legal-cmd-line-switches* :test #'string-equal)
		 (not *complain-about-illegal-switches*)))
	    (t (warn "~S is an illegal switch" switch)))
      (lisp::finish-standard-output-streams))))

(defmacro defswitch (name &optional function)
  "Cause $function to be called when the switch $name appears in the
   command line.  Name is a simple-string that need only begin with a
   hyphen if switch name really does begin with one.

   If $function is (), then the switch is simply included in
   *command-line-switches*.  This suppresses the warning which would
   otherwise take place.  The warning can also be globally suppressed via
   *complain-about-illegal-switches*."
  (let ((gname (gensym))
	(gfunction (gensym)))
    `(let ((,gname ,name)
	   (,gfunction ,function))
       (check-type ,gname simple-string)
       (check-type ,gfunction (or symbol function) "a symbol or function")
       (push ,gname *legal-cmd-line-switches*)
       (when ,gfunction
	 (push (cons ,gname ,gfunction) *command-switch-demons*)))))

#[ Command Line Options

The following switches can occur on the command line:

  % -quiet

    decrease verbosity of output.

  % -batch

    specifies batch mode, where all input is directed
    from standard input. An error code of 0 is returned upon
    encountering the end of input and 1 on error.  Any files
    given as arguments are loaded before standard input is processed.

  % -core

    requires an argument that should be the name of a
    core file.  Rather than using the default core file
    (\file{lib/lisp.core}), the specified core file is
    loaded.

  % -edit

    specifies to enter the editor.  A file to edit may be
    specified by placing the name of the file between the program name
    (usually "nightshade") and the first switch.

  % -eval

    accepts one argument which should be a Lisp form to evaluate during
    the start up sequence.  The value of the form will not be printed
    unless it is wrapped in a form that does output.

  % -einit

    accepts an argument that should be the name of the editor init file
    to load the first time the function `ed' is invoked.  The default is
    to load "nightshade-ed.fasl", or failing that "nightshade-ed.lisp",
    from the user's home directory.  If the file is in any other
    directory, the full path must be specified.

  % -init

    accepts an argument that should be the name of an init file to load
    during the normal start up sequence.  The default is to load
    "nightshade.fasl" or, failing that, "nightshade.lisp" from the user's
    home directory.  If the file is in any other directory, the full path
    must be specified.

  % -noinit

    accepts no arguments and specifies that an init file should not
    be loaded during the normal start up sequence.  Also, this switch
    suppresses the loading of an editor init file when the editor is
    started up with the -edit switch.

  % -xoff

    specifies that the editor must run in the terminal instead of as an X
    application

  % -load

    accepts an argument which should be the name of a file to load
    into Lisp before entering the Lisp read-eval-print loop.

  % -slave

    specifies that Lisp should start up as a slave Lisp and try to
    connect to an editor Lisp.  The name of the editor to connect to must
    be specified -- to find the editor's name, use the editor "Accept
    Slave Connections" command.  The name for
    the editor Lisp is of the form:

        machine-name:socket

    where machine-name is the internet host name for the machine and
    socket is the decimal number of the socket to connect to.

[Editor Command Line Options] details the use of the -edit and -slave
switches.

Arguments to the above switches can be specified in one of two ways:

    switch=value

 or

    switch value

For example, either of the following two commands start up the saved core
file test.core:

    lisp -core=test.core
    lisp -core test.core
]#

(defun eval-switch-demon (switch)
  (let ((cmds (cmd-switch-arg switch)))
    (do ((length (length cmds))
	 (start 0))
	((>= start length))
      (multiple-value-bind (form next)
	  (read-from-string cmds nil nil :start start)
	(eval form)
	(lisp::finish-standard-output-streams)
	(setf start next)))))
(defswitch "eval" #'eval-switch-demon)

(defun load-switch-demon (switch)
  (load (cmd-switch-arg switch)))
(defswitch "load" #'load-switch-demon)

(defun quiet-switch-demon (switch)
  (declare (ignore switch))
  (setq *load-verbose* nil
        *compile-verbose* nil
        *compile-print* nil
        *compile-progress* nil
        *gc-verbose* nil
        *herald-items* nil))
(defswitch "quiet" #'quiet-switch-demon)

(defun cmd-switch-arg (switch)
  "Return the first value that true out of `cmd-switch-value', the first
   element in `cmd-switch-words', or the first word in
   `command-line-words', each called on $switch."
  (or (cmd-switch-value switch)
      (car (cmd-switch-words switch))
      (car *command-line-words*)))

(defswitch "batch")
(defswitch "core")
(defswitch "init")
(defswitch "noinit")
(defswitch "einit")
(defswitch "dynamic-space-size")
(defswitch "edit")
