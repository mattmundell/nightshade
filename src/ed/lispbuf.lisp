;;; Stuff to do lisp hacking in the editor Lisp environment.

(in-package "ED")

(export '(defun-region))

#[ Manipulating the Editor Process

When customizing the editor, it is useful to be able to manipulate the
editor Lisp environment from the editor.

{command:Editor Describe}
{command:Room}
{command:Editor Load File}

[ Editor Mode    ]
[ Eval Mode      ]
[ Error Handling ]
]#

(define-file-option "Package" (buffer value)
  (defevar "Current Package"
    "The package used for evaluation of Lisp in this buffer."
    :buffer buffer
    :value
    (let* ((eof (list nil))
	   (thing (read-from-string value nil eof)))
      (when (eq thing eof) (error "Bad package file option value."))
      (cond
       ((stringp thing)
	thing)
       ((symbolp thing)
	(symbol-name thing))
       ((characterp thing)
	(string thing))
       (t
	(message
	 "Ignoring \"package\" file option -- cannot convert to a string."))))))


;;;; Eval Mode Interaction.

#[ Eval Mode

`Eval' mode is a minor mode that simulates a read
eval print loop running within the editor process.  Since Lisp
program development is usually done in a separate eval server process (see section
[Eval Servers]), `Eval' mode is used primarily for debugging code
that must run in the editor process.  `Eval' mode shares some commands with
`Typescript' mode: see section [Typescripts].

`Eval' mode doesn't completely support terminal I/O: it binds
`standard-output' to a stream that inserts into the buffer and
`standard-input' to a stream that signals an error for all operations.
the editor cannot correctly support the interactive evaluation of forms that read
from the `Eval' interactive buffer.

{command:Select Eval Buffer}
{command:Confirm Eval Input}
{command:Abort Eval Input}
]#

(proclaim '(special * ** *** - + ++ +++ / // /// *prompt*))

(defun setup-eval-mode (buffer)
  (let ((point (buffer-point buffer)))
    (setf (buffer-minor-mode buffer "Eval") t)
    (setf (buffer-minor-mode buffer "Editor") t)
    (setf (buffer-major-mode buffer) "Lisp")
    (buffer-end point)
    (defevar "Current Package"
      "This variable holds the name of the package currently used for Lisp
       evaluation and compilation.  If it is Nil, the value of *Package* is used
       instead."
      :buffer buffer)
    (unless (editor-bound-p 'buffer-input-mark :buffer buffer)
      (defevar "Buffer Input Mark"
	"Mark used for Eval Mode input."
	:buffer buffer
	:value (copy-mark point :right-inserting))
      (defevar "Eval Output Stream"
	"Output stream used for Eval Mode output in this buffer."
	:buffer buffer
	:value (make-editor-output-stream point))
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
	:value 0))
    (let ((*standard-output*
	   (variable-value 'eval-output-stream :buffer buffer)))
      (fresh-line)
      (princ (if (functionp *prompt*)
		 (funcall *prompt*)
		 *prompt*)))
    (move-mark (variable-value 'buffer-input-mark :buffer buffer) point)))

(defmode "Eval" :major-p nil :setup-function #'setup-eval-mode)

(defun eval-mode-lisp-mode-hook (buffer on)
  "Turn on Lisp mode when we go into Eval Mode."
  (when on
    (setf (buffer-major-mode buffer) "Lisp")))
;;;
(add-hook eval-mode-hook 'eval-mode-lisp-mode-hook)

(defevar "Editor Definition Info"
  "When this is true, the editor Lisp is used to determine definition
   editing information; otherwise, the slave Lisp is used."
  :value t
  :mode "Eval")

(defvar *selected-eval-buffer* nil)

(defcommand "Select Eval Buffer" ()
  "Change to the `Eval' buffer if it exists, else create one.  The `Eval'
   buffer is created with `Lisp' as the major mode and `Eval' and `Editor'
   as minor modes."
  (unless *selected-eval-buffer*
    (if (getstring "Eval" *buffer-names*)
	(editor-error "There is already a buffer named \"Eval\"."))
    (setf *selected-eval-buffer*
	  (make-buffer "Eval"
		       :delete-hook
		       (list #'(lambda (buf)
				 (declare (ignore buf))
				 (setf *selected-eval-buffer* nil)))))
    (setf (buffer-minor-mode *selected-eval-buffer* "Eval") t))
  (change-to-buffer *selected-eval-buffer*))

(defvar lispbuf-eof '(nil))

(defevar "Unwedge Interactive Input Confirm"
  "When set, confirmation of interactive input when the point is before the
   input mark causes the editor to ask the user if the point needs to be
   unwedged.  When clear, an editor error is signaled, informing the user
   that the point is before the input mark."
  :value t)

(defun unwedge-eval-buffer ()
  (abort-eval-input-command))

(defevar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-eval-buffer
  :mode "Eval")

(defevar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Prompt again at the end of the buffer? "
  :mode "Eval")

(defcommand "Confirm Eval Input" ()
  "Evaluate all the forms between the end of the last output and the end of
   the buffer, inserting the results of their evaluation in the buffer.
   Beep if the form is erroneous.  Use Linefeed to insert line breaks in
   the middle of a form.

   Use `Unwedge Interactive Input Confirm' in the same way `Confirm
   Interactive Input' does."
  (let ((input-region (get-interactive-input)))
    (when input-region
      (let* ((output (value eval-output-stream))
	     (*standard-output* output)
	     (*error-output* output)
	     (*trace-output* output))
	(fresh-line)
	(in-lisp
	 ;; Copy the region to keep the output and input streams from interacting
	 ;; since input-region is made of permanent marks into the buffer.
	 (with-input-from-region (stream (copy-region input-region))
	   (loop
	     (let ((form (read stream nil lispbuf-eof)))
	       (when (eq form lispbuf-eof)
		 ;; Move the buffer's input mark to the end of the buffer.
		 (move-mark (region-start input-region)
			    (region-end input-region))
		 (return))
	       (setq +++ ++ ++ + + - - form)
	       (let ((this-eval (multiple-value-list (eval form))))
		 (fresh-line)
		 (dolist (x this-eval) (prin1 x) (terpri))
		 (princ (if (functionp *prompt*)
			    (funcall *prompt*)
			    *prompt*))
		 (setq /// // // / / this-eval)
		 (setq *** ** ** * * (car this-eval)))))))))))

(defcommand "Abort Eval Input" ()
  "Move to the end of the buffer and prompt."
  (let ((point (current-point)))
    (buffer-end point)
    (insert-character point #\newline)
    (insert-string point "Aborted.")
    (insert-character point #\newline)
    (insert-string point
		   (if (functionp *prompt*)
		       (funcall *prompt*)
		       *prompt*))
    (move-mark (value buffer-input-mark) point)))


;;;; General interactive commands used in eval and typescript buffers.

(defun get-interactive-input ()
  "Tries to return a region.  When the point is not past the input mark,
   and *Unwedge Interactive Input Confirm* is set, the buffer is optionally
   fixed up, and () is returned.  Otherwise, an editor error is signalled.
   When a region is returned, the start is the current buffer's input mark,
   and the end is the current point moved to the end of the buffer."
  (let ((point (current-point))
	(mark (value buffer-input-mark)))
    (cond
     ((mark>= point mark)
      (buffer-end point)
      (let* ((input-region (region mark point))
	     (string (region-to-string input-region))
	     (ring (value interactive-history)))
	(if (and (or (zerop (ring-length ring))
		     (string/= string (region-to-string (ring-ref ring 0))))
		 (> (length string) (value minimum-interactive-input-length)))
	    (ring-push (copy-region input-region) ring))
	input-region))
     ((value unwedge-interactive-input-confirm)
      (beep)
      (when (prompt-for-y-or-n
	     :prompt (concatenate 'simple-string
				  "Point before input mark.  "
				  (value unwedge-interactive-input-string))
	     :must-exist t :default t :default-string "Y")
	(funcall (value unwedge-interactive-input-fun))
	(message "Point moved to input mark."))
      nil)
     (t
      (editor-error "Point before input mark.")))))

(defevar "Interactive History Length"
  "The length used for the history ring in interactive buffers.  Must be
   set before turning on the mode."
  :value 350)

(defevar "Minimum Interactive Input Length"
  "When the number of characters in an interactive buffer exceeds this
   value, it is pushed onto the interactive history."
  :value 2)

(defvar *previous-input-search-string* "initial")

(defvar *previous-input-search-pattern*
  ;; Give it a bogus string since you can't give it the empty string.
  (new-search-pattern :string-insensitive :forward "initial"))

(defun get-previous-input-search-pattern (string)
  (if (string= *previous-input-search-string* string)
      *previous-input-search-pattern*
      (new-search-pattern :string-insensitive :forward
			  (setf *previous-input-search-string* string)
			  *previous-input-search-pattern*)))

(defcommand "Search Previous Interactive Input" ()
  "Search backward through the interactive history using the current input
   as a search string.  When invoked directly after an invocation, repeat
   the previous search."
  (let* ((mark (value buffer-input-mark))
	 (ring (value interactive-history))
	 (point (current-point))
	 (just-invoked (eq (last-command-type) :searching-interactive-input)))
    (if (mark<= point mark)
	(editor-error "Point not past input mark."))
    (if (zerop (ring-length ring))
	(editor-error "No previous input in this buffer."))
    (or just-invoked
	(get-previous-input-search-pattern (region-to-string (region mark point))))
    (let ((found-it (find-previous-input ring just-invoked)))
      (or found-it
	  (editor-error "Couldn't find ~a." *previous-input-search-string*))
      (delete-region (region mark point))
      (insert-region point (ring-ref ring found-it))
      (setf (value searching-interactive-pointer) found-it))
  (setf (last-command-type) :searching-interactive-input)))

(defun find-previous-input (ring againp)
  (let ((ring-length (ring-length ring))
	(base (if againp
		  (+ (value searching-interactive-pointer) 1)
		  0)))
      (loop
	(when (= base ring-length)
	  (if againp
	      (setf base 0)
	      (return nil)))
	(with-mark ((m (region-start (ring-ref ring base))))
	  (when (find-pattern m *previous-input-search-pattern*)
	    (return base)))
	(incf base))))

(defcommand "Previous Interactive Input" (p)
  "Rotate the interactive history forward, inserting the current entry in
   the buffer.  Leave the region around the inserted text.  With a prefix
   argument, rotate that many times."
  (let* ((point (current-point))
	 (mark (value buffer-input-mark))
	 (ring (value interactive-history))
	 (length (ring-length ring))
	 (p (or p 1)))
    (if (zerop length) (editor-error "Interactive history empty."))
    (if (mark< point mark)
	(editor-error
	 "Point must be after the buffer input mark (the prompt)."))
    (cond
     ((eq (last-command-type) :interactive-history)
      (let ((base (mod (+ (value interactive-pointer) p) length)))
	(delete-region (region mark point))
	(insert-region point (ring-ref ring base))
	(setf (value interactive-pointer) base)))
     (t
      (let ((base (mod (if (minusp p) p (1- p)) length))
	    (region (delete-and-save-region (region mark point))))
	(insert-region point (ring-ref ring base))
	(or (mark= (region-start region) (region-end region))
	    (progn
	      (ring-push region ring)
	      (incf base)))
	(setf (value interactive-pointer) base)))))
  (setf (last-command-type) :interactive-history))

(defcommand "Next Interactive Input" (p)
  "Rotate the interactive history backwards, inserting the current entry in
   the buffer.  Leave the region around the inserted text.  With a prefix
   argument, rotate that many times."
  (previous-interactive-input-command (- (or p 1))))

(defcommand "Kill Interactive Input" ()
  "Kill any input to an interactive mode (Eval or Typescript)."
  (let ((point (buffer-point (current-buffer)))
	(mark (value buffer-input-mark)))
    (if (mark< point mark)
	(editor-error
	 "Point must be after the buffer input mark (the prompt)."))
    (kill-region (region mark point) :kill-backward)))

(defcommand "Interactive Beginning of Line" (p)
  "If on line with current prompt, go to the end of the prompt, otherwise
   move the point to the beginning of the current line.  With prefix
   argument, move the point to the beginning of the prefix'th next line."
  (let ((mark (value buffer-input-mark))
	(point (current-point)))
    (if (and (same-line-p point mark) (or (not p) (= p 1)))
	(move-mark point mark)
	(beginning-of-line-command p))))

(defcommand "Reenter Interactive Input" ()
  "Copies the form to the left of point to be after the interactive
   buffer's input mark.  When the current region is active, copy the
   current region instead.

   This is sometimes easier to use to get a previous input that is either
   so far back that it has fallen off the history or is visible and more
   readily yanked than gotten with successive invocations of the history
   commands."
  (or (editor-bound-p 'buffer-input-mark)
      (editor-error "Must be in an interactive buffer."))
  (let ((point (current-point)))
    (let ((region (if (region-active-p)
		      ;; Copy this, so moving point doesn't affect the region.
		      (copy-region (current-region))
		      (with-mark ((start point)
				  (end point))
			(pre-command-parse-check start)
			(or (form-offset start -1)
			    (editor-error "Not after complete form."))
			(region (copy-mark start) (copy-mark end))))))
      (buffer-end point)
      (push-buffer-mark (copy-mark point))
      (insert-region point region)
      (setf (last-command-type) :ephemerally-active))))


;;; Other stuff.

#[ Editor Mode

When `Editor' mode is on, alternate versions of the Lisp interaction
commands are bound in place of the eval server based commands.  These commands
manipulate the editor process instead of the current eval server.  Turning on
editor mode in a buffer allows incremental development of code within the
running editor.

{mode:Editor}

The following commands are similar to the standard commands, but modify or
examine the Lisp process that the editor is running in.  Terminal I/O is
done on the initial window for the editor's Lisp process.  Output is
directed to a pop-up window or the editor's window instead of to the
background buffer.

FIX link to standard commands

    Editor Compile Defun
    Editor Compile Region
    Editor Evaluate Buffer
    Editor Evaluate Defun
    Editor Evaluate Region
    Editor Macroexpand Expression
    Editor Evaluate Defvar
    Editor Describe Function Call
    Editor Describe Symbol

    Editor Compile Buffer File}
    Editor Compile File}
    Editor Compile Group

In addition to compiling in the editor process, these commands differ from
the eval server versions in that they direct output to the the `Compiler
Warnings' buffer.

{command:Editor Evaluate Expression}
]#

(defmode "Editor"
  :documentation
  "When in editor mode, most lisp compilation and evaluation commands
   manipulate the editor process instead of the current eval server.")

(define-file-option "Editor" (buffer value)
  (declare (ignore value))
  (setf (buffer-minor-mode buffer "Editor") t))

(defevar "Editor Definition Info"
  "When this is true, the editor Lisp is used to determine definition
   editing information; otherwise, the slave Lisp is used."
  :value t
  :mode "Editor")

(defcommand "Editor Compile Defun" ()
  "Compile the current or next top-level form in the editor Lisp.  First
   the form is evaluated, then the result of this evaluation is passed to
   compile.  If the current region is active, this compiles the region."
  "Evaluate the current or next top-level form in the editor Lisp."
  (if (region-active-p)
      (editor-compile-region (current-region))
      (editor-compile-region (defun-region (current-point)) t)))

(defcommand "Editor Compile Region" ()
  "Compiles lisp forms between the point and the mark in the editor Lisp."
  (editor-compile-region (current-region)))

(defun previous-sexp-region (mark)
  "This returns the region around the current or next defun with respect to mark.
   Mark is not used to form the region.  If there is no appropriate top level
   form, this signals an editor-error.  This calls PRE-COMMAND-PARSE-CHECK."
  (with-mark ((start mark)
	      (end mark))
    (pre-command-parse-check start) ; FIX required?
    (or (mark-previous-sexp start end)
	(editor-error "No current or next top level form."))
    (region start end)))

(defun defun-region (mark)
  "Return a region around the current or next defun with respect to $mark.
   Allocate new marks to form the region.  Signal an editor error on
   failing to find an appropriate top level form.  Call
   `pre-command-parse-check' first."
  (with-mark ((start mark)
	      (end mark))
    (pre-command-parse-check start)
    (cond ((not (mark-top-level-form start end))
	   (editor-error "No current or next top level form."))
	  (t (region start end)))))

(defun editor-compile-region (region &optional quiet)
  (or quiet (message "Compiling region ..."))
  (in-lisp
   (with-input-from-region (stream region)
     (with-pop-up-display (*error-output* :height 19)
       (c::compile-from-stream stream
			       :source-info
			       (buffer-pathname (current-buffer)))))))

(defcommand "Editor Evaluate Defun" (p)
  "Evaluates the current or next top-level form in the editor Lisp.  If the
   current region is active, this evaluates the region.  With a prefix
   argument this inserts the result at point."
  "Evaluates the current or next top-level form in the editor Lisp."
  (if (region-active-p)
      (editor-evaluate-region-command)
      (with-input-from-region (stream (defun-region (current-point)))
	(clear-echo-area)
	(in-lisp
	 (let* ((buffer (current-buffer))
		(eval::*interp-source-hack* (buffer-pathname buffer))
		(eval::*interp-position-hack*
		 (count-characters
		  (region (buffer-start-mark buffer)
			  (buffer-point buffer))))
		(form (read stream)))
	   (when (eq (car form) 'defvar)
	     ;; FIX check if already bound
	     (message "Resetting value of variable ~A..." (cadr form))
	     (makunbound (cadr form)))
	   (let ((str (format () "~S" (eval form
					    (buffer-pathname buffer)
					    (count-characters
					     (region (buffer-start-mark buffer)
						     (buffer-point buffer)))))))
	     (if p
		 (insert-string (current-point) str)
		 (message "Editor Evaluation returned ~S" str))))))))

(defcommand "Editor Evaluate Region" ()
  "Evaluates lisp forms between the point and the mark in the editor Lisp."
  (with-input-from-region (stream (current-region))
    (clear-echo-area)
    (write-string "Evaluating region in the editor ..." *echo-area-stream*)
    (finish-output *echo-area-stream*)
    (in-lisp
     (do ((object (read stream nil lispbuf-eof)
		  (read stream nil lispbuf-eof)))
	 ((eq object lispbuf-eof))
       (eval object)))
    (message "Evaluation complete.")))

(defcommand "Editor Evaluate Defvar" ()
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound.  This occurs in the editor Lisp."
  (with-input-from-region (stream (defun-region (current-point)))
    (clear-echo-area)
    (in-lisp
     (let ((form (read stream)))
       (or (eq (car form) 'defvar)
	   (editor-error "Form must be a defvar."))
       (makunbound (cadr form))
       (message "Evaluation returned ~S" (eval form))))))

(defcommand "Editor Macroexpand Expression" (p)
  "Show the macroexpansion of the current expression in the null
   environment.  With an argument, use `macroexpand' instead of
   `macroexpand-1'."
  (let ((point (buffer-point (current-buffer))))
    (with-mark ((start point))
      (pre-command-parse-check start)
      (with-mark ((end start))
        (or (form-offset end 1)
	    (editor-error "Point must be followed by a form to expand."))
	(or (in-lisp
	     (with-input-from-region (s (region start end))
	       (let ((name (read s ())))
		 (when name
		   (with-pop-up-display (rts)
		     (write-string (prin1-to-string (funcall (if p
								 'macroexpand
								 'macroexpand-1)
							     name))
				   rts)
		     t)))))
	    (editor-error "List must be a function call."))))))

(defcommand "Editor Evaluate Expression" (p)
  "Evaluate a prompted expression in the editor Lisp, displaying the result
   in the echo area.  With a prefix argument insert the result at point."
  (in-lisp
   (let ((str (multiple-value-call #'format () "~@{~#[~;~S~:;~S, ~]~}"
		(eval (prompt-for-expression
		       :prompt "Editor Eval: "
		       :help "Expression to evaluate")))))
     (if p
	 (insert-string (current-point) str)
	 (let ((int (parse-integer str :errorp ())))
	   (if int
	       (if (and (plusp int) (< int 256)) ; FIX (plusp (< int 256))?
		   (message "=> ~A (#x~X #o~O #b~B #\\~C)" int int int int (code-char int))
		   (message "=> ~A (#x~X #o~O #b~B)" int int int int))
	       (message "=> ~A" str)))))))

(defcommand "Editor Evaluate Buffer" ()
  "Evaluates the text in the current buffer in the editor Lisp."
  "Evaluates the text in the current buffer redirecting *Standard-Output* to
   the echo area.  This occurs in the editor Lisp.  The prefix argument is
   ignored."
  (clear-echo-area)
  (write-string "Evaluating buffer in the editor ..." *echo-area-stream*)
  (finish-output *echo-area-stream*)
  (with-input-from-region (stream (buffer-region (current-buffer)))
    (let ((*standard-output* *echo-area-stream*))
      (in-lisp
       (do ((object (read stream nil lispbuf-eof)
		    (read stream nil lispbuf-eof)))
	   ((eq object lispbuf-eof))
	 (eval object))))
    (message "Evaluation complete.")))

(defcommand "Editor Compile File" ()
  "Prompts for file to compile in the editor Lisp.  Does not compare source
   and binary write dates.  Does not check any buffer for that file for
   whether the buffer needs to be saved."
  "Prompts for file to compile."
  (let ((pn (prompt-for-file :default
			     (buffer-default-pathname (current-buffer))
			     :prompt "File to compile: ")))
    (with-output-to-window (*error-output* "Compiler Warnings")
      (in-lisp (compile-file (namestring pn) :error-file nil)))))

(defcommand "Editor Compile Buffer File" (p)
  "Compile the file in the current buffer in the editor Lisp if its
   associated binary file (of type .fasl) is older than the source or
   doesn't exist.  When the binary file is up to date, the user is asked if
   the source should be compiled anyway.  When the prefix argument is
   supplied, compile the file without checking the binary file.  When
   *Compile Buffer File Confirm* is set, this command will ask for
   confirmation when it otherwise would not."
  "Compile the file in the current buffer in the editor Lisp if the fasl
   file isn't up to date.  When $p, always do it."
  (let* ((buf (current-buffer))
	 (pn (buffer-pathname buf)))
    (or pn (editor-error "Buffer must have an associated pathname."))
    (cond ((buffer-modified buf)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Save and compile file ~A? "
				    (namestring pn))))
	     (write-buffer-file buf pn)
	     (with-output-to-window (*error-output* "Compiler Warnings")
	       (in-lisp (compile-file (namestring pn) :error-file nil)))))
	  ((older-or-non-existent-fasl-p pn p)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Compile file ~A? " (namestring pn))))
	     (with-output-to-window (*error-output* "Compiler Warnings")
	       (in-lisp (compile-file (namestring pn) :error-file nil)))))
	  (t (when (or p
		       (prompt-for-y-or-n
			:default t :default-string "Y"
			:prompt
			"Fasl file up to date, compile source anyway? "))
	       (with-output-to-window (*error-output* "Compiler Warnings")
		 (in-lisp (compile-file (namestring pn) :error-file nil))))))))

(defcommand "Editor Compile Group" (p)
  "Compile each file in the current group which needs it in the editor Lisp.
   If a file has type LISP and there is a curresponding file with type
   FASL which has been written less recently (or it doesn't exit), then
   the file is compiled, with error output directed to the \"Compiler Warnings\"
   buffer.  If a prefix argument is provided, then all the files are compiled.
   All modified files are saved beforehand."
  "Do a Compile-File in each file in the current group that seems to need it
   in the editor Lisp."
  (save-all-files-command)
  (unless *active-file-group* (editor-error "No active file group."))
  (dolist (file *active-file-group*)
    (when (string-equal (pathname-type file) "lisp")
      (let ((tn (probe-file file)))
	(cond ((not tn)
	       (message "File ~A not found." (namestring file)))
	      ((older-or-non-existent-fasl-p tn p)
	       (with-output-to-window (*error-output* "Compiler Warnings")
		 (in-lisp (compile-file (namestring tn) :error-file nil)))))))))

(defcommand "List Compile Group" ()
  "List any files that would be compiled by `Compile Group'.  All Modified
   files are saved before checking to generate a consistent list."
  (save-all-files-command)
  (or *active-file-group* (editor-error "No active file group."))
  (with-pop-up-display (s)
    (write-line "\"Compile Group\" would compile the following files:" s)
    (force-output s)
    (dolist (file *active-file-group*)
      (when (string-equal (pathname-type file) "lisp")
	(let ((tn (probe-file file)))
	  (cond ((not tn)
		 (format s "File ~A not found.~%" (namestring file)))
		((older-or-non-existent-fasl-p tn)
		 (write-line (namestring tn) s)))
	  (force-output s))))))

(defevar "Load Pathname Defaults"
  "The fallback pathname for the `Load File' command.")

(defcommand "Editor Load File" ()
  "Load a prompted file into the editor Lisp."
  (let ((name (truename (prompt-for-file
			 :default
			 (or (value load-pathname-defaults)
			     (buffer-default-pathname (current-buffer)))
			 :prompt "Editor file to load: "
			 :help "The name of the file to load"))))
    (setv load-pathname-defaults name)
    (in-lisp (load name))))


;;;; Lisp documentation stuff.

;;; FUNCTION-TO-DESCRIBE is used in "Editor Describe Function Call" and
;;; "Describe Function Call".
;;;
(defmacro function-to-describe (var error-name)
  `(cond ((not (symbolp ,var))
	  (,error-name "~S is not a symbol." ,var))
	 ((macro-function ,var))
	 ((fboundp ,var)
	  (if (listp (symbol-function ,var))
	      ,var
	      (symbol-function ,var)))
	 (t
	  (,error-name "~S is not a function." ,var))))

(defcommand "Editor Describe Function Call" ()
  "Describe the most recently typed function name in the editor Lisp."
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    (pre-command-parse-check mark1)
    (or (backward-up-list mark1)
	(editor-error "Point must be enclosed by a list."))
    (form-offset (move-mark mark2 (mark-after mark1)) 1)
    (with-input-from-region (s (region mark1 mark2))
      (or (in-lisp
	   (let ((sym (read s ())))
	     (when sym
	       (let ((fun (function-to-describe sym editor-error)))
		 (with-pop-up-display (*standard-output*)
		   (editor-describe-function fun sym)))
	       t)))
	  (editor-error "List must be a function call.")))))

(defcommand "Editor Describe Symbol" ()
  "Describe the previous s-expression if it is a symbol in the editor Lisp."
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    (mark-symbol mark1 mark2)
    (with-input-from-region (s (region mark1 mark2))
      (in-lisp
       (let ((thing (read s)))
	 (if (symbolp thing)
	     (with-pop-up-display (*standard-output*)
	       (describe thing))
	     (if (and (consp thing)
		      (or (eq (car thing) 'quote)
			  (eq (car thing) 'function))
		      (symbolp (cadr thing)))
		 (with-pop-up-display (*standard-output*)
		   (describe (cadr thing)))
		 (editor-error "~S is not a symbol, or 'symbol, or #'symbol."
			       thing))))))))

;;; MARK-SYMBOL moves mark1 and mark2 around the previous or current
;;; symbol.  However, if the marks are immediately before the first
;;; constituent char of the symbol name, we use the next symbol since the
;;; marks probably correspond to the point, and the editor's cursor display
;;; makes it look like the point is within the symbol name.  This also
;;; tries to ignore :prefix characters such as quotes, commas, etc.
;;;
(defun mark-symbol (mark1 mark2)
  (pre-command-parse-check mark1)
  (with-mark ((tmark1 mark1)
	      (tmark2 mark1))
    (cond ((and (form-offset tmark1 1)
		(form-offset (move-mark tmark2 tmark1) -1)
		(or (mark= mark1 tmark2)
		    (and (find-attribute tmark2 :lisp-syntax
					 #'(lambda (x) (not (eq x :prefix))))
			 (mark= mark1 tmark2))))
	   (form-offset mark2 1))
	  (t
	   (form-offset mark1 -1)
	   (find-attribute mark1 :lisp-syntax
			   #'(lambda (x) (not (eq x :prefix))))
	   (form-offset (move-mark mark2 mark1) 1)))))

(defcommand "Editor Describe" ()
  "Prompt for an expression, then evaluate the expression and describes the
   result in the editor process."
  (in-lisp
   (let* ((exp (prompt-for-expression
		:prompt "Object: "
		:help "Expression to evaluate to get object to describe."))
	  (obj (eval exp)))
     (with-pop-up-display (*standard-output*)
       (describe obj)))))

(defcommand "Editor Describe Function" ()
  "Prompt for the name of a function and describe the function."
  (let* ((name (prompt-for-string
		:prompt "Function: "
		:default (name-at-point)
		:help "Name of function to describe."))
	 (function (symbol-function (read-from-string name))))
    (in-lisp
     (with-pop-up-display (*standard-output*
			   :reference
			   (list (fun-defined-from-pathname function)
				 :function
				 name))
       (describe function)))))

(defcommand "Editor Describe Variable" ()
  "Prompt for the name of a variable and describe the variable."
  (let* ((name (prompt-for-string
		:prompt "Variable: "
		:default (word-at-point)
		:help "Name of variable to describe."))
	 (variable (read-from-string name)))
    (in-lisp
     (with-pop-up-display (*standard-output*
; FIX
; 			   :reference
; 			   (list (fun-defined-from-pathname function)
; 				 :function
; 				 name)
			   )
       (describe variable)))))


#[ Filtering

Filtering is a simple way to perform a fairly arbitrary transformation
on text.  Filtering text replaces the string in each line with the result
of applying a Lisp function of one argument to that string.  The function must
neither destructively modify the argument nor the return value.  It is an
error for the function to return a string containing newline characters.

{command:Filter Region}
]#

(defcommand "Filter Region" ()
  "Evaluate a prompted expression to obtain a function and apply the
   function to each line of text in the current region.  The function must
   take the line as a constant string and return the new version of the
   string.  For example, to capitalize all the lines in the region one
   could respond as follows.

       Function: #'string-capitalize

   Since the function may be called many times, it may need to be compiled.
   Functions for one-time use can be compiled using the compile function as
   in the following example which removes all the semicolons on any line
   which contains the string \"PASCAL\".

	Function: (compile () '(lambda (s)
				  (if (search \"PASCAL\" s)
				      (remove #\; s)
				      s)))"
  (let* ((exp (prompt-for-expression
	       :prompt "Function: "
	       :help "Expression to evaluate to get function to use as filter."))
	 (fun (in-lisp (eval exp)))
	 (region (current-region)))
    (check-region-query-size region)
    (let* ((start (copy-mark (region-start region) :left-inserting))
	   (end (copy-mark (region-end region) :left-inserting))
	   (region (region start end))
	   (undo-region (copy-region region)))
      (filter-region fun region)
      (make-region-undo :twiddle "Filter Region" region undo-region))))
