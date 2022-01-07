;;; File and buffer manipulating commands.

(in-package "ED")

(export '(buffer-default-pathname *buffer-history* change-to-buffer
	  delete-buffer-if-possible delete-buffer-safely find-file-buffer
	  pathname-to-buffer-name previous-buffer process-file-options
	  read-buffer-file write-buffer-file
	  define-file-option define-file-type-hook))


;;;; process-file-options.

(defvar *content-type-handlers* ()
  "Content type handlers.  Modify with `define-content-type'.")

(defvar *mode-option-handlers* ()
  "File option handlers.  Modify with `define-file-option'.")

(defvar *file-type-hooks* ()
  "Hooks to run according to file type on reading a file.  Modify with
   `define-file-type-hook'.")

(defvar *file-pathname-hooks* ()
  "Hooks to run according to file pathname on reading a file.  Modify with
   `define-file-pathname-hook'.")

(defvar *file-name-hooks* ()
  "Hooks to run according to file name on reading a file.  Modify with
   `define-file-name-hook'.")

(defvar *file-content-hooks* ()
  "Hooks to run according to file content on reading a file.  Modify with
   `define-file-content-hook'.")

(defvar *loader-directive-handlers* ()
  "Hooks to run according to file loader directive on reading a file.
   Modify with `define-loader-directive-handlers'.")

;; FIX rename, maybe make more general, mv to code:?  trim-white? white-trim?
(defun trim-subseq (string start &optional end)
  (declare (simple-string string))
  (string-trim '(#\Space #\Tab) (subseq string start end)))

;;; PROCESS-FILE-OPTIONS checks the first line of buffer for the file options
;;; indicator "-*-".  If it finds this, then it enters a do-file-options block.
;;; If any parsing errors occur while picking out options, we return from this
;;; block.  Staying inside this function at this point, allows us to still set
;;; a major mode if no file option specified one.
;;;
;;; We also cater to old style mode comments:
;;;    -*- Lisp -*-
;;;    -*- Text -*-
;;; This kicks in if we find no colon on the file options line.
;;;
(defun process-file-options (buffer &optional
				    (pathname (buffer-pathname buffer)))
  "Check for file options in BUFFER and invoke handlers if there are any.
   PATHNAME falls back to buffer's pathname and may be ().  First look for
   a `Mode' file option that specifies a major mode, and failing that if
   pathname has a type, then try to invoke the appropriate file type hook.
   `read-buffer-file' calls this."
  (let* ((string
	  (line-string (mark-line (buffer-start-mark buffer))))
	 (found (search "-*-" string))
	 (major-mode ())
	 (type (if pathname (pathname-type pathname))))
    (declare (simple-string string))
    (when found
      (block do-file-options
	(let* ((start (+ found 3))
	       (end (search "-*-" string :start2 start)))
	  (unless end
	    (loud-message "No closing \"-*-\".  Aborting file options.")
	    (return-from do-file-options))
	  (cond
	   ((find #\: string :start start :end end)
	    (do ((opt-start start (1+ semi)) colon semi)
		(nil)
	      (setq colon (position #\: string :start opt-start :end end))
	      (unless colon
		(loud-message "Missing \":\".  Aborting file options.")
		(return-from do-file-options))
	      (setq semi (or (position #\; string :start colon :end end) end))
	      (let* ((option (nstring-downcase
			      (trim-subseq string opt-start colon)))
		     (handler (assoc option *mode-option-handlers*
				     :test #'string=)))
		(declare (simple-string option))
		(cond
		 (handler
		  (let ((result (funcall (cdr handler) buffer
					 (trim-subseq string (1+ colon) semi))))
		    (when (string= option "mode")
		      (setq major-mode result))))
		 (t (message "Unknown file option: ~S" option)))
		(when (= semi end) (return nil)))))
	   (t
	    ;; Old style mode comment.
	    (if (funcall (cdr (assoc "mode" *mode-option-handlers*
				     :test #'string=))
			 buffer (trim-subseq string start end))
		(setq major-mode t)))))))
    (or major-mode
	;; FIX should this be a content hook?
	;; Look for "Content-Type: <mime-type>" directive at buffer
	(with-mark ((mark (buffer-start-mark buffer)))
	  (let ((string (line-string (mark-line mark))))
	    (when (and (> (length string) 14)
		       (string= string "Content-Type: " :end1 14))
	      (msg "sub ~A" (trim-subseq string 14))
	      (let ((hook (assoc (trim-subseq string 14)
				 *content-type-handlers*
				 :test #'string=)))
		(when hook
		  (msg "call")
		  (funcall (cdr hook) buffer type)
		  t)))))
	(let ((hook))
	  ;; Try name hooks.
	  (progn
	    (setq hook (assoc (pathname-name pathname)
			      *file-name-hooks* :test #'string=))
	    (when hook (funcall (cdr hook)
				buffer
				(pathname-name pathname)
				pathname)))
	  (or hook
	      ;; Try pathname hooks.
	      (progn
		(setq hook
		      (assoc (namestring (truename pathname))
			     *file-pathname-hooks*
			     :test
			     (lambda (name hooks-name)
			       (and (probe-file hooks-name)
				    (string= (namestring
					      (truename hooks-name))
					     name)))))
		(when hook (funcall (cdr hook) buffer type))))
	  (or hook
	      ;; Try type hooks.
	      (when type
		(setq hook (assoc (string-downcase type) *file-type-hooks*
				  :test #'string=))
		(when hook (funcall (cdr hook) buffer type))))
	  (or hook
	      ;; Try content hooks.
	      (while ((hooks *file-content-hooks* (cdr hooks)))
		     (hooks)
		(if (setq hook (funcall (cdar hooks) buffer))
		    (return))))
	  (or hook
	      ;; Look for shell loader directive (e.g. #!/bin/lisp).
	      (with-mark ((mark (buffer-start-mark buffer)))
		(when (find-attribute mark :whitespace #'zerop)
		  (when (eq (next-character mark) #\#)
		    (mark-after mark)
		    (when (eq (next-character mark) #\!)
		      (mark-after mark)
		      (find-attribute mark :whitespace #'zerop)
		      (with-mark ((tem mark))
			(find-attribute tem :whitespace)
			(let* ((name (region-to-string (region mark tem)))
			       (hook (assoc (pathname-name name)
					    *loader-directive-handlers*
					    :test #'string=)))
			  (when hook
			    (funcall (cdr hook)
				     buffer
				     (pathname-name name))))))))))))))

(defun process-filename-options (buffer &optional
					(pathname (buffer-pathname buffer)))
  "Call buffer hooks according to pathname.  Called by `read-buffer-file'."
  (let ((type (if pathname (pathname-type pathname)))
	(hook))
    ;; Try name hooks.
    (progn
      (setq hook (assoc (pathname-name pathname)
			*file-name-hooks* :test #'string=))
      (when hook (funcall (cdr hook)
			  buffer
			  (pathname-name pathname)
			  pathname)))
    (or hook
	;; Try pathname hooks.
	(progn
	  (setq hook
		;; `p-file-o' does (truename pathname).
		(assoc (namestring pathname)
		       *file-pathname-hooks*
		       :test
		       (lambda (name hooks-name)
			 (and (probe-file hooks-name)
			      (string= (namestring
					(truename hooks-name))
				       name)))))
	  (when hook (funcall (cdr hook) buffer type))))
    (or hook
	;; Try type hooks.
	(when type
	  (setq hook (assoc (string-downcase type) *file-type-hooks*
			    :test #'string=))
	  (when hook (funcall (cdr hook) buffer type))))))

#[ File Options and Type Hooks

The user specifies file options with a special syntax on the first line of a
file.  If the first line contains the string "-*-", then the editor
interprets the text between the first such occurrence and the second, which
must be contained in one line, as a list of "option: value"
pairs separated by semicolons.  The following is a typical example:

    ;;; -*- Mode: Lisp, Editor; Package: Ed -*-

[Type Hooks and File Options] in the Editor Manual has more details and
predefined options.

File type hooks are executed when the editor reads a file into a buffer
based on the type of the pathname.  When the user specifies a `Mode' file
option that turns on a major mode, the editor ignores type hooks.  This
mechanism is mostly used as a simple means for turning on some appropriate
default major mode.

{function:ed:define-file-option}
{function:ed:define-file-type-hook}
{function:ed:process-file-options}
]#


;;;; File options, and type and pathname hooks.

(defmacro define-file-option (name (buffer value) &body body)
  "Define a new file option with $name.  The $body is evaluated with
   $buffer bound to the buffer that the file has been read into and $value
   to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name *mode-option-handlers*  :test #'string=)
		    (car (push (cons ,name nil) *mode-option-handlers*))))
	   #'(lambda (,buffer ,value) ,@body))))

(define-file-option "Mode" (buffer str)
  (let ((seen-major-mode-p)
	(lastpos 0))
    (loop
      (let* ((pos (position #\, str :start lastpos))
	     (substr (trim-subseq str lastpos pos)))
	(cond ((getstring substr *mode-names*)
	       (cond ((mode-major-p substr)
		      (when seen-major-mode-p
			(loud-message
			 "Major mode already processed. Using ~S now."
			 substr))
		      (setf seen-major-mode-p t)
		      (setf (buffer-major-mode buffer) substr))
		     (t
 		      (setf (buffer-minor-mode buffer substr) t))))
	      (t
	       (loud-message "Failed to find major mode ~S -- skipped."
			     substr)))
	(or pos (return seen-major-mode-p))
	(setf lastpos (1+ pos))))))

;; FIX Consider generic var-setting fallback.
(define-file-option "Flush-Trailing-Whitespace" (buffer str)
  (defevar "Flush Trailing Whitespace"
    "If true then flush trailing whitespace."
    :buffer buffer
    :value (if (equal (string-downcase str) "nil") nil str)))

(defmacro define-file-type-hook (type-list (buffer type) &body body)
  "Define some code that `process-file-options' executes when the file
   options fail to set a major mode.  This associates each type (a
   simple-string) in $type-list with a routine of $body that binds $buffer
   to the buffer the file is in and $type to the type of the pathname."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,type) ,@body))
       (dolist (,str ',(mapcar #'string-downcase type-list))
	 (setf (cdr (or (assoc ,str *file-type-hooks*  :test #'string=)
			(car (push (cons ,str nil) *file-type-hooks*))))
	       #',fun)))))

(define-file-type-hook ("1" "2" "3" "4" "5" "6" "7" "8" "9") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Roff"))

;; FIX l is also used for lisp
(define-file-type-hook ("c" "h" "y" "l") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "C"))

(define-file-type-hook ("c++" "cpp" "cc" "hh" "hpp") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "C++"))

(define-file-type-hook ("csv") (buffer type)
  (declare (ignore type))
  (setf (buffer-minor-mode buffer "CSV") t))

(define-file-type-hook ("diff") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "VC Comparison"))

; (define-file-type-hook ("enr")
; 		       (buffer type)
;   (declare (ignore type))
;   (setf (buffer-major-mode buffer) "Enriched"))

(define-file-type-hook ("java") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Java"))

(define-file-type-hook ("links") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Links"))

(define-file-type-hook ("lisp" "slisp" "l" "lsp" "mcl"
			"nids" ; Nightshade Dynamic Service
			"el"  ; Emacs Lisp
			"scm" ; Scheme
			"jl") ; Sawfish Librep
		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp"))

(define-file-type-hook ("mk" "makefile") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Make"))

(define-file-type-hook ("pas" "pasmac" "macro" "defs" "spc" "bdy")
  		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Pascal"))

;; Perl has same the extension as Prolog.
(define-file-type-hook ("pl") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Perl"))

(define-file-type-hook ("py") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Python"))

(define-file-type-hook ("roff") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Roff"))

(define-file-type-hook ("SGML" "HTML" "HTM" "XML" "sgml"
			"html" "htm" "xml" "glade")
		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "SGML"))

(define-file-type-hook ("sh" "bash" "ksh") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Shell"))

(define-file-type-hook ("tex" "sty") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Tex"))

(define-file-type-hook ("txt" "text" "tx") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Text"))

(defmacro define-file-pathname-hook (name-list (buffer name) &body body)
  "define-file-pathname-hook ({name}*) ($buffer $name) {form}*

   Define some code to be evaluated when a file having one of the specified
   Pathnames is read by a file command.  Buffer is bound to the buffer the
   file is in, and Name is the actual pathname read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,name) ,@body))
       (dolist (,str ',name-list)
	 (setf (cdr (or (assoc (namestring ,str)
			       *file-pathname-hooks* :test #'string=)
			(car (push (cons (namestring ,str) nil)
				   *file-pathname-hooks*))))
	       #',fun)))))

(defmacro define-file-name-hook (name-list (buffer name pathname) &body body)
  "Define-File-Name-Hook ({Name}*) (Buffer Name) {Form}*
   Define some code to be evaluated when a file having one of the specified
   names is read by a file command.  Buffer is bound to the buffer the
   file is in, and Name is the actual name read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,name ,pathname) ,@body))
       (dolist (,str ',name-list)
	 (setf (cdr (or (assoc ,str *file-name-hooks* :test #'string=)
			(car (push (cons ,str nil) *file-name-hooks*))))
	       #',fun)))))

(define-file-name-hook ("Makefile" "GNUmakefile" "GNUMakefile") (buffer name pathname)
  (declare (ignore name pathname))
  (setf (buffer-major-mode buffer) "Make"))

(define-file-name-hook ("ChangeLog" "Changelog") (buffer name pathname)
  (declare (ignore name pathname))
  (setf (buffer-major-mode buffer) "ChangeLog"))

(define-file-name-hook ("CMakeLists") (buffer name pathname)
  (declare (ignore name))
  (if (equal (pathname-type pathname) "txt")
      (setf (buffer-major-mode buffer) "Shell")))

(define-file-name-hook ("Doxyfile") (buffer name pathname)
  (declare (ignore name pathname))
  (setf (buffer-major-mode buffer) "Shell"))

(defmacro define-file-content-hook (string (buffer) &body body)
  "Define-File-Content-Hook (String) (Buffer) {Form}*

   Define some code to be evaluated when any file is opened.  Buffer is
   bound to the buffer the file is in.

   If the code returns true then file option processing ends."
  (let ((fun (gensym)))
    `(flet ((,fun (,buffer) ,@body))
       (push (cons ,string #',fun) *file-content-hooks*))))

(defmacro define-loader-directive-handler (name-list (buffer name) &body body)
  "define-loader-directive-handler ({name}*) (buffer name) {form}*

   Define some code to be evaluated when a file containing a interpreter
   loading directive (e.g. #!/bin/sh) is read by a file command.  Buffer is
   bound to the buffer the file is in, and Name is the actual interpreter
   name."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,name) ,@body))
       (dolist (,str ',name-list)
	 (setf (cdr (or (assoc ,str
			       *loader-directive-handlers* :test #'string=)
			(car (push (cons ,str nil)
				   *loader-directive-handlers*))))
	       #',fun)))))

(define-loader-directive-handler ("sh" "bash" "ksh" "csh") (buffer interp)
  (declare (ignore interp))
  (setf (buffer-major-mode buffer) "Shell"))

(define-loader-directive-handler ("perl") (buffer interp)
  (declare (ignore interp))
  (setf (buffer-major-mode buffer) "Perl"))

(define-loader-directive-handler ("nightshade" "ni") (buffer interp)
  (declare (ignore interp))
  (setf (buffer-major-mode buffer) "Lisp"))

(define-loader-directive-handler ("make") (buffer interp)
  (declare (ignore interp))
  (setf (buffer-major-mode buffer) "Make"))


;;;; Content types.

(defmacro define-content-type (type (buffer value) &body body)
  "define-content-type name (buffer value) {form}*

   Define a new content type.  FIX Evaluate $body with $buffer bound to the
   buffer the file has been read into and $value to the string argument to
   the option."
  (let ((type (string-downcase type)))
    ;; FIX use [string] table?
    `(setf (cdr (or (assoc ,type *content-type-handlers*  :test #'string=)
		    (car (push (cons ,type nil) *content-type-handlers*))))
	   #'(lambda (,buffer ,value) ,@body))))

(define-content-type "text/enriched" (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Enriched"))


#[ Filename Defaulting and Merging

When the editor prompts for the name of a file, it always offers a default.
Except for a few commands that have their own defaults, filename defaults are
computed in a standard way.  If it exists, the associated file for the current
buffer is used as the default, otherwise a more complex mechanism creates a
default.

These variables control the association of files with buffers.

{evariable:Pathname Defaults}
{evariable:Last Resort Pathname Defaults Function}
{evariable:Last Resort Pathname Defaults}

When a suggestion is present in the prompt for a file, the editor merges
the given input with the default filename.  The semantics of merging,
FIX described in the Common Lisp manual, is somewhat involved, but the editor
has a few rules it uses:

  1) If the editor can find the user's input as a file on the "default:" search
     list, then it forgoes merging with the displayed default.  Basically, the
     system favors the files in your current working directory over those found by
     merging with the defaults offered in the prompt.

  2) Merging comes in two flavors, just merge with the displayed default's directory
     or just merge with the displayed default's file-namestring.  If the user
     only responds with a directory specification, without any name or type
     information, then the editor merges the default's file-namestring.  If the
     user responds with any name or type information, then the editor only merges with
     the default's directory.  Specifying relative directories in this second
     situation coordinates with the displayed defaults, not the current working
     directory.
]#

#[ Pathnames and Buffers

There is no good way to uniquely identify buffer names and pathnames.  However,
the editor has one way of mapping pathnames to buffer names that should be used
for consistency among customizations and primitives.  Independent of this,
the editor provides a means for consistently generating prompting defaults when
asking the user for pathnames.

{function:ed:pathname-to-buffer-name}
{evariable:Pathname Defaults}
{evariable:Last Resort Pathname Defaults Function}
{evariable:Last Resort Pathname Defaults}

`Pathname Defaults' holds a "sticky" filename default.  Commands that
prompt for files set this to the file specified, and the value is used as a
basis for filename defaults.  It is undesirable to offer the unmodified
value as a default, since it is usually the name of an existing file that
we don't want to overwrite.  If the current buffer's name is all
alphanumeric, then the default is computed by substituting the buffer name
for the the name portion of `Pathname Defaults'.  Otherwise, the default is
computed by calling `Last Resort Pathname Defaults Function' with the
buffer as an argument.

The default value of `Last Resort Pathname Defaults Function' merges `Last
Resort Pathname Defaults' with `Pathname Defaults'.  Unlike `Pathname
Defaults', `Last Resort Pathname Defaults' is not modified by file
commands, so setting it to a silly name ensures that real files aren't
inappropriately offered as defaults.

{function:ed:buffer-default-pathname}
]#


;;;; Support for file hacking commands.

(defevar "Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults
   when we don't have anything better."
  :value (pathname "gazonk.del"))

(defevar "Last Resort Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults when
   we don't have anything better, but unlike *Pathname Defaults*, this is
   never set to some buffer's pathname."
  :value (pathname "gazonk"))

(defevar "Last Resort Pathname Defaults Function"
  "This variable contains a function that is called when a default pathname
   is needed, the buffer has no pathname, and the buffer's name is not
   entirely composed of alphanumerics.  The default value is a function
   that simply returns *Last Resort Pathname Defaults*.  The function must
   take a buffer as an argument, and it must return some pathname."
  :value #'(lambda (buffer)
	     (declare (ignore buffer))
	     (merge-pathnames (value last-resort-pathname-defaults)
			      (value pathname-defaults))))

(defun buffer-default-pathname (buffer)
  "Return the pathname of $buffer if it is bound, else if $buffer's name is
   composed solely of alphnumeric characters, then return a pathname formed
   from $buffer's name, else if $buffer's name has other characters in it,
   then call *Last Resort Pathname Defaults Function* on buffer and return
   the result."
  (or (buffer-pathname buffer)
      (if (every #'alphanumericp (the simple-string (buffer-name buffer)))
	  (merge-pathnames (make-pathname :name (buffer-name buffer))
			   (value pathname-defaults))
	  (funcall (value last-resort-pathname-defaults-function) buffer))))


(defun pathname-to-buffer-name (pathname)
  "Return a simple-string using components from $pathname."
  (let ((pathname (pathname pathname)))
    (concatenate 'simple-string
		 (file-namestring pathname)
		 " "
		 (directory-namestring pathname))))


#[ Type Hooks and File Options

When a file is read either by `Find File' or `Visit File', the editor
attempts to guess the correct mode in which to put the buffer, based on the
file's type (the part of the filename after the last dot).  Any default
action may be overridden by specifying the mode in the file's file
options.

The user specifies file options with a special syntax on the first line of a
file.  If the first line contains the string "-*-", then the editor
interprets the text between the first such occurrence and the second, which
must be contained in one line , as a list of "option: value"
pairs separated by semicolons.  The following is a typical example:

;;; -*- Mode: Lisp, Editor; Package: Ed -*-


These options are currently defined:

  Dictionary
     The argument is the filename of a spelling dictionary associated
     with this file.  The handler for this option merges the argument with the
     name of this file.  See comrefSet Buffer Spelling Dictionary.

  Log
     The argument is the name of the change log file associated with this file
     (see page pagereflog-files).  The handler for this option merges the
     argument with the name of this file.

  Mode
     The argument is a comma-separated list of the names of modes to turn on
     in the buffer that the file is read into.

  Package
     The argument is the name of the package to be used for reading code in
     the file.  This is only meaningful for Lisp code (see page
     pagereflisp-package.)

  Editor
     The handler for this option ignores its argument and turns on
     `Editor' mode (see comrefEditor Mode).

If the option list contains no ":" then the entire string is used as
the name of the major mode for the buffer.

{command:Process File Options}
]#


;;;; File hacking commands.

(defcommand "Process File Options" ()
  "Reprocess the file options in the current buffer.  This is useful when
   the options have been changed or when a file is created."
  (process-file-options (current-buffer)))

(defcommand "Insert File" (p pathname (buffer (current-buffer)))
  "Inserts a prompted file at the point, pushing a buffer mark before
   inserting."
  "Inserts a prompted file at the point in $buffer, pushing a buffer mark
   before inserting."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file :default (buffer-default-pathname buffer)
				  :prompt "Insert File: "
				  :help "Name of file to insert")))
	 (point (buffer-point buffer))
	 ;; start and end will be deleted by undo stuff
	 (start (copy-mark point :right-inserting))
	 (end (copy-mark point :left-inserting))
	 (region (region start end)))
    (setv pathname-defaults pn)
    (push-buffer-mark (copy-mark end))
    (read-file pn end)
    (make-region-undo :delete "Insert File" region)))

(defcommand "Write Region" (p pathname)
  "Write the current region to a prompted file."
  "Write the current region to $pathname, prompting for a file if pathname
   is ()."
  (declare (ignore p))
  (let ((region (current-region))
	(pn (or pathname
		(prompt-for-file :prompt "File to Write: "
				 :help "The name of the file to write the region to. "
				 :default (buffer-default-pathname
					   (current-buffer))
				 :must-exist nil))))
    (write-file region pn)
    (message "~A written." (namestring (truename pn)))))


;;;; Visiting and reverting files.

(defcommand "Visit File" (p pathname (buffer (current-buffer)))
  "Read a prompted file into the current buffer, setting the associated
   filename.  Offer the option of saving the existing contents of the
   buffer if it is modified.  If the file is new, then empty the buffer,
   and echo \"(New buffer)\"; the file may then be created by saving the
   buffer.  Warn if some other buffer also contains the file."
  "Read a prompted file into $buffer, setting the associated filename.
   Offer the option of saving the existing contents $buffer if it is
   modified.  If the file is new, then empty $buffer, and echo \"(New
   buffer)\"; the file may then be created by saving $buffer.  Warn if some
   other buffer also contains the file."
  (declare (ignore p))
  (and (buffer-modified buffer)
       (prompt-for-y-or-n :prompt "Buffer is modified, save it? ")
       (save-file-command () buffer))
  (let ((pn (or pathname
		(prompt-for-file :prompt "Visit File: "
				 :must-exist nil
				 :help "Name of file to visit."
				 :default (buffer-default-pathname buffer)))))
    (setf (buffer-writable buffer) t)
    (read-buffer-file pn buffer)
    (let ((n (pathname-to-buffer-name (buffer-pathname buffer))))
      (or (getstring n *buffer-names*)
	  (setf (buffer-name buffer) n))
      (warn-about-visit-file-buffers buffer))))

(defun warn-about-visit-file-buffers (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (or buffer-pn (error "Buffer should have a pathname."))
    (dolist (b *buffer-list*)
      (or (eq b buffer)
	  (let ((bpn (buffer-pathname b)))
	    (when (equal bpn buffer-pn)
	      (loud-message "Buffer ~A also contains ~A."
			    (buffer-name b) (namestring buffer-pn))))))))

(defevar "Revert File Confirm"
  "If this is true, Revert File will prompt before reverting a modified
   buffer."
  :value t)

(defevar "Revert Consider Checkpoint"
  "If Save Mode when this is true Revert File will consider reverting to a
   checkpoint file if there is one."
  :value t)

(defcommand "Revert File" (p (buffer (current-buffer)) quiet)
  "Normally read in the last saved version of the file in the current
   buffer.  When in Save Mode, if Revert Consider Checkpoint is true read
   in the last checkpoint or the last saved version, whichever is more
   recent, otherwise read in the last saved version.

   With a prefix always use the last saved version.  In either case, if the
   buffer has been modified and *Revert File Confirm* is true, ask for
   confirmation beforehand.  Make an attempt to maintain the relative
   position of the point.  If the original file is reverted to, then clear
   the modified flag, otherwise leave it set."
  (let* ((buffer-pn (buffer-pathname buffer))
	 (point (buffer-point buffer))
	 (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (multiple-value-bind (revert-pn used-checkpoint)
			 (if p buffer-pn (revert-pathname buffer))
      (or revert-pn
	  (editor-error "Reverting only works for buffers associated with files."))
      (when (or (not (value revert-file-confirm))
		(not (buffer-modified buffer))
		(prompt-for-y-or-n
		 :prompt
		 "Buffer contains changes, are you sure you want to revert? "
		 :help (list
 "Reverting the file will lose any changes by reading in the last ~
 ~:[saved version~;checkpoint file~]." used-checkpoint)
		 :default t))
	(if (value revert-consider-checkpoint)
	    (read-buffer-file revert-pn buffer)
	    (elet ((auto-save-offer-revert ()))
	      (read-buffer-file revert-pn buffer)))
	(if used-checkpoint
	    (progn
	      (setf (buffer-modified buffer) t)
	      (setf (buffer-pathname buffer) buffer-pn)
	      (or quiet
		  (message "Reverted to checkpoint file ~A." (namestring revert-pn))))
	    (or quiet
		(message "Reverted to disk version." (namestring revert-pn))))
	(fi (line-offset point lines)
	    (buffer-end point))))))

;;; REVERT-PATHNAME -- Internal
;;;
;;; If in Save Mode and Revert Consider Checkpoint is true return either
;;; the checkpoint pathname or the buffer pathname, whichever is more
;;; recent.  Otherwise return the buffer-pathname if it exists. If neither
;;; file exists, return NIL.
;;;
(defun revert-pathname (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (if buffer-pn
	(let ((buffer-pn-date (file-write-date buffer-pn)))
	  (fi (value revert-consider-checkpoint)
	      (cond (buffer-pn-date (values buffer-pn nil))
		    (t (values nil nil)))
	      (let* ((checkpoint-pn (get-checkpoint-pathname buffer))
		     (checkpoint-pn-date (and checkpoint-pn
					      (file-write-date checkpoint-pn))))
		(if checkpoint-pn-date
		    (if (> checkpoint-pn-date (or buffer-pn-date 0))
			(values checkpoint-pn t)
			(values buffer-pn nil))
		    (cond (buffer-pn-date (values buffer-pn nil))
			  (t (values nil nil)))))))
	(values nil nil))))


;;;; Find file.

(defcommand "Find File" (p pathname)
  "Visit a prompted file in its own buffer.  If the file is already in some
   buffer, select that buffer, otherwise make a new buffer with the same
   name as the file and read the file into it.

   If the file is new, then leave the buffer empty, and echo \"(New
   buffer)\"; the file may then be created by saving the buffer.

   The buffer name created is in the form \"name directory\", so
   \"/sys/emacs/teco.mid\" has \"teco.mid /sys/emacs/\" as buffer name.  If
   the buffer already exists and has another file in it, then prompt for
   the buffer to use, as by `Create Buffer'.

   Takes special action if the file has been modified on disk since it was
   read into the editor, which happens, for example, when several people
   are simultaneously editing a file.  If the buffer is modified, then
   prompt for a single key-event to indicate what action to take, otherwise
   just ask for confirmation before reading in the new version.  If
   buffer is modified the single key-event can be:

      Return, Space, y
	  Prompt for a file in which to save the current buffer and then
          read in the file found to be modified on disk.

      Delete, Backspace, n
	  Forego reading the file.

      r
	  Read the file found to be modified on disk into the buffer
          containing the earlier version with modifications.  This loses
          all changes in the buffer."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file
		  :prompt "Find File: "
		  :must-exist nil
		  :help "Name of file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (find-file-buffer pn)))
    (change-to-buffer buffer)
    buffer))

(defcommand "Find File Literally" (p pathname)
  "Visit a file in its own buffer literally.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it."
  "Make a buffer containing the text of file Pathname current, creating
   a buffer if necessary returning the buffer."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file
		  :prompt "Find File Literally: "
		  :must-exist nil
		  :help "Name of file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (find-file-buffer pn t)))
    (change-to-buffer buffer)
    buffer))

(defvar *check-disk-version-consistent* t)

(defevar "Set Buffer Read Only After File"
  "If true when reading a file a read-only file then set the buffer
   read-only, otherwise leave the buffer as writable.")

(declaim (special lisp::*ignore-wildcards*))

(defun find-file-buffer (pathname &optional literally)
  "Return a buffer associated with the file $pathname, reading the file
   into a new buffer if necessary.  Return a second value, T on creation of
   a buffer else ().

   If the file has already been read, check to see if the file has been
   modified on disk since it was read, prompting with various recovery
   options.

   This is the basis of the `Find File' command."
  (let ((lisp::*ignore-wildcards* t)) ;; FIX ::
    (let* ((pathname (pathname pathname))
	   (trial-pathname (or (probe-file pathname)
			       (merge-pathnames pathname (dired-current-directory))))
	   (found (find trial-pathname (the list *buffer-list*)
			:key #'buffer-pathname :test #'equal)))
      (if found
	  ;; FIX need to revert if literally and was translated, and vice versa.
	  (if (if *check-disk-version-consistent*
		  (check-disk-version-consistent pathname found)
		  ())
	      (values found ())
	      (progn
		(read-buffer-file pathname found literally)
		(values found ())))
	  (let* ((name (pathname-to-buffer-name trial-pathname))
		 (found (getstring name *buffer-names*))
		 (use (if found
			  (prompt-for-buffer
			   :prompt "Buffer to use: "
			   :help
			   "Buffer name in use; give another buffer name, or confirm to reuse."
			   :default found :must-exist ())
			  (make-buffer name)))
		 (buffer (if (stringp use) (make-buffer use) use)))
	    (and (buffer-modified buffer)
		 (prompt-for-y-or-n :prompt
				    "Buffer is modified, save it? ")
		 (save-file-command () buffer))
	    (read-buffer-file pathname buffer literally)
	    (fi* (file-writable pathname)
	      (if (value set-buffer-read-only-after-file)
		  (setf (buffer-writable buffer) ()))
	      (message "Read-only file."))
	    (values buffer t))))))

;;; Check-Disk-Version-Consistent  --  Internal
;;;
(defun check-disk-version-consistent (pathname buffer)
  "Check that $buffer contains a valid version of the file $pathname,
   harass the user otherwise.  Return true if the buffer is OK, and () if
   the file should be read."
  (let ((ndate (file-write-date pathname))
	(odate (buffer-write-date buffer)))
    (cond ((not (and ndate odate (/= ndate odate)))
	   t)
	  ((buffer-modified buffer)
	   (beep)
	   (clear-input)
	   (command-case (:prompt (list
 "File has been changed on disk since it was read and you have made changes too!~
 ~%Read in the disk version of ~A? [Y] " (namestring pathname))
			  :help
 "The file in disk has been changed since the editor last saved it, meaning
  that someone else has probably overwritten it.  Since the version read
  into the editor has been changed as well, the two versions may have
  inconsistent changes.  If this is the case, it would be a good idea to
  save your changes in another file and compare the two versions.  Type one
  of the following commands:")
	     ((:confirm :yes)
 "Prompt for a file to write the buffer out to, then read in the disk version."
	      (write-buffer-file
	       buffer
	       (prompt-for-file
		:prompt "File to save changes in: "
		:help (list "Save buffer ~S to this file before reading ~A."
			    (buffer-name buffer) (namestring pathname))
		:must-exist nil
		:default (buffer-default-pathname buffer)))
	      nil)
	     (:no
	      "Change to the buffer without reading the new version."
	      t)
	     (#\r
	      "Read in the new version, clobbering the changes in the buffer."
	      nil)))
	   (t
	    (not (prompt-for-yes-or-no :prompt
				       (list
 "File has been changed on disk since it was read.~
 ~%Read in the disk version of ~A? "
					(namestring pathname))
				       :help
 "Type Y to read in the new version or N to just switch to the buffer."
				       :default t))))))

(defevar "Read File Hook"
  "A list of functions called when a file is read into a buffer.  Each
   function is passed two arguments -- the buffer the file was read into
   and true if the file existed.")

(defun read-buffer-file (pathname buffer &optional literally)
  "Clear the region in $buffer and use read-file to read $pathname into it,
   including the following:

     - Set the write date of $buffer date to the write date of pathname if
       the file exists; otherwise, message that this is a new file and
       clear the buffer write date.

     - Move $buffer's point to the beginning.

     - Clear the modification flag on $buffer.

     - Set the pathname of $buffer to the result of probing $pathname if
       the file exists; otherwise set the pathname of $buffer to the result
       of merging $pathname with the current directory.

     - Set `Pathname Defaults' to the result of the previous item.

     - Process the file options, as in [Type Hooks and File Options].

     - Invoke *Read File Hook*."
  (setf (buffer-writable buffer) t)
  (delete-region (buffer-region buffer))
  (let* ((lisp::*literal-pathnames* t) ; FIX ::
	 (pathname (pathname pathname))
	 (probed-pathname (probe-file pathname)))
    (cond (probed-pathname
	   (read-file probed-pathname (buffer-point buffer))
	   (setf (buffer-write-date buffer) (file-write-date probed-pathname)))
	  (t
	   (message "(New buffer)")
	   ;(clearf (buffer-write-date buffer)) ; FIX
	   (setf (buffer-write-date buffer) ())))
    (buffer-start (buffer-point buffer))
    ;(clearf (buffer-modified buffer)) ; FIX
    (setf (buffer-modified buffer) ())
    (let ((stored-pathname (or probed-pathname
			       (merge-pathnames pathname
						(dired-current-directory)))))
      (setf (buffer-pathname buffer) stored-pathname)
      (setf (value pathname-defaults) stored-pathname)
      (or literally
	  (if probed-pathname
	      (progn
		;; Convert the buffer to UTF-8 if it has a charset.
		(if (editor-bound-p 'charset :buffer buffer)
		    (let ((charset (variable-value 'charset
						   :buffer buffer)))
		      (case charset
			(:utf-8)
			(())
			(t (let ((charset (edi::find-charset charset))
				 (modified (buffer-modified buffer)))
			     (filter-region
			      (edi::charset-to-utf-8-fun charset)
			      (buffer-region buffer))
			     (or modified
				 (setf (buffer-modified buffer)
				       ())))))))
		(process-file-options buffer stored-pathname)
		(invoke-hook read-file-hook buffer probed-pathname))
	      (progn
		(process-filename-options buffer stored-pathname)
		;; FIX may need create-buffer-hook
		))))))


;;;; Generic find.

(defun urlp (url)
  "Return t if Url is an URL, else ()."
  (if (>= (length url) 7)
      (or (string= (string-downcase url) "http://" :end1 7)
	  (and (>= (length url) 8)
	       (string= (string-downcase url) "https://" :end1 8)))))

;; FIX should relate to `find'
(defcommand "Find" (p pathname)
  ;; FIX prefix for pathname at point clashes dired prefix for showing .files.
  ;; prefer dired s h
  "Visit a file or directory in its own buffer.  If the file or directory
   is already in some buffer, select that buffer, otherwise make a new
   buffer with the same name as the file and read the file into the new
   buffer.  With a prefix argument suggest any filename at point at the
   prompt."
  "Make a buffer containing the file or directory $pathname current,
   creating a buffer if necessary.  Return the buffer."
  (let* (url
	 (pn (or pathname
		 (progn
		   (if p (setq url (url-at-point)))
		   (if url
		       (prompt-for-string
			:default url
			:trim t
			:prompt (if (> p 4)
				    "Find URL externally: "
				    "Find URL: ")
			:help "URL to browse.")
		       (prompt-for-file
			:prompt "Find File Or Dir: "
			:must-exist ()
			:help "Enter the name of a file or directory to read into a buffer."
			:default (or (and p (pathname-at-point))
				     (buffer-default-pathname
				      (current-buffer)))))))))
    (if (or url (and pathname (urlp pn)))
	(if (and p (> p 4))
	    (view-url pn)
	    (www-command () pn))
	(if (or (wild-pathname-p pn) (directoryp pn))
	    (dired-command () (namestring pn))
	    (if (directory-name-p pn)
		(if (prompt-for-y-or-n
		     :prompt `("Create directory ~A? " ,pn))
		    (progn
		      (ensure-directories-exist pn)
		      (dired-command () (namestring pn)))
		    (editor-error "Find cancelled."))
		(change-to-buffer (find-file-buffer pn)))))))


;;;; File writing.

(defevar "Add End Newline on Writing File"
  "When set some file writing commands add a newline at the end of the file
   if the last line contains text.  This value may be

     :ask-user
	Ask whether to add a newline.

     t
	Automatically add a newline and print a message.

     ()

	Leave the text as it is.  Some, often older, programs lose the text
	on the last line or get an error when the last line ends in text."
  :value :ask-user)

(defevar "Keep Backup Files"
  "Whenever a file is written by `Save File' and similar commands, the old
   file is renamed by appending \".BAK\" to the name, ensuring that some
   version of the file will survive a system crash during the write.  If
   set to true, this backup file will be left in the directory, even when the write
   successfully completes.")

(defevar "Write File Hook"
  "A list of functions called when a buffer has been written.  Each
   function is passed the buffer as an argument.")

(defevar "Before Write File Hook"
  "These functions are called immediately before a buffer is written.  Each
   function must take the buffer as an argument.")

(defun write-buffer-file (buffer pathname)
  "Write $buffer to the file named by $pathname, including the following:

     - Assume $pathname is somehow related to $buffer's pathname:
       either the buffer and pathname write dates are the same or prompt
       the user for confirmation before overwriting the file.

     - Consult *Add End Newline on Writing File* (section [Files] of the
       Editor manual lists possible values), interacting with the user if
       necessary.

     - Set `Pathname Defaults', and after using write-file, clear the
       modification flag on buffer.

     - Update that pathname and write date of $buffer.

     - Rename the buffer according to the new pathname if possible.

     - Invoke *Write File Hook*."
  (let ((buffer-pn (buffer-pathname buffer)))
    (let ((date (buffer-write-date buffer))
	  (file-date (when (probe-file pathname)
		       (file-write-date pathname))))
      (when (and buffer-pn date file-date
		 (equal (make-pathname :version nil :defaults buffer-pn)
			(make-pathname :version nil :defaults pathname))
		 (/= date file-date))
	(or (prompt-for-yes-or-no :prompt (list
 "File has been changed on disk since it was read.~%Overwrite ~A anyway? "
 (namestring buffer-pn))
				      :help
				      "Type No to cancel writing the file or Yes to overwrite the disk version."
				      :default nil)
	    (editor-error "Write cancelled."))))
    (let ((val (value add-end-newline-on-writing-file)))
      (when val
	(let ((end (buffer-end-mark buffer)))
	  (or (start-line-p end)
	      (when (if (eq val :ask-user)
			(prompt-for-y-or-n
			 :prompt
			 (list "~A~%Buffer missing final newline, add one? "
			       (buffer-name buffer))
			 :default t)
			t)
		(insert-character end #\newline)
		(message "Added newline at end of buffer."))))))
    (invoke-hook before-write-file-hook buffer)
    (setv pathname-defaults pathname)
    (write-file (or (edi::buffer-charset-region buffer)
		    (buffer-deep-region buffer)
		    (buffer-region buffer))
		pathname)
    (let ((tn (truename pathname)))
      (message "~A written." (namestring tn))
      (setf (buffer-modified buffer) ())
      (or (equal tn buffer-pn)
	  (setf (buffer-pathname buffer) tn))
      (setf (buffer-write-date buffer) (file-write-date tn))
      (let ((name (pathname-to-buffer-name tn)))
	(or (getstring name *buffer-names*)
	    (setf (buffer-name buffer) name)))))
  (invoke-hook write-file-hook buffer))

(defcommand "Write File" (p pathname (buffer (current-buffer)))
  "Writes the contents of Buffer, which defaults to the current buffer to
  the file named by Pathname.  The prefix argument is ignored."
  "Prompts for a file to write the contents of the current Buffer to.
  The prefix argument is ignored."
  (declare (ignore p))
  (write-buffer-file
   buffer
   (or pathname
       (prompt-for-file :prompt "Write File: "
			:must-exist nil
			:help "Name of file to write to"
			:default (buffer-default-pathname buffer)))))

(defcommand "Save File" (p (buffer (current-buffer)))
  "Write the contents of the current buffer out to the associated file and
   reset the buffer modification flag.

   Prompt for an associated file if necessary.  Prompt for confirmation if
   the file is already up to date with the buffer.  If the file has been
   modified on disk since the last time it was read, prompt for
   confirmation before overwriting the file."
  (declare (ignore p))
  (when (or (buffer-modified buffer)
	    (prompt-for-y-or-n
	     :prompt "Buffer is unmodified, write it anyway? "
	     :default t))
    (write-buffer-file
     buffer
     (or (let ((pn (buffer-pathname buffer)))
	   (if (file-name-p pn) pn))
	 (prompt-for-file :prompt "Save File: "
			  :help "Name of file to write to"
			  :default (buffer-default-pathname buffer)
			  :must-exist nil)))))

(defevar "Save All Save Process Buffers"
  "If true Save All Files will include Process-mode buffers."
  :value t)

(defevar "Save All Files Confirm"
  "When true, the file saving commands prompt for confirmation before
   writing each modified buffer."
  :value t)

(defcommand "Save All Files" ()
  "Saves all modified buffers in their associated files.  A buffer is only
   considered for saving if it has an associated file."
  (let ((saved-count 0))
    (dolist (b *buffer-list*)
      (or (if (value save-all-save-process-buffers)
	      (buffer-minor-mode b "Process"))
	  (let ((pn (buffer-pathname b))
		(name (buffer-name b)))
	    (when (and (buffer-modified b)
		       pn
		       (pathname-name pn)
		       (or (not (value save-all-files-confirm))
			   (prompt-for-y-or-n
			    :prompt (list
				     "Write ~:[buffer ~A as file ~S~;file ~*~S~], ~
				      Y or N: "
				     (string= (pathname-to-buffer-name pn) name)
				     name (namestring pn))
			    :default t)))
	      (write-buffer-file b pn)
	      (incf saved-count)))))
    (if (zerop saved-count)
	(message "All files already saved.")
	(message "Saved ~S file~:P." saved-count))))

; FIX Save and Exit?
(defcommand "Save All Files and Exit" ()
  "Saves all modified buffers in their associated files, and then exit.  A
   buffer is only considered for saving if it has an associated file."
  (save-all-files-command)
  (exit))

(defcommand "Backup File" ()
  "Write the buffer to a file, leaving the associated filename and
   modification flag in tact.  This is useful for saving the current state
   somewhere else, for example on a reliable machine."
  (let ((file (prompt-for-file :prompt "Backup to File: "
			       :help
 "Name of a file to backup the current buffer in."
			       :default (buffer-default-pathname (current-buffer))
			       :must-exist nil)))
    (write-file (buffer-region (current-buffer)) file)
    (message "~A written." (namestring (truename file)))))


;;;; Buffer hacking commands.

(defvar *buffer-history* ()
  "A list of buffers, ordered from those most recently selected to those
   selected farthest in the past.

   On buffer creation, a function in *Make Buffer Hook* adds the buffer to
   the end of this list.  On buffer deletion, a function in *Delete Buffer
   Hook* removes the buffer from this list.

   Each buffer occurs in this list exactly once.  The Echo Area buffer
   (*echo-area-buffer*) is always left off the list.")  ; FIX treat echo area buf more normally?

(defun previous-buffer ()
  "Return the first buffer from `*buffer-history*' other than the
   current-buffer.  If the buffers are all the current buffer return ()."
  (let ((b (car *buffer-history*)))
    (or (if (eq b (current-buffer)) (cadr *buffer-history*) b)
	(find-if-not #'(lambda (x)
			 (or (eq x (current-buffer))
			     (eq x *echo-area-buffer*)))
		     (the list *buffer-list*)))))

;;; after-buffer
;;;
;;; Return the next buffer in *buffer-list*.
;;;
(defun after-buffer ()
  (find-if-not (lambda (b)
		 (or (eq b (current-buffer))
		     (eq b *echo-area-buffer*)))
	       (the list *buffer-list*)))

(defun other-buffer ()
  "Return the buffer in the next window if there is one, else calls
   `after-buffer'."
  (if (<= (length *window-list*) 2)
      (after-buffer)
      (window-buffer (next-window (current-window)))))

;;; ADD-BUFFER-HISTORY-HOOK makes sure every buffer will be visited by
;;; "Circulate Buffers" even if it has never been before.
;;;
(defun add-buffer-history-hook (buffer)
  (let ((ele (last *buffer-history*))
	(new-stuff (list buffer)))
    (if ele
	(setf (cdr ele) new-stuff)
	(setf *buffer-history* new-stuff))))
;;;
(add-hook make-buffer-hook 'add-buffer-history-hook)

;;; DELETE-BUFFER-HISTORY-HOOK makes sure we never end up in a dead buffer.
;;;
(defun delete-buffer-history-hook (buffer)
  (setq *buffer-history* (delq buffer *buffer-history*)))
;;;
(add-hook delete-buffer-hook 'delete-buffer-history-hook)


(defun change-to-buffer (buffer)
  "Switch to $buffer in the current window, maintaining *buffer-history*
   and *buffer-list*."
  (setq *buffer-history*
	(cons (current-buffer) (delq (current-buffer) *buffer-history*)))
  (or (eq (car *buffer-list*) buffer)
      (setq *buffer-list* (cons buffer (delq buffer *buffer-list*))))
  (setf (current-buffer) buffer)
  (setf (window-buffer (current-window)) buffer))

(defun delete-buffer-if-possible (buffer)
  "Delete $buffer if at all possible, using `delete-buffer'.  If $buffer
   and the echo area are the only buffers, signal an error.  Otherwise,
   find some recently current buffer, and make all of $buffer's windows
   display this recent buffer.  If $buffer is current, set the current
   buffer to be the recently current buffer."
  (let ((new-buf (flet ((frob (b)
			  (or (eq b buffer) (eq b *echo-area-buffer*))))
		   (or (find-if-not #'frob (the list *buffer-list*))
		       (find-if-not #'frob (the list *buffer-history*))))))
    (or new-buf (error "Attempt to delete only buffer ~S." buffer))
    (dolist (w (buffer-windows buffer))
      (setf (window-buffer w) new-buf))
    (if (eq buffer (current-buffer))
	(setf (current-buffer) new-buf)))
  (delete-buffer buffer))

(defun delete-buffer-safely (buffer)
  "Move $buffer off any windows, closing them if necessary, and pass $buffer
   to `delete-buffer'."
  (if (eq buffer (current-buffer))
      (let ((new (or (after-buffer)
		     (progn
		       (edi::get-ring-marker)  ; FIX
		       (after-buffer)))))
	(dolist (w (buffer-windows buffer))
	  (setf (window-buffer w) new))
	(setf (current-buffer) new))
      (dolist (w (buffer-windows buffer))
	(delete-window w)))
  (delete-buffer buffer))

(defvar *create-buffer-count* 0)

(defcommand "Create Buffer" (p buffer-name)
  "Prompt for a buffer name.  If such a buffer exists then make that buffer
   the current buffer otherwise create a new empty buffer with the
   specified name.  Either way switch to the named buffer as with `Select
   Buffer'."
  (declare (ignore p))
  (let ((name (or buffer-name
		  (prompt-for-buffer :prompt "Create Buffer: "
				     :default-string
				     (format nil "Buffer ~D"
					     (incf *create-buffer-count*))
				     :must-exist nil))))
    (if (bufferp name)
	(change-to-buffer name)
	(change-to-buffer (or (getstring name *buffer-names*)
			      (make-buffer name))))))

(defcommand "Select Buffer" ()
  "Prompt for the name of an existing buffer and make that buffer the
   current buffer.  Display the newly selected buffer in the current
   window.  Editing commands now edit the text in that buffer.  Each buffer
   has its own point, and the point is in the place it was the last time
   the buffer was selected.  When prompting for the buffer, suggest the
   buffer that was selected before the current one."
  (let ((buf (prompt-for-buffer :prompt "Select Buffer: "
				:default (previous-buffer))))
    (if (eq buf *echo-area-buffer*)
	(editor-error "Attempt to select the Echo Area buffer."))
    (change-to-buffer buf)))

(defcommand "Select Or Create Buffer" ()
  "Select a buffer, creating it if required."
  (let ((name (prompt-for-buffer :prompt "Select Or Create Buffer: "
				 :default (previous-buffer)
				 :must-exist ())))
    (if (bufferp name)
	(change-to-buffer name)
	(change-to-buffer (or (getstring name *buffer-names*)
			      (prog1 (make-buffer name)
				(message "(New buffer)")))))))

(defvar *buffer-history-ptr* ()
  "The successively previous buffer to the current buffer.")

(defcommand "Select Previous Buffer" (p)
  "Select the buffer that has been selected most recently, similar to M-x
   Select Buffer Return Return.  If given a prefix argument, then call
   `Circulate Buffers', i.e. select the successively previous buffer to the
   current one leaving the buffer history as it is."
  (if p
      (circulate-buffers-command)
      (let ((b (previous-buffer)))
	(or b (editor-error "First buffer."))
	(change-to-buffer b)
	;;
	;; If the pointer goes to (), then "Circulate Buffers" will keep
	;; doing "Select Previous Buffer".
	(setf *buffer-history-ptr* (cddr *buffer-history*))
	(setf (last-command-type) :previous-buffer))))

(defcommand "Circulate Buffers" ()
  "Move back into successively earlier buffers in the buffer history.

   If the previous command was `Circulate Buffers' or `Select Previous
   Buffer' then move to the next most recent buffer, otherwise do the same
   thing as `Select Previous Buffer' (select the most recently selected
   buffer).

   The original buffer at the start of a `Circulate Buffers' excursion is
   made the previous buffer, so `Select Previous Buffer' can be used
   afterward the excursion to immediately bring back the original buffer."
  (if (and (eq (last-command-type) :previous-buffer)
	   *buffer-history-ptr*) ; Possibly () if before first `change-to-buffer'.
      (let ((b (pop *buffer-history-ptr*)))
	(if (eq b (current-buffer))
	    (setf b (pop *buffer-history-ptr*)))
	(unless b
	  (setf *buffer-history-ptr*
		(or (cdr *buffer-history*) *buffer-history*))
	  (setf b (car *buffer-history*)))
	(setf (current-buffer) b)
	(setf (window-buffer (current-window)) b)
	(setf (last-command-type) :previous-buffer))
      (select-previous-buffer-command)))

(defcommand "Switch to Buffer" (p buffer-or-name)
  "Switch to a buffer, creating it if required.  At the prompt suggest the
   next buffer on the buffer list (as from `after-buffer')."
  (declare (ignore p))
  (change-to-buffer
   (or buffer-or-name
       (prompt-for-buffer :prompt "Switch to Buffer: "
			  :default (after-buffer)
			  :must-exist t))))

; (defcommand "Switch to Buffer" (p buffer-or-name)
;   "Switch to a buffer, creating it if required.  The buffer offered at the
;    prompt is the next buffer on the buffer list."
;   (declare (ignore p))
;   (or buffer-or-name
;       (setq buffer-or-name
; 	    (prompt-for-buffer :prompt "Switch to Buffer: "
; 			       :default (let ((b (car *buffer-list*)))
; 					  (if (eq b (current-buffer))
; 					      (cadr *buffer-list*)
; 					      b))
; 			       :must-exist t)))
;   (if (bufferp buffer-or-name)
;       (change-to-buffer buffer-or-name)
;       (multiple-value-bind
; 	  (prefix key)
; 	  (complete-string buffer-or-name (list *buffer-names*))
; 	(if (eq key :none)
; 	    (change-to-buffer (or (getstring buffer-or-name *buffer-names*)
; 				  (prog1 (make-buffer buffer-or-name)
; 				    (message "(New buffer)"))))
; 	    (message "pre ~A" prefix)))))

(defcommand "Copy Buffer" (p (buffer (current-buffer)))
  "Create and switch to a copy of the current buffer."
  "Create and switch to a copy of $buffer."
  (declare (ignore p))
  (change-to-buffer (copy-buffer buffer)))

(defcommand "Copy Buffer Next Window" (p (buffer (current-buffer)))
  "Create and switch to a copy of the current buffer in the next window."
  "Create and switch to a copy of $buffer in the next window."
  (declare (ignore p))
  (if (<= (length *window-list*) 2)
      (split-window-command)
      (setf (current-window) (next-window (current-window))))
  (change-to-buffer (copy-buffer buffer)))

(defcommand "Switch to Next Copy" ()
  "Switch to the next buffer named like the current buffer."
  (let* ((current (current-buffer))
	 (name (buffer-name current))
	 (pos (position #\  name :from-end t))
	 (int (if pos
		  (parse-integer name :start pos :junk-allowed t)))
	 (current-buffer-prefix (if int (subseq name 0 pos) name)))
    (dolist (buffer *buffer-list*)
      (or (eq buffer current)
	  (let* ((name (buffer-name buffer))
		 (pos (position #\  name :from-end t))
		 (int (if pos
			  (parse-integer name :start pos :junk-allowed t)))
		 (prefix (if int (subseq name 0 pos) name)))
	    (when (string= current-buffer-prefix prefix)
	      (change-to-buffer buffer)
	      (return-from switch-to-next-copy-command)))))
    (message "This is the only copy.")))

(defevar "Rotate Buffers Ensure Unique"
  "When true, ensure that the new buffer is unique amongst the visible
   buffers when rotating backwards or forwards, if possible."
  :value ())

(defcommand "Rotate Buffers Forward" (p)
  "Advance into the buffer list, moving the current buffer to the end of
   the list.  With a prefix rotate that many times."
  (when (> (length (the list *buffer-list*)) 1)
    (or p (setq p 1))
    (if (> p 0)
	(dotimes (i p)
	  (let ((b (current-buffer)))
	    ;; Move buffer to end of buffer list.
	    (setq *buffer-list*
		  (nconc (if (eq (car *buffer-list*) b)
			     (cdr *buffer-list*)
			     (delq b *buffer-list*))
			 (list b)))
	    ;; Change to the highest buffer on the list, skipping any
	    ;; buffer already in a window if *Rotate Buffers Ensure
	    ;; Unique*.
	    (let ((buffer-list *buffer-list*))
	      (if (value rotate-buffers-ensure-unique)
		  (while () ((buffer-windows (car buffer-list)))
		    (pop buffer-list)))
	      (change-to-buffer (car buffer-list)))))
	(when (< p 0)
	  (dotimes (i (- p))
	    (loop
	      for i downfrom (1- (length *buffer-list*)) to 0
	      for last = (nth i *buffer-list*)
	      do
		(or (eq last (current-buffer))
		    (if (value rotate-buffers-ensure-unique)
			(buffer-windows last))
		    (progn
		      (change-to-buffer last)
		      (return-from nil)))))))))

(defcommand "Rotate Buffers Backward" (p)
  "Advance out of buffer list, moving list buffer to the front.
   With a prefix argument rotate that many times."
  (rotate-buffers-forward-command (- (or p 1))))

(defcommand "Clear Buffer Modified" ()
  "Clear the modification flag of the current buffer."
  (setf (buffer-modified (current-buffer)) ())
  (message "Buffer modification flag cleared."))

(defcommand "Check Buffer Modified" ()
  "Echo whether the buffer is modified."
  (clear-echo-area)
  (message "Buffer ~S ~:[is in sync with the filesystem~;is modified~]."
	   (buffer-name (current-buffer)) (buffer-modified (current-buffer))))

(defcommand "Set Buffer Read Only" ()
  "Toggle the read-only flag for the current buffer."
  (let ((buffer (current-buffer)))
    (setf (buffer-writable buffer) (if (buffer-writable buffer) nil t))
    (message "Buffer ~S is now ~:[read-only~;writable~]."
	     (buffer-name buffer)
	     ;; FIX When the setf is here the result is always "read-only".
	     (buffer-writable buffer))))

(defcommand "Set Buffer Writable" ()
  "Ensure the current buffer is modifiable."
  (let ((buffer (current-buffer)))
    (setf (buffer-writable buffer) t)
    (message "Buffer ~S is now writable." (buffer-name buffer))))

(defevar "Kill Buffer Prompt for New"
  "If true Kill Buffer prompts for the buffer to make current after
   killing.")

(defevar "Kill Buffer Prompt to Save Process Buffers"
  "If true Kill Buffer prompts to save modified Process-mode buffers."
  :value t)

(defevar "Kill Buffer Only Prompt to Save File Buffers"
  "If true Kill Buffer only prompts to save a modified buffer if there is a
   file associated with the buffer.")

(defevar "Kill Buffer Prompt to Save"
  "If true Kill Buffer prompts to save a modified buffer."
  :value t)

(defun kill-current-buffer ()
  "Kill the current buffer.  For recovery from errors."
  (setf (buffer-modified (current-buffer)) ())
  (kill-buffer-command () (buffer-name (current-buffer))))

(defcommand "Kill Buffer" (p buffer-name)
  "Make a buffer go away.  There is no way to restore a buffer that has
   been killed, so prompt to save the buffer if it has been modified.  FIX
   This command is poorly named, since it has nothing to do with killing
   text.  When deleting the current buffer and `Kill Buffer Prompt For New'
   is set, prompt for a new buffer to select.  If a buffer other than the
   current one is deleted then delete any windows into it."
  (declare (ignore p))
  (let ((buffer (if buffer-name
		    (getstring buffer-name *buffer-names*)
		    (prompt-for-buffer :prompt "Kill Buffer: "
				       :default (current-buffer)))))
    (or buffer (editor-error "No buffer named ~S" buffer-name))
    (if (and (buffer-modified buffer)
	     (if (editor-bound-p 'kill-buffer-prompt-to-save
				 :buffer buffer)
		 (variable-value 'kill-buffer-prompt-to-save
				 :buffer buffer)
		 (value kill-buffer-prompt-to-save))
	     (if (value kill-buffer-only-prompt-to-save-file-buffers)
		 (buffer-pathname buffer)
		 t)
	     (if (buffer-minor-mode buffer "Process")
		 (value kill-buffer-prompt-to-save-process-buffers)
		 t)
	     (prompt-for-y-or-n :prompt "Save it first? "))
	(save-file-command () buffer))
    (if (eq buffer (current-buffer))
	(let ((new (if (value kill-buffer-prompt-for-new)
		       (let ((tem-new
			      (prompt-for-buffer :prompt "New Buffer: "
						 :default (after-buffer)
 :help "Buffer to change to after the current one is killed.")))
			 (when (eq tem-new buffer)
			   (editor-error "You must select a different buffer."))
			 tem-new)
		       (after-buffer))))
	  (dolist (w (buffer-windows buffer))
	    (setf (window-buffer w) new))
	  (setf (current-buffer) new))
	(dolist (w (buffer-windows buffer))
	  (delete-window w)))
    (delete-buffer buffer)))

(defcommand "Rename Buffer" ()
  "Rename the current buffer to a prompted name.  At the prompt suggest a
   name derived from the associated filename."
  (let* ((buf (current-buffer))
	 (pn (buffer-pathname buf))
	 (name (if pn (pathname-to-buffer-name pn) (buffer-name buf)))
	 (new (prompt-for-string :prompt "New Name: "
				 :help "Give a new name for the current buffer"
				 :default name)))
    (multiple-value-bind (entry foundp) (getstring new *buffer-names*)
      (cond ((or (not foundp) (eq entry buf))
	     (setf (buffer-name buf) new))
	    (t (editor-error "Name ~S already in use." new))))))

(defcommand "Rename Buffer Uniquely" ()
  "Change the current buffer's name, appending a number to the current name
   to make it unique."
  "Change the name of the current buffer uniquely."
  (let ((buf (current-buffer)))
    (setf (buffer-name buf) (unique-buffer-name (buffer-name buf)))))

(defcommand "Insert Buffer" ()
  "Insert the contents of a prompted buffer at point."
  (let ((point (current-point))
	(region (buffer-region (prompt-for-buffer
				:default (after-buffer)
				:help
				"Type the name of a buffer to insert."))))
    ;;
    ;; start and end will be deleted by undo stuff
    (let ((save (region (copy-mark point :right-inserting)
			(copy-mark point :left-inserting))))
      (push-buffer-mark (copy-mark point))
      (insert-region point region)
      (make-region-undo :delete "Insert Buffer" save))))


(defevar "Flush Trailing Whitespace"
  "If true then flush whitespace from lines."
  :value t)

(defun flush-trailing-whitespace (buffer)
  "Flush trailing whitespace from $buffer."
  (when (value flush-trailing-whitespace) ;; FIX check buffer,mode vals
    (do-buffer-lines (line buffer)
      (let* ((length (line-length line))
	     (mark (mark line length)))
	(when (reverse-find-attribute mark :whitespace #'zerop)
	  (if (eq (mark-line mark) line)
	      (if (plusp (character-attribute :space
					      (next-character mark)))
		  (delete-characters mark (- length (mark-charpos mark))))
	      (delete-region (region (mark line 0) (mark line length)))))))))

(defcommand "Flush Trailing Whitespace" ()
  "Flush trailing whitespace from the current buffer."
  (flush-trailing-whitespace (current-buffer)))


;;;; File utility commands.

(defcommand "Directory" (p)
  "Pop up a directory listing.  If an argument is supplied, then list
   hidden (dot) files too.  Prompt for a pathname which may contain
   wildcards in the name and type."
  (let* ((dpn (value pathname-defaults))
	 (pn (prompt-for-file
	      :prompt "Directory: "
	      :help "Pathname to do directory on."
	      :default (make-pathname :device (pathname-device dpn)
				      :directory (pathname-directory dpn))
	      :must-exist ())))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (in-directory (or (buffer-pathname (current-buffer))
		      (directory-namestring dpn))
      (with-pop-up-display (s)
	(print-directory pn s :all p)))))

(defcommand "Verbose Directory" (p pathname)
  "Pop up a directory listing, displaying one file per line with
   information about the file like size and protection.  If an argument is
   supplied, then list hidden (dot) files too.  Prompt for a pathname which
   may contain wildcards in the name and type."
  (let* ((dpn (value pathname-defaults))
	 (pn (or pathname
		 (prompt-for-file
		  :prompt "Verbose Directory: "
		  :help "Pathname to do directory on."
		  :default (make-pathname :device (pathname-device dpn)
					  :directory (pathname-directory dpn))
		  :must-exist ()))))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (in-directory (or (buffer-pathname (current-buffer))
		      (directory-namestring dpn))
      (with-pop-up-display (s)
	(print-directory pn s :verbose t :all p)))))

#[ Change Logs

The editor change log facility encourages the recording of changes to a
system by making it easy to do so.  The change log is kept in a separate
file so that it doesn't clutter up the source code.  The name of the log
for a file is specified by the Log file option (see page
pagereffile-options.)

{command:Log Change}
{evariable:Log Entry Template}
]#


;;;; Change log stuff.

(define-file-option "Log" (buffer value)
  (defevar "Log File Name"
    "The name of the change log file for the file in this buffer."
    :buffer buffer  :value value))

(defevar "Log Entry Template"
  "The format string used to generate the template for a change-log entry.
   Three arguments are given: the file, the date (created if available, now
   otherwise) and the file author (if available, else ()).  If there is at
   least one \"@\" in the template the last one is released and the point
   placed where it was."
  :value "~A, ~A, Edit by ~:[???~;~:*~:(~A~)~].~%  @~2%")

(defmode "Log"
  :major-p t
  :setup-function
  #'(lambda (buffer)
      (setf (buffer-minor-mode buffer "Fill") t))
  :cleanup-function
  #'(lambda (buffer)
      (setf (buffer-minor-mode buffer "Fill") nil)))

(defevar "Fill Prefix" "The fill prefix in Log mode."
  :value "  "  :mode "Log")

(define-file-type-hook ("log") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Log"))

(defun universal-time-to-string (ut)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time ut)
    (format nil "~2,'0D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    day (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
			  "Sep" "Oct" "Nov" "Dec")
		       (1- month))
	    (rem year 100)
	    hour min sec)))

(defvar *back-to-@-pattern* (new-search-pattern :character :backward #\@))

(defcommand "Log Change" ()
  "Make an entry in the change log associated with this buffer.

   First save the current buffer if it is modified.  Then find the log
   file, as specified either in the \"Log\" file option or in a file called
   \".nighshade-log\" in the buffer's directory, add the change-log entry
   template at the beginning, and do a recursive edit, saving the log file
   on exit."
  (or (editor-bound-p 'log-file-name)
      (let ((log-namer (merge-pathnames ".nightshade-log"
					(directory-namestring
					 (buffer-pathname
					  (current-buffer))))))
	(when (probe-file log-namer)
	  (with-open-file (stream log-namer
			   :if-does-not-exist nil :direction :input)
	    (let ((line (read-line stream)))
	      (if (string= line "")
		  nil
		  (defevar "Log File Name"
		    "The name of the change log file for the file in this buffer."
		    :buffer (current-buffer)  :value line))))))
      (editor-error "Log file name required."))
  (let* ((buffer (current-buffer))
	 (pathname (buffer-pathname buffer)))
    (when (or (buffer-modified buffer) (null pathname))
      (save-file-command))
    (unwind-protect
	(progn
	  (find-file-command nil (merge-pathnames
				  (value log-file-name)
				  (buffer-default-pathname buffer)))
	  (let ((point (current-point)))
	    (buffer-start point)
	    (with-output-to-mark (s point :full)
	      (format s (value log-entry-template)
		      (namestring pathname)
		      (universal-time-to-string
		       (or (file-write-date pathname)
			   (get-universal-time)))
		      (file-author pathname)))
	    (when (find-pattern point *back-to-@-pattern*)
	      (delete-characters point 1)))
	  (do-recursive-edit)
	  (when (buffer-modified (current-buffer)) (save-file-command)))
      (if (member buffer *buffer-list* :test #'eq)
	  (change-to-buffer buffer)
	  (editor-error "Old buffer has been deleted.")))))


;;;; Timestamping.
;; FIX doc somewhere
;;     (add-hook before-write-file-hook #'update-timestamp)
;;     Time-stamp: "14 Oct 2006 17:26:55"
;;         marker case folded, must have "s, anything b/w marker,"

(defevar "Timestamp Length"
  "Number of lines to search from beginning of buffer for timestamp."
  :value 20)

(defevar "Timestamp Marker"
  "Text that marks the timestamp."
  :value "time-stamp:")

(defevar "Timestamp Options"
  "List of options to pass to format-universal-time to produce timestamp."
  :value '(:style :rfc1123 :print-weekday nil :print-timezone nil))

(defun update-timestamp (buffer)
  "Update any timestamp in the first Timestamp Length lines of Buffer."
  (let ((mark (copy-mark (buffer-start-mark buffer))))
    (when (and (find-string mark (value timestamp-marker) :fold t)
	       (<= (count-lines (region (buffer-start-mark buffer)
					mark))
		   (value timestamp-length))
	       (find-character mark #\"))
      (mark-after mark)
      (let ((end (copy-mark mark)))
	(when (find-character end #\")
	  (delete-region (region mark end))
	  (with-output-to-mark (stream mark)
	    (apply 'format-universal-time
		   stream
		   (get-universal-time)
		   (value timestamp-options))))))))

(defcommand "Update Timestamp" ()
  "Update any timestamp in first Timestamp Length lines of current buffer."
  (update-timestamp (current-buffer)))


;;;; Window hacking commands.

;; FIX if 1 win split p times
(defcommand "Next Window" (p)
  "Move to the next window.  If the next window is the bottom window then
   wrap around to the top window.  With a prefix move that many windows,
   moving through the previous windows if the prefix is negative.  If there
   is only one window split the window and move to the new window."
  "Move to the next window.  If the next window is the bottom window then
   wrap around to the top window.  If P then move that many windows, moving
   through the previous windows if P is negative.  If there is only one
   window split the window and move to the new window."
  (or p (setq p 1))
  (if (zerop p)
      (current-window)
      (let* ((next (while ((count (abs p) (1- count))
			   (window (current-window)
				   (if (plusp p)
				       (next-window window)
				       (previous-window window))))
			  ((plusp count)
			   window)))
	     (buffer (window-buffer next)))
	(or (eq (current-window) *echo-area-window*)
	    (if (eq next (current-window))
		(setq next (split-window-command))))
	(setf (current-buffer) buffer  (current-window) next)
	;; Ensure that the current buffer is first on the buffer list.
	(or (eq (car *buffer-list*) buffer)
	    (while ((last)
		    (buffers *buffer-list* (cdr buffers)))
		   (buffers)
	      (when (eq (car buffers) buffer)
		;; Swap the current buffer and the first buffer.
		(setf (cdr last)
		      (cons (car *buffer-list*) (cdr buffers)))
		(setq *buffer-list* (cons buffer (cdr *buffer-list*)))
		(return))
	      (setq last buffers)))
	;; Return the new current window.
	next)))

(defcommand "Previous Window" (p)
  "Move to the previous window.  If the previous window is the bottom
   window then wrap around to the top window.  With a prefix move that many
   windows, moving through the next windows if the prefix is negative.  If
   there is only one window split the window and move to the new window."
  (next-window-command (if p (- p) -1)))

(defcommand "Split Window" ()
  "Make a new window by splitting the current window.  Make the new window
   the current window and display starting at the same place as the current
   window."
  (let ((new (make-window (window-display-start (current-window)))))
    (or new (editor-error "Failed to make a new window."))
    (redisplay-all)  ; So that window moves to include point.
    (setf (current-window) new)))

(defcommand "Create Window" ()
  "Make a new window and go to it.  Display the same buffer in the new
   window as in the current one."
  (let ((new (make-window (window-display-start (current-window))
			  :ask-user t)))
    (or new (editor-error "Could not make a new window."))
    (redisplay-all)  ; So that window moves to include point.
    (setf (current-window) new)))

(defcommand "Delete Window" ()
  "Delete the current window, going to the previous window."
  (when (= (length *window-list*) 2)
    (editor-error "Attempt to delete the only window."))
  (let ((window (current-window)))
    (previous-window-command)
    (delete-window window)))

(defcommand "Line to Top of Window" ()
  "Scroll the current window until the current line is at the top of the
   window."
  (with-mark ((mark (current-point)))
    (move-mark (window-display-start (current-window)) (line-start mark))))

(defcommand "Line to Center of Window" ()
  "Scroll the current window so that the current line is vertically
   centered in the window."
  (center-window (current-window) (current-point)))

(defcommand "Delete Next Window" ()
  "Deletes the next window on display, leaving the current window as it is."
  (if (<= (length *window-list*) 2)
      (editor-error "Cannot delete only window")
      (delete-window (next-window (current-window)))))

(defcommand "Go To Initial Window" ()
  "Delete all windows leaving one at *Default Initial Window X*, *Default
   Initial Window Y*.  The remaining window retains the contents, width and
   height of the current window."
  (let* ((current (current-window))
	 (win (make-window (window-display-start current)
			   :ask-user t
			   :x (value default-initial-window-x)
			   :y (value default-initial-window-y)
			   :width (value default-initial-window-width)
			   :height (value default-initial-window-height))))
    (setf (current-window) win)
    (dolist (w *window-list*)
      (or (eq w win)
	  (eq w *echo-area-window*)
	  (delete-window w)))))

(defcommand "Go To One Window" ()
  "Delete all windows, leaving the current one."
  (let ((current (current-window)))
    (dolist (w *window-list*)
      (or (eq w current)
	  (eq w *echo-area-window*)
	  (delete-window w)))))

(defcommand "Compact Window" ()
  "Make the current window as small as possible while still displaying the
   same buffer text."
  (compact-window (current-window)))

(defcommand "Increment Window Height" (p)
  "Increment the height of the current window by one line.  With an
   argument increment by that many lines."
  (let ((window (current-window)))
    (set-window-height window
		       (+ (window-height window)
			  (or p 1)))))

