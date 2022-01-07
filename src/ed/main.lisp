;;; Initialization code and random debugging stuff.

(in-package "EDI")

(export '(*global-variable-names* *mode-names* *buffer-names*
	  *character-attribute-names* *command-names* *buffer-list*
	  *window-list* *last-key-event-typed* after-editor-initializations))


(in-package "EXTENSIONS")
(export '(save-all-buffers))
(in-package "EDI")



;;;; Setup editor herald.

(pushnew :ed *features*)
(setf (getf ext:*herald-items* :ed) `("    Editor"))


;;;; %init-editor.

(defvar *editor-initialized* nil)

(defun %init-editor ()
  "Initialize data structures internal to the editor."
  (if *editor-initialized*
      (warn "%init-editor called with *editor-initialized* true."))
  ;;
  ;; This function is defined in buffer.lisp.  It creates fundamental mode
  ;; and the buffer main.  This must be done in order to define or use
  ;; editor variables.
  (setup-initial-buffer)
  ;;
  ;; Define some of the system variables.
  (define-some-variables)
  ;;
  (setf (ext:search-list "ginfo:") '("/usr/share/info/"))
  ;;
  ;; Site initializations such as window system variables.
  (site-init)
  ;;
  ;; Set up syntax table data structures.
  (%init-syntax-table)
  ;;
  ;; Define print representations for funny characters.
  (%init-line-image)
  (setq *editor-initialized* t))


;;;; Define some globals.

;;; These globals cannot be defined in the appropriate file due to compilation
;;; or load time constraints.
;;;

;;; The following belong in other files, but those files are loaded before
;;; table.lisp which defines MAKE-STRING-TABLE.
;;;
;;; vars.lisp
(defvar *global-variable-names* (make-string-table)
  "A String Table of global variable names, the values are the symbol names.")
;;;
;;; buffer.lisp
(defvar *mode-names* (make-string-table) "A String Table of Mode names.")
(defvar *buffer-names* (make-string-table)
  "A String Table of Buffer names and their corresponding objects.")
;;;
;;; interp.lisp
(defvar *command-names* (make-string-table) "String table of command names.")
;;;
;;; syntax.lisp
(defvar *character-attribute-names* (make-string-table)
 "String Table of character attribute names and their corresponding keywords.")



;;;; DEFINE-SOME-VARIABLES.

;;; This is necessary to define "Default Status Line Fields" which belongs
;;; beside the other modeline variables.  This DEFVAR would live in
;;; Morecoms.Lisp, but it is compiled and loaded after this file.
;;;
(proclaim '(special ed::*recursive-edit-count*))
;;;
(make-modeline-field
 :name :edit-level :width 15
 :function #'(lambda (buffer window)
	       (declare (ignore buffer window))
	       (if (zerop ed::*recursive-edit-count*)
		   ""
		   (format nil "Edit Level: ~2,'0D "
			   ed::*recursive-edit-count*))))

;;; This is necessary to define "Default Status Line Fields" which belongs
;;; beside the other modeline variables.  This DEFVAR would live in
;;; Completion.Lisp, but it is compiled and loaded after this file.
;;;
(proclaim '(special ed::*completion-mode-possibility*))
;;; Hack for now until completion mode is added.
(defvar ed::*completion-mode-possibility* "")
;;;
(make-modeline-field
 :name :completion :width 40
 :function #'(lambda (buffer window)
	       (declare (ignore buffer window))
	       ed::*completion-mode-possibility*))


(defun define-some-variables ()
  (defhvar "Default Modes"
    "This variable contains the default list of modes for new buffers."
    :value '("Fundamental" "Save"))
  (defhvar "Echo Area Height"
    "Number of lines in the echo area window."
    :value 3)
  (defhvar "Make Buffer Hook"
    "This hook is called with the new buffer whenever a buffer is created.")
  (defhvar "Delete Buffer Hook"
    "This hook is called with the buffer whenever a buffer is deleted.")
  (defhvar "Enter Recursive Edit Hook"
    "This hook is called with the new buffer when a recursive edit is
     entered.")
  (defhvar "Exit Recursive Edit Hook"
    "This hook is called with the value returned when a recursive edit
     is exited.")
  (defhvar "Abort Recursive Edit Hook"
    "This hook is called with the editor-error args when a recursive
     edit is aborted.")
  (defhvar "Buffer Major Mode Hook"
    "This hook is called with the buffer and the new mode when a buffer's
     major mode is changed.")
  (defhvar "Buffer Minor Mode Hook"
    "This hook is called a minor mode is changed.  The arguments are
     the buffer, the mode affected and T or NIL depending on when the
     mode is being turned on or off.")
  (defhvar "Buffer Writable Hook"
    "This hook is called whenever someone sets whether the buffer is
     writable.  It takes the buffer and the new value for writable flag.
     It is called after the flag is set.")
  (defhvar "Buffer Name Hook"
    "This hook is called with the buffer and the new name when the name of a
     buffer is changed.")
  (defhvar "Buffer Pathname Hook"
    "This hook is called with the buffer and the new Pathname when the Pathname
     associated with the buffer is changed.")
  (defhvar "Buffer Modified Hook"
    "This hook is called whenever a buffer changes from unmodified to modified
     and vice versa.  It takes the buffer and the new value for modification
     flag.")
  (defhvar "Set Buffer Hook"
    "This hook is called with the new buffer when the current buffer is set.")
  (defhvar "After Set Buffer Hook"
    "This hook is invoked with the old buffer after the current buffer has
     been changed.")
  (defhvar "Set Window Hook"
    "This hook is called with the new window when the current window
     is set.")
  (defhvar "Make Window Hook"
    "This hook is called with a new window when one is created.")
  (defhvar "Delete Window Hook"
    "This hook is called with a window before it is deleted.")
  (defhvar "Window Buffer Hook"
    "This hook is invoked with the window and new buffer when a window's
     buffer is changed.")
  (defhvar "Delete Variable Hook"
    "This hook is called when a variable is deleted with the args to
     delete-variable.")
  (defhvar "Entry Hook"
    "this hook is called when the editor is entered.")
  (defhvar "Exit Hook"
    "This hook is called when the editor is exited.  Signalling an
     editor-error, as with (editor-error) will halt the exit.")
  (defhvar "After Change Hook"
    "This is called on a buffer whenever the buffer text is changed."
    :value nil)
  (defhvar "After Command Hook"
    "This is called after every command."
    :value nil)
  (defhvar "Redisplay Hook"
    "This is called on the current window from REDISPLAY and REDISPLAY-ALL
     after checking the window display start, window image, and
     recentering.  The smart/dumb redisplay method for the device is called
     after calling the functions in this hook."
    :value nil)
  (defhvar "Key Echo Delay"
    "Wait this many seconds before echoing keys in the command loop.  This
     feature is inhibited when nil."
    :value 1.0)
  (defhvar "Input Hook"
    "The functions in this variable are invoked each time a character enters
     the editor."
    :value nil)
  (defhvar "Abort Hook"
    "These functions are invoked when ^G is typed.  No arguments are passed."
    :value nil)
  (defhvar "Command Abort Hook"
    "These functions get called when commands are aborted, such as with
     EDITOR-ERROR."
    :value nil)
  (defhvar "Character Attribute Hook"
    "This hook is called with the attribute, character and new value
     when the value of a character attribute is changed.")
  (defhvar "Shadow Attribute Hook"
    "This hook is called when a mode character attribute is made.")
  (defhvar "Unshadow Attribute Hook"
    "This hook is called when a mode character attribute is deleted.")
  (defhvar "Default Modeline Fields"
    "The default list of modeline-fields for MAKE-WINDOW."
    :value (list (modeline-field :buffer-state)
		 (modeline-field :package)
		 (modeline-field :buffer-short-name)
		 ; or (modeline-field :buffer-name)
		 ;; :vc-status goes here.
		 (modeline-field :modes)
		 ; FIX combine these into one col,line,% for convenience
		 (modeline-field :column)
		 (modeline-field :space)
		 (modeline-field :line)
		 (modeline-field :space)
		 (modeline-field :%)
		 (modeline-field :space)
		 ;(modeline-field :position)
		 ;; FIX :buffer-directory  cp buffer-pathname-ml-field-fun in window.lisp
		 (modeline-field :buffer-pathname)))
  (defhvar "Default Status Line Fields"
    "This is the default list of modeline-fields for the echo area window's
     modeline which is used for general information."
    :value (list (modeline-field :edit-level)
		 (modeline-field :doublespace)
		 (modeline-field :time)
		 (modeline-field :space)
		 (modeline-field :date)
		 (modeline-field :space)
		 (modeline-field :mail)
		 (modeline-field :completion)))
  (defhvar "Maximum Modeline Pathname Length"
    "When set, this variable is the maximum length of the display of a pathname
     in a modeline.  When the pathname is too long, the :buffer-pathname
     modeline-field function chops off leading directory specifications until
     the pathname fits.  \"...\" indicates a truncated pathname."
    :value nil
    :hooks (list 'maximum-modeline-pathname-length-hook))
  (defhvar "Quit on Exit"
    "If true then Nightshade exits on editor exit."
    :value t)
  ;; FIX depends on rompsite; here for top-level schedule-event calls
  (defhvar "Schedule Event Hook"
    "Invoked on the event when an event is scheduled."
    :value nil))


;;;; ED.

(defvar *editor-has-been-entered* ()
  "True if and only if the editor has been entered.")
(defvar *in-the-editor* ()
  "True if we are inside the editor.  This is used to prevent ill-advised
   \"recursive\" edits.")

(defvar *after-editor-initializations-funs* nil
  "A list of functions to be called after the editor has been initialized
   (including loading the initialization file) upon entering the first
   time.")

(defmacro after-editor-initializations (&rest forms)
  "Causes forms to be executed after the editor has been initialized
   (including loading the initialization file).  Forms supplied with
   successive uses of this macro will be executed after forms supplied with
   previous uses."
  `(push #'(lambda () ,@forms)
	 *after-editor-initializations-funs*))

(defun ed (&optional x
	   &key (init t)
	        (display (cdr (assoc :display ext:*environment-list*))))
  "Invoke the editor.  If X is supplied and is a symbol, the definition of
   X is put into a buffer, and that buffer is selected.  If X is a
   pathname, the file specified by X is visited in a new buffer.  If X is
   not supplied or Nil, the editor is entered in the same state as when
   last exited.  When :init is supplied as t (the default), the file
   \"nightshade-ed.lisp\", or \".nightshade-ed.lisp\" is loaded from the
   home directory, but the Lisp command line switch -einit can be used to
   specify a different name.  Any compiled version of the source is
   preferred when choosing the file to load.  If the argument is non-nil
   and not t, then it should be a pathname that will be merged with the
   home directory."
  (when *in-the-editor* (error "The editor is already running."))
  (let ((*in-the-editor* t)
	(display (unless *editor-has-been-entered*
		   ;; Setup : search path.
		   (setf (search-list ":") "home:")
		   (setf (ext:search-list "ginfo:") '("/usr/share/info/"))
		   (maybe-load-init init)
		   ;; Device dependent initializaiton.
		   (init-raw-io display))))
    (handler-bind
	((editor-top-level-catcher #'(lambda (condition)
				       (declare (ignore condition))
				       (if (value ed::quit-on-exit)
					   (quit)
					   (return-from ed)))))
      (site-wrapper-macro
       (let ((continue t)) ; Flag for stopping exit in an exit-hook.
	 (loop while continue do
	   (catch 'editor-exit
	     (or *editor-has-been-entered*
		 (block ed-init
		   ;; Make an initial window, and set up redisplay's
		   ;; internal data structures.
		   (%init-redisplay display)
		   (setq *editor-has-been-entered* t)
		   (handler-bind
		       ((editor-top-level-catcher
			 #'(lambda (condition)
			     (declare (ignore condition))
			     (return-from ed-init))))
		     ;; Pick up user initializations to be done after
		     ;; initialization.
		     (invoke-hook
		      (reverse *after-editor-initializations-funs*)))))

	     (block ed-x
	       (handler-bind
		   ((editor-top-level-catcher
		     #'(lambda (condition)
			 (declare (ignore condition))
			 (return-from ed-x))))
		 (cond ((and x (symbolp x))
			(let* ((name (nstring-capitalize
				      (concatenate 'simple-string
						   "Edit "
						   (string x))))
			       (buffer (or (getstring name *buffer-names*)
					   (make-buffer name)))
			       (*print-case* :downcase))
			  (delete-region (buffer-region buffer))
			  (with-output-to-mark
			      (*standard-output* (buffer-point buffer))
			    (eval `(grindef ,x))	; hackish, I know...
			    (terpri)
			    (ed::change-to-buffer buffer)
			    (buffer-start (buffer-point buffer)))))
		       ((or (stringp x) (pathnamep x))
			(ed::find-file-command () x))
		       (x
			(error
			 "~S must be a symbol or pathname." x)))))

	     (invoke-hook ed::entry-hook)
	     (unwind-protect
		 (loop
		   (block ed-command-loop
		     (handler-case
;			 (handler-case
			 (handler-bind ((error #'lisp-error-error-handler))
			     (progn
			       (invoke-hook ed::abort-hook)  ; control-g
			       (%command-loop)))
; FIX how to pass :internal to handler-bind
; 			   (error (condition)
; 			     (lisp-error-error-handler
; 			      condition :internal)))
		       (editor-top-level-catcher ()
			 (return-from ed-command-loop)))))
	       (setq continue nil)
	       (handler-case
		   (invoke-hook ed::exit-hook)
		 (editor-error () (setq continue t))
		 (editor-top-level-catcher () (setq continue t))
		 (error (condition)
			(let ((device (device-hunk-device
				       (window-hunk (current-window)))))
			  (funcall (device-exit device) device))
			(invoke-debugger condition))))))))))
  (if (value ed::quit-on-exit) (quit)))

(defun maybe-load-init (init)
  (when init
    (let* ((switch (find "einit" *command-line-switches*
			 :test #'string-equal
			 :key #'cmd-switch-name))
	   (spec-name
	    (if (not (eq init t))
		init
		(and switch
		     (or (cmd-switch-value switch)
			 (car (cmd-switch-words switch)))))))
      (if spec-name
	  (load (merge-pathnames spec-name (user-homedir-pathname))
		:if-does-not-exist nil)
	  (or (load "home:nightshade-ed" :if-does-not-exist nil)
	      (load "home:.nightshade-ed" :if-does-not-exist nil))))))


;;;; SAVE-ALL-BUFFERS.

;;; SAVE-ALL-BUFFERS -- Public.
;;;
(defun save-all-buffers (&optional (list-unmodified-buffers nil))
  "This prompts users with each modified buffer as to whether they want to
   write it out.  If the buffer has no associated file, this will also prompt
   for a file name.  Supplying the optional argument non-nil causes this
   to prompt for every buffer."
  (dolist (buffer *buffer-list*)
    (when (or list-unmodified-buffers (buffer-modified buffer))
      (maybe-save-buffer buffer))))

(defun maybe-save-buffer (buffer)
  (let* ((modified (buffer-modified buffer))
	 (pathname (buffer-pathname buffer))
	 (name (buffer-name buffer))
	 (string (if pathname (namestring pathname))))
    (format t "Buffer ~S is ~:[UNmodified~;modified~], Save it? "
	    name modified)
    (force-output)
    (when (y-or-n-p)
      (let ((name (read-line-default "File to write" string)))
	(format t "Writing file ~A..." name)
	(force-output)
	(write-file (buffer-region buffer) name)
	(write-line "written.")))))

(defun read-line-default (prompt default)
  (format t "~A:~@[ [~A]~] " prompt default)
  (force-output)
  (do ((result (read-line) (read-line)))
      (())
    (declare (simple-string result))
    (when (plusp (length result)) (return result))
    (when default (return default))
    (format t "~A:~@[ [~A]~] " prompt default)
    (force-output)))

(or *editor-initialized* (%init-editor))
