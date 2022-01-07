;;; File and buffer manipulating commands.

(in-package "ED")

(export '(buffer-default-pathname *buffer-history* change-to-buffer
	  delete-buffer-if-possible find-file-buffer
	  pathname-to-buffer-name previous-buffer process-file-options
	  read-buffer-file write-buffer-file))


;;;; process-file-options.

(defvar *content-type-handlers* ()
  "Content type handlers.  Modify with Define-Content-Type.")

(defvar *mode-option-handlers* ()
  "File option handlers.  Modify with Define-File-Option.")

(defvar *file-type-hooks* ()
  "Hooks to run according to file type on reading a file.  Modify with
   Define-File-Type-Hook.")

(defvar *file-pathname-hooks* ()
  "Hooks to run according to file pathname on reading a file.  Modify with
   Define-File-Pathname-Hook.")

(defvar *file-name-hooks* ()
  "Hooks to run according to file name on reading a file.  Modify with
   Define-File-Name-Hook.")

(defvar *loader-directive-handlers* ()
  "Hooks to run according to file loader directive on reading a file.
   Modify with Define-Loader-Directive-Handlers.")

;; FIX mv to code:
(defun trim-subseq (string start &optional end)
  (declare (simple-string string))
  (string-trim '(#\Space #\Tab) (subseq string start end)))

;;; PROCESS-FILE-OPTIONS checks the first line of buffer for the file options
;;; indicator "-*-".  IF it finds this, then it enters a do-file-options block.
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
  "Checks for file options and invokes handlers if there are any.  If no
   \"Mode\" mode option is specified, then this tries to invoke the appropriate
   file type hook."
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
	    (setq major-mode t)
	    (funcall (cdr (assoc "mode" *mode-option-handlers*
				 :test #'string=))
		     buffer (trim-subseq string start end)))))))
    (or major-mode
	;; Look for "Content-Type: <mime-type>" directive at buffer
	;; beginning.
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
	  (when type
	    ;; Try type hooks.
	    (setq hook (assoc (string-downcase type) *file-type-hooks*
			      :test #'string=))
	    (when hook (funcall (cdr hook) buffer type)))
	  (or hook
	      ;; Try name hooks.
	      (progn
		(setq hook (assoc (pathname-name pathname)
				  *file-name-hooks* :test #'string=))
		(when hook (funcall (cdr hook) buffer type))))
	  (or hook
	      ;; Try pathname hooks.
	      (progn
		(setq hook (assoc (namestring (truename pathname))
				  *file-pathname-hooks* :test #'string=))
		(when hook (funcall (cdr hook) buffer type))))
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



;;;; File options, and type and pathname hooks.

(defmacro define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
   Defines a new file option to be user in the -*- line at the top of a file.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name *mode-option-handlers*  :test #'string=)
		    (car (push (cons ,name nil) *mode-option-handlers*))))
	   #'(lambda ,lambda-list ,@body))))

(define-file-option "Mode" (buffer str)
  (let ((seen-major-mode-p nil)
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
	       (loud-message "~S is not a defined mode -- ignored." substr)))
	(unless pos
	  (return seen-major-mode-p))
	(setf lastpos (1+ pos))))))

;; FIX Consider generic var-setting fallback.
(define-file-option "Flush-Trailing-Whitespace" (buffer str)
  (declare (ignore buffer))
  ;; FIX set in buffer
  (setv flush-trailing-whitespace
	(if (eq (string-downcase str) "nil") nil str)))

(defmacro define-file-type-hook (type-list (buffer type) &body body)
  "Define-File-Type-Hook ({Type}*) (Buffer Type) {Form}*
  Define some code to be evaluated when a file having one of the specified
  Types is read by a file command.  Buffer is bound to the buffer the
  file is in, and Type is the actual type read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,type) ,@body))
       (dolist (,str ',(mapcar #'string-downcase type-list))
	 (setf (cdr (or (assoc ,str *file-type-hooks*  :test #'string=)
			(car (push (cons ,str nil) *file-type-hooks*))))
	       #',fun)))))

(define-file-type-hook ("txt" "text" "tx") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Text"))

; (define-file-type-hook ("enr")
; 		       (buffer type)
;   (declare (ignore type))
;   (setf (buffer-major-mode buffer) "Enriched"))

(define-file-type-hook ("lisp" "slisp" "l" "lsp" "mcl"
			"el"  ; Emacs Lisp
			"scm" ; Scheme
			"jl") ; Sawfish Librep
		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp"))

(define-file-type-hook ("c" "h") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "C"))

(define-file-type-hook ("c++" "cpp" "cc" "hh") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "C++"))

;; Perl has same the extension as Prolog.
(define-file-type-hook ("pl") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Perl"))

(define-file-type-hook ("java") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Java"))

(define-file-type-hook ("py") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Python"))

(define-file-type-hook ("pas" "pasmac" "macro" "defs" "spc" "bdy")
  		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Pascal"))

(define-file-type-hook ("sh" "bash" "ksh") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Shell"))

(define-file-type-hook ("tex" "sty") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Tex"))

(define-file-type-hook ("roff") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Roff"))

(define-file-type-hook ("1" "2" "3" "4" "5" "6" "7" "8" "9") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Roff"))


(defmacro define-file-pathname-hook (name-list (buffer name) &body body)
  "Define-File-Pathname-Hook ({Name}*) (Buffer Name) {Form}*
  Define some code to be evaluated when a file having one of the specified
  Pathnames is read by a file command.  Buffer is bound to the buffer the
  file is in, and Name is the actual pathname read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,name) ,@body))
       (dolist (,str ',name-list)
	 (setf (cdr (or (assoc (namestring (truename ,str))
			       *file-pathname-hooks* :test #'string=)
			(car (push (cons (namestring (truename ,str)) nil)
				   *file-pathname-hooks*))))
	       #',fun)))))

(defmacro define-file-name-hook (name-list (buffer name) &body body)
  "Define-File-Name-Hook ({Name}*) (Buffer Name) {Form}*
   Define some code to be evaluated when a file having one of the specified
   names is read by a file command.  Buffer is bound to the buffer the
   file is in, and Name is the actual name read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,name) ,@body))
       (dolist (,str ',name-list)
	 (setf (cdr (or (assoc ,str *file-name-hooks* :test #'string=)
			(car (push (cons ,str nil) *file-name-hooks*))))
	       #',fun)))))

(define-file-name-hook ("Makefile" "GNUmakefile" "GNUMakefile") (buffer name)
  (declare (ignore name))
  (setf (buffer-major-mode buffer) "Make"))

(defmacro define-loader-directive-handler (name-list (buffer name) &body body)
  "Define-Loader-Directive-Handler ({Name}*) (Buffer Name) {Form}* Define
   some code to be evaluated when a file containing a interpreter loading
   directive (e.g. #!/bin/sh) is read by a file command.  Buffer is bound
   to the buffer the file is in, and Name is the actual interpreter name."
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



;;;; Content types.

(defmacro define-content-type (type lambda-list &body body)
  "Define-Content-Type Name (Buffer Value) {Form}*
   Defines a new content type.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((type (string-downcase type)))
    ;; FIX use [string] table?
    `(setf (cdr (or (assoc ,type *content-type-handlers*  :test #'string=)
		    (car (push (cons ,type nil) *content-type-handlers*))))
	   #'(lambda ,lambda-list ,@body))))

(define-content-type "text/enriched" (buffer type)
  (declare (ignore type))
  (msg "setf")
  (setf (buffer-major-mode buffer) "Enriched"))


;;;; Support for file hacking commands:

(defhvar "Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults
   when we don't have anything better."
  :value (pathname "gazonk.del"))

(defhvar "Last Resort Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults when
   we don't have anything better, but unlike \"Pathname Defaults\", this is
   never set to some buffer's pathname."
  :value (pathname "gazonk"))

(defhvar "Last Resort Pathname Defaults Function"
  "This variable contains a function that is called when a default pathname is
   needed, the buffer has no pathname, and the buffer's name is not entirely
   composed of alphanumerics.  The default value is a function that simply
   returns \"Last Resort Pathname Defaults\".  The function must take a buffer
   as an argument, and it must return some pathname."
  :value #'(lambda (buffer)
	     (declare (ignore buffer))
	     (merge-pathnames (value last-resort-pathname-defaults)
			      (value pathname-defaults))))

(defun buffer-default-pathname (buffer)
  "Returns \"Buffer Pathname\" if it is bound.  If it is not, and buffer's name
   is composed solely of alphnumeric characters, then return a pathname formed
   from the buffer's name.  If the buffer's name has other characters in it,
   then return the value of \"Last Resort Pathname Defaults Function\" called
   on buffer."
  (or (buffer-pathname buffer)
      (if (every #'alphanumericp (the simple-string (buffer-name buffer)))
	  (merge-pathnames (make-pathname :name (buffer-name buffer))
			   (value pathname-defaults))
	  (funcall (value last-resort-pathname-defaults-function) buffer))))


(defun pathname-to-buffer-name (pathname)
  "Returns a simple-string using components from pathname."
  (let ((pathname (pathname pathname)))
    (concatenate 'simple-string
		 (file-namestring pathname)
		 " "
		 (directory-namestring pathname))))



;;;; File hacking commands.

(defcommand "Process File Options" (p)
  "Reprocess this buffer's file options."
  "Reprocess this buffer's file options."
  (declare (ignore p))
  (process-file-options (current-buffer)))

(defcommand "Insert File" (p &optional pathname (buffer (current-buffer)))
  "Inserts a file which is prompted for into the current buffer at the point.
  The prefix argument is ignored."
  "Inserts the file named by Pathname into Buffer at the point."
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

(defcommand "Write Region" (p &optional pathname)
  "Writes the current region to a file. "
  "Writes the current region to a file. "
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

(defcommand "Visit File" (p &optional pathname (buffer (current-buffer)))
  "Replaces the contents of Buffer with the file Pathname.  The prefix
   argument is ignored.  The buffer is set to be writable, so its region
   can be deleted."
  "Replaces the contents of the current buffer with the text in the file
   which is prompted for.  The prefix argument is, of course, ignored p times."
  (declare (ignore p))
  (when (and (buffer-modified buffer)
	     (prompt-for-y-or-n :prompt "Buffer is modified, save it? "))
    (save-file-command () buffer))
  (let ((pn (or pathname
		(prompt-for-file :prompt "Visit File: "
				 :must-exist nil
				 :help "Name of file to visit."
				 :default (buffer-default-pathname buffer)))))
    (setf (buffer-writable buffer) t)
    (read-buffer-file pn buffer)
    (let ((n (pathname-to-buffer-name (buffer-pathname buffer))))
      (unless (getstring n *buffer-names*)
	(setf (buffer-name buffer) n))
      (warn-about-visit-file-buffers buffer))))

(defun warn-about-visit-file-buffers (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (dolist (b *buffer-list*)
      (unless (eq b buffer)
	(let ((bpn (buffer-pathname b)))
	  (when (equal bpn buffer-pn)
	    (loud-message "Buffer ~A also contains ~A."
			  (buffer-name b) (namestring buffer-pn))
	    (return)))))))


(defhvar "Revert File Confirm"
  "If this is true, Revert File will prompt before reverting."
  :value t)

(defhvar "Revert Consider Checkpoint"
  "If Save Mode when this is true Revert File will consider reverting to a
   checkpoint file if there is one."
  :value t)

(defcommand "Revert File" (p)
  "Normally reads in the last saved version of the file in the current
   buffer.  When in Save Mode, if Revert Consider Checkpoint is true reads
   in the last checkpoint or the last saved version, whichever is more
   recent, otherwise reads in the last saved version.  An argument will
   always force Revert File to use the last saved version.  In either case,
   if the buffer has been modified and \"Revert File Confirm\" is true,
   then Revert File will ask for confirmation beforehand.  An attempt is
   made to maintain the points relative position."
  "With an argument reverts to the last saved version of the file in the
   current buffer.  Without, reverts to the last checkpoint or last saved
   version, whichever is more recent."
  (let* ((buffer (current-buffer))
	 (buffer-pn (buffer-pathname buffer))
	 (point (current-point))
	 (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (multiple-value-bind (revert-pn used-checkpoint)
			 (if p buffer-pn (revert-pathname buffer))
      (or revert-pn
	  (editor-error "No file associated with buffer to revert to!"))
      (when (or (not (value revert-file-confirm))
		(not (buffer-modified buffer))
		(prompt-for-y-or-n
		 :prompt
		 "Buffer contains changes, are you sure you want to revert? "
		 :help (list
 "Reverting the file will undo any changes by reading in the last ~
 ~:[saved version~;checkpoint file~]." used-checkpoint)
		 :default t))
	(if (value revert-consider-checkpoint)
	    (read-buffer-file revert-pn buffer)
	    (hlet ((auto-save-offer-revert ()))
	      (read-buffer-file revert-pn buffer)))
	(if used-checkpoint
	    (progn
	      (setf (buffer-modified buffer) t)
	      (setf (buffer-pathname buffer) buffer-pn)
	      (message "Reverted to checkpoint file ~A." (namestring revert-pn)))
	    (message "Reverted to last saved version." (namestring revert-pn)))
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

(defcommand "Find File" (p &optional pathname)
  "Visit a file in its own buffer.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it."
  "Make a buffer containing the file Pathname current, creating a buffer if
   necessary.  The buffer is returned."
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

(defcommand "Find File Literally" (p &optional pathname)
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

(defun find-file-buffer (pathname &optional literally)
  "Return a buffer associated with the file Pathname, reading the file into a
   new buffer if necessary.  The second value is T if we created a buffer, NIL
   otherwise.  If the file has already been read, we check to see if the file
   has been modified on disk since it was read, giving the user various
   recovery options."
  (let* ((pathname (pathname pathname))
	 (trial-pathname (or (probe-file pathname)
			     (merge-pathnames pathname (default-directory))))
	 (found (find trial-pathname (the list *buffer-list*)
		     :key #'buffer-pathname :test #'equal)))
    (cond ((not found)
	   (let* ((name (pathname-to-buffer-name trial-pathname))
		  (found (getstring name *buffer-names*))
		  (use (if found
			   (prompt-for-buffer
			    :prompt "Buffer to use: "
			    :help
  "Buffer name in use; give another buffer name, or confirm to reuse."
			    :default found :must-exist nil)
			   (make-buffer name)))
		  (buffer (if (stringp use) (make-buffer use) use)))
	     (when (and (buffer-modified buffer)
			(prompt-for-y-or-n :prompt
					   "Buffer is modified, save it? "))
	       (save-file-command () buffer))
	     (read-buffer-file pathname buffer literally)
	     (values buffer (stringp use))))
	  ;; FIX need to revert if literally and was translated, and vice versa.
	  ((check-disk-version-consistent pathname found)
	   (values found nil))
	  (t
	   (read-buffer-file pathname found literally)
	   (values found nil)))))

;;; Check-Disk-Version-Consistent  --  Internal
;;;
;;;    Check that Buffer contains a valid version of the file Pathname,
;;; harrassing the user if not.  We return true if the buffer is O.K., and
;;; false if the file should be read.
;;;
(defun check-disk-version-consistent (pathname buffer)
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


(defhvar "Read File Hook"
  "These functions are called when a file is read into a buffer.  Each function
   must take two arguments -- the buffer the file was read into and whether the
   file existed (non-nil) or not (nil).")

(defun read-buffer-file (pathname buffer &optional literally)
  "Delete the buffer's region, and use READ-FILE to read pathname into it.
   If the file exists, set the buffer's write date to the file's; otherwise,
   MESSAGE that this is a new file and set the buffer's write date to nil.
   Move buffer's point to the beginning, set the buffer unmodified.  If the
   file exists, set the buffer's pathname to the probed pathname; else, set it
   to pathname merged with DEFAULT-DIRECTORY.  Set \"Pathname Defaults\" to the
   same thing.  Process the file options, and then invoke \"Read File Hook\"."
  (setf (buffer-writable buffer) t)
  (delete-region (buffer-region buffer))
  (let* ((pathname (pathname pathname))
	 (probed-pathname (probe-file pathname)))
    (cond (probed-pathname
	   (read-file probed-pathname (buffer-point buffer))
	   (setf (buffer-write-date buffer) (file-write-date probed-pathname)))
	  (t
	   (message "(New File)")
	   (setf (buffer-write-date buffer) nil)))
    (buffer-start (buffer-point buffer))
    (setf (buffer-modified buffer) nil)
    (let ((stored-pathname (or probed-pathname
			       (merge-pathnames pathname (default-directory)))))
      (setf (buffer-pathname buffer) stored-pathname)
      (setf (value pathname-defaults) stored-pathname)
      (or literally
	  (when probed-pathname
	    (process-file-options buffer stored-pathname)
	    ;; FIX may need create-buffer-hook
	    (invoke-hook read-file-hook buffer probed-pathname))))))



;;;; File writing.

(defhvar "Add Newline at EOF on Writing File"
  "This controls whether WRITE-BUFFER-FILE adds a newline at the end of the
   file when it ends at the end of a non-empty line.  When set, this may be
   :ask-user and WRITE-BUFFER-FILE will prompt; otherwise, just add one and
   inform the user.  When nil, never add one and don't ask."
  :value :ask-user)

(defhvar "Keep Backup Files"
  "When set, .BAK files will be saved upon file writing.  This defaults to nil."
  :value nil)

(defhvar "Write File Hook"
  "These functions are called when a buffer has been written.  Each function
   must take the buffer as an argument.")

(defhvar "Before Write File Hook"
  "These functions are called immediately before a buffer is written.  Each
   function must take the buffer as an argument.")

(defun write-buffer-file (buffer pathname)
  "Write's buffer to pathname.  This assumes pathname is somehow related to
   the buffer's pathname, and if the buffer's write date is not the same as
   pathname's, then this prompts the user for confirmation before overwriting
   the file.  This consults \"Add Newline at EOF on Writing File\" and
   interacts with the user if necessary.  This sets \"Pathname Defaults\", and
   the buffer is marked unmodified.  The buffer's pathname and write date are
   updated, and the buffer is renamed according to the new pathname if possible.
   This invokes \"Write File Hook\"."
  (let ((buffer-pn (buffer-pathname buffer)))
    (let ((date (buffer-write-date buffer))
	  (file-date (when (probe-file pathname) (file-write-date pathname))))
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
    (let ((val (value add-newline-at-eof-on-writing-file)))
      (when val
	(let ((end (buffer-end-mark buffer)))
	  (unless (start-line-p end)
	    (when (if (eq val :ask-user)
		      (prompt-for-y-or-n
		       :prompt
		       (list "~A~%File does not have a newline at EOF, add one? "
			     (buffer-name buffer))
		       :default t)
		      t)
	      (insert-character end #\newline)
	      (message "Added newline at EOF."))))))
    (invoke-hook before-write-file-hook buffer)
    (setv pathname-defaults pathname)
    (write-file (buffer-region buffer) pathname)
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

(defcommand "Write File" (p &optional pathname (buffer (current-buffer)))
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

(defcommand "Save File" (p &optional (buffer (current-buffer)))
  "Writes the contents of the current buffer to the associated file.  If there
   is no associated file, prompts for one."
  "Writes the contents of the current buffer to the associated file."
  (declare (ignore p))
  (when (or (buffer-modified buffer)
	    (prompt-for-y-or-n
	     :prompt "Buffer is unmodified, write it anyway? "
	     :default t))
    (write-buffer-file
     buffer
     (or (buffer-pathname buffer)
	 (prompt-for-file :prompt "Save File: "
			  :help "Name of file to write to"
			  :default (buffer-default-pathname buffer)
			  :must-exist nil)))))

(defhvar "Save All Save Process Buffers"
  "If true Save All Files will include Process-mode buffers."
  :value t)

(defhvar "Save All Files Confirm"
  "When non-nil, Save All Files prompts for confirmation before writing
   each modified buffer."
  :value t)

(defcommand "Save All Files" (p)
  "Saves all modified buffers in their associated files.
  If a buffer has no associated file it is ignored even if it is modified.."
  "Saves each modified buffer that has a file."
  (declare (ignore p))
  (let ((saved-count 0))
    (dolist (b *buffer-list*)
      (or (if (value save-all-save-process-buffers)
	      (buffer-minor-mode b "Process"))
	  (let ((pn (buffer-pathname b))
		(name (buffer-name b)))
	    (when
		(and (buffer-modified b)
		     pn
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

(defcommand "Save All Files and Exit" (p)
  "Save all modified buffers in their associated files and exit;
   a combination of \"Save All Files\" and \"Exit\"."
  "Do a save-all-files-command and then an exit."
  (declare (ignore p))
  (save-all-files-command ())
  (exit))

(defcommand "Backup File" (p)
  "Write the buffer to a file without changing the associated name."
  "Write the buffer to a file without changing the associated name."
  (declare (ignore p))
  (let ((file (prompt-for-file :prompt "Backup to File: "
			       :help
 "Name of a file to backup the current buffer in."
			       :default (buffer-default-pathname (current-buffer))
			       :must-exist nil)))
    (write-file (buffer-region (current-buffer)) file)
    (message "~A written." (namestring (truename file)))))


;;;; Buffer hacking commands:

(defvar *buffer-history* ()
  "A list of buffers, in order from most recently to least recently selected.")

(defun previous-buffer ()
  "Returns some previously selected buffer that is not the current buffer.
   Returns nil if no such buffer exists."
  (let ((b (car *buffer-history*)))
    (or (if (eq b (current-buffer)) (cadr *buffer-history*) b)
	(find-if-not #'(lambda (x)
			 (or (eq x (current-buffer))
			     (eq x *echo-area-buffer*)))
		     (the list *buffer-list*)))))

(defun other-buffer ()
  "Returns the buffer in the next window if there is one, else calls
   previous-buffer."
  (if (<= (length *window-list*) 2)
      (previous-buffer)
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
  "Switches to buffer in the current window maintaining *buffer-history*
   and *buffer-list*."
  (setq *buffer-history*
	(cons (current-buffer) (delq (current-buffer) *buffer-history*)))
  (or (eq (car *buffer-list*) buffer)
      (setq *buffer-list* (cons buffer (delq buffer *buffer-list*))))
  (setf (current-buffer) buffer)
  (setf (window-buffer (current-window)) buffer))

(defun delete-buffer-if-possible (buffer)
  "Deletes a buffer if at all possible.  If buffer is the only buffer, other
   than the echo area, signals an error.  Otherwise, find some recently current
   buffer, and make all of buffer's windows display this recent buffer.  If
   buffer is current, set the current buffer to be this recently current
   buffer."
  (let ((new-buf (flet ((frob (b)
			  (or (eq b buffer) (eq b *echo-area-buffer*))))
		   (or (find-if-not #'frob (the list *buffer-history*))
		       (find-if-not #'frob (the list *buffer-list*))))))
    (or new-buf
	(error "Cannot delete only buffer ~S." buffer))
    (dolist (w (buffer-windows buffer))
      (setf (window-buffer w) new-buf))
    (when (eq buffer (current-buffer))
      (setf (current-buffer) new-buf)))
  (delete-buffer buffer))


(defvar *create-buffer-count* 0)

(defcommand "Create Buffer" (p &optional buffer-name)
  "Create a new buffer.  If a buffer with the specified name already exists,
   then go to it."
  "Create or go to the buffer with the specifed name."
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

(defcommand "Select Buffer" (p)
  "Select a different buffer.
   The buffer to go to is prompted for."
  "Select a different buffer.
   The buffer to go to is prompted for."
  (declare (ignore p))
  (let ((buf (prompt-for-buffer :prompt "Select Buffer: "
				:default (previous-buffer))))
    (when (eq buf *echo-area-buffer*)
      (editor-error "Cannot select Echo Area buffer."))
    (change-to-buffer buf)))


(defvar *buffer-history-ptr* ()
  "The successively previous buffer to the current buffer.")

(defcommand "Select Previous Buffer" (p)
  "Select the buffer selected before this one.  If called repeatedly
   with an argument, select the successively previous buffer to the
   current one leaving the buffer history as it is."
  "Select the buffer selected before this one."
  (if p
      (circulate-buffers-command nil)
      (let ((b (previous-buffer)))
	(unless b (editor-error "No previous buffer."))
	(change-to-buffer b)
	;;
	;; If the pointer goes to nil, then "Circulate Buffers" will keep doing
	;; "Select Previous Buffer".
	(setf *buffer-history-ptr* (cddr *buffer-history*))
	(setf (last-command-type) :previous-buffer))))

(defcommand "Circulate Buffers" (p)
  "Advance through buffer history, selecting successively previous buffer."
  "Advance through buffer history, selecting successively previous buffer."
  (declare (ignore p))
  (if (and (eq (last-command-type) :previous-buffer)
	   *buffer-history-ptr*) ;Possibly nil if never CHANGE-TO-BUFFER.
      (let ((b (pop *buffer-history-ptr*)))
	(when (eq b (current-buffer))
	  (setf b (pop *buffer-history-ptr*)))
	(unless b
	  (setf *buffer-history-ptr*
		(or (cdr *buffer-history*) *buffer-history*))
	  (setf b (car *buffer-history*)))
	(setf (current-buffer) b)
	(setf (window-buffer (current-window)) b)
	(setf (last-command-type) :previous-buffer))
      (select-previous-buffer-command nil)))

(defcommand "Switch to Buffer" (p &optional buffer-or-name)
  "Switch to a buffer, creating it if required.  The buffer offered at the
   prompt is the next buffer on the buffer list."
  "Switch to a buffer, creating it if required.  The buffer offered at the
   prompt is the next buffer on the buffer list."
  (declare (ignore p))
  (change-to-buffer
   (or buffer-or-name
       (prompt-for-buffer :prompt "Switch to Buffer: "
			  :default (let ((b (car *buffer-list*)))
				     (if (eq b (current-buffer))
					 (cadr *buffer-list*)
					 b))
			  :must-exist t))))

; (defcommand "Switch to Buffer" (p &optional buffer-or-name)
;   "Switch to a buffer, creating it if required.  The buffer offered at the
;    prompt is the next buffer on the buffer list."
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
; 				    (message "(New Buffer)"))))
; 	    (message "pre ~A" prefix)))))

(defcommand "Copy Buffer" (p &optional (buffer (current-buffer)))
  "Create and switch to a copy of the current buffer."
  "Create and switch to a copy of Buffer."
  (declare (ignore p))
  (change-to-buffer (copy-buffer buffer)))

(defcommand "Copy Buffer Next Window" (p &optional (buffer (current-buffer)))
  "Create and switch to a copy of the current buffer in the next window."
  "Create and switch to a copy of Buffer in the next window."
  (declare (ignore p))
  (if (<= (length *window-list*) 2)
      (split-window-command nil)
      (setf (current-window) (next-window (current-window))))
  (change-to-buffer (copy-buffer buffer)))

(defcommand "Switch to Next Copy" (p)
  "Switch to the next buffer named like the current buffer."
  "Switch to the next buffer named like the current buffer."
  (declare (ignore p))
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

(defcommand "Rotate Buffers Forward" (p)
  "Advance into buffer list, moving current buffer to end of list.
   With a prefix argument rotate that many times."
  "Advance into buffer list, moving current buffer to end of list.
   With a prefix argument rotate that many times."
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
	    (let ((buffer-list *buffer-list*))
	      (loop while (buffer-windows (car buffer-list)) do
		(pop buffer-list))
	      (change-to-buffer (car buffer-list)))))
	(when (< p 0)
	  (dotimes (i (- p))
	    (loop
	      for i downfrom (1- (length *buffer-list*)) to 0
	      for last = (nth i *buffer-list*)
	      do
		(or (buffer-windows last)
		    (progn
		      (change-to-buffer last)
		      (return-from nil)))))))))

(defcommand "Rotate Buffers Backward" (p)
  "Advance out of buffer list, moving list buffer to the front.
   With a prefix argument rotate that many times."
  "Advance out of buffer list, moving list buffer to the front.
   With a prefix argument rotate that many times."
  (rotate-buffers-forward-command (- (or p 1))))

(defcommand "Buffer Not Modified" (p)
  "Make the current buffer not modified."
  "Make the current buffer not modified."
  (declare (ignore p))
  (setf (buffer-modified (current-buffer)) nil)
  (message "Buffer marked as unmodified."))

(defcommand "Check Buffer Modified" (p)
  "Say whether the buffer is modified or not."
  "Say whether the current buffer is modified or not."
  (declare (ignore p))
  (clear-echo-area)
  (message "Buffer ~S ~:[is not~;is~] modified."
	   (buffer-name (current-buffer)) (buffer-modified (current-buffer))))

(defcommand "Set Buffer Read-Only" (p)
  "Toggles the read-only flag for the current buffer."
  "Toggles the read-only flag for the current buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (setf (buffer-writable buffer) (if (buffer-writable buffer) nil t))
    (message "Buffer ~S is now ~:[read-only~;writable~]."
	     (buffer-name buffer)
	     ;; FIX When the setf is here the result is always "read-only".
	     (buffer-writable buffer))))

(defcommand "Set Buffer Writable" (p)
  "Make the current buffer modifiable."
  "Make the current buffer modifiable."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (setf (buffer-writable buffer) t)
    (message "Buffer ~S is now writable." (buffer-name buffer))))

(defhvar "Kill Buffer Prompt for New"
  "If true Kill Buffer will prompt for the new buffer."
  :value t)

(defhvar "Kill Buffer Prompt to Save Process Buffers"
  "If true Kill Buffer prompts to save modified Process-mode buffers."
  :value t)

(defhvar "Kill Buffer Only Prompt to Save File Buffers"
  "If true Kill Buffer only prompts to save a modified buffer if there is a
   file associated with the buffer."
  :value nil)

(defun kill-current-buffer ()
  "Kill the current buffer.  For recovery from an error."
  (kill-buffer-command () (buffer-name (current-buffer))))

(defcommand "Kill Buffer" (p &optional buffer-name)
  "Prompts for a buffer to delete.
  If the buffer is modified, then let the user save the file before doing so.
  When deleting the current buffer, prompts for a new buffer to select.  If
  a buffer other than the current one is deleted then any windows into it
  are deleted."
  "Delete buffer Buffer-Name, doing sensible things if the buffer is displayed
  or current."
  (declare (ignore p))
  (let ((buffer (if buffer-name
		    (getstring buffer-name *buffer-names*)
		    (prompt-for-buffer :prompt "Kill Buffer: "
				       :default (current-buffer)))))
    (or buffer (editor-error "No buffer named ~S" buffer-name))
    (if (and (buffer-modified buffer)
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
						 :default (previous-buffer)
 :help "Buffer to change to after the current one is killed.")))
			 (when (eq tem-new buffer)
			   (editor-error "You must select a different buffer."))
			 tem-new)
		       (previous-buffer))))
	  (dolist (w (buffer-windows buffer))
	    (setf (window-buffer w) new))
	  (setf (current-buffer) new))
	(dolist (w (buffer-windows buffer))
	  (delete-window w)))
    (delete-buffer buffer)))

(defcommand "Rename Buffer" (p)
  "Change the current buffer's name.
  The name, which is prompted for, defaults to the name of the associated
  file."
  "Change the name of the current buffer."
  (declare (ignore p))
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

(defcommand "Rename Buffer Uniquely" (p)
  "Change the current buffer's name, appending a number to the current
   name to make it unique."
  "Change the name of the current buffer uniquely."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (setf (buffer-name buf) (unique-buffer-name (buffer-name buf)))))

(defcommand "Insert Buffer" (p)
  "Insert the contents of a buffer.
  The name of the buffer to insert is prompted for."
  "Prompt for a buffer to insert at the point."
  (declare (ignore p))
  (let ((point (current-point))
	(region (buffer-region (prompt-for-buffer
				:default (previous-buffer)
				:help
				"Type the name of a buffer to insert."))))
    ;;
    ;; start and end will be deleted by undo stuff
    (let ((save (region (copy-mark point :right-inserting)
			(copy-mark point :left-inserting))))
      (push-buffer-mark (copy-mark point))
      (insert-region point region)
      (make-region-undo :delete "Insert Buffer" save))))


(defhvar "Flush Trailing Whitespace"
  "If true then flush whitespace from lines."
  :value t)

(defun flush-trailing-whitespace (buffer)
  "Flush trailing whitespace from Buffer."
  (when (value flush-trailing-whitespace) ;; FIX check buffer,mode vals
    (do-lines (line buffer)
      (let* ((length (line-length line))
	     (mark (mark line length)))
	(when (reverse-find-attribute mark :whitespace #'zerop)
	  (if (eq (mark-line mark) line)
	      (if (plusp (character-attribute :space
					      (next-character mark)))
		  (delete-characters mark (- length (mark-charpos mark))))
	      (delete-region (region (mark line 0) (mark line length)))))))))

(defcommand "Flush Trailing Whitespace" (p)
  "Flush trailing whitespace from the current buffer."
  "Flush trailing whitespace from the current buffer."
  (declare (ignore p))
  (flush-trailing-whitespace (current-buffer)))


;;;; File utility commands:

(defcommand "Directory" (p)
  "Do a directory into a pop-up window.  If an argument is supplied, then
   dot files are listed too (as with ls -a).  Prompts for a pathname which
   may contain wildcards in the name and type."
  "Do a directory into a pop-up window."
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

(defcommand "Verbose Directory" (p)
  "Do a directory into a pop-up window.  If an argument is supplied, then
   dot files are listed too (as with ls -a).  Prompts for a pathname which
   may contain wildcards in the name and type."
  "Do a directory into a pop-up window."
  (let* ((dpn (value pathname-defaults))
	 (pn (prompt-for-file
	      :prompt "Verbose Directory: "
	      :help "Pathname to do directory on."
	      :default (make-pathname :device (pathname-device dpn)
				      :directory (pathname-directory dpn))
	      :must-exist nil)))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (in-directory (or (buffer-pathname (current-buffer))
		      (directory-namestring dpn))
      (with-pop-up-display (s)
	(print-directory pn s :verbose t :all p)))))


;;;; Change log stuff:

(define-file-option "Log" (buffer value)
  (defhvar "Log File Name"
    "The name of the change log file for the file in this buffer."
    :buffer buffer  :value value))

(defhvar "Log Entry Template"
  "The format string used to generate the template for a change-log entry.
  Three arguments are given: the file, the date (create if available, now
  otherwise) and the file author, or NIL if not available.  The last \"@\"
  is deleted and the point placed where it was."
  :value "~A, ~A, Edit by ~:[???~;~:*~:(~A~)~].~%  @~2%")

(defmode "Log"
  :major-p t
  :setup-function
  #'(lambda (buffer)
      (setf (buffer-minor-mode buffer "Fill") t))
  :cleanup-function
  #'(lambda (buffer)
      (setf (buffer-minor-mode buffer "Fill") nil)))

(defhvar "Fill Prefix" "The fill prefix in Log mode."
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
(defcommand "Log Change" (p)
  "Make an entry in the change-log file for this buffer."
  "Save the file in the current buffer if it is modified.  Then find the
   file specified either in the \"Log\" file option or in a file called
   \".nighshade-log\" in the directory of the file in the buffer, adds the
   template for a change-log entry at the beginning, and do a recursive
   edit, saving the log file on exit."
  "Find and edit the change-log file."
  (declare (ignore p))
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
		  (defhvar "Log File Name"
		    "The name of the change log file for the file in this buffer."
		    :buffer (current-buffer)  :value line))))))
      (editor-error "Log file name required."))
  (let* ((buffer (current-buffer))
	 (pathname (buffer-pathname buffer)))
    (when (or (buffer-modified buffer) (null pathname))
      (save-file-command ()))
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
	  (when (buffer-modified (current-buffer)) (save-file-command ())))
      (if (member buffer *buffer-list* :test #'eq)
	  (change-to-buffer buffer)
	  (editor-error "Old buffer has been deleted.")))))


;;;; Timestamping.
;; FIX doc somewhere
;;     (add-hook before-write-file-hook #'update-timestamp)
;;     Time-stamp: "14 Oct 2006 17:26:55"
;;         marker case folded, must have "s, anything b/w marker,"

(defhvar "Timestamp Length"
  "Number of lines to search from beginning of buffer for timestamp."
  :value 20)

(defhvar "Timestamp Marker"
  "Text that marks the timestamp."
  :value "time-stamp:")

(defhvar "Timestamp Options"
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

(defcommand "Update Timestamp" (p)
  "Update any timestamp in first Timestamp Length lines of current buffer."
  "Update any timestamp in first Timestamp Length lines of current buffer."
  (declare (ignore p))
  (update-timestamp (current-buffer)))


;;;; Window hacking commands:

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
  (or (zerop p)
      (let* ((next (loop
		     repeat (abs p)
		     for window = (current-window)
		         then (if (plusp p)
				  (next-window window)
				  (previous-window window))
		     finally return window))
	     (buffer (window-buffer next)))
	(or (eq (current-window) *echo-area-window*)
	    (if (eq next (current-window))
		(setq next (split-window-command nil))))
	(setf (current-buffer) buffer  (current-window) next))))

(defcommand "Previous Window" (p)
  "Move to the previous window.  If the previous window is the bottom
   window then wrap around to the top window.  With a prefix move that many
   windows, moving through the next windows if the prefix is negative."
  "Move to the previous window.  If the previuos window is the bottom
   window then wrap around to the top window.  If P then move that many
   windows, moving through the next windows if P is negative."
  (next-window-command (if p (- p) -1)))

(defcommand "Split Window" (p)
  "Make a new window by splitting the current window.
   The new window is made the current window and displays starting at
   the same place as the current window."
  "Create and return a new window which displays starting at the same place
   as the current window."
  (declare (ignore p))
  (let ((new (make-window (window-display-start (current-window)))))
    (or new (editor-error "Could not make a new window."))
    (redisplay-all)  ; So that window moves to include point.
    (setf (current-window) new)))

(defcommand "New Window" (p)
  "Make a new window and go to it.
   The window will display the same buffer as the current one."
  "Create a new window which displays starting at the same place
   as the current window."
  (declare (ignore p))
  (let ((new (make-window (window-display-start (current-window))
			  :ask-user t)))
    (or new (editor-error "Could not make a new window."))
    (redisplay-all)  ; So that window moves to include point.
    (setf (current-window) new)))

(defcommand "Delete Window" (p)
  "Delete the current window, going to the previous window."
  "Delete the window we are in, going to the previous window."
  (declare (ignore p))
  (when (= (length *window-list*) 2)
    (editor-error "Cannot delete only window."))
  (let ((window (current-window)))
    (previous-window-command nil)
    (delete-window window)))

(defcommand "Line to Top of Window" (p)
  "Move current line to top of window."
  "Move current line to top of window."
  (declare (ignore p))
  (with-mark ((mark (current-point)))
    (move-mark (window-display-start (current-window)) (line-start mark))))

(defcommand "Delete Next Window" (p)
  "Deletes the next window on display."
  "Deletes then next window on display."
  (declare (ignore p))
  (if (<= (length *window-list*) 2)
      (editor-error "Cannot delete only window")
      (delete-window (next-window (current-window)))))

(defcommand "Go to One Window" (p)
  "Deletes all windows leaving one with the \"Default Initial Window X\",
   \"Default Initial Window Y\", \"Default Initial Window Width\", and
   \"Default Initial Window Height\"."
  "Deletes all windows leaving one with the \"Default Initial Window X\",
   \"Default Initial Window Y\", \"Default Initial Window Width\", and
   \"Default Initial Window Height\"."
  (declare (ignore p))
  (let ((win (make-window (window-display-start (current-window))
			  :ask-user t
			  :x (value default-initial-window-x)
			  :y (value default-initial-window-y)
			  :width (value default-initial-window-width)
			  :height (value default-initial-window-height))))
    (setf (current-window) win)
    (dolist (w *window-list*)
      (unless (or (eq w win)
		  (eq w *echo-area-window*))
	(delete-window w)))))

(defcommand "Line to Center of Window" (p)
  "Moves current line to the center of the window."
  "Moves current line to the center of the window."
  (declare (ignore p))
  (center-window (current-window) (current-point)))
