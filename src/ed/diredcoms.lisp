;;; Directory editing support.  This file contains site dependent calls.

(in-package "ED")

#[ Dired Mode

The directory editing extension, "Dired", provides file management
facilities.

These facilities include opening files, descending into directories, and
processing files and/or directories (copying, renaming, touching, etc).
Dired also provides a simple wildcard feature and an interface to a few
version control systems.

{command:Dired Quit}

[ Editing Directories              ]  Entering Dired.
[ Modifying the Display            ]  Updating.  Hidden and backups files.
[ Copying Files                    ]
[ Renaming Files                   ]
[ Other File Operations            ]  Touch, link, compare, compress, shell.
[ Marking Files for Deletion       ]     D...
[ Marking Files for Processing     ]     *...
[ Manipulating Marks of Both Types ]     *...   or  D...
[ Version Control Interface        ]  SVN, CVS, RCS.
]#

(declaim (special *mode-highlighters*))

(defun setup-dired-mode (buffer)
  (highlight-visible-dired-buffer buffer)
  (pushnew '("Dired" t highlight-visible-dired-buffer) *mode-highlighters*))

(defmode "Dired" :major-p t
  :setup-function 'setup-dired-mode
  :documentation
  "Dired permits convenient directory browsing and file operations including
   viewing, deleting, copying, renaming, and wildcard specifications.")

(defstruct (dired-information (:print-function print-dired-information)
			      (:conc-name dired-info-))
  pathname	   ; Pathname of directory.
  pattern	   ; FILE-NAMESTRING with wildcard possibly.
  dot-files-p      ; Whether to include Unix dot files.
  backup-files-p   ; Whether to include Unix backup files.
  coldefs	   ; Description of columns to display, () for the usual format.
  recurse          ; Whether to recurse into subdirectories.
  write-date	   ; Write date of directory.
  files		   ; Simple-vector of dired-file structures.
  file-list	   ; List of pathnames for files, excluding directories.
  generator        ; Function to generate files.  Called on a dir-info.
  propagating-generator-p ; Whether generator propagates to subdirectories.
  col-posns        ; Positions of columns, for highlighting.
  extra-data       ; Optn. per-file data: '(file*) or '((file line extra*)*).
  extra-data-2)    ; Optn. extra data (stores toggled version control info).

(defun print-dired-information (obj str n)
  (declare (ignore n))
  (format str "#<Dired Info ~S>" (namestring (dired-info-pathname obj))))

(defstruct (dired-file (:print-function print-dired-file)
		       (:constructor make-dired-file (pathname)))
  pathname          ; A string.
  (deleted-p ())
  (marked-p ())
  (write-date ()))

(defun print-dired-file (obj str n)
  (declare (ignore n))
  (format str "#<Dired-file ~A>" (namestring (dired-file-pathname obj))))


#[ Editing Directories

{command:Dired}
{command:Dired with Pattern}
{command:Dired from Buffer Pathname}
{command:Dired Help}
{command:Dired View File}
{command:Dired Edit File}
{command:Dired Up Directory}
{command:Dired Update Buffer}
{command:Dired Next File}
{command:Dired Previous File}
]#


;;;; "Dired" command.

(defevar "Create Empty Dired Buffers"
  "If true then Dired will enter empty directories."
  :value t)

;;; *pathnames-to-dired-buffers* is an a-list mapping directory namestrings to
;;; buffers that display their contents.
;;;
(defvar *pathnames-to-dired-buffers* ())

(make-modeline-field
 :name :dired-cmds :width 20 :replace t
 :function
 #'(lambda (buffer window)
     (declare (ignore buffer window))
     "  Type ? for help.  "))

(defun dired-guts (patternp dot-files-p directory
		   &optional backup-files-p coldefs recurse-p extra-data
		             buffer-name
			     generator propagating-generator-p)
  (let* ((dpn (directory-namestring (or (buffer-pathname (current-buffer))
					(value pathname-defaults))))
	 (directory (or directory
			(prompt-for-file
			 :prompt (if recurse-p
				     "Edit Directory Recursively: "
				     "Edit Directory: ")
			 :help "Pathname to edit."
			 :default (make-pathname
				   :device (pathname-device dpn)
				   :directory (pathname-directory dpn))
			 :must-exist t)))
	 (dir (directory-namestring (directorify directory)))
	 (pattern (if patternp
		      (prompt-for-string
		       :prompt "Filename pattern: "
		       :help "Type a filename with a single asterisk."
		       :trim t)
		      (file-namestring (directorify directory))))
	 (full-name (or buffer-name
			(namestring (if pattern
					(merge-pathnames (truename dir)
							 pattern)
					(truename dir)))))
	 (short-name (namestring (if pattern
				     (merge-pathnames dir pattern)
				     dir)))
	 (name (or buffer-name
		   (concatenate 'simple-string "Dired " short-name)))
	 (buffer (cdr (assoc full-name *pathnames-to-dired-buffers*
			     :test #'string=))))
    (declare (simple-string short-name))
    ;; FIX pathname-defaults is a global var?
    (setf (value pathname-defaults) (merge-pathnames dir dpn))
    (change-to-buffer
     (cond (buffer
	    buffer)
	   (t
	    (let ((buffer (make-buffer
			   name :modes '("Dired")
			   :modeline-fields
			   (append (value default-modeline-fields)
				   (list (modeline-field :dired-cmds)))
			   :delete-hook (list 'dired-buffer-delete-hook)))
		  (ok))
	      (unwind-protect
		  (if (initialize-dired-buffer (truename dir) pattern
					       dot-files-p backup-files-p
					       coldefs recurse-p extra-data
					       buffer generator
					       propagating-generator-p)
		      (setq ok t)
		      (progn
			(delete-buffer-if-possible buffer)
			(editor-error "No entries for ~A." short-name)))
		(or ok (delete-buffer-safely buffer)))
	      (push (cons full-name buffer)
		    *pathnames-to-dired-buffers*)
	      buffer))))))

(defcommand "Dired" (p directory propagate recurse)
  "Find a directory into a buffer for editing the directory.  If a dired
   for that directory already exists, go to that buffer.  With an argument,
   include hidden (dot) files.

   If $propagate is true then propagate settings from the current buffer
   (if it is a Dired buffer)."
  (if propagate
      (let* ((info (if (editor-bound-p 'dired-information)
		       (value dired-information)))
	     (generator (if info (dired-info-generator info)))
	     (propagating-generator-p
	      (if generator (dired-info-propagating-generator-p info)))
	     (extra-data))
	(if generator
	    (if propagating-generator-p
		(let ((tem-info
		       (make-dired-information
			:dot-files-p (dired-info-dot-files-p info)
			:pathname directory
			:backup-files-p (dired-info-backup-files-p info)
			:coldefs (dired-info-coldefs info)
			:recurse (or recurse (dired-info-recurse info))
			:extra-data (dired-info-extra-data info)
			:generator (dired-info-generator info))))
		  (setq extra-data (funcall generator tem-info)))
		(progn
		  (setq extra-data t)
		  (setq generator ())))
	    (if info (setq extra-data (eq (dired-info-extra-data info) t))))
	(dired-guts ()
		    ;; Propagate dot-files property to subdirectory edits.
		    (or (and info (dired-info-dot-files-p info)) p)
		    directory
		    ;; Propagate backup-files property to subdirectory
		    ;; edits.
		    (and info (dired-info-backup-files-p info))
		    ;; Propagate column definitions to subdirectory edits.
		    (and info (dired-info-coldefs info))
		    ;; Propagate recurse property to subdirectory edits.
		    (or recurse (and info (dired-info-recurse info)))
		    ;; Propagate either the t'ness of the extra data slot
		    ;; or new extra data if there's a propagating
		    ;; generator.  Determines if the subdir edit will use
		    ;; print-directory or print-files.
		    extra-data
		    () ; Buffer name.
		    generator
		    propagating-generator-p))
      (dired-guts () p directory)))

(defcommand "Dired with Pattern" (p)
  "Prompt for a directory and a pattern that may contain at most one
   wildcard (an asterisk) and find or create a buffer for editing the files
   in the directory that matches the pattern, as a list.

   When the prefix argument is supplied, include hidden (dot) files.  If a
   dired buffer already exists for this directory, then switch to the
   buffer and makes sure it displays hidden (dot) files if appropriate."
  (dired-guts t p () t #| FIX should be according to evar |#
	      () () t))

(defcommand "Dired Recursively" (p directory propagate)
  "Edit a recursive directory listing."
  (dired-command p directory propagate t))

;;; INITIALIZE-DIRED-BUFFER gets a dired in the buffer and defines some
;;; variables to make it usable as a dired buffer.  If there are files
;;; satisfying directory or CREATE-EMPTY-DIRED-BUFFERS is true, then this
;;; returns t, otherwise ().
;;;
(defun initialize-dired-buffer (directory pattern
			        dot-files-p backup-files-p coldefs recurse
				extra-data buffer generator
				propagating-generator-p)
  (let ((dired-info (make-dired-information :pathname directory
					    :pattern pattern
					    :dot-files-p dot-files-p
					    :backup-files-p backup-files-p
					    :coldefs coldefs
					    :recurse recurse
					    :extra-data extra-data
					    :write-date (file-write-date
							 directory)
					    :generator generator
					    :propagating-generator-p
					    propagating-generator-p)))
    (or (eq extra-data t)
	generator
	(setq extra-data t))
    (defevar "Dired Information"
      "Contains the information necessary to manipulate Dired buffers."
      :buffer buffer
      :value dired-info)
    (multiple-value-bind (pathnames dired-files)
			 (dired-in-buffer directory pattern
					  dot-files-p backup-files-p
					  coldefs recurse extra-data
					  buffer)
      (or (> (length dired-files) 0)
	  (multiple-value-bind (res dev ino mode)
			       (file-stats directory)
	    (declare (ignore dev ino))
	    (if res
		(logand mode unix:readall)
		(progn
		  (message "Failed to read mode of ~A." directory)
		  t)))
	  (loud-message "Read access to ~A withheld." directory))
      (setf (buffer-pathname buffer) directory)
      (when (or (plusp (length dired-files))
		(value create-empty-dired-buffers))
	(setf (dired-info-files dired-info) dired-files)
	(setf (dired-info-file-list dired-info) pathnames)
	(setf (dired-info-extra-data dired-info) extra-data)
	t))))

(defun call-print-directory (directory mark dot-files-p backup-files-p
			     coldefs recurse extra-data)
  "Call `print-directory', report errors, clean up the dired buffer."
  (handler-case
      (with-output-to-mark (stream mark :full)
	(if (eq extra-data t)
	    (multiple-value-bind (unique-part wild-part)
				 (common-prefix directory)
	      (in-directory unique-part
		(print-directory (merge-pathnames wild-part #p"*.*.*")
				 stream
				 :all dot-files-p
				 :verbose t
				 :return-list t
				 :backups backup-files-p
				 :coldefs coldefs
				 :recurse recurse)))
	    (print-files directory extra-data stream
			 :verbose t :return-list t :coldefs coldefs)))
    (error (condx)
	   (delete-buffer-if-possible (line-buffer (mark-line mark)))
	   (editor-error "~A" condx))))

(defun dired-buffer-delete-hook (buffer)
  "Called on dired buffers upon deletion.  Remove the buffer from the
   pathnames mapping, and delete any buffer local variables referring to
   it."
  (setf *pathnames-to-dired-buffers*
	(delete buffer *pathnames-to-dired-buffers* :test #'eq :key #'cdr)))


#[ Marking files for Deletion

== Marking ==

{command:Dired Delete File and Down Line}
{command:Dired Delete File with Pattern}
{command:Dired Delete File}

== Clearing the Marks ==

{command:Dired Undelete File and Down Line}
{command:Dired Undelete File with Pattern}
{command:Dired Undelete File}

== Expunging ==

{evariable:Dired File Expunge Confirm}
{evariable:Dired Directory Expunge Confirm}
{command:Dired Expunge Files}
]#


;;;; Dired deletion and undeletion.

(defcommand "Dired Delete File" (p)
  "Mark the current file for deletion.  With an argument, prompt for a
   pattern that may contain at most one wildcard, an asterisk, and flag all
   names matching the pattern for deletion."
  (dired-frob-deletion p t))

(defcommand "Dired Undelete File" (p)
  "Remove any delete mark on the current file.  With an argument, prompt
   for a pattern that may contain at most one wildcard, an asterisk, and
   clear the delete mark on all names matching the pattern."
  (dired-frob-deletion p ()))

(defcommand "Dired Delete File and Down Line" (p)
  "Mark the current file for deletion and move down a line.  With an
   argument, prompt for a pattern that may contain at most one wildcard, an
   asterisk, and flag all names matching the pattern for deletion."
  (dired-frob-deletion p t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File and Down Line" ()
  "Clear the delete mark on the current file and move down a line."
  (dired-frob-deletion () ())
  (dired-down-line (current-point)))

(defcommand "Dired Delete File with Pattern" ()
  "Prompt for a name pattern that may contain at most one wildcard (an
   asterisk) and mark the names matching the pattern for deletion."
  (dired-frob-deletion t t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File with Pattern" ()
  "Prompt for a name pattern that may contain at most one wildcard (an
   asterisk) and clear any delete marks on the names matching the pattern."
  (dired-frob-deletion t ())
  (dired-down-line (current-point)))

;;; DIRED-FROB-DELETION takes arguments indicating whether to prompt for a
;;; pattern and whether to mark the file deleted or undeleted.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER, and if not in a dired buffer, signals
;;; an error.
;;;
(defun dired-frob-deletion (patternp deletep)
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  (with-mark ((mark (current-point) :left-inserting))
    (with-literal-pathnames
      (let* ((dir-info (value dired-information))
	     (files (dired-info-files dir-info))
	     (del-files
	      (if patternp
		  (dired:pathnames-from-pattern
		   (prompt-for-string
		    :prompt "Filename pattern: "
		    :help "Type a filename with a single asterisk."
		    :trim t)
		   (dired-info-file-list dir-info))
		  (list (dired-file-pathname
			 (array-element-from-mark mark files)))))
	     (directory (dired-info-pathname dir-info))
	     (note-char (if deletep #\D #\space)))
	(with-writable-buffer ((current-buffer))
	  (dolist (f del-files)
	    (let* ((pos (position f files :test #'equal
				  :key #'dired-file-pathname))
		   (dired-file (svref files pos)))
	      (buffer-start mark)
	      (line-offset mark pos 0)
	      (setf (dired-file-deleted-p dired-file) deletep)
	      (if deletep
		  (setf (dired-file-write-date dired-file)
			(file-write-date
			 (merge-pathnames (dired-file-pathname
					   dired-file)
					  directory)
			 :check-for-links t))
		  (setf (dired-file-write-date dired-file) ()))
	      (setf (next-character mark) note-char))))))))

(defun dired-down-line (point)
  (line-offset point 1)
  (when (blank-line-p (mark-line point))
    (line-offset point -1)))


#[ Marking Files for Processing

{command:Dired Mark File}
{command:Dired Clear File Mark}
{command:Dired Toggle Marks}
{command:Dired Mark File and Down Line}
{command:Dired Clear File Mark and Down Line}
{command:Dired Mark File with Pattern}
]#


;;;; Generic Dired marks.

(defcommand "Dired Mark File" (p)
  "Marks the current file in a Dired buffer.
   With an argument, prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be marked."
  "Marks the current file in a Dired buffer."
  (dired-frob-mark p t))

(defcommand "Dired Clear File Mark" (p)
  "Clears any generic mark from the current file in a Dired buffer.
   With an argument, prompts for a pattern that may contain at most one
   wildcard, an asterisk, and any marks on the names matching the pattern
   will be cleared."
  "Clears any generic mark from the current file in a Dired buffer."
  (dired-frob-mark p ()))

(defcommand "Dired Mark File and Down Line" ()
  "In a Dired buffer marks the current file and moves down a line.
   With an argument, prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be marked."
  "In a Dired buffer marks the current file and moves down a line."
  (dired-frob-mark () t)
  (dired-down-line (current-point)))

(defcommand "Dired Clear File Mark and Down Line" ()
  "Clears any generic mark from the current file, and moves down a line."
  (dired-frob-mark () ())
  (dired-down-line (current-point)))

(defcommand "Dired Mark File with Pattern" ()
  "Prompts for a pattern and marks matching files."
  "Prompts for a pattern and marks matching files for deletion."
  (dired-frob-mark t t)
  (dired-down-line (current-point)))

(defcommand "Dired Clear File Mark with Pattern" ()
  "Prompts for a pattern and clears any generic marks from matching files."
  (dired-frob-mark t ())
  (dired-down-line (current-point)))

(defcommand "Dired Toggle Marks" ()
  "Toggle any generic marks in a Dired buffer.  Clear marks from marked
   files, mark the rest."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  (with-writable-buffer ((current-buffer))
    (with-mark ((mark (buffer-start-mark (current-buffer)) :left-inserting))
      (mark-after mark)
      (do* ((files (dired-info-files (value dired-information)))
	    (pos 0 (incf pos))
	    (end-mark (buffer-end-mark (current-buffer))))
	   ((mark= mark end-mark))
	(let ((file (svref files pos)))
	  (if (dired-file-marked-p file)
	      (setf (dired-file-marked-p file) ()
		    (next-character mark) #\space)
	      (setf (dired-file-marked-p file) t
		    (next-character mark) #\*)))
	(line-offset mark 1 1)))))

;;; DIRED-FROB-MARK takes arguments indicating whether to prompt for a
;;; pattern and whether to add or clear the file mark.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER.  The buffer must be a Dired buffer,
;;; else an error is signalled.
;;;
(defun dired-frob-mark (patternp markp)
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  (with-mark ((mark (current-point) :left-inserting))
    (let* ((dir-info (value dired-information))
	   (files (dired-info-files dir-info))
	   (mark-files
	    (if patternp
		(in-directory (dired-info-pathname dir-info)
		  (dired:pathnames-from-pattern
		   (prompt-for-string
		    :prompt "Filename pattern: "
		    :help "Type a filename with a single asterisk."
		    :trim t)
		   (dired-info-file-list dir-info)))
		(list (dired-file-pathname
		       (array-element-from-mark mark files)))))
	   (note-char (if markp #\* #\space)))
      (with-writable-buffer ((current-buffer))
	(dolist (f mark-files)
	  (let* ((pos (position f files :test #'equal
				:key #'dired-file-pathname))
		 (dired-file (svref files pos)))
	    (buffer-start mark)
	    (line-offset mark pos 0)
	    (setf (dired-file-marked-p dired-file) markp)
	    (mark-after mark)
	    (setf (next-character mark) note-char)
	    (mark-before mark)))))))


#[ Manipulating Marks of Both Types

{command:Dired Swap Marks}
{command:Dired Clear All Marks}
{command:Dired Clear File Marks with Pattern}
{command:Dired Clear File Marks and Down Line}
]#


;;;; Both types of Dired marks.

(defcommand "Dired Clear File Marks" (p)
  "Clears any marks from the current file in a Dired buffer.
   With an argument, prompts for a pattern that may contain at most one
   wildcard, an asterisk, and any marks on the names matching the pattern
   will be cleared."
  "Clears any mark from the current file in a Dired buffer."
  (dired-frob-deletion p ())
  (dired-frob-mark p ()))

(defcommand "Dired Clear File Marks and Down Line" ()
  "Clears any marks from the current file in a Dired buffer, and moves down
   a line."
  "Clears any mark from the current file, and moves down a line."
  (dired-frob-deletion () ())
  (dired-frob-mark () ())
  (dired-down-line (current-point)))

(defcommand "Dired Clear File Marks with Pattern" ()
  "Clear any marks from files matching a prompted pattern."
  (dired-frob-mark t ())
  (dired-frob-deletion t ())
  (dired-down-line (current-point)))

(defcommand "Dired Clear All Marks" (p)
  "Clear any generic marks and any delete marks from a Dired buffer.  With
   a positive prefix clear only generic (*) marks; with a negative prefix
   clear only delete marks."
  "Clear any generic marks and any delete marks from a Dired buffer.  If P
   is positive clear only generic (*) marks; if P is negative clear only
   delete marks."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  (with-writable-buffer ((current-buffer))
    (with-mark ((mark (buffer-start-mark (current-buffer)) :left-inserting))
      (mark-after mark)
      (do* ((files (dired-info-files (value dired-information)))
	    (len (length files))
	    (pos 0 (1+ pos)))
	   ((equal pos len))
	(let ((file (svref files pos)))
	  (when (if p (plusp p) t)
	    (setf (dired-file-marked-p file) ())
	    (setf (next-character mark) #\ ))
	  (when (if p (minusp p) t)
	    (setf (dired-file-deleted-p file) ())
	    (setf (previous-character mark) #\ )))
	(line-offset mark 1 1)))))

(defcommand "Dired Swap Marks" ()
  "Converts any delete (D) marks into generic (*) marks, and vice versa."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  (with-writable-buffer ((current-buffer))
    (with-mark ((mark (buffer-start-mark (current-buffer)) :left-inserting))
      (mark-after mark)
      (do* ((files (dired-info-files (value dired-information)))
	    (pos 0 (incf pos))
	    (end-mark (buffer-end-mark (current-buffer))))
	   ((mark= mark end-mark))
	(let ((file (svref files pos)))
	  (cond
	   ((dired-file-marked-p file)
	    (setf (dired-file-marked-p file) ()
		  (dired-file-deleted-p file) t
		  (next-character mark) #\D))
	   ((dired-file-deleted-p file)
	    (setf (dired-file-marked-p file) t
		  (dired-file-deleted-p file) ()
		  (next-character mark) #\*))))
	(line-offset mark 1 1)))))


;;;; Dired file finding and going to dired buffers.

(defcommand "Dired Edit File" (p)
  "Read in the file on the current line as if by `Find File'.  If the line
   describes a directory then `Dired' the directory instead.  With a prefix
   argument edit the file in the next window, splitting the window if
   necessary."
  (let ((point (current-point)))
    (if (blank-line-p (mark-line point))
	(editor-error "Point must be on a file line."))
    (with-literal-pathnames
      (let* ((dired-info (value dired-information))
	     (relative (dired-file-pathname
			(array-element-from-mark
			 point
			 (dired-info-files dired-info))))
	     (pathname (merge-pathnames
			relative
			;; FIX ok if buffer pathname wild?
			(buffer-pathname (current-buffer))))
	     (pos (count-lines (region (buffer-start-mark
					(current-buffer))
				       (current-point)))))
	(if p
	    (if (eq (next-window (current-window)) (current-window))
		(split-window-command)
		(next-window-command)))
	(if (directoryp pathname)
	    ;(dired-command () (directory-namestring pathname) :propagate)
	    (dired-command () pathname :propagate)
	    (let ((extra-data (dired-info-extra-data dired-info)))
	      (change-to-buffer (find-file-buffer pathname))
	      (if (and extra-data
		       (consp extra-data)
		       (listp (car extra-data)))
		  (let ((line-number (cadr (nth (1- pos) extra-data))))
		    (if line-number
			(or (string= line-number "")
			    (go-to-absolute-line-command
			     (read-from-string line-number))))))))))))

(defcommand "Dired Edit File Next Window" ()
  "Read in file or \"Dired\" a directory in next window."
  (dired-edit-file-command t))

(defcommand "Dired View File" ()
  "Read in the file on the current line as if by `View File'.  If the line
   describes a directory then `Dired' the directory instead.  Associate the
   View buffer with the dired buffer.  If the line describes a known image
   type then open the image with an external program."
  (with-literal-pathnames
    (let ((point (current-point)))
      (if (blank-line-p (mark-line point))
	  (editor-error "Point must be on a file line."))
      (let ((pathname (merge-pathnames
		       (dired-file-pathname
			(array-element-from-mark
			 point (dired-info-files (value dired-information))))
		       (buffer-pathname (current-buffer)))))
	(view pathname)
	#|
	(if (directoryp pathname)
	    (dired-command () (directory-namestring pathname))
	    (let* ((dired-buf (current-buffer))
		   (trial-pathname (or (probe-file pathname)
				       (merge-pathnames pathname
							(dired-current-directory))))
		   (found
		    ;; Search for an existing view of the file.
		    (or (let ((start 0))
			  (loop
			    for pos
			    = (position trial-pathname
					(the list *buffer-list*)
					:key #'buffer-pathname
					:test #'equal
					:start start)
			    while pos
			    do
			    (if (buffer-minor-mode (nth pos *buffer-list*)
						   "View")
				(return (nth pos *buffer-list*)))
			    (setq start (1+ pos))))
			(let ((b (getstring (pathname-to-buffer-name
					     trial-pathname)
					    *buffer-names*)))
			  (and b (buffer-minor-mode b "View") b))))
		   (buffer (if found
			       (progn
				 (change-to-buffer found)
				 (if (< (buffer-write-date found)
					(file-write-date (buffer-pathname found)))
				     (revert-file-command))
				 found)
			       (view-file-command () pathname))))
	      (push #'(lambda (buffer)
			(declare (ignore buffer))
			(setf dired-buf ()))
		    (buffer-delete-hook dired-buf))
	      (setf (variable-value 'view-return-function :buffer buffer)
		    #'(lambda ()
			(if dired-buf
			    (change-to-buffer dired-buf)
			    (dired-from-buffer-pathname-command))))))
	|#
	))))

(defcommand "Dired from Buffer Pathname" ()
  "Invoke `Dired' on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory."
  (let ((pathname (or (if (editor-bound-p 'dired-information)
			  (dired-info-pathname (value dired-information))
			  (buffer-pathname (current-buffer)))
		      (editor-error
         "Current buffer must have an associate pathname."))))
    ;; Ensure that there is at least one ancestor in the pathname.
    (if (string= (directory-namestring pathname) "")
	(setq pathname (truename pathname)))
    ;; Show the parent directory for dired buffers showing the entire
    ;; directory, as these buffers are already showing the directory.
    (if (editor-bound-p 'dired-information :buffer (current-buffer))
	(or (dired-info-pattern (value dired-information))
	    (setq pathname (namify pathname))))
    ;; FIX if hiddenp and dired exists ensure that existing dired shows
    ;;     hidden files
    (dired-command (if (probe-file pathname) (hiddenp pathname))
		   (directory-namestring pathname))
    (setq pathname (if (directoryp pathname)
		       (let ((pathname (file-namestring
					(namify pathname))))
			 (if pathname (ensure-trailing-slash pathname)))
		       (file-namestring pathname)))
    (when pathname
      (until* ((pos 0 (incf pos))
	       (files (dired-info-files (value dired-information)))
	       (end (length files)))
	      ((eq pos end))
	(when (equal pathname (dired-file-pathname (aref files pos)))
	  (buffer-start (current-point))
	  (line-offset (current-point) pos)
	  (center-window (current-window) (current-point))
	  (return-from dired-from-buffer-pathname-command))))))

(defcommand "Dired Up Directory" ()
  "Invoke \"Dired\" on the directory up one level from the current
   directory."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  (let ((dirs (or (pathname-directory
		   (dired-info-pathname (value dired-information)))
		  '(:relative))))
    (dired-command ()
		   (truename (make-pathname :directory
					    (concatenate 'list
							 dirs
							 '(:up)))))))

#[ Modifying the Display

{command:Dired Update Buffer}
{command:Dired Toggle Recurse}
{command:Dired Toggle Hidden Files}
{command:Dired Toggle Backups}
{command:Dired Toggle All Files}
]#


;;;; Dired display control commands.

(defcommand "Dired Toggle Recurse" ()
  "Toggle recursive display, updating buffer."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  ;; FIX (decf? ie (- universe universe)
  (edi::complf (dired-info-recurse (value dired-information)))
  (dired-update-buffer-command () t))

(defcommand "Dired Toggle Backups" ()
  "Toggle display of backup files, updating buffer."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  ;; FIX (decf? ie (- universe universe)
  (edi::complf (dired-info-backup-files-p (value dired-information)))
  (dired-update-buffer-command () t))

(defcommand "Dired Toggle Hidden Files" ()
  "Toggle display of files starting with `.', updating buffer."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  ;; FIX
  (edi::complf (dired-info-dot-files-p (value dired-information)))
  (dired-update-buffer-command () t))

(defcommand "Dired Toggle All Files" ()
  "Toggle display of hidden and backup files, updating buffer."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in a Dired buffer."))
  ;; FIX
  (edi::complf (dired-info-backup-files-p (value dired-information)))
  (edi::complf (dired-info-dot-files-p (value dired-information)))
  (dired-update-buffer-command () t))



;;;; Dired misc. commands -- update, help, line motion.

(defcommand "Dired Update Buffer" (p regenerate)
  "Recompute the contents of the dired buffer in the current buffer.
   Maintain flags for files that stay the same."
  "Recompute the contents of the dired buffer in the current buffer.
   Maintain flags for files that stay the same.  If $regenerate is true and
   there is a generator then regenerate the listing."
  (declare (ignore p))
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in Dired buffer."))
  (let* ((dir-info (value dired-information))
	 (generator (dired-info-generator dir-info)))
    (when (and generator
	       (or regenerate
		   (prompt-for-y-or-n :prompt "Regenerate listing? "
  :help "Y to rerun the processing that produced the listed files."
                                      :must-exist t
				      :default ()
				      :default-string "N")))
      (setf (dired-info-extra-data dir-info) (funcall generator dir-info)))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 (current-buffer))))

(defun update-dired-buffer (directory pattern buffer)
  "Update $buffer with a dired of $directory, deleting whatever is in
   $buffer already.  Assume $buffer was previously used as a dired buffer
   having necessary variables bound.  Compare the new files to the old
   ones, propagating any deleted flags if the name and the write date is
   the same for both specifications."
  (let* ((dir-info (variable-value 'dired-information :buffer buffer))
	 (point (buffer-point buffer))
	 (current-file (fi (blank-line-p (mark-line point))
			   (dired-file-pathname
			    (array-element-from-mark
			     point
			     (dired-info-files dir-info))))))
    (fi (probe-file (dired-info-pathname dir-info))
	;; Presumably directory was moved/flushed.
	;; FIX maybe kill the buffer? then also kill edit buffers
	;;         general style seems to be to let buffers be
	()
	(with-writable-buffer (buffer)
	  (delete-region (buffer-region buffer))
	  (multiple-value-bind
	      (pathnames new-dired-files)
	      (dired-in-buffer directory pattern
			       (dired-info-dot-files-p dir-info)
			       (dired-info-backup-files-p dir-info)
			       (dired-info-coldefs dir-info)
			       (dired-info-recurse dir-info)
			       (dired-info-extra-data dir-info)
			       buffer)
	    (let ((point (buffer-point buffer))
		  (old-dired-files (dired-info-files dir-info)))
	      (declare (simple-vector old-dired-files))
	      (dotimes (i (length old-dired-files))
		(let ((old-file (svref old-dired-files i)))
		  (when (dired-file-deleted-p old-file)
		    (let ((pos (position (dired-file-pathname old-file)
					 new-dired-files :test #'equal
					 :key #'dired-file-pathname)))
		      (when pos
			(let* ((new-file (svref new-dired-files pos))
			       (write-date (file-write-date
					    (dired-file-pathname new-file)
					    :check-for-links t)))
			  (when (= (dired-file-write-date old-file) write-date)
			    (setf (dired-file-deleted-p new-file) t)
			    (setf (dired-file-write-date new-file) write-date)
			    (setf (next-character
				   (line-offset (buffer-start point) pos 0))
				  #\D))))))
		  (when (dired-file-marked-p old-file)
		    (let ((pos (position (dired-file-pathname old-file)
					 new-dired-files :test #'equal
					 :key #'dired-file-pathname)))
		      (when pos
			(let ((new-file (svref new-dired-files pos)))
			  (setf (dired-file-marked-p new-file) t)
			  (setf (next-character
				 (line-offset (buffer-start point) pos 1))
				#\*)))))))
	      (setf (dired-info-files dir-info) new-dired-files)
	      (setf (dired-info-file-list dir-info) pathnames)
	      (setf (dired-info-write-date dir-info)
		    (file-write-date directory))
	      (move-mark point (buffer-start-mark buffer))
	      (if current-file
		  (let ((pos (position current-file
				       new-dired-files :test #'equal
				       :key #'dired-file-pathname)))
		    (if pos (line-offset point pos)))
		  (buffer-end point))))))))

(defun true-and-directory-p (pathname)
  (if pathname (directoryp pathname)))

(defun dired-in-buffer (directory pattern dot-files-p backup-files-p coldefs
			recurse extra-data buffer)
  "Insert a dired listing of DIRECTORY in BUFFER, returning two values: a
   list of pathnames of files only, and an array of dired-file structures.
   Use FILTER-REGION to insert a space for the indication of whether the
   file is flagged for deletion.  FIX Then clean up extra header and
   trailing lines known to be in the output."
  (let ((point (buffer-point buffer)))
    (with-writable-buffer (buffer)
      (multiple-value-bind (pathnames col-posns stats)
			   (call-print-directory
			    (if pattern
				(merge-pathnames directory pattern)
				directory)
			    point
			    dot-files-p
			    backup-files-p
			    coldefs
			    recurse
			    extra-data)
	(if (editor-bound-p 'dired-information :buffer buffer)
	    (setf (dired-info-col-posns (variable-value 'dired-information
							:buffer buffer))
		  col-posns))
	(let ((dired-files (make-array (length pathnames))))
	  (declare (list pathnames) (simple-vector dired-files))
	  (filter-region #'(lambda (str)
			     (concatenate 'simple-string "  " str))
			 (buffer-region buffer))
	  (delete-characters point -2)
	  (delete-region (line-to-region (mark-line (buffer-start point))))
	  (delete-characters point)
	  (while ((p pathnames (cdr p))
		  (i 0 (1+ i)))
		 (p)
	    (setf (svref dired-files i) (make-dired-file (car p))))
	  (values (in-directory directory
		    (let ((index -1))
		      (delete-if (lambda (pathname)
				   (incf index)
				   (and pathname
					(eq (file-kind-from-mode
					     (nth 3 (aref stats index)))
					    :directory)))
				 pathnames)))
		  dired-files))))))

(defcommand "Dired Help" ()
  "Pop up a help window listing the various `Dired' commands."
  (describe-mode-command () "Dired"))

(defcommand "Dired Next File" (p)
  "Move to the next file, skipping over delete-marked files."
  (fi (dired-line-offset (current-point) (or p 1))
      (editor-error "Too few lines.")))

(defcommand "Dired Previous File" (p)
  "Move to the previous file, skipping over delete-marked files."
  (fi (dired-line-offset (current-point) (or p -1))
      (editor-error "Too few lines.")))

(defun dired-line-offset (mark n)
  "Move mark N undeleted file lines, returning mark.  Only move mark If
   there are enough lines.  Return ()."
  (with-mark ((m mark))
    (let ((step (if (plusp n) 1 -1)))
      (dotimes (i (abs n) (move-mark mark m))
	(loop
	  (or (line-offset m step 0)
	      (return-from dired-line-offset ()))
	  (when (blank-line-p (mark-line m))
	    (return-from dired-line-offset ()))
	  (when (char= (next-character m) #\space)
	    (return)))))))


;;;; Dired user interaction functions.

(defun dired-error-function (string &rest args)
  (apply #'editor-error string args))

(defun dired-report-function (string &rest args)
  (clear-echo-area)
  (apply #'message string args))

(defun dired-yesp-function (string &rest args)
  (prompt-for-y-or-n :prompt (cons string args) :default t))


;;;; Buffer consistency.

(defun maintain-dired-consistency ()
  (dolist (info *pathnames-to-dired-buffers*)
    (let* ((directory (car info))
	   (buffer (cdr info))
	   (dir-info (variable-value 'dired-information :buffer buffer)))
      (when (and (eq (dired-info-extra-data dir-info) t)
		 ;; Could be () if an error occurred during Dired.
		 (dired-info-write-date dir-info))
	(or (= (dired-info-write-date dir-info)
	       (or (file-write-date (directory-namestring directory)) 0))
	    (update-dired-buffer (directory-namestring directory)
				 (dired-info-pattern dir-info)
				 buffer))))))


;;;; Dired expunging and quitting.

#|
;; FIX This needs more work to be useful: it only treats buffers of the
;; exact same path and name as the given pathname (maybe check what
;; find-file-buffer does).  Maybe it's cooler to keep the buffers anyway
;; given that find-file prompts to reuse the buffer if the file is opened
;; again.
(defun dired-post-delete-function (pathname)
  "Delete any buffer named according to Pathname."
  (let ((name (pathname-to-buffer-name pathname)))
    (when name
      (let ((buffer (getstring (namestring name) *buffer-names*)))
	(when buffer
	  (or (buffer-modified buffer) (delete-buffer buffer)))))))
|#

(defcommand "Dired Expunge Files" ()
  "Expunge files and directories marked for deletion, recursively.  Ask for
   confirmation for files if *Dired File Expunge Confirm* is set and for
   directories if *Dired Directory Expunge Confirm* is set."
  (if (expunge-dired-files) (dired-update-buffer-command))
  (maintain-dired-consistency))

(defcommand "Dired Quit" ()
  "Expunge any marked files and directories as if by `Expunge Dired Files'
   then delete the dired buffer."
  (expunge-dired-files)
  (delete-buffer-if-possible (current-buffer)))

(defevar "Dired File Expunge Confirm"
  "When set, `Dired Expunge Files' and `Dired Quit' ask for confirmation
   before deleting the marked files."
  :value t)

(defevar "Dired Directory Expunge Confirm"
  "When set, `Dired Expunge Files' and `Dired Quit' will ask for
   confirmation before deleting each marked directory."
  :value t)

(defun expunge-dired-files ()
  (with-literal-pathnames
    (multiple-value-bind (marked-files marked-dirs)
			 (get-delete-marked-dired-files)
      (let ((dired:*error-function* #'dired-error-function)
	    (dired:*report-function* #'dired-report-function)
	    (dired:*yesp-function* #'dired-yesp-function)
	    did-something-p)
	(when (and marked-files
		   (if (value dired-file-expunge-confirm)
		       (with-pop-up-window (buffer window)
			 (let ((point (buffer-point buffer))
			       (end-line (mark-line
					  (window-display-end
					   (current-window)))))
			   (dolist (file-info marked-files)
			     (insert-string
			      point
			      (format () "~A~%"
				      (file-namestring
				       (namify (car file-info)))))
			     (when (equal (mark-line point) end-line)
			       ;;; FIX check
			       (insert-string point "...")
			       (return))))
			 (prompt-for-y-or-n :prompt
					    (format () "Really delete file~P? "
						    (length marked-files))
					    :default t
					    :must-exist t
					    :default-string "Y"))
		       t))
	  (setf did-something-p t)
	  (dolist (file-info marked-files)
	    (let ((pathname (car file-info))
		  (write-date (cdr file-info)))
	      ;; FIX assuming write-date only () for broken symlinks
	      (if (or (symlinkp pathname)
		      (if (and write-date
			       (file-write-date pathname
						:check-for-links t))
			  (= write-date
			     (file-write-date pathname
					      :check-for-links t))
			  t))
		  (dired:delete-file (namestring pathname) :clobber t
				     :recurse ())
		  (message "~A has been modified, so it remains."
			   (namestring pathname))))))
	(when (and marked-dirs
		   (if (value dired-directory-expunge-confirm)
		       (with-pop-up-window (buffer window)
			 (let ((point (buffer-point buffer)))
			   (dolist (dir-info marked-dirs)
			     (insert-string
			      point
			      (format () "~A~%"
				      (file-namestring
				       (namify (car dir-info)))))))
			 (prompt-for-yes-or-no :prompt
					       (format ()
						       "Really delete dir~P? "
						       (length marked-dirs))
					       :default ()
					       :must-exist t
					       :default-string "No"))
		       t))
	  (dolist (dir-info marked-dirs)
	    (let ((dir (car dir-info))
		  (write-date (cdr dir-info)))
	      ;; FIX assuming write-date only () for broken symlinks
	      (if (or (symlinkp dir)
		      (if (and write-date
			       (file-write-date dir :check-for-links t))
			  (= write-date
			     (file-write-date dir :check-for-links t))
			  t))
		  (if (symlinkp (namify dir))
		      ;; FIX These are more like files.
		      (progn
			(dired:delete-file (namify dir)
					   :clobber t
					   :recurse ())
			(setf did-something-p t))
		      (progn
			(dired:delete-file (directory-namestring dir)
					   :clobber t
					   :recurse t)
			(setf did-something-p t)))
		  (message "~A has been modified, so it remains."
			   dir)))))
	did-something-p))))

(defun clear-marks (files point dired-info-files directory)
  (dolist (file files)
    (buffer-start point)
    (line-offset point
		 (position (pathname (car file)) dired-info-files
			   :test 'equal
			   :key #'(lambda (pn)
				    (merge-pathnames
				     (dired-file-pathname pn)
				     directory))))
    (dired-frob-deletion () ())))

(defcommand "Dired Delete and Expunge" (p)
  "Expunge any marked files, or mark and expunge the file on the current
   line."
  (multiple-value-bind (marked-files marked-dirs)
		       (with-literal-pathnames
			 (get-delete-marked-dired-files))
      (if (or marked-files marked-dirs)
	  (dired-expunge-files-command)
	  (multiple-value-bind (marked-files marked-dirs)
			       (with-literal-pathnames
				 (get-marked-dired-files))
	    (let* ((file)
		   (point (current-point))
		   (dired-info (value dired-information))
		   (info-files (dired-info-files dired-info))
		   (directory (dired-info-pathname dired-info)))
	      (if (or marked-files marked-dirs)
		  (progn
		    (if marked-files
			(dolist (f marked-files)
			  (buffer-start point)
			  (line-offset
			   point
			   (or (position
				(pathname (car f)) info-files
				:test 'equal
				:key #'(lambda (pn)
					 (with-literal-pathnames
					   (merge-pathnames
					    (dired-file-pathname
					     pn)
					    directory))))
			       (error "Failed to find ~A in directory info"
				      (pathname (car f)))))
			  (dired-frob-deletion () t)))
		    (if marked-dirs
			(dolist (f marked-dirs)
			  (buffer-start point)
			  (line-offset
			   point
			   (position
			    (pathname (car f)) info-files
			    :test 'equal
			    :key #'(lambda (pn)
				     (with-literal-pathnames
				       (merge-pathnames
					(dired-file-pathname pn)
					directory)))))
			  (dired-frob-deletion () t))))
		  (progn
		    (setq file
			  (array-element-from-mark point info-files))
		    (dired-frob-deletion p t)
		    ;; Hack to keep point near original position.
		    (line-offset point -1)))
	      (unwind-protect
		  (dired-expunge-files-command)
		(if ()
		    (if file
			(let ((position (position file info-files)))
			  (when position
			    (buffer-start point)
			    (line-offset point position)
			    (dired-frob-deletion () ())))
			(progn
			  (if marked-files
			      (clear-marks marked-files point
					   info-files directory))
			  (if marked-dirs
			      (clear-marks marked-dirs point
					   info-files
					   directory)))))))))))


#[ Copying Files

{command:Dired Copy File}
{command:Dired Copy with Wildcard}
{evariable:Dired Copy File Confirm}
]#

#[ Renaming Files

{command:Dired Rename File}
{command:Dired Rename with Wildcard}
{evariable:Dired Rename File Confirm}
]#

#[ Other File Operations

{command:Dired Touch File}
{command:Dired Symlink File}
{command:Dired Compare Files}
{command:Dired Toggle File Compression}
{command:Dired Shell Command on File}
]#


;;;; Dired operations on files and directories.

(defevar "Dired Copy File Confirm"
  "Control of the interaction when copying:

     true
	When the destination specification exists, the copying process
	stops and asks if it should overwrite the destination.

     ()
	The copying process always copies the source file to the
	destination specification.

     :update
	When the destination specification exists, and its write date is
	newer than the source's write date, then the copying process stops
	and asks the user if it should overwrite the destination."
  :value t)

(defevar "Dired Rename File Confirm"
  "When true Dired will query before overwriting an existing file."
  :value t)

(defevar "Dired Symlink File Confirm"
  "When true Dired will query before overwriting an existing file with a
   symlink."
  :value t)

(defcommand "Dired Copy File" (p)
  "Copy the file on the current line to a prompted destination.  The
   destination is either a directory specification or a file name.  When it
   is a directory, copy the source into the directory with its current file
   name and extension.

   With a prefix follow symbolic links, otherwise preserve symbolic links."
  (with-literal-pathnames
    (multiple-value-bind (marked-files marked-dirs)
			 (get-marked-dired-files)
      (let* ((point (current-point))
	     (confirm (value dired-copy-file-confirm))
	     (dired-info (value dired-information))
	     (source (if (or marked-files marked-dirs)
			 ()
			 (merge-pathnames
			  (dired-file-pathname
			   (array-element-from-mark
			    point
			    (dired-info-files dired-info)))
			  (dired-info-pathname dired-info))))
	     (files (if source
			()
			(mapcar 'car (append marked-files
					     marked-dirs))))
	     (dest (merge-pathnames
		    (prompt-for-file
		     :prompt (if (if source
				     (directoryp source)
				     (> (length files) 1))
				 "Destination Directory Name: "
				 "Destination Filename: ")
		     :help "Name of new file."
		     :default (if (if source
				      (directoryp source)
				      (> (length files) 1))
				  (dired-info-pathname dired-info)
				  source)
		     :must-exist ())
		    (dired-info-pathname dired-info)))
	     (dired:*error-function* #'dired-error-function)
	     (dired:*report-function* #'dired-report-function)
	     (dired:*yesp-function* #'dired-yesp-function))
	(ensure-directories-exist dest)
	(if source
	    (dired:copy-file source dest
			     :update (if (eq confirm :update) t ())
			     :clobber (fi confirm)
			     :check-for-links (fi p))
	    (dolist (f files)
	      (dired:copy-file f dest
			       :update (if (eq confirm :update) t ())
			       :clobber (fi confirm)
			       :check-for-links (fi p)))))))
  (maintain-dired-consistency))

;; FIX need check-for-links?
(defcommand "Dired Rename File" ()
  "Rename the file or directory under the point."
  (with-literal-pathnames
    (multiple-value-bind (marked-files marked-dirs)
			 (get-marked-dired-files)
      (let* ((point (current-point))
	     (dired-info (value dired-information))
	     (directory (dired-info-pathname dired-info))
	     (source
	      (fi (or marked-files marked-dirs)
		  (namify
		   (merge-pathnames (dired-file-pathname
				     (array-element-from-mark
				      point
				      (dired-info-files dired-info)))
				    directory))))
	     (files (fi source
			(mapcar #'car (append marked-files
					      marked-dirs))))
	     (dest (merge-pathnames
		    (prompt-for-file
		     :prompt (if (> (length files) 1)
				 "Destination Directory Name: "
				 "New Filename: ")
		     :help (if (> (length files) 1)
			       "The destination directory for the marked files."
			       "The new name for this file.")
		     :default source
		     :must-exist ())
		    (dired-info-pathname dired-info)))
	     (dired:*error-function* #'dired-error-function)
	     (dired:*report-function* #'dired-report-function)
	     (dired:*yesp-function* #'dired-yesp-function))
	(if (> (length files) 1)
	    (or (directory-name-p dest)
		(setq dest (concatenate 'simple-string
					(namestring dest) "/"))))
	;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
	(if source
	    (dired:rename-file source dest
			       :clobber
			       (fi (value dired-rename-file-confirm)))
	    (let ((clobber (fi (value dired-rename-file-confirm))))
	      (dolist (f files)
		(dired:rename-file (merge-pathnames f directory)
				   dest :clobber clobber)))))))
  ;; FIX It'd be nice if point came out on the new file.
  (maintain-dired-consistency))

(defcommand "Dired Symlink File" ()
  "Symbolic link to the file under the point."
  (with-literal-pathnames
    (multiple-value-bind (marked-files marked-dirs)
			 (get-marked-dired-files)
      (let* ((point (current-point))
	     (dired-info (value dired-information))
	     (dest (if (or marked-files marked-dirs)
		       ()
		       (dired-file-pathname
			(array-element-from-mark
			 point
			 (dired-info-files dired-info)))))
	     (files (if dest
			()
			(append marked-files marked-dirs)))
	     (dired:*error-function* #'dired-error-function)
	     (dired:*report-function* #'dired-report-function)
	     (dired:*yesp-function* #'dired-yesp-function)
	     ;(directory (dired-info-pathname dired-info))
	     )
	(flet ((prompt (dest)
		 ;(merge-pathnames
		 (prompt-for-file
		  :prompt (format () "Link to ~A from: " dest)
		  :help "A name for the link to the file."
		  :default (namify (namestring dest))
		  :must-exist ())
		 ;directory)
		 ))
	  (if dest
	      (dired:symlink-file
	       (prompt dest) dest
	       :clobber (fi (value dired-symlink-file-confirm)))
	      (let ((clobber (fi (value dired-symlink-file-confirm))))
		(dolist (f files)
		  (dired:symlink-file (prompt (car f)) (car f)
				      :clobber clobber))))))))
  (maintain-dired-consistency))

(defcommand "Dired Touch File" ()
  "Touch the file under point."
  (let* ((dir-info (value dired-information))
	 (directory (dired-info-pathname dir-info)))
    (with-literal-pathnames
      (multiple-value-bind (marked-files marked-dirs)
			   (get-marked-dired-files)
	(if (or marked-files marked-dirs)
	    (dolist (f (append marked-files marked-dirs))
	      (touch-file (merge-pathnames (car f) directory)))
	    (touch-file (merge-pathnames
			 (dired-file-pathname
			  (array-element-from-mark
			   (current-point)
			   (dired-info-files dir-info)))
			 directory)))))
    ;; FIX this should also update other direds of this dir
    ;;     as maitain-d-c depends on dir write-date, which is
    ;;     the same after touching the files
    ;;     maybe m-d-c s/b adjusted
    (update-dired-buffer (dired-current-directory)
			 (dired-info-pattern dir-info)
			 (current-buffer))
    (maintain-dired-consistency)))

(defcommand "Dired Copy with Wildcard" (p)
  "Prompt for a name pattern that may contain at most one wildcard (an
   asterisk) and copy all the names matching the pattern.  Prompt for the
   destination suggesting the `Dired' buffer's directory.  The destination
   is either a directory specification or a file name with a wildcard.
   When it is a directory, copy all the source files into the directory
   under their current file names and extensions.  When it is a file name,
   for each sources file substitute the matching part of the source
   wildcard with the corresponding wildcard of the destination pattern.
   This enables, for example, \"*.txt\" to be copied to \"*.text\".

   With a prefix follow symbolic links, otherwise preserve symbolic links."
  (with-literal-pathnames
    (let* ((dir-info (value dired-information))
	   (confirm (value dired-copy-file-confirm))
	   (pattern (prompt-for-string
		     :prompt "Filename pattern: "
		     :help "Type a filename with a single asterisk."
		     :trim t))
	   (destination (namestring
			 (prompt-for-file
			  :prompt "Destination Spec: "
			  :help "Destination spec.  May contain ONE asterisk."
			  :default (dired-info-pathname dir-info)
			  :must-exist ())))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*yesp-function* #'dired-yesp-function)
	   (dired:*report-function* #'dired-report-function))
      (in-directory (dired-info-pathname dir-info)
	(dired:copy-file pattern destination
			 :update (if (eq confirm :update) t ())
			 :clobber (not confirm)
			 :directory (dired-info-file-list dir-info)
			 :check-for-links (fi p)))))
  (maintain-dired-consistency))

(defcommand "Dired Rename with Wildcard" ()
  "Rename files that match a pattern containing one wildcard."
  (with-literal-pathnames
    (let* ((dir-info (value dired-information))
	   (pattern (prompt-for-string
		     :prompt "Filename pattern: "
		     :help "Type a filename with a single asterisk."
		     :trim t))
	   (destination (namestring
			 (prompt-for-file
			  :prompt "Destination Spec: "
			  :help "Destination spec.  May contain one asterisk."
			  :default (dired-info-pathname dir-info)
			  :must-exist ())))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*yesp-function* #'dired-yesp-function)
	   (dired:*report-function* #'dired-report-function))
      (dired:rename-file pattern destination
			 :clobber (not (value dired-rename-file-confirm))
			 :directory (dired-info-file-list dir-info))))
  (maintain-dired-consistency))

(defcommand "Dired Compare Files" ()
  "Compare Dired files."
  (with-literal-pathnames
    (multiple-value-bind (marked-files marked-dirs)
			 (get-marked-dired-files)
      (declare (ignore marked-dirs))
      (let* ((point (current-point))
	     (dired-info (value dired-information))
	     (directory (dired-info-pathname dired-info))
	     (one
	      (if marked-files
		  (caar marked-files)
		  (namify (merge-pathnames
			   (dired-file-pathname
			    (array-element-from-mark
			     point
			     (dired-info-files dired-info)
			     "Point must be on a file line, or a file must be marked."))
			   directory))))
	     (two (or (and marked-files (caadr marked-files))
		      (merge-pathnames
		       (in-directory directory
			 (prompt-for-file
			  :prompt "Second file: "
			  :help (format () "File to compare with ~A."
					one)
			  :default (file-namestring one)
			  :must-exist t))
		       directory)))
	     (dired:*error-function* #'dired-error-function)
	     (dired:*report-function* #'dired-report-function)
	     (dired:*yesp-function* #'dired-yesp-function))
	;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
	(compare-files-command () one two)))))

(defcommand "Make Directory" (p pathname)
  "Make a prompted directory"
  (declare (ignore p))
  (with-literal-pathnames
    (let ((pn (or pathname
		  (prompt-for-file
		   :prompt "Directory to make: "
		   :must-exist ()
		   :help "Name of directory to create."
		   :default (directory-namestring
			     (buffer-default-pathname
			      (current-buffer)))))))
      ;; FIX ensure-d-e should use mode from umask mode
      (multiple-value-bind (success error)
			   (ensure-directories-exist
			    (concatenate 'simple-string
					 (namestring pn)
					 "/"))
	(or success (editor-error "~A" error))))))

(defcommand "Dired Make Directory" (&optional p pathname)
  "Make directory Pathname, prompting for a name if Pathname is ().
   Expect to be run in a Dired.  Update buffer afterwards."
  (make-directory-command p pathname)
  (maintain-dired-consistency))

(defcommand "Dired Change File Mode" ()
  "Change the mode of the file under point."
  (let ((mode (prompt-for-mode)))
    (multiple-value-bind (marked-files marked-dirs)
			 (with-literal-pathnames
			     (get-marked-dired-files))
      (let* ((dir-info (value dired-information))
	     (directory (dired-info-pathname dir-info)))
	(with-literal-pathnames
	  (if (or marked-files marked-dirs)
	      (dolist (f (append marked-files marked-dirs))
		(setf (file-mode (merge-pathnames (car f) directory))
		      mode))
	      (setf (file-mode (merge-pathnames
				(dired-file-pathname
				 (array-element-from-mark
				  (current-point)
				  (dired-info-files dir-info)))
				directory))
		    mode)))
	;; FIX Update only modified lines?
	(update-dired-buffer (dired-current-directory)
			     (dired-info-pattern dir-info)
			     (current-buffer)))))
  (maintain-dired-consistency))


#[ File Utility Commands

This section describes some general file operation commands and quick directory
commands.

[Dired Mode] describes the editor's directory editing mechanism.

{command:Copy File}
{command:Rename File}

To rename a directory leave the trailing slash off the source specification.

{command:Delete File}
{command:Directory}
{command:Verbose Directory}
]#

(defun calculate-suggestion ()
  (let ((buffer (current-buffer))
	(point (current-point)))
    (if (string= (buffer-major-mode buffer) "Dired")
	(let ((dired-info (value dired-information)))
	  (if (blank-line-p (mark-line point))
	      (file-namestring (dired-info-pathname dired-info))
	      (dired-file-pathname
	       (array-element-from-mark
		point
		(dired-info-files dired-info)))))
	(file-namestring (buffer-pathname buffer)))))

;; FIX wildcards: "xx is ambiguous"
(defcommand "Delete File" (p)
  "Delete a file." ;; FIX p
  (let ((*literal-pathnames* p)
	(lisp::*ignore-wildcards* p)) ; FIX
    (let* ((spec (namestring
		  (prompt-for-file
		   :prompt "Delete File: "
		   :help '("Name of File or Directory to delete.  ~
			    One wildcard is permitted.")
		   :default (calculate-suggestion)
		   :must-exist ())))
	   (directoryp (fi (wild-pathname-p spec) (directoryp spec)))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      (when (if (and directoryp
		     (value dired-directory-expunge-confirm))
		(prompt-for-yes-or-no
		 :prompt (list "~A is a directory. Delete it? "
			       (directory-namestring spec))
		 :default () :must-exist t :default-string "No")
		t)
	(dired:delete-file (merge-pathnames spec
					    (dired-current-directory))
			   :recurse t
			   :clobber
			   (or directoryp
			       (value dired-file-expunge-confirm))))))
  (maintain-dired-consistency))

(defcommand "Copy File" (p)
  "Copy a file, allowing one wildcard in the filename.  Prompt for source
   and destination specifications.

   If these are both directories, then copy recursively on the source,
   skipping the copying of the destination if it is in the source directory
   tree.  If the source is dir-spec-1/* then copy only the files in the
   first level of the source directory instead of recursing into
   subdirectories.

   If the destination specification is a directory, and the source is a
   file, then copy the file into the destination with the same filename.

   Maintain the write date of the source when copying.

   When the destination exists interact according to the variable `Dired
   Copy File Confirm'.

   With a prefix follow symbolic links, otherwise preserve symbolic links."
  (with-literal-pathnames
    (let* ((confirm (value dired-copy-file-confirm))
	   (source (namestring
		    (prompt-for-file
		     :prompt "Source Filename: "
		     :help "Name of File to copy.  One wildcard is permitted."
		     :default (calculate-suggestion)
		     :must-exist ())))
	   (dest (namestring
		  (prompt-for-file
		   :prompt (if (directoryp source)
			       "Destination Directory Name: "
			       "Destination Filename: ")
		   :help "Name of new file."
		   :default source
		   :must-exist ())))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      (dired:copy-file (merge-pathnames source (dired-current-directory))
		       (merge-pathnames dest (dired-current-directory))
		       :update (if (eq confirm :update) t)
		       :clobber (fi confirm)
		       :check-for-links p)))
  (maintain-dired-consistency))

(defcommand "Rename File" ()
  "Rename a file, allowing one wildcard in the filename.  Prompt for source
   and destination specifications.

   If the destination is a directory, then move the file(s) indicated by the
   source into the directory with their original filenames."
  (with-literal-pathnames
    (let* ((source (namestring
		    (prompt-for-file
		     :prompt "Source Filename: "
		     :help "Name of file to rename.  One wildcard is permitted."
		     :default (calculate-suggestion)
		     :must-exist ())))
	   (dest (namestring
		  (prompt-for-file
		   :prompt (if (directoryp source)
			       "Destination Directory Name: "
			       "Destination Filename: ")
		   :help "Name of new file."
		   :default source
		   :must-exist ())))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      (dired:rename-file (merge-pathnames source (dired-current-directory))
			 (merge-pathnames dest (dired-current-directory))
			 :clobber (not (value dired-rename-file-confirm)))))
  (maintain-dired-consistency))

(defcommand "Symlink File" ()
  "Create a symbolic link to a file."
  (with-literal-pathnames
    (let* ((source (namestring
		    (prompt-for-file
		     :prompt "Link name: "
		     :help "Name to link to file."
		     :default (calculate-suggestion)
		     :must-exist ())))
	   (dest (namestring
		  (prompt-for-file
		   :prompt "Target name: "
		   :help "Name of the file to which to link."
		   :default source
		   :must-exist ())))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      (dired:symlink-file (merge-pathnames source (dired-current-directory))
			  (merge-pathnames dest (dired-current-directory)))))
  (maintain-dired-consistency))

(defcommand "Make Directory" (p pathname)
  "Make a prompted directory"
  (declare (ignore p))
  (with-literal-pathnames
    (let* ((pn (or pathname
		   (prompt-for-file
		    :prompt "Directory to make: "
		    :must-exist ()
		    :help "Name of directory to create."
		    :default (directory-namestring (buffer-default-pathname
						    (current-buffer)))))))
      ;; FIX ensure-d-e should use mode from umask mode
      (multiple-value-bind (success error)
			   (ensure-directories-exist (concatenate 'simple-string
								  (namestring pn)
								  "/"))
	(or success (editor-error "~A" error)))))
  (maintain-dired-consistency))


;;;; Dired and the shell.

(declaim (special *shell-command-in-buffer-history*))

(defun dired-toggle-file-compression (file)
  "Toggle compression of $file.  Return new name."
  (let ((type (pathname-type file))
	(arg `(,(namestring file)))
	(file (namestring file)))
    (cond ((string= type "gz")
	   (let ((ret (ext:run-program "gunzip" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (1 (message "gunzip exited with error."))
		 (2 (message "gunzip exited with warning."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "bz2")
	   (let ((ret (ext:run-program "bunzip2" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "bunzip2 exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "zip")
	   (let ((ret (ext:run-program "unzip" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "unzip exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ((string= type "Z")
	   (let ((ret (ext:run-program "uncompress" arg)))
	     (when ret
	       (case (process-exit-code ret)
		 (0)
		 (t (message "uncompress exited with error."))))
	     (subseq file 0 (- (length file) (length type) 1))))
	  ;(t (ext:run-program "bzip2" arg))
	  (t (ext:run-program "gzip" arg)
	     (concatenate 'simple-string file ".gz")))))

;; FIX Creates archive of dir.  Perhaps should replace the dir with the
;; archive.
(defun dired-toggle-dir-compression (name)
  "Toggle compression of directory Name."
  (let ((ret (ext:run-program "tar"
			      `("jcf"
				,(concatenate 'simple-string
					      (namify (namestring name))
					      ".tar.bz2")
				,(namestring name)))))
    (when ret
      (case (process-exit-code ret)
	(0)
	(t (message "tar exited with error."))))))

(defcommand "Dired Toggle File Compression" ()
  "Compress or inflate file(s).  Operate on marked files if there are any,
   else on the file at point."
  (multiple-value-bind (marked-files marked-dirs)
		       (with-literal-pathnames
			(get-marked-dired-files))
    (if (or marked-files marked-dirs)
	(progn
	  (with-literal-pathnames
	   (if marked-files
	       (dolist (file marked-files)
		 (dired-toggle-file-compression (car file))))
	   (if marked-dirs
	       (dolist (f marked-dirs)
		 (dired-toggle-dir-compression (car f)))))
	  (maintain-dired-consistency))
	(let ((name (dired-file-pathname
		     (array-element-from-mark
		      (current-point)
		      (dired-info-files (value dired-information))))))
	  (line-offset (current-point) 1)
	  (if (with-literal-pathnames (directoryp name))
	      (progn
		(with-literal-pathnames
		 (dired-toggle-dir-compression name))
		(maintain-dired-consistency))
	      (let ((name (with-literal-pathnames
			   (dired-toggle-file-compression (namify name)))))
		(maintain-dired-consistency)
		(let ((files (dired-info-files (value dired-information))))
		  ;; FIX The pos'n fails when this is run twice in quick succession.
		  ;; (redisplay)
		  (move-mark (current-point) (buffer-start-mark (current-buffer)))
		  (line-offset (current-point)
			       (or (position name files
					     :test #'equal
					     :key #'dired-file-pathname)
				   (editor-error
				    "Failed to find ~A in dired-info-files."
				    name))))))))))

(defcommand "Dired Shell Command on File" ()
  "Run a prompted shell command on the marked files or the current file.
   If the command includes \"$A\" or \"${A}\" then export A as the name of
   the file(s) before running the command, otherwise append a space and the
   filename(s) to the command."
  (with-literal-pathnames
    (multiple-value-bind (marked-files marked-dirs)
			 (get-marked-dired-files)
      (let* ((pathname (if (or marked-files marked-dirs)
			   ()
			   (dired-file-pathname
			    (array-element-from-mark
			     (current-point)
			     (dired-info-files
			      (value dired-information))))))
	     (files (fi pathname
			(string-trim
			 '(#\space)
			 (apply 'concatenate
				'simple-string
				(mapcar (lambda (car)
					  (format () "~A " (car car)))
					(append marked-files
						marked-dirs))))))
	     (command (prompt-for-string
		       :trim t
		       :prompt
		       (format () "Command to execute on ~A: "
			       (if pathname
				   (or (if (pathname-type pathname)
					   (format
					    () "~A.~A"
					    (pathname-name pathname)
					    (pathname-type pathname))
					   (pathname-name pathname))
				       (directory-namestring pathname))
				   "marked files"))
		       :help (format ()
				     "Shell command line to execute on the ~A."
				     (if pathname "file" "files"))
		       :history *shell-command-in-buffer-history*
		       :history-pointer
		       '*shell-command-in-buffer-history-pointer*)))
	(make-new-shell t ()
			(if (or (search "$A" command)
				(search "${A}" command))
			    ;; FIX \" correct? check w/ many files
			    (format () "export A=\"~A\"; ~A"
				    (or pathname files)
				    command)
			    (format () "~A ~A" command (or pathname
							   files)))
			:clear-buffer (value clear-shell-buffers)))))
  ;; FIX if rm file should update buffer?
  (maintain-dired-consistency))


;;;; Command to edit specific directories.

(defcommand "/" (p)
  "Edit the root of the file system."
  (dired-command p "/"))

(defcommand "Home" (p)
  "Edit the home of the current user."
  (dired-command p "home:"))

(defcommand "Desktop" (p)
  "Edit the Desktop directory of the current user."
  (dired-command p "home:Desktop/"))


;;;; Dired utilities.

;;; GET-DELETE-MARKED-DIRED-FILES returns as multiple values a list of file
;;; specs and a list of directory specs that have been marked for deletion.
;;; This assumes the current buffer is a "Dired" buffer.
;;;
(defun get-delete-marked-dired-files ()
  (let* ((files (dired-info-files (value dired-information)))
	 (length (length files))
	 (marked-files ())
	 (marked-dirs ())
	 (directory (dired-info-pathname (value dired-information))))
    (or files (editor-error "Not in Dired buffer."))
    (do ((i 0 (1+ i)))
	((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let* ((thing (svref files i))
	     (pathname (merge-pathnames (dired-file-pathname thing)
					directory)))
	(when (and (dired-file-deleted-p thing) ; file marked for delete
		   (or (symlinkp pathname)
		       (probe-file pathname))) 	; file still exists
	  (if (directoryp pathname)
	      (push (cons pathname (file-write-date pathname)) marked-dirs)
	      (push (cons pathname (file-write-date pathname
						    :check-for-links t))
		    marked-files)))))))

;;; GET-MARKED-DIRED-FILES returns as multiple values a list of file
;;; specs and a list of directory specs that have been *-marked.
;;; This assumes the current buffer is a "Dired" buffer.
;;;
(defun get-marked-dired-files ()
  (let* ((dired-info (value dired-information))
	 (files (dired-info-files dired-info))
	 (length (length files))
	 (marked-files ())
	 (marked-dirs ())
	 (directory (dired-info-pathname dired-info)))
    (do ((i 0 (1+ i)))
	((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let ((thing (svref files i)))
	;; The pathname can be false if the stat failed in print-directory.
	(when (dired-file-pathname thing)
	  (let ((pathname (merge-pathnames (dired-file-pathname thing) directory)))
	    (when (and (dired-file-marked-p thing) ; file marked
		       (nth-value 1 (probe-file pathname :check-for-links t))) ; file still exists
	      (if (directoryp pathname :check-for-links t)
		  (push (cons pathname (file-write-date pathname)) marked-dirs)
		  (push (cons pathname (file-write-date pathname
							:check-for-links t))
			marked-files)))))))))

;;; ARRAY-ELEMENT-FROM-MARK -- Internal Interface.
;;;
;;; This counts the lines between it and the beginning of the buffer.  The
;;; number is used to index vector as if each line mapped to an element
;;; starting with the zero'th element (lines are numbered starting at 1).
;;; This must use AREF since some modes use this with extendable vectors.
;;;
(defun array-element-from-mark (mark vector
				&optional (error-msg "Invalid line."))
  (if (blank-line-p (mark-line mark)) (editor-error error-msg))
  (aref vector
	(1- (count-lines (region
			  (buffer-start-mark (line-buffer (mark-line mark)))
			  mark)))))

;;; ARRAY-ELEMENT-FROM-POINTER-POS -- Internal Interface.
;;;
(defun array-element-from-pointer-pos (vector
				       &optional (error-msg "Invalid line."))
  (multiple-value-bind (x y window)
		       (last-key-event-cursorpos)
    (or x (editor-error "Failed to get cursor position."))
    (array-element-from-mark (or (cursorpos-to-mark x y window)
				 (editor-error
				  "Cursor position out of view."))
			     vector
			     error-msg)))

(defun dired-current-directory ()
  "Return the current directory."
  (let ((bpn (buffer-pathname (current-buffer))))
    (if bpn
	(directory-namestring bpn)
	(or (let ((res (current-directory)))
	      (if res (directorify bpn)))
	    (value pathname-defaults)))))


;;;; Locate.

(defun execute-locate (regex stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format () "locate \"~A\" 2>/dev/null" regex))
   :output stream))

(defun locate (regex)
  "Return the list of files in the locate database that match REGEX."
  ;; FIX (with-temp-buffer?
  (let* ((buf-name (format () "Locate Results ~a" regex))
	 (new-buffer (progn
		       (if (getstring buf-name *buffer-names*)
			   (kill-buffer-command () buf-name))
		       (make-buffer buf-name
				    :modes '("Fundamental"))))
	 (buffer (or new-buffer (error "Failed to create temp buffer.")))
	 (point (buffer-point buffer))
	 files)
    (with-writable-buffer (buffer)
      (with-output-to-mark (s point :full)
	(execute-locate regex s)))
    (do* ((line (mark-line (buffer-end-mark buffer)) (line-previous line))
	  (line-string (line-string line) (line-string line))
	  (first-line (mark-line (buffer-start-mark buffer))))
	 ((eq line first-line)
	  (setq files (if (string= line-string "")
			  (or files '())
			  (cons (line-string line) files))))
      (or (string= line-string "")
	  (setq files (cons line-string files))))
    (kill-buffer-command () buf-name)
    files))

(defcommand "Locate" ()
  "Prompt for a regex and edit the result of a locate on the regex.  If a
   dired of that search already exists, go to and update that buffer,
   otherwise create one."
  (let ((regex (prompt-for-string
		:prompt "Locate: "
		:help "Regular expression describing files to locate."
		:trim t)))
    (if (if (string= regex "")
	    (prompt-for-y-or-n
	     :prompt "Locate every file in the file system? "
:help "Y to confirm locate of every file (which usually takes a very long time)."
	     :default () :must-exist t :default-string "N")
	    t)
	(let* ((name (format () "Locate \"~A\"" regex))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(change-to-buffer buffer)
		(dired-update-buffer-command))
	      (let ((files (locate regex)))
		(if files
		    (dired-guts () () "/" () () () files name
				(eval `(lambda (info)
					 (declare (ignore info))
					 (locate ,regex))))
		    (message "Failed to locate any matching files."))))))))


;;;; Find files.

#|
(defun execute-find (pathname exp stream &optional recurse)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (concatenate 'simple-string
		      "find "
		      (namestring (truename pathname))
		      (if recurse "" " -maxdepth 1")
		      (if (string= exp "")
 			  " 2>/dev/null"
			  (format () " \\( ~A \\) 2>/dev/null" exp))))
   :output stream))

(defun find-files (pathname exp &optional recurse)
  "Return the list of files in PATHNAME that match EXP."
  ;; FIX (with-temp-buffer?
  (let* ((buf-name (format () "Find Results ~a" exp))
	 (new-buffer (progn
		       (if (getstring buf-name *buffer-names*)
			   (kill-buffer-command () buf-name))
		       (make-buffer buf-name
				    :modes '("Fundamental"))))
	 (buffer (or new-buffer (error "Failed to create temp buffer.")))
	 (point (buffer-point buffer))
	 (files ()))
    (with-writable-buffer (buffer)
      (with-output-to-mark (s point :full)
	(execute-find pathname exp s recurse)))
    ;; FIX Same as in Locate above.
    (do* ((line (mark-line (buffer-end-mark buffer)) (line-previous line))
	  (line-string (line-string line) (line-string line))
	  (first-line (mark-line (buffer-start-mark buffer))))
	 ((eq line first-line)
	  (setq files (if (string= line-string "")
			  (or files '())
			  (cons (line-string line) files))))
      (or (string= line-string "")
	  (setq files (cons line-string files))))
    (kill-buffer-command () buf-name)
    files))

(defun dired-find (&optional recurse)
  "Find files, dired the found files."
  (let* ((dpn (directory-namestring (or (buffer-pathname (current-buffer))
					(value pathname-defaults))))
	 (pathname (prompt-for-file
		    :prompt "Find in Directory: "
		    :help "Pathname in which to find files."
		    :default (make-pathname
			      :device (pathname-device dpn)
			      :directory (pathname-directory dpn))
		    :must-exist t))
	 (exp (prompt-for-string
	       :prompt "Find Expression: "
	       :help "Find expression describing files to find."
	       :trim t)))
    (if (if (string= exp "")
	    (prompt-for-y-or-n
	     :prompt (list "Find every file in ~A? " pathname)
:help "Confirm find of every file (can take long time for large dirs)."
	     :default () :must-exist t :default-string "N")
	    t)
	(let* ((name (format () "Dired find \"~A\" in ~A~A"
			     exp pathname
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(change-to-buffer buffer)
		(dired-update-buffer-command))
	      (let ((files (find-files pathname exp recurse)))
		(if files
		    (dired-guts () () "/" () () ()
				files name
				(eval `(lambda (info)
					 (declare (ignore info))
					 (find-files ,pathname
						     ,exp
						     ,recurse))))
		    (message "Failed to find any files."))))))))
|#

(defun dired-find (&optional recurse)
  "Find files, dired the found files."
  (let* ((dpn (directory-namestring (or (buffer-pathname (current-buffer))
					(value pathname-defaults))))
	 (pathname (prompt-for-file
		    :prompt "Find in Directory: "
		    :help "Pathname in which to find files."
		    :default (make-pathname
			      :device (pathname-device dpn)
			      :directory (pathname-directory dpn))
		    :must-exist t))
	 (exp (prompt-for-expression
	       :must-exist ()
	       :prompt "Expression: "
	       ;; FIX comes out in upper case
	       :default `(string= (pathname-name ,(read-from-string "file")) "")
	       :help "Expression with which to filter files.")))
    (if (if (equal exp "")
	    (prompt-for-y-or-n
	     :prompt (list "Find every file in ~A? " pathname)
:help "Confirm find of every file (can take a long time for large dirs)."
             :default () :must-exist t :default-string "N")
	    t)
	(let* ((name (format () "Dired find ~A in ~A~A"
			     exp pathname
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(change-to-buffer buffer)
		(dired-update-buffer-command))
	      (let* ((predicate
		      (eval (list 'lambda (list (read-from-string "file"))
				  (if (equal exp "")
				      `(declare (ignore ,(read-from-string "file")))
				      exp))))
		     (files (list-files pathname predicate :recurse recurse
					:follow-links t)))
		(if files
		    (dired-guts () () pathname () () ()
				files name
				(eval `(lambda (info)
					 (declare (ignore info))
					 (list-files ,pathname
						     ,predicate
						     :recurse ,recurse
						     :follow-links t))))
		    (message "Failed to find any files."))))))))

(defcommand "Dired Find" ()
  "Find files in a single directory, and dired the resulting files.  If a
   dired of such a search already exists then go to and update the existing
   dired."
  (dired-find))

(defcommand "Dired Find Recursively" ()
  "Find files in a directory tree, and dired the resulting files.  If a
   dired of such a search already exists then go to and update the existing
   dired."
  (dired-find t))


;;;; Search files.

(defun execute-search (pathname exp stream &optional print-match recurse backups)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (if recurse
	     (format ()
		     "cd ~A && find . ~A 2>/dev/null | xargs grep ~A -I -e \"~A\" 2>/dev/null"
		     (namestring (truename pathname))
		     (if backups "" " -not -name \\*.BAK -not -name \\*.CKP -not -name \\*~")
		     (if print-match "-Hn" "-l")
		     exp)
	     (format ()
		     "cd ~A && find . ~A -maxdepth 1 2>/dev/null | xargs grep ~A -I -e \"~A\" 2>/dev/null"
		     (namestring (truename pathname))
		     (if backups "" " -not -name \\*.BAK -not -name \\*.CKP -not -name \\*~")
		     (if print-match "-Hn" "-l")
		     exp
		     (namestring (truename pathname)))))
   :output stream))

(defun search-files (pathname string &optional recurse
		     display-matches backups (max-length 80))
  "Return the list of files in $pathname that match $string."
  (remove-duplicates
   (with-group (pathname)
	       (if display-matches
		   (let ((matches (search-group string recurse backups))
			 (longest-name 0)
			 (longest-number 0))
		     (dolist (match matches)
		       (let ((name (length (car match)))
			     (number (length (cadr match))))
			 (if (> name longest-name)
			     (setq longest-name name))
			 (if (> number longest-number)
			     (setq longest-number number))))
		     (let ((length (- max-length
				      (+ longest-name longest-number))))
		       (mapcar (lambda (file)
				 (let ((text (safe-subseq
					      (caddr file)
					      0 length)))
				   (list (car file)
					 (cadr file)
					 (map 'simple-string
					      (lambda (char)
						(if (or (standard-char-p char)
							(char= char #\tab)
							(char= char #\page))
						    char
						    #\.))
					      text))))
			       matches)))
		   (let (matches)
		     (dolist (item (search-group string recurse backups))
		       (pushnew (car item) matches))
		     matches)))))

(defun search-files-for-exp (pathname exp &optional recurse display-matches backups)
  "Return the list of files in PATHNAME that match EXP."
  ;; FIX (with-temp-buffer?
  (let* ((buf-name (format () "Search Results ~a" exp))
	 (new-buffer (progn
		       (if (getstring buf-name *buffer-names*)
			   (kill-buffer-command () buf-name))
		       (make-buffer buf-name
				    :modes '("Fundamental"))))
	 (buffer (or new-buffer (error "Failed to create temp buffer.")))
	 (point (buffer-point buffer))
	 (files))
    (with-writable-buffer (buffer)
      (with-output-to-mark (s point :full)
	(execute-search pathname exp s display-matches recurse backups)))
    ;; FIX use this style in others?
    (flet ((convert-line-string (line-string)
	     (or (string= line-string "")
		 (if display-matches
		     ;; FIX could use split
		     (let* ((sep1-start (search ":" line-string))
			    (sep2-start (if sep1-start
					    (search ":" line-string
						    :start2 (+ sep1-start 1)))))
		       (when (and sep1-start (> sep1-start 1) sep2-start)
			 (setq files
			       (cons (list (subseq line-string 2 sep1-start)
					   (subseq line-string
						   (+ sep1-start 1)
						   sep2-start)
					   (subseq line-string
						   (+ sep2-start 1)))
				     files))))
		     (setq files (cons line-string files))))))
      (do* ((line (mark-line (buffer-end-mark buffer)) (line-previous line))
	    (line-string (line-string line) (line-string line))
	    (first-line (mark-line (buffer-start-mark buffer))))
	   ((eq line first-line)
	    (convert-line-string line-string))
	(convert-line-string line-string)))
    (kill-buffer-command () buf-name)
    files))

#|  FIX start at full lisp version (too slow)

(defun search-files (pathname rule &optional recurse display-matches)
  "Return the list of files in PATHNAME that match RULE."
  (let ((*files*)
	(*pattern* (new-search-pattern :parser :forward rule)))
    (if display-matches
	;; File name, line number, line text.
; 		     (loop
; 		       for num = (find-pattern point *pattern*)
; 		       while num
; 		       do
; 		       (setq *files*
; 			     (append *files*
; 				     (list (line-string (mark-line point)))))
; 		       ;; FIX multiple elements in *files* for lines w multiple matches
; 		       (character-offset point num)))))
	(editor-error "FIX display-matches"))
    ;; FIX test (was map-files when last used)
    (do-files (file pathname
	       :recurse recurse
	       :follow-links t)
      ;; FIX may be better to use a stream for this case
      ;; (with-open-file
      (or (directoryp file)
	  (edi::with-temp-buffer (buffer file)
	    (message "search ~A (~A) for .~A. from ~A with ~A" buffer file rule (buffer-point buffer) *pattern*)
	    (if (find-pattern (buffer-point buffer) *pattern*)
		(setq *files* (append *files* (list file)))))))
    *files*))
|#

(defvar *last-search-buffer-stack* '()
  "Stack of search buffer.")

#|  FIX parser version
(defun dired-search-files (&optional recurse display-matches)
  "Search and dired a dir.  Recursively search subdirs if RECURSE is true."
  (let* ((dpn (directory-namestring (or (buffer-pathname (current-buffer))
					(value pathname-defaults))))
	 (pathname (prompt-for-file
		    :prompt "Search files in directory: "
		    :help "Directory in which to search files."
		    :default (make-pathname
			      :device (pathname-device dpn)
			      :directory (pathname-directory dpn))
		    :must-exist t))
; 	 (exp (prompt-for-string
; 	       :prompt "Search regexp: "
; 	       :help "Regular expression with which to search files."
; 	       :default (word-at-point)
; 	       :trim t)))
	 (exp (prompt-for-expression
	       :must-exist () :prompt "Search rule: "
	       :help "Parser rule with which to search files.")))
    (if (if (and (stringp exp) (string= exp ""))
	    (prompt-for-y-or-n
	     :prompt "Proceed with empty search expression? "
	     :default () :must-exist t :default-string "N")
	    t)
	(let* ((exp (list exp))
	       (name (format () "~ASearch ~A for \"~A\"~A"
			     (if display-matches "" "Dired ")
			     pathname exp
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(push buffer *last-search-buffer-stack*)
		(change-to-buffer buffer)
		(dired-update-buffer-command))
	      (let ((files (search-files pathname exp
					 recurse display-matches)))
		(if files
		    (push (dired-guts () () "/" ()
				      (if display-matches
					  '(:name ": " 1 ": " 2))
				      () files name
				      (eval `(lambda (info)
					       (declare (ignore info))
					       (search-files ,pathname
							     ,exp
							     ,recurse
							     ,display-matches))))
			  *last-search-buffer-stack*)
		    (message "Failed to match in any files."))))))))
|#

(defevar "Dired Search Fill Column"
  "The width of the text in a Search buffer.  If () then \"Fill
   Column\" will be used instead."
  :value ())

(defun dired-search-files (&optional recurse display-matches backups)
  "Search and dired a dir.  Recursively search subdirs if $recurse is true."
  (let* ((dpn (directory-namestring (or (buffer-pathname (current-buffer))
					(value pathname-defaults))))
	 (pathname (prompt-for-file
		    :prompt "Search files in directory: "
		    :help "Directory in which to search files."
		    :default (make-pathname
			      :device (pathname-device dpn)
			      :directory (pathname-directory dpn))
		    :must-exist t))
 	 (exp (prompt-for-string
 	       :prompt "Search string: "
 	       :help "String for which to search in files."
 	       :default (word-at-point)
	       ;:trim t
	       )))
    (if (if (and (stringp exp) (string= exp ""))
	    (prompt-for-y-or-n
	     :prompt "Proceed with empty search expression? "
	     :default () :must-exist t :default-string "N")
	    t)
	(let* ((name (format () "~ASearch ~A for \"~A\"~A~A"
			     (if display-matches "" "Dired ")
			     pathname exp
			     (if recurse " R" "")
			     (if backups " B" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(push buffer *last-search-buffer-stack*)
		(change-to-buffer buffer)
		(dired-update-buffer-command))
	      (multiple-value-bind (prefix suffix)
				   (common-prefix pathname)
		(let ((files (in-directory prefix
			       (search-files suffix exp recurse
					     display-matches backups
					     (- (window-width
						 (current-window))
						6)))))
		  (if files
		      (push (dired-guts
			     () () pathname ()
			     (if display-matches
				 '(:name ":" 1 ": " 2))
			     () files name
			     (eval `(lambda (info)
				      (declare (ignore info))
				      (search-files ,pathname
						    ,exp
						    ,recurse
						    ,display-matches
						    ,backups
						    (- (window-width
							(current-window))
						       6)))))
			    *last-search-buffer-stack*)
		      (message "Failed to match in any files.")))))))))

(defcommand "Dired Search" (p)
  "Search the files in a single directory for a regex, and dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired.  With an argument search also in
   backup files."
  "Search the files in a single directory for a regex, and dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired.  If P is true search also in backup
   files."
  (dired-search-files () () p))

(defcommand "Dired Search Recursively" (p)
  "Search the files in a directory tree for a regex, and Dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired.  With an argument search also in
   backup files."
  "Search the files in a directory tree for a regex, and Dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired.  If P is true search also in backup
   files."
  (dired-search-files t () p))

(defcommand "Search" (p)
  "Search the files in a single directory for a regex, and edit the
   matching lines.  If an edit of such a search already exists then go to
   the existing edit.  With an argument search also in backup files."
  "Search the files in a single directory for a regex, and dired the
   matching lines.  If an edit of such a search already exists then go to
   and update the existing edit.  If P is true search also in backup
   files."
  (dired-search-files () t p))

(defcommand "Search Recursively" (p)
  "Search the files in a directory tree for a regex, and edit the matching
   lines.  If an edit of such a search already exists then go to the
   existing edit.  With an argument search also in backup files."
  "Search the files in a directory tree for a regex, and dired the matching
   lines.  If an edit of such a search already exists then go to and update
   the existing edit.  If P is true search also in backup files."
  (dired-search-files t t p))

;; FIX Maybe this should be per-search (dired/normal and single/recursive).
;;     Add "Last Find" and "Next Find Match" too?
(defcommand "Last Search" ()
  "Switch to the most recently performed search, if there is one."
  (when *last-search-buffer-stack*
    (do ((buf (car *last-search-buffer-stack*) (car *last-search-buffer-stack*)))
	((if buf (memq (car *last-search-buffer-stack*) *buffer-list*) t)
	 (progn
	   (if buf
	       (let ((wins (buffer-windows buf)))
		 (if wins
		     (setf (current-buffer) buf  (current-window) (car wins))
		     (change-to-buffer buf))))
	   buf))
      (pop *last-search-buffer-stack*))))

(defcommand "Next Search Match" ()
  "Switch to the next match from the most recent search."
  (when (last-search-command)
    (let ((point (current-point)))
      (line-offset point 1)
      (if (blank-line-p (mark-line point))
	  (editor-error "Past last match."))
      (dired-edit-file-command t))))


;;;; Version control.

; (defmode "VC" :major-p ()
;   :documentation
;   "Version control \"sub-mode\" of Dired.")

(defun list-files-with-vc (dir-info &optional connect)
  (let* ((files) (max-version 0) (max-status 0)
	 (pathname (dired-info-pathname dir-info))
	 (all (dired-info-dot-files-p dir-info))
	 (backups (dired-info-backup-files-p dir-info))
	 (recurse (dired-info-recurse dir-info))
	 (root-len (length (namestring pathname))))
    (if connect (message "Connecting to VC server."))
    (do-files (file pathname
	       :follow-links t
	       :all all :backups backups
	       :recurse recurse)
      (let ((vc-info (make-vc-info file connect)))
	(if vc-info
	    (let ((version (or (vc-info-version vc-info) ""))
		  (status (string (or (vc-info-status vc-info) ""))))
	      (if (> (length version) max-version)
		  (setq max-version (length version)))
	      (if (> (length status) max-status)
		  (setq max-status (length status)))
	      ;; Empty string is dummy line number.
	      (push (list (subseq file root-len) ""
			  vc-info version status)
		    files))
	    ;; Empty strings: dummy line number, version, status.
	    (push (list (subseq file root-len) ""
			(make-new-vc-info) "" "")
		  files))))
    ;; Normalise string widths.
    (mapcar #'(lambda (list)
		(rplaca (nthcdr 3 list)
			(format ()
				(format () "~~~DA" max-version)
				(nth 3 list)))
		(rplaca (nthcdr 4 list)
			(format ()
				(format () "~~~DA" max-status)
				(nth 4 list))))
	    files)
    (nreverse files)))

(defcommand "Dired Toggle Version Control" ()
  "Toggles version control format."
  (or (editor-bound-p 'dired-information)
      (editor-error "Must be in Dired buffer."))
  (let* ((dir-info (value dired-information))
	 (generator (dired-info-generator dir-info)))
    (if (and generator (eq generator #'list-files-with-vc))
	(setf (dired-info-extra-data-2 dir-info)
	      (dired-info-extra-data dir-info)
	      (dired-info-extra-data dir-info) t
	      (dired-info-generator dir-info) ()
	      ;(buffer-minor-mode (current-buffer) "VC") ()
	      (dired-info-coldefs dir-info) ())
	(let ((generator #'list-files-with-vc))
	  (if (dired-info-extra-data-2 dir-info)
	      (setf (dired-info-extra-data dir-info)
		    (dired-info-extra-data-2 dir-info))
	      (setf (dired-info-extra-data dir-info)
		    (list-files-with-vc dir-info)))
	  (setf (dired-info-generator dir-info) generator)
	  (setf (dired-info-propagating-generator-p dir-info) t)
	  ;(setf (buffer-minor-mode (current-buffer) "VC") t)
	  (setf (dired-info-coldefs dir-info) '("  " 4 " " 3 " " :name))))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 (current-buffer))))

(defcommand "Dired VC Log Entry" ()
  "Show the log of the current file."
  (let ((pathname (dired-file-pathname
		   (array-element-from-mark
		    (current-point)
		    (dired-info-files (value dired-information))))))
    (vc-file-log-entry-command () pathname)))

(defcommand "Dired VC Compare File" ()
  "Compare the file at point with the repository version."
  (let ((pathname (dired-file-pathname
		   (array-element-from-mark
		    (current-point)
		    (dired-info-files (value dired-information))))))
    (vc-compare-file-command () pathname)))

(defcommand "Dired VC Update File" ()
  "Update marked files if any, otherwise the file at point."
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((dir-info (value dired-information))
	   (pathname (if (or marked-files marked-dirs)
			 (dired-info-pathname dir-info)
			 (dired-file-pathname
			  (array-element-from-mark
			   (current-point)
			   (dired-info-files dir-info)))))
	   (files (if (or marked-files marked-dirs)
		      (mapcar
		       (lambda (file) (car file))
		       (if marked-files
			   (if marked-dirs
			       (append marked-files marked-dirs)
			       marked-files)
			   marked-dirs)))))
      (vc-update-file-command () pathname files ())
      (setf (dired-info-extra-data dir-info)
	    (funcall (dired-info-generator dir-info) dir-info))
      (update-dired-buffer (dired-info-pathname dir-info)
			   (dired-info-pattern dir-info)
			   (current-buffer))
      ;; FIX update other vc eds of this dir too
      (maintain-dired-consistency))))

(declaim (special *cvs-update-file-recurse*))

(defcommand "Dired VC Update Directory" ()
  "Update all the files listed in the current buffer, recursively."
  (let* ((dir-info (value dired-information))
	 (pathname (dired-info-pathname dir-info))
	 (*cvs-update-file-recurse* t))
    (vc-update-file-command () (namestring pathname) () ())
    (setf (dired-info-extra-data dir-info)
	  (funcall (dired-info-generator dir-info) dir-info))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 (current-buffer))
    ;; FIX update other vc eds of this dir too
    (maintain-dired-consistency)))

(defcommand "Dired VC Commit File" ()
  "Commit marked files if any, otherwise the file at point."
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((dir-info (value dired-information))
	   (pathname (if (or marked-files marked-dirs)
			 (dired-info-pathname dir-info)
			 (dired-file-pathname
			  (array-element-from-mark
			   (current-point)
			   (dired-info-files dir-info)))))
	   (files (if (or marked-files marked-dirs)
		      (mapcar
		       (lambda (file) (car file))
		       (if marked-files
			   (if marked-dirs
			       (append marked-files marked-dirs)
			       marked-files)
			   marked-dirs)))))
      (vc-commit-file-command () pathname files)
      (setf (dired-info-extra-data dir-info)
	    (funcall (dired-info-generator dir-info) dir-info))
      (update-dired-buffer (dired-info-pathname dir-info)
			   (dired-info-pattern dir-info)
			   (current-buffer))
      ;; FIX update other vc eds of this dir too
      (maintain-dired-consistency))))


;;;; WWW.

(defcommand "Dired WWW File" ()
  "Browse the file at point."
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((dir-info (value dired-information))
	   (files (if (or marked-files marked-dirs)
		      (append marked-files marked-dirs)
		      (list (dired-file-pathname
			     (array-element-from-mark
			      (current-point)
			      (dired-info-files dir-info))))))
	   (file (merge-pathnames (car files) (dired-info-pathname dir-info))))
      ;; FIX also browse rest
      (message "~A" file)
      (www-command () file))))

(defcommand "Dired WWW File Externally" ()
  "Browse the file at point in the external browser."
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((dir-info (value dired-information))
	   (files (if (or marked-files marked-dirs)
		      (append marked-files marked-dirs)
		      (list (dired-file-pathname
			     (array-element-from-mark
			      (current-point)
			      (dired-info-files dir-info))))))
	   (file (merge-pathnames (car files) (dired-info-pathname dir-info))))
      ;; FIX also browse rest
      (let ((file (format () "file://~A" (namestring file))))
	(message "~A" file)
	(view-url file)))))


#[ View Mode

`View' mode provides for scrolling through a file read-only, terminating
the buffer upon reaching the end.

{command:View File}
{command:View Help}
{command:View Edit File}
{command:View Scroll Down}
{evariable:View Scroll Deleting Buffer}
{command:View Return}
{command:View Quit}

Also, bound in `View' mode are the following commands:

  backspace, delete
     Scrolls the window up.

  <
     Goes to the beginning of the buffer.

  >
     Goes to the end of the buffer.
]#

#[ Using View Mode

`View' mode supports scrolling through files automatically terminating the
buffer at end-of-file as well as commands for quitting the mode and popping
back to the buffer that spawned the `View' mode buffer.  Modes such as
`Dired' use this to view files and description of library entries.

Modes that want similar commands should use view-file-command to view a
file and get a handle on the view buffer.  To allow the `View Return' and
`View Quit' commands to return to the originating buffer, you must set the
variable `View Return Function' in the viewing buffer to a function that
knows how to do this.  Furthermore, since you now have a reference to the
originating buffer, you must add a buffer local delete hook to it that will
clear the view return function's reference.  This needs to happen for two
reasons in case the user deletes the originating buffer:

You don't want the return function to go to a non-existing, invalid buffer.

Since the viewing buffer still exists, its `View Return Function' buffer
local variable still exists.  This means the function still references the
deleted originating buffer, and garbage collection cannot reclaim the memory
locked down by the deleted buffer.

The following is a piece of code that could implement part of `Dired View
File' that uses two closures to accomplish that described above:

    (let* ((dired-buf (current-buffer))
	   (buffer (view-file-command () pathname)))
      (push #'(lambda (buffer)
		(declare (ignore buffer))
		(setf dired-buf ()))
	    (buffer-delete-hook dired-buf))
      (setf (variable-value 'view-return-function :buffer buffer)
	    #'(lambda ()
		(if dired-buf
		    (change-to-buffer dired-buf)
		    (dired-from-buffer-pathname-command)))))

The `Dired' buffer's delete hook clears the return function's reference to
the `Dired' buffer.  The return function tests the variable to see if it
still holds a buffer when the function executes.
]#


;;;; View Mode.

(defmode "View" :major-p ()
  :setup-function 'setup-view-mode
  :cleanup-function 'cleanup-view-mode
  :precedence 5.0
  :documentation
  "View mode scrolls forwards and backwards in a file with the buffer read-only.
   Scrolling off the end optionally deletes the buffer.")

(defun setup-view-mode (buffer)
  (defevar "View Return Function"
    "Function that gets called when quitting or returning from view mode."
    :value ()
    :buffer buffer)
  (setf (buffer-writable buffer) ()))
;;;
(defun cleanup-view-mode (buffer)
  (delete-variable 'view-return-function :buffer buffer)
  (setf (buffer-writable buffer) t))

(defcommand "View File" (p pathname)
  "Read a file into a buffer in `View Mode' as if by `Visit File' and set
   the buffer read-only."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file
		  :prompt "View File: " :must-exist t
		  :help "Name of existing file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (make-buffer (format () "View File ~A" (gensym)))))
    (visit-file-command () pn buffer)
    (setf (buffer-minor-mode buffer "View") t)
    (change-to-buffer buffer)
    buffer))

(defcommand "View Return" ()
  "Return to the buffer that created the current buffer in `View' mode."
  (fi (call-view-return-fun)
      (editor-error "No View return method for this buffer.")))

(defcommand "View Quit" ()
  "Return to the buffer that created the current buffer in `View' mode.

   After invoking the viewing return function, if there is one, delete the
   buffer that was current initially."
  (let* ((buf (current-buffer))
	 (funp (call-view-return-fun)))
    (delete-buffer-if-possible buf)
    (fi funp (editor-error "No View return method for this buffer."))))

(defun call-view-return-fun ()
  "If there is a current view-return-function call it and return t,
   otherwise return ()."
  (if (editor-bound-p 'view-return-function)
      (let ((fun (value view-return-function)))
	(when fun
	  (funcall fun)
	  t))))

(defevar "View Scroll Deleting Buffer"
  "When set, \"View Scroll Down\" deletes the buffer when the end of the
   file is visible.")

(defcommand "View Scroll Down" (p)
  "Scroll the current window down through its buffer.  If the end of the
   file is visible, then delete the buffer if `View Scroll Deleting Buffer'
   is set.  If the buffer is associated with a dired buffer, then return
   there instead of to the previous buffer."
  (if (and (not p)
	   (displayed-p (buffer-end-mark (current-buffer))
			(current-window))
	   (value view-scroll-deleting-buffer))
      (view-quit-command)
      (scroll-window-down-command p)))

(defcommand "View Edit File" ()
  "Make a `View' mode buffer a normal editing buffer, warning if the file
   exists in another buffer simultaneously."
  (let ((buf (current-buffer)))
    (setf (buffer-minor-mode buf "View") ())
    (warn-about-visit-file-buffers buf)))

(defcommand "View Help" ()
  "Show the \"View\" mode help message."
  (describe-mode-command () "View"))


;;;; Highlighting.

(defun highlight-dired-line (line chi-info)
  (or (plusp (line-length line))
      (editor-bound-p 'dired-information)
      (return-from highlight-dired-line))
  (let ((dired-info (value dired-information)))
    (and dired-info
	 (dired-info-files dired-info)
	 (in-directory (dired-current-directory) ; FIX
	   (with-literal-pathnames
	     (let* ((col-posns (dired-info-col-posns dired-info))
		    (file (array-element-from-mark
			   (mark line 0)
			   (dired-info-files dired-info))))
	       (and file
		    col-posns
		    (progn
		      ;; FIX dirp,symp cause remote stats
		      ;;       maybe cache file mode after call-p-d
		      (if (directoryp (dired-file-pathname file))
			  (if (assoc :name col-posns)
			      (chi-mark line
					(+ (cdr (assoc :name col-posns))
					   2)
					*special-form-font*
					:special-form
					chi-info)))
		      (if (symlinkp (dired-file-pathname file))
			  (if (assoc :name col-posns)
			      (let ((exists
				     (probe-file
				      (dired-file-pathname file))))
				(chi-mark line
					  (+ (cdr (assoc :name
							 col-posns))
					     (length
					      (dired-file-pathname
					       file))
					     3)
					  (if exists
					      *preprocessor-font*
					      *comment-font*)
					  (if exists
					      :preprocessor
					      :comment)
					  chi-info))))))))))))

(defun highlight-dired-buffer (buffer)
  (highlight-chi-buffer buffer highlight-dired-line))

(defun highlight-visible-dired-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-dired-line))
