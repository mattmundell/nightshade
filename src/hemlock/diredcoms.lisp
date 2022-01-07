;;; -*- Log: hemlock.log; Package: Hemlock; Mode: Editor -*-
;;;
;;; Simple directory editing support.
;;; This file contains site dependent calls.

(in-package "HEMLOCK")


(defmode "Dired" :major-p t
  :documentation
  "Dired permits convenient directory browsing and file operations including
   viewing, deleting, copying, renaming, and wildcard specifications.")


(defstruct (dired-information (:print-function print-dired-information)
			      (:conc-name dired-info-))
  pathname	   ; Pathname of directory.
  pattern	   ; FILE-NAMESTRING with wildcard possibly.
  dot-files-p      ; Whether to include UNIX dot files.
  backup-files-p   ; Whether to include UNIX backup files.
  coldefs	   ; Description of columns to display, nil for the usual format.
  recurse          ; Whether to recurse into subdirectories.
  write-date	   ; Write date of directory.
  files		   ; Simple-vector of dired-file structures.
  file-list	   ; List of pathnames for files, excluding directories.
  generator        ; Function to generate files.  Called on a dir-info.
  extra-data)      ; Optn. per-file data: '(file*) or '((file line extra*)*).

(defun print-dired-information (obj str n)
  (declare (ignore n))
  (format str "#<Dired Info ~S>" (namestring (dired-info-pathname obj))))


(defstruct (dired-file (:print-function print-dired-file)
		       (:constructor make-dired-file (pathname)))
  pathname
  (deleted-p nil)
  (marked-p nil)
  (write-date nil))

(defun print-dired-file (obj str n)
  (declare (ignore n))
  (format str "#<Dired-file ~A>" (namestring (dired-file-pathname obj))))



;;;; "Dired" command.

(defhvar "Create Empty Dired Buffers"
  "If true then Dired will enter empty directories."
  :value nil)

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

;; FIX convert these to keys?
(defun dired-guts (patternp dot-files-p directory
		   &optional backup-files-p coldefs recurse-p extra-data
		             buffer-name generator)
  (let* ((dpn (directory-namestring (or (buffer-pathname (current-buffer))
					(value pathname-defaults))))
	 (directory (dired-directorify
		     (or directory
			 (prompt-for-file
			  :prompt (if recurse-p
				      "Edit Directory Recursively: "
				      "Edit Directory: ")
			  :help "Pathname to edit."
			  :default (make-pathname
				    :device (pathname-device dpn)
				    :directory (pathname-directory dpn))
			  :must-exist t))))
	 (pattern (if patternp
		      (prompt-for-string
		       :prompt "Filename pattern: "
		       :help "Type a filename with a single asterisk."
		       :trim t)))
	 (full-name (or buffer-name
			(namestring (if pattern
					(merge-pathnames directory pattern)
					directory))))
	 (name (or buffer-name
		   (concatenate 'simple-string "Dired " full-name)))
	 (buffer (cdr (assoc full-name *pathnames-to-dired-buffers*
			     :test #'string=))))
    (declare (simple-string full-name))
    ;; FIX pathname-defaults is a global var?
    (setf (value pathname-defaults) (merge-pathnames directory dpn))
    (change-to-buffer
     (cond (buffer
	    (let ((dir-info (variable-value 'dired-information
					    :buffer buffer))
		  (update nil))
	      ;; FIX clean this up?
	      (if dot-files-p
		  (or (dired-info-dot-files-p dir-info)
		      (setf (dired-info-dot-files-p dir-info) t
			    update t)))
	      (if backup-files-p
		  (or (dired-info-backup-files-p dir-info)
		      (setf (dired-info-backup-files-p dir-info) t
			    update t)))
	      (if coldefs
		  (or (dired-info-coldefs dir-info)
		      (setf (dired-info-coldefs dir-info) coldefs
			    update t)))
	      (if recurse-p
		  (or (dired-info-recurse dir-info)
		      (setf (dired-info-recurse dir-info) t
			    update t)))
	      (if extra-data
		  (or (dired-info-extra-data dir-info)
		      (setf (dired-info-extra-data dir-info) extra-data
			    update t)))
	      (if update
		  (update-dired-buffer directory pattern buffer)))
	    buffer)
	   (t
	    (let ((buffer (make-buffer
			   name :modes '("Dired")
			   :modeline-fields
			   (append (value default-modeline-fields)
				   (list (modeline-field :dired-cmds)))
			   :delete-hook (list 'dired-buffer-delete-hook))))
	      (unless (initialize-dired-buffer directory pattern
					       dot-files-p backup-files-p
					       coldefs recurse-p extra-data
					       buffer generator)
		(delete-buffer-if-possible buffer)
		(editor-error "No entries for ~A." full-name))
	      (push (cons full-name buffer) *pathnames-to-dired-buffers*)
	      buffer))))))

(defcommand "Dired" (p &optional directory)
  "Prompts for a directory and edits it.  If a dired for that directory
   already exists, goes to that buffer, otherwise creates one.  With an
   argument, includes UNIX dot files."
  "Prompts for a directory and edits it.  If a dired for that directory
   already exists, go to that buffer, otherwise create one.  With an
   argument, include UNIX dot files."
  (let ((info (if (hemlock-bound-p 'dired-information)
		  (value dired-information))))
    (dired-guts nil
		;; Propagate dot-files property to subdirectory edits.
		(or (and info (dired-info-dot-files-p info))
		    p)
		directory
		;; Propagate backup-files property to subdirectory edits.
		(or (and info (dired-info-backup-files-p info))
		    nil)
		;; Propagate column definitions to subdirectory edits.
		(or (and info (dired-info-coldefs info))
		    nil)
		;; Propagate recurse property to subdirectory edits.
		(or (and info (dired-info-recurse info))
		    nil)
		t)))

(defcommand "Dired with Pattern" (p)
  "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dot files."
  "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dot files."
  (dired-guts t p nil t #| FIX should be according to hvar |#
	      nil nil t))

;; FIX ~ should propogate recurse only: on update, and via a special key
(defcommand "Dired Recursively" (p)
  "Edit a recursively directory listing."
  "Edit a recursively directory listing."
  (let ((info (if (hemlock-bound-p 'dired-information)
		  (value dired-information))))
    (dired-guts nil
		;; Propagate dot-files property to subdirectory edits.
		(or (and info (dired-info-dot-files-p info))
		    p)
		nil
		;; Propagate backup-files property to subdirectory edits.
		(or (and info (dired-info-backup-files-p info))
		    nil)
		;; Propagate column definitions to subdirectory edits.
		(or (and info (dired-info-coldefs info))
		    nil)
		;; Propagate recurse property to subdirectory edits.
		(or (and info (dired-info-recurse info))
		    t)
		t)))

;;; INITIALIZE-DIRED-BUFFER gets a dired in the buffer and defines some
;;; variables to make it usable as a dired buffer.  If there are files
;;; satisfying directory or CREATE-EMPTY-DIRED-BUFFERS is true, then this
;;; returns t, otherwise nil.
;;;
(defun initialize-dired-buffer (directory pattern
			        dot-files-p backup-files-p coldefs recurse
				extra-data buffer generator)
  (multiple-value-bind (pathnames dired-files)
		       (dired-in-buffer directory pattern
					dot-files-p backup-files-p
					coldefs recurse extra-data
					buffer)
    (or (> (length dired-files) 0)
	(unix:unix-access (unix-namestring directory) unix:r_ok)
	(loud-message "Read access to ~A withheld." directory))
    (if (or (> (length dired-files) 0)
	    (value create-empty-dired-buffers))
	(defhvar "Dired Information"
	  "Contains the information necessary to manipulate dired buffers."
	  :buffer buffer
	  :value (make-dired-information :pathname directory
					 :pattern pattern
					 :dot-files-p dot-files-p
					 :backup-files-p backup-files-p
					 :coldefs coldefs
					 :recurse recurse
					 :extra-data extra-data
					 :write-date (file-write-date directory)
					 :files dired-files
					 :file-list pathnames
					 :generator generator)))))

;;; CALL-PRINT-DIRECTORY gives us a nice way to report PRINT-DIRECTORY errors
;;; to the user and to clean up the dired buffer.
;;;
(defun call-print-directory (directory mark dot-files-p backup-files-p
			     coldefs recurse extra-data)
  (handler-case (with-output-to-mark (s mark :full)
		  (if (eq extra-data t)
		      (print-directory directory s
				       :all dot-files-p :verbose t :return-list t
				       :backups backup-files-p :coldefs coldefs
				       :recurse recurse)
		      (print-files extra-data s
				   :verbose t :return-list t :coldefs coldefs)))
    (error (condx)
	   (delete-buffer-if-possible (line-buffer (mark-line mark)))
	   (editor-error "~A" condx))))

;;; DIRED-BUFFER-DELETE-HOOK is called on dired buffers upon deletion.  This
;;; removes the buffer from the pathnames mapping, and it deletes and buffer
;;; local variables referring to it.
;;;
(defun dired-buffer-delete-hook (buffer)
  (setf *pathnames-to-dired-buffers*
	(delete buffer *pathnames-to-dired-buffers* :test #'eq :key #'cdr)))



;;;; Dired deletion and undeletion.

(defcommand "Dired Delete File" (p)
  "Marks a file for deletion; signals an error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be flagged
   for deletion."
  "Marks a file for deletion; signals an error if not in a dired buffer."
  (dired-frob-deletion p t))

(defcommand "Dired Undelete File" (p)
  "Removes a mark for deletion; signals an error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be unflagged
   for deletion."
  "Removes a mark for deletion; signals an error if not in a dired buffer."
  (dired-frob-deletion p nil))

(defcommand "Dired Delete File and Down Line" (p)
  "Marks file for deletion and moves down a line.
   See \"Dired Delete File\"."
  "Marks file for deletion and moves down a line.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion nil t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File and Down Line" (p)
  "Marks file undeleted and moves down a line.
   See \"Dired Delete File\"."
  "Marks file undeleted and moves down a line.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion nil nil)
  (dired-down-line (current-point)))

(defcommand "Dired Delete File with Pattern" (p)
  "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\"."
  "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion t t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File with Pattern" (p)
  "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\"."
  "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion t nil)
  (dired-down-line (current-point)))

;;; DIRED-FROB-DELETION takes arguments indicating whether to prompt for a
;;; pattern and whether to mark the file deleted or undeleted.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER, and if not in a dired buffer, signals
;;; an error.
;;;
(defun dired-frob-deletion (patternp deletep)
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (with-mark ((mark (current-point) :left-inserting))
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
		      (file-write-date (dired-file-pathname dired-file)))
		(setf (dired-file-write-date dired-file) nil))
	    (setf (next-character mark) note-char)))))))

(defun dired-down-line (point)
  (line-offset point 1)
  (when (blank-line-p (mark-line point))
    (line-offset point -1)))


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
  (dired-frob-mark p nil))

(defcommand "Dired Mark File and Down Line" (p)
  "In a Dired buffer marks the current file and moves down a line.
   With an argument, prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be marked."
  "In a Dired buffer marks the current file and moves down a line."
  (declare (ignore p))
  (dired-frob-mark nil t)
  (dired-down-line (current-point)))

(defcommand "Dired Clear File Mark and Down Line" (p)
  "Clears any generic mark from the current file, and moves down a line."
  "Clears any generic mark from the current file, and moves down a line."
  (declare (ignore p))
  (dired-frob-mark nil nil)
  (dired-down-line (current-point)))

(defcommand "Dired Mark File with Pattern" (p)
  "Prompts for a pattern and marks matching files."
  "Prompts for a pattern and marks matching files for deletion."
  (declare (ignore p))
  (dired-frob-mark t t)
  (dired-down-line (current-point)))

(defcommand "Dired Clear File Mark with Pattern" (p)
  "Prompts for a pattern and clears any generic marks from matching files."
  "Prompts for a pattern and clears any generic marks from matching files."
  (declare (ignore p))
  (dired-frob-mark t nil)
  (dired-down-line (current-point)))

(defcommand "Dired Toggle Marks" (p)
  "Toggles any generic marks in a Dired buffer.
   Clears marks from marked files, marks the rest."
  "Toggles any generic marks in a Dired buffer.
   Clears marks from marked files, marks the rest."
  (declare (ignore p))
  (unless (hemlock-bound-p 'dired-information)
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
	      (progn
		(setf (dired-file-marked-p file) nil)
		(setf (next-character mark) #\ ))
	      (progn
		(setf (dired-file-marked-p file) t)
		(setf (next-character mark) #\*))))
	(line-offset mark 1 1)))))

;;; DIRED-FROB-MARK takes arguments indicating whether to prompt for a
;;; pattern and whether to add or clear the file mark.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER.  The buffer must be a Dired buffer,
;;; else an error is signalled.
;;;
(defun dired-frob-mark (patternp markp)
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Must be in a Dired buffer."))
  (with-mark ((mark (current-point) :left-inserting))
    (let* ((dir-info (value dired-information))
	   (files (dired-info-files dir-info))
	   (mark-files
	    (if patternp
		(dired:pathnames-from-pattern
		 (prompt-for-string
		  :prompt "Filename pattern: "
		  :help "Type a filename with a single asterisk."
		  :trim t)
		 (dired-info-file-list dir-info))
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


;;;; Both types of Dired marks.

(defcommand "Dired Clear File Marks" (p)
  "Clears any marks from the current file in a Dired buffer.
   With an argument, prompts for a pattern that may contain at most one
   wildcard, an asterisk, and any marks on the names matching the pattern
   will be cleared."
  "Clears any mark from the current file in a Dired buffer."
  (dired-frob-deletion p nil)
  (dired-frob-mark p nil))

(defcommand "Dired Clear File Marks and Down Line" (p)
  "Clears any marks from the current file in a Dired buffer, and moves down
   a line."
  "Clears any mark from the current file, and moves down a line."
  (declare (ignore p))
  (dired-frob-deletion nil nil)
  (dired-frob-mark nil nil)
  (dired-down-line (current-point)))

(defcommand "Dired Clear File Marks with Pattern" (p)
  "Prompts for a pattern and clears any marks from matching files."
  "Prompts for a pattern and clears any generic marks from matching files."
  (declare (ignore p))
  (dired-frob-mark t nil)
  (dired-frob-deletion t nil)
  (dired-down-line (current-point)))

(defcommand "Dired Clear All Marks" (p)
  "Clears any generic marks and any delete marks from a Dired buffer.  With
   a positive prefix clears only generic (*) marks; with a negative prefix
   clears only delete marks."
  "Clears any generic marks and any delete marks from a Dired buffer.  If P
   is positive clears only generic (*) marks; if P is negative clears only
   delete marks."
  (unless (hemlock-bound-p 'dired-information)
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
	    (setf (dired-file-marked-p file) nil)
	    (setf (next-character mark) #\ ))
	  (when (if p (minusp p) t)
	    (setf (dired-file-deleted-p file) nil)
	    (setf (previous-character mark) #\ )))
	(line-offset mark 1 1)))))


;;;; Dired file finding and going to dired buffers.

(defcommand "Dired Edit File" (p)
  "Read in file or recursively \"Dired\" a directory.  With a prefix
   argument edit the file in the next window, splitting the window if
   necessary."
  "Read in file or recursively \"Dired\" a directory.  If P is true edit
   the file in the next window, splitting the window if necessary."
  (let ((point (current-point)))
    (when (blank-line-p (mark-line point))
      (editor-error "Point must be on a file line."))
    (let* ((dired-info (value dired-information))
	   (pathname (dired-file-pathname
		      (array-element-from-mark point
					       (dired-info-files dired-info)))))
      (if p
	  (if (eq (next-window (current-window)) (current-window))
	      (split-window-command nil)
	      (next-window-command nil)))
      (if (directoryp pathname)
	  (dired-command nil (directory-namestring pathname))
	  (let ((extra-data (dired-info-extra-data dired-info)))
	    (change-to-buffer (find-file-buffer pathname))
	    (if (and extra-data
		     (consp extra-data)
		     (listp (car extra-data)))
		(let ((line-number (cadr (assoc pathname extra-data))))
		  (if line-number
		      (or (string= line-number "")
			  (goto-absolute-line-command (read-from-string
						       line-number)))))))))))

(defcommand "Dired View File" (p)
  "Read in file as if by \"View File\" or recursively \"Dired\" a directory.
   This associates the file's buffer with the dired buffer."
  "Read in file as if by \"View File\".
   This associates the file's buffer with the dired buffer."
  (declare (ignore p))
  (let ((point (current-point)))
    (if (blank-line-p (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
		     (array-element-from-mark
		      point (dired-info-files (value dired-information))))))
      (if (directoryp pathname)
	  (dired-command nil (directory-namestring pathname))
	  (let* ((dired-buf (current-buffer))
		 (trial-pathname (or (probe-file pathname)
				     (merge-pathnames pathname
						      (default-directory))))
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
				   (revert-file-command nil))
			       found)
			     (view-file-command nil pathname))))
	    (push #'(lambda (buffer)
		      (declare (ignore buffer))
		      (setf dired-buf nil))
		  (buffer-delete-hook dired-buf))
	    (setf (variable-value 'view-return-function :buffer buffer)
		  #'(lambda ()
		      (if dired-buf
			  (change-to-buffer dired-buf)
			  (dired-from-buffer-pathname-command nil)))))))))

(defcommand "Dired from Buffer Pathname" (p)
  "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompts for a file pattern within that directory."
  "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompts for a file pattern within that directory."
  (let ((pathname (buffer-pathname (current-buffer))))
    (or pathname
	(editor-error "No pathname associated with buffer."))
    (dired-command p (directory-namestring pathname))
    (do* ((pos 0 (incf pos))
	  (files (dired-info-files (value dired-information)))
	  (end (length files)))
	 ((eq pos end))
      (when (equal pathname (dired-file-pathname (aref files pos)))
	(line-offset (current-point) pos)
	(return-from dired-from-buffer-pathname-command)))))

(defcommand "Dired Up Directory" (p)
  "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer."
  "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((dirs (or (pathname-directory
		   (dired-info-pathname (value dired-information)))
		  '(:relative))))
    (dired-command nil
		   (truename (make-pathname :directory (nconc dirs '(:UP)))))))


;;;; Dired display control commands.

(defcommand "Dired Toggle Backups" (p)
  "Toggle display of backup files, updating buffer."
  "Toggle display of backup files, updating buffer."
  (declare (ignore p))
  (or (hemlock-bound-p 'dired-information)
      (editor-error "Must be in Dired buffer."))
  ;; FIX (decf? ie (- universe universe)
  (hi::complf (dired-info-backup-files-p (value dired-information)))
  (dired-update-buffer-command nil t))

(defcommand "Dired Toggle Hidden Files" (p)
  "Toggle display of files starting with `.', updating buffer."
  "Toggle display of files starting with `.', updating buffer."
  (declare (ignore p))
  (or (hemlock-bound-p 'dired-information)
      (editor-error "Must be in Dired buffer."))
  ;; FIX
  (hi::complf (dired-info-dot-files-p (value dired-information)))
  (dired-update-buffer-command nil t))



;;;; Dired misc. commands -- update, help, line motion.

(defcommand "Dired Update Buffer" (p &optional regenerate)
  "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified."
  "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified."
  (declare (ignore p))
  (or (hemlock-bound-p 'dired-information)
      (editor-error "Must be in Dired buffer."))
  (let* ((dir-info (value dired-information))
	 (generator (dired-info-generator dir-info)))
    (when (and generator
	       (or regenerate
		   (prompt-for-y-or-n :prompt "Regenerate listing? "
  :help "Y to rerun the processing that produced the listed files."
                                      :must-exist t
				      :default nil
				      :default-string "N")))
      (setf (dired-info-extra-data dir-info) (funcall generator dir-info)))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 (current-buffer))))
  
;;; UPDATE-DIRED-BUFFER updates buffer with a dired of directory, deleting
;;; whatever is in the buffer already.  This assumes buffer was previously
;;; used as a dired buffer having necessary variables bound.  The new files
;;; are compared to the old ones propagating any deleted flags if the name
;;; and the write date is the same for both specifications.
;;;
(defun update-dired-buffer (directory pattern buffer)
  (let* ((dir-info (variable-value 'dired-information :buffer buffer))
	 (point (buffer-point buffer))
	 (current-file (if (blank-line-p (mark-line point))
			     nil
			     (dired-file-pathname
			      (array-element-from-mark
			       point
			       (dired-info-files dir-info))))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (multiple-value-bind (pathnames new-dired-files)
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
					(dired-file-pathname new-file))))
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
	  (setf (dired-info-write-date dir-info) (file-write-date directory))
	  (move-mark point (buffer-start-mark buffer))
	  (let ((pos (position current-file
			       new-dired-files :test #'equal
			       :key #'dired-file-pathname)))
	    (if pos (line-offset point pos))))))))

;;; DIRED-IN-BUFFER inserts a dired listing of directory in buffer returning
;;; two values: a list of pathnames of files only, and an array of dired-file
;;; structures.  This uses FILTER-REGION to insert a space for the indication
;;; of whether the file is flagged for deletion.  Then we clean up extra header
;;; and trailing lines known to be in the output (into every code a little
;;; slime must fall).
;;;
(defun dired-in-buffer (directory pattern dot-files-p backup-files-p coldefs
			recurse extra-data buffer)
  (let ((point (buffer-point buffer)))
    (with-writable-buffer (buffer)
      (let* ((pathnames (call-print-directory
			 (if pattern
			     (merge-pathnames directory pattern)
			     directory)
			 point
			 dot-files-p
			 backup-files-p
			 coldefs
			 recurse
			 extra-data))
	     (dired-files (make-array (length pathnames))))
	(declare (list pathnames) (simple-vector dired-files))
	(filter-region #'(lambda (str)
			   (concatenate 'simple-string "  " str))
		       (buffer-region buffer))
	(delete-characters point -2)
	(delete-region (line-to-region (mark-line (buffer-start point))))
	(delete-characters point)
	(do ((p pathnames (cdr p))
	     (i 0 (1+ i)))
	    ((null p))
	  (setf (svref dired-files i) (make-dired-file (car p))))
	(values (delete-if #'directoryp pathnames) dired-files)))))


(defcommand "Dired Help" (p)
  "How to use dired."
  "How to use dired."
  (declare (ignore p))
  (describe-mode-command nil "Dired"))

(defcommand "Dired Next File" (p)
  "Moves to next undeleted file."
  "Moves to next undeleted file."
  (unless (dired-line-offset (current-point) (or p 1))
    (editor-error "Not enough lines.")))

(defcommand "Dired Previous File" (p)
  "Moves to previous undeleted file."
  "Moves to next undeleted file."
  (unless (dired-line-offset (current-point) (or p -1))
    (editor-error "Not enough lines.")))

;;; DIRED-LINE-OFFSET moves mark n undeleted file lines, returning mark.  If
;;; there are not enough lines, mark remains unmoved, this returns nil.
;;;
(defun dired-line-offset (mark n)
  (with-mark ((m mark))
    (let ((step (if (plusp n) 1 -1)))
      (dotimes (i (abs n) (move-mark mark m))
	(loop
	  (unless (line-offset m step 0)
	    (return-from dired-line-offset nil))
	  (when (blank-line-p (mark-line m))
	    (return-from dired-line-offset nil))
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



;;;; Dired expunging and quitting.

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
	  (or (buffer-modified buffer)
	      (delete-buffer buffer)))))))

(defcommand "Dired Expunge Files" (p)
  "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\"."
  "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\"."
  (declare (ignore p))
  (when (expunge-dired-files)
    (dired-update-buffer-command nil))
  (maintain-dired-consistency))

(defcommand "Dired Quit" (p)
  "Expunges the files in a dired buffer and then exits."
  "Expunges the files in a dired buffer and then exits."
  (declare (ignore p))
  (expunge-dired-files)
  (delete-buffer-if-possible (current-buffer)))

(defhvar "Dired File Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting the marked files."
  :value t)

(defhvar "Dired Directory Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting each marked directory."
  :value t)

(defun expunge-dired-files ()
  (multiple-value-bind (marked-files marked-dirs) (get-delete-marked-dired-files)
    (let ((dired:*error-function* #'dired-error-function)
	  (dired:*report-function* #'dired-report-function)
	  (dired:*yesp-function* #'dired-yesp-function)
	  (we-did-something nil)
	  (really 0))
      (when (and marked-files
		 (if (value dired-file-expunge-confirm)
		     (setq really
			   (prompt-for-y-or-n :prompt "Really delete file(s)? "
					      :default t
					      :must-exist t
					      :default-string "Y"))
		     t))
	(setf we-did-something t)
	(dolist (file-info marked-files)
	  (let ((pathname (car file-info))
		(write-date (cdr file-info)))
	    (if (= write-date (file-write-date pathname))
		(dired:delete-file (namestring pathname) :clobber t
				   :recursive nil)
		(message "~A has been modified, so it remains."
			 (namestring pathname))))))
      (when marked-dirs
	(dolist (dir-info marked-dirs)
	  (let ((dir (car dir-info))
		(write-date (cdr dir-info)))
	    (if (= write-date (file-write-date dir))
		(if (symlinkp (dired-namify dir))
		    (when (or (eq really t)
			      (if (value dired-file-expunge-confirm)
				  (if really
				      (prompt-for-y-or-n
				       :prompt "Really delete file(s)? "
				       :default t
				       :must-exist t
				       :default-string "Y"))
				  t))
		      (dired:delete-file (dired-namify dir)
					 :clobber t
					 :recursive nil)
		      (setf we-did-something t))
		    (when (if (value dired-directory-expunge-confirm)
			      (prompt-for-yes-or-no
			       :prompt (list "~a is a directory. Delete it? "
					     (directory-namestring dir))
			       :default nil
			       :must-exist t
			       :default-string "No")
			      t)
		      (dired:delete-file (directory-namestring dir)
					 :clobber t
					 :recursive t)
		      (setf we-did-something t)))
		(message "~A has been modified, so it remains.")))))
      we-did-something)))

(defun clear-marks (files point dired-info-files)
  (dolist (file files)
    (buffer-start point)
    (line-offset point
		 (position (pathname (car file)) dired-info-files
			   :test 'equal
			   :key 'dired-file-pathname))
    (dired-frob-deletion nil nil)))

(defcommand "Dired Delete and Expunge" (p)
  "Expunge any marked files, or mark and expunge the file on the current line."
  "Expunge any marked files, or mark and expunge the file on the current line."
  (multiple-value-bind (marked-files marked-dirs)
		       (get-delete-marked-dired-files)
    (if (or marked-files marked-dirs)
	(dired-expunge-files-command nil)
	(multiple-value-bind (marked-files marked-dirs)
			     (get-marked-dired-files)
	  (let ((file)
		(point (current-point))
		(info-files (dired-info-files (value dired-information))))
	    (if (or marked-files marked-dirs)
		(progn
		  (if marked-files
		      (dolist (f marked-files)
			(buffer-start point)
			(line-offset point
				     (position (pathname (car f)) info-files
					       :test 'equal
					       :key 'dired-file-pathname))
			(dired-frob-deletion nil t)))
		  (if marked-dirs
		      (dolist (f marked-dirs)
			(buffer-start point)
			(line-offset point
				     (position (pathname (car f)) info-files
					       :test 'equal
					       :key 'dired-file-pathname))
			(dired-frob-deletion nil t))))
		(progn
		  (setq file
			(array-element-from-mark point info-files))
		  (dired-frob-deletion p t)
		  ;; Hack to keep point near original position.
		  (line-offset point -1)))
	    (unwind-protect
		(dired-expunge-files-command nil)
	      (if file
		  (let ((position (position file info-files)))
		    (when position
		      (buffer-start point)
		      (line-offset point position)
		      (dired-frob-deletion nil nil)))
		  (progn
		    (if marked-files
			(clear-marks marked-files point info-files))
		    (if marked-dirs
			(clear-marks marked-dirs point info-files))))))))))


;;;; Dired operations on files.

(defhvar "Dired Copy File Confirm"
  "Can be either t, nil, or :update.  T means always query before clobbering an
   existing file, nil means don't query before clobbering an existing file, and
   :update means only ask if the existing file is newer than the source."
  :value T)

(defhvar "Dired Rename File Confirm"
  "When non-nil, dired will query before clobbering an existing file."
  :value T)

(defcommand "Dired Copy File" (p)
  "Copy the file under the point."
  "Copy the file under the point."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((point (current-point))
	   (confirm (value dired-copy-file-confirm))
	   (dired-info (value dired-information))
	   (source (if (or marked-files marked-dirs)
		       nil
		       (dired-file-pathname
			(array-element-from-mark
			 point
			 (dired-info-files dired-info)))))
	   (files (if source
		      nil
		      ;; FIX just append?
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
		   :default source
		   :must-exist nil)
		  (dired-info-pathname dired-info)))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      (if source
	  (dired:copy-file source dest :update (if (eq confirm :update) t nil)
			   :clobber (not confirm))
	  (dolist (f files)
	    (dired:copy-file f dest :update (if (eq confirm :update) t nil)
			     :clobber (not confirm))))
      (maintain-dired-consistency))))

(defcommand "Dired Rename File" (p)
  "Rename the file or directory under the point."
  "Rename the file or directory under the point."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((point (current-point))
	   (dired-info (value dired-information))
	   (source
	    (if (or marked-files marked-dirs)
		nil
		(dired-namify (dired-file-pathname
			       (array-element-from-mark
				point
				(dired-info-files dired-info))))))
	   (files (if source
		      nil
		      ;; FIX just append?
		      (mapcar 'car (append marked-files
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
		   :must-exist nil)
		  (dired-info-pathname dired-info)))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      ;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
      (if source
	  (dired:rename-file source dest
			     :clobber (value dired-rename-file-confirm))
	  (let ((confirm (value dired-rename-file-confirm)))
	    (dolist (f files)
	      (dired:rename-file f dest :clobber confirm))))
      ;; FIX It'd be nice if point came out on the new file.
      (maintain-dired-consistency))))

(defcommand "Dired Symlink File" (p)
  "Symbolic link to the file under the point."
  "Symbolic link to the file under the point."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((point (current-point))
	   (dired-info (value dired-information))
	   (dest (if (or marked-files marked-dirs)
		     nil
		     (dired-file-pathname
		      (array-element-from-mark
		       point
		       (dired-info-files dired-info)))))
	   (files (if dest
		      nil
		      (append marked-files marked-dirs)))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      (flet ((prompt (dest)
	       (merge-pathnames
		(prompt-for-file
		 :prompt (format nil "Link to ~A from: " dest)
		 :help "A name for the link to the file."
		 :default (dired-namify (namestring dest))
		 :must-exist nil)
		(dired-info-pathname dired-info))))
	(if dest
	    (dired:symlink-file (prompt dest) dest)
	    (dolist (f files)
	      (dired:symlink-file (prompt (car f)) f))))
      (maintain-dired-consistency))))

(defcommand "Dired Copy with Wildcard" (p)
  "Copy files that match a pattern containing ONE wildcard."
  "Copy files that match a pattern containing ONE wildcard."
  (declare (ignore p))
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
			:must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*yesp-function* #'dired-yesp-function)
	 (dired:*report-function* #'dired-report-function))
    (dired:copy-file pattern destination :update (if (eq confirm :update) t nil)
		     :clobber (not confirm)
		     :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency))

(defcommand "Dired Rename with Wildcard" (p)
  "Rename files that match a pattern containing ONE wildcard."
  "Rename files that match a pattern containing ONE wildcard."
  (declare (ignore p))
  (let* ((dir-info (value dired-information))
	 (pattern (prompt-for-string
		   :prompt "Filename pattern: "
		   :help "Type a filename with a single asterisk."
		   :trim t))
	 (destination (namestring
		       (prompt-for-file
			:prompt "Destination Spec: "
			:help "Destination spec.  May contain ONE asterisk."
			:default (dired-info-pathname dir-info)
			:must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*yesp-function* #'dired-yesp-function)
	 (dired:*report-function* #'dired-report-function))
    (dired:rename-file pattern destination
		       :clobber (not (value dired-rename-file-confirm))
		       :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency))

(defcommand "Dired Compare Files" (p)
  "Compare Dired files."
  "Compare Dired files."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (declare (ignore marked-dirs))
    (let* ((point (current-point))
	   (dired-info (value dired-information))
	   (one
	    (if marked-files
		(car marked-files)
		(dired-namify (dired-file-pathname
			       (array-element-from-mark
				point
				(dired-info-files dired-info)
  "Point must be on a file line, or a file must be marked.")))))
	   (two (or (and marked-files (cadr marked-files))
		    (merge-pathnames
		     (prompt-for-file
		      :prompt "Second file: "
		      :help (format nil "File to compare with ~A." one)
		      :default one
		      :must-exist t))))
	   (dired:*error-function* #'dired-error-function)
	   (dired:*report-function* #'dired-report-function)
	   (dired:*yesp-function* #'dired-yesp-function))
      ;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
      (compare-files-command nil one two))))

(defcommand "Delete File" (p)
  "Delete a file.  Specify directories with a trailing slash."
  "Delete a file.  Specify directories with a trailing slash."
  (declare (ignore p))
  (let* ((spec (namestring
		(prompt-for-file
		 :prompt "Delete File: "
		 :help '("Name of File or Directory to delete.  ~
			  One wildcard is permitted.")
		 :must-exist nil)))
	 (ed::directoryp (directoryp spec))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function)
	 (dired:*post-delete-function* #'dired-post-delete-function))
    (when (or (not directoryp)
	      (not (value dired-directory-expunge-confirm))
	      (prompt-for-yes-or-no
	       :prompt (list "~A is a directory. Delete it? "
			     (directory-namestring spec))
	       :default nil :must-exist t :default-string "No")))
    (dired:delete-file spec :recursive t
		       :clobber (or directoryp
				    (value dired-file-expunge-confirm))))
  (maintain-dired-consistency))

(defcommand "Copy File" (p)
  "Copy a file, allowing ONE wildcard."
  "Copy a file, allowing ONE wildcard."
  (declare (ignore p))
  (let* ((confirm (value dired-copy-file-confirm))
	 (source (namestring
		  (prompt-for-file
		   :prompt "Source Filename: "
		   :help "Name of File to copy.  One wildcard is permitted."
		   :must-exist nil)))
	 (dest (namestring
		(prompt-for-file
		 :prompt (if (directoryp source)
			     "Destination Directory Name: "
			     "Destination Filename: ")
		 :help "Name of new file."
		 :default source
		 :must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
		     :clobber (not confirm)))
  (maintain-dired-consistency))

(defcommand "Rename File" (p)
  "Rename a file, allowing ONE wildcard."
  "Rename a file, allowing ONE wildcard."
  (declare (ignore p))
  (let* ((source (namestring
		  (prompt-for-file
		   :prompt "Source Filename: "
		   :help "Name of file to rename.  One wildcard is permitted."
		   :must-exist nil)))
	 (dest (namestring
		(prompt-for-file
		 :prompt (if (directoryp source)
			     "Destination Directory Name: "
			     "Destination Filename: ")
		 :help "Name of new file."
		 :default source
		 :must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (dired:rename-file source dest
		       :clobber (not (value dired-rename-file-confirm))))
  (maintain-dired-consistency))

(defcommand "Symlink File" (p)
  "Create a symbolic link to a file."
  "Create a symbolic link to a file."
  (declare (ignore p))
  (let* ((source (namestring
		  (prompt-for-file
		   :prompt "Link name: "
		   :help "Name to link to file."
		   :must-exist nil)))
	 (dest (namestring
		(prompt-for-file
		 :prompt "Target name: "
		 :help "Name of the file to which to link."
		 :default source
		 :must-exist nil)))
	 (dired:*error-function* #'dired-error-function)
	 (dired:*report-function* #'dired-report-function)
	 (dired:*yesp-function* #'dired-yesp-function))
    (dired:symlink-file source dest))
  (maintain-dired-consistency))

(defcommand "Compare Files" (p &optional file1 file2)
  "Compare two prompted files."
  "Compare two prompted files."
  (declare (ignore p))
  (let* ((one (namestring
	       (or file1
		   (prompt-for-file
		    :prompt "First file: "
		    :help "Name of first of files to compare."
		    :default (buffer-pathname (current-buffer))
		    :must-exist nil))))
	 (two (namestring
	       (or file2
		   (prompt-for-file
		    :prompt "Second file: "
		    :help "Name of second of files to compare."
		    :default (buffer-pathname (current-buffer))
		    :must-exist nil))))
	 (buffer-one (make-buffer (format nil "~A-~A"
					  (pathname-name one)
					  (gensym))))
	 (buffer-two (make-buffer (format nil "~A-~A"
					  (pathname-name two)
					  (gensym)))))
    ;; FIX Could print-dir into buffers if files are dirs.
    (read-buffer-file one buffer-one)
    (read-buffer-file two buffer-two)
    (compare-buffers-command nil buffer-one buffer-two)
    (delete-buffer buffer-one)
    (delete-buffer buffer-two)))

(defun maintain-dired-consistency ()
  (dolist (info *pathnames-to-dired-buffers*)
    (let* ((directory (car info)) ; FIX was (directory-namestring (car info)))
	   (buffer (cdr info))
	   (dir-info (variable-value 'dired-information :buffer buffer)))
      (when (eq (dired-info-extra-data dir-info) t)
	(or (= (dired-info-write-date dir-info)
	       (file-write-date directory))
	    (update-dired-buffer directory
				 (dired-info-pattern dir-info)
				 buffer))))))


;;;; Dired and the shell.

(declaim (special *shell-command-in-buffer-history*))

(defun dired-toggle-file-compression (file)
  "Toggle compression of File.  Return new name."
  (let ((type (pathname-type file))
	(arg `(,(namestring file))))
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
					      (dired-namify (namestring name))
					      ".tar.bz2")
				,(namestring name)))))
    (when ret
      (case (process-exit-code ret)
	(0)
	(t (message "tar exited with error."))))))

(defcommand "Dired Toggle File Compression" (p)
  "Compress or inflate file(s).  Operate on marked files if there are any,
   else on the file at point."
  "Compress or inflate file(s).  Operate on marked files if there are any,
   else on the file at point."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
     (if (or marked-files marked-dirs)
	 (progn
	   (when marked-files
	     (dolist (file marked-files)
	       (dired-toggle-file-compression (car file))))
	   (when marked-dirs
	     (dolist (f marked-dirs)
	       (dired-toggle-dir-compression (car f))))
	   (maintain-dired-consistency))
	 (let ((name (dired-file-pathname
		      (array-element-from-mark
		       (current-point)
		       (dired-info-files (value dired-information))))))
	   (line-offset (current-point) 1)
	   (if (directoryp name)
	       (progn
		 (dired-toggle-dir-compression name)
		 (maintain-dired-consistency))
	       (let ((name (dired-toggle-file-compression (dired-namify name))))
		 (maintain-dired-consistency)
		 (let ((files (dired-info-files (value dired-information))))
; FIX The pos'n fails when this is run twice in quick succession.
;		   (redisplay)
		   (move-mark (current-point) (buffer-start-mark (current-buffer)))
		   (line-offset (current-point)
				(or (position (pathname name) files
					      :test #'equal
					      :key #'dired-file-pathname)
				    (editor-error
				     "Failed to find ~A in dired-info-files."
				     name))))))))))
    
(defcommand "Dired Shell Command on File" (p)
  "Run a prompted shell command on the marked files or the current file.
   If the command includes \"$A\" or \"${A}\" then export A as the name of
   the file(s) before running the command, otherwise append a space and the
   filename(s) to the command."
  "Run a prompted shell command on the marked files or the current file.
   If the command includes \"$A\" or \"${A}\" then export A as the name of
   the file(s) before running the command, otherwise append a space and the
   filename(s) to the command."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let* ((pathname (if (or marked-files marked-dirs)
			 nil
			 (dired-file-pathname
			  (array-element-from-mark
			   (current-point)
			   (dired-info-files (value dired-information))))))
	   (files (if pathname
		      nil
		      (string-trim
		       '(#\space)
		       (apply 'concatenate
			      'simple-string
			      (mapcar (lambda (car) (format nil "~A " (car car)))
				      (append marked-files marked-dirs))))))
	   (command (prompt-for-string
		     :trim t
		     :prompt (format nil "Command to execute on ~A: "
				     (if pathname
					 (or (if (pathname-type pathname)
						 (format nil "~A.~A"
							 (pathname-name pathname)
							 (pathname-type pathname))
						 (pathname-name pathname))
					     (directory-namestring pathname))
					 "marked files"))
		     :help (format nil
				   "Shell command line to execute on the ~A."
				   (if pathname "file" "files"))
		     :history *shell-command-in-buffer-history*
		     :history-pointer
		     '*shell-command-in-buffer-history-pointer*)))
      (make-new-shell t nil
		      (if (or (search "$A" command)
			      (search "${A}" command))
			  (format nil "export A=\"~A\"; ~A"
				  (or pathname files)
				  command)
			  (format nil "~A ~A" command (or pathname
							  files)))))))
  

;;;; Dired utilities.

;;; GET-DELETE-MARKED-DIRED-FILES returns as multiple values a list of file
;;; specs and a list of directory specs that have been marked for deletion.
;;; This assumes the current buffer is a "Dired" buffer.
;;;
(defun get-delete-marked-dired-files ()
  (let* ((files (dired-info-files (value dired-information)))
	 (length (length files))
	 (marked-files ())
	 (marked-dirs ()))
    (unless files (editor-error "Not in Dired buffer."))
    (do ((i 0 (1+ i)))
	((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let* ((thing (svref files i))
	     (pathname (dired-file-pathname thing)))
	(when (and (dired-file-deleted-p thing) ; file marked for delete
		   (probe-file pathname)) 	; file still exists
	  (if (directoryp pathname)
	      (push (cons pathname (file-write-date pathname)) marked-dirs)
	      (push (cons pathname (file-write-date pathname))
		    marked-files)))))))

;;; GET-MARKED-DIRED-FILES returns as multiple values a list of file
;;; specs and a list of directory specs that have been *-marked.
;;; This assumes the current buffer is a "Dired" buffer.
;;;
(defun get-marked-dired-files ()
  (let* ((files (dired-info-files (value dired-information)))
	 (length (length files))
	 (marked-files ())
	 (marked-dirs ()))
    (do ((i 0 (1+ i)))
	((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let* ((thing (svref files i))
	     (pathname (dired-file-pathname thing)))
	(when (and (dired-file-marked-p thing)  ; file marked
		   (probe-file pathname)) 	; file still exists
	  (if (directoryp pathname)
	      (push (cons pathname (file-write-date pathname)) marked-dirs)
	      (push (cons pathname (file-write-date pathname))
		    marked-files)))))))

;;; ARRAY-ELEMENT-FROM-MARK -- Internal Interface.
;;;
;;; This counts the lines between it and the beginning of the buffer.  The
;;; number is used to index vector as if each line mapped to an element
;;; starting with the zero'th element (lines are numbered starting at 1).
;;; This must use AREF since some modes use this with extendable vectors.
;;;
(defun array-element-from-mark (mark vector
				&optional (error-msg "Invalid line."))
  (when (blank-line-p (mark-line mark)) (editor-error error-msg))
  (aref vector
	 (1- (count-lines (region
			   (buffer-start-mark (line-buffer (mark-line mark)))
			   mark)))))

;;; DIRED-NAMIFY and DIRED-DIRECTORIFY are implementation dependent slime.
;;;
(defun dired-namify (pathname)
  (let* ((string (namestring pathname))
	 (last (1- (length string))))
    (if (char= (schar string last) #\/)
	(subseq string 0 last)
	string)))
;;;
;;; This is necessary to derive a canonical representation for directory
;;; names, so "Dired" can map various strings naming one directory to that
;;; one directory.
;;;
(defun dired-directorify (pathname)
  (let ((directory (ext:unix-namestring pathname)))
    (if directory
	(if (directoryp directory)
	    directory
	    (pathname (concatenate 'simple-string (namestring directory) "/"))))
    pathname))


;;;; Locate.

(defun execute-locate (regex stream)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (format nil "locate \"~A\" 2>/dev/null" regex))
   :output stream))

(defun locate (regex)
  "Return the list of files in the locate database that match REGEX."
  ;; FIX (with-temp-buffer?
  (let* ((buf-name (format nil "Locate Results ~a" regex))
	 (new-buffer (progn
		       (if (getstring buf-name *buffer-names*)
			   (kill-buffer-command nil buf-name))
		       (make-buffer buf-name
				    :modes '("Fundamental"))))
	 (buffer (or new-buffer (error "Failed to create temp buffer.")))
	 (point (buffer-point buffer))
	 (files nil))
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
    (kill-buffer-command nil buf-name)
    files))

(defcommand "Locate" (p)
  "Prompt for a regex and edit the result of a locate on the regex.  If a
   dired of that search already exists, go to and update that buffer,
   otherwise create one."
  "Prompt for a regex and edit the result of a locate on the regex.  If a
   dired of that search already exists, go to and update that buffer,
   otherwise create one."
  (declare (ignore p))
  (let ((regex (prompt-for-string
		:prompt "Locate: "
		:help "Regular expression describing files to locate."
		:trim t)))
    (if (if (string= regex "")
	    (prompt-for-y-or-n
	     :prompt "Locate every file in the file system? "
:help "Y to confirm locate of every file (which usually takes a very long time)."
	     :default nil :must-exist t :default-string "N")
	    t)
	(let* ((name (format nil "Locate \"~A\"" regex))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(change-to-buffer buffer)
		(dired-update-buffer-command nil))
	      (let ((files (locate regex)))
		(if files
		    (dired-guts nil nil "/" nil nil nil files name `(locate ,regex))
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
			  (format nil " \\( ~A \\) 2>/dev/null" exp))))
   :output stream))

(defun find-files (pathname exp &optional recurse)
  "Return the list of files in PATHNAME that match EXP."
  ;; FIX (with-temp-buffer?
  (let* ((buf-name (format nil "Find Results ~a" exp))
	 (new-buffer (progn
		       (if (getstring buf-name *buffer-names*)
			   (kill-buffer-command nil buf-name))
		       (make-buffer buf-name
				    :modes '("Fundamental"))))
	 (buffer (or new-buffer (error "Failed to create temp buffer.")))
	 (point (buffer-point buffer))
	 (files nil))
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
    (kill-buffer-command nil buf-name)
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
	     :default nil :must-exist t :default-string "N")
	    t)
	(let* ((name (format nil "Dired find \"~A\" in ~A~A"
			     exp pathname
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(change-to-buffer buffer)
		(dired-update-buffer-command nil))
	      (let ((files (find-files pathname exp recurse)))
		(if files
		    (dired-guts nil nil "/" nil nil nil
				files name `(find-files ,pathname
							,exp
							,recurse))
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
	       :must-exist nil
	       :prompt "Expression: "
	       ;; FIX comes out in upper case
	       :default `(string= (pathname-name ,(read-from-string "file")) "")
	       :help "Expression with which to filter files.")))
    (if (if (equal exp "")
	    (prompt-for-y-or-n
	     :prompt (list "Find every file in ~A? " pathname)
:help "Confirm find of every file (can take a long time for large dirs)."
             :default nil :must-exist t :default-string "N")
	    t)
	(let* ((name (format nil "Dired find ~A in ~A~A"
			     exp pathname
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(change-to-buffer buffer)
		(dired-update-buffer-command nil))
	      (let* ((predicate
		      (eval (list 'lambda (list (read-from-string "file"))
				  (if (equal exp "")
				      `(declare (ignore ,(read-from-string "file")))
				      exp))))
		     (files (list-files pathname predicate :recurse recurse)))
		(if files
		    (dired-guts nil nil "/" nil nil nil
				files name `(list-files ,pathname
							,predicate
							:recurse ,recurse))
		    (message "Failed to find any files."))))))))

(defcommand "Dired Find" (p)
  "Find files in a single directory, and dired the resulting files.  If a
   dired of such a search already exists then go to and update the existing
   dired."
  "Find files in a single directory, and dired the resulting files.  If a
   dired of such a search already exists then go to and update the existing
   dired."
  (declare (ignore p))
  (dired-find))

(defcommand "Dired Find Recursively" (p)
  "Find files in a directory tree, and dired the resulting files.  If a
   dired of such a search already exists then go to and update the existing
   dired."
  "Find files in a directory tree, and dired the resulting files.  If a
   dired of such a search already exists then go to and update the existing
   dired."
  (declare (ignore p))
  (dired-find t))



;;;; Search files.

(defun execute-search (pathname exp stream &optional print-match recurse)
  (ext:run-program
   "/bin/sh"
   (list "-c"
	 (if recurse
	     (format nil
		     "find ~A 2>/dev/null | xargs grep ~A -I -e \"~A\" 2>/dev/null"
		     (namestring (truename pathname))
		     (if print-match "-Hn" "-l")
		     exp)
	     (format nil
		     "grep ~A -I -e \"~A\" ~A\* 2>/dev/null"
		     (if print-match "-Hn" "-l")
		     exp
		     (namestring (truename pathname)))))
   :output stream))

(defun search-files (pathname exp &optional recurse display-matches)
  "Return the list of files in PATHNAME that match EXP."
  ;; FIX (with-temp-buffer?
  (let* ((buf-name (format nil "Search Results ~a" exp))
	 (new-buffer (progn
		       (if (getstring buf-name *buffer-names*)
			   (kill-buffer-command nil buf-name))
		       (make-buffer buf-name
				    :modes '("Fundamental"))))
	 (buffer (or new-buffer (error "Failed to create temp buffer.")))
	 (point (buffer-point buffer))
	 (files nil))
    (with-writable-buffer (buffer)
      (with-output-to-mark (s point :full)
	(execute-search pathname exp s display-matches recurse)))
    ;; FIX use this style in others?
    (flet ((convert-line-string (line-string)
	     (or (string= line-string "")
		 (if display-matches
		     ;; FIX could use split
		     (let* ((sep1-start (search ":" line-string))
			    (sep2-start (search ":" line-string
						:start2 (+ sep1-start 1))))
		       (when (and sep1-start sep2-start)
			 (setq files
			       (cons (list (subseq line-string 0 sep1-start)
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
    (kill-buffer-command nil buf-name)
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
    (map-files pathname
	       (lambda (file)
		 ;; FIX may be better to use a stream for this case
		 ;; (with-open-file
		 (or (eq (unix::unix-file-kind file) :directory)
		     (hi::with-temp-buffer (buffer file)
		       (message "search ~A (~A) for .~A. from ~A with ~A" buffer file rule (buffer-point buffer) *pattern*)
		       (if (find-pattern (buffer-point buffer) *pattern*)
			   (setq *files* (append *files* (list file)))))))
	       :recurse recurse
	       :follow-links t)
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
	       :must-exist nil :prompt "Search rule: "
	       :help "Parser rule with which to search files.")))
    (if (if (and (stringp exp) (string= exp ""))
	    (prompt-for-y-or-n
	     :prompt "Proceed with empty search expression? "
	     :default nil :must-exist t :default-string "N")
	    t)
	(let* ((exp (list exp))
	       (name (format nil "~ASearch ~A for \"~A\"~A"
			     (if display-matches "" "Dired ")
			     pathname exp
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(push buffer *last-search-buffer-stack*)
		(change-to-buffer buffer)
		(dired-update-buffer-command nil))
	      (let ((files (search-files pathname exp recurse display-matches)))
		(if files
		    (push (dired-guts nil nil "/" nil
				      (if display-matches '(:name ": " 1 ": " 2))
				      nil files name
				      `(search-files ,pathname ,exp
						     ,recurse ,display-matches))
			  *last-search-buffer-stack*)
		    (message "Failed to match in any files."))))))))
|#

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
 	 (exp (prompt-for-string
 	       :prompt "Search regexp: "
 	       :help "Regular expression with which to search files."
 	       :default (word-at-point)
 	       :trim t)))
    (if (if (and (stringp exp) (string= exp ""))
	    (prompt-for-y-or-n
	     :prompt "Proceed with empty search expression? "
	     :default nil :must-exist t :default-string "N")
	    t)
	(let* ((name (format nil "~ASearch ~A for \"~A\"~A"
			     (if display-matches "" "Dired ")
			     pathname exp
			     (if recurse " Recursively" "")))
	       (buffer (cdr (assoc name *pathnames-to-dired-buffers*
				   :test #'string=))))
	  (if buffer
	      (progn
		(push buffer *last-search-buffer-stack*)
		(change-to-buffer buffer)
		(dired-update-buffer-command nil))
	      (let ((files (search-files pathname exp recurse display-matches)))
		(if files
		    (push (dired-guts nil nil "/" nil
				      (if display-matches '(:name ": " 1 ": " 2))
				      nil files name
				      `(search-files ,pathname ,exp
						     ,recurse ,display-matches))
			  *last-search-buffer-stack*)
		    (message "Failed to match in any files."))))))))

(defcommand "Dired Search" (p)
  "Search the files in a single directory for a regex, and dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired."
  "Search the files in a single directory for a regex, and dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired."
  (declare (ignore p))
  (dired-search-files))

(defcommand "Dired Search Recursively" (p)
  "Search the files in a directory tree for a regex, and Dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired."
  "Search the files in a directory tree for a regex, and Dired the files
   containing matches.  If a dired of such a search already exists then go
   to and update the existing dired."
  (declare (ignore p))
  (dired-search-files t))

(defcommand "Search" (p)
  "Search the files in a single directory for a regex, and edit the
   matching lines.  If an edit of such a search already exists then go to
   the existing edit."
  "Search the files in a single directory for a regex, and dired the
   matching lines.  If an edit of such a search already exists then go to
   and update the existing edit."
  (declare (ignore p))
  (dired-search-files nil t))

(defcommand "Search Recursively" (p)
  "Search the files in a directory tree for a regex, and edit the matching
   lines.  If an edit of such a search already exists then go to the
   existing edit."
  "Search the files in a directory tree for a regex, and dired the matching
   lines.  If an edit of such a search already exists then go to and update
   the existing edit."
  (declare (ignore p))
  (dired-search-files t t))

;; FIX Maybe this should be per-search (dired/normal and single/recursive).
;;     Add "Last Find" and "Next Find Match" too?
(defcommand "Last Search" (p)
  "Switch to the most recently performed search, if there is one."
  "Switch to the most recently performed search, if there is one."
  (declare (ignore p))
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

(defcommand "Next Search Match" (p)
  "Switch to the next match from the most recent search."
  "Switch to the next match from the most recent search."
  (declare (ignore p))
  (when (last-search-command nil)
    (let ((point (current-point)))
      (line-offset point 1)
      (if (blank-line-p (mark-line point))
	  (editor-error "Past last match."))
      (dired-edit-file-command t))))
  


;;;; Version control.

; (defmode "VC" :major-p nil
;   :documentation
;   "Version control \"sub-mode\" of Dired.")

(defun list-files-with-vc (dir-info)
  (let ((files) (max-version 0) (max-status 0)
	(pathname (dired-info-pathname dir-info))
	(all (dired-info-dot-files-p dir-info))
	(backups (dired-info-backup-files-p dir-info))
	(recurse (dired-info-recurse dir-info)))
    (map-files pathname
	       (lambda (file)
		 (let ((vc-info (make-vc-info file t)))
		   (when vc-info
		     (let ((version (or (vc-info-version vc-info) ""))
			   (status (string (or (vc-info-status vc-info)
					       ""))))
		       (if (> (length version) max-version)
			   (setq max-version (length version)))
		       (if (> (length status) max-status)
			   (setq max-status (length status)))
		       ;; Empty string dummy line number.
		       (push (list file "" vc-info version status)
			     files)))))
	       :follow-links t
	       :all all :backups backups
	       :recurse recurse)
    ;; Normalise string widths.
    (mapcar #'(lambda (list)
		(rplaca (nthcdr 3 list)
			(format nil
				(format nil "~~~DA" max-version)
				(nth 3 list)))
		(rplaca (nthcdr 4 list)
			(format nil
				(format nil "~~~DA" max-status)
				(nth 4 list))))
	    files)
    (nreverse files)))

(defcommand "Dired Toggle Version Control" (p)
  "Toggles version control format."
  "Toggles version control format."
  (declare (ignore p))
  (or (hemlock-bound-p 'dired-information)
      (editor-error "Must be in Dired buffer."))
  (let* ((dir-info (value dired-information))
	 (generator (dired-info-generator dir-info)))
    (if (and generator (eq generator #'list-files-with-vc))
	(setf (dired-info-extra-data dir-info) t
	      (dired-info-generator dir-info) nil
;	      (buffer-minor-mode (current-buffer) "VC") nil
	      (dired-info-coldefs dir-info) nil)
	(let ((generator #'list-files-with-vc))
	  (setf (dired-info-extra-data dir-info)
		(funcall generator dir-info))
	  (setf (dired-info-generator dir-info) generator)
;	  (setf (buffer-minor-mode (current-buffer) "VC") t)
	  (setf (dired-info-coldefs dir-info) '("  " 4 " " 3 " /" :name))))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 (current-buffer))))

(defcommand "Dired VC Log Entry" (p)
  "Show the log of the current file."
  "Show the log of the current file."
  (declare (ignore p))
  (let ((pathname (dired-file-pathname
		   (array-element-from-mark
		    (current-point)
		    (dired-info-files (value dired-information))))))
    (vc-file-log-entry-command nil pathname)))

(defcommand "Dired VC Compare File" (p)
  "Compare the file at point with the repository version."
  "Compare the file at point with the repository version."
  (declare (ignore p))
  (let ((pathname (dired-file-pathname
		   (array-element-from-mark
		    (current-point)
		    (dired-info-files (value dired-information))))))
    (vc-compare-file-command nil pathname)))

(defcommand "Dired VC Update File" (p)
  "Update marked files if any, otherwise the file at point."
  "Update marked files if any, otherwise the file at point."
  (declare (ignore p))
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
      (vc-update-file-command nil pathname files nil)
      (setf (dired-info-extra-data dir-info)
	    (funcall (dired-info-generator dir-info) dir-info))
      (update-dired-buffer (dired-info-pathname dir-info)
			   (dired-info-pattern dir-info)
			   (current-buffer))
      ;; FIX update other vc eds of this dir too
      (maintain-dired-consistency))))

(declaim (special *cvs-update-file-recurse*))

(defcommand "Dired VC Update Directory" (p)
  "Update all the files listed in the current buffer."
  "Update all the files listed in the current buffer."
  (declare (ignore p))
  (let* ((dir-info (value dired-information))
	 (pathname (dired-info-pathname dir-info))
	 (*cvs-update-file-recurse* (dired-info-recurse dir-info)))
    (vc-update-file-command nil (namestring pathname) nil nil)
    (setf (dired-info-extra-data dir-info)
	  (funcall (dired-info-generator dir-info) dir-info))
    (update-dired-buffer (dired-info-pathname dir-info)
			 (dired-info-pattern dir-info)
			 (current-buffer))
    ;; FIX update other vc eds of this dir too
    (maintain-dired-consistency)))

(defcommand "Dired VC Commit File" (p)
  "Commit marked files if any, otherwise the file at point."
  "Commit marked files if any, otherwise the file at point."
  (declare (ignore p))
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
      (vc-commit-file-command nil pathname files)
      (setf (dired-info-extra-data dir-info)
	    (funcall (dired-info-generator dir-info) dir-info))
      (update-dired-buffer (dired-info-pathname dir-info)
			   (dired-info-pattern dir-info)
			   (current-buffer))
      ;; FIX update other vc eds of this dir too
      (maintain-dired-consistency))))


;;;; WWW.

(defcommand "Dired WWW File" (p)
  "Browse the file at point."
  "Browse the file at point."
  (declare (ignore p))
  (multiple-value-bind (marked-files marked-dirs)
		       (get-marked-dired-files)
    (let ((files (if (or marked-files marked-dirs)
		     (append marked-files marked-dirs)
		     (list (dired-file-pathname
			    (array-element-from-mark
			     (current-point)
			     (dired-info-files (value dired-information))))))))
      ;; FIX also browse rest
      (message "~A" (car files))
      (www-command nil (car files)))))


;;;; View Mode.

(defmode "View" :major-p nil
  :setup-function 'setup-view-mode
  :cleanup-function 'cleanup-view-mode
  :precedence 5.0
  :documentation
  "View mode scrolls forwards and backwards in a file with the buffer read-only.
   Scrolling off the end optionally deletes the buffer.")

(defun setup-view-mode (buffer)
  (defhvar "View Return Function"
    "Function that gets called when quitting or returning from view mode."
    :value nil
    :buffer buffer)
  (setf (buffer-writable buffer) nil))
;;;
(defun cleanup-view-mode (buffer)
  (delete-variable 'view-return-function :buffer buffer)
  (setf (buffer-writable buffer) t))

(defcommand "View File" (p &optional pathname)
  "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience."
  "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file
		  :prompt "View File: " :must-exist t
		  :help "Name of existing file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (make-buffer (format nil "View File ~A" (gensym)))))
    (visit-file-command nil pn buffer)
    (setf (buffer-minor-mode buffer "View") t)
    (change-to-buffer buffer)
    buffer))

(defcommand "View Return" (p)
  "Return to a parent buffer, if it exists."
  "Return to a parent buffer, if it exists."
  (declare (ignore p))
  (unless (call-view-return-fun)
    (editor-error "No View return method for this buffer.")))

(defcommand "View Quit" (p)
  "Delete a buffer in view mode."
  "Delete a buffer in view mode, invoking VIEW-RETURN-FUNCTION if it exists for
   this buffer."
  (declare (ignore p))
  (let* ((buf (current-buffer))
	 (funp (call-view-return-fun)))
    (delete-buffer-if-possible buf)
    (unless funp (editor-error "No View return method for this buffer."))))

;;; CALL-VIEW-RETURN-FUN returns nil if there is no current
;;; view-return-function.  If there is one, it calls it and returns t.
;;;
(defun call-view-return-fun ()
  (if (hemlock-bound-p 'view-return-function)
      (let ((fun (value view-return-function)))
	(cond (fun
	       (funcall fun)
	       t)))))


(defhvar "View Scroll Deleting Buffer"
  "When this is set, \"View Scroll Down\" deletes the buffer when the end
   of the file is visible."
  :value t)

(defcommand "View Scroll Down" (p)
  "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer."
  "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer."
  (if (and (not p)
	   (displayed-p (buffer-end-mark (current-buffer))
			(current-window))
	   (value view-scroll-deleting-buffer))
      (view-quit-command nil)
      (scroll-window-down-command p)))

(defcommand "View Edit File" (p)
  "Turn off \"View\" mode in this buffer."
  "Turn off \"View\" mode in this buffer."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (setf (buffer-minor-mode buf "View") nil)
    (warn-about-visit-file-buffers buf)))

(defcommand "View Help" (p)
  "Shows \"View\" mode help message."
  "Shows \"View\" mode help message."
  (declare (ignore p))
  (describe-mode-command nil "View"))
