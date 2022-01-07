;;; -*- Package: HEMLOCK; Mode: Lisp -*-
;;;
;;; Version control interface.
;;;
;;; To extend this to other version control systems add maker functions to
;;; *vc-info-makers*.

(in-package "HEMLOCK")


;;;; Structure.

(defhvar "VC Keep Around After Unlocking"
  "If non-NIL (the default) keep the working file around after unlocking
   it.  When NIL, the working file and buffer are deleted."
  :value t)

(defhvar "VC Commit File Hook"
  "VC Commit File Hook"
  :value nil)

(defhvar "VC Update File Hook"
  "VC Update File Hook"
  :value nil)

(defhvar "VC Lock File Hook"
  "VC Lock File Hook"
  :value nil)

(defhvar "VC Log Buffer Hook"
  "Version control log buffer hook."
  :value nil)

(defhvar "VC Log Entry Buffer"
  "Name in which to buffer version control log entries."
  :value "VC Log")

(defhvar "VC Comparison Buffer"
  "Name in which to buffer version control comparisons."
  :value "VC Comparison")

(defstruct (vc-information (:conc-name vc-info-)
			   (:constructor %make-vc-info
					 (type
					  insert-log-fun
					  update-fun
					  commit-fun
					  modeline-string-fun)))
  "Per-buffer version control information."
  type ; Type of version control the file is under, e.g. :cvs or :rcs.
  status
  version
  date
  module
  repository
  ;; Functions.
  ;;
  ;; Insert a log in a buffer.  Takes a vc-info, pathname and stream.
  insert-log-fun
  ;; Update a file from the repository.  Takes vc-info, buffer, pathname,
  ;; files, lock flag, always-overwrite flag.
  update-fun
  ;; Commit the changes in a file to the repository.  Takes vc-info,
  ;; buffer, pathname, files, keep-lock flag.
  commit-fun
  ;; Toggle the lock on a file.  Takes vc-info, buffer, pathname.
  (lock-fun #'vc-continue)
  ;; Compare file to repository.  Takes vc-info, stream, pathname.
  (compare-fun #'vc-continue)
  ;; Annotate a file.  Takes vc-info...
  (annotate-fun #'vc-continue)
  ;; Make a modeline field string.  Takes vc-info.
  modeline-string-fun
  ;;
  ;; Any extra data needed per version control system.
  extra)

(defvar *vc-info-makers* '(maybe-make-cvs-info maybe-make-rcs-info)
  "List of functions called in make-vc-info to make a vc-info structure for
   a buffer.  The functions are called on the buffer pathname in turn until
   one returns a true value.")

(defvar *last-vc-command-name* nil)
(defvar *last-vc-command-args* nil)
(defvar *last-vc-command-output-string* nil)
(defvar *vc-output-stream* (make-string-output-stream))

(defmode "VC-Log" :major-p nil
  :precedence 5.0
  :documentation
  "Mode for viewing version control change logs.")

(defmode "VC-Log-Entry" :major-p nil
  :precedence 5.0
  :documentation
  "Mode for entering version control change logs.")


;;;; Interface (i.e. used in Dired).

(defun make-vc-info (pathname &optional connect)
  "Return a version-control-info struct made from Pathname.  If Connect is
   true then it is OK for the maker to access the repository."
  (dolist (maker *vc-info-makers*)
    (let ((vc-info (funcall maker pathname connect)))
      (if vc-info (return-from make-vc-info vc-info)))))


;;;; Helper functions.

(defun current-vc-info (&key (must-exist t))
  "Return the version control info structure for the current buffer."
  (let ((buffer (current-buffer)))
    (or (editor-bound-p 'vc-info :buffer buffer)
	(update-buffer-vc-info buffer))
    (or (variable-value 'vc-info :buffer buffer)
	(if must-exist
	    (editor-error "File in buffer must be under version control.")))))

(defun current-buffer-pathname ()
  (let ((pathname (buffer-pathname (current-buffer))))
    (or pathname (editor-error "The buffer has no pathname."))
    pathname))

(defmacro do-command (error-on-error command &rest args)
  "Call run-program on Command and Args.  If error-on-error is true then
   throw an editor-error if the program exits with an error code."
  `(progn
     (setf *last-vc-command-name* ',command)
     (setf *last-vc-command-args*
	   (apply 'concatenate 'simple-string
		  (mapcar (lambda (arg) (format nil " ~A" (eval arg)))
			  ,(car args))))
     (get-output-stream-string *vc-output-stream*)
     (let ((process (ext:run-program ',command ,@args
				     :error *vc-output-stream*)))
       (setf *last-vc-command-output-string*
	     (get-output-stream-string *vc-output-stream*))
       (case (ext:process-status process)
	 (:exited
	  ,(if error-on-error
	       (list 'or '(zerop (ext:process-exit-code process))
		     (list 'editor-error
			   "~A aborted with an error; ~
			    use the ``Last VC Command Output'' command for ~
			    more information" command))))
	 (:signaled
	  (editor-error "~A killed with signal ~A~@[ (core dumped)]."
			',command
			(ext:process-exit-code process)
			(ext:process-core-dumped process)))
	 (t
	  (editor-error "~S still alive?" process))))))

(defun buffer-different-from-file (buffer filename)
  (with-open-file (file filename)
    (do ((buffer-line (mark-line (buffer-start-mark buffer))
		      (line-next buffer-line))
	 (file-line (read-line file nil nil)
		    (read-line file nil nil)))
	((and (or (null buffer-line)
		  (zerop (line-length buffer-line)))
	      (null file-line))
	 nil)
      (when (or (null buffer-line)
		(null file-line)
		(string/= (line-string buffer-line) file-line))
	(return t)))))

(defun turn-auto-save-off (buffer)
  (setf (buffer-minor-mode buffer "Save") nil)
  ;;
  ;; William's personal hack
  (when (getstring "Ckp" *mode-names*)
    (setf (buffer-minor-mode buffer "Ckp") nil)))

(defun vc-make-modeline-string (vc-info)
  (format nil "[~A~@[:~A~]~@[:~A~]] "
	  (vc-info-type vc-info)
	  (vc-info-version vc-info)
	  (ecase (vc-info-status vc-info)
	    (:out-of-date "OLD")
	    (:locked "LOCK")
	    (:unlocked "RCS")
	    ((nil)))))

(defun vc-continue (vc-info &rest args)
  (declare (ignore args))
  (message "Empty operation for ~A." (vc-info-type vc-info)))


;;;; Commit commands.

(defcommand "VC Commit Buffer File" (p)
  "Commit the file in the current buffer.  With a prefix keep any lock."
  "Commit the file in the current buffer.  If P is true keep any lock."
  (let* ((buffer (current-buffer))
	 (pathname (current-buffer-pathname))
	 (vc-info (current-vc-info)))
    (when (buffer-modified buffer)
      (save-file-command nil))
    (funcall (vc-info-commit-fun vc-info)
	     vc-info buffer pathname nil p)
    (when (member buffer *buffer-list*)
      ;; If the buffer still exists ensure it is up to date with the file.
      (visit-file-command nil pathname buffer))))

(defcommand "VC Commit File" (p &optional pathname files)
  "Commit a prompted file.  With a prefix keep any lock."
  "Commit a prompted file.  If P is true keep any lock.  If Pathname is
   true commit Pathname instead.  If Pathname and Files are true commit the
   files listed in Files which are rooted in directory Pathname."
  (let* ((pathname (or pathname
		       (prompt-for-file :prompt "File to commit: "
					:default
					(buffer-default-pathname
					 (current-buffer))
					:must-exist nil)))
	 (vc-info (make-vc-info (if (listp pathname)
				    (car pathname)
				    pathname))))
    (funcall (vc-info-commit-fun vc-info)
	     vc-info nil pathname files p)))


;;;; Update commands.

(defcommand "VC Update Buffer File" (p)
  "Update the file in the current buffer.  With a prefix, lock the file."
  "Update the file in the current buffer.  If P is true, lock the file."
  (let* ((buffer (current-buffer))
	 (pathname (current-buffer-pathname))
	 (point (current-point))
	 (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (when (buffer-modified buffer)
      (or (prompt-for-y-or-n :prompt "Buffer is modified, overwrite? ")
	  (editor-error "Update canceled.")))
    (setf (buffer-modified buffer) nil)
    (let ((vc-info (current-vc-info)))
      (setf pathname (funcall (vc-info-update-fun vc-info)
			      vc-info buffer pathname p nil)))
    (when p
      (setf (buffer-writable buffer) t)
      (message "Buffer is now writable."))
    (visit-file-command nil pathname)
    (or (line-offset point lines) (buffer-end point))))

(defcommand "VC Update File" (p &optional pathname files (find t))
  "Attempt to update a prompted file.  With a prefix, lock the file."
  "Attempt to update a prompted file.  If P is true, lock the file."
  (let* ((pathname (or pathname
		       (prompt-for-file :prompt "File to update: "
					:default (buffer-default-pathname
						  (current-buffer))
					:must-exist nil)))
	 (vc-info (make-vc-info pathname)))
    (setf pathname (funcall (vc-info-update-fun vc-info)
			    vc-info nil pathname files p nil))
    (if find (find-file-command nil pathname))))


;;;; Last command output.

(defcommand "Last VC Command Output" (p)
  "Pop-up the full output of the last version control command."
  "Pop-up the full output of the last version control command."
  (declare (ignore p))
  (or (and *last-vc-command-name* *last-vc-command-output-string*)
      (editor-error "Yet to execute a VC command."))
  (with-pop-up-display (s :buffer-name "VC Command Output")
    (format s "Output from ``~A~A'':~%~%"
	    *last-vc-command-name*
	    *last-vc-command-args*)
    (write-line *last-vc-command-output-string* s)))


;;;; Locking commands.

(defcommand "VC Toggle Buffer File Lock" (p)
  "Toggle the lock on the file in the current buffer."
  "Toggle the lock on the file in the current buffer."
  (declare (ignore p))
  (let* ((pathname (current-buffer-pathname))
	 (buffer (current-buffer))
	 (vc-info (make-vc-info pathname)))
    (funcall (vc-info-lock-fun vc-info)
	     vc-info buffer pathname)))

(defcommand "VC Toggle File Lock" (p)
  "Toggle the lock on a prompted file."
  "Toggle the lock on a prompted file."
  (declare (ignore p))
  (let* ((pathname (prompt-for-file :prompt "Toggle lock on: "
				    :default (buffer-default-pathname
					      (current-buffer))
				    :must-exist nil))
	 (vc-info (make-vc-info pathname)))
    (funcall (vc-info-lock-fun vc-info)
	     vc-info nil pathname)))


;;;; Log commands.

(defun get-log-buffer ()
  (let ((buffer (getstring (value vc-log-entry-buffer) *buffer-names*)))
    (or buffer
	(progn
	  (setf buffer (make-buffer (value vc-log-entry-buffer)
				    :modes '("Fundamental" "VC-Log")))
	  (turn-auto-save-off buffer)
	  (invoke-hook vc-log-buffer-hook buffer)))
    buffer))

(defcommand "VC Buffer File Log Entry" (p)
  "Buffer the version control log for the current file."
  "Buffer the version control log for the current file."
  (declare (ignore p))
  (let ((buffer (get-log-buffer))
	(pathname (current-buffer-pathname)))
    (delete-region (buffer-region buffer))
    (message "Extracting log info ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (current-vc-info)))
	(funcall (vc-info-insert-log-fun vc-info)
		 vc-info
		 (make-hemlock-output-stream mark)
		 (namestring pathname))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))

(defcommand "VC File Log Entry" (p &optional file)
  "Buffer the version control log for a prompted file."
  "Buffer the version control log for a prompted file."
  (declare (ignore p))
  (let ((file (or file
		  (prompt-for-file :prompt "Show log for file: "
				   :default (buffer-default-pathname
					     (current-buffer))
				   :must-exist nil)))
	(buffer (get-log-buffer)))
    (delete-region (buffer-region buffer))
    (message "Extracting log info ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (make-vc-info file)))
	(funcall (vc-info-insert-log-fun vc-info)
		 vc-info
		 (make-hemlock-output-stream mark)
		 (namestring file))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))

(defcommand "VC File Log Entry" (p &optional file)
  "Buffer the version control log for a prompted file."
  "Buffer the version control log for a prompted file."
  (declare (ignore p))
  (let ((file (or file
		  (prompt-for-file :prompt "Show log for file: "
				   :default (buffer-default-pathname
					     (current-buffer))
				   :must-exist nil)))
	(buffer (get-log-buffer)))
    (delete-region (buffer-region buffer))
    (message "Extracting log info ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (make-vc-info file)))
	(funcall (vc-info-insert-log-fun vc-info)
		 vc-info
		 (make-hemlock-output-stream mark)
		 (namestring file))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))


;;;; Comparison commands.

(defun get-comparison-buffer ()
  (let ((buffer (getstring (value vc-comparison-buffer) *buffer-names*)))
    (or buffer
	(progn
	  (setf buffer (make-buffer (value vc-comparison-buffer)
				    :modes '("Fundamental" "VC-Log")))
	  (turn-auto-save-off buffer)))
    buffer))

(defcommand "VC Compare Buffer File" (p)
  "Compare buffered file to repository."
  "Compare buffered file to repository."
  (declare (ignore p))
  (let ((buffer (get-comparison-buffer))
	(pathname (current-buffer-pathname)))
    (delete-region (buffer-region buffer))
    (message "Comparing to repository ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (current-vc-info)))
	(funcall (vc-info-compare-fun vc-info)
		 vc-info
		 (make-hemlock-output-stream mark)
		 (namestring pathname))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)
    (message "Done.")))

(defcommand "VC Compare File" (p &optional file)
  "Compare a prompted file to the version in the repository."
  "Compare a prompted file to the version in the repository."
  (declare (ignore p))
  (let ((file (or file
		  (prompt-for-file :prompt "Compare file: "
				   :default (buffer-default-pathname
					     (current-buffer))
				   :must-exist nil)))
	(buffer (get-comparison-buffer)))
    (delete-region (buffer-region buffer))
    (message "Comparing to repository ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (make-vc-info file)))
	(funcall (vc-info-compare-fun vc-info)
		 vc-info
		 (make-hemlock-output-stream mark)
		 (namestring file))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)
    (message "Done.")))


;;;; VC Info updating.

(defhvar "VC Info"
  "VC information for this buffer."
  :value nil)

(defun update-buffer-vc-info (buffer &optional existp)
  (declare (ignore existp))
  (or (editor-bound-p 'vc-info :buffer buffer)
      (defhvar "VC Info"
	"VC information for this buffer."
	:buffer buffer
	:value nil))
  (setf (variable-value 'vc-info :buffer buffer)
	(make-vc-info (buffer-pathname buffer)))
  (hi::update-modelines-for-buffer buffer))
;;;
(add-hook read-file-hook 'update-buffer-vc-info)
(add-hook write-file-hook 'update-buffer-vc-info)

(defcommand "VC Update All VC Info Variables" (p)
  "Update the ``VC Info'' variable for all buffers."
  "Update the ``VC Info'' variable for all buffers."
  (declare (ignore p))
  (dolist (buffer *buffer-list*)
    (update-buffer-vc-info buffer))
  (dolist (window *window-list*)
    (update-modeline-fields (window-buffer window) window)))

(defun vc-action-hook (buffer pathname)
  (flet ((update-buffers (pathname)
	   (let ((pathname (probe-file pathname)))
	     (when pathname
	       (dolist (buffer *buffer-list*)
		 (let ((buffer-pathname (buffer-pathname buffer)))
		   (when (equal pathname buffer-pathname)
		     (update-buffer-vc-info buffer))))))))
    (cond (buffer
	   (update-buffer-vc-info buffer))
	  ((listp pathname)
	   (dolist (name pathname)
	     (update-buffers name)))
	  (t
	   (update-buffers pathname)))))
;;;
(add-hook vc-commit-file-hook 'vc-action-hook)
(add-hook vc-update-file-hook 'vc-action-hook)
(add-hook vc-lock-file-hook 'vc-action-hook)


;;;; Modeline field.

(make-modeline-field
 :name :vc-status :replace t
 :function #'(lambda (buffer window)
	       (declare (ignore window))
	       (let ((vc-info))
		 (if (and (editor-bound-p 'vc-info :buffer buffer)
			  (setq vc-info (variable-value 'vc-info
							:buffer buffer)))
		     (let ((fun (vc-info-modeline-string-fun vc-info)))
		       (if fun (funcall fun vc-info) ""))
		     ""))))

(let ((third (nthcdr 2 (value default-modeline-fields))))
  (setf (cdr third)
	(cons (modeline-field :vc-status) (cdr third))))


;;;; CVS.

(defun cvs-insert-log (vc-info stream pathname)
  "Insert in Stream the log for the file Pathname."
  (declare (ignore vc-info))
  (in-directory pathname
    (do-command t "cvs" (list "log" (file-namestring pathname))
		:output stream)))

(defun cvs-compare (vc-info stream pathname)
  "Insert in Stream a comparison of Pathname and the repository version."
  (declare (ignore vc-info))
  (in-directory pathname
    (do-command nil "cvs" (list "diff" (file-namestring pathname))
		:output stream)))

(defvar *cvs-update-file-recurse* nil
  "If true cvs-update-file will recurse into subdirectories when updating a
   directory.")

(defun cvs-update-file (vc-info buffer pathname files lock always-overwrite-p)
  "Update Pathname from CVS.  If Pathname and Files is nil update the
   directory (recursively if *cvs-update-file-recurse* is true).  If Files
   is true then update the list in Files instead."
  (declare (ignore vc-info lock always-overwrite-p))
  (let* ((pn-len (length (unix-namestring pathname)))
	 (names (if files
		    (mapcar (lambda (file)
			      (subseq (unix-namestring file) pn-len))
			    files)
		    (list (file-namestring pathname)))))
    (if files
	(message "Updating ~A: ~A ..." pathname
		 ;; FIX
		 (apply 'concatenate 'simple-string
			(mapcan (lambda (file) (list file " "))
				names)))
	(message "Updating ~A ..." pathname))
    (in-directory pathname
      (do-command t "cvs" (if (car names)
			      (cons "update" names)
			      (if *cvs-update-file-recurse*
				  (list "update" ".")
				  (list "update" "-l" ".")))))
    (invoke-hook vc-update-file-hook buffer pathname)
    (message "Done.")
    (car names)))

(defvar *vc-log-history* (make-ring 70)
  "Previously input VC logs.")

(defvar *vc-log-history-pointer* 0
  "Current position during a historical exploration.")

(defun cvs-commit (vc-info buffer pathname files keep-lock)
  "Commit Pathname into CVS repository, offering to add the file if
   required.  If Files is true commit instead the absolute pathnames listed
   by Files, which are all rooted in directory Pathname."
  (declare (ignore keep-lock))
  (let ((old-buffer (current-buffer))
	(allow-delete nil)
	(descr-buffer nil)
	(log-buffer nil))
    ;; Add the file if required.
    (or files
	(and vc-info (vc-info-version vc-info))
	(if (prompt-for-y-or-n
	     :prompt (format nil "Add ~A to CVS? " (file-namestring pathname))
	     :default t)
	    (unwind-protect
		(when (block in-recursive-edit
			(setf descr-buffer
			      (make-unique-buffer
			       (format nil "CVS Description of ~S"
				       (file-namestring pathname))
			       :modes '("Text" "Fill" "Spell" "VC-Log-Entry")
			       :delete-hook
			       (list #'(lambda (buffer)
					 (declare (ignore buffer))
					 (or allow-delete
					     (return-from in-recursive-edit t))))))
			(turn-auto-save-off descr-buffer)
			(let ((string (prompt-in-buffer descr-buffer)))
			  (message "Adding ~A ..." (namestring pathname))
			  (in-directory pathname
			    (do-command t "cvs"
					(list "add" "-m"
					      string
					      (file-namestring pathname)))))
			nil)
		  (editor-error "Add and commit canceled."))
	      (when (member old-buffer *buffer-list*)
		(change-to-buffer old-buffer))
	      (setf allow-delete t)
	      (delete-buffer-if-possible descr-buffer))
	    (editor-error "Commit canceled.")))
    ;; Commit the file(s).
    (let* ((pn-len (length (unix-namestring pathname)))
	   (names (if files
		      (mapcar (lambda (file)
				(subseq (unix-namestring file) pn-len))
			      files)
		      (list (file-namestring pathname))))
	   (log-name (format nil "CVS Log Entry for ~:[~S~;~S ...~]"
			     files
			     (if files
				 (file-namestring (car names))
				 (file-namestring pathname))))
	   (message (if files
			(format nil "Committing ~A: ~A ..." pathname
				;; FIX
				(apply 'concatenate 'simple-string
				       (mapcan (lambda (file) (list file " "))
					       names)))
			(format nil "Committing ~A ..." pathname)))
	   (allow-delete nil))
      (unwind-protect
	  (when (block in-recursive-edit
		  (setf log-buffer
			(make-unique-buffer
			 log-name
			 :modes '("Text" "Fill" "Spell" "VC-Log-Entry")
			 :delete-hook
			 (list #'(lambda (buffer)
				   (declare (ignore buffer))
				   (or allow-delete
				       (return-from in-recursive-edit t))))))
		  (turn-auto-save-off log-buffer)
		  (let ((string (prompt-in-buffer log-buffer)))
		    (message message)
		    (in-directory pathname
		      (do-command t "cvs"
				  (append (list "commit" "-m"
						string names)))))
		  (invoke-hook vc-commit-file-hook buffer names)
		  nil)
	    (editor-error "CVS commit canceled."))
	(when (member old-buffer *buffer-list*)
	  (change-to-buffer old-buffer))
	(setf allow-delete t)
	(delete-buffer-if-possible log-buffer)))))

(defun cvs-make-modeline-string (vc-info)
  (let ((version (vc-info-version vc-info)))
    (if version
	(format nil "[~A~@[:~A~]~@[:~A~]] "
		(vc-info-type vc-info)
		version
		(case (vc-info-status vc-info)
		  (:modified "*")
		  (:committed "C")
		  (:merged "M")
		  (:needs-update "NU")
		  (:needs-merge "NM")))
	"")))

(defun parse-cvs-module (cvs-dir)
  (let ((file (merge-pathnames "Repository" cvs-dir)))
    (with-open-file (stream file :direction :input
			         :if-does-not-exist :error)
      (read-line stream))))

(defun parse-cvs-root (cvs-dir)
  (let ((file (merge-pathnames "Root" cvs-dir)))
    (with-open-file (stream file :direction :input
			         :if-does-not-exist :error)
      (read-line stream))))

(defun parse-cvs-entry (entry)
  "Return the version and date from Entry."
  (let ((strings (split entry #\/)))
    (when (and strings (> (length strings) 3))
      (values (caddr strings) (cadddr strings)))))

(defun parse-cvs-entries (cvs-dir pathname)
  "Return the version and date of Pathname, according to Cvs-dir."
  (let* ((file (merge-pathnames "Entries" cvs-dir))
	 (name (file-namestring pathname)))
    (with-open-file (stream file :direction :input
			    :if-does-not-exist :error)
      (loop for line = (read-line stream nil) while line do
	(let ((strings (split line #\/)))
	  (when (and strings (> (length strings) 3))
	    (if (string= (cadr strings) name)
		(return-from parse-cvs-entries
			     (values (caddr strings) (cadddr strings))))))))))

(defun cvs-version-> (version1 version2)
  "Return true if Version1 is later in time than Version2."
  (let ((version1-strings (split version1 #\.))
	(version2-strings (split version2 #\.)))
    (loop for string1 = (car version1-strings)
          for string2 = (car version2-strings)
          while (and string1 (plusp (length string1))
		     string2 (plusp (length string2)))
          do
      (if (> (parse-integer string1) (parse-integer string2))
	  (return-from cvs-version-> t))
      (pop version1-strings)
      (pop version2-strings))
    nil))

(defun maybe-make-cvs-info (pathname connect)
  (let* ((dir (directory-namestring pathname))
	 (cvs-dir
	  (format nil "~A/"
		  (merge-pathnames "CVS" dir)))
	 (stream (make-string-output-stream)))
    (when (probe-file cvs-dir)
      (multiple-value-bind (rev-version rev-date)
			   (parse-cvs-entries cvs-dir pathname)
	(let ((vc-info (%make-vc-info :cvs
				      #'cvs-insert-log
				      #'cvs-update-file
				      #'cvs-commit
				      #'cvs-make-modeline-string)))
	  (setf (vc-info-compare-fun vc-info) #'cvs-compare)
	  (setf (vc-info-module vc-info)
		(parse-cvs-module cvs-dir))
	  (setf (vc-info-repository vc-info)
		(parse-cvs-root cvs-dir))
	  (setf (vc-info-version vc-info) rev-version)
	  (let ((entry (when connect
			 (in-directory dir
			   (do-command nil "cvs"
				       `("ls" "-e"
					 ,(file-namestring pathname))
				       :output stream))
			 (get-output-stream-string stream))))
	    (multiple-value-bind (cvs-version cvs-date)
				 (parse-cvs-entry entry)
	      (declare (ignore cvs-date))
	      (setf (vc-info-status vc-info)
; FIX
; /test/1.2/Result of merge+Sat Oct 28 21:46:53 2006//
		    (if (and (> (length rev-date) 14)
			     (string= rev-date "Result of merge" :end1 15))
			:merged
			(let* ((date (file-write-date pathname))
			       (rev-date (parse-time rev-date))
; 			   (cvs-date (parse-time cvs-date))
; 			   (needs-update (and cvs-date rev-date
; 					      (> cvs-date rev-date))))
			       (needs-update (and cvs-version rev-version
						  (cvs-version-> cvs-version
								 rev-version))))
			  (cond
			   ((and date rev-date (> date rev-date))
			    (if needs-update :needs-merge :modified))
			   (needs-update :needs-update))))))
	    vc-info))))))


;;;; RCS.

(defun rcs-insert-log (vc-info stream pathname)
  (declare (ignore vc-info))
  (in-directory pathname
    (do-command t "rlog" (list (file-namestring pathname)) :output stream)))

(defvar *translate-file-names-before-locking* nil)

(defun maybe-rcs-update (vc-info buffer pathname files lock
				 always-overwrite-p)
  ;; FIX should use one cmd
  (let ((files (or files (list pathname))))
    (dolist (pathname files)
      (when (and lock *translate-file-names-before-locking*)
	(multiple-value-bind (unmatched-dir new-dirs file-name)
			     (maybe-translate-definition-file pathname)
	  (when new-dirs
	    (let ((new-name (translate-definition-file unmatched-dir
						       (car new-dirs)
						       file-name)))
	      (when (probe-file (directory-namestring new-name))
		(setf pathname new-name))))))
      (cond
       ((if always-overwrite-p
	    nil
	    (let ((pn (probe-file pathname)))
	      (and pn (ext:file-writable pn))))
	;; File exists and is writable so check and see if the user really
	;; wants to check it out.
	(command-case (:prompt
		       (format nil "The file ~A is writable.  Overwrite? "
			       (file-namestring pathname))
		       :help
		       "Type one of the following single-character commands:")
	  ((:yes :confirm)
	   "Overwrite the file."
	   (rcs-update-file vc-info buffer pathname lock))
	  (:no
	   "Don't check it out after all.")
	  ((#\r #\R)
	   "Rename the file before checking it out."
	   (let ((new-pathname (prompt-for-file
				:prompt "New Filename: "
				:default (buffer-default-pathname
					  (current-buffer))
				:must-exist nil)))
	     (rename-file pathname new-pathname)
	     (rcs-update-file vc-info buffer pathname lock)))))
       (t
	(rcs-update-file vc-info buffer pathname lock)))
      pathname)))

(defun rcs-update-file (vc-info buffer pathname lock)
  (declare (ignore vc-info))
  (message "Updating ~A~:[~; with a lock~] ..." (namestring pathname) lock)
  (in-directory pathname
    (let* ((file (file-namestring pathname))
	   (backup (if (probe-file file)
		       (lisp::pick-backup-name file))))
      (when backup (rename-file file backup))
      (do-command t "co" `(,@(if lock '("-l")) ,file))
      (invoke-hook vc-update-file-hook buffer pathname)
      (when backup (delete-file backup)))))

(defun rcs-commit (vc-info buffer pathname files keep-lock)
  (declare (ignore vc-info))
  (let ((old-buffer (current-buffer))
	(allow-delete nil)
	(log-buffer nil))
    (let* (
; FIX for doing all in one cmd
;          (pn-len (length (unix-namestring pathname)))
; 	   (files (if files
; 		      (mapcar (lambda (file)
; 				(subseq (unix-namestring file) pn-len))
; 			      files)
; 		      (list (file-namestring pathname))))
	   (names (or files
		      (list (file-namestring pathname))))
	   (log-name (format nil "RCS Log Entry for ~:[~S~;~S ...~]"
			     files
			     (if files
				 (file-namestring (car names))
				 (file-namestring pathname))))
	   (message (if files
			(format nil "Checking in ~A: ~A ~@[keeping the lock ~]..."
				pathname
				;; FIX
				(apply 'concatenate 'simple-string
				       (mapcan (lambda (file) (list file " "))
					       names))
				keep-lock)
			(format nil "Checking in ~A ~@[keeping the lock ~]..."
				pathname keep-lock))))
      (unwind-protect
	  (when (block in-recursive-edit
		  (do ((i 0 (1+ i)))
		      ((not (null log-buffer)))
		    (setf log-buffer
			  (make-buffer
			   log-name
			   :modes '("Text" "Fill" "Spell")
			   :delete-hook
			   (list #'(lambda (buffer)
				     (declare (ignore buffer))
				     (or allow-delete
					 (return-from in-recursive-edit
						      t)))))))
		  (turn-auto-save-off log-buffer)
		  (prompt-in-buffer log-buffer)

		  (message message)
		  ;; FIX should do in one cmd
		  (dolist (file names)
		    (let ((log-stream (make-hemlock-region-stream
				       (buffer-region log-buffer))))
		      (sub-rcs-commit-file file buffer keep-lock log-stream)))
		  (invoke-hook vc-commit-file-hook buffer pathname)
		  nil)
	    (editor-error "RCS commit canceled."))
	(when (member old-buffer *buffer-list*)
	  (change-to-buffer old-buffer))
	(setf allow-delete t)
	(delete-buffer-if-possible log-buffer)))))

(defun sub-rcs-commit-file (pathname buffer keep-lock log-stream)
  (let* ((filename (file-namestring pathname))
	 (rcs-filename (concatenate 'simple-string
				    "./RCS/" filename ",v"))
	 (keep-working-copy (or keep-lock
				(if (editor-bound-p
				     'vc-keep-around-after-unlocking)
				    (value
				     vc-keep-around-after-unlocking)))))
    (in-directory pathname
      (do-command t "ci" `(,@(if keep-lock '("-l"))
			     ,@(if keep-working-copy '("-u"))
			     ,filename)
		  :input log-stream)
      (if keep-working-copy
	  ;; Set the times on the user's file to be equivalent to those of
	  ;; the RCS file.
	  #-(or hpux svr4)
	  (multiple-value-bind
	      (dev ino mode nlink uid gid rdev size atime mtime)
	      (unix:unix-stat rcs-filename)
	    (declare (ignore mode nlink uid gid rdev size))
	    (cond (dev
		   (multiple-value-bind
		       (wonp errno)
		       (unix:unix-utimes filename atime 0 mtime 0)
		     (or wonp
			 (editor-error "unix:unix-utimes failed: ~A"
				       (unix:get-unix-error-msg errno)))))
		  (t
		   (editor-error "unix:unix-stat failed: ~A"
				 (unix:get-unix-error-msg ino)))))
	  (delete-buffer-if-possible buffer)))))

(defun pick-temp-file (defaults)
  (let ((index 0))
    (loop
      (let ((name (merge-pathnames (format nil ",vctmp-~D" index)
				   defaults)))
	(cond ((probe-file name)
	       (incf index))
	      (t
	       (return name)))))))

(defun rcs-toggle-file-lock (vc-info buffer pathname)
  (case (vc-info-status vc-info)
    (:locked
     (message "Releasing lock on ~A ..." (namestring pathname))
     (let ((file (file-namestring pathname)))
       (in-directory pathname
	 (do-command t "rcs" `("-u" ,file))
	 (multiple-value-bind (success dev ino mode)
			      (unix:unix-stat file)
	   (declare (ignore ino))
	   (cond (success
		  (unix:unix-chmod file
				   (logand mode (logcom unix:writeown))))
		 (t
		  (editor-error
		   "unix:unix-stat failed in rcs-toggle-file-lock: ~A"
		   (unix:get-unix-error-msg dev)))))
	 (invoke-hook vc-lock-file-hook buffer pathname)))
     (when buffer
       (setf (buffer-writable buffer) nil)
       (message "Buffer is now read only.")))
    (t
     (if buffer
	 (let ((name (pick-temp-file "/tmp/")))
	   (rcs-lock-file buffer pathname)
	   (unwind-protect
	       (progn
		 (in-directory pathname
		   (do-command t "co" `("-p" ,(file-namestring pathname))
			       :output (namestring name)))
		 (when (buffer-different-from-file buffer name)
		   (message
		    "RCS file is different; be sure to merge in your changes."))
		 (setf (buffer-writable buffer) t)
		 (message "Buffer is now writable."))
	     (when (probe-file name)
	       (delete-file name))))
	 (rcs-lock-file nil pathname)))))

(defun rcs-lock-file (buffer pathname)
  (message "Locking ~A ..." (namestring pathname))
  (in-directory pathname
    (let ((file (file-namestring pathname)))
      (do-command t "rcs" `("-l" ,file))
      (multiple-value-bind (won dev ino mode) (unix:unix-stat file)
	(declare (ignore ino))
	(cond (won
	       (unix:unix-chmod file (logior mode unix:writeown)))
	      (t
	       (editor-error "unix:unix-stat failed in rcs-lock-file: ~A"
			     (unix:get-unix-error-msg dev)))))))
  (invoke-hook vc-lock-file-hook buffer pathname))

(defun maybe-make-rcs-info (pathname connect)
  (declare (ignore connect))
  ;;; FIX Probably needs more, at least for branched files.
  (let* ((directory (directory-namestring pathname))
	 (filename (file-namestring pathname))
	 (rcs-dir (concatenate 'simple-string directory
			       "RCS/"))
	 (rcs-file (concatenate 'simple-string directory
				"RCS/" filename ",v")))
    (if (probe-file rcs-dir)
	;; Pathname is in an RCS directory.
	(let ((vc-info (%make-vc-info :rcs
				      #'rcs-insert-log
				      #'maybe-rcs-update
				      #'rcs-commit
				      #'vc-make-modeline-string))
	      (probe-file (probe-file pathname)))
	  (setf (vc-info-lock-fun vc-info) #'rcs-toggle-file-lock)
	  (if (probe-file rcs-file)
	      ;; Pathname is an RCS file.
	      (setf (vc-info-status vc-info)
		    (cond ((and probe-file (file-writable probe-file))
			   :locked)
			  ((or (not probe-file)
			       (< (file-write-date pathname)
				  (file-write-date rcs-file)))
			   :out-of-date)
			  (t
			   :unlocked))))
	  vc-info))))
