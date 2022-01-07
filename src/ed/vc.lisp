;;; Version control interface.
;;;
;;; To extend this to other version control systems add maker functions to
;;; *vc-info-makers*.

(in-package "ED")


;;;; Structure.

(defevar "VC Keep Around After Unlocking"
  "If true keep the working file around after unlocking
   it.  When NIL, the working file and buffer are deleted."
  :value t)

(defevar "VC Commit File Hook"
  "VC Commit File Hook")

(defevar "VC Update File Hook"
  "VC Update File Hook")

(defevar "VC Lock File Hook"
  "VC Lock File Hook")

(defevar "VC Log Buffer Hook"
  "Version control log buffer hook.")

(defevar "VC Log Entry Buffer"
  "Name in which to buffer version control log entries."
  :value "VC Log")

(defevar "VC Comparison Buffer"
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
  ;; Type of version control the file is under: :svn, :cvs, :rcs, or :new.
  ;; :new indicates that version control still needs to be set up in the
  ;; directory.
  type
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

(defvar *vc-info-makers* '(maybe-make-svn-info
			   maybe-make-cvs-info
			   maybe-make-rcs-info)
  "List of functions called in make-vc-info to make a vc-info structure for
   a buffer.  The functions are called on the buffer pathname in turn until
   one returns a true value.")

(defvar *last-vc-command-name* ())
(defvar *last-vc-command-args* ())
(defvar *last-vc-command-output-string* ())
(defvar *vc-output-stream* (make-string-output-stream))

(defvar *vc-update-file-recurse* ()
  "If true svn-update-file will recurse into subdirectories when updating a
   directory.")


;;;; VC Log and VC Log modes.

(defun highlight-vc-log-line (line chi-info)
  (case (next-character (mark line 0))
    (#\- (chi-mark line 0 *variable-font* :variable chi-info))))

(defun highlight-vc-log-buffer (buffer)
  (highlight-chi-buffer buffer highlight-vc-log-line))

(defun highlight-visible-vc-log-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-vc-log-line))

(defun setup-vc-log-mode (buffer)
  (highlight-visible-vc-log-buffer buffer)
  (pushnew '("VC Log" () highlight-visible-vc-log-buffer) *mode-highlighters*))

(defmode "VC Log" :major-p nil
  :short-name "VC-Log"
  :precedence 5.0
  :setup-function 'setup-vc-log-mode
  :documentation
  "Mode for viewing version control change logs.")

(defmode "VC Log Entry" :major-p ()
  :short-name "VC-Log-Entry"
  :precedence 5.0
  :documentation
  "Mode for entering version control change logs.")


;;;; VC Compare mode.

(defun highlight-vc-cmp-line (line chi-info)
  (case (next-character (mark line 0))
    (#\- (chi-mark line 0 *comment-font* :comment chi-info))
    (#\+ (chi-mark line 0 *string-font* :string chi-info))
    (#\= (chi-mark line 0 *special-form-font* :special-form chi-info))
    ;(#\@ (chi-mark line 0 *original-font* :variable chi-info))
    (#\@ (chi-mark line 0 *original-font* :special-form chi-info))))

(defun highlight-vc-cmp-buffer (buffer)
  (highlight-chi-buffer buffer highlight-vc-cmp-line))

(defun highlight-visible-vc-cmp-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-vc-cmp-line))

(defun setup-vc-cmp-mode (buffer)
  (highlight-visible-vc-cmp-buffer buffer)
  (pushnew '("VC Comparison" t highlight-visible-vc-cmp-buffer)
	   *mode-highlighters*))

(defmode "VC Comparison" :major-p t
  :short-name "VC-Cmp"
  :setup-function 'setup-vc-cmp-mode
  :documentation
  "Mode for viewing version control file comparisons.")

(defcommand "Next VC Comparison" (p)
  "Move point to the next section of the comparison.  With a prefix move
   that many sections."
  (dotimes (time (or p 1))
    (with-mark ((mark (current-point)))
      (while* ((mark (line-offset mark 1 0) (line-offset mark 1 0)))
	      (mark)
	(let ((char (next-character mark)))
	  (and char (char= char #\@) (return))))
      (move-mark (current-point) mark))))

(defcommand "Previous VC Comparison" (p (point (current-point)))
  "Move point to the next section of the comparison.  With a prefix move
   that many sections."
  (dotimes (time (or p 1))
    (with-mark ((mark point))
      (while* ((mark (line-offset mark -1 0) (line-offset mark -1 0)))
	      (mark)
	(let ((char (next-character mark)))
	  (and char (char= char #\@) (return))))
      (move-mark point mark))))

(defcommand "Previous VC File" (p (point (current-point)))
  "Move point to the next file in the comparison.  With a prefix move that
   many files."
  (dotimes (time (or p 1))
    (with-mark ((mark point))
      (while* ((mark (line-offset mark -1 0) (line-offset mark -1 0)))
	      (mark)
	(let ((char (next-character mark)))
	  (and char (char= char #\=) (return))))
      (move-mark point mark))))

(defcommand "Next VC File" (p)
  "Move point to the next file in the comparison.  With a prefix move that
   many files."
  (dotimes (time (or p 1))
    (with-mark ((mark (current-point)))
      (while* ((mark (line-offset mark 1 0) (line-offset mark 1 0)))
	      (mark)
	(let ((char (next-character mark)))
	  (and char (char= char #\=) (return))))
      (move-mark (current-point) mark))))

(defcommand "Edit VC Comparison" (p)
  "Edit the file associated with the current point of the comparison.  With
   a prefix edit the file in the next window."
  (let* ((point (current-point))
	 (mark (copy-mark point)))
    (line-start mark)
    (let ((char (next-character mark))
	  (line-number 0))
      ;; Calculate the line number of the start of the comparison section.
      (or (and char (char= char #\@))
	  (previous-vc-comparison-command () mark))
      (if (mark= mark (buffer-start-mark (current-buffer)))
	  (progn
	    (or (and (at* "Index: " mark)
		     (character-offset mark 7))
		(editor-error
		 "Failed to figure out which file is under comparison."))
	    (setq line-number 1))
	  (progn
	    (or (and (find-character mark #\+)
		     (character-offset mark 1)
		     (setq line-number
			   (with-input-from-region
			       (in (region mark
					   (let ((line (mark-line mark)))
					     (mark line (line-length line)))))
			     (read in)))
		     (integerp line-number)
		     (>= line-number 0))
		(message "Failed to calculate line number, going to line 0."))
	    ;; Calculate the line number under point.
	    (or (eq (mark-line mark) (mark-line point))
		(progn
		  (line-offset mark 1)
		  (let ((region (region mark point)))
		    (do-region-lines (line region)
		      (case (char (line-string line) 0)
			(#\-)
			(t (incf line-number)))))))
	    ;; Figure out which file.
	    (or (and char (char= char #\=))
		(previous-vc-file-command () mark))
	    (or (and (line-offset mark -1 0)
		     (at* "Index: " mark)
		     (character-offset mark 7))
		(editor-error
		 "Failed to figure out which file is under comparison."))))
      ;; Edit the file.
      (let ((vc-compare-pathname (value vc-compare-pathname)))
	(if p
	    (if (eq (next-window (current-window)) (current-window))
		(split-window-command)
		(next-window-command)))
	(find-file-command ()
			   (merge-pathnames
			    (subseq (line-string (mark-line mark))
				    (mark-charpos mark))
			    vc-compare-pathname)))
      (go-to-absolute-line-command line-number)
      (refresh-screen-command))))

(defcommand "Edit VC Comparison Next Window" ()
  "Edit the file associated with the current point of the comparison in the
   next window."
  (edit-vc-comparison-command t))

(defun refresh-vc-comparison (buffer pathname &optional tag1 tag2)
  (delete-region (buffer-region buffer))
  (if (directory-namestring pathname)
      (setf (buffer-pathname buffer) (directory-namestring pathname)))
  (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
    (message "Comparing to repository...")
    (let* ((vc-info (current-vc-info))
	   (sss (make-editor-output-stream mark)))
      (funcall (vc-info-compare-fun vc-info)
	       vc-info
	       sss
	       (namestring pathname)
	       tag1 tag2)))
  (defevar "VC Compare Pathname"
    "Pathname of the file under comparison."
    :buffer buffer
    :value (truename pathname))
  (buffer-start (buffer-point buffer))
  (setf (buffer-modified buffer) ())
  (message "Comparing to repository... done."))

(defcommand "Refresh VC Comparison" ()
  "Refresh the version control comparison in the current buffer."
  (let* ((buffer (current-buffer))
	 (point (current-point))
	 (pos (count-lines (region (buffer-start-mark buffer) point))))
    (refresh-vc-comparison buffer (value vc-compare-pathname))
    (if (and (plusp pos)
	     (<= pos (count-lines (buffer-region buffer))))
	(line-offset point (1- pos))
	(buffer-end point))))


;;;; Interface (i.e. used in Dired).

(defun make-vc-info (pathname &optional connect)
  "Return a version-control-info struct made from Pathname.  If Connect is
   true then it is OK for the maker to access the repository."
  (dolist (maker *vc-info-makers*)
    (let ((vc-info (funcall maker pathname connect)))
      (if vc-info (return-from make-vc-info vc-info)))))

(defun make-new-vc-info ()
  "Return a version-control-info struct of type :new."
  (%make-vc-info :new () () () ()))


;;;; Helper functions.

(defun conflict-p (pathname)
  "Return t if there is a conflict in PATHNAME, else ()."
  (with-temp-buffer (buffer pathname)
    (do-buffer-lines (line buffer)
      (if (and (> (line-length line) 7)
	       (at* "<<<<<<<" (mark line 0)))
	  (return-from conflict-p t)))))

(defun version-> (version1 version2)
  "Return true if string $version1 is later in time than string $version2."
  (let ((version1-strings (split version1 #\.))
	(version2-strings (split version2 #\.)))
    (while ((string1 (car version1-strings) (car version1-strings))
	    (string2 (car version2-strings) (car version2-strings)))
	   ((and string1 (plusp (length string1))
		 string2 (plusp (length string2))))
      (if (> (parse-integer string1) (parse-integer string2))
	  (return-from version-> t))
      (pop version1-strings)
      (pop version2-strings))))

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

#|
(defmacro do-vc-command (error-on-error command &rest args)
  "Call run-program on Command and Args.  If error-on-error is true then
   throw an editor-error if the program exits with an error code."
  `(progn
     (setf *last-vc-command-name* ',command)
     (setf *last-vc-command-args*
	   (apply 'concatenate 'simple-string
		  (mapcar (lambda (arg) (format nil " ~A" (eval arg)))
			  ',args)))
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
|#

(defun do-vc-command (error-on-error command command-args &rest key-args)
  "Call run-program on Command and Args.  If error-on-error is true then
   throw an editor-error if the program exits with an error code."
  (setf *last-vc-command-name* command)
  (setf *last-vc-command-args*
	(apply 'concatenate 'simple-string
	       (mapcar (lambda (arg) (format () " ~A" arg))
		       command-args)))
  (get-output-stream-string *vc-output-stream*)
  (let ((process (apply #'ext:run-program
			command command-args
			:error *vc-output-stream*
			key-args)))
    (setf *last-vc-command-output-string*
	  (get-output-stream-string *vc-output-stream*))
    (case (ext:process-status process)
      (:exited
       (if error-on-error
	   (or (zerop (ext:process-exit-code process))
	       (editor-error
		"~A aborted with an error; ~
		 use the ``Last VC Command Output'' command for ~
		 more information" command))))
      (:signaled
       (editor-error "~A killed with signal ~A~@[ (core dumped)]."
		     command
		     (ext:process-exit-code process)
		     (ext:process-core-dumped process)))
      (t
       (editor-error "~S still alive?" process)))))

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
      (save-file-command))
    (funcall (vc-info-commit-fun vc-info)
	     vc-info buffer pathname nil p)
    (when (member buffer *buffer-list*)
      ;; If the buffer still exists ensure it is up to date with the file.
      (visit-file-command nil pathname buffer))))

(defcommand "VC Commit File" (p pathname files)
  "Commit a prompted file.  With a prefix keep any lock."
  "Commit a prompted file.  If P is true keep any lock.  If Pathname is
   true commit Pathname instead.  If Pathname and Files are true commit the
   files listed in Files which are rooted in directory Pathname."
  (if files (or pathname (editor-error "$pathname required for $files.")))
  (let* ((pathname (or pathname
		       (prompt-for-file :prompt "File to commit: "
					:default
					(buffer-default-pathname
					 (current-buffer))
					:must-exist nil)))
	 (vc-info (make-vc-info (if files
				    (car files)
				    (if (listp pathname)
					(car pathname)
					pathname)))))
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

(defcommand "VC Update File" (p pathname files (find t))
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

(defcommand "Last VC Command Output" ()
  "Pop-up the full output of the last version control command."
  (or (and *last-vc-command-name* *last-vc-command-output-string*)
      (editor-error "Yet to execute a VC command."))
  (with-pop-up-display (s :buffer-name "VC Command Output")
    (format s "Output from ``~A~A'':~%~%"
	    *last-vc-command-name*
	    *last-vc-command-args*)
    (write-line *last-vc-command-output-string* s)))


;;;; Locking commands.

(defcommand "VC Toggle Buffer File Lock" ()
  "Toggle the lock on the file in the current buffer."
  (let* ((pathname (current-buffer-pathname))
	 (buffer (current-buffer))
	 (vc-info (make-vc-info pathname)))
    (funcall (vc-info-lock-fun vc-info)
	     vc-info buffer pathname)))

(defcommand "VC Toggle File Lock" ()
  "Toggle the lock on a prompted file."
  (let* ((pathname (prompt-for-file :prompt "Toggle lock on: "
				    :default (buffer-default-pathname
					      (current-buffer))
				    :must-exist nil))
	 (vc-info (make-vc-info pathname)))
    (funcall (vc-info-lock-fun vc-info)
	     vc-info nil pathname)))


;;;; Log commands.

(defhistory *vc-log-history* *vc-log-history-pointer* 70)

(defun get-log-buffer ()
  (let ((buffer (getstring (value vc-log-entry-buffer) *buffer-names*)))
    (or buffer
	(progn
	  (setf buffer (make-buffer (value vc-log-entry-buffer)
				    :modes '("Fundamental" "VC Log")))
	  (turn-auto-save-off buffer)
	  (invoke-hook vc-log-buffer-hook buffer)))
    buffer))

(defcommand "VC Buffer File Log Entry" ()
  "Buffer the version control log for the current file."
  (let ((buffer (get-log-buffer))
	(pathname (current-buffer-pathname)))
    (delete-region (buffer-region buffer))
    (message "Extracting log info ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (current-vc-info)))
	(funcall (vc-info-insert-log-fun vc-info)
		 vc-info
		 (make-editor-output-stream mark)
		 (namestring pathname))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))

(defcommand "VC File Log Entry" (p file)
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
		 (make-editor-output-stream mark)
		 (namestring file))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) nil)))

(defcommand "VC File Log Entry" (p file)
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
		 (make-editor-output-stream mark)
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
				    :modes '("VC Comparison")))
	  (turn-auto-save-off buffer)))
    buffer))

(defcommand "VC Compare Buffer File" (p)
  "Compare buffered file to repository.  With a prefix prompt for versions
   to compare."
  "Compare buffered file to repository.  If P is true prompt for versions
   to compare."
  (let* ((buffer (get-comparison-buffer))
	 (pathname (or (current-buffer-pathname)
		       (editor-error "Buffer must have a file.")))
	 (vc-info (current-vc-info))
	 (tag1 (if p (prompt-for-string
		      :prompt "First version: "
		      :help "Version of first of files to compare."
		      :default (vc-info-version vc-info))))
	 (tag2 (if p (prompt-for-string
		      :prompt "Second version: "
		      :help "Version of second of files to compare."
		      :default (vc-info-version vc-info)))))
    (refresh-vc-comparison buffer pathname tag1 tag2)
    (change-to-buffer buffer)))

(defcommand "VC Compare File" (p file)
  "Compare a prompted file to the version in the repository."
  (declare (ignore p))
  (let ((file (or file
		  (prompt-for-file :prompt "Compare file: "
				   :default (buffer-default-pathname
					     (current-buffer))
				   :must-exist ())))
	(buffer (get-comparison-buffer)))
    (delete-region (buffer-region buffer))
    (message "Comparing to repository ...")
    (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
      (let ((vc-info (make-vc-info file)))
	(funcall (vc-info-compare-fun vc-info)
		 vc-info
		 (make-editor-output-stream mark)
		 (namestring file))))
    (defevar "VC Compare Pathname"
      "Pathname of the file under comparison."
      :buffer buffer
      :value (truename file))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (setf (buffer-modified buffer) ())
    (message "Done.")))


;;;; VC Info updating.

(defevar "VC Info"
  "VC information for this buffer.")

(defun update-buffer-vc-info (buffer &optional existp)
  (declare (ignore existp))
  (or (editor-bound-p 'vc-info :buffer buffer)
      (defevar "VC Info"
	"VC information for this buffer."
	:buffer buffer))
  (setf (variable-value 'vc-info :buffer buffer)
	(make-vc-info (buffer-pathname buffer)))
  (edi::update-modelines-for-buffer buffer))
;;;
(add-hook read-file-hook 'update-buffer-vc-info)
(add-hook write-file-hook 'update-buffer-vc-info)

(defcommand "VC Update All VC Info Variables" ()
  "Update the ``VC Info'' variable for all buffers."
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


;;;; Subversion (SVN).

(defun svn-insert-log (vc-info stream pathname)
  "Insert in Stream the log for the file Pathname."
  (declare (ignore vc-info))
  (in-directory pathname
    (do-vc-command t "svn" (list "log" (file-namestring pathname))
		   :output stream)))

(defun svn-compare (vc-info stream pathname &optional tag1 tag2)
  "Insert in Stream a comparison of Pathname and the repository version.
   If tag1 and tag2 are given compare those versions of Pathname."
  (declare (ignore vc-info))
  (in-directory pathname
    (do-vc-command () "svn"
		   (if tag1
		       (list "diff" "-r" (format () "~A:~A" tag1 tag2)
			     (file-namestring pathname))
		       (list "diff" (file-namestring pathname)))
		   :output stream)))

(defun svn-update-file (vc-info buffer pathname files lock always-overwrite-p)
  "Update Pathname from SVN.  If Pathname and Files is nil update the
   directory (recursively if *vc-update-file-recurse* is true).  If Files
   is true then update the list in Files instead."
  (declare (ignore vc-info lock always-overwrite-p))
  (let* ((pn-len (length (os-namestring pathname)))
	 (names (if files
		    (mapcar (lambda (file)
			      (subseq (os-namestring file) pn-len))
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
      (do-vc-command t "svn" (if (car names)
				 (cons "update" names)
				 (if *vc-update-file-recurse*
				     (list "update" ".")
				     (list "update" "-N" ".")))
		     :output *vc-output-stream*))
    (invoke-hook vc-update-file-hook buffer pathname)
    (message "Done.")
    (car names)))

(defun svn-commit (vc-info buffer pathname files keep-lock)
  "Commit $pathname into SVN repository, offering to add the file if
   required.  If $files is true commit instead the absolute pathnames
   listed by $files, which are all rooted in directory $pathname."
  (declare (ignore keep-lock))
  (let ((old-buffer (current-buffer))
	(log-buffer))
    ;; Add the file(s) if required.
    (collect ((new))
      (if files
	  (dolist (file files)
	    (let ((file (merge-pathnames file pathname)))
	      (let ((vc-info (make-vc-info file)))
		(or (and vc-info
			 (vc-info-version vc-info))
		    (new file)))))
	  (or (and vc-info
		   (vc-info-version vc-info))
	      (new pathname)))
      (when (new)
	(let ((files (mapcar #'namestring (new))))
	  (or (prompt-for-y-or-n
	       :prompt (format () "Add ~A to SVN? "
			       (if (cdr files)
				   "multiple files"
				   (car files)))
	       :default t)
	      (editor-error "Commit canceled."))
	  (when (progn
		  (message "Adding ~A ..." (if (cdr files)
					       "multiple files"
					       (car files)))
		  (in-directory pathname
		    (do-vc-command
		     t "svn"
		     (cons "add" files)
		     :output *vc-output-stream*))
		  ())
	    (editor-error "Add and commit canceled.")))))
    ;; Commit the file(s).
    (let* ((pn-len (length (os-namestring pathname)))
	   (names (if files
		      (mapcar (lambda (file)
				(subseq (os-namestring file) pn-len))
			      files)
		      (list (file-namestring pathname))))
	   (log-name (format nil "SVN Log Entry for ~:[~S~;~S ...~]"
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
	   (allow-delete))
      (unwind-protect
	  (when (block in-recursive-edit
		  (setf log-buffer
			(make-unique-buffer
			 log-name
			 :modes '("Text" "Fill" "Spell" "VC Log Entry")
			 :delete-hook
			 (list #'(lambda (buffer)
				   (declare (ignore buffer))
				   (or allow-delete
				       (return-from
					in-recursive-edit t))))))
		  (turn-auto-save-off log-buffer)
		  (let ((string (prompt-in-buffer log-buffer)))
		    (message message)
		    (in-directory pathname
		      (do-vc-command t "svn"
				     (append (list "commit" "-m" string)
					     names)
				     :output *vc-output-stream*)))
		  (invoke-hook vc-commit-file-hook buffer names)
		  nil)
	    (editor-error "SVN commit canceled."))
	(when (member old-buffer *buffer-list*)
	  (change-to-buffer old-buffer))
	(setf allow-delete t)
	(delete-buffer-if-possible log-buffer)))))

(defun svn-make-modeline-string (vc-info)
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

(defun parse-svn-module (svn-dir)
  (let ((file (merge-pathnames "entries" svn-dir))
	;(name (file-namestring pathname))
	)
    ;; FIX this reads the version
    (with-open-file (stream #| FIX |# (namestring file) :direction :input
			    :if-does-not-exist :error)
      (dotimes (time 3)
	(or (read-line stream ()) (return-from parse-svn-module)))
      (let ((line (read-line stream ())))
	(or line (return-from parse-svn-module))
	(read-from-string line ())))))

(defun parse-svn-root (svn-dir)
  (let* ((file (merge-pathnames "entries" svn-dir))
	 ;(name (file-namestring pathname))
	 )
    ;; FIX this reads the version
    (with-open-file (stream #| FIX |# (namestring file) :direction :input
			    :if-does-not-exist :error)
      (dotimes (time 3)
	(or (read-line stream ()) (return-from parse-svn-root)))
      (let ((line (read-line stream ())))
	(or line (return-from parse-svn-root))
	(read-from-string line ())))))

(defun parse-svn-entry (entry)
  "Return the version and date from Entry."
  (let ((strings (split entry #\/)))
    (when (and strings (> (length strings) 3))
      (values (caddr strings) (cadddr strings)))))
; (parse-svn-entry "   1809 mattm            1097 Nov 21 16:51 README")

(defun parse-svn-entries (svn-dir pathname)
  "Return the version and date of $pathname, according to $svn-dir."
  (let* ((file (merge-pathnames "entries" svn-dir))
	 (name (file-namestring pathname)))
    (with-open-file (stream #| FIX |# (namestring file) :direction :input
			    :if-does-not-exist :error)
      (dotimes (time 3)
	(or (read-line stream ()) (return-from parse-svn-entries)))
      (let ((line (read-line stream ())))
	(or line (return-from parse-svn-entries))
	(let ((version (string (read-from-string line ())))
	      (date (file-write-date (merge-pathnames
				      (concatenate 'simple-string
						   "text-base/"
						   name
						   ".svn-base")
				      svn-dir))))
	  (and version date
	       (values version date)))))))

(defun maybe-make-svn-info (pathname connect)
  (let* ((dir (directory-namestring (namify pathname)))
	 (svn-dir
	  (format () "~A/" (merge-pathnames ".svn" dir)))
	 (stream (make-string-output-stream)))
    (when (probe-file svn-dir)
      ;; FIX for every file this parses entries  (cache with file time check?)
      (multiple-value-bind (rev-version rev-date)
			   (parse-svn-entries svn-dir pathname)
	(let ((vc-info (%make-vc-info :svn
				      #'svn-insert-log
				      #'svn-update-file
				      #'svn-commit
				      #'svn-make-modeline-string)))
	  (setf (vc-info-compare-fun vc-info) #'svn-compare)
#|
	  (setf (vc-info-module vc-info)
		(parse-svn-module svn-dir))
	  (setf (vc-info-repository vc-info)
		(parse-svn-root svn-dir))
|#
	  (setf (vc-info-version vc-info) rev-version)
	  (let ((entry (when connect
			 (in-directory dir
			   (do-vc-command () "svn"
					  `("ls" "--non-interactive" "-v"
					    ,(file-namestring pathname))
					  :output stream))
			 (get-output-stream-string stream))))
	    (multiple-value-bind (svn-version svn-date)
				 (parse-svn-entry entry)
	      (declare (ignore svn-date))
	      (setf (vc-info-status vc-info)
		    (if (and svn-version
			     (fi (directoryp pathname) (conflict-p pathname)))
			:conflict
			(let* ((date (file-write-date pathname))
			       ;(svn-date (parse-time svn-date))
			       ;(needs-update (and svn-date rev-date
			       ;(> svn-date rev-date))))
			       (needs-update (and svn-version rev-version
						  (version-> svn-version
							     rev-version))))
			  (cond
			   ((and date rev-date (> date rev-date))
			    (if needs-update :needs-merge :modified))
			   (needs-update :needs-update))))))
	    vc-info))))))


;;;; Concurrent Versions System (CVS).

(defun cvs-insert-log (vc-info stream pathname)
  "Insert in Stream the log for the file Pathname."
  (declare (ignore vc-info))
  (in-directory pathname
    (do-vc-command t "cvs" (list "log" (file-namestring pathname))
		   :output stream)))

(defun cvs-compare (vc-info stream pathname &optional tag1 tag2)
  "Insert in Stream a comparison of Pathname and the repository version.
   If tag1 and tag2 are given compare those versions of Pathname."
  (declare (ignore vc-info))
  (in-directory pathname
    (do-vc-command () "cvs"
		   (if tag1
		       (list "diff" "-r" tag1 "-r" tag2
			     (file-namestring pathname))
		       (list "diff" (file-namestring pathname)))
		   :output stream)))

(defun cvs-update-file (vc-info buffer pathname files lock always-overwrite-p)
  "Update Pathname from CVS.  If Pathname and Files is nil update the
   directory (recursively if *vc-update-file-recurse* is true).  If Files
   is true then update the list in Files instead."
  (declare (ignore vc-info lock always-overwrite-p))
  (let* ((pn-len (length (os-namestring pathname)))
	 (names (if files
		    (mapcar (lambda (file)
			      (subseq (os-namestring file) pn-len))
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
      (do-vc-command t "cvs" (if (car names)
				 (cons "update" names)
				 (if *vc-update-file-recurse*
				     (list "update" "-dP" ".")
				     (list "update" "-dPl" ".")))
		     :output *vc-output-stream*))
    (invoke-hook vc-update-file-hook buffer pathname)
    (message "Done.")
    (car names)))


#| Old add segment from below.  Added only one file; prompted for a
   description.

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
			       :modes '("Text" "Fill" "Spell" "VC Log Entry")
			       :delete-hook
			       (list #'(lambda (buffer)
					 (declare (ignore buffer))
					 (or allow-delete
					     (return-from in-recursive-edit t))))))
			(turn-auto-save-off descr-buffer)
			(let ((string (prompt-in-buffer descr-buffer)))
			  (message "Adding ~A ..." (namestring pathname))
			  (in-directory pathname
			    (do-vc-command t "cvs"
					   (list "add" "-m"
						 string
						 (file-namestring pathname))
					   :output *vc-output-stream*)))
			nil)
		  (editor-error "Add and commit canceled."))
	      (when (member old-buffer *buffer-list*)
		(change-to-buffer old-buffer))
	      (setf allow-delete t)
	      (delete-buffer-if-possible descr-buffer))
	    (editor-error "Commit canceled.")))
|#

(defun cvs-commit (vc-info buffer pathname files keep-lock)
  "Commit $pathname into CVS repository, offering to add the file if
   required.  If $files is true commit instead the absolute pathnames
   listed by $files, which are all rooted in directory $pathname."
  (declare (ignore keep-lock))
  (let ((old-buffer (current-buffer))
	(log-buffer))
    ;; Add the file(s) if required.
    (collect ((new))
      (if files
	  (dolist (file files)
	    (let ((file (merge-pathnames file pathname)))
	      (let ((vc-info (make-vc-info file)))
		(or (and vc-info
			 (vc-info-version vc-info))
		    (new file)))))
	  (or (and vc-info
		   (vc-info-version vc-info))
	      (new pathname)))
      (when (new)
	(let ((files (mapcar #'namestring (new))))
	  (or (prompt-for-y-or-n
	       :prompt (format () "Add ~A to CVS? "
			       (if (cdr files)
				   "multiple files"
				   (car files)))
	       :default t)
	      (editor-error "Commit canceled."))
	  (when (progn
		  (message "Adding ~A ..." (if (cdr files)
					       "multiple files"
					       (car files)))
		  (in-directory pathname
		    (do-vc-command
		     t "cvs"
		     (cons "add" files)
		     :output *vc-output-stream*))
		  ())
	    (editor-error "Add and commit canceled.")))))
    ;; Commit the file(s).
    (let* ((pn-len (length (os-namestring pathname)))
	   (names (if files
		      (mapcar (lambda (file)
				(subseq (os-namestring file) pn-len))
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
	   (allow-delete))
      (unwind-protect
	  (when (block in-recursive-edit
		  (setf log-buffer
			(make-unique-buffer
			 log-name
			 :modes '("Text" "Fill" "Spell" "VC Log Entry")
			 :delete-hook
			 (list #'(lambda (buffer)
				   (declare (ignore buffer))
				   (or allow-delete
				       (return-from in-recursive-edit t))))))
		  (turn-auto-save-off log-buffer)
		  (let ((string (prompt-in-buffer log-buffer)))
		    (message message)
		    (in-directory pathname
		      (do-vc-command t "cvs"
				     (append (list "commit" "-m" string)
					     names)
				     :output *vc-output-stream*)))
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
  "Return the version and date from $entry."
  (let ((strings (split entry #\/)))
    (when (and strings (> (length strings) 3))
      (values (caddr strings) (cadddr strings)))))

(defun parse-cvs-entries (cvs-dir pathname)
  "Return the version and date of Pathname, according to Cvs-dir."
  (let* ((file (merge-pathnames "Entries" cvs-dir))
	 (name (file-namestring pathname)))
    (with-open-file (stream #| FIX |# (namestring file) :direction :input
			    :if-does-not-exist :error)
      (loop for line = (read-line stream nil) while line do
	(let ((strings (split line #\/)))
	  (when (and strings (> (length strings) 3))
	    (if (string= (cadr strings) name)
		(return-from parse-cvs-entries
			     (values (caddr strings) (cadddr strings))))))))))

(defun maybe-make-cvs-info (pathname connect)
  (let* ((dir (directory-namestring (namify pathname)))
	 (cvs-dir
	  (format nil "~A/"
		  (merge-pathnames "CVS" dir)))
	 (stream (make-string-output-stream)))
    (when (probe-file cvs-dir)
      ;; FIX for every file this parses entries  (cache with file time check?)
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
			   (do-vc-command () "cvs"
					  `("ls" "-e"
					    ,(file-namestring pathname))
					  :output stream))
			 (get-output-stream-string stream))))
	    (multiple-value-bind (cvs-version cvs-date)
				 (parse-cvs-entry entry)
	      (declare (ignore cvs-date))
	      (setf (vc-info-status vc-info)
		    (if (and cvs-version
			     (fi (directoryp pathname) (conflict-p pathname)))
			:conflict
			; FIX
			; /test/1.2/Result of merge+Sat Oct 28 21:46:53 2006//
			(if (and (> (length rev-date) 14)
				 (string= rev-date "Result of merge" :end1 15))
			    :merged
			    (let* ((date (file-write-date pathname))
				   (rev-date (parse-time rev-date))
				   ;(cvs-date (parse-time cvs-date))
				   ;(needs-update (and cvs-date rev-date
				   ;(> cvs-date rev-date))))
				   (needs-update (and cvs-version rev-version
						      (version-> cvs-version
								 rev-version))))
			      (cond
			       ((and date rev-date (> date rev-date))
				(if needs-update :needs-merge :modified))
			       (needs-update :needs-update)))))))
	    vc-info))))))


;;;; RCS.

(defun rcs-insert-log (vc-info stream pathname)
  (declare (ignore vc-info))
  (in-directory pathname
    (do-vc-command t "rlog" (list (file-namestring pathname))
		   :output stream)))

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
      (do-vc-command t "co" `(,@(if lock '("-l")) ,file)
		     :output *vc-output-stream*)
      (invoke-hook vc-update-file-hook buffer pathname)
      (when backup (delete-file backup)))))

(defun rcs-commit (vc-info buffer pathname files keep-lock)
  (declare (ignore vc-info))
  (let ((old-buffer (current-buffer))
	(allow-delete nil)
	(log-buffer nil))
    (let* (
; FIX for doing all in one cmd
;          (pn-len (length (os-namestring pathname)))
; 	   (files (if files
; 		      (mapcar (lambda (file)
; 				(subseq (os-namestring file) pn-len))
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
		    (let ((log-stream (make-editor-region-stream
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
      (do-vc-command t "ci" `(,@(if keep-lock '("-l"))
				,@(if keep-working-copy '("-u"))
				,filename)
		     :input log-stream
		     :output *vc-output-stream*)
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

(defun rcs-toggle-file-lock (vc-info buffer pathname)
  (case (vc-info-status vc-info)
    (:locked
     (message "Releasing lock on ~A ..." (namestring pathname))
     (let ((file (file-namestring pathname)))
       (in-directory pathname
	 (do-vc-command t "rcs" `("-u" ,file)
		     :output *vc-output-stream*)
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
	 (let ((name (pick-new-file "/tmp/,vctmp-~D-~D")))
	   (rcs-lock-file buffer pathname)
	   (unwind-protect
	       (progn
		 (in-directory pathname
		   (do-vc-command t "co" `("-p" ,(file-namestring pathname))
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
      (do-vc-command t "rcs" `("-l" ,file)
		     :output *vc-output-stream*)
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
