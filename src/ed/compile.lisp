;;; Mode and commands for running shell compile commands.

(in-package "ED")


(defhvar "Compile Command"
  "Proposed value for command at Compile prompt.  Passed as format
   specifier to Format with name of file associated with current buffer as
   first format argument."
  :value "make ~A")


;;;; Mode.

(defmode "Compile" :major-p nil
  :precedence 4.0
  :documentation
  "Mode for output from compilation processes.")


;;;; Helpers.

(defconstant *last-compile-buffers-max-len* 25)
(defvar *last-compile-buffers-len* 0)
(defvar *last-compile-buffers* '()
  "A list of the most recently produced Compile buffers.")

(defun push-compile-buffer (buffer)
  "Push Buffer onto *last-compile-buffers*."
  ; Ensure that only one such buffer on list.
  (let ((pos (position buffer *last-compile-buffers*)))
    (when pos
      (setq *last-compile-buffers* (remove buffer *last-compile-buffers*))))
  (when (>= *last-compile-buffers-len* *last-compile-buffers-max-len*)
    (setf (cdr (last *last-compile-buffers* 2)) nil))
  (push buffer *last-compile-buffers*))

(defun pop-compile-buffer ()
  "Pop *last-compile-buffers*."
  (when *last-compile-buffers*
    (decf *last-compile-buffers-len*)
    (pop *last-compile-buffers*)))

(defun parse-reference-line (line buffer)
  "Return the name and line number of the reference on Line, if there is
   one, else return nil and nil."
  (let* ((chars (line-string line))
	 (pos1 (position #\: chars)))
    (when pos1
      (let ((name (merge-pathnames (subseq chars 0 pos1)
				   (directory-namestring
				    (buffer-pathname buffer)))))
	(when (probe-file name)
	  (let ((pos2 (position #\: chars :start (1+ pos1))))
	    (when pos2
	      (let ((offset (read-from-string (subseq chars
						      (1+ pos1)
						      pos2))))
		(if (numberp offset)
		    (return-from parse-reference-line
				 (values name offset))))))))))
  (values nil nil))


;;;; Commands.

(defcommand "Compile" (p)
  "Run a prompted compile shell command."
  "Run a prompted compile shell command."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let ((current-file (buffer-pathname (current-buffer))))
    (make-new-shell t nil nil :clear-buffer t
		    :proposed-command
		    (string-trim " "
				 (format nil (value compile-command)
					 (or (and current-file
						  (pathname-name current-file))
					     ""))))
    (let ((buffer (current-buffer)))
      (setf (buffer-minor-mode buffer "Compile") t)
      (push-compile-buffer buffer))
    (defhvar "Compile Referenced"
      "t if a reference in the buffer has been followed."
      :value nil
      :buffer (current-buffer))))

(defcommand "Last Compile" (p)
  "Switch to the last compile."
  "Switch to the last compile."
  (declare (ignore p))
  (when *last-compile-buffers*
    (do ((buf (car *last-compile-buffers*)
	      (car *last-compile-buffers*)))
	((if buf (memq buf *buffer-list*) t)
	 (progn
	   (if buf
	       (let ((windows (buffer-windows buf)))
		 (if windows
		     (setf (current-buffer) buf  (current-window) (car windows))
		     (change-to-buffer buf))))
	   buf))
      (pop-compile-buffer))))

(defcommand "Follow Compile Reference" (p)
  "Show the file referenced at point.
   With a prefix show the file in the other window."
  "Show the file referenced at point.
   With a prefix show the file in the other window."
  (or (editor-bound-p 'compile-referenced)
      (editor-error "Current buffer must be a Compile buffer."))
  (multiple-value-bind
      (name offset)
      (parse-reference-line (mark-line (current-point)) (current-buffer))
    (when name
      (push-compile-buffer (current-buffer))
      (when p
	(if (eq (next-window (current-window)) (current-window))
	    (split-window-command nil)
	    (next-window-command nil)))
      (change-to-buffer (find-file-buffer name))
      (buffer-start (current-point))
      (line-offset (current-point) (1- offset)))))

(defcommand "Follow Compile Reference in Other Window" (p)
  "Show the file referenced at point in the other window."
  "Show the file referenced at point in the other window."
  (declare (ignore p))
  (follow-compile-reference-command t))

(defcommand "Switch to Next Compile Reference" (p)
  "Switch to the next file reference from the most recent compilation."
  "Switch to the next file reference from the most recent compilation."
  (declare (ignore p))
  (when (last-compile-command nil)
    (next-compile-reference-command nil)
    (follow-compile-reference-command t)))

(defcommand "Next Compile Reference" (p)
  "Move to the next file reference."
  "Move to the next file reference."
  (or (editor-bound-p 'compile-referenced)
      (editor-error "Current buffer must be a Compile buffer."))
  (let ((forward (cond ((numberp p)
			(cond ((plusp p) t)
			      ((zerop p)
			       (return-from
				next-compile-reference-command))
			      (t
			       (setq p (- p))
			       nil)))
		       (t (setq p 1)))))
    (let ((point (current-point))
	  (buffer (current-buffer)))
      (do* ((line
	     (if (value compile-referenced)
		 (if forward
		     (line-next (mark-line point))
		     (line-previous (mark-line point)))
		 (progn
		   (setv compile-referenced t)
		   (mark-line (if forward
				  (buffer-start-mark buffer)
				  (buffer-end-mark buffer)))))
	     (if forward
		 (line-next line)
		 (line-previous line)))
	    (buffer-edge-line (mark-line
			       (if forward
				   (buffer-end-mark buffer)
				   (buffer-start-mark buffer)))))
	   ((equal line buffer-edge-line)
	    (if forward
		(message "On last reference or past any references.")
		(message "On first reference or before any references.")))
	(multiple-value-bind (name offset)
			     (parse-reference-line line buffer)
	  (declare (ignore offset))
	  (when name
	    (decf p)
	    (when (< p 1)
	      (line-start point line)
	      (return-from next-compile-reference-command))))))))

(defcommand "Previous Compile Reference" (p)
  "Move to the previous file reference."
  "Move to the previous file reference."
  (next-compile-reference-command (if p (- p) -1)))

(defcommand "Update Compile Buffer" (p)
  "Update a Compile mode buffer."
  "Update a Compile mode buffer."
  (declare (ignore p))
  (or (editor-bound-p 'compile-referenced)
      (editor-error "Current buffer must be a Compile buffer."))
  (when (prompt-for-y-or-n
	 :prompt (format nil "Rerun compile command? ")
	 :help "Y to turn rerun the compile."
	 :default nil)
    (let ((buffer (current-buffer)))
      (push-compile-buffer buffer)
      (setv compile-referenced nil)
      (delete-region (buffer-region buffer))
      (multiple-value-bind (got dir)
			   (unix:unix-current-directory)
	(or got (editor-error "Failed to get current directory: ~A." dir))
	(unwind-protect
	    (progn
	      (unix:unix-chdir (buffer-pathname buffer))
	      (setv process (eval (value process-start-expression))))
	  (unix:unix-chdir dir)))
      (update-process-buffer buffer))))
