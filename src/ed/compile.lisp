;;; Mode and commands for running shell compile commands.

(in-package "ED")


(defevar "Compile Command"
  "Proposed value for command at Compile prompt.  Passed as format
   specifier to Format with name of file associated with current buffer as
   first format argument."
  :value "make ~A")


;;;; Mode.

(defun setup-compile-mode (buffer)
  (highlight-visible-compile-buffer buffer)
  (pushnew '("Compile" () highlight-visible-compile-buffer)
	   *mode-highlighters*))

(defmode "Compile" :major-p nil
  :setup-function 'setup-compile-mode
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

;;; Internal
;;;
;;; Get the root directory of a referenced file.  That is, get the
;;; directory to which the file is relative.
;;;
(defun get-root (name)
  (if (editor-bound-p 'compile-root :buffer (current-buffer))
      ;; FIX should be merged with pn?
      (value compile-root)
      (let* ((dir (prompt-for-file
		   :prompt "Root of referenced file: "
		   :help "Enter the root directory of the referenced file."
		   :default (current-directory)))
	     (pn (merge-pathnames (file-namestring name) dir)))
	(if (probe-file pn)
	    (progn
	      (defevar "Compile Root"
		"Root of references in compile buffer."
		:buffer (current-buffer)
		:value pn)
	      pn)
	    (get-root (string-trim '(#\space #\tab) name))))))

(defun parse-reference-line (line buffer)
  "Return the name and line number of the reference on Line, if there is
   one, else () and an arbitrary value."
  (let* ((chars (line-string line))
	 (pos1 (position #\: chars)))
    (when pos1
      (let ((name (merge-pathnames (subseq chars 0 pos1)
				   (directory-namestring
				    (buffer-pathname buffer)))))
	(let ((pos2 (position #\: chars :start (1+ pos1))))
	  (when pos2
	    (let* ((field (subseq chars (1+ pos1) pos2))
		   (offset (read-from-string field)))
	      (if (numberp offset)
		  (if (probe-file name)
		      (values name offset)
		      (let* ((trim (string-trim '(#\space #\tab)
					       (subseq chars 0 pos1)))
			     (name (merge-pathnames
				    trim
				    (directory-namestring
				     (buffer-pathname buffer)))))
			(if (probe-file name)
			    (values name offset)
			    (values (get-root name) offset))))
		  (if (or (probe-file field)
			  (setq field
				(let ((trim (string-trim '(#\space #\tab)
							 field)))
				  (if (probe-file trim)
				      trim)))
			  ;; FIX check this
			  (setq field (get-root name)))
		      (let ((pos1 (position #\: chars :start (1+ pos2))))
			(when pos1
			  (let ((offset (read-from-string (subseq chars
								  (1+ pos2)
								  pos1))))
			    (if (numberp offset)
				(return-from parse-reference-line
					     (values field offset)))))))))))))))


;;;; Commands.

(defhistory *compile-command-history*
	    *compile-command-history-pointer*
	    350)

(defcommand "Compile" ()
  "Run a prompted compile shell command."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ( ;(current-file (buffer-pathname (current-buffer)))
	(*shell-command-in-buffer-history* *compile-command-history*)
	(*shell-command-in-buffer-history-pointer*
	 *compile-command-history-pointer*))
    (make-new-shell t nil nil :clear-buffer t
#|
		    :proposed-command
		    (string-trim " "
				 (format nil (value compile-command)
					 (or (and current-file
						  (pathname-name current-file))
					     "")))
|#
)
    (let ((buffer (current-buffer)))
      (setf (buffer-minor-mode buffer "Compile") t)
      (push-compile-buffer buffer))
    (defevar "Compile Referenced"
      "t if a reference in the buffer has been followed."
      :buffer (current-buffer))))

(defcommand "Last Compile" ()
  "Switch to the last compile."
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
	    (split-window-command)
	    (next-window-command)))
      (change-to-buffer (find-file-buffer name))
      (buffer-start (current-point))
      (line-offset (current-point) (1- offset)))))

(defcommand "Follow Compile Reference in Other Window" ()
  "Show the file referenced at point in the other window."
  (follow-compile-reference-command t))

(defcommand "Switch to Next Compile Reference" ()
  "Switch to the next file reference from the most recent compilation."
  (when (last-compile-command)
    (next-compile-reference-command)
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
      (until ((line
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
	     ((if line (equal line buffer-edge-line) t)
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

(defcommand "Update Compile Buffer" ()
  "Update a Compile mode buffer."
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
      (in-directory (buffer-pathname buffer)
	(setv process (eval (value process-start-expression))))
      (update-process-buffer buffer))))


;;;; Highlighting.

(defun highlight-compile-line (line chi-info)
  ;; /usr/include/stdint.h:37: error: expected ...
  (when (plusp (line-length line))
    (let* ((string (line-string line))
	   (line-length (length string))
	   (one (position #\: string)))
      (when (and one (< one (- line-length 1)))
	(let ((two (position #\: string :start (1+ one))))
	  (when (and two
		     (< two (- line-length 1))
		     (parse-integer string
				    :start (1+ one)
				    :end two
				    :junk-allowed ()
				    :errorp ()))
	    (chi-mark line 0 *special-form-font* :special-form
		      chi-info)
	    (chi-mark line (1+ one) *variable-font* :variable chi-info)
	    (chi-mark line (1+ two) *original-font* :window-foreground
		      chi-info)))))))

#| Old mark version.
(defun highlight-compile-line (line chi-info)
  (when (plusp (line-length line))
    (let ((mark (mark line 0)))
      (when (find-character mark #\:)
	(let ((mark2 (copy-mark mark)))
	  (when (and (mark-after mark2)
		     (find-character mark2 #\:)
		     (parse-integer (region-to-string (region mark
							      mark2))
				    :junk-allowed t
				    :errorp ()))
	    (chi-mark line 0 *special-form-font* :special-form
		      chi-info)
	    (mark-after mark)
	    (chi-mark line (mark-charpos mark) *variable-font*
		      :variable chi-info)
	    (mark-after mark2)
	    (chi-mark line (mark-charpos mark2) *original-font*
		      :window-foreground chi-info)))))))
|#

(defun highlight-compile-buffer (buffer)
  (highlight-chi-buffer buffer highlight-compile-line))

(defun highlight-visible-compile-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-compile-line))
