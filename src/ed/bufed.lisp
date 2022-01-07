;;; Bufed (Buffer list Editing) mode.

(in-package "ED")

#[ Bufed Mode

The editor provides a mechanism for managing buffers as an itemized list.
`Bufed' supports conveniently deleting several buffers at once, saving
them, going to one, etc., all in a key stroke.

{command:Bufed}
{command:Bufed Help}
{command:Bufed Delete}
{evariable:Virtual Buffer Deletion}
{evariable:Bufed Delete Confirm}
{command:Bufed Undelete}
{command:Bufed Expunge}
{command:Bufed Quit}
{command:Bufed Goto}
{command:Bufed Goto and Quit}
{command:Bufed Save File}
]#

(defvar *bufed-modified-char* #\-)


;;;; Representation of existing buffers.

(defmacro bufed-buffer (x) `(car ,x))
(defmacro bufed-buffer-buffer (x) `(car ,x))
(defmacro bufed-buffer-mark (x) `(cdr ,x))
(defmacro make-bufed-buffer (buffer) `(list ,buffer))

;;; This is the bufed buffer if it exists.
;;;
(defvar *bufed-buffer* ())

;;; This is the cleanup method for deleting *bufed-buffer*.
;;;
(defun delete-bufed-buffers (buffer)
  (when (eq buffer *bufed-buffer*)
    (setf *bufed-buffer* ())))


;;;; Commands.

(defun setup-bufed-buffer (buffer)
  (defevar "Bufed Search Expression"
    "The search string used to collect the buffers in a bufed buffer, or ()
     for all buffers."
    :buffer buffer)
  (defevar "Bufed Buffers"
    "The array of buffers in the bufed buffer.  Each element is a cons,
     where the CAR is the buffer, and the CDR indicates the mark status of
     the buffer."
    :buffer buffer)
  (defevar "Bufed Buffers End"
    "The index of the last element of the set of buffers in this buffer."
    :buffer buffer))

(defmode "Bufed" :major-p t
  :setup-function #'setup-bufed-buffer
  :documentation
  "Bufed is a mode for editing buffers, including saving, going to and
   killing a buffer.")

(defevar "Virtual Buffer Deletion"
  "When set, `Bufed Delete' marks a buffer for deletion instead of
   immediately deleting it."
  :value t)

(defevar "Bufed Delete Confirm"
  "When set, \"Bufed\" commands that actually delete buffers ask for
   confirmation before taking action."
  :value t)

(defcommand "Bufed Delete" ()
  "Delete the buffer on the current line.  Display some other buffer in any
   windows displaying this buffer.  If `Bufed Delete Confirm' is set,
   prompt for confirmation first.  If the buffer is modified prompt to save
   it beforehand.

   When `Virtual Buffer Deletion' is set, merely flag the buffer for
   deletion.  `Bufed Expunge' and `Bufed Quit' executes the deletion."
  (let* ((point (current-point))
	 (buf-info (array-element-from-mark point (value bufed-buffers))))
    (if (and (not (value virtual-buffer-deletion))
	     (or (not (value bufed-delete-confirm))
		 (prompt-for-y-or-n :prompt "Delete buffer? " :default t
				    :must-exist t :default-string "Y")))
	(delete-bufed-buffer (bufed-buffer buf-info))
	(with-writable-buffer ((current-buffer))
	  (setf (bufed-buffer-mark buf-info) t)
	  (with-mark ((point point))
	    (setf (next-character (line-start point)) #\D))))))

(defcommand "Bufed Delete and Next" ()
  "Delete the buffer and move to the next line.
   Any windows displaying this buffer will display some other buffer."
  "Delete the buffer indicated by the current line and move to the next
   line.  Any windows displaying this buffer will display some other
   buffer."
  (bufed-delete-command)
  (next-line-command))

(defcommand "Bufed Undelete" ()
  "Undelete the buffer on the current line. Any windows displaying this
   buffer will display some other buffer."
  (with-writable-buffer ((current-buffer))
    (setf (bufed-buffer-mark (array-element-from-mark
			      (current-point) (value bufed-buffers)))
	  ())
    (with-mark ((point (current-point)))
      (setf (next-character (line-start point)) #\space))))

(defcommand "Bufed Expunge" ()
  "Expunge buffers marked for deletion, regarding `Bufed Delete Confirm'."
  (expunge-bufed-buffers))

(defcommand "Bufed Quit" ()
  "Kill the `Bufed' buffer, expunging any buffer marked for deletion."
  (expunge-bufed-buffers)
  (if *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*))
  (if (editor-bound-p 'bufed-buffers :buffer (current-buffer))
      (rotate-buffers-forward-command)))

;;; EXPUNGE-BUFED-BUFFERS deletes the marked buffers in the bufed buffer,
;;; signalling an error if the current buffer is not the bufed buffer.  This
;;; returns t if it deletes some buffer, otherwise nil.  We build a list of
;;; buffers before deleting any because the BUFED-DELETE-HOOK moves elements
;;; around in FIX *bufed-buffers*.
;;;
(defun expunge-bufed-buffers ()
  (or (editor-bound-p 'bufed-buffers)
      (editor-error "Must be in a Bufed buffer."))
  (let (buffers)
    (dotimes (i (value bufed-buffers-end))
      (let ((buf-info (svref (value bufed-buffers) i)))
	(if (bufed-buffer-mark buf-info)
	    (push (bufed-buffer buf-info) buffers))))
    (if (and buffers
	     (if (value bufed-delete-confirm)
		 (prompt-for-y-or-n :prompt "Delete buffers? " :default t
				    :must-exist t :default-string "Y")
		 t))
	(dolist (b buffers t) (delete-bufed-buffer b)))))

(defun delete-bufed-buffer (buf)
  (when (and (buffer-modified buf)
	     (value kill-buffer-prompt-to-save-process-buffers)
	     (prompt-for-y-or-n :prompt (list "~A is modified.  Save it first? "
					      (buffer-name buf))))
    (save-file-command () buf))
  (delete-buffer-if-possible buf))

(defcommand "Bufed Goto" ()
  "Change to the buffer under point."
  (change-to-buffer
   (bufed-buffer (array-element-from-mark (current-point) (value bufed-buffers)))))

(defcommand "Bufed Goto in Next Window" ()
  "Change to the buffer under point in the next window."
  (let* ((point (current-point))
	 (buffer (bufed-buffer (array-element-from-mark point
							(value bufed-buffers)))))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command)
	(next-window-command))
    (change-to-buffer buffer)))

(defcommand "Bufed Goto and Quit" ()
  "Change to the buffer under point, quitting Bufed.  This supplies a
   function for `Generic Pointer Up' which is a no-op."
  (expunge-bufed-buffers)
  (point-to-here-command)
  (let ((bufed-buffer (current-buffer)))
    (change-to-buffer
     (bufed-buffer (array-element-from-pointer-pos (value bufed-buffers)
						   "No buffer on that line.")))
    (delete-buffer-if-possible bufed-buffer))
  (supply-generic-pointer-up-function #'(lambda () ())))

(defcommand "Bufed Save File" ()
  "Save the buffer on the current line."
  (save-file-command
   ()
   (bufed-buffer (array-element-from-mark (current-point) (value bufed-buffers)))))

(defun search-buffers (pattern string &optional display-matches)
  "Return a list of bufed buffers for the buffers that match $string."
  (declare (ignore pattern))
  (get-search-pattern string :forward)
  (collect ((matches))
    (if display-matches
	(dolist (buffer *buffer-list*)
	  (error "FIX")
	  (let ((mark (copy-mark (buffer-point buffer))))
	    (do ((won (find-pattern mark *last-search-pattern*)
		      (find-pattern mark *last-search-pattern*)))
		((fi won))
	      (character-offset mark won)
	      (matches (list buffer
			     (string (count-lines (region (buffer-start-mark buffer)
							  mark)))
			     (line-string (mark-line mark)))))))
	(dolist (buffer *buffer-list*)
	  (let ((mark (copy-mark (buffer-point buffer))))
	    (if (find-pattern mark *last-search-pattern*)
		(matches (make-bufed-buffer buffer))))))
    (matches)))

(defcommand "Search Buffers" ()
  "Bufed all the buffers that contain a prompted search string."
  (let* ((exp (prompt-for-string
	      :prompt "Search buffers for: "
	      :help "String for which to search in buffers."
	      :default (word-at-point)
	      :trim t))
	 (name (format () "Search buffers for ~A" exp))
	 (buffer (getstring exp *buffer-names*)))
    (and buffer
	 (variable-value 'bufed-search-expression
			 :buffer buffer)
	 (prompt-for-y-or-n :prompt "Regenerate buffer list? ")
	 (make-bufed "dummy"
		     buffer
		     (search-buffers t
				     (variable-value 'bufed-search-expression
						     :buffer buffer))))
    (let ((buffer (or buffer (make-bufed name (search-buffers t exp)))))
      (setf (variable-value 'bufed-search-expression :buffer buffer) exp)
      (change-to-buffer buffer))))

(defcommand "Bufed" ()
  "Create a list of buffers in a buffer supporting operations such as
   deletion and selection.  If there already is a bufed buffer, just go to
   it."
  (change-to-buffer
   (or *bufed-buffer*
       (collect ((buffers))
	 (do-strings (name buffer *buffer-names*)
	   (declare (ignore name))
	   (or (eq buffer *echo-area-buffer*)
	       (eq buffer edi::*message-buffer*)
	       (buffers (make-bufed-buffer buffer))))
	 (setq *bufed-buffer* (make-bufed "Bufed" (buffers)))))))

(defun refresh-bufed-buffer (buf buffers bufed-buffers &optional clear)
  "Refresh bufed buffer $buf, which lists the list of $buffers using array
   $bufed-buffer.  Assume that `Bufed Buffer End' is already set."
  (setf (buffer-writable buf) t)
  (if clear (delete-region (buffer-region *bufed-buffer*)))
  (with-output-to-mark (stream (buffer-point buf))
    (let ((i 0))
      (dolist (bb buffers)
	(bufed-write-line (bufed-buffer-buffer bb)
			  (buffer-name (bufed-buffer-buffer bb))
			  stream)
	(setf (svref bufed-buffers i) bb)
	(incf i))))
  #|
  FIX
  (clearf (buffer-writable buf))
  (clearf (buffer-modified buf))
  |#
  (setf (buffer-writable buf) ())
  (setf (buffer-modified buf) ())

  (let ((fields (buffer-modeline-fields buf)))
    (setf (cdr (last fields))
	  (list (or (modeline-field :bufed-cmds)
		    (make-modeline-field
		     :name :bufed-cmds :width 18
		     :function
		     #'(lambda (buffer window)
			 (declare (ignore buffer window))
			 "  Type ? for help.")))))
    (setf (buffer-modeline-fields buf) fields))
  (buffer-start (buffer-point buf)))

(defun make-bufed (name buffers &optional buffer clear)
  "Make a bufed buffer listing $buffers."
  (let* ((buf (or buffer
		  (make-buffer name :modes '("Bufed")
			       :delete-hook (list #'delete-bufed-buffers))))
	 (bufed-buffers (make-array (setf (variable-value 'bufed-buffers-end
							  :buffer buf)
					  (length buffers))
				    :initial-element ())))
    (setf (variable-value 'bufed-buffers :buffer buf) bufed-buffers)
    (refresh-bufed-buffer buf buffers bufed-buffers clear)
    buf))

(defun bufed-write-line (buffer name s
		         &optional (buffer-pathname (buffer-pathname buffer)))
  (let ((modified (buffer-modified buffer)))
    (write-string (if modified
		      (concat "  " (string *bufed-modified-char*))
		      "   ")
		  s)
    (write-char (if (active-process-p buffer) #\+ #\ ) s)
    (if (and buffer-pathname (file-namestring buffer-pathname))
; 	(format s "~A  ~A~:[~50T~A~;~]~%"
; 		(file-namestring buffer-pathname)
; 		(directory-namestring buffer-pathname)
; 		(string= (pathname-to-buffer-name buffer-pathname) name)
; 		name)
	(format s "~A  ~A~:[~50T~A~;~]~%"
		(file-namestring buffer-pathname)
		(directory-namestring buffer-pathname)
		(string= (pathname-to-buffer-name buffer-pathname) name)
		name)
	(write-line name s))))

(defcommand "Bufed Help" ()
  "Pop up a display of `Bufed' help."
  (describe-mode-command () "Bufed"))

(defcommand "Bufed Refresh" ()
  "Refresh the current buffer."
  (or (editor-bound-p 'bufed-buffers)
      (editor-error "Must be in a Bufed buffer."))
  (if (eq (current-buffer) *bufed-buffer*)
      (collect ((buffers))
	(do-strings (name buffer *buffer-names*)
	  (declare (ignore name))
	  (or (eq buffer *echo-area-buffer*)
	      (eq buffer *bufed-buffer*)
	      (eq buffer edi::*message-buffer*)
	      (buffers (make-bufed-buffer buffer))))
	(make-bufed "Bufed" (buffers) *bufed-buffer* t))
      (progn
	(or (value bufed-search-expression)
	    (editor-error "Expected a search expression in `Bufed Search Expression'."))
	(make-bufed "dummy"
		    (search-buffers t
				    (value bufed-search-expression))
		    (current-buffer)))))


;;;; Maintenance hooks.

(eval-when (compile eval)
(defmacro do-bufed-points ((point buffer-var buffer &optional pos) &rest body)
  (let ((pos (or pos (gensym)))
	(bufed-buffers (gensym)))
    `(dolist (,buffer-var *buffer-list*)
       (when (editor-bound-p 'bufed-buffers :buffer ,buffer-var)
	 (let* ((,bufed-buffers (variable-value 'bufed-buffers :buffer ,buffer-var))
		(,pos (position ,buffer ,bufed-buffers :key #'car
				:test #'eq :end (variable-value 'bufed-buffers-end
								:buffer ,buffer-var))))
	   (when ,pos
	     (let ((,point (buffer-point ,buffer-var)))
	       (or (line-offset (buffer-start ,point) ,pos 0)
		   (error "Failed to line-offset in bufed buffer."))
	       (with-writable-buffer (,buffer-var) ,@body))))))))
) ;eval-when

(defun bufed-modified-hook (b modified)
  (do-bufed-points (point buffer b)
    (setf (next-character (mark-after point)) (if modified
						  *bufed-modified-char*
						  #\space))))
;;;
(add-hook buffer-modified-hook 'bufed-modified-hook)

(defun bufed-make-hook (buffer)
  (declare (ignore buffer))
  (when *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*)))
;;;
(add-hook make-buffer-hook 'bufed-make-hook)

(defun bufed-delete-hook (b)
  (do-bufed-points (point buffer b pos)
    (with-mark ((temp point :left-inserting))
      (line-offset temp 1)
      (delete-region (region point temp)))
    (let* ((end (variable-value 'bufed-buffers-end :buffer buffer))
	   (buffers (variable-value 'bufed-buffers :buffer buffer))
	   (len-1 (1- end)))
      (replace buffers buffers
	       :start1 pos :end1 len-1
	       :start2 (1+ pos) :end1 end)
      (setf (svref buffers len-1) ())
      (setf (variable-value 'bufed-buffers-end :buffer buffer)
	    len-1))))
;;;
(add-hook delete-buffer-hook 'bufed-delete-hook)

(defun bufed-name-hook (b name)
  (do-bufed-points (point buffer b)
    (with-mark ((temp point :left-inserting))
      (line-offset temp 1)
      (delete-region (region point temp)))
    (with-output-to-mark (s point)
      (bufed-write-line buffer name s))))
;;;
(add-hook buffer-name-hook 'bufed-name-hook)

(defun bufed-pathname-hook (b pathname)
  (do-bufed-points (point buffer b)
    (with-mark ((temp point :left-inserting))
      (line-offset temp 1)
      (delete-region (region point temp)))
    (with-output-to-mark (s point)
      (bufed-write-line buffer (buffer-name buffer) s pathname))))
;;;
(add-hook buffer-pathname-hook 'bufed-pathname-hook)
