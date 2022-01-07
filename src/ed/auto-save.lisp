;;; Auto-Save mode.

(in-package "ED")


#[ Auto Save Mode

`Save' mode protects against loss of work in system crashes by periodically
saving modified buffers in checkpoint files.

{mode:Save}
{evariable:Auto Save Checkpoint Frequency}
{evariable:Auto Save Key Count Threshold}
{evariable:Auto Save Cleanup Checkpoints}

The next two variables determine the naming of checkpoint files.

{evariable:Auto Save Filename Pattern}
{evariable:Auto Save Pathname Hook}
]#


;;;; Per buffer state information.

;;; The auto-save-state structure is used to store the state information for
;;; a particular buffer in "Save" mode, namely the buffer-signature at the last
;;; key stroke, the buffer-signature at the time of the last checkpoint, a count
;;; of the number of destructive keystrokes which have occured since the time of
;;; the last checkpoint, and the pathname used to write the last checkpoint.  It
;;; is generally kept in a buffer-local hvar called "Auto Save State".
;;;
(defstruct (auto-save-state
	    (:conc-name save-state-)
	    (:print-function print-auto-save-state))
  "Per buffer state for auto-save"
  (buffer nil)				   ; buffer this state is for; for printing
  (key-signature 0 :type fixnum)	   ; buffer-signature at last keystroke
  (last-ckp-signature 0 :type fixnum)	   ; buffer-signature at last checkpoint
  (key-count 0 :type fixnum)		   ; # destructive keystrokes since ckp
  (pathname nil))			   ; pathname used to write last ckp file

(defun print-auto-save-state (auto-save-state stream depth)
  (declare (ignore depth))
  (format stream "#<Auto Save Buffer State for buffer ~A>"
	  (buffer-name (save-state-buffer auto-save-state))))

;;; GET-AUTO-SAVE-STATE tries to get the auto-save-state for the buffer.  If
;;; the buffer is not in "Save" mode then this function returns NIL.
;;;
(defun get-auto-save-state (buffer)
  (if (editor-bound-p 'auto-save-state :buffer buffer)
       (variable-value 'auto-save-state :buffer buffer)))

;;; RESET-AUTO-SAVE-STATE resets the auto-save-state of the buffer making it
;;; look as if the buffer was just checkpointed.  This is in fact how
;;; checkpoint-buffer updates the state.  If the buffer is not in "Save" mode
;;; this function punts the attempt and does nothing.
;;;
(defun reset-auto-save-state (buffer)
  (let ((state (get-auto-save-state buffer)))
    (when state
      (let ((signature (buffer-signature buffer)))
	(setf (save-state-key-signature state)
	      signature)
	(setf (save-state-last-ckp-signature state)
	      signature)
	(setf (save-state-key-count state)
	      0)))))


;;;; Checkpoint Pathname Interface/Internal Routines

;;; GET-CHECKPOINT-PATHNAME -- Interface
;;;
;;; Returns the pathname of the checkpoint file for the specified
;;; buffer;  Returns NIL if no checkpoints have been written thus
;;; far or if the buffer isn't in "Save" mode.
;;;
(defun get-checkpoint-pathname (buffer)
  "Returns the pathname of the checkpoint file for the specified buffer.
   If no checkpoints have been written thus far, or if the buffer is not in
   \"Save\" mode, return nil."
  (let ((state (get-auto-save-state buffer)))
    (if state
	(save-state-pathname state))))

;;; MAKE-UNIQUE-SAVE-PATHNAME is a candidate for "Auto Save Pathname Hook".
;;;
(defun make-unique-save-pathname (buffer)
  "Return a pathname for a non-existing file in DEFAULT-DIRECTORY.  Use
   GENSYM to form a file name: save-GENSYM.CKP."
  (declare (ignore buffer))
  (let ((def-dir (current-directory)))
    (loop
      (let* ((sym (gensym))
	     (f (merge-pathnames (format nil "save-~A.CKP" sym) def-dir)))
	(or (probe-file f) (return f))))))

(defevar "Auto Save Pathname Hook"
  "A function called by `Auto Save' to get a checkpoint pathname to
   associate with a buffer.  The function should take a buffer as its
   argument and return the checkpoint pathname or ().  If the function
   returns (), or if this variable is (), then `Save' mode is turned off in
   the buffer.")

;;; MAKE-BUFFER-CKP-PATHNAME attempts to form a pathname by using the buffer's
;;; associated pathname (from buffer-pathname).  If there isn't a pathname
;;; associated with the buffer, the function returns nil.  Otherwise, it uses
;;; the "Auto Save Filename Pattern" and FORMAT to make the checkpoint
;;; pathname.
;;;
(defun make-buffer-ckp-pathname (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (if buffer-pn
	(pathname (format nil
			  (value auto-save-filename-pattern)
			  (directory-namestring buffer-pn)
			  (file-namestring buffer-pn))))))


;;;; Buffer-level Checkpoint Routines

;;; write-checkpoint-file -- Internal
;;;
;;; Does the low-level write of the checkpoint.  Returns T if it succeeds
;;; and () if it fails.  Echoes outcome.
;;;
(defun write-checkpoint-file (pathname buffer)
  (let ((ns (namestring pathname)))
    (cond ((file-writable pathname)
	   (if (value auto-save-verbosely)
	       (message "Saving ~A" ns))
	   (handler-case (progn
			   (write-file
			    (buffer-region buffer)
			    pathname
			    :keep-backup nil
			    :access #o600) ;read/write by owner.
			   t)
	     (error (condition)
	       (loud-message "Auto Save failure (~A): ~A"
			     pathname condition)
	       nil)))
	  (t
	   (message "Write protected: ~A" ns)
	   nil))))

;; FIX doc
;;; To save... and to save as what?
;;;
;;; First, make-buffer-ckp-pathname is called. It will return either NIL or
;;; a pathname formed by using buffer-pathname in conjunction with the hvar
;;; "Auto Save Filename Pattern".  If there isn't an associated pathname or
;;; make-buffer-ckp-pathname returns NIL, then we use the pathname we used
;;; the last time we checkpointed the buffer.  If we've never checkpointed
;;; the buffer, then we check "Auto Save Pathname Hook".  If it is NIL then
;;; we turn Save mode off for the buffer, else we funcall the function on
;;; the hook with the buffer as an argument.  The function on the hook should
;;; return either NIL or a pathname. If it returns NIL, we toggle Save mode
;;; off for the buffer;  otherwise, we use the pathname it returned.

;;; checkpoint-buffer -- Internal
;;;
;;; This functions takes a buffer as its argument and attempts to write a
;;; checkpoint for that buffer.  See the notes at the beginning of this page
;;; for how it determines what pathname to use as the checkpoint pathname.
;;; Note that a checkpoint is not necessarily written -- instead "Save"
;;; mode may be turned off for the buffer.
;;;
(defun checkpoint-buffer (buffer)
  (let* ((state (get-auto-save-state buffer))
	 (buffer-ckp-pn (make-buffer-ckp-pathname buffer))
	 (last-pathname (save-state-pathname state)))
    (cond (buffer-ckp-pn
	   (when (write-checkpoint-file buffer-ckp-pn buffer)
	     (reset-auto-save-state buffer)
	     (setf (save-state-pathname state) buffer-ckp-pn)
	     (when (and last-pathname
			(not (equal last-pathname buffer-ckp-pn))
			(probe-file last-pathname))
	       (delete-file last-pathname))))
	  (last-pathname
	   (when (write-checkpoint-file last-pathname buffer)
	     (reset-auto-save-state buffer)))
	  (t
	   (let* ((save-pn-hook (value auto-save-pathname-hook))
		  (new-pn (if save-pn-hook
			      (funcall save-pn-hook buffer))))
	     (cond ((or (not new-pn)
			(zerop (length
				(the simple-string (namestring new-pn)))))
		    (setf (buffer-minor-mode buffer "Save") ()))
		   (t
		    (when (write-checkpoint-file new-pn buffer)
		      (reset-auto-save-state buffer)
		      (setf (save-state-pathname state) new-pn)))))))))

;;; checkpoint-all-buffers -- Internal
;;;
;;; This function looks through the buffer list and checkpoints
;;; each buffer that is in "Save" mode that has been modified since
;;; its last checkpoint.
;;;
(defun checkpoint-all-buffers (elapsed-time)
  (declare (ignore elapsed-time))
  (dolist (buffer *buffer-list*)
    (let ((state (get-auto-save-state buffer)))
      (when (and state
		 (buffer-modified buffer)
		 (not (eql
		       (save-state-last-ckp-signature state)
		       (buffer-signature buffer))))
	(checkpoint-buffer buffer)))))


;;;; Random Hooks: cleanup, buffer-modified, change-save-freq,
;;;;               warn-if-checkpoint-newer.

;;; cleanup-checkpoint -- Internal
;;;
;;; Cleans up checkpoint file for a given buffer if Auto Save Cleanup
;;; Checkpoints is true.  This is called via "Write File Hook"
;;;
(defun cleanup-checkpoint (buffer)
  (let ((ckp-pathname (get-checkpoint-pathname buffer)))
    (when (and (value auto-save-cleanup-checkpoints)
	       ckp-pathname
	       (probe-file ckp-pathname))
      (delete-file ckp-pathname))))

(add-hook write-file-hook 'cleanup-checkpoint)

;;; notice-buffer-modified -- Internal
;;;
;;; This function is called on "Buffer Modified Hook" to reset
;;; the Auto Save state.  It makes the buffer look like it has just
;;; been checkpointed.
;;;
(defun notice-buffer-modified (buffer flag)
  ;; we care only when the flag has gone to false
  (when (not flag)
    (reset-auto-save-state buffer)))

(add-hook buffer-modified-hook 'notice-buffer-modified)

;;; change-save-frequency -- Internal
;;;
;;; This keeps us scheduled at the proper interval.  It is stuck on
;;; the hook list for the hvar "Auto Save Checkpoint Frequency" and
;;; is therefore called whenever this value is set.
;;;
(defun change-save-frequency (name kind where new-value)
  (declare (ignore name kind where))
  (setq new-value (truncate new-value))
  (remove-scheduled-function 'checkpoint-all-buffers)
  (when (and new-value
	     (plusp new-value))
    (schedule-event new-value 'checkpoint-all-buffers t)))

;;; "Save" mode is in "Default Modes", so turn it off in these modes.
;;;

(defun interactive-modes (buffer on)
  (when on (setf (buffer-minor-mode buffer "Save") nil)))

(add-hook typescript-mode-hook 'interactive-modes)
(add-hook eval-mode-hook 'interactive-modes)

;;; warn-if-checkpoint-newer -- Internal
;;;
(defun warn-if-checkpoint-newer (buffer pathname)
  "Warn if checkpoint for Buffer is newer than the file in the buffer.  If
   *Auto Save Offer Revert* is true then offer to revert the file."
  (let ((ckp-pathname (make-buffer-ckp-pathname buffer)))
    (when ckp-pathname
      (let ((date (buffer-write-date buffer)))
	(when (and date (probe-file ckp-pathname))
	  (or (>= date (file-write-date ckp-pathname))
	      (if (value auto-save-offer-revert)
		  (when (prompt-for-y-or-n
			 :prompt (format nil
		         "Checkpoint newer than ~A.~%Revert to checkpoint? "
			 (namestring pathname)))
		    (read-buffer-file ckp-pathname buffer)
		    (setf (buffer-modified buffer) t)
		    (setf (buffer-pathname buffer) pathname)
		    (message "Reverted buffer to checkpoint file ~A."
			     (namestring ckp-pathname)))
		  (message "Checkpoint is newer than file."))))))))

(add-hook read-file-hook 'warn-if-checkpoint-newer)


;;;; Key Count Routine for input hook.

;;; auto-save-count-keys -- Internal
;;;
;;; This function sits on the Input Hook to eat cycles.  If the current
;;; buffer is not in Save mode or if the current buffer is the echo area
;;; buffer, it does nothing.  Otherwise, we check to see if we have exceeded
;;; the key count threshold (and write a checkpoint if we have) and we
;;; increment the key count for the buffer.
;;;
(defun auto-save-count-keys ()
  (declare (optimize speed))
  (let ((buffer (current-buffer)))
    (unless (eq buffer *echo-area-buffer*)
      (let ((state (value auto-save-state))
	    (threshold (value auto-save-key-count-threshold)))
	(when (and state threshold)
	  (let ((signature (buffer-signature buffer)))
	    (declare (fixnum signature))
	    (when (not (eql signature
			    (save-state-key-signature state)))
	      ;; see if we exceeded threshold last time...
	      (when (>= (save-state-key-count state)
			(the fixnum threshold))
		(checkpoint-buffer buffer))
	      ;; update state
	      (setf (save-state-key-signature state) signature)
	      (incf (save-state-key-count state)))))))))

(add-hook input-hook 'auto-save-count-keys)


;;;; Save Mode editor variables.

(defevar "Auto Save Filename Pattern"
  "A format string used to name the checkpoint files for buffers with
   associated files.  Format is called with two arguments: the directory
   and file namestrings of the associated file."
  :value "~A~A.CKP")

(defevar "Auto Save Key Count Threshold"
  "The number of effective keystrokes that automatically trigger a
   checkpoint.  A value of () turns this feature off."
  :value 256)

(defevar "Auto Save Cleanup Checkpoints"
  "If this variable is true, then any checkpoint file for a buffer will be
   deleted when the buffer is successfully saved in its associated file."
  :value t)

(defevar "Auto Save Checkpoint Frequency"
  "All modified buffers (in \"Save\" mode) are checkpointed at latest after
   this number of seconds.  () and numbers <= 0 turn this feature off."
  :value (* 2 60)
  :hooks '(change-save-frequency))

(defevar "Auto Save State"
  "Shadow magic.  This variable is seen when in buffers that are not
  in \"Save\" mode.  Do not change this value or you will lose.")

(defevar "Auto Save Offer Revert"
  "If true when reading a file, Auto Save offers to revert the file if it
   is older than the checkpoint.")

(defevar "Auto Save Verbosely"
  "If true then print a message when a checkpoint is written."
  :value t)


;;;; "Save" mode.

(defun setup-auto-save-mode (buffer)
  (let* ((signature (buffer-signature buffer))
	 (state (make-auto-save-state
		 :buffer buffer
		 :key-signature (the fixnum signature)
		 :last-ckp-signature (the fixnum signature))))
    ;; shadow the global value with a variable which will
    ;; contain our per buffer state information
    (defevar "Auto Save State"
      "This is the \"Save\" mode state information for this buffer."
      :buffer buffer
      :value state)))

(defun cleanup-auto-save-mode (buffer)
  (delete-variable 'auto-save-state
		   :buffer buffer))

(defmode "Save"
  :setup-function 'setup-auto-save-mode
  :cleanup-function 'cleanup-auto-save-mode
  :documentation
  "When in \"Save\" mode, files are automatically checkpointed every *Auto
   Save Checkpoint Frequency* seconds or every *Auto Save Key Count
   Threshold* destructive keystrokes.  If there is a pathname associated
   with the buffer, the filename used for the checkpoint file is controlled
   by the hvar *Auto Save Filename Pattern*.  Otherwise, the hook *Auto
   Save Pathname Hook* is used to generate a checkpoint pathname.  If the
   buffer's pathname changes between checkpoints, the checkpoint file will
   be written under the new name and the old checkpoint file will be
   deleted if it exists.  When a buffer is written out, the checkpoint will
   be deleted if the editor variable *Auto Save Cleanup Checkpoints* is
   true.")
