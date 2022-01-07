;;; Auto-refresh of buffers.

(in-package "ED")

#[ Refresh Mode

{mode:refresh}

Useful to continuosly update a buffer, for example a buffer to a log file,
or to a Linux /proc file like /proc/meminfo.

Also useful to continuously refresh a shell command.

{command:Refresh Mode}
{evariable:Refresh Frequency}
]#

(defun cleanup-refresh-mode (buffer)
  "Cleanup Refresh mode in $buffer."
  (when (editor-bound-p 'refresh-function :buffer buffer)
    (remove-scheduled-event (variable-value 'refresh-function
					    :buffer buffer))
    (setf (variable-value 'refresh-function :buffer buffer) ())))

(defun cleanup-refresh-buffer (buffer)
  (if (editor-bound-p 'refresh-function :buffer buffer)
      (cleanup-refresh-mode buffer)))

(add-hook delete-buffer-hook 'cleanup-refresh-buffer)

(defun setup-refresh-buffer (buffer frequency)
  (let ((lambda (eval `(lambda (time)
			 (declare (ignore time))
			 (setf (buffer-modified ,buffer) ())
			 (setf (buffer-write-date ,buffer)
			       (file-write-date (buffer-pathname ,buffer)))
			 (if (editor-bound-p 'process :buffer ,buffer)
			     (update-shell-buffer ,buffer)
			     (elet ((revert-file-confirm)
				    (revert-consider-checkpoint))
			       (let ((*check-disk-version-consistent*))
				 (revert-file-command t ,buffer t))))))))
    (if (editor-bound-p 'process :buffer buffer)
	(setf (variable-value 'update-process-buffer-quietly
			      :buffer buffer)
	      t))
    (if (editor-bound-p 'refresh-function :buffer buffer)
	(setf (variable-value 'refresh-function :buffer buffer)
	      lambda)
	(defevar "Refresh Function"
	  "The function used to refresh this buffer."
	  :value lambda
	  :buffer buffer))
    (let ((frequency (or frequency
			(if (editor-bound-p 'refresh-frequency
					    :buffer buffer)
			    (variable-value 'refresh-frequency :buffer buffer)
			    (value refresh-frequency)))))
      (if (plusp frequency)
	  (schedule-event frequency
			  (variable-value 'refresh-function
					  :buffer buffer))))))

(defun setup-refresh-mode (buffer)
  "Setup Refresh mode in $buffer."
  ;; Setting *Refresh Frequency* runs hook function setup-refresh-buffer.
  (if (editor-bound-p 'refresh-frequency :buffer buffer)
      (setf (variable-value 'refresh-frequency :buffer buffer)
	    (value refresh-frequency))
      (defevar "Refresh Frequency"
	"The number of seconds between refreshes of this buffer."
	:hooks '(update-refresh-frequency)
	:value (value refresh-frequency)
	:buffer buffer)))

(defmode "Refresh"
  :setup-function 'setup-refresh-mode
  :cleanup-function 'cleanup-refresh-mode
  :documentation
  "In Refresh mode, a buffer is reverted every *Refresh Frequency*
   seconds.")

(defun update-refresh-frequency (&optional name kind where value)
  "Update buffer to new refresh frequency."
  (declare (ignore name))
  (when (eq kind :buffer)
    (cleanup-refresh-mode where)
    (setup-refresh-buffer where value)))

(defevar "Refresh Frequency"
  "The initial refresh frequency, in seconds, for buffers in `Refresh
   Mode'.  A frequency of 0 or less turns off the refreshing."
  :value 2)

(defevar "Refresh Function"
  "The function used to refreshes buffers.  Set per buffer.")
