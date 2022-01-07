;;; Telnet interface.

(in-package "ED")

(defun setup-telnet-buffer (buffer)
  (let ((mark (copy-mark (buffer-point buffer) :right-inserting)))
    (defhvar "Buffer Input Mark"
      "The buffer input mark for this buffer."
      :buffer buffer
      :value mark)
    (defhvar "Interactive History"
      "A ring of the regions input to an interactive mode (Eval or Typescript)."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))
    (defhvar "Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)
    (defhvar "Searching Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)
    (defhvar "Telnet Stream"
      "Inet stream."
      :buffer buffer)
    (defhvar "Telnet Port" ; FIX s/b in stream
      "Inet port."
      :buffer buffer)
    (defhvar "Telnet Account"
      "Account."
      :buffer buffer))
    (defhvar "Telnet Connector"
      "Function to reconnect session."
      :buffer buffer
      :value #'internet:telnet-init)
    (defhvar "Telnet Commander"
      "Function to send command to server."
      :buffer buffer
      :value #'internet:inet-command)
    (or (buffer-modeline-field-p buffer :telnet-status)
	(setf (buffer-modeline-fields buffer)
	      (nconc (buffer-modeline-fields buffer)
		     (list (modeline-field :space)
			   (modeline-field :telnet-status))))))

(defmode "Telnet" :major-p t :setup-function #'setup-telnet-buffer)

(defhvar "Telnet Prompt"
  "String for prompt."
  :value "> ")

(defhvar "Telnet Port"
  "The standard Telnet port."
  :value 23)

(defhvar "FTP Port"
  "The standard FTP port."
  :value 21)

(defhvar "SMTP Port"
  "The standard SMTP port."
  :value 25)

(defhvar "Telnet Timeout"
  "Maximum number of seconds to try telnet to a host."
  :value 30)

(defun cleanup-telnet-buffer (buffer)
  (when (editor-bound-p 'stream :buffer buffer)
    (let ((stream (variable-value 'stream :buffer buffer)))
      (if (and stream (open-stream-p stream))
	  (internet:inet-quit stream)))))

(defun insert-response (mark stream)
  (let ((response (internet:inet-stream-response stream)))
    (when response
      (flet ((write-response (response)
	       (if (char= (char response (1- (length response))) #\return)
		   (insert-string mark response 0 (1- (length response)))
		   (insert-string mark response))
	       (insert-character mark #\newline)))
	(write-response response)
 	(loop while (listen stream) do
	  (write-response (read-line stream ())))))))

(defcommand "Telnet" (p)
  "Telnet to a prompted host.  With a prefix also prompt for the port."
  "Telnet to a prompted host.  With a prefix also prompt for the port."
  (let* ((host (prompt-for-string :prompt "Telnet to: "
				  :help "Enter name of host."))
	 (port (if p
		   (prompt-for-integer
		    :prompt "Port: "
		    :help "Enter name of host."
		    :default (value telnet-port))
		   (value telnet-port)))
	 (buf (make-unique-buffer (format () "~A ~A" host port)
				  :modes '("Telnet")
				  :delete-hook (list #'cleanup-telnet-buffer)))
	 (account (internet:make-inet-account host)))
    (internet:fill-from-netrc account)
    (change-to-buffer buf)
    (multiple-value-bind
	(stream response)
	(internet:telnet-init account
			      port
			      (value telnet-timeout))
      (or stream
	  (editor-error "Failed to connect to ~A on ~A~%  => ~A"
			host port response))
      (setv telnet-stream stream)
      (setv telnet-port (parse-integer port))
      (insert-response (current-point) stream)
      (insert-string (current-point) (value telnet-prompt))
      ;; Move "Buffer Input Mark" to end of buffer.
      (let ((input-region (get-interactive-input)))
	(move-mark (region-start input-region) (region-end input-region)))
      (setv telnet-account account))))

(defhvar "Passive Connection"
  "Passive FTP connection.")

(defun ensure-passive (control)
  "Ensure that a passive FTP connection exists, returning the connection."
  (let ((data (value passive-connection)))
    (if (fi data t (open-stream-p data))
	(setf (value passive-connection)
	      (internet:ftp-connect-passive control)))))

(defun ftp-cmd (stream command &rest command-args)
  "Issue FTP COMMAND on STREAM with COMMAND-ARGS.  In most cases just call
   internet:ftp-command."
  (case= command
    ("ls"
     (let ((data (ensure-passive stream)))
       (with-output-to-mark (out (current-point))
	 (internet:ftp-ls stream data out))))
    (t
     (internet:ftp-command stream command command-args))))

(defun cleanup-ftp-buffer (buffer)
  (when (editor-bound-p 'passive-connection :buffer buffer)
    (let ((stream (variable-value 'passive-connection :buffer buffer)))
      (if (and stream (open-stream-p stream)) (close stream))))
  (cleanup-telnet-buffer buffer))

(defcommand "FTP" (p)
  "FTP to a prompted host.  With a prefix also prompt for the port."
  "FTP to a prompted host.  With a prefix also prompt for the port."
  (let* ((host (prompt-for-string :prompt "FTP to: "
				  :help "Enter name of host."))
	 (port (if p
		   (prompt-for-integer
		    :prompt "Port: "
		    :help "Enter name of host."
		    :default (value ftp-port))
		   (value ftp-port)))
	 (buf (make-unique-buffer (format () "~A ~A" host port)
				  :modes '("Telnet")
				  :delete-hook (list #'cleanup-ftp-buffer)))
	 (account (internet:make-inet-account host)))
    (internet:fill-from-netrc account)
    (change-to-buffer buf)
    (defhvar "Passive Connection"
      "Passive stream (i.e. data connection)."
      :buffer buf)
    (setv telnet-connector #'internet:ftp-init)
    (setv telnet-commander #'ftp-cmd)
    (multiple-value-bind
	(stream response)
	(internet:ftp-init account port (value telnet-timeout))
      (or stream
	  (editor-error "Failed to connect to ~A on ~A~%  => ~A"
			host port response))
      (setv telnet-stream stream)
      (setv telnet-port port)
      (insert-response (current-point) stream)
      (ftp-cmd stream "SYST")
      (insert-response (current-point) stream)
      (insert-string (current-point) (value telnet-prompt))
      ;; Move "Buffer Input Mark" to end of buffer.
      (let ((input-region (get-interactive-input)))
	(move-mark (region-start input-region) (region-end input-region)))
      (setv telnet-account account))))

(defcommand "SMTP" (p)
  "SMTP to a prompted host.  With a prefix also prompt for the port."
  "SMTP to a prompted host.  With a prefix also prompt for the port."
  (let* ((host (prompt-for-string :prompt "SMTP to: "
				  :help "Enter name of host."))
	 (port (if p
		   (prompt-for-integer
		    :prompt "Port: "
		    :help "Enter name of host."
		    :default (value smtp-port))
		   (value smtp-port)))
	 (buf (make-unique-buffer (format () "~A ~A" host port)
				  :modes '("Telnet")
				  :delete-hook (list #'cleanup-telnet-buffer)))
	 (account (internet:make-inet-account host () ())))
    (internet:fill-from-netrc account)
    (change-to-buffer buf)
    (setv telnet-connector #'internet:smtp-init)
    (setv telnet-commander #'internet:inet-command)
    (multiple-value-bind
	(stream response)
	(internet:smtp-init account port (value telnet-timeout))
      (or stream
	  (editor-error "Failed to connect to ~A on ~A~%  => ~A"
			host port response))
      (setv telnet-stream stream)
      (setv telnet-port port)
      (insert-response (current-point) stream)
      (insert-string (current-point) (value telnet-prompt))
      ;; Move "Buffer Input Mark" to end of buffer.
      (let ((input-region (get-interactive-input)))
	(move-mark (region-start input-region) (region-end input-region)))
      (setv telnet-account account))))

(defcommand "Confirm Telnet Input" (p)
  "Evaluate Telnet Mode input between the point and last prompt."
  "Evaluate Telnet Mode input between the point and last prompt."
  (declare (ignore p))
  (or (editor-bound-p 'telnet-stream :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (let* ((stream (value telnet-stream))
	 (input-region (get-interactive-input))
	 (point (current-point))
	 (command (region-to-string input-region)))
    (insert-character point #\newline)
    (or (open-stream-p stream)
	(progn
	  (setq stream (or (funcall (value telnet-connector)
				    (value telnet-account)
				    (value telnet-port)
				    (value telnet-timeout))
			   (editor-error "Failed to reconnect.")))
	  (message "Reconnected.")
	  (setv telnet-stream stream)
	  (insert-response (current-point) stream)))
    (and (funcall (value telnet-commander) stream command)
	 (string= (string-upcase command) "QUIT")
	 (close stream))
    (insert-response point stream)
    (insert-string point (value telnet-prompt))
    ;; Move "Buffer Input Mark" to end of buffer.
    (move-mark (region-start input-region) (region-end input-region))
    (update-modeline-field (current-buffer) (current-window)
			   (modeline-field :telnet-status))))


;;;; Modeline support.

(defun modeline-telnet-status (buffer window)
  (declare (ignore window))
  (when (editor-bound-p 'telnet-stream :buffer buffer)
    (let ((stream (variable-value 'telnet-stream :buffer buffer)))
      (if (and stream (open-stream-p stream)) "Open" "Closed"))))

(make-modeline-field :name :telnet-status :replace t
		     :function #'modeline-telnet-status)
