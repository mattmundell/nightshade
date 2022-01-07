;;; Telnet interface.

(in-package "ED")

;; FIX gensym this symbol in case input hook uses a *insertp*
(declaim (special *insertp*))

(defvar *telnet-mess* ())
(defvar *telnet-mess-level* 0)

(defun incf-telnet-mess () (incf *telnet-mess-level* 2))
(defun decf-telnet-mess () (decf *telnet-mess-level* 2))

(defun telnet-mess (format &rest args)
  (when *telnet-mess*
    (funcall *telnet-mess* "~A~A"
	     (make-string *telnet-mess-level* :initial-element #\space)
	     (apply #'funcall #'format () format args))))

(defcommand "Toggle Telnet Trace" ()
  "Toggle tracing output from telnet client communication."
  (setq *telnet-mess* (fi *telnet-mess* #'ed::message)))


;;; Internal
;;;
;;; A list of (fd stream buffer handler) for each connection.
;;;
(defvar *streams* ())

;;; Internal
;;;
;;; Return the stream on success, else ().
;;;
(defun init-telnet (buffer &optional
			   (name "Telnet")
			   (handler #'handle-telnet))
  (let* ((stream (internet::raw-connect
		  (internet:inet-account-server
		   (variable-value 'telnet-account :buffer buffer))
		  (variable-value 'telnet-port :buffer buffer)
		  (variable-value 'telnet-timeout :buffer buffer)
		  name)))
    (when stream
      (pushnew (list (system::fd-stream-fd stream)
		     stream
		     buffer
		     (system:add-fd-handler
		      (system::fd-stream-fd stream)
		      :input handler))
	       *streams*)
      (setf (variable-value 'telnet-stream :buffer buffer) stream))))

(defun cleanup-telnet-buffer (buffer)
  (let ((assoc (rassoc buffer *streams* :key #'cadr)))
    (when assoc
      (system:remove-fd-handler (cadddr assoc))
      (setq *streams* (delete (car assoc) *streams* :key #'car))))
  (when (editor-bound-p 'stream :buffer buffer)
    (let ((stream (variable-value 'stream :buffer buffer)))
      (if (and stream (open-stream-p stream))
	  (internet:inet-quit stream)))))

(defun close-telnet-stream (stream)
  (let ((assoc (assoc (fd-stream-fd stream) *streams*)))
    (when assoc
      (system:remove-fd-handler (cadddr assoc))
      (setq *streams* (delete (car assoc) *streams* :key #'car))))
  (close stream))

(defun telnet-para-command (buffer stream command &rest command-args)
  (declare (ignore buffer))
  (apply #'internet:inet-para-command stream command command-args))

(defun telnet-normal-command (buffer stream command &rest command-args)
  (declare (ignore buffer))
  (apply #'internet:inet-command stream command command-args))

(defun setup-telnet-buffer (buffer)
  (insert-string (buffer-point buffer) (value telnet-prompt))
  (let ((mark (copy-mark (buffer-point buffer) :right-inserting)))
    (defevar "Buffer Input Mark"
      "The buffer input mark for this buffer."
      :buffer buffer
      :value mark)
    (defevar "Server Mark"
      "Mark for inserting server messages."
      :buffer buffer
      :value (copy-mark (buffer-start-mark buffer) :left-inserting))
    (defevar "Interactive History"
      "A ring of the regions input to an interactive mode (Eval or Typescript)."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))
    (defevar "Interactive Pointer"
      "Pointer into `Interactive History'."
      :buffer buffer
      :value 0)
    (defevar "Searching Interactive Pointer"
      "Pointer into `Interactive History'."
      :buffer buffer
      :value 0)
    (defevar "Telnet Stream"
      "Inet stream."
      :buffer buffer)
    (defevar "Telnet Port" ; FIX s/b in stream
      "Inet port."
      :buffer buffer)
    (defevar "Telnet Expect Response"
      "True if a response follows each command."
      :value t
      :buffer buffer)
    (defevar "Telnet Prompt"
      "The prompt string."
      :value (value telnet-prompt)
      :buffer buffer)
    (defevar "Telnet Account"
      "Account."
      :buffer buffer))
    (defevar "Telnet Connector"
      "Function to reconnect session."
      :buffer buffer
      :value #'init-telnet)
    (defevar "Telnet Commander"
      "Function to send command to server."
      :buffer buffer
      :value #'telnet-para-command)
    (defevar "Telnet Closer"
      "Function to close the stream.  Takes the stream."
      :buffer buffer
      :value #'close-telnet-stream)
    (defevar "Telnet Timeout"
      "Timeout on connection."
      :buffer buffer
      :value (value telnet-timeout))
    (defevar "Telnet Input Hook"
      "Function called to determine how to insert input into buffer.

       Accept mark, line string and stream.  Return :done to stop any
       further functions running.  Clear *insertp* to prevent line being
       inserted."
      :buffer buffer)
    (or (buffer-modeline-field-p buffer :telnet-status)
	(setf (buffer-modeline-fields buffer)
	      (nconc (buffer-modeline-fields buffer)
		     (list (modeline-field :space)
			   (modeline-field :telnet-status))))))

(defmode "Telnet" :major-p t :setup-function #'setup-telnet-buffer)

(defevar "Telnet Prompt"
  "String for prompt."
  :value "> ")

(defevar "Telnet Port"
  "The standard Telnet port."
  :value 23)

(defevar "FTP Port"
  "The standard FTP port."
  :value 21)

(defevar "SMTP Port"
  "The standard SMTP port."
  :value 25)

(defevar "IRC Port"
  "The standard IRC port."
  :value 6667)

(defevar "Gopher Port"
  "The standard Gopher port."
  :value 70)

(defevar "Telnet Timeout"
  "Maximum number of seconds to try telnet to a host."
  :value 30)

(defun insert-response (mark stream)
  (let ((response (internet:inet-stream-response stream)))
    (when response
      (flet ((write-response (response)
	       (if (and (plusp (length response))
			(char= (char response (1- (length response)))
			       #\return))
		   (insert-string mark response 0 (1- (length response)))
		   (insert-string mark response))
	       (insert-character mark #\newline)))
	(write-response response)
 	(while () ((ignore-errors (listen stream)))
	  (write-response (read-line stream ())))))))

;;; Internal
;;;
;;; Size of read buffer used by `handle-telnet'.
;;;
(defvar *handle-telnet-array-size* 2048)

(defun read-any-bytes (stream array start max eof-errorp)
  (let ((stream (lisp::in-synonym-of stream)))
    (or (lisp::lisp-stream-p stream)
	(error "$stream must be a lisp stream"))
    (while ((start start (1+ start))
	    (count max (1- count)))
	   ((and (plusp count) (listen stream))
	    (- max count))
      (let ((char (read-char stream eof-errorp)))
	(when char
	  (setf (aref array start)
		(char-code char)))))))

(defun telnet-insert-array (mark array stream filters)
  (let ((count (length array)))
    (when (plusp count)
      (let* ((line
	      (map 'string #'code-char
		   (subseq
		    array 0
		    (if (and (> count 1)
			     (eq (aref array (- count 1))
				 (char-code #\newline))
			     (eq (aref array (- count 2))
				 (char-code #\return)))
			(progn
			  (setf (aref array (- count 2))
				(char-code #\newline))
			  (- count 1))
			count)))))
	(dolist (function filters
		 (when *insertp*
		   (insert-string mark line)))
	  (case (funcall function mark line stream)
	    (:done (return))))))))

(defun handle-telnet (fd)
  (ignore-errors
   (let* ((assoc (assoc fd *streams*))
	  (stream (cadr assoc)))
     (when stream
       (let ((buffer (caddr assoc))
	     (handler (cadddr assoc)))
	 (when handler
	   (handler-bind ((error (lambda (condition)
				   (system:remove-fd-handler handler)
				   (setq *streams*
					 (delete fd *streams*
						 :key #'car))
				   (message
				    "Cleared handling of fd ~A: ~A.~%~%~A"
				    fd condition
				    (with-output-to-string
					(debug::*debug-io*)
				      (debug:backtrace))))))

	     (or buffer
		 (setf (car (cdr assoc))
		       (make-unique-buffer (format () "Telnet fd~A" fd)
					   :modes '("Telnet"))))
	     (let ((mark (variable-value 'server-mark :buffer buffer)))
	       (when mark
		 (flet ((close-telnet ()
			  (system:remove-fd-handler handler)
			  (setq *streams* (delete fd *streams*
						  :key #'car))
			  (insert-string mark "Connection closed.")
			  (insert-character mark #\newline)))
		   (if (open-stream-p stream)
		       (while ((array (make-array
				       *handle-telnet-array-size*)))
			      ((listen stream)
			       (or (open-stream-p stream)
				   (close-telnet)))
			 (let ((count (read-any-bytes
				       stream array 0
				       *handle-telnet-array-size* ()))
			       (*insertp* t))
			   (or (plusp count) (return))
			   (while ((arrays (split (subseq array 0 count)
						  (list
						   (char-code #\return)
						   (char-code #\newline)))
					   (cdr arrays)))
				  (arrays)
			     (let ((array (car arrays)))
			       (telnet-insert-array
				mark array stream
				(variable-value 'telnet-input-hook
						:buffer buffer))))))
		       (close-telnet)))))
	     (setf (buffer-modified buffer) ()))))))))

(defun prompt-telnet
       (p &key (prompt "Host: ")
	  (port (value telnet-port))
	  (help "Enter a host name.

To specify the port, follow the host name with space and a port number."))
  (let* ((host (prompt-for-string :prompt prompt
				  :help help))
	 (port (if p
		   (prompt-for-integer
		    :prompt "Port: "
		    :help "Enter port."
		    :default port)
		   (or (let* ((pos (position #\space host :from-end t))
			      (int (if pos
				       (parse-integer
					host
					:start pos
					:junk-allowed t))))
			 (when int
			   (setq host
				 (subseq host
					 0
					 (position #\space host)))
			   int))
		       port))))
    (values host port)))

(defcommand "Telnet" (p)
  "Telnet to a prompted host.  With a prefix also prompt for the port."
  (multiple-value-bind (host port)
		       (prompt-telnet p
				      :prompt "Telnet to: ")
    (let ((buf (make-unique-buffer (format () "~A ~A" host port)
				   :modes '("Telnet")
				   :delete-hook
				   (list #'cleanup-telnet-buffer)))
	  (account (internet:make-inet-account host)))
      (internet:fill-from-netrc account)
      (change-to-buffer buf)
      ;; Move "Buffer Input Mark" to end of buffer.
      (let ((input-region (get-interactive-input)))
	(move-mark (region-start input-region) (region-end input-region)))
      (setv telnet-account account)
      (setv telnet-port port)
      (funcall (value telnet-connector) buf))))

#|
      (fi* (open-stream-p stream)
	(message "Server closed connection.")
	(update-modeline-field (current-buffer) (current-window)
			       (modeline-field :telnet-status))))))
|#

(defcommand "Confirm Telnet Input" ()
  "Evaluate Telnet Mode input between the point and last prompt."
  (or (editor-bound-p 'telnet-stream :buffer (current-buffer))
      (editor-error "Must be in a process buffer."))
  (let* ((stream (value telnet-stream))
	 (input-region (get-interactive-input))
	 (point (current-point))
	 (command (region-to-string input-region)))
    (insert-character point #\newline)
    (if (value telnet-expect-response)
	(progn
	  (insert-character point #\newline)
	  (move-mark (value server-mark) point)
	  (insert-string point (value telnet-prompt))
	  (line-offset (value server-mark) -1 0))
	(progn
	  (move-mark (value server-mark) point)
	  (insert-string point (value telnet-prompt))
	  (line-start (value server-mark))))
    ;; Move "Buffer Input Mark" to end of buffer.
    (move-mark (region-start input-region) (region-end input-region))
    (or (open-stream-p stream)
	(progn
	  (editor-error "Connection timed out.")
#|
	  (setq stream (or (funcall (value telnet-connector)
				    (value telnet-account)
				    (value telnet-port)
				    (value telnet-timeout))
			   (editor-error "Failed to reconnect.")))
	  (message "Reconnected.")
	  (setv telnet-stream stream)
	  (insert-response (current-point) stream)
|#
	))
    (let ((buffer (current-buffer))
	  (window (current-window)))
      (setf (buffer-modified buffer) ())
      ;; The telnet-commander may change current buffer and window.
      (funcall (value telnet-commander) buffer stream command)
      (update-modeline-field buffer window
			     (modeline-field :telnet-status)))
    (or (open-stream-p stream)
	(message "Server closed connection."))))


;;;; File Transfer Protocol (FTP) client.

(defevar "Passive Connection"
  "True for passive FTP connection.")

(defun ensure-passive (control)
  "Ensure that a passive FTP connection exists, returning the connection."
  (let ((data (value passive-connection)))
    (if (fi data t (open-stream-p data))
	(setf (value passive-connection)
	      (internet:ftp-connect-passive control)))))

;; FIX use stream cached in internet:*controllers*?
(defun ftp-cmd (stream command &rest command-args)
  "Issue FTP COMMAND on STREAM with COMMAND-ARGS.  In most cases just call
   internet:ftp-command."
  (case= command
    ("ls"
     (let ((data (ensure-passive stream)))
       (with-output-to-mark (out (current-point))
	 (internet:ftp-ls stream data out "" :long))))
    (t
     (internet:ftp-command stream command command-args))))

(defun cleanup-ftp-buffer (buffer)
  (when (editor-bound-p 'passive-connection :buffer buffer)
    (let ((stream (variable-value 'passive-connection :buffer buffer)))
      (if (and stream (open-stream-p stream)) (close stream))))
  (cleanup-telnet-buffer buffer))

#|
(defcommand "FTP" (p)
  "FTP to a prompted host.  With a prefix also prompt for the port."
  (multiple-value-bind (host port)
		       (prompt-telnet p
				      :prompt "FTP to: "
				      :port (value ftp-port))
    (let ((buf (make-unique-buffer (format () "~A ~A" host port)
				   :modes '("Telnet")
				   :delete-hook (list #'cleanup-ftp-buffer)))
	  (account (internet:make-inet-account host)))
      (internet:fill-from-netrc account)
      (message "Logging into ~A as ~A..."
	       (internet:inet-account-server account)
	       (internet:account-user account))
      (change-to-buffer buf)
      (defevar "Passive Connection"
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
	(ftp-cmd stream "STAT")
	(insert-response (current-point) stream)
	(ftp-cmd stream "SYST")
	(insert-response (current-point) stream)
	(insert-string (current-point) (value telnet-prompt))
	;; Move "Buffer Input Mark" to end of buffer.
	(let ((input-region (get-interactive-input)))
	  (move-mark (region-start input-region) (region-end input-region)))
	(setv telnet-account account)))))
|#

(defun init-ftp (buffer)
  (let* ((stream (internet:ftp-init
		  (variable-value 'telnet-account :buffer buffer)
		  (variable-value 'telnet-port :buffer buffer)
		  (variable-value 'telnet-timeout :buffer buffer))))
    (when stream
      (let ((mark (variable-value 'server-mark :buffer buffer)))
	(insert-string mark (internet:inet-stream-response stream))
	(insert-character mark #\newline))
      (pushnew (list (system::fd-stream-fd stream)
		     stream
		     buffer
		     (system:add-fd-handler
		      (system::fd-stream-fd stream)
		      :input #'handle-telnet))
	       *streams*)
      (setf (variable-value 'telnet-stream :buffer buffer) stream))))

(defun command-ftp (buffer stream command &rest command-args)
  (declare (ignore buffer))
  (internet:ftp-command stream command command-args)
  (let* ((assoc (assoc (fd-stream-fd stream) *streams*))
	 (stream (cadr assoc)))
    (when stream
      (let* ((buffer (caddr assoc))
	     (mark (variable-value 'server-mark :buffer buffer))
	     (*insertp* t)
	     (line (internet:inet-stream-response stream)))
	;; FIX split line as in handle-telnet
	(dolist (function
		 (variable-value 'telnet-input-hook
				 :buffer buffer)
		 (when *insertp*
		   (insert-string mark line)
		   (insert-character mark #\newline)))
	  (case (funcall function mark line stream)
	    (:done (return))))))))

(defcommand "FTP" (p)
  "FTP to a prompted host.  With a prefix also prompt for the port."
  (multiple-value-bind (host port)
		       (prompt-telnet p
				      :prompt "FTP to: "
				      :port (value ftp-port))
    (let* ((buf (make-unique-buffer
		 (format () "~A ~A" host port)
		 :modes '("Telnet")
		 :delete-hook (list #'cleanup-telnet-buffer)))
	   (account (internet:make-inet-account host () ())))
      (internet:fill-from-netrc account)
      (change-to-buffer buf)
      ;; Move "Buffer Input Mark" to end of buffer.
      (let ((input-region (get-interactive-input)))
	(move-mark (region-start input-region) (region-end input-region)))
      (setv telnet-connector #'init-ftp)
      (setv telnet-commander #'command-ftp)
      (setv telnet-account account)
      (setv telnet-port port)
      (funcall (value telnet-connector) buf))))


;;;; Simple Mail Transfer Protocol client.
;;;
;;; Like `Telnet' to *SMTP Port* with automatic login.

(defcommand "SMTP" (p)
  "SMTP to a prompted host, logging in automatically.  With a prefix also
   prompt for the port."
  (multiple-value-bind (host port)
		       (prompt-telnet p
				      :prompt "SMTP to: "
				      :port (value smtp-port))
    (let ((buf (make-unique-buffer (format () "~A ~A" host port)
				   :modes '("Telnet")
				   :delete-hook
				   (list #'cleanup-telnet-buffer)))
	  (account (internet:make-inet-account host () ())))
      (internet:fill-from-netrc account)
      (change-to-buffer buf)
      (setv telnet-connector #'internet:smtp-init)
      (setv telnet-commander #'telnet-normal-command)
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
	  (move-mark (region-start input-region)
		     (region-end input-region)))
	(setv telnet-account account)))))


;;;; Internet Relay Chat (IRC) client.

#[ IRC

The `IRC' command enters the Internet Relay Chat (IRC) client.

{command:IRC}
{evariable:IRC Autojoins}
{evariable:IRC Dir}
]#

(defevar "IRC Autojoins"
  "A list defining channels to join automatically on connecting to IRC
   servers.

   Each entry in the list is like (\"server\" (\"chan1\" \"chan2\")).

   The leading # on channel names is optional.")

(defevar "IRC Dir"
  "Directory in which logs are stored."
  :value "home:IRC/")

(defun irc-para-command (stream account command line)
  (when (open-stream-p stream)
    (log-irc :out account line command)
    (internet::inet-para-command stream line)))

(defun irc-name (account)
  (safe-subseq (or (internet::inet-account-user account)
		   (user-name))
	       0 15))

(defun init-irc (buffer)
  (let ((stream (init-telnet buffer "IRC" #'handle-telnet)))
    (when stream
      (let ((account (variable-value 'telnet-account :buffer buffer)))
	(when account
	  ;; FIX inet.lisp
	  (internet::write-inet stream
				"PASS ~A"
				`(,(or (internet::inet-account-password
					account)
				       (user-name))))
	  (internet::write-inet stream
				"NICK ~A"
				`(,(irc-name account)))
	  (internet::write-inet stream
				"USER ~A ~A server :~A"
				(list
				 (irc-name account)
				 (machine-instance)
				 (let ((full (user-full-name)))
				   (if (plusp (length full))
				       full "FIX"))))
	  (if (internet::inet-account-password account)
	      (internet::write-inet
	       stream
	       "NickServ IDENTIFY ~A ~A"
	       (list (internet::inet-account-password account)
		     (irc-name account))))
	  ;; FIX confirm registered first?
	  (let ((assoc (assoc (internet::inet-account-server account)
			      (value irc-autojoins)
			      :test #'string=)))
	    (dolist (channel (cadr assoc))
	      (if (plusp (length channel))
		  (irc-para-command
		   stream account :JOIN
		   (concatenate 'simple-string
				"JOIN "
				(if (char= (char channel 0) #\#)
				    channel
				    (concatenate 'simple-string
						 "#" channel))))))))))))

;                        10:49:41
(defvar *dummy-irc-now* "        ")

(defun irc-now ()
  (format-time
   ()
   :print-seconds t
   :print-meridian ()
   :print-timezone ()
   :print-weekday ()
   :print-date ()))

(defun parse-irc-address (address)
  (let ((pos (position #\! address)))
    (if pos
	(values (subseq address 0 pos)
		;; FIX check for leading ~
		(subseq address (1+ pos)))
	address)))

(defun parse-irc-line (line)
  (when (and line (> (length line) 1))
    (fi (char= (char line 0) #\:)
	(let* ((rest-pos (position #\: line :start 1))
	       (command (string-right-trim '(#\space #\tab)
					   (subseq line 0 rest-pos))))
	  (values t
		  ()
		  (intern (string-upcase command) *keyword-package*)
		  ()
		  (if rest-pos
		      (subseq line (1+ rest-pos))
		      "")))
	(let* ((rest-pos (position #\: line :start 1))
	       (message (string-right-trim '(#\space #\tab)
					   (subseq line 1 rest-pos)))
	       ;; FIX handle multiple whitespace between each
	       (split (split message '(#\space #\tab))))
	  (when split
	    (multiple-value-bind (nick address)
				 (parse-irc-address (car split))
	      (values
	       t
	       nick
	       (when (> (length split) 1)
		 (let* ((command (cadr split))
			(int (parse-integer command
					    :errorp ())))
		   (if int
		       int
		       (intern (string-upcase command)
			       *keyword-package*))))
	       (cddr split)
	       (if rest-pos
		   (subseq line (1+ rest-pos))
		   "")
	       address)))))))
#| TODO test
(parse-irc-line ":mattm MODE mattm :+R")
(parse-irc-line ":NickServ!services@services.oftc.net NOTICE mattm :You are successfully identified as ^Bmattm^B.")
(parse-irc-line ":solenoid.oftc.net 353 mattm = #debian-med :FloodServ")
(parse-irc-line ":adrian!~adrian@87.117.229.252 PRIVMSG mattm :thx")
(parse-irc-line ":adrian!~adrian@87.117.229.252 PRIVMSG mattm :")
(parse-irc-line ":adrian!~adrian@87.117.229.252 PRIVMSG #test :thx")
(parse-irc-line ":mwiegand!~michael@aktaia.intevation.org QUIT :Quit: Leaving")
(parse-irc-line "PING :irc.a.b")
(parse-irc-line ":bchandra!~bchandra@122.167.72.108 JOIN :#openvas")
|#

(defun irc-insert-string (mark string)
  (insert-string mark string)
  (insert-character mark #\newline))

(defun irc-format (mark format-string &rest args)
  (insert-string mark (apply #'format () format-string args))
  (insert-character mark #\newline))

(defvar *channel-buffers* (make-string-table))

(defun make-irc-buffer (channel name parent)
  (let* ((buffer (make-unique-buffer
		  name
		  :modes '("Telnet" "IRC")
		  :delete-hook (list #'cleanup-irc-buffer)))
	 (account (variable-value 'telnet-account :buffer parent)))
    (setf (variable-value 'telnet-stream :buffer buffer)
	  (variable-value 'telnet-stream :buffer parent))
    (setf (variable-value 'telnet-connector :buffer buffer) #'init-irc)
    (setf (variable-value 'telnet-commander :buffer buffer) #'send-irc)
    (setf (variable-value 'telnet-expect-response :buffer buffer) ())
    (setf (variable-value 'telnet-prompt :buffer buffer)
	  (format () "~A ~15<~A~>> "
		  *dummy-irc-now*
		  (irc-name account)))
    (setf (variable-value 'telnet-port :buffer buffer)
	  (variable-value 'telnet-port :buffer parent))
    (setf (variable-value 'telnet-account :buffer buffer) account)
    (setf (variable-value 'telnet-input-hook :buffer buffer)
	  (list #'handle-irc-input))
    (defevar "IRC Server"
      "Buffer of server of this channel or conversation."
      :value parent
      :buffer buffer)
    (defevar "IRC Channel"
      "Channel or personal conversation in buffer."
      :value channel
      :buffer buffer)
    (defevar "IRC Channels"
      "Channels joined or personal conversations started in buffer."
      :buffer buffer)
    (defevar "Kill Buffer Prompt to Save"
      "Whether `Kill Buffer' prompts to save modified buffers."
      :buffer buffer)
    buffer))

(defun channel-mark (account name parent-buffer)
  (let* ((channel name)
	 (server (internet:inet-account-server account))
	 (name (concatenate 'simple-string name " " server))
	 (mark (getstring name *channel-buffers*)))
    (or mark
	(let ((buffer (make-irc-buffer channel name parent-buffer)))
	  (defevar "IRC Channel"
	    "Channel or personal conversation in buffer."
	    :value channel
	    :buffer buffer)
	  (push buffer
		(variable-value 'irc-channels :buffer parent-buffer))
	  (setf (getstring name *channel-buffers*)
		(variable-value 'server-mark :buffer buffer))))))

(defun channel-buffer-exists (account name)
  (let* ((server (internet:inet-account-server account))
	 (name (concatenate 'simple-string name " " server)))
    (getstring name *channel-buffers*)))

(defevar "IRC Log Skip Commands"
  "Commands to skip when logging IRC lines.  Commands must be keywords."
  :value '(:ping :pong))

(defun log-irc (direction account line &optional command)
  (or (member command (value irc-log-skip-commands))
      (ecase direction
	((:in :out)
	 (multiple-value-bind (sec min hr day month year)
			      (get-decoded-time)
	   (declare (ignore sec min hr))
	   (let* ((name (format () "~A/~A/~2,'0D/~2,'0D"
				(internet:inet-account-server account)
				year month day))
		  (pathname (merge-pathnames name (value irc-dir))))
	     (ensure-directories-exist pathname)
	     (to-file (out pathname :if-exists :append
			   :if-does-not-exist :create)
	       (format out "~C ~A ~A~%"
		       (if (eq direction :in) #\< #\>)
		       (irc-now)
		       line))))))))

#|
;;; ensure-channel-displayed  --  Internal
;;;
;;; As this can happen at any time, point may be in the
;;; Echo window, so calling `change-to-buffer' or
;;; `current-window' would cause problems.
;;;
;;; FIX this is generic
;;;
(defun ensure-channel-displayed (channel-buffer)
  (or (dolist (window *window-list*)
	(if (equal (window-buffer window)
		   channel-buffer)
	    (return t)))
      (let ((current (find *echo-area-window*
			   *window-list*
			   :test-not #'equal)))
	(if (eq current (next-window current))
	    (split-window-command () current))
	(setf (window-buffer (next-window current))
	      channel-buffer))))
|#
(defun ensure-channel-displayed (channel-buffer)
  (if (eq (current-window) *echo-area-window*)
      (message "Newly joined channel or /msg in buffer ~A"
	       (buffer-name channel-buffer))
      (or (dolist (window *window-list*)
	    (if (equal (window-buffer window)
		       channel-buffer)
		(return t)))
	  (let ((current (current-window)))
	    (if (eq current (next-window current))
		(split-window-command))
	    (setf (window-buffer (next-window (current-window)))
		  channel-buffer)))))

(defun handle-irc-input (mark line stream)
  (let ((account (variable-value 'telnet-account
				 :buffer (mark-buffer mark))))
    (telnet-mess "line: ~A" line)
    (multiple-value-bind
	(success nick command subcommands rest)
	(parse-irc-line line)
      (log-irc :in account line (if success command))
      (when success
	(setq *insertp* ())

	;(setf (getf line irc-original-line) line)

	(cond ((and account
		    (eq command :ping))
	       (irc-para-command
		stream account :PONG
		(format () "PONG ~A :~A"
			(subseq line 6)
			(irc-name account))))
	      ((eq command :pong))
	      ((eq command :privmsg)
	       (when (and (plusp (length nick)) subcommands)
		 (let ((channel-mark
			(channel-mark
			 account
			 (if (string= (car subcommands)
				      (irc-name account))
			     nick
			     (car subcommands))
			 (mark-buffer mark))))
		   (irc-format channel-mark
			       "~A ~15<~A~>> ~A"
			       (irc-now) nick rest)
		   (ensure-channel-displayed
		    (mark-buffer channel-mark)))))
	      ((eq command :join)
	       (let ((channel-mark (channel-mark
				    account
				    rest
				    (mark-buffer mark))))
		 (irc-format channel-mark
			     "~A  ~A has joined channel ~A"
			     (irc-now) nick
			     (variable-value 'irc-channel
					     :buffer (mark-buffer
						      channel-mark)))
		 (when (string= nick (irc-name account))
		   (ensure-channel-displayed (mark-buffer
					      channel-mark)))))
	      ((eq command :part)
	       ;; Check if the buffer exists, as `cleanup-irc-buffer' may
	       ;; have sent the PART.
	       (if (channel-buffer-exists
		    account
		    (buffer-name (mark-buffer mark)))
		   (let ((channel-mark (channel-mark
					account
					(car subcommands)
					(mark-buffer mark))))
		     (irc-format channel-mark
				 "~A  ~A has left channel ~A (~A)"
				 (irc-now) nick
				 (car subcommands) rest))))
	      ((eq command :quit)
	       ;; FIX also in personal,channel buffers
	       (irc-format mark
			   "~A  ~A has quit (~A)"
			   (irc-now) nick rest))
	      ((eq command :notice)
	       (irc-format mark
			   "~A  ~A ~A: ~A"
			   (irc-now) nick
			   subcommands
			   rest))
	      ((integerp command)
	       (irc-format mark
			   "~A  ~A ~A: ~A"
			   (irc-now) nick
			   subcommands
			   rest))
	      (t (setq *insertp* t)))))))

(defvar *irc-commands* (make-string-table))

(defmacro def-irc-command (command &optional string)
  (let ((command-var (gensym)))
    `(let ((,command-var (string-upcase ,command)))
       (setf (getstring ,command-var *irc-commands*)
	     (or ,string t)))))

(def-irc-command "BYE" "QUIT")
(def-irc-command "JOIN")
(def-irc-command "MSG")
(def-irc-command "NICK")
(def-irc-command "NOTICE")
(def-irc-command "PART")
(def-irc-command "PASS")
(def-irc-command "PING")
(def-irc-command "PONG")
(def-irc-command "PRIVMSG")
(def-irc-command "USER")
(def-irc-command "QUIT")

(defun irc-cmd (command)
  (multiple-value-bind
      (prefix key value)
      (complete-string command (list *irc-commands*))
    (if (eq key :ambiguous)
	(editor-error "Ambiguous command: ~A" command)
	(if prefix
	    (if (eq value t) prefix value)
	    command))))

(defun send-irc (buffer stream command &rest command-args)
  (declare (ignore command-args))
  (telnet-mess "command: ~A" command)
  (when (plusp (length command))
    (if (char= (char command 0) #\/)
	(let* ((command (string-trim '(#\space #\tab) command))
	       (space (position #\space command :start 1)) ; FIX or #\tab
	       (name (string-upcase
		      (irc-cmd (if space
				   (subseq command 1 space)
				   (subseq command 1)))))
	       (rest (if space
			 (subseq command space)
			 (case= name
			   ("PART"
			    ;; FIX check if var exists
			    (variable-value 'irc-channel
					    :buffer
					    buffer))
			   (t name))))
	       (account (variable-value 'telnet-account
					:buffer buffer)))
	  (case= name
	    ("MSG"
	     (let ((split (split (string-trim '(#\space #\tab) rest)
				 '(#\space #\tab))))
	       (when split
		 (or (and (eq (length split) 1)
			  (zerop (length (car split))))
		     (progn
		       (change-to-buffer (mark-buffer
					  (channel-mark
					   account
					   (car split)
					   buffer)))
		       (when (cdr split)
			 (let ((msg (subseq rest
					  ;; Extra 1 for space.
					  (+ (length (car split))
					     2))))
			   (irc-para-command
			    stream account :PRIVMSG
			    (format ()
				    "PRIVMSG ~A :~A"
				    (car split) msg))
			   (let ((mark (value server-mark)))
			     (insert-string mark (value telnet-prompt))
			     (insert-string mark msg)
			     (insert-character mark #\newline)))))))))
	    (t (irc-para-command
		stream account
		(intern (string-upcase name) *keyword-package*)
		(concatenate 'simple-string
			     name " " rest)))))
	(let ((account (variable-value 'telnet-account :buffer buffer)))
	  (if (variable-value 'irc-channel :buffer buffer)
	      (irc-para-command
	       stream account :PRIVMSG
	       (format ()
		       "PRIVMSG ~A :~A"
		       (variable-value 'irc-channel :buffer buffer)
		       command))
	      (irc-para-command stream account () command))))))

(defun highlight-irc-line (line chi-info)
  (let* ((line-string (line-string line))
	 (pos (position #\> line-string)))
    (if pos
	(progn
	  (chi-mark line 0 *original-font* :preprocessor chi-info)
	  (chi-mark line (1+ pos) *original-font*
		    (let ((mark (mark line (1+ pos))))
		      (if (and (find-attribute mark :space #'zerop)
			       (char= (next-character mark) #\\))
			  :error
			  :window-foreground))
		    chi-info))
	(chi-mark line 0 *original-font* :special-form chi-info))))

(defun highlight-irc-buffer (buffer)
  (highlight-chi-buffer buffer highlight-irc-line))

(defun highlight-visible-irc-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-irc-line))

(defun setup-irc-mode (buffer)
  (highlight-visible-irc-buffer buffer)
  (pushnew '("IRC" () highlight-visible-irc-buffer) *mode-highlighters*))

(defmode "IRC" :major-p () :setup-function #'setup-irc-mode)

(defun cleanup-irc-buffer (buffer)
  (let ((channel (variable-value 'irc-channel :buffer buffer))
	(stream (variable-value 'telnet-stream :buffer buffer))
	(account (variable-value 'telnet-account :buffer buffer)))
    (if channel
	(let ((parent-buffer (variable-value 'irc-server
					     :buffer buffer)))
	  ;;; Channel or personal conversation buffer.
	  (if (and (plusp (length channel))
		   (char= (char channel 0) #\#))
	      ;;; Channel buffer.
	      (ignore-errors
	       (irc-para-command
		stream account :PART
		(format () "PART ~A" channel))))
	  (setf (variable-value 'irc-channels :buffer parent-buffer)
		(delq buffer
		      (variable-value 'irc-channels
				      :buffer parent-buffer))))
	;;; Server buffer.
	(progn
	  (dolist (buffer
		   (variable-value 'irc-channels :buffer buffer))
	    (kill-buffer-command () (buffer-name buffer)))
	  (ignore-errors
	   (irc-para-command stream account :QUIT "QUIT")))))
  (cleanup-telnet-buffer buffer)
  (setq *irc-buffers* (delq buffer *irc-buffers*))
  (delete-string (parse-unique-name (buffer-name buffer))
		 *channel-buffers*))

(defvar *irc-buffers* ())

(defcommand "IRC" (p host port)
  "IRC to a prompted host.  With a prefix also prompt for the port."
  (multiple-value-bind (host port)
		       (if host
			   (values host
				   (or port (value irc-port)))
			   (prompt-telnet p
					  :prompt "IRC server: "
					  :port (value irc-port)))
    (let ((buffer (make-unique-buffer
		   (format () "~A ~A" host port)
		   :modes '("Telnet" "IRC")
		   :delete-hook (list #'cleanup-irc-buffer)))
	  (account (internet:make-inet-account host () ())))
      (pushnew buffer *irc-buffers*)
      (internet:fill-from-netrc account)
      (change-to-buffer buffer)
      ;; Move "Buffer Input Mark" to end of buffer.
      (let ((input-region (get-interactive-input)))
	(move-mark (region-start input-region) (region-end input-region)))
      (defevar "Telnet Expect Response"
	"Channel or personal conversation in buffer."
	:buffer buffer)
      (setv telnet-prompt
	    (format () "~A ~15<~A~>> "
		    *dummy-irc-now*
		    (irc-name account)))
      (setv telnet-connector #'init-irc)
      (setv telnet-commander #'send-irc)
      (setv telnet-account account)
      (setv telnet-port port)
      (setv telnet-input-hook (list #'handle-irc-input))
      (defevar "IRC Channel"
	"Channel or personal conversation in buffer."
	:buffer buffer)
      (defevar "IRC Channels"
	"Channels joined or personal conversations started in buffer."
	:buffer buffer)
      (defevar "Kill Buffer Prompt to Save"
	"Whether `Kill Buffer' prompts to save modified buffers."
	:buffer buffer)
      (funcall (value telnet-connector) buffer))))

(defun exit-irc ()
  (when *irc-buffers*
    (message "Closing all IRC buffers...")
    (dolist (buffer *irc-buffers*)
      (delete-buffer-safely buffer))))

(add-hook exit-hook 'exit-irc)
(add-hook suspend-hook 'exit-irc)


;;;; Gopher

#[ Gopher

{command:gopher}
]#

(defun gopher (out page account port timeout)
  (let* ((in (internet::raw-connect
	      (internet:inet-account-server account)
	      port timeout "Gopher")))
    (write-line page in)
    (loop
      (let ((line (read-line in () :eof)))
	(if (eq line :eof) (return))
	(if line (write-line line out))))))

(defun refresh-gopher-buffer (buffer account port reference
				     &key (add-to-history t))
  (defevar "Gopher Account"
    "Account of connection in buffer."
    :buffer buffer
    :value account)
  (defevar "Gopher Port"
    "Port of connection in buffer."
    :buffer buffer
    :value port)
  (defevar "Gopher Reference"
    "Reference in buffer."
    :buffer buffer
    :value reference)
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (with-output-to-mark (stream (buffer-point buffer))
      (gopher stream reference account port 12))
    (flet ((filter-line (line)
	     (fi (plusp (length line))
		 (values () "")
		 (let ((split (split line #\tab :start 1)))
		   (values
		    (char line 0)
		    (case (char line 0)
		      (#\i
		       ;; Text.
		       (car split))
		      ((#\0 #\1)
		       ;; A link.
		       (format () "~A  ~A  ~A:~A"
			       (car split)
			       (cadr split)
			       (caddr split)
			       (let* ((port (cadddr split))
				      (len (length port)))
				 (subseq (cadddr split)
					 0
					 (if (char= (char port
							  (1- len))
						    #\return)
					     (1- len)
					     len))))
		       (format () "~A ~A"
			       (case (char line 0)
				 (#\0 "-") ; file
				 (#\1 "+")) ; dir
			       (car split))
		       #|
		       (format () "~A  ~A  ~A:~A"
			       (car split)
			       (cadr split)
			       (caddr split)
			       (let* ((port (cadddr split))
				      (len (length port)))
				 (subseq (cadddr split)
					 0
					 (if (char= (char port
							  (1- len))
						    #\return)
					     (1- len)
					     len))))
		       |#
		       ))
		    split)))))
      (do-region-lines (line (buffer-region buffer))
	(let ((original-string (line-string line)))
	  (multiple-value-bind (type string split)
			       (filter-line original-string)
	    (delete-region (region (mark line 0)
				   (mark line (line-length line))))
	    (insert-string (mark line 0) string)
	    (setf (getf (line-plist line) 'gopher-line-type) type)
	    (setf (getf (line-plist line) 'gopher-line-split) split)
	    (setf (getf (line-plist line) 'gopher-line) original-string)))))
    (when add-to-history
      (let* ((current-history (variable-value 'current-gopher-history
					      :buffer buffer))
	     (history-element (cons (list account port reference) ()))
	     (new-history (cons history-element current-history)))
	(if current-history
	    ;; Link the next "slot" of the previous element to the new
	    ;; history, for moving forward through the history.
	    (setf (cdar current-history) new-history))
	(setf (variable-value 'current-gopher-history :buffer buffer)
	      new-history)
	(setf (variable-value 'gopher-history :buffer buffer)
	      new-history)))
    (buffer-start (buffer-point buffer))))

(defun highlight-gopher-line (line chi-info)
  (case (getf (line-plist line) 'gopher-line-type)
    ;; Highlight like Dired, with text like code comments.
    (#\0)
    (#\1 (chi-mark line (if (> (line-length line) 2) 2 0)
		   *special-form-font* :special-form
		   chi-info))
    (#\i (chi-mark line 0
		   *comment-font* :comment
		   chi-info))))

(defun highlight-gopher-buffer (buffer)
  (highlight-chi-buffer buffer highlight-gopher-line))

(defun highlight-visible-gopher-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-gopher-line))

(defun setup-gopher-buffer (buffer)
  (defevar "Gopher History"
    "A history of Gopher pages that have been displayed in this buffer."
    :buffer buffer
    :value ())
  (defevar "Current Gopher History"
    "The history from the current page to the beginning."
    :buffer buffer
    :value ())
  (highlight-visible-gopher-buffer buffer)
  (pushnew '("Gopher" t highlight-visible-gopher-buffer)
	   *mode-highlighters*))

(defmode "Gopher" :major-p t
  :setup-function #'setup-gopher-buffer)

(defvar *gopher-buffer* ())

(defcommand "Gopher" (p (reference "/"))
  "Switch to the Gopher buffer, creating a new one from a prompted host if
   necessary.  With a prefix also prompt for the port."
  (if *gopher-buffer*
      (change-to-buffer *gopher-buffer*)
      (setq *gopher-buffer*
	    (multiple-value-bind (host port)
				 (prompt-telnet p
						:prompt "Gopher: "
						:port (value gopher-port))
	      (let ((buffer (make-unique-buffer
			     "Gopher"
			     :modes '("Gopher" "View")
			     :delete-hook (list (lambda (buffer)
						  (declare (ignore buffer))
						  (setq *gopher-buffer* ())))))
		    (account (internet:make-inet-account host () ())))
		(internet:fill-from-netrc account)
		(change-to-buffer buffer)
		(refresh-gopher-buffer buffer account port reference)
		buffer)))))

(defun gopher-info-line-p (line)
  (let ((plist (getf (line-plist line) 'gopher-line-type)))
    (if plist
	(eq (getf (line-plist line) 'gopher-line-type) #\i)
	t)))

(defcommand "Refresh Gopher Buffer" ()
  "Refresh the current buffer."
  (or (string= (buffer-major-mode (current-buffer)) "Gopher")
      (editor-error "Must be in a Gopher buffer."))
  (refresh-gopher-buffer (current-buffer)
			 (value gopher-account)
			 (value gopher-port)
			 (value gopher-reference)))

(defcommand "Next Gopher Reference" (p)
  "Move to next reference."
  (or (string= (buffer-major-mode (current-buffer)) "Gopher")
      (editor-error "Must be in a Gopher buffer."))
  (let ((p (or p 1)))
    (let ((mark (copy-mark (current-point) :temporary)))
      (loop
	(or (line-offset mark p 0)
	    (editor-error
	     (if (plusp p)
		 (if (gopher-info-line-p (mark-line (current-point)))
		     "Passed last reference."
		     "On last reference.")
		 (if (gopher-info-line-p (mark-line (current-point)))
		     "Passed first reference."
		     "On first reference."))))
	(or (gopher-info-line-p (mark-line mark))
	    (progn
	      (character-offset mark 2)
	      (move-mark (current-point) mark)
	      (return)))))))

(defcommand "Previous Gopher Reference" (p)
  "Move to previous reference."
  (next-gopher-reference-command (if p (- p) -1)))

(defun gopher-view (buffer account port reference)
  (declare (ignore buffer))
  (let ((buffer (make-unique-buffer
		 (format () "View ~A ~A ~A"
			 reference
			 (internet:inet-account-server account)
			 port)
		 :modes '("Fundamental" "View"))))
    (change-to-buffer buffer)
    (with-writable-buffer (buffer)
      (with-output-to-mark (out (buffer-point buffer))
	(gopher out reference account port 12))
      (buffer-start (buffer-point buffer))
      (setf (buffer-modified buffer) ()))))

(defcommand "Gopher Resource from Point" ()
  "Move to next reference."
  (or (string= (buffer-major-mode (current-buffer)) "Gopher")
      (editor-error "Must be in a Gopher buffer."))
  (or (gopher-info-line-p (current-line))
      (let* ((split (getf (line-plist (current-line))
			 'gopher-line-split))
	     (reference (cadr split))
	     (port (or (parse-integer (cadddr split)
				      :errorp ()
				      :junk-allowed t)
		       (value gopher-port)))
	     (buffer (current-buffer))
	     (account (internet:make-inet-account (caddr split) () ())))
	(internet:fill-from-netrc account)
	(if (eq (getf (line-plist (current-line)) 'gopher-line-type) #\0)
	    ;; A file.
	    (gopher-view buffer account port reference)
	    ;; A gopher map.
	    (refresh-gopher-buffer buffer account port reference)))))

(defcommand "Forward Gopher Page" ()
  "Show the next page from the history of pages."
  (or (string= (buffer-major-mode (current-buffer)) "Gopher")
      (editor-error "Must be in a Gopher buffer."))
  (let* ((hist (value current-gopher-history))
	 (next (if hist (cdar hist))))
    (if next
	(progn
	  (setv current-gopher-history next)
	  (refresh-gopher-buffer (current-buffer)
				 (caaar next)
				 (cadaar next)
				 (caddr (caar next))
				 :add-to-history ()))
	(message "End of history."))))

(defcommand "Backward Gopher Page" ()
  "Show the previous page from the history of pages."
  (or (string= (buffer-major-mode (current-buffer)) "Gopher")
      (editor-error "Must be in a Gopher buffer."))
  (let* ((hist (value current-gopher-history))
	 (prev (if hist (cdr hist))))
    (if prev
	(progn
	  (setv current-gopher-history prev)
	  (refresh-gopher-buffer (current-buffer)
				 (caaar prev)
				 (cadaar prev)
				 (caddr (caar prev))
				 :add-to-history ()))
	(message "Beginning of history."))))


;;;; Modeline support.

(defun modeline-telnet-status (buffer window)
  (declare (ignore window))
  (when (editor-bound-p 'telnet-stream :buffer buffer)
    (let ((stream (variable-value 'telnet-stream :buffer buffer)))
      (if (and stream (open-stream-p stream)) "Open" "Closed"))))

(make-modeline-field :name :telnet-status :replace t
		     :function #'modeline-telnet-status)
