;;; Interface for internet clients.

(in-package "INTERNET")

(export '(fill-from-netrc
	  inet-account make-inet-account
	  inet-stream inet-stream-response make-inet-stream
	  inet-command inet-quit
	  pop-init pop-stat pop-retr pop-dele
	  telnet-init telnet-quit
	  ftp-init ftp-command ftp-translate-command))


;;;; Accounts.

(defstruct (account)
  user
  password)

(defstruct (inet-account (:include account)
			 (:constructor
			  make-inet-account
			  (server user password)))
  server)

(defun fill-from-netrc (account)
  "Complete any () fields in internet ACCOUNT from home:.netrc." ;; FIX describe .netrc format
  (with-open-file (stream "home:.netrc" :direction :input)
    (loop for line = (read-line stream ()) while line do
      (and (> (length line) 8)
	   (string= line "machine " :end1 8 :end2 8)
	   (string= line (inet-account-server account) :start1 8)
	   (let ((user) (password))
	     ;; Prefer the highest entry in the file, even if there is more
	     ;; detail in a lower entry.
	     (loop for line = (read-line stream ())
	           for line-length = (length line) do
	       (if (or (string= line "")
		       (every (lambda (char) (or (char= char #\space)
						 (char= char #\tab)))
			      line))
		   (return))
	       (cond
		((and (> line-length 6)
		      (string= line "login " :end1 6))
		 (setq user (subseq line 6)))
		((and (> line-length 8)
		      (string= line "password " :end1 9))
		 (setq password
		       (if (and (> line-length 9)
				(char= (char line (1- line-length)) #\")
				(char= (char line 9)) #\")
			   (subseq line 10 (1- line-length))
			   (subseq line 9))))))
	     (let ((account-user (inet-account-user account)))
	       (when (if user
			 (if account-user
			     (string= user account-user)
			     (setf (inet-account-user account) user))
			 (or account-user
			     (setf (inet-account-user account) "anonymous")))
		 (or (inet-account-password account)
		     (setf (inet-account-password account)
			   (or password
			       ;; FIX
			       (format () "~A@~A"
				       (cdr (assoc :user ext:*environment-list*))
				       (machine-instance)))))
		 (return))))))
    account))


;;;; Stream.

(defstruct (inet-stream
	    (:include system::fd-stream)
	    (:constructor
	     %make-inet-stream
	     (&key fd
		   name
		   file
		   original
		   delete-original
		   pathname
		   buffering
		   timeout)))
  (response nil :type (or simple-string null)))

;;; MAKE-INET-STREAM -- Public.
;;;
;;; Returns an INET-STREAM on the given file.
;;;
(defun make-inet-stream (fd
			 &key
			 (input nil input-p)
			 (output nil output-p)
			 (element-type 'base-char)
			 (buffering :full)
			 timeout
			 file
			 original
			 delete-original
			 pathname
			 input-buffer-p
			 (name (if file
				   (format nil "file ~S" file)
				   (format nil "descriptor ~D" fd)))
			 auto-close)
  (declare (type index fd) (type (or index null) timeout)
	   (type (member :none :line :full) buffering))
  "Create a stream for the given unix file descriptor.
   If input is non-nil, allow input operations.
   If output is non-nil, allow output operations.
   If neither input nor output are specified, default to allowing input.
   Element-type indicates the element type to use (as for open).
   Buffering indicates the kind of buffering to use.
   Timeout (if true) is the number of seconds to wait for input.  If NIL (the
   default), then wait forever.  When we time out, we signal IO-TIMEOUT.
   File is the name of the file (will be returned by PATHNAME).
   Name is used to identify the stream when printed."
  (cond ((not (or input-p output-p))
	 (setf input t))
	((not (or input output))
	 (error "File descriptor must be opened either for input or output.")))
  (let ((stream (%make-inet-stream :fd fd
				   :name name
				   :file file
				   :original original
				   :delete-original delete-original
				   :pathname pathname
				   :buffering buffering
				   :timeout timeout)))
    (lisp::set-routines stream element-type input output input-buffer-p)
    (when (and auto-close (fboundp 'finalize))
      (finalize stream
		#'(lambda ()
		    (unix:unix-close fd)
		    (format *terminal-io* "** Closed file descriptor ~D~%"
			    fd))))
    stream))


;;;; General functions.

(defun raw-connect (server port timeout name)
  (or port (error "raw-connect requires a port."))
  (make-inet-stream
   (ext:connect-to-inet-socket server port)
   :input t :output t :buffering :line :name name
   :timeout (or timeout 30)))

(defun write-inet (stream command &optional command-args)
;  (ed::msg ".~A." (apply #'format () command command-args))
  (apply #'format stream command command-args)
  (write-char #\return stream)
  (write-char #\newline stream)
  (force-output stream))

(defun inet-command (stream command &rest command-args)
  (write-inet stream command command-args)
  (let ((line (setf (inet-stream-response stream) (read-line stream))))
    (and (plusp (length line)) (char= (char line 0) #\+) line)))

;;; Public.
;;;
(defun inet-quit (stream)
  (prog1 (inet-command stream "QUIT")
    (close stream)))


;;;; Telnet.

(defun telnet-init (account port timeout)
  "Initialize a telnet connection for ACCOUNT.  Return the created stream
   on success, else ()."
  (let* ((stream (raw-connect (inet-account-server account)
			      port timeout "TELNET"))
	 (line (setf (inet-stream-response stream)
		     (read-line stream))))
    (or (and (plusp (length line))
	     (char= (char line 0) #\+)
	     (etypecase account
	       (inet-account
		(and (inet-command stream
				   "USER ~A"
				   (inet-account-user account))
		     (inet-command stream
				   "PASS ~A"
				   (inet-account-password account)))))
	     stream)
	(progn
	  (write-inet stream "QUIT")
	  (close stream)
	  (values () (inet-stream-response stream))))))


;;;; Post Office Protocol (POP).

(defvar *pop-last-command-type* ()
  "Used to recover from a POP timeout.")

(defvar *pop-last-command-issued* ()
  "Used to recover from a POP timeout.")

(defun write-pop (stream type command &optional command-args)
  (setf *pop-last-command-type* type)
  (setf *pop-last-command-issued* command)
  (apply #'format stream command command-args)
  (write-char #\return stream)
  (write-char #\newline stream)
  (force-output stream))

(defun pop-command (stream type command &rest command-args)
  (write-pop stream type command command-args)
  (let ((line (setf (inet-stream-response stream) (read-line stream))))
    (and (plusp (length line)) (char= (char line 0) #\+) line)))

(defun auth (user password)
  (declare (ignore user password))
  (error "FIX auth"))

;;; Public.
;;;
(defun pop-init (account type port timeout)
  "Initialize a POP connection for ACCOUNT, return the stream or ()."
  (let* ((stream (raw-connect (inet-account-server account)
			      port timeout "POP"))
	 (line (read-line stream)))
    (or (and (plusp (length line))
	     (char= (char line 0) #\+)
	     (ecase type
	       (:apop
		(let ((str (auth (inet-account-user account)
				 (inet-account-password account))))
		  (if str
		      (pop-command stream :auth
				   "APOP ~A" str))))
	       (:rpop
		(and (pop-command stream :user
				  "USER ~A" (inet-account-user account))
		     (pop-command stream :pass
				  "RPOP ~A" (inet-account-password account))))
	       (:kpop
		(and (pop-command stream :user
				  "USER ~A" (inet-account-user account))
		     (pop-command stream :pass
				  "PASS ~A" (inet-account-password account)))))
	     stream)
	(progn
	  (write-pop stream :user "QUIT")
	  (close stream)
	  ()))))

;;; Public.
;;;
(defun pop-stat (stream)
  "Return the number and size in bytes of messages in ACCOUNT via
   connection STREAM."
  (let ((response (pop-command stream :user "STAT")))
;    (ed::msg "s ~A" (inet-stream-response stream))
    (and response
	 (char= (char response 1) #\O)
	 (char= (char response 2) #\K)
	 (multiple-value-bind (val next)
			      (parse-integer response
					     :start 3
					     :junk-allowed t
					     :errorp ())
	   (if val
	       (values val
		       (parse-integer response
				      :start next
				      :junk-allowed t
				      :errorp ())))))))

;;; Public.
;;;
(defun pop-retr (pop-stream id file-stream)
  "Retrieve ID from POP STREAM into FILE-STREAM."
  (when (pop-command pop-stream :retr "RETR ~D" id)
;    (ed::msg "r ~A" (inet-stream-response pop-stream))
    (loop
      for line = (read-line pop-stream)
      while (and line (plusp (length line)))
      do
      (and (eq (length line) 2)
	   (string= line ".")
	   (return))
;      (ed::msg "l ~A" line)
      (write-line line file-stream :end (1- (length line))))))

;;; Public.
;;;
(defun pop-dele (pop-stream id)
  "Retrieve ID from POP STREAM into FILE-STREAM."
  (pop-command pop-stream :dele "DELE ~D" id))


;;;; File Transfer Protocol (FTP).

;; FIX table
(defun ftp-translate-command (command)
  (cond ((string= (string-upcase command) "?")
	 "HELP")
	((string= (string-upcase command) "ASCII")
	 "TYPE A")
	((string= (string-upcase command) "BIN")
	 "TYPE I")
	((string= (string-upcase command) "BYE")
	 "QUIT")
	((string= (string-upcase command) "CD")
	 "CWD")
	((string= (string-upcase command) "MKDIR")
	 "MKD")
	((string= (string-upcase command) "RMDIR")
	 "RMD")
	(t command)))

(defun read-lines (stream line code)
  (loop for next = (read-line stream) do
    (setq line (format () "~a~%~a"
		       (subseq line 0 (1- (length line)))
		       next))
    (and (> (length next) 2)
	 (string= next code :end1 3)
	 (return)))
  line)

(defun ftp-command (stream command &rest command-args)
  (let ((command (ftp-translate-command command)))
    (write-inet stream command command-args)
    (let* ((line (setf (inet-stream-response stream)
		       (read-line stream)))
	   (code (and (plusp (length line))
		      (parse-integer line :junk-allowed t))))
;      (ed::msg "code ~A" code)
      (case code
	((()))
	(211 ; Status.
	 (setf (inet-stream-response stream)
	       (read-lines stream line "211")))
	(214 ; Help.
	 (setf (inet-stream-response stream)
	       (read-lines stream line "214")))
	(221 ; Quit.
	 (close stream)
	 line)
	(221 ; Passive mode.
	 line)
	(412 ; Timeout.
	 (close stream)
	 ())
	(t
	 line)))))

(defun ftp-init (account port timeout)
  "Initialize an FTP connection for ACCOUNT.  Return the created stream on
   success, else () and the response."
  (let* ((stream (raw-connect (inet-account-server account)
			      port timeout "FTP"))
	 (line (setf (inet-stream-response stream)
		     (read-line stream))))
    (or (and (plusp (length line))
	     (char= (char line 0) #\2)
	     (etypecase account
	       (inet-account
		(and (ftp-command stream
				  "USER ~A"
				  (inet-account-user account))
		     (ftp-command stream
				  "PASS ~A"
				  (inet-account-password account)))))
	     stream)
	(progn
	  (write-inet stream "QUIT")
	  (close stream)
	  (values () (inet-stream-response stream))))))

#|

:down/rfc959.txt

(setq stream (system:make-fd-stream
	      (ext:connect-to-inet-socket "www.mundell.ukfsn.org"
					  21)
	      :input t :output t :buffering :line :name "FTP"
	      :timeout 10))
(progn stream)
(read-line stream)
(write-line "user mundell" stream)
(write-line "pass bkt533ec" stream)
(write-line "pwd" stream)
;(write-line "pasv" stream)     217,158,120,144,189,245
; else makes connection on port U
(write-line "type i" stream)
(write-line "syst" stream)  ; os type
(write-line "list" stream)
(write-line "noop" stream)
(write-line "quit" stream)

|#
