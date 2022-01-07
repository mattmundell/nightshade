;;; Interface for internet clients.

(in-package "INTERNET")

(export '(fill-from-netrc
	  inet-account make-inet-account
	  inet-stream inet-stream-response make-inet-stream
	  inet-command inet-quit
	  pop-dele pop-init pop-retr pop-stat
	  telnet-init
	  ftp-command ftp-connect-passive ftp-init ftp-list-dir
	  ftp-ls ftp-put-file ftp-translate-command
	  smtp-init smtp-mail))


;;;; Accounts.

(defstruct (account)
  user
  password)

(defstruct (inet-account (:include account)
			 (:constructor
			  make-inet-account
			  (server &optional user password)))
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


;;;; General.

(defun raw-connect (server port timeout name)
  (or port (error "raw-connect requires a port."))
  (make-inet-stream
   (ext:connect-to-inet-socket server port)
   :input t :output t :buffering :line :name name
   :timeout (or timeout 30)))

(defun write-inet (stream command &optional command-args)
  (apply #'format stream command command-args)
  (write-char #\return stream)
  (write-char #\newline stream)
  (force-output stream))

(defun inet-command (stream command &rest command-args)
  ;(apply #'ed::msg (concat "-> " command) command-args)
  (write-inet stream command command-args)
  (let ((line (setf (inet-stream-response stream) (read-line stream))))
    ;(ed::msg "<- ~A" line)
    (and (plusp (length line)) #| (char= (char line 0) #\+) FIX |# line)))

;;; Public.
;;;
(defun inet-quit (stream)
  (prog1 (inet-command stream "QUIT")
    (close stream)))

(defvar *telnet-port* 23
  "Standard Telnet port.")

(defvar *ftp-port* 21
  "Standard FTP port.")

(defvar *ftp-timeout* 10
  "FTP timeout.")


;;;; Telnet.

(defun telnet-init (account port timeout)
  "Initialize a telnet connection for ACCOUNT.  Return the created stream
   on success, else ()."
  (let* ((stream (raw-connect (inet-account-server account)
			      port timeout "TELNET"))
	 (line (setf (inet-stream-response stream)
		     (read-line stream))))
    (fi (eq port *telnet-port*)
	stream
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
	      (values () (inet-stream-response stream)))))))


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
    (loop
      for line = (read-line pop-stream)
      while (and line (plusp (length line)))
      do
      (and (eq (length line) 2)
	   (string= line ".")
	   (return))
      (write-line line file-stream :end (1- (length line))))))

;;; Public.
;;;
(defun pop-dele (pop-stream id)
  "Retrieve ID from POP STREAM into FILE-STREAM."
  (pop-command pop-stream :dele "DELE ~D" id))


;;;; File Transfer Protocol (FTP).

(defun ftp-translate-command (command)
  (let ((parts (split command '(#\space))))
    (concat (case= (string-upcase (car parts))
	      ("?"      "HELP")
	      ("ASCII"  "TYPE A")
	      ("BIN"    "TYPE I")
	      ("BYE"    "QUIT")
	      ("CD"     "CWD")
	      ("MKDIR"  "MKD")
	      ("RM"     "DELE")
	      ("RMDIR"  "RMD")
	      (t (car parts)))
	    (subseq command (length (car parts))))))

(defun read-lines (stream line code)
  (loop for next = (read-line stream) do
    (setq line (format () "~a~%~a"
		       (subseq line 0 (1- (length line)))
		       next))
    (and (> (length next) 2)
	 (string= next code :end1 3)
	 (return)))
  line)

(defun read-any-lines (stream line)
  (loop for next = (read-line stream ()) while next do
    (setq line (format () "~a~%~a"
		       (subseq line 0 (1- (length line)))
		       next)))
  line)

(defun ftp-connect-passive (control)
  (when (ftp-command control "PASV")
    (multiple-value-bind (host port)
			 (parse-pasv control)
      (raw-connect host port *ftp-timeout* "FTP"))))

(defun ftp-command (stream command &rest command-args)
  (let ((command (ftp-translate-command command)))
    (write-inet stream command command-args)
    (let* ((line (setf (inet-stream-response stream)
		       (read-line stream)))
	   (code (and (plusp (length line))
		      (parse-integer line :junk-allowed t))))
      (case code
	((()))
	(211 ; Status.
	 (setf (inet-stream-response stream)
	       (read-lines stream line "211")))
	(214 ; Help.
	 (setf (inet-stream-response stream)
	       (read-lines stream line "214")))
	(221 ; Quit.
;	 (close stream)
	 line)
	(221 ; Passive mode.
	 (read-any-lines stream line))
	(412 ; Timeout.
;	 (close stream)
	 ())
	(t
	 (fi (>= code 500) line))))))

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

(defun parse-pasv (stream)
  (let ((response (inet-stream-response stream)))
    (when (> (length response) 27)
      (let ((split (split response '(#\,) :start 27)))
	(values (format () "~A.~A.~A.~A"
			(pop split) (pop split)
			(pop split) (pop split))
		(+ (ash (parse-integer (pop split)) 8)
		   (parse-integer (pop split) :junk-allowed t)))))))

(defun ftp-ls (control data out)
  (when (ftp-command control "LIST")
    ;; FIX check response
    (transfer data out)
    (setf (inet-stream-response control) (read-line control))))

(defun ftp-list-dir (account directory out)
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (unwind-protect
	(when (ftp-command control "CWD ~A" directory)
	  (when (ftp-command control "PASV")
	    (multiple-value-bind (host port)
				 (parse-pasv control)
	      (let ((in (raw-connect host port *ftp-timeout* "FTP")))
		(unwind-protect
		    (ftp-ls control in out)
		  (close in))))))
      (ftp-command control "QUIT")
      (close control))))

(defun ftp-put-file (account src dest)
  "Put local file SRC into DEST at ACCOUNT."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (unwind-protect
	(when (ftp-command control "CWD ~A" (directory-namestring dest))
	  (when (ftp-command control "TYPE I")
	    (when (ftp-command control "PASV")
	      (multiple-value-bind (host port)
				   (parse-pasv control)
		(with-open-file (in src)
		  (let ((out (raw-connect host port *ftp-timeout* "FTP")))
		    (unwind-protect
			(when (ftp-command control "STOR ~A" (file-namestring dest))
			  (transfer in out))
		      (close out))))))))
      (ftp-command control "QUIT")
      (close control))))

(defun ftp-get-file (account src dest)
  "Get SRC from ACCOUNT into local file DEST."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (unwind-protect
	(when (ftp-command control "CWD ~A" (directory-namestring src))
	  (when (ftp-command control "TYPE I")
	    (when (ftp-command control "PASV")
	      (multiple-value-bind (host port)
				   (parse-pasv control)
		(let ((in (raw-connect host port *ftp-timeout* "FTP")))
		  (unwind-protect
		      (when (ftp-command control "RETR ~A" (file-namestring src))
			(with-open-file (out dest
					     :direction :output
					     :if-does-not-exist :create)
			  (transfer in out)))
		    (close in)))))))
      (ftp-command control "QUIT")
      (close control))))

#|
(setq acc (internet:make-inet-account "www.mundell.ukfsn.org"))
(fill-from-netrc acc)
(setq control (ftp-init acc *ftp-port* *ftp-timeout*))

(setq r (ftp-command control "CWD ~A" "/"))

(ed::msg "~A" (inet-stream-response control))

(ed::with-pop-up-display (out)
  (ftp-list-dir acc "/" out))
(ed::with-pop-up-display (out)
  (ftp-list-dir acc "website" out))

(ftp-put-file acc ":tmp/2" "/two2")

(ftp-get-file acc "/xtext.cc" ":tmp/tttt.cc")
(ftp-get-file acc "/text.cc" ":tmp/tttt.cc")
|#


;;;; Simple Mail Transfer Protocol (SMTP).

(defvar *smtp-port* 25
  "Standard SMTP port.")

(defvar *smtp-timeout* 10
  "FIX Timeout for mail function.")

(defun smtp-init (account port timeout)
  "Initialize an SMTP connection for ACCOUNT.  Return the created stream on
   success, else () and the error string."
  (let* ((stream (raw-connect (inet-account-server account)
			      port timeout "SMTP"))
	 (line (setf (inet-stream-response stream)
		     (read-line stream))))
    (or (and (> (length line) 2)
	     (string= line "220" :end1 3)
	     (etypecase account
	       (inet-account
		(and (inet-command stream "EHLO ~A" (machine-instance))
		     (let ((line (inet-stream-response stream)))
		       ;; FIX else HELO fallback
		       (prog1 (and line (> (length line) 2)
				   (string= line "250" :end1 3))
			 (loop while (listen stream) do
			   (let ((line (read-line stream ())))
			     ;; FIX check max size
			     ;; FIX check if auth login avail
			     (or (and (> (length line) 2)
				      (string= line "250" :end1 3))
				 (return))))))
		     ;; FIX only auth if user,password in account
		     ;; FIX other are probably also like this
		     (inet-command stream "AUTH LOGIN")
		     (let ((line (inet-stream-response stream)))
		       (and (> (length line) 2)
			    ;; FIX Assume rest of line "Username:"
			    (string= line "334" :end1 3)))
		     ;; Authenticate.
		     (inet-command stream (base64:base64-encode
					   (account-user account)))
		     (let ((line (inet-stream-response stream)))
		       (and (> (length line) 2)
			    ;; FIX Assume rest of line "Password:"
			    (string= line "334" :end1 3)))
		     (inet-command stream (base64:base64-encode
					   (account-password account)))
		     (let ((line (inet-stream-response stream)))
		       (and (> (length line) 2)
			    (string= line "235" :end1 3))))))
	     stream)
	(progn
	  (write-inet stream "QUIT")
	  (close stream)
	  (values () (inet-stream-response stream))))))

(defmacro smtp-mail ((stream to from account) &body body)
  "Send a message to the addresses listed in TO from address FROM, reading
   account details from ACCOUNT.  Bind STREAM to the SMTP connection and
   invoke BODY to write the message contents (i.e. to follow the DATA
   command), then write the trailing \".\".  Return t on success, else ()
   and the error response."
  (let ((error (gensym)) (line (gensym)))
    `(multiple-value-bind
	 (,stream ,error)
	 (smtp-init ,account *smtp-port* *smtp-timeout*)
       (fi ,stream
	   (values () ,error)
	   (or (unwind-protect
		   (and (inet-command ,stream
				      (format () "MAIL FROM:<~A>" ,from))
			(let ((,line (inet-stream-response ,stream)))
			  (and (> (length ,line) 2)
			       (string= ,line "250" :end1 3)))
			(etypecase ,to
			  (string
			   (inet-command ,stream
					 (format () "RCPT TO:<~A>" ,to)))
			  (list
			   (prog1 (inet-command ,stream
						(format () "RCPT TO:<~A>"
							(car ,to)))
			     (loop for addr in (cdr ,to) do
			       (inet-command ,stream
					     (format () "RCPT TO:<~A>"
						     addr))))))
			(let ((,line (inet-stream-response ,stream)))
			  (and (> (length ,line) 2)
			       (string= ,line "250" :end1 3)))
			(inet-command ,stream "DATA")
			(let ((,line (inet-stream-response ,stream)))
			  (and (> (length ,line) 2)
			       (string= ,line "354" :end1 3)))
			(progn
			  ,@body
			  (write-line "." ,stream)
			  t))
		 (inet-quit ,stream))
	       (values () (inet-stream-response ,stream)))))))
