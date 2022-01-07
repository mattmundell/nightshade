;;; Interface for internet clients.

(in-package "INTERNET")

(export '(fill-from-netrc
	  account-user account-password
	  make-inet-account inet-account inet-account-protocol
	  inet-account-server
	  inet-stream inet-stream-response make-inet-stream
	  inet-command inet-para-command inet-quit
	  pop-dele pop-init pop-retr pop-stat
	  telnet-init
	  ftp-command ftp-connect-passive ftp-add-dir ftp-release-dir
	  ftp-get-file ftp-init ftp-list-dir
	  ftp-ls ftp-probe-file ftp-put-file ftp-translate-command
	  ftp-file-stats
	  mail
	  ssh-command ssh-add-dir ssh-release-dir
	  ssh-get-file ssh-probe-file ssh-put-file
	  smtp-init smtp-mail ukfsn-mail
	  add-remote-directory release-remote-directory
	  do-remote-directory remote-file-stats get-remote-file put-remote-file
	  delete-remote-file probe-remote-file read-remote-file
	  write-remote-file set-remote-write-date
	  ;;
	  *services* probe-service start-service stop-service))

;; FIX ftp tar.bz2 transfer leaves ~ trailing garbage after final newline
;;        due to `transfer', recheck

(defvar *mess* ())
(defvar *mess-level* 0)

(defun incf-mess () (incf *mess-level* 2))
(defun decf-mess () (decf *mess-level* 2))

(defun mess (format &rest args)
  (when *mess*
    (funcall *mess* "~A~A"
	     (make-string *mess-level* :initial-element #\space)
	     (apply #'funcall #'format () format args))))


;;;; Accounts.

(defstruct (account)
  user ; FIX login?
  password)

(defstruct (inet-account (:include account)
			 (:constructor
			  make-inet-account
			  (server &optional user password protocol)))
  server
  protocol)

(defun parse-pair-file (pathname)
  "Parse a .netrc style file of entries.

   Each entry is a block of continuous lines specifying values of fields.
   The word that starts the line is the field name, the rest of the line is
   the field value.  Return a list of entries where each entry is a list
   with the field name first and the value of the field next."
  (when (probe-file pathname)
    (collect ((entries))
      (from-file (stream pathname)
	(labels ((blank-char-p (char)
		   (if (member char '(#\space #\tab)) t))
		 (blank-line-p (line)
		   (every #'blank-char-p line))
		 ;; Read and return the field in line.
		 (read-field (line)
		   (until ((index 0 (1+ index))
			   (end (1- (length line))))
			  ((or (= index end)
			       (blank-char-p (char line index)))
			   (if (= index end) (incf index))
			   (list (subseq line 0 index)
				 ;; Read over any whitespace between.
				 (while ((index index (1+ index)))
					((and (<= index end)
					      (blank-char-p (char line index)))
					 (subseq line index)))))))
		 ;; Read and return the entry starting at line.
		 (read-entry (line)
		   (collect ((entry))
		     (while ((line line (read-line stream ())))
			    (line (entry))
		       (if (or (zerop (length line))
			       (blank-char-p (aref line 0)))
			   (return (entry))
			   (entry (read-field line)))))))
	  ;; Read over lines between entries, calling `read-entry' to read
	  ;; the entries.
	  (while ((line (read-line stream ()) (read-line stream ())))
		 (line)
	    (or (zerop (length line))
		(blank-char-p (aref line 0))
		(let ((entry (read-entry line)))
		  (if entry
		      (entries entry)
		      (return)))))))
      (entries))))

(defun fill-from-netrc (account)
  ;; FIX describe .netrc format
  ;; FIX This is some kind of constraint solver.
  "Complete any empty fields in internet $account from home:.netrc.

   Return $account and a second value to indicate the match: #t if $account
   matched an entry in the .netrc, else ().

   Require $account to contain a value in the machine slot."
  (or (inet-account-server account)
      (error "Machine required in $account: ~A" account))
  (labels ((strip-quotes (string)
	     (let ((string-length (length string)))
	       (if (and (> string-length 1)
			(char= (char string 0) #\")
			(char= (char string (1- string-length)) #\"))
		 (subseq string 1 (1- string-length))
		 string)))
	   (fill-protocol (netrc-list)
	     (setf (inet-account-protocol account)
		   (strip-quotes (cadr (assoc "protocol" netrc-list
					      :test #'string=)))))
	   (fill-password (netrc-list)
	     (setf (account-password account)
		   (strip-quotes (cadr (assoc "password" netrc-list
					      :test #'string=)))))
	   (fill-login (netrc-list)
	     (setf (account-user account)
		   (cadr (assoc "login" netrc-list
				:test #'string=)))))
    (values
     account
     ;;; Find the first entry with the most information.
     (let* ((netrc (parse-pair-file "home:.netrc"))
	    ;; Look for an entry with the given machine.
	    (machine (keep-if (lambda (ele)
				(equal (cadr (assoc "machine" ele
						    :test #'string=))
				       (inet-account-server account)))
			      netrc)))
       (fi machine
	   ;; Account holds more info (machine), fill with fallback if
	   ;; available.
	   (let ((netrc-list (find "default" netrc
				   :key #'caar :test #'string=)))
	     (when netrc-list
	       (setf (account-user account)
		     (cadr (assoc "login" netrc-list
				  :test #'string=)))
	       (setf (account-password account)
		     (cadr (assoc "password" netrc-list
				  :test #'string=)))
	       (setf (inet-account-protocol account)
		     (cadr (assoc "protocol" netrc-list
				  :test #'string=)))
	       t))
	   (fi (account-user account)
	       (if (inet-account-protocol account)
		   ;; Look for an entry with the given protocol.
		   (let ((protocol
			  (keep-if
			   (lambda (ele)
			     (equal (cadr (assoc "protocol" ele
						 :test #'string=))
				    (inet-account-protocol
				     account)))
			   machine)))
		     (if protocol
			 ;; Fill from the first protocol matching entry.
			 (progn
			   (fill-password (car protocol))
			   t)
			 ;; Account holds more info (protocol).
			 ()))
		   ;; Fill from the first machine matching entry.
		   (progn
		     (fill-protocol (car machine))
		     (fill-password (car machine))
		     (fill-login (car machine))
		     t))
	       ;; Look for an entry with the given login.
	       (let ((login (keep-if
			     (lambda (ele)
			       (equal (cadr (assoc "login" ele
						   :test #'string=))
				      (account-user account)))
			     machine)))
		 (fi login
		     ;; Account holds more info (login).
		     ()
		     (fi (inet-account-protocol account)
			 ;; Fill from the first entry with the given login.
			 (progn
			   (fill-protocol (car login))
			   (fill-password (car login))
			   t)
			 ;; Look for an entry with the given protocol.
			 (let ((protocol
				(keep-if
				 (lambda (ele)
				   (equal (cadr (assoc "protocol" ele
						       :test #'string=))
					  (inet-account-protocol
					   account)))
				 machine)))
			   (if protocol
			       ;; Fill from the first password matching
			       ;; entry.
			       (progn
				 (fill-password (car protocol))
				 t)
			       ;; Account holds more info (protocol).
			       ())))))))))))


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
				   (format () "file ~S" file)
				   (format () "descriptor ~D" fd)))
			 auto-close)
  (declare (type lisp::index fd) (type (or lisp::index null) timeout)
	   (type (member :none :line :full) buffering))
  "Create and return a stream for the given Unix file descriptor.
   If input is true, allow input operations.
   If output is true, allow output operations.
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
		    (format *terminal-io* "** Closed inet file descriptor ~D~%"
			    fd))))
    stream))


;;;; Controller processes and streams.

(defstruct ftp-controller
  stream
  dir)

;; FIX close all on exit
(defvar *controllers* ()
  "Alist of control streams.")

(defun %get-controller (server protocol user)
  "Get the controller associated with $server, $protocol and $user in
   *controllers*."
  (assoc user (cdr (assoc protocol
			  (cdr (assoc server *controllers*
				      :test #'string=))
			  :test #'string=))
	 :test #'string=))

(defun store-controller (controller server protocol user)
  "Ensure $controller is associated with $server, $protocol and $user in
   *controllers*."
  (let ((entry (%get-controller server protocol user)))
    (if entry
	(progn
	  (ignore-errors (close (cdr entry)))
	  (setf (cdr entry) controller))
	(pushnew (cons server
		       `((,protocol . ((,user . ,controller)))))
		 *controllers*
		 :test #'equal))))

(defun get-controller (server protocol user)
  "Get the open stream associated with $server, $protocol and $user in
   *controllers*."
  (let* ((entry (%get-controller server protocol user))
	 (stream (cdr entry)))
    (if stream (fi (timed-out-p protocol stream) stream))))

(defun timed-out-p (protocol controller)
  "Return t if $controller (which is for $protocol) has timed out."
  (case= protocol
    ("ssh"
     (fi (process-alive-p controller)))
    ("ftp"
     (let ((stream (ftp-controller-stream controller)))
       (if (open-stream-p stream) (ftp-timed-out-p stream))))
    (t
     (let ((stream (ftp-controller-stream controller)))
       (if (open-stream-p stream) (ftp-timed-out-p stream))))))


;;;; General.

(defvar *telnet-port* 23
  "Standard Telnet port.")

(defvar *ftp-port* 21
  "Standard FTP port.")

(defvar *ftp-timeout* 20
  "FTP timeout in seconds.")

(defvar *http-port* 80
  "Standard HTTP port.")

(defvar *http-timeout* 15
  "HTTP timeout in seconds.")

(defun raw-connect (server port timeout name)
  (or port (error "raw-connect requires a port."))
  (mess "raw connect to ~A:~A" server port)
  (make-inet-stream
   (ext:connect-to-inet-socket server port)
   :input t :output t :buffering :none :name name
   :timeout (or timeout 30)))

(defun write-inet (stream command &optional command-args)
  (apply #'mess (concat "wi> " command) command-args)
  (apply #'format stream command command-args)
  (write-char #\return stream)
  (write-char #\newline stream)
  (force-output stream))

(defun inet-command (stream command &rest command-args)
  (apply #'mess (concat "-> " command) command-args)
  (write-inet stream command command-args)
  (let ((line (setf (inet-stream-response stream) (read-line stream))))
    (mess "<- ~A" line)
    (and (plusp (length line)) #| (char= (char line 0) #\+) FIX |# line)))

(defun inet-para-command (stream command &rest command-args)
  (apply #'mess (concat "-> " command) command-args)
  (write-inet stream command command-args))

;;; Public.
;;;
(defun inet-quit (stream)
  (prog1 (inet-command stream "QUIT")
    (close stream)))

(defun parse-ls-line (line)
  "Parse the directory listing $line, returning success-p, permissions,
   links, uid, gid, size and mod time."
  ;;-rw----r--    1 1714     750           894 Jan 29  2007 text.lisp
  ;;-rw-r--r--    1 root     root          117 Jan  1 00:27 dhcp.leases
  (when (> (length line) 21)
    (let ((line line))
      (with-input-from-string (in line)
	(let* ((permissions (or (read in ()) (return-from parse-ls-line ())))
	       (links (or (read in ()) (return-from parse-ls-line ())))
	       (uid (or (read in ()) (return-from parse-ls-line ())))
	       (gid (or (read in ()) (return-from parse-ls-line ())))
	       (size (or (read in ()) (return-from parse-ls-line ())))
	       (pos (file-position in))
	       (mod-time (- (or (parse-time line :start pos :junk-allowed t)
				(return-from parse-ls-line ()))
			    lisp::unix-to-universal-time)))
	  (values t (symbol-name permissions) links uid gid size mod-time))))))


;;;; General interface (abstracts over protocols).

;;; FIX use per-protocol function pointers so that this is extensible

;;; Public
;;;
(defun add-remote-directory (pathname)
  "Add remote directory $pathname."
  (let ((account (make-inet-account (remote-pathname-host pathname)))
	(file (remote-pathname-local pathname)))
    (fill-from-netrc account)
    (flet ((ftp-add (account file)
	     (ftp-add-dir account file)))
      (case= (inet-account-protocol account)
	("ssh"
	 (ssh-add-dir account file))
	("ftp"
	 (ftp-add account file))
	(t
	 (ftp-add account file))))))

;;; Public
;;;
(defun release-remote-directory (pathname)
  "Release remote directory $pathname."
  (let ((account (make-inet-account (remote-pathname-host pathname)))
	(file (remote-pathname-local pathname)))
    (fill-from-netrc account)
    (case= (inet-account-protocol account)
      ("ssh"
       (ssh-release-dir account file))
      ("ftp"
       (ftp-release-dir account file))
      (t
       (ftp-release-dir account file)))))

;;; Public
;;;
(defmacro do-remote-directory ((directory file-var) &body body)
  "Do $body with $file-var bound to every file in remote $directory."
  (let ((account (gensym)))
    `(let ((,account (internet:make-inet-account
		      (or (remote-pathname-host ,directory)
			  (error "~A is local." ,directory)))))
       (internet:fill-from-netrc ,account)
       (case= (internet:inet-account-protocol ,account)
	 ("ssh"
	  (do-ssh-directory (,account
			     (remote-pathname-local ,directory)
			     ,file-var)
	    ,@body))
	 ("ftp"
	  (do-ftp-directory (,account
			     (remote-pathname-local ,directory)
			     ,file-var)
	    ,@body))
	 (t
	  (do-ftp-directory (,account
			     (remote-pathname-local ,directory)
			     ,file-var)
	    ,@body))))))
#| FIX deftest
(do-remote-directory ("localhost:" file) t)
|#

(defun remote-file-stats (file)
  "Return information about $file: reslt dev-or-err ino mode nlink uid gid
   rdev size atime mtime $file."
  (let ((account (make-inet-account (remote-pathname-host file)))
	(file (remote-pathname-local file)))
    (fill-from-netrc account)
    (flet ((ftp-stats (account file)
	     (multiple-value-bind (success-p err perms links uid gid size mod-time)
				  (ftp-file-stats account file)
	       (if success-p
		   (values t err () perms links uid gid () size () mod-time file)))))
      (case= (inet-account-protocol account)
	("ssh"
	 (multiple-value-bind (success-p err perms links uid gid size mod-time)
			      (ssh-file-stats account file)
	   (if success-p
	       (values t err () perms links uid gid () size () mod-time file))))
	("ftp"
	 (ftp-stats account file))
	(t
	 (ftp-stats account file))))))

(defun set-remote-write-date (file date)
  "Set the write date of remote $file to universal $date, if the protocol
   supports setting the date."
  (let ((account (make-inet-account (remote-pathname-host file)))
	(file (remote-pathname-local file)))
    (fill-from-netrc account)
    (case= (inet-account-protocol account)
      ("ssh"
       (ssh-set-write-date account file date))
      (t
       ()))))

(defun get-remote-file (src dest)
  "Copy remote file $src to $dest.  Throw an error if $dest exists
   already."
  (if (probe-file dest) (error "~A exists already." dest))
  (let ((account (make-inet-account (remote-pathname-host src))))
    (fill-from-netrc account)
    (flet ((ftp-get (account src dest)
	     (ftp-get-file account
			   (remote-pathname-local src)
			   (pathname dest))))
      (case= (inet-account-protocol account)
	("ssh"
	 (ssh-get-file account
		       (remote-pathname-local src)
		       (pathname dest)))
	("ftp"
	 (ftp-get account src dest))
	(t
	 (ftp-get account src dest))))))

(defun put-remote-file (src dest)
  "Copy $src to remote file $dest.  Throw an error if $dest exists
   already."
  (if (probe-file dest) (error "~A exists already." dest))
  (let ((account (make-inet-account (remote-pathname-host dest))))
    (fill-from-netrc account)
    (flet ((ftp-put (account src dest)
	     (ftp-put-file account
			   (pathname src)
			   (remote-pathname-local dest))))
      (case= (inet-account-protocol account)
	("ssh"
	 (ssh-put-file account
		       (pathname src)
		       (remote-pathname-local dest)))
	("ftp"
	 (ftp-put account src dest))
	(t
	 (ftp-put account src dest))))))

(defun delete-remote-file (file)
  "Delete remote $file."
  (let ((account (make-inet-account (remote-pathname-host file))))
    (fill-from-netrc account)
    (flet ((ftp-del (account file)
	     (ftp-delete-file account (remote-pathname-local file))))
      (case= (inet-account-protocol account)
	("ssh"
	 (ssh-delete-file account (remote-pathname-local file)))
	("ftp"
	 (ftp-del account file))
	(t
	 (ftp-del account file))))))

(defun probe-remote-file (pathname)
  "Return t and a type (:file or :directory) if remote $pathname exists,
   else ()."
  (let ((account (make-inet-account (remote-pathname-host pathname))))
    (fill-from-netrc account)
    (flet ((simplify (pathname)
	     (concat (remote-pathname-host pathname)
		     ":"
		     (let ((name (unix:unix-simplify-pathname
				  (remote-pathname-local pathname))))
		       (if (string= name "./")
			   "" name)))))
      (flet ((ftp-probe (account pathname)
	       (multiple-value-bind (success type)
				    (ftp-probe-file
				     account
				     (remote-pathname-local pathname))
		 (if success (values (simplify pathname) type)))))
	(case= (inet-account-protocol account)
	  ("ssh"
	   (multiple-value-bind (success type)
				(ssh-probe-file
				 account
				 (remote-pathname-local pathname))
	     (if success (values (simplify pathname) type))))
	  ("ftp"
	   (ftp-probe account pathname))
	  (t
	   (ftp-probe account pathname)))))))

(defun read-remote-file (pathname stream)
  "Read remote file $pathname into $stream."
  (let ((account (make-inet-account (remote-pathname-host pathname))))
    (fill-from-netrc account)
    (case= (inet-account-protocol account)
      ("ssh"
       (ssh-get-file account
		     (remote-pathname-local pathname)
		     stream))
      ("ftp"
       (ftp-get-file account
		     (remote-pathname-local pathname)
		     stream))
      (t
       (ftp-get-file account
		     (remote-pathname-local pathname)
		     stream)))))

(defun write-remote-file (pathname stream if-exists-action)
  "Write a file from $stream into $pathname."
  ;;; Write to the host named like the search list.
  (let ((account (make-inet-account (remote-pathname-host pathname))))
    (fill-from-netrc account)
    (if (eq if-exists-action :append) (error "FIX :append"))
    (case= (inet-account-protocol account)
      ("ssh"
       (ssh-put-file account stream (remote-pathname-local pathname)))
      ("ftp"
       (ftp-put-file account stream (remote-pathname-local pathname)))
      (t
       (ftp-put-file account stream (remote-pathname-local pathname))))))


;;;; Telnet.

(defun telnet-init (account port timeout)
  "Initialize a telnet connection for ACCOUNT.  Return the created stream
   on success, else ()."
  (let* ((stream (raw-connect (inet-account-server account)
			      port timeout "TELNET"))
	 (line (setf (inet-stream-response stream)
		     (read-line stream ()))))
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
  (apply #'mess (concat "-> " command) command-args)
  (write-pop stream type command command-args)
  (let ((line (setf (inet-stream-response stream) (read-line stream))))
    (mess "<- ~A" line)
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
    (concatenate 'simple-string
		 (case= (string-upcase (car parts))
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

(defun read-lines (stream line end)
  (while ((next (read-line stream) (read-line stream))
	  (end-length (length end)))
	 (next)
    (setq line (format () "~a~%~a"
		       (subseq line 0 (1- (length line)))
		       next))
    (and (>= (length next) end-length)
	 (string= next end :end1 end-length)
	 (return)))
  line)

(defun read-any-lines (stream line)
  (while ((next (read-line stream ()) (read-line stream ())))
	 (next)
    (setq line (format () "~a~%~a"
		       (subseq line 0 (1- (length line)))
		       next)))
  line)

(defun ftp-connect-passive (control)
  (when (ftp-command control "PASV")
    (multiple-value-bind (host port)
			 (parse-pasv control)
      (raw-connect host port *ftp-timeout* "FTP"))))

(defun ftp-read-response (stream)
  (let* ((line (setf (inet-stream-response stream)
		     (read-line stream)))
	 (code (and (plusp (length line))
		    (parse-integer line :junk-allowed t))))
    (mess "ftp< ~A" line)
    (case code
      ((()))
      (211 ; Status.
       (setf (inet-stream-response stream)
	     (read-lines stream line "211 ")))
      (213 ; Stat.
       (setf (inet-stream-response stream)
	     (read-lines stream line "213")))
      (214 ; Help.
       (setf (inet-stream-response stream)
	     (read-lines stream line "214")))
      (221 ; Quit.
       (if (open-stream-p stream) (ignore-errors (close stream)))
       line)
      (221 ; Passive mode.
       (read-any-lines stream line))
      (412 ; Timeout.
       (if (open-stream-p stream) (ignore-errors (close stream)))
       ())
      (550 ; Delete failed.
       ())
      (t
       (fi (>= code 500) line)))))

(defun ftp-command (stream command &rest command-args)
  (let ((command (ftp-translate-command command)))
    (write-inet stream command command-args)
    ;; Wait a bit in case the command is slow.
    (sleep 0.07) ; FIX how to garauntee response?
    (ftp-read-response stream)))

(defun ftp-init (account port timeout)
  "Initialize an FTP connection for $account.  Return the created stream on
   success, else () and the response."
  (or (get-controller (inet-account-server account)
		      "ftp"
		      (account-user account))
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
		 (let ((controller (make-ftp-controller
				      :stream stream
				      :dir "/")))
		   (store-controller controller
				     (inet-account-server account)
				     "ftp"
				     (account-user account))
		   controller))
	    (progn
	      (write-inet stream "QUIT")
	      (ignore-errors (close stream))
	      (values () (inet-stream-response stream)))))))

(defun parse-pasv (stream)
  (let ((response (inet-stream-response stream)))
    (when (> (length response) 27)
      (let ((split (split response '(#\,) :start 27)))
	(or (and (> (length split) 4)
		 (every (lambda (ele) (and ele
					   (integerp (parse-integer ele
								    :junk-allowed t))))
			split))
	    (error "Failed to parse PASV response~%  => ~A." response))
	(values (format () "~A.~A.~A.~A"
			(pop split) (pop split)
			(pop split) (pop split))
		(+ (ash (parse-integer (pop split)) 8)
		   (parse-integer (pop split) :junk-allowed t)))))))

(defun ftp-ls (control data out &optional (spec "") details-p)
  (when (ftp-command control (format () (if details-p "LIST ~A" "NLST ~A") spec))
    ;; FIX check response
    (transfer data out)
    (setf (inet-stream-response control) (read-line control))))

;;; Public
;;;
(defun ftp-list-dir (account directory out)
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (if (if directory
	    (or (string= directory "")
		(ftp-command control "CWD ~A" directory))
	    t)
	(when (ftp-command control "PASV")
	  (multiple-value-bind (host port)
			       (parse-pasv control)
	    (let ((in (raw-connect host port *ftp-timeout* "FTP")))
	      (unwind-protect
		  (ftp-ls control in out "" t)
		(close in))))))))

(defun ftp-put-file (account src dest)
  "Put local file SRC into DEST at ACCOUNT."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (let ((dest-dir (directory-namestring dest)))
      (if (if dest-dir
	      (or (string= dest-dir "")
		  (ftp-command control "CWD ~A" dest-dir))
	      t)
	  (if (ftp-command control "TYPE I")
	      (if (ftp-command control "PASV")
		  (multiple-value-bind (host port)
				       (parse-pasv control)
		    (let ((out (raw-connect host port
					    *ftp-timeout* "FTP")))
		      (if (ftp-command control "STOR ~A"
				       (file-namestring dest))
			  (progn
			    (unwind-protect
				(progn
				  (etypecase src
				    (stream
				     (transfer src out))
				    ((or pathname string)
				     (with-open-file (in src)
				       (transfer in out))))
				  t)
			      (close out))
			    (ftp-read-response control))
			  (values () (inet-stream-response control)))))
		  (values () (inet-stream-response control)))
	      (values () (inet-stream-response control)))
	  (values () (inet-stream-response control))))))

(defun ftp-get-file (account src dest)
  "Get $src from $account into local file $dest."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (let ((src-dir (directory-namestring src)))
      (when (if src-dir
		(or (string= src-dir "")
		    (ftp-command control "CWD ~A" src-dir))
		t)
	(when (ftp-command control "TYPE I")
	  (when (ftp-command control "PASV")
	    (multiple-value-bind (host port)
				 (parse-pasv control)
	      (let ((in (raw-connect host port *ftp-timeout* "FTP")))
		(unwind-protect
		    (when (ftp-command control "RETR ~A" (file-namestring src))
		      (etypecase dest
			(stream
			 (transfer in dest))
			(pathname
			 (with-open-file (out dest
					      :direction :output
					      :if-does-not-exist :create)
			   (transfer in out))))
		      (ftp-read-response control)
		      t)
		  (close in))))))))))

(defun ftp-add-dir (account pathname)
  "Add directory $pathname to $account."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (ftp-command control "MKDIR ~A" pathname)))

(defun ftp-release-dir (account pathname)
  "Release directory $pathname from $account."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (ftp-command control "RMDIR ~A" pathname)))

(defmacro do-ftp-directory ((account path file) &body body)
  "Do $body with $file bound to every file in $path at $account."
  (let ((controller (gensym)) (control (gensym)) (resp (gensym))
	(path2 (gensym)) (in (gensym)))
    `(let* ((,controller (ftp-init ,account *ftp-port* *ftp-timeout*))
	    (,control (ftp-controller-stream ,controller)))
       (or (ftp-command ,control "CWD /")
	   (error "Failed to change to root directory."))
       (let ((,path2 (if (if ,path (string= (namestring ,path) "") t)
			 "/"
			 (namestring ,path))))
	 ;; FIX consider just treating vars as streams, automatically
	 (let ((,resp (with-output-to-string (str)
			(ftp-ls ,control
				(ftp-connect-passive ,control)
				str
				,path2))))
	   ;(mess "resp ~A" ,resp)
	   (with-input-from-string (,in ,resp)
	     (while ((,file (read-line ,in ()) (read-line ,in ())))
		    (,file)
	       ;; Flush trailing linefeeds.
	       (setq ,file
		     (file-namestring (string-right-trim '(#\return) ,file)))
	       ,@body)))))))

(defun ftp-probe-file (account path)
  "If $path exists at $account return t and the kind of $path (:file or
   :directory), else return ()."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (string= (ftp-controller-dir controller) "/")
	(ftp-command controller "CWD /")
	(error "Failed to change to root directory."))
    (if (if path (string= path "") t)
	(setq path "/"))
    ;(mess "rsp1: .~A." (inet-stream-response control))
    (when (ftp-command control "STAT ~A" path)
      (let ((resp (inet-stream-response control))
	    (pos 0))
	(and (plusp (length resp))
	     (char= (char resp (1- (length resp))) #\return)
	     (setf (char resp (1- (length resp))) #\newline))
	;(mess "rsp: .~A." resp)
	(loop repeat 3 do
	  (setq pos (1+ (or (position #\newline resp :start pos)
			    (return-from ftp-probe-file ())))))
	(values t (if (position #\newline resp :start pos)
		      :directory :file))))))

(defun ftp-delete-file (account path)
  "Delete $path at FTP $account.  Return true if successful."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (ftp-command control (format () "CWD /~A"
				     (directory-namestring path)))
	(error "Failed to change to /~A."
	       (directory-namestring path)))
    (ftp-command control "DELE ~A" (file-namestring path))))

(defun ftp-file-stats (account pathname)
  "Return information about $pathname: result dev-or-err mode nlink uid gid
   rdev size atime mtime."
  (let* ((controller (ftp-init account *ftp-port* *ftp-timeout*))
	 (control (ftp-controller-stream controller)))
    (or (string= (ftp-controller-dir controller) "/")
	(ftp-command controller "CWD /")
	(error "Failed to change to root directory."))
    (if (if pathname (string= pathname "") t)
	(setq pathname "/"))
    (when (ftp-command control "STAT ~A" pathname)
      (let ((resp (inet-stream-response control))
	    (pos 0))
	(when (> (length resp) 2)
	  (if (char= (char resp (1- (length resp))) #\return)
	      (setf (char resp (1- (length resp))) #\newline)))
	;(mess "rsp: ~A" resp)
	(setq pos (1+ (or (position #\newline resp :start pos)
			  (return-from ftp-file-stats ()))))
	(multiple-value-bind (success-p perms links uid gid size mod-time)
			     (parse-ls-line (subseq resp pos))
	  (when success-p
	    (values t () (parse-mode-string perms) links uid gid size mod-time)))))))

(defun ftp-timed-out-p (stream)
  "Return t if $stream has timed out."
  (if (listen stream)
      (ftp-read-response stream)
      (handler-case
	  (ftp-command stream "NOOP")
	(error ()
	       (ignore-errors (close stream))
	       (return-from ftp-timed-out-p t))))
  ;; In either above, ftp-read-response closes $stream if it has timed out.
  (fi (open-stream-p stream)))


;;;; Gopher.

#|
(defun ftp-command (stream command &rest command-args)
  (let ((command (ftp-translate-command command)))
    (write-inet stream command command-args)
    ;; Wait a bit in case the command is slow.
    (sleep 0.07) ; FIX how to garauntee response?
    (ftp-read-response stream)))

(defun ftp-ls (control data out &optional (spec "") details-p)
  (when (ftp-command control (format () (if details-p "LIST ~A" "NLST ~A") spec))
    ;; FIX check response
    (transfer data out)
    (setf (inet-stream-response control) (read-line control))))

(defun ftp-list-dir (account directory out)
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (if (if directory
		(or (string= directory "")
		    (ftp-command control "CWD ~A" directory))
		t)
	    (when (ftp-command control "PASV")
	      (multiple-value-bind (host port)
				   (parse-pasv control)
		(let ((in (raw-connect host port *ftp-timeout* "FTP")))
		  (unwind-protect
		      (ftp-ls control in out "" t)
		    (close in))))))))

(defun ftp-put-file (account src dest)
  "Put local file SRC into DEST at ACCOUNT."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (let ((dest-dir (directory-namestring dest)))
      (if (if dest-dir
	      (or (string= dest-dir "")
		  (ftp-command control "CWD ~A" dest-dir))
	      t)
	  (if (ftp-command control "TYPE I")
	      (if (ftp-command control "PASV")
		  (multiple-value-bind (host port)
				       (parse-pasv control)
		    (let ((out (raw-connect host port
					    *ftp-timeout* "FTP")))
		      (if (ftp-command control "STOR ~A"
				       (file-namestring dest))
			  (progn
			    (unwind-protect
				(progn
				  (etypecase src
				    (stream
				     (transfer src out))
				    ((or pathname string)
				     (with-open-file (in src)
				       (transfer in out))))
				  t)
			      (close out))
			    (ftp-read-response control))
			  (values () (inet-stream-response control)))))
		  (values () (inet-stream-response control)))
	      (values () (inet-stream-response control)))
	  (values () (inet-stream-response control))))))

(defun ftp-get-file (account src dest)
  "Get $src from $account into local file $dest."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (let ((src-dir (directory-namestring src)))
      (when (if src-dir
		(or (string= src-dir "")
		    (ftp-command control "CWD ~A" src-dir))
		t)
	(when (ftp-command control "TYPE I")
	  (when (ftp-command control "PASV")
	    (multiple-value-bind (host port)
				 (parse-pasv control)
	      (let ((in (raw-connect host port *ftp-timeout* "FTP")))
		(unwind-protect
		    (when (ftp-command control "RETR ~A" (file-namestring src))
		      (etypecase dest
			(stream
			 (transfer in dest))
			(pathname
			 (with-open-file (out dest
					      :direction :output
					      :if-does-not-exist :create)
			   (transfer in out))))
		      (ftp-read-response control)
		      t)
		  (close in))))))))))

(defun ftp-add-dir (account pathname)
  "Add directory $pathname to $account."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (ftp-command control "MKDIR ~A" pathname)))

(defun ftp-release-dir (account pathname)
  "Release directory $pathname from $account."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (ftp-command control "RMDIR ~A" pathname)))

(defmacro do-ftp-directory ((account path file) &body body)
  "Do $body with $file bound to every file in $path at $account."
  (let ((control (gensym)) (resp (gensym)) (path2 (gensym))
	(in (gensym)))
    `(let ((,control (ftp-init ,account *ftp-port* *ftp-timeout*)))
       (or (ftp-command ,control "CWD /")
	   (error "Failed to change to root directory."))
       (let ((,path2 (if (if ,path (string= (namestring ,path) "") t)
			 "/"
			 (namestring ,path))))
	 ;; FIX consider just treating vars as streams, automatically
	 (let ((,resp (with-output-to-string (str)
			(ftp-ls ,control
				(ftp-connect-passive ,control)
				str
				,path2))))
	   ;(mess "resp ~A" ,resp)
	   (with-input-from-string (,in ,resp)
	     (loop for ,file = (read-line ,in ()) while ,file do
	       ;; Flush trailing linefeeds.
	       (setq ,file
		     (file-namestring (string-right-trim '(#\return) ,file)))
	       ,@body)))))))

(defun ftp-probe-file (account path)
  "If $path exists at $account return t and the kind of $path (:file or
   :directory), else return ()."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (if (if path (string= path "") t)
	(setq path "/"))
    ;(mess "rsp1: .~A." (inet-stream-response control))
    (when (ftp-command control "STAT ~A" path)
      (let ((resp (inet-stream-response control))
	    (pos 0))
	(and (plusp (length resp))
	     (char= (char resp (1- (length resp))) #\return)
	     (setf (char resp (1- (length resp))) #\newline))
	;(mess "rsp: .~A." resp)
	(loop repeat 3 do
	  (setq pos (1+ (or (position #\newline resp :start pos)
			    (return-from ftp-probe-file ())))))
	(values t (if (position #\newline resp :start pos)
		      :directory :file))))))

(defun ftp-delete-file (account path)
  "Delete $path at FTP $account.  Return true if successful."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control (format () "CWD /~A"
				     (directory-namestring path)))
	(error "Failed to change to /~A."
	       (directory-namestring path)))
    (ftp-command control "DELE ~A" (file-namestring path))))

(defun ftp-file-stats (account pathname)
  "Return information about $pathname: result dev-or-err mode nlink uid gid
   rdev size atime mtime."
  (let ((control (ftp-init account *ftp-port* *ftp-timeout*)))
    (or (ftp-command control "CWD /")
	(error "Failed to change to root directory."))
    (if (if pathname (string= pathname "") t)
	(setq pathname "/"))
    (when (ftp-command control "STAT ~A" pathname)
      (let ((resp (inet-stream-response control))
	    (pos 0))
	(when (> (length resp) 2)
	  (if (char= (char resp (1- (length resp))) #\return)
	      (setf (char resp (1- (length resp))) #\newline)))
	;(mess "rsp: ~A" resp)
	(setq pos (1+ (or (position #\newline resp :start pos)
			  (return-from ftp-file-stats ()))))
	(multiple-value-bind (success-p perms links uid gid size mod-time)
			     (parse-ls-line (subseq resp pos))
	  (when success-p
	    (values t () (parse-mode-string perms) links uid gid size mod-time)))))))

(defun ftp-timed-out-p (stream)
  "Return t if $stream has timed out."
  (if (listen stream)
    (ftp-read-response stream)
    (handler-case
	(ftp-command stream "NOOP")
      (error ()
	     (ignore-errors (close stream))
	     (return-from ftp-timed-out-p t))))
  ;; In either above, ftp-read-response closes $stream if it has timed out.
  (fi (open-stream-p stream)))
|#


;;;; HyperText Transfer Protocol (HTTP).

(defun query-param-value (params key)
  "Return the value associated with key in params."
  (cdr (assoc key params)))

(defun hex-char-to-number (char)
  "Return the number represented by hexidecimal $char."
  (let ((code (char-code char)))
    (cond ((and (>= code #.(char-code #\0))
		(<= code #.(char-code #\9)))
	   (- code #.(char-code #\0)))
	  ((and (>= code #.(char-code #\A))
		(<= code #.(char-code #\F)))
	   (+ (- code #.(char-code #\A)) 10))
	  (t (error "$char out of range: ~A" char)))))

(defun hex-digit-to-char (digit)
  "Return the character representing hexidecimal $digit."
  (cond ((and (>= digit 0)
	      (<= digit 9)
	 (code-char (+ digit #.(char-code #\0)))))
	((and (>= digit 10)
	      (<= digit 15))
	 (code-char (+ (- digit 10) #.(char-code #\A))))
	(t (error "$digit out of range: ~A" digit))))

(defun parse-query-string (string)
  "Return an alist of keywords and values from the HTTP query string
   $string, translating query string character encoding in the process."
  (flet ((filter (string)
	   "Return a string like $string, translating query string
	    character encoding."
	   (while ((index 0 (1+ index))
		   (out-index 0 (1+ out-index))
		   (out (copy-seq string))
		   (end (length string)))
		  ((< index end)
		   (subseq out 0 out-index))
	     (let ((char (aref string index)))
	       (case char
		 (#\+ (setf (aref out out-index) #\space))
		 (#\% (setf (aref out out-index)
			    (code-char
			     (+ (* (hex-char-to-number
				    (aref string (incf index)))
				   16)
				(hex-char-to-number
				 (aref string (incf index)))))))
		 (t (setf (aref out out-index) char)))))))
    (collect ((params))
      (dolist (param (split string #\&))
	(let ((pair (split param #\=)))
	  (params (cons
		   (let ((*package* *keyword-package*))
		     (read-from-string (filter (car pair))))
		   (filter (cadr pair))))))
      (params))))

;; FIX merge to above
(defun parse-http-get-args (args)
  (when (plusp (length args))
    (mapcar (lambda (ele)
	      (let ((pos (position #\= ele)))
		(if pos
		    (cons (subseq ele 0 pos)
			  (subseq ele (1+ pos)))
		    (list ele))))
	    (split args #\&))))
#|
(parse-http-get-args "a=1&b=2")
(parse-http-get-args "a&b=2")
(parse-http-get-args "a")
(parse-http-get-args "")
(parse-http-get-args "  ")
(parse-http-get-args "a&&b")
|#



(defun encode-query-string (string)
  "Return a string like $string, encoding special characters."
  (while ((index 0 (1+ index))
	  (out-index 0 (1+ out-index))
	  (out (make-string (* (length string) 3)))
	  (end (length string)))
	 ((< index end)
	  (subseq out 0 out-index))
    (let ((char (aref string index)))
      (case char
	(#\space (setf (aref out out-index) #\+))
	(#\% (setf (aref out out-index) #\%
		   (aref out (incf out-index)) #\2
		   (aref out (incf out-index)) #\5))
	(#\& (setf (aref out out-index) #\%
		   (aref out (incf out-index)) #\2
		   (aref out (incf out-index)) #\6))
	(#\= (setf (aref out out-index) #\%
		   (aref out (incf out-index)) #\3
		   (aref out (incf out-index)) #\D))
	(t (let ((code (char-code char)))
	     (cond ((or (< code 32) (> code 126))
		    ;; FIX surely some of these are errors
		    (setf (aref out out-index) #\%)
		    (setf (aref out (incf out-index))
			  (hex-digit-to-char (truncate code 16)))
		    (setf (aref out (incf out-index))
			  (hex-digit-to-char (mod code 16))))
		   (t
		    (setf (aref out out-index) char)))))))))

(defun http-init (account port timeout)
  "Initialize an HTTP connection on $port with $timeout to the server in
   $account."
  (raw-connect (inet-account-server account)
	       port timeout "HTTP"))

(defun http-get (account url dest)
  "Get $url from $account into local file $dest."
  (let ((in (http-init account *http-port* *http-timeout*)))
    (write-inet in "GET ~A" (list url))
    (with-open-file (out dest :direction :output)
      (transfer in out))
    (close in)))

(defun http-post (account url env params dest)
  "Get $url from $account into local file $dest, passing $params as query
   arguments to $url."
  (let ((in (http-init account *http-port* *http-timeout*))
	(content (with-output-to-string (stream)
		   (when params
		     (format stream "~%~A=~A"
			     (encode-query-string (caar params))
			     (encode-query-string
			      (string (cadar params))))
		     (dolist (param (cdr params))
		       (format stream "&~A=~A"
			       (encode-query-string (car param))
			       (encode-query-string
				(string (cadr param)))))
		     (push '("Connection" "close") env)
		     (push '("Content-Type"
			     "application/x-www-form-urlencoded")
			   env)))))
    (if content (push `("Content-Length" ,(length content)) env))
    (write-inet in "POST ~A HTTP/1.0~%~{~A~%~}~A~%"
		(list url
		      (mapcar (lambda (ele)
				(concatenate 'simple-string
					     (encode-query-string
					      (car ele))
					     ": "
					     (encode-query-string
					      (string (cadr ele)))))
			      env)
		      content))
    (with-open-file (out dest :direction :output)
      (transfer in out))
    (close in)))


;;;; Secure Shell (SSH).

(defun %ssh-command (process command &rest command-args)
  (write-line (apply #'format () command command-args)
	      (process-pty process))
  (force-output (process-pty process))
  ;; Read the echo'd command.
  (read-line (process-pty process) ()))

(defun ssh-command (process command &rest command-args)
  "Apply $command-args to $command with format and write the result to the
   pty of $process.  Read (into lisp) and return the output of the
   command."
  (apply #'%ssh-command process command command-args)
  ;; Read the command output.
  (read-from-string (read-line (process-pty process) ())))

(defun ssh-init (account)
  (or (get-controller (inet-account-server account)
		      "ssh"
		      (account-user account))
      (let ((process (run-program "ssh" (list ;"-B" ; Batch.
					 (format () "~A@~A"
						 (account-user account)
						 (inet-account-server account))
					 "/bin/sh 2>/dev/null")
				  :pty t :wait ()
				  :output t :input t)))
	(when (and process (eq (process-status process) :running))
	  (write-line "echo oxlskkslxo" (process-pty process))
	  (force-output (process-pty process))
	  ;; FIX read w timeout? (and close process on timeout)
	  (loop for line = (read-line (process-pty process) ()) while line do
	    (when (if (eq (length line) (1+ (length "oxlskkslxo")))
		      (string= line "oxlskkslxo")
		      (string= line "oxlskkslxo"))
	      (store-controller process
				(inet-account-server account)
				"ssh"
				(account-user account))
	      (return process)))))))

(defun ssh-probe-file (account file)
  "Return t and a type (:file or :directory) if $file exists at $account,
   else ()."
  (let ((process (ssh-init account)))
    (when process
      (case (ssh-command process "if [ -e ~A ]; then if [ -d ~A ]; then echo 7; else echo 8; fi; else echo 9; fi" file file)
	(7 (values t :directory))
	(8 (values t :file))
	(t ())))))

(defun ssh-ls (process &optional (spec "") details-p)
  (%ssh-command process
		(format () (if details-p
			       "ls -Llna ~A; echo \-\-end\-marker\-\-"
			       "ls -a ~A; echo \-\-end\-marker\-\-")
			spec)))

(defun ssh-add-dir (account pathname)
  "Add directory $pathname to $account."
  (let ((process (ssh-init account)))
    (case (ssh-command process "mkdir ~A; echo $?" pathname)
      (0 t)
      (t ()))))

(defun ssh-release-dir (account pathname)
  "Release directory $pathname from $account."
  (let ((process (ssh-init account)))
    (case (ssh-command process "rmdir ~A; echo $?" pathname)
      (0 t)
      (t ()))))

(defmacro do-ssh-directory ((account path file) &body body)
  "Do $body with $file bound to every file in $path at $account."
  (let ((process (gensym)) (in (gensym)) (str (gensym)))
    `(let ((,process (ssh-init ,account)))
       (ssh-ls ,process ,path)
       (let ((,str (with-output-to-string (out)
		     ;; FIX
		     ;; Wait for the first line, so that the command can
		     ;; complete.
		     (let ((line (read-line (process-pty ,process) ())))
		       (if line (write-string line out)))
		     ;; Wait again, just to make sure.
		     (sleep 0.2)
		     (loop for line = (read-line (process-pty ,process) ())
		       while line do
		       (or (listen (process-pty ,process))
			   (if (and (>= (length line) (length "--end-marker--"))
				    (or (string= line "--end-marker--")
					(string= line "--end-marker--")))
			       (return)))
		       (write-line line out)))))
	 (with-input-from-string
	     (,in ,str)
	   (loop for ,file = (read-line ,in ())
	     while ,file do
	     ;; Flush trailing linefeeds.
	     (setq ,file
		   (file-namestring (string-right-trim '(#\return)
						       ,file)))
	     (if (find #\return ,file)
		 ;; It seems that sometimes the newline is missing from the
		 ;; first line.
		 (loop for ,file in (split ,file '(#\return)) do
		   (setq ,file
			 (file-namestring (string-right-trim '(#\return)
							     ,file)))
		   ,@body)
		 ,@body)))))))

(defun ssh-delete-file (account path)
  "Delete $path at SSH $account.  Return true if successful."
  (let ((process (ssh-init account)))
    (case (ssh-command process "rm -f ~A; echo $?" path)
      (0 t)
      (t ()))))

(defun ssh-get-file (account src dest)
  "Copy $src from $account into local file $dest."
  (let* ((dest1 (if (streamp dest)
		    (pick-new-file)
		    (namestring dest)))
	 (process (run-program "scp" (list ;"-B" ; Batch.
				      (format () "~A@~A:~A"
					      (account-user account)
					      (inet-account-server account)
					      src)
				      dest1)
			       :wait t
			       :pty t)))
    (if (streamp dest)
	(with-open-file (in dest1 :direction :input)
	  (transfer in dest)))
    (when process
      (if (process-pty process) (close (process-pty process)))
      (case (process-exit-code process)
	(0 (process-close process) t)
	(t (process-close process) ())))))

#|
(defun ssh-get-file (account src dest)
  "Get $src from $account into local file or stream $dest."
  (error "convert to scp")
  (let ((process (ssh-init account))
	(dest1 (if (streamp dest) (pick-new-file) dest)))
    (%ssh-command process "cat ~A; echo \-\-end\-marker\-\-"
		  (namestring src))
    (with-open-file (out dest1 :direction :output)
      (loop for line = (read-line (process-pty process) ())
	while line do
	(or (listen (process-pty process))
	    (if (and (>= (length line) (length "--end-marker--"))
		     (or (string= line "--end-marker--")
			 (string= line "--end-marker--")))
		(return)))
	(write-line (string-right-trim '(#\return) line) out)))
    (if (streamp dest)
	(with-open-file (in dest1) (transfer in dest)))))
|#

(defun ssh-put-file (account src dest)
  "Put local file SRC into dest from ACCOUNT."
  (let* ((src1 (if (streamp src)
		   (let ((src1 (pick-new-file)))
		     ;; FIX just (transfer src out)?
		     (with-open-file (out src1 :direction :output)
		       (transfer src out))
		     src1)
		   src))
	 (process (run-program "scp" (list ;"-B" ; Batch.
				      (namestring src1)
				      (format () "~A@~A:~A"
					      (account-user account)
					      (inet-account-server account)
					      dest))
			       :wait t
			       :pty t)))
    (when process
      (if (process-pty process) (close (process-pty process)))
      (case (process-exit-code process)
	(0 (process-close process) t)
	(t (process-close process) ())))))

#|
(defun ssh-put-file (account src dest)
  "Put local file $src into $dest at $account."
  (let ((process (ssh-init account)))
    (when (ssh-command process "echo > ~A; echo $?" dest)
      (if (streamp src)
	  (loop for line = (read-line src ()) while line do
	    (or (ssh-command process "echo ~A >> ~A; echo $?" line dest)
		(return-from ssh-put-file ())))
	  (with-open-file (in src :direction :input)
	    (loop for line = (read-line in ()) while line do
	      (or (ssh-command process "echo ~A >> ~A; echo $?" line dest)
		  (return-from ssh-put-file ())))))
      t)))
|#

(defun ssh-file-stats (account pathname)
  "Return information about $pathname: result dev-or-err mode nlink uid gid
   rdev size atime mtime."
  (let ((process (ssh-init account)))
    ;; Take care with the ls options, they must serve GNU and busybox, at least.
    (%ssh-command process "ls -Llnd ~A; echo oxlxo;" pathname)
    (let ((line (read-line (process-pty process) ())))
      (mess "line: ~A" line)
      (when line
	(unwind-protect
	    (progn
	      (setq line (string-right-trim '(#\return) line))
	      (and (= (length line) (length "oxlxo"))
		   (string= line "oxlxo")
		   (return-from ssh-file-stats ()))
	      (multiple-value-bind (success-p perms links uid gid size mod-time)
				   (parse-ls-line line)
		;; Ensure get end marker line.
		(loop for line = (read-line (process-pty process) ())
		  while line do
		  (mess "line 2: ~A" line)
		  (and (>= (length line) (length "oxlxo"))
		       (or (string= line "oxlxo")
			   (string= line "oxlxo"))
		       (return)))
		(when success-p
		  (values t () (parse-mode-string perms)
			  links uid gid size mod-time))))
	  ;; Read any remaining lines.
	  (loop for line = (if (listen (process-pty process))
			       (read-line (process-pty process) ()))
	    while line do
	    (mess "line 3: ~A" line)))))))

(defun ssh-set-write-date (account pathname date)
  "Set write date of $pathname of $account to $date."
  (let ((process (ssh-init account)))
    (ssh-command process "touch --no-create -t ~A ~A; echo $?"
		 (format-universal-time () date :style :condensed)
		 pathname)))


;;;; Simple Mail Transfer Protocol (SMTP).

(defvar *smtp-port* 25
  "Standard SMTP port.")

(defvar *smtp-timeout* 20
  "Timeout for SMTP, in seconds.")

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
		     ;; Only auth if user,password set in account.
		     (fi (and (account-user account)
			      (account-password account))
			 t
			 (progn
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
				  (string= line "235" :end1 3))))))))
	     stream)
	(progn
	  (write-inet stream "QUIT")
	  (close stream)
	  (values () (inet-stream-response stream))))))

(defmacro %smtp-mail ((stream to from account) &body body)
  "Send a message to the addresses listed in TO from address FROM, reading
   account details from ACCOUNT.  Bind STREAM to the SMTP connection and
   invoke BODY to write the message contents (i.e. to follow the SMTP DATA
   command), then write the trailing \".\".  On success return t, otherwise
   return () and the error response."
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

(defun smtp-mail (to from account writer &rest writer-args)
  "Send an SMTP message to the addresses listed in $to from address $from,
   reading account details from $account.

   Call the function $writer to write the entire message, including the
   headers.  Pass $writer the following arguments: the stream to which to
   write, $to, $from and $writer-args.  $writer must return true on
   success, false and an error message otherwise.  Return the values
   returned from $writer."
  (%smtp-mail (stream to from account)
    (apply writer stream to from writer-args)))


;;;; UKFSN webmail mailer hack.  This is intended only for use in emergencies.

;; TODO Generalise to any Iloha Mail server.

(defun ukfsn-mail (to from account writer &rest writer-args)
  "Send $message to the addresses listed in $to from address $from, reading
   account details from $account.  The name and password in $account must
   be a valid UKFSN account (http://www.ukfsn.org).  Use the UKFSN webmail
   system to send the mail.

   Call the function $writer to write the entire message, including the
   headers.  Pass $writer the following arguments: the stream to which to
   write, $to, $from and $arg.  $writer must return true on success, false
   and an error message otherwise.  Return the values returned from
   $writer."
  (declare (ignore account))
  (or (eq (length to) 1)
      ;; FIX
      (error "Webmail hack only supports one address in To header: ~S"
	     to))
  (with-temp-file (message-stream :direction :io)
    ;; Write the message to a temp file.
    (apply writer message-stream to from writer-args)
    ;; Parse the headers from the temp file.
    (file-position message-stream 0)
    (let* ((headers (or (mh::scan-message message-stream)
			(error "Failed to scan message")))
	   (entry (cons 0 headers))
	   (subject (or (cdr (mh::get-header entry "Subject")) ""))
	   (cc (or (cdr (mh::get-header entry "Cc")) ""))
	   (bcc (or (cdr (mh::get-header entry "Bcc")) ""))
	   (reply (or (cdr (mh::get-header entry "In-reply-to")) ""))
	   (body-start (caddr (mh::get-body entry)))
	   (body (with-output-to-string (out)
		   (file-position message-stream body-start)
		   (transfer message-stream out)))
	   (account (internet:make-inet-account "www.ukfsn.org")))
      (declare (ignore reply))
      (fill-from-netrc account)
      (or (inet-account-user account)
	  (error "Webmail hack requires user name in account (via :.netrc)."))
      ;;
      ;; Send the message.
      (with-temp-file (response-stream :direction :io)
	;; Login.
	(http-post account
		   "http://www.ukfsn.org/webmail/index.php"
		   () ; Environment.
		   `(("logout" 0)
		     ("user" ,(inet-account-user account))
		     ("password" ,(or (inet-account-password account)
				      ""))
		     ("host" "ukfsn.org")
		     ("port" 143)
		     ("rootdir"))
		   (pathname response-stream))
	;; Find the session user ID in the response.
	(file-position response-stream 0)
	(copy-file (pathname response-stream) "/tmp/out1")
	(let ((user (while ((line (read-line response-stream ())
				  (read-line response-stream ())))
			   (line
			    (error "Failed to find user in response"))
		      (let* ((unique "main.php?folder=INBOX&user=")
			     (pos (search unique line)))
			(when pos
			  (let* ((start (+ pos (length unique)))
				 (end-pos (search "\"" line
						  :start2 start)))
			    (when end-pos
			      (return (subseq line start
					      end-pos)))))))))
	  (mess "user id: ~A" user)
	  (unwind-protect
	      (progn
		;; Send.
		(http-post account
			   "http://www.ukfsn.org/webmail/compose2.php"
			   () ; Environment.
			   (let ((params
				  `(("user" ,user)
				    ("subject" ,subject)
				    ("to" ,(car to))
				    ("cc" ,cc)
				    ("bcc" ,bcc)
				    ("message" ,body)
				    ("send_1" "t"))))
			     #| FIX
			     (if reply
				 ("in_reply_to" "INBOX:<id>")
				 ("replyto_messageID" "INBOX:<id>")
				 ("folder" "INBOX"))
			     |#
			     params)
			   (pathname response-stream))
		;; Check the result.
		(file-position response-stream 0)
		(copy-file (pathname response-stream) "/tmp/out2")
		(while ((line (read-line response-stream ())
			      (read-line response-stream ())))
		       (line
			(values ()
				"Failed to find success string in response."))
		  (let ((pos (search "Message successfully sent" line)))
		    (when pos
		      (return t)))))
	    ;; Logout.
	    (http-get account
		      (format ()
			      "http://www.ukfsn.org/webmail/login.php?logout=1&user=~A"
			      user)
		      (pathname response-stream))
	    (file-position response-stream 0)
	    (copy-file (pathname response-stream) "/tmp/out3")))))))

#|
(delete-file ":tmp/out")
(let ((account (internet:make-inet-account "www.mundell.ukfsn.org.")))
  (internet::http-get account "http://www.mundell.ukfsn.org/" ":tmp/out"))
(let ((account (internet:make-inet-account "www.ukfsn.org.")))
  (internet::http-get account "http://www.ukfsn.org/webmail/" ":tmp/out"))
(let ((account (internet:make-inet-account "www.ukfsn.org.")))
  (internet::http-get account "https://www.ukfsn.org/webmail/" ":tmp/out"))

FIX https

(let ((account (internet:make-inet-account "www.ukfsn.org.")))
  (internet::http-post account "http://www.ukfsn.org/webmail/index.php"
		       ()
		       `(("logout" 0)
			 ("user" "mundell")
			 ("password" "bkt533ec")
			 ("host" "ukfsn.org")
			 ("port" 143)
			 ("rootdir" ""))
		       ":tmp/out"))

FIX parse session user  1213495500-24881

;; request compose page
(let ((account (internet:make-inet-account "www.ukfsn.org.")))
  (internet::http-get account "http://www.ukfsn.org/webmail/compose2.php?user=1213495500-24881"
		      ":tmp/out"))

;; request send
(let ((account (internet:make-inet-account "www.ukfsn.org.")))
  (internet::http-post account "http://www.ukfsn.org/webmail/compose2.php?user=1213495500-24881&subject=manual%20test%202&to=matt@mundell.ukfsn.org&message=test%20body&send_1=t"
		       ;; cc=&bcc=&
		       ":tmp/out"))

|#


;;;; FTP Server

(export '(start-ftp-server stop-ftp-server))

;;; Internal
;;;
;;; The FTP server handler.
;;;
(defvar *ftp-server* ())

;;; Internal
;;;
;;; List of FTP server sessions.
;;;
(defvar *ftp-server-sessions* ())

(defun service-reply (string)
  ;; FIX
  (declare (ignore string)))

(defun starts (line string)
  ;; FIX
  (declare (ignore line string)))

(defun serve-ftp-login (socket)
  (let* ((stream (session-stream *ftp-server-sessions* socket))
	 (line (read-line stream ())))
    ;; FIX if line () close session
    (if (starts line "USER")
	(service-reply "0 OK") ; FIX read more lines or install next handler
	(service-reply
	 "500 Error.  Start with \"USER <username> | anonymous\"."))))

(defun accept-ftp-session (socket)
  (multiple-value-bind
      (newconn)
      (ext:accept-tcp-connection socket)
    (when newconn
      (system:add-fd-handler
       newconn :input #'serve-ftp-login))))

;;; Public
;;;
(defun start-ftp-server ()
  (or *ftp-server*
      (let ((socket (ext:create-inet-listener *ftp-port*)))
	(setq *ftp-server*
	      (system:add-fd-handler
	       socket :input
	       #'accept-ftp-session)))))

;;; Public
;;;
(defun stop-ftp-server ()
  (when *ftp-server*
    (system:remove-fd-handler *ftp-server*)
    (setq *ftp-server* ())))


;;;; File Transfer Protocol (FTP) Service

;; FIX same as above

;;; Internal
;;;
;;; The FTP server handler.
;;;
(defvar *ftp-server* ())

;;; Internal
;;;
;;; List of FTP server sessions.
;;;
(defvar *ftp-server-sessions* ())

(defun serve-ftp-login (socket)
  (let* ((stream (session-stream *ftp-server-sessions* socket))
	 (line (read-line stream ())))
    ;; FIX if line () close session
    (if (starts line "USER")
	(service-reply "0 OK") ; FIX read more lines or install next handler
	(service-reply
	 "500 Error.  Start with \"USER <username> | anonymous\"."))))


;;;; Service support.

;;; Internal
;;;
;;; A list of all Internet services.
;;;
(defvar *services* (make-string-table))

(defstruct service
  name
  starter
  stopper
  prober
  handler)

(defvar *log-directory* "/var/log/ni/")

(defun log-now ()
  (format-time
   ()
   :style :iso8601))

(defun log-service (service format &rest args)
  (let ((service-info (getstring service *services*)))
    (when service-info
      (with-open-file (log (merge-pathnames (string-downcase service)
					    *log-directory*)
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :append)
	(format log "~A  " (log-now))
	(apply #'funcall #'format log format args)))))

(defun accept-session (socket handler sessions-var)
  (multiple-value-bind
      (newconn addr)
      (ext:accept-tcp-connection socket)
    (when newconn
      (let ((stream (make-inet-stream newconn
				      :input t :output t
				      :buffering :none)))
	(set sessions-var
	     (cons (list newconn
			 stream
			 (system:add-fd-handler newconn :input handler)
			 ()) ; Service specific.
		   (symbol-value sessions-var)))
	(values t addr stream)))))

(defun session-stream (sessions socket)
  (cadr (assoc socket sessions)))

(defun session-handler (sessions socket)
  (caddr (assoc socket sessions)))

(defun session-extra (sessions socket)
  (cadddr (assoc socket sessions)))

(defun %set-session-extra (sessions socket value)
  (setf (car (cdddr (assoc socket sessions))) value))

(defsetf session-extra %set-session-extra)

(defun end-session (sessions-var socket)
  (let* ((sessions (symbol-value sessions-var))
	 (assoc (assoc socket sessions)))
    (system:remove-fd-handler (caddr assoc))
    (set sessions-var (delete assoc sessions))
    ;; FIX do something if err
    ;(unix:unix-close socket)
    ))

(defmacro defservice (name port handler &optional acceptor)
  (let* ((restartp (gensym))
	 (name (string-upcase name))
	 (starter-name (intern (format ()
				       "START-~A-SERVICE"
				       name)))
	 (stopper-name (intern (format ()
				       "STOP-~A-SERVICE"
				       name)))
	 (service-defvar-name (intern (format ()
					      "*~A-SERVICE*"
					      name)))
	 (port-var (intern (format ()
				   "*~A-SERVICE-PORT*"
				   name)))
	 (sessions-var (intern (format ()
				       "*~A-SERVICE-SESSIONS*"
				       name))))
    `(progn
       (let ((,restartp (and (getstring ,name *services*)
			     (probe-service ,name))))
	 (if ,restartp (stop-service ,name))
	 (defvar ,service-defvar-name ())
	 (defvar ,port-var ,port)
	 (defvar ,sessions-var ())
	 (defun ,starter-name ()
	   (or ,service-defvar-name
	       ;; FIX try read port with unix-getservbyname
	       (let ((socket (ext:create-inet-listener ,port-var)))
		 (setq ,service-defvar-name
		       (system:add-fd-handler
			socket :input
			(lambda (socket)
			  (multiple-value-bind
			      (ok addr ,@(if acceptor (list 'stream)))
			      (accept-session socket
					      ,handler
					      ',sessions-var)
			    (if ok
				(log-service ,name
					     "+ ~A~%" addr))
			    ,(if acceptor
				 `(funcall ,acceptor stream)))))))))
	 (defun ,stopper-name ()
	   (when ,service-defvar-name
	     (let ((socket (lisp::handler-descriptor
			    ,service-defvar-name)))
	       (system:remove-fd-handler ,service-defvar-name)
	       ;; FIX close all sessions
	       (close-socket socket))
	     (setq ,service-defvar-name ())))
	 (setf (getstring ,name *services*)
	       (make-service :name ,name
			     :starter #',starter-name
			     :stopper #',stopper-name
			     :handler ,handler))
	 (when ,restartp
	   (start-service ,name)
	   (warn ,(format () "Restarted service ~A." name)))))))

(export '(start-service stop-service))

(defun start-service (name)
  (let ((service (getstring name *services*)))
    (when service
      (prog1 (funcall (service-starter service))
	(log-service name "start~%")))))


;;;; Echo service.

(defun serve-echo (socket)
  ;; FIX if first listen () close session
  (while ((stream (session-stream *echo-service-sessions* socket)))
	 ((listen stream))
    ;; FIX will this read block if something else reads the data here?
    (multiple-value-bind (line eofp)
			 (read-line stream ())
      (or line (return))
      (write-line line stream)
      (or eofp (write-char #\newline stream)))))

(defservice "echo" 7777 #'serve-echo)


;;;; HTTP service.

(defun print-directory-html (stream &optional (pathname ""))
  (let ((contents (sort (delete-duplicates
			 (list-files pathname #'identity
				     :all t
				     :check-for-subdirs t
				     :follow-links t  :backups ()
				     :recurse ()))
			#'string<)))
    (format stream "<HTML><BODY><TABLE>~%")
    (if (probe-file "HEADER.html")
	(from-file (in "HEADER.html") (transfer in stream)))
    (format stream "<TR>~
		    <TD>Mode</TD>~
		    <TD>Owner</TD>~
		    <TD>Size</TD>~
		    <TD>Mod_time</TD>~
		    <TD>Name</TD>~
		    </TR>~%")
    (dolist (entry contents)
      (multiple-value-bind (res dev ino mode nlink uid gid rdev size atime mtime)
			   (file-stats entry)
	(declare (ignore dev ino nlink gid rdev atime))
	(if res
	    (format stream "<TR>~
			    <TD>~A</TD>~
			    <TD>~A</TD>~
			    <TD>~A</TD>~
			    <TD>~12A</TD>~
			    <TD><A HREF=\"~A\">~A</A></TD>~
			    </TR>~%"
		    (with-output-to-string (out)
		      (lisp::print-mode mode out))
		    (format () "~A"
			    (if uid
				(or (lisp::lookup-login-name uid)
				    uid)
				""))
		    size
		    (multiple-value-bind (sec min hr date month year day ds tz)
					 (get-decoded-time)
		      (declare (ignore sec min hr date month day ds))
		      (lisp::decode-universal-time-for-files mtime year tz))
		    entry entry))))
    (format stream "</TABLE>")
    (if (probe-file "FOOTER.html")
	(from-file (in "FOOTER.html") (transfer in stream)))
    (format stream "</BODY></HTML>~%")))

(declaim (special *args* *headers*))

(defun serve-http-entity (stream name)
  (log-service "http" " >> ~A~%" name)
  (let ((pathname (merge-pathnames name "http:")))
    (multiple-value-bind (exists kind)
			 (probe-file pathname)
      (if exists
	  (if (eq kind :directory)
	      ;; FIX Even with the directorify the browser asks for
	      ;; http:website/style.css instead of
	      ;; http:website/cv/style.css for http:website/cv (which is a
	      ;; dir).  It asks for the right css with apache.  Maybe
	      ;; apache replies with a redirect?
	      (in-directory (directorify pathname)
		(cond ((probe-file "index.html")
		       (from-file (in "index.html")
			 (transfer in stream)))
		      ((probe-file "index.htm")
		       (from-file (in "index.htm")
			 (transfer in stream)))
		      (t
		       (print-directory-html stream))))
	      (if (string= (pathname-type pathname) "nids")
		  (ignore-errors
		   (let ((*standard-output* stream))
		     ;; FIX NB this runs in the server lisp so any
		     ;; changes to the state of the server lisp remain
		     ;; FIX at least run in temporary current package
		     ;; FIX maybe run in slave?
		     (load pathname :verbose () :print ())))
		  (from-file (in pathname) (transfer in stream))))
	  (format stream
		  "404 Error: failed to probe file: \"~A\".~%"
		  name)))))

(defun serve-http (socket)
  (let ((stream (session-stream *http-service-sessions* socket)))
    ;; FIX if listen is () close session
    (when (listen stream)
      ;; FIX will this read block if something else reads the data here?
      (let ((line (read-line stream ())))
	(log-service "http" " <- ~A~%" line)
	(cond
	 ((and line
	       (> (length line) 4)
	       (string= (string-upcase line :end 3) "GET" :end1 3))
	  (let* ((rest (string-trim '(#\space #\tab
					      #\newline #\return)
				    (subseq line 4)))
		 (full-name (subseq rest 0
				    (or (position #\? rest)
					(position #\space rest))))
		 (args-pos (position #\? rest))
		 (*args* (if args-pos
			     (parse-query-string
			      (subseq rest
				      (1+ args-pos)
				      (position #\space rest)))))
		 (name (if (char= (char full-name 0) #\/)
			   (subseq full-name 1)
			   full-name)))
	    (collect ((headers))
	      (while ()
		     ((listen stream))
		(let ((line (read-line stream ())))
		  (log-service "http" " <- ~A~%" line)
		  (if (or (zerop (length line))
			  (string= line (string #\return)))
		      (return)
		      (let ((colon (search ": " line)))
			(headers (cons (subseq line 0 colon)
				       (subseq line (+ colon 2)
					       (1- (length line)))))))))
	      (let ((*headers* (headers)))
		(serve-http-entity stream name)))))
	 ((and line
	       (> (length line) 5)
	       (string= (string-upcase line :end 4) "POST" :end1 4))
	  (let* ((rest (string-trim '(#\space #\tab #\newline #\return)
				    (subseq line 5)))
		 (full-name (subseq rest 0
				    (or (position #\? rest)
					(position #\space rest))))
		 (args-pos (position #\? rest))
		 (*args* (if args-pos
			     (parse-query-string
			      (subseq rest
				      (1+ args-pos)
				      (position #\space rest)))))
		 (name (if (char= (char full-name 0) #\/)
			   (subseq full-name 1)
			   full-name)))
	    (collect ((headers))
	      (while ()
		     ((listen stream))
		(let ((line (read-line stream ())))
		  (log-service "http" " <- ~A~%" line)
		  (if (or (zerop (length line))
			  (string= line (string #\return)))
		      (return)
		      (let ((colon (search ": " line)))
			(headers (cons (subseq line 0 colon)
				       (subseq line (+ colon 2)
					       (1- (length line)))))))))
	      (let ((content-length (assoc "Content-Length" (headers)
					   :test #'string=))
		    (content-type (assoc "Content-Type" (headers)
					 :test #'string=)))
		(when (and (cdr content-type)
			   (string= (string-downcase (cdr content-type))
				    "application/x-www-form-urlencoded")
			   (cdr content-length))
		  (let ((length (parse-integer (cdr content-length))))
		    (when length
		      (let* ((buffer (make-string length))
			     (count (lisp::read-n-bytes
				     stream buffer 0 length)))
			(when count
			  (log-service "http" " <= ~A~%"
				       (subseq buffer 0 count))
			  (setq *args*
				(append (parse-query-string
					 (subseq buffer 0 count))
					*args*))))))))
	      (let ((*headers* (headers)))
		(serve-http-entity stream name)))))
	 (t
	  (format stream
		  "400 Error: failed to parse request: \"~A\".~%"
		  line)))
	;; FIX Sleeping between close prevents connection broken errs in
	;; browsers.  Err prbly due to removing fd from handlers in
	;; end-session.  Why?
	(unix:unix-close socket)
	(end-session '*http-service-sessions* socket)
	(sleep 0.1)))))

(setf (search-list "http:") "/var/www/")

(defservice "http" 80 #'serve-http)


;;;; SMTP service.

(defstruct (smtp-session-info)
  mail-from
  rcpt-to
  state
  data)

(defvar smtp-cache-directory "/var/cache/ni/smtp/")

(defvar *smtp-next*
  (from-file (in (merge-pathnames "next" smtp-cache-directory))
    (read in)))

(defun serve-smtp (socket)
  (handler-bind ((error #'(lambda (condition)
			    (log-service "smtp" "Error: ~A~%" condition)
			    (unix:unix-close socket)
			    (end-session '*smtp-service-sessions*
					 socket)
			    ;; FIX Sleeping between close prevents
			    ;; connection broken errs in browsers.  Err
			    ;; prbly due to removing fd from handlers in
			    ;; end-session.  Why?
			    (sleep 0.1)
			    (return-from serve-smtp))))
    (let ((stream (session-stream *smtp-service-sessions* socket)))
      ;; FIX if listen is () close session
      (when (listen stream)
	;; FIX will this read block if something else reads the data here?
	(let ((line (read-line stream ()))
	      (extra (or (session-extra *smtp-service-sessions* socket)
			 (setf (session-extra *smtp-service-sessions* socket)
			       (make-smtp-session-info)))))
	  (log-service "smtp" " <- ~A~%" line)
	  (cond
	   ((and line (eq (smtp-session-info-state extra) :data))
	    (fi (string= (string-trim '(#\newline #\return) line) ".")
		(push line (smtp-session-info-data extra))
		(let* ((address (string-trim
				 '(#\< #\>)
				 (smtp-session-info-rcpt-to extra)))
		       (addr (split address #\@)))
		  ;(assert (and addr (< (length addr) 3))
		  (catch 'done
		    (if (eq (length addr) 2)
			(or (member (cadr addr)
				    '("localhost" "127.0.0.1")
				    :test #'string=)
			    (progn
			      (format stream "400 Local delivery only.~%")
			      (throw 'done ()))))
		    (let* ((drop-pathname (merge-pathnames
					   (car addr)
					   "/var/spool/mail/"))
			   (lock (concatenate 'simple-string
					      (namestring drop-pathname)
					      ".lock")))
		      ;; Lock the location, at least from other Nightshades.
		      (system:block-interrupts
		       (if (probe-file lock)
			   (progn
			     ;; FIX queue
			     (format stream "400 Mail drop locked.~%")
			     (throw 'done ()))
			   (with-open-file
			       (lock-stream lock :if-exists :error
					    :if-does-not-exist :create))))
		      (unwind-protect
			  (let ((exists (probe-file drop-pathname)))
			    (with-open-file
				(out drop-pathname
				     :direction :output
				     :if-does-not-exist :create
				     :if-exists :append)
			      (if exists
				  (or (zerop (file-size drop-pathname))
				      (terpri out)))
			      (format out "From ~A~%" address)
			      (format out "Date: ~A~%"
				      (format-time () :style :rfc1123))
			      (dolist (line (nreverse
					     (smtp-session-info-data extra)))
				(write-line (string-trim '(#\return) line)
					    out)))
			    (format stream "200 Message sent.~%"))
			(delete-file lock))))
		  #|
		  (to-file (out (merge-pathnames (string *smtp-next*)
						 smtp-cache-directory)
				:if-exists :error
				:if-does-not-exist :create)
		    (format out "~A~%~A"
			    (get-universal-time)
			    (smtp-session-info-data extra)))
		  |#
		  (setf (smtp-session-info-mail-from extra) ())
		  (setf (smtp-session-info-rcpt-to extra) ())
		  (setf (smtp-session-info-data extra) ())
		  (setf (smtp-session-info-state extra) ()))))
	   ((and line
		 (> (length line) 3)
		 (string= (string-upcase line :end 4) "EHLO" :end1 4))
	    (let* ((rest (string-trim '(#\space #\tab
						#\newline #\return)
				      (subseq line 4))))
	      (format stream "250 EHLO ~A~%" rest)))
	   ((and line
		 (> (length line) 3)
		 (string= (string-upcase line :end 4) "HELO" :end1 4))
	    (let* ((rest (string-trim '(#\space #\tab
						#\newline #\return)
				      (subseq line 4))))
	      (format stream "250 HELO ~A~%" rest)))
	   ((and line
		 (> (length line) 3)
		 (string= (string-upcase line :end 4) "QUIT" :end1 4))
	    (format stream "250 BYE~%")
	    ;; FIX Sleeping between close prevents connection broken errs in
	    ;; browsers.  Err prbly due to removing fd from handlers in
	    ;; end-session.  Why?
	    (unix:unix-close socket)
	    (end-session '*smtp-service-sessions* socket)
	    (sleep 0.1))
	   ((and line
		 (> (length line) 10)
		 (string= (string-upcase line :end 10) "MAIL FROM:" :end1 10))
	    (let* ((rest (string-trim '(#\space #\tab #\newline #\return)
				      (subseq line 10))))
	      (setf (smtp-session-info-mail-from extra) rest)
	      (format stream "250 MAIL FROM OK~%")))
	   ((and line
		 (> (length line) 8)
		 (string= (string-upcase line :end 8) "RCPT TO:" :end1 8))
	    (let* ((rest (string-trim '(#\space #\tab #\newline #\return)
				      (subseq line 8))))
	      (setf (smtp-session-info-rcpt-to extra) rest)
	      (format stream "250 RCPT TO OK~%")))
	   ((and line
		 (> (length line) 4)
		 (string= (string-upcase line :end 4) "DATA" :end1 4))
	    (fi (smtp-session-info-mail-from extra)
		(format stream "400 Error: send MAIL FROM first.~%")
		(fi (smtp-session-info-rcpt-to extra)
		    (format stream "400 Error: send RCPT TO first.~%")
		    (progn
		      (setf (smtp-session-info-state extra) :data)
		      (format stream
			      "354 OK Send message data followed by '.'~%")))))
	   (t
	    (format stream
		    "400 Error: failed to parse request: \"~A\".~%"
		    line))))))))

(defun accept-smtp (stream)
  (format stream "220 SMTP service on ~A~%" (machine-instance)))

(defservice "smtp" 25 #'serve-smtp #'accept-smtp)


;;;; More service support.
;;;
;;; Service support functions that must follow the service definitions
;;; above so that the support functions work the previous way when
;;; redefining services above.

(defun stop-service (name)
  "Stop service $name."
  (let ((service (getstring name *services*)))
    (when service
      (prog1 (funcall (service-stopper service))
	(log-service name "stop~%")))))

(defun probe-service (name)
  "Return true if service $name is running."
  (symbol-value (intern (format () "*~A-SERVICE*"
				(string-upcase name)))))
