;;; This file implements the reading of bulletin boards (that is,
;;; newsgroups) via a known NNTP server.  FIX Something should probably be
;;; done so that when the server is down the editor doesn't hang as I
;;; suspect it will.
;;;
;;; FIX
;;; Warning:    Throughout this file, it may appear I should have bound
;;;             the nn-info-stream and nn-info-header-stream slots instead
;;;             of making multiple structure accesses.  This was done on
;;;             purpose because we don't find out if NNTP timed us out until
;;;             we make an attempt to execute another command.  This code
;;;             recovers by resetting the header-stream and stream slots in
;;;             the nn-info structure to new streams.  If the structure
;;;             access were not made again and NNTP had timed us out, we
;;;             would be making requests on a defunct stream.

(in-package "ED")

; FIX mv nntp interface out to code:, add login,passwd

#[ Netnews

[ Introduction to Editor Netnews ]
[ Setting Up Netnews             ]
[ Starting Netnews               ]
[ Reading Messages               ]
[ Replying to Messages           ]
[ Posting Messages               ]
[ Netnews Bindings Wallchart     ]
]#

#[ Introduction to Editor Netnews

The editor provides a facility for reading bulletin boards through the
NetNews Transfer Protocol (NNTP).  You can easily read Netnews, reply to
news posts, post messages, etc.  The news reading interface is consistent
with that of the the editor mailer, and most Netnews commands function in the
same manner as their mailer counterparts.

Netnews can be read in one of two different modes.

The first mode, invoked by the `Netnews' command, allows the user to read
new messages in groups which the user has specified.  This method of
reading netnews will track the highest numbered message in each newsgroup
and only show new messages which have arrived since then.

The `Netnews Browse' command invokes the other method of reading netnews.
This mode displays a list of all newsgroups, and the user may choose to
read messages in any of them.  By default, the news reader will not track
the latest message read when browsing, and it will always display the last
few messages.
]#

#[ Setting Up Netnews

To start reading bulletin boards from the editor you probably need to create a
file containing the newsgroups you want to read.

{evariable:Netnews Group File}
{evariable:Netnews Database File}

{command:List All Groups}

{evariable:Netnews NNTP Server}
{evariable:Netnews NNTP Timeout Period}

[ News Browse Mode ]
]#


(defvar *netnews-mess* ())
(defvar *netnews-mess-level* 0)

(defun nn-incf-mess () (incf *netnews-mess-level* 2))
(defun nn-decf-mess () (decf *netnews-mess-level* 2))

(defun nn-mess (format &rest args)
  (when *netnews-mess*
    (funcall *netnews-mess* "~A~A"
	     (make-string *netnews-mess-level* :initial-element #\space)
	     (apply #'funcall #'format () format args))))


;;;; Highlighting.

(defun highlight-news-headers-line (line chi-info)
  (let ((line-length (line-length line)))
    (when (> line-length 13)
      (chi-mark line 14 *special-form-font* :special-form chi-info)
      (if (> line-length 35)
	  (chi-mark line 36 *string-font* :string chi-info)))))

(defun highlight-news-headers-buffer (buffer)
  (highlight-chi-buffer buffer highlight-news-headers-line))

(defun highlight-visible-news-headers-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-news-headers-line))


;;;; Netnews data structures.

(defparameter default-netnews-headers-length 1000
  "How long the header-cache and message-ids arrays should be made on
   startup.")

(defstruct (netnews-info
	    (:conc-name nn-info-)
	    (:print-function
	     (lambda (nn s d)
	       (declare (ignore nn d))
	       (write-string "#<Netnews Info>" s))))
  (updatep (ext:required-argument) :type (or null t))
  (from-end-p () :type (or null t))
  ;;
  ;; The string name of the current group.
  (current (ext:required-argument) :type simple-string)
  ;;
  ;; The number of the latest message read in the current group.
  (latest () :type (or null fixnum))
  ;;
  ;; The cache of header info for the current group.  Each element contains
  ;; an association list of header fields to contents of those fields.  Indexed
  ;; by id offset by the first message in the group.
  (header-cache () :type (or null simple-vector))
  ;;
  ;; The number of HEAD requests currently waiting on the header stream.
  (batch-count () :type (or null fixnum))
  ;;
  ;; The list of newsgroups to read.
  (groups (ext:required-argument) :type cons)
  ;;
  ;; A vector of message ids indexed by buffer-line for this headers buffer.
  (message-ids () :type (or null vector))
  ;;
  ;; Where to insert the next batch of headers.
  mark
  ;;
  ;; The message buffer used to view article bodies.
  buffer
  ;;
  ;; A list of message buffers that have been marked as undeletable by the user.
  (other-buffers () :type (or null cons))
  ;;
  ;; The window used to display buffer when *Netnews Read Style* is :multiple.
  message-window
  ;;
  ;; The window used to display headers when *Netnews Read Style* is
  ;; :multiple.
  headers-window
  ;;
  ;; How long the message-ids and header-cache arrays are.  Reuse this array,
  ;; but don't break if there are more messages than we can handle. FIX break what?
  (array-length default-netnews-headers-length :type fixnum)
  ;;
  ;; The id of the first message in the current group.
  (first () :type (or null fixnum))
  ;;
  ;; The id of the last message in the current-group.
  (last () :type (or null fixnum))
  ;;
  ;; Article number of the first visible header.
  (first-visible () :type (or null fixnum))
  ;;
  ;; Article number of the last visible header.
  (last-visible () :type (or null fixnum))
  ;;
  ;; Number of the message that is currently displayed in buffer.  Initialize
  ;; to -1 so I don't have to constantly check for the nullness of it.
  (current-displayed-message -1 :type (or null fixnum))
  ;;
  ;; T if the last batch of headers is waiting on the header stream.
  ;; This is needed so NN-WRITE-HEADERS-TO-MARK can set the messages-waiting
  ;; slot to ().
  (last-batch-p () :type (or null t))
  ;;
  ;; T if there are more headers in the current group.  () otherwise.
  (messages-waiting () :type (or null t))
  ;;
  ;; The stream on which we request headers from NNTP.
  header-stream
  ;;
  ;; The stream on which we request everything but headers from NNTP.
  stream)

(defun setup-news-headers-mode (buffer)
  (highlight-visible-news-headers-buffer buffer)
  (pushnew '("News Headers" t highlight-visible-news-headers-buffer)
	   *mode-highlighters*))

(defmode "News Headers" :major-p t
  :setup-function #'setup-news-headers-mode
  :short-name "News-Headers")


;;;; The netnews-message-info and post-info structures.

(defstruct (netnews-message-info
	    (:conc-name nm-info-)
	    (:print-function
	     (lambda (nn s d)
	       (declare (ignore nn d))
	       (write-string "#<Netnews Message Info>" s))))
  ;; The headers buffer (if there is one) associated with this message buffer.
  headers-buffer
  ;; The draft buffer (if there is one) associated with this message buffer.
  draft-buffer
  ;; The post buffer (if there is one) associated with this message buffer.
  post-buffer
  ;; This is need because we want to display what message this is in the
  ;; modeline field of a message buffer.
  (message-number () :type (or null fixnum))
  ;;  Set to T when we do not want to reuse this buffer.
  keep-p)

(defstruct (post-info
	    (:print-function
	     (lambda (nn s d)
	       (declare (ignore nn d))
	       (write-string "#<Post Info>" s))))
  ;; The NNTP stream over which to send this post.
  stream
  ;; When replying in another window, the reply window.
  reply-window
  ;; When replying in another window, the message window.
  message-window
  ;; The message buffer associated with this post.
  message-buffer
  ;; The Headers buffer associated with this post.
  headers-buffer)


;;;; Command Level Implementation of "News Headers" mode.

#[ Starting Netnews

Once your `Netnews Group File' is set up, you may begin reading netnews.
`Netnews' is the main entry point for reading bulletin boards.

{command:Netnews}

A pointer to the last message read in each of group is kept in `Netnews
Database File'.  Newsgroups can be added to the `Netnews Group File'
manually or by using the `Netnews Browse' facility.

{command:Netnews Look at Newsgroup}

Reading a group creates a buffer listing header information for each
message in the group.  There are four fields in each header: the date the
message was sent, the number of lines in the message, the address from
which the message was sent and the message subject.  If a field for a
message is not available, NA will appear instead.  The length of each of
these fields is determined by the following the editor variables:

{evariable:Netnews Before Date Field Pad}
{evariable:Netnews Date Field Length}
{evariable:Netnews Line Field Length}
{evariable:Netnews From Field Length}
{evariable:Netnews Subject Field Length}
{evariable:Netnews Field Padding}

For increased speed, the editor only inserts headers for a subset of the
messages in each group.  If you have never read a certain group, and the
value of `Netnews New Group Style' is :from-end (the default), the editor
inserts some number of the last messages in the group, determined by the
value of `Netnews Batch Count'.  If the value of `Netnews New Group Style'
is :from-start, the editor will insert the first batch of messages in the
group.  If you have read a group before, the editor will insert the batch
of messages following the highest numbered message that you had read
previously.

{evariable:Netnews Start Over Threshold}

You may at any time go beyond the messages that are visible using the
`Netnews Next Line', `Netnews Previous Line',
`Netnews Headers Scroll Window Up', and
`Netnews Headers Scroll Down' commands in `News-Headers' mode,
or the `Netnews Next Article' and `Netnews Previous Article'
commands in `News-Message' mode.

{evariable:Netnews Fetch All Headers}
{evariable:Netnews Batch Count}
{evariable:Netnews New Group Style}
]#

(defevar "Netnews Database File"
  "The `Netnews' command merges this value with the home: directory.  This
   file maintains a pointer to the highest numbered message read in each
   group in *Netnews Group File*."
  :value (config:config-pathname "netnews"))

(defevar "Netnews Read Style"
  "How you like to read netnews.  A value of :single will cause netnews
   mode to use a single window for headers and messages, and a value of
   :multiple will cause the current window to be split so that Headers take
   up *Netnews Headers Proportion* of what was the current window, and a
   message bodies buffer the remaining portion.  Changing the value of this
   variable dynamically affects netnews reading."
  :value :multiple)

(or (modeline-field :netnews-message)
    (make-modeline-field
     :name :netnews-message
     :width 14
     :function #'(lambda (buffer window)
		   (declare (ignore window))
		   (let* ((nm-info (variable-value 'netnews-message-info
						   :buffer buffer))
			  (nn-info (variable-value 'netnews-info
						   :buffer (nm-info-headers-buffer
							    nm-info))))
		     (format () "~A of ~A"
			     (nm-info-message-number nm-info)
			     (1+ (- (nn-info-last nn-info)
				    (nn-info-first nn-info))))))))

(or (modeline-field :netnews-header-info)
    (make-modeline-field
     :name :netnews-header-info
     :width 24
     :function
     #'(lambda (buffer window)
	 (declare (ignore window))
	 (let ((nn-info (variable-value 'netnews-info :buffer buffer)))
	   (format () "~A before, ~A after"
		   (- (nn-info-first-visible nn-info) (nn-info-first nn-info))
		   (- (nn-info-last nn-info) (nn-info-last-visible nn-info)))))))

(defvar *nn-headers-buffer* ()
  "If `Netnews' was invoked without an argument an not exited, this holds
   the headers buffer for reading netnews.")

(defvar *netnews-kill-strings* ())

(defevar "Netnews Kill File"
  "This value is merged with your home directory to get the pathname of
   your netnews kill file.  If any of the strings in this file (one per
   line) appear in a subject header while reading netnews, they will have a
   \"K\" in front of them, and `Netnews Next Line' and `Netnews Previous
   Line' will never land you on one.  Use `Next Line' and `Previous Line'
   to read killed messages."
  :value (config:config-pathname "netnews-kill"))

(defevar "Netnews New Group Style"
  "Control of the behaviour when reading a group for the first time:

     :from-start
         read from the beginning of the new group forward

     :from-end
         read the group from the end backward.

     a number
         read that many messages from the end of the group and read forward
         from there."
  :value :from-end)

(defevar "Netnews Start Over Threshold"
  "If the number of new messages in a group exceeds the value of this
   variable and `Netnews New Group Style' is :from-end, offer to continue
   reading the group from the end."
  :value 300)

(defcommand "Netnews" (p group-name from-end-p browse-buf (updatep t))
  "Read each newsgroup in the file named in *Netnews Group File*.  With an
   argument, prompt for the name of a newsgroup and read it."
  (cond
   ((and *nn-headers-buffer* (not p) (not group-name))
    (change-to-buffer *nn-headers-buffer*))
   (t
    (let* ((single-group (if p (prompt-for-string :prompt "Group to read: "
						  :help "Type the name of ~
						  the group you want ~
						  to scan."
						  :trim t)))
	   (groups (cond
		    (group-name (list group-name))
		    (single-group (list single-group))
		    (t
		     (let ((group-file (merge-pathnames
					(value netnews-group-file)
					(user-homedir-pathname))))
		       (when (probe-file group-file)
			 (let ((res ()))
			   (with-open-file (s group-file :direction :input)
			     (loop
			       (let ((group (read-line s () ())))
				 (or group (return (nreverse res)))
				 (pushnew group res)))))))))))
      (or p groups
	  (editor-error "At least one group is required.  Add groups to
			 *Netnews Group File* directly or with `Netnews
			 Browse'."))
      (if updatep (nn-assure-database-exists))
      (nn-parse-kill-file)
      (multiple-value-bind (stream header-stream) (streams-for-nntp)
	(multiple-value-bind
	    (buffer-name clashp)
	    (nn-unique-headers-name (car groups))
	  (if (and (or p group-name) clashp)
	      (change-to-buffer (getstring clashp *buffer-names*))
	      (let* ((buffer (make-buffer
			      buffer-name
			      :modes '("News Headers")
			      :modeline-fields
			      (append (value default-modeline-fields)
				      (list (modeline-field
					     :netnews-header-info)))
			      :delete-hook
			      (list #'netnews-headers-delete-hook)))
		     (nn-info (make-netnews-info
			       :current (car groups)
			       :groups groups
			       :updatep updatep
			       :headers-window (current-window)
			       :mark (copy-mark (buffer-point buffer))
			       :header-stream header-stream
			       :stream stream)))
		(or p group-name
		    (setf *nn-headers-buffer* buffer))
		(and clashp
		     (fi (or p group-name)
			 (message "Buffer ~S also contains headers for ~A"
				  clashp (car groups))))
		(defevar "Netnews Info"
		  "A structure containing the current group, a list of
		   groups, a book-keeping mark, a stream we get headers on,
		   and the stream on which we request articles."
		  :buffer buffer
		  :value nn-info)
		(setf (buffer-writable buffer) ())
		(defevar "Netnews Browse Buffer"
		  "This variable is the associated \"News Browse\" buffer
		   in a \"News-Headers\" buffer created from
		   \"News Browse\" mode."
		  :buffer buffer
		  :value browse-buf)
		(setup-group (car groups) nn-info buffer from-end-p)))))))))

(defun nn-parse-kill-file ()
  (let ((filename (merge-pathnames (value netnews-kill-file)
				   (user-homedir-pathname))))
    (when (probe-file filename)
      (with-open-file (s filename :direction :input)
	(loop
	  (let ((kill-string (read-line s () ())))
	    (or kill-string (return))
	    (pushnew kill-string *netnews-kill-strings*)))))))

;;; NETNEWS-HEADERS-DELETE-HOOK closes the stream slots in netnews-info,
;;; deletes the bookkeeping mark into buffer, sets the headers slots of any
;;; associated post-info or netnews-message-info structures to () so
;;; "Netnews Go To Headers Buffer" will not land you in a buffer that does
;;; not exist, and sets *nn-headers-buffer* to () so next time we invoke
;;; "Netnews" it will start over.
;;;
(defun netnews-headers-delete-hook (buffer)
  (let ((nn-info (variable-value 'netnews-info :buffer buffer)))
    ;; Disassociate all message buffers.
    ;;
    (dolist (buf (nn-info-other-buffers nn-info))
      (setf (nm-info-headers-buffer (variable-value 'netnews-message-info
						    :buffer buf))
	    ()))
    (let ((message-buffer (nn-info-buffer nn-info)))
      (when message-buffer
	(setf (nm-info-headers-buffer (variable-value 'netnews-message-info
						      :buffer message-buffer))
	      ())))
    (close (nn-info-stream nn-info))
    (close (nn-info-header-stream nn-info))
    (delete-mark (nn-info-mark nn-info))
    (when (eq *nn-headers-buffer* buffer)
      (setf *nn-headers-buffer* ()))))

(defun nn-unique-headers-name (group-name)
  (let ((original-name (concatenate 'simple-string "Netnews " group-name)))
    (if (getstring original-name *buffer-names*)
	(let ((name ())
	      (number 0))
	  (loop
	    (setf name (format () "Netnews ~A ~D" group-name (incf number)))
	    (or (getstring name *buffer-names*)
		(return (values name original-name)))))
	(values original-name ()))))

;;; NN-ASSURE-DATABASE-EXISTS does just that.  If the file determined by the
;;; value of "Netnews Database Filename" does not exist, then it gets
;;; created.
;;;
(defun nn-assure-database-exists ()
  (let ((filename (merge-pathnames (value netnews-database-file)
				   (user-homedir-pathname))))
    (unless (probe-file filename)
      (message "Creating netnews database file.")
      (close (open filename :direction :output :if-does-not-exist :create)))))

(defevar "Netnews Fetch All Headers"
  "When true the netnews reading commands fetch all headers at once,
   otherwise they fetch the headers in batches, for increased efficiency.
   Fetching all headers can take a long time if there are many headers.")

(defcommand "Netnews Look at Newsgroup" (p)
  "Prompt for the name of a newsgroup and read it, regardless of what is in
   FIX, leaving the *Netnews Database File* as it is."
  (declare (ignore p))
  (netnews-command () (prompt-for-string :prompt "Group to look at: "
					  :help "Type the name of ~
					  the group you want ~
					  to look at."
					  :trim t)
		   () () ()))

;;; SETUP-GROUP is the guts of this group reader.  It sets up a headers
;;; buffer in buffer for group group-name.  This consists of sending a group
;;; command to both the header-stream and normal stream and then getting the
;;; last message read in group-name from the database file and setting the
;;; appropriate slots in the nn-info structure.  The first batch of messages
;;; is then requested and inserted, and room for message-ids is allocated.
;;;
(defun setup-group (group-name nn-info buffer &optional from-end-p)
  ;; Do not bind stream or header-stream because if a timeout has occurred
  ;; before these calls are invoked, they would be bogus.
  ;;
  (nntp-group group-name (nn-info-stream nn-info)
	      (nn-info-header-stream nn-info))
  (let ((response (process-status-response (nn-info-header-stream nn-info)
					   nn-info)))
    ; FIX a guess 2009-04-20
    (process-status-response (nn-info-stream nn-info) nn-info)
    (cond ((not response)
	   (message "~A is not the name of a netnews group.~%"
		    (nn-info-current nn-info))
	   (change-to-next-group nn-info buffer))
	  (t
	   (multiple-value-bind (number first last)
				(group-response-args response)
	     (declare (ignore first))
	     (message "Setting up ~A" group-name)
	     ;; If nn-info-updatep is (), then we fool ourselves into
	     ;; thinking we've never read this group before by making
	     ;; last-read ().  We determine first here because the first
	     ;; that NNTP gives us is way way out of line.
	     ;;
	     (let ((last-read (if (nn-info-updatep nn-info)
				  (nn-last-read-message-number group-name)))
		   (first (1+ (- last number))))
	       ;; Make sure there is at least one new message in this group.
	       (cond
		((and last-read (= last-read last))
		 (message "No new messages in ~A" group-name)
		 (setf (nn-info-latest nn-info) last)
		 (change-to-next-group nn-info buffer))
		((zerop number)
		 (message "No messages AVAILABLE in ~A" group-name)
		 (setf (nn-info-latest nn-info) last)
		 (change-to-next-group nn-info buffer))
		(t
		 (let ((latest (if (and last-read
					(> last-read first)
					;; 2008-04-23 I think this can
					;; happen if a long time passes
					;; between reads.
					(<= last-read last))
				   last-read
				   first)))
		   (if (or (and (eq (value netnews-new-group-style)
				    :from-end)
				(or (= latest first)
				    (and (> (- last latest)
					    (value
					     netnews-start-over-threshold))
					 (prompt-for-y-or-n
					  :prompt
					  `("There are ~D new messages.  ~
					     Read from the end of this ~
					     group? " ,(- last latest))
					  :default "Y"
					  :default-string "Y"
					  :help "Y starts reading from the ~
					         end.  N starts reading where ~
						 you left off many messages ~
						 back."))))
			   from-end-p)
		       (setf (nn-info-from-end-p nn-info) t))

		   (cond ((nn-info-from-end-p nn-info)
			  (setf (nn-info-first-visible nn-info) ())
			  (setf (nn-info-last-visible nn-info) last))
			 (t
			  ; (setf (nn-info-first-visible nn-info) latest)
			  (setf (nn-info-first-visible nn-info) (1+ latest))
			  (setf (nn-info-last-visible nn-info) ())))
		   (setf (nn-info-first nn-info) first)
		   (setf (nn-info-last nn-info) last)
		   (setf (nn-info-latest nn-info) latest))
		 ;;
		 ;; Request the batch before setting message-ids so they start
		 ;; coming before we need them.
		 (nn-request-next-batch nn-info
					(value netnews-fetch-all-headers))
		 (let ((message-ids (nn-info-message-ids nn-info))
		       (header-cache (nn-info-header-cache nn-info))
		       (length (1+ (- last first))))
		   (multiple-value-setq
		       (message-ids header-cache)
		       (cond ((> length (nn-info-array-length nn-info))
			      (setf (nn-info-array-length nn-info) length)
			      (values (make-array length :fill-pointer 0)
				      (make-array length
						  :initial-element ())))
			     (message-ids
			      (setf (fill-pointer message-ids) 0)
			      (values message-ids header-cache))
			     (t
			      (values (make-array (nn-info-array-length nn-info)
						  :fill-pointer 0)
				      (make-array (nn-info-array-length nn-info)
						  :initial-element ())))))
		   (setf (nn-info-message-ids nn-info) message-ids)
		   (setf (nn-info-header-cache nn-info) header-cache))
		 (nn-write-headers-to-mark nn-info buffer)
		 (change-to-buffer buffer)))))))))

;;; NN-LAST-READ-MESSAGE-NUMBER reads the last read message in group-name
;;; from the value of "Netnews Database File".  It is SETF'able and the
;;; SETF method is %SET-LAST-READ-MESSAGE-NUMBER.
;;;
(defun nn-last-read-message-number (group-name)
  (with-open-file (s (merge-pathnames (value netnews-database-file)
				      (user-homedir-pathname))
		     :direction :input :if-does-not-exist :error)
    (loop
      (let ((read-group-name (read-line s () ())))
	(or read-group-name (return ()))
	(when (string-equal read-group-name group-name)
	  (let ((last-read (read-line s () ())))
	    (if last-read
		(return (parse-integer last-read))
		(error "Should have been a message number ~
			following ~S in database file."
		       group-name))))))))

(defun %set-nn-last-read-message-number (group-name new-value)
  (with-open-file (s (merge-pathnames (value netnews-database-file)
				      (user-homedir-pathname))
		     :direction :io :if-does-not-exist :error
		     :if-exists :overwrite)
    (unless (loop
	      (let ((read-group-name (read-line s () ())))
		(or read-group-name (return ()))
		(when (string-equal read-group-name group-name)
		  ;; File descriptor streams do not do the right thing with
		  ;; :io/:overwrite streams, so work around it by setting it
		  ;; explicitly.
		  ;;
		  (file-position s (file-position s))
		  ;; Justify the number so that if the number of digits in it
		  ;; changes, we won't overwrite the next group name.
		  ;;
		  (format s "~14D~%" new-value)
		  (return t))))
      (write-line group-name s)
      (format s "~14D~%" new-value))))

(defsetf nn-last-read-message-number %set-nn-last-read-message-number)

(defconstant nntp-eof "."
  "NNTP marks the end of a textual response with this.  NNTP also recognizes
   this as the end of a post.")

;;; This macro binds a variable to each successive line of input from NNTP
;;; and exits when it sees the NNTP end-of-file-marker, a period by itself on
;;; a line.
;;;
(defmacro with-input-from-nntp ((var stream) &body body)
  "Body is executed with var bound to successive lines of input from nntp.
   Exits at the end of a response, returning whatever the last execution of
   Body returns, or () if there was no input.

   Take note: this is only to be used for textual responses.  Status responses
   are of an entirely different nature."
  (let ((return-value (gensym)))
    `(let ((,return-value ())
	   (,var ""))
       (declare (simple-string ,var))
       (loop
	 (setf ,var (read-line ,stream))
	 (when (string= ,var nntp-eof) (return ,return-value))
	 (setf ,return-value (progn ,@body))))))

;;; Writing the date, from, and subject fields to a mark.

(defevar "Netnews Before Date Field Pad"
  "The number of spaces inserted before the date in Netnews."
  :value 1)

(defevar "Netnews Date Field Length"
  "The length of the date field in \"News-Headers\" buffers."
  :value 6)

(defevar "Netnews Line Field Length"
  "The length of the line field in \"News-Headers\" buffers."
  :value 3)

(defevar "Netnews From Field Length"
  "The length of the from field in \"News-Headers\" buffers."
  :value 20)

(defevar "Netnews Subject Field Length"
  "The length of the subject field in \"News-Headers\" buffers."
  :value 43)

(defevar "Netnews Field Padding"
  "The number of spaces left between the netnews date, from, lines, and
   subject fields."
  :value 2)

;;;
(defconstant netnews-space-string
  (make-string 70 :initial-element #\space))
;;;
(defconstant missing-message (cons () ())
  "Use this as a marker so `nn-write-headers-to-mark' doesn't try to insert
   a message that is not really there.")

;;; NN-CACHE-HEADER-INFO stashes all header information into an array for
;;; later use.
;;;
(defun nn-cache-header-info (nn-info howmany use-header-stream-p)
  (let* ((cache (nn-info-header-cache nn-info))
	 (message-ids (nn-info-message-ids nn-info))
	 (stream (if use-header-stream-p
		     (nn-info-header-stream nn-info)
		     (nn-info-stream nn-info)))
	 (from-end-p (nn-info-from-end-p nn-info))
	 (old-count 0))
    (declare (fixnum old-count))
    (when from-end-p
      (setf old-count (length message-ids))
      (do ((i (length message-ids) (1- i)))
	  ((minusp i) ())
	(setf (aref message-ids (+ i howmany)) (aref message-ids i)))
      (setf (fill-pointer message-ids) 0))
    (let ((missing-message-count 0)
	  (offset (nn-info-first nn-info)))
      (dotimes (i howmany)
	(let ((response (process-status-response stream)))
	  (nn-mess "response: ~A" response)
	  (if response
	      (let* ((id (head-response-args response))
		     (index (- id offset)))
		(nn-mess "id: ~A" id)
		(nn-mess "offset: ~A" id)
		(vector-push id message-ids)
		(setf (svref cache index) ())
		(with-input-from-nntp (string stream)
				      (let ((colonpos (position #\: string)))
					(when colonpos
					  (push (cons (subseq string 0 colonpos)
						      (subseq string
							      (+ colonpos 2)))
						(svref cache index))))))
	      (incf missing-message-count))))
      (when from-end-p
	(when (plusp missing-message-count)
	  (dotimes (i old-count)
	    (setf (aref message-ids (- (+ i howmany) missing-message-count))
		  (aref message-ids (+ i howmany)))))
	(setf (fill-pointer message-ids)
	      (- (+ old-count howmany) missing-message-count))))))

(defconstant netnews-field-na "NA"
  "This string gets inserted when NNTP doesn't find a field.")

(defconstant netnews-field-na-length (length netnews-field-na)
  "The length of netnews-field-na")

(defun nn-write-headers-to-mark (nn-info buffer &optional fetch-rest-p
					 out-of-order-p)
  (let* ((howmany (nn-info-batch-count nn-info))
	 (from-end-p (nn-info-from-end-p nn-info))
	 (cache (nn-info-header-cache nn-info))
	 (old-point (copy-mark (buffer-point buffer) (if from-end-p
							 :left-inserting
							 :right-inserting)))
	 (messages-waiting (nn-info-messages-waiting nn-info))
	 (mark (nn-info-mark nn-info)))
    (or messages-waiting
	(return-from nn-write-headers-to-mark ()))
    (if from-end-p
	(buffer-start mark)
	(buffer-end mark))
    (nn-cache-header-info nn-info howmany (fi out-of-order-p))
    (with-writable-buffer (buffer)
      (with-mark ((check-point mark :right-inserting))
	(macrolet ((mark-to-pos (mark pos)
		     `(insert-string ,mark netnews-space-string
				     0 (- ,pos (mark-column ,mark))))
		   (insert-field (mark field-string field-length)
		     `(if ,field-string
			  (insert-string ,mark ,field-string
					 0 (min ,field-length
						(1- (length ,field-string))))
			  (insert-string ,mark netnews-field-na
					 0 (min ,field-length
						netnews-field-na-length)))))
	  (let* ((line-start (+ (value netnews-before-date-field-pad)
				(value netnews-date-field-length)
				(value netnews-field-padding)))
		 (from-start (+ line-start
				(value netnews-line-field-length)
				(value netnews-field-padding)))
		 (subject-start (+ from-start
				   (value netnews-from-field-length)
				   (value netnews-field-padding)))
		 (start (- messages-waiting (nn-info-first nn-info)))
		 (end (1- (+ start howmany))))
	    (do ((i start (1+ i)))
		((> i end))
	      (let ((assoc-list (svref cache i)))
		(unless (null assoc-list)
		  (insert-string mark netnews-space-string
				 0 (value netnews-before-date-field-pad))
		  (let* ((date-field (cdr (assoc "date" assoc-list
						 :test #'string-equal)))
			 (universal-date (if date-field
					     (ext:parse-time date-field
							     :end (1- (length date-field))))))
		    (insert-field
		     mark
		     (if universal-date
			 (string-capitalize
			  (format-universal-time () universal-date
						 :style :government
						 :print-weekday ()))
			 date-field)
		     (value netnews-date-field-length)))
		  (mark-to-pos mark line-start)
		  (insert-field mark (cdr (assoc "lines" assoc-list
						 :test #'string-equal))
				(value netnews-line-field-length))
		  (mark-to-pos mark from-start)
		  (insert-field mark (cdr (assoc "from" assoc-list
						 :test #'string-equal))
				(value netnews-from-field-length))
		  (mark-to-pos mark subject-start)
		  (insert-field mark (cdr (assoc "subject" assoc-list
						 :test #'string-equal))
				(value netnews-subject-field-length))
		  (insert-character mark #\newline))))))
	(cond (out-of-order-p
	       (setf (nn-info-first-visible nn-info) messages-waiting))
	      (t
	       (if (nn-info-from-end-p nn-info)
		   (setf (nn-info-first-visible nn-info) messages-waiting)
		   (setf (nn-info-last-visible nn-info)
			 (1- (+ messages-waiting howmany))))
	       (if (nn-info-last-batch-p nn-info)
		   (setf (nn-info-messages-waiting nn-info) ())
		   (nn-request-next-batch nn-info fetch-rest-p))))
	(if (mark= mark check-point)
	    (message "All messages in last batch were missing, getting more."))
	(move-mark (buffer-point buffer) old-point)
	(delete-mark old-point)))))

;;; NN-MAYBE-GET-MORE-HEADERS gets more headers if the point of the headers
;;; buffer is on an empty line and there are some.  Returns whether it got more
;;; headers, i.e., if it is time to go on to the next group.
;;;
(defun nn-maybe-get-more-headers (nn-info)
  (let ((headers-buffer (line-buffer (mark-line (nn-info-mark nn-info)))))
    (when (empty-line-p (buffer-point headers-buffer))
      (cond ((and (nn-info-messages-waiting nn-info)
		  (not (nn-info-from-end-p nn-info)))
	     (nn-write-headers-to-mark nn-info headers-buffer)
	     t)
	    (t :go-on)))))

(defevar "Netnews Batch Count"
  "The numer of header the Netnews facility fetches at a time."
  :value 50)

;;; NN-REQUEST-NEXT-BATCH requests the next batch of messages in a group.
;;; For safety, don't do anything if there is no next-batch start.
;;;
(defun nn-request-next-batch (nn-info &optional fetch-rest-p)
  (if (nn-info-from-end-p nn-info)
      (nn-request-backward nn-info fetch-rest-p)
      (nn-request-forward nn-info fetch-rest-p)))

(defun nn-request-forward (nn-info fetch-rest-p)
  (let* ((last-visible (nn-info-last-visible nn-info))
	 (last (nn-info-last nn-info))
	 (batch-start (if last-visible
			  (1+ (nn-info-last-visible nn-info))
			  (1+ (nn-info-latest nn-info))))
	 (header-stream (nn-info-header-stream nn-info))
	 (batch-end (if fetch-rest-p
			last
			(1- (+ batch-start (value netnews-batch-count))))))
    ;; If this is the last batch, adjust batch-end appropriately.
    ;;
    (when (>= batch-end last)
      (setf batch-end last)
      (setf (nn-info-last-batch-p nn-info) t))
    (setf (nn-info-batch-count nn-info) (1+ (- batch-end batch-start)))
    (setf (nn-info-messages-waiting nn-info) batch-start)
    (nn-send-many-head-requests header-stream batch-start batch-end ())))

(defun nn-request-backward (nn-info fetch-rest-p
				    &optional (use-header-stream-p t))
  (let* ((first-visible (nn-info-first-visible nn-info))
	 (batch-end (if first-visible
			(1- (nn-info-first-visible nn-info))
			(nn-info-last nn-info)))
	 (stream (if use-header-stream-p
		     (nn-info-header-stream nn-info)
		     (nn-info-stream nn-info)))
	 (first (nn-info-first nn-info))
	 (batch-start (if fetch-rest-p
			  first
			  (1+ (- batch-end (value netnews-batch-count))))))
    ;; If this is the last batch, adjust batch-end appropriately.
    ;;
    (when (<= batch-start first)
      (setf batch-start first)
      (setf (nn-info-last-batch-p nn-info) t))
    (setf (nn-info-batch-count nn-info) (1+ (- batch-end batch-start)))
    (setf (nn-info-messages-waiting nn-info) batch-start)
    (nn-send-many-head-requests stream batch-start batch-end
				(not use-header-stream-p))))

;;; NN-REQUEST-OUT-OF-ORDER is called when the user is reading a group
;;; normally and decides he wants to see some messages before the first one
;;; visible.  To accomplish this without disrupting the normal flow of
;;; things, we fool ourselves into thinking we are reading the group from
;;; the end, remembering several slots that could be modified in requesting
;;; these messages.  When we are done, return state to what it was for
;;; reading a group forward.
;;;
(defun nn-request-out-of-order (nn-info headers-buffer)
  (let ((messages-waiting (nn-info-messages-waiting nn-info))
	(batch-count (nn-info-batch-count nn-info))
	(last-batch-p (nn-info-last-batch-p nn-info)))
    (nn-request-backward nn-info () ())
    (setf (nn-info-from-end-p nn-info) t)
    (nn-write-headers-to-mark nn-info headers-buffer () t)
    (setf (nn-info-messages-waiting nn-info) messages-waiting)
    (setf (nn-info-batch-count nn-info) batch-count)
    (setf (nn-info-last-batch-p nn-info) last-batch-p)
    (setf (nn-info-from-end-p nn-info) ())))

(proclaim '(special *nn-last-command-issued*))

(defun nn-send-many-head-requests (stream first last out-of-order-p)
  (do ((i first (1+ i)))
      ((> i last))
    (nntp-head i stream))
  (setf *nn-last-command-issued*
	(list (if out-of-order-p :out-of-order :header)
	      first last out-of-order-p)))

(defvar nn-minimum-header-batch-count 30
  "The minimum number of headers to fetch at any given time.")


;;;; "News Message" mode.

(defmode "News Message" :major-p t
  :short-name "News-Message"
  :setup-function 'setup-news-message-mode)

(defun setup-news-message-mode (buffer)
  (highlight-visible-message-buffer buffer)
  (pushnew '("News Message" t highlight-visible-message-buffer)
	   *mode-highlighters*))


;;;; Commands for viewing articles.

#[ Reading Messages

A `News-Headers' buffer provides for reading messages, reply to messages
via the editor mailer and replying to messages via post.  There are also
commands that ease getting from one header to another.

{command:Netnews Show Article}
{command:Netnews Show Article in Other Window}
{evariable:Netnews Read Style}
{evariable:Netnews Headers Proportion}
{evariable:Netnews Message Header Fields}
{command:Netnews Show Whole Header}
{command:Netnews Next Line}
{evariable:Netnews Last Header Style}
{command:Netnews Previous Line}
{command:Netnews Headers Scroll Window Down}
{command:Netnews Headers Scroll Window Up}
{command:Netnews Next Article}
{command:Netnews Previous Article}
{command:Netnews Message Scroll Down}
{evariable:Netnews Scroll Show Next Message}
{command:Netnews Message Quit}
{command:Netnews Go To Headers Buffer}
{command:Netnews Message Keep Buffer}
{command:Netnews Select Message Buffer}
{command:Netnews Append to File}
{evariable:Netnews Message File}
{command:Netnews Headers File Message}
{command:Netnews Message File Message}
{command:Fetch All Headers}
{command:Netnews Go To Next Group}
{command:Netnews Quit Starting Here}
{command:Netnews Group Punt Messages}
{command:Netnews Exit}
{evariable:Netnews Exit Confirm}
]#

(defcommand "Netnews Show Article" (p)
  "Show the body of the message header under point in a `News-Message'
   buffer.

   If the value of `Netnews Read Style' is :single, change to the
   `News-Message' buffer.  If it is :multiple, then split the current
   window into two windows, one for headers and one for message bodies.
   The headers window takes up a proportion of the current window based on
   the value of `Netnews Headers Proportion'.  If the window displaying the
   `News-Headers' buffer has already been split, and the message currently
   displayed in the `News-Message' window is the same as the one under the
   current point, then call `Netnews Message Scroll Down'."
  (nn-show-article (value netnews-info) p))

(defcommand "Netnews Show Article in Other Window" (p)
  "Show the body of the message header under point in a `News-Message'
   buffer in the other window.  Split the current window if it the only
   window.

   If the value of `Netnews Read Style' is :single, change to the
   `News-Message' buffer.  If it is :multiple, then split the current
   window into two windows, one for headers and one for message bodies.
   The headers window takes up a proportion of the current window based on
   the value of `Netnews Headers Proportion'.  If the window displaying the
   `News-Headers' buffer has already been split, and the message currently
   displayed in the `News-Message' window is the same as the one under the
   current point, then call `Netnews Message Scroll Down'."
  (let ((netnews-info (value netnews-info))
	(buffer (current-buffer)))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command)
	(next-window-command))
    (setf (current-buffer) buffer)
    (setf (window-buffer (current-window)) buffer)
    (nn-show-article netnews-info p)))

(defcommand "Netnews Next Article" ()
  "Show the next article in the current newsgroup in a message buffer."
  (let* ((what-next (netnews-next-line-command () (nn-get-headers-buffer))))
    (when (and (not (eq what-next :done))
	       (or (eq what-next t)
		   (eq (value netnews-last-header-style) :next-article)))
      ;; Reget the headers buffer because the call to netnews-next-line-command
      ;; might have moved us into a different buffer.
      ;;
      (nn-show-article (variable-value 'netnews-info
				       :buffer (nn-get-headers-buffer))
		       t))))

(defcommand "Netnews Previous Article" ()
  "Show the next article in the current newsgroup in a message buffer."
  (let ((buffer (nn-get-headers-buffer)))
    (netnews-previous-line-command () buffer)
    (nn-show-article (variable-value 'netnews-info :buffer buffer) t)))

;;; NN-SHOW-ARTICLE checks first to see if we need to get more headers.  If
;;; NN-MAYBE-GET-MORE-HEADERS returns () then don't do anything because we
;;; changed to the next group.  Then see if the message the user has
;;; requested is already in the message buffer.  If the it isn't, put it
;;; there.  If it is, and maybe-scroll-down is t, then scroll the window
;;; down p lines in :multiple mode, or just change to the buffer in :single
;;; mode.  I use scroll-window down becuase this function is called by
;;; "Netnews Show Article", "Netnews Next Article", and "Netnews Previous
;;; Article".  It doesn't make sense to scroll the window down if the guy
;;; just read a message, moved the point up one line and invoked "Netnews
;;; Next Article".  He expects to see the article again, not the second
;;; page of it.  Also check to make sure there is a message under the
;;; point.  If there is not, then get some more headers.  If there are no
;;; more headers, then go on to the next group.
;;;
(defun nn-show-article (nn-info dont-scroll-down &optional p)
  (let ((headers-buffer (nn-get-headers-buffer))
	(message-buffer (nn-info-buffer nn-info)))
    (cond
     ((eq (nn-maybe-get-more-headers nn-info) :go-on)
      (case (value netnews-last-header-style)
	(:this-headers (change-to-buffer headers-buffer)
		       (buffer-start (buffer-point headers-buffer))
		       (editor-error "Last header."))
	(:next-headers (change-to-next-group nn-info headers-buffer))
	(:next-article (change-to-next-group nn-info headers-buffer)
		       (netnews-show-article-command))))
     (t
      (cond ((and (not dont-scroll-down)
		  (= (nn-info-current-displayed-message nn-info)
		     (array-element-from-mark (buffer-point headers-buffer)
					      (nn-info-message-ids nn-info))))
	     (ecase (value netnews-read-style)
	       (:single (buffer-start (buffer-point message-buffer))
			(change-to-buffer message-buffer))
	       (:multiple
		(multiple-value-bind
		    (headers-window message-window newp)
		    (nn-assure-multi-windows nn-info)
		  (nn-put-buffers-in-windows headers-buffer message-buffer
					     headers-window message-window
					     :headers)
		  ;; If both windows were visible to start with, just scroll
		  ;; down.  If they weren't, then show the message over
		  ;; again.
		  ;;
		  (cond (newp (buffer-start (buffer-point message-buffer))
			      (buffer-start (window-point message-window)))
			(t (netnews-message-scroll-down-command
			    p message-buffer message-window)))))))
 	    (t
	     (nn-put-article-in-buffer nn-info headers-buffer)
	     (setf message-buffer (nn-info-buffer nn-info))
	     (multiple-value-bind
		 (headers-window message-window)
		 (ecase (value netnews-read-style) ; Only need windows in
		   (:single (values () ()))      ; :multiple mode.
		   (:multiple (nn-assure-multi-windows nn-info)))
	       (ecase (value netnews-read-style)
		 (:multiple
		  ;; When there is only one window displaying the headers
		  ;; buffer, move the window point of that buffer to the
		  ;; buffer-point.
		  (when (= (length (buffer-windows headers-buffer)) 1)
		    (move-mark (window-point headers-window)
			       (buffer-point headers-buffer)))
		  (buffer-start (window-point message-window))
		  (nn-put-buffers-in-windows headers-buffer message-buffer
					     headers-window message-window
					     :headers))
		 (:single (change-to-buffer message-buffer))))))))))

(defcommand "Netnews Message Quit" ()
  "Delete the current message buffer, and make the associated
   `News-Headers' buffer current."
  (or (editor-bound-p 'netnews-message-info)
      (editor-error "Must be in a News-Message Buffer"))
  (let ((message-buffer (current-buffer)))
    (change-to-buffer (nn-get-headers-buffer))
    (delete-buffer-if-possible message-buffer)))

(defevar "Netnews Message Header Fields"
  "A list of fields to include in message headers.  FIX The empty list
   means all available fields.  Otherwise, this variable should contain If
   an element of this list is an atom, then it should be the string name of
   a field.  If it is a cons, then the car should be the string name of a
   field, and the cdr should be the length to which this field should be
   limited.  Any string name is acceptable.  Only fields that exist are
   considered.")

(defcommand "Netnews Show Whole Header" ()
  "Display the entire header for the current message.  This is to revert
   the message after a `Netnews Message Header Fields'."
  (let* ((headers-buffer (nn-get-headers-buffer))
	 (nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (buffer (nn-get-message-buffer nn-info)))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (nn-put-article-in-buffer nn-info headers-buffer t))))

;;; NN-PUT-ARTICLE-IN-BUFFER puts the article under the point into the
;;; associated message buffer if it is not there already.  Uses value of
;;; "Netnews Message Header Fields" to determine what fields should appear
;;; in the message header.  Returns the number of the article under the
;;; point.
;;;
(defun nn-put-article-in-buffer (nn-info headers-buffer &optional override)
  (let ((stream (nn-info-stream nn-info))
	(article-number (array-element-from-mark
			 (buffer-point headers-buffer)
			 (nn-info-message-ids nn-info)))
	(message-buffer (nn-get-message-buffer nn-info)))
    (setf (nm-info-message-number (variable-value 'netnews-message-info
						  :buffer message-buffer))
	  (1+ (- article-number (nn-info-first nn-info))))
    (cond ((and (= (nn-info-current-displayed-message nn-info) article-number)
		(not override))
	   (buffer-start (buffer-point message-buffer)))
	  (t
	   ;; Request article as soon as possible to avoid waiting for reply.
	   ;;
	   (nntp-body article-number stream)
	   (setf (nn-info-current-displayed-message nn-info) article-number)
	   (process-status-response stream nn-info)
	   (with-writable-buffer (message-buffer)
	     (let ((point (buffer-point message-buffer))
		   (info (svref (nn-info-header-cache nn-info)
				(- article-number (nn-info-first nn-info))))
		   (message-fields (value netnews-message-header-fields))
		   key field-length)
	       (cond ((and message-fields
			   (not override))
		      (dolist (ele message-fields)
			(etypecase ele
			  (atom (setf key ele field-length ()))
			  (cons (setf key (car ele) field-length (cdr ele))))
			(let ((field-string (cdr (assoc key info
							:test #'string-equal))))
			  (when field-string
			    (insert-string point (string-capitalize key))
			    (insert-string point ": ")
			    (insert-string point field-string
					   0
					   (max
					    (if field-length
						(min field-length
						     (1- (length field-string)))
						(1- (length field-string)))
					    0))
			    (insert-character point #\newline)))))
		     (t
		      (dolist (ele info)
			(insert-string point (string-capitalize (car ele)))
			(insert-string point ": ")
			(insert-string point (cdr ele)
				       0 (max 0 (1- (length (cdr ele)))))
			(insert-character point #\newline))))
	       (insert-character point #\newline)
	       (nntp-insert-textual-response point (nn-info-stream nn-info))))
	   (buffer-start (buffer-point message-buffer))
	   (when (> article-number (nn-info-latest nn-info))
	     (setf (nn-info-latest nn-info) article-number))))
    article-number))

;;; NN-PUT-BUFFERS-IN-WINDOWS makes sure the message buffer goes in the message
;;; window and the headers buffer in the headers window.  If which-current
;;; is :headers, the headers buffer/window will be made current, if it is
;;; :message, the message buffer/window will be made current.
;;;
(defun nn-put-buffers-in-windows (headers-buffer message-buffer headers-window
				  message-window which-current)
  (setf (window-buffer message-window) message-buffer
	(window-buffer headers-window) headers-buffer)
  (setf (current-window) (ecase which-current
			   (:headers headers-window)
			   (:message message-window))
	(current-buffer) (case which-current
			   (:headers headers-buffer)
			   (:message message-buffer))))

(defevar "Netnews Headers Proportion"
  "Determines how much of the current window will display headers when
   *Netnews Read Style* is :multiple."
  :value .25)

(defun nn-assure-multi-windows (nn-info)
  (let ((newp ()))
    (unless (and (member (nn-info-message-window nn-info) *window-list*)
		 (member (nn-info-headers-window nn-info) *window-list*))
      (setf newp t)
      (setf (nn-info-message-window nn-info) (current-window)
	    (nn-info-headers-window nn-info)
	    (make-window (buffer-start-mark (nn-get-headers-buffer))
			 :proportion (value netnews-headers-proportion))))
    (values (nn-info-headers-window nn-info)
	    (nn-info-message-window nn-info)
	    newp)))

;;; NN-GET-MESSAGE-BUFFER returns the message buffer for an nn-info structure.
;;; If there is not one, this function makes it and sets the slot in nn-info.
;;;
(defun nn-get-message-buffer (nn-info)
  (let* ((message-buffer (nn-info-buffer nn-info))
	 (nm-info (if message-buffer
		      (variable-value 'netnews-message-info
				      :buffer message-buffer))))
    (cond ((and message-buffer (not (nm-info-keep-p nm-info)))
	   (with-writable-buffer (message-buffer)
	     (delete-region (buffer-region message-buffer)))
	   message-buffer)
	  (t
	   (let ((buf (make-buffer (nn-unique-message-buffer-name
				    (nn-info-current nn-info))
				   :modeline-fields
				   (append (value default-modeline-fields)
					   (list (modeline-field
						  :netnews-message)))
				   :modes '("News Message")
				   :delete-hook
				   (list #'nn-message-buffer-delete-hook))))
	     (setf (nn-info-buffer nn-info) buf)
	     (defevar "Netnews Message Info"
	       "Structure that keeps track of buffers in \"News-Message\"
	        mode."
	       :value (make-netnews-message-info
		       :headers-buffer (current-buffer))
	       :buffer buf)
	     buf)))))

;;; The usual.  Clean everything up.
;;;
(defun nn-message-buffer-delete-hook (buffer)
  (let* ((headers-buffer (nm-info-headers-buffer
			  (variable-value 'netnews-message-info
					  :buffer buffer)))
	 (nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (nm-info (variable-value 'netnews-message-info :buffer buffer)))
    (setf (nn-info-buffer nn-info) ())
    (setf (nn-info-current-displayed-message nn-info) -1)
    (let ((post-buffer (nm-info-post-buffer nm-info)))
      (when post-buffer
	(setf (post-info-message-buffer (variable-value
					 'post-info :buffer post-buffer))
	      ())))))

;;; NN-UNIQUE-MESSAGE-BUFFER-NAME likes to have a simple name, i.e.
;;; "Netnews Message for rec.music.synth".  When there is already a buffer
;;; by this name, however, we start counting until the name is unique.
;;;
(defun nn-unique-message-buffer-name (group)
  (let ((name (concatenate 'simple-string "Netnews Message for " group))
	(number 0))
    (loop
      (unless (getstring name *buffer-names*) (return name))
      (setf name (format () "Netnews Message ~D" number))
      (incf number))))

;;; INSERT-TEXTUAL-RESPONSE inserts a textual response from nntp at mark.
;;;
(defun nntp-insert-textual-response (mark stream)
  (with-input-from-nntp (string stream)
    (insert-string mark string 0 (1- (length string)))
    (insert-character mark #\newline)))

;;; NN-GET-HEADERS-BUFFER returns the headers buffer if we are in a message or
;;; headers buffer.
;;;
(defun nn-get-headers-buffer ()
  (cond ((editor-bound-p 'netnews-info)
	 (current-buffer))
	((editor-bound-p 'netnews-message-info)
	 (nm-info-headers-buffer (value netnews-message-info)))
	((editor-bound-p 'post-info)
	 (post-info-headers-buffer (value post-info)))
	(t ())))

(defcommand "Netnews Previous Line" (p (headers-buffer (current-buffer)))
  "Move the point to the last header before the point that is still alive.
   When on the first visible header, and there are more previous messages
   insert the headers for these messages, otherwise go on to the next group
   in \"Netnews Groups\"."
  (declare (ignore p))
  (let ((point (buffer-point headers-buffer))
	(nn-info (variable-value 'netnews-info :buffer headers-buffer)))
    (with-mark ((original-position point)
		(start point)
		(end point))
      (loop
	(unless (line-offset point -1)
	  (cond ((and (nn-info-from-end-p nn-info)
		      (nn-info-messages-waiting nn-info))
		 (nn-write-headers-to-mark nn-info headers-buffer)
		 (netnews-previous-line-command () headers-buffer))
		(t
		 (cond ((= (nn-info-first-visible nn-info)
			   (nn-info-first nn-info))
			(move-mark point original-position)
			(editor-error "No previous unKilled headers."))
		       (t
			(message "Requesting backward...")
			(nn-request-out-of-order nn-info headers-buffer)
			(netnews-previous-line-command () headers-buffer))))))
	(line-start (move-mark start point))
	(character-offset (move-mark end start) 1)
	(unless (string= (region-to-string (region start end)) "K")
	  (return))))))

(defevar "Netnews Last Header Style"
  "This variable determines the behaviour after the last message in a
   newsgroup is read.  Takes one of three values: :this-headers,
   :next-headers, or :next-article.  :this-headers means put show the
   headers buffer for this newsgroup.  :next-headers means go to the next
   newsgroup and show that headers buffer.  :next-article means go on to
   the next newsgroup and show the first unread article."
  :value :feep)

(defcommand "Netnews Next Line"
	    (p &optional (headers-buffer (current-buffer)))
  "Move the current point to the next line.  If the current message is the
   last message in the buffer, insert headers for more messages in the
   group if there are any, else act based on the value of `Netnews Last
   Header Style':

     :feep

         beep, indicating that all messages have been read

     :next-headers

         read in the headers for the next group in *Netnews Group File*.

     :next-article

         go on to the next group and shows the first unread message."
  "Move the current point to the next line.  If the current message is the
   last message in the buffer, insert headers for for more messages in the
   group if there are any, else act based on the value of `Netnews Last
   Header Style':

     :feep
         beep indicating that all messages have been read

     :next-headers

         read in the headers for the next group in *Netnews Group File*.

     :next-article

         go on to the next group and shows the first unread message.

   Return () after having gone on to the next group, :done if all groups
   had been read, or T if everything is normal."
  (declare (ignore p))
  (let* ((nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (point (buffer-point headers-buffer)))
    (with-mark ((start point)
		(end point))
      (loop
	(line-offset point 1)
	(cond ((eq (nn-maybe-get-more-headers nn-info) :go-on)
	       (case (value netnews-last-header-style)
		 (:feep
		  (editor-error "Last header."))
		 (:this-headers
		  (let ((headers-buffer (nn-get-headers-buffer)))
		    (change-to-buffer headers-buffer))
		  (editor-error "Last header."))
		 ;; FIX :next-article?
		 (t
		  (return (change-to-next-group nn-info headers-buffer)))))
	      (t
	       (line-start (move-mark start point))
	       (character-offset (move-mark end start) 1)
	       (unless (string= (region-to-string (region start end)) "K")
		 (return t))))))))

(defcommand "Netnews Headers Scroll Window Up" (p)
  "Scroll the headers window up one screenfull.  If the end of the buffer
   is visible, insert the next batch of headers."
  (scroll-window-up-command p)
  (let ((headers-buffer (current-buffer))
	(nn-info (value netnews-info)))
    (when (and (displayed-p (buffer-start-mark headers-buffer)
			    (current-window))
	       (not (= (nn-info-first nn-info)
		       (nn-info-first-visible nn-info))))
      (buffer-start (current-point))
      (netnews-previous-line-command))))

(defcommand "Netnews Headers Scroll Window Down" (p)
  "Scroll the headers window down one screenfull.  If the end of the buffer
   is visible, insert the next batch of headers."
  (scroll-window-down-command p)
  (let ((headers-buffer (current-buffer))
	(nn-info (value netnews-info)))
    (when (and (displayed-p (buffer-end-mark headers-buffer) (current-window))
	       (not (= (nn-info-last nn-info) (nn-info-last-visible nn-info))))
      (buffer-end (current-point))
      (netnews-next-line-command))))

; FIX name vs mh Keep Message
(defcommand "Netnews Message Keep Buffer" ()
  "Keep the current message buffer intact and start reading messages in
   another buffer.

   Usually one buffer displays all messages in a group, one at a time.  "
  (or (editor-bound-p 'netnews-message-info)
      (editor-error "Not in a News-Message buffer."))
  (setf (nm-info-keep-p (value netnews-message-info)) t))

(defcommand "Netnews Go To Headers Buffer" ()
  "When invoked from a `News Message' buffer with an associated
   `News Headers' buffer, place the associated `News Headers' buffer into
   the current window."
  (or (editor-bound-p 'netnews-message-info)
      (editor-error "Not in a message buffer."))
  (let ((headers-buffer (nm-info-headers-buffer (value netnews-message-info))))
    (or headers-buffer (editor-error "Headers buffer has been deleted"))
    (change-to-buffer headers-buffer)))

(defcommand "Netnews Go To Post Buffer" ()
  "In a `News-Message' or `Draft' buffer with an associated `News Headers'
   buffer, change to the associated `News Headers' buffer."
  (or (editor-bound-p 'netnews-message-info)
      (editor-error "Not in a News-Message buffer."))
  (let ((post-buffer (nm-info-post-buffer (value netnews-message-info))))
    (or post-buffer (editor-error "No associated post buffer."))
    (change-to-buffer post-buffer)))

(defcommand "Netnews Go To Draft Buffer" ()
  "In a `News-Message' buffer with an associated `Draft' buffer, change to the
   `Draft' buffer."
  (or (editor-bound-p 'netnews-message-info)
      (editor-error "Not in a News-Message buffer."))
  (let ((draft-buffer (nm-info-draft-buffer (value netnews-message-info))))
    (or draft-buffer (editor-error "No associated post buffer."))
    (change-to-buffer draft-buffer)))

(defcommand "Netnews Select Message Buffer" ()
  "In `News Headers' mode, select the buffer containing the last message
   read.  In `Post' mode, select the associated `News-Message' buffer, if
   there is one."
  (let* ((cbuf (current-buffer))
	 (mbuf (cond ((editor-bound-p 'post-info :buffer cbuf)
		      (post-info-message-buffer (value post-info)))
		     ((editor-bound-p 'netnews-info :buffer cbuf)
		      (nn-info-buffer (value netnews-info)))
		     (t
		      (editor-error "Not in a \"Post\" or \"News Headers\" ~
		                     buffer.")))))
    (or mbuf (editor-error "No assocated message buffer."))
    (change-to-buffer mbuf)))

;;; CHANGE-TO-NEXT-GROUP deletes nn-info's headers buffer region and sets
;;; up the next group in that buffer.  If there are no more groups to read,
;;; exits gracefully.
;;;
(defun change-to-next-group (nn-info headers-buffer)
  (when (nn-info-updatep nn-info)
    (nn-update-database-file (nn-info-latest nn-info)
			     (nn-info-current nn-info)))
  (let ((next-group (cadr (member (nn-info-current nn-info)
				  (nn-info-groups nn-info) :test #'string=))))
    (cond (next-group
	   (message "Going on to ~A" next-group)
	   (force-output *echo-area-stream*)
	   (let ((message-buffer (nn-info-buffer nn-info)))
	     (if message-buffer
		 (setf (buffer-name message-buffer)
		       (nn-unique-message-buffer-name next-group))))
	   (setf (buffer-name headers-buffer)
		 (nn-unique-headers-name next-group))
	   (setf (nn-info-current nn-info) next-group)
	   (with-writable-buffer (headers-buffer)
	     (delete-region (buffer-region headers-buffer)))
	   (setup-group next-group nn-info headers-buffer)
	   ())
	  (t
	   (if (eq headers-buffer *nn-headers-buffer*)
	       (message "This was the last group.  Exiting Netnews.")
	       (message "Done with ~A.  Exiting Netnews."
			(nn-info-current nn-info)))
	   (netnews-exit-command () t headers-buffer)
	   :done))))

(defun nn-update-database-file (latest group-name)
  (when latest (setf (nn-last-read-message-number group-name) latest)))


;;;; More commands.

(defevar "Netnews Scroll Show Next Message"
  "When true, show the next message in a group after scrolling off the end
   of one.  Otherwise editor error."
  :value t)

(defcommand "Netnews Message Scroll Down" (p (buffer (current-buffer))
					     (window (current-window)))
  "Scroll the current window down one screenful.  If the end of the message
   is already visible, if `Netnews Scroll Show Next Message' is true then
   inserts the next message into this message buffer, else beep."
  (if (displayed-p (buffer-end-mark buffer) window)
      (if (value netnews-scroll-show-next-message)
	  (netnews-next-article-command)
	  (editor-error "At end of buffer."))
      (scroll-window-down-command p window)))

(defcommand "Netnews Go to Next Group" (p)
  "Go to the next group in *Netnews Group File*, setting the group
   pointer for this group to the the latest message read.  With an argument
   leave the group pointer where it is."
  (nn-punt-headers (if p :none :latest)))

(defcommand "Netnews Group Punt Messages" (p)
  "Go on to the next group in *Netnews Group File* setting the netnews
   pointer for this group to the last message.  With an argument, set the
   pointer to the last visible message in this group."
  (nn-punt-headers (if p :last-visible :punt)))

(defcommand "Netnews Quit Starting Here" ()
  "Go to the next group in the *Netnews Group File*, setting the netnews
   pointer for the current group to the message before the one under the
   current point, so the next time this group is read, the message
   indicated by the point will appear first."
  (nn-punt-headers :this-one))

(defun nn-punt-headers (pointer-type)
  (let* ((headers-buffer (nn-get-headers-buffer))
	 (nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (stream (nn-info-header-stream nn-info)))
    (message "Exiting ~A" (nn-info-current nn-info))
    (setf (nn-info-latest nn-info)
	  (ecase pointer-type
	    (:latest (nn-info-latest nn-info))
	    (:punt (nn-info-last nn-info))
	    (:last-visible (nn-info-last-visible nn-info))
	    (:this-one
	     (1- (if (minusp (nn-info-current-displayed-message nn-info))
		     (array-element-from-mark (buffer-point headers-buffer)
					      (nn-info-message-ids nn-info))
		     (nn-info-current-displayed-message nn-info))))
	    (:none ())))
    ;; This clears out all headers that waiting on header-stream.
    ;; Must process each response in case a message is not really there.
    ;; If it isn't, then the call to WITH-INPUT-FROM-NNTP will gobble up
    ;; the error message and the next real article.
    ;;
    (when (nn-info-messages-waiting nn-info)
      (dotimes (i (nn-info-batch-count nn-info))
	(let ((response (process-status-response stream)))
	  (when response (with-input-from-nntp (string stream))))))
    (change-to-next-group nn-info headers-buffer)))

(defcommand "Fetch All Headers" ()
  "In a forward reading `Netnews headers' buffer, insert all headers after
   the last visible one into the headers buffer.  When reading the group
   backward, insert all headers before the first visible one into the
   headers buffer."
  (let* ((headers-buffer (nn-get-headers-buffer))
         (nn-info (variable-value 'netnews-info :buffer headers-buffer)))
    (if (nn-info-messages-waiting nn-info)
        (message "Fetching the rest of the headers for ~A"
                 (nn-info-current nn-info))
        (editor-error "All headers are in buffer."))
    ;; The first of these calls writes the headers that are waiting on the
    ;; headers stream and requests the rest.  The second inserts the rest, if
    ;; there are any.
    ;;
    (nn-write-headers-to-mark nn-info headers-buffer t)
    (nn-write-headers-to-mark nn-info headers-buffer)))

(defcommand "List All Groups" (p buffer)
  "Create a buffer called `Netnews Groups' and insert the names of all
   accessible Netnews groups into it alphabetically."
  (declare (ignore p))
  (let* ((headers-buffer (nn-get-headers-buffer))
	 (nn-info (if headers-buffer
		      (variable-value 'netnews-info :buffer headers-buffer)))
	 (stream (if headers-buffer
		     (nn-info-stream nn-info)
		     (connect-to-nntp))))
    (nntp-list stream)
    (message "Fetching group list...")
    (process-status-response stream)
    (let* ((buffer (or buffer (make-buffer (nn-new-list-newsgroups-name))))
	   (point (buffer-point buffer))
	   (groups (make-array 1500 :fill-pointer 0 :adjustable t)))
      (with-input-from-nntp (string (if headers-buffer
					(nn-info-stream nn-info)
					stream))
	(vector-push-extend string groups))
      (sort groups #'string<)
      (dotimes (i (length groups))
	(let ((group (aref groups i)))
	  (multiple-value-bind (last first) (list-response-args group)
	    (declare (ignore first))
	    (insert-string point group 0 (position #\space group))
	    (insert-string point (format () ": ~D~%" last)))))
      (setf (buffer-modified buffer) ())
      (buffer-start point)
      (change-to-buffer buffer))
    (fi headers-buffer (close stream))))

(defun nn-new-list-newsgroups-name ()
  (let ((name "Newsgroups List")
	(number 0))
    (declare (simple-string name)
	     (fixnum number))
    (loop
      (unless (getstring name *buffer-names*) (return name))
      (setf name (format () "Newsgroups List ~D" number))
      (incf number))))

(defevar "Netnews Message File"
  "This value is merged with your home directory to get the pathname of the
   file to which the editor will append messages."
  :value "nightshade.messages")

(defevar "Netnews Exit Confirm"
  "When true `Netnews Exit' prompts for confirmation on exit."
  :value t)

(defcommand "Netnews Exit" (p no-prompt-p
			      (headers-buf (nn-get-headers-buffer)))
  "Clean up and delete the `News Headers' buffer and all associated
   `News-Message' buffers.  If the value of `Netnews Exit Confirm' is true,
   then confirm the exit."
  (declare (ignore p))
  (let ((browse-buffer (variable-value 'netnews-browse-buffer
				       :buffer headers-buf)))
    (when (or browse-buffer
	      no-prompt-p
	      (not (value netnews-exit-confirm))
	      (prompt-for-y-or-n :prompt "Exit Netnews? "
				 :default "Y"
				 :default-string "Y"
				 :help "Yes exits netnews mode."))
      (let* ((nn-info (variable-value 'netnews-info :buffer headers-buf))
	     (message-buffer (nn-info-buffer nn-info))
	     (headers-window (nn-info-headers-window nn-info))
	     (message-window (nn-info-message-window nn-info)))
	(when (nn-info-updatep nn-info)
	  (nn-update-database-file (nn-info-latest nn-info)
				   (nn-info-current nn-info)))
	(when (and (eq (value netnews-read-style) :multiple)
		   (member headers-window *window-list*)
		   (member message-window *window-list*))
	  (delete-window message-window))
	(when message-buffer (delete-buffer-if-possible message-buffer))
	(delete-buffer-if-possible headers-buf)
	(when browse-buffer (change-to-buffer browse-buffer))))))


;;;; Commands to append messages to a file or file messages into mail folders.

(defcommand "Netnews Append to File" (p)
  "In a \"News Headers\" buffer, append the message under the point onto a
   prompted file, suggesting the file named by *Netnews Message File*.  In
   a \"News Message\" buffer, appends the message in the current buffer to
   the same file."
  (let* ((filename (merge-pathnames (value netnews-message-file)
				    (user-homedir-pathname)))
	 (file (prompt-for-file :prompt "Append to what file: "
				:must-exist ()
				:default filename
				:default-string (namestring filename))))
    (and p (probe-file file) (delete-file file))
    (message "Appending message to ~S" (namestring file))
    (cond ((editor-bound-p 'netnews-info)
	   (let* ((nn-info (value netnews-info))
		  (stream (nn-info-stream nn-info))
		  (article-number (array-element-from-mark
				   (current-point)
				   (nn-info-message-ids nn-info)
				   "Must be a header under point.")))
	     (with-open-file (file file :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)
	       (nntp-article article-number stream)
	       (process-status-response stream)
	       (with-input-from-nntp (string (nn-info-stream nn-info))
		 (write-line string file :end (1- (length string)))))))
	  (t
	   (write-file (buffer-region (current-buffer)) file)))
    ;; Put a page separator and some whitespace between messages for
    ;; readability when printing or scanning.
    ;;
    (with-open-file (f file :direction :output :if-exists :append)
      (terpri f)
      (terpri f)
      (write-line "" f)
      (terpri f))))

(defcommand "Netnews Headers File Message" ()
  "File the message under the point in a prompted mail folder.  Create the
   folder if it is new."
  (nn-file-message (value netnews-info) :headers))

(defcommand "Netnews Message File Message" ()
  "File the message in the current buffer in a prompted mail folder.
   Create the folder if it is new."
  (nn-file-message (variable-value 'netnews-info
				   :buffer (nn-get-headers-buffer))
		   :message))

(defun nn-file-message (nn-info kind)
  (let ((article-number (array-element-from-mark (current-point)
						 (nn-info-message-ids nn-info)
						 "No header under point."))
	(folder (prompt-for-folder :prompt "MH Folder: "
				   :must-exist ())))
    (or (mh:folder-exists-p folder)
	(if (prompt-for-y-or-n
	     :prompt "Destination folder doesn't exist.  Create it? "
	     :default t :default-string "Y")
	    (mh:create-folder folder)
	    (editor-error "Not filing message.")))
    (message "Filing message into ~A" folder)
    (ecase kind
      (:headers (nntp-article article-number (nn-info-stream nn-info))
		(process-status-response (nn-info-stream nn-info))
		(with-open-file (s "/tmp/temp.msg" :direction :output
				   :if-exists :rename-and-delete
				   :if-does-not-exist :create)
		  (with-input-from-nntp (string (nn-info-stream nn-info))
		    (write-line string s :end (1- (length string))))))
      (:message (write-file (buffer-region (current-buffer)) "/tmp/temp.msg"
			    :keep-backup ())))
    ;; FIX
    (mh "inc" `(,folder "-silent" "-file" "/tmp/temp.msg"))
    (message "Done.")))


;;;; "Post" Mode and supporting commands.

#[ Posting Messages

{command:Netnews Deliver Post}
{evariable:Netnews Deliver Post Confirm}
{command:Netnews Abort Post}

As in the mailer, when replying to a message you can be excerpt sections of
it using `Insert Message Buffer' and `Insert Message Region' in `Post' and
`News-Message' modes, respectively.  You can also use these commands when
replying to a message via mail in a `Draft' buffer.  In all cases, the same
binding is used: H-y.
]#

(defmode "Post" :major-p ())

;; FIX unique-buffer-name
(defun nn-unique-post-buffer-name ()
  (let ((name "Post")
	(number 0))
    (loop
      (or (getstring name *buffer-names*) (return name))
      (setf name (format () "Post ~D" number))
      (incf number))))

;;; We usually know what the subject and newsgroups are, so keep these patterns
;;; around to make finding where to insert the information easy.
;;;
(defvar *draft-subject-pattern*
  (new-search-pattern :string-insensitive :forward "Subject:"))

(defvar *draft-newsgroups-pattern*
  (new-search-pattern :string-insensitive :forward "Newsgroups:"))

(defcommand "Netnews Post Message" ()
  "Set up a buffer for posting to netnews, a `Post' buffer.  In a
   `News Headers' or `News Message' buffer, fill in the newsgroups field
   with the current group."
  (let ((headers-buf (nn-get-headers-buffer))
	(post-buf (nn-make-post-buffer)))
    ;; If we're in a "News Headers" or "News Message" buffer, fill in the
    ;; newsgroups: slot in the header.
    (when headers-buf
      (insert-string-after-pattern (buffer-point post-buf)
				   *draft-newsgroups-pattern*
				   (nn-info-current
				    (variable-value
				     'netnews-info :buffer headers-buf))))
    (nn-post-message () post-buf)))

(defcommand "Netnews Abort Post" ()
  "Delete the current `Post' buffer."
  (delete-buffer-if-possible (current-buffer)))

(defun foobie-frob (post-info buffer)
  (declare (ignore post-info))
  (change-to-buffer buffer))
#|
 #'(lambda (post-info buffer)
     (declare (ignore post-info))
     (print :changing) (force-output)
     (change-to-buffer buffer)
     (print :changed) (force-output))
|#
(defvar *netnews-post-frob-windows-hook* #'foobie-frob
  "This hook is FUNCALled in NN-POST-MESSAGE with a post-info structure and
   the corresponding \"POST\" buffer before a post is done.")

;;; NN-POST-MESSAGE sets up a buffer for posting.  If message buffer is
;;; supplied, it is associated with the post-info structure for the post
;;; buffer.
;;;
(defun nn-post-message (message-buffer &optional (buffer (nn-make-post-buffer)))
  (setf (buffer-modified buffer) ())
  (when message-buffer
    (setf (nm-info-post-buffer (variable-value 'netnews-message-info
					       :buffer message-buffer))
	  buffer))
  (let ((post-info (make-post-info :stream (connect-to-nntp)
				   :headers-buffer (nn-get-headers-buffer)
				   :message-buffer message-buffer)))
    (defevar "Post Info"
      "Information needed to manipulate post buffers."
      :buffer buffer
      :value post-info)
    (funcall *netnews-post-frob-windows-hook* post-info buffer)))

(defun nn-make-post-buffer ()
  (let* ((buffer (make-buffer (nn-unique-post-buffer-name)
			      :delete-hook (list #'nn-post-buffer-delete-hook)))
	 (stream (make-editor-output-stream (buffer-point buffer))))
    (setf (buffer-minor-mode buffer "Post") t)
    (write-line "Newsgroups: " stream)
    (write-line "Subject: " stream)
;   (write-string "Date: " stream)
;   (format stream "~A~%" (string-capitalize
;			   (format-universal-time () (get-universal-time)
;						  :style :government
;						  :print-weekday ())))
    (write-char #\newline stream)
    (write-char #\newline stream)
    buffer))

;;; The usual again.  NULLify the appropriate stream slots in associated
;;; structures.  Also call NN-REPLY-CLEANUP-SPLIT-WINDOWS to see if we
;;; need to delete one of the current windows.
;;;
(defun nn-post-buffer-delete-hook (buffer)
  (when (editor-bound-p 'post-info)
    (nn-reply-cleanup-split-windows buffer)
    (let* ((post-info (variable-value 'post-info :buffer buffer))
	   (message-buffer (post-info-message-buffer post-info)))
      (close (post-info-stream post-info))
      (when message-buffer
	(setf (nm-info-post-buffer (variable-value 'netnews-message-info
						   :buffer message-buffer))
	      ())))))

#[ Replying to Messages

The the editor Netnews interface also provides an easy way of replying to
messages through the the editor Mailer or via `Post' mode.

{command:Netnews Reply to Sender}
{command:Netnews Reply to Sender in Other Window}
{command:Netnews Reply to Group}
{command:Netnews Reply to Group in Other Window}
{command:Netnews Post Message}
{command:Netnews Forward Message}
{command:Netnews Go to Post Buffer}
{command:Netnews Go to Draft Buffer}
]#

;;; NN-REPLY-USING-CURRENT-WINDOW makes sure there is only one window for a
;;; normal reply.  *netnews-post-frob-windows-hook* is bound to this when
;;; "Netnews Reply to Group" is invoked."
;;;
(defun nn-reply-using-current-window (post-info buffer)
  (declare (ignore post-info))
  ;; Make sure there is only one window in :multiple mode.
  ;;
  (let* ((nn-info (variable-value 'netnews-info
				  :buffer (nn-get-headers-buffer)))
	 (headers-window (nn-info-headers-window nn-info))
	 (message-window (nn-info-message-window nn-info)))
    (when (and (eq (value netnews-read-style) :multiple)
	       (member message-window *window-list*)
	       (member headers-window *window-list*))
      (setf (current-window) message-window)
      (delete-window headers-window))
    (change-to-buffer buffer)))

;;; NN-REPLY-IN-OTHER-WINDOW-HOOK does what NN-REPLY-USING-CURRENT-WINDOW
;;; does, but in addition splits the current window in half, displaying the
;;; message buffer on top, and the reply buffer on the bottom.  Also set some
;;; slots in the post info structure so the cleanup function knowd to delete
;;; one of the two windows we've created.
;;;
(defun nn-reply-in-other-window-hook (post-info buffer)
  (nn-reply-using-current-window post-info buffer)
  (let* ((message-window (current-window))
	 (reply-window (make-window (buffer-start-mark buffer))))
    (setf (window-buffer message-window) (post-info-message-buffer post-info)
	  (current-window) reply-window
	  (post-info-message-window post-info) message-window
	  (post-info-reply-window post-info) reply-window)))

;;; NN-REPLY-CLEANUP-SPLIT-WINDOWS just deletes one of the windows that
;;; "Netnews Reply to Group in Other Window" created, if they still exist.
;;;
(defun nn-reply-cleanup-split-windows (post-buffer)
  (let* ((post-info (variable-value 'post-info :buffer post-buffer))
	 (message-window (post-info-message-window post-info)))
    (when (and (member (post-info-reply-window post-info) *window-list*)
	       (member message-window *window-list*))
      (delete-window message-window))))

(defcommand "Netnews Reply to Group" ()
  "Set up a `Post' buffer and insert the proper newgroups: and subject:
   fields.  Should be invoked from a \"News Message\" or \"News Headers\"
   buffer.  In a message buffer, reply to the message in that buffer, in a
   headers buffer, reply to the message under the point."
  (let ((*netnews-post-frob-windows-hook* #'nn-reply-using-current-window))
    (nn-reply-to-message)))

(defcommand "Netnews Reply to Group in Other Window" ()
  "Set up a `Post' buffer in the other window and insert the proper
   newgroups: and subject: fields.  Should be invoked from a
   \"News-Message\" or \"News Headers\" buffer.  In a message buffer, reply
   to the message in that buffer, in a headers buffer, reply to the message
   under the point."
  (let ((*netnews-post-frob-windows-hook* #'nn-reply-in-other-window-hook))
    (nn-reply-to-message)))

(defun nn-setup-for-reply-by-mail ()
  (let* ((headers-buffer (nn-get-headers-buffer))
	 (nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (message-buffer (nn-info-buffer nn-info))
	 (nm-info (variable-value 'netnews-message-info :buffer message-buffer))
	 (draft-buffer (sub-setup-message-draft "comp" :to-field))
	 (dinfo (variable-value 'draft-information :buffer draft-buffer)))
    (setf (buffer-delete-hook draft-buffer)
	  (list #'cleanup-netnews-draft-buffer))
    (when (nm-info-draft-buffer nm-info)
      (delete-variable 'message-buffer :buffer (nm-info-draft-buffer nm-info)))
    (setf (nm-info-draft-buffer nm-info) draft-buffer)
    (when headers-buffer
      (defevar "Headers Buffer"
	"This is bound in message and draft buffers to their associated
	 headers-buffer"
	:value headers-buffer :buffer draft-buffer))
    (setf (draft-info-headers-mark dinfo)
	  (copy-mark (buffer-point headers-buffer)))
    (defevar "Message Buffer"
      "This is bound in draft buffers to their associated message buffer."
      :value message-buffer :buffer draft-buffer)
    (values draft-buffer message-buffer)))

(defcommand "Netnews Forward Message" ()
  "Create a `Post' buffer.  In a `Netnews Headers' or `News-Message'
   buffer, put the text of the current message into the buffer along with
   marker lines to delimit the forwarded message."
  (multiple-value-bind (draft-buffer message-buffer)
		       (nn-setup-for-reply-by-mail)
    (with-mark ((mark (buffer-point draft-buffer) :left-inserting))
      (buffer-end mark)
      (insert-string mark (format () "~%------- Forwarded Message~%~%"))
      (insert-string mark (format () "~%------- End of Forwarded Message~%"))
      (line-offset mark -2 0)
      (insert-region mark (buffer-region message-buffer)))
    (nn-reply-using-current-window () draft-buffer)))

(defun nn-reply-to-sender ()
  (let* ((headers-buffer (nn-get-headers-buffer))
	 (nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (article (if (and (editor-bound-p 'netnews-info)
			   (minusp (nn-info-current-displayed-message
				    nn-info)))
		      (nn-put-article-in-buffer nn-info headers-buffer)
		      (nn-info-current-displayed-message nn-info))))
    (multiple-value-bind (draft-buffer message-buffer)
			 (nn-setup-for-reply-by-mail)
      (let ((point (buffer-point draft-buffer))
	    (to-field (or (nn-get-one-field nn-info "Reply-To" article)
			  (nn-get-one-field nn-info "From" article))))
	(insert-string-after-pattern point
				     *draft-to-pattern*
				     to-field
				     :end (1- (length to-field)))
	(let ((subject-field (nn-subject-replyify
			      (nn-get-one-field nn-info "Subject" article))))
	  (insert-string-after-pattern point
				       *draft-subject-pattern*
				       subject-field
				       :end (1- (length subject-field)))))
      (nn-reply-using-current-window () draft-buffer)
      (values draft-buffer message-buffer))))

(defcommand "Netnews Reply to Sender" ()
  "Reply to the sender of a message.

   Create a `Draft' buffer and try to fill in the \"To\" and \"Subject\"
   fields of the draft.  For the \"To\" field, looks at the reply-to field
   of the message being replied to, or failing that, the \"From\" field.
   Ensure that the \"Subject\" field starts with Re:, to signify that this
   is a reply."
  (nn-reply-to-sender))

(defcommand "Netnews Reply to Sender in Other Window" ()
  "Reply to the sender of a message in the other window, splitting the
   current window if it is the only window.

   Create a `Draft' buffer and try to fill in the \"To\" and \"Subject\"
   fields of the draft.  For the \"To\" field, looks at the reply-to field
   of the message being replied to, or failing that, the \"From\" field.
   Ensure that the \"Subject\" field starts with Re:, to signify that this
   is a reply."
  (multiple-value-bind (draft-buffer message-buffer)
		       (nn-reply-to-sender)
    (let* ((message-window (current-window))
	   (reply-window (make-window (buffer-start-mark draft-buffer))))
      (defevar "Split Window Draft"
	"Indicates window needs to be cleaned up for draft."
	:value t :buffer draft-buffer)
      (setf (window-buffer message-window) message-buffer
	    (current-window) reply-window))))

;;; CLEANUP-NETNEWS-DRAFT-BUFFER replaces the normal draft buffer delete hook
;;; because the generic one tries to set some slots in the related message-info
;;; structure which doesn't exist.  This function just sets the draft buffer
;;; slot of netnews-message-info to () so it won't screw you when you try
;;; to change to the associated draft buffer.
;;;
(defun cleanup-netnews-draft-buffer (buffer)
  (when (editor-bound-p 'message-buffer :buffer buffer)
    (setf (nm-info-draft-buffer
	   (variable-value 'netnews-message-info
			   :buffer (variable-value 'message-buffer
						   :buffer buffer)))
	  ())))

;;; NN-REPLYIFY-SUBJECT simply adds "Re: " to the front of a string if it is
;;; not already there.
;;;
(defun nn-subject-replyify (subject)
  (if (>= (length subject) 3)
      (if (not (string= subject "Re:" :end1 3 :end2 3))
	  (concatenate 'simple-string "Re: " subject)
	  subject)
      (concatenate 'simple-string "Re: " subject)))

(defun insert-string-after-pattern (mark search-pattern string
				    &key (start 0) (end (length string)))
  (buffer-start mark)
  (when (and (plusp end)
	     (find-pattern mark search-pattern))
    (insert-string (line-end mark) string start end))
  (buffer-end mark))

(defun nn-reply-to-message ()
  (let* ((headers-buffer (nn-get-headers-buffer))
	 (nn-info (variable-value 'netnews-info :buffer headers-buffer))
	 (article (if (and (editor-bound-p 'netnews-info)
			   (minusp (nn-info-current-displayed-message nn-info)))
		      (nn-put-article-in-buffer nn-info headers-buffer)
		      (nn-info-current-displayed-message nn-info)))
	 (post-buffer (nn-make-post-buffer))
	 (point (buffer-point post-buffer)))

    (let ((groups-field (nn-get-one-field nn-info "Newsgroups" article)))
      (insert-string-after-pattern point
				   *draft-newsgroups-pattern*
				   groups-field
				   :end (1- (length groups-field))))
    (let ((subject-field (nn-subject-replyify
			  (nn-get-one-field nn-info "Subject" article))))
      (insert-string-after-pattern point
				   *draft-subject-pattern*
				   subject-field
				   :end (1- (length subject-field))))
    (nn-post-message (nn-info-buffer nn-info) post-buffer)))

(defun nn-get-one-field (nn-info field article)
  (cdr (assoc field (svref (nn-info-header-cache nn-info)
			   (- article (nn-info-first nn-info)))
	      :test #'string-equal)))

(defvar *nntp-timeout-handler* 'nn-recover-from-timeout
  "This function gets FUNCALled when NNTP times out on us with the note passed
   to PROCESS-STATUS-RESPONSE.  The default assumes the note is an NN-INFO
   structure and tries to recover from the timeout.")

(defvar *nn-last-command-issued* ()
  "The last string issued to one of the NNTP streams.  Used to recover from
   a nntp timeout.")

;;; NN-RECOVER-FROM-POSTING-TIMEOUT is the recover method used when posting.
;;; It just resets the value of \"NNTP Stream\" and issues the last command
;;; again.
;;;
(defun nn-recover-from-posting-timeout (ignore)
  (declare (ignore ignore))
  (let ((stream (connect-to-nntp)))
    (setf (post-info-stream (value post-info)) stream)
    (write-nntp-command *nn-last-command-issued* stream :recover)
    (process-status-response stream)))

(defevar "Netnews Reply Address"
  "What the From: field will be when you post messages.  If this is (),
   the From: field will be determined using the association of :USER
   in *environment-list* and your machine name.")

(defevar "Netnews Signature Filename"
  "This value is merged with your home directory to get the pathname of
   your signature, which is appended to every post you make."
  :value ".signature")

(defevar "Netnews Deliver Post Confirm"
  "This determines whether Netnews Deliver Post will ask for confirmation
   before posting the current message."
  :value t)

(defcommand "Netnews Deliver Post" ()
  "Deliver the current Post buffer to the NNTP server.  If the file named
   by the value of \"Netnews Signature Filename\" exists, append it to the
   end of the message after adding a newline.  If `Netnews Deliver Post
   Confirm' is t, ask for confirmation before posting the message.  Beep if
   NNTP refuses the message."
  (when (or (not (value netnews-deliver-post-confirm))
	    (prompt-for-y-or-n :prompt "Post message? " :default t))
    (let* ((*nntp-timeout-handler* #'nn-recover-from-posting-timeout)
	   (stream (post-info-stream (value post-info))))
      (nntp-post stream)
      (let ((winp (process-status-response stream))
	    ;; Rebind stream here because the stream may have been pulled out
	    ;; from under us by an NNTP timeout.  The recover method for posting
	    ;; resets the editor variable.
	    (stream (post-info-stream (value post-info))))
	(or winp (editor-error "Posting prohibited in this group."))
	(let ((buffer (current-buffer))
	      (username (value netnews-reply-address)))
	  (nn-write-line (format () "From: ~A"
				 (if username
				     username
				     (string-downcase
				      ;; FIX should be std way to this
				      (format () "~A@~A"
					      (cdr (assoc :user
							  ext:*environment-list*))
					      (machine-instance)))))
			 stream)
	  (filter-region #'(lambda (string)
			     (when (string= string ".")
			       (write-char #\. stream))
			     (nn-write-line string stream))
			 (buffer-region buffer))
	  ;; Append signature
	  ;;
	  (let ((filename (merge-pathnames (value netnews-signature-filename)
					   (user-homedir-pathname))))
	    (when (probe-file filename)
	      (with-open-file (istream filename :direction :input)
		(loop
		  (let ((line (read-line istream () ())))
		    (or line (return))
		    (nn-write-line line stream))))))
	  (write-line nntp-eof stream)
	  (delete-buffer-if-possible buffer)
	  (let ((headers-buffer (nn-get-headers-buffer)))
	    (when headers-buffer (change-to-buffer headers-buffer)))
	  (message "Message Posted."))))))

(defun nn-write-line (line stream)
  (write-string line stream)
  (write-char #\return stream)
  (write-char #\newline stream)
  line)


;;;; News-Browse mode.

#[ News-Browse Mode

`News-Browse' mode provides an easy method of adding groups to
the `Netnews Group File'.

{command:Netnews Browse}
{command:Netnews Browse Add Group To File}
{command:Netnews Browse Pointer Add Group to File}
{command:Netnews Browse Read Group}
{command:Netnews Browse Pointer Read Group}
{command:Netnews Quit Browse}

The `Next Line' and `Previous Line' commands are conveniently bound to
n and p in this mode.
]#

(defmode "News Browse" :major-p t)

(defevar "Netnews Group File"
  "If set, \"Netnews\" merges this variable with the home: directory and
   looks there for a list of newsgroups (one per line) to read.  Groups may
   be added using `Netnews Browse' and related commands, or by editing this
   file."
  :value (config:config-pathname "netnews-groups"))

(defcommand "Netnews Browse" ()
  "Set up a buffer in `News-Browse' mode with all available groups listed
   one per line.  Bind commands for reading groups and adding them to the
   file specified by *Netnews Group File*."
  (let ((buffer (make-buffer "Netnews Browse")))
    (cond (buffer
	   (list-all-groups-command () buffer)
	   (setf (buffer-major-mode buffer) "News Browse")
	   (setf (buffer-writable buffer) ()))
	  (t (change-to-buffer (getstring "Netnews Browse" *buffer-names*))))))

(defcommand "Netnews Quit Browse" ()
  "Exit News-Browse Mode."
  (delete-buffer-if-possible (current-buffer)))

(defcommand "Netnews Browse Read Group" (p (mark (current-point)))
  "Read the group on the line under the point.  With a prefix use and
   modify the contents of the *Netnews Database File*, otherwise present
   the last few messages in the group."
  (let ((group-info-string (line-string (mark-line mark))))
    (netnews-command () (subseq group-info-string
				 0 (position #\: group-info-string))
		     () (current-buffer) p)))

(defcommand "Netnews Browse Pointer Read Group" (p)
  "Read the group on the line under the mouse pointer.  With a prefix use
   and modify the contents of the *Netnews Database File*, otherwise
   present the last few messages in the group.  Leave the point where it
   is."
  (multiple-value-bind (x y window) (last-key-event-cursorpos)
    (or window (editor-error "Failed to figure out where last click was."))
    (or y (editor-error "Expected a group in the modeline."))
    (netnews-browse-read-group-command p (cursorpos-to-mark x y window))))

(defcommand "Netnews Browse Add Group to File" (p (mark (current-point)))
  "Append the newsgroup on the line under the point to the file specified
   by *Netnews Group File*.  With an argument, delete all groups that
   were there to start with."
  (declare (ignore p))
  (let* ((group-info-string (line-string (mark-line mark)))
	 (group (subseq group-info-string 0 (position #\: group-info-string))))
    (with-open-file (s (merge-pathnames (value netnews-group-file)
					(user-homedir-pathname))
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
      (write-line group s))
    (message "Adding ~S to newsgroup file." group)))

(defcommand "Netnews Browse Pointer Add Group to File" ()
  "Append the newsgroup under the mouse pointer the file specified by
   *Netnews Group File*, leaving the point where it is"
  (multiple-value-bind (x y window) (last-key-event-cursorpos)
    (or window (editor-error "Failed to figure out where last click was."))
    (or y (editor-error "There is no group in the modeline."))
    (netnews-browse-add-group-to-file-command
     () (cursorpos-to-mark x y window))))


;;;; Low-level stream operations.

(defun streams-for-nntp ()
  (clear-echo-area)
  ; FIX message
  (format *echo-area-stream* "Connecting to NNTP...~%")
  (force-output *echo-area-stream*)
  (values (connect-to-nntp) (connect-to-nntp)))

(defparameter *nntp-port* 119
  "The nntp port number for NNTP as specified in RFC977.")

(defevar "Netnews NNTP Server"
  "The hostname of the NNTP server to use for reading Netnews."
  :value "news")

(defevar "Netnews NNTP Timeout Period"
  "The number of seconds to wait while trying to connect to the NNTP
   server.  After time period the connection will time out with an error."
  :value 30)

(defun raw-connect-to-nntp (server)
  (nn-mess " nn raw connect to ~A:~A" server *nntp-port*)
  (let ((stream (system:make-fd-stream
		 (ext:connect-to-inet-socket server *nntp-port*)
		 :input t :output t :buffering :line :name "NNTP"
		 :timeout (value netnews-nntp-timeout-period))))
    (process-status-response stream)
    stream))

(defun connect-to-nntp ()
  (or (value netnews-nntp-server)
      (editor-error
       "Need the name of a news server in *Netnews NNTP Server*."))
  (handler-case
      (let ((account (internet:make-inet-account
		      (value netnews-nntp-server))))
	;(setf (internet:inet-account-protocol account) "nntp")
	(internet:fill-from-netrc account)
	(let ((stream (raw-connect-to-nntp
		       (internet:inet-account-server account))))
	  (when (internet:account-user account)
	    (nn-mess " nn auth as ~A" (internet:account-user account))
	    ;; Authenticate.
	    (write-nntp-command (format () "AUTHINFO user ~A"
					(internet:account-user
					 account))
				stream
				:auth)
	    (process-status-response stream)
	    (write-nntp-command (format () "AUTHINFO pass ~A"
					(internet:account-password
					 account))
				stream
				:auth)
	    (process-status-response stream))
	  stream))
    (io-timeout ()
      (editor-error "Connection to NNTP timed out.  Try again later."))))

(defvar *nn-last-command-type* ()
  "Used to recover from a nntp timeout.")

(defun write-nntp-command (command stream type)
  (setf *nn-last-command-type* type)
  (setf *nn-last-command-issued* command)
  (write-string command stream)
  (write-char #\return stream)
  (write-char #\newline stream)
  (prog1 (force-output stream)
    (nn-mess ">nn ~A" command)))


;;;; PROCESS-STATUS-RESPONSE and NNTP error handling.

(defconstant nntp-error-codes '(#\4 #\5)
  "These codes signal that NNTP could not complete the request you asked for.")

(defvar *nntp-error-handlers* ())

;;; PROCESS-STATUS-RESPONSE makes sure a response waiting at the server is
;;; valid.  If the response code starts with a 4 or 5, then look it up in
;;; *nntp-error-handlers*.  If an error handler is defined, then FUNCALL it
;;; on note.  Otherwise editor error.  If the response is not an error code,
;;; then just return what NNTP returned to us for parsing later.
;;;
(defun process-status-response (stream &optional note)
  (let ((str (read-line stream)))
    (nn-mess "<nn  ~A" str)
    (if (member (schar str 0) nntp-error-codes :test #'char=)
	(let ((error-handler (cdr (assoc str *nntp-error-handlers*
					 :test #'(lambda (string1 string2)
						   (string= string1 string2
							    :end1 3
							    :end2 3))))))
	  (or error-handler
	      (error "NNTP error -- ~A" (subseq str 4 (1- (length str)))))
	  (funcall error-handler note))
	str)))

(defun nn-recover-from-timeout (nn-info)
  (message "NNTP timed out, attempting to reconnect and continue...")
  (let ((stream (nn-info-stream nn-info))
	(header-stream (nn-info-header-stream nn-info)))
    ;; If some messages are waiting on the header stream, insert them.
    ;;
    (when (listen header-stream)
      (nn-write-headers-to-mark nn-info (nn-get-headers-buffer)))
    (close stream)
    (close header-stream)
    (setf stream (connect-to-nntp)
	  header-stream (connect-to-nntp)
	  (nn-info-stream nn-info) stream
	  (nn-info-header-stream nn-info) header-stream)
    (let ((last-command *nn-last-command-issued*)
	  (last-command-type *nn-last-command-type*)
	  (current (nn-info-current nn-info)))
      (nntp-group current stream header-stream)
      (process-status-response stream)
      (process-status-response header-stream)
      (if (consp last-command)
	  (let ((stream-type (car last-command)))
	    (apply #'nn-send-many-head-requests
		   (cons (if (eq stream-type :header) header-stream stream)
			 (cdr last-command))))
	  (ecase last-command-type
	    ((:list :article :body)
	     (write-nntp-command last-command stream :recover)
	     (process-status-response stream))
	    ((:header-group :normal-group)
	     (write-nntp-command last-command stream :recover)
	     (write-nntp-command last-command header-stream :recover)))))))

;;; DEF-NNTP-ERROR-HANDLER takes a code and a function and associates the two
;;; in *nntp-error-handlers*.  If while PROCESSING a STATUS RESPONSE we come
;;; across one of these error codes, then FUNCALL the appropriate handler.
;;;
(defun def-nntp-error-handler (code function)
  (pushnew (cons (format () "~D" code) function) *nntp-error-handlers*))

;;; 503 is an NNTP timeout.  The code I wrote reconnects and recovers
;;; completely.
;;;
(def-nntp-error-handler 503 #'(lambda (note)
				(funcall *nntp-timeout-handler* note)))

;;; 400 means NNTP is cutting us of for some reason.  There is really nothing
;;; we can do.
;;;
(def-nntp-error-handler 400 #'(lambda (ignore)
				(declare (ignore ignore))
				(editor-error "NNTP discontinued service.  ~
				You should probably quit netnews and try ~
				again later.")))

;;; Some functions just need to know that something went wrong so they can
;;; do something about it, so let them know by returning ().
;;;
;;; 411  -   The group you tried to read is not a netnews group.
;;; 423  -   You requested a message that wasn't really there.
;;; 440  -   Posting is not allowed.
;;; 441  -   Posting is allowed, but the attempt failed for some other reason.
;;;
(flet ((false-function (ignore)
	 (declare (ignore ignore))
	 ()))
  (def-nntp-error-handler 423 #'false-function)
  (def-nntp-error-handler 411 #'false-function)
  (def-nntp-error-handler 440 #'false-function)
  (def-nntp-error-handler 441 #'false-function))


;;;; Implementation of NNTP response argument parsing.

;;; DEF-NNTP-ARG-PARSER returns a form that parses a string for arguments
;;; corresponding to each element of $types.  For instance, if $types is
;;; (:integer :string :integer :integer), this function returns a form that
;;; parses an integer, a string, and two more integers out of an nntp
;;; status response.
;;;
(defmacro def-nntp-arg-parser (types)
  (let ((form (gensym))
	(start (gensym))
	(res ()))
    (do ((type types (cdr type)))
	((endp type) form)
      (ecase (car type)
	(:integer
	 (push `(parse-integer string :start ,start
			       :end (setf ,start
					  (position #\space string
						    :start (1+ ,start)))
			       :junk-allowed t)
	       res))
	(:string
	 (push `(subseq string (1+ ,start)
			(position #\space string
				  :start (setf ,start (1+ ,start))))
	       res))))
    `(let ((,start (position #\space string)))
       (values ,@(nreverse res)))))

(defun def-nntp-xhdr-arg-parser (string)
  (let ((position (position #\space string)))
    (values (subseq string (1+ position))
	    (parse-integer string :start 0 :end position))))

(defun xhdr-response-args (string)
  (def-nntp-xhdr-arg-parser string))

;;; GROUP-RESPONSE-ARGS, ARTICLER-RESPONSE-ARGS, HEAD-RESPONSE-ARGS,
;;; BODY-RESPONSE-ARGS, and STAT-RESPONSE-ARGS define NNTP argument parsers
;;; for the types of arguments each command will return.
;;;
(defun group-response-args (string)
  "Group response args are an estimate of how many messages there are, the
   number of the first message, the number of the last message, and \"y\"
   or \"n\", indicating whether the user has rights to post in this group."
  (def-nntp-arg-parser (:integer :integer :integer)))

(defun list-response-args (string)
  (def-nntp-arg-parser (:integer :integer)))

(defun article-response-args (string)
  "Article response args are the message number and the message ID."
  (def-nntp-arg-parser (:integer :string)))

(defun head-response-args (string)
  "Head response args are the message number and the message ID."
  (def-nntp-arg-parser (:integer :string)))

(defun body-response-args (string)
  "Body response args are the message number and the message ID."
  (def-nntp-arg-parser (:integer :string)))

(defun stat-response-args (string)
  "Stat response args are the message number and the message ID."
  (def-nntp-arg-parser (:integer :string)))


;;;; Functions that send standard NNTP commands.

;;; NNTP-XHDR sends an XHDR command to the NNTP server.  We think this is a
;;; local extension, but using it is pragmatic.  It takes over three
;;; minutes to HEAD every message in a newsgroup.
;;;
(defun nntp-xhdr (field start end stream)
  (write-nntp-command (format () "xhdr ~A ~D-~D"
			      field
			      (if (numberp start) start (parse-integer start))
			      (if (numberp end) end (parse-integer end)))
		      stream
		      :xhdr))

(defun nntp-group (group-name stream header-stream)
  (let ((command (concatenate 'simple-string "group " group-name)))
    (write-nntp-command command stream :normal-group)
    (write-nntp-command command header-stream :header-group)))

(defun nntp-list (stream)
  (write-nntp-command "list" stream :list))

(defun nntp-head (article stream)
  (write-nntp-command (format () "head ~D" article) stream :head))

(defun nntp-article (number stream)
  (write-nntp-command (format () "article ~D" number) stream :article))

(defun nntp-body (number stream)
  (write-nntp-command (format () "body ~D" number) stream :body))

(defun nntp-post (stream)
  (write-nntp-command "post" stream :post))


#[ Netnews Bindings Wallchart

Key bindings for the editor [Netnews] interface.

== Global bindings ==

    Netnews Post Message                      C-x P

== News Headers and News Message modes ==

    Netnews Next Article                      n
    Netnews Previous Article                  p
    Netnews Go to Next Group                  g
    Netnews Group Punt Messages               G
    List All Groups                           l
    Netnews Append to File                    a
    Netnews Forward Message                   f
    Netnews Reply to Sender in Other Window   r
    Netnews Reply to Group in Other Window    R
    Netnews Quit Starting Here                .

== News Headers mode bindings ==

    Netnews Show Article                      Space
    Netnews Previous Line                     C-p, Uparrow
    Netnews Next Line                         C-n, Downarrow
    Netnews Headers Scroll Window Down        C-v
    Netnews Headers Scroll Window Up          M-v
    Netnews Select Message Buffer             H-m
    Netnews Exit                              q
    Netnews Headers File Message              o

== News-Message mode bindings ==

    Netnews Message Scroll Down               Space
    Scroll Window Up                          Backspace
    Netnews Go To Headers Buffer               H-h, ^
    Netnews Message Keep Buffer               k
    Netnews Message Quit                      q
    Netnews Message File Message              o
    Netnews Go To Post Buffer                  H-p
    Netnews Go To Draft Buffer                 H-d
    Insert Message Region                     H-y

== Post mode bindings ==

    Netnews Select Message Buffer             H-m
    Netnews Deliver Post                      H-s
    Netnews Abort Post                        H-q
    Insert Message Buffer                     H-y

== News-Browse mode bindings ==

    Netnews Quit Browse                       q
    Netnews Browse Add Group To File          a
    Netnews Browse Read Group                 Space
    Next Line                                 n
    Previous Line                             p
]#
