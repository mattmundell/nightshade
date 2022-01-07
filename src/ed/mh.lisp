;;; A mailer interface to MH.

(in-package "ED")


;;;; General stuff.

(defhvar "Trash Folder"
  "Name of trash folder, with leading +."
  :value "+trash")

(defvar *new-mail-buffer* ())

(defvar *mh-utility-bit-bucket* (make-broadcast-stream))

(defattribute "Digit"
  "This is just a (mod 2) attribute for base 10 digit characters.")
;;;
(dotimes (i 10)
  (setf (character-attribute :digit (digit-char i)) 1))


(defvar *boundary-pattern*
  (new-search-pattern :string-sensitive :forward "boundary=\""))
(defvar *two-nl-pattern*
  (new-search-pattern :string-sensitive :forward "

"))

(defmacro do-headers-buffers ((buffer-var folder &optional hinfo-var)
			      &rest forms)
  "The Forms are evaluated with Buffer-Var bound to each buffer containing
   headers lines for folder.  Optionally Hinfo-Var is bound to the
   headers-information structure."
  (let ((folder-var (gensym))
	(hinfo (gensym)))
    `(let ((,folder-var ,folder))
       (declare (simple-string ,folder-var))
       (dolist (,buffer-var *buffer-list*)
	 (when (editor-bound-p 'headers-information :buffer ,buffer-var)
	   (let ((,hinfo (variable-value 'headers-information
					 :buffer ,buffer-var)))
	     (when (string= (the simple-string (headers-info-folder ,hinfo))
			    ,folder-var)
	       ,@(if hinfo-var
		     `((let ((,hinfo-var ,hinfo))
			 ,@forms))
		     forms))))))))

(defmacro do-headers-lines ((hbuffer &key line-var mark-var) &rest forms)
  "Forms are evaluated for each non-blank line.  When supplied Line-Var and
   Mark-Var are to the line and a :left-inserting mark at the beginning of the
   line.  This works with DELETE-HEADERS-BUFFER-LINE, but one should be careful
   using this to modify the hbuffer."
  (let ((line-var (or line-var (gensym)))
	(mark-var (or mark-var (gensym)))
	(id (gensym)))
    `(with-mark ((,mark-var (buffer-point ,hbuffer) :left-inserting))
       (buffer-start ,mark-var)
       (loop
	 (let* ((,line-var (mark-line ,mark-var))
		(,id (line-message-id ,line-var)))
	   (or (blank-line-p ,line-var)
	       ,@forms)
	   (if (or (not (eq ,line-var (mark-line ,mark-var)))
		   (string/= ,id (line-message-id ,line-var)))
	       (line-start ,mark-var)
	       (or (line-offset ,mark-var 1 0) (return))))))))

(defmacro with-headers-mark ((mark-var hbuffer msg) &rest forms)
  "Forms are executed with Mark-Var bound to a :left-inserting mark at the
   beginning of the headers line representing msg.  If no such line exists,
   no execution occurs."
  (let ((line (gensym)))
    `(do-headers-lines (,hbuffer :line-var ,line :mark-var ,mark-var)
       (when (string= (the simple-string (line-message-id ,line))
		      (the simple-string ,msg))
	 ,@forms
	 (return)))))


;;;; Headers Mode.

(defmode "Headers" :major-p t)

(defhvar "Headers Information"
  "This holds the information about the current headers buffer."
  :value nil)

(defstruct (headers-info (:print-function print-headers-info))
  buffer	 ; Buffer for these headers.
  folder	 ; String name of folder with leading "+".
  msg-seq	 ; MH sequence of messages in buffer.
  msg-strings	 ; List of strings representing msg-seq.
  other-msg-bufs ; List of message buffers referencing this headers buffer.
  draft-bufs	 ; List of draft buffers referencing this headers buffer.
  msg-buffer)

(defun print-headers-info (obj str n)
  (declare (ignore n))
  (format str "#<Headers Info ~S>" (headers-info-folder obj)))

(defmacro line-message-deleted (line)
  `(getf (line-plist ,line) 'mh-msg-deleted))

(defmacro line-message-id (line)
  `(getf (line-plist ,line) 'mh-msg-id))

(defun headers-current-message (hinfo)
  (let* ((point (buffer-point (headers-info-buffer hinfo)))
	 (line (mark-line point)))
    (fi (blank-line-p line)
	(values (line-message-id line)
		(copy-mark point)))))

(defcommand "Message Headers" (p)
  "Prompts for a folder and messages, displaying headers in a buffer in the
   current window.  With an argument, prompt for a pick expression."
  "Show some headers."
  (let ((folder (prompt-for-folder)))
    (new-message-headers
     folder
     (prompt-for-message :prompt (if p
				     "MH messages to pick from: "
				     "MH messages: ")
			 :folder folder
			 :messages "all")
			 p)))

(defcommand "All Message Headers" (p)
  "Prompts for a folder, displaying headers of all messages in a buffer in
   the current window."
  "Show some headers."
  (declare (ignore p))
  (let ((folder (prompt-for-folder)))
     (new-message-headers folder (breakup-message-spec "all"))))

(defcommand "Refresh Headers" (p)
  "If the current buffer is a headers buffer then refresh the headers."
  "If the current buffer is a headers buffer then refresh the headers."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (when hinfo
      (new-message-headers (headers-info-folder hinfo)
			   (headers-info-msg-strings hinfo))
      (message "Messages refreshed."))))

(defcommand "Refresh All Headers" (p)
  "If in a headers buffer then refresh the buffer, showing all headers."
  "If in a headers buffer then refresh the buffer, showing all headers."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (when hinfo
      (new-message-headers (headers-info-folder hinfo) '("all"))
      (message "Messages refreshed (all)."))))

(defcommand "Sort Headers" (p)
  "Sort the folder associated with the headers, date-major subject-minor."
  "Sort the folder associated with the headers, date-major subject-minor."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (when hinfo
      (mh:sort-folder (headers-info-folder hinfo))
      (message "Messages sorted.")
      (refresh-headers-command nil))))

(defcommand "Pick Headers" (p)
  "Further narrow the selection of this folders headers.
   Prompts for a pick expression to pick over the headers in the current
   buffer.  Entering an empty expression displays all the headers for that
   folder."
  "Prompts for a pick expression to pick over the headers in the current
   buffer."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (or hinfo
	(editor-error "Pick Headers only works in a headers buffer."))
    (pick-message-headers hinfo)))

;;; PICK-MESSAGE-HEADERS picks messages from info's messages based on an
;;; expression provided by the user.  If the expression is empty, we do
;;; headers on all the messages in folder.  The buffer's name is changed to
;;; reflect the messages picked over and the expression used.
;;;
(defun pick-message-headers (hinfo)
  (let ((folder (headers-info-folder hinfo))
	(msgs (headers-info-msg-strings hinfo)))
    (let* ((pick (prompt-for-pick-expression))
	   (hbuffer (headers-info-buffer hinfo))
	   (new-mail-buf-p (eq hbuffer *new-mail-buffer*))
	   (region (cond (pick
			  (message-headers-to-region
			   folder (mh:pick-messages folder msgs pick)))
			 (new-mail-buf-p
			  (maybe-get-new-mail-msg-hdrs folder))
			 (t (message-headers-to-region folder
						       (list "all"))))))
      (with-writable-buffer (hbuffer)
	(revamp-headers-buffer hbuffer hinfo)
	(when region (insert-message-headers hbuffer hinfo region)))
      (setf (buffer-modified hbuffer) ())
      (buffer-start (buffer-point hbuffer))
      (setf (buffer-name hbuffer)
	    (cond (pick (format () "Headers ~A ~A ~A" folder msgs pick))
		  (new-mail-buf-p (format () "Unseen Headers ~A" folder))
		  (t (format () "Headers ~A (all)" folder)))))))

;;; NEW-MESSAGE-HEADERS picks over msgs if pickp is non-nil, or it just
;;; scans msgs.  It is important to pick and get the message headers region
;;; before making the buffer and info structures since
;;; MESSAGE-HEADERS-TO-REGION will call EDITOR-ERROR if they fail.  The
;;; buffer name is chosen based on folder, msgs, and an optional pick
;;; expression.
;;;
(defun new-message-headers (folder msgs &optional pickp)
  (let* ((pick-exp (if pickp (prompt-for-pick-expression)))
	 (pick (if pick-exp (mh:pick-messages folder msgs pick-exp)))
	 (region (message-headers-to-region folder (or pick msgs)))
	 (hbuffer (maybe-make-mh-buffer (format () "Headers ~A ~A~:[~; ~S~]"
						folder msgs pick pick-exp)
					:headers))
	 (hinfo (make-headers-info :buffer hbuffer :folder folder)))
    (insert-message-headers hbuffer hinfo region)
    (defhvar "Headers Information"
      "This holds the information about the current headers buffer."
      :value hinfo :buffer hbuffer)
    (setf (buffer-modified hbuffer) nil)
    (setf (buffer-writable hbuffer) nil)
    (buffer-start (buffer-point hbuffer))
    (setf (buffer-pathname hbuffer) (mh:folder-pathname folder))
    (change-to-buffer hbuffer)))

;; FIX probably better way to do this
(defhvar "Message Headers Fill Column"
  "The width of the text in a Message Headers Buffer.  If nil then \"Fill
   Column\" will be used instead."
  :value ())

;;; MESSAGE-HEADERS-TO-REGION uses the MH interface to output headers into
;;; buffer for folder and msgs.
;;;
;;; (value fill-column) should really be done as if the buffer were
;;; current, but the editor doesn't let you do this without the buffer
;;; being current.
;;;
(defun message-headers-to-region (folder msgs &optional width)
  (let ((region (make-empty-region)))
    (with-output-to-mark (stream (region-end region) :full)
      (mh:summarize-messages folder msgs stream
			     (or width
				 (value message-headers-fill-column)
				 (value fill-column))))
    region))

(defun insert-message-headers (hbuffer hinfo region)
  (ninsert-region (buffer-point hbuffer) region)
  (let ((seq (set-message-headers-ids hbuffer :return-seq)))
    (setf (headers-info-msg-seq hinfo) seq)
    (setf (headers-info-msg-strings hinfo) (mh:sequence-strings seq)))
  (when (value virtual-message-deletion)
    (note-deleted-headers hbuffer
			  (mh:sequence-list (headers-info-folder hinfo)
					    "edtrash"))))

(defun set-message-headers-ids (hbuffer &optional return-seq)
  (let ((msgs nil))
    (do-headers-lines (hbuffer :line-var line)
      (let* ((line-str (line-string line))
	     (num (parse-integer line-str :junk-allowed t)))
	(declare (simple-string line-str))
	(or num
	    ;; FIX
	    (editor-error "MH scan lines must contain the message id as the ~
			   first thing on the line for the editor interface."))
	(setf (line-message-id line) (number-string num))
	(when return-seq (setf msgs (mh:sequence-insert num msgs)))))
    msgs))

(defun note-deleted-headers (hbuffer deleted-seq)
  (when deleted-seq
    (do-headers-lines (hbuffer :line-var line :mark-var hmark)
      (if (mh:sequence-member-p (line-message-id line) deleted-seq)
	  (note-deleted-message-at-mark hmark)
	  (setf (line-message-deleted line) nil)))))

(defcommand "Delete Headers Buffer and Message Buffers" (p &optional buffer)
  "Prompts for a headers buffer to delete along with its associated message
   buffers.  Any associated draft buffers are left alone, but their associated
   message buffers will be deleted."
  "Deletes the current headers buffer and its associated message buffers."
  (declare (ignore p))
  (let* ((default (cond ((value headers-information) (current-buffer))
			((value message-information) (value headers-buffer))))
	 (buffer (or buffer
		     (prompt-for-buffer :default default
					:default-string
					(if default (buffer-name default))))))
    (or (editor-bound-p 'headers-information :buffer buffer)
	(editor-error "Not a headers buffer -- ~A" (buffer-name buffer)))
    (let* ((hinfo (variable-value 'headers-information :buffer buffer))
	   ;; Copy list since buffer cleanup hook is destructive.
	   (other-bufs (copy-list (headers-info-other-msg-bufs hinfo)))
	   (msg-buf (headers-info-msg-buffer hinfo)))
      (when msg-buf (delete-buffer-if-possible msg-buf))
      (dolist (b other-bufs) (delete-buffer-if-possible b))
      (delete-buffer-if-possible (headers-info-buffer hinfo)))))

(defhvar "Expunge Messages Confirm"
  "When set (the default), \"Expunge Messages\" and \"Quit Headers\" will ask
   for confirmation before expunging messages and packing the folder's message
   id's."
  :value t)

(defhvar "Temporary Draft Folder"
  "This is the folder name where MH fcc: messages are kept that are intended
   to be deleted and expunged when messages are expunged for any other
   folder -- \"Expunge Messages\" and \"Quit Headers\"."
  :value nil)

(defun delete-messages (folder messages &optional (verbose t))
  "Move the trash from FOLDER to the trash folder or if FOLDER is the trash
   folder clear out the trash folder."
  (if (string= folder (value trash-folder))
      (progn
	(if verbose (message "Deleting messages ..."))
	(if (prompt-for-y-or-n
	     :prompt "Really delete files? "
	     :default t
	     :default-string "Y")
	    (mh:delete-messages folder messages)
	    (if verbose (message "   ...canceled."))))
      (progn
	(if verbose (message "Moving messages to trash ..."))
	(mh:move-messages folder (value trash-folder) messages))))

;;; "Quit Headers" doesn't expunge or compact unless there is a deleted
;;; sequence.  This collapses other headers buffers into the same folder
;;; differently than "Expunge Messages" since the latter assumes there will
;;; always be one remaining headers buffer.  This command folds all headers
;;; buffers into the folder that are not the current buffer or the new mail
;;; buffer into one buffer.  When the current buffer is the new mail buffer
;;; we do not check for more unseen headers since we are about to delete
;;; the buffer anyway.  The other headers buffers must be deleted before
;;; making the new one due to aliasing the buffer structure and
;;; MAYBE-MAKE-MH-BUFFER.
;;;
(defcommand "Quit Headers" (p)
  "Quit headers buffer possibly expunging deleted messages.
   This affects the current headers buffer.  When there are deleted messages
   the user is asked for confirmation on expunging the messages and packing the
   folder's message id's.  Then the buffer and all its associated message
   buffers are deleted.  Setting \"Quit Headers Confirm\" to nil inhibits
   prompting.  When \"Temporary Draft Folder\" is bound, this folder's messages
   are deleted and expunged."
  "This affects the current headers buffer.  When there are deleted messages
   the user is asked for confirmation on expunging the messages and packing
   the folder.  Then the buffer and all its associated message buffers are
   deleted."
  (declare (ignore p))
  (let* ((hinfo (value headers-information))
	 (minfo (value message-information))
	 (hdrs-buf (cond (hinfo (current-buffer))
			 (minfo (value headers-buffer)))))
    (or hdrs-buf
	(editor-error "Must be in or associated with a headers buffer."))
    (let* ((folder (cond (hinfo (headers-info-folder hinfo))
			 (minfo (message-info-folder minfo))))
	   (deleted-seq (mh:sequence-list folder "edtrash")))
      (when (and deleted-seq
		 (or (not (value expunge-messages-confirm))
		     (prompt-for-y-or-n
		      :prompt (list "Expunge messages and pack folder ~A? "
				    folder)
		      :default t
		      :default-string "Y")))
	(delete-messages folder "trash")
	(let ((*standard-output* *mh-utility-bit-bucket*))
	  (message "Compacting folder ...")
	  (mh:pack-folder folder))
	(message "Maintaining consistency ...")
	(let (hbufs)
	  (declare (list hbufs))
	  (do-headers-buffers (b folder)
	    (or (eq b hdrs-buf) (eq b *new-mail-buffer*)
		(push b hbufs)))
	  (dolist (b hbufs)
	    (delete-headers-buffer-and-message-buffers-command nil b))
	  (when hbufs
	    (new-message-headers folder (list "all"))))
	(expunge-messages-fix-draft-buffers folder)
	(or (eq hdrs-buf *new-mail-buffer*)
	    (expunge-messages-fix-unseen-headers folder))
	(delete-and-expunge-temp-drafts)))
    (delete-headers-buffer-and-message-buffers-command nil hdrs-buf)))

;;; DELETE-AND-EXPUNGE-TEMP-DRAFTS deletes all the messages in the
;;; temporary draft folder if there is one defined.  Any headers buffers
;;; into this folder are deleted with their message buffers.  We have to
;;; create a list of buffers to delete since buffer deletion destructively
;;; modifies the same list DO-HEADERS-BUFFERS uses.  `delete-messages' is
;;; run with error reporting off, to prevent it from signalling an error.
;;; This function must return; for example, "Quit Headers" would not
;;; complete successfully if this ended up calling EDITOR-ERROR.
;;;
(defun delete-and-expunge-temp-drafts ()
  (let ((temp-draft-folder (value temporary-draft-folder)))
    (when temp-draft-folder
      (setf temp-draft-folder (mh:coerce-folder-name temp-draft-folder))
      (message "Deleting and expunging temporary drafts ...")
      ;; FIX move to trash?
      (when (mh:delete-messages temp-draft-folder t nil)
	(let (hdrs)
	  (declare (list hdrs))
	  (do-headers-buffers (b temp-draft-folder)
	    (push b hdrs))
	  (dolist (b hdrs)
	    (delete-headers-buffer-and-message-buffers-command nil b)))))))


;;;; Message Mode.

(defmode "Message" :major-p t)

(defhvar "Message Information"
  "This holds the information about the current message buffer."
  :value nil)

(defstruct message/draft-info
  headers-mark)		; Mark pointing to a headers line in a headers buffer.

(defstruct (message-info (:include message/draft-info)
			 (:print-function print-message-info))
  folder		; String name of folder with leading "+".
  msgs			; List of strings representing messages to be shown.
  draft-buf		; Possible draft buffer reference.
  keep)			; Whether message buffer may be re-used.

(defun print-message-info (obj str n)
  (declare (ignore n))
  (format str "#<Message Info ~S ~S>"
	  (message-info-folder obj) (message-info-msgs obj)))

;; FIX sort resulting headers
(defun filter-message-headers (mbuffer)
  "Filter out most headers from the message in Buffer.  Return the mime
   boundary string if there is one, else nil."
  (let* ((point (buffer-point mbuffer))
	 (end (copy-mark point))
	 mime-boundary mime-subtype)
    (when (find-pattern end *two-nl-pattern*)
      (mark-after end)
      (setf (variable-value 'message-headers :buffer mbuffer)
	    (copy-region (region point end)))
      (setf (variable-value 'message-headers-short :buffer mbuffer) nil)
      (let ((line (mark-line point)))
	;; FIX handle multi-line fields
	(loop for keep = () do
	  (if (mark= (mark line 0) end) (return))
	  (let* ((chars (line-string line))
		 (len (length chars)))
	    (if (and (>= len 13)
		     (string= chars "Content-Type:"
			      :end1 13 :end2 13))
		;; Content type field.
		(let* ((start (mark line 14))
		       (end (mark line 14))
		       type subtype)
		  ;; Parse types. ; parse mime type
		  (find-attribute end :word-delimiter)
		  (setq type (region-to-string (region start end)))
		  (mark-after end)
		  (move-mark start end)
		  (find-attribute end :word-delimiter)
		  (setq subtype (region-to-string (region start end)))
		  ;; Check for multipart boundary.
		  (when (and (string= (string-downcase type) "multipart")
			     (find-pattern start *boundary-pattern*))
		    (setq mime-subtype subtype)
		    (character-offset start 10)
		    (let ((end (copy-mark start)))
		      ;; FIX (find-character end #\")
		      (find-pattern end
				    (new-search-pattern :character
							:forward #\"))
		      (setq mime-boundary
			    (region-to-string (region start end))))))
		;; Other fields.
		(dolist (field
			 '("Subject:" "To:" "From:"
			   "Cc:" "Reply-To:" "Date:"
			   "Resent-To:" "Resent-From:"
			   "Resent-Cc:" "Resent-Date:"))
		  (let ((field-len (length field)))
		    (when (and (>= len (length field))
			       (string= chars field
					:end1 field-len :end2 field-len))
		      (setq keep t)
		      (return)))))
	    (if keep
		(progn
		  (setq line (line-next line))
; FIX loop over rest lines in multi-line field
; 		  (loop for chars = (line-string line)
; 		    for len = (length chars)
; 		    while (and line
; 			       (plusp len)
; 			       (member (char chars 0)
; 				       '(#\space #\newline)))
; 		    do
; 		    (delete-region (region (mark line 0)
; 					   (mark-after (mark line len)))))
		  )
		(let ((region (region (mark line 0)
				      (mark-after (mark line len)))))
		  (delete-region region)))))))
    (values mime-boundary (string-downcase mime-subtype))))

(defun parse-mime-type (mark)
  "Return the MIME type and subtype at Mark."
  ; text/plain
  (let ((start (copy-mark mark))
	(type))
    (when (find-attribute mark :word-delimiter)
      (setq type
	    (string-downcase (region-to-string (region start
						       mark))))
      (mark-after mark)
      (move-mark start mark)
      (when (find-attribute mark :word-delimiter)
	(values type (string-downcase (region-to-string
				       (region start mark))))))))

;; FIX name
(defun prepare-message (mbuffer folder message-id)
  "Filter most headers out of the message in Mbuffer and convert MIME parts
   into links to files."
  (multiple-value-bind (boundary mime-subtype)
		       (filter-message-headers mbuffer)
    (when boundary
      (let ((pattern (new-search-pattern :string-sensitive :forward
					 boundary))
	    (mark (copy-mark (buffer-start-mark mbuffer)))
	    (alternative-p (string= mime-subtype "alternative")))
	(or alternative-p
	    (progn
	      ;; Change into the directory of the message for mhn.
	      (multiple-value-bind (success dir)
				   (unix:unix-current-directory)
		(or success
		    (editor-error "Failed to get current directory: ~A."
				  dir))
		(unwind-protect
		    (progn
		      (unix:unix-chdir (directory-namestring
					(buffer-pathname mbuffer)))
		      ;; Could use "-auto" to store to name given in part.
		      (mh "mhn" `(,folder ,message-id "-store")))
		  (unix:unix-chdir dir)))
	      (message "MIME parts stored.")))
	;; Handle parts.
	(loop for part = 1 then (1+ part)
	  while (find-pattern mark pattern)
	  do
	  (line-offset mark 1 14)
	  (or (blank-line-p (mark-line mark))
	      (let ((end (copy-mark mark))
		    type subtype)
		;; Parse types.  ; FIX parse-mime-type
		(find-attribute end :word-delimiter)
		(setq type
		      (string-downcase (region-to-string (region mark
								 end))))
		(mark-after end)
		(move-mark mark end)
		(find-attribute end :word-delimiter)
		(setq subtype
		      (string-downcase (region-to-string (region mark
								 end))))
		;; FIX handle nested multiparts, may require mhn work
		(or (string= type "multipart")
		    ;; FIX check for multiple text/plain parts when alt-p?
		    (and (string= type "text")
			 (string= subtype "plain"))
		    (when (find-pattern mark *two-nl-pattern*)
		      (mark-after (mark-after mark))
		      ;; FIX if alt-p neatly filter out entire of
		      ;;     each alternative part
		      (or alternative-p
			  (progn
			    (setf (mark-kind mark) :left-inserting)
			    (insert-string
			     mark
			     (format nil "Stored in: ~A.~D.~A~%~%"
				     (buffer-pathname mbuffer)
				     part subtype))))
		      (let ((end (copy-mark mark)))
			(when (find-pattern end pattern)
			  (delete-region (region mark end)))))))))))))

(defcommand "Next Message" (p)
  "Show the next message.
   When in a message buffer, shows the next message in the associated headers
   buffer.  When in a headers buffer, moves point down a line and shows that
   message."
  "When in a message buffer, shows the next message in the associated headers
   buffer.  When in a headers buffer, moves point down a line and shows that
   message."
  (declare (ignore p))
  (show-message-offset 1))

(defcommand "Previous Message" (p)
  "Show the previous message.
   When in a message buffer, shows the previous message in the associated
   headers buffer.  When in a headers buffer, moves point up a line and shows
   that message."
  "When in a message buffer, shows the previous message in the associated
   headers buffer.  When in a headers buffer, moves point up a line and
   shows that message."
  (declare (ignore p))
  (show-message-offset -1))

(defcommand "Next Undeleted Message" (p)
  "Show the next undeleted message.
   When in a message buffer, shows the next undeleted message in the associated
   headers buffer.  When in a headers buffer, moves point down to a line
   without a deleted message and shows that message."
  "When in a message buffer, shows the next undeleted message in the associated
   headers buffer.  When in a headers buffer, moves point down to a line without
   a deleted message and shows that message."
  (declare (ignore p))
  (show-message-offset 1 :undeleted))

(defcommand "Previous Undeleted Message" (p)
  "Show the previous undeleted message.
   When in a message buffer, shows the previous undeleted message in the
   associated headers buffer.  When in a headers buffer, moves point up a line
   without a deleted message and shows that message."
  "When in a message buffer, shows the previous undeleted message in the
   associated headers buffer.  When in a headers buffer, moves point up a line
   without a deleted message and shows that message."
  (declare (ignore p))
  (show-message-offset -1 :undeleted))

(defun show-message-offset (offset &optional undeleted)
  (let ((minfo (value message-information)))
    (cond
     ((not minfo)
      (let ((hinfo (value headers-information)))
	(or hinfo (editor-error "Not in a message or headers buffer."))
	(show-message-offset-hdrs-buf hinfo offset undeleted)))
     ((message-info-keep minfo)
      (let ((hbuf (value headers-buffer)))
	(or hbuf (editor-error "Not associated with a headers buffer."))
	(let ((hinfo (variable-value 'headers-information :buffer hbuf))
	      (point (buffer-point hbuf)))
	  (move-mark point (message-info-headers-mark minfo))
	  (show-message-offset-hdrs-buf hinfo offset undeleted))))
     (t
      (show-message-offset-msg-buf minfo offset undeleted)))))

(defun show-message-offset-hdrs-buf (hinfo offset undeleted)
  (or hinfo (editor-error "Not in a message or headers buffer."))
  (or (show-message-offset-mark (buffer-point (headers-info-buffer hinfo))
				offset undeleted)
      (editor-error "No ~:[previous~;next~] ~:[~;undeleted ~]message."
		    (plusp offset) undeleted))
  (show-headers-message hinfo))

(defun show-message-offset-msg-buf (minfo offset undeleted)
  (let ((msg-mark (message-info-headers-mark minfo)))
    (or msg-mark (editor-error "Not associated with a headers buffer."))
    (unless (show-message-offset-mark msg-mark offset undeleted)
      (let ((hbuf (value headers-buffer))
	    (mbuf (current-buffer)))
	(or (buffer-windows hbuf)
	    (progn
	      (setf (current-buffer) hbuf)
	      (setf (window-buffer (current-window)) hbuf)
	      (delete-buffer-if-possible mbuf))))
      (editor-error "~:[First~;Last~] ~:[~;undeleted ~]message."
		    (plusp offset) undeleted))
    (move-mark (buffer-point (line-buffer (mark-line msg-mark))) msg-mark)
    (let* ((next-msg (line-message-id (mark-line msg-mark)))
	   (folder (message-info-folder minfo))
	   (mbuffer (current-buffer)))
      (with-writable-buffer (mbuffer)
	(delete-region (buffer-region mbuffer))
	(setf (buffer-name mbuffer)
	      (get-storable-msg-buf-name folder next-msg))
	(setf (message-info-msgs minfo) next-msg)
	(let ((path (merge-pathnames next-msg (mh:folder-pathname folder))))
	  (read-message folder next-msg mbuffer
			(if (string= (mh:strip-folder-name folder)
				     (mh:draft-folder))
			    t
			    (value message-headers)))
	  (setf (buffer-pathname mbuffer) path))
	(let ((unseen-seq (mh:profile-component "unseen-sequence")))
	  (when unseen-seq
	    (mh:mark-message folder next-msg unseen-seq :delete))))))
  (let ((dbuffer (message-info-draft-buf minfo)))
    (when dbuffer
      (delete-variable 'message-buffer :buffer dbuffer)
      (setf (message-info-draft-buf minfo) nil))))

(defun get-storable-msg-buf-name (folder msg)
  (let ((name (format nil "Message ~A ~A" folder msg)))
    (if (not (getstring name *buffer-names*))
	name
	(let ((n 2))
	  (loop
	    (setf name (format nil "Message ~A ~A copy ~D" folder msg n))
	    (or (getstring name *buffer-names*) (return name))
	    (incf n))))))

(defun show-message-offset-mark (msg-mark offset undeleted)
  (with-mark ((temp msg-mark))
    (let ((winp
	   (cond (undeleted
		  (loop
		    (or (and (line-offset temp offset 0)
			     (fi (blank-line-p (mark-line temp))))  ;; FIX
			(return nil))
		    (or (line-message-deleted (mark-line temp))
			(return t))))
		 ((and (line-offset temp offset 0)
		       (not (blank-line-p (mark-line temp)))))
		 (t nil))))
      (if winp (move-mark msg-mark temp)))))

(defcommand "Show Message" (p)
  "Shows the current message.
   Prompts for a folder and message(s), displaying this in the current window.
   When invoked in a headers buffer, shows the message on the current line."
  "Show a message."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (if hinfo
	;; FIX orig just called show-h-m here
	(if (string= (mh:strip-folder-name (headers-info-folder hinfo))
		     (mh:draft-folder))
	    ;(show-draft-message hinfo) FIX
	    (show-headers-message hinfo)
	    (show-headers-message hinfo))
	(let ((folder (prompt-for-folder)))
	  (show-prompted-message folder (prompt-for-message :folder folder))))))

(defcommand "Show Message Next Window" (p)
  "Shows the current message in the next window.  Prompts for a folder and
   message(s), displaying this in the current window.  When invoked in a
   headers buffer, shows the message on the current line."
  "Show a message."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (if (<= (length *window-list*) 2)
	(split-window-command nil)
	(setf (current-window) (next-window (current-window))))
    (if hinfo
	(progn
	  ;; FIX orig just called show-h-m here
	  (if (string= (mh:strip-folder-name (headers-info-folder hinfo))
		       (mh:draft-folder))
	      ;(show-draft-message hinfo) FIX
	      (show-headers-message hinfo)
	      (show-headers-message hinfo)))
	(progn
	  (let ((folder (prompt-for-folder)))
	    (show-prompted-message folder (prompt-for-message :folder folder)))))))

(defcommand "Show Message Nicely" (p)
  "Shows the current message in the next window shrinking the headers
   windows.  Prompts for a folder and message(s), displaying this in the
   current window.  When invoked in a headers buffer, shows the message on
   the current line."
  "Show a message."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (if hinfo
	(progn
	  ;; FIX flickers
	  (go-to-one-window-command nil)
	  (split-window-command nil)
	  (next-window-command nil)
	  (split-window-command nil)
	  (next-window-command nil)
	  (delete-window-command nil)
	  ;; FIX orig just called show-h-m here
	  (if (string= (mh:strip-folder-name (headers-info-folder hinfo))
		       (mh:draft-folder))
	      ;(show-draft-message hinfo) FIX
	      (show-headers-message hinfo)
	      (show-headers-message hinfo)))
	(progn
	  (if (<= (length *window-list*) 2)
	      (split-window-command nil)
	      (setf (current-window) (next-window (current-window))))
	  (let ((folder (prompt-for-folder)))
	    (show-prompted-message folder (prompt-for-message :folder folder)))))))

;;; SHOW-HEADERS-MESSAGE shows the current message for hinfo.  If there is a
;;; main message buffer, clobber it, and we don't have to deal with kept
;;; messages or draft associations since those operations should have moved
;;; the message buffer into the others list.  Remove the message from the
;;; unseen sequence, and make sure the message buffer is displayed in some
;;; window.
;;;
(defun show-headers-message (hinfo)
  (multiple-value-bind (cur-msg cur-mark)
		       (headers-current-message hinfo)
    (or cur-msg (editor-error "Point must be on a header line."))
    (let* ((mbuffer (headers-info-msg-buffer hinfo))
	   (folder (headers-info-folder hinfo))
	   (buf-name (get-storable-msg-buf-name folder cur-msg))
	   (writable nil))
      (cond (mbuffer
	     (setf (buffer-name mbuffer) buf-name)
	     (setf writable (buffer-writable mbuffer))
	     (setf (buffer-writable mbuffer) t)
	     (delete-region (buffer-region mbuffer))
	     (let ((minfo (variable-value 'message-information :buffer mbuffer)))
	       (move-mark (message-info-headers-mark minfo) cur-mark)
	       (delete-mark cur-mark)
	       (setf (message-info-msgs minfo) cur-msg)))
	    (t (setf mbuffer (maybe-make-mh-buffer buf-name :message))
	       (setf (headers-info-msg-buffer hinfo) mbuffer)
	       (defhvar "Message Information"
		 "This holds the information about the current headers buffer."
		 :value (make-message-info :folder folder
					   :msgs cur-msg
					   :headers-mark cur-mark)
		 :buffer mbuffer)
	       (defhvar "Headers Buffer"
		 "This is bound in message and draft buffers to their
		  associated headers buffer."
		 :value (headers-info-buffer hinfo) :buffer mbuffer)
	       (defhvar "Message Headers"
		 "List of message headers."
		 :value (value message-headers) :buffer mbuffer)
	       (defhvar "Message Headers Alternate"
		 "List of message headers for message header toggling."
		 :value t :buffer mbuffer)))
      (let ((path (merge-pathnames cur-msg (mh:folder-pathname folder))))
	(read-message folder cur-msg mbuffer
		      (if (string= (mh:strip-folder-name
				    (headers-info-folder hinfo))
				   (mh:draft-folder))
			  t
			  (value message-headers)))
	(setf (buffer-pathname mbuffer) path))
      (setf (buffer-writable mbuffer) writable)
      (let ((unseen-seq (mh:profile-component "unseen-sequence")))
	(when unseen-seq (mh:mark-message folder cur-msg unseen-seq :delete)))
      (get-message-buffer-window mbuffer))))

;;; SHOW-PROMPTED-MESSAGE takes an arbitrary message spec and blasts those
;;; messages into a message buffer.  First we pick the message to get them
;;; individually specified as normalized message ID's -- all integers and
;;; no funny names such as "last".
;;;
(defun show-prompted-message (folder msgs)
  (let* ((msgs (mh:pick-messages folder msgs ()))
	 (mbuffer (maybe-make-mh-buffer (format () "Message ~A ~A" folder msgs)
					:message)))
    (defhvar "Message Information"
      "This holds the information about the current headers buffer."
      :value (make-message-info :folder folder :msgs msgs)
      :buffer mbuffer)
    (let ((stream (make-hemlock-output-stream (buffer-point mbuffer)
					      :full)))
      (apply #'mh:show-messages stream folder msgs)
      (setf (buffer-modified mbuffer) nil))
    (buffer-start (buffer-point mbuffer))
    (setf (buffer-writable mbuffer) nil)
    (get-message-buffer-window mbuffer)))

;;; GET-MESSAGE-BUFFER-WINDOW currently just changes to buffer, unless buffer
;;; has any windows, in which case it uses the first one.  It could prompt for
;;; a window, split the current window, split the current window or use the
;;; next one if there is one, funcall an Hvar.  It could take a couple
;;; arguments to control its behaviour.  Whatever.
;;;
(defun get-message-buffer-window (mbuffer)
  (let ((wins (buffer-windows mbuffer)))
    (cond (wins
	   (setf (current-buffer) mbuffer)
	   (setf (current-window) (car wins)))
	  (t (change-to-buffer mbuffer)))))


(defhvar "Scroll Message Showing Next"
  "When this is set, \"Scroll Message\" shows the next message when the end
   of the current message is visible."
  :value t)

(defcommand "Scroll Message" (p)
  "Scroll the current window down through the current message.
   If the end of the message is visible, then show the next undeleted message
   if \"Scroll Message Showing Next\" is non-nil."
  "Scroll the current window down through the current message."
  (if (and (not p)
	   (displayed-p (buffer-end-mark (current-buffer)) (current-window))
	   (value scroll-message-showing-next))
      (show-message-offset 1 :undeleted)
      (scroll-window-down-command p)))


(defcommand "Keep Message" (p)
  "Keeps the current message buffer from being re-used.  Also, if the buffer
   would be deleted due to a draft completion, it will not be."
  "Keeps the current message buffer from being re-used.  Also, if the buffer
   would be deleted due to a draft completion, it will not be."
  (declare (ignore p))
  (let ((minfo (value message-information)))
    (or minfo (editor-error "Not in a message buffer."))
    (let ((hbuf (value headers-buffer)))
      (when hbuf
	(let ((mbuf (current-buffer))
	      (hinfo (variable-value 'headers-information :buffer hbuf)))
	  (when (eq (headers-info-msg-buffer hinfo) mbuf)
	    (setf (headers-info-msg-buffer hinfo) nil)
	    (push mbuf (headers-info-other-msg-bufs hinfo))))))
    (setf (message-info-keep minfo) t)))

(defcommand "Edit Message Buffer" (p)
  "Recursively edit message buffer.
   Puts the current message buffer into \"Text\" mode allowing modifications in
   a recursive edit.  While in this state, the buffer is associated with the
   pathname of the message, so saving the file is possible."
  "Puts the current message buffer into \"Text\" mode allowing modifications in
   a recursive edit.  While in this state, the buffer is associated with the
   pathname of the message, so saving the file is possible."
  (declare (ignore p))
  (let ((minfo (value message-information)))
    (or minfo (editor-error "Not in a message buffer."))
    (let* ((msgs (message-info-msgs minfo))
	   (mbuf (current-buffer))
	   (mbuf-name (buffer-name mbuf))
	   (writable (buffer-writable mbuf))
	   (abortp t))
      (when (consp msgs)
	(editor-error
	 "There appears to be more than one message in this buffer."))
      (or (string= (message-info-folder minfo)
		   (mh:coerce-folder-name (mh:draft-folder)))
	  (editor-error "Only drafts can be edited, at least for now."))
      (unwind-protect
	  (progn
	    (setf (buffer-writable mbuf) t)
	    (setf (buffer-pathname mbuf)
		  (merge-pathnames
		   msgs
		   (mh:folder-pathname (message-info-folder minfo))))
	    (setf (buffer-major-mode mbuf) "Text")
	    (setf (buffer-minor-mode mbuf "Draft") t)
	    (do-recursive-edit)
	    (setf abortp nil))
	(when (and (not abortp)
		   (buffer-modified mbuf)
		   (prompt-for-y-or-n
		    :prompt "Message buffer modified, save it? "
		    :default t))
	  (save-file-command nil mbuf))
	(setf (buffer-modified mbuf) nil)
	;; "Save File", which the user may have used, changes the buffer's name.
	(or (getstring mbuf-name *buffer-names*)
	    (setf (buffer-name mbuf) mbuf-name))
	(setf (buffer-writable mbuf) writable)
	(setf (buffer-pathname mbuf) nil)
	(setf (buffer-major-mode mbuf) "Message")
	(setf (buffer-minor-mode mbuf "Draft") nil)))))

(defcommand "Toggle Message Headers" (p)
  "Toggle message headers display between all and some."
  "Toggle message headers display between all and some."
  (declare (ignore p))
  (let ((buffer (current-buffer))
	(minfo (value message-information)))
    (or minfo
	(editor-error "The current buffer must be a message buffer."))
    (with-writable-buffer (buffer)
      (let* ((mark (copy-mark (buffer-start-mark buffer)))
	     (end (copy-mark mark)))
	(when (find-pattern end *two-nl-pattern*)
	  (mark-after end)
	  (mark-after end)
	  (delete-region (region mark end))
	  (with-output-to-mark (stream mark)
	    (mh:write-headers (message-info-folder minfo)
			      (message-info-msgs minfo)
			      stream
			      (lisp::swap (value message-headers) ; FIX lisp::
					  (value message-headers-alternate)))))))))

(defcommand "View MIME Part" (p)
  "View any MIME part described under point."
  "View any MIME part described under point."
  (declare (ignore p))
  (let ((minfo (value message-information)))
    (or minfo
	(editor-error "The current buffer must be a message buffer."))
    (let ((mark (copy-mark (current-point))))
      (line-start mark)
      (if (eq (next-character mark) #\#)
	  (let ((part (mh:get-part
		       (message-info-folder minfo)
		       (message-info-msgs minfo)
		       (count-characters
			(region (buffer-start-mark (current-buffer))
				mark)))))
	    (if part (msg "part")))))))

(defcommand "Save MIME Part" (p)
  "Save any MIME part described under point."
  "Save any MIME part described under point."
  (declare (ignore p))
  (let ((minfo (value message-information)))
    (or minfo
	(editor-error "The current buffer must be a message buffer."))
    (let ((mark (copy-mark (current-point))))
      (line-start mark)
      (if (eq (next-character mark) #\#)
	  (let ((part (mh:get-part
		       (message-info-folder minfo)
		       (message-info-msgs minfo)
		       (count-characters
			(region (buffer-start-mark (current-buffer))
				mark)))))
	    (if part
		(with-open-file
		    (stream
		     (loop
		       for file =
		       (prompt-for-file
			:must-exist ()
			:default (directory-namestring
				  (buffer-default-pathname (current-buffer)))
			:prompt "Save MIME part to file: "
			:help "Name of file in which to save MIME part")
		       do
		       (if (probe-file file)
			   (if (prompt-for-y-or-n
				:prompt "File exists, overwrite? "
				:default ())
			       (return file))
			   (return file)))
		     :direction :output
		     :if-does-not-exist :create)
		  (write-string part stream))))))))

(defcommand "Next MIME Part" (p)
  "Move to the next MIME part in the current buffer."
  "Move to the next MIME part in the current buffer."
  (or p (setq p 1))
  (let* ((mark (copy-mark (current-point)))
	 (buffer (current-buffer))
	 (end (if (plusp p)
		  (buffer-end-mark buffer)
		  (buffer-start-mark buffer))))
    (line-offset mark p 0)
    (loop
      (if (mark= mark end) (return))
      (when (and (next-character mark)
		 (char= (next-character mark) #\#))
	(move-mark (current-point) mark)
	(return))
      (line-offset mark p))))

(defcommand "Previous MIME Part" (p)
  "Move to the previous MIME part in the current buffer."
  "Move to the previous MIME part in the current buffer."
  (next-mime-part-command (- (or p 1))))


;;;; Draft Mode.

(defmode "Draft")

(defhvar "Draft Information"
  "This holds the information about the current draft buffer."
  :value nil)

(defstruct (draft-info (:include message/draft-info)
		       (:print-function print-draft-info))
  folder		;String name of draft folder with leading "+".
  message		;String id of draft folder message.
  pathname		;Pathname of draft in the draft folder directory.
  delivered		;This is set when the draft was really sent.
  replied-to-folder	;Folder of message draft is in reply to.
  replied-to-msg)	;Message draft is in reply to.

(defun print-draft-info (obj str n)
  (declare (ignore n))
  (format str "#<Draft Info ~A>" (draft-info-message obj)))


(defhvar "Reply to Message CC"
  "This is one of :all, :others and ().  This value determines which
   addresses the Reply to Message commands add to the CC field."
  :value :others)

(defhvar "Reply to Message Prefix CC"
  "This is one of :all, :others and ().  This value determines which
   addresses the Reply to Message commands add to the CC field when they
   are called with prefix arguments."
  :value ())

(defcommand "Reply to Message" (p)
  "Sets up a draft in reply to the current message.  Prompts for a folder
   and message to reply to.  When in a headers buffer, replies to the
   message on the current line.  When in a message buffer, replies to that
   message.  Regard \"Reply to Message CC\" for carbon copy
   behaviour; with a prefix regard \"Reply to Message Prefix CC\"."
  "Prompts for a folder and message to reply to.  When in a headers buffer,
   replies to the message on the current line.  When in a message buffer,
   replies to that message."
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (if (string= (mh:strip-folder-name (headers-info-folder hinfo))
			(mh:draft-folder))
	       (editor-error "Attempt to reply to a draft."))
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Point must be on a header line."))
	     (setup-reply-draft (headers-info-folder hinfo)
				cur-msg hinfo cur-mark p)))
	  (minfo
	   (setup-message-buffer-draft (current-buffer) minfo :reply p))
	  (t
	   (let ((folder (prompt-for-folder)))
	     (setup-reply-draft folder
				(car (prompt-for-message :folder folder))
				nil nil p))))))

;;; SETUP-REPLY-DRAFT takes a folder and msg to draft a reply to.  Optionally,
;;; a headers buffer and mark are associated with the draft.  First, the draft
;;; buffer is associated with the headers buffer if there is one.  Then the
;;; message buffer is created and associated with the drafter buffer and
;;; headers buffer.  Argument may be used to pass in the argument from the
;;; command.
;;;
(defun setup-reply-draft (folder msg &optional hinfo hmark argument)
  (let* ((dbuffer (sub-setup-message-draft
		   :repl :end-of-buffer
		   `(,folder ,msg ,argument)))
	 (dinfo (variable-value 'draft-information :buffer dbuffer))
	 (h-buf (if hinfo (headers-info-buffer hinfo))))
    (setf (draft-info-replied-to-folder dinfo) folder)
    (setf (draft-info-replied-to-msg dinfo) msg)
    (when h-buf
      (defhvar "Headers Buffer"
	"This is bound in message and draft buffers to their associated
	headers buffer."
	:value h-buf :buffer dbuffer)
      (setf (draft-info-headers-mark dinfo) hmark)
      (push dbuffer (headers-info-draft-bufs hinfo)))
    (let ((msg-buf (maybe-make-mh-buffer (format nil "Message ~A ~A" folder msg)
					 :message)))
      (defhvar "Message Information"
	"This holds the information about the current headers buffer."
	:value (make-message-info :folder folder :msgs msg
				  :headers-mark
				  (if h-buf (copy-mark hmark) hmark)
				  :draft-buf dbuffer)
	:buffer msg-buf)
      (when h-buf
	(defhvar "Headers Buffer"
	  "This is bound in message and draft buffers to their associated
	  headers buffer."
	  :value h-buf :buffer msg-buf)
	(push msg-buf (headers-info-other-msg-bufs hinfo)))
      (read-message folder msg msg-buf)
      (setf (buffer-writable msg-buf) nil)
      (defhvar "Message Buffer"
	"This is bound in draft buffers to their associated message buffer."
	:value msg-buf :buffer dbuffer))
    (get-draft-buffer-window dbuffer)))


(defcommand "Forward Message" (p)
  "Forward current message.
   Prompts for a folder and message to forward.  When in a headers buffer,
   forwards the message on the current line.  When in a message buffer,
   forwards that message."
  "Prompts for a folder and message to reply to.  When in a headers buffer,
   replies to the message on the current line.  When in a message buffer,
   replies to that message."
  (declare (ignore p))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (setup-forward-draft (headers-info-folder hinfo)
				  cur-msg hinfo cur-mark)))
	  (minfo
	   (setup-message-buffer-draft (current-buffer) minfo :forward))
	  (t
	   (let ((folder (prompt-for-folder)))
	     (setup-forward-draft folder
				  (car (prompt-for-message :folder folder))))))))

;;; SETUP-FORWARD-DRAFT sets up a draft forwarding folder's msg.  When there
;;; is a headers buffer involved (hinfo and hmark), the draft is associated
;;; with it.
;;;
;;; This function is like SETUP-REPLY-DRAFT (in addition to "forw" and
;;; :to-field), but it does not setup a message buffer.  If this is added as
;;; something forward drafts want, then SETUP-REPLY-DRAFT should be
;;; parameterized and renamed.
;;;
(defun setup-forward-draft (folder msg &optional hinfo hmark)
  (let* ((dbuffer (sub-setup-message-draft :forw :to-field
					   (list folder msg)))
	 (dinfo (variable-value 'draft-information :buffer dbuffer))
	 (h-buf (if hinfo (headers-info-buffer hinfo))))
    (when h-buf
      (defhvar "Headers Buffer"
	"This is bound in message and draft buffers to their associated
	headers buffer."
	:value h-buf :buffer dbuffer)
      (setf (draft-info-headers-mark dinfo) hmark)
      (push dbuffer (headers-info-draft-bufs hinfo)))
    (get-draft-buffer-window dbuffer)))


(defcommand "Send Message" (p)
  "Setup a draft buffer.
   Setup a draft buffer, reserving a draft folder message.  When invoked in a
   headers buffer, the current message is available in an associated message
   buffer."
  "Setup a draft buffer, reserving a draft folder message.  When invoked in
   a headers buffer, the current message is available in an associated
   message buffer."
  (declare (ignore p))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo (setup-headers-message-draft hinfo))
	  (minfo (setup-message-buffer-draft (current-buffer) minfo :compose))
	  (t (setup-message-draft)))))

(defun setup-message-draft ()
  (get-draft-buffer-window (sub-setup-message-draft :comp :to-field)))

;;; SETUP-HEADERS-MESSAGE-DRAFT sets up a draft buffer associated with a
;;; headers buffer and a message buffer.  The headers current message is
;;; inserted in the message buffer which is also associated with the headers
;;; buffer.  The draft buffer is associated with the message buffer.
;;;
(defun setup-headers-message-draft (hinfo)
  (multiple-value-bind (cur-msg cur-mark)
		       (headers-current-message hinfo)
    (or cur-msg (message "Draft not associated with any message."))
    (let* ((dbuffer (sub-setup-message-draft :comp :to-field))
	   (dinfo (variable-value 'draft-information :buffer dbuffer))
	   (h-buf (headers-info-buffer hinfo)))
      (when cur-msg
	(defhvar "Headers Buffer"
	  "This is bound in message and draft buffers to their associated headers
	  buffer."
	  :value h-buf :buffer dbuffer)
	(push dbuffer (headers-info-draft-bufs hinfo)))
      (when cur-msg
	(setf (draft-info-headers-mark dinfo) cur-mark)
	(let* ((folder (headers-info-folder hinfo))
	       (msg-buf (maybe-make-mh-buffer
			 (format nil "Message ~A ~A" folder cur-msg)
			 :message)))
	  (defhvar "Message Information"
	    "This holds the information about the current headers buffer."
	    :value (make-message-info :folder folder :msgs cur-msg
				      :headers-mark (copy-mark cur-mark)
				      :draft-buf dbuffer)
	    :buffer msg-buf)
	  (defhvar "Headers Buffer"
	    "This is bound in message and draft buffers to their associated
	     headers buffer."
	    :value h-buf :buffer msg-buf)
	  (push msg-buf (headers-info-other-msg-bufs hinfo))
	  (read-message folder cur-msg msg-buf)
	  (setf (buffer-writable msg-buf) nil)
	  (defhvar "Message Buffer"
	    "This is bound in draft buffers to their associated message buffer."
	    :value msg-buf :buffer dbuffer)))
      (get-draft-buffer-window dbuffer))))

;;; SETUP-MESSAGE-BUFFER-DRAFT takes a message buffer and its message
;;; information.  A draft buffer is created according to type, and the two
;;; buffers are associated.  Any previous association of the message buffer and
;;; a draft buffer is dropped.  Any association between the message buffer and
;;; a headers buffer is propagated to the draft buffer, and if the message
;;; buffer is the headers buffer's main message buffer, it is moved to "other"
;;; status.  Argument may be used to pass in the argument from the command.
;;;
(defun setup-message-buffer-draft (msg-buf minfo type &optional argument)
  (let* ((msgs (message-info-msgs minfo))
	 (cur-msg (if (consp msgs) (car msgs) msgs))
	 (folder (message-info-folder minfo))
	 (dbuffer
	  (ecase type
	    (:reply
	     (sub-setup-message-draft :repl
				      :end-of-buffer
				      `(,folder ,cur-msg ,argument)))
	    (:compose
	     (sub-setup-message-draft :comp :to-field))
	    (:forward
	     (sub-setup-message-draft :forw :to-field
				      (list folder cur-msg)))))
	 (dinfo (variable-value 'draft-information :buffer dbuffer)))
    (when (message-info-draft-buf minfo)
      (delete-variable 'message-buffer :buffer (message-info-draft-buf minfo)))
    (setf (message-info-draft-buf minfo) dbuffer)
    (when (eq type :reply)
      (setf (draft-info-replied-to-folder dinfo) folder)
      (setf (draft-info-replied-to-msg dinfo) cur-msg))
    (when (editor-bound-p 'headers-buffer :buffer msg-buf)
      (let* ((hbuf (variable-value 'headers-buffer :buffer msg-buf))
	     (hinfo (variable-value 'headers-information :buffer hbuf)))
	(defhvar "Headers Buffer"
	  "This is bound in message and draft buffers to their associated
	  headers buffer."
	  :value hbuf :buffer dbuffer)
	(setf (draft-info-headers-mark dinfo)
	      (copy-mark (message-info-headers-mark minfo)))
	(push dbuffer (headers-info-draft-bufs hinfo))
	(when (eq (headers-info-msg-buffer hinfo) msg-buf)
	  (setf (headers-info-msg-buffer hinfo) nil)
	  (push msg-buf (headers-info-other-msg-bufs hinfo)))))
    (defhvar "Message Buffer"
      "This is bound in draft buffers to their associated message buffer."
      :value msg-buf :buffer dbuffer)
    (get-draft-buffer-window dbuffer)))

(defvar *draft-to-pattern*
  (new-search-pattern :string-insensitive :forward "To:"))

;; FIX works only when interpreted
;      when compiled:
; 	Function with declared result type NIL returned:
; 	   MH:DRAFT-NEW
;      when byte-compiled:
; 	Type-error in KERNEL::OBJECT-NOT-FUNCTION-ERROR-HANDLER:
; 	   95822313 is not of type FUNCTION
;
(eval-when (eval load)

(defun sub-setup-message-draft (utility point-action &optional args)
  #|
  (mh:draft-new)  ;; works like this
  (if (eq utility :comp)
      (progn
	(ed::msg "comp")
	(mh:draft-new)  ;; still error like this
	(ed::msg "post comp")
	t)...)
  |#
  (ecase utility
    (:comp (mh:draft-new))
    (:forw (mh:draft-forward (car args) (cadr args)))
    (:repl (mh:draft-reply (car args) (cadr args)
			    (if (caddr args)
				(value reply-to-message-prefix-cc)
				(value reply-to-message-cc)))))
  (let* ((folder (mh:draft-folder))
	 (draft-msg (mh:current-message folder))
	 (msg-pn (merge-pathnames draft-msg (mh:draft-folder-pathname)))
	 (dbuffer (maybe-make-mh-buffer (format () "Draft ~A" draft-msg)
					:draft)))
    (read-message (mh:draft-folder) draft-msg dbuffer)
    (setf (buffer-pathname dbuffer) msg-pn)
    (defhvar "Draft Information"
      "This holds the information about the current draft buffer."
      :value (make-draft-info :folder (mh:coerce-folder-name folder)
			      :message draft-msg
			      :pathname msg-pn)
      :buffer dbuffer)
    (let ((point (buffer-point dbuffer)))
      (ecase point-action
	(:to-field
	 (when (find-pattern point *draft-to-pattern*)
	   (line-end point)))
	(:end-of-buffer (buffer-end point))))
    dbuffer))

) ; eval-when load

(defparser `((:mime-field     :mime-space :mime-type
			      (or :mime-subtype) :mime-space (or :mime-comment)
			      (any :mime-param))
	     (:mime-space     (group (any (or #\space #\tab))
				     (or (group #\newline (or #\tab #\space))
					 (any (or #\space #\tab)))))
	     (:mime-type      (group (many (or :alphanumeric #\. #\- #\_)))
			      (or :mime-comment))
	     (:mime-subtype   #\/ :mime-space (or :mime-comment)
			      :mime-type)
	     (:mime-param     #\; :mime-space (or :mime-comment)
			      (group (many :alphanumeric)) #\=
			      :mime-space
			      (or :mime-quoted
				  (group (many :alphanumeric)))
			      :mime-space (or :mime-comment))
	     (:mime-quoted    #\" (group (many (or "\\\\" "\\\"" :mime-qchar))) #\") ;; FIX flush \'s
	     (:mime-qchar     (cond (fi (member ch '(#\" #\newline)))))
	     (:mime-comment   #\( (group (any (or :mime-comment
						  (many :mime-cchar))))
			      #\))
	     (:mime-cchar     (cond (fi (member ch '(#\( #\)))))))
;  :bufferp t
)
#|
;(parse 'buffer-parse-mime-field)multipart/mixed; boundary="=-=-="
;(parse 'buffer-parse-mime-field)multipart/mixed; boundary=aaaa
(setq n (parse 'buffer-parse-mime-field))multipart/mixed; boundary="=-=-="
(parse 'buffer-parse-mime-field)multipart/related;
	type="multipart/alternative";
	boundary="----=_NextPart_000_0007_01C763F1.7CF6B680"
|#

(defun ct-parser (*stream*)
  (let* ((*streams* `((,*stream* 0))))
    (when *stream*
      (let ((node (parse-mime-field)))
	(when node
	  (setq node (node-next (node-content node)))
	  (values (region-to-string (node-content (node-content node)))
		  (progn
		    (setq node (node-next node))
		    (if (eq (type-of node) 'mime-subtype-node)
			(region-to-string
			 (node-content
			  (node-content
			   (node-next
			    (node-next
			     (node-next (node-content node)))))))))
		  (collect ((params))
		    (loop
		      for param = (node-next (node-next (node-next node)))
		      then (node-next param)
		      while (eq (type-of param) 'mime-param-node)
		      do
		      (let* ((name-node (node-next
					 (node-next
					  (node-next
					   (node-content param)))))
			     (value-node (node-next
					  (node-next
					   (node-next name-node)))))
			(params (cons (region-to-string
				       (node-content name-node))
				      (etypecase value-node
					(region-node
					 (region-to-string
					  (node-content value-node)))
					(mime-quoted-node
					 (region-to-string
					  (node-content
					   (node-next
					    (node-content value-node))))))))))
		    (params))))))))

(defun read-message (folder message buffer &optional (headers t))
  (with-output-to-mark (stream (buffer-point buffer))
    (or (mh:write-message folder message stream headers #'ct-parser)
	(editor-error "Failed to read message ~A in folder ~A."
		      message (mh:strip-folder-name folder))))
  (setf (buffer-write-date buffer)
	(file-write-date (merge-pathnames message
					  (mh:folder-pathname folder))))
  (buffer-start (buffer-point buffer))
  (setf (buffer-modified buffer) ()))


(defvar *draft-buffer-window-fun* 'change-to-buffer
  "This is called by GET-DRAFT-BUFFER-WINDOW to display a new draft buffer.
   The default is CHANGE-TO-BUFFER which uses the current window.")

;;; GET-DRAFT-BUFFER-WINDOW is called to display a new draft buffer.
;;;
(defun get-draft-buffer-window (dbuffer)
  (funcall *draft-buffer-window-fun* dbuffer))


(defcommand "Reply to Message in Other Window" (p)
  "Reply to message, creating another window for draft buffer.
   Prompts for a folder and message to reply to.  When in a headers buffer,
   replies to the message on the current line.  When in a message buffer,
   replies to that message.  The current window is split displaying the draft
   buffer in the new window and the message buffer in the current."
  "Prompts for a folder and message to reply to.  When in a headers buffer,
   replies to the message on the current line.  When in a message buffer,
   replies to that message."
  (let ((*draft-buffer-window-fun* #'draft-buffer-in-other-window))
    (reply-to-message-command p)))

(defun draft-buffer-in-other-window (dbuffer)
  (when (editor-bound-p 'message-buffer :buffer dbuffer)
    (let ((mbuf (variable-value 'message-buffer :buffer dbuffer)))
      (when (not (eq (current-buffer) mbuf))
	(change-to-buffer mbuf))))
  (setf (current-buffer) dbuffer)
  (setf (current-window) (make-window (buffer-start-mark dbuffer)))
  (defhvar "Split Window Draft"
    "Indicates window needs to be cleaned up for draft."
    :value t :buffer dbuffer))

(defcommand "Reply to Message with Message" (p)
  "Sets up a draft in reply to the current message, quoting the message.
   Regard \"Reply to Message CC\" for carbon copy behaviour; with a prefix
   regard \"Reply to Message Prefix CC\"."
  "Sets up a draft in reply to the current message, quoting the message.
   Regard \"Reply to Message CC\" for carbon copy behaviour; with a prefix
   regard \"Reply to Message Prefix CC\"."
  (reply-to-message-command p)
  (end-of-buffer-command nil)
  ;; FIX maybe if region in message body then insert region
  (insert-message-buffer-command nil)
  (deactivate-region)  ;; FIX why region still highlighted?
  (exchange-point-and-mark-command nil)
  ;; Filter out headers and leading and trailing blank lines.
  (let* ((point (copy-mark (current-point)))
	 (mark1 (copy-mark point))
 	 (mbip (value message-buffer-insertion-prefix))
	 (pattern (new-search-pattern :string-sensitive
				      :forward
				      (concatenate 'simple-string
						   (string #\newline)
						   mbip
						   (string #\newline))))
	 (chars (find-pattern mark1 pattern)))
    (when chars
      (character-offset mark1 chars)
      (let ((mark2 (copy-mark mark1)))
	(block find
	  (loop
	    (setq chars (find-pattern mark1 pattern))
	    (or chars (return-from find))
	    (character-offset mark1 chars)
	    (or (mark= mark1 mark2)
		(return-from find))
	    (move-mark mark2 mark1)))
	(delete-region (region point mark2))))
    ;; Trailing.
    (move-mark mark1 (buffer-end-mark (current-buffer)))
    (let ((pattern (new-search-pattern :string-sensitive
				       :backward
				       (concatenate 'simple-string
						    (string #\newline)
						    mbip
						    (string #\newline)))))
      (let ((mark2 (copy-mark mark1)))
	(block find
	  (loop
	    (setq chars (find-pattern mark1 pattern))
	    (or chars (return-from find))
	    (character-offset mark1 chars)
	    (or (mark= mark1 mark2)
		(return-from find))
	    (character-offset mark1 (1+ (- chars)))
	    (move-mark mark2 mark1)))
	(delete-region (region mark2 (buffer-end-mark (current-buffer))))))))

(defhvar "SMTP Account"
  "SMTP Account, created with make-smtp-account."
  :value (internet:make-inet-account "localhost"))

(defhvar "Deliver Message Confirm"
  "When set, \"Deliver Message\" will ask for confirmation before sending the
   draft."
  :value t)

(defcommand "Deliver Message" (p)
  "Save and deliver the current draft buffer.
   When in a draft buffer, saves the file and uses SEND to deliver the
   draft.  When in a Message buffer from the draft folder, delivers the
   draft.  Otherwise, prompts for a draft message id, invoking SEND."
  "When in a draft buffer, saves the file and uses SEND to deliver the
   draft.  When in a Message buffer from the draft folder, delivers the
   draft.  Otherwise, prompts for a draft message id, invoking SEND."
  (declare (ignore p))
  (let ((dinfo (value draft-information)))
    (cond (dinfo
	   (deliver-draft-buffer-message dinfo))
	  (t
	   (let ((folder (mh:coerce-folder-name (mh:draft-folder)))
		 (msg-info (value message-information))
		 (msg))
	     (if (and msg-info
		      (string= (message-info-folder msg-info) folder))
		 (let ((msgs (message-info-msgs msg-info)))
		   (setq msg
			 (if (if msgs (consp msgs) t)
			     (prog1
			       (prompt-for-message :folder folder)
			       (editor-error "FIX insert attachments"))
			     (let ((buf (current-buffer)))
			       (setf (buffer-writable buf) t)
			       (insert-attachments buf)
			       (when (buffer-modified buf)
				 (write-buffer-file buf
						    (buffer-pathname buf)))
			       (breakup-message-spec msgs)))))
		 (setq msg (prompt-for-message :folder folder)))
	     ;; FIX Other case annotates message to which draft replies.
	     (multiple-value-bind (success error)
				  (mh:deliver-messages (value smtp-account) msg folder)
	       (or success (editor-error "Delivery failed: ~A" error))))))))


(defun deliver-draft-buffer-message (dinfo)
  (if (draft-info-delivered dinfo)
      (editor-error "This draft has already been delivered."))
  (when (if (value deliver-message-confirm)
	    (prompt-for-y-or-n :prompt "Deliver message? " :default t)
	    t)
    (let ((dbuffer (current-buffer)))
      (setf (buffer-writable dbuffer) t)
      (insert-attachments dbuffer)
      (if (buffer-modified dbuffer)
	  (write-buffer-file dbuffer (buffer-pathname dbuffer)))
      (message "Delivering draft ...")
      (multiple-value-bind (success error)
			   (mh:deliver-messages (value smtp-account)
						(list (draft-info-message dinfo))
						(draft-info-folder dinfo))
	(or success (editor-error "Delivery failed: ~A" error)))
      (setf (draft-info-delivered dinfo) t)
      (let ((replied-folder (draft-info-replied-to-folder dinfo))
	    (replied-msg (draft-info-replied-to-msg dinfo)))
	(when replied-folder
	  (message "Annotating message being replied to ...")
	  (mh:annotate-message replied-folder replied-msg "replied")
	  (do-headers-buffers (hbuf replied-folder)
	    (with-headers-mark (hmark hbuf replied-msg)
	      (mark-to-note-replied-msg hmark)
	      (with-writable-buffer (hbuf)
		(setf (next-character hmark) #\A))))
	  (dolist (b *buffer-list*)
	    (and (editor-bound-p 'message-information :buffer b)
		 (buffer-modeline-field-p b :replied-to-message)
		 (dolist (w (buffer-windows b))
		   (update-modeline-field b w :replied-to-message))))))
      (maybe-delete-extra-draft-window dbuffer (current-window))
      (let ((mbuf (value message-buffer)))
	(and mbuf
	     (or (editor-bound-p 'netnews-message-info :buffer mbuf)
		 (let ((minfo (variable-value 'message-information :buffer mbuf)))
		   (if minfo (or (message-info-keep minfo)
				 (delete-buffer-if-possible mbuf)))))))
      (delete-buffer-if-possible dbuffer))))

(defun make-boundary (buffer mark)
  "Return a boundary string for Buffer that is unique after Mark."
  (declare (ignore buffer))
  (let* ((boundary "=-=-=")
	 (m2 (copy-mark mark))
	 (pattern (new-search-pattern :string-sensitive :forward
				      boundary)))
    (loop while (find-pattern m2 pattern)
          do
      (move-mark m2 mark)
      (setq boundary (format nil "~A-=" boundary))
      (setq pattern (new-search-pattern :string-sensitive :forward
					boundary)))
    boundary))

;#define CPERLIN 76              chars per output line (= 57 chars per input line)
;#define BPERLIN (CPERLIN / 4)   = 19 bytes per line of output
(defvar *base64-chars-per-line* 76
  "Characters per line of base64 output in MIME attachments.")

; FIX uip/mhn.c h/mhn.h
(defun insert-mime-part (mark boundary type subtype file &optional description)
  "Insert a MIME part at Mark."
  (with-output-to-mark (out mark)
    (format out "--~A~%Content-Type: ~A/~A~%" boundary type subtype)
    (format out "Content-Disposition: attachment; filename=\"~A\"~%"
	    (file-namestring file))
    ;; FIX when to use base64? uip/mhn.c
    (format out "Content-Transfer-Encoding: base64~%")
    (if description
	(format out "Content-Description: ~A" description))
    (format out "~%~%")
    (with-open-file (in file :direction :input
			     :if-does-not-exist :error)
      (with-temp-buffer (buffer file)
	(let* ((coded (base64:base64-encode
		       (region-to-string (buffer-region buffer))))
	       (len (length coded))
	       (start 0))
	  (loop for dist = (min (- len start) *base64-chars-per-line*)
	        while (< start len)
	        do
;	    (message "write ~A from ~A to ~A" coded start (+ start dist))
	    (write-string coded out :start start :end (+ start dist))
	    (terpri out)
	    (or (eq dist *base64-chars-per-line*) (return))
; 	    (if (eq dist *base64-chars-per-line*)
; 		(terpri out)
; 		;; FIX always terminate w an =? right to add an extra one?
; 		(progn
; 		  (if (eq (previous-character mark) #\=)
; 		      (terpri out)
; 		      (format out "=~%"))
; 		  (return)))
	    (incf start dist)))))))

(defvar *attach-start-pattern*
  (new-search-pattern :string-sensitive :forward "--[Attached: \""))

(defhvar "MH Attach in Lisp"
  "If true process attachments before calling the send program, otherwise
   attaching is left to MH (which requires an \"automhnproc: mhn\" line in
   :.mh_profile."
  :value t)

(defun insert-attachments (buffer)
  "Insert the files named by any attachment clauses in Buffer."
  (when (value mh-attach-in-lisp)
    (let ((mark (copy-mark (buffer-start-mark buffer))))
      ;; FIX ensure sep alone on line
      (when (find-string mark (value message-header-body-separator))
	(let ((sep-start (copy-mark mark))
	      (end (copy-mark mark))
	      (start (copy-mark mark))
	      (boundary (make-boundary buffer mark))
	      (found))
	  (move-mark mark sep-start) ; In case make-boundary moved mark.
	  (line-offset mark 1 0)
	  ; #text/plain [Todo list] /home/matt/todo
	  ; FIX should probably loop through lines instead of # search
	  (loop while (find-character mark #\#) do
	    ;; FIX if message text then insert boundary before text (only
	    ;;     after first part)
	    (and found (mark> mark end)
		 ;; Text between parts.
		 (with-output-to-mark (out end)
		   (format out "--~A~%~%" boundary)))
	    (when (zerop (mark-charpos mark))
	      (move-mark end mark)
	      (mark-after end)
	      (multiple-value-bind (type subtype)
				   (parse-mime-type end)
		(mark-after end)
		(when (and type subtype
			   (eq (previous-character end) #\ )
			   (eq (next-character end) #\[))
		  (mark-after end)
		  (move-mark start end)
		  (when (find-character end #\])
		    (let ((descr (region-to-string (region start end))))
		      (mark-after (mark-after end))
		      (when (eq (previous-character end) #\ )
			(move-mark start end)
			(line-end end)
			(let ((file (region-to-string (region start end))))
			  (mark-after end)
			  (delete-region (region mark end))
			  (insert-mime-part mark boundary type subtype
					    file descr)
			  (move-mark end mark) ; For first when in loop.
			  (mark-before mark) ; About to mark-after.
			  (setq found t))))))))
	    (mark-after mark))
	  (when found
	    ;; FIX what usually deals w separator?
	    ;;       maybe send/post blanks line following headers
	    (let ((sep-line (mark-line sep-start)))
	      (delete-region (region sep-start
				     (mark sep-line (line-length sep-line)))))
	    (with-output-to-mark (stream sep-start)
	      (format stream
  "MIME-Version: 1.0~%Content-Type: multipart/mixed; boundary=\"~A\"~%"
                      boundary)
	      (mark-after sep-start)
	      (or (string= (line-string (mark-line sep-start))
			   (format nil "--~A" boundary))
		  ;; First part is text.
		  (format stream "--~A~%~%" boundary)))
	    (with-output-to-mark (stream mark)
	      (format stream "--~A--~%" boundary))))))))

(defcommand "Delete Draft and Buffer" (p)
  "Delete the current draft and associated message and buffer."
  "Delete the current draft and associated message and buffer."
  (declare (ignore p))
  (let ((dinfo (value draft-information))
	(dbuffer (current-buffer)))
    (or dinfo (editor-error "No draft associated with buffer."))
    (maybe-delete-extra-draft-window dbuffer (current-window))
    (delete-file (draft-info-pathname dinfo))
    (let ((mbuf (value message-buffer)))
      (when (and mbuf
		 (not (editor-bound-p 'netnews-message-info :buffer mbuf)))
	(let ((minfo (variable-value 'message-information :buffer mbuf)))
	  (when (and minfo (not (message-info-keep minfo)))
	    (delete-buffer-if-possible mbuf)))))
    (delete-buffer-if-possible dbuffer)))

;;; MAYBE-DELETE-EXTRA-DRAFT-WINDOW -- Internal.
;;;
;;; This takes a draft buffer and a window into it that should not be deleted.
;;; If "Split Window Draft" is bound in the buffer, and there are at least two
;;; windows in dbuffer-window's group, then we delete some window.  Blow away
;;; the variable, so we don't think this is still a split window draft buffer.
;;;
(defun maybe-delete-extra-draft-window (dbuffer dbuffer-window)
  (when (and (editor-bound-p 'split-window-draft :buffer dbuffer)
	     ;; Since we know bitmap devices have window groups, this loop is
	     ;; more correct than testing the length of *window-list* and
	     ;; accounting for *echo-area-window* being in there.
	     (do ((start dbuffer-window)
		  (count 1 (1+ count))
		  (w (next-window dbuffer-window) (next-window w)))
		 ((eq start w) (> count 1))))
    (delete-window (next-window dbuffer-window))
    (delete-variable 'split-window-draft :buffer dbuffer)))

(defcommand "Remail Message" (p)
  "Prompts for a folder and message to remail.  Prompts for a resend-to
   address string and resend-cc address string.  When in a headers buffer,
   remails the message on the current line.  When in a message buffer,
   remails that message."
  "Prompts for a folder and message to remail.  Prompts for a resend-to
   address string and resend-cc address string.  When in a headers buffer,
   remails the message on the current line.  When in a message buffer,
   remails that message."
  (declare (ignore p))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (remail-message (headers-info-folder hinfo) cur-msg
			     (prompt-for-string :prompt "Resend To: ")
			     (prompt-for-string :prompt "Resend Cc: "))))
	  (minfo
	   (remail-message (message-info-folder minfo)
			   (message-info-msgs minfo)
			   (prompt-for-string :prompt "Resend To: ")
			   (prompt-for-string :prompt "Resend Cc: ")))
	  (t
	   (let ((folder (prompt-for-folder)))
	     (remail-message folder
			     (car (prompt-for-message :folder folder))
			     (prompt-for-string :prompt "Resend To: ")
			     (prompt-for-string :prompt "Resend Cc: "))))))
  (message "Message remailed."))


;;; REMAIL-MESSAGE claims a draft folder message with mh:draft-message.
;;; This is then sucked into a buffer and modified by inserting the
;;; supplied addresses.  "send" is used to deliver the draft, but it
;;; requires certain environment variables to make it do the right thing.
;;; "mhdist" says the draft is only remailing information, and "mhaltmsg"
;;; is the message to send.  "mhannotate" must be set due to a bug in MH's
;;; "send"; it will not notice the "mhdist" flag unless there is some
;;; message to be annotated.  This command does not provide for annotation
;;; of the remailed message.
;;;
(defun remail-message (folder msg resend-to resend-cc)
  (mh:draft-resend)
  (let* ((draft-folder (mh:draft-folder))
	 (draft-msg (mh:current-message draft-folder)))
    (setup-remail-draft-message draft-msg resend-to resend-cc)
    (mh:resend-message (value smtp-account) msg folder draft-msg draft-folder))
;     (mh "send" `("-draftfolder" ,draft-folder "-draftmessage" ,draft-msg)
; 	:environment
; 	`((:|mhdist| . "1")
; 	  (:|mhannotate| . "1")
; 	  (:|mhaltmsg| . ,(namestring
; 			   (merge-pathnames msg (mh:folder-pathname folder)))))))
  )

;;; SETUP-REMAIL-DRAFT-MESSAGE takes a draft folder and message that have
;;; been created with mh:draft-message.  A buffer is created with this
;;; message's pathname, searching for "resent-to:" and "resent-cc:",
;;; filling in the supplied argument values.  After writing out the
;;; results, the buffer is deleted.
;;;
(defvar *draft-resent-to-pattern*
  (new-search-pattern :string-insensitive :forward "resent-to:"))
(defvar *draft-resent-cc-pattern*
  (new-search-pattern :string-insensitive :forward "resent-cc:"))

(defun setup-remail-draft-message (msg resend-to resend-cc)
  (let* ((msg-pn (merge-pathnames msg (mh:draft-folder-pathname)))
	 (dbuffer (maybe-make-mh-buffer (format () "Draft ~A" msg)
					:draft))
	 (point (buffer-point dbuffer)))
    (read-message (mh:draft-folder) msg dbuffer)
    (when (find-pattern point *draft-resent-to-pattern*)
      (line-end point)
      (insert-string point resend-to))
    (buffer-start point)
    (when (find-pattern point *draft-resent-cc-pattern*)
      (line-end point)
      (insert-string point resend-cc))
    (write-file (buffer-region dbuffer) msg-pn :keep-backup nil)
    ;; Touch the draft directory to ensure that the cache is updated.
    (touch-file (mh:draft-folder-pathname))
    ;; The draft buffer delete hook expects this to be bound.
    (defhvar "Draft Information"
      "This holds the information about the current draft buffer."
      :value :ignore
      :buffer dbuffer)
    (delete-buffer dbuffer)))

;;; SHOW-DRAFT-MESSAGE shows the current message for hinfo.  If there is a
;;; main message buffer, clobber it, and we don't have to deal with kept
;;; messages since that operation should have moved the message buffer into
;;; the others list.  Make sure the message buffer is displayed in some
;;; window.
;;;
(defun show-draft-message (hinfo)
  (multiple-value-bind (cur-msg cur-mark)
		       (headers-current-message hinfo)
    (or cur-msg (editor-error "Not on a header line."))
    (let* ((mbuffer (headers-info-msg-buffer hinfo))
	   (folder (headers-info-folder hinfo))
;; FIX?
;;	   (buf-name (get-storable-msg-buf-name folder cur-msg))
	   (buf-name (format nil "Draft ~A" cur-msg))
	   (writable nil)
	   (msg-pathname (merge-pathnames cur-msg
					  (mh:draft-folder-pathname))))
      (cond (mbuffer
	     (setf (buffer-name mbuffer) buf-name)
	     (setf writable (buffer-writable mbuffer))
	     (setf (buffer-writable mbuffer) t)
	     (delete-region (buffer-region mbuffer))
	     (let ((minfo (variable-value 'message-information :buffer mbuffer)))
	       (move-mark (message-info-headers-mark minfo) cur-mark)
	       (delete-mark cur-mark)
	       (setf (message-info-msgs minfo) cur-msg)))
	    (t (setf mbuffer (maybe-make-mh-buffer buf-name :draft))
	       (setf (headers-info-msg-buffer hinfo) mbuffer)
	       (defhvar "Draft Information"
		 "This holds the information about the current draft buffer."
		 :value (make-draft-info :folder folder
					 :message cur-msg
					 :pathname msg-pathname)
		 :buffer mbuffer)
	       (defhvar "Message Information"
		 "This holds the information about the current headers buffer."
		 :value (make-message-info :folder folder
					   :msgs cur-msg
					   :headers-mark cur-mark)
		 :buffer mbuffer)
	       (defhvar "Headers Buffer"
		 "This is bound in message and draft buffers to their
		  associated headers buffer."
		 :value (headers-info-buffer hinfo) :buffer mbuffer)))
      (read-message (mh:draft-folder) cur-msg mbuffer)
      (setf (buffer-writable mbuffer) writable)
      (get-message-buffer-window mbuffer))))

(defcommand "Edit Draft Buffer" (p)
  "Edit draft buffer."
  "Edit draft buffer."
  (declare (ignore p))
  (let* ((minfo (value message-information)))
    (or minfo (editor-error "Not in a message buffer."))
    (let* ((msgs (message-info-msgs minfo))
	   (mbuf (current-buffer))
	   (mbuf-name (buffer-name mbuf))
	   (writable (buffer-writable mbuf))
	   (abortp t))
      (when (consp msgs)
	(editor-error
	 "There appears to be more than one message in this buffer."))
      (unwind-protect
	  (progn
	    (setf (buffer-writable mbuf) t)
	    (setf (buffer-pathname mbuf)
		  (merge-pathnames
		   msgs
		   (mh:folder-pathname (message-info-folder minfo))))
	    (setf (buffer-major-mode mbuf) "Text")
	    (do-recursive-edit)
	    (setf abortp nil))
	(when (and (not abortp)
		   (buffer-modified mbuf)
		   (prompt-for-y-or-n
		    :prompt "Message buffer modified, save it? "
		    :default t))
	  (save-file-command nil mbuf))
	(setf (buffer-modified mbuf) nil)
	;; "Save File", which the user may have used, changes the buffer's name.
	(or (getstring mbuf-name *buffer-names*)
	    (setf (buffer-name mbuf) mbuf-name))
	(setf (buffer-writable mbuf) writable)
	(setf (buffer-pathname mbuf) nil)
	(setf (buffer-major-mode mbuf) "Message")))))

#| FIX first attempt, completes manually on single word
(defcommand "Complete or Space" (p)
  "Complete address when point is on first line, else indent."
  "Complete address when point is on first line, else indent."
  (declare (ignore p))
  (let ((point (current-point)))
    (if (line-previous (mark-line point))
	(insert-character point #\ )
	(let ((mark (copy-mark (current-point))))
	  (if (plusp (character-attribute :whitespace (next-character mark)))
	      (mark-before mark))
	  (if (plusp (character-attribute :word-delimiter
					  (next-character mark)))
	      (insert-character point #\ )
	      (let ((record (db:find-record (word-at-point mark))))
		(when record
		  (let ((emails (db:db-record-emails record))
			(name (db:db-record-full-name record)))
		    (when emails
		      (word-start mark)
		      (word-end point)
		      (kill-region (region mark point) :kill-forward)
		      (push-buffer-mark (copy-mark point) t)
		      (with-output-to-mark (stream mark)
			(if name
			    (format stream "\"~A\" <~A>" name (car emails))
			    (format stream "~A" (car emails)))))))))
	  (delete-mark mark)))))
|#

(defun insert-email (point record)
  "Insert at Point the email address in Record."
  (when record
    (let ((emails (db:db-record-emails record))
	  (name (db:db-record-full-name record)))
      (or emails (editor-error "FIX Add email."))
      (when emails
	(push-buffer-mark (copy-mark point) t)
	(with-output-to-mark (stream point)
	  (if (string= name "")
	      (format stream "~A" (car emails))
	      (format stream "\"~A\" <~A>" name (car emails))))))))

(defcommand "Draft Insert Space" (p)
  "Complete address when point is on first line, else insert a space."
  "Complete address when point is on first line, else insert a space."
  (declare (ignore p))
  (let ((point (current-point)))
    (if (or (line-previous (mark-line point))
	    (eq (previous-character point) #\"))
	(insert-character point #\ )
	(let ((mark (copy-mark point)))
	  (when (or (find-character mark #\; :backward t)
		    (find-character mark #\: :backward t))
	    (find-attribute mark :word-delimiter 'zerop)
	    (if (let ((pre (region-to-string (region mark point))))
		  (or (position #\" pre)
		      (position #\< pre)
		      (position #\> pre)))
		(insert-character point #\ )
		(progn
		  (or (find-character point #\;) (line-end point))
		  (when (mark< mark point)
		    (let ((string (region-to-string (region mark point))))
		      (multiple-value-bind
			  (prefix key record field ambig); last-field-p)
			  (complete-string string (list (db:get-db-table)))
			(declare (ignore field key))
			(if (eq (char string (1- (length string))) #\ )
			    (let ((*parse-help* "Email address")
				  (*parse-type* :keyword)
				  (*parse-string-tables* (list (db:get-db-table)))
				  (*parse-input-region* (region mark point)))
			      (help-on-parse-command nil)))
			(when prefix
			  (word-start mark)
			  (delete-region (region mark point))
			  (if ambig
			      (progn
				(insert-string mark prefix)
				(insert-character mark #\ ))
			      (insert-email point record))))))
		  (delete-mark mark))))))))

(defcommand "Draft New Line" (p)
  "Confirm address when point is on first line, else call New Line."
  "Confirm address when point is on first line, else call New Line."
  (declare (ignore p))
  (let ((point (current-point)))
    (if (line-previous (mark-line point))
	(new-line-command nil)
	(let ((mark (copy-mark point)))
	  (when (or (find-character mark #\; :backward t)
		    (find-character mark #\: :backward t))
	    (find-attribute mark :word-delimiter 'zerop)
	    (or (find-character point #\;) (line-end point))
	    (setf (mark-kind mark) :right-inserting)
	    (let* ((*parse-string-tables* (list (db:get-db-table)))
		   (*parse-value-must-exist* t)
		   (*parse-input-region* (region mark
						 (copy-mark point)))
		   (string (region-to-string *parse-input-region*)))
	      (let ((ret (edi::keyword-verification-function string)))
		(if (if ret
			(if (cadr ret)
			    (progn
			      (delete-region *parse-input-region*)
			      (insert-email point (cadr ret))
			      nil)
			    (if (eq (last-command-type)
				    :draft-addr-confirm)
				(string= (car ret) string)))
			(eq (last-command-type) :draft-addr-confirm))
		    ;; Ambiguous or completes (key :complete) to same
		    ;; string (FIX with point at same posn?)
		    (let ((mark2 (copy-mark point)))
		      (or (find-character mark2 #\;) (line-end mark2))
		      (let ((*parse-help* "Email address")
			    (*parse-type* :keyword)
			    (*parse-string-tables* (list (db:get-db-table)))
			    (*parse-input-region* (region mark mark2)))
			(help-on-parse-command nil)))))
	      (setf (last-command-type) :draft-addr-confirm)))))))

(defhvar "Message Header Body Separator"
  "String that separates the header and body of a message."
  :value "--------")

(defcommand "Correct Message Spelling" (p)
  "Correct spelling over mail, skipping cited lines."
  "Correct spelling over mail, skipping cited lines."
  (declare (ignore p))
  (let ((start (copy-mark (buffer-start-mark (current-buffer)))))
    (or (find-pattern start
		      (get-search-pattern (value message-header-body-separator)
					  :forward))
	(editor-error "Failed to find start of message body."))
    (correct-buffer-spelling-command t start)))

(defcommand "Check and Deliver Message" (p)
  "Correct message spelling, then call Deliver Message."
  "Correct message spelling, then call Deliver Message."
  (declare (ignore p))
  (correct-message-spelling-command nil)
  (deliver-message-command nil)
  (rotate-buffers-forward-command nil)
  (refresh-all-headers-command nil) ;; In case in draft buffer.
  (message "Message checked and sent."))


;;;; Message and Draft Stuff.

;; FIX other var same name
(defhvar "Message Headers"
  "List of message headers to display in message buffers, or T for all."
  :value '("Resent-To" "To" "Resent-Cc" "Cc"
	   "Resent-From" "From" "Reply-To" "Date" "Subject"
	   "Resent-Date"))

(defhvar "Message Headers Alternate"
  "List of alternate message headers to display in message buffers, or T
   for all."
  :value t)

(defhvar "Headers Buffer"
  "This is bound in message and draft buffers to their associated headers
   buffer."
  :value nil)

(defcommand "Goto Headers Buffer" (p)
  "Selects associated headers buffer if it exists, else rotate buffers.
   The headers buffer's point is moved to the appropriate line, pushing a
   buffer mark where point was."
  "Selects associated headers buffer if it exists, else rotates buffers."
  (declare (ignore p))
  (let ((h-buf (value headers-buffer)))
    (if h-buf
	(if (buffer-windows h-buf)
	    (delete-window-command nil)
	    (let ((info (or (value message-information) (value draft-information))))
	      (change-to-buffer h-buf)
	      (push-buffer-mark (copy-mark (current-point)))
	      (move-mark (current-point) (message/draft-info-headers-mark info))))
	(rotate-buffers-forward-command ()))))

(defhvar "Message Buffer"
  "This is bound in draft buffers to their associated message buffer."
  :value nil)

(defcommand "Goto Message Buffer" (p)
  "Selects associated message buffer if it exists."
  "Selects associated message buffer if it exists."
  (declare (ignore p))
  (let ((msg-buf (value message-buffer)))
    (or msg-buf (editor-error "No associated message buffer."))
    (change-to-buffer msg-buf)))


(defhvar "Message Insertion Prefix"
  "This is a fill prefix that is used when inserting text from a message buffer
   into a draft buffer by \"Insert Message Region\".  It defaults to three
   spaces."
  :value "   ")

(defhvar "Message Insertion Column"
  "This is a fill column that is used when inserting text from a message buffer
   into a draft buffer by \"Insert Message Region\"."
  :value 75)

(defcommand "Insert Message Region" (p)
  "Copy the current region into the associated draft or post buffer.  When
   in a message buffer that has an associated draft or post buffer, the
   current active region is copied into the draft or post buffer.  It is
   filled using \"Message Insertion Prefix\" and \"Message Insertion
   Column\".  If an argument is supplied, the filling is inhibited.
   If both a draft buffer and post buffer are associated with this, then it
   is inserted into the draft buffer."
  "When in a message buffer that has an associated draft or post buffer,
   the current active region is copied into the post or draft buffer.  It is
   filled using \"Message Insertion Prefix\" and \"Message Insertion
   Column\".  If an argument is supplied, the filling is inhibited."
  (let* ((minfo (value message-information))
	 (nm-info (if (editor-bound-p 'netnews-message-info)
		      (value netnews-message-info)))
	 (post-buffer (and nm-info (nm-info-post-buffer nm-info)))
	 (post-info (and post-buffer
			 (variable-value 'post-info :buffer post-buffer)))
	 dbuf kind)
    (cond (minfo
	   (setf kind :mail)
	   (setf dbuf (message-info-draft-buf minfo)))
	  (nm-info
	   (setf kind :netnews)
	   (setf dbuf (or (nm-info-draft-buffer nm-info)
			  (nm-info-post-buffer nm-info))))
	  (t (editor-error "Not in a netnews message or message buffer.")))
    (or dbuf
	(editor-error "Message buffer not associated with any draft or post ~
		       buffer."))
    (let* ((region (copy-region (current-region)))
	   (dbuf-point (buffer-point dbuf))
	   (dbuf-mark (copy-mark dbuf-point)))
      (cond ((and (eq kind :mail)
		  (editor-bound-p 'split-window-draft :buffer dbuf)
		  (> (length (the list *window-list*)) 2)
		  (buffer-windows dbuf))
	     (setf (current-buffer) dbuf
		   (current-window) (car (buffer-windows dbuf))))
	    ((and (eq kind :netnews)
		  (and (member (post-info-message-window post-info)
			       *window-list*)
		       (member (post-info-reply-window post-info)
			       *window-list*)))
	     (setf (current-buffer) dbuf
		   (current-window) (post-info-reply-window post-info)))
	    (t (change-to-buffer dbuf)))
      (push-buffer-mark dbuf-mark)
      (ninsert-region dbuf-point region)
      (or p
	  (fill-region-by-paragraphs (region dbuf-mark dbuf-point)
				     (value message-insertion-prefix)
				     (value message-insertion-column)))))
  (setf (last-command-type) :ephemerally-active))


(defhvar "Message Buffer Insertion Prefix"
  "This is a line prefix that is inserted at the beginning of every line in
   a message buffer when inserting those lines into a draft buffer with
   \"Insert Message Buffer\".  It defaults to four spaces."
  :value "    ")

(defcommand "Insert Message Buffer" (p)
  "Insert entire (associated) message buffer into (associated) draft or
   post buffer.  When in a draft or post buffer with an associated message
   buffer, or when in a message buffer that has an associated draft or post
   buffer, the message buffer is inserted into the draft buffer.  When
   there are both an associated draft and post buffer, the text is inserted
   into the draft buffer.  Each inserted line is modified by prefixing it
   with \"Message Buffer Insertion Prefix\".  If an argument is supplied
   the prefixing is inhibited."
  "When in a draft or post buffer with an associated message buffer, or
   when in a message buffer that has an associated draft or post buffer, the
   message buffer is inserted into the draft buffer.  Each inserted line is
   modified by prefixing it with \"Message Buffer Insertion Prefix\".  If an
   argument is supplied the prefixing is inhibited."
  (let ((minfo (value message-information))
	(dinfo (value draft-information))
	mbuf dbuf message-kind)
    (cond (minfo
	   (setf message-kind :mail)
	   (setf dbuf (message-info-draft-buf minfo))
	   (or dbuf
	       (editor-error
		"Message buffer not associated with any draft buffer."))
	   (setf mbuf (current-buffer))
	   (change-to-buffer dbuf))
	  (dinfo
	   (setf message-kind :mail)
	   (setf mbuf (value message-buffer))
	   (or mbuf
	       (editor-error
		"Draft buffer not associated with any message buffer."))
	   (setf dbuf (current-buffer)))
	  ((editor-bound-p 'netnews-message-info)
	   (setf message-kind :netnews)
	   (setf mbuf (current-buffer))
	   (let ((nm-info (value netnews-message-info)))
	     (setf dbuf (or (nm-info-draft-buffer nm-info)
			    (nm-info-post-buffer nm-info)))
	     (or dbuf
		 (editor-error "Message buffer not associated with any draft ~
				or post buffer.")))
	   (change-to-buffer dbuf))
	  ((editor-bound-p 'post-info)
	   (setf message-kind :netnews)
	   (let ((post-info (value post-info)))
	     (setf mbuf (post-info-message-buffer post-info))
	     (or mbuf
		 (editor-error "Post buffer not associated with any message ~
				buffer.")))
	   (setf dbuf (current-buffer)))
	  (t (editor-error "Not in a draft, message, news-message, or post ~
	                    buffer.")))
    (let* ((dbuf-point (buffer-point dbuf))
	   (dbuf-mark (copy-mark dbuf-point)))
      (push-buffer-mark dbuf-mark)
      (insert-region dbuf-point (buffer-region mbuf))
      (or p
	  (let ((prefix (value message-buffer-insertion-prefix)))
	    (with-mark ((temp dbuf-mark :left-inserting))
	      (loop
		(when (mark>= temp dbuf-point) (return))
		(insert-string temp prefix)
		(or (line-offset temp 1 0) (return)))))))
    (ecase message-kind
      (:mail
       (insert-message-buffer-cleanup-split-draft dbuf mbuf))
      (:netnews
       (nn-reply-cleanup-split-windows dbuf))))
  (setf (last-command-type) :ephemerally-active))

;;; INSERT-MESSAGE-BUFFER-CLEANUP-SPLIT-DRAFT tries to delete an extra window
;;; due to "Reply to Message in Other Window".  Since we just inserted the
;;; message buffer in the draft buffer, we don't need the other window into
;;; the message buffer.
;;;
(defun insert-message-buffer-cleanup-split-draft (dbuf mbuf)
  (when (and (editor-bound-p 'split-window-draft :buffer dbuf)
	     (> (length (the list *window-list*)) 2))
    (let ((win (car (buffer-windows mbuf))))
      (cond
       (win
	(when (eq win (current-window))
	  (let ((dwin (car (buffer-windows dbuf))))
	    (or dwin
		(editor-error "Couldn't fix windows for split window draft."))
	    (setf (current-buffer) dbuf)
	    (setf (current-window) dwin)))
	(delete-window win))
       (t ;; This happens when invoked with the message buffer current.
	(let ((dwins (buffer-windows dbuf)))
	  (when (> (length (the list dwins)) 1)
	    (delete-window (find-if #'(lambda (w)
					(not (eq w (current-window))))
				    dwins)))))))
    (delete-variable 'split-window-draft :buffer dbuf)))


;;; CLEANUP-MESSAGE-BUFFER is called when a buffer gets deleted.  It cleans
;;; up references to a message buffer.
;;;
(defun cleanup-message-buffer (buffer)
  (let ((minfo (variable-value 'message-information :buffer buffer)))
    (when (editor-bound-p 'headers-buffer :buffer buffer)
      (let* ((hinfo (variable-value 'headers-information
				    :buffer (variable-value 'headers-buffer
							    :buffer buffer)))
	     (msg-buf (headers-info-msg-buffer hinfo)))
	(if (eq msg-buf buffer)
	    (setf (headers-info-msg-buffer hinfo) nil)
	    (setf (headers-info-other-msg-bufs hinfo)
		  (delete buffer (headers-info-other-msg-bufs hinfo)
			  :test #'eq))))
      (delete-mark (message-info-headers-mark minfo))
      ;;
      ;; Do this for MAYBE-MAKE-MH-BUFFER since it isn't necessary for GC.
      (delete-variable 'headers-buffer :buffer buffer))
    (when (message-info-draft-buf minfo)
      (delete-variable 'message-buffer
		       :buffer (message-info-draft-buf minfo)))))

;;; CLEANUP-DRAFT-BUFFER is called when a buffer gets deleted.  It cleans
;;; up references to a draft buffer.
;;;
(defun cleanup-draft-buffer (buffer)
  (let ((dinfo (variable-value 'draft-information :buffer buffer)))
    (when (editor-bound-p 'headers-buffer :buffer buffer)
      (let* ((hinfo (variable-value 'headers-information
				    :buffer (variable-value 'headers-buffer
							    :buffer buffer))))
	(setf (headers-info-draft-bufs hinfo)
	      (delete buffer (headers-info-draft-bufs hinfo) :test #'eq))
	(delete-mark (draft-info-headers-mark dinfo))))
    (when (editor-bound-p 'message-buffer :buffer buffer)
      (setf (message-info-draft-buf
	     (variable-value 'message-information
			     :buffer (variable-value 'message-buffer
						     :buffer buffer)))
	    nil))))

;;; CLEANUP-HEADERS-BUFFER is called when a buffer gets deleted.  It cleans
;;; up references to a headers buffer.
;;;
(defun cleanup-headers-buffer (buffer)
  (let* ((hinfo (variable-value 'headers-information :buffer buffer))
	 (msg-buf (headers-info-msg-buffer hinfo)))
    (when msg-buf
      (cleanup-headers-reference
       msg-buf (variable-value 'message-information :buffer msg-buf)))
    (dolist (b (headers-info-other-msg-bufs hinfo))
      (cleanup-headers-reference
       b (variable-value 'message-information :buffer b)))
    (dolist (b (headers-info-draft-bufs hinfo))
      (cleanup-headers-reference
       b (variable-value 'draft-information :buffer b)))))

(defun cleanup-headers-reference (buffer info)
  (delete-mark (message/draft-info-headers-mark info))
  (setf (message/draft-info-headers-mark info) nil)
  (delete-variable 'headers-buffer :buffer buffer)
  (when (typep info 'draft-info)
    (setf (draft-info-replied-to-folder info) nil)
    (setf (draft-info-replied-to-msg info) nil)))

;;; REVAMP-HEADERS-BUFFER cleans up a headers buffer for immediate re-use.
;;; After deleting the buffer's region, there will be one line in the
;;; buffer because of how the editor regions work, so we have to delete
;;; that line's plist.  Then we clean up any references to the buffer and
;;; delete the main message buffer.  The other message buffers are left
;;; alone assuming they are on the "others" list because they are being
;;; used in some particular way (for example, a draft buffer refers to one
;;; or the user has kept it).  Then some slots of the info structure are
;;; set to nil.
;;;
(defun revamp-headers-buffer (hbuffer hinfo)
  (delete-region (buffer-region hbuffer))
  (setf (line-plist (mark-line (buffer-point hbuffer))) nil)
  (let ((msg-buf (headers-info-msg-buffer hinfo)))
    ;; Deleting the buffer sets the slot to nil.
    (when msg-buf (delete-buffer-if-possible msg-buf))
    (cleanup-headers-buffer hbuffer))
  (setf (headers-info-other-msg-bufs hinfo) nil)
  (setf (headers-info-draft-bufs hinfo) nil)
  (setf (headers-info-msg-seq hinfo) nil)
  (setf (headers-info-msg-strings hinfo) nil))

(defcommand "Append File" (p)
  "Append a file at the end of the buffer."
  "Append a file at the end of the buffer."
  (declare (ignore p))
  (let ((name (prompt-for-file
	       :default (buffer-default-pathname (current-buffer))
	       :prompt "Append file: "
	       :help "Name of file to append to message"))
	(point (current-point)))
    (buffer-end point)
    (mark-before point)
    (or (blank-line-p (mark-line point))
	(insert-character point #\newline))
    (mark-after point)
    (with-output-to-mark (stream point)
      (format stream
	      "~&== File ~A ====================================================~%"
	      name)
      ;; FIX insert file manually, and add undo
      (insert-file-command nil name)
      (format stream
	      "== End of file ~A =============================================~%"
	      name))))

(defcommand "Attach File" (p)
  "Insert file attachment clause at point."
  "Insert file attachment clause at point."
  (declare (ignore p))
  (let ((name (prompt-for-file
	       :default (buffer-default-pathname (current-buffer))
	       :prompt "Attach file: "
	       :help "Name of file to attach to message."))
	(type (prompt-for-string
	       :default "text/plain"
	       :prompt "Content type: "
	       :help "MIME type of the attachment."))
	(description (prompt-for-string
		      :prompt "Description: "
		      :help "One line description of the attachment."))
	(mark (copy-mark (current-point))))
    (or (blank-line-p (mark-line mark))
	(insert-character mark #\newline))
    (with-output-to-mark (stream mark)
      (format stream "#~A [~A] ~A" type description name))))


;;;; Incorporating new mail.

(defhvar "New Mail Folder"
  "This is the folder new mail is incorporated into."
  :value "+inbox")

(defcommand "Incorporate New Mail" (p)
  "Incorporates new mail into \"New Mail Folder\", displaying INC output in
   a pop-up window."
  "Incorporates new mail into \"New Mail Folder\", displaying INC output in
   a pop-up window."
  (declare (ignore p))
  (with-pop-up-display (s)
    (incorporate-new-mail s)))

(defhvar "Unseen Headers Message Spec"
  "This is an MH message spec suitable for any message prompt.  It is used
   to supply headers for the unseen headers buffer, in addition to the
   unseen-sequence name that is taken from the user's MH profile, when
   incorporating new mail and after expunging.  This value is a string."
  :value nil)

(defcommand "Incorporate and Read New Mail" (p)
  "Incorporates new mail and generates a headers buffer.
   Incorporates new mail into \"New Mail Folder\", and creates a headers buffer
   with the new messages.  To use this, you must define an unseen- sequence in
   your profile.  Each time this is invoked the unseen-sequence is SCAN'ed, and
   the headers buffer's contents are replaced."
  "Incorporates new mail into \"New Mail Folder\", and creates a headers
   buffer with the new messages.  This buffer will be appended to with
   successive uses of this command."
  (declare (ignore p))
  (let ((unseen-seq (mh:profile-component "unseen-sequence")))
    (or unseen-seq
	(editor-error "No unseen-sequence defined in MH profile."))
    (incorporate-new-mail)
    (let* ((folder (value new-mail-folder))
	   ;; Stash current message before fetching unseen headers.
	   (cur-msg (mh:current-message folder))
	   (region (get-new-mail-msg-hdrs folder unseen-seq)))
      ;; Fetch message headers before possibly making buffer in case we
      ;; error.
      (or (and *new-mail-buffer*
	       (member *new-mail-buffer* *buffer-list* :test #'eq))
	  (let ((name (format nil "Unseen Headers ~A" folder)))
	    (when (getstring name *buffer-names*)
	      (editor-error "There already is a buffer named ~S!" name))
	    (setf *new-mail-buffer*
		  (make-buffer name :modes (list "Headers")
			       :delete-hook '(new-mail-buf-delete-hook)))
	    (setf (buffer-writable *new-mail-buffer*) nil)))
      (cond ((editor-bound-p 'headers-information
			     :buffer *new-mail-buffer*)
	     (let ((hinfo (variable-value 'headers-information
					  :buffer *new-mail-buffer*)))
	       (or (string= (headers-info-folder hinfo) folder)
		   (editor-error
		    "An unseen headers buffer already exists but into another ~
		     folder.  Your mail has already been incorporated into the ~
		     specified folder."))
	       (with-writable-buffer (*new-mail-buffer*)
		 (revamp-headers-buffer *new-mail-buffer* hinfo))
	       ;; Restore the name in case someone used "Pick Headers".
	       (setf (buffer-name *new-mail-buffer*)
		     (format nil "Unseen Headers ~A" folder))
	       (insert-new-mail-message-headers hinfo region cur-msg)))
	    (t
	     (let ((hinfo (make-headers-info :buffer *new-mail-buffer*
					     :folder folder)))
	       (defhvar "Headers Information"
		 "This holds the information about the current headers buffer."
		 :value hinfo :buffer *new-mail-buffer*)
	       (insert-new-mail-message-headers hinfo region cur-msg)))))))

;;; NEW-MAIL-BUF-DELETE-HOOK is invoked whenever the new mail buffer is
;;; deleted.
;;;
(defun new-mail-buf-delete-hook (buffer)
  (declare (ignore buffer))
  (setf *new-mail-buffer* nil))

;;; GET-NEW-MAIL-MSG-HDRS takes a folder and the unseen-sequence name.  It
;;; returns a region with the unseen message headers and any headers due to
;;; the "Unseen Headers Message Spec" variable.
;;;
(defun get-new-mail-msg-hdrs (folder unseen-seq)
  (let* ((unseen-headers-message-spec (value unseen-headers-message-spec))
	 (other-msgs (if unseen-headers-message-spec
			 (breakup-message-spec
			  (string-trim '(#\space #\tab)
				       unseen-headers-message-spec))))
	 (msg-spec (cond ((null other-msgs)
			  (list unseen-seq))
			 ((member unseen-seq other-msgs :test #'string=)
			  other-msgs)
			 (t (cons unseen-seq other-msgs)))))
    (message-headers-to-region folder msg-spec)))

;;; INSERT-NEW-MAIL-MESSAGE-HEADERS inserts region in the new mail buffer.
;;; Then we look for the header line with cur-msg id, moving point there.
;;; There may have been unseen messages before incorporating new mail, and
;;; cur-msg should be the first new message.  Then we either switch to the
;;; new mail headers, or show the current message.
;;;
(defun insert-new-mail-message-headers (hinfo region cur-msg)
  (declare (simple-string cur-msg))
  (with-writable-buffer (*new-mail-buffer*)
    (insert-message-headers *new-mail-buffer* hinfo region))
  (let ((point (buffer-point *new-mail-buffer*)))
    (buffer-start point)
    (with-headers-mark (cur-mark *new-mail-buffer* cur-msg)
      (move-mark point cur-mark)))
  (change-to-buffer *new-mail-buffer*))


(defhvar "Incorporate New Mail Hook"
  "Functions on this hook are invoked immediately after new mail is
   incorporated."
  :value nil)

(defhvar "Mail Drops"
  "The list list of mail drops." ;; FIX explain adding
  :value ())

(defvar *new-mail-p* ()
  "Set to t in `new-mail-p' when there is mail.  Cleared when mail is
   incorporated.  Used for :mail modeline field.")

(defun incorporate-new-mail (&optional stream)
  "Incorporates new mail, passing INC's output to stream.  When stream is
   (), output is flushed."
  (or (new-mail-p) (editor-error "Any mail already in."))
  (message "Incorporating new mail ...")
  (when (mh:incorporate (value mail-drops) (value new-mail-folder) stream)
    (setq *new-mail-p* ()))
  (when (value incorporate-new-mail-hook)
    (message "Invoking new mail hooks ...")
    (invoke-hook incorporate-new-mail-hook)))

;; FIX ~ Auto Refile Mail Rule? Refile Headers rule.
(defhvar "Split Mail Rule"
  "A list of (\"folder\" pick-expression) lists, to control mail
   splitting."
  :value ())

(defun symbols-to-keywords (list)
  "Returns a list like List with symbols converted to keywords."
  "Returns a list like List with symbols converted to keywords."
  (typecase list
    (symbol (lisp::make-keyword list))
    (string list)
    (list  (mapcar 'symbols-to-keywords list))
    (t list)))

(defcommand "Split Mail" (p &optional (folder (value new-mail-folder)))
  "Split mail in folder into folders, according to \"Split Mail Rule\"."
  "Split mail in folder into folders, according to \"Split Mail Rule\"."
  (declare (ignore p))
  (let ((rules (value split-mail-rule)))
    (when rules
      (message "Splitting mail from ~A..." folder)
      (mh:split-messages folder rules #'refile-message)
      (message "... done."))))


;;;; Deletion.

(defhvar "Virtual Message Deletion"
  "When set, \"Delete Message\" merely MARK's a message into the
   \"edtrash\" sequence; otherwise, the message is deleted."
  :value t)

(defcommand "Delete Message and Show Next" (p)
  "Delete message and show next undeleted message.
   This command is only valid in a headers buffer or a message buffer
   associated with some headers buffer.  The current message is deleted, and
   the next undeleted one is shown."
  "Delete the current message and show the next undeleted one."
  (declare (ignore p))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (delete-message (headers-info-folder hinfo) cur-msg)))
	  (minfo
	   (delete-message (message-info-folder minfo)
			   (message-info-msgs minfo)))
	  (t
	   (editor-error "Not in a headers or message buffer."))))
  (show-message-offset 1 :undeleted))

;;; "Delete Message" unlike "Headers Delete Message" cannot know for sure
;;; which message id's have been deleted, so when virtual message deletion
;;; is not used, we cannot use DELETE-HEADERS-BUFFER-LINE to keep headers
;;; buffers consistent.  However, the message id's in the buffer (if deleted)
;;; will generate MH errors if operations are attempted with them, and
;;; if the user ever packs the folder with "Expunge Messages", the headers
;;; buffer will be updated.
;;;
(defcommand "Delete Message" (p)
  "Prompts for a folder, messages to delete, and pick expression.  When in
   a headers buffer into the same folder specified, the messages prompt
   defaults to those messages in the buffer; \"all\" may be entered if this is
   not what is desired.  When \"Virtual Message Deletion\" is set, messages are
   only MARK'ed for deletion.  See \"Expunge Messages\".  When this feature is
   not used, headers and message buffers message id's my not be consistent
   with MH."
  "Prompts for a folder and message to delete.  When \"Virtual Message
   Deletion\" is set, messages are only MARK'ed for deletion.  See \"Expunge
   Messages\"."
  (declare (ignore p))
  (let* ((folder (prompt-for-folder))
	 (hinfo (value headers-information))
	 (temp-msgs (prompt-for-message
		     :folder folder
		     :messages
		     (if (and hinfo
			      (string= folder
				       (the simple-string
					    (headers-info-folder hinfo))))
			 (headers-info-msg-strings hinfo))
		     :prompt "MH messages to pick from: "))
	 (pick-exp (prompt-for-pick-expression))
	 (msgs (mh:pick-messages folder temp-msgs pick-exp))
	 (virtually (value virtual-message-deletion)))
    (declare (simple-string folder))
    (if virtually
	(mh:mark-messages folder msgs "edtrash" :add)
	(delete-messages folder msgs ()))
    (if virtually
	(let ((deleted-seq (mh:sequence-list folder "edtrash")))
	  (when deleted-seq
	    (do-headers-buffers (hbuf folder)
	      (with-writable-buffer (hbuf)
		(note-deleted-headers hbuf deleted-seq)))))
	(do-headers-buffers (hbuf folder hinfo)
	  (do-headers-lines (hbuf :line-var line :mark-var hmark)
	    (when (member (line-message-id line) msgs :test #'string=)
	      (delete-headers-buffer-line hinfo hmark)))))))

(defcommand "Headers Delete Message" (p)
  "Delete current message.
   When in a headers buffer, deletes the message on the current line.  When
   in a message buffer, deletes that message.  When \"Virtual Message
   Deletion\" is set, messages are only MARK'ed for deletion.  See \"Expunge
   Messages\"."
  "When in a headers buffer, deletes the message on the current line.  When
   in a message buffer, deletes that message.  When \"Virtual Message
   Deletion\" is set, messages are only MARK'ed for deletion.  See \"Expunge
   Messages\"."
  (declare (ignore p))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (delete-message (headers-info-folder hinfo) cur-msg)))
	  (minfo
	   (let ((msgs (message-info-msgs minfo)))
	     (delete-message (message-info-folder minfo)
			     (if (consp msgs) (car msgs) msgs)))
	   (message "Message deleted."))
	  (t (editor-error "Not in a headers or message buffer.")))))

(defcommand "Headers Delete Message and Down Line" (p)
  "Delete the current message, moving point to the next line."
  "Delete current message and moves point down a line."
  (declare (ignore p))
  (let ((hinfo (value headers-information)))
    (or hinfo (editor-error "Not in a headers buffer."))
    (multiple-value-bind (cur-msg cur-mark)
			 (headers-current-message hinfo)
      (or cur-msg (editor-error "Not on a header line."))
      (delete-message (headers-info-folder hinfo) cur-msg)
      (when (line-offset cur-mark 1)
	(or (blank-line-p (mark-line cur-mark))
	    (move-mark (current-point) cur-mark)))
      (delete-mark cur-mark))))

;;; DELETE-MESSAGE takes a folder and message id and either flags this message
;;; for deletion or deletes it.  All headers buffers into folder are updated,
;;; either by flagging a headers line or deleting it.
;;;
(defun delete-message (folder msg)
  (cond ((value virtual-message-deletion)
	 (mh:mark-message folder msg "edtrash" :add)
	 (do-headers-buffers (hbuf folder)
	   (with-headers-mark (hmark hbuf msg)
	     (with-writable-buffer (hbuf)
	       (note-deleted-message-at-mark hmark)))))
	(t (delete-messages folder msg ())
	   (do-headers-buffers (hbuf folder hinfo)
	     (with-headers-mark (hmark hbuf msg)
	       (delete-headers-buffer-line hinfo hmark)))))
  (dolist (b *buffer-list*)
    (when (and (editor-bound-p 'message-information :buffer b)
	       (buffer-modeline-field-p b :deleted-message))
      (dolist (w (buffer-windows b))
	(update-modeline-field b w :deleted-message)))))

;;; NOTE-DELETED-MESSAGE-AT-MARK takes a mark at the beginning of a valid
;;; headers line, sticks a "D" on the line, and frobs the line's deleted
;;; property.  This assumes the headers buffer is modifiable.
;;;
(defun note-deleted-message-at-mark (mark)
  (find-attribute mark :digit)
  (find-attribute mark :digit #'zerop)
  (character-offset mark 2)
  (setf (next-character mark) #\D)
  (setf (line-message-deleted (mark-line mark)) t))

;;; DELETE-HEADERS-BUFFER-LINE takes a headers information and a mark on the
;;; line to be deleted.  Before deleting the line, we check to see if any
;;; message or draft buffers refer to the buffer because of the line.  Due
;;; to how regions are deleted, line plists get messed up, so they have to
;;; be regenerated.  We regenerate them for the whole buffer, so we don't have
;;; to hack the code to know which lines got messed up.
;;;
(defun delete-headers-buffer-line (hinfo hmark)
  (delete-headers-line-references hinfo hmark)
  (let ((id (line-message-id (mark-line hmark)))
	(hbuf (headers-info-buffer hinfo)))
    (with-writable-buffer (hbuf)
      (with-mark ((end (line-start hmark) :left-inserting))
	(or (line-offset end 1 0) (buffer-end end))
	(delete-region (region hmark end))))
    (let ((seq (mh:sequence-delete id (headers-info-msg-seq hinfo))))
      (setf (headers-info-msg-seq hinfo) seq)
      (setf (headers-info-msg-strings hinfo) (mh:sequence-strings seq)))
    (set-message-headers-ids hbuf)
    (when (value virtual-message-deletion)
      (let ((deleted-seq (mh:sequence-list (headers-info-folder hinfo)
					   "edtrash")))
	(do-headers-lines (hbuf :line-var line)
	  (setf (line-message-deleted line)
		(mh:sequence-member-p (line-message-id line) deleted-seq)))))))


;;; DELETE-HEADERS-LINE-REFERENCES removes any message buffer or draft buffer
;;; pointers to a headers buffer or marks into the headers buffer.  Currently
;;; message buffers and draft buffers are identified differently for no good
;;; reason; probably message buffers should be located in the same way draft
;;; buffers are.  Also, we currently assume only one of other-msg-bufs could
;;; refer to the line (similarly for draft-bufs), but this might be bug
;;; prone.  The message buffer case couldn't happen since the buffer name
;;; would cause MAYBE-MAKE-MH-BUFFER to re-use the buffer, but you could reply
;;; to the same message twice simultaneously.
;;;
(defun delete-headers-line-references (hinfo hmark)
  (let ((msg-id (line-message-id (mark-line hmark)))
	(main-msg-buf (headers-info-msg-buffer hinfo)))
    (declare (simple-string msg-id))
    (when main-msg-buf
      (let ((minfo (variable-value 'message-information :buffer main-msg-buf)))
	(when (string= (the simple-string (message-info-msgs minfo))
		       msg-id)
	  (cond ((message-info-draft-buf minfo)
		 (cleanup-headers-reference main-msg-buf minfo)
		 (setf (headers-info-msg-buffer hinfo) nil))
		(t (delete-buffer-if-possible main-msg-buf))))))
    (dolist (mbuf (headers-info-other-msg-bufs hinfo))
      (let ((minfo (variable-value 'message-information :buffer mbuf)))
	(when (string= (the simple-string (message-info-msgs minfo))
		       msg-id)
	  (cond ((message-info-draft-buf minfo)
		 (cleanup-headers-reference mbuf minfo)
		 (setf (headers-info-other-msg-bufs hinfo)
		       (delete mbuf (headers-info-other-msg-bufs hinfo)
			       :test #'eq)))
		(t (delete-buffer-if-possible mbuf)))
	  (return)))))
  (dolist (dbuf (headers-info-draft-bufs hinfo))
    (let ((dinfo (variable-value 'draft-information :buffer dbuf)))
      (when (same-line-p (draft-info-headers-mark dinfo) hmark)
	(cleanup-headers-reference dbuf dinfo)
	(setf (headers-info-draft-bufs hinfo)
	      (delete dbuf (headers-info-draft-bufs hinfo) :test #'eq))
	(return)))))


(defcommand "Undelete Message" (p)  ;; Restore?
  "Prompts for a folder, messages to undelete, and pick expression.  When in
   a headers buffer into the same folder specified, the messages prompt
   defaults to those messages in the buffer; \"all\" may be entered if this is
   not what is desired.  This command is only meaningful if you have
   \"Virtual Message Deletion\" set."
  "Prompts for a folder, messages to undelete, and pick expression.  When in
   a headers buffer into the same folder specified, the messages prompt
   defaults to those messages in the buffer; \"all\" may be entered if this is
   not what is desired.  This command is only meaningful if you have
   \"Virtual Message Deletion\" set."
  (declare (ignore p))
  (or (value virtual-message-deletion)
      (editor-error "Only possible with virtual message deletion."))
  (let* ((folder (prompt-for-folder))
	 (hinfo (value headers-information))
	 (temp-msgs (prompt-for-message
		     :folder folder
		     :messages
		     (if (and hinfo
			      (string= folder
				       (the simple-string
					    (headers-info-folder hinfo))))
			 (headers-info-msg-strings hinfo))
		     :prompt "Messages to pick from: "))
	 (pick-exp (prompt-for-pick-expression))
	 (msgs (or (if pick-exp
		       (mh:pick-messages folder temp-msgs pick-exp))
		   temp-msgs)))
    (declare (simple-string folder))
    (mh:mark-messages folder msgs "edtrash" :delete)
    (let ((deleted-seq (mh:sequence-list folder "edtrash")))
      (do-headers-buffers (hbuf folder)
	(with-writable-buffer (hbuf)
	  (do-headers-lines (hbuf :line-var line :mark-var hmark)
	    (when (and (line-message-deleted line)
		       (not (mh:sequence-member-p (line-message-id line)
						  deleted-seq)))
	      (note-undeleted-message-at-mark hmark))))))))

(defcommand "Headers Undelete Message" (p)
  "Undelete the current message.
   When in a headers buffer, undeletes the message on the current line.  When
   in a message buffer, undeletes that message.  This command is only
   meaningful if you have \"Virtual Message Deletion\" set."
  "When in a headers buffer, undeletes the message on the current line.  When
   in a message buffer, undeletes that message.  This command is only
   meaningful if you have \"Virtual Message Deletion\" set."
  (declare (ignore p))
  (or (value virtual-message-deletion)
      (editor-error "You don't use virtual message deletion."))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (undelete-message (headers-info-folder hinfo) cur-msg)))
	  (minfo
	   (undelete-message (message-info-folder minfo)
			     (message-info-msgs minfo))
	   (message "Message undeleted."))
	  (t (editor-error "Not in a headers or message buffer.")))))

(defcommand "Headers Undelete Message and Down Line" (p)
  "Undelete the current message.  When in a headers buffer, undeletes the
   message on the current line and moves down a line.  When in a message
   buffer, undeletes that message.  This command is only meaningful if you
   have \"Virtual Message Deletion\" set."
  "When in a headers buffer, undeletes the message on the current line.  When
   in a message buffer, undeletes that message.  This command is only
   meaningful if you have \"Virtual Message Deletion\" set."
  (declare (ignore p))
  (or (value virtual-message-deletion)
      (editor-error "You don't use virtual message deletion."))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (undelete-message (headers-info-folder hinfo) cur-msg)
	     (when (line-offset cur-mark 1)
	       (or (blank-line-p (mark-line cur-mark))
		   (move-mark (current-point) cur-mark)))
	     (delete-mark cur-mark)))
	  (minfo
	   (undelete-message (message-info-folder minfo)
			     (message-info-msgs minfo))
	   (message "Message undeleted."))
	  (t (editor-error "Not in a headers or message buffer.")))))

;;; UNDELETE-MESSAGE takes a folder and a message id.  All headers buffers into
;;; folder are updated.
;;;
(defun undelete-message (folder msg)
  (mh:mark-message folder msg "edtrash" :delete)
  (do-headers-buffers (hbuf folder)
    (with-headers-mark (hmark hbuf msg)
      (with-writable-buffer (hbuf)
	(note-undeleted-message-at-mark hmark))))
  (dolist (b *buffer-list*)
    (when (and (editor-bound-p 'message-information :buffer b)
	       (buffer-modeline-field-p b :deleted-message))
      (dolist (w (buffer-windows b))
	(update-modeline-field b w :deleted-message)))))

;;; NOTE-UNDELETED-MESSAGE-AT-MARK takes a mark at the beginning of a valid
;;; headers line, sticks a space on the line in place of a "D", and frobs the
;;; line's deleted property.  This assumes the headers buffer is modifiable.
;;;
(defun note-undeleted-message-at-mark (hmark)
  (find-attribute hmark :digit)
  (find-attribute hmark :digit #'zerop)
  (character-offset hmark 2)
  (setf (next-character hmark) #\space)
  (setf (line-message-deleted (mark-line hmark)) nil))


(defcommand "Expunge Messages" (p)
  "Expunge messages marked for deletion.  Prompt for a folder, deleting the
   messages in the \"edtrash\" sequence after asking the user for
   confirmation.  Setting \"Expunge Messages Confirm\" to nil inhibits
   prompting.  Pack the messages in the folder.  When in a headers buffer,
   use that folder.  When in a message buffer, use its folder, updating any
   associated headers buffer.  When \"Temporary Draft Folder\" is bound,
   delete and expunge this folder's messages."
  "Prompt for a folder, deleteing the messages in the \"edtrash\" sequence
   and packing the message id's.  When in a headers buffer, uses that
   folder."
  (declare (ignore p))
  (let* ((hinfo (value headers-information))
	 (minfo (value message-information))
	 (folder (cond (hinfo (headers-info-folder hinfo))
		       (minfo (message-info-folder minfo))
		       (t (prompt-for-folder))))
	 (deleted-seq (mh:sequence-list folder "edtrash")))
    ;;
    ;; Delete the messages if there are any.
    ;; This deletes "edtrash" from sequence file; we don't have to. (FIX ?)
    (when (and deleted-seq
	       (or (not (value expunge-messages-confirm))
		   (prompt-for-y-or-n
		    :prompt (list "Expunge messages and pack folder ~A? "
				  folder)
		    :default t
		    :default-string "Y")))
      (delete-messages folder "edtrash")
      ;;
      ;; Compact the message id's after deletion.
      (let ((*standard-output* *mh-utility-bit-bucket*))
	(message "Compacting folder ...")
	(mh:pack-folder folder))
      ;;
      ;; Maintain much consistency.
      (let ((new-buf-p (eq (current-buffer) *new-mail-buffer*)))
	(message "Maintaining consistency ...")
	(expunge-messages-fold-headers-buffers folder)
	(expunge-messages-fix-draft-buffers folder)
	(expunge-messages-fix-unseen-headers folder)
	(when new-buf-p (change-to-buffer *new-mail-buffer*)))
      (delete-and-expunge-temp-drafts))))

;;; EXPUNGE-MESSAGES-FOLD-HEADERS-BUFFERS deletes all headers buffers into the
;;; compacted folder.  We can only update the headers buffers by installing all
;;; headers, so there may as well be only one such buffer.  First we get a list
;;; of the buffers since DO-HEADERS-BUFFERS is trying to iterate over a list
;;; being destructively modified by buffer deletions.
;;;
(defun expunge-messages-fold-headers-buffers (folder)
  (let (hbufs)
    (declare (list hbufs))
    (do-headers-buffers (b folder)
      (or (eq b *new-mail-buffer*)
	  (push b hbufs)))
    (unless (zerop (length hbufs))
      (dolist (b hbufs)
	(delete-headers-buffer-and-message-buffers-command nil b))
      (new-message-headers folder (list "all")))))

;;; EXPUNGE-MESSAGES-FIX-DRAFT-BUFFERS finds any draft buffer that was set up
;;; as a reply to some message in folder, removing this relationship in case
;;; that message id does not exist after expunge folder compaction.
;;;
(defun expunge-messages-fix-draft-buffers (folder)
  (declare (simple-string folder))
  (dolist (b *buffer-list*)
    (when (editor-bound-p 'draft-information :buffer b)
      (let* ((dinfo (variable-value 'draft-information :buffer b))
	     (reply-folder (draft-info-replied-to-folder dinfo)))
	(when (and reply-folder
		   (string= (the simple-string reply-folder) folder))
	  (setf (draft-info-replied-to-folder dinfo) nil)
	  (setf (draft-info-replied-to-msg dinfo) nil))))))

;;; EXPUNGE-MESSAGES-FIX-UNSEEN-HEADERS specially handles the unseen headers
;;; buffer apart from the other headers buffers into the same folder when
;;; messages have been expunged.  We must delete the associated message buffers
;;; since REVAMP-HEADERS-BUFFER does not, and these potentially reference bad
;;; message id's.  When doing this we must copy the other-msg-bufs list since
;;; the delete buffer cleanup hook for them is destructive.  Then we check for
;;; more unseen messages.
;;;
(defun expunge-messages-fix-unseen-headers (folder)
  (declare (simple-string folder))
  (when *new-mail-buffer*
    (let ((hinfo (variable-value 'headers-information
				 :buffer *new-mail-buffer*)))
      (when (string= (the simple-string (headers-info-folder hinfo))
		     folder)
	(let ((other-bufs (copy-list (headers-info-other-msg-bufs hinfo))))
	  (dolist (b other-bufs) (delete-buffer-if-possible b)))
	(with-writable-buffer (*new-mail-buffer*)
	  (revamp-headers-buffer *new-mail-buffer* hinfo)
	  ;; Restore the name in case someone used "Pick Headers".
	  (setf (buffer-name *new-mail-buffer*)
		(format nil "Unseen Headers ~A" folder))
	  (let ((region (maybe-get-new-mail-msg-hdrs folder)))
	    (when region
	      (insert-message-headers *new-mail-buffer* hinfo region))))))))

;;; MAYBE-GET-NEW-MAIL-MSG-HDRS returns a region suitable for a new mail buffer
;;; or nil.  Folder is probed for unseen headers, and if there are some, then
;;; we call GET-NEW-MAIL-MSG-HDRS which also uses "Unseen Headers Message Spec".
;;; If there are no unseen headers, we only look for "Unseen Headers Message
;;; Spec" messages.  We go through these contortions to keep MH from outputting
;;; errors.
;;;
(defun maybe-get-new-mail-msg-hdrs (folder)
  (let ((unseen-seq-name (mh:profile-component "unseen-sequence")))
    (multiple-value-bind (unseen-seq foundp)
			 (mh:sequence-list folder unseen-seq-name)
      (if (and foundp unseen-seq)
	  (get-new-mail-msg-hdrs folder unseen-seq-name)
	  (let ((spec (value unseen-headers-message-spec)))
	    (when spec
	      (message-headers-to-region
	       folder
	       (breakup-message-spec (string-trim '(#\space #\tab) spec)))))))))


;;;; Folders.

(defcommand "List Folders" (p)
  "Pop up a list of folders at top-level."
  "Pop up a list of folders at top-level."
  (declare (ignore p))
  (with-pop-up-display (stream)
    (mh:list-folders stream)))

(defcommand "Update Folders" (p)
  "Update folder names, clearing any cache folder information."
  "Update folder names, clearing any cache folder information."
  (declare (ignore p))
  (mh:update-folder-table))

(defcommand "Create Folder" (p)
  "Create a folder.  If the folder already exists signal an error."
  "Create a folder.  If the folder already exists signal an error."
  (declare (ignore p))
  (let ((folder (prompt-for-folder :must-exist nil)))
    (when (mh:folder-existsp folder)
      (editor-error "Folder already exists -- ~S!" folder))
    (mh:create-folder folder)))

(defcommand "Delete Folder" (p)
  "Delete a prompted folder."
  "Delete a prompted folder."
  (declare (ignore p))
  (let* ((folder (prompt-for-folder)))
    (if (prompt-for-yes-or-no
	 :prompt (list "Delete folder ~A? (yes or no) "
		       (mh:strip-folder-name folder))
	 :help "Confirm deletion of folder.")
	(multiple-value-bind (success files)
			     (mh:delete-folder folder)
	  (if success
	      (message "Done.")
	      (dolist (file files)
		(message "Skipped ~A." file)))))))

(defvar *refile-default-destination* nil)

(defcommand "Refile Message" (p)
  "Prompts for a source folder, messages, pick expression, and a destination
   folder to refile the messages."
  "Prompts for a source folder, messages, pick expression, and a destination
   folder to refile the messages."
  (declare (ignore p))
  (let* ((src-folder (prompt-for-folder :prompt "Source folder: "))
	 (hinfo (value headers-information))
	 (temp-msgs (prompt-for-message
		     :folder src-folder
		     :messages
		     (if (and hinfo
			      (string= src-folder
				       (the simple-string
					    (headers-info-folder hinfo))))
			 (headers-info-msg-strings hinfo))
		     :prompt "Messages to pick from: "))
	 ;; Return pick result or temp-msgs individually specified in a list.
	 (msgs (mh:pick-messages src-folder temp-msgs
				 (prompt-for-pick-expression))))
    (declare (simple-string src-folder))
    (refile-message src-folder msgs
		    (prompt-for-folder :must-exist nil
				       :prompt "Destination folder: "
				       :default *refile-default-destination*))))

(defcommand "Headers Refile Message" (p)
  "Refile the current message.
   When in a headers buffer, refiles the message on the current line, and when
   in a message buffer, refiles that message, prompting for a destination
   folder."
  "When in a headers buffer, refiles the message on the current line, and when
   in a message buffer, refiles that message, prompting for a destination
   folder."
  (declare (ignore p))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Point must be on a header line."))
	     (delete-mark cur-mark)
	     (refile-message (headers-info-folder hinfo) cur-msg
			     (prompt-for-folder
			      :must-exist nil
			      :prompt "Destination folder: "
			      :default *refile-default-destination*))))
	  (minfo
	   (refile-message
	    (message-info-folder minfo) (message-info-msgs minfo)
	    (prompt-for-folder :must-exist nil
			       :prompt "Destination folder: "
			       :default *refile-default-destination*))
	   (message "Message refiled."))
	  (t
	   (editor-error "Not in a headers or message buffer.")))))

;;; REFILE-MESSAGE refiles msg from src-folder to dst-folder.  If dst-buffer
;;; doesn't exist, the user is prompted for creating it.  All headers buffers
;;; concerning src-folder are updated.  When msg is a list, we did a general
;;; message prompt, and we cannot know which headers lines to delete.
;;;
(defun refile-message (src-folder msg dst-folder)
  (or (mh:folder-existsp dst-folder)
      (cond ((prompt-for-y-or-n
	      :prompt (format nil
			      "Destination folder ~A doesn't exist.  Create it? "
			      (mh:strip-folder-name dst-folder))
	      :default t :default-string "Y")
	     (mh:create-folder dst-folder))
	    (t (editor-error "Not refiling message."))))
  (apply #'mh:move-messages src-folder dst-folder msg ())
  (setf *refile-default-destination* (mh:strip-folder-name dst-folder))
  (if (listp msg)
      (do-headers-buffers (hbuf src-folder hinfo)
	(do-headers-lines (hbuf :line-var line :mark-var hmark)
	  (when (member (line-message-id line) msg :test #'string=)
	    (delete-headers-buffer-line hinfo hmark))))
      (do-headers-buffers (hbuf src-folder hinfo)
	(with-headers-mark (hmark hbuf msg)
	  (delete-headers-buffer-line hinfo hmark)))))


;;;; Miscellaneous commands.

(defcommand "Mark Message" (p)
  "Prompts for a folder, message, and sequence.  By default the message is
   added, but if an argument is supplied, the message is deleted.  When in
   a headers buffer or message buffer, only a sequence is prompted for."
  "Prompts for a folder, message, and sequence.  By default the message is
   added, but if an argument is supplied, the message is deleted.  When in
   a headers buffer or message buffer, only a sequence is prompted for."
  (let* ((hinfo (value headers-information))
	 (minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg
		 (editor-error "Point must be on a header line."))
	     (delete-mark cur-mark)
	     (let ((seq-name (prompt-for-string :prompt "Sequence name: "
						:trim t)))
	       (declare (simple-string seq-name))
	       (when (string= "" seq-name)
		 (editor-error "Sequence name cannot be empty."))
	       (mh:mark-message (headers-info-folder hinfo)
				cur-msg seq-name (if p :delete :add)))))
	  (minfo
	   (let ((msgs (message-info-msgs minfo))
		 (seq-name (prompt-for-string :prompt "Sequence name: "
					      :trim t)))
	     (declare (simple-string seq-name))
	     (when (string= "" seq-name)
	       (editor-error "Sequence name cannot be empty."))
	     (mh:mark-message (message-info-folder minfo)
			      (if (consp msgs) (car msgs) msgs)
			      seq-name (if p :delete :add))))
	  (t
	   (let ((folder (prompt-for-folder))
		 (seq-name (prompt-for-string :prompt "Sequence name: "
					      :trim t)))
	     (declare (simple-string seq-name))
	     (when (string= "" seq-name)
	       (editor-error "Sequence name cannot be empty."))
	     (mh:mark-messages folder (prompt-for-message :folder folder)
			       seq-name (if p :delete :add)))))))


(defcommand "List Mail Buffers" (p)
  "Show a list of all mail associated buffers.
   If the buffer has an associated message buffer, it is displayed to the right
   of the buffer name.  If there is no message buffer, but the buffer is
   associated with a headers buffer, then it is displayed.  If the buffer is
   modified then a * is displayed before the name."
  "Display the names of all buffers in a with-random-typeout window."
  (declare (ignore p))
  (let ((buffers nil))
    (declare (list buffers))
    (do-strings (n b *buffer-names*)
      (declare (ignore n))
      (or (eq b *echo-area-buffer*)
	  (cond ((editor-bound-p 'message-buffer :buffer b)
		 ;; Catches draft buffers associated with message buffers first.
		 (push (cons b (variable-value 'message-buffer :buffer b))
		       buffers))
		((editor-bound-p 'headers-buffer :buffer b)
		 ;; Then draft or message buffers associated with headers buffers.
		 (push (cons b (variable-value 'headers-buffer :buffer b))
		       buffers))
		((or (editor-bound-p 'draft-information :buffer b)
		     (editor-bound-p 'message-information :buffer b)
		     (editor-bound-p 'headers-information :buffer b))
		 (push b buffers)))))
    (with-pop-up-display (s :height (length buffers))
      (dolist (ele (nreverse buffers))
	(let* ((association (if (consp ele) (cdr ele)))
	       (b (if association (car ele) ele))
	       (buffer-pathname (buffer-pathname b))
	       (buffer-name (buffer-name b)))
	  (write-char (if (buffer-modified b) #\* #\space) s)
	  (if buffer-pathname
	      (format s "~A  ~A~:[~;~50T~:*~A~]~%"
		      (file-namestring buffer-pathname)
		      (directory-namestring buffer-pathname)
		      (if association (buffer-name association)))
	      (format s "~A~:[~;~50T~:*~A~]~%"
		      buffer-name
		      (if association (buffer-name association)))))))))


(defcommand "Message Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Message"))

(defcommand "Headers Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Headers"))

(defcommand "Draft Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Draft"))

(defcommand "Fill Paragraph Respect Comment" (p) ;; FIX comment?
  "Fill the region with \"Fill Prefix\" set to the comment char if the line
   starts with comment char."
  "Fill the region with \"Fill Prefix\" set to the comment char if the line
   starts with comment char."
  (declare (ignore p))

;; FIX lispmode similar "Fill Lisp Comment Para"
;; (to-line-command (line-start mark) (value comment-start))

;; FIX message-[buffer-]insertion-prefix

  ;; FIX empty cited line
  (let ((fill-prefix (concatenate 'simple-string
				  (or (value comment-start) ">")
				  " "))
	(mark (copy-mark (current-point))))
    (line-start mark)
    (let ((chars (region-to-string (region (copy-mark mark :temporary)
					   (line-end mark)))))
      (if (and (>= (length chars) (length fill-prefix))
	       (string= fill-prefix chars :end2 (length fill-prefix)))
	  (hlet ((fill-prefix fill-prefix))
	    (fill-paragraph-command nil))
	  (fill-paragraph-command nil)))))


;;;; Prompting.

;;; Folder prompting.
;;;

(defun prompt-for-folder (&key (must-exist t) (prompt "MH Folder: ")
			       (default (mh:current-folder)))
  "Prompts for a folder, using MH's idea of the current folder as a default.
   The result will have a leading + in the name."
  (let ((folder (prompt-for-keyword (list (mh:get-folder-table))
				    :must-exist must-exist :prompt prompt
				    :default default :default-string default
				    :help "Enter folder name.")))
    (declare (simple-string folder))
    (when (string= folder "") (editor-error "Must supply folder!"))
    (let ((name (mh:coerce-folder-name folder)))
      (if must-exist
	  (or (mh:folder-existsp name)
	      (editor-error "Folder does not exist -- ~S." name)))
      name)))


;;; Message prompting.
;;;

(defun prompt-for-message (&key (folder (mh:current-folder))
				(prompt "MH messages: ")
				messages)
   "Prompts for a message spec, using messages as a default.  If messages is
    not supplied, then the current message for folder is used.  The result is
    a list of strings which are the message ids, intervals, and/or sequence
    names the user entered."
  (let* ((cur-msg (cond ((not messages) (mh:current-message folder))
			((stringp messages) messages)
			((consp messages)
			 (if (= (length (the list messages)) 1)
			     (car messages)
			     (format nil "~{~A~^ ~}" messages))))))
    (breakup-message-spec (prompt-for-string :prompt prompt
					     :default cur-msg
					     :default-string cur-msg
					     :trim t
					     :help "Enter MH message id(s)."))))

(defun breakup-message-spec (msgs)
  (declare (simple-string msgs))
  (let ((start 0)
	(result nil))
    (loop
      (let ((end (position #\space msgs :start start :test #'char=)))
	(or end
	    (return (if (zerop start)
			(list msgs)
			(nreverse (cons (subseq msgs start) result)))))
	(push (subseq msgs start end) result)
	(setf start (1+ end))))))


;;; PICK expression prompting.
;;;

(defun prompt-for-pick-expression ()
  "Prompt for a pick expression."
  (read-from-string (prompt-for-string
		     :prompt "Message pick expression: "
		     :help "Expression to pick over mail messages."
		     :trim t)))


;;; Password prompting.
;;;

(defun prompt-for-password (&optional (prompt "Password: "))
  "Prompts for password with prompt."
  (let ((edi::*parse-verification-function* #'(lambda (string) (list string))))
    (let ((edi::*parse-prompt* prompt))
      (edi::display-prompt-nicely))
    (let ((start-window (current-window)))
      (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
      (setf (current-window) *echo-area-window*)
      (unwind-protect
	  (use-buffer *echo-area-buffer*
	    (let ((result ()))
	      (declare (list result))
	      (loop
		(let ((key-event (get-key-event *editor-input*)))
		  (ring-pop edi::*key-event-history*)
		  (cond ((eq key-event #k"return")
			 (return (prog1 (coerce (nreverse result)
						'simple-string)
				   (fill result nil))))
			((or (eq key-event #k"control-u")
			     (eq key-event #k"control-U"))
			 (setf result nil))
			(t (push (ext:key-event-char key-event) result)))))))
	(setf (current-window) start-window)))))


;;;; Making mail buffers.

;;; MAYBE-MAKE-MH-BUFFER looks up buffer with name, returning it if it exists
;;; after cleaning it up to a state "good as new".  Currently, we don't
;;; believe it is possible to try to make two draft buffers with the same name
;;; since that would mean that composition, draft folder interaction, and
;;; draft folder current message didn't do what we expected -- or some user
;;; was modifying the draft folder in some evil way.
;;;
(defun maybe-make-mh-buffer (name use)
  (let ((buf (getstring name *buffer-names*)))
    (cond ((not buf)
	   (let ((new-buf
		  (ecase use
		    (:headers (make-buffer name
					   :modes '("Headers")
					   :delete-hook '(cleanup-headers-buffer)))

		    (:message
		     (make-buffer name :modes '("Message")
				  :modeline-fields
				  (value default-message-modeline-fields)
				  :delete-hook '(cleanup-message-buffer)))

		    (:draft
		     (let ((buf (make-buffer
				 name :delete-hook '(cleanup-draft-buffer))))
		       (setf (buffer-minor-mode buf "Draft") t)
		       buf)))))
	     new-buf))
	  ((editor-bound-p 'headers-information :buffer buf)
	   (setf (buffer-writable buf) t)
	   (delete-region (buffer-region buf))
	   (cleanup-headers-buffer buf)
	   (delete-variable 'headers-information :buffer buf)
	   buf)
	  ((editor-bound-p 'message-information :buffer buf)
	   (setf (buffer-writable buf) t)
	   (delete-region (buffer-region buf))
	   (cleanup-message-buffer buf)
	   (delete-variable 'message-information :buffer buf)
	   buf)
	  ((editor-bound-p 'draft-information :buffer buf)
	   (error "Attempt to create multiple draft buffers to same draft ~
	           folder message -- ~S"
		  name)))))


;;;; Message buffer modeline fields.

(make-modeline-field
 :name :deleted-message :width 2 :replace t
 :function
 #'(lambda (buffer window)
     "Returns \"D \" when message in buffer is deleted."
     (declare (ignore window))
     (let* ((minfo (variable-value 'message-information :buffer buffer))
	    (hmark (message-info-headers-mark minfo)))
       (cond ((not hmark)
	      (let ((msgs (message-info-msgs minfo)))
		(if (and (value virtual-message-deletion)
			 (mh:sequence-member-p
			  (if (consp msgs) (car msgs) msgs)
			  (mh:sequence-list (message-info-folder minfo)
					    "edtrash")))
		    "D "
		    "")))
	     ((line-message-deleted (mark-line hmark))
	      "D ")
	     (t "")))))

(make-modeline-field
 :name :replied-to-message :width 1 :replace t
 :function
 #'(lambda (buffer window)
     "Returns \"A\" when message in buffer is deleted."
     (declare (ignore window))
     (let* ((minfo (variable-value 'message-information :buffer buffer))
	    (hmark (message-info-headers-mark minfo)))
       (cond ((not hmark)
	      ;; Could do something nasty here to figure out the right value.
	      "")
	     (t
	      (mark-to-note-replied-msg hmark)
	      (if (char= (next-character hmark) #\A)
		  "A"
		  ""))))))

;;; MARK-TO-NOTE-REPLIED-MSG moves the headers-buffer mark to a line position
;;; suitable for checking or setting the next character with respect to noting
;;; that a message has been replied to.
;;;
(defun mark-to-note-replied-msg (hmark)
  (line-start hmark)
  (find-attribute hmark :digit)
  (find-attribute hmark :digit #'zerop)
  (character-offset hmark 1))


(defhvar "Default Message Modeline Fields"
  "This is the default list of modeline-field objects for message buffers."
  :value
  (list (modeline-field :buffer-state)
	(modeline-field :package)
	(modeline-field :buffer-short-name)
	(modeline-field :modes)
	(modeline-field :replied-to-message)
	(modeline-field :deleted-message)
	(modeline-field :column)
	(modeline-field :space)
	(modeline-field :line)
	(modeline-field :space)
	(modeline-field :%)
	;;(modeline-field :position)
	(modeline-field :space)
	(modeline-field :buffer-pathname)))


;;;; MH interface.

;;; Running an MH utility.
;;;

(defhvar "MH Utility Pathname"
  "MH utility names are merged with this.  The default is
   \"/usr/misc/.mh/bin/\"."
  :value (pathname "/usr/misc/.mh/bin/"))

(defvar *mh-error-output* (make-string-output-stream))

(defun mh (utility args &key (errorp t) environment)
  "Runs the MH utility with the list of args (suitable for EXT:RUN-PROGRAM),
   outputting to *standard-output*.  Environment is a list of strings
   appended with ext:*environment-list*.  This returns t, unless there is
   an error.  When errorp, this reports any MH errors in the echo area as
   an editor error, and this does not return; otherwise, nil and the error
   output from the MH utility are returned."
  (fresh-line)
  (let* ((utility
	  (namestring
	   (or (probe-file (merge-pathnames utility
					    (value mh-utility-pathname)))
	       utility)))
	 (proc (ext:run-program
		utility args
		:output *standard-output*
		:error *mh-error-output*
		:env (append environment ext:*environment-list*))))
    (fresh-line)
    (ext:process-close proc)
    (cond ((zerop (ext:process-exit-code proc))
	   (values t nil))
	  (errorp
	   (editor-error "MH Error -- ~A"
			 (get-output-stream-string *mh-error-output*)))
	  (t (values nil (get-output-stream-string *mh-error-output*))))))




;;;; Checking for mail.

(defvar *mailbox* nil)

(defun new-mail-p ()
  (when (mh:new-mail-p (value mail-drops))
    (setq *new-mail-p* t)))


;;;; Folder browsing.

(defmode "Mail-Browse" :major-p t
  :documentation
  "Mail folder browsing mode.")

(defun refresh-folders (buffer)
  "Refresh the folders listed in BUFFER."
  (let ((pos (count-lines (region (buffer-start-mark buffer)
				  (buffer-point buffer)))))
    (setf (buffer-writable buffer) t)
    (delete-region (buffer-region buffer))
    (with-output-to-mark (stream (buffer-point buffer))
      (mh:print-folders stream "  "))
    (setf (buffer-major-mode buffer) "Mail-Browse")
    (setf (buffer-writable buffer) ())
    (setf (buffer-modified buffer) ())
    (buffer-start (buffer-point buffer))
    (line-offset (buffer-point buffer) (1- pos))))

(defcommand "Browse Folders" (p)
  "Browse mail folders."
  "Browse mail folders."
  (declare (ignore p))
  (let ((buffer (make-buffer "Folders")))
    (cond (buffer
	   (refresh-folders buffer)
	   (change-to-buffer buffer))
	  (t (change-to-buffer (getstring "Folders" *buffer-names*))))))

(defun folder-on-line (line)
  "Return the folder on LINE."
  (if (blank-line-p line)
      (editor-error "Point must be on a folder line."))
  (let ((string (line-string line)))
    (subseq string 2 (position #\space string :start 2))))

(defun line-of-folder (folder buffer)
  "Return the line on which FOLDER resides in BUFFER."
  (do-lines (line buffer)
    (or (blank-line-p line)
	(if (string= folder
		     (subseq (line-string line)
			     2 (position #\space (line-string line)
					 :start 2)))
	    (return-from line-of-folder line)))))

(defcommand "Mail-Browse Browse Folder" (p)
  "Browse the headers of the folder at point."
  "Browse the headers of the folder at point."
  (declare (ignore p))
  (new-message-headers (folder-on-line (mark-line (current-point)))
		       (breakup-message-spec "all")))

(defcommand "Mail-Browse Browse Folder in Other Window" (p)
  "Browse the headers of the folder at point in the other window."
  "Browse the headers of the folder at point in the other window."
  (declare (ignore p))
  (let ((folder (folder-on-line (mark-line (current-point)))))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command nil)
	(next-window-command nil))
    (new-message-headers folder (breakup-message-spec "all"))))

(defcommand "Refresh Folders" (p)
  "Refresh the browsed folders."
  "Refresh the browsed folders."
  (declare (ignore p))
  (refresh-folders (current-buffer)))

(defcommand "Mail-Browse Delete Folder" (p)
  "Mark the folder at point for deletion."
  "Mark the folder at point for deletion."
  (declare (ignore p))
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((mark (copy-mark (current-point)))
	(buffer (current-buffer)))
    (line-start mark)
    (setf (buffer-writable buffer) t)
    (delete-characters mark)
    (insert-character mark #\D)
    (setf (buffer-writable buffer) ())))

(defcommand "Mail-Browse Mark Folder" (p)
  "Mark the folder at point."
  "Mark the folder at point."
  (declare (ignore p))
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((mark (copy-mark (current-point)))
	(buffer (current-buffer)))
    (line-start mark)
    (mark-after mark)
    (setf (buffer-writable buffer) t)
    (delete-characters mark)
    (insert-character mark #\*)
    (setf (buffer-writable buffer) ())
    (next-line-command ())))

(defcommand "Mail-Browse Clear Folder Marks" (p)
  "Clear any marks on the folder at point."
  "Clear any marks on the folder at point."
  (declare (ignore p))
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((mark (copy-mark (current-point)))
	(buffer (current-buffer)))
    (line-start mark)
    (setf (buffer-writable buffer) t)
    (delete-characters mark 2)
    (insert-string mark "  ")
    (setf (buffer-writable buffer) ())
    (next-line-command ())))

(defcommand "Expunge Folders" (p)
  "Expunge any folders marked for deletion."
  "Expunge any folders marked for deletion."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
	 (mark (copy-mark (buffer-start-mark buffer)))
	 (remain))
    (collect ((folders)
	      (generic-remains))  ; *-marked folders to remain.
      (line-start mark)
      (setf (buffer-writable buffer) t)
      (loop for line = (mark-line mark) do
	(if (blank-line-p line) (return))
	(let ((string (line-string (mark-line mark))))
	  (if (char= (aref string 0) #\D)
	      (folders (folder-on-line line))
	      (if (char= (aref string 1) #\*)
		  (generic-remains (folder-on-line line)))))
	(line-offset mark 1))
      (let ((folder-count (length (folders))))
	(flet ((prompt-and-do (prompt)
		 (if (prompt-for-yes-or-no
		      :prompt prompt
		      :help "Confirm deletion of folders.")
		     (dolist (folder (folders))
		       ;; FIX prompt again if folder contains messages
		       (or (mh:delete-folder folder)
			   (progn
			     (message "Failed to delete ~A." folder)
			     (push folder remain))))
		     (editor-error "Expunge canceled."))))
	  (if (> folder-count 1)
	    ;; FIX with-pop-up-list?
	    (with-pop-up-window (buffer)
	      (with-output-to-mark (stream (buffer-point buffer))
		(let* ((pos 0)
		       (width (/ (value fill-column) 3))
		       (format-0 (format () "~~@~D<~~A~~>" width))
		       (format-1 (format () " ~~@~D<~~A~~>" width)))
		  (dolist (folder (folders))
		    (case pos
		      (0 (format stream format-0 folder)
			 (setq pos 1))
		      (1 (format stream format-1 folder)
			 (setq pos 2))
		      (2 (format stream " ~A~%" folder)
			 (setq pos 0))))))
	      (prompt-and-do
	       (list "Delete ~D folders, including any messages? (yes or no) "
		     folder-count)))
	    (prompt-and-do
	     (list "Delete ~A, including any messages? (yes or no) "
		   (car (folders)))))))
      (refresh-folders buffer)
      ;; Remark failures.
      (setf (buffer-writable buffer) t)
      (dolist (folder remain)
	(let* ((line (line-of-folder folder buffer))
	       (mark (mark line 0)))
	  (line-start mark)
	  (delete-characters mark)
	  (insert-character mark #\D)))
      ;; Remark generic marks (*'s).
      (dolist (folder (generic-remains))
	(let* ((line (line-of-folder folder buffer))
	       (mark (mark line 0)))
	  (line-start mark)
	  (mark-after mark)
	  (delete-characters mark)
	  (insert-character mark #\*)))
      (setf (buffer-writable buffer) ()))))
