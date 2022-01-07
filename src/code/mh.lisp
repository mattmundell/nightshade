;; Mail handler (MH) library.

(in-package "MH")

(export '(*address* *alternate-addresses* *body-scan-length*
	  *html-handler* *from-name* *mailer*

	  profile-component root-pathname folder-pathname
	  draft-folder current-folder current-message

	  sequence-insert sequence-delete sequence-member-p
	  sequence-strings sequence-list

	  get-folder-table update-folder-table
	  coerce-folder-name strip-folder-name folder-exists-p
	  create-folder delete-folder rename-folder pack-folder
	  sort-folder summarize-folder
	  do-folders print-folders

	  annotate-message delete-message get-headers-part get-part
	  mark-message
	  move-message write-file-message write-headers write-message
	  resend-message summarize-message

	  message-header

	  draft-forward draft-new draft-reply draft-resend

	  delete-messages deliver-messages mark-messages
	  maybe-messages-before-p move-messages pick-messages
	  split-messages summarize-messages write-messages

	  parse-content-type

	  make-drop make-drops new-mail-p incorporate

	  with-fresh-state

	  cc f-cc from f-from to f-to subject f-subject
	  content f-content date f-date -- f---
	  before after))

;; FIX  current message (sequence) handling
;;      structures,arrays instd of lists?


;;;; Tracing.

(defvar *mess* ())
(defvar *mess-level* 0)

;;; incf-mess  --  Internal
;;;
;;; Increment the nesting level of tracing messages.
;;;
(defun incf-mess () (incf *mess-level* 2))

;;; decf-mess  --  Internal
;;;
;;; Decrement the nesting level of tracing messages.
;;;
(defun decf-mess () (decf *mess-level* 2))

;;; decf-mess  --  Internal
;;;
;;; Write a tracing message.
;;;
(defun mess (format &rest args)
  (when *mess*
    (funcall *mess* "~A~A"
	     (make-string *mess-level* :initial-element #\space)
	     (apply #'funcall #'format () format args))))


;;;; User profile variables.

;;; Internal
;;;
;;; Last resort folder in which to archive replies.
;;;
(defvar *reply-archive* "archive/misc")

;;; Internal
;;;
;;; Return the folder in which to archive a reply to $message from $folder.
;;;
(defun archive-for-reply (folder message)
  (declare (ignore message))
  (let ((folder (strip-folder-name folder)))
    (case= folder
      ("inbox" *reply-archive*)
      (t folder))))

;;; Public
;;;
(defvar *from-name* (user-full-name)
  "Traditional MH signature (i.e. the name before the email address in the
   From header).")

;;; Public
;;;
(defvar *address* (user-email)
  "Email address.  Last resort address for Reply-to and From headers.")

;;; Public
;;;
(defvar *alternate-addresses* ()
  "A list of alternate addresses, as strings.")

;;; Internal
;;;
;;; Return the address for the reply-to header of a reply to $message from
;;; $folder.
;;;
(defun address-for-reply-to (folder message)
  (declare (ignore message))
  (let ((folder (strip-folder-name folder)))
    (case= folder
      ("inbox" *address*)
      (t *address*))))

;;; Internal
;;;
;;; Return the address for the from header of a reply to $message from
;;; $folder.
;;;
(defun address-for-from (folder message)
  (declare (ignore message))
  (let ((folder (strip-folder-name folder)))
    (case= folder
      ("inbox" *address*)
      (t *address*))))


;;;; General variables.

(defvar *forward-start*
  "-------------------- Start of forwarded message --------------------")

(defvar *forward-end*
  "-------------------- End of forwarded message --------------------")


;;;; Structure.

;;; Internal
;;;
;;; String table naming all folders.
;;;
(defvar *folder-table* ())

;;; Information about a folder, for caching.
;;;
(defstruct (folder-info (:constructor
			 make-folder-info
			 (pathname write-date highest
				   &optional messages sequences)))
  pathname    ; Pathname of folder directory.
  write-date  ; Last time directory modified, in universal format.
  (highest 0) ; ID of highest message in folder; 0 when empty.
  lowest      ; ID of lowest message in folder if known; else ().
  messages    ; List of messages in folder, sorted ascending by ID.
  (start () :type (or integer null))   ; ID of lowest in messages.
  (end   () :type (or integer null t)) ; ID of highest; t if all.
  sequences)  ; List of (sequence-name ((msg | (start . end))*)) lists.


;;;; Date

;;; Internal
;;;
;;; Write $date to $stream as the value of a header named $name.
;;;
(defun write-date-header (name stream
			       &optional (date (get-universal-time)))
  (format stream "~A: " name)
  (format-universal-time stream date :style :rfc1123 :print-weekday ())
  (terpri stream))


;;;; Profile.

;;; Internal
;;;
;;; Return the pathname of the MH profile.
;;;
(defun profile-pathname ()
  (merge-pathnames (or (cdr (assoc :mh ext:*environment-list*))
		       ".mh_profile")
		   (truename (user-homedir-pathname))))

;;; Public
;;;
(defun profile-component (name &optional (pathname (profile-pathname))
			                 (error-on-open t))
  "Return the trimmed string value for the MH profile component $name.  If
   the component is missing, return ().  This may be used on MH context and
   sequence files as well, due to their having the same format.

   If $error-on-open is true then signal any file open errors.

   Signal an error if name is the empty string."
  (if (stringp name)
      (or (plusp (length name))
	  (error "$name must be at least one character long."))
      (error "$name must be a string: ~A" name))
  (with-open-stream (s (if error-on-open
			   (open pathname)
			   (ignore-errors (open pathname))))
    (if s
	(loop
	  (multiple-value-bind (line eofp) (read-line s nil :eof)
	    (when (equal line :eof) (return nil))
	    (let ((colon (position #\: (the simple-string line)
				   :test #'char=)))
	      (or colon
		  (error "Bad record ~S in file ~S."
			 line (namestring pathname)))
	      (when (string-equal name line :end2 colon)
		(return (string-trim '(#\space #\tab)
				     (subseq line (1+ colon))))))
	    (when eofp (return nil)))))))


;;;; Message specs.

;;; Internal
;;;
;;; Return $message converted to an integer or symbol.
;;;
;;; $folder-info must include the highest message.
;;;
(defun clean-message-spec (message folder-info)
  (etypecase message
    (integer message)
    (symbol
     (ecase message
       (:highest (folder-info-highest folder-info))))
    (string
     (if (string= message "all")
	 (error "Message spec \"all\" defines multiple messages.")
	 (let ((int (parse-integer message :errorp ())))
	   (if int
	       int
	       ;; Assume message is the name of a sequence.
	       (ecase= message
		 ("highest" (folder-info-highest folder-info)))))))))

;;; Internal
;;;
;;; Return $messages sorted from highest to lowest, converting all string
;;; message IDs to integers or symbols.
;;;
(defun clean-messages-spec (messages sequences)
  ;; FIX could add "cur", "next", "prev", ...   (counterparts of "all")
  (collect ((msgs))
    (dolist (id messages (let ((*sequences* sequences))
			   (sort (msgs) #'messages-item-<)))
      (etypecase id
	(integer
	 (msgs id))
	(symbol
	 (ecase id
	   (:all (return '(:all)))))
	(list
	 (let ((first (parse-integer (car id)))
	       (rest (parse-integer (cdr id))))
	   (msgs (if (< rest first)
		     (cons rest first)
		     (cons first rest)))))
	(string
	 (if (string= id "all")
	     (return '(:all))
	     (let ((int (parse-integer id
				       :errorp ()
				       :junk-allowed t)))
	       (if int
		   (let ((pos (position #\- id)))
		     (if pos
			 (let ((end (parse-integer id
						   :start (1+ pos))))
			   (msgs
			    (if (< end int)
				(cons end int)
				(cons int end))))
			 (msgs int)))
		   ;; Assume id is the name of a sequence.
		   (msgs `(:sequence . ,id))))))))))


;;;; Folder names.

;; TODO: Consider srapping the environment variable configs, caching the
;;       configs read from file, and checking whether the file has been
;;       updated every time the cache is read.

;;; Public
;;;
(defun coerce-folder-name (folder)
  "Return string $folder, ensuring that it has a leading +.

   Signal an error if folder is the empty string."
  (if (char= (schar folder 0) #\+)
      folder
      (concatenate 'simple-string "+" folder)))

;;; Public
;;;
(defun strip-folder-name (folder)
  "Return string $folder, stripping off any leading +.

   Signal an error if folder is the empty string."
  (or (plusp (length folder))
      (error "Folder name should be at least one character long."))
  (if (char= (schar folder 0) #\+)
      (subseq folder 1)
      folder))

;;; Public
;;;
(defun root-pathname ()
  "Return the pathname of the mail directory.

   Signal an error if the Path component is missing."
  (let ((pathname (merge-pathnames
		   (or (profile-component "path")
		       (error "Mail profile must contain a Path component."))
		   (user-homedir-pathname))))
    (truename
     (ensure-directories-exist
      (directorify pathname)))))

;;; Public
;;;
(defun folder-pathname (folder)
  "Return the pathname of $folder, including the trailing slash.

   Signal an error if string $folder is empty."
  (merge-pathnames (ensure-trailing-slash (strip-folder-name folder))
		   (root-pathname)))

;;; Internal
;;;
;;; Return the folder name of $pathname.
;;;
(defun pathname-folder (pathname)
  (subseq (namify (namestring pathname))
	  (length (directorify (namestring (root-pathname))))))

;;; Public
;;;
(defun draft-folder ()
  "Return the name of the draft folder."
  (namify (or (profile-component "draft-folder")
	      (error "There must be a draft-folder profile component"))))

;;; Public
;;;
;;; Return the pathname of the context file.
;;;
(defun context-pathname ()
  (merge-pathnames (or (profile-component "context") "context")
		   (root-pathname)))

;;; Public
;;;
(defun current-folder ()
  "Return the current folder from the context file, if there is such a
   folder, else return ().

   FIX Updating of the current folder is yet to be implemented (in the
   editor a history keeps track of the current folder)."
  (profile-component "current-folder" (context-pathname)))

;;; Public
;;;
;;; This can cache $folder, via `add-highest-sequence'.
;;;
(defun current-message (folder)
  "Return the current message from $folder's sequence file."
  (declare (simple-string folder))
  (mess "(current-message ~A)" folder)
  (incf-mess)
  (unwind-protect
      (let ((folder (strip-folder-name folder)))
	(in-directory (folder-pathname folder)
	  (let* ((sequences (get-sequences))
		 (current (cadr (assoc "cur" sequences
				       :test #'string=))))
	    (if current
		(nth-value 0 (parse-integer current))
		(let ((current
		       (or (cadr (assoc "highest" sequences
					:test #'string=))
			   (string (add-highest-sequence
				    folder
				    (folder-pathname folder))))))
		  (mark-message folder current "cur" :add)
		  (nth-value 0 (parse-integer current)))))))
    (decf-mess)))
;;
(defun %set-current-message (folder message)
  "If $message exists in $folder then set the current message for $folder
   to $message, returning $message, otherwise throw an error."
  (or (zerop message)
      (probe-file (merge-pathnames (string message)
				   (folder-pathname folder)))
      (error "Message must exist in folder ~A: ~A" folder message))
  (mark-message folder :all "cur" :delete)
  (mark-message folder (string message) "cur" :add)
  message)
;;
(defsetf current-message %set-current-message
  "Set the current message.")


;;;; Sequences.

;;; Internal
;;;
;;; True during folder caching (`cache-folder').
;;;
(defvar *in-cache-folder* ())

;;; find-highest  --  Internal
;;;
;;; Return the ID of the highest message in $dir if there is one, else
;;; return 0.
;;;
(defun find-highest (dir)
  (mess "(find-highest ~A)" dir)
  (incf-mess)
  (mess "cd: ~A" (current-directory))
  (let ((highest 0))
    (do-files (file dir
		    :recurse ()
		    :follow-links t
		    :check-for-subdirs ()
		    :backups ())
      (let* ((name (pathname-name file))
	     (num (and (every #'digit-char-p name)
		       (parse-integer name
				      :errorp t
				      :junk-allowed t))))
	(if (and num (> num highest))
	    (setq highest num))))
    (mess "returning ~A" highest)
    (decf-mess)
    highest))

;;; Internal
;;;
;;; Find and add a "highest" sequence to $dir (which holds $folder).
;;; Return the value of the sequence.
;;;
;;; The "highest" sequence is a single ID, the highest message ID in the
;;; dir."
;;;
(defun add-highest-sequence (folder dir)
  (mess "(add-highest-sequence ~A ~A)" folder dir)
  (incf-mess)
  (let ((highest (find-highest dir)))
    (if *in-cache-folder*
	(progn
	  (mark-message-directly folder :all "highest" :delete)
	  (mark-message-directly folder (string highest)
				 "highest" :add))
	(progn
	  ;; The id must be added first, as `mark-message' calls
	  ;; `scan-folder' which needs a highest sequence.
	  (mess "mark highest ~A" highest)
	  (mark-message folder (string highest) "highest" :add)
	  (let ((folder-info (scan-folder folder :sequences)))
	    (do-sequence (id (cdr (assoc "highest"
					 (folder-info-sequences
					  folder-info)
					 :test #'string=)))
	      (or (eq id highest)
		  (mark-message folder (string id) "highest" :delete))))))
    (decf-mess)
    highest))

;;; Internal
;;;
;;; Add or delete $msg from the sequence list $seq-list (which is for the
;;; sequence named $sequence) in the ".mh_sequence" file of $folder.
;;; $add-or-delete is either :add or :delete.
;;;
;;; If $msg is :all and $add-or-delete is :delete, then mark all messages.
;;;
;;; Leave the folder info alone, to be safe when called from
;;; `cache-folder'.
;;;
(defun %mark-message-directly (folder msg sequence seq-list add-or-delete)
  (if (eq msg :all)
      (ecase add-or-delete
	;; TODO Add all in folder.
	(:add (error "$msg :all with $add-or-delete :add"))
	(:delete
	 (update-sequence folder sequence ())))
      (ecase add-or-delete
	(:add
	 (update-sequence folder sequence (sequence-insert msg seq-list)))
	(:delete
	 (when (sequence-member-p msg seq-list)
	   (update-sequence folder sequence
			    (sequence-delete msg seq-list)))))))

;;; Internal
;;;
;;; Get the $sequence list for $sequence, and call %mark-message-directly.
;;;
(defun mark-message-directly (folder msg sequence add-or-delete)
  (mess "(mark-message-directly ~A ~A ~A ~A)"
	folder msg sequence add-or-delete)
  (let ((seq-list (sequence-list folder sequence t)))
    (%mark-message-directly folder msg sequence seq-list add-or-delete)))

;;; Public
;;;
(defun mark-message (folder msg sequence add-or-delete)
  "Add or delete $msg from the sequence named $sequence in the
   \".mh_sequence\" file of $folder.  $add-or-delete is either :add or
   :delete.

   If $msg is :all and $add-or-delete is :delete, then mark all messages."
  (mess "(mark-message ~A ~A ~A ~A)" folder msg sequence add-or-delete)
  (incf-mess)
  (let* ((folder (namify folder))
	 (seq-list (sequence-list folder sequence))
	 (folder-info (scan-folder folder :sequences)))
    (%mark-message-directly folder msg sequence seq-list add-or-delete)
    (let ((dir (folder-pathname folder)))
      (in-directory dir
	(setf (folder-info-sequences folder-info)
	      (get-sequences))
	(setf (folder-info-write-date folder-info)
	      (file-write-date dir)))))
  (decf-mess))

;;; Public
;;;
(defun mark-messages (folder messages sequence add-or-delete)
  "Add or delete $messages from the sequence named $sequence in the cache
   and .mh_sequence file of $folder.  Expect $add-or-delete to be either
   :add or :delete.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\".

   Throw an error if $messages contains any sequence names."
  (let* ((folder (namify folder))
	 (seq-list (sequence-list folder sequence))
	 (folder-info (scan-folder folder :sequences))
	 (messages (clean-messages-spec
		    messages
		    (folder-info-sequences folder-info))))
    (dolist (message messages)
      (typecase message
	(list
	 (if (eq (car message) :sequence)
	     (error "$messages contains a sequence name: ~A"
		    (cdr message))))
	(symbol
	 ;; FIX is this consistent?
	 (or (eq message :all)
	     (error "$messages contains a symbol: ~A" message)))))
    (ecase add-or-delete
      (:add
       (if (eq (car messages) :all)
	   ;; TODO Add all messages in folder.
	   (error "All messages specified with $add-or-delete :add")
	   (dolist (message messages)
	     (setq seq-list (sequence-insert message seq-list)))))
      (:delete
       (if (eq (car messages) :all)
	   (setq seq-list ())
	   (dolist (message messages)
	     ;; If the message is a range try take it, otherwise first
	     ;; confirm that it is in the sequence.
	     (when (or (listp message)
		       (sequence-member-p message seq-list))
	       (setq seq-list (sequence-delete message seq-list)))))))
    (update-sequence folder sequence seq-list)
    ;; Update the sequences in the cache of folder.
    (let ((dir (folder-pathname folder)))
      (in-directory dir
	(setf (folder-info-sequences folder-info)
	      (get-sequences))
	(setf (folder-info-write-date folder-info)
	      (file-write-date dir))))))

;;; Internal
;;;
;;; Write $seq-list (which is for sequence $name) to the ".mh_sequences"
;;; file in $folder.  If $seq-list is (), remove the sequence from the
;;; file.
;;;
(defun update-sequence (folder name seq-list)
  (declare (simple-string folder))
  (mess "(update-sequence ~A ~A ~A)" folder name seq-list)
  (let* ((folder (strip-folder-name folder))
	 (input (merge-pathnames ".mh_sequences"
				 (folder-pathname folder)))
	 (output (pick-new-file "/tmp/.mh_sequences-tem-~D-~D"))
	 (found nil))
    (fi (file-writable output)
	(error "Cannot write sequence temp file ~A.~%~
		Aborting output of ~S sequence."
	       name (namestring output))
	(progn
	  (with-open-file (in input :if-does-not-exist :create)
	    (with-open-file (out output :direction :output)
	      ;; Transfer the sequence to the temporary file, using the new
	      ;; list for $name if there is such an existing sequence.
	      (loop
		(multiple-value-bind (line eofp) (read-line in () :eof)
		  (if (eq line :eof) (return))
		  (let ((colon (position #\: (the simple-string line)
					 :test #'char=)))
		    (or colon
			(error "Bad record ~S in file ~S."
			       line (namestring input)))
		    (cond ((and (not found)
				(string-equal name line :end2 colon))
			   ;; Write the new sequence to the output file.
			   (write-seq-list
			    out (subseq line 0 colon) seq-list)
			   (setf found t))
			  (t
			   ;; Write the existing sequence to the output
			   ;; file.
			   (write-line line out))))
		  (if eofp (return))))
	      ;; Check if the sequence was found.
	      (unless found
		(fresh-line out)
		;; Append the new sequence to the output file.
		(write-seq-list out name seq-list))))
	  (rename-file output input)))))

;;; Internal
;;;
;;; If $seq-list is true, write the list to $stream as sequence $name.
;;;
(defun write-seq-list (stream name seq-list)
  (when seq-list
    (write-string name stream)
    (write-char #\: stream)
    (let ((*print-base* 10))
      (dolist (range seq-list)
	(write-char #\space stream)
	(let ((low (car range))
	      (high (cdr range)))
	  (declare (fixnum low high))
	  (cond ((= low high)
		 (prin1 low stream))
		(t (prin1 low stream)
		   (write-char #\- stream)
		   (prin1 high stream))))))
    (terpri stream)))

;;; Internal
;;;
;;; Return t if X is less than Y, else ().
;;;
;;; Keeps `sort' from consing rest args when `funcall'ing `<'.
;;;
(defun sequence-< (x y)
  (< x y))

;;; Internal
;;;
;;; Return t if X is greater than Y, else ().
;;;
;;; Keeps `sort' from consing rest args when `funcall'ing `>'.
;;;
(defun sequence-> (x y)
  (> x y))

;;; Public
;;;
(defun sequence-insert (item seq-list)
  "Insert $item into sequence list $seq-list.  $item can be a string like
   \"23\", a number like 23, or a cons of two numbers like (23 . 23) or (3
   . 5).

   Return the new list."
  (let ((range (typecase item
		 (string (let ((id (parse-integer item)))
			   (cons id id)))
		 (cons item)
		 (number (cons item item)))))
    (cond (seq-list
	   (setf seq-list (sort (cons range seq-list)
				#'sequence-< :key #'car))
	   (coalesce-sequence-ranges seq-list))
	  (t (list range)))))

;;; Internal
;;;
;;; Coalesce the ranges in $seq-list.
;;;
(defun coalesce-sequence-ranges (seq-list)
  (when seq-list
    (let* ((current seq-list)
	   (next (cdr seq-list))
	   (current-range (car current))
	   (current-end (cdr current-range)))
      (declare (fixnum current-end))
      (loop
	(unless next
	  (setf (cdr current-range) current-end)
	  (setf (cdr current) nil)
	  (return))
	(let* ((next-range (car next))
	       (next-start (car next-range))
	       (next-end (cdr next-range)))
	  (declare (fixnum next-start next-end))
	  (cond ((<= (1- next-start) current-end)
		 ;;
		 ;; Extend the current range since the next one overlaps.
		 (when (> next-end current-end)
		   (setf current-end next-end)))
		(t
		 ;;
		 ;; Update the current range since the next one doesn't
		 ;; overlap.
		 (setf (cdr current-range) current-end)
		 ;;
		 ;; Make the next range succeed current.  Then make it
		 ;; current.
		 (setf (cdr current) next)
		 (setf current next)
		 (setf current-range next-range)
		 (setf current-end next-end))))
	(setf next (cdr next))))
    seq-list))

;;; Public
;;;
(defun sequence-delete (item seq-list)
  "Remove $item from sequence list $seq-list.  $item can be a string
   (\"23\"), number (23), or a cons of two numbers ((23 . 23) or (3 . 5)).

   Return the new sequence list."
  (let ((range (typecase item
		 (string (let ((id (parse-integer item)))
			   (cons id id)))
		 (cons item)
		 (number (cons item item)))))
    (when seq-list
      (until ((id (car range) (1+ id))
	      (end (cdr range)))
	     ((> id end))
	(setf seq-list (sub-sequence-delete id seq-list)))
      seq-list)))

;;; Internal
;;;
;;; Return a sequence list containing all the elements before and after $id
;;; in $seq-list.
;;;
(defun sub-sequence-delete (id seq-list)
  (while ((prev () seq)
	  (seq seq-list (cdr seq)))
	 (seq)
    (let* ((range (car seq))
	   (low (car range))
	   (high (cdr range)))
      (cond ((> id high))
	    ((< id low)
	     (return))
	    ((= id low)
	     (cond ((/= low high)
		    (setf (car range) (1+ id)))
		   (prev
		    (setf (cdr prev) (cdr seq)))
		   (t (setf seq-list (cdr seq-list))))
	     (return))
	    ((= id high)
	     (setf (cdr range) (1- id))
	     (return))
	    ((< low id high)
	     (setf (cdr range) (1- id))
	     (setf (cdr seq) (cons (cons (1+ id) high) (cdr seq)))
	     (return)))))
  seq-list)

;;; Public
;;;
(defun sequence-member-p (item seq-list)
  "Return whether $item is in sequence list $seq-list.  $item can be a
   string, like \"23\", or a number, like 23."
  (let ((id (typecase item
	      (string (parse-integer item))
	      (number item))))
    (dolist (range seq-list ())
      (let ((low (car range))
	    (high (cdr range)))
	(when (<= low id high) (return t))))))

;;; Public
;;;
(defun sequence-strings (seq-list)
  "Return a list of strings representing the ranges and message id's in
   $seq-list.  If $seq-list is empty return ()."
  (let ((result nil))
    (dolist (range seq-list)
      (let ((low (car range))
	    (high (cdr range)))
	(if (= low high)
	    (push (string low) result)
	    (push (format nil "~D-~D" low high) result))))
    (nreverse result)))

;;; Internal
;;;
;;; Parse and return the list of messages in $string.
;;;
(defun parse-sequence (string)
  (collect ((msgs))
    (dolist (msg (split string '(#\space #\newline)))
      (let ((sep (position #\- msg)))
	(msgs (if sep
		  (cons (subseq msg 0 sep)
			(subseq msg (1+ sep) (length msg)))
		  msg))))
    (msgs)))

;;; Internal
;;;
;;; Parse and return the list of sequences from the .mh_sequences file in
;;; the current directory.
;;;
(defun get-sequences ()
  (collect ((sequences))
    (when (probe-file ".mh_sequences")
      (with-open-file (stream ".mh_sequences" :direction :input)
	(loop
	  (multiple-value-bind (name text)
			       (read-header stream t)
	    (or name (return))
	    (if (equal name :body)
		(error "Body boundary in .mh_sequence file."))
	    (sequences (cons name (parse-sequence text)))))
	(sequences)))))

;;; Internal
;;;
;;; Return a list representing the messages and ranges of IDs for the
;;; sequence $name in $sequences.  Return a second value indicating whether
;;; the sequence was found.
;;;
(defun %sequence-list (name sequences)
  (let ((assoc (assoc name sequences :test #'string=)))
    (if assoc
	;; FIX format required by editor  (and in .mh_sequences?)
	(collect ((seqs))
	  (dolist (seq (cdr assoc))
	    (etypecase seq
	      (list
	       (etypecase (car seq)
		 ;; FIX assume cdr same type as car
		 (integer
		  (seqs (cons (car seq) (cdr seq))))
		 (string
		  (seqs (cons (parse-integer (car seq))
			      (parse-integer (cdr seq)))))))
	      (string
	       ;; Allow junk in case the sequence is empty.
	       (let ((id (parse-integer seq :junk-allowed t)))
		 (if id (seqs (cons id id)))))
	      (integer
	       (seqs (cons seq seq)))))
	  (values (seqs) t))
	(values () ()))))

;;; Public
;;;
(defun sequence-list (folder name &optional directly)
  "Return a list representing the messages and ID ranges in the sequence
   $name in $folder.  Return a second value, true if the sequence was
   found, false otherwise."
  (declare (simple-string folder))
  (%sequence-list name
		  (if directly
		      (in-directory (folder-pathname folder)
			(get-sequences))
		      (folder-info-sequences
		       (scan-folder folder :sequences)))))

;;; Internal
;;;
;;; Return the ID of the lowest message in $sequence-name from $sequences.
;;;
(defun sequences-lowest (sequence-name sequences)
  (let* ((list (%sequence-list sequence-name sequences))
	 (lowest (caar list)))
    (dolist (range (cdr list))
      (if (< (car range) lowest)
	  (setq lowest (car range))))
    lowest))

;;; Internal
;;;
;;; Return the ID of the highest message in $sequence-name from
;;; $sequences.
;;;
(defun sequences-highest (sequence-name sequences)
  (let* ((list (%sequence-list sequence-name sequences))
	 (highest (cdar list)))
    (dolist (range (cdr list))
      (if (> (cdr range) highest)
	  (setq highest (cdr range))))
    highest))

;;; Internal
;;;
;;; Update the sequences in $folder-info and the .mh_sequences file in the
;;; current directory to match the messages in the current directory.
;;;
;;; Do so by ensuring that all files in the $folder-info sequences exist on
;;; disk, and by overwriting the .mh_sequences file according to the
;;; $folder-info sequences.
;;;
(defun update-sequences (folder-info)
  (mess "(update-sequences .)")
  (incf-mess)
  (let ((high (sequences-highest "highest"
				 (folder-info-sequences folder-info))))
    (when high
      (mess "releasing all from highest")
      (mark-message (pathname-folder (folder-info-pathname folder-info))
		    :all
		    "highest"
		    :delete)))
  (when (folder-info-highest folder-info)
    (mess "adding ~A" (folder-info-highest folder-info))
    (mark-message (pathname-folder (folder-info-pathname folder-info))
		  (string (folder-info-highest folder-info))
		  "highest"
		  :add))
  (collect ((empty))
    ;; Update the sequence list according to the message files on disk.
    (dolist (sequence (folder-info-sequences folder-info))
      (collect ((messages))
	(dolist (message (cdr sequence))
	  (if (listp message)
	      (let ((start (parse-integer (car message)))
		    (end (parse-integer (cdr message))))
		(while ((id start (1+ id)))
		       ((<= id end))
		  (let ((msg (string id)))
		    (or (probe-file msg)
			(progn
			  (cond ((eq id start)) ; Skip the first.
				((eq (1- id) start)
				 ;; Split off the first message.
				 (messages (string (1- id))))
				((eq id end)
				 ;; Skip the final message.
				 (setq end (1- id))
				 (return))
				(t
				 ;; Split the range.
				 (messages (cons (string start)
						 (string (1- id))))))
			  (setq start (1+ id))))))
		(cond ((< start end)
		       (messages (cons (string start)
				       (string end))))
		      ((equal start end)
		       (messages start))))
	      (if (probe-file message) (messages message))))
	(if (messages)
	    (rplacd sequence (messages))
	    (empty sequence))))
    ;; FIX should empty sequences be deleted?
    (dolist (sequence (empty))
      (if (string= (car sequence) "cur")
	  (rplacd sequence
		  (list (string (folder-info-highest folder-info))))
	  (setf (folder-info-sequences folder-info)
		(delq sequence (folder-info-sequences folder-info))))))
  ;;
  ;; Replace sequences on disk.
  (with-open-file (stream ".mh_sequences" :direction :output
			  :if-exists :new-version)
    (dolist (sequence (folder-info-sequences folder-info))
      (format stream "~A:" (car sequence))
      (dolist (message (cdr sequence))
	(etypecase message
	  (integer ;; FIX integers allowed?
	   (format stream " ~A" message))
	  (string
	   (format stream " ~A" message))
	  (list
	   (format stream " ~A-~A" (car message) (cdr message)))))
      (terpri stream))
    (decf-mess)))

;;; Internal
;;;
;;; Update sequences in $folder given that message $old is now called $new.
;;; The write-date in $folder-info must equal the folder directory for
;;; `mark-message' to use the information cached in $folder-info (else
;;; $folder-info is re-populated from disk).
;;;
(defun rename-in-sequences (folder folder-info old new)
  (mess "(rename-in-sequences ~A . ~A ~A)" folder old new)
  (incf-mess)
  (dolist (sequence (folder-info-sequences folder-info))
    ;; FIX folder-info should hold sequence-list -compat format?
    (let ((seq-list (sequence-list folder (car sequence))))
      (when (sequence-member-p old seq-list)
	;;; Add new first, so that if old is the highest `mark-message'
	;;; will know if old is in the folder cache.
	(mark-message folder new (car sequence) :add)
	(mark-message folder old (car sequence) :delete))))
  (decf-mess))


;;;; Folders.

;;; Public
;;;
(defun maybe-messages-before-p (folder message)
  "Return true if there might be messages before $message in $folder.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (mess "(maybe-messages-before-p ~A ~A)" folder message)
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info)))
    (unwind-protect
	(fi (eq message 1)
	    (let ((lowest (folder-info-lowest (scan-folder folder
							   message))))
	      (if lowest
		  (> message lowest)
		  t)))
      (decf-mess))))

;;; Internal
;;;
;;; Recursively add the folders in $directory (relative to `root-pathname')
;;; to *folder-table*.
;;;
(defun notice-directory (directory)
  (mess "(notice-directory ~A)" directory)
  (let ((dir-len (length (namestring (truename (root-pathname)))))
	(old-table *folder-table*)
	(done))
    (unwind-protect
	(progn
	  (setq *folder-table* (make-string-table))
	  (do-dirs (pathname (merge-pathnames directory (root-pathname))
			     :follow-links t
			     :backups t ; Faster.
			     :recurse t)
	    (when (directoryp (namestring pathname))
	      (mess "  ~A" pathname)
	      (setf (getstring (namify (subseq (namestring pathname)
					       dir-len))
			       *folder-table*)
		    t)))
	  (file-folder-names)
	  (setq done t))
      (if done
	  (mess "  noticed")
	  (setq *folder-table* old-table)))))

;;; Internal
;;;
;;; Ensure that *folder-table* exists.
;;;
(defun ensure-folder-table ()
  (or *folder-table*
      (read-filed-folder-names)
      ; FIX warn adding ~ first time, to stdout
      (notice-directory ".")))

;;; Internal
;;;
;;; Read the names for *folder-table* from file.
;;;
(defun read-filed-folder-names ()
  ;; FIX save old-table, as in notice-directory above
  (mess "(read-filed-folder-names)")
  (let ((cache-pathname (merge-pathnames "folders" (root-pathname))))
    (when (probe-file cache-pathname)
      (mess "  found cached folders file")
      (setq *folder-table* (make-string-table))
      (from-file (stream cache-pathname)
	(ext::do-lines (line stream)
	  (if (plusp (length line))
	      (setf (getstring (namify line) *folder-table*) t)))))
    *folder-table*))

;;; Internal
;;;
;;; Write the folder names in *folder-table* out to the folders file.
;;;
(defun file-folder-names ()
  (if *folder-table*
      (to-file (stream (merge-pathnames "folders"
					       (root-pathname))
		       :if-does-not-exist :create
		       :if-exists :supersede)
	(do-strings (string value *folder-table*)
	  (format stream "~A~%" string)))))

;;; Public
;;;
(defun get-folder-table ()
  "Return the table of folder names."
  (ensure-folder-table)
  *folder-table*)

;;; Public
;;;
(defun update-folder-table ()
  "Update the table of folder names."
  (setq *folder-table* ())
  (get-folder-table))

;;; Public
;;;
(defun folder-exists-p (folder)
  "Return true if the directory for $folder exists.  Expect $folder to be a
   simple-string specifying a folder name relative to the MH mail
   directory."
  (declare (simple-string folder))
  (let ((pf (probe-file (folder-pathname folder))))
    (and pf (directoryp pf))))

;;; Public
;;;
(defun create-folder (folder)
  "Create a mail directory called $folder.

   If \"Folder-Protect\" component exists then use that value as the
   permission of the directory (in octal), otherwise use #o755
   (which is drwxr-xr-x).

   Signal an error if creating the directory fails."
  (declare (simple-string folder))
  (let* ((folder (namify (strip-folder-name folder)))
	 (pathname (folder-pathname folder))
	 (ses-name (namestring pathname))
	 (length-1 (1- (length ses-name)))
	 (name (if (= (position #\/ ses-name :test #'char= :from-end t)
		      length-1)
		   ses-name
		   (subseq ses-name 0 (1- (length ses-name)))))
	 (permission (profile-component "folder-protect"))
	 ;; Get the folder table before adding the directory, otherwise the
	 ;; directory may be noticed when getting the table.
	 (folder-table (get-folder-table)))
    (multiple-value-bind (success err)
			 (ensure-directories-exist name)
      (or success
	  (error "Failed to make directory ~S: ~A" name err)))
    (setf (getstring folder folder-table) t)
    (file-folder-names)
    (if permission
	(setf (file-mode name)
	      (parse-integer permission :radix 8 :junk-allowed t)))))

;;; Public
;;;
(defun delete-folder (folder)
  "Delete $folder from the mail directory, the folder table and the cache
   of folders.

   Return t on success.  If the folder contains files other than the usual
   mail files then only remove the folder from the folder table and folder
   cache and return () and a list of these files.

   Signal an error on failure to delete the folder from the directory."
  ;; FIX error if folder current dir
  ;; FIX sort out new folder? (new current folder?)
  ;; FIX if COMPAT m_delete current - m_mailpath(folder)
  ;; FIX check for profile entry atr- current - m_mailpath?
  ;;
  ;; FIX why return skipped at all?
  (in-directory (folder-pathname folder)
    (let (others (skipped))
      (do-files (file (folder-pathname folder)
		 :recurse t
		 :follow-links ()
		 :backups t)
	(block nil
	  (let ((name (file-namestring file)))
	    (case (char name 0)
	      ((#\. #\, #\+ #\_ #\#)) ;; FIX some COMPAT,UCI
	      (t (or (parse-integer name :errorp ())
		     (string= name "cur")  ;; FIX COMPAT current
		     (string= name "@")  ;; FIX LINK
		     (string= name ",")  ;; FIX SBACKUP
		     (progn
		       (push name skipped)
		       (setq others t)
		       (return-from nil)))))
	    (delete-file name))))
      ;; FIX rma folder   seems to clear all "atr- folder" headers
      (delete-string (namify (strip-folder-name folder))
		     (get-folder-table))
      (file-folder-names)
      (if others
	  (values () skipped)
	  (t (delete-dir (folder-pathname folder)))))))

;;; Public
;;;
(defun rename-folder (folder new-name)
  "Rename $folder to $new-name.

   Signal an error if folder $new-name already exists, if $folder is
   missing, or if renaming the folder directory fails."
  (if (probe-file (folder-pathname new-name))
      (error "Folder already exists: ~A" new-name))
  (let ((dir (folder-pathname folder))
	(new (namestring (folder-pathname new-name)))
	;; Get the table before renaming the directory, otherwise
	;; `get-folder-table' might notice the new directory.
	(folder-table (get-folder-table)))
    (ensure-directories-exist (namify new))
    (rename-file dir new)
    (delete-string (namify (strip-folder-name folder)) folder-table)
    (setf (getstring (namify (strip-folder-name new-name))
		     folder-table)
	  t)
    (file-folder-names)))

;;; Public
;;;
(defun print-folders (stream &optional (prefix "") new-p)
  "Output the name of each folder to $stream.  Output $prefix before every
   name and a new line after every name.

   If $stream is t then print to standard output.

   If $new-p is true then print a count of new messages between $prefix and
   the folder.

   Print $prefix with `format' directive ~A."
  (or stream (error "$stream must be a stream or t."))
  (if new-p
      (let ((in-sequence (profile-component "unseen-sequence")))
	(do-folders folder
	  (format stream "~A~@3<~A~> ~A~%"
		  prefix
		  (let ((seqs (sequence-list folder in-sequence))
			(count 0))
		    (dolist (seq seqs)
		      (etypecase seq
			(integer (incf count))
			(list (incf count (1+ (- (cdr seq) (car seq)))))))
		    (if (zerop count) "" count))
		  folder)))
      (do-folders folder
	(format stream "~A~A~%" prefix folder))))

;;; Internal
;;;
;;; Write a single message from $input on $output, reading over the message
;;; separator line.
;;;
(defun transfer-message (input output)
  ;; FIX what happens when a message contains a line starting "From "?
  (while ((line (read-line input ()) (read-line input ()))
	  (last-line-was-empty))
	 (line)
    ;; FIX should check for delimiter how?
    ;; FIX how to search in stream?
    (if (zerop (length line))
	(setq last-line-was-empty t)
	(if last-line-was-empty
	    (progn
	      (and (>= (length line) 5)
		   (string= line "From " :end1 5)
		   ;; The "From " line must be followed by at least one
		   ;; header.
		   (let ((pos (file-position input))
			 (next (read-header input t)))
		     (prog1 (fi (eq next :body))
		       (file-position input pos)))
		   (return))
	      (setq last-line-was-empty ()))))
    (write-line line output)))

;;; Internal
;;;
;;; Return an alist of the headers in stream $input.  If date synthesis is
;;; required use the last modification time of $file or the current time if
;;; $file is ().
;;;
(defun scan-message (input &optional file (scan-body t))
  (let ((alist (read-headers input scan-body)))
    (when alist
      (let* ((date (cdr (assoc "Date" alist :test #'string=)))
	     (uni (if date
		      (or (parse-time
			   date
			   :patterns
			   (append ext::*http-date-time-patterns*
				   ext::*default-date-time-patterns*))
			  ;; FIX parse-time failures
			  ;;(error "parse-time failed on ~A" date))
			  ())
		      (if file
			  (file-write-date file)
			  (get-universal-time)))))
	(push (cons "Universal-Date" uni) alist))
      alist)))

;;; Internal
;;;
;;; Return the ID of the highest message in $messages.  Return :highest to
;;; indicate that the highest in $messages is the highest in the folder.
;;;
(defun messages-highest (messages sequences)
  (when messages
    (etypecase messages
      (integer messages)
      (symbol
       (ecase messages
	 (:all :highest)
	 (:sequences :highest)
	 (:highest :highest)))
      (list
       ;; Search through all the messages even though they should be
       ;; sorted, to account for sequences and overlapping ranges.
       ;;
       ;; TODO This could take advantage of the last range or integer
       ;; always being the highest of the ranges and integers.
       (let ((highest))
	 (dolist (message messages)
	   (etypecase message
	     (integer
	      (if (if highest (> message highest) t)
		  (setq highest message)))
	     (symbol
	      (ecase message
		((or :all :highest)
		 (return-from messages-highest :highest))))
	     (list
	      (let ((first (car message)))
		(etypecase first
		  (symbol
		   (ecase first
		     ((or :all :highest)
		      (return-from messages-highest :highest))
		     (:sequence
		      (let ((current (sequences-highest (cdr message)
							sequences)))
			(if (if highest (> current highest) t)
			    (setq highest current))))))
		  (integer
		   (if (if highest (> (cdr message) highest) t)
		       (setq highest (cdr message)))))))))
	 highest)))))

;;; Internal
;;;
;;; Return the ID of the lowest message in $messages.  Return :lowest if
;;; $messages described all messages in the folder.
;;;
(defun messages-lowest (messages sequences)
  (when messages
    (etypecase messages
      (integer messages)
      (symbol
       (ecase messages
	 (:all :lowest)
	 (:sequences :highest)
	 (:highest :highest)))
      (list
       ;; Search through all the messages even though they should be
       ;; sorted, to account for sequences and overlapping ranges.
       (let ((lowest))
	 (dolist (message messages)
	   (etypecase message
	     (integer
	      (if (if lowest (< message lowest) t)
		  (setq lowest message)))
	     (symbol
	      (ecase message
		(:all
		 (return-from messages-lowest :lowest))))
	     (list
	      (let ((first (car message)))
		(etypecase first
		  (symbol
		   (ecase first
		     (:all (return-from messages-lowest :lowest))
		     (:sequence
		      (let ((current (sequences-lowest (cdr message)
						       sequences)))
			(if (if lowest (< current lowest) t)
			    (setq lowest current))))))
		  (integer
		   (if (if lowest (< first lowest) t)
		       (setq lowest first))))))))
	 lowest)))))

;;; Internal
;;;
;;; Add a description of $folder to *folder-table*, returning the
;;; description.
;;;
;;; If the cleaned message spec, $messages, is given then ensure that the
;;; cache includes all the messages.  If $range is also given then ensure
;;; that the cache includes $range messages after the lowest message in
;;; $messages (or before the highest if $range if negative).
;;;
;;; If $messages is (), cache all messages.
;;;
;;; Assume that the sequences for folder are up to date on disk.
;;;
(defun cache-folder (folder &optional messages range)
  (mess "(cache-folder ~A ~A ~A)" folder messages range)
  (incf-mess)
  (if range
      (if (plusp range)
	  (error "FIX implement positive range")
	  (setq range (- range))))
  (let ((dir (folder-pathname folder))
	(folder (strip-folder-name folder))
	(msgs)
	(*in-cache-folder* t))
    (in-directory dir
      (let* ((sequences (get-sequences))
	     (highest (or (sequences-highest "highest" sequences)
			  (prog1
			      (add-highest-sequence folder dir)
			    (setq sequences (get-sequences)))))
	     (index (if messages
			(let ((m-h (messages-highest messages
						     sequences)))
			  (case m-h
			    (:highest highest)
			    (:lowest 1)
			    (t m-h)))
			highest))
	     (lowest (if messages
			 (let ((m-l (messages-lowest messages
						     sequences)))
			   (case m-l
			     (:lowest 1)
			     (:highest highest)
			     (t m-l)))
			 (if (plusp index) 1 0)))
	     (count 0)
	     (last ()))
	(mess "index ~A" index)
	(mess "highest ~A" highest)
	(mess "lowest ~A" lowest)
	(mess "range ~A" range)
	;; Ensure the current sequence exists.
	(fi* (sequences-highest "cur" sequences)
 	  (mark-message-directly folder :all "cur" :delete)
	  (mark-message-directly folder (string highest) "cur" :add)
	  (setq sequences (get-sequences)))
	(let ((descr (make-folder-info dir
				       (file-write-date dir)
				       highest
				       ()
				       sequences)))
	  ;; Index can be () for an empty or missing sequence.
	  (when index
	    (setf (folder-info-end descr)
		  (if messages
		      (or (and (eq highest index)
			       (eq lowest 1))
			  index)
		      t))
	    (flet ((scan-msg (name)
		     (with-open-file (input name :direction :input)
		       (or (scan-message input name)
			   (progn
			     ; FIX consider (f (format ..))
			     ;     equiv of `t'
			     (format t
				     "Failed to scan message ~A in ~A~%"
				     name folder)
			     ())))))
	      (flet ((cache-msg (name id)
		       (when (probe-file name)
			 (push (cons id (scan-msg name)) msgs)
			 (setq last id))))
		;; msgs comes out in increasing order.
		(loop
		  (cache-msg (string index) index)
		  (if range
		      (when (>= (incf count) range)
			(if (= index 1)
			    ;; Tried all messages.
			    (setf (folder-info-lowest descr) last))
			(return))
		      (when (= index lowest)
			(if (= index 1)
			    ;; Tried all messages.
			    (setf (folder-info-lowest descr) last))
			(return)))
		  (when (<= index 1)
		    ;; Tried all messages.
		    (setf (folder-info-lowest descr) last)
		    (return))
		  (decf index))))
	    (setf (folder-info-start descr) (or last 0))
	    (setf (folder-info-messages descr) msgs))
	  (setf (getstring folder (get-folder-table)) descr)
	  (mess "returning:")
	  (mess "  sequences ~A" (folder-info-sequences descr))
	  (mess "  highest ~A" (folder-info-highest descr))
	  (mess "  lowest ~A" (folder-info-lowest descr))
	  (mess "  end ~A" (folder-info-end descr))
	  (mess "  start ~A" (folder-info-start descr))
	  (decf-mess)
	  descr)))))

;;; Internal
;;;
;;; Return true if $folder-info includes all the messages listed by integer
;;; ID in $messages and the $range messages after the lowest message in
;;; $messages (or before the highest if $range is negative)."
;;;
(defun folder-info-includes (folder-info messages &optional range)
  (mess "(folder-info-includes . ~A ~A)" messages range)
  (or (eq (folder-info-end folder-info) t)
      (eq messages :sequences)
      (let* ((sequences (progn
			  (incf-mess)
			  (folder-info-sequences folder-info)))
	     (highest (or (messages-highest messages sequences)
			  :highest))
	     (lowest (or (messages-lowest messages sequences)
			 :lowest)))
	(mess "sequences: ~A" sequences)
	(case highest
	  (:highest
	   (setq highest
		 (or (caar (%sequence-list "highest" sequences))
		     ;; Highest sequence is missing, force call to
		     ;; `cache-folder', which adds the sequence.
		     (progn
		       (decf-mess)
		       (return-from folder-info-includes))))))
	(case lowest
	  (:highest
	   (setq lowest highest))
	  (:lowest
	   ;; $messages describes all messages.
	   (mess "lowest was :lowest")
	   (decf-mess)
	   (return-from folder-info-includes)))
	(mess "highest requested: ~A" highest)
	(mess "lowest requested: ~A" lowest)
	(mess "start: ~A" (folder-info-start folder-info))
	(mess "end: ~A" (folder-info-end folder-info))
	(or (and (folder-info-start folder-info)
		 (folder-info-end folder-info))
	    ;; Probably empty, force cache anyway.
	    (return-from folder-info-includes))
	(if (> lowest highest) (swap lowest highest))
	(decf-mess)
	(if range
	    (if (plusp range)
		(error "FIX implement positive $range")
		(and (<= highest (folder-info-end folder-info))
		     (let ((pos (position lowest
					  (folder-info-messages
					   folder-info))))
		       (and pos (> pos (- range))))))
	    (and (<= highest (folder-info-end folder-info))
		 (>= lowest (folder-info-start folder-info)))))))

;;; Internal
;;;
;;; Return a folder-info structure describing $folder.
;;;
(defun scan-folder (folder &optional messages range)
  (mess "(scan-folder ~A ~A ~A)" folder messages range)
  (incf-mess)
  (let* ((dir (folder-pathname folder))
	 (info (getstring (strip-folder-name folder)
			  (get-folder-table))))
    (case info
      ((t)
       (mess "caching first time")
       (prog1
	   (cache-folder folder messages range)
	 (decf-mess)))
      ((())
       ;; FIX This can happen if the folder name cache (as in
       ;; `file-folder-names') is out of date (for example, if the
       ;; directory is added manually).
       (decf-mess)
       (error "~A should be in *folder-table* already." folder))
      (t
       (let ((date (file-write-date dir)))
	 (or date (error "Failed to read modification time on ~A." dir))
	 ;; FIX assumes normal msgs (besides drafts) always stay the same
	 ;;     eg might need to update if a msg has been annotated
	 (if (equal (folder-info-write-date info) date)
	     (cond ((eq (folder-info-end info) t)
		    (mess "cache holds all")
		    (decf-mess)
		    info)
		   ((if messages
			(folder-info-includes info messages range))
		    (mess "cache includes messages")
		    (decf-mess)
		    info)
		   (t
		    (mess "cache too small")
		    (prog1
			(cache-folder folder messages range)
		      (decf-mess))))
	     (progn
	       (mess "cache out of date")
	       (prog1
		   (cache-folder folder messages range)
		 (decf-mess)))))))))

;;; Public
;;;
;;; TODO: Add [sub-]sorting by other headers, e.g. Subject.
;;;
(defun sort-folder (folder)
  "Sort the messages in $folder by date."
  (mess "(sort-folder ~A)" folder)
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :all))
	 (dir (folder-pathname folder))
	 order)
    ;; Prepare an ordered list of the available message numbers.
    (dolist (msg (folder-info-messages folder-info))
      (push (car msg) order))
    (setq order (sort order #'<))
    ;; Sort the list of messages.
    (in-directory dir
      (handler-bind ((error (lambda (err)
			      (declare (ignore err))
			      ;; Try match the disk sequences to the cache
			      ;; state.
			      (update-sequences folder-info)
			      ;; Clear the cache of the folder.
			      (setf (getstring folder
					       (get-folder-table))
				    t))))
	(setf (folder-info-messages folder-info)
	      (sort (folder-info-messages folder-info) #'message-older))
	;; Rename the files to match the sorted list.
	(let* ((tem-name (pick-new-file "tem-~D~D"))
	       (tem-id (1+ (folder-info-highest folder-info)))
	       (tem-msg (string tem-id)))
	  (while ((sorted (folder-info-messages folder-info)
			  (cdr sorted))
		  (ordered order (cdr ordered)))
		 ((and sorted ordered)
		  ;; Update the rest of folder-info.
		  (let ((high (car (last order))))
		    (setf (folder-info-lowest folder-info)
			  (car order))
		    (setf (folder-info-highest folder-info) high)
		    (setf (folder-info-start folder-info)
			  (car order))
		    (setf (folder-info-end folder-info) t)
		    ;; Update .mh_sequences so that it matches the
		    ;; sequences in folder-info.  Also update the highest
		    ;; sequence in folder-info to match the highest slot.
		    ;;
		    ;; FIX This is a bit of a waste as cache sequences
		    ;; should match the files already.
		    (update-sequences folder-info)))
	    (let ((smsg (string (caar sorted)))
		  (omsg (string (car ordered))))
	      (or (string= smsg omsg)
		  (progn
		    ;; FIX Ensure that the renaming happens completely, or is
		    ;;     reverted on failure.
		    ;; Swap the file names.
		    (rename-file omsg tem-name)
		    (rename-file smsg omsg)
		    (rename-file tem-name smsg)
		    ;; Ensure `rename-in-sequences' uses folder-info.
		    (setf (folder-info-write-date folder-info)
			  (file-write-date dir))
		    ;; Update folder cache sequences.  This includes the
		    ;; current and highest message.
		    (rename-in-sequences folder folder-info
					 omsg tem-msg)
		    (rename-in-sequences folder folder-info
					 smsg omsg)
		    (rename-in-sequences folder folder-info
					 tem-msg smsg)
		    ;; Rename the entries in the sorted list.
		    (rplaca (assoc (car ordered) sorted)
			    (caar sorted))
		    (rplaca (car sorted) (car ordered))))))
	  (if (probe-file tem-name) (delete-file tem-name)))))
    (decf-mess)))

;;; Public
;;;
(defun pack-folder (folder)
  "Number the messages in $folder consecutively, starting from one."
  (mess "(pack-folder ~A)" folder)
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :all))
	 (dir (folder-pathname folder)))
    ;; Sort the list of messages by message number.
    (setf (folder-info-messages folder-info)
	  (sort (folder-info-messages folder-info) #'message-<=))
    ;; Rename the files.
    (in-directory dir
      (handler-bind
	  ((error (lambda (err)
		    (declare (ignore err))
		    ;; Try match the disk sequences to the cache
		    ;; state.
		    (update-sequences folder-info)
		    ;; Clear the cache of the folder.
		    (setf (getstring folder (get-folder-table))
			  t))))
	(let ((num 0))
	  (while ((messages (folder-info-messages folder-info)
			    (cdr messages)))
		 (messages)
	    (incf num)
	    (let ((id (string (caar messages)))
		  (new-id (string num)))
	      (or (string= id new-id)
		  (progn
		    (rename-file id new-id)
		    ;; Ensure `rename-in-sequences' uses folder-info.
		    (setf (folder-info-write-date folder-info)
			  (file-write-date dir))
		    ;; Update sequences, including current message.
		    (rename-in-sequences folder folder-info
					 id new-id)
		    ;; Rename the entry in the cached message list.
		    (rplaca (car messages) new-id)))))
	  (if (plusp num)
	      (setf (folder-info-start folder-info) 1
		    (folder-info-end folder-info) num
		    (folder-info-lowest folder-info) 1
		    (folder-info-highest folder-info) num)
	      (setf (folder-info-start folder-info) 0
		    (folder-info-end folder-info) 0
		    (folder-info-lowest folder-info) 0
		    (folder-info-highest folder-info) 0))
	  ;; Update cache and .mh_sequences.
	  (update-sequences folder-info)))
      (decf-mess))))


;;;; Headers.

;;; Public
;;;
(defvar *body-scan-length* 50
  "Maximum number of characters to scan from message body.")

;;; Internal
;;;
;;; Read and return name and text of next header in $stream.  If at the
;;; message body and $scan-body is true return *body-scan-length*
;;; characters from the beginning of the body.  For the body header return
;;; the position of the start of the body as a third value.
;;;
(defun read-header (stream scan-body)
  (let ((char (read-char stream ())))
    (when char
      (if (member char '(#\newline #\- #\return))
	  (if scan-body
	      ;; Header body separator.
	      (let ((body (make-string *body-scan-length*
				       :initial-element #\space)))
		(while ((ch char (read-char stream ())))
		       (ch)
		  (if (member ch '(#\newline #\return)) (return)))
		(let ((pos (file-position stream)))
		  (while ((len 0 (1+ len))
			  (char (read-char stream ())
				(read-char stream ())))
			 ((and char (< len (1- *body-scan-length*))))
		    (setf (char body len)
			  (cond ((member char
					 '(#\tab #\newline
					   #\return #\null))
				 #\space)
				(t char))))
		  (values :body body pos))))
	  ;; Header.
	  (let ((text ""))
	    (declare (simple-string text))
	    (unread-char char stream)
	    ;; First line.
	    (let* ((line (read-line stream))
		   (pos (or (position #\: line)
			    (return-from read-header ())))
		   (name (subseq line 0 pos))
		   (line-length (length line)))
	      (incf pos) ; :
	      (if (>= pos line-length)
		  (setq text "")
		  (progn
		    ;; FIX ensure pos stays within string
		    (until ((po pos (1+ po)))
			   ((eq po line-length))
		      (let ((char (char line po)))
			(or (char= char #\space)
			    (char= char #\tab)
			    (return))
			(incf pos)))
		    (setq text
			  (subseq line pos
				  (if (char= (char line
						   (1- line-length))
					     #\return)
				      (1- line-length)
				      line-length)))
		    ;; More lines.
		    (while ((char (peek-char () stream ())
				  (peek-char () stream ())))
			   (char)
		      (or (char= char #\space)
			  (char= char #\tab)
			  (return))
		      (let* ((line (read-line stream))
			     ;(line-length (length line))
			     )
			(declare (simple-string line))
			#|
			(when (every (lambda (ch)
				       (member ch '(#\space #\tab)))
				     line)
			  (unread-char #\newline stream)
			  (return))
			|#
			;; Keep the leading space of these lines, so that
			;; it is written if this message is sent.
			;; FIX ^M check
			(setq text
			      (concatenate
			       'simple-string
			       text
			       "
				"
			       line
			       #| FIX too slow, eg for many addresses
			       (subseq
				line 0
				(if (char= (char line
						 (1- line-length))
					   #\return)
				    (1- line-length)
				    line-length))
			       |#
			       ))))))
	      (values name text)))))))

;;; Internal
;;;
;;; Return an alist of the headers in the first message in $stream.
;;; Include the message body excerpt (with key :body) if $scan-body is
;;; true.
;;;
(defun read-headers (stream scan-body)
  (collect ((alist))
    (loop
      (multiple-value-bind (name text pos)
			   (read-header stream scan-body)
	(or name (return))
	(when (eq name :body)
	  (alist (list (if (stringp name)
			   (nstring-capitalize name)
			   name)
		       text
		       pos))
	  (return))
	(alist (cons (if (stringp name)
			 (nstring-capitalize name)
			 name)
		     text))))
    (alist)))

;;; Internal
;;;
;;; Return a short date string from header $date.  Return () if parsing the
;;; date fails.
;;;
(defun strip-date (date)
  ;;; Sun, 10 Dec 2006 15:49:51 +0000
  ;;; Wed,  6 Dec 2006 15:51:45 +0000
  ;;; 12 Oct 2006 05:26:43 -0000
  (multiple-value-bind (sec min hour day month)
		       (decode-universal-time
			(or (parse-time
			     date
			     :patterns
			     (append ext::*http-date-time-patterns*
				     ext::*default-date-time-patterns*))
			    (return-from strip-date ())))
    (declare (ignore sec min hour))
    ;; FIX if date > 365 days ago print mon-yr (same for file-date below)
    (format () "~2,'0D-~A" day (short-month-name month))))

;;; Internal
;;;
;;; Return a short date from the write date of $file.
;;;
(defun file-date (file)
  (multiple-value-bind (sec min hour date month)
		       (decode-universal-time
			(or (file-write-date file)
			    (return-from file-date ())))
    (declare (ignore sec min hour))
    (format () "~2,'0D-~A" date (short-month-name month))))

;;; Internal
;;;
;;; Return a name from From header $from.
;;;
;;;   "name" <a@b.c>  =>  name
;;;   name <a@b.c>    =>  name
;;;   <a@b.c>         =>  a@b.c
;;;   a@b.c           =>  a@b.c
;;;
(defun strip-name (from)
  (if from
      (let* ((from (string-left-trim '(#\space #\tab) from))
	     (left (position #\< from)))
	(if left
	    (if (> left 0)
		(let ((from (string-right-trim
			     '(#\space #\tab)
			     (subseq from
				     (if (char= (char from 0) #\") 1 0)
				     left))))
		  (if (char= (char from (1- (length from))) #\")
		      ;; "name" <a@b.c>
		      (string-right-trim
		       '(#\space #\tab)
		       (subseq from 0 (1- (length from))))
		      ;; name <a@b.c>
		      from))
		;; <a@b.c>
		(let ((right (position #\> from)))
		  (subseq from (1+ left) right)))
	    ;; a@b.c
	    (string-right-trim '(#\space #\tab) from)))
      ""))


;;;; Iteration.

;;; Public
;;;
(defmacro do-folders (folder &body body)
  "do-folders folder body

   Run $body on the name of every folder with $folder bound to the name."
  ;; FIX folder.c dother also prints folders named in atr-cur- profile
  ;;     header
  (let ((two (gensym)))
    `(progn
       (ext::do-strings (,folder ,two (get-folder-table))
	 (declare (ignore ,two))
	 ,@body))))

;;; Internal
;;;
;;; Run $body with each message in $sequence bound to $message.  $sequence
;;; is a list as in the folder-info sequence slot.
;;;
(defmacro do-sequence ((message sequence) &body body)
  (let ((id (gensym)))
    `(dolist (,message ,sequence)
       (if (listp ,message)
	   (while ((,id (parse-integer (car ,message)) (1+ ,id)))
		  ((<= ,id (parse-integer (cdr ,message))))
	     (let ((,message ,id))
	       ,@body))
	   (let ((,message (parse-integer ,message :junk-allowed t)))
	     (if ,message ,@body))))))

;;; do-messages  --  Internal
;;;
;;; For each message in $messages run $body with $message bound to the
;;; integer id of message and $entry bound to the cache entry for the
;;; message.
;;;
;;; Assume that the messages are already in the cache, as with
;;; `scan-folder'.
;;;
;;; $messages can be a single message or a list or messages.  A message in
;;; $messages can be an integer message id, a (start-id-integer .
;;; end-id-integer) range or a (:sequence . string-name) sequence
;;; description, the symbol :all or a list where the first element is the
;;; symbol :all.
;;;
;;; If $messages is (), the symbol :all or a list starting with :all, then
;;; do all messages in $folder.
;;;
;;; FIX body is inlined 4 times (make it an flet?)
;;;
(defmacro do-messages ((message entry messages folder folder-info)
		       &body body)
  (let ((id (gensym))
	(last (gensym))
	(sequence (gensym))
	(folder-messages (gensym))
	(sequences (gensym)))
    `(let ((,folder-messages (folder-info-messages ,folder-info))
	   (,sequences (folder-info-sequences ,folder-info)))
       (mess "sequences: ~A" ,sequences)
       (if (if ,messages
	       (or (eq ,messages :all)
		   (eq (car ,messages) :all))
	       t)
	   ;; Do all the messages in the cache.
	   (dolist (,entry ,folder-messages)
	     (let ((,message (car ,entry)))
	       ,@body))
	   (dolist (,message
		    ;; FIX ,messages always list now?
		    (if (listp ,messages) ,messages (list ,messages))
		    ,last)
	     (etypecase ,message
	       (integer
		(setq ,last ,message)
		(let ((,entry (assoc ,message ,folder-messages)))
		  (if ,entry
		      (progn
			,@body)
		      ;; FIX an error if message missing.  eg for
		      ;;     `delete-messages'.
		      ;;     one other pub fun expects it just to skip.
		      (error
		       "Failed to find message ~A in ~A cache."
		       ,message ,folder))))
	       (list
		(etypecase (car ,message)
		  (symbol
		   (ecase (car ,message)
		     (:sequence
		      (let ((,sequence (cdr (assoc (cdr ,message)
						   ,sequences
						   :test #'string=))))
			(if ,sequence
			    (do-sequence (,message ,sequence)
			      (let ((,entry (assoc ,message
						   ,folder-messages)))
				(setq ,last ,message)
				(or ,entry
				    (error
				     "Failed to find sequence message ~A in ~A cache."
				     ,message ,folder))
				(progn ,@body))))))))
		  (integer
		   (iterate iter ((,id (car ,message)))
		     (let ((,entry (assoc ,id ,folder-messages)))
		       (or ,entry
			   (error "Failed to find range message ~A in ~A cache."
				  ,id ,folder))
		       (setq ,last ,id)
		       (let ((,message ,id)) ,@body))
		     (or (eq ,id (cdr ,message))
			 (iter (1+ ,id))))))))))
       ;;
       #|
       (when ,range
	 (if (plusp ,range) (error "FIX implement positive range"))
	 ;; The range includes the highest message in the associated
	 ;; message spec.
	 (incf ,range)
	 ;; FIX this could hang, maybe add safety check on (< ,message highest)
	 (loop
	   (mess "range ~A, message ~A" ,range ,message)
	   (if (zerop ,range) (return))
	   (decf ,message)
	   (let ((,entry (assoc ,message ,folder-messages)))
	     (when entry
	       (progn ,@body)
	       (incf ,range)))))
       |#
       )))


;;;; Summarizing.

;;; Internal
;;;
;;; Replace newlines in $string with spaces and return $string.
;;;
(defun replace-newlines (string)
  (while ((index 0 (1+ index))
	  (end (length string)))
	 ((< index end))
    (if (char= (char string index) #\newline)
	(setf (char string index) #\space)))
  string)

;;; Public
;;;
(defun summarize-message (pathname stream &optional (width 80) current)
  "If $pathname contains a message with headers then summarize the message
   on $stream and return #t, else return ().

   Limit the width of the summary to $width characters.

   Mark the message with a + after the ID if $current is true."
  ;; 293   10-Dec   837 Anacron           Anacron job 'cron.daily' on cspcz01   <</etc/cron.daily/logrotate: apac
  (let ((pathname (string pathname)))
    (with-open-file (input pathname :direction :input)
      (let ((alist (read-headers input t)))
	(when alist
	  (let ((from (cdr (assoc "From" alist :test #'string=)))
		(body (cadr (assoc :body alist)))
		(date (cdr (assoc "Date" alist :test #'string=))))
	    ;; If the message is from the current user then put "To: " and
	    ;; the To address in the from column.
	    (dolist (box (cons *from-name* *alternate-addresses*)
			 (setq from (strip-name from)))
	      (when (search box from)
		(setq from
		      (concat "To: "
			      (strip-name
			       (cdr (assoc "To" alist
					   :test #'string=)))))
		(return)))
	    (format stream
		    "~4<~A~>~:[ ~;+~]~:[ ~;-~] ~6<~A~>~:[*~; ~] ~5<~A~> ~@17<~A~> ~@37<~A~>"
		    (safe-subseq (file-namestring pathname) 0 4)
		    current
		    (cdr (assoc "Replied" alist :test #'string=))
		    (if date
			(or (strip-date date) "FIX")
			(file-date pathname))
		    date
		    (let ((size (file-size pathname)))
		      (cond
		       ((> size 10238975)
			(format () "~DM" (truncate (/ (/ size 1024.0)
						      1024))))
		       ((> size 99999)
			(format () "~DK" (truncate (/ size 1024))))
		       (t size)))
		    (safe-subseq from 0 17)
		    ;; FIX This can leave trailing space if it is the last
		    ;;     output.
		    (safe-subseq (replace-newlines
				  (or (cdr (assoc "Subject" alist
						  :test #'string=))
				      ""))
				 0 37))
	    ;; pre-width is the length of string produced by format above.
	    (let ((pre-width 77))
	      (if (and body (> width pre-width))
		  (let ((body (string-trim
			       '(#\space #\tab)
			       (if (> (- width pre-width)
				      (length body))
				   body
				   (subseq body
					   0 (- width pre-width))))))
		    (or (string= body "")
			(format stream "  ~A" body))))))
	  (terpri stream)
	  t)))))

(declaim (special *sequences*))

;;; Internal
;;;
;;; Return true if $item1 is less than $item2.
;;;
(defun messages-item-< (item1 item2)
  (etypecase item1
    (symbol ()) ; :all or :highest
    (list (etypecase item2
	    (symbol t) ; :all or :highest
	    (list
	     (case (car item1)
	       (:sequence
		(> (case (car item2)
		     (:sequence (sequences-lowest (cdr item2)
						  *sequences*))
		     (t (cdr item2)))
		   (sequences-lowest (cdr item1) *sequences*)))
	       (t
		(> (case (car item2)
		     ; FIX also check sequence-highest?
		     (:sequence (sequences-lowest (cdr item2)
						  *sequences*))
		     (t (cdr item2)))
		   (cdr item1)))))
	    (integer
	     (case (car item1)
	       (:sequence (> item2 (sequences-lowest (cdr item1)
						     *sequences*)))
	       (t (> item2 (cdr item1)))))))
    (integer
     (etypecase item2
       (integer (< item1 item2))
       (symbol t) ; :all or :highest
       (list (case (car item2)
	       (:sequence (< item1 (sequences-lowest (cdr item2)
						     *sequences*)))
	       (t (< item1 (cdr item2)))))))))

;;; Public
;;;
;;; FIX "range" clashes with range in spec
;;;
(defun summarize-messages (folder messages stream
				  &key (width 80) range)
  "Summarize $messages in $folder on $stream.  Limit the width of the
   summary to $width characters.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\".

   If $range is given, include that many message before $messages if range
   is negative, or throw an error if range is positive."
  (mess "(summarize-messages ~A ~A . :range ~A)" folder messages range)
  (incf-mess)
  (let* ((folder (namify folder))
	 (dir (folder-pathname folder))
	 (width (or width 80))
	 ;; Scan to cache the sequences.
	 (folder-info (scan-folder folder :sequences))
	 (messages (clean-messages-spec
		    messages
		    (folder-info-sequences folder-info)))
	 ;; Scan again to cache messages.
	 (folder-info (scan-folder folder messages range)))
    (in-directory dir
      (cond (range
	     (let ((info-messages (folder-info-messages folder-info))
		   (msgs))
	       (while ((index (let ((hi (messages-highest
					 messages
					 (folder-info-sequences
					  folder-info))))
				(if (eq hi :highest)
				    (car (last info-messages))
				    hi))
			      (1- index)))
		      ((and index (plusp index)))
		 ;; FIX cached message list should be an array?
		 (let ((entry (assoc index info-messages)))
		   (when entry
		     (let ((message (car entry)))
		       (push (car entry) msgs))
		     (or (minusp (incf range)) (return)))))
	       (dolist (message msgs)
		 (mess "summarize ~A" message)
		 (summarize-message message stream width)
		 (mess "summarize ~A done" message))))
	    (t
	     (do-messages (message entry messages folder folder-info)
	       (mess "summarize ~A" message)
	       (summarize-message message stream width)
	       (mess "summarize ~A done" message))))))
  (decf-mess))

;;; Public
;;;
(defun summarize-folder (folder stream &optional width)
  "Summarize all the messages in $folder on $stream.  Limit the width of
   the summary to $width characters."
  (summarize-messages folder '(:all) stream :width width))


;;;; Quoted-printable translation.

;;; Internal
;;;
(defvar *hex2nib*
  #(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
    #x08 #x09 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x0A #x0B #x0C #x0D #x0E #x0F #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x0a #x0b #x0c #x0d #x0e #x0f #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

;;; to-quoted  --  Internal
;;;
;;; Return a plain text string translated from quoted-printable $string, or
;;; () if there is an error.
;;;
(defun from-quoted (string)
  (flet ((hex-digit-p (digit)
	   (let ((code (char-code digit)))
	     (or (and (>= code 48) (<= code 57))      ; 0 .. 10
		 (and (>= code 65) (<= code 70))      ; A .. F
		 (and (>= code 97) (<= code 102)))))) ; a .. f
    (while* ((end (length string))
	     (out (make-string end))
	     (out-index 0)
	     (index 0))
	    ((< index end)
	     (subseq out 0 out-index))
      (case= (aref string index)
	(#\=
	 (incf index)
	 (or (< index end) (return-from from-quoted))
	 (let ((one (aref string index)))
	   (incf index)
	   (or (char= one #\newline)
	       (let (two)
		 (or (hex-digit-p one) (return-from from-quoted))
		 (setq two (aref string index))
		 (or (hex-digit-p two) (return-from from-quoted))
		 (incf index)
		 (setf (aref out out-index)
		       (code-char
			(logior (ash (aref *hex2nib*
					   (logand (char-code one)
						   #x7f))
				     4)
				(aref *hex2nib*
				      (logand (char-code two)
					      #x7f)))))
		 (incf out-index)))))
	(t
	 (setf (aref out out-index)
	       (aref string index))
	 (incf index)
	 (incf out-index))))))


;;;; Messages.

#[ Message Specification

A message specification is a list that defines a set of messages.  Each
element of the list can be

  * the ID of a message, as an integers, like 23,

  * the ID of a message, as a string, like "23",

  * a cons of string IDs, like (("20" . "23")), that defines all the
    messages from the car to the cdr,

  * a range string like "20-23" that defines all the messages from the
    first number to the second,

  * a sequence name as a string, like "highest",

  * the string "all",

  * the symbol :all.

Examples:

    '(1 2 3)

    '(1 "2" "7-13" ("100" . "110"))

    '(:all)
]#

;;; Internal
;;;
;;; Return t if the message denoted by cache entry $entry1 is older than or
;;; the same age as the message denoted by $entry2.
;;;
(defun message-older (entry1 entry2)
  (let ((date1 (cdr (get-header entry1 "Universal-Date")))
	(date2 (cdr (get-header entry2 "Universal-Date"))))
    (if date2
	(if date1
	    (if (<= date1 date2) t ())
	    ())
	t)))

;;; Internal
;;;
;;; Return t if the id of $entry1 is less than the id of $entry2.
;;;
(defun message-<= (entry1 entry2)
  (<= (car entry1) (car entry2)))

;;; Internal
;;;
;;; Return an integer of the highest entry in the messages cached in
;;; $folder-info if there are any messages, else 0.
;;;
(defun highest (folder-info)
  (let ((highest 0))
    ;; FIX these should be sorted anyway
    (dolist (message (folder-info-messages folder-info))
      (let ((id (car message)))
	(if (> id highest)
	    (setq highest id))))
    highest))

;;; Public
;;;
(defun move-messages (source dest messages)
  "Move $messages from folder $source to folder $dest.  Assume $dest exists
   and $source is readable.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\"."
  (mess "(move-messages ~A ~A ~A)" source dest messages)
  (incf-mess)
  (let ((source (namify source))
	(dest (namify dest)))
    (fi (string= source dest)
	(let* ((folder-info (scan-folder source :sequences))
	       (messages (clean-messages-spec
			  messages
			  (folder-info-sequences folder-info)))
	       (source-info (scan-folder source messages))
	       (dest-info (scan-folder dest :highest))
	       (source-messages (folder-info-messages source-info))
	       (in-sequence-name (profile-component "unseen-sequence"))
	       (source-in-seq-list (%sequence-list
				    in-sequence-name
				    (folder-info-sequences
				     source-info)))
	       (dest-in-seq-list (%sequence-list
				  in-sequence-name
				  (folder-info-sequences dest-info)))
	       (dest-dir (namestring (folder-pathname dest))))
	  (mess "move-messages sequences: ~A"
		(folder-info-sequences source-info))
	  (in-directory (folder-pathname source)
	    (unwind-protect
		(progn
		  (do-messages (message entry messages
					source source-info)
		    ;; FIX the rest of this block should be ~atomic
		    (let ((dest-id (incf (folder-info-highest
					  dest-info))))
		      (rename-file (string message)
				   (format () "~A/~D"
					   dest-dir dest-id))
		      ;; Update the cache.
		      ;;
		      ;; `update-sequences' adjusts the source sequences
		      ;; afterwards.  The only changes in the dest
		      ;; sequences will be the highest, current and in
		      ;; sequences, all of which are also updated
		      ;; afterwards.
		      ;;
		      ;; Update start, end and lowest before moving the
		      ;; entry, as the `message-*' functions need entry to
		      ;; be present.
		      (if (eq message (folder-info-start source-info))
			  ;; Adjust the source cache start.
			  (setf (folder-info-start source-info)
				(message-after
				 (folder-info-messages source-info)
				 message)))
		      (if (eq message (folder-info-end source-info))
			  ;; Adjust the source cache end.
			  (setf (folder-info-end source-info)
				(message-before
				 (folder-info-messages source-info)
				 message)))
		      (if (eq message (folder-info-lowest source-info))
			  ;; Adjust the source cache lowest.
			  (setf (folder-info-lowest source-info)
				(message-after
				 (folder-info-messages source-info)
				 message)))
		      (or (string= (strip-folder-name dest) "trash")
			  (if (sequence-member-p message
						 source-in-seq-list)
			      ;; Add message to the destination "in" sequence.
			      (setq dest-in-seq-list
				    (sequence-insert
				     dest-id
				     dest-in-seq-list))))
		      ;; Move entry to the destination cache, keeping the
		      ;; cache sorted.
		      (setf (folder-info-messages source-info)
			    (setq source-messages
				  (delq entry source-messages)))
		      (setf (folder-info-end dest-info) dest-id)
		      (rplaca entry dest-id)
		      (setf (folder-info-messages dest-info)
			    (append (folder-info-messages dest-info)
				    (list entry)))
		      ;; This must come after moving the entry, as
		      ;; `highest' uses folder-info-messages.
		      (if (eq message (folder-info-highest source-info))
			  ;; Adjust the source highest.
			  (setf (folder-info-highest source-info)
				(let ((high (highest source-info)))
				  (if (plusp high)
				      high
				      (find-highest "./")))))))
		  ;; Update the folder cache dates.
		  (setf (folder-info-write-date source-info)
			(file-write-date (folder-pathname source)))
		  (setf (folder-info-write-date dest-info)
			(file-write-date (folder-pathname dest))))
	      ;; Update source cache and disk sequences, including current
	      ;; message.
	      (update-sequences source-info)
	      ;; Update dest sequences.
	      (let ((high (folder-info-highest dest-info)))
		(update-sequence dest "highest" (list (cons high
							    high)))
		(update-sequence dest "cur" (list (cons high high)))
		(or (string= (strip-folder-name dest) "trash")
		    (progn
		      (update-sequence dest
				       in-sequence-name
				       dest-in-seq-list)
		      ;; Update dest sequences on disk so that the cache
		      ;; "in" sequence is up to date.
		      (setf (folder-info-sequences dest-info)
			    (in-directory (folder-pathname dest)
			      (get-sequences))))))
	      (decf-mess)))))))

;;; Public
;;;
(declaim (inline move-message))
(defun move-message (source dest message)
  "Move $message from folder $source to folder $dest.  Assume $dest exists
   and $source is readable."
  (move-messages source dest (list message)))

;;; Public
;;;
(defun write-messages (stream folder messages)
  "Write $messages from $folder onto $stream.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\"."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :sequences))
	 (messages (clean-messages-spec
		    messages
		    (folder-info-sequences folder-info))))
    (setq folder-info (scan-folder folder messages))
    (in-directory (folder-pathname folder)
      (do-messages (message entry messages folder folder-info)
	(with-open-file (input (string message) :direction :input)
	  (transfer input stream))))))

;;; Internal
;;;
;;; Return ID of the message after $message in $messages.
;;;
(defun message-after (messages message)
  (caadr (nthcdr (position (assoc message messages)
			   messages)
		 messages)))

;;; Internal
;;;
;;; Return the message before $message in $messages.
;;;
(defun message-before (messages message)
  (let ((pos (position (assoc message messages) messages)))
    (fi (zerop pos)
	(caar (nthcdr (1- pos) messages)))))

;;; %delete-message  --  Internal
;;;
;;; Rename $msg with a , prefix.  Must be called with $dir current.
;;;
(declaim (inline %delete-message))
(defun %delete-message (msg folder dir folder-info errorp)
  (mess "(%delete-message ~A ~A ~A . .)" msg folder dir)
  (incf-mess)
  (let* ((mlist (folder-info-messages folder-info))
	 (entry (assoc msg mlist)))
    (fi entry
	(when errorp
	  (decf-mess)
	  (error "Failed to find ~A in ~A cache." msg folder))
	(progn
	  ;; FIX rest should be ~atomic
	  (while ((name (concat "," (string msg)) (concat "," name)))
		 ((probe-file name)
		  (rename-file (string msg) name)))
	  ;; Update the folder cache.
	  (if (eq msg (folder-info-start folder-info))
	      (setf (folder-info-start folder-info)
		    (message-after (folder-info-messages folder-info)
				   msg)))
	  (if (eq msg (folder-info-end folder-info))
	      (setf (folder-info-end folder-info)
		    (message-before (folder-info-messages folder-info)
				    msg)))
	  (setf (folder-info-messages folder-info)
		(delq entry mlist))
	  (setf (folder-info-write-date folder-info)
		(file-write-date dir))
	  (if (eq msg (folder-info-lowest folder-info))
	      (setf (folder-info-lowest folder-info) ()))
	  (if (eq msg (folder-info-highest folder-info))
	      (setf (folder-info-highest folder-info)
		    (or (highest folder-info)
			;; Need to find the highest.
			;;
			;; Prevent `add-highest-sequence' from affecting
			;; the cache.
			(let ((*in-cache-folder* t))
			  (prog1 (add-highest-sequence folder dir)
			    ;; Reread the sequences into the folder info.
			    ;; FIX This is a bit lazy as the highest
			    ;; sequences could be updated directly.
			    (setf (folder-info-sequences folder-info)
				  (get-sequences)))))))
	  (decf-mess)))))

;;; Public
;;;
(defun delete-message (folder message &optional (errorp t))
  "Delete $message from $folder.  If $errorp is true then signal an error
   if the given message is missing.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (mess "(delete-message ~A ~A ~A)" folder message errorp)
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 (dir (folder-pathname folder))
	 (current-message (current-message folder)))
    (setq folder-info (scan-folder folder message))
    (in-directory dir
      (unwind-protect
	  (%delete-message message folder dir folder-info errorp)
	(or (probe-file (string current-message))
	    (setf (current-message folder) (highest folder-info)))
	(update-sequences folder-info)
	(decf-mess)))))

;;; Public
;;;
(defun delete-messages (folder &optional (messages '(:all)))
  "Delete $messages from $folder.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\"."
  (mess "(delete-messages ~A ~A)" folder messages)
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :sequences))
	 (messages (clean-messages-spec
		    messages
		    (folder-info-sequences folder-info)))
	 (dir (folder-pathname folder))
	 (current-message (current-message folder)))
    (setq folder-info (scan-folder folder messages))
    (in-directory dir
      (unwind-protect
	  (do-messages (message entry messages folder folder-info)
	    (%delete-message message folder dir folder-info t))
	(or (probe-file (string current-message))
	    (setf (current-message folder) (highest folder-info)))
	(update-sequences folder-info)
	(decf-mess)))))

(declaim (inline get-header get-body))

;;; Internal
;;;
;;; Return the header named $header from the cache $entry.
;;;
(defun get-header (entry header)
  (assoc header (cdr entry) :test #'string=))

;;; get-headers  --  Internal
;;;
;;; Return all the headers in $entry named $header, as a list of
;;; associations.
;;;
(defun get-headers (entry header)
  (collect ((headers))
    (dolist (assoc (cdr entry))
      (if (string= (car assoc) header)
	  (headers assoc)))
    (headers)))

;;; Internal
;;;
;;; Return the body portion from cache $entry.
;;;
(defun get-body (entry)
  (assoc :body (cdr entry)))

(declaim (special *entry*))

;;; Public
;;;
(defun cc (string)
  "Return true if $string is a substring of the Cc header of *entry*."
  (search string (cdr (get-header *entry* "Cc"))))

;;; Public
;;;
(defun f-cc (string)
  "With case folding, return true if $string is a substring of the Cc
   header of *entry*."
  (search (string-upcase string)
	  (string-upcase (cdr (or (get-header *entry* "Cc")
				  (return-from f-cc))))))

;;; Public
;;;
(defun from (string)
  "Return true if $string is a substring of the From header of *entry*."
  (search string (cdr (get-header *entry* "From"))))

;;; Public
;;;
(defun f-from (string)
  "With case folding, return true if $string is a substring of the From
   header of *entry*."
  ;; FIX (let ((*case-fold* t)) ...
  (search (string-upcase string)
	  (string-upcase (cdr (or (get-header *entry* "From")
				  (return-from f-from))))))

;;; Public
;;;
(defun to (string)
  "Return true if $string is a substring of the To header of *entry*."
  (or (search string (cdr (get-header *entry* "Delivered-To")))
      (search string (cdr (get-header *entry* "To")))))

;;; Public
;;;
(defun f-to (string)
  "With case folding, return true if $string is a substring of the To
   header of *entry*."
  (or (let ((delivered-to (cdr (get-header *entry* "Delivered-To"))))
	(if delivered-to
	    (search (string-downcase string)
		    (string-downcase delivered-to))))
      (let ((to (cdr (get-header *entry* "To"))))
	(if to
	    (search (string-downcase string)
		    (string-downcase to))))))

;;; Public
;;;
(defun subject (string)
  "Return true if $string is a substring of the Subject header of *entry*."
  (search string (or (cdr (get-header *entry* "Subject"))
		     (return-from subject))))

;;; Public
;;;
(defun f-subject (string)
  "With case folding, return true if $string is a substring of the Subject
   header of *entry*."
  (search (string-upcase string)
	  (string-upcase (or (cdr (get-header *entry* "Subject"))
			     (return-from f-subject)))))

;;; Public
;;;
(defun content (string)
  "Return true if $string is a substring of any part of message *entry*.
   Expect to be called in the directory in which *entry* resides."
  (with-open-file (input (string (car *entry*)) :direction :input)
    (while ((line (read-line input ()) (read-line input ())))
	   (line)
      (if (search string line) (return-from content t)))))

;;; Public
;;;
(defun f-content (string)
  "With case folding, return true if $string is a substring of any part of
   message *entry*.  Expect to be called in the directory in which *entry*
   resides."
  (with-open-file (input (string (car *entry*)) :direction :input)
    (while ((line (read-line input ()) (read-line input ())))
	   (line)
      (if (search (string-upcase string) (string-upcase line))
	  (return-from f-content t)))))

;;; Public
;;;
(defun date (string &optional (header "Date"))
  "Return true if $string is a substring of $header in *entry*."
  (search string (cdr (get-header *entry* header))))

;;; Public
;;;
(defun f-date (string &optional (header "Date"))
  "With case folding, return true if $string is a substring of $header in
   *entry*."
  (let ((header (cdr (get-header *entry* header))))
    (if header (search (string-upcase string) (string-upcase header)))))

;;; Public
;;;
(defun before (date &optional (header "Date"))
  "Return true if string $date is earlier than $header in *entry*.  Expect
   the current directory to be the one in which *entry* resides."
  (> (or (parse-time date) (return-from before))
     (or (parse-time (cdr (or (get-header *entry* header)
			      (return-from before))))
	 (return-from before))))

;;; Public
;;;
(defun after (date &optional (header "Date"))
  "Return true if string $date is later than $header in *entry*.  Expect
   the current directory to be the one in which *entry* resides."
  (< (or (parse-time date) (return-from after))
     (or (parse-time (cdr (or (get-header *entry* header)
			      (return-from after))))
	 (return-from after))))

;;; Public
;;;
(defun -- (header string)
  "Return true if $string is a substring of $header in *entry*."
  (search string (or (cdr (get-header *entry* header))
		     (return-from --))))

;;; Public
;;;
(defun f--- (header string)
  "With case folding, return true if $string is a substring of $header in
   *entry*."
  (search (string-upcase string)
	  (string-upcase (cdr (or (get-header *entry* header)
				  (return-from f---))))))

;;; Public
;;;
(defun pick-messages (folder messages &optional expression)
  "Return the list of message from $messages in $folder that are described
   by $expression.

   For each message in $messages evaluate $expression with *entry* bound to
   the cache entry of the message.  If the expression returns true then
   include the message in the return list.

   A few functions are predefined for use in $expression. `mh::to',
   `mh::cc', `mh::from', `mh::subject', `mh::content' and `mh::date' search
   for a given string in the respective headers of the message.
   `mh::after' and `mh::before' check if the message is after or before a
   given date string.  `mh::--' searches for a given string in a given
   header.

   If $expression is () then pick all of $messages.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\"."
  (mess "(pick-messages ~A ~A ~A)" folder messages expression)
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :sequences))
	 (messages (clean-messages-spec messages
					(folder-info-sequences
					 folder-info))))
    (setq folder-info (scan-folder folder messages))
    (collect ((pick) (msgs))
      (in-directory (folder-pathname folder)
	;; Expand messages into a list of messages.
	(do-messages (message entry messages folder folder-info)
	  (msgs message))
	(decf-mess)
	(fi expression
	    (msgs)
	    (let ((folder-msgs (folder-info-messages folder-info)))
	      (dolist (message (msgs))
		(let ((*entry* (assoc message folder-msgs)))
		  (when *entry*
		    (if (eval expression) (pick message)))))
	      (pick)))))))

;;; Public
;;;
(defun split-messages (folder messages rules
			      &optional (refiler #'move-messages))
  "Split the specified $messages in $folder according to $rules using
   function $refiler.

   $refiler is called with a source folder, destination folder and a
   message list.

   $rules is a list of split rules.  A split rule is a list with a folder
   name first and a pick expression [FIX] second.  Any of $messages matched
   by the expression are moved into the folder.  The rules are processed in
   the order given, so rules nearer the front of the list take precedence."
  (mess "(split-messages ~A ~A .)" folder messages)
  (let* ((folder (coerce-folder-name (namify folder)))
	 (messages (pick-messages folder messages)))
    (dolist (rule rules)
      (mess "messages: ~A" messages)
      (or messages (return))
      (when (cadr rule)
	(let ((pick (pick-messages folder messages (cadr rule))))
	  (when pick
	    (setq messages (nset-difference messages pick))
	    (funcall refiler
		     folder
		     (coerce-folder-name (namify (car rule)))
		     pick)))))))

;;; Public
;;;
(defun draft-new (&optional (components-name "components") components)
  "Draft a new message, returning the number of the draft.  Insert in the
   message $components if $components is set, otherwise the contents of the
   file named $components-name in the mail directory if the file exists,
   otherwise a simple set of headers."
  (mess "(draft-new ~A ~A)" components-name components)
  (incf-mess)
  (let* ((draft-folder (draft-folder))
	 (folder-info (scan-folder draft-folder :highest))
	 (new (1+ (folder-info-highest folder-info)))
	 (dir (folder-pathname draft-folder))
	 (comppath (fi components
		       (merge-pathnames components-name
					(root-pathname)))))
    (mess "comppath: ~A" comppath)
    ;;
    ;; Create file.
    (flet ((create-draft (dir name in)
	     (with-open-file (out (merge-pathnames (string name) dir)
				  :direction :output)
	       (transfer in out))))
      (cond (components
	     (with-input-from-string (in components)
	       (create-draft dir new in)))
	    ((probe-file comppath)
	     (with-open-file (in comppath :direction :input)
	       (create-draft dir new in)))
	    (t
	     (with-input-from-string (in #.(format () "To:~%~
						       Cc:~%~
						       Subject:~%~
						       --------~%"))
	       (create-draft dir new in)))))
    ;;
    ;; Update cache.
    (setf (folder-info-highest folder-info) new)
    (setf (folder-info-end folder-info) new)
    (setf (folder-info-messages folder-info)
	  ;; Keep message list sorted.
	  (append
	   (folder-info-messages folder-info)
	   (list
	    ;; Make a folder cache entry.
	    (let* ((msg (string new))
		   (pathname (merge-pathnames (string new)
					      dir)))
	      (cons new
		    (from-file (input pathname)
		      (or (scan-message input pathname)
			  (progn
			    (setf (getstring draft-folder
					     (get-folder-table))
				  t)
			    (error
			     "Failed to scan new draft (~A in ~A)"
			     msg draft-folder)))))))))
    ;; Update the current message directly.
    (mark-message-directly draft-folder :all "cur" :delete)
    (mark-message-directly draft-folder new "cur" :add)
    (setf (folder-info-sequences folder-info) (get-sequences))
    ;; Update folder cache write date.
    (setf (folder-info-write-date folder-info) (file-write-date dir))
    ;; Update cache and disk sequences.
    (in-directory dir (update-sequences folder-info))
    (mess "folder-info: ~A" folder-info)
    (decf-mess)
    new))

;;; Public
;;;
(defun draft-resend ()
  "Create a resend draft."
  (draft-new () (format () "Resent-To: ~%Resent-Cc: ~%")))

;;; Public
;;;
(defun draft-forward (folder message &optional (mime :inline))
  "Draft a forward of $message from $folder.

   If $mime is true produce a MIME attachment, otherwise produce a text
   attachment.  If $mime is :inline then inline the MIME attachment.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info)))
    (with-open-file (in
		     (merge-pathnames (string message)
				      (folder-pathname folder))
		     :direction :input)
      (let ((new (draft-new "forwcomps")))
	(with-open-file (out
			 (merge-pathnames
			  (string new)
			  (folder-pathname (draft-folder)))
			 :direction :output
			 :if-does-not-exist :error
			 :if-exists :append)
	  (if mime
	      (format out
		      "~&#~Amessage/rfc822 [Forward: ~A] ~A~%"
		      (if (eq mime :inline) ":" "")
		      (or (message-header folder message "Subject") "")
		      (merge-pathnames (string message)
				       (folder-pathname folder)))
	      (progn
		(format out "~&~%~A~%~%" *forward-start*)
		(transfer in out)
		(format out "~&~A~%" *forward-end*))))
	new))))

;;; Public
;;;
(defun draft-reply (folder message &optional cc)
  "Draft a reply to $message from $folder.

   Carbon copy according $cc, as follows.

     :all     copy message to all To and Cc addresses

     :others  copy message to all To and Cc addresses, filtering out any
              addresses in *address* and *alternate-addresses*

     ()       forego any carbon copying.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 (folder-info (scan-folder folder message))
	 (messages (folder-info-messages folder-info))
	 (entry (assoc message messages)))
    (or entry (error "Failed to find ~A in ~A" message folder))
    ; To: Renewals <address@eg.org>
    ; cc: surname@eg.org
    ; Fcc: archive/misc
    ; Subject: Re: Account renewal
    ; In-reply-to: Message of Sun, 25 Feb 2007 04:53:35 +0000.
    ; 		<20070225045335.F24DEE6DAE@mail.eg.org>
    ; From: F Surname <address2@eg.org>
    ; Reply-To: address2@eg.org
    ; --------
    (draft-new
     ()
     (format () "To: ~A~%~
		 ~A~
		 Fcc: ~A~%~
		 Subject: Re: ~A~%~
		 In-reply-to: Message of ~A.~%~
		 ~A~
		 From: ~A <~A>~%~
		 Reply-To: ~A~%~
		 --------~%"
	     ;; To:
	     (or (cdr (get-header entry "Reply-To"))
		 (cdr (get-header entry "From"))
		 "")
	     ;; Cc:
	     (flet ((filter-addresses (to)
		      (let ((alt-addrs (cons *address*
					     *alternate-addresses*)))
			(collect ((new-to))
			  (dolist (address (split to '(#\, #\;)))
			    (let ((address (string-trim '(#\space #\tab)
							address)))
			      (or (dolist (alt alt-addrs)
				    (if (search alt address)
					(return t)))
				  (if address (new-to address)))))
			  (new-to))))
		    (split-addresses (to)
		      (collect ((new-to))
			(dolist (address (split to '(#\, #\;)))
			  (if address
			      (let ((address (string-trim '(#\space
							    #\tab)
							  address)))
				(new-to address))))
			(new-to))))
	       (ecase cc
		 ;; FIX These will split up an address containing , or ;.
		 (:all
		  (let* ((cc (sort
			      (delete-duplicates
			       (append
				(split-addresses
				 (cdr (get-header entry "To")))
				(split-addresses
				 (cdr (get-header entry "Cc"))))
			       :test #'string=)
			      #'string-lessp)))
		    (mess "cc: ~A" cc)
		    (with-output-to-string (stream)
		      (when cc
			(format stream "Cc: ~A" (first cc))
			(dolist (addr (rest cc))
			  (format stream ",~%    ~A" addr))
			(format stream "~%")))))
		 (:others
		  (let* ((cc (sort
			      (delete-duplicates
			       (append
				(filter-addresses
				 (cdr (get-header entry "To")))
				(filter-addresses
				 (cdr (get-header entry "Cc"))))
			       :test #'string=)
			      #'string-lessp)))
		    (mess "cc: ~A" cc)
		    (mess "to: ~A" (cdr (get-header entry "To")))
		    (mess "fto: ~A" (filter-addresses
				    (cdr (get-header entry "To"))))
		    (mess "cc: ~A" (cdr (get-header entry "Cc")))
		    (mess "fcc: ~A" (filter-addresses
				     (cdr (get-header entry "Cc"))))

		    (with-output-to-string (stream)
		      (when cc
			(format stream "Cc: ~A" (first cc))
			(dolist (addr (rest cc))
			  (format stream ",~%    ~A" addr))
			(format stream "~%")))))
		 ((()) "")))
	     ;; Fcc:
	     (archive-for-reply folder message)
	     ;; Subject:
	     (let ((subject (cdr (get-header entry "Subject"))))
	       (if (and (> (length subject) 2)
			(string= (string-upcase subject) "RE:"
				 :end1 3))
		   (subseq subject
			   (if (and (> (length subject) 3)
				    (char= (char subject 3) #\ ))
			       4
			       3))
		   subject))
	     ;; In reply to message of
	     (or (cdr (get-header entry "Date"))
		 "??")
	     (let ((id (cdr (get-header entry "Message-Id"))))
	       (if id (format () "          ~A~%" id) ""))
	     ;; From:
	     *from-name*
	     (address-for-from folder message)
	     ;; Reply-To:
	     (address-for-reply-to folder message)))))

;;; Internal
;;;
;;; Return the first email address in $string and the position in $string
;;; of the end of the address.
;;;
(defun parse-address (string)
  (let ((at-pos (position #\@ string)))
    (when at-pos
      (let* ((string (substitute #\space #\newline string))
	     (start (position #\space string
			      :from-end t :end at-pos))
	     (end (or (position #\space string :start at-pos)
		      (length string))))
	(if start (incf start) (setq start 0))
	(while () ((member (char string start) '(#\, #\; #\<)))
	  (incf start))
	(while () ((member (char string (1- end)) '(#\, #\; #\>)))
	  (decf end))
	(values (subseq string start end) end)))))

;;; Internal
;;;
;;; Write delivery-time headers to $stream.
;;;
(defun finish-headers (stream from &optional msgid)
  (let ((time (get-universal-time)))
    (write-date-header "Date" stream time)
    (if msgid
	(format stream "Message-ID: <~D.~D@~A>~%"
		(unix:unix-getpid)
		time
		(machine-instance)))
    (format stream "Sender: ~A~%" from)))

(defun write-mail (stream to from message folder-pathname entry)
  "Write the message in cache $entry to $stream.  The message is from $from
   to $to."
  (declare (ignore to))
  (let* ((pos)
	 (fcc-folder (cdr (get-header entry "Fcc")))
	 (fcc-folder-info (if fcc-folder
			      (scan-folder fcc-folder
					   :highest)))
	 (fcc-message (if fcc-folder
			  (1+ (folder-info-highest
			       fcc-folder-info))))
	 (fcc (when fcc-folder
		(ensure-directories-exist
		 (folder-pathname fcc-folder))
		(in-directory
		    (folder-pathname fcc-folder)
		  (while () ((probe-file (string fcc-message)))
		    ;; For some reason there is already a
		    ;; file named with the chosen ID.
		    ;; Maybe the file was added directly on
		    ;; disk.  Maybe the folder cache lost
		    ;; sync.  Clear the folder cache and
		    ;; try the next ID.
		    (setf (getstring fcc-folder (get-folder-table)) t)
		    (setq fcc-message (1+ fcc-message))))
		(open (merge-pathnames
		       (string fcc-message)
		       (folder-pathname fcc-folder))
		      :direction :io
		      :if-does-not-exist :create
		      :if-exists :error)))
	 (stream (if fcc
		     (make-broadcast-stream stream fcc)
		     stream)))
    (while ((headers (cdr entry) (cdr headers)))
	   (headers)
      (case= (caar headers)
	("Universal-Date" "Fcc")
	(:body (setq pos (caddar headers)))
	(t
	 (format stream "~A: " (caar headers))
	 (write-string (cdar headers) stream)
	 (terpri stream))))
    (finish-headers stream from)
    (terpri stream)
    (with-open-file (in
		     (merge-pathnames (string message)
				      folder-pathname)
		     :direction :input)
      (file-position in pos)
      (transfer in stream))
    (when fcc
      (file-position fcc :start)
      (let ((alist (scan-message fcc)))
	(when alist
	  ;; Update the fcc folder cache.
	  (push (cons (string fcc-message) alist)
		; FIX msgs should be sorted?
		;; FIX ok to push for first msg?
		(folder-info-messages fcc-folder-info))
	  (setf (folder-info-highest fcc-folder-info)
		fcc-message)
	  (setf (folder-info-end fcc-folder-info)
		fcc-message)
	  (setf (folder-info-write-date fcc-folder-info)
		(file-write-date (folder-pathname
				  fcc-folder)))))
      (close fcc))))

;;; Public
;;;
(defvar *mailer* #'internet:smtp-mail
  "Function called to send mail.  Must take as arguments a stream, a list
   of to addresses, a from address, and a function to call to write the
   message.")

;;; Public
;;;
(defun deliver-messages (account messages
				 &optional (folder (draft-folder)))
  "Deliver $messages from $folder via $account.  Return t on success, else
   () and the error response.

   $messages is a [message specification]: a list of any number of message
   ID integers (23), message ID strings (\"23\"), message range pairs
   ((\"20\" . \"23\")), range strings (\"20-23\"), sequence names
   (\"highest\"), the symbol :all, or the string \"all\"."
  (let* ((folder-info (scan-folder folder :highest))
	 (messages (clean-messages-spec
		    messages
		    (folder-info-sequences folder-info)))
	 (folder-info (scan-folder folder messages))
	 (folder-messages (folder-info-messages folder-info))
	 (folder-pathname (folder-pathname folder)))
    (loop for message in messages do
      (let ((entry (assoc message folder-messages))
	    (to))
	(or entry (error "Failed to find ~A in ~A" message folder))
	(if (get-header entry "Bcc") (error "Bcc"))
	(if (get-header entry "Resent-Bcc") (error "Resent-Bcc"))
	(let ((resent-to (get-header entry "Resent-to"))
	      (resent-cc (get-header entry "Resent-cc")))
	  (loop for string in
	    (if (or resent-to resent-cc)
		`(,resent-to ,resent-cc)
		`(,(cdr (get-header entry "Cc"))
		  ,(cdr (get-header entry "To")))) do
	    (loop
	      (multiple-value-bind (address end)
				   (parse-address string)
		(or address (return))
		(push address to)
		(setq string (subseq string end)))))
	  (or to (error "Message must have a destination address."))
	  (let ((from (if (or resent-to resent-cc)
			  (or (parse-address (cdr (get-header
						   entry
						   "Resent-from")))
			      (error "Message must have a Resent-From address."))
			  (or (parse-address (cdr (get-header
						   entry "From")))
			      (error "Message must have a From address.")))))
	    (multiple-value-bind
		(success error)
		(funcall *mailer* to from account
			 #'write-mail message folder-pathname entry)
	      (if success
		  (delete-message folder message)
		  (return-from deliver-messages
			       (values () error)))))))))
  t)

;;; %write-headers  --  Internal
;;;
;;; Write $out-headers from $entry to $stream.
;;;
(defun %write-headers (entry out-headers stream)
  (if (eq out-headers t)
      (while ((headers (cdr entry) (cdr headers)))
	     (headers)
	(let ((header (caar headers)))
	  (case= header
	    ;; Universal-Date is a special header used internally.  FIX
	    ;; Could be a problem if there is a real Universal-Date header.
	    ((:body "Universal-Date"))
	    (t (format stream "~A: ~A~%" header (cdar headers))))))
      (while ((headers out-headers (cdr headers)))
	     (headers)
	(let ((header (get-header entry (car headers))))
	  (if header
	      (format stream "~A: ~A~%"
		      (car header) (cdr header)))))))

;;; Public
;;;
(defun write-headers (folder message stream &optional (headers t))
  "Write $headers of $message in $folder to $stream.  Return t on success,
   else ().

   If $headers is t write all headers, otherwise expect $headers to be a
   list of header names, where the header names are capitalized strings.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 (folder-info (scan-folder folder message))
	 (entry (assoc message (folder-info-messages folder-info))))
    (%write-headers entry headers stream)))

;;; Public
;;;
(defun message-header (folder message header)
  "Return $header from $message in $folder, if there is such a header, else
   return ().  If there are more than one instances of $header return as a
   second value a list of the other headers.

   Expect $header to be a capitalized string naming the header, like
   \"From\".

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 (headers (get-headers
		  (assoc message
			 (folder-info-messages
			  (scan-folder folder message)))
		  header)))
      (values (cdar headers) (mapcar #'cdr (cdr headers)))))

(eval-when (compile eval load)

  (parse:defparser
   '((:mime-header     :mime-space :mime-type
		      (or :mime-subtype) :mime-space (or :mime-comment)
		      (any :mime-param))
     (:mime-space     (group (any (or #\space #\tab))
			     (or #\newline)
			     (any (or #\space #\tab))))
     (:mime-type      (group (many (or :alphanumeric #\. #\- #\_)))
		      (or :mime-comment))
     (:mime-subtype   #\/ :mime-space (or :mime-comment)
		      :mime-type)
     (:mime-param     #\; :mime-space (or :mime-comment)
		      (group (many (or :alphanumeric #\-))) #\=
		      :mime-space
		      (or :mime-quoted
			  (group (many (or :alphanumeric #\- #\_ #\.))))
		      :mime-space (or :mime-comment))
     (:mime-quoted    #\"
		      (group (many (or "\\\\"
				       "\\\""
				       :mime-qchar)))
		      #\") ;; FIX flush \'s
     (:mime-qchar     (cond (fi (member parse:ch '(#\" #\newline)))))
     (:mime-comment   #\( (group (any (or :mime-comment
					  (many :mime-cchar))))
		      #\))
     (:mime-cchar     (cond (fi (member parse:ch '(#\( #\))))))
     (:alphanumeric   (cond (alphanumericp parse:ch)))))

  ) ; eval-when

;;; Public.
;;;
(defun parse-content-type (parse:*stream*)
  "Parse the content type at parse:*stream*.  Return the type, subtype and
   a alist of parameters."
  (when parse:*stream*
    (let* ((parse:*streams* `((,parse:*stream* 0)))
	   (node (or (parse-mime-header)
		     (return-from parse-content-type))))
      (setq node (parse:node-next (parse:node-content node)))
      (values (parse:node-content (parse:node-content node))
	      (progn
		(setq node (parse:node-next node))
		(if (eq (type-of node) 'mime-subtype-node)
		    (parse:node-content
		     (parse:node-content
		      (parse:node-next
		       (parse:node-next
			(parse:node-next
			 (parse:node-content node))))))))
	      (collect ((params))
		(while ((param (parse:node-next
				(parse:node-next
				 (parse:node-next node)))
			       (parse:node-next param)))
		       ((eq (type-of param) 'mime-param-node))
		  (let* ((name-node (parse:node-next
				     (parse:node-next
				      (parse:node-next
				       (parse:node-content param)))))
			 (value-node (parse:node-next
				      (parse:node-next
				       (parse:node-next name-node)))))
		    (params (cons (parse:node-content name-node)
				  (etypecase value-node
				    (parse:region-node
				     (parse:node-content value-node))
				    (mime-quoted-node
				     (parse:node-content
				      (parse:node-next
				       (parse:node-content
					value-node)))))))))
		(params))))))

(declaim (special *attachments*))

;;; end-boundary-p  --  Internal
;;;
;;; Return true if $line is an end $boundary line.
;;;
(declaim (inline end-boundary-p))
(defun end-boundary-p (line boundary boundary-line-len)
  (and (> (length line) 2)
       (char= (char line 0) #\-)
       (char= (char line 1) #\-)
       (> (length line) boundary-line-len)
       (string= line boundary
		:start1 2
		:end1 boundary-line-len)))

;;; boundary-p  --  Internal
;;;
;;; Return true if $line is a $boundary line.
;;;
(declaim (inline boundary-p))
(defun boundary-p (line boundary boundary-line-len)
  (and (> (length line) 2)
       (char= (char line 0) #\-)
       (char= (char line 1) #\-)
       (>= (length line) boundary-line-len)
       (string= line boundary
		:start1 2
		:end1 boundary-line-len)))

;;; read-to-end-boundary  --  Internal
;;;
;;; Read from $in up to and over an end boundary line like $boundary.
;;;
(defun read-to-end-boundary (in boundary boundary-line-len)
  (while ((line (read-line in ()) (read-line in ())))
	 (line)
    ;(mess "<== ~A" line)
    (when (end-boundary-p line boundary boundary-line-len)
      ;; At the special end boundary.
      (return))))

;;; read-to-boundary  --  Internal
;;;
;;; Read from $in to the next boundary.  Return true if more parts from the
;;; same multipart follow.
;;;
(defun read-to-boundary (in boundary boundary-line-len)
  (while ((line (read-line in ()) (read-line in ())))
	 (line)
    ;(mess "<== ~A" line)
    (if (boundary-p line boundary boundary-line-len)
	(return (= (length line) boundary-line-len)))))

;;; write-nested-forward  --  Internal
;;;
;;; Write to stream $out $out-headers and the message up to $boundary in
;;; stream $in.  Return the parts of the forwarded message.
;;;
(defun write-nested-forward (in out out-headers boundary)
  ;; FIX is this even standard?
  ;; FIX read to *forward-end* into a string, `write-message' the string
  ;;         hence drop boundary param
  (let* ((entry (cons 0 ; Dummy msg id.
		      (scan-message in () ()))) ; Only scan headers.
	 (body-start (file-position in)))
    (%write-headers entry out-headers out)
    (terpri out)
    ;; Write body.
    (%write-body in out entry body-start out-headers boundary)))

;;; boundary-transfer  --  Internal
;;;
;;; Transfer from $in to $out up to $boundary, filtering any nested
;;; forwarded messages.  Push any parts gathered from nested messages onto
;;; *attachments*.  Return true if there are more parts in $in.
;;;
(defun boundary-transfer (in out out-headers boundary boundary-len)
  (while ((line (read-line in ()) (read-line in ()))
	  (forward-start-len (length *forward-start*)))
	 (line)
    ;(mess "<== ~A" line)
    (when (boundary-p line boundary boundary-len)
      (or (eq (length line) boundary-len)
	  ;; At the end boundary.
	  (return-from boundary-transfer))
      (return-from boundary-transfer t))
    (if (and (>= (length line) forward-start-len)
	     (string= (string-downcase line) *forward-start*))
	;; Handle a forwarded message.
	;;
	;; FIX This is only going to work for plain text, as encoding
	;; translation is done later in `transfer-part'.  It's a waste to
	;; check each line of an encoded message.
	(progn
	  (format out "~A~%" line)
	  (write-nested-forward in out out-headers boundary)
	  ;; `write-nested-forward' should have stopped at a *forward-end*,
	  ;; so continue the `while' loop to the boundary.
	  )
	(format out "~A~%" line)))
  ;; Failed to find boundary.
  ())

;;; end-transfer  --  Internal.
;;;
;;; Transfer everything from $in to $out, filtering any nested forwarded
;;; messages.  Push any parts gathered from nested messages to
;;; *attachments*.
;;;
(defun end-transfer (in out out-headers)
  (while ((line (read-line in ()) (read-line in ()))
	  (forward-start-len (length *forward-start*)))
	 (line)
    ;(mess "<== ~A" line)
    (if (and (>= (length line) forward-start-len)
	     (string= (string-downcase line)
		      *forward-start*))
	;; Handle a forwarded message.
	;;
	;; FIX Same note as in `boundary-transfer'.
	(progn
	  (format out "~A~%" line)
	  (write-nested-forward in out out-headers ()))
	(format out "~A~%" line)))
  ;; All parts read.
  ())

;;; write-nested-rfc822  --  Internal
;;;
;;; Write to stream $out $out-headers and the message in $stream $in up to
;;; a line like $boundary.
;;;
(defun write-nested-rfc822 (in out enc out-headers boundary boundary-len)
  (mess "(write-nested-rfc822 . . ~A ~A ~A ~A)"
	enc out-headers boundary boundary-len)
  (incf-mess)
  (flet ((write-nested (stream)
	   (let* ((entry (cons 0 ; Dummy msg id.
			       ;; Only scan headers.
			       (scan-message stream () ())))
		  (body-start (file-position stream)))
	     (%write-headers entry out-headers out)
	     (terpri out)
	     ;; FIX should this indicate if more follow?
	     (%write-body stream out entry body-start out-headers
			  boundary))))
    (unwind-protect
	(if (and enc (string= (string-downcase enc) "base64"))
	    (let (continue-p)
	      (with-input-from-string
		  (stream (base64:base64-decode
			   (string-trim
			    '(#\space #\tab #\newline #\return)
			    (with-output-to-string (code)
			      (setq continue-p
				    (if boundary
					(boundary-transfer
					 in code out-headers
					 boundary
					 boundary-len)
					(end-transfer
					 in code out-headers)))))))
		(write-nested stream))
	      continue-p)
	    (write-nested in))
      (decf-mess))))

(defvar *html-handler* ())

;;; transfer-part  --  Internal
;;;
;;; Transfer the body part at stream $in to stream $out handling encoding
;;; $enc.  If $boundary is true transfer up to a line like $boundary,
;;; reading over the line.  Filter any nested forward messages nicely.
;;; Return true if there are more parts in $in.  Push any parts read from
;;; nested messages onto *attachments*.
;;;
(defun transfer-part (in out enc out-headers boundary boundary-len)
  (cond ((and enc (string= (string-downcase enc) "base64"))
	 (let (continue-p)
	   (write-string (base64:base64-decode
			  (string-trim
			   '(#\space #\tab #\newline #\return)
			   (with-output-to-string (code)
			     (setq continue-p
				   (if boundary
				       (boundary-transfer
					in code out-headers
					boundary
					boundary-len)
				       (end-transfer
					in code out-headers))))))
			 out)
	   continue-p))
	((and enc (string= (string-downcase enc) "quoted-printable"))
	 (let (continue-p)
	   (write-string (or (from-quoted
			      (with-output-to-string (code)
				(setq continue-p
				      (if boundary
					  (boundary-transfer
					   in code out-headers
					   boundary
					   boundary-len)
					  (end-transfer
					   in code out-headers)))))
			     "Failed to translate Quoted-Printable part.")
			 out)
	   continue-p))
	(t
	 (if boundary
	     (boundary-transfer in out out-headers
				boundary boundary-len)
	     (end-transfer in out out-headers)))))

;;; store-part  --  Internal
;;;
;;; Store the part at $in in *attachments*.
;;;
(defun store-part (pos headers in boundary boundary-len)
  (mess "(store-part ~A ~A . . .)" pos headers)
  (incf-mess)
  (let ((continue-p t))
    (push (list pos
		(with-output-to-string (part)
		  (while ((line (read-line in ())
				(read-line in ())))
			 (line
			  ;; Reached end of stream.
			  (setq continue-p ()))
		    (when (boundary-p line boundary boundary-len)
		      (or (eq (length line) boundary-len)
			  ;; At the end boundary.
			  (setq continue-p ()))
		      (return))
		    (format part "~A~%" line)))
		headers)
	  *attachments*)
    (decf-mess)
    continue-p))

;;; write-part  --  Internal
;;;
;;; Write the part at $in to $out.  Return true if there are more parts in
;;; $in.  Push any parts scanned to *attachments* (there can be many if the
;;; part is a multipart or contains a nested message).
;;;
(defun write-part (in out out-headers boundary boundary-len)
  (mess "(write-part . . ~A ~A ~A)" boundary boundary-len out-headers)
  (incf-mess)
  (let ((headers (read-headers in ()))) ; Scan up to the body.
    (unwind-protect
	(write-part-body in out headers out-headers boundary boundary-len)
      (decf-mess))))

;;; Write the part body at $in to $out.  Return true if there are more
;;; parts in $in.  Push an parts scanned to *attachments* (there can be
;;; many if the part is a multipart or contains a nested message).
;;; $headers is the headers of the message.  $out-headers defines the
;;; headers to include in nested messages.
;;;
(defun write-part-body (in out headers out-headers boundary boundary-len)
  (mess "(write-part-body . . ~A ~A ~A)" out-headers boundary boundary-len)
  (incf-mess)
  (let* ((displayp (and (eq (length headers) 1)
			(eq (caar headers) :body)))
	 (type (cdr (get-header (cons :dummy headers)
				"Content-Type")))
	 (enc (cdr (get-header (cons :dummy headers)
			       "Content-Transfer-Encoding")))
	 (position (cdr (get-header (cons :dummy headers)
				    "Content-Disposition")))
	 (inlinep (and position
		       (>= (length position) 6)
		       (string= (string-downcase position)
				"inline"
				:end1 6)))
	 (attachp (and position
		       (>= (length position) 10)
		       (string= (string-downcase position)
				"attachment"
				:end1 10)))
	 (pos (file-position out))
	 nested-rfc822-p
	 html-p
	 (continue-p t))
    (mess "type: ~A" type)
    (mess "position: ~A" position)
    (mess "pos: ~A" pos)
    (or type
	(when position
	  #| FIX try parse type from position |#)
	(setq type "text/plain"))
    (when type
      ;;
      ;; Decide what to do with the part.
      (setq type (nsubstitute #\space #\newline
			      (string-trim '(#\space) type)))
      (mess "type: ~A" type)
      (multiple-value-bind (type subtype params)
			   (with-input-from-string (stream type)
			     (parse-content-type stream))
	(mess "type: ~A, subtype: ~A, params: ~A" type subtype params)
	(flet ((output-attach ()
		 (format out "#~A/~A [~A]~@[ ~A~]~%"
			 type subtype
			 (or (cdr (get-header (cons :dummy headers)
					      "Content-Description"))
			     "")
			 params params)))
	  (cond (inlinep
		 ;; Honour explicit inline, where possible.
		 (case= (string-downcase type)
		   ("message"
		    (case= (string-downcase subtype)
		      ("rfc822"
		       (setq nested-rfc822-p t))
		      ;; The rest written out due to inlinep being t.
		      ("delivery-status")
		      (t
		       (format out "(FIX new message subtype)~%~%"))))
		   ("text"
		    (case= (string-downcase subtype)
		      ("html"
		       (setq html-p t))
		      ("plain")
		      (t
		       (format out "(FIX new text subtype)~%~%"))))
		   ("multipart"
		    (multiple-value-bind
			(ret cont-p)
			(write-mime-body
			 in out (cons :dummy headers)
			 (caddr (get-body (cons :dummy headers)))
			 out-headers
			 boundary)
		      (or (and ret cont-p)
			  (setq continue-p ())))
		    (setq inlinep ()))
		   (t
		    ;; TODO Improve file type handling in general.
		    (setq inlinep ())
		    (setq attachp t)
		    (output-attach))))
		;; Honour explicit attachment.
		(attachp (output-attach))
		(t
		 ;; Pick an appropriate action.
		 (case= (string-downcase type)
		   ("message"
		    (case= (string-downcase subtype)
		      ("rfc822"
		       (setq nested-rfc822-p t))
		      ("delivery-status"
		       (setq inlinep t))
		      (t
		       (format out "(FIX new message subtype)~%~%")
		       (setq inlinep t))))
		   ("text"
		    (case= (string-downcase subtype)
		      ("html"
		       (setq html-p t))
		      ("plain"
		       (setq inlinep t))
		      (t
		       (format out "(FIX new text subtype)~%~%")
		       (setq inlinep t))))
		   ("multipart"
		    (multiple-value-bind
			(ret cont-p)
			(write-mime-body
			 in out (cons :dummy headers)
			 (caddr (get-body (cons :dummy headers)))
			 out-headers
			 boundary)
		      (or (and ret cont-p)
			  (setq continue-p ())))
		    (setq attachp ()))
		   (t
		    ;; Attachment.
		    (setq attachp t)
		    (output-attach))))))))
    ;;
    ;; Write out or store the part.
    (cond (nested-rfc822-p
	   ;;; Write the part body, which is an embedded message.
	   ;; FIX are there more parts?
	   (write-nested-rfc822 in out enc
				out-headers
				boundary boundary-len))
	  (html-p
	   (when *html-handler*
	     (let ((tem-file (pick-new-file "/tmp/mh-~D-~D.html")))
	       (with-open-file (tem tem-file :direction :output)
		 (let ((cont-p (transfer-part in tem enc
					      out-headers boundary
					      boundary-len)))
		   (or cont-p (setq continue-p ()))))
	       ;; FIX perhaps move www to code:
	       (funcall *html-handler* out tem-file))))
	  ((or displayp inlinep)
	   (let ((cont-p (transfer-part in out enc out-headers
					boundary boundary-len)))
	     (or cont-p (setq continue-p ()))))
	  (attachp
	   ;; Store the attachment in the cache for `get-part'.
	   (let ((cont-p (store-part pos headers in
				     boundary boundary-len)))
	     (or cont-p (setq continue-p ())))))
    (decf-mess)
    continue-p))

;;; write-multipart  --  Internal
;;;
;;; Write the multipart MIME message part at stream $in to stream $out.  If
;;; $out-headers is t write all headers, else write the headers listed in
;;; $out-headers.
;;;
;;; Read only to the multipart end boundary, leaving the caller to read to
;;; any following parent boundary.  FIX What happens if the end boundary is
;;; missing?  It'd be nice to check for the parent boundary too.
;;;
;;; Return true on success.
;;;
(defun write-multipart (in out entry subtype enc params out-headers)
  (declare (ignore entry))
  (mess "(write-multipart . . . ~A ~A . .)" subtype enc)
  (incf-mess)
  ;;
  ;; Check the encoding.
  (if enc
      ;; FIX which encodings are really allowed?
      ;; FIX what affect does the encoding have?
      (or (string= enc "7bit") (string= enc "8bit")
	  (format *standard-output*
		  "Multipart encoding should be \"7bit\" or \"8bit\".")))
  ;;
  ;; Get the boundary.
  (let* ((boundary (or (and params
			    (cdr (assoc "boundary" params
					:test #'string=)))
		       ;(error "Multipart must have a \"boundary\" parameter.")
		       (progn
			 (decf-mess)
			 (return-from write-multipart ()))))
	 (boundary-len (+ (length boundary) 2))
	 (line (read-line in ()))
	 (alternative-p (string= subtype "alternative")))
    (mess "boundary: ~A" boundary)
    ;;
    ;; Skip lines to the first boundary.
    (while () (line)
      (if (and (>= (length line) boundary-len)
	       (char= (char line 0) #\-)
	       (char= (char line 1) #\-)
	       (string= line boundary
			:start1 2
			:end1 boundary-len))
	  (return))
      (setq line (read-line in ())))
    (when (and line (eq (length line) boundary-len))
      ;;
      ;; Write the parts.
      (if alternative-p
	  ;; multipart/alternative
	  (progn
	    ;; Write only the first part.
	    (write-part in out out-headers boundary boundary-len)
	    ;; Read to the end boundary, in case the caller needs to read
	    ;; more parts after the multipart.
	    (read-to-end-boundary in boundary boundary-len))
	  ;; multipart/mixed, multipart/related or multipart/report: write
	  ;; all parts.
	  (while () ((write-part in out out-headers
				 boundary boundary-len)))))
    (decf-mess)
    t))

;;; write-mime-body  --  Internal
;;;
;;; Write the body of MIME message $entry from $in to $out.  $body-start is
;;; the file position of the start of the body.  Add any new content parts
;;; to $entry.  If $out-headers is t write all headers, else write the
;;; headers listed in $out-headers.
;;;
;;; Return t on success, or () on encountering an error.  Return a second
;;; value true if there are parts following the body (possibly when
;;; $parent-boundary is given).
;;;
;;; TODO Handle more MIME types.
;;;
(defun write-mime-body (in out entry body-start out-headers
			   &optional parent-boundary)
  (mess "(write-mime-body . . . . ~A ~A)" out-headers parent-boundary)
  (incf-mess)
  (let ((content-type
	 (nsubstitute
	  #\space #\newline
	  (string-trim
	   '(#\space)
	   ;; Get the Content-Type header.
	   (or (cdr (get-header entry "Content-Type"))
	       (if parent-boundary
		   ;; Try read the header from the headers of the first
		   ;; part.
		   (let (line)
		     ;; Skip empty lines.
		     (loop
		       (setq line (read-line in ()))
		       (or (and line (string= line ""))
			   (return)))
		     ;; Try read the header.
		     (if (and line
			      (> (length line) 2)
			      (char= (char line 0) #\-)
			      (char= (char line 1) #\-)
			      (string= line parent-boundary
				       :start1 2))
			 (multiple-value-bind (name text)
					      (read-header in ())
			   (if (and name
				    (string= (string-downcase name)
					     "content-type"))
			       text)))))
	       (progn
		 (decf-mess)
		 (return-from write-mime-body))))))
	(enc (cdr (get-header entry "Content-Transfer-Encoding")))
	(continue-p)
	(ret t)
	(parent-boundary-len (+ (length parent-boundary) 2)))
    (multiple-value-bind (type subtype params)
			 (with-input-from-string (stream content-type)
			   (parse-content-type stream))
      (setq continue-p
	    (case= (string-downcase type)
	      ("message"
	       (file-position in body-start)
	       (write-part-body in out (cdr entry) out-headers
				parent-boundary
				parent-boundary-len))
	      ("text"
	       (file-position in body-start)
	       (write-part-body in out (cdr entry) out-headers
				parent-boundary
				parent-boundary-len))
	      ("multipart"
	       (file-position in body-start)
	       (setq ret (write-multipart in out entry subtype
					  enc params out-headers))
	       ;; Read to the boundary that follows the multipart.  This
	       ;; returns whether there are more parts in the stream.
	       (read-to-boundary in parent-boundary
				 parent-boundary-len))
	      (t
	       (file-position in body-start)
	       (write-part-body in out (cdr entry) out-headers
				parent-boundary
				parent-boundary-len)))))
    (decf-mess)
    (values ret continue-p)))

;;; %write-body  --  Internal
;;;
;;; Write the message body at stream $in to stream $out.  $out-headers is a
;;; list of headers to be written for any nested messages.  Return #t on
;;; success, else () and an error message string.
;;;
(defun %write-body (in out entry body-start out-headers &optional boundary)
  (mess "(%write-body . . ~A ~A ~A ~A"
	entry body-start out-headers boundary)
  (incf-mess)
  (let ((mime-version (get-header entry "Mime-Version"))
	(ret t)
	err)
    (if mime-version
	(progn
	  (or (and (> (length (cdr mime-version)) 2)
		   (string= (cdr mime-version) "1.0" :end1 3))
	      (setq ret ()
		    err    "Expected MIME version 1.0"))
	  (or (write-mime-body in out entry body-start
			       out-headers boundary)
	      (progn
		(decf-mess)
		(return-from %write-body
			     (values () "Failed to write part")))))
	(progn
	  (file-position in body-start)
	  (boundary-transfer in out out-headers
			     boundary (+ (length boundary) 2))))
    (decf-mess)
    (values ret err)))

;;; Public
;;;
(defun write-message (folder message stream &optional (headers t))
  ;; FIX general error handling: should throw dedicated err instd?
  "Write $message in $folder to $stream.  Return #t on success, else () and
   an error message.

   If $headers is t write all headers, otherwise expect $headers to be a
   list of capitalized header names and write those headers in that order.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info)))
    (setq folder-info (scan-folder folder message))
    (with-open-file
	(in (merge-pathnames (string message)
			     (folder-pathname folder))
	    :direction :input
	    :if-does-not-exist ())
      (or in (return-from write-message
			  (values () "Failed to open message")))
      (let* ((folder-messages (folder-info-messages folder-info))
	     (entry (assoc message folder-messages))
	     (body-start (caddr (get-body entry)))
	     (*attachments*))
	;; Write headers.
	(%write-headers entry headers stream)
	(fi body-start
	    ;; FIX It may be an error for a MIME message to have an empty
	    ;; body.  If the MIME content-disposition is attachment then
	    ;; perhaps the result should be an empty attachment.  Should
	    ;; probably always return an error as message is probably
	    ;; screwed.
	    t
	    ;; Transfer body.
	    (progn
	      (terpri stream)
	      (multiple-value-bind
		  (ret err)
		  (%write-body in stream entry body-start headers)
		(if *attachments*
		    (pushnew (cons :parts (nreverse *attachments*))
			     (cdr entry)))
		(values ret err))))))))

;;; Public
;;;
(defun write-file-message (stream pathname &optional (headers t))
  ;; FIX general error handling: should throw dedicated err instd?
  "Write the message in $pathname to $stream.  Return #t and an alist of
   headers on success, else () and an error message.

   If $headers is t write all headers, otherwise expect $headers to be a
   list of capitalized header names and write those headers in that order."
  (with-open-file (in pathname
		      :direction :input
		      :if-does-not-exist ())
    (or in (return-from write-file-message
			(values () "Failed to open message")))
    (let* ((scanned-headers (scan-message in pathname))
	   (entry (cons 0 scanned-headers))
	   (body-start (caddr (get-body entry)))
	   (*attachments*))
      ;; Write headers.
      (%write-headers entry headers stream)
      (fi body-start
	  ;; FIX It may be an error for a MIME message to have an empty
	  ;; body.  If the MIME content-disposition is attachment then
	  ;; perhaps the result should be an empty attachment.  Should
	  ;; probably always return an error as message is probably
	  ;; screwed.
	  (values t (cdr entry))
	  ;; Transfer body.
	  (progn
	    (terpri stream)
	    (multiple-value-bind
		(ret err)
		(%write-body in stream entry body-start headers)
	      (if *attachments*
		  (pushnew (cons :parts (nreverse *attachments*))
			   (cdr entry)))
	      (values ret (if ret (cdr entry) err))))))))

;;; Public
;;;
(defun get-part (folder message position)
  "Return the part at $position in $message from $folder, and the content
   type header of the part.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 (folder-info (scan-folder folder message))
	 (messages (folder-info-messages folder-info))
	 (entry (assoc message messages))
	 (part (assoc position (cdr (assoc :parts (cdr entry))))))
    (when part
      (let* ((part-headers (caddr part))
	     (encoding (cdr (assoc "Content-Transfer-Encoding"
				   part-headers
				   :test #'string=)))
	     (content-type (cdr (assoc "Content-Type"
				       part-headers
				       :test #'string=))))
	(values
	 (case= encoding
	   ("base64" (base64:base64-decode (cadr part)))
	   (t (cadr part)))
	 content-type)))))

;;; Public
;;;
(defun get-headers-part (headers position)
  "Return the part at $position in $headers.

   $headers is as returned from `write-file-message'."
  (let* ((part (assoc position (cdr (assoc :parts headers)))))
    (when part
      (let* ((part-headers (caddr part))
	     (encoding (cdr (get-header part-headers
					"Content-Transfer-Encoding")))
	     (content-type (cdr (get-header part-headers
					    "Content-Type"))))
	(values
	 (case= encoding
	   ("base64" (base64:base64-decode (cadr part)))
	   (t (cadr part)))
	 content-type)))))

;;; Public
;;;
;;; FIX guessing this should only write certain headers from message into
;;; the new draft (currently writes headers like message-id and
;;; delivered-to).
;;;
(defun resend-message (account message folder draft-message
			       &optional (draft-folder (draft-folder)))
  "Resend $message in $folder via $account, as defined by $draft-message in
   $draft-folder.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence.

   FIX Beware, implementation needs testing."
  (let* ((folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 ;; Scan folder again to cache message.
	 (folder-info (scan-folder folder message))
	 (new (string (draft-new (merge-pathnames
				  (string draft-message)
				  (folder-pathname draft-folder)))))
	 (folder-messages (folder-info-messages folder-info))
	 (folder-pathname (folder-pathname folder))
	 (entry (assoc message folder-messages))
	 ;; Scan draft folder to cache sequences.
	 (draft-info (scan-folder draft-folder :highest))
	 (draft-message (clean-message-spec draft-message
					    draft-info))
	 ;; Scan draft folder again to cache draft message.
	 (draft-info (scan-folder draft-folder draft-message))
	 (draft-messages (folder-info-messages draft-info)))
    ;; FIX new must added to folder cache?
    ;; Append the message to a copy of the draft, prepending any "Resent"
    ;; header in the message with "Prev-".
    (with-open-file (out (merge-pathnames new (folder-pathname draft-folder))
			 :direction :output
			 :if-exists :supersede)
      (loop for header in (cdr entry) while header do
	(let ((name (car header)))
	  (if (and (stringp name)
		   (> (length name) 7)
		   (string= name "Resent-" :end1 7))
	      (format out "Prev-Resent-~A: ~A~%"
		      (subseq name 7) (cdr header))
	      (case= name
		("Universal-Date")
		(:body ; FIX assumes body last
		 ;; Write the resent headers from the draft.
		 ;; FIX draft-message must be rescanned as ed has just modified the headers
		 ;; FIX    a general problem w drafts?
		 ;;           if msg has been mod'd should rescan
		 ;;               maybe check file-write-date
		 (let* ((entry (assoc draft-message draft-messages)))
		   (loop for header in (cdr entry) while header do
		     (or (eq (car header) :body)
			 (string= (car header) "Universal-Date")
			 (format out "~A: ~A~%"
				 (car header) (cdr header)))))
		 ;; Write the from and date resent headers.
		 (or (get-header entry "Resent-From")
		     (format out "Resent-From: ~A~%"
			     (address-for-from folder message)))
		 (or (get-header entry "Resent-Date")
		     (write-date-header "Resent-Date" out))
		 (terpri out)
		 ;; Write the body.
		 (with-open-file (in (merge-pathnames message
						      folder-pathname))
		   (file-position in (caddr header))
		   (loop for line = (read-line in ()) while line do
		     (write-line line out)))
		 (return))
		(t
		 (format out "~A: ~A~%" (car header) (cdr header))))))))
    ;; Send the newly created draft.
    (multiple-value-bind (success response)
			 (deliver-messages account (list new))
      (if success
	  (progn
	    ;; FIX Annotate message being resent (the original message).
	    (delete-message draft-folder draft-message)
	    t)
	  ;; FIX If fail delete new draft
	  (progn
	    (delete-message draft-folder draft-message)
	    (values success response))))))

;;; Public
;;;
(defun annotate-message (folder message component &key text (datep t))
  "Annotate $message from $folder with $component.  That is, add a header
   named $component to $message.

   If $datep then set the content of the header to the current date.  If
   $text add a header named $component with $text as content (in addition
   to any date header).

   If $text and $date are both () then add an empty header named
   $component.

   Expect $message to be an integer message (2), a string message (\"2\"),
   the symbol :highest or a string naming a sequence."
  (mess "(annotate-message ~A ~A ~A ~A ~A)"
	folder message component text datep)
  (or (plusp (length component))
      (error "$component must have length."))
  (if (find #\: component)
      (error "$component contains a colon: ~A" component))
  (incf-mess)
  (let* ((folder (namify folder))
	 (folder-info (scan-folder folder :highest))
	 (message (clean-message-spec message folder-info))
	 (folder-info (scan-folder folder message))
	 (messages (folder-info-messages folder-info))
	 (entry (assoc message messages))
	 (tem (pick-new-file))
	 (msg-name (string (car entry)))
	 (in-pathname (merge-pathnames msg-name
				       (folder-pathname folder))))
    (with-open-file (out tem :direction :output)
      (if datep (write-date-header component out))
      (if text (format out "~A: ~A~%" component text))
      (or datep text (format out "~A:~%" component))
      (with-open-file (in in-pathname :direction :input)
	(transfer in out)))
    (rename-file in-pathname (merge-pathnames in-pathname "*.BAK"))
    (rename-file tem in-pathname)
    ;; Update the message in the cache.
    (setf (cdr entry)
	  (with-open-file (input in-pathname :direction :input)
	    (or (scan-message input in-pathname)
		(error "Failed to rescan annotated message ~A in ~A"
		       msg-name
		       folder))))
    (setf (folder-info-write-date folder-info)
	  (file-write-date (folder-pathname folder))))
  (decf-mess))


;;;; Drops.

;;; A drop: some mechanism from which mail can be fetched.
;;;
(defstruct (drop)
  new-fun
  inc-fun)

;;; A local drop: a (FIX confirm) mbox format mailbox.
;;;
(defstruct (local-drop
	    (:include drop
		      (new-fun #'new-local-mail-p)
		      (inc-fun #'incorporate-local))
	    (:constructor
	     make-local-drop
	     (&optional
	      (location
	       (or (cdr (assoc :mail ext:*environment-list*))
		   (cdr (assoc :maildrop ext:*environment-list*))
		   (profile-component "MailDrop") ; FIX
		   (let* ((user (cdr (assoc :user
					    ext:*environment-list*)))
			  (mbox (merge-pathnames
				 user "/var/spool/mail/")))
		     (if (probe-file mbox)
			 mbox
			 (let ((mbox (merge-pathnames
				      user "/usr/spool/mail/")))
			   (if (probe-file mbox) mbox)))))))))
  location)

;;; A POP drop.
;;;
(defstruct (pop-drop (:include drop
			       (new-fun #'new-pop-mail-p)
			       (inc-fun #'incorporate-pop))
		     (:constructor %make-pop-drop
				   (type account port timeout)))
  type
  account
  port
  timeout)
;;
(defun make-pop-drop (type &key port timeout server user password)
  (%make-pop-drop type
		  (make-inet-account server user password)
		  (or port 110)  ; POP3 (POP2 is 109)
		  (or timeout 10)))

;;; Interface
;;;
(defvar *drop-makers* '((:local . make-local-drop)
			(:pop . make-pop-drop))
  "List of type and maker associations for mail drops.")

;;; Public
;;;
;;; FIX doc types, incl args required for mail drops
;;;
(defun make-drop (type &rest args)
  "Make and return a mail drop of $type.

   Type is a symbol denoting the type of mail drop, e.g. :local and :pop."
  (let ((assoc (assoc type *drop-makers*)))
    (if assoc
	(apply (cdr assoc) args)
	(error "Failed to find drop type ~A." type))))

;;; Public
;;;
(defun make-drops (list)
  "Make and return the mail drops desribed by $list.

   Apply `make-drop' to each element of the list to make the drops."
  (collect ((drops))
    (dolist (drop list)
      (drops (apply #'make-drop drop)))
    (drops)))


;;;; New mail.

;;; Public
;;;
(defun new-mail-p (drops)
  "Return true if there is mail available in any of the mail drops listed
   in $drops."
  (dolist (drop drops)
    (if (funcall (drop-new-fun drop) drop) (return t))))

;;; Internal
;;;
;;; Return true if there is mail in local drop $drop.
;;;
(defun new-local-mail-p (drop)
  (let ((location (local-drop-location drop)))
    ;; FIX unix-stat does this in one call
    (and (probe-file location)
	 (plusp (logand (file-mode location) unix:readown))
	 (plusp (file-size location)))))

;;; Internal
;;;
;;; Return true if there is mail in local drop $drop.
;;;
(defun new-pop-mail-p (drop)
  (let* ((account (fill-from-netrc (pop-drop-account drop)))
	 (stream (pop-init account
			   (pop-drop-type drop)
			   (pop-drop-port drop)
			   (pop-drop-timeout drop))))
    (and stream
	 (let ((msg-count (pop-stat stream)))
	   (prog1 (if (and msg-count (plusp msg-count)) msg-count)
	     (inet-quit stream))))))


;;;; Incorporating mail.

;;; Public
;;;
(defun incorporate (drops &optional folder stream)
  "Incorporate into $folder all mail from the mail drops listed in $drops.
   Return true if all mail was incorporated.

   If $stream is true log progress on $stream."
  (let ((all t))
    (dolist (drop drops all)
      (or (funcall (drop-inc-fun drop) drop folder stream)
	  (setq all ())))))

;;; Internal
;;;
;;; Incorporate mail from local mail $drop into $folder.  Return true on
;;; successfully incorporating all mail.
;;;
;;; If $log-stream is given log any progress on the stream.
;;;
;;; FIX actually always errs on failure
(defun incorporate-local (drop &optional folder log-stream)
  (let ((location (local-drop-location drop)))
    ;; FIX unix-stat does this in one call
    (when (and (probe-file location)
	       (plusp (logand (file-mode location) unix:readown))
	       (plusp (file-size location)))
      (let* ((folder (namify (or folder
				 (profile-component "inbox")
				 "inbox")))
	     (pathname (folder-pathname folder))
	     (lock (concat location ".lock")))
	(or (probe-file pathname) (create-folder folder))
	;; Lock the location, at least from other Nightshades.
	(system:block-interrupts
	 (if (probe-file lock)
	     (error "Drop location ~A already locked with ~A."
		    location lock)
	     (with-open-file (lock-stream lock :if-exists :error
					  :if-does-not-exist :create))))
	(unwind-protect
	    (let* ((folder-info (scan-folder folder :highest))
		   (in-sequence (profile-component "unseen-sequence")))
	      (in-directory pathname
		;; Read messages into files.
		(with-open-file (in location :direction :input
				    :if-does-not-exist :error)
		  (let ((line (read-line in ())))
		    (or (and line
			     (> (length line) 5)
			     (string= line "From " :end1 5))
			(error "Location must start with \"From \".")))
		  (while* ((new-id (1+ (folder-info-highest
					folder-info))
				   (1+ new-id))
			   (name (string new-id) (string new-id)))
			  ((peek-char () in ()))
		    (with-open-file
			(file-stream name
				     :direction :io
				     :if-does-not-exist :create
				     :if-exists :error)
		      (transfer-message in file-stream)
		      (file-position file-stream :start)
		      (let ((alist (scan-message file-stream)))
			(when alist
			  ;; Update the folder cache.
			  ;; Keep message list sorted.
			  (setf (folder-info-messages folder-info)
				(append (folder-info-messages
					 folder-info)
					(list (cons new-id alist))))
			  (mark-message-directly folder :all
						 "highest" :delete)
			  (mark-message-directly folder name
						 "highest" :add)
			  (setf (folder-info-highest folder-info)
				new-id)
			  (or (eq (folder-info-end folder-info) t)
			      (setf (folder-info-end folder-info)
				    new-id))
			  (mark-message-directly folder name
						 in-sequence :add)
			  ;; Reread the sequences into the folder info.
			  ;; FIX This is lazy as the highest and "in"
			  ;; sequences in the cache could be updated
			  ;; directly.
			  (setf (folder-info-sequences folder-info)
				(get-sequences))
			  (setf (folder-info-write-date folder-info)
				(file-write-date pathname))))
		      (if log-stream
			  (format log-stream "Added message ~D to ~A."
				  new-id folder))))))
	      (truncate-file location))
	  (delete-file lock)))))
  t)

;;; Internal
;;;
;;; Incorporate all mail from POP $drop into $folder.  Return true on
;;; successfully incorporating all mail.
;;;
;;; If $log-stream is true, log progress on $log-stream.
;;;
(defun incorporate-pop (drop &optional folder log-stream)
  ;; FIX Orig inc logs progress, for Inc New Mail.
  (declare (ignore log-stream))
  (let* ((account (fill-from-netrc (pop-drop-account drop)))
	 (pop-stream (pop-init account
			       (pop-drop-type drop)
			       (pop-drop-port drop)
			       (pop-drop-timeout drop))))
    (when pop-stream
      (unwind-protect
	  (let ((msg-count (pop-stat pop-stream)))
	    (or msg-count (return))
	    (if (minusp msg-count) (return))
	    (when (plusp msg-count)
	      (let* ((folder (or folder
				 (profile-component "inbox")
				 "inbox"))
		     (pathname (folder-pathname folder)))
		(ensure-directories-exist pathname)
		(let* ((folder-info (scan-folder folder :highest))
		       (in-sequence (profile-component
				     "unseen-sequence")))
		  (in-directory pathname
		    (loop
		      for pop-id from 1 to msg-count
		      for new-id = (1+ (folder-info-highest
					folder-info))
		                 then (1+ new-id)
		      do
		      (let ((name (string new-id)))
			(with-open-file
			    (file-stream name
					 :direction :io
					 :if-does-not-exist :create
					 :if-exists :error)
			  (pop-retr pop-stream pop-id file-stream)
			  (file-position file-stream :start)
			  (let ((alist (scan-message file-stream)))
			    (when alist
			      ;; Update the folder cache.
			      ;; Keep message list sorted.
			      (setf (folder-info-messages folder-info)
				    (append (folder-info-messages
					     folder-info)
					    (list (cons new-id alist))))
			      (mark-message-directly folder :all
						     "highest" :delete)
			      (mark-message-directly folder name
						     "highest" :add)
			      (setf (folder-info-highest folder-info)
				    new-id)
			      (or (eq (folder-info-end folder-info) t)
				  (setf (folder-info-end folder-info)
					new-id))
			      (mark-message-directly folder name
						     in-sequence :add)
			      ;; Reread the sequences into the folder info.
			      ;; FIX This is lazy as the highest and "in"
			      ;; sequences in the cache could be updated
			      ;; directly.
			      (setf (folder-info-sequences folder-info)
				    (get-sequences))
			      (setf (folder-info-write-date folder-info)
				    (file-write-date pathname))
			      ;; Flush message.
			      (or (pop-dele pop-stream pop-id)
				  ;; FIX try get rest anyway?
				  (error "Failed to flush ~A from POP.")))))))))))
	    t)
	(inet-quit pop-stream)))))


(defmacro with-fresh-state (&rest body)
  "with-fresh-state body

   Evaluate $body with any stored mail handling variables set to the
   initial values.  This includes the folder cache, mail configuration and
   some special variables used in mail handling.

   The tests use this to ensure a known start state, and to preserve the
   current mail settings."
  `(let ((*reply-archive* "archive/misc")
	 (*from-name* (user-full-name))
	 (*address* (user-email))
	 *alternate-addresses*
	 *folder-table*
	 *in-cache-folder*
	 (*body-scan-length* 50)
	 *html-handler*)
     ,@body))
