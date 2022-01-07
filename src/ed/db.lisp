;;; Interface to database of people and other entities, and Objed mode.

(in-package "ED")


;;; Structure.

(add-hook exit-hook 'db:ensure-db-saved)

(defmode "DB" :major-p ()
  :precedence 4.0
  :documentation
  ".db database mode.")


;;; Mode.

(defmode "Record" :major-p t)


;;; Commands.

(defun refresh-record-buffer (buffer name records)
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (defevar "Name"
      "The name matched by the records in this buffer."
      :value name
      :buffer buffer)
    (defevar "Records"
      "The records in this buffer."
      :value records
      :buffer buffer)
    (let ((point (buffer-point buffer)))
      (while ((records records (cdr records)))
	     (records)
	(with-output-to-mark (stream point)
	  (db:write-record stream (car records)))
	(setf (getf (line-plist (mark-line point)) 'db-entry) records)
	(insert-string point "================================================")
	(insert-character point #\newline))
      (buffer-start point))
    (highlight-record-buffer buffer)))

(defcommand "Describe Record" (p)
  "Present a description of a prompted record.  With a prefix pop up the
   record."
  (db:ensure-db-read #'message)
  (let ((name (prompt-for-string
	       :default (word-at-point)
	       :trim t
	       :prompt "Name, surname or AKA: "
	       :help "Enter name, surname or an AKA of the .db entry to edit.")))
    (if p
	(let ((records (progn
			 (message "Finding matching records.")
			 (db:find-records-any name))))
	  (or records (editor-error "Search for ~A failed." name))
	  (dolist (record records)
	    (with-pop-up-display (stream)
	      (db:write-record stream record)
	      (format stream "================================================~%"))))
	(let* ((buffer-name (format () "Record ~A" name))
	       (buffer (getstring buffer-name *buffer-names*)))
	  (if buffer
	      (switch-to-buffer-command () buffer)
	      (let ((records (progn
			       (message "Finding matching records...")
			       (db:find-records-any name))))
		(or records (editor-error "Search for ~A failed." name))
		(let* ((buffer (make-unique-buffer buffer-name
						   :modes '("Record"))))
		  (refresh-record-buffer buffer name records)
		  (switch-to-buffer-command () buffer))))))))

(defcommand "Refresh Record Buffer" ()
  "Refresh a record buffer."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Buffer must be in Record mode."))
  (let ((records (progn
		   (message "Finding matching records...")
		   (db:find-records-any (value name)))))
    (fi* records
      (with-writable-buffer ((current-buffer))
	(delete-region (buffer-region (current-buffer))))
      (editor-error "Search for ~A failed." (value name)))
    (refresh-record-buffer (current-buffer) (value name) records)))

(defun edit-record (record)
  (let ((*record* record))
    (find-object (car *record*)
		 (lambda (record)
		   (db:set-db-modified)
		   (setf (car *record*) record)))
    (setf (buffer-minor-mode (current-buffer) "DB") t)))

#|
(defmode "EdRecord" :major-p t)

;;; edit-record  --  Internal
;;;
;;; Edit the first of list $records in a buffer.
;;;
(defun edit-record (records)
  (let* ((record (car records))
	 (name (format () "~A ~A"
		       (db:db-record-forename record)
		       (db:db-record-surname record)))
	 (found (getstring name *buffer-names*))
	 (buffer (if (and found
			  (string= (buffer-major-mode found)
				   "EdRecord"))
		     found
		     (make-unique-buffer
		      name
		      :modes '("EdRecord")))))
    (change-to-buffer buffer)
    (or found
	(progn
	  (delete-region (buffer-region buffer))
	  (with-output-to-mark (stream (current-point))
	    (db:write-record stream record))))
    (setf (buffer-modified buffer) nil)))
|#

#|
(defcommand "Edit Record" (p name)
  "Switch to a buffer of a prompted .db record, for editing."
  (declare (ignore p))
  (db:ensure-db-read #'message)
  (edit-record (or (db:find-record (or name
				       (prompt-for-string
					:default (word-at-point)
					:trim t
					:prompt
					"Name, surname or AKA: "
					:help
					"Enter name, surname of an AKA of the .db entry to edit.")))
		   (editor-error "Search for record failed."))))
|#

(defcommand "Edit Record Lisp" (p name)
  "Switch to a buffer of the Lisp of a prompted .db record, for editing."
  (declare (ignore p))
  (db:ensure-db-read #'message)
  (edit-record (or (db:find-record (or name
				       (prompt-for-string
					:default (word-at-point)
					:trim t
					:prompt
					"Name, surname or AKA: "
					:help
					"Enter name, surname of an AKA of the .db entry to edit.")))
		   (editor-error "Search for record failed."))))

(defcommand "Create Contact" ()
  "Add a contact to the contacts database."
  (db:ensure-db-read #'message)
  (let* ((forename (prompt-for-string
		    :default ""
		    :trim t
		    :prompt "Forename: "
		    :help "Enter first name of the person or entity."))
	 (surname (prompt-for-string
		   :default ""
		   :trim t
		   :prompt "Surname: "
		   :help "Enter surname of the person or entity."))
	 (phone (prompt-for-string
		 :default ""
		 :trim t
		 :prompt "Phone number: "
		 :help "Enter phone number of the person or entity."))
	 (email (prompt-for-string
		 :default ""
		 :trim t
		 :prompt "Email: "
		 :help "Enter email address of the person or entity."))
	 (date (db:now)))
    (assert (eq (length db::*db-headings*) 8))
    (db:add-record (make-array 8
			       :initial-contents
			       (list forename surname () ()
				     (list (make-array 2
						       :initial-contents
						       `("phone" ,phone)))
				     ()
				     `(,email)
				     (list (cons 'CREATION-DATE date)
					   (cons 'TIMESTAMP date)))))))

(defcommand "Save DB" ()
  "Save the .db database to disk."
  (db:ensure-db-saved))

(defcommand "Next Record" (p)
  "Move to the next entry.  With a prefix move that many entries down (up
   if negative)."
  (or p (setq p 1))
  (cond ((zerop p))
	((minusp p)
	 (dotimes (time (- p))
	   (fi* (do-lines-from-mark (line (current-point) :backwards t)
		  (when (db-entry-on-line line)
		    (move-mark (current-point) (mark line 0))
		    (line-offset (current-point) p 0)
		    (return t)))
	     (buffer-start (current-point))
	     (return))))
	((plusp p)
	 (dotimes (time p)
	   (do-lines-from-mark (line (current-point))
	     (when (db-entry-on-line line)
	       (move-mark (current-point) (mark line 0))
	       (line-offset (current-point) p 0)
	       (return)))))))

(defcommand "Previous Record" (p)
  "Move to the previous entry.  With a prefix move that many entries up
   (down if negative)."
  (next-record-command (if p (- p) -1)))

(defcommand "Edit Contact" ()
  "Edit the Lisp description of the contact under point."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  ;; Search for the separator line.
  (do-lines-from-mark (line (current-point)
			    :after
			    (editor-error "There must be an entry under point."))
    (when (db-entry-on-line line)
      (edit-record (db:get-record (car (db-entry-on-line line))))
      (return))))

;;; parse-field-string  --  Internal
;;;
;;; Return the field name and value in $string.
;;;
(defun parse-field-string (string)
  (let ((pos (position #\: string)))
    (when pos
      (values (subseq string 0 (incf pos))
	      (if (> (length string) pos)
		  (subseq string (1+ pos))
		  "")))))

;;; parse-field  --  Internal
;;;
;;; Return the field name, subfield name and value at $mark.
;;;
(defun parse-field (mark)
  (with-mark ((mark2 mark))
    (line-start mark2)
    (case= (next-character mark2)
      ;; Subfield.
      (#\tab (while ((mark mark2 (line-offset mark2 -1)))
		    ((and mark2
			  (member (next-character mark2)
				  '(#\space #\tab)
				  :test #'char=))))
	     (apply #'values
		    (line-string (mark-line mark2))
		    (multiple-value-list
		     (parse-field-string
		      (line-string (mark-line mark))))))
      ;; Notes.
      ((#\newline #\space)
       (while ((mark mark2 (line-offset mark2 -1)))
	      ((and mark2
		    (member (next-character mark2)
			    '(#\space #\tab)
			    :test #'char=))))
       ;; Leave getting the value to the caller.
       "NOTES               :")
      (t
       ;; Other field.
       (multiple-value-bind (name value)
			    (parse-field-string
			     (line-string (mark-line mark2)))
	 (values name () value))))))

;;; address-from-string  --  Internal
;;;
;;; Return a vector of the lines of the address in $string.
;;;
(defun address-from-string (string)
  (let ((lines (split string #\,)))
    (or (> (length lines) 4)
	(editor-error "Address must have 5 or more lines (4 commas)."))
    (while ((lines lines (cdr lines)))
	   (lines)
      (let ((line (car lines)))
	(and (plusp (length line))
	     (char= (aref line 0) #\space)
	     (setf (car lines) (subseq line 1)))))
    (vector (while ((index 0 (1+ index))
		    (end (- (length lines) 4))
		    (list))
		   ((< index end)
		    list)
	      (setq list (append list (list (pop lines)))))
	    (pop lines) ; City
	    (pop lines) ; Province.
	    (pop lines) ; Post code.
	    (car lines)))) ; Country

(defcommand "Edit Field" ()
  "Edit the field under point."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  (multiple-value-bind (field subfield value)
		       (parse-field (current-point))
    ;; Search for the separator line.
    (do-lines-from-mark
	(line (current-point)
	      :after
	      (editor-error "There must be an entry under point."))
      (when (db-entry-on-line line)
	;; Update the field.
	(let* ((record (car (db:get-record
			     (car (db-entry-on-line line)))))
	       (value (prompt-for-string
		       ; FIX "" is hack to allow entry of ""
		       :default-string ""
		       :prompt `("~A " ,(or subfield field))
		       :initial (if (string= "NOTES"
					     (string-trim
					      '(#\space #\tab #\:)
					      field))
				    (db:db-record-notes record)
				    value)))
	       (field (string-trim '(#\space #\tab #\:) field)))
	  (if (and subfield (string= field "ADDRESSES"))
	      (let ((addrs (address-from-string value)))
		(db:set-field record
			      field
			      (if subfield
				  (string-trim '(#\space #\tab #\:)
					       subfield))
			      (aref addrs 0)
			      (aref addrs 1)
			      (aref addrs 2)
			      (aref addrs 3)
			      (aref addrs 4)))
	      (if (fi subfield (string= field "AKAS"))
		  (setf (db:field record
				  field
				  (if subfield
				      (string-trim '(#\space #\tab #\:)
						   subfield)))
			(mapcar (lambda (ele)
				  (ed::msg "e ~A" ele)
				  (string-trim '(#\space #\tab #\:) ele))
				(split value #\,)))
		  (setf (db:field record
				  field
				  (if subfield
				      (string-trim '(#\space #\tab #\:)
						   subfield)))
			value)))
	  (refresh-record-buffer (current-buffer)
				 (value name)
				 (value records)))
	(return)))))

(defcommand "Add Field" ()
  "Add a field to the record under point."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  (do-lines-from-mark
      (line (current-point)
	    :after
	    (editor-error "There must be an entry under point."))
    (when (db-entry-on-line line)
      ;; Insert the field.
      (let* ((record (car (db:get-record
			   (car (db-entry-on-line line)))))
	     (field (string-trim '(#\space #\tab #\:)
				 (prompt-for-string
				  :prompt "Add field named: ")))
	     (subfield (if (db:subfields-p field)
			   (prompt-for-string
			    :prompt "Subfield name: ")))
	     (value (prompt-for-string
		     ; FIX "" is hack to allow entry of ""
		     :default-string ""
		     :prompt `("Value for ~A: " ,(or subfield field)))))
	(if (string= field "ADDRESSES")
	    (let ((addrs (address-from-string value)))
	      (db:set-field record
			    field
			    (if subfield
				(string-trim '(#\space #\tab #\:)
					     subfield))
			    (aref addrs 0)
			    (aref addrs 1)
			    (aref addrs 2)
			    (aref addrs 3)
			    (aref addrs 4)))
	    (setf (db:field record
			    field
			    (if subfield
				(string-trim '(#\space #\tab #\:)
					     subfield)))
		  value))
	(refresh-record-buffer (current-buffer)
			       (value name)
			       (value records)))
      (return))))

(defcommand "Remove Field" ()
  "Add a field to the record under point."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  (multiple-value-bind (field subfield)
		       (parse-field (current-point))
    ;; Search for the separator line.
    (do-lines-from-mark
	(line (current-point)
	      :after
	      (editor-error "There must be an entry under point."))
      (when (db-entry-on-line line)
	;; Remove the field.
	(let* ((record (car (db:get-record
			     (car (db-entry-on-line line)))))
	       (field (prompt-for-string
		       :prompt "Remove field named: "
		       :default (string-trim '(#\space #\tab #\:)
					     field)))
	       (subfield (if (db:subfields-p field)
			     (prompt-for-string
			      :prompt "Subfield to remove: "
			      :default (string-trim
					'(#\space #\tab #\:)
					subfield)))))
	  (when (prompt-for-y-or-n
		 :default t
		 :prompt `("Remove field ~A? " ,field))
	    (db:set-field record
			  (string-trim '(#\space #\tab #\:) field)
			  (if subfield
			      (string-trim '(#\space #\tab #\:)
					   subfield))
			  ())))
	(refresh-record-buffer (current-buffer)
			       (value name)
			       (value records))
	(return)))))

(defcommand "Delete Contact" ()
  "Delete the contact under point."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  ;; Search for the separator line.
  (do-lines-from-mark (line (current-point)
			    :after
			    (editor-error "There must be an entry under point."))
    (when (db-entry-on-line line)
      (let ((entry (car (db-entry-on-line line))))
	(when (prompt-for-yes-or-no
	       :prompt (list "Delete record \"~A ~A\"? "
			     (db:db-record-forename entry)
			     (db:db-record-surname entry))
	       :must-exist t
	       :default ())
	  (db:free-record entry)
	  (refresh-record-buffer-command)))
      (return))))

(defcommand "Mail Contact" ()
  "Mail the contact under point."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  ;; Search for the separator line.
  (do-lines-from-mark (line (current-point)
			    :after
			    (editor-error "There must be an entry under point."))
    (when (db-entry-on-line line)
      (let ((emails (db:db-record-emails (car (db-entry-on-line line)))))
	(if (and emails (plusp (length (car emails))))
	    (progn
	      (send-message-command)
	      (let ((point (buffer-point (current-buffer))))
		(insert-string point (car emails))
		(buffer-end point)))
	    ;; FIX ~ offer to add one?
	    (editor-error "Entry must have an email address.")))
      (return))))

(defcommand "WWW Contact" (p)
  "Browse the URL associated with the contact under point.  With a prefix
   open the URL externally."
  (or (editor-bound-p 'name :buffer (current-buffer))
      (editor-error "Must be in a Record buffer."))
  ;; Search for the next separator line.
  (do-lines-from-mark (line (current-point)
			    :after
			    (editor-error "There must be an entry under point."))
    (when (db-entry-on-line line)
      (let ((url (db:db-record-url (car (db-entry-on-line line)))))
	(if url
	    (if p (view-url url) (www-command () url))
	    ;; FIX offer to add one?
	    (editor-error "Entry must have an URL field.")))
      (return))))

(defcommand "WWW Contact Externally" (p)
  "Browse the URL associated with the contact under point in an external
   program.  With a prefix browse with WWW."
  (www-contact-command (fi p)))


;;; FIX instance, value?
;;; Objed mode.

(defmode "Objed" :major-p nil
  :precedence 4.0
  :documentation
  "Object editing mode.")

(defcommand "Save Object" ()
  "Save the object represented in the current buffer."
  (let ((object (value object)))
    (when object
      (let ((buffer (current-buffer)))
	(with-input-from-region (stream (buffer-region buffer))
	  (setv object (or (read stream ())
			   (editor-error "Failed to read a record from the buffer."))))
	(setf (buffer-modified buffer) ())
	(invoke-hook save-object-hook (value object))))))

(defun object-to-buffer-name (object)
  (let ((obj-str (format nil "~A" object)))
    (substitute #\  #\tab
		(substitute #\% #\newline
			    (format nil
				    "Objed \"~A\"..."
				    (if (> (length obj-str) 12)
					(subseq obj-str 0 12)
					obj-str))))))

(defun find-object (object
		    &optional save-hook
		              (printer (lambda (value stream)
					 (write value :stream stream))))
  (let* ((name (object-to-buffer-name object))
	 (found (getstring name *buffer-names*))
	 (use (if found
		  (prompt-for-buffer
		   :prompt "Buffer to use: "
		   :help
  "Buffer name in use; name another buffer, or confirm to reuse."
                   :default found :must-exist nil)
		  (make-buffer name
			       :modes '("Fundamental" "Objed"))))
	 (buffer (if (stringp use)
		     (make-buffer use :modes '("Fundamental" "Objed"))
		     use)))
    (change-to-buffer buffer)
    (if use (or (stringp use) (delete-region (buffer-region buffer))))
    (defevar "Object"
      "The object under edit in this buffer."
      :buffer buffer
      :value object)
    (defevar "Save Object Hook"
      "Hook to invoke when object under edit is saved."
      :buffer buffer
      :value (list save-hook))
    (with-output-to-mark (stream (current-point))
      (funcall printer object stream))
    (setf (buffer-modified buffer) nil)))

(defcommand "Inspect Object" ()
  "Inspect the result of the evaluation of a prompted Lisp form."
  (let* ((form (prompt-for-expression
		:prompt "Inspect: "
		:help "Enter Lisp form; the result will be inspected.")))
    (find-object (eval form)
		 #'identity
		 (lambda (object stream)
		   (inspect::tty-display-object (inspect::describe-parts object) stream)))))


;;;; Highlighting

(defun db-entry-on-line (line)
  "Return the db entry that ends on $line, if any."
  (getf (line-plist line) 'db-entry))

(defmacro rehighlight-db-separator (line info)
  `(progn
     (dolist (fmark (ch-info-font-marks ,info))
       (delete-font-mark fmark))
     (push (color-mark ,line 0 :special-form)
	   (ch-info-font-marks ,info))))

(defmacro rehighlight-db-line (line info)
  `(progn
     (dolist (fmark (ch-info-font-marks ,info))
       (delete-font-mark fmark))
     (push (color-mark ,line 0 :comment)
	   (ch-info-font-marks ,info))
     (and (> (line-length line) 20) ; FIX db:*heading-width*
	  (let ((pos (position #\: (line-string line))))
	    (if pos
		(if (> (line-length line) pos)
		    (push (color-mark ,line (1+ pos) :window-foreground)
			  (ch-info-font-marks ,info))))))))

#|
(defun highlight-visible-record-buffer (buffer)
  (dolist (window (buffer-windows buffer))
    (or (eq (edi::window-first-changed window) edi::the-sentinel)
	(loop
	  for num from (window-height window) downto 1
	  for line = (mark-line (window-display-start window))
	  then (line-next line)
	  while line
	  do
	  (if (plusp (line-length line))
	      (let ((info (check-highlight-line line 'db-entry-chi)))
		(and info
		     (db-on-line line)
		     (rehighlight-db-line line info))))))))
|#

(defun highlight-record-buffer (buffer)
  (do-buffer-lines (line buffer)
    (when (plusp (line-length line))
      (let ((info (check-highlight-line line 'db-entry-chi)))
	(when info
	  (case (char (line-string line) 0)
	    (#\space)
	    (#\=
	     (and (db-entry-on-line line)
		  (rehighlight-db-separator line info)))
	    (t
	     (rehighlight-db-line line info))))))))
