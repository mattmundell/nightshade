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
    (defevar "name"
      "The name matched by the records in this buffer."
      :value name
      :buffer buffer)
    (with-output-to-mark (stream (buffer-point buffer))
      (loop for record in records do
	(db:write-record stream record)
	(format stream "================================================~%")))
    (buffer-start (buffer-point buffer))))

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
			 (db::find-records name))))
	  (or records (editor-error "Search for ~A failed." name))
	  (loop for record in records do
	    (with-pop-up-display (stream)
	      (db:write-record stream record)
	      (format stream "================================================~%"))))
	(let* ((buffer-name (format () "Record ~A" name))
	       (buffer (getstring buffer-name *buffer-names*)))
	  (if buffer
	      (switch-to-buffer-command () buffer)
	      (let ((records (progn
			       (message "Finding matching records...")
			       (db::find-records name))))
		(or records (editor-error "Search for ~A failed." name))
		(let* ((buffer (make-unique-buffer buffer-name
						   :modes '("Record"))))
		  (refresh-record-buffer buffer name records)
		  (switch-to-buffer-command () buffer))))))))

(defcommand "Refresh Record Buffer" ()
  "Refresh a record buffer."
  (or (editor-bound-p name :buffer buffer)
      (editor-error "Buffer must be in Record mode."))
  (let ((records (progn
		   (message "Finding matching records...")
		   (db::find-records (value name)))))
    (fi* records
      (with-writable-buffer ((current-buffer))
	(delete-region (buffer-region (current-buffer))))
      (editor-error "Search for ~A failed." (value name)))
    (refresh-record-buffer (current-buffer) (value name) records)))

(defcommand "Edit Record" ()
  "Switch to a buffer of a prompted .db record, for editing."
  (db:ensure-db-read #'message)
  (let* ((name (prompt-for-string
		:default (word-at-point)
		:trim t
		:prompt "Name, surname or AKA: "
		:help "Enter name, surname of an AKA of the .db entry to edit."))
	 (*record* (db:find-record name)))
    (or *record* (editor-error "Search for record failed."))
    (find-object (car *record*)
		 (lambda (record)
		   (db:set-db-modified)
		   (setf (car *record*) record)))
    (setf (buffer-minor-mode (current-buffer) "DB") t)))

(defcommand "Add Record" ()
  "Add a .db record."
  (db:ensure-db-read #'message)
  (let* ((forename (prompt-for-string
		    :default (word-at-point)
		    :trim t
		    :prompt "Forename: "
		    :help "Enter first name of the person or entity."))
	 (surname (prompt-for-string
		   :default (word-at-point)
		   :trim t
		   :prompt "Surname: "
		   :help "Enter surname of the person or entity."))
	 (phone (prompt-for-string
		 :default (word-at-point)
		 :trim t
		 :prompt "Phone number: "
		 :help "Enter phone number of the person or entity."))
	 (email (prompt-for-string
		 :default (word-at-point)
		 :trim t
		 :prompt "Email: "
		 :help "Enter email address of the person or entity.")))
    (multiple-value-bind
	(secs mins hours day month year)
	(get-decoded-time)
      (declare (ignore secs))
      (let ((date (format nil "~D-~D-~D ~Dh~:[0~;~]~D"
			  year month day hours (> mins 9) mins)))
	(assert (eq (length db::*db-headings*) 8))
	(db:add-record (make-array 8
				   :initial-contents
				   (list forename surname nil nil
					 (list (make-array 2
							   :initial-contents
							   `("phone" ,phone)))
					 ()
					 `(,email)
					 (list (cons 'CREATION-DATE date)
					       (cons 'TIMESTAMP date)))))))))

(defcommand "Save DB" ()
  "Save the .db database to disk."
  (db:ensure-db-saved))


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
