;;; Interface to database of people and other entities, and Objed mode.

(in-package "ED")


;;; Structure.

(add-hook exit-hook 'db:save-db)

(defmode "DB" :major-p nil
  :precedence 4.0
  :documentation
  ".db database mode.")


;;; Commands.

(defcommand "Describe Record" (p)
  "Pop up a .db description of a prompted record.  With a prefix present
   the record in a buffer."
  "Pop up a .db description of a prompted record.  If P is true then
   present the record in a buffer."
  (db:ensure-db-read)
  (let* ((name (prompt-for-string
		:default (word-at-point)
		:trim t
		:prompt "Name, surname or AKA: "
		:help "Enter name, surname or an AKA of the .db entry to edit."))
	 (record (db:find-record name)))
    (or record (editor-error "Search for record failed."))
    (if p
	(progn
	  (switch-to-buffer-command () (find-file-buffer "FIX db"))
	  (with-output-to-mark (stream (current-point))
	    (db:write-record stream (car record))))
	(with-pop-up-display (stream)
	  (db:write-record stream (car record))))))

(defcommand "Edit Record" (p)
  "Switch to a buffer of a prompted .db record, for editing."
  "Switch to a buffer of a prompted .db record, for editing."
  (declare (ignore p))
  (db:ensure-db-read)
  (let* ((name (prompt-for-string
		:default (word-at-point)
		:trim t
		:prompt "Name, surname or AKA: "
		:help "Enter name, surname of an AKA of the .db entry to edit."))
	 (*record* (db:find-record name)))
    (or *record* (editor-error "Search for record failed."))
    (find-object (car *record*)
		 (lambda (record)
		   (setf (car *record*) record)))
    (setf (buffer-minor-mode (current-buffer) "DB") t)))

(defcommand "Add Record" (p)
  "Add a .db record."
  "Add a .db record."
  (declare (ignore p))
  (db:ensure-db-read)
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

(defcommand "Save DB" (p)
  "Save the .db database to disk."
  "Save the .db database to disk."
  (declare (ignore p))
  (db:save-db))


;;; Objed mode.

(defmode "Objed" :major-p nil
  :precedence 4.0
  :documentation
  "Object editing mode.")

(defcommand "Save Object" (p)
  "Save the object represented in the current buffer."
  "Save the object represented in the current buffer."
  (declare (ignore p))
  (let ((object (value object)))
    (when object
      (let ((buffer (current-buffer)))
	(with-input-from-region (stream (buffer-region buffer))
	  (setv object (or (read stream nil)
			   (editor-error "nil was read from buffer"))))
	(setf (buffer-modified buffer) nil)
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
    (defhvar "Object"
      "The object under edit in this buffer."
      :buffer buffer
      :value object)
    (defhvar "Save Object Hook"
      "Hook to invoke when object under edit is saved."
      :buffer buffer
      :value (list save-hook))
    (with-output-to-mark (stream (current-point))
      (funcall printer object stream))
    (setf (buffer-modified buffer) nil)))

(defcommand "Inspect Object" (p)
  "Inspect the result of the evaluation of a prompted Lisp form."
  "Inspect the result of the evaluation of a prompted Lisp form."
  (declare (ignore p))
  (let* ((form (prompt-for-expression
		:prompt "Inspect: "
		:help "Enter Lisp form; the result will be inspected.")))
    (find-object (eval form)
		 #'identity
		 (lambda (object stream)
		   (inspect::tty-display-object (inspect::describe-parts object) stream)))))
