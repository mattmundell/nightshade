;;; .db database of people and other entities, and Objed mode.

(in-package "HEMLOCK")


;;; Structure.

(defmode "DB" :major-p nil
  :precedence 4.0
  :documentation
  ".db database mode.")

(defvar *db-records* nil
  "List of .db database records.")
(defvar *db-headings* nil
  "List of .db database headings.")
(defvar *db-table* nil
  "String table of .db names.")

(declaim (special *db-records* *db-headings))


;;; Helper functions.

(defun get-db-table ()
  "Return the string table of .db names."
  (or *db-table* (read-db)))

;; FIX do,use others
(defun db-record-forename (record)
  "Return the forename field of .db Record."
  (aref record 0))

(defun db-record-surname (record)
  "Return the surname field of .db Record."
  (aref record 1))

(defun db-record-akas (record)
  "Return the AKAs field of .db Record."
  (aref record 2))

(defun db-record-emails (record)
  "Return the emails field of .db Record."
  (aref record 6))

(defun db-record-full-name (record)
  "Return the emails field of .db Record."
  (let ((forename (db-record-forename record))
	(surname (db-record-surname record)))
    (string-trim " " (format nil "~A ~A" forename surname))))

(defun read-db ()
  "Read the .db database into memory."
  (let ((db (with-open-file (file ":.db" :direction :input
				  :if-does-not-exist :error)
	      (let ((*package*
		     (or (find-package "HEMLOCK")
			 (editor-error "Failed to find Hemlock package."))))
		(read file nil)))))
    (setq *db-records* (cdr db))
    (setq *db-headings* (car db)))
  (add-hook exit-hook 'save-db)
  (setq *db-table* (make-string-table))
  (dolist (record *db-records*)
    (setf (getstring (db-record-full-name record) *db-table*) record)
    (dolist (aka (db-record-akas record))
      (setf (getstring aka *db-table*) record)))
  *db-table*)

(defun save-db ()
  "Save the .db database to disk."
  (let ((dired:*report-function* 't))
    (dired:copy-file ":.db" ":.db.BAK" :update nil :clobber t)
    (with-open-file (file ":.db" :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
      (let ((*package* (or (find-package "HEMLOCK")
			   (editor-error "Failed to find Hemlock package."))))
	(write (cons *db-headings* *db-records*)
	       :stream file :readably t)
	(terpri file))
      t)))

(defun write-record (stream record)
  "Write .db Record to Stream."
  (let ((*field-number* 0)
	(heading-format (format nil "~~~DA: " 20))
	(sub-heading-format (format nil "    ~~~DA: " 16)))
    (map nil
	 (lambda (field)
	   (when field
	     (typecase field
	       (cons
		(typecase (car field)
		  (string
		   (format stream heading-format (nth *field-number* *db-headings*))
		   (format stream "~A" (car field))
		   (when (cdr field)
		     (mapcar (lambda (ele) (format stream ", ~A" ele)) (cdr field)))
		   (format stream "~%"))
		  (vector
		   (format stream "~A~%" (nth *field-number* *db-headings*))
		   (dolist (subrecord field)
		     (format stream sub-heading-format (aref subrecord 0))
		     (let ((*first* t))
		       (every (lambda (ele)
				(if *first*
				    (setq *first* nil)
				    (format stream "~A " ele))
				t)
			      subrecord))
		     (format stream "~%")))
		  (cons
		   (dolist (alistrecord field)
		     (format stream heading-format (car alistrecord))
		     (format stream "~A~%" (cdr alistrecord))))))
	       (string
		(format stream heading-format (nth *field-number* *db-headings*))
		(format stream "~A~%" field))
	       (vector
		(format stream "#~%"))))
	   (incf *field-number*))
	 record)))

(defun find-record (name)
  "Return index of record Name in *db-records*, else nil."
  (let ((name-len (length name)))
    (car
     (nthcdr
      (or
       (search `(,name) *db-records*
	       :test (lambda (field forename)
		       (let ((db-forename (aref field 0)))
			 (if (>= (length db-forename) name-len)
			     (string= db-forename forename :end1 name-len)))))
       (search `(,name) *db-records*
	       :test (lambda (field surname)
		       (let ((db-surname (aref field 1)))
			 (if (>= (length db-surname) name-len)
			     (string= db-surname surname :end1 name-len)))))
       (search `(,name) *db-records*
	       :test (lambda (field akas)
		       (find akas (aref field 2)
			     :test
			     (lambda (aka db-aka)
			       (if (>= (length db-aka) name-len)
				   (string= db-aka aka :end1 name-len))))))
       ;; Try again with case folding.
       (progn
	 (setq name (string-upcase name))
	 (or
	  (search `(,name) *db-records*
		  :test (lambda (field forename)
			  (let ((db-forename (string-upcase (aref field 0))))
			    (if (>= (length db-forename) name-len)
				(string= db-forename forename :end1 name-len)))))
	  (search `(,name) *db-records*
		  :test (lambda (field surname)
			  (let ((db-surname (string-upcase (aref field 1))))
			    (if (>= (length db-surname) name-len)
				(string= db-surname surname :end1 name-len)))))
	  (search `(,name) *db-records*
		  :test (lambda (field akas)
			  (find akas (aref field 2) ;; AKA.
				:test
				(lambda (aka db-aka)
				  (if (>= (length db-aka) name-len)
				      (string= (string-upcase db-aka) aka
					       :end1 name-len))))))
	  (return-from find-record))))
      *db-records*))))


;;; Commands.

(defcommand "Describe Record" (p)
  "Pop up a .db description of a prompted record.  With a prefix present the
   record in a buffer."
  "Pop up a .db description of a prompted record.  If P is true then present
   the record in a buffer."
  (or *db-records* (read-db))
  (let* ((name (prompt-for-string
		:default (word-at-point)
		:trim t
		:prompt "Name, surname or AKA: "
		:help "Enter name, surname or an AKA of the .db entry to edit."))
	 (record (find-record name)))
    (or record (editor-error "Search for record failed."))
    (if p
	(progn
	  (switch-to-buffer-command nil (find-file-buffer "FIX db"))
	  (with-output-to-mark (stream (current-point))
	    (write-record stream record)))
	(with-pop-up-display (stream)
	  (write-record stream record)))))

(defcommand "Edit Record" (p)
  "Switch to a buffer of a prompted .db record, for editing."
  "Switch to a buffer of a prompted .db record, for editing."
  (declare (ignore p))
  (or *db-records* (read-db))
  (let* ((name (prompt-for-string
		:default (word-at-point)
		:trim t
		:prompt "Name, surname or AKA: "
		:help "Enter name, surname of an AKA of the .db entry to edit."))
	 (*record* (find-record name)))
    (or *record* (editor-error "Search for record failed."))
    (find-object *record*
		 (lambda (record)
		   (setf (car *record*) record)))
    (setf (buffer-minor-mode (current-buffer) "DB") t)))

(defcommand "Add Record" (p)
  "Add a .db record."
  "Add a .db record."
  (declare (ignore p))
  (or *db-records* (read-db))
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
	(assert (eq (length *db-headings*) 9))
	(setq *db-records*
	      (cons (make-array 9
				:initial-contents
				(list forename surname nil nil
				      (list (make-array 2
							:initial-contents
							`("phone" ,phone)))
				      nil
				      `(,email)
				      (list (cons 'CREATION-DATE date)
					    (cons 'TIMESTAMP date))
				      ;; For what's this extra field?
				      nil))
		    *db-records*))))))

(defcommand "Save DB" (p)
  "Save the .db database to disk."
  "Save the .db database to disk."
  (declare (ignore p))
  (save-db))


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

(defun find-object (object &optional save-hook)
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
    (if use
	(or (stringp use) (delete-region (buffer-region buffer))))
    (defhvar "Object"
      "The object under edit in this buffer."
      :buffer buffer
      :value object)
    (defhvar "Save Object Hook"
      "Hook to invoke when object under edit is saved."
      :buffer buffer
      :value (list save-hook))
    (with-output-to-mark (stream (current-point))
      (write object :stream stream))
    (setf (buffer-modified buffer) nil)))
