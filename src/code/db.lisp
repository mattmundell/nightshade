;;; Interface to .db -- mini entity (org/person) oriented database.
;;; FIX mini contact database? contacts.lisp

(in-package "DB")

(export '(db-record-forename db-record-surname db-record-akas
	  db-record-emails db-record-url db-record-full-name
	  db-record-address
	  add-record find-record write-record
	  address-line-1 address-town address-code address-country
	  ensure-db-read get-db-table
	  read-db save-db))

(defvar *db-records* nil
  "List of .db database records.")
(defvar *db-headings* nil
  "List of .db database headings.")
(defvar *db-table* nil
  "String table of .db names.")

(declaim (special *db-records* *db-headings))

(defun get-db-table ()
  "Return the string table of .db names."
  (or *db-table* (read-db)))

;; FIX rename - db-
;; FIX def,use others

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

(defun db-record-url (record)
  "Return the URL field of .db Record."
  (cdr (assoc 'URL (aref record 7))))

(defun db-record-address (record)
  "Return the first address of .db Record."
  (car (aref record 5)))

(defun address-line-1 (address)
  "Return the first line of .db ADDRESS."
  (car (aref address 1)))

(defun address-town (address)
  "Return the town of .db ADDRESS."
  (aref address 2))

(defun address-code (address)
  "Return the post/zip code of .db ADDRESS."
  (aref address 4))

(defun address-country (address)
  "Return the country of .db ADDRESS."
  (aref address 5))

(defun db-record-full-name (record)
  "Return the emails field of .db Record."
  (let ((forename (db-record-forename record))
	(surname (db-record-surname record)))
    (string-trim " " (format nil "~A ~A" forename surname))))

(defun ensure-db-read ()
  "Ensure that the .db database is in memory."
  (or *db-records* (read-db)))

(defun read-db ()
  "Read the .db database into memory."
  (let ((db (with-open-file (file ":.db" :direction :input
				  :if-does-not-exist :error)
	      (let ((*package*
		     (or (find-package "DB")
			 (error "Failed to find DB package."))))
		(read file nil)))))
    (setq *db-records* (cdr db))
    (setq *db-headings* (car db)))
  (setq *db-table* (make-string-table))
  (dolist (record *db-records*)
    (setf (getstring (db-record-full-name record) *db-table*) record)
    (dolist (aka (db-record-akas record))
      (setf (getstring aka *db-table*) record)))
  *db-table*)

(defun save-db ()
  "Save the .db database to disk."
  (when (and *db-headings* *db-records*)
    (fs-copy-file "home:.db" "home:.db.BAK")
    (with-open-file (file ":.db.TEM" :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
      (let ((*package* (or (find-package "ED")
			   (error "Failed to find ED package."))))
	(write (cons *db-headings* *db-records*)
	       :stream file :readably t)
	(terpri file)))
    (delete-file "home:.db")
    (fs-copy-file "home:.db.TEM" "home:.db")
    (delete-file "home:.db.TEM")))

;; FIX rather pass in fields as args
(defun add-record (record)
  "Add Record to .db."
  (push record *db-records*))

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
  "Return record with Name in *db-records*, else nil."
  (let ((name-len (length name)))
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
     *db-records*)))
