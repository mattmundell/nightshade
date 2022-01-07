;;; Interface to .db -- mini entity (org/person) oriented database.
;;; FIX mini contact database? contacts.lisp

(in-package "DB")

(export '(db-record-forename db-record-surname db-record-akas
	  db-record-emails db-record-url db-record-full-name
	  db-record-address
	  add-record find-record find-records write-record
	  address-line-1 address-town address-code address-country
	  ensure-db-read get-db-table
	  read-db save-db set-db-modified ensure-db-saved))

(defvar *db-records* ()
  "List of .db database records.")
(defvar *db-headings* ()
  "List of .db database headings.")
(defvar *db-table* ()
  "String table of .db names.")
(defvar *db-modified* ()
  "True if *db-records* has been modified.")

(declaim (special *db-records* *db-headings))

(defun get-db-table ()
  "Return the string table of .db names."
  (or *db-table* (read-db)))

;; FIX rename - db-
;; FIX def,use others

(declaim (inline db-record-forename db-record-surname
		 db-record-akas db-record-emails db-record-url
		 db-record-address address-line-1 address-town
		 db-record-full-name))

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
  (string-trim " " (format () "~A ~A"
			   (db-record-forename record)
			   (db-record-surname record))))

(defun ensure-db-read (&optional message)
  "Ensure that the .db database is in memory."
  (or *db-records*
      (progn
	(if message (funcall message "Loading database from disk..."))
	(read-db))))

#|
(defun read-db ()
  "Read the .db database into memory."
  (let ((db (with-open-file (file ":.db" :direction :input
				  :if-does-not-exist :error)
	      (let ((*package*
		     (or (find-package "DB")
			 (error "Failed to find DB package."))))
		(read file ())))))
    (setq *db-records* (cdr db))
    (setq *db-headings* (car db)))
  (setq *db-table* (make-string-table))
  (dolist (record *db-records*)
    (setf (getstring (db-record-full-name record) *db-table*) record)
    (dolist (aka (db-record-akas record))
      (setf (getstring aka *db-table*) record)))
  *db-table*)
|#

(defun read-db ()
  "Read the .db database into memory."
  (setq *db-table* (make-string-table))
  (load (config:config-pathname "db-data") :verbose ())
  (dolist (record *db-records*)
    (setf (getstring (db-record-full-name record) *db-table*) record)
    (dolist (aka (db-record-akas record))
      (setf (getstring aka *db-table*) record)))
  *db-table*)

#|
(defun save-db ()
  "Save the .db database to disk."
  (when (and *db-headings* *db-records*)
    (copy-file "home:.db" "home:.db.BAK")
    (with-open-file (file ":.db.TEM" :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
      (let ((*package* (or (find-package "ED")
			   (error "Failed to find ED package."))))
	(write (cons *db-headings* *db-records*)
	       :stream file :readably t)
	(terpri file)))
    (delete-file "home:.db")
    (lisp:copy-file "home:.db.TEM" "home:.db")
    (delete-file "home:.db.TEM")))
|#

(defun set-db-modified ()
  (setq *db-modified* t))

(defun ensure-db-saved ()
  "Save the .db database to disk if *db-modified* is true."
  (when *db-modified*
    (save-db)
    (setq *db-modified* ())))

(defun save-db ()
  "Save the .db database to disk."
  (when (and *db-headings* *db-records*)
    (copy-file (config-pathname "db-data.lisp") (config-pathname "db-data.lisp.BAK"))
    (with-open-file (file (config-pathname "db-data.lisp.TEM") :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
      (let ((*package* (or (find-package "ED")
			   (error "Failed to find ED package."))))
	(write '(in-package "DB") :stream file :readably t)
	(terpri file)
	(write '(setq *db-headings*
		      '(DB::FORENAME DB::SURNAME DB::AKAS DB::ORG DB::PHONES DB::ADDRESSES DB::EMAILS
				     (DB::CREATION-DATE DB::TIMESTAMP DB::MAIL-ALIAS DB::NOTES DB::FAX DB::URL
							DB::IRC-NICK DB::FINGER-HOST DB::IRC-CHANNEL DB::SIP)))
	       :stream file :readably t)
	(terpri file)
	(write `(setq *db-records* ',*db-records*)
	       :stream file :readably t)
	(terpri file)))
    (delete-file (config-pathname "db-data.lisp"))
    (lisp:copy-file (config-pathname "db-data.lisp.TEM") (config-pathname "db-data.lisp"))
    (delete-file (config-pathname "db-data.lisp.TEM"))
    (compile-file (config-pathname "db-data.lisp")
		  :verbose () :print () :byte-compile ())))

;; FIX rather pass in fields as args
(defun add-record (record)
  "Add Record to .db, setting *db-modified*."
  (set-db-modified)
  (push record *db-records*))

(defun write-record (stream record)
  "Write .db Record to Stream."
  (let ((*field-number* 0)
	(heading-format (format () "~~~DA:" 20))
	(sub-heading-format (format () "    ~~~DA:" 16)))
    (map ()
	 (lambda (field)
	   (when field
	     (typecase field
	       (cons
		(typecase (car field)
		  (string
		   (format stream heading-format (nth *field-number* *db-headings*))
		   (if (or (plusp (length (car field))) (cdr field))
		       (write-string " " stream))
		   (format stream "~A" (car field))
		   (if (cdr field)
		       (mapcar (lambda (ele) (format stream ", ~A" ele)) (cdr field)))
		   (format stream "~%"))
		  (vector
		   (format stream "~A~%" (nth *field-number* *db-headings*))
		   (dolist (subrecord field)
		     (format stream sub-heading-format (aref subrecord 0))
		     (if (plusp (length subrecord))
			 (write-string " " stream))
		     (let ((*first* t))
		       (every (lambda (ele)
				(if *first*
				    (setq *first* ())
				    (format stream "~A" ele))
				t)
			      subrecord))
		     (format stream "~%")))
		  (cons
		   (dolist (alistrecord field)
		     (format stream heading-format (car alistrecord))
		     (format stream " ~A~%" (cdr alistrecord))))))
	       (string
		(format stream heading-format (nth *field-number* *db-headings*))
		(when (plusp (length field))
		  (write-string " " stream)
		  (format stream "~A~%" field)))
	       (vector
		(format stream "#~%"))))
	   (incf *field-number*))
	 record)))

(defun find-record (name)
  "Return record with Name in *db-records* if any, else ()."
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
		 :test (lambda (field name-aka)
			 (find name-aka (aref field 2) ;; AKA.
			       :test
			       (lambda (aka db-aka)
				 (if (>= (length db-aka) name-len)
				     (string= (string-upcase db-aka) aka
					      :end1 name-len))))))
	 (return-from find-record))))
     *db-records*)))

(defun find-records (name)
  "Return list of records with $name in *db-records* if any, else ()."
  (let ((name (string-upcase name))
	(name-len (length name)))
    (delete-duplicates
     (append (keep-if (lambda (field)
			(let ((db-forename (string-upcase (aref field 0))))
			  (if (>= (length db-forename) name-len)
			      (string= db-forename name :end1 name-len))))
		      *db-records*)
	     (keep-if (lambda (field)
			(let ((db-surname (string-upcase (aref field 0))))
			  (if (>= (length db-surname) name-len)
			      (string= db-surname name :end1 name-len))))
		      *db-records*)
	     (keep-if (lambda (field)
			(find name (aref field 2) ;; AKA.
			      :test
			      (lambda (aka db-aka)
				(if (>= (length db-aka) name-len)
				    (string= (string-upcase db-aka) aka
					     :end1 name-len)))))
		      *db-records*)))))
