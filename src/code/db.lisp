;;; Interface to .db -- mini entity (org/person) oriented database.
;; FIX mini contact database? contacts.lisp

(in-package "DB")

(export '(db-record-forename db-record-surname db-record-akas
	  db-record-emails db-record-notes db-record-url
	  db-record-full-name db-record-address
	  add-record find-record free-record find-records
	  find-records-any get-record write-record
	  field set-field subfields-p
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
		 db-record-notes
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

(defun db-record-notes (record)
  "Return the notes field of .db $record."
  (cdr (assoc 'NOTES (aref record 7))))

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

(defun field (record name &optional subname)
  "Return the value of field $name in $record.

   If $subname is given return the subfield $subname.  If subfield has
   multiple elements return them in a string, separating them by a \", \"."
  (let* ((sub-position)
	 (position (position name *db-headings*
			     :test
			     (lambda (name head)
			       (if (listp head)
				   (setq sub-position t)
				   (string= name
					    (symbol-name head)))))))
    (when position
      (let ((field (if sub-position
		       (cdr (or (assoc name (aref record position)
				       :test #'string=)
				(error "Field must exist: ~A" name)))
		       (aref record position))))
	(if subname
	    (let ((subfield (find subname field
				  :test (lambda (name ele)
					  (string= (aref ele 0)
						   name)))))
	      (when subfield
		(if (<= (length subfield) 2)
		    ;; Single element.
		    (aref subfield 1)
		    ;; Multiple elements, join them.
		    (macrolet ((maybe-add-comma ()
				 '(if (plusp (length rest))
				      (setq rest
					    (concatenate 'simple-string
							 ", " rest)))))
		      ;; First convert any ()'s to ""'s.
		      (map-into subfield
				(lambda (ele)
				  (or ele ""))
				subfield)
		      ;; Then join.
		      (reduce (lambda (ele rest)
				(concatenate
				 'simple-string
				 (etypecase ele
				   (string ele)
				   (list (reduce (lambda (ele rest)
						   (concatenate
						    'simple-string
						    ele ", " rest))
						 ele)))
				 ", "
				 rest))
			      subfield :start 1)))))
	    field)))))

(defun set-field (record name subname &rest values)
  "Set field $name in $record to the first of $values.  If $subname is
   given set subfield $subname of field $name to the $values.

   If $name names a new field then add the field with $value.

   Return $values on success.

   Signal an error on failing to find the field."
  (let* ((extension-p)
	 (position (position name *db-headings*
			     :test
			     (lambda (name head)
			       (if (listp head)
				   (setq extension-p t)
				   (string= name
					    (symbol-name head)))))))
    (if subname
	(let ((field (aref record position)))
	  (if extension-p
	      (error "Subname given with extension field: ~A with ~A"
		     subname name))
	  (let ((subfield (find subname field
				:test (lambda (name ele)
					(string= (aref ele 0)
						 name)))))
	    (if subfield
		(map-into subfield #'identity
			  (cons subname values))
		(setf (aref record position)
		      (cons (apply #'vector
				   subname
				   values)
			    field)))))
	(if extension-p
	    (if (car values)
		(if (position name (aref record position)
			      :test #'string= :key #'car)
		    ;; Existing field.
		    (setf (cdr (assoc name (aref record position)
				      :test #'string=))
			  (car values))
		    ;; New field.
		    (setf (aref record position)
			  (append (aref record position)
				  (list (cons (intern name "DB")
					      (car values))))))
		;; Remove the field.
		(delete-if (lambda (ele)
			     (string= (car ele) name))
			   (aref record position)))
	    (setf (aref record position) (car values))))
    (db:set-db-modified)
    (values-list values)))
;;
(defsetf field set-field
  "Set field $name in $record to $value.  Return true if the value was
   set.")

;; FIX move to contacts.lisp
(defun subfields-p (field)
  "Return true if $field can have subfields."
  (member field '("ADDRESSES" "PHONES") :test #'string=))

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

(defun free-record (record)
  "Release $record from .db, setting *db-modified*."
  (set-db-modified)
  (setq *db-records* (delq record *db-records*)))

(defun write-record (stream record)
  "Write .db $record to $stream."
  (let ((stream (lisp::make-indenting-stream stream))
	(*field-number* 0)
	(heading-format (format () "~~~DA:" 20))
	(sub-heading-format (format () "~C~~~DA:" #\tab 16)))
    (map ()
	 (lambda (field)
	   (when field
	     (typecase field
	       (cons
		(typecase (car field)
		  (string
		   (format stream heading-format
			   (nth *field-number* *db-headings*))
		   (if (or (plusp (length (car field))) (cdr field))
		       (write-string " " stream))
		   (format stream "~A" (car field))
		   (if (cdr field)
		       (mapcar (lambda (ele) (format stream ", ~A" ele))
			       (cdr field)))
		   (format stream "~&"))
		  (vector
		   (let ((field-name (nth *field-number*
					  *db-headings*)))
		     (format stream "~A~%" field-name)
		     (dolist (subfield field)
		       (format stream sub-heading-format
			       (aref subfield 0))
		       (format stream " ~A"
			       (field record
				      field-name
				      (aref subfield 0)))
#|
		     (let ((*first* t))
		       (every (lambda (ele)
				(if *first*
				    (setq *first* ())
				    (if (plusp (length ele))
					(format stream " ~A" ele)))
				t)
			      subfield))
|#
		       (format stream "~&"))))
		  (cons
		   (dolist (alistrecord field)
		     (format stream heading-format (car alistrecord))
		     ;; FIX this breaks generality, mv write-record to contacts.lisp
		     (if (string= (car alistrecord) "NOTES")
			 (progn
			   (setf (lisp::indenting-stream-indentation
				  stream)
				 4)
			   (format stream "~%~A~&" (cdr alistrecord))
			   (setf (lisp::indenting-stream-indentation
				  stream)
				 0))
			 (progn
			   (write-char #\space stream)
			   (format stream "~A~&" (cdr alistrecord))))))))
	       (string
		(format stream heading-format
			(nth *field-number* *db-headings*))
		(when (plusp (length field))
		  (write-string " " stream)
		  (format stream "~A" field))
		(terpri stream))
	       (vector
		(format stream "#~%"))))
	   (incf *field-number*))
	 record)))

(defun get-record (record)
  "Return the portion of the db list headed by $record."
  (while ((records *db-records* (cdr records)))
	 (records)
    (if (eq record (car records)) (return records))))

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
			(let ((db-forename (string-upcase (db-record-forename field))))
			  (if (>= (length db-forename) name-len)
			      (string= db-forename name :end1 name-len))))
		      *db-records*)
	     (keep-if (lambda (field)
			(let ((db-surname (string-upcase (db-record-surname field))))
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

;; TODO + search TAGS in `find-*'

(defun find-records-any (name)
  "Return a list of the records in *db-records* that contain the string
   $name anywhere in the forename, surname or AKA fields."
  (let ((name (string-upcase name)))
    (delete-duplicates
     (append (keep-if (lambda (field)
			(let ((db-forename (string-upcase
					    (db-record-forename
					     field))))
			  (search name db-forename)))
		      *db-records*)
	     (keep-if (lambda (field)
			(let ((db-surname (string-upcase
					   (db-record-surname field))))
			  (search name db-surname)))
		      *db-records*)
	     (keep-if (lambda (field)
			(find name (db-record-akas field)
			      :test
			      (lambda (aka db-aka)
				(search aka (string-upcase db-aka)))))
		      *db-records*)
	     (keep-if (lambda (field)
			(let ((db-notes (string-upcase (db-record-notes field))))
			  (search name db-notes)))
		      *db-records*)))))
