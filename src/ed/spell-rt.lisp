;;; -*- Package: Spell -*-
;;;
;;; This file contains system dependent primitives for the spelling
;;; checking/correcting code in spell-correct.lisp, spell-augment.lisp,
;;; and spell-build.lisp.

(defpackage "SPELL"
  (:use "LISP" "EXTENSIONS" "SYSTEM")
  (:export correct-spelling
	   maybe-read-spell-dictionary max-entry-length
	   read-dictionary
	   spell-try-word spell-root-word spell-collect-close-words
	   spell-read-dictionary spell-add-entry spell-root-flags
	   spell-remove-entry)
  (:documentation "Spelling checker, including spelling correction and
dictionary handling."))

(in-package "SPELL")


;;;; System Area Referencing and Setting

(eval-when (compile eval)

;;; MAKE-SAP returns pointers that *dictionary*, *descriptors*, and
;;; *string-table* are bound to.  Address is in the system area.
;;;
(defmacro make-sap (address)
  `(system:int-sap ,address))

(defmacro system-address (sap)
  `(system:sap-int ,sap))


(defmacro allocate-bytes (count)
  `(system:allocate-system-memory ,count))

(defmacro deallocate-bytes (address byte-count)
  `(system:deallocate-system-memory (int-sap ,address) ,byte-count))


(defmacro sapref (sap offset)
  `(system:sap-ref-16 ,sap (* ,offset 2)))

(defsetf sapref (sap offset) (value)
  `(setf (system:sap-ref-16 ,sap (* ,offset 2)) ,value))


(defmacro sap-replace (dst-string src-string src-start dst-start dst-end)
  `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end))

(defmacro string-sapref (sap index)
  `(system:sap-ref-8 ,sap ,index))


;;;; Primitive String Hashing

;;; STRING-HASH employs the instruction SXHASH-SIMPLE-SUBSTRING which takes
;;; an end argument, so we do not have to use SXHASH.  SXHASH would mean
;;; doing a SUBSEQ of entry.
;;;
(defmacro string-hash (string length)
  `(ext:truly-the lisp::index
		  (%primitive sxhash-simple-substring
			      ,string
			      (the fixnum ,length))))

) ;eval-when


;;;; Binary Dictionary File I/O

(defun open-dictionary (f)
  (let* ((filename (ext:os-namestring f))
	 (kind (and filename (file-kind filename))))
    (or kind (error "Cannot find dictionary -- ~S." filename))
    (multiple-value-bind (fd err)
			 (unix:unix-open filename unix:o_rdonly 0)
      (or fd (error "Opening ~S failed: ~A." filename err))
      (multiple-value-bind (winp dev-or-err) (unix:unix-fstat fd)
	(or winp (error "Opening ~S failed: ~A." filename dev-or-err))
	fd))))

(defun close-dictionary (fd)
  (unix:unix-close fd))

(defun read-dictionary-structure (fd bytes)
  (let* ((structure (allocate-bytes bytes)))
    (multiple-value-bind (read-bytes err)
			 (unix:unix-read fd structure bytes)
      (or read-bytes
	  (= bytes read-bytes)
	  (progn
	    (deallocate-bytes (system-address structure) bytes)
	    (error "Reading dictionary structure failed: ~A." err)))
      structure)))
