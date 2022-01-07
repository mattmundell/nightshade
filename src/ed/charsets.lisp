;;; Character sets and handling.
;;;
;;; FIX mv most to code:

(in-package "EDI")

(export '(*charsets-table*
	  buffer-charset-region
	  charset-from-utf-8-fun charset-to-utf-8-fun
	  charset-name
	  define-charset
	  find-charset))


;;;; Character set handling.

(defun iso-8859-1-to-utf-8 (string)
  (let ((result (make-string (* (length string) 2))))
    (while ((index 0 (1+ index))
	    (utf-index 0)
	    (end (length string)))
	   ((< index end)
	    (subseq result 0 utf-index))
      (let* ((char (aref string index))
	     (code (char-code char)))
	(cond ((< code 127)
	       (setf (aref result utf-index) char)
	       (incf utf-index))
	      ((eq code #xC4) ; A umlaut FIX confirm
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 132))
	       (incf utf-index))
	      ((eq code #xE4) ; a umlaut
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 164))
	       (incf utf-index))
	      ((eq code #xD6) ; O umlaut
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 150))
	       (incf utf-index))
	      ((eq code #xF6) ; o umlaut
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 182))
	       (incf utf-index))
	      ((eq code #xDC) ; U umlaut
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 188))
	       (incf utf-index))
	      ((eq code #xFC) ; u umlaut
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 188))
	       (incf utf-index))
	      ((eq code #xDF) ; ss
	       (setf (aref result utf-index) (code-char 195))
	       (incf utf-index)
	       (setf (aref result utf-index) (code-char 159))
	       (incf utf-index))
	      (t
	       (setf (aref result utf-index) #\?)
	       (incf utf-index)
	       (setf (aref result utf-index) #\?)
	       (incf utf-index)
	       (message "FIX ~A (char ~A)" code index)))))))

(defun utf-8-to-iso-8859-1 (string)
  (let ((result (make-string (length string))))
    (while ((index 0 (1+ index))
	    (iso-index 0)
	    (end (length string)))
	   ((< index end)
	    (subseq result 0 iso-index))
      (let* ((char (aref string index))
	     (code (char-code char)))
	(cond ((< code 127)
	       (setf (aref result iso-index) char)
	       (incf iso-index))
	      ((eq code 195)
	       (incf index)
	       ;;; Byte missing on end of string.
	       (or (< index end) (return))
	       (setq char (aref string index))
	       (setq code (char-code char))
	       (case code
		 (132 ; A umlaut
		  (setf (aref result iso-index) (code-char #xC4)) ; FIX confirm
		  (incf iso-index))
		 (164 ; a umlaut
		  (setf (aref result iso-index) (code-char #xE4))
		  (incf iso-index))
		 (150 ; O umlaut
		  (setf (aref result iso-index) (code-char #xD6))
		  (incf iso-index))
		 (182 ; o umlaut
		  (setf (aref result iso-index) (code-char #xF6))
		  (incf iso-index))
		 (156 ; U umlaut
		  (setf (aref result iso-index) (code-char #xDC))
		  (incf iso-index))
		 (188 ; u umlaut
		  (setf (aref result iso-index) (code-char #xFC))
		  (incf iso-index))
		 (159 ; ss
		  (setf (aref result iso-index) (code-char #xDF))
		  (incf iso-index))
		 (t
		  (setf (aref result iso-index) #\?)
		  (incf iso-index)
		  (message "FIX 195 ~A (char ~A)" code index))))
	      (t
	       (setf (aref result iso-index) #\?)
	       (incf iso-index)
	       (message "FIX ~A (char ~A)" code index)))))))

#|
(defun convert-from-iso-8859-1 (buffer)
  (filter-region #'iso-8859-1-to-utf-8 (buffer-region buffer))
  (defevar "Charset"
    "The character set of the text in the file shown in the buffer.  The
     text in the buffer is always in the internal character set."
    :value :iso-8859-1
    :buffer buffer))
|#

;;; Internal
;;;
;;; List of all the charsets.
;;;
(defvar *charsets* ())

;;; Internal
;;;
;;; String table of all the charsets.
;;;
(defvar *charsets-table* (make-string-table))

;;; Internal
;;;
;;; A character set.
;;;
(defstruct (charset
	    (:constructor make-charset
			  (name to-utf-8-fun from-utf-8-fun)))
  name            ; Symbol naming the charset, like :utf-8.
  to-utf-8-fun    ; Function, translates string from utf-8 to charset.
  from-utf-8-fun) ; Function, translates string from charset to utf-8.

(defun find-charset (name)
  "Return the charset named by $name, if there is one."
  (find name *charsets* :key #'charset-name))

(defmacro define-charset (name to-utf-8-fun from-utf-8-fun)
  "Define a character set with $name, $to-utf-8-fun and $from-utf-8-fun."
  (let ((charset (gensym)))
    `(progn
       (when (find-charset ,name)
	 (delete ,name *charsets* :key #'charset-name)
	 (delete-string (symbol-name ,name) *charsets-table*))
       (let ((,charset (make-charset ,name
				     ,to-utf-8-fun
				     ,from-utf-8-fun)))
	 (setf (getstring (symbol-name ,name) *charsets-table*) ,charset)
	 (push ,charset *charsets*)))))

(define-charset :iso-8859-1 #'iso-8859-1-to-utf-8 #'utf-8-to-iso-8859-1)
(define-charset :utf-8 #'identity #'identity)

;;; Internal
;;;
;;; If the buffer has a underlying charset then return a region like
;;; region, in the buffer charset, otherwise return ().
;;;
(defun buffer-charset-region (buffer)
  (let ((charset-name (if (editor-bound-p 'charset :buffer buffer)
			  (variable-value 'charset :buffer buffer))))
    (if charset-name
	(fi (eq charset-name :utf-8)
	    (let ((charset (find-charset charset-name)))
	      (if (and charset (charset-to-utf-8-fun charset))
		  (filter-region
		   (charset-from-utf-8-fun charset)
		   (copy-region (buffer-region buffer)))))))))

