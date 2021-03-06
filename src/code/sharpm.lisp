;;; Interim sharp macro.
;;;
;;; FIX Sort out eof-errorp for fundamental streams.

(in-package "LISP")

(export '(*read-eval*))


(proclaim '(special *read-suppress* std-lisp-readtable ; reader.lisp
		    *bq-vector-flag*)) ; backq.lisp

(defun ignore-numarg (sub-char numarg)
  (when numarg
    (warn "Numeric argument ignored in #~D~A." numarg sub-char)))

(defun sharp-left-bracket (stream bracket numarg eof-errorp eof-value)
  (ignore-numarg bracket numarg)
  (let* ((stream (in-synonym-of stream))
	 (pos (file-position stream))
	 (doc (with-output-to-string (out)
		(if (lisp-stream-p stream)
		    (prepare-for-fast-read-char stream
		      (do ((level 1)
			   (prev (fast-read-char eof-errorp eof-value)
				 char)
			   (char (fast-read-char eof-errorp eof-value)
				 (fast-read-char eof-errorp
						 eof-value)))
			  (())
			(or eof-errorp
			    (when (eq char eof-value)
			      (done-with-fast-read-char)
			      (return-from sharp-left-bracket
					   eof-value)))
			;; FIX any reason for nesting?
			;; FIX should be ok to #[ in doc.
			;; FIX how to ]# in doc?
			(cond ((and (char= prev #\])
				    (char= char #\#))
			       (setq level (1- level))
			       (when (zerop level)
				 (done-with-fast-read-char)
				 (return (values)))
			       (setq char
				     (fast-read-char eof-errorp
						     eof-value))
			       (or eof-errorp
				   (when (eq char eof-value)
				     (done-with-fast-read-char)
				     (return-from sharp-left-bracket
						  eof-value))))
			      ((and (char= prev #\#) (char= char #\[))
			       (%reader-error stream "Nested #[.")
			       (setq char
				     (fast-read-char eof-errorp
						     eof-value))
			       (or eof-errorp
				   (when (eq char eof-value)
				     (done-with-fast-read-char)
				     (return-from sharp-left-bracket
						  eof-value)))
			       (setq level (1+ level))))
			(write-char prev out)))
		    ;; Fundamental-stream.
		    (do ((level 1)
			 (prev (read-char stream t) char)
			 (char (read-char stream t) (read-char stream t)))
			(())
		      (cond ((and (char= prev #\]) (char= char #\#))
			     (setq level (1- level))
			     (when (zerop level)
			       (return (values)))
			     (setq char (read-char stream t)))
			    ((and (char= prev #\#) (char= char #\[))
			     (%reader-error stream "Nested #[.")
			     (setq char (read-char stream t))
			     (setq level (1+ level))))
			(write-char prev out))))))
    `(add-documentation
      ,(string-trim '(#\space #\tab) doc)
      ,(let ((pathname (pathname stream)))
	 (if pathname
	     (subseq (namestring pathname)
		     (length (namestring (truename "n:src/"))))))
      ,pos)))

(defun sharp-backslash (stream backslash numarg eof-errorp eof-value)
  (ignore-numarg backslash numarg)
  (let ((charstring (read-extended-token-escaped stream *readtable*
						 eof-errorp eof-value)))
    (or eof-errorp
	(if (eq charstring eof-value)
	    (return-from sharp-backslash eof-value)))
    (cond (*read-suppress* ())
	  ((= (the fixnum (length charstring)) 1)
	   (char charstring 0))
	  ((name-char charstring))
	  (t
	   (%reader-error stream "Unrecognized character name: ~S"
			  charstring)))))

(defun sharp-quote (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  ;; 4th arg tells read that this is a recursive call.
  (let ((symbol (read stream eof-errorp eof-value t)))
    (if (fi eof-errorp (eq symbol eof-value))
	eof-value
	`(function ,symbol))))

(defun sharp-left-paren (stream char length eof-errorp eof-value)
  (declare (ignore char) (special *backquote-count*))
  (let ((list (read-list stream () eof-errorp eof-value)))
    (if (fi eof-errorp (eq list eof-value))
	eof-value
	(let ((listlength (length list)))
	  (declare (fixnum listlength))
	  (cond (*read-suppress* ())
		((zerop *backquote-count*)
		 (if length
		     (cond ((> listlength (the fixnum length))
			    (%reader-error
			     stream
			     "Vector longer than specified length: #~S~S"
			     length list))
			   (t
			    (fill (the simple-vector
				       (replace (the simple-vector
						     (make-array length))
						list))
				  (car (last list))
				  :start listlength)))
		     (coerce list 'vector)))
		(t (cons *bq-vector-flag* list)))))))

(defun sharp-star (stream char numarg eof-errorp eof-value)
  (declare (ignore char eof-errorp eof-value))
  (multiple-value-bind (bstring escape-appearedp)
		       (read-extended-token stream)
    (declare (simple-string bstring))
    (cond (*read-suppress* ())
	  (escape-appearedp
	   (%reader-error stream "Escape character appeared after #*"))
	  ;; FIX This could be due to reaching EOF.
	  ((and numarg (zerop (length bstring)) (not (zerop numarg)))
	   (%reader-error
	    stream
	    "A bit vector with length requires at least one bit."))
	  ((or (null numarg) (>= (the fixnum numarg) (length bstring)))
	   (let* ((len1 (length bstring))
		  (last1 (1- len1))
		  (len2 (or numarg len1))
		  (bvec (make-array len2 :element-type 'bit
				    :initial-element 0)))
	     (declare (fixnum len1 last1 len2))
	     (do ((i 0 (1+ i))
		  (char ()))
		 ((= i len2))
	       (declare (fixnum i))
	       (setq char (elt bstring (if (< i len1) i last1)))
	       (setf (elt bvec i)
		     (cond ((char= char #\0) 0)
			   ((char= char #\1) 1)
			   (t
			    (%reader-error
			     stream
			     "Illegal element given for bit-vector: ~S"
			     char)))))
	     bvec))
	  (t
	   (%reader-error stream
			  "Bit vector is longer than specified length #~A*~A"
			  numarg bstring)))))

(defun sharp-colon (stream sub-char numarg eof-errorp eof-value)
  (declare (ignore eof-errorp eof-value))
  (ignore-numarg sub-char numarg)
  (multiple-value-bind (token escapep colon)
		       (read-extended-token stream)
    (declare (simple-string token) (ignore escapep))
    (cond
     (*read-suppress* ())
     (colon
      (%reader-error stream
		     "Symbol following #: contains a package marker: ~S"
		     token))
     (t
      (make-symbol token)))))


;;;; #. handling.

(defvar *read-eval* t
  "If true, then the #. read macro is enabled.")

(defun sharp-dot (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (let ((token (read stream eof-errorp eof-value t)))
    (if (fi eof-errorp (eq token eof-value))
	eof-value
	(unless *read-suppress*
	  (or *read-eval*
	      (%reader-error
	       stream
	       "Attempt to read #. while *READ-EVAL* is bound to ()."))
	  (eval token)))))


;;;; Numeric radix stuff.

(defun sharp-R (stream sub-char radix eof-errorp eof-value)
  (cond (*read-suppress*
	 (read-extended-token stream)
	 ())
	((not radix)
	 ;; Could be due to EOF, if stream ends in "#R".
	 (if eof-errorp
	     (%reader-error stream "Radix missing in #R.")
	     (if (eq (peek-char () stream () :eof) :eof)
		 eof-value
		 (%reader-error stream "Radix missing in #R."))))
	((not (<= 2 radix 36))
	 (%reader-error stream "Illegal radix for #R: ~D." radix))
	(t
	 (let ((res (let ((*read-base* radix))
		      (read stream eof-errorp eof-value t))))
	   (if (fi eof-errorp (eq res eof-value))
	       eof-value
	       (or (typep res 'rational)
		   (%reader-error stream
				  "#~A (base ~D) value is not a rational: ~S."
				  sub-char radix res)))
	   res))))

(defun sharp-B (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (sharp-r stream sub-char 2 eof-errorp eof-value))

(defun sharp-O (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (sharp-r stream sub-char 8 eof-errorp eof-value))

(defun sharp-X (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (sharp-r stream sub-char 16 eof-errorp eof-value))

(defun sharp-A (stream char dimensions eof-errorp eof-value)
  (declare (ignore char))
  (when *read-suppress*
    (let ((form (read stream eof-errorp eof-value t)))
      (return-from sharp-A
		   (if (fi eof-errorp (eq form eof-value))
		       eof-value))))
  (cond (dimensions
	 (collect ((dims))
	   (let* ((contents (read stream eof-errorp eof-value t))
		  (seq contents))
	     (or eof-errorp
		 (if (eq contents eof-value)
		     (return-from sharp-A eof-value)))
	     (dotimes (axis dimensions
		       (make-array (dims) :initial-contents contents))
	       (or (typep seq 'sequence)
		   (%reader-error stream
				  "#~DA axis ~D is not a sequence:~%  ~S"
				  dimensions axis seq))
	       (let ((len (length seq)))
		 (dims len)
		 (unless (= axis (1- dimensions))
		   (when (zerop len)
		     (%reader-error stream
				    "#~DA axis ~D is empty, but is not ~
				     the last dimension."
				    dimensions axis))
		   (setq seq (elt seq 0))))))))
	(t
	 (let ((list (read stream eof-errorp eof-value t)))
	   (if (fi eof-errorp (eq list eof-value))
	       eof-value
	       (destructuring-bind (element-type dims contents) list
		 (make-array dims :element-type element-type
			     :initial-contents contents)))))))

(defun sharp-S (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  ;; This needs to know about defstruct implementation.
  (when *read-suppress*
    (let ((form (read stream eof-errorp eof-value t)))
      (return-from sharp-S
		   (if (fi eof-errorp (eq form eof-value))
		       eof-value))))
  (let ((char (read-char stream eof-errorp eof-value))) ; FIX recursivep t?
    (or eof-errorp
	(if (eq char eof-value)
	    (return-from sharp-S eof-value)))
    (or (char= char #\( )
	(%reader-error stream "A list must follow #S")))
  (let ((body (read-list stream () eof-errorp eof-value)))
    (or eof-errorp
	(if (eq body eof-value)
	    (return-from sharp-S eof-value)))
    (or (listp body)
	(%reader-error stream "A list must follow #S: ~S" body))
    (or (symbolp (car body))
	(%reader-error stream "Structure type must be a symbol: ~S" (car body)))
    (let ((class (find-class (car body) ())))
      (or (typep class 'structure-class)
	  (%reader-error stream "~S must be a defined structure type."
			 (car body)))
      (let ((def-con (dd-default-constructor
		      (layout-info (class-layout class)))))
	(or def-con
	    (%reader-error
	     stream "The ~S structure must have a default constructor."
	     (car body)))
	(apply (fdefinition def-con) (rest body))))))


;;;; #=/##

;;; Holds values already seen by CIRCLE-SUBST.
;;;
(defvar *sharp-equal-circle-table*)

;; This function is kind of like FIX to NSUBLIS, but checks for
;; circularities and substitutes in arrays and structures as well as lists.
;; The first arg is an alist of the things to be replaced assoc'd with the
;; things to replace them.
;;
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree '(or cons (array t) structure-object)))
	 (let ((entry (find tree old-new-alist :key #'second)))
	   (if entry (third entry) tree)))
	((null (gethash tree *sharp-equal-circle-table*))
	 (setf (gethash tree *sharp-equal-circle-table*) t)
	 (cond ((typep tree 'structure-object)
		(do ((i 1 (1+ i))
		     (end (%instance-length tree)))
		    ((= i end))
		  (let* ((old (%instance-ref tree i))
			 (new (circle-subst old-new-alist old)))
		    (or (eq old new)
			(setf (%instance-ref tree i) new)))))
	       ((arrayp tree)
		(with-array-data ((data tree) (start) (end))
		  (declare (fixnum start end))
		  (do ((i start (1+ i)))
		      ((>= i end))
		    (let* ((old (aref data i))
			   (new (circle-subst old-new-alist old)))
		      (or (eq old new)
			  (setf (aref data i) new))))))
	       (t
		(let ((a (circle-subst old-new-alist (car tree)))
		      (d (circle-subst old-new-alist (cdr tree))))
		  (or (eq a (car tree))
		      (rplaca tree a))
		  (or (eq d (cdr tree))
		      (rplacd tree d)))))
	 tree)
	(t tree)))

;;; Sharp-equal works as follows.  When a label is assigned (ie when #= is
;;; called) we GENSYM a symbol which is used as an unforgeable tag.
;;; *SHARP-SHARP-ALIST* maps the integer tag to this gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label.  Resolution of the reference is deferred
;;; until the read done by #= finishes.  Any already resolved tags (in
;;; *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved object.
;;; Then for each entry in the *SHARP-SHARP-ALIST, the current object is
;;; searched and any uses of the gensysm token are replaced with the actual
;;; value.
;;;
(defvar *sharp-sharp-alist* ())
;;;
(defun sharp-equal (stream char label eof-errorp eof-value)
  (declare (ignore char))
  (if *read-suppress* (return-from sharp-equal (values)))
  (or label
      (%reader-error stream "Missing label for #=." label))
  (if (or (assoc label *sharp-sharp-alist*)
	  (assoc label *sharp-equal-alist*))
      (%reader-error stream "Multiply defined label: #~D=" label))
  (let* ((tag (gensym))
	 (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
	 (obj (read stream eof-errorp eof-value t)))
    (or eof-errorp
	(if (eq obj eof-value)
	    (return-from sharp-equal eof-value)))
    (if (eq obj tag)
	(%reader-error stream
		       ; FIX
		       "Have to tag something more than just #~D#."
		       label))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test #'eq :size 20)))
      (circle-subst *sharp-equal-alist* obj))))
;;;
(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp ()))
  (or label
      (%reader-error stream "Missing label for ##." label))
  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
	(third entry)
	(let ((pair (assoc label *sharp-sharp-alist*)))
	  (or pair
	      (%reader-error stream "Object is not labelled #~S#" label))
	  (cdr pair)))))


;;;; #+/-

(flet ((guts (stream not-p eof-errorp eof-value)
	 (block ()
	   (or (if (handler-case
		       (let* ((*package* *keyword-package*)
			      (*read-suppress*)
			      (form (read stream eof-errorp
					  eof-value t)))
			 (or eof-errorp
			     (if (eq form eof-value)
				 (return (values eof-value))))
			 (featurep form))
		     (reader-package-error
		      (condition)
		      (declare (ignore condition))
		      ()))
		   (not not-p)
		   not-p)
	       (let* ((*read-suppress* t)
		      (form (read stream eof-errorp eof-value t)))
		 (or eof-errorp
		     (if (eq form eof-value)
			 (return (values eof-value))))))
	   (values))))

  (defun sharp-plus (stream sub-char numarg eof-errorp eof-value)
    (ignore-numarg sub-char numarg)
    (guts stream () eof-errorp eof-value))

  (defun sharp-minus (stream sub-char numarg eof-errorp eof-value)
    (ignore-numarg sub-char numarg)
    (guts stream t eof-errorp eof-value)))

(defun sharp-C (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  ;; Next thing better be a list of two numbers.
  (let ((cnum (read stream eof-errorp eof-value t)))
    (or eof-errorp
	(if (eq cnum eof-value)
	    (return-from sharp-C eof-value)))
    (if *read-suppress* (return-from sharp-c ()))
    (if (and (listp cnum) (= (length cnum) 2))
	(complex (car cnum) (cadr cnum))
	(%reader-error stream "Illegal complex number format: #C~S" cnum))))

(defun sharp-vertical-bar (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
          (do ((level 1)
	       (prev (fast-read-char eof-errorp eof-value)
		     char)
	       (char (fast-read-char eof-errorp eof-value)
		     (fast-read-char eof-errorp eof-value)))
	      (())
	    (or eof-errorp
		(when (eq char eof-value)
		  (done-with-fast-read-char)
		  (return-from sharp-vertical-bar eof-value)))
	    (cond ((and (char= prev #\|) (char= char #\#))
		   (setq level (1- level))
		   (when (zerop level)
		     (done-with-fast-read-char)
		     (return (values)))
		   (setq char (fast-read-char eof-errorp eof-value))
		   (or eof-errorp
		       (when (eq char eof-value)
			 (done-with-fast-read-char)
			 (return-from sharp-vertical-bar eof-value))))
		  ((and (char= prev #\#) (char= char #\|))
		   (setq char (fast-read-char eof-errorp eof-value))
		   (or eof-errorp
		       (when (eq char eof-value)
			 (done-with-fast-read-char)
			 (return-from sharp-vertical-bar eof-value)))
		   (setq level (1+ level))))))
	;; Fundamental-stream.
	(do ((level 1)
	     (prev (read-char stream t) char)
	     (char (read-char stream t) (read-char stream t)))
	    (())
	  (cond ((and (char= prev #\|) (char= char #\#))
		 (setq level (1- level))
		 (when (zerop level)
		   (return (values)))
		 (setq char (read-char stream t)))
		((and (char= prev #\#) (char= char #\|))
		 (setq char (read-char stream t))
		 (setq level (1+ level))))))))

(defun sharp-exclaim (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
          (do ((char (fast-read-char eof-errorp eof-value)
		     (fast-read-char eof-errorp eof-value)))
	      (())
	    (or eof-errorp
		(when (eq char eof-value)
		  (done-with-fast-read-char)
		  (return (values eof-value))))
	    (cond ((char= char #\newline)
		   (done-with-fast-read-char)
		   (return (values))))))
	;; Fundamental-stream.
	(do ((char (read-char stream t) (read-char stream t)))
	    (())
	  (cond ((char= char #\newline)
		 (return (values))))))))

(defun sharp-illegal (stream sub-char ignore)
  (declare (ignore ignore))
  (%reader-error stream "Illegal sharp character ~S" sub-char))

(defun sharp-P (stream sub-char numarg eof-errorp eof-value)
  (ignore-numarg sub-char numarg)
  (let ((namestring (read stream eof-errorp eof-value t)))
    (or eof-errorp
	(if (eq namestring eof-value)
	    (return-from sharp-P eof-value)))
    (if namestring
	(if (eq namestring t)
	    (if (pathname stream) (namestring (pathname stream)))
	    ;; FIX make relative to root? (n:src/)
	    (parse-namestring namestring)))))

(defun sharp-true (stream sub-char numarg eof-errorp eof-value)
  (declare (ignore stream eof-errorp eof-value))
  (ignore-numarg sub-char numarg)
  t)

(defun sharp-false (stream sub-char numarg eof-errorp eof-value)
  (declare (ignore stream eof-errorp eof-value))
  (ignore-numarg sub-char numarg)
  ())

(make-dispatch-macro-character #\# t)
(set-dispatch-macro-character #\# #\\ #'sharp-backslash)
(set-dispatch-macro-character #\# #\' #'sharp-quote)
(set-dispatch-macro-character #\# #\( #'sharp-left-paren)
(set-dispatch-macro-character #\# #\[ #'sharp-left-bracket)
(set-dispatch-macro-character #\# #\* #'sharp-star)
(set-dispatch-macro-character #\# #\: #'sharp-colon)
(set-dispatch-macro-character #\# #\. #'sharp-dot)
(set-dispatch-macro-character #\# #\R #'sharp-R)
(set-dispatch-macro-character #\# #\r #'sharp-R)
(set-dispatch-macro-character #\# #\B #'sharp-B)
(set-dispatch-macro-character #\# #\b #'sharp-B)
(set-dispatch-macro-character #\# #\O #'sharp-O)
(set-dispatch-macro-character #\# #\o #'sharp-O)
(set-dispatch-macro-character #\# #\X #'sharp-X)
(set-dispatch-macro-character #\# #\x #'sharp-X)
(set-dispatch-macro-character #\# #\A #'sharp-A)
(set-dispatch-macro-character #\# #\a #'sharp-A)
(set-dispatch-macro-character #\# #\S #'sharp-S)
(set-dispatch-macro-character #\# #\s #'sharp-S)
(set-dispatch-macro-character #\# #\= #'sharp-equal)
(set-dispatch-macro-character #\# #\# #'sharp-sharp)
(set-dispatch-macro-character #\# #\+ #'sharp-plus)
(set-dispatch-macro-character #\# #\- #'sharp-minus)
(set-dispatch-macro-character #\# #\C #'sharp-C)
(set-dispatch-macro-character #\# #\c #'sharp-C)
(set-dispatch-macro-character #\# #\| #'sharp-vertical-bar)
(set-dispatch-macro-character #\# #\! #'sharp-exclaim)
(set-dispatch-macro-character #\# #\p #'sharp-p)
(set-dispatch-macro-character #\# #\P #'sharp-p)
(set-dispatch-macro-character #\# #\t #'sharp-true)
(set-dispatch-macro-character #\# #\f #'sharp-false)
(set-dispatch-macro-character #\# #\tab #'sharp-illegal)
(set-dispatch-macro-character #\# #\  #'sharp-illegal)
(set-dispatch-macro-character #\# #\) #'sharp-illegal)
(set-dispatch-macro-character #\# #\< #'sharp-illegal)
(set-dispatch-macro-character #\# #\form #'sharp-illegal)
(set-dispatch-macro-character #\# #\return #'sharp-illegal)
