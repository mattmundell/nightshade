;;; Document processing system.

(in-package "DOC")

(export '(*encode-emails*
	  defun-doc-to-text doc-to-text
	  defun-doc-to-html doc-to-html
	  defun-doc-to-latex doc-to-latex
	  return-from-many return-from-row))

;;; TODO the emit versions should produce as much of the final
;;;      doc as possible
;;;      ie they should recurse into descendent nodes first to find
;;;      out if the descendents are static and evaluate those
;;;      that are static and output code to write the text resulting
;;;      from the static nodes (instead of outputting code which writes
;;;      text for every single descendent in the tree separately).

;;; FIX move to manual (pkg doc? reader doc linked from pkg doc), complete
;;;
;;; The defun-doc-to-* macros convert a description of a document into a
;;; function that produces the document in a given format.  For example
;;; `defun-doc-to-text' produces a function that produces a plain text
;;; version of the document given to defun-doc-to-text.  A document
;;; description is a list of lists where each of the lists is a part of the
;;; document.  The content produced by each node can be static or dynamic.
;;;
;;; For example
;;;
;;;     `((table ((row (((string . "CURRICULUM"))
;;;                    (((data . (db-record-full-name
;;;                               (get-db-record))))))))))
;;;
;;; defines a document that is a single table with a single row with two
;;; cells: a static string "CURRICULUM" and the result of the function call
;;; (db-record-full-name (get-db-record)).  The document is dynamic; the
;;; call to `db-record-full-name' is made when the document is rendered or
;;; converted.
;;;
;;; A string node results in output to the stream in the variable *stream*,
;;; as does a data node if the associated function call returns a true
;;; value.
;;;
;;; Node types: string, data, rule, row, table, description, section, many,
;;; ref.


;;;; Description accessors.

(defun gnode-type (node)
  "Return the type of NODE."
  (caar node))

(defun gnode-next (node)
  "Return the node following NODE."
  (cdr node))

(defun gnode-content (node)
  "Return the content of NODE."
  (cadar node))

(defun gnode-args (node)
  "Return the args of NODE."
  (cddar node))

(defun gnode-single-content (node)
  "Return the content of single NODE (e.g. a string or data node)."
  (cdar node))


;;;; Helpers.

(declaim (inline return-from-many))
(defun return-from-many (&optional value)
  "Return VALUE from the code handling the innermost many node."
  (throw 'end-many value))

(declaim (inline return-from-row))
(defun return-from-row (&optional value)
  "Return VALUE from the code handling the current row."
  (throw 'end-row value))

(defun style-size-to-portion (size)
  (fi size
      0
      (let ((size (string-trim " " size)))
	(cond ((char= (char size (1- (length size))) #\%)  ;; FIX (last size)?
	       (/ (parse-integer (subseq size 0 (1- (length size)))) ;; FIX (subseq size 0)?
		  100))
	      ;; FIX correct em handling
	      ((and (char= (char size (1- (length size))) #\m)
		    (char= (char size (- (length size) 2)) #\e))
	       (concat (subseq size 0 (- (length size) 2)) "pt")
	       (error "FIX em: ~A" size))
	      ((or (string= size "0") (string= (string-upcase size) "AUTO"))
	       0)
	      (t
	       (error "FIX style-size-to-portion: \"~A\"" size))))))

(defvar *encode-emails* ()
  "If true email addresses will be encoded.")

(defun encode-email (address)
  "Encode ADDRESS slightly: a@b.c => a(at)b.c."
  (if *encode-emails*
      (let ((parts (split address #\@)))
	(concat (car parts) "(at)" (cadr parts)))
      address))


;;;; Plain text.

(declaim (special *emit* *first* *indent* *indents*
		  *column-widths* *fill-widths* *section-depth*))

(defvar *verbatim* ())

;; FIX use flet instead of labels  (somehow fix to keep functions)
;;        may be rqrd for cont (if every used)
;; FIX can the emit-switched cases combine?
;;     what is the combined result? ie the generic term for it
;;         some sort of combined compiler,interpreter implementation
;;     combination exploring in test.lisp
(defmacro translate-doc-to-text (emit &body body)
  "If $emit is true then return code that will translate $body, a document
   description (a list of nodes), into the body of a function that will
   produce plain text for the documents described by $body i.e. compile a
   function that will produce the defined document.  Otherwise return the
   body of an interpreter that will produce a document in plain text given
   a description (list of nodes) i.e. compile a doc definition to text
   document translator."
  `(labels
       ((open-line ()
	  ,(if (eval emit)
	       '`(if *first*
		     (setq *first* ())
		     (terpri stream))
	       '(if *first*
		    (setq *first* ())
		    (terpri stream))))
	(buffer-rows (rows)
	  "Return the table defined by $rows, as a list of lists of
	   strings."
	  ,(if (eval emit)
	       '(collect ((out))
		  (flet
		      ((collect-row (row)
			 (collect ((row-out))
			   (while ((cell (gnode-content row)
					 (gnode-next cell))
				   (i 0 (1+ i)))
				  (cell)
			     ;; A cell is a list of nodes.
			     (while ((nodes)
				     (node (car cell) (cdr node)))
				    (node
				     (row-out
				      ;; Beware, generating a var called
				      ;; nodes too.
				      `(let* ((nodes (list ,@(nreverse
							      nodes)))
					      (max (nthcdr ,i
							   *column-widths*))
					      (width (+ (reduce #'+
								nodes
								:key
								#'length)
							3))) ; FIX *space-between-columns*
					 ;(format t "nodes ~A~%" nodes)
					 ;(format t "w ~A m ~A c ~A~%" width max *column-widths*)
					 (if max
					     (if (< (car max) width)
						 (setf (car max) width))
					     (setq *column-widths*
						   (append *column-widths*
							   (list width))))
					 nodes)))
			       (push
				`(let* ((ss (make-string-output-stream))
					(stream (lisp::make-indenting-stream
						 ss)))
				   ,(write-generic-node node)
				   (let* ((str (get-output-stream-string
						ss)))
				     str))
				nodes)))
			   (row-out))))
		    (loop for row = rows then (gnode-next row) while row do
		      (if (eq (intern (symbol-name (gnode-type row)) "DOC")
			      'many)
			  ;; FIX other many loops check cont and write-g-node return
			  (progn
			    (out `(catch 'end-many
				    (loop
				      (catch 'end-row
					(buffer-list (list ,@(collect-row
							      (gnode-content row)))))))))
			  (out `(catch 'end-row
				  (buffer-list (list ,@(collect-row row))))))))
		  `(progn
		     ;; FIX dynamic context would be neater
		     (push fill-width *fill-widths*)
		     (setq fill-width ())
		     (collect ((buffer-list))
		       (unwind-protect
			   (progn ,@(out))
			 (setq fill-width (pop *fill-widths*)))
		       (buffer-list))))
	       '(collect ((out))
		  (push fill-width *fill-widths*)
		  (setq fill-width ())
		  (unwind-protect
		      (while ((row rows (gnode-next row)))
			     (row)
			(flet
			    ((collect-row (row)
			       (collect ((row-out))
				 (while ((cell (gnode-content row)
					       (gnode-next cell))
					 (i 0 (1+ i)))
					(cell)
				   ;; A cell is a list of nodes.
				   (while ((nodes)
					   (node (car cell)
						 (cdr node)))
					  (node
					   (let* ((nodes
						   (nreverse nodes))
						  (max (nthcdr
							i
							*column-widths*))
						  (width (+ (reduce #'+ nodes
								    :key #'length)
							    3))) ; FIX *space-between-columns*
					     (if max
						 (if (< (car max) width)
						     (setf (car max) width))
						 (setq *column-widths*
						       (append
							*column-widths*
							(list width))))
					     (row-out nodes)))
				     ;; Simulate binding of `stream', as
				     ;; "call" to write-generic-node
				     ;; jumps out of binding.
				     (push
				      (let ((ss (make-string-output-stream))
					    (old-stream stream))
					(setq stream
					      (lisp::make-indenting-stream
					       ss))
					(unwind-protect
					    (write-generic-node node)
					  (setq stream old-stream))
					(let* ((str (get-output-stream-string
						     ss)))
					  str))
				      nodes)))
				 (out (row-out)))))
			  ;; FIX note there can only be one many per row ((table ((many ((row..
			  (if (eq (intern (symbol-name (gnode-type row)) "DOC")
				  'many)
			      (catch 'end-many
				(loop
				  (catch 'end-row
				    (collect-row (gnode-content row)))))
			      (catch 'end-row (collect-row row)))))
		    (setq fill-width (pop *fill-widths*)))
		  (out))))
	(push-indentation (args)
	  (if (cadr args)
	      ,(if (eval emit)
		   '`(let ((indent (truncate (* ,(cadr args) fill-width))))
		       (push indent *indents*)
		       (incf *indent* indent)
		       (setf (lisp::indenting-stream-indentation stream)
			     *indent*))
		   '(let ((indent (truncate (* (cadr args) fill-width))))
		      (push indent *indents*)
		      (incf *indent* indent)
		      (setf (lisp::indenting-stream-indentation stream)
			    *indent*)))))
	(push-absolute-indentation (width)
	  ,(if (eval emit)
	       '`(let ((indent ,width))
		   (push indent *indents*)
		   (incf *indent* indent)
		   (setf (lisp::indenting-stream-indentation stream)
			 *indent*))
	       '(let ((indent width))
		  (push indent *indents*)
		  (incf *indent* indent)
		  (setf (lisp::indenting-stream-indentation stream)
			*indent*))))
	(pop-indentation (args)
	  (if (cadr args)
	      ,(if (eval emit)
		   ''(setf (lisp::indenting-stream-indentation stream)
			   (decf *indent* (pop *indents*)))
		   '(setf (lisp::indenting-stream-indentation stream)
			  (decf *indent* (pop *indents*))))))
	(pop-absolute-indentation ()
	  ,(if (eval emit)
	       ''(setf (lisp::indenting-stream-indentation stream)
		       (decf *indent* (pop *indents*)))
	       '(setf (lisp::indenting-stream-indentation stream)
		      (decf *indent* (pop *indents*)))))
	(output-string (string &optional fill)
	  ,(if (eval emit)
	       '`(let* ((string ,string)
			(start 0)
			(len (length string)))
		   (if fill-width
		       (let ((fill-width (- fill-width *indent*)))
			 (while ((end fill-width (+ end fill-width)))
				(t)
			   (when (>= end len)
			     (write-string string stream :start start)
			     (if ,fill
				 (write-string (make-string
						(- end len)
						:initial-element #\space)
					       stream))
			     (return))
			   (until ((new-end end (1- new-end)))
				  ((or (< new-end start)
				       (char= (char string new-end)
					      #\space))
				   (or (>= start new-end)
				       (setq end new-end))))
			   (write-line string stream
				       :start start :end end)
			   (setq start (incf end))))
		       ;; For buffer-rows.
		       (write-string string stream :start start))
		   t)
	       `(let* ((start 0)
		       (len (length string)))
		  (if fill-width
		      (let ((fill-width (- fill-width *indent*)))
			;; Write the line in portions, to keep the text to
			;; a certain width.
			(while ((end fill-width (+ end fill-width)))
			       (t)
			  (when (>= end len)
			    ;; Enough space on line, write the rest.
			    (write-string string stream :start start)
			    (when fill
			      (write-string (make-string
					     (- end len)
					     :initial-element #\space)
					    stream))
			    (return))
			  ;; Move end backwards to the beginning of a word.
			  (until ((new-end end (1- new-end)))
				 ((or (< new-end start)
				      (char= (char string new-end)
					     #\space))
				  (or (>= start new-end)
				      (setq end new-end))))
			  (write-line string stream
				      :start start :end end)
			  (setq start (incf end))))
		      ;; For buffer-rows.
		      (write-string string stream))
		  t)))
	(write-generic-node (node)
	  (ecase (intern (symbol-name (gnode-type node)) "DOC")
	    (many
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let ((cont t))
			,(push-indentation args)
			(catch 'end-many
			  (while ()
				 ((and ,(write-generic-nodes
					 (gnode-content node))
				       cont)
				  cont)))
			,(pop-indentation args))
		    '(let ((cont t))
		       (push-indentation args)
		       (catch 'end-many
			 (while ()
				((and (write-generic-nodes
				       (gnode-content node))
				      cont)
				 cont)))
		       (pop-indentation args)))))
	    (interpret
	     ,(if (eval emit)
		  '`(%doc-to-text ,(car (gnode-content node))
				  stream styles fill-width *indent* *indents*)
		  '(%doc-to-text (eval (car (gnode-content node)))
				 stream styles fill-width *indent* *indents*)))
	    (list
	     ,(if (eval emit)
		  '(collect ((out))
		     (while ((item (gnode-content node) (gnode-next item)))
			    (item)
		       (out (open-line))
		       (out '(write-string "- " stream))
		       (out (push-absolute-indentation 2))
		       (out `(let ((*first*))
			       (setq last ,(write-generic-node item))))
		       (out (pop-absolute-indentation))
		       (out '(fresh-line stream)))
		     `(let ((last))
			(progn ,@(out))
			last))
		  '(let ((last))
		     (while ((item (gnode-content node) (gnode-next item)))
			    (item)
		       (open-line)
		       (write-string "- " stream)
		       (push-absolute-indentation 2)
		       (let ((*first*))
			 (setq last (write-generic-node item)))
		       (pop-absolute-indentation)
		       (fresh-line stream))
		     last)))
	    (paragraph
	     ,(if (eval emit)
		  ;; FIX buffer paragraph else terpri[,write] when
		  ;;     return-from-many
		  '`(prog1
			(progn
			  ,(open-line)
			  ,(write-generic-nodes (gnode-content node)))
		      (fresh-line stream))
		  '(prog1
		       (progn
			 (open-line)
			 (write-generic-nodes (gnode-content node)))
		     (fresh-line stream))))
	    (verbatim
	     ,(if (eval emit)
		  ;; FIX buffer paragraph else terpri[,write] when
		  ;;     return-from-many
		  '`(prog1
			(progn
			  ,(open-line)
			  ,(write-generic-nodes (gnode-content node)))
		      (fresh-line stream))
		  '(prog1
		       (progn
			 (open-line)
			 (write-generic-nodes (gnode-content node)))
		     (fresh-line stream))))
	    (section
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(progn
			#|
		       let* ((props (css:style styles :section ,(car args)))
			     (width (css:style-property props :width))
			     (margin-left (css:style-property props
							      :margin-left))
			     (margin-right (css:style-property props
							       :margin-right))
			     (portion (if width
					  (style-size-to-portion width)
					  1)))
			|#
			(incf *section-depth*)
			,(open-line)
			(case *section-depth*
			  (1
			   (terpri stream)
			   #|
			   (let ((heading (substitute #\_ #\space ,(cadr args))))
			     (format stream "~A" heading)
			     (loop repeat (- fill-width (length heading)) do  ;; FIX - fill-width indent?
			       (write-char #\_ stream)))
			   |#
			   (format stream "~A~%" ,(cadr args))
			   (loop repeat (length ,(cadr args)) do
			     (write-char #\= stream)))
			  (2
			   (format stream "~A~%" ,(cadr args))
			   (terpri stream)
			   (loop repeat (length ,(cadr args)) do
			     (write-char #\- stream)))
			  (3
			   (format stream "~A~%" ,(cadr args))
			   (loop repeat (length ,(cadr args)) do
			     (write-char #\. stream)))
			  (t
			   (format stream "~A" ,(cadr args))))
			;; FIX
			;,(push-indentation '(t 1/2))
			;,(push-indentation '(list t portion))
			(terpri stream)
			(unwind-protect
			    ,(write-generic-nodes (gnode-content node))
			  (progn
			    ;,(push-indentation '(t 1/2))
			    ;,(pop-indentation (list t portion))
			    ;,(pop-indentation (list t portion))
			    (decf *section-depth*))))
		    '(progn
		       #| let* ((props (css:style styles :section (car args)))
			    (width (css:style-property props :width))
			    (margin-left (css:style-property props
							     :margin-left))
			    (margin-right (css:style-property props
							      :margin-right))
			    (portion (if width
					 (style-size-to-portion width)
					 1)))
		       |#
		       (incf *section-depth*)
		       (open-line)
		       (case *section-depth*
			 (1
			  (terpri stream)
			  #|
			  (let ((heading (substitute #\_ #\space (cadr args))))
			    (format stream "~A" heading)
			    (loop repeat (- fill-width (length heading)) do
			      (write-char #\_ stream)))
			  |#
			  (format stream "~A~%" (cadr args))
			  (loop repeat (length (cadr args)) do
			    (write-char #\= stream)))
			 (2
			  (terpri stream)
			  (format stream "~A~%" (cadr args))
			  (loop repeat (length (cadr args)) do
			    (write-char #\- stream)))
			 (2
			  (format stream "~A~%" (cadr args))
			  (loop repeat (length (cadr args)) do
			    (write-char #\. stream)))
			 (t
			  (format stream "~A" (cadr args))))
		       ;; FIX
		       ;(push (list t portion) *fill-widths*) ; Temp, for passing.
		       (unwind-protect
			   (progn
			     ;(push-indentation (car *fill-widths*))
			     (terpri stream)
			     (write-generic-nodes (gnode-content node)))
			 ;(pop-indentation '(list t portion))
			 (pop *fill-widths*)
			 (decf *section-depth*))))))
	    (table
	     ;; FIX This needs more work to handle tables where text in the first
	     ;;     cols can wrap (the last col work ok now).  Probably
	     ;;     buffer-rows should return a list with the cols already
	     ;;     of the correct size ie it should handle wrapping of
	     ;;     text past the edges of the columns.
	     (let* ((rows (gnode-content node)))
	       ,(if (eval emit)
		    '`(let ((row-buffer (progn
					  (setq *column-widths* ())
					  ,(buffer-rows rows))))
			(when row-buffer
			  ,(open-line)
			  (push fill-width *fill-widths*)
			  (while ((row row-buffer (cdr row)))
				 (row)
			    (setq fill-width *indent*)
			    (while ((cells (car row) (cdr cells))
				    (col 0 (1+ col)))
				   (cells)
			      (let ((old-width fill-width))
				(setq fill-width
				      (min (car *fill-widths*)
					   (+ fill-width (nth col *column-widths*))))
				(if (plusp col)
				    ,(push-absolute-indentation 'old-width)))
			      (while ((node (car cells) (cdr node)))
				     (node)
				,(output-string '(car node)
						;; Either last node in row
						;; or fill.
						'(fi (cdr node) (cdr cells))))
			      (if (plusp col)
				  ,(pop-absolute-indentation)))
			    (terpri stream))
			  (setq fill-width (pop *fill-widths*)))
			t)
		    `(let ((row-buffer (progn
					 (setq *column-widths* ())
					 (buffer-rows rows))))
		       (when row-buffer
			 (open-line)
			 (push fill-width *fill-widths*)
			 (while ((row row-buffer (cdr row)))
				(row)
			   (setq fill-width *indent*)
			   (while ((cells (car row) (cdr cells))
				   (col 0 (1+ col)))
				  (cells)
			     (let ((old-width fill-width))
			       (setq fill-width
				     (min (car *fill-widths*)
					  (+ fill-width (nth col *column-widths*))))
			       (if (plusp col)
				   (push-absolute-indentation old-width)))
			     (while ((node (car cells) (cdr node)))
				    (node)
			       (output-string (car node)
					      ;; Either last node in row or
					      ;; fill.
					      (fi (cdr node) (cdr cells))))
			     (if (plusp col)
				 (pop-absolute-indentation)))
			   (terpri stream))
			 (setq fill-width (pop *fill-widths*)))
		       t))))
	    (description
	     ;; FIX just copied in from table above
	     ;; FIX This needs more work to handle tables where text in the first
	     ;;     cols can wrap (the last col work ok now).  Probably
	     ;;     buffer-rows should return a list with the cols already
	     ;;     of the correct size ie it should handle wrapping of
	     ;;     text past the edges of the columns.
	     (let* ((rows (gnode-content node)))
	       ,(if (eval emit)
		    '`(let ((row-buffer (progn
					  (setq *column-widths* ())
					  ,(buffer-rows rows))))
			(when row-buffer
			  ,(open-line)
			  (push fill-width *fill-widths*)
			  (while ((row row-buffer (cdr row)))
				 (row)
			    (setq fill-width *indent*)
			    (while ((cells (car row) (cdr cells))
				    (col 0 (1+ col)))
				   (cells)
			      (let ((old-width fill-width))
				(setq fill-width
				      (min (car *fill-widths*)
					   (+ fill-width (nth col *column-widths*))))
				(if (plusp col)
				    ,(push-absolute-indentation 'old-width)))
			      (while ((node (car cells) (cdr node)))
				     (node)
				,(output-string '(car node)
						;; Either last node in row
						;; or fill.
						'(fi (cdr node) (cdr cells))))
			      (if (plusp col)
				  ,(pop-absolute-indentation)))
			    (terpri stream))
			  (setq fill-width (pop *fill-widths*)))
			t)
		    `(let ((row-buffer (progn
					 (setq *column-widths* ())
					 (buffer-rows rows))))
		       (when row-buffer
			 (open-line)
			 (push fill-width *fill-widths*)
			 (while ((row row-buffer (cdr row)))
				(row)
			   (setq fill-width *indent*)
			   (while ((cells (car row) (cdr cells))
				   (col 0 (1+ col)))
				  (cells)
			     (let ((old-width fill-width))
			       (setq fill-width
				     (min (car *fill-widths*)
					  (+ fill-width (nth col *column-widths*))))
			       (if (plusp col)
				   (push-absolute-indentation old-width)))
			     (while ((node (car cells) (cdr node)))
				    (node)
			       (output-string (car node)
					      ;; Either last node in row or
					      ;; fill.
					      (fi (cdr node) (cdr cells))))
			     (if (plusp col)
				 (pop-absolute-indentation)))
			   (terpri stream))
			 (setq fill-width (pop *fill-widths*)))
		       t))))
	    (rule
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let* ((props (css:style styles :hr ,(car args)))
			     (width (css:style-property props :width))
			     (margin-left (css:style-property props
							      :margin-left))
			     (margin-right (css:style-property props
							       :margin-right))
			     (portion (if width
					  (style-size-to-portion width)
					  1))
			     (margin (if (and (string= (string-upcase margin-left) "AUTO")
					      (string= (string-upcase margin-right) "AUTO"))
					 (* (/ (- 1 portion) 2) (- fill-width *indent*))
					 0)))
			,(open-line)
			(loop repeat margin do
			  (write-char #\space stream))
			(loop repeat (* portion (- fill-width *indent*)) do
			  (write-char #\- stream))
			(terpri stream)
			t)
		    `(let* ((props (css:style styles :hr (car args)))
			    (width (css:style-property props :width))
			    (margin-left (css:style-property props
							     :margin-left))
			    (margin-right (css:style-property props
							      :margin-right))
			    (portion (if width
					 (style-size-to-portion width)
					 1))
			    (margin (if (and (string= (string-upcase margin-left) "AUTO")
					     (string= (string-upcase margin-right) "AUTO"))
					(* (/ (- 1 portion) 2) (- fill-width *indent*))
					0)))
		       (open-line)
		       (loop repeat margin do
			 (write-char #\space stream))
		       (loop repeat (* portion (- fill-width *indent*)) do
			 (write-char #\- stream))
		       (terpri stream)
		       t))))
	    (ref
	     ,(if (eval emit)
		  '`(let ((link (let* ((sstream (make-string-output-stream))
				       (stream (lisp::make-indenting-stream sstream)))
				  ,(write-generic-nodes (gnode-content node))
				  (get-output-stream-string sstream))))
		      (write-string (if (string= (string-upcase (safe-subseq link 0 7))
						 "MAILTO:")
					(encode-email (subseq link 7))
					link)
				    stream))
		  '(let ((link (let* ((sstream (make-string-output-stream))
				      (stream (lisp::make-indenting-stream sstream)))
				 (write-generic-nodes (gnode-content node))
				 (get-output-stream-string sstream))))
		     (write-string (if (string= (string-upcase (safe-subseq link 0 7))
						"MAILTO:")
				       (encode-email (subseq link 7))
				       link)
				   stream))))
	    (string
	     ,(if (eval emit)
		  '(output-string
		    (substitute #\space #\newline
				(string-right-trim '(#\newline)
						   (gnode-single-content
						    node))))
		  '(output-string
		    (substitute #\space #\newline
				(string-right-trim '(#\newline)
						   (gnode-single-content
						    node))))))
	    (data
	     ,(if (eval emit)
		  '`(let ((result ,(gnode-single-content node)))
		      (if result
			  ,(output-string
			    '(substitute #\space #\newline
					 (string-right-trim '(#\newline)
							    result)))))
		  '(let ((result (eval (gnode-single-content node))))
		     (if result
			 (output-string
			  (substitute #\space #\newline
				      (string-right-trim '(#\newline)
							 result)))))))))
	(write-generic-nodes (nodes)
	  ,(if (eval emit)
	       '(collect ((out))
		  (while ((node nodes (gnode-next node)))
			 (node)
		    (out (write-generic-node node)))
		  `(progn ,@(out)))
	       '(let ((last))
		  (while ((node nodes (gnode-next node)))
			 (node)
		    (setq last (write-generic-node node)))
		  last))))
     ,@body))

(defmacro defun-doc-to-text (name nodes)
  "Define a function NAME that produces a plain text version of the
   document described by NODES."
  (translate-doc-to-text t
    `(defun ,name (&optional (stream *standard-output*)
			     styles
			     (fill-width 72)
			     (*indent* 0)
			     *indents*)
       "Write a particular text document on $stream according to $styles
	with $fill-width."
       (let ((stream (lisp::make-indenting-stream stream))
	     (*section-depth* 0) (*column-widths*) (*fill-widths*) (*first* t))
	 ,(write-generic-nodes (eval nodes))))))

(defun %doc-to-text (nodes &optional (stream *standard-output*)
			   styles
			   (fill-width 72)
			   (*indent* 0)
			   *indents*)
  "Write the document described by $nodes to $stream as plain text."
  ;; FIX stream may already be indenting
  (translate-doc-to-text () (write-generic-nodes nodes)))

(defun doc-to-text (nodes &optional (stream *standard-output*)
			  styles
			  (fill-width 72)
			  (*indent* 0)
			  *indents*)
  "Write the document described by $nodes to $stream as plain text."
  ;; FIX stream may already be indenting?
  (let ((stream (lisp::make-indenting-stream stream))
	(*section-depth* 0) (*column-widths*) (*fill-widths*) (*first* t))
    (%doc-to-text nodes stream styles fill-width *indent* *indents*)))


;;;; HTML.

;; FIX as in -to-text
(defmacro translate-doc-to-html (emit &body body)
  ;; FIX title, styles
  "If EMIT is true return code that will translate a document description
   (list of nodes) into the body of a function that will produce HTML for
   the documents described by that description.  Otherwise return the body
   of an interpreter that will produce an HTML document given a description
   (list of nodes)."
  `(labels
       ((open-line ()
	  ,(if (eval emit)
	       '`(if *first*
		     (setq *first* ())
		     (terpri stream))
	       '(if *first*
		    (setq *first* ())
		    (terpri stream))))
	(buffer-rows (rows)
	  "Return the table defined by ROWS, as a list of lists of
	   strings."
	  ,(if (eval emit)
	       '(collect ((out))
		  (flet
		      ((collect-row (row)
			 (collect ((row-out))
			   (while ((cell (gnode-content row)
					 (gnode-next cell))
				   (i 0 (1+ i)))
				  (cell)
			     ;; A cell is a list of nodes.
			     (while ((nodes)
				     (node (car cell) (cdr node)))
				    (node
				     (row-out
				      ;; Beware, generating a var called
				      ;; nodes too.
				      `(let* ((nodes (list ,@(nreverse
							      nodes)))
					      (max (nthcdr ,i
							   *column-widths*))
					      (width (+ (reduce #'+
								nodes
								:key
								#'length)
							3))) ; FIX *space-between-columns*
					 ;(format t "nodes ~A~%" nodes)
					 ;(format t "w ~A m ~A c ~A~%" width max *column-widths*)
					 (if max
					     (if (< (car max) width)
						 (setf (car max) width))
					     (setq *column-widths*
						   (append *column-widths*
							   (list width))))
					 nodes)))
			       (push
				`(let* ((ss (make-string-output-stream))
					(stream (lisp::make-indenting-stream
						 ss)))
				   ,(write-generic-node node)
				   (let* ((str (get-output-stream-string
						ss)))
				     str))
				nodes)))
			   (row-out))))
		    (loop for row = rows then (gnode-next row) while row do
		      (if (eq (intern (symbol-name (gnode-type row)) "DOC")
			      'many)
			  ;; FIX other many loops check cont and write-g-node return
			  (progn
			    (out `(catch 'end-many
				    (loop
				      (catch 'end-row
					(buffer-list (list ,@(collect-row
							      (gnode-content row)))))))))
			  (out `(catch 'end-row
				  (buffer-list (list ,@(collect-row row))))))))
		  `(progn
		     ;; FIX dynamic context would be neater
		     (collect ((buffer-list))
		       (progn ,@(out))
		       (buffer-list))))
	       '(collect ((out))
		  (while ((row rows (gnode-next row)))
			 (row)
		    (flet
			((collect-row (row)
			   (collect ((row-out))
			     (while ((cell (gnode-content row)
					   (gnode-next cell))
				     (i 0 (1+ i)))
				    (cell)
			       ;; A cell is a list of nodes.
			       (while ((nodes)
				       (node (car cell)
					     (cdr node)))
				      (node
				       (let* ((nodes
					       (nreverse nodes))
					      (max (nthcdr
						    i
						    *column-widths*))
					      (width (+ (reduce #'+ nodes
								:key #'length)
							3))) ; FIX *space-between-columns*
					 (if max
					     (if (< (car max) width)
						 (setf (car max) width))
					     (setq *column-widths*
						   (append
						    *column-widths*
						    (list width))))
					 (row-out nodes)))
				 ;; Simulate binding of `stream', as
				 ;; "call" to write-generic-node
				 ;; jumps out of binding.
				 (push
				  (let ((ss (make-string-output-stream))
					(old-stream stream))
				    (setq stream
					  (lisp::make-indenting-stream
					   ss))
				    (unwind-protect
					(write-generic-node node)
				      (setq stream old-stream))
				    (let* ((str (get-output-stream-string
						 ss)))
				      str))
				  nodes)))
			     (out (row-out)))))
		      ;; FIX note there can only be one many per row ((table ((many ((row..
		      (if (eq (intern (symbol-name (gnode-type row)) "DOC")
			      'many)
			  (catch 'end-many
			    (loop
			      (catch 'end-row
				(collect-row (gnode-content row)))))
			  (catch 'end-row (collect-row row)))))
		  (out))))
	(output-string (string)
	  ,(if (eval emit)
	       '`(progn
		   (write-string ,string stream)
		   t)
	       `(progn
		  (write-string string stream)
		  t)))
	(write-generic-node (node)
	  (ecase (intern (symbol-name (gnode-type node)) "DOC")
	    (many
	     (let ((args (gnode-args node)))
	       (declare (ignore args))
	       ,(if (eval emit)
		    '`(let ((cont t))
			(catch 'end-many
			  (loop while (and ,(write-generic-nodes (gnode-content node))
					   cont)
			    finally return cont)))
		    '(let ((cont t))
		       (catch 'end-many
			 (loop while (and (write-generic-nodes (gnode-content node))
					  cont)
			   finally return cont))))))
	    (interpret
	     ,(if (eval emit)
		  '`(%doc-to-html ,(car (gnode-content node))
				  stream title sheet *indent*)
		  '(%doc-to-html (eval (car (gnode-content node)))
				 stream title sheet *indent*)))
	    (list
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    `(collect ((out))
		       (let ((class (if (car args)
					(format () " CLASS=\"~A\"" (car args))
					"")))
			 (out `(format stream "<UL~A>" ,class))
			 (loop for item = (gnode-content node) then (gnode-next item) while item do
			   (out (open-line))
			   (out `(format stream "<LI~A>" ,class))
			   (out `(setq last ,(write-generic-node item)))
			   (out '(write-string "</LI>" stream))
			   (out '(fresh-line stream)))
			 (out '(write-string "</UL>" stream))
			 `(progn ,@(out) last)))
		    '(let ((last)
			   (class (if (car args)
				      (format () " CLASS=\"~A\"" (car args))
				      "")))
		       (format stream "<UL~A>" class)
		       (loop for item = (gnode-content node) then (gnode-next item) while item do
			 (open-line)
			 (format stream "<LI~A>" class)
			 (setq last (write-generic-node item))
			 (write-string "</LI>" stream)
			 (fresh-line stream))
		       (write-string "</UL>" stream)
		       last))))
	    (paragraph
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    ;; FIX buffer paragraph else terpri<LI> when
		    ;;     return-from-many
		    '`(let ((class (if ,(car args)
				       (format () " CLASS=\"~A\"" ,(car args))
				       "")))
			,(open-line)
			(format stream "<P~A>" class)
			(prog1 ,(write-generic-nodes (gnode-content node))
			  (write-string "</P>" stream)
			  (fresh-line stream)))
		    '(let ((class (if (car args)
				      (format () " CLASS=\"~A\"" (car args))
				      "")))
		       (open-line)
		       (format stream "<P~A>" class)
		       (prog1 (write-generic-nodes (gnode-content node))
			 (write-string "</P>" stream)
			 (fresh-line stream))))))
	    (verbatim
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    ;; FIX buffer paragraph else terpri<LI> when
		    ;;     return-from-many
		    '`(let ((class (if ,(car args)
				       (format () " CLASS=\"~A\"" ,(car args))
				       "")))
			,(open-line)
			(format stream "<PRE~A>" class)
			(prog1 ,(write-generic-nodes (gnode-content node))
			  (write-string "</PRE>" stream)
			  (fresh-line stream)))
		    '(let ((class (if (car args)
				      (format () " CLASS=\"~A\"" (car args))
				      "")))
		       (open-line)
		       (format stream "<PRE~A>" class)
		       (prog1 (write-generic-nodes (gnode-content node))
			 (write-string "</PRE>" stream)
			 (fresh-line stream))))))
	    (section
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(progn
			,(open-line)
			(format stream
				"~%<p>~
	                         <table CLASS=\"~A-heading\">~%~
			         <tr><td CLASS=\"~A-heading\">~A</td></tr>~%~
				 </table></p>~%"
				,(car args) ,(car args) ,(cadr args) ,(car args))
			,(write-generic-nodes (gnode-content node)))
		    '(progn
		       (open-line)
		       (terpri stream)
		       (let ((style (car args)))
			 (format stream
				 "~%<p>~
				  <table CLASS=\"~A-heading\">~%~
				  <tr><td CLASS=\"~A-heading\">~A</td></tr>~%~
				  </table></p>~%"
				 style style (cadr args) style))
		       (write-generic-nodes (gnode-content node))))))
	    (table
	     (let* ((rows (gnode-content node))
		    (args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let ((row-buffer (progn
					  (setq *column-widths* ())
					  ,(buffer-rows rows)))
			    (class (if ,(car args)
				       (format () " CLASS=\"~A\"" ,(car args))
				       "")))
			,(open-line)
			(format stream "<TABLE~A>" class)
			(while ((row row-buffer (cdr row)))
			       (row)
			  (format stream "<TR~A>" class)
			  (while ((cells (car row) (cdr cells)))
				 (cells)
			    (format stream "<TD~A>" class)
			    (while ((nodes (car cells) (cdr nodes)))
				   (nodes)
			      ,(output-string '(car nodes)))
			    (write-string "</TD>" stream))
			  (write-string "</TR>" stream)
			  (terpri stream))
			(format stream "</TABLE>~%")
			t)
		    '(let ((row-buffer (progn
					 (setq *column-widths* ())
					 (buffer-rows rows)))
			   (class (if (car args)
				      (format () " CLASS=\"~A\"" (car args))
				      "")))
		       (open-line)
		       (format stream "<TABLE~A>" class)
		       (while ((row row-buffer (cdr row)))
			      (row)
			 (format stream "<TR~A>" class)
			 (while ((cells (car row) (cdr cells)))
				(cells)
			   (format stream "<TD~A>" class)
			   (while ((nodes (car cells) (cdr nodes)))
				  (nodes)
			     (output-string (car nodes)))
			   (write-string "</TD>" stream))
			 (write-string "</TR>" stream)
			 (terpri stream))
		       (write-string "</TABLE>" stream)
		       t))))
	    (description
	     (let* ((rows (gnode-content node))
		    (args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let ((row-buffer (progn
					  (setq *column-widths* ())
					  ,(buffer-rows rows)))
			    (class (if ,(car args)
				       (format () " CLASS=\"~A\"" ,(car args))
				       "")))
			,(open-line)
			(format stream "<DL~A>" class)
			(while (row row-buffer (cdr row))
			       (row)
			  (while ((cells (car row) (cdr cells))
				  (col 0 (1+ col)))
				 (cells
				  (if (plusp col)
				      (write-string "</DD>" stream)))
			    (if (zerop col)
				(format stream "<DT~A>" class))
			    (while ((node (car cells) (cdr node)))
				   (node)
			      ,(output-string '(car node)))
			    (if (zerop col)
				(format stream "</DT>~%<DD>~%")))
			  (terpri stream))
			(format stream "</DL>~%")
			t)
		    '(let ((row-buffer (progn
					 (setq *column-widths* ())
					 (buffer-rows rows)))
			   (class (if (car args)
				      (format () " CLASS=\"~A\"" (car args))
				      "")))
		       (open-line)
		       (format stream "<DL~A>" class)
		       (while ((row row-buffer (cdr row)))
			       (row)
			 (while ((cells (car row) (cdr cells))
				 (col 0 (1+ col)))
				(cells
				 (if (plusp col)
				     (write-string "</DD>" stream)))
			   (if (zerop col)
			       (format stream "<DT~A>" class))
			   (while ((node (car cells) (cdr node))
				   (col 0 (1+ col)))
				  (node)
			     (output-string (car node)))
			   (if (zerop col)
			       (write-string "</DT><DD>" stream))))
		       (write-string "</DL>" stream)
		       t))))
	    (rule
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let ((class (if ,(car args)
				       (format () " CLASS=\"~A-rule\"" ,(car args))
				       ""))
			    (inner (if ,(car args)
				       (format () " CLASS=\"~A-rule-inner\"" ,(car args))
				       "")))
			,(open-line)
			(format stream "<TABLE ~A><TR><TD></TD></TR></TABLE>~%"
				class inner)
			t)
		    `(let ((class (if (car args)
				      (format () " CLASS=\"~A-rule\"" (car args))
				      ""))
			   (inner (if (car args)
				      (format () " CLASS=\"~A-rule-inner\"" (car args))
				      "")))
		       (open-line)
		       (format stream "<TABLE ~A><TR><TD></TD></TR></TABLE>~%"
			       class inner)
		       t))))
	    (ref
	     ,(if (eval emit)
		  '`(let ((link (let* ((sstream (make-string-output-stream))
				       (old-stream stream))
				  (setq stream (lisp::make-indenting-stream sstream))
				  (unwind-protect
				      ,(write-generic-nodes (gnode-content node))
				    (setq stream old-stream))
				  (get-output-stream-string sstream))))
		      (if (and (> (length link) 4)
			       (string= (string-upcase (subseq link 0 4)) "DOC:"))
			  (format stream "<A HREF=\"~A.html\">~A</A>" (subseq link 4) (subseq link 4))
			  (let ((addr (if (and (> (length link) 6)
					       (string= (string-upcase (subseq link 0 7))
							"MAILTO:"))
					  (encode-email (subseq link 7))
					  link)))
			    (format stream "<A HREF=\"~A\">~A</A>" link addr))))
		  '(let ((link (let* ((sstream (make-string-output-stream))
				      (old-stream stream))
				 (setq stream (lisp::make-indenting-stream sstream))
				 (unwind-protect
				     (write-generic-nodes (gnode-content node))
				   (setq stream old-stream))
				 (get-output-stream-string sstream))))
		      (if (and (> (length link) 4)
			       (string= (string-upcase (subseq link 0 4)) "DOC:"))
			  (format stream "<A HREF=\"~A.html\">~A</A>" (subseq link 4) (subseq link 4))
			  (let ((addr (if (and (> (length link) 6)
					       (string= (string-upcase (subseq link 0 7))
							"MAILTO:"))
					  (encode-email (subseq link 7))
					  link)))
			    (format stream "<A HREF=\"~A\">~A</A>" link addr))))))
	    (string
	     ,(if (eval emit)
		  '(output-string
		    ;(substitute #\space #\newline
				;(string-right-trim '(#\newline)
						   (or (gnode-single-content
							node)
						       ""));))
		  '(output-string
		    ;(substitute #\space #\newline
				;(string-right-trim '(#\newline)
		                                   (or (gnode-single-content
							node)
						       ""))));))
	    (data
	     ,(if (eval emit)
		  '`(let ((result ,(gnode-single-content node)))
		      (if result
			  ,(output-string
			    '(substitute #\space #\newline
					 (string-right-trim '(#\newline)
							    result)))))
		  '(let ((result (eval (gnode-single-content node))))
		     (if result
			 (output-string
			  (substitute #\space #\newline
				      (string-right-trim '(#\newline)
							 result)))))))))
	(write-generic-nodes (nodes)
	  ,(if (eval emit)
	       '(collect ((out))
		  (loop for node = nodes then (gnode-next node) while node do
		    (out (write-generic-node node)))
		  `(progn ,@(out)))
	       '(let ((last))
		  (loop for node = nodes then (gnode-next node) while node do
		    (setq last (write-generic-node node)))
		  last))))
     ,@body))

(defun begin-html (stream title sheet)
  "Write initial HTML to STREAM."
  (format stream
	  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%~
	   <HTML>~%~
	   <HEAD>~%")
  (if title (format stream "  <title>~A</title>~%" title))
  (if sheet
      (format stream
	      "  <link rel=\"stylesheet\" type=\"text/css\" href=\"~A\">~%"
	      sheet))
  (format stream "  </HEAD>~%<BODY>    <TABLE CLASS=\"BODY-TABLE\"><TR><TD>~%~%"))

(defmacro defun-doc-to-html (name nodes)
  "Define a function NAME that produces an HTML version of the document
   described by NODES."
  (translate-doc-to-html t
    `(defun ,name (&optional (stream *standard-output*)
			     title
			     sheet
			     (*indent* 0))
       "Write a particular HTML document on STREAM according to STYLES with
	FILL-WIDTH."
       (let ((stream (lisp::make-indenting-stream stream))
	     (*column-widths*) (*first* t)
	     *indents*)
	 (begin-html stream title sheet)
	 ,(write-generic-nodes (eval nodes))
	 (format stream "  </TD></TR></BODY>~%  </HTML>~%")))))

(defun %doc-to-html (nodes &optional (stream *standard-output*)
			   title
			   sheet
			   (*indent* 0)
			   *indents*)
  "Write the document described by NODES to STREAM as HTML."
  ;; FIX stream may already be indenting
  (translate-doc-to-html () (write-generic-nodes nodes)))

(defun doc-to-html (nodes &optional (stream *standard-output*)
			  title
			  sheet
			  (*indent* 0)
			  *indents*)
  "Write the document described by $nodes to $stream as HTML."
  ;; FIX stream may already be indenting?
  (let ((stream (lisp::make-indenting-stream stream))
	(*column-widths*) (*first* t))
    (begin-html stream title sheet)
    (%doc-to-html nodes stream title sheet *indent* *indents*)
    (format stream "  </TD></TR></BODY>~%  </HTML>~%")))


;;;; LaTeX.

;; FIX as in -to-text
(defmacro translate-doc-to-latex (emit &body body)
  ;; FIX title, styles (classes?)
  "If EMIT is true return code that will translate a document description
   (list of nodes) into the body of a function that will produce HTML for
   the documents described by that description.  Otherwise return the body
   of an interpreter that will produce a LaTeX document given a description
   (list of nodes)."
  `(labels
       ((begin-wrap-styles (type class)
	  ;; FIX maybe buffer the end forms for end-wrap-styles
	  ,(if (eval emit)
	       '`(let* ((props (css:style styles ,type ,class))
			(font-weight (css:style-property props :font-weight))
			(margin-left (css:style-property props :margin-left))
			(margin-right (css:style-property props :margin-right)))
		   ;; FIX precedence?
		   (if font-weight
		       (cond ((string= (string-upcase font-weight) "BOLD")
			      (format stream "\\textbf{~%"))))
		   (if font-weight
		       (cond ((string= (string-upcase font-weight) "bold")
			      (format stream "\\textbf{~%"))))
		   (if (or margin-left margin-right)
		       (format stream
			       "\\begin{adjustwidth}{\\textwidth * ~A}{\\textwidth * ~A}~%"
			       (style-size-to-portion margin-left)
			       (style-size-to-portion margin-right)))
		   (let ((width (css:style-property props :width)))
		     (if width
			 (format stream
				 "\\changetext{}{- \\textwidth * ~A}{}{}{}~%"
				 (- 1 (style-size-to-portion width)))))
		   (and (string= (string-upcase margin-left) "AUTO")
			margin-right
			(string= (string-upcase margin-right) "AUTO")
			(format stream "\\centering~%")))
	       '(let* ((props (css:style styles type class))
		       (font-weight (css:style-property props :font-weight))
		       (margin-left (css:style-property props :margin-left))
		       (margin-right (css:style-property props :margin-right)))
		  ;; FIX precedence?
		  (if font-weight
		      (cond ((string= (string-upcase font-weight) "BOLD")
			     (format stream "\\textbf{~%"))))
		  (if (or margin-left margin-right)
		      (format stream
			      "\\begin{adjustwidth}{\\textwidth * ~A}{\\textwidth * ~A}~%"
			      (style-size-to-portion margin-left)
			      (style-size-to-portion margin-right)))
		  (let ((width (css:style-property props :width)))
		    (if width
			(format stream
				"\\changetext{}{- \\textwidth * ~A}{}{}{}~%"
				(- 1 (style-size-to-portion width)))))
		  (and (string= (string-upcase margin-left) "AUTO")
		       margin-right
		       (string= (string-upcase margin-right) "AUTO")
		       (format stream "\\centering~%")))))
	(end-wrap-styles (type class)
	  ,(if (eval emit)
	       '`(let* ((props (css:style styles ,type ,class))
			(font-weight (css:style-property props :font-weight))
			(width (css:style-property props :width)))
		   (if width
		       (let ((ratio (style-size-to-portion width)))
			 (format stream
				 "\\changetext{}{\\textwidth * ~A / ~A - \\textwidth}{}{}{}~%"
				 (denominator ratio)
				 (numerator ratio))))
		   (if (css:style-property props :margin-left)
		       (format stream "\\end{adjustwidth}~%"))
		   (if font-weight
		       (cond ((string= (string-upcase font-weight) "BOLD")
			      (format stream "}~%")))))
	       '(let* ((props (css:style styles type class))
		       (font-weight (css:style-property props :font-weight))
		       (width (css:style-property props :width)))
		  (if width
		      (let ((ratio (style-size-to-portion width)))
			(format stream
				"\\changetext{}{\\textwidth * ~A / ~A - \\textwidth}{}{}{}~%"
				(denominator ratio)
				(numerator ratio))))
		  (if (css:style-property props :margin-left)
		      (format stream "\\end{adjustwidth}~%"))
		  (if font-weight
		      (cond ((string= (string-upcase font-weight) "BOLD")
			     (format stream "}~%")))))))
	(open-line ()
	  ,(if (eval emit)
	       '`(if *first*
		     (setq *first* ())
		     (terpri stream))
	       '(if *first*
		    (setq *first* ())
		    (terpri stream))))
	(buffer-rows (rows)
	  "Return the table defined by ROWS, as a list of lists of
	   strings."
	  ,(if (eval emit)
	       '(collect ((out))
		  (flet
		      ((collect-row (row)
			 (collect ((row-out))
			   (while ((cell (gnode-content row)
					 (gnode-next cell))
				   (i 0 (1+ i)))
				  (cell)
			     ;; A cell is a list of nodes.
			     (while ((nodes)
				     (node (car cell) (cdr node)))
				    (node
				     (row-out
				      ;; Beware, generating a var called
				      ;; nodes too.
				      `(let* ((nodes (list ,@(nreverse
							      nodes)))
					      (max (nthcdr ,i
							   *column-widths*))
					      (width (+ (reduce #'+
								nodes
								:key
								#'length)
							3))) ; FIX *space-between-columns*
					 ;(format t "nodes ~A~%" nodes)
					 ;(format t "w ~A m ~A c ~A~%" width max *column-widths*)
					 (if max
					     (if (< (car max) width)
						 (setf (car max) width))
					     (setq *column-widths*
						   (append *column-widths*
							   (list width))))
					 nodes)))
			       (push
				`(let* ((ss (make-string-output-stream))
					(stream (lisp::make-indenting-stream
						 ss)))
				   ,(write-generic-node node)
				   (let* ((str (get-output-stream-string
						ss)))
				     str))
				nodes)))
			   (row-out))))
		    (loop for row = rows then (gnode-next row) while row do
		      (if (eq (intern (symbol-name (gnode-type row)) "DOC")
			      'many)
			  ;; FIX other many loops check cont and write-g-node return
			  (progn
			    (out `(catch 'end-many
				    (loop
				      (catch 'end-row
					(buffer-list (list ,@(collect-row
							      (gnode-content row)))))))))
			  (out `(catch 'end-row
				  (buffer-list (list ,@(collect-row row))))))))
		  `(progn
		     ;; FIX dynamic context would be neater
		     (collect ((buffer-list))
		       (progn ,@(out))
		       (buffer-list))))
	       '(collect ((out))
		  (while ((row rows (gnode-next row)))
			 (row)
		    (flet
			((collect-row (row)
			   (collect ((row-out))
			     (while ((cell (gnode-content row)
					   (gnode-next cell))
				     (i 0 (1+ i)))
				    (cell)
			       ;; A cell is a list of nodes.
			       (while ((nodes)
				       (node (car cell)
					     (cdr node)))
				      (node
				       (let* ((nodes
					       (nreverse nodes))
					      (max (nthcdr
						    i
						    *column-widths*))
					      (width (+ (reduce #'+ nodes
								:key #'length)
							3))) ; FIX *space-between-columns*
					 (if max
					     (if (< (car max) width)
						 (setf (car max) width))
					     (setq *column-widths*
						   (append
						    *column-widths*
						    (list width))))
					 (row-out nodes)))
				 ;; Simulate binding of `stream', as
				 ;; "call" to write-generic-node
				 ;; jumps out of binding.
				 (push
				  (let ((ss (make-string-output-stream))
					(old-stream stream))
				    (setq stream
					  (lisp::make-indenting-stream
					   ss))
				    (unwind-protect
					(write-generic-node node)
				      (setq stream old-stream))
				    (let* ((str (get-output-stream-string
						 ss)))
				      str))
				  nodes)))
			     (out (row-out)))))
		      ;; FIX note there can only be one many per row ((table ((many ((row..
		      (if (eq (intern (symbol-name (gnode-type row)) "DOC")
			      'many)
			  (catch 'end-many
			    (loop
			      (catch 'end-row
				(collect-row (gnode-content row)))))
			  (catch 'end-row (collect-row row)))))
		  (out))))
	(output-string (string)
	  ,(if (eval emit)
	       '`(progn
		   (write-string ,string stream)
		   t)
	       `(progn
		  (write-string string stream)
		  t)))
	(write-generic-node (node)
	  (ecase (intern (symbol-name (gnode-type node)) "DOC")
	    (many
	     (let ((args (gnode-args node)))
	       (declare (ignore args))
	       ,(if (eval emit)
		    '`(let ((cont t))
			(catch 'end-many
			  (while ()
				 ((and ,(write-generic-nodes
					 (gnode-content node))
				       cont)
				  cont))))
		    '(let ((cont t))
		       (catch 'end-many
			 (while ()
				((and (write-generic-nodes
				       (gnode-content node))
				      cont)
				 cont)))))))
	    (interpret
	     ,(if (eval emit)
		  '`(%doc-to-latex ,(car (gnode-content node))
				   stream styles *indent*)
		  '(%doc-to-latex (eval (car (gnode-content node)))
				  stream styles *indent*)))
	    (list
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    `(collect ((out))
		       (out `(if ,(car args)
				 ,(begin-wrap-styles :ul (car args))))
		       (out '(format stream "\\begin{itemize}"))
		       (loop for item = (gnode-content node) then (gnode-next item) while item do
			 (out (open-line))
			 (out '(write-string "\\item " stream))
			 (out `(setq last ,(write-generic-node item)))
			 (out '(fresh-line stream)))
		       (out '(format stream "\\end{itemize}~%"))
		       (out `(if ,(car args)
				 ,(end-wrap-styles :ul (car args))))
		       `(let ((last)) ,@(out) last))
		    '(let ((last))
		       (if (car args)
			   (begin-wrap-styles :ul (car args)))
		       (format stream "\\begin{itemize}")
		       (loop for item = (gnode-content node) then (gnode-next item) while item do
			 (open-line)
			 (write-string "\\item " stream)
			 (setq last (write-generic-node item))
			 (fresh-line stream))
		       (format stream "\\end{itemize}~%")
		       (if (car args)
			   (end-wrap-styles :ul (car args)))
		       last))))
	    (paragraph
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    ;; FIX buffer paragraph else may output when
		    ;;     return-from-many
		    '`(progn
			,(open-line)
			(if ,(car args)
			    ,(begin-wrap-styles :p (car args)))
			(prog1 ,(write-generic-nodes (gnode-content node))
			  (fresh-line stream)
			  (if ,(car args)
			      ,(end-wrap-styles :p (car args)))))
		    '(progn
		       (open-line)
		       (if (car args)
			   (begin-wrap-styles :p (car args)))
		       (prog1 (write-generic-nodes (gnode-content node))
			 (fresh-line stream)
			 (if (car args)
			     (end-wrap-styles :p (car args))))))))
	    (verbatim
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    ;; FIX buffer paragraph else may output when
		    ;;     return-from-many
		    (let ((*verbatim* t))
		      '`(progn
			  ,(open-line)
			  (if ,(car args)
			      ,(begin-wrap-styles :verbatim (car args)))
			  (format stream "\\begin{verbatim}~%")
			  (prog1 ,(write-generic-nodes (gnode-content node))
			    (fresh-line stream)
			    (format stream "\\end{verbatim}~%")
			    (if ,(car args)
				,(end-wrap-styles :verbatim (car args))))))
		    '(let ((*verbatim* t))
		       (open-line)
		       (if (car args)
			   (begin-wrap-styles :verbatim (car args)))
		       (format stream "\\begin{verbatim}~%")
		       (prog1 (write-generic-nodes (gnode-content node))
			 (fresh-line stream)
			 (format stream "\\end{verbatim}~%")
			 (if (car args)
			     (end-wrap-styles :verbatim (car args))))))))
	    (section
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(progn
			,(open-line)
			(incf *section-depth*)
			(unwind-protect
			    (case *section-depth*
			      (1
			       (format stream "\\part*{~A}" ,(cadr args))
			       ,(write-generic-nodes (gnode-content node)))
			      (2
			       (format stream "\\section*{~A}" ,(cadr args))
			       ,(write-generic-nodes (gnode-content node)))
			      (3
			       (format stream "\\subsection*{~A}" ,(cadr args))
			       ,(write-generic-nodes (gnode-content node)))
			      (4
			       (format stream "\\subsubsection*{~A}" ,(cadr args))
			       ,(write-generic-nodes (gnode-content node)))
			      (t
			       (format stream "~A~%" ,(cadr args))
			       ,(write-generic-nodes (gnode-content node))))
			  (decf *section-depth*)))
		    '(progn
		       (open-line)
		       (terpri stream)
		       (incf *section-depth*)
		       (unwind-protect
			   (case *section-depth*
			     (1
			      (format stream "\\part*{~A}" (cadr args))
			      (write-generic-nodes (gnode-content node)))
			     (2
			      (format stream "\\section*{~A}" (cadr args))
			      (write-generic-nodes (gnode-content node)))
			     (3
			      (format stream "\\subsection*{~A}" (cadr args))
			      (write-generic-nodes (gnode-content node)))
			     (4
			      (format stream "\\subsubsection*{~A}" (cadr args))
			      (write-generic-nodes (gnode-content node)))
			     (t
			      (format stream "~A~%" (cadr args))
			      (write-generic-nodes (gnode-content node))))
			 (decf *section-depth*))))))
	    (table
	     (let* ((rows (gnode-content node))
		    (args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let ((row-buffer (progn
					  (setq *column-widths* ())
					  ,(buffer-rows rows))))
			(when row-buffer
			  ,(open-line)
			  (if ,(car args)
			      ,(begin-wrap-styles :table (car args)))
			  (if ,(car args)
			      (let* ((props (css:style styles :table ,(car args)))
				     (margin-left (css:style-property props :margin-left))
				     (margin-right (css:style-property props :margin-right)))
				(if (or margin-left margin-right)
				    (let ((margin-left (style-size-to-portion margin-left))
					  (margin-right (style-size-to-portion margin-right)))
				      (format stream
					      "\\begin{tabularx}{\\textwidth * ~A}{@{\\extracolsep{\\fill}}"
					      (- 1 (+ margin-left margin-right))))
				    (format stream
					    "\\begin{tabularx}{\\textwidth}{@{\\extracolsep{\\fill}}")))
			      (format stream
				      "\\begin{tabularx}{\\textwidth}{@{\\extracolsep{\\fill}}"))
			  (loop repeat (1- (apply #'longest-length row-buffer)) do
			    (write-char #\l stream))
			  (write-char #\X stream)
			  (format stream "}~%")
			  (while ((row row-buffer (cdr row)))
				 (row)
			    (while ((cells (car row) (cdr cells))
				    ;(col 0 (1+ col))
				    )
				   (cells)
			      (while ((nodes (car cells) (cdr nodes)))
				     (nodes)
				,(output-string '(car nodes)))
			      (if (cdr cells) (write-string "&" stream)))
			    (format stream "\\\\~%"))
			  (format stream "\\end{tabularx}~%")
			  (if ,(car args)
			      ,(end-wrap-styles :table (car args))))
			t)
		    '(let ((row-buffer (progn
					 (setq *column-widths* ())
					 (buffer-rows rows))))
		       (when row-buffer
			 (open-line)
 			 (if (car args)
			     (begin-wrap-styles :table (car args)))
			 (if (car args)
			     (let* ((props (css:style styles :table (car args)))
				    (margin-left (css:style-property props :margin-left))
				    (margin-right (css:style-property props :margin-right)))
			       (if (or margin-left margin-right)
				   (let ((margin-left (style-size-to-portion margin-left))
					 (margin-right (style-size-to-portion margin-right)))
				     (format stream
					     "\\begin{tabularx}{\\textwidth * ~A}{@{\\extracolsep{\\fill}}"
					     (- 1 (+ margin-left margin-right))))
				   (format stream
					   "\\begin{tabularx}{\\textwidth}{@{\\extracolsep{\\fill}}")))
			     (format stream
				     "\\begin{tabularx}{\\textwidth}{@{\\extracolsep{\\fill}}"))
			 (loop repeat (1- (apply #'longest-length row-buffer)) do
			   (write-char #\l stream))
			 (write-char #\X stream)
			 (format stream "}~%")
			 (while ((row row-buffer (cdr row)))
				(row)
			   (while ((cells (car row) (cdr cells))
				   ;(col 0 (1+ col))
				   )
				  (cells)
			     (while ((nodes (car cells) (cdr nodes)))
				    (nodes)
			       (output-string (car nodes)))
			     (if (cdr cells) (write-string "&" stream)))
			   (format stream "\\\\~%"))
			 (format stream "\\end{tabularx}~%")
			 (if (car args)
			     (end-wrap-styles :table (car args))))
		       t))))
	    (description
	     (let* ((rows (gnode-content node))
		    (args (gnode-args node)))
	       ,(if (eval emit)
		    '`(let ((row-buffer (progn
					  (setq *column-widths* ())
					  ,(buffer-rows rows))))
			(when row-buffer
			  ,(open-line)
			  (if ,(car args)
			      ,(begin-wrap-styles :table (car args)))
			  (format stream
				  "\\begin{description}~%")
			  (while ((row row-buffer (cdr row)))
				 (row)
			    (when (car row)
			      (format stream "\\item [")
			      (while ((nodes (caar row) (cdr nodes))
				      ;(col 0)
				      )
				     (nodes)
				,(output-string '(car nodes)))
			      (format stream "] "))
			    (while ((cells (cdar row) (cdr cells))
				    ;(col 0 (1+ col))
				    )
				   (cells)
			      (while ((nodes (car cells) (cdr nodes)))
				     (nodes)
				,(output-string '(car nodes)))))
			  (format stream "\\end{description}~%")
			  (if ,(car args)
			      ,(end-wrap-styles :table (car args))))
			t)
		    '(let ((row-buffer (progn
					 (setq *column-widths* ())
					 (buffer-rows rows))))
		       (when row-buffer
			 (open-line)
 			 (if (car args)
			     (begin-wrap-styles :table (car args)))
			 (format stream "\\begin{description}~%")
			 (while ((row row-buffer (cdr row)))
				(row)
			    (when (car row)
			      (format stream "\\item [")
			      (while ((nodes (caar row) (cdr nodes))
				      ;(col 0)
				      )
				     (nodes)
				(output-string (car nodes)))
			      (format stream "] "))
			   (while ((cells (cdar row) (cdr cells))
				   ;(col 0 (1+ col))
				   )
				  (cells)
			     (while ((nodes (car cells) (cdr nodes)))
				    (nodes)
			       (output-string (car nodes)))))
			 (format stream "\\end{description}~%")
			 (if (car args)
			     (end-wrap-styles :table (car args))))
		       t))))
	    (rule
	     (let ((args (gnode-args node)))
	       ,(if (eval emit)
		    '`(progn
			,(open-line)
			(if ,(car args)
			    ,(begin-wrap-styles :hr (car args)))
			(let* ((props (css:style styles :hr ,(car args)))
			       (prop (css:style-property props :width)))
			  (if prop
			      (format stream "\\rule{\\textwidth}{0.1pt}~%")
			      (format stream "\\hrulefill~%")))
			(if ,(car args)
			    ,(end-wrap-styles :hr (car args)))
			t)
		    `(progn
		       (open-line)
		       (if (car args)
			   (begin-wrap-styles :hr (car args)))
		       (let* ((props (css:style styles :hr (car args)))
			      (prop (css:style-property props :width)))
			 (if prop
			     (format stream "\\rule{\\textwidth}{0.1pt}~%")
			     (format stream "\\hrulefill~%")))
		       (if (car args)
			   (end-wrap-styles :hr (car args)))
		       t))))
	    (ref
	     ,(if (eval emit)
		  '`(let ((link (let* ((sstream (make-string-output-stream))
				       (stream (lisp::make-indenting-stream sstream)))
				  ,(write-generic-nodes (gnode-content node))
				  (get-output-stream-string sstream))))
		      (format stream "\\url{~A}"
;			      link
			      (if (string= (string-upcase (safe-subseq link 0 7))
					   "MAILTO:")
				  (encode-email (safe-subseq link 7))
				  link)))
		  ;; Simulate binding of `stream', as "call" to
		  ;; write-generic-node jumps out of binding.
		  '(let ((link (let* ((sstream (make-string-output-stream))
				      (old-stream stream))
				 (setq stream (lisp::make-indenting-stream sstream))
				 (unwind-protect
				     (write-generic-nodes (gnode-content node))
				   (setq stream old-stream))
				 (get-output-stream-string sstream))))
		     (format stream "\\url{~A}"
;			     link
			     (if (string= (string-upcase (safe-subseq link 0 7))
					  "MAILTO:")
				 (encode-email (safe-subseq link 7))
				 link)))))
#|
	    (string
	     ,(if (eval emit)
		  '(output-string
		    (substitute #\space #\newline
				(string-right-trim '(#\newline)
						   (gnode-single-content
						    node))))
		  '(output-string
		    (substitute #\space #\newline
				(string-right-trim '(#\newline)
						   (gnode-single-content
						    node))))))
|#
	    (string
	     ,(if (eval emit)
		  (if *verbatim*
		      '(output-string (gnode-single-content node))
		      '(output-string
			(substitute #\space #\newline
				    (gnode-single-content node))))
		  '(if *verbatim*
		       (output-string (gnode-single-content node))
		       (output-string
			(substitute #\space #\newline
				    (gnode-single-content node))))))
	    (data
	     ,(if (eval emit)
		  '`(let ((result ,(gnode-single-content node)))
		      (if result
			  ,(output-string
			    '(substitute #\space #\newline
					 (string-right-trim '(#\newline)
							    result)))))
		  '(let ((result (eval (gnode-single-content node))))
		     (if result
			 (output-string
			  (substitute #\space #\newline
				      (string-right-trim '(#\newline)
							 result)))))))))
	(write-generic-nodes (nodes)
	  ,(if (eval emit)
	       '(collect ((out))
		  (while ((node nodes (gnode-next node)))
			 (node)
		    (out (write-generic-node node)))
		  `(progn ,@(out)))
	       '(let ((last))
		  (while ((node nodes (gnode-next node)))
			 (node)
		    (setq last (write-generic-node node)))
		  last))))
     ,@body))

(defun begin-latex (stream)
  "Write initial LaTeX to STREAM."
  (format stream
	  "\\documentclass{article}
\\pagestyle{empty}

%\\usepackage{color}
\\usepackage{tabularx}
\\usepackage{geometry}
\\usepackage{comment}
\\usepackage{titlesec}
\\usepackage{chngpage}
\\usepackage{calc}
\\usepackage{url}

%\\geometry{verbose,a4paper,tmargin=5mm}
\\geometry{verbose,a4paper,tmargin=24mm,bottom=24mm}
%\\geometry{verbose,a4paper}
%\\geometry{verbose,a4paper,tmargin=14mm,bottom=14mm}
%\\geometry{verbose,a5paper,tmargin=5mm}
\\setlength{\\parskip}{\\smallskipamount}
\\setlength{\\parindent}{0pt}

\\titleformat{\\part}{\\normalsize}{\\thepart.}{}{\\bfseries}[\\titlerule]
\\titleformat{\\section}{\\normalsize}{\\thesection.}{}{\\bfseries}

% nice, but causes 0 width html columns from latex2html
%\\newlength{\\datecollen}
%\\setlength{\\datecollen}{48mm}

\\begin{document}~%"))

(defmacro defun-doc-to-latex (name nodes)
  "Define a function NAME that produces a LaTeX version of the document
   described by NODES."
  (translate-doc-to-latex t
    `(defun ,name (&optional (stream *standard-output*)
			     styles
			     (*indent* 0))
       "Write a particular LaTeX document on STREAM according to STYLES
	with FILL-WIDTH."
       (let ((stream (lisp::make-indenting-stream stream))
	     (*column-widths*) (*first* t) (*section-depth* 0)
	     *indents*)
	 (begin-latex stream)
	 ,(write-generic-nodes (eval nodes))
	 (format stream "\\end{document}~%")))))

(defun %doc-to-latex (nodes &optional (stream *standard-output*)
			    styles
			    (*indent* 0)
			    *indents*)
  "Write the document described by $nodes to $stream as LaTeX."
  ;; FIX stream may already be indenting
  (translate-doc-to-latex () (write-generic-nodes nodes)))

(defun doc-to-latex (nodes &optional (stream *standard-output*)
			   styles
			   (*indent* 0)
			   *indents*)
  "Write the document described by $nodes to $stream as LaTeX."
  ;; FIX stream may already be indenting?
  (let ((stream (lisp::make-indenting-stream stream))
	(*column-widths*) (*first* t) (*section-depth* 0))
    (begin-latex stream)
    (%doc-to-latex nodes stream styles *indent* *indents*)
    (format stream "\\end{document}~%")))


;;; FIX odt, docbook, man, texinfo, svg, xml, ontology?, vym
;;;     will be much quicker if interp,compile case is combined
