;;; Attempt to combine node interpreter compiler and direct doc-generator
;;; function compiler for doc:doc-to-* implementation.  Strongest is note
;;; in final `emit'.

(defun gnode-type (node)
  "Return the type of NODE."
  (caar node))

(defun gnode-next (node)
  "Return the node following NODE."
  (cdr node))

(defmacro translate-doc-to-text (emit &body body)
  "If EMIT is true return code that will translate a document description
   (list of nodes) into the body of a function that will produce plain text
   for the documents described by that description.  Otherwise return the
   body of an interpreter that will produce a document in plain text given
   a description (list of nodes)."
  `(labels
       ((write-generic-nodes (nodes)
	  ,(if (eval emit)
	       '(collect ((out))
		  (loop for node = nodes then (gnode-next node) while node do
		    (out (write-generic-node node)))
		  `(progn ,@(out)))
	       '(let ((last))
		  (loop for node = nodes then (gnode-next node) while node do
		    (setq last (write-generic-node node)))
		  last)))
	(write-generic-node (node)
	  ,(if (eval emit)
	       '`(format stream "~A"
			 ,(symbol-name (gnode-type node)) "DOC")
	       '(format stream "~A"
			(symbol-name (gnode-type node)) "DOC"))))
     ,@body))

(defmacro emit (emit form)
  "Emit FORM."
  (if (eval emit)
      `,form
      form))

(defmacro bq (form)
  `',form)

(defmacro bq (form)
  ``',,form)

(defmacro emit (emit form)
  "Emit FORM."
  `(if ,emit
       ',(eval form)
       ',form))

#|
(emit t  `(format () "~A" ,(progn 1)))
(emit () `(format () "~A" ,(progn 1)))

(emit t (format () "~A" (progn 1)))
(emit () (format () "~A" (progn 1)))
|#



(defmacro emit (emit form)
  "Emit FORM."
  `(if ,emit
       (read-from-string (concat "`" (write-to-string ',form)))
       ',form))

#|
(emit t (bq (f (comma (progn 1)))))

(emit t (f (comma (progn stream))))

(read-from-string (concat "`" (write-to-string `(format ,(progn stream)))))
(read-from-string (write-to-string '(format stream)))
(read-from-string "`(format ,(progn stream))")
(read-from-string (concat "`" "(format ,(progn stream)))"))
(read-from-string (concat "`" "(format ,(progn stream)))"))

(read-from-string (concat "`" (write-to-string '(format ,(progn stream)))))
|#

(defmacro emit (emit form)
  "Emit FORM."
  `(if ,emit
       ;; Maybe (eval ,form) so that it can be passed in as
       ;;     `(... ,(maybe-comma xxx) ...)
       ;; where maybe-comma can return ,xxx if emit else xxx
       ;; and change something to alow bare ,xxx forms to be
       ;; passed around...
       (backquote ,form)
       ',form))



(defmacro translate-doc-to-text (emit &body body)
  "If EMIT is true return code that will translate a document description
   (list of nodes) into the body of a function that will produce plain text
   for the documents described by that description i.e. compile a program
   that will produce the defined document.  Otherwise return the body of an
   interpreter that will produce a document in plain text given a
   description (list of nodes) i.e. compile a doc definition to text
   document translator."
  `(labels
       ((write-generic-nodes (nodes)
	  ,(if (eval emit)
	       '(collect ((out))
		  (loop for node = nodes then (gnode-next node) while node do
		    (out (write-generic-node node)))
		  `(progn ,@(out)))
	       '(let ((last))
		  (loop for node = nodes then (gnode-next node) while node do
		    (setq last (write-generic-node node)))
		  last)))
	(write-generic-node (node)
	  ,(emit emit (format stream "~A"
			      ,(stall (intern (symbol-name (gnode-type node))
					      "DOC"))))))
; 	  ,(if (eval emit)
; 	       '`(format stream "~A"
; 			 ,(symbol-name (gnode-type node)) "DOC")
; 	       '(format stream "~A"
; 			(symbol-name (gnode-type node)) "DOC"))))
     ,@body))

(defun doc-to-text (nodes &optional (stream *standard-output*))
  "Write the document described by NODES to STREAM as plain text."
  (translate-doc-to-text () (write-generic-nodes nodes)))

(defmacro defun-doc-to-text (name nodes)
  "Define a function NAME that produces a plain text version of the
   document described by NODES."
  (translate-doc-to-text t
    `(defun ,name (&optional (stream *standard-output*))
       ,(write-generic-nodes (eval nodes)))))

(defun-doc-to-text x-to-x '((string . "aaa")))

(ed::with-output-to-mark (stream (ed::current-point))
   (x-to-x stream))

(ed::with-output-to-mark (stream (ed::current-point))
   (doc-to-text '((string . "aaa")) stream))
