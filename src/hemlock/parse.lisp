;;; -*- Log: hemlock.log; Package: Hemlock; Mode: Editor -*-
;;;
;;; Parser generator.

(in-package "HEMLOCK")


;;;; Interface definitions.

(declaim (special *required-functions* *defined-functions*
		  *stream* *streams*
		  *mark* *marks*
		  *fold-case-p*))

(defmacro defparser (rules &key bufferp (eval t))
  "Define a stream or buffer parser.

   Rules is a list of BNF-style rules, named by the keyword in the car of
   the rule.  The first rule is the root rule.  A parser is produced for
   the root rule and for any rule required by the root rule (recursively).

   If Eval is true then the parser functions are evaluated and the last one
   is returned, otherwise a list of the function definitions is returned.

   If Bufferp is true the parser will parse from the mark in *mark* and
   the generated functions are named buffer-parse-<rule>, otherwise the
   functions read from the stream in *stream* and the are named like
   parse-<rule>.  Each parse function returns a node containing the parsed
   information.

   Here is an example parser

      (defparser '((:paragraph    (many (or :line #\\newline))
                   (:line         (group (many :alphanumeric)))))

   which produces functions parse-paragraph and parse-line.

   The cdr of a rule lists the requirements of the rule.  Requirements can
   be characters, strings, keywords naming rules, or operations on
   requirements.  An operation is written in the style

       (or #\\a (many :char) \"bc\").

   The available operations are

     - many, m   parse the requirements once and then as many times as
                 possible, as in the rule

                    (:line          (many :alphanumeric) #\\newline)

     - any, a    parse the requirements as many times as possible, as in the
                 rule

                    (:ab          (any #\a) #\b)

                 which matches \"aaab\" and \"cb\"

     - or, o     parse any one of the requirements, trying them in the given
                 order, as in the rule

                    (:paragraph     (many (or :line #\\newline)))

     - group, g  group the result of parsing the requirements into a single
                 node, the contents of which is a region containing the
                 parse, as in the rule

                    (:line          (group (many :alphanumeric)) #\\newline)

     - cond      parse by testing each character with the Lisp expression
                 given in the requirement with variable `ch' bound to the
                 parsed character during the test, as in the rule

                    (:alphanumeric  (cond (alphanumericp ch)))

     - fold      parse the requirements with case folding (upper and lower
                 case of a character are considered equal), as in the rule

                    (:folded        (fold \"abc\"))

     - after     after parsing the single requirement argument evaluate the
                 second argument as a Lisp expression with the result of
                 the parse bound to variables `head' and `tail' (the head
                 and tail of the chain of parsed nodes), as in the rule

		    (:line-part     (after
			              :include
			              (when head
                                         (switch-to-stream head)
				         (values head tail)))).

   The rules :char and :c are predefined to match any character, and the
   rule :alphanumeric is predefined to match any alphabet or number
   character.

   A rule can reference rules defined in other parsers, so care may
   be required when choosing rule names.  An \"epsilon\" rule, which
   matches the empty string, can be created with

       (:epsilon   \"\")."
  (if eval
      (cons 'progn (list-parser (eval rules) :bufferp bufferp))
      `',(cons 'progn (list-parser (eval rules) :bufferp bufferp))))

(defun list-parser (rules &key bufferp)
  "Return the definitions of the parser defined by Rules, as described in
   the Defparser documentation."
  (let ((rules rules))
    (when rules
      (let ((*required-functions* nil)
	    (*defined-functions* nil)
	    (*fold-case-p* nil))
	(generate-parse-function-name (caar rules) nil bufferp)
	(list-parse-functions rules :bufferp bufferp)))))

(defun parse (buffer-parser)
  (let* ((*mark* (copy-mark (current-point)))
	 (*marks* `((,*mark*))))
    ; FIX perhaps after parse kill buffers assoc w marks
    ;	  (first-mark *marks*))
    (prog1 (funcall buffer-parser)
      (delete-mark *mark*))))

(defun define-parser (def &optional (compile t))
  "Return a symbol for the parser defined by Def, creating the parser if
   required.  Def can be a defparser style parser rule list, a single such
   rule or a symbol for a parser."
  (if (consp def)
      (let* ((parser-def-p (and (consp (car def)) (consp (caar def))))
	     (name (if parser-def-p
		       (caaar def)
		       (lisp::make-keyword (gensym "parser")))))
	(eval (cons 'progn
		    (ed::list-parser (if parser-def-p
					 (car def)
					 ;; Def is a single rule.
					 (list (cons name def)))
				     :bufferp t)))
	(let ((symbol (read-from-string (format nil
						"buffer-parse-~A"
						name))))
	  (if compile (compile symbol))
	  symbol))
      def))


;;;; Structure predefined for the parser.

(defstruct (node (:constructor make-node
			       (&key content next previous parent)))
  "A node."
  content
  next
  previous
  parent)

(defstruct (char-node (:constructor make-char-node
				    (&key content next previous parent))
		      (:include node))
  "A char node.")

(defstruct (region-node (:constructor make-region-node
				      (&key content next previous parent))
			(:include node))
  "A region node.")


;;;; Helper functions.

(defun generate-parser-variable-name (name)
  "Return a symbol for the function list variable for parser Name."
  (read-from-string (format nil "parse-~A-functions" name)))

(defun generate-constructor-name (name)
  "Return a symbol for the structure for rule Name."
  (read-from-string (format nil "make-~A-node" name)))

(defun generate-parse-function-name (name &optional groupp bufferp)
  "Return a symbol of the function associated with the rule named Name.
   Push the two names and the content flag onto *required-functions* once
   ever, so that the function can be generated later."
  (let ((defined (assoc name *defined-functions*)))
    (if (and defined (eq (caddr defined) groupp))
	(cadr defined)
	(let ((required (assoc name *required-functions*)))
	  (if (and required (eq (caddr required) groupp))
	      (cadr required)
	      (let ((function-name
		     (read-from-string
		      (format nil
			      "~:[~;group-~]~:[~;buffer-~]~:[~;f~]parse-~A"
			      groupp bufferp *fold-case-p* name))))
		(setq *required-functions*
		      (append *required-functions*
			      (list (list name function-name groupp *fold-case-p*))))
		function-name))))))


;;;; Functions that generate code.

(defun list-many-parsing (arg-parsing-list &key groupp bufferp)
  "List the code that will parse according to a \"many\" requirement.
   Arg-parsing-list is code to parse a single unit of the \"many\" form."
  ;; FIX flet for single version of arg-parsing-list?
  (if groupp
      `(when (block nil ,arg-parsing-list)
	 (loop
	   (let ((old-positions ,(if bufferp
				     '(copy-marks *marks*)
				     '(copy-streams *streams*)))
		 (positions ,(if bufferp '*marks* '*streams*)))
	     (or (when (block nil ,arg-parsing-list)
		   ,(if bufferp '(free-marks old-positions) nil)
		   t)
		 (progn
		   ,(if bufferp
			'(progn
			   (recover-marks positions old-positions)
			   (free-marks old-positions))
			'(recover-streams positions old-positions))
		   (return-from nil t))))))
      `(multiple-value-bind (head previous)
			    (block nil ,arg-parsing-list)
	 (when head
	   (loop
	     (let ((old-positions ,(if bufferp
				       '(copy-marks *marks*)
				       '(copy-streams *streams*)))
		   (positions ,(if bufferp '*marks* '*streams*)))
	       (multiple-value-bind (parse-head parse-tail)
				    (block nil ,arg-parsing-list)
		 (if parse-head
		     (progn
		       (setf (node-next previous) parse-head)
		       (setq previous parse-tail)
		       ,(if bufferp '(free-marks old-positions) nil))
		     (progn
		       ,(if bufferp
			    '(progn
			       (recover-marks positions old-positions)
			       (free-marks old-positions))
			    '(recover-streams positions old-positions))
		       (return-from nil (values head previous)))))))))))

(defun list-group-parsing (reqs &key groupp bufferp)
  "List code that will parse and group Reqs, a list of requirements."
  (if groupp
      `(block nil ,(list-req-parsing reqs nil :groupp t :bufferp bufferp))
      `(block nil 
	 ;; FIX for bufferp perhaps use existing region (ie ~ just rem
	 ;;     start and end marks)
	 (let* ((region (make-empty-region))
		(mark (region-end region)))
	   ,(list-req-parsing reqs nil :groupp t :bufferp bufferp)
	   (do ((line (mark-line (region-start region))
		      (line-next line))
		(start (region-start region))
		(end (region-end region)))
	       ((eq line nil))
	     (let ((marks (hi::line-marks line)))
	       (when marks
		 (dolist (m marks)
		   (or (eq m start)
		       (eq m end)
		       (delete-mark m))))))
	   (let ((head (make-region-node :next next
					 :previous previous
					 :parent parent
					 :content region)))
	     (when previous
	       (setf (node-next previous) head))
	     (values head head))))))

(defun list-or-parsing-rest (reqs &key groupp bufferp)
  "List code that will parse Reqs, the rest of an \"or\" requirement list."
  `(progn
     ,(if bufferp
	  '(recover-marks positions old-positions)
	  '(recover-streams positions old-positions))
     (multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    :groupp groupp
						    :bufferp bufferp)
       (if head
	   (progn
	     ,(if bufferp '(free-marks old-positions))
	     (values head tail))
	   ,(if (cdr reqs)
		(list-or-parsing-rest (cdr reqs)
				      :groupp groupp :bufferp bufferp)
		(if bufferp '(progn (free-marks old-positions) nil)))))))

(defun list-or-parsing (reqs &key groupp bufferp)
  "List code that will parse the \"or\" requirements Reqs."
  `(let ((old-positions ,(if bufferp
			     '(copy-marks *marks*)
			     '(copy-streams *streams*)))
	 (positions ,(if bufferp '*marks* '*streams*)))
     (multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    :groupp groupp
						    :bufferp bufferp)
       (if head
	   (progn
	     ,(if bufferp '(free-marks old-positions))
	     (values head tail))
	   ,(list-or-parsing-rest (if (cdr reqs) (cdr reqs) '(""))
				  :groupp groupp :bufferp bufferp)))))

(defun list-cond-parsing (form &key groupp bufferp)
  "List code that will parse the \"cond\" requirement Form."
  `(let ((ch ,(if bufferp
		  '(buffer-parser-read-char)
		  '(parser-read-char))))
     (when ch
       (if ,form
	   ,(if groupp
		'(progn (insert-character mark ch) ch)
		'(let ((node (make-char-node :content ch
					     :next next
					     :previous previous
					     :parent parent)))
		   (when previous
		     (setf (node-next previous) node))
		   (values node node)))))))

(defun list-after-parsing (req-and-code &key groupp bufferp)
  "List code that will parse the \"after\" requirement in Req-and-code."
  `(multiple-value-bind (head tail)
			,(list-single-req-parsing (car req-and-code)
						  :bufferp bufferp
						  :groupp groupp)
     ,@(cdr req-and-code)))

(defun list-single-req-parsing (req &key groupp bufferp)
  "List code that will parse according the rule requirement Req."
  (flet ((list-char-parsing (req)
	   (if groupp
	       (if bufferp
		   (if *fold-case-p*
		       `(primitive-group-buffer-fparse-char ,req mark)
		       `(primitive-group-buffer-parse-char ,req mark))
		   (if *fold-case-p*
		       `(primitive-group-fparse-char ,req mark)
		       `(primitive-group-parse-char ,req mark)))
	       (if bufferp
		   (if *fold-case-p*
		       `(primitive-buffer-fparse-char ,req nil previous parent)
		       `(primitive-buffer-parse-char ,req nil previous parent))
		   (if *fold-case-p*
		       `(primitive-fparse-char ,req nil previous parent)
		       `(primitive-parse-char ,req nil previous parent)))))
	 (list-string-parsing (req)
	   (if groupp
	       (if bufferp
		   (if *fold-case-p*
		       `(primitive-group-buffer-fparse-string ,req mark)
		       `(primitive-group-buffer-parse-string ,req mark))
		   (if *fold-case-p*
		       `(primitive-group-fparse-string ,req mark)
		       `(primitive-group-parse-string ,req mark)))
	       (if bufferp
		   (if *fold-case-p*
		       `(primitive-buffer-fparse-string ,req nil previous parent)
		       `(primitive-buffer-parse-string ,req nil previous parent))
		   (if *fold-case-p*
		       `(primitive-fparse-string ,req nil previous parent)
		       `(primitive-parse-string ,req nil previous parent))))))
    (etypecase req
      (keyword
       (if groupp
	   `(,(generate-parse-function-name req t bufferp) region mark)
	   `(,(generate-parse-function-name req nil bufferp) nil previous parent)))
      (string
       (list-string-parsing req))
      (base-char
       (list-char-parsing req))
      (symbol
       (setq req (symbol-name req))
       (if (eq (length req) 1)
	   (list-char-parsing (aref req 0))
	   (list-string-parsing req)))
      (cons
       (or (cdr req)
	   (error "Requirement operation must have at least one argument."))
       (let ((op (string-downcase (symbol-name (car req)))))
	 (cond
	  ((or (string= op "many") (string= op "m"))
	   (list-many-parsing (list-req-parsing (cdr req) nil
						:groupp groupp :bufferp bufferp)
			      :groupp groupp :bufferp bufferp))
	  
	  ((or (string= op "any") (string= op "a"))
	   (list-or-parsing `((many ,@(cdr req)))
			    :groupp groupp :bufferp bufferp))
	  
	  ((or (string= op "or") (string= op "o"))
	   (list-or-parsing (cdr req) :groupp groupp :bufferp bufferp))
	  
	  ((or (string= op "group") (string= op "g"))
	   (list-group-parsing (cdr req) :groupp groupp :bufferp bufferp))
	  
	  ((string= op "cond")
	   (list-cond-parsing (cadr req) :groupp groupp :bufferp bufferp))
	  
	  ((string= op "after")
	   (list-after-parsing (cdr req) :groupp groupp :bufferp bufferp))
	  
	  ((or (string= op "fold") (string= op "f"))
	   (let ((*fold-case-p* t))
	     `(block nil
		,(list-req-parsing (cdr req) nil
				   :groupp groupp :bufferp bufferp))))
	  
	  (t
	   (editor-error "Fell through operation cond."))))))))

(defun list-req-parsing-rest (reqs &optional block &key bufferp)
  "List code that will parse Reqs, the rest of the requirements of a rule.
   Block is the symbol from which to return if parsing fails."
  `(multiple-value-bind (head previous)
			,(list-single-req-parsing (car reqs)
						  :bufferp bufferp)
     (or head (return-from ,block))
     ,(if (cdr reqs)
	(list-req-parsing-rest (cdr reqs) block :bufferp bufferp)
	'previous)))

(defun list-req-parsing (reqs &optional block &key groupp bufferp)
  "List code that will parse Reqs, the requirements of a rule.  Block is
   the symbol from which to return if parsing fails."
  (if groupp
      `(let ((branch-mark (copy-mark mark :right-inserting)))
	 (or ,(cons 'and
		    (mapcar (lambda (req)
			      (list-single-req-parsing req
						       :groupp t
						       :bufferp bufferp))
			    reqs))
	     (progn
	       ;; FIX comment: Reset region bounds in case branch.
	       (set-region-bounds region
				  (region-start region)
				  (copy-mark branch-mark :left-inserting))
	       (move-mark mark branch-mark)
	       (delete-mark branch-mark)
	       (return-from ,block))))
      `(multiple-value-bind (head previous)
			    ,(list-single-req-parsing (car reqs)
						      :bufferp bufferp)
	 (or head (return-from ,block))
	 (values head
		 ,(if (cdr reqs)
		      (list-req-parsing-rest (cdr reqs) block
					     :bufferp bufferp)
		      'previous)))))

(defun list-group-parse-function (name rule &key bufferp)
  "Return a group parsing function definition that will parse Rule.  A
   group parsing function inserts the parsed text at a given mark."
  (or (cdr rule)
      (error "Each rule must have at least one requirement."))
  `(defun ,name (region mark)
     ,(format nil
	      "Parse from ~:[*stream*~;*mark*~] into Mark according to the ~A rule."
	      bufferp (symbol-name name))
     ,(list-req-parsing (cdr rule) name :groupp t :bufferp bufferp)))

(defun list-parse-function (name rule &key bufferp)
  "Return a parsing function definition according to Rule."
  (or (cdr rule)
      (error "Each rule must have at least one requirement."))
  `(defun ,name (&optional next previous parent)
     ,(format nil
	      "Return a node parsed from ~:[*stream*~;*mark*~] according to the ~A rule."
	      bufferp (symbol-name name))
     ;; FIX May be faster to create the node later on, or the compiler may
     ;; already do it.  Quite hard to do, possibly splitting the
     ;; requirement processing into two parts.
     (let ((parent (,(generate-constructor-name (car rule))
		    :next next
		    :previous previous
		    :parent parent)))
       ,(list 'setf '(node-content parent) 
	      (list 'let '((previous nil))
		    (list-req-parsing (cdr rule) name :bufferp bufferp)))
       (when previous
	 (setf (node-next previous) parent))
       (values parent parent))))

(defun list-parse-structure (name)
  "Return the definition of the node structure named Name."
  `(defstruct (,(read-from-string (format nil "~A-node" name))
	       (:constructor ,(generate-constructor-name name)
			     (&key content next previous parent))
	       (:include node))
     ,(format nil "A ~A node." name)))

(defun list-parse-functions (rules &key bufferp)
  "Return the list of definitions of the parsing functions named in
   *required-functions* and any parsing functions called in those
   functions, recursively."
  (when *required-functions*
    (let ((descr (pop *required-functions*)))
      (let ((rule (assoc (car descr) rules)))
	(push descr *defined-functions*)
	(if rule
	    (cons (list-parse-structure (car descr)) ;; FIX may be output twice
		  (cons (let ((*fold-case-p* (cadddr descr)))
			  (if (caddr descr)
			      (list-group-parse-function (cadr descr)
							 rule
							 :bufferp bufferp)
			      (list-parse-function (cadr descr) rule
						   :bufferp bufferp)))
			(list-parse-functions rules :bufferp bufferp)))
	    (list-parse-functions rules :bufferp bufferp))))))


;;;; Function predefined for the parser.

(defun care-push (ele list)
  "Push Ele onto List by inserting a copy of List head after head and
   replacing the head car with Ele."
  (setf (cdr list) (cons (car list) (cdr list)))
  (setf (car list) ele)
  list)

(defun care-pop (list)
  "Pop List by replacing the head with cdr of the head."
  (if (cdr list)
      (progn
	(setf (car list) (cadr list))
	(setf (cdr list) (cddr list)))
      (editor-error "Attempt to care-pop single cons.")))

(defun copy-marks (marks)
  "Return a copy of the list Marks, updating the marks in the cdrs of the
   list elements to copies of the cars."
  (mapcar (lambda (l) (list (car l) (copy-mark (car l)))) marks))

(defun free-marks (marks)
  "Calls delete-mark on the marks in the cdrs of Marks."
  (mapcar (lambda (l) (delete-mark (cadr l))) marks))

(defun recover-marks (marks branch-marks)
  "Recover *marks* and *mark* after a failed \"many\" or \"or\" branch.
   *marks* is recovered to be like Branch-marks.  Marks is the original
   (list) position of Branch-marks in the list *marks*.  Branch-marks is a
   copy of *marks* from that position made at the branch, with a copy of
   the mark in the car of each element in the element cdr."
  (let ((current (caar *marks*)))
    (do* ((marks marks (cdr marks))
	  (m (caar marks) (caar marks)))
	 ((eq marks nil)
	  (error "Reached end of marks."))
      (let ((branch-pair (assoc m branch-marks)))
	(if branch-pair
	    (move-mark m (cadr branch-pair))
	    (progn
	      (care-pop marks)
	      (delete-buffer (line-buffer (mark-line m)))
	      ;; (delete-mark m) FIX
	      )))
      (when (eq m current)
	(setq *marks* marks)
	(setq *mark* (caar *marks*))
	(return-from nil)))))

(defun copy-streams (streams)
  "Return a copy of the (stream position) list Streams, updating the
   positions in the copy."
  (mapcar (lambda (l) (list (car l) (file-position (car l))))
	  streams))

(defun recover-streams (streams old-streams)
  "Recover *streams* and *stream* after a failed \"many\" or \"or\" branch.
   *streams* should be like Old-streams.  Streams is the original (list)
   position of Old-streams in the list *streams*.  Old-streams is a copy
   with the original file positions."
  (let ((current (caar *streams*)))
    (do* ((streams streams (cdr streams))
	  (s (caar streams) (caar streams)))
	 ((eq streams nil)
	  (error "Reached end of streams."))
      (let ((old-pair (assoc s old-streams)))
	(if old-pair
	    (file-position s (cadr old-pair))
	    (progn
	      (care-pop streams)
	      (close s))))
      (when (eq s current)
	(setq *streams* streams)
	(setq *stream* (caar *streams*))
	(return-from nil)))))

(defun buffer-parser-read-char ()
  "Return the next character at *mark*, moving mark forward one character.
   If at the end of the buffer then pop a mark from *marks* into *mark*,
   and retry.  If at the end of then buffer then switch to the next mark in
   *marks*, and retry."
  (if (mark-after *mark*)
      (previous-character *mark*)
      (progn
	(when (cdr *marks*)
	  (setq *marks* (cdr *marks*))
	  (setq *mark* (car *marks*))
	  (buffer-parser-read-char)))))

(defun parser-read-char ()
  "Read and return the next character in *stream*.  If the end of stream
   has already been reached then pop a stream from *streams* into *stream*,
   and retry."
  (let ((ch (read-char *stream* nil :eof)))
    (if (eq ch :eof)
	(progn
	  (when (cdr *streams*)
	    (setq *streams* (cdr *streams*))
	    (setq *stream* (caar *streams*))
	    (parser-read-char)))
	ch)))

(declaim (inline buffer-parser-read-and-compare-char))

(defun buffer-parser-read-and-compare-char (char)
  "Read the next character at *mark*, moving mark forward one character.
   Return t if the character is Char, else nil.  If at the end of the
   buffer then switch to the next mark in *marks*, and retry."
  (if (mark-after *mark*)
      (eq (previous-character *mark*) char)
      (progn
	(when (cdr *marks*)
	  (setq *marks* (cdr *marks*))
	  (setq *mark* (car *marks*))
	  (buffer-parser-read-and-compare-char-1 char)))))

(defun buffer-parser-read-and-compare-char-1 (char)
  "Recursive buffer-parser-read-and-compare-char."
  (if (mark-after *mark*)
      (eq (previous-character *mark*) char)
      (progn
	(when (cdr *marks*)
	  (setq *marks* (cdr *marks*))
	  (setq *mark* (car *marks*))
	  (buffer-parser-read-and-compare-char-1 char)))))

(declaim (inline buffer-parser-read-and-fcompare-char))

(defun buffer-parser-read-and-fcompare-char (char)
  "Read the next character at *mark*, moving mark forward one character.
   Return t if the character is upper or lower case Char, else nil.  If at
   the end of the buffer then switch to the next mark in *marks*, and
   retry."
  (if (mark-after *mark*)
      (eq (char-upcase (previous-character *mark*)) (char-upcase char))
      (progn
	(when (cdr *marks*)
	  (setq *marks* (cdr *marks*))
	  (setq *mark* (car *marks*))
	  (buffer-parser-read-and-fcompare-char-1 char)))))

(defun buffer-parser-read-and-fcompare-char-1 (char)
  "Recursive buffer-parser-read-and-compare-char."
  (if (mark-after *mark*)
      (eq (char-upcase (previous-character *mark*)) (char-upcase char))
      (progn
	(when (cdr *marks*)
	  (setq *marks* (cdr *marks*))
	  (setq *mark* (car *marks*))
	  (buffer-parser-read-and-fcompare-char-1 char)))))

(declaim (inline parser-read-and-compare-char))

(defun parser-read-and-compare-char (char)
  "Read the next character in *stream*, returning t if it is Char else
   false.  If the end of stream has already been reached then pop a stream
   from *streams* into *stream*, and retry."
  (let ((ch (read-char *stream* nil :eof)))
    (or (eq ch char)
	(when (and (eq ch :eof)
		   (cdr *streams*))
	  (setq *streams* (cdr *streams*))
	  (setq *stream* (caar *streams*))
	  (parser-read-and-compare-char-1 char)))))

(defun parser-read-and-compare-char-1 (char)
  "Recursive parser-read-and-compare-char-1."
  (let ((ch (read-char *stream* nil :eof)))
    (or (eq ch char)
	(when (and (eq ch :eof)
		   (cdr *streams*))
	  (setq *streams* (cdr *streams*))
	  (setq *stream* (caar *streams*))
	  (parser-read-and-compare-char-1 char)))))

(declaim (inline parser-read-and-fcompare-char))

(defun parser-read-and-fcompare-char (char)
  "Read the next character in *stream*, returning t if it is upper or lower
   case Char else false.  If the end of stream has already been reached
   then pop a stream from *streams* into *stream*, and retry."
  (let ((ch (read-char *stream* nil nil)))
    (if ch
	(eq (char-upcase ch) (char-upcase char))
	(when (cdr *streams*)
	  (setq *streams* (cdr *streams*))
	  (setq *stream* (caar *streams*))
	  (parser-read-and-fcompare-char-1 char)))))

(defun parser-read-and-fcompare-char-1 (char)
  "Recursive parser-read-and-compare-char-1."
  (let ((ch (read-char *stream* nil nil)))
    (if ch
	(eq (char-upcase ch) (char-upcase char))
	(when (cdr *streams*)
	  (setq *streams* (cdr *streams*))
	  (setq *stream* (caar *streams*))
	  (parser-read-and-fcompare-char-1 char)))))

(defun buffer-parse-alphanumeric (&optional next previous parent)
  "Read next character at *mark*.  If an alphabet or numeric character is
   read return two values both the same node of the character, otherwise
   return nil."
  (let ((ch (buffer-parser-read-char)))
    (if (and ch (alphanumericp ch))
	(let ((node (make-char-node :content ch
				    :next next
				    :previous previous
				    :parent parent)))
	  (when previous
	    (setf (node-next previous) node))
	  (values node node)))))

(defun parse-alphanumeric (&optional next previous parent)
  "Read next character in *stream*.  If an alphabet or numeric character is
   read return two values both the same node of the character, otherwise
   return nil."
  (let ((ch (parser-read-char)))
    (if (and ch (alphanumericp ch))
	(let ((node (make-char-node :content ch
				    :next next
				    :previous previous
				    :parent parent)))
	  (when previous
	    (setf (node-next previous) node))
	  (values node node)))))

(defun group-buffer-parse-alphanumeric (region mark)
  "Read next character at *mark*.  If an alphabet or numeric character is
   read insert the character at Mark and return the character, otherwise
   return nil."
  (declare (ignore region))
  (let ((ch (buffer-parser-read-char)))
    (when (and ch (alphanumericp ch))
      (insert-character mark ch)
      ch)))

(defun group-parse-alphanumeric (region mark)
  "Read next character in *stream*.  If an alphabet or numeric character is
   read insert the character at Mark and return the character, otherwise
   return nil."
  (declare (ignore region))
  (let ((ch (parser-read-char)))
    (when (and ch (alphanumericp ch))
      (insert-character mark ch)
      ch)))

(defun buffer-parse-char (&optional next previous parent)
  "Read next character at *mark*.  If the read succeeds return two values
   both the same node of the read character, otherwise return nil."
  (let ((char (buffer-parser-read-char)))
    (when char
      (let ((node (make-char-node :content char
				  :next next
				  :previous previous
				  :parent parent)))
	(values node node)))))

(defun parse-char (&optional next previous parent)
  "Read next character in *stream*.  If the read succeeds return two values
   both the same node of the read character, otherwise return nil."
  (let ((char (parser-read-char)))
    (when char
      (let ((node (make-char-node :content char
				  :next next
				  :previous previous
				  :parent parent)))
	(values node node)))))

(declaim (inline parse-c buffer-parse-c))

(defun parse-c (&optional next previous parent)
  "Call `parse-char' on the arguments."
  (parse-char next previous parent))

(defun buffer-parse-c (&optional next previous parent)
  "Call `buffer-parse-char' on the arguments."
  (buffer-parse-char next previous parent))

(defun buffer-group-parse-char (region mark)
  "Read next character at *mark*.  If the read succeeds insert the
   character at Mark and return the character, otherwise return nil."
  (declare (ignore region))
  (let ((ch (buffer-parser-read-char)))
    (when ch
      (insert-character mark ch)
      ch)))

(defun group-parse-char (region mark)
  "Read next character in *stream*.  If the read succeeds insert the
   character at Mark and return the character, otherwise return nil."
  (declare (ignore region))
  (let ((ch (parser-read-char)))
    (when ch
      (insert-character mark ch)
      ch)))

(defun primitive-buffer-parse-char (char &optional next previous parent)
  "Read next character at *mark*.  Return a node of Char if Char was read,
   otherwise return nil."
  (when (buffer-parser-read-and-compare-char char)
    (let ((node (make-char-node :content char
				:next next
				:previous previous
				:parent parent)))
      (when previous
	(setf (node-next previous) node))
      (values node node))))

(defun primitive-buffer-fparse-char (char &optional next previous parent)
  "Read next character at *mark*.  Return a node of Char if upper or lower
   case Char was read, otherwise return nil."
  (when (buffer-parser-read-and-fcompare-char char)
    (let ((node (make-char-node :content char
				:next next
				:previous previous
				:parent parent)))
      (when previous
	(setf (node-next previous) node))
      (values node node))))

(defun primitive-parse-char (char &optional next previous parent)
  "Read next character in *stream*.  Return a node of Char if Char was
   read, otherwise return nil."
  (when (parser-read-and-compare-char char)
    (let ((node (make-char-node :content char
				:next next
				:previous previous
				:parent parent)))
      (when previous
	(setf (node-next previous) node))
      (values node node))))

(defun primitive-fparse-char (char &optional next previous parent)
  "Read next character in *stream*.  Return a node of Char if upper or
   lower case Char was read, otherwise return nil."
  (when (parser-read-and-fcompare-char char)
    (let ((node (make-char-node :content char
				:next next
				:previous previous
				:parent parent)))
      (when previous
	(setf (node-next previous) node))
      (values node node))))

(defun primitive-group-buffer-parse-char (char mark)
  "Read next character at *mark*.  Insert Char at Mark and return Char if
   Char was read, otherwise return nil."
  (when (buffer-parser-read-and-compare-char char)
    (insert-character mark char)
    char))

(defun primitive-group-buffer-fparse-char (char mark)
  "Read next character at *mark*.  Insert Char at Mark and return Char if
   upper or lower case Char was read, otherwise return nil."
  (when (buffer-parser-read-and-fcompare-char char)
    (insert-character mark char)
    char))

(defun primitive-group-parse-char (char mark)
  "Read next character in *stream*.  Insert Char at Mark and return Char if
   Char was read, otherwise return nil."
  (when (parser-read-and-compare-char char)
    (insert-character mark char)
    char))

(defun primitive-group-fparse-char (char mark)
  "Read next character in *stream*.  Insert Char at Mark and return Char if
   upper or lower case Char was read, otherwise return nil."
  (when (parser-read-and-fcompare-char char)
    (insert-character mark char)
    char))

(defun primitive-buffer-parse-string (string &optional next previous parent)
  "Read String from *mark*.  Return a node of the parsed string if
   successful, else return nil."
  (dotimes (pos (length string))
    (or (buffer-parser-read-and-compare-char (char string pos))
	(return-from primitive-buffer-parse-string nil)))
  (let ((node (make-region-node :content string
				:next next
				:previous previous
				:parent parent)))
    (when previous
      (setf (node-next previous) node))
    (values node node)))

#|  
(defun primitive-buffer-parse-string (string &optional next previous parent)
  "Read String from *mark*.  Return a node of the parsed string if
   successful, else return nil."
  (dotimes (pos (length string))
; FIX how much does check for *marks* cost?
;    (or (buffer-parser-read-and-compare-char (char string pos))
    (mark-after *mark*)
    (or (eq (next-character *mark*) (char string pos))
	(return-from primitive-buffer-parse-string nil)))
  (let ((node (make-region-node :content string
				:next next
				:previous previous
				:parent parent)))
    (when previous
      (setf (node-next previous) node))
    (values node node)))
|#

(defun primitive-buffer-fparse-string (string &optional next previous parent)
  "Read String from *mark* with case folding.  Return a node of String if
   successful, else return nil."
  (let ((mark *mark*))
    (dotimes (pos (length string))
      (or (buffer-parser-read-and-fcompare-char (char string pos))
	  (return-from primitive-buffer-fparse-string nil)))
    (let ((node (make-region-node :content (region-to-string
					    (region mark *mark*))
				  :next next
				  :previous previous
				  :parent parent)))
      (when previous
	(setf (node-next previous) node))
      (values node node))))
  
(defun primitive-parse-string (string &optional next previous parent)
  "Read String from *stream*.  Return a node of String if successful, else
   return nil."
  (dotimes (pos (length string))
    (or (parser-read-and-compare-char (char string pos))
	(return-from primitive-parse-string nil)))
  (let ((node (make-region-node :content string
				:next next
				:previous previous
				:parent parent)))
    (when previous
      (setf (node-next previous) node))
    (values node node)))
  
(defun primitive-fparse-string (string &optional next previous parent)
  "Read String from *stream* with case folding.  Return a node of the read
   string if successful, else return nil."
  (let* ((len (length string))
	 (result (make-string len)))
    (dotimes (pos len)
      (let ((ch (parser-read-char)))
	(or (and ch (eq (char-upcase ch) (char-upcase (char string pos))))
	    (return-from primitive-fparse-string nil))
	(setf (char result pos) ch)))
    (let ((node (make-region-node :content result
				  :next next
				  :previous previous
				  :parent parent)))
      (when previous
	(setf (node-next previous) node))
      (values node node))))
  
(defun primitive-group-buffer-parse-string (string mark)
  "Read String from *mark*.  If successful insert string at Mark and
   return String, otherwise return nil."
  (dotimes (pos (length string))
    (or (buffer-parser-read-and-compare-char (char string pos))
	(return-from primitive-group-buffer-parse-string nil)))
  (insert-string mark string)
  string)

(defun primitive-group-buffer-fparse-string (string mark)
  "Read String from *mark* with case folding.  If successful insert string
   at Mark and return String, otherwise return nil."
  (let ((start *mark*))
    (dotimes (pos (length string))
      (or (buffer-parser-read-and-compare-char (char string pos))
	  (return-from primitive-group-buffer-fparse-string nil)))
    (let ((result (region-to-string (region start *mark*))))
      (insert-string mark result)
      result)))

(defun primitive-group-parse-string (string mark)
  "Read String from *stream*.  If successful insert String at Mark and
   return String, otherwise return nil."
  (dotimes (pos (length string))
    (or (parser-read-and-compare-char (char string pos))
	(return-from primitive-group-parse-string nil)))
  (insert-string mark string)
  string)

(defun primitive-group-fparse-string (string mark)
  "Read String from *stream* with case folding.  If successful insert the
   read string at Mark and return the read string, otherwise return nil."
  (let* ((len (length string))
	 (result (make-string len)))
    (dotimes (pos len)
      (let ((ch (parser-read-char)))
	(or (and ch (eq (char-upcase ch) (char-upcase (char string pos))))

	    (return-from primitive-group-fparse-string nil))
	(setf (char result pos) ch)))
    (insert-string mark result)
    result))
