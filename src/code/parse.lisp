;;; BNF-style parser generator.

(in-package "PARSE")

(export '(ch
	  char-node-content char-node-next char-node-previous
	  char-node-parent
	  defparser
	  group-parse-alphanumeric group-parse-char
	  list-parser
	  make-node make-char-node make-region-node
	  node char-node region-node
	  node-content node-next node-previous node-parent
	  parser-read-char
	  parse-alphanumeric parse-char parse-c
	  primitive-group-parse-char primitive-group-fparse-char
	  primitive-parse-char primitive-fparse-char
	  primitive-parse-string primitive-fparse-string
	  region-node-content region-node-next region-node-next
	  region-node-previous region-node-parent
	  *mark* *marks* *stream* *streams*))


;;;; Interface definitions.

(declaim (special *required-functions* *defined-functions*
		  *stream* *streams*
		  *mark* *marks*
		  *fold-case-p*))

;;; Public
;;;
(defun list-parser (rules &key bufferp)
  "Return the definitions of the parser described by Rules, as detailed in
   the `defparser' documentation."
  (let ((rules rules))
    (when rules
      (let ((*required-functions* nil)
	    (*defined-functions* nil)
	    (*fold-case-p* nil))
	(generate-parse-function-name (caar rules) nil bufferp)
	(list-parse-functions rules :bufferp bufferp)))))

;;; Public
;;;
(defmacro defparser (rules &key bufferp (eval t))
  "Define a stream or buffer parser.

   Rules is a list of BNF-style rules, each named by the keyword in the car
   of the rule.  The first rule is the root rule.  A parser is produced for
   the root rule and for any rule required by the root rule (recursively).

   If Eval is true then the parser functions are evaluated and the last one
   is returned, otherwise a list of the function definitions is returned.

   If Bufferp is true the parser will parse from the mark in *mark* and
   the generated functions are named buffer-parse-<rule>, otherwise the
   functions read from the stream in *stream* and are named like
   parse-<rule>.  Each parse function returns a node containing the parsed
   information.

   Here is an example parser

      (defparser '((:paragraph    (many (or :line #\\newline))
                   (:line         (group (many :alphanumeric)))))

   which produces functions parse-paragraph and parse-line, which would be
   called like

      (let* ((*stream* *standard-input*)
             (*streams* (list (list *stream*))))
        (parse-paragraph)).

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

                 which matches \"aaab\" and \"b\"

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
      (cons 'progn (parse:list-parser (eval rules) :bufferp bufferp))
      `',(cons 'progn (parse:list-parser (eval rules) :bufferp bufferp))))

;;; Public
;;;
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
		    (parse:list-parser (if parser-def-p
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


;;;; Public structures predefined for the parser.

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
  "A region node.  In stream parsers the content is an adjustable array
   with a fill pointer.  In buffer parsers the content is an editor
   region.")


;;;; Helper functions.

(defun generate-parser-variable-name (name)
  "Return a symbol for the function list variable for parser Name."
  (read-from-string (format () "parse-~A-functions" name)))

(defun generate-constructor-name (name)
  "Return a symbol for the structure for rule Name."
  (read-from-string (format () "make-~A-node" name)))

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

;; FIX replace parse:: w parse:

(defun list-many-parsing (arg-parsing-list &key groupp bufferp)
  "List the code that will parse according to a \"many\" requirement.
   Arg-parsing-list is code to parse a single unit of the \"many\" form."
  ;; FIX flet for single version of arg-parsing-list?
  (if groupp
      `(when (block nil ,arg-parsing-list)
	 (loop
	   (let ((old-positions ,(if bufferp
				     '(ed::copy-marks parse::*marks*)
				     '(parse::copy-streams parse::*streams*)))
		 (positions ,(if bufferp 'parse::*marks* 'parse::*streams*)))
	     (or (when (block nil ,arg-parsing-list)
		   ,(if bufferp '(ed::free-marks old-positions) nil)
		   t)
		 (progn
		   ,(if bufferp
			'(progn
			   (ed::recover-marks positions old-positions)
			   (ed::free-marks old-positions))
			'(parse::recover-streams positions old-positions))
		   (return-from nil t))))))
      `(multiple-value-bind (head previous)
			    (block nil ,arg-parsing-list)
	 (when head
	   (loop
	     (let ((old-positions ,(if bufferp
				       '(ed::copy-marks parse::*marks*)
				       '(parse::copy-streams parse::*streams*)))
		   (positions ,(if bufferp 'parse::*marks* 'parse::*streams*)))
	       (multiple-value-bind (parse-head parse-tail)
				    (block nil ,arg-parsing-list)
		 (if parse-head
		     (progn
		       (setf (node-next previous) parse-head)
		       (setq previous parse-tail)
		       ,(if bufferp '(ed::free-marks old-positions) nil))
		     (progn
		       ,(if bufferp
			    '(progn
			       (ed::recover-marks positions old-positions)
			       (ed::free-marks old-positions))
			    '(parse::recover-streams positions old-positions))
		       (return-from nil (values head previous)))))))))))

(defun list-group-parsing (reqs &key groupp bufferp)
  "List code that will parse and group Reqs, a list of requirements."
  (if groupp
      `(block nil ,(list-req-parsing reqs nil :groupp t :bufferp bufferp))
      `(block nil
	 (let ((region (make-array 0
				   :element-type 'base-char
				   :adjustable t
				   :fill-pointer t)))
	   ,(list-req-parsing reqs nil :groupp t :bufferp bufferp)
	   (let ((head (make-region-node :next next
					 :previous previous
					 :parent parent
					 :content ,(if bufferp
						       '(ed::call-string-to-region region)
						       'region))))
	     (if previous
		 (setf (node-next previous) head))
	     (values head head))))))

(defun list-or-parsing-rest (reqs &key groupp bufferp)
  "List code that will parse Reqs, the rest of an \"or\" requirement list."
  `(progn
     ,(if bufferp
	  '(ed::recover-marks positions old-positions)
	  '(parse::recover-streams positions old-positions))
     (multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    :groupp groupp
						    :bufferp bufferp)
       (if head
	   (progn
	     ,(if bufferp '(ed::free-marks old-positions))
	     (values head tail))
	   ,(if (cdr reqs)
		(list-or-parsing-rest (cdr reqs)
				      :groupp groupp :bufferp bufferp)
		(if bufferp '(progn (ed::free-marks old-positions) nil)))))))

(defun list-or-parsing (reqs &key groupp bufferp)
  "List code that will parse the \"or\" requirements Reqs."
  `(let ((old-positions ,(if bufferp
			     '(ed::copy-marks parse::*marks*)
			     '(parse::copy-streams parse::*streams*)))
	 (positions ,(if bufferp 'parse::*marks* 'parse::*streams*)))
     (multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    :groupp groupp
						    :bufferp bufferp)
       (if head
	   (progn
	     ,(if bufferp '(ed::free-marks old-positions))
	     (values head tail))
	   ,(list-or-parsing-rest (if (cdr reqs) (cdr reqs) '(""))
				  :groupp groupp :bufferp bufferp)))))

(defun list-cond-parsing (form &key groupp bufferp)
  "List code that will parse the \"cond\" requirement Form."
  `(let ((ch ,(if bufferp
		  '(ed::buffer-parser-read-char)
		  '(parse::parser-read-char))))
     (when ch
       (if ,form
	   ,(if groupp
		'(with-output-to-string (stream region)
		   (write-char ch stream))
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
		       `(ed::primitive-group-buffer-fparse-char ,req region)
		       `(ed::primitive-group-buffer-parse-char ,req region))
		   (if *fold-case-p*
		       `(parse::primitive-group-fparse-char ,req region)
		       `(parse::primitive-group-parse-char ,req region)))
	       (if bufferp
		   (if *fold-case-p*
		       `(ed::primitive-buffer-fparse-char ,req nil previous parent)
		       `(ed::primitive-buffer-parse-char ,req nil previous parent))
		   (if *fold-case-p*
		       `(parse::primitive-fparse-char ,req nil previous parent)
		       `(parse::primitive-parse-char ,req nil previous parent)))))
	 (list-string-parsing (req)
	   (if groupp
	       (if bufferp
		   (if *fold-case-p*
		       `(ed::primitive-group-buffer-fparse-string ,req region)
		       `(ed::primitive-group-buffer-parse-string ,req region))
		   (if *fold-case-p*
		       `(parse::primitive-group-fparse-string ,req region)
		       `(parse::primitive-group-parse-string ,req region)))
	       (if bufferp
		   (if *fold-case-p*
		       `(ed::primitive-buffer-fparse-string ,req nil previous parent)
		       `(ed::primitive-buffer-parse-string ,req nil previous parent))
		   (if *fold-case-p*
		       `(parse::primitive-fparse-string ,req nil previous parent)
		       `(parse::primitive-parse-string ,req nil previous parent))))))
    (etypecase req
      (keyword
       (if groupp
	   `(,(generate-parse-function-name req t bufferp) region)
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
	   (error "Fell through operation cond."))))))))

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
      `(let ((branch (fill-pointer region)))
	 (or ,(cons 'and
		    (mapcar (lambda (req)
			      (list-single-req-parsing req
						       :groupp t
						       :bufferp bufferp))
			    reqs))
	     (progn
	       ;; Failed to match $reqs, reset region to the position kept
	       ;; in branch.
	       (setf (fill-pointer region) branch)
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
   group parsing function inserts the parsed text at the end of a given
   array."
  (or (cdr rule)
      (error "Each rule must have at least one requirement."))
  `(defun ,name (region)
     ,(format nil
	      "Parse from ~:[*stream*~;*mark*~] into array $region according to the ~A rule."
	      bufferp (car rule))
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
       (if previous
	   (setf (node-next previous) parent))
       (values parent parent))))

(defun list-parse-structure (name)
  "Return the definition of the node structure named Name."
  `(defstruct (,(read-from-string (format nil "~A-node" name))
	       (:constructor ,(generate-constructor-name name)
			     (&key content next previous parent))
	       (:include parse::node))
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
  "Pop List by replacing the head with the cdr of the head."
  (if (cdr list)
      (progn
	(setf (car list) (cadr list))
	(setf (cdr list) (cddr list)))
      (error "Attempt to `care-pop' single cons.")))

;;; Public
;;;
(defun copy-streams (streams)
  "Return a copy of the (stream position) list Streams, updating the
   positions in the copy."
  (mapcar (lambda (l) (list (car l) (file-position (car l))))
	  streams))

;;; Public
;;;
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

;;; Public
;;;
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

(declaim (inline parser-read-and-compare-char))

(defun parser-read-and-compare-char (char)
  "Read the next character in *stream*, returning t if it is Char else
   false.  If the end of stream has already been reached then pop a stream
   from parse::streams into *stream*, and retry."
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

;;; Public
;;;
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

;;; Public
;;;
(defun group-parse-alphanumeric (region)
  "Read next character in *stream*.  If an alphabet or numeric character is
   read then append the character to array $region and return the
   character, otherwise return nil."
  (let ((ch (parser-read-char)))
    (when (and ch (alphanumericp ch))
      (with-output-to-string (stream region)
	(write-char ch stream))
      ch)))

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

(declaim (inline parse-c))

(defun parse-c (&optional next previous parent)
  "Call `parse-char' on the arguments."
  (parse-char next previous parent))

;; FIX the group function are surely slowed by each calling
;;     with-output-to-string.

;;; Public
;;;
(defun group-parse-char (region)
  "Read next character in *stream*.  If the read succeeds append the
   character to array $region and return the character, otherwise return
   nil."
  (let ((ch (parser-read-char)))
    (when ch
      (with-output-to-string (stream region)
	(write-char ch stream))
      ch)))

;;; Public
;;;
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

;;; Public
;;;
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

;;; Public
;;;
(defun primitive-group-parse-char (char region)
  "Read next character in *stream*.  Append Char to array $region and
   return Char, if Char was read, otherwise return nil."
  (when (parser-read-and-compare-char char)
    (with-output-to-string (stream region)
      (write-char char stream))
    char))

;;; Public
;;;
(defun primitive-group-fparse-char (char region)
  "Read next character in *stream*.  Append Char to array $region and
   return Char if upper or lower case Char was read, otherwise return nil."
  (when (parser-read-and-fcompare-char char)
    (with-output-to-string (stream region)
      (write-char char stream))
    char))

;;; Public
;;;
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
    (if previous
	(setf (node-next previous) node))
    (values node node)))

;;; Public
;;;
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
      (if previous
	  (setf (node-next previous) node))
      (values node node))))

;;; Public
;;;
(defun primitive-group-parse-string (string region)
  "Read String from *stream*.  If successful append String to array $region
   and return String, otherwise return nil."
  (dotimes (pos (length string))
    (or (parser-read-and-compare-char (char string pos))
	(return-from primitive-group-parse-string nil)))
  (with-output-to-string (stream region)
    (write-string string stream))
  string)

;;; Public
;;;
(defun primitive-group-fparse-string (string region)
  "Read String from *stream* with case folding.  If successful append the
   read string to array $region and return the read string, otherwise
   return nil."
  (let* ((len (length string))
	 (result (make-string len)))
    (dotimes (pos len)
      (let ((ch (parser-read-char)))
	(or (and ch (eq (char-upcase ch) (char-upcase (char string pos))))

	    (return-from primitive-group-fparse-string nil))
	(setf (char result pos) ch)))
    (with-output-to-string (stream region)
      (write-string result stream))
    result))
