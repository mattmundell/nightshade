;;; -*- Mode: Editor -*-
;;

; char digit upper-char lower-char whitespace word-delimiter...

; (group (many :char) for "abc", (many :char) for ~(#\a #\b #\c)

;; FIX moved to parse-test.lisp

(defparser `((:manual  :char)))

(defparser `((:manual  :char :char)))

(defparser `((:manual  (many :char))))

(defparser `((:manual  "@chap" :char)))

(defparser `((:manual  (group (many :mchar)))
	     (:mchar   (or #\c #\o #\m))))

(defparser `((:manual  (group :mchar (many #\newline)))
	     (:mchar   (or #\c #\o #\m))))

(defparser `((:manual   (many (or #\b #\a :c) #\newline))
	     (:a        #\a)
	     (:b        #\b)
	     (:c        #\c)))

(defparser `((:manual  (cond (eq ch #\I)))))

(defparser `((:manual  (many (cond (eq ch #\a))))))

(defparser `((:manual  (many (cond (if (eq ch #\a) nil t))))))
(defparser `((:manual  (many (cond (if (eq ch #\@) nil t))))))

(defparser `((:manual  (many (cond (cond ((eq ch #\@) nil) (t t)))))))

(defparser `((:manual  (many (or (group (many :mchar)) #\@)))
	     (:mchar   (cond (if (eq ch #\@) nil t)))))

(defparser `((:manual  (many (group (many :mchar))))
	     (:mchar   (cond (if (eq ch #\@) nil t)))))

(defparser `((:manual  (many (or (group (many :mchar)) #\@)))
	     (:mchar   (cond (if (eq ch #\@) nil t)))))

(defparser `((:manual  (many :mchar))
	     (:mchar   (cond (if (eq ch #\@) nil t)))))

(defparser `((:manual  (many (or :mchar #\@)))
	     (:mchar   (cond (if (eq ch #\@) nil t)))))

(defparser `((:manual  (or :mchar #\@))
	     (:mchar   (cond (if (eq ch #\@) nil t)))))

(defparser `((:manual  (many (or :mchar #\@)))
	     (:mchar   (or #\a #\b #\c))))

(defparser `((:manual  :chapter)
	     (:chapter :char :char)))

(defparser
  `((:manual     :chapter)
    (:chapter    "@chap" :char)))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :char))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline "@hemlock"))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:text       :char))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:text       (many :char)))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:text       (group :char :char)))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:text       (many (many :char))))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:text       (group (many :char))))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:paragraph  (group :text))
  (:text       (many :char)))

(defparser
  `((:manual     (or :char "@chap"))))

(defparser
  `((:manual     (many (or :char "@chap")))))

(defparser
  (:manual     (or (group (many :char)) "@chap")))

(defparser
  (:manual     (many (or :text "@hemlock")))
  (:text       (many :char)))

(defparser
  (:manual     (many :text))
  (:text       (group (many :char))))

(defparser
  (:manual     "@chap" #\newline (many (or :text "@hemlock")))
  (:text       (group (many :char))))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline (many (or :text "@hemlock")))
  (:text       (group (many :char))))

(defparser
  `((:manual   (group (or #\@ #\a) #\c))))

(defparser
  `((:manual   (or #\a #\@) #\c)))

(defparser `((:manual   (or #\@ #\a) #\c)))

(defparser
  `((:manual   (group (or #\a #\@) #\c))))

(defparser  `((:manual   (or (many :char) #\@))))
(defparser  `((:manual   (or #\@ (many :char)))))

(defparser  `((:manual   (group #\@ #\c))))

(defparser  `((:manual   (group (many :char)))))

(defparser `((:manual     :char)))

(defparser `((:manual     (or ,@chars))))

(defparser `((:manual     (or #\@ ,@(remove #\@ chars)))))

(defparser `((:manual     (many (or ,@chars)))))

(defparser `((:manual  :tchar)
	     (:tchar   (or ,@chars))))

(defparser `((:manual  (many :tchar))
	     (:tchar   (or ,@chars))))

(defparser
  `((:manual     (many (or "@hemlock" #\newline #\  :tchar)))
    (:tchar      (or ,@chars))))

(defparser
  `((:manual     (group :tchar #\c))
    (:tchar      :char)))

(defparser
  `((:manual     (many :tchar))
    (:tchar      :char)))

(defparser
  `((:manual     (group (many :tchar)))
    (:tchar      :char)))

(defparser
  `((:manual     (group (many :tchar)))
    (:tchar      (or ,@chars #\newline))))

(defparser
  `((:manual     :chapter)
    (:chapter    "@chap" #\newline (many (or "@hemlock" :text)))
    (:text       (group (many :tchar)))
    (:tchar      (or #\newline ,@chars))))

(defparser
  `((:manual     :chapter)
    (:chapter    "@chap" #\newline (many (or :text "@hemlock")))
    (:text       (group (many :tchar)))
    (:tchar      (or #\newline ,@(remove #\@ chars)))))

(defparser `((:manual  #\@ (or #\c #\b) #\h)))

(defparser `((:manual  #\@ (or #\h) #\c)))
(defparser `((:manual  #\@ (or #\c) #\h)))

(defparser `((:manual  #\@ (or #\c :epsilon) #\h)
	     (:epsilon)))

(defparser `((:manual     (many #\I) #\newline)))

(defparser `((:manual     (many :char) #\newline)))  ; :char incl's newline, so fails

(defparser `((:manual  (or (group #\a #\p) "@chap"))))

(defparser `((:manual  (or (group (many (or #\a #\p))) "@chap"))))

(defparser `((:manual  (many (or (group (many :ch)) :chap)))
	     (:chap    "@chap")
	     (:ch      (or #\a #\p))))

(defparser `((:manual  #\a #\b #\newline)))

(defparser `((:manual  (many (or #\b #\a)))))

(defparser `((:manual  (many (or #\a "@chap")))))

(defparser `((:manual  (many (or (group (many (or #\  #\a #\b))) :hemlock)))))

(defparser `((:test  :one "@\\" "abc")
	     (:one   "one")))

(defparser `((:manual  (many (or (many #\a) "@chap")))))

(defparser `((:test (or (many #\a #\a #\c) #\newline))))
(defparser `((:test (many (or (many #\a #\a #\c) #\newline)))))

(defparser `((:test (many (or :para #\newline)))
	     (:para (many (many #\a) :a))
	     (:a    (group (many #\b)) "@\\" :c)
	     (:c    :para)))

(defparser `((:test (many (or :para #\newline)))
	     (:para (many (or :c :a) #\newline))
	     (:a    #\b #\b #\a)
	     (:c    #\c)))

;; HERE

(defparser `((:test (group ", bind " :alphanumeric))))

(defvar closebraces '(#\] #\} #\> #\)))
(defparser `((:test       "com \"" (many :alphanumeric) #\"  ; "
	                  (or (group ", bind "
				     :openbrace (many (or :alphanumeric #\- #\ )) :closebrace) :epsilon))
	     (:openbrace  (or #\[ #\{ #\< #\())
	     (:closebrace (or ,@closebraces))
	     (:epsilon )))

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline (many (or :text "@hemlock")))
  (:text       (group (many :tchar)))
  (:tchar      (or ,(remove #\@ chars) "@@")))   ; @@ for escaping eg

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline :text)
  (:text       (or (group (many :char)))))   ; single or for optionally?
                                             ; maybe name-only rule (:epsilon) then (or x :epsilon)

;; alternatives, additions

(defparser
  (:manual     :chapter)
  (:chapter    "@chap" #\newline (many (or :text "@hemlock")))
  (:text       (group (many :tchar)))
  (:tchar      (subtract :char #\@)))

(defparser
  '((manual     chapter)
    (chapter    "@chap" #\newline (or text "@hemlock"))
    (text       (group (may (many char))))
    (char       (or (subtract :char #\@) "@@"))))   ; @@ for escaping eg

;; FIX test, fail cases

(with-open-file (file "/home/matt/src/tests/lisp/chapter" :direction :input) (parse-manual file))
(with-open-file (file "h:chapter" :direction :input) (parse-manual file))

;; start defparser

;; FIX moved to h:parse.lisp :src/lisp/parse.lisp

(declaim (special *required-functions* *defined-functions*))

(defun generate-parse-function-name (name &optional content)
  "Return a parsing function name string according to rule name Name and
   flag Content.  Push the two names and the content flag onto
   *required-functions* once ever, so that the function can be generated
   later."
  (let ((defined (assoc name *defined-functions*)))
    (if (and defined (eq (caddr defined) content))
	(cadr defined)
	(let ((required (assoc name *required-functions*)))
	  (if (and required (eq (caddr required) content))
	      (cadr required)
	      (let ((fun-name (read-from-string (format nil
							(if content
							    "content-parse-~A"
							    "parse-~A")
							name))))
		(setq *required-functions*
		      (append *required-functions* (list (list name fun-name content))))
		fun-name))))))

(defun list-many-parsing (arg-parsing-list)
  "List the code required to parse a \"many\" form into a list.
   Arg-parsing-list is code to parse a single unit of the \"many\" form."
  ;; FIX flet for single version of arg-parsing-list?
  `(multiple-value-bind (head tail)
			(block nil ,arg-parsing-list)
     (when head
       (loop
	 (let ((pos (file-position stream)))
	   (multiple-value-bind (parse parse-tail)
				(block nil ,arg-parsing-list)
	     (if parse
		 (progn
		   (setf (cdr tail) parse)
		   (setq tail parse-tail))
		 (progn
		   (file-position stream pos)
		   (return-from nil (values head tail))))))))))
  
(defun list-req-concat (reqs &optional fun-name &key content)
  "List code that will parse Reqs, the requirements of a rule.  Fun-name is
   the name of a function from which to return if parsing fails.  The
   returned code will return the parse as a single string, in a single
   node."
  (when reqs
    `(multiple-value-bind (head tail)
			  ,(list-req-parsing reqs fun-name :content t)
       (declare (ignore tail))
       ,(if content
	    '(apply 'concatenate 'simple-string head)
	    '(list '(:name :string)
		   (list :content (apply 'concatenate 'simple-string head)))))))

(defun list-or-parsing-rest (reqs &optional fun-name &key (content nil))
  "List code that will parse Reqs, the rest of an \"or\" rule requirements.
   Fun-name is the name of a function from which to return if parsing
   fails."
  `(progn
     (file-position stream pos)
     (multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    fun-name
						    :content content)
       (if head
	   (values head tail)
	   ,(when (cdr reqs)
	      (list-or-parsing-rest (cdr reqs) fun-name :content content))))))

(defun list-or-parsing (reqs &optional fun-name &key (content nil))
  "List code that will parse the \"or\" rule requirements Reqs.  Fun-name
   is the name of a function from which to return if parsing fails."
  `(let ((pos (file-position stream)))
     (multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    fun-name
						    :content content)
       (if head
	   (values head tail)
	   ,(if (cdr reqs)
		(list-or-parsing-rest (cdr reqs)
				      fun-name
				      :content content)
		(error "Single \"or\" requirement."))))))

(defun list-single-req-parsing (req &optional fun-name &key (content nil))
  "List code that will parse Req, a requirement of a rule.  Fun-name is the
   name of the function from which to return if parsing fails."
  (etypecase req
    (keyword
     `(,(generate-parse-function-name req content) stream))
    (string
     `(let ((parse (,(if content
			 'primitive-content-parse-string
			 'primitive-parse-string) ,req stream)))
	(when parse
	  (let ((head (list parse)))
	    (values head head)))))
    (base-char
     `(let ((parse (,(if content
			 'primitive-content-parse-char
			 'primitive-parse-char) ,req stream)))
	(when parse
	  (let ((head (list parse)))
	    (values head head)))))
    (cons
     (ecase (car req)
       (many
	(list-many-parsing (list-req-parsing (cdr req) nil :content content)))
       (group
	`(block nil 
	   (let ((head (list ,(list-req-concat (cdr req) nil :content content))))
	     (values head head))))
       (cond
	`(let ((ch (read-char stream nil)))
	   (when ch
	     (if ,(cadr req)
		 (let ((l (list ,(if content
				     '(string ch)
				     '(list '(:name :char)
					    (list :content ch))))))
		   (values l l))))))
       (or
	(list-or-parsing (cdr req) fun-name :content content))))))

(defun list-req-parsing-rest (reqs &optional fun-name &key (content nil))
  "List code that will parse the rest of Reqs, the requirements of a rule.
   Fun-name is the name of the function from which to return if parsing
   fails."
  (when reqs
    `(multiple-value-bind (parse-head parse-tail)
			  ,(list-single-req-parsing (car reqs)
						    fun-name
						    :content content)
       (or parse-head (return-from ,fun-name))
       (setf (cdr tail) parse-head)
       (setq tail parse-tail)
       ,(list-req-parsing-rest (cdr reqs) fun-name :content content)
       (values head tail))))
  
(defun list-req-parsing (reqs &optional fun-name &key (content nil))
  "List code that will parse Reqs, the requirements of a rule.  Fun-name is
   the name of the function from which to return if parsing fails."
  (when reqs
    `(multiple-value-bind (head tail)
			  ,(list-single-req-parsing (car reqs)
						    fun-name
						    :content content)
       (or head (return-from ,fun-name))
       ,(list-req-parsing-rest (cdr reqs) fun-name :content content)
       (values head tail))))
  
(defun list-content-parse-function (name rule)
  "Return a content parsing function definition according to Rule.  A
   content parsing function returns the entire parse as a list of a single
   string."
  (if (cdr rule)
      `(defun ,name (stream)
	 ,(list-req-parsing (cdr rule) name :content t))
      `(defun ,name (stream)
	 (declare (ignore stream))
	 '(""))))

(defun list-parse-function (name rule)
  "Return a parsing function definition according to Rule."
  (when rule
    (if (cdr rule)
	`(defun ,name (stream)
	   ;; Parse the requirements before creating the list, in case
	   ;; the parsing fails.
	   (let* ((reqs ,(list-req-parsing (cdr rule) name))
		  (l (list (list (list :name ,(car rule))
				 (cons :content reqs)))))
	     (values l l)))
	`(defun ,name (stream)
	   (declare (ignore stream))
	   (let ((l (list (list (list :name ,(car rule))
				(list :content "")))))
	     (values l l))))))

(defun list-parse-functions (rules)
  "Return the list of definitions of the parsing functions named in
   *required-functions* and required by those."
  (when *required-functions*
    (let ((descr (pop *required-functions*)))
      (let ((rule (assoc (car descr) rules)))
	(push descr *defined-functions*)
	(if rule
	    (cons (if (caddr descr)
		      (list-content-parse-function (cadr descr) rule)
		      (list-parse-function (cadr descr) rule))
		  (list-parse-functions rules))
	    (list-parse-functions rules))))))

(defmacro defparser (rules)
  "Define a parser.  Rules is a list of BNF-style rules.  A rule is named
   by the keyword in the car of the rule.  The first rule is the root rule.
   A parser (defun parse-<root> (stream) ...) is produced, where <root> is
   the name of the root rule.  The parser returns a node list.  The cdr of
   a rule lists the requirements of the rule.  Requirements can be
   characters, strings, keywords naming other rules, or operations on
   requirements.  An operation can be \"many\", \"group\" or \"or\", in the
   style (or #\a (many :char) #\c).  The result of parsing the requirements
   of a \"group\" are concatenated into a single node.  The rule :char is
   predefined to match any character.  The rule :alphanumeric is predefined
   to match any alphabet or number character."
  (let ((rules (eval rules)))
    (when rules
      (let ((*required-functions* nil)
	    (*defined-functions* nil))
	(generate-parse-function-name (caar rules))
	(cons 'prog1 (list-parse-functions rules))))))

;;; Functions predefined for the parser.

(defun parse-alphanumeric (stream)
  "Read next character in Stream.  If an alphabet or numeric character is
   read return nil, otherwise return two values both the same cons (node .
   nil) where node is a node of the character."
  (let ((ch (read-char stream nil)))
    (if (and ch (alphanumericp ch))
	(let ((l `(((:name :char) (:content ,ch)))))
	  (values l l)))))

(defun content-parse-alphanumeric (stream)
  "Read next character in Stream.  If an alphabet or numeric character is
   read return nil, otherwise return two values both the same cons (node .
   nil) where node is a node of the character."
  (let ((ch (read-char stream nil)))
    (if (and ch (alphanumericp ch))
	(let ((l (list (string ch))))
	  (values l l)))))

(defun parse-char (stream)
  "Read next character in Stream.  If the read fails return nil, otherwise
   return two values both the same cons (node . nil) where node is a node
   of the character."
  (let ((char (read-char stream nil)))
    (when char
      (let ((l `(((:name :char) (:content ,char)))))
	(values l l)))))

(defun content-parse-char (stream)
  "Read next character in Stream.  If the read fails return nil, otherwise
   return two values both the same cons (string . nil) where string
   contains only the character."
  (let ((ch (read-char stream nil)))
    (if ch
	(let ((l (list (string ch))))
	  (values l l)))))

(defun primitive-parse-char (char stream)
  "Read next character in Stream. Return a node of Char if Char was read,
   else put back the character and return nil."
  (let ((ch (read-char stream nil)))
    (if (eq ch char)
	`((:name :char) (:content ,char)))))

(defun primitive-content-parse-char (char stream)
  "Read next character in Stream, return Char as a string if it was read,
   else put back the character and return nil."
  (let ((ch (read-char stream nil)))
    (if (eq ch char)
	(string char))))

(defun primitive-parse-string (string stream)
  "Parse String in Stream.  Return a node of String if successful, else
   return nil."
  (dotimes (pos (length string))
    (or (eq (read-char stream nil) (char string pos))
	(return-from primitive-parse-string nil)))
  `((:name :string) (:content ,string)))
  
(defun primitive-content-parse-string (string stream)
  "Parse String in Stream.  Return String if successful, else set the
   stream to the original position and return nil."
  (dotimes (pos (length string))
    (or (eq (read-char stream nil) (char string pos))
	(return-from primitive-content-parse-string nil)))
  string)

;;; end defparser

;;; old

;;; Predefined character sets.

(defvar chars '())

(progn
  (setq chars '())
  (do ((i 255 (decf i)))
      ((eq i 31)
       chars)
    (push (code-char i) chars)))

;;; many

(defun group-many (parse-function stream)
  (do* ((parse (funcall parse-function stream)
	       (funcall parse-function stream))
	(result))
       ((eq parse nil)
	result)
    (setf result (concatenate 'simple-string result parse))))

(defun many (parse-function stream)
  (do* ((parse (funcall parse-function stream)
	       (funcall parse-function stream))
	(result (if parse "")))
       ((eq parse nil)
	result)
    (setf result (append result (cons parse nil)))))

;;; manually

(defvar chars ...)

`((:manual     :chapter)
  (:chapter    "@chap" #\newline (or :text "@hemlock"))
  (:text       (group-one-or-more :tchar))
  (:tchar      (add (subtract char #\@) "@@")))   ; add for @ escaping

(defun parse-manual (stream)
  (let ((chapter (parse-chapter stream)))
    (when chapter
      `((:name :manual) (:content ,chapter)))))

(defun parse-chapter (stream)
  (let ((xxx (primitive-parse-string "@chap" stream)))
    (when xxx
      (let ((xxx3 (primitive-parse-char #\newline stream)))
	(when xxx3
	  (let ((text (parse-text stream)))
	    (if text
		`((:name :chapter) (:content ,xxx ,text))
		(let ((xxx2 (primitive-parse-string "@hemlock" stream)))
		  (when xxx2
		    `((:name :chapter) (:content ,xxx ,xxx3 ,xxx2)))))))))))

(defun parse-text (stream)
;  (let ((text (group-many 'content-parse-char stream)))
  (let ((text (group-many 'content-parse-char stream)))
    (when text
      (or (string= text "")
	  `((:name :text) (:content ,text))))))

;; predefined
(defun parse-char (stream)
  (let ((char (read-char stream nil)))
    (when char
      `((:name :char) (:content ,char)))))

(defun content-parse-char (stream)
  (let ((char (read-char stream nil)))
    (when char
      (if (eq char #\@) (return-from content-parse-char nil)) ;; FIX
      (string char))))

(defun primitive-parse-string (string stream)
  (dotimes (pos (length string))
    (or (eq (read-char stream nil) (char string pos))
	(return nil)))
  ;; xxx must be unique, chosen by the generator, ~ must be hemlock doc type
  `((:name :xxx) (:content ,string)))

(defun primitive-parse-char (char stream)
  (if (eq (read-char stream nil) char)
      ;; ch must be unique, chosen by the generator, ~ must be hemlock doc type
      `((:name :ch) (:content ,char))))

(defun group-many (parse-function stream)
  (do* ((parse (funcall parse-function stream)
	       (funcall parse-function stream))
	(result (if parse "")))
       ((eq parse nil)
	result)
    (setf result (concatenate 'simple-string result parse))))

(with-open-file (file "/home/matt/src/tests/lisp/chapter" :direction :input)
  (parse-manual file))
