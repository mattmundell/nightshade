;;; Scribe manual parser.

(in-package "ED")

(eval-when (compile eval load)

(defun p-start (name) `((or "begin" "Begin") :openbrace ,name :closebrace
			(or (group (many #\ )) :epsilon)))
(defun p-end (name) `((or "@end" "@End") :openbrace ,name :closebrace
		      (or (group (many #\ )) :epsilon)))

(defvar closebraces '(#\] #\} #\> #\)))
(defvar openbraces '(#\[ #\{ #\< #\())
(defvar braces (append closebraces openbraces))

(defparser
  `((:manual         :make :head (or (many :chapter) :epsilon))

    (:make           "@make" :argument (group (many (or #\  #\newline))))

    (:head           (many (or :head-part :text)))
    (:head-part      #\@ (or :device :style :use :libraryfile :string
			     :researchcredit :commandstring))

    (:device         (or "Device" "device") :argument)
    (:style          (group (or "Style" "style") (or (group (many #\ ))
						     :epsilon))
		     :argument)
    (:use            (or "Use" "use") :argument)
    (:libraryfile    (or "Libraryfile" "libraryfile") :argument)
    (:string         (or "String" "string") :argument)
    (:commandstring  "commandstring" :bracedarg)
    (:researchcredit ,@(p-start "ResearchCredit") #\newline
		     (or "@ArpaCredit" "@arpacredit") :argument #\newline
		     ,@(p-end "ResearchCredit") #\newline)

    (:chapter        (group (or "@chapter" "@chap" "@Chapter" "@Chap")
			    (or (group (many #\ )) :epsilon))
		     :argument
		     :text (or (many :section) :epsilon))
    (:section        (group "@section" (or (many #\ ) :epsilon))
		     :argument :text (or (many :subsection) :epsilon))
    (:subsection     (group "@subsection" (or (many #\ ) :epsilon))
		     :argument (many (or :text :named-para)))

    (:named-para     "@paragraph" :argument :text)

    (:text           (many (or #\newline
			       :center-block :comment-block
			       :text-block :format-block
			       :paragraph :heading)))

    (:heading        #\@ (or "Heading" "heading") :argument)

    (:paragraph      (many (or :paragraph-part :line) #\newline))

    ;; FIX lead #\@ slows :line-part at begin of line
    (:paragraph-part #\@ (or :description :enumerate :itemize
			     :example :program-eg :program-eg-block
			     :quotation	:lisp
			     ;; FIX combine for speed
			     :defun :defvar :defcon :defmac :defcom :defhvar))

    (:line           (many (or :tab (group (many :mchar)) :line-part)))

    ;; FIX big or; could order most common first
    (:line-part      #\@ (or :at :hemlock :clisp :llisp :windows
			     :dash :true :false :nil
			     (after
			      :include
			      (when head
				(let* ((name (node-next
					      (node-content
					       (node-content
						(node-next (node-content head))))))
				       (nstring (concatenate
						 'simple-string
						 (region-to-string (node-content name))
						 ".mss"))
				       (file (merge-pathnames
					      nstring
					      (if (fd-stream-p *stream*)
						  (directory-namestring
						   (lisp::fd-stream-file *stream*))
						  ".")))
				       (s (open file
						:direction :input
						:if-does-not-exist :error)))
				  (when s
				    (care-push `(,s 0) *streams*)
				    (setq *stream* s)
				    (values head tail)))))
			     :comment :index :label :tag :hid :kwd
			     :ref :pageref :funref :macref :varref :comref :hvarref
			     :binding
			     :i :f :f1 :t :w :y :bf :b :center :val :var
			     :tabclear :newpage :tabdivide :blankspace
			     :emacs :mh :foot :comma :caret
			     :multiple
			     :optional :rest :key :mstar :mopt :mor :mgroup
			     ;; A line-part to cater for an indented itemize entry.
			     :block))

    (:tabclear       (or "tabclear" "Tabclear"))
    (:newpage        "newpage")
    (:tabdivide      "tabdivide" :argument)
    (:blankspace     (or "Blankspace" "blankspace") :argument)

    (:tab            "@\\") ; FIX Is @\ a tab?    "

    (:at             #\@)
    (:hemlock        (or "hemlock" "Hemlock"))
    (:emacs          (or "emacs" "Emacs"))
    (:clisp          "clisp")
    (:llisp          "llisp")
    (:windows        "windows")
    (:mh             "mh")
    (:dash           "dash")
    (:true           "true")
    (:false          "false")
    (:nil            (or "nil" "Nil"))
    (:include        (group (or "include" "Include") (or (many #\ ) :epsilon))
		     :argument)
    (:comment        "comment" :argument)
    ;; FIX Maybe the middle space parsing should be built into argument.
    (:index          (group (or "index" "Index") (or (many #\ ) :epsilon))
		     :argument)
    (:label          (or "label" "Label") :argument)
    (:tag            (group (or "tag" "Tag") (or (many #\ ) :epsilon))
		     :argument)
    (:hid            "hid" :argument)
    (:kwd            "kwd" :argument)
    (:ref            "ref" :argument)
    (:pageref        "pageref" :argument)
    (:funref         "funref" :argument)
    (:macref         "macref" :argument)
    (:varref         "varref" :argument)
    (:comref         "comref" :argument)
    (:hvarref        "hvarref" :argument)
    (:binding        "binding" :argument)
    ;; FIX seems like all command args can be quoted or braced
    (:i              (or #\i #\I) (or :bracedarg :quotedarg))
    (:f              #\f :bracedarg)
    (:f1             "f1" :bracedarg)
    (:t              #\t :bracedarg)
    (:y              #\Y :bracedarg)
    (:w              #\w :bracedarg)
    (:bf             "bf" :argument)
    (:b              "b" :bracedarg)
    (:center         "center" :bracedarg)
    ;; FIX Struct value-node already exists in HI, so "val" instead of "value".
    (:val            "value" :argument)
    (:var            "var" :argument)
    (:foot           "foot" :argument)
    (:comma          #\;) ; What is this?
    (:caret          #\^) ; What is this?
    (:multiple       "multiple" :bracedarg)
    ;; These might only occur in def args.
    (:optional       "optional")
    (:rest           "rest")
    (:key            "key")
    (:mstar          (or "mstar" "MSTAR" "Mstar") (or :bracedarg :quotedarg))
    (:mopt           (or "mopt" "MOPT") :bracedarg)
    (:mor            "MOR")
    (:mgroup         (or "mgroup" "Mgroup") (or :bracedarg :quotedarg))

    (:lisp           "lisp" :text "@endlisp")

    (:description    :description-start
		     (many (many #\newline) :descript)
		     (or #\newline :epsilon) :description-end)
    (:description-start ,@(p-start '(or "description" "Description")))
    (:description-end   ,@(p-end '(or "description" "Description"))
			(or (many #\ ) :epsilon))
    ; The many form is to leave the :tab (@\) out of a :line.
    (:descript       :described
		     "@\\" (or #\newline :epsilon) :paragraph) ;"
    (:described      (many (or (group (many :mchar)) :line-part)))

    (:enumerate      :enumerate-start
		     (many (many #\newline) :paragraph)
		     :enumerate-end)
    (:enumerate-start ,@(p-start "enumerate"))
    (:enumerate-end   ,@(p-end "enumerate") (or (many #\ ) :epsilon))

    (:itemize        :itemize-start
		     (many (many #\newline) :paragraph)
		     :itemize-end)
    (:itemize-start  ,@(p-start '(or "itemize" "Itemize")))
    (:itemize-end    ,@(p-end '(or "itemize" "Itemize")) (or (many #\ ) :epsilon))

    (:example        "example" :argument)

    (:program-eg     "programexample" :argument)

    (:program-eg-block  :program-eg-start :text :program-eg-end)
    (:program-eg-start  ,@(p-start '(or "programexample" "ProgramExample")))
    (:program-eg-end    ,@(p-end '(or "programexample" "ProgramExample"))
		        (or (many #\ ) :epsilon))

    ;; For @begin[example] blocks.
    (:block          :block-start
		     ;; Include the case when "@end{*}" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :block-end)
    (:block-start    ,@(p-start '(many :alphanumeric)))
    (:block-end      ,@(p-end '(many :alphanumeric)))

    (:center-block   :c-b-start (or :text :line) :c-b-end)
    (:c-b-start      #\@ ,@(p-start "Center"))
    (:c-b-end        ,@(p-end "Center"))

    (:comment-block  :co-b-start (or :text :line) :co-b-end)
    (:co-b-start      #\@ ,@(p-start "comment"))
    (:co-b-end        ,@(p-end "comment"))

    (:text-block     :t-b-start :text :t-b-end)
    (:t-b-start      "@begin" :openbrace "Text" (group (many :argchar)) :closebrace)
    (:t-b-end        ,@(p-end "Text"))

    (:format-block    :f-b-start :text :f-b-end)
    (:f-b-start      "@begin" :openbrace "format" (group (many :argchar)) :closebrace)
    (:f-b-end        ,@(p-end "format"))

    (:quotation      :quota-start :text :quota-end)
    (:quota-start    "begin" :openbrace "quotation" (group (many :argchar)) :closebrace)
    (:quota-end      ,@(p-end "quotation"))

    (:defun          "defun" :defun-arg
		     (or (many (or #\newline :defun1 :defhvar1 :defvar1 :defcon1))
 			 :epsilon)
		     ;; Include the case where "@endefun" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :enddef)
    (:enddef         (group "@enddef" (many :mchar)))
    (:defun1         "@defun1" :defun-arg)
    (:defun-arg      :openbrace
		     (or "fun " "Fun ") :bracedarg
		     (or :defun-arg-pkg :epsilon)
		     (or :defun-arg-label :epsilon)
		     (or :defun-arg-args :epsilon)
		     (or :defun-arg-keys :epsilon)
		     (or (many :defun-arg-mkeys) :epsilon)
		     (or (group (many #\ )) :epsilon)
 		     :closebrace (or (group (many (or #\  #\tab))) :epsilon))
    (:defun-arg-label  ", funlabel " :argpart)
    (:defun-arg-args   (group #\,
			      (or (many (or #\  #\newline #\tab)) :epsilon)
			      (or "args" "Args")
			      (or (many (or #\  #\newline #\tab)) :epsilon))
		       :argpart)
    (:defun-arg-keys   (group #\,
			      (or (many (or #\  #\newline #\tab)) :epsilon)
			      (or "keys" "Keys")
			      (or (many (or #\  #\newline #\tab)) :epsilon))
		       :argpart)
    (:defun-arg-mkeys  (group #\,
			      (or (many (or #\  #\newline #\tab)) :epsilon)
			      (or "morekeys" "Morekeys")
			      (or (many (or #\  #\newline #\tab)) :epsilon))
		       :argpart)
    (:defun-arg-pkg    (group #\,
			      (or (many (or #\  #\newline #\tab)) :epsilon)
			      (or "package" "Package")
			      (or (many (or #\  #\newline #\tab)) :epsilon))
		       :argpart)

    (:defvar         "defvar" :defvar-arg
		     (or (many (or #\newline :defvar1 :defhvar1))
 			 :epsilon)
		     ;; Include the case where "@endefun" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :enddef)
    (:defvar1        "@defvar1" :defvar-arg)
    (:defvar-arg     :openbrace
		     "var {" (group (many :argchar)) #\}
		     (or :defun-arg-pkg :epsilon)
		     (or :d-a-spec :epsilon)
		     (or (group (many #\ )) :epsilon)
 		     :closebrace)

    (:defcon         "defcon" :defvar-arg
		     (or (many (or #\newline :defcon1 :defvar1))
 			 :epsilon)
		     ;; Include the case where "@endefcon" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :enddef)
    (:defcon1        "@defcon1" :defvar-arg)

    (:defmac         (or "defmac" "Defmac") :defun-arg
  		     (or (many (or #\newline #\  :defmac1 :defhvar1 :defvar1))
 			 :epsilon)
		     ;; Include the case where "@enddefmac" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :enddef)
    (:defmac1        "@defmac1" :defun-arg)

    (:defcom         "defcom" :defcom-arg
 		     (or (many (or #\newline :defcom1 :defhvar1))
			 :epsilon)
		     ;; Include the case where "@enddef*" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :enddef)
    (:defcom1        "@defcom1" :defcom-arg)
    (:defcom-arg     :openbrace
		     (or "com " "Com ") (or :quotedarg :bracedarg)
;		     "com \"" (group (many (or :alphanumeric #\/ #\( #\) #\  #\-))) #\"
		     (or :d-a-body :epsilon)
		     (or (group (many #\ )) :epsilon)
 		     :closebrace)
    (:d-a-body       (or :d-a-com :d-a-stuff :d-a-long) :bindspec)
    (:d-a-com        ", bind ")
    (:d-a-stuff      ", stuff ")
    (:d-a-long       (group #\, (or (many #\ ) :epsilon) #\newline
			    (many (or #\tab #\ ))
			    "stuff "))
    (:bindspec       (or :bindspec1 :bindspec2 :bindspec3 :bindspec4))
    (:bindspec1      #\[ (many (cond (if (equal ch #\]) nil t))) #\])
    (:bindspec2      #\{ (many (cond (if (equal ch #\}) nil t))) #\})
    (:bindspec3      #\< (many (cond (if (equal ch #\>) nil t))) #\>)
    (:bindspec4      #\( (many (cond (if (equal ch #\)) nil t))) #\))
    (:defhvar        "defhvar" :defhvar-arg
 		     (or (many (or #\newline :defcom1 :defhvar1))
			 :epsilon)
		     ;; Include the case where "@endef*" is on the same
		     ;; line as the text.
		     (or (many (or :paragraph #\newline)) :epsilon)
		     (or :line :epsilon)
		     :enddef)
    (:defhvar1       "@defhvar1" :defhvar-arg)
    (:defhvar-arg    :openbrace
		     "var " (or :quotedarg :bracedarg)
		     (or :d-a-spec :epsilon)
		     :closebrace)
    (:d-a-spec       ", val" (or #\  :epsilon) (or :bindspec
						   (group (many :argchar))))

    (:arg            (many :argpart))
    (:argpart        (or (group (many :argchar)) :bracedarg :line-part :tab))
    (:bracedarg      (or :bracedarg1 :bracedarg2 :bracedarg3 :bracedarg4))
    (:bracedarg1     #\[ (or (many (or ,@(remove #\] braces) :argpart))
			     :epsilon) #\])
    (:bracedarg2     #\{ (or (many (or ,@(remove #\} braces) :argpart))
			     :epsilon) #\})
    (:bracedarg3     #\< (or (many (or ,@(remove #\> braces) :argpart))
			     :epsilon) #\>)
    (:bracedarg4     #\( (or (many (or ,@(remove #\) braces) :argpart))
			     :epsilon) #\))

    (:qargpart       (or (group (many :qargchar)) :line-part :tab))
    (:quotedarg      (or :quotedarg1 :quotedarg2))
    (:quotedarg1     #\' (or (many (or #\" ,@braces :qargpart)) :epsilon) #\')
    (:quotedarg2     #\" (or (many (or #\' ,@braces :qargpart)) :epsilon) #\")

    (:argument       (or :argument1 :argument2 :argument3 :argument4))

; FIX biggest mem usage is char-node
;     implicit parts complicate translating back to the original
;    (:argument1      (pass #\[) (or (group (many (cond...

    (:argument1      #\[ (or (group (many (cond
					   (or (eq ch #\newline)
					       (eq ch #\tab)
					       (when (graphic-char-p ch)
						 (if (eq ch #\])
						     nil
						     t))))))
			     :epsilon)
		     #\])
    (:argument2      #\{ (or (group (many (cond
					   (or (eq ch #\newline)
					       (eq ch #\tab)
					       (when (graphic-char-p ch)
						 (if (eq ch #\})
						     nil
						     t))))))
			     :epsilon)
		     #\})
    (:argument3      #\< (or (group (many (cond
					   (or (eq ch #\newline)
					       (eq ch #\tab)
					       (when (graphic-char-p ch)
						 (if (eq ch #\>)
						     nil
						     t))))))
			     :epsilon)
		     #\>)
    (:argument4      #\( (or (group (many (cond
					   (or (eq ch #\newline)
					       (eq ch #\tab)
					       (when (graphic-char-p ch)
						 (if (eq ch #\))
						     nil
						     t))))))
			     :epsilon)
		     #\))

;    (:mchar          (cond (let ((code (char-code ch)))
;			     (or (< 31 code 64) (< 64 code 127)))))
;                      or tab (9)
    (:mchar          (cond (if (or (graphic-char-p ch)
				   (eq ch #\tab))
			       (if (eq ch #\@) nil t))))
    (:argchar        (cond (if (or (eq ch #\@)
				   (memq ch '(,@closebraces ,@openbraces)))
			       nil
			       t)))
    (:qargchar       (cond (if (or (eq ch #\@)
				   (memq ch '(,@closebraces ,@openbraces))
				   (eq ch #\')
				   (eq ch #\"))
			       nil
			       t)))
    (:openbrace      (or #\[ #\{ #\< #\())
    (:closebrace     (or ,@closebraces))
    (:epsilon        "")))

) ; eval-when (compile eval load)
