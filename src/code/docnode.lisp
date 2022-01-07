;;; Text-based system for defining simple documents.

(in-package "DOCNODE")

(export '(docnode-to-doc))


#[ A Test Node

Paragraph 1 is one line.

Paragraph 2 spans multiple lines... ababa convert a description of a
document into a function that produces the baba ababa... containing a link
[ A Test Node ].

A menu:

[ A Test Node ]
[ A Test Node ]   item description
[ A Test Node ]   item description 2

Inlined documentation:

{function:lisp:car}

{variable:*print-readably*}

{command:Save File}

{evariable:Spaces per Tab}

end of inlined doc.

== Section 1 ==

A numbered list:

  * list item one

  * list item two

end of section 1.

== Section 2 ==

A description list:

  % name

    description

  % name 2

    description 2

== Section 3 ==

a bullet list

  - list item one

  - list item two

== Section 5 ==

    A verbatim line.

    A verbatim paragraph.  That is, a paragraph across multiple lines where
    each line is indented by at least four spaces.

== Section 4 ==

End of test node.
]#


;;;; The grammar.

(eval-when (eval load)

(parse:defparser
 `((:docnode               (or (many :docnode-line-end)) (or :docnode-block)
			   (or (many (many :docnode-line-end) :docnode-block)))
   (:docnode-block         (or :docnode-menu
			       :docnode-heading
			       :docnode-list
			       :docnode-description
			       :docnode-verbatim
			       :docnode-paragraph))
   (:docnode-menu          (many (or :docnode-menu-separator)
				 :docnode-menu-item))
   (:docnode-menu-item     :docnode-link (or (group "  " :docnode-string))
			   :docnode-line-end)
   (:docnode-menu-separator :docnode-line-end)
   (:docnode-heading       "== " :docnode-string)
   (:docnode-list          :docnode-list-item
			   (or (many :docnode-line-end :docnode-list-item)))
   (:docnode-list-item     (group "  " (or #\- #\*) #\space)
			   :docnode-line
			   (or :docnode-indented-para))
   (:docnode-description   (many :docnode-descr-item))
   (:docnode-descr-item    "  % " :docnode-line
			   (many  :docnode-line-end
				  (or :docnode-indented-list
				      :docnode-indented-para)))
   (:docnode-verbatim      (many :docnode-verbatim-line))
   (:docnode-verbatim-line "    " :docnode-string :docnode-line-end)
   (:docnode-paragraph     (many :docnode-line))
   (:docnode-indented-para (many "    " :docnode-line))
   (:docnode-indented-list (many "    " :docnode-list-item))
   (:docnode-line          (many (or :docnode-link
				     :docnode-in-function
				     :docnode-in-constant
				     :docnode-in-variable
				     :docnode-in-command
				     :docnode-in-evariable
				     :docnode-in-mode
				     :docnode-string))
			   :docnode-line-end)
   (:docnode-line-end      (cond (member parse:ch '(#\linefeed #\return))))
   (:docnode-link          #\[ (group (many (or :docnode-string :docnode-line-end))) #\])
   (:docnode-in-function   "{function:" :docnode-in-string #\})
   (:docnode-in-constant   "{constant:" :docnode-in-string #\})
   (:docnode-in-variable   "{variable:" :docnode-in-string #\})
   (:docnode-in-command    "{command:" :docnode-in-string #\})
   (:docnode-in-evariable  "{evariable:" :docnode-in-string #\})
   (:docnode-in-mode       "{mode:" :docnode-in-string #\})
   (:docnode-string        (group (many :docnode-char)))
   (:docnode-in-string     (group (many :docnode-in-char)))
   (:docnode-char          (cond (fi (member parse:ch '(#\[ #\] #\linefeed #\return)))))
   (:docnode-in-char       (cond (fi (member parse:ch '(#\} #\linefeed #\return))))))
 :eval t)

) ; eval-when


;;;; The functions.

(declaim (special *level*))

(defun %print-docnode (doc-node &optional (stream t))
  (while ((node doc-node (and (typep node 'parse:node)
			      (parse:node-next node))))
	 (node)
    (loop repeat *level* do (format t " "))
    (format stream "~A~%" (type-of node))
    (let ((*level* (+ *level* 2)))
      (etypecase node
	(character
	 (loop repeat *level* do (format stream " "))
	 (format stream "character: #\\~A~%" node))
	(string
	 (loop repeat *level* do (format stream " "))
	 (format stream "string: \"~A\"~%" node))
	(parse:node
	 (if (parse:node-content node)
	     (%print-docnode (parse:node-content node))))))))

(defun print-docnode (doc-node &optional stream)
  (let ((*level* 0)) (%print-docnode doc-node stream)))

(defun filter-ref (link-docnode)
  "Return a doc ref string for $link-docnode."
  (concat "doc:"
	  (substitute #\space #\return
		      (substitute #\space #\newline
				 (string-trim " "
					      (parse:node-content
					       (parse:node-next
						(parse:node-content link-docnode))))))))

(defun line-to-doc (line)
  "Return a doc node for $line."
  (collect ((elements))
    (while ((subline (parse:node-content line)
		     (parse:node-next subline)))
	   (subline)
      (etypecase subline
	(docnode-line-end-node)
	(docnode-in-function-node
	 (let* ((name (parse:node-content
		       (parse:node-content
			(parse:node-next
			 (parse:node-content subline)))))
		(function (read-from-string name))
		(args (and function
			   (fboundp function)
			   (symbol-function function)
			   (function-args (symbol-function function))))
		(doc (documentation function 'function)))
	   (elements
	    `(table ((row (((string . "Function:"))
			   ((string . ,(format () "~A~A"
					       name
					       (if (listp args)
						   (format () " ~A"
							   args)
						   ""))))))
		     (row (((string . ""))
			   ,(if doc
				(docnode-string-to-doc doc)
				'((string . ""))))))
		    :function))))
	(docnode-in-constant-node
	 (let* ((name (parse:node-content
		       (parse:node-content
			(parse:node-next
			 (parse:node-content subline)))))
		(variable (read-from-string name))
		(doc (documentation variable 'variable)))
	   (elements
	    `(table ((row (((string . "Constant:"))
			   ((string . ,(format () "~A" name)))))
		     (row (((string . ""))
			   ,(if doc
				(docnode-string-to-doc doc)
				'((string . ""))))))
		    :constant))))
	(docnode-in-variable-node
	 (let* ((name (parse:node-content
		       (parse:node-content
			(parse:node-next
			 (parse:node-content subline)))))
		(variable (read-from-string name))
		(doc (if variable (documentation variable 'variable))))
	   (elements
	    `(table ((row (((string . "Variable:"))
			   ((string . ,(format () "~A" name)))))
		     (row (((string . ""))
			   ,(if doc
				(docnode-string-to-doc doc)
				'((string . ""))))))
		    :variable))))
	(docnode-in-command-node
	 (let* ((name (parse:node-content
		       (parse:node-content
			(parse:node-next
			 (parse:node-content subline)))))
		(command (getstring name ed::*command-names*))
		(doc (if command (ed:command-documentation command))))
	   (elements
	    `(table ((row (((string . "Command:"))
			   ((string . ,(format () "~A" name)))))
		     (row (((string . ""))
			   ,(if doc
				(docnode-string-to-doc doc)
				'((string . ""))))))
		    :command))))
	(docnode-in-evariable-node
	 (let* ((name (parse:node-content
		       (parse:node-content
			(parse:node-next
			 (parse:node-content subline)))))
		(variable (dolist (table (ed:current-variable-tables :current))
			    (let ((var (getstring name table)))
			      (if var (return var)))))
		(doc (if variable (ed:variable-documentation variable))))
	   (elements
	    `(table ((row (((string . "Editor variable:"))
			   ((string . ,(format () "~A" name)))))
		     (row (((string . ""))
			   ,(if doc
				(docnode-string-to-doc doc)
				'((string . ""))))))
		    :editor-variable))))
	(docnode-in-mode-node
	 (let* ((name (parse:node-content
		       (parse:node-content
			(parse:node-next
			 (parse:node-content subline)))))
		(doc (ed:mode-documentation name)))
	   (elements
	    `(table ((row (((string . "Mode:"))
			   ((string . ,(format () "~A" name)))))
		     (row (((string . ""))
			   ,(if doc
				(docnode-string-to-doc doc)
				'((string . ""))))))
		    :mode))))
	(docnode-link-node
	 (elements
	  `(ref ((string . ,(filter-ref subline))))))
	(docnode-string-node
	 (elements
	  `(string . ,(concat (parse:node-content
			       (parse:node-content subline))
			      " "))))))
    (elements)))

(defun indented-list-to-doc (list)
  "Return a doc node for indented $list."
  (let ((items))
    (iterate iter ((item-node (parse:node-content list)))
      (when item-node
	(let ((elements))
	  (etypecase item-node
	    (parse:region-node)
	    (docnode-list-item-node
	     ;; As for normal list item.
	     (let ((first-line
		    (parse:node-next
		     (parse:node-content item-node))))
	       (setq elements
		     (append elements
			     (line-to-doc first-line)))
	       (when (parse:node-next first-line)
		 (while ((line (parse:node-content
				(parse:node-next first-line))
			       (parse:node-next line)))
			(line)
		   (etypecase line
		     (string (return))
		     (parse:region-node)
		     (docnode-line-node
		      (setq elements
			    (append elements
				    (line-to-doc line))))))))
	     (setq items (append items `((paragraph ,elements))))))
	  (iter (parse:node-next item-node)))))
    `(list ,items)))

(defun indented-para-to-doc (para)
  "Return a doc node for indented paragraph $para."
  (let ((elements))
    (iterate iter ((line (parse:node-content para)))
      (when line
	;; As for normal paragraph.
	(etypecase line
	  (parse:region-node)
	  (docnode-line-node
	   (setq elements
		 (append elements
			 (line-to-doc line)))))
	(iter (parse:node-next line))))
    `(paragraph ,elements)))

(defun docnode-parse-to-doc (doc-node &optional title)
  "Convert docnode parse node $doc-node to a documentation list, adding
   $title if given."
  ;(print-docnode doc-node)
  (collect ((doc))
    (if title (doc `(table ((row (((string . ,title))))) :heading)))
    (while ((block (parse:node-content doc-node) (parse:node-next block)))
	   (block)
      ;(format t "block ~A~%" (type-of block))
      (etypecase block
	(parse:region-node)  ;; Assume epsilon (empty `or' in :docnode rule).
	(docnode-line-end-node)
	(docnode-block-node
	 (iterate iter ((subblock (parse:node-content block)))
	   (when subblock
	     (etypecase subblock
	       (docnode-menu-node
		(collect ((row))
		  (iterate do-menu ((menu-item (parse:node-content subblock)))
		    (etypecase menu-item
		      (parse:region-node)
		      (docnode-menu-separator-node
		       (row `(row (((string . ""))
				   ((string . "")))
				  :menu-separator)))
		      (docnode-menu-item-node
		       (let ((item-text
			      (parse:node-next
			       (parse:node-content menu-item))))
			 (row
			  `(row (((ref ((string
					 . ,(filter-ref
					     (parse:node-content
					      menu-item))))))
				 ((string . ,(etypecase item-text
					       (docnode-line-end-node "")
					       (parse:region-node
						(parse:node-content
						 item-text))))))
				:menu)))))
		    (if (parse:node-next menu-item)
			(do-menu (parse:node-next menu-item))))
		  (doc (list 'table (row) :menu))))
	       (docnode-heading-node
		(let* ((heading (parse:node-content
				 (parse:node-content
				  (parse:node-next
				   (parse:node-content subblock)))))
		       (length (length heading)))
		  (and (> length 3)
		       (string= heading " ==" :start1 (- length 3))
		       (setq heading (subseq heading 0 (- length 3))))
		  (doc `(section () :section ,heading 1))))
	       (docnode-list-node
		(let ((items))
		  (iterate do-item ((item-node (parse:node-content subblock)))
		    (let ((elements))
		      (etypecase item-node
			(parse:region-node)
			(docnode-list-item-node
			 (let ((first-line
				(parse:node-next
				 (parse:node-content item-node))))
			   (setq elements
				 (append elements
					 (line-to-doc first-line)))
			   (when (parse:node-next first-line)
			     (while ((line (parse:node-content
					    (parse:node-next first-line))
					   (parse:node-next line)))
				    (line)
			       (etypecase line
				 (string (return))
				 (parse:region-node)
				 (docnode-line-node
				  (setq elements
					(append elements
						(line-to-doc line))))))))
			 (setq items (append items
					     `((paragraph ,elements)))))
			(docnode-line-end-node)))
		    (if (parse:node-next item-node)
			(do-item (parse:node-next item-node))))
		  (doc `(list ,items))))
	       (docnode-description-node
		(collect ((rows))
		  (iterate do-item ((item-node (parse:node-content subblock)))
		    (let* ((line (parse:node-next
				  (parse:node-content item-node)))
			   (name (line-to-doc line)))
		      (collect ((description))
			(iterate do-sub-item ((sub-item (parse:node-next
							 (parse:node-next line))))
			  (when sub-item
			    (etypecase sub-item
			      (docnode-line-end-node)
			      (docnode-indented-list-node
			       (description (indented-list-to-doc sub-item)))
			      (docnode-indented-para-node
			       (description (indented-para-to-doc sub-item))))
			    (do-sub-item (parse:node-next sub-item))))
			(rows `(row (,name ,(description))))))
		    (if (parse:node-next item-node)
			(do-item (parse:node-next item-node))))
		  (doc `(description ,(rows)))))
	       (docnode-verbatim-node
		(collect ((elements))
		  (while ((line (parse:node-content subblock)
				(parse:node-next line)))
			 (line)
		    (etypecase line
		      (docnode-verbatim-line-node
		       (elements
			`(string . ,(concat (parse:node-content
					     (parse:node-content
					      (parse:node-next
					       (parse:node-content line))))
					    (string #\newline)))))))
		  ;(format t "eles ~A~%" (elements))
		  (if (elements)
		      (doc (list 'verbatim (elements))))))
	       (docnode-paragraph-node
		(let ((elements))
		  (loop for line = (parse:node-content subblock)
		    then (parse:node-next line)
		    while line do
		    (etypecase line
		      (docnode-line-node
		       (setq elements
			     (append elements
				     (line-to-doc line))))))
		  ;(format t "eles ~A~%" (elements))
		  (if elements
		      (doc (list 'paragraph elements))))))
	     (iter (parse:node-next subblock)))))))
    ;(format t "doc ~A~%" (doc))
    (doc)))

;;; Public.
;;;
(defun docnode-to-doc (name)
  "Convert docnode $name to a documentation list."
  (let ((parse (with-input-from-string
		   (parse:*stream* (lisp::docnode-content
				    (getstring name
					       lisp::*documentation*)))
		 (let ((parse:*streams* (list (list parse:*stream*))))
		   (parse-docnode)))))
    (docnode-parse-to-doc parse name)))

;;; Public.
;;;
(defun docnode-string-to-doc (string)
  "Convert $string to a documentation list."
  (let ((string-length (length string)))
    (if (plusp string-length)
	(let ((parse (with-input-from-string
			 (parse:*stream*
			  (if (char= (char string (1- string-length))
				     #\newline)
			      string
			      (concatenate 'string
					   string
					   (string #\newline))))
		       (let ((parse:*streams* (list (list parse:*stream*))))
			 (parse-docnode)))))
	  (docnode-parse-to-doc parse)))))

#|
(with-input-from-string
    (parse:*stream* (lisp::docnode-content
		     (getstring "A Test Node"
				lisp::*documentation*)))
  (let ((parse:*streams* (list (list parse:*stream*))))
    (parse-docnode)))

(typep (make-docnode-string-node) 'parse:node)
|#
