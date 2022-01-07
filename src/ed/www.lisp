;;; w3m interface.

(in-package "ED")


;;; Structure.

(defmode "WWW" :major-p nil
  :precedence 4.0
  :documentation
  "World Wide Web browsing mode.")

(defevar "WWW Home"
  "WWW browsing home page."
  :value "http://localhost/")

(make-modeline-field
 :name :www-title :replace t
 :function
 #'(lambda (buffer window)
     (declare (ignore buffer window))
     ;; FIX bound-p or hang when www and some other buffer in other win
     (if (editor-bound-p 'www-title) (value www-title) "")))


;;; Functions.

(eval-when (compile eval load)

  (parse:defparser
   `((:html-element        #\< "" ;(or :html-space "")?
			   (or :html-mark :html-name-anchor :html-tag))

     ;; FIX
     (:html-name-anchor    :html-anchor
			   :html-space :html-name-attribute
			   (or :html-attributes "")
			   (or :html-space "")
			   #\>)

     ;; single tag, like <br>
     (:html-mark           (or :html-br
			       :html-_id
			       :html-title-alt
			       :html-link)
			   (or :html-attributes "")
			   (or :html-space "")
			   #\>)

     (:html-tag            (or :html-bold
			       :html-anchor
			       :html-_symbol
			       :html-u
			       :html-pre-int
			       :html-nobr
			       :html-img-alt)
			   (or :html-attributes "")
			   (or :html-space "")
			   #\>
			   (or :html-text "")
			   :html-tag-end)

     (:html-tag-end        "</"
			   (or :html-bold
			       :html-anchor
			       :html-_symbol
			       :html-u
			       :html-pre-int
			       :html-nobr
			       :html-img-alt)
			   #\>)

     (:html-text             (many (or :html-element
				       (group (many :html-text-char)))))

     (:html-attributes       (many :html-space :html-attribute))
     (:html-name-attribute   (or "name" "NAME")
			     #\= (or #\newline "")
			     (or :html-quoted-value :html-value))
     (:html-attribute        (group (many :html-tag-char))
			     #\= (or #\newline "")
			     (or :html-quoted-value :html-value))
     (:html-value            (group (many :html-tag-char)))
     (:html-quoted-value     #\"
			     (group (many (cond (if (eq parse:ch #\") nil t))))
			     #\")

     (:html-tag-char    (cond (if (or (eq parse:ch #\>)
				      (eq parse:ch #\ )
				      (eq parse:ch #\=)) nil t)))
     (:html-text-char   (cond (if (or (eq parse:ch #\<)) nil t)))

     (:html-bold        (or #\B #\b))
     (:html-u           (or #\U #\u))
     (:html-br          "BR")
     (:html-_id         "_id")
     (:html-pre-int     "pre_int")
     (:html-nobr        "nobr")
     (:html-_symbol     "_SYMBOL")
     (:html-anchor      (or #\A #\a))
     (:html-img-alt     "img_alt")
     (:html-link        "link")
     (:html-title-alt   "title_alt")

     (:html-space       (group (many (or #\  #\newline #\tab)))))
   :bufferp t)

  ) ; eval-when

(defun region-node-length (node)
  "Return the length of the region-node Node."
  (let ((content (parse:node-content node)))
    (etypecase content
      (region
       (count-characters content))
      (string
       (length content)))))

(declaim (inline html-space-length))

(defun html-space-length (node)
  "Return the length of the html-space-node Node."
  (region-node-length (parse:node-content node)))

(declaim (inline html-name-attribute-length))

(defun html-name-attribute-length (node)
  "Return the length of the html-name-attribute-node Node."
  (or (eq (type-of node) 'html-name-attribute-node)
      (editor-error "FIX"))
  (let ((content (parse:node-content node)))
    (+ (region-node-length content)
       1 ; =
       (if (eq (type-of (setq content
			      (parse:node-next (parse:node-next content))))
	       'char-node)
	   1
	   0)
       (if (eq (type-of (setq content
			      (parse:node-next content)))
	       'html-value)
	   (count-characters (parse:node-content (parse:node-content content)))
	   (+ (count-characters (parse:node-content
				 (parse:node-next
				  (parse:node-content content))))
	      2)))))

(declaim (inline html-attributes-length))

(defun html-attributes-length (attribs-node)
  "Return the length of the html-attributes-node Node."
  (let ((count 0))
    (loop
      for node = (parse:node-content attribs-node) then (parse:node-next node)
      while node do
      (incf count (html-space-length node))
      (setq node (parse:node-next node))
      (incf count (attribute-pair-length node)))
    count))

(declaim (inline attribute-pair-length))

(defun attribute-pair-length (attribute-node)
  "Return the length of the entire Attribute-node."
  (let* ((node (parse:node-content attribute-node))
	 (chars (1+ (count-characters (parse:node-content node)))))
    (setq node (parse:node-next (parse:node-next node)))
    (if (eq (type-of node) 'char-node)
	(incf chars))
    (let ((attrib-value (parse:node-next node)))
      (etypecase attrib-value
	(html-value-node
	 (let ((val (parse:node-content (parse:node-content attrib-value))))
	   (+ (count-characters val) chars)))
	(html-quoted-value-node
	 (let ((val (parse:node-content
		     (parse:node-next (parse:node-content attrib-value)))))
	   (+ (count-characters val) chars 2)))))))

(defun attribute-length-and-value (attribute-node)
  "Return the region in value node in Attribute-node and the length of the
   region including any quotation marks."
  (let ((attrib-value (parse:node-next
		       (parse:node-next
			(parse:node-next
			 (parse:node-content attribute-node))))))
    (typecase attrib-value
      (html-value-node
       (let ((val (parse:node-content (parse:node-content attrib-value))))
	 (values (count-characters val) val)))
      (html-quoted-value-node
       (let ((val (parse:node-content
		   (parse:node-next (parse:node-content attrib-value)))))
	 (values (+ (count-characters val) 2) val))))))

(defcommand "Reset Profile Time" ()
  "Reset the profiling time."
  (profile::reset-time))

(defcommand "Report Profile Time" ()
  "Report the profiling time."
  (with-pop-up-display (*trace-output*)
    (profile::report-time)))

(defmacro www-render-nodes (mark nodes buffer)
  "Render Nodes starting at Mark."
  `(loop for node = ,nodes then (parse:node-next node) while node do
     (etypecase node
       (parse:region-node
	(character-offset ,mark (count-characters (parse:node-content node))))
       (html-element-node
	(www-render-html-element-node ,mark
				      (parse:node-next
				       (parse:node-content node))
				      ,buffer)))))

(defmacro www-render-html-element-node-macro (mark buffer)
  "Render HTML element node."
  `(progn
     (delete-characters ,mark (if (eq (type-of node) 'html-space-node)
				  (1+ (html-space-length node))
				  1))
     (setq node (parse:node-next node))
     (etypecase node
       (html-mark-node
	(setq node (parse:node-content node))
	(etypecase node
	  (html-_id-node
	   (delete-characters ,mark 7) ; "_id id="
	   ;; FIX may need to account for html-space-nodes
	   (multiple-value-bind (length value)
				(attribute-length-and-value
				 (parse:node-next (parse:node-content (parse:node-next node))))
	     (delete-characters ,mark length)
	     (push (cons (region-to-string value) (copy-mark ,mark))
		   (variable-value 'www-ids :buffer ,buffer))))

	  (html-title-alt-node
	   (delete-characters ,mark 16) ; "title_alt title="
	   ;; FIX may need to account for html-space-nodes
	   (multiple-value-bind (length value)
				(attribute-length-and-value
				 (parse:node-next (parse:node-content (parse:node-next node))))
	     (delete-characters ,mark length)
	     (setf (variable-value 'www-title :buffer buffer)
		   (region-to-string value))))

	  (html-link-node
	   (setq node (parse:node-next node))
	   (delete-characters
	    ,mark
	    (+ 4 ; "link"
	       (if (eq (type-of node) 'html-attributes-node)
		   (html-attributes-length node)
		   0)
	       (if (eq (type-of (parse:node-next node)) 'html-space-node)
		   (html-space-length (parse:node-next node))
		   0)))))
	(delete-characters ,mark 1)) ; >

       (html-name-anchor-node
	(setq node (parse:node-next (parse:node-content node)))
	(delete-characters ,mark
			   (+ 1 ; a
			      (html-space-length node)
			      (html-name-attribute-length (setq node (parse:node-next node)))
			      (progn
				(setq node (parse:node-next node))
				(if (eq (type-of node) 'html-attributes-node)
				    (html-attributes-length node)
				    0))
			      (if (eq (type-of (setq node (parse:node-next node))) 'html-space-node)
				  (html-space-length node)
				  0)
			      1))) ; >

       (html-tag-node
	(let* ((tag-node (parse:node-content node))
	       (text-node (parse:node-next (parse:node-next (parse:node-next
						 (parse:node-next tag-node))))))
	  ;	  (message "tag-node a ~A at ~A" (type-of tag-node) ,mark)
	  (etypecase tag-node
	    (html-pre-int-node
	     (delete-characters ,mark 8) ; pre_int>
	     (www-render-nodes ,mark (parse:node-content text-node) ,buffer)
	     (delete-characters ,mark 10)) ; </pre_int>

	    (html-nobr-node
	     (delete-characters ,mark 5) ; nobr>
	     (www-render-nodes ,mark (parse:node-content text-node) ,buffer)
	     (delete-characters ,mark 7)) ; </nobr>

	    (html-bold-node
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 1)
	     (delete-characters ,mark 2) ; b>
	     (www-render-nodes ,mark (parse:node-content text-node) ,buffer)
	     (delete-characters ,mark 4) ; </b>
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 0))

	    (html-u-node
	     (font-mark (mark-line ,mark) (mark-charpos ,mark)
			*underline-font*)
	     (delete-characters ,mark 2) ; u>
	     (www-render-nodes ,mark (parse:node-content text-node) ,buffer)
	     (delete-characters ,mark 4) ; </u>
	     (font-mark (mark-line ,mark) (mark-charpos ,mark)
			*original-font*))

	    (html-anchor-node
	     (delete-characters ,mark 1) ; a
	     (let ((url-region))
	       (do ((attrib (parse:node-content (parse:node-next tag-node))
			    (parse:node-next attrib)))
		   ((eq attrib nil))
		 (etypecase attrib
		   (html-space-node
		    (delete-characters ,mark 1))  ; FIX len of space
		   (html-attribute-node
		    (let* ((content (parse:node-content attrib))
			   (attrib-name (string-upcase (region-to-string
							(parse:node-content content)))))
		      (cond
		       ((string= attrib-name "HREF")
			(multiple-value-bind (len value)
					     (attribute-length-and-value attrib)
			  (setq url-region value)
			  (delete-characters
			   ,mark
			   (+ 5
			      (if (eq (type-of (parse:node-next (parse:node-next content)))
				      'char-node)
				  1 0)
			      len))))
		       ((string= attrib-name "HSEQ")
			(delete-characters ,mark
					   (+ 5
					      (attribute-length-and-value
					       attrib))))
		       ((string= attrib-name "TITLE")
			(delete-characters ,mark
					   (+ 6
					      (attribute-length-and-value
					       attrib))))
		       ((string= attrib-name "ID")
			(delete-characters ,mark
					   (+ 3
					      (attribute-length-and-value
					       attrib))))
		       ((string= attrib-name "ACCESSKEY")
			(delete-characters ,mark
					   (+ 10
					      (attribute-length-and-value
					       attrib))))
		       ;; Target frame for link.
		       ((string= attrib-name "TARGET")
			(delete-characters ,mark
					   (+ 7
					      (attribute-length-and-value
					       attrib))))
		       ;; Link type of dest.
		       ((string= attrib-name "REL")
			(delete-characters ,mark
					   (+ 4
					      (attribute-length-and-value
					       attrib))))
		       ;; Link type of this doc w.r.t. dest.
		       ((string= attrib-name "REV")
			(delete-characters ,mark
					   (+ 4
					      (attribute-length-and-value
					       attrib))))
		       (t (message "FIX add attrib ~A" attrib-name)))))))
	       (delete-characters ,mark 1)
	       (let ((fmark (color-mark (mark-line ,mark)
					(mark-charpos ,mark)
					:special-form)))
		 (etypecase text-node
		   (parse:region-node)
		   (html-text-node
		    (www-render-nodes ,mark (parse:node-content text-node) ,buffer)))
		 (delete-characters ,mark 4)
		 (setf (variable-value 'www-hrefs :buffer buffer)
		       (cons (list fmark
				   (font-mark (mark-line ,mark)
					      (mark-charpos ,mark)
					      0)
				   url-region)
			     (variable-value 'www-hrefs :buffer ,buffer))))))

	    (html-_symbol-node
	     (delete-characters ,mark
				(+ 8  ; "_SYMBOL "
				   5  ; TYPE=
				   (attribute-length-and-value
				    (parse:node-next
				     (parse:node-content
				      (parse:node-next tag-node))))
				   1)) ; >
	     ;; FIX should perhaps recurse
	     (do ((text-part (parse:node-content text-node) (parse:node-next text-part)))
		 ((eq text-part nil))
	       (etypecase text-part
		 (html-element-node) ;; FIX
		 (parse:region-node
		  (character-offset ,mark
				    (count-characters (parse:node-content text-part))))))
	     (delete-characters ,mark 10)) ; "</_SYMBOL>"
	    (html-img-alt-node
	     (color-mark (mark-line ,mark) (mark-charpos ,mark) :function)
	     (delete-characters
	      ,mark
	      (+ 7 ; "img_alt"
		 (let ((chars 0))
		   (loop
		     for pair = (parse:node-content (parse:node-next tag-node))
		     then (parse:node-next pair)
		     while pair
		     do
		     (incf chars
			   (count-characters (parse:node-content
					      (parse:node-content pair))))
		     (setq pair (parse:node-next pair))
		     (incf chars (attribute-pair-length pair)))
		   chars)
		 1)) ; >
	     ;; FIX should perhaps recurse
	     (loop
	       for text-part = (parse:node-content text-node) then (parse:node-next text-part)
	       while text-part do
	       (etypecase text-part
		 (html-element-node) ;; FIX
		 (parse:region-node
		  (character-offset ,mark
				    (count-characters (parse:node-content text-part))))))
	     (delete-characters ,mark 10)
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 0))))))))  ; "</img_alt>"

(defun www-render-html-element-node (mark node buffer)
  "Render HTML element node."
  (www-render-html-element-node-macro mark buffer))

; (defun free-node-marks (node)
;   "Call delete-mark on the marks in Node."
;   (loop
;     for node = (parse:node-content nodes) then (parse:node-next node)
;     while node do
;     (typecase node
;       (parse:region-node
;        (delete-region

(declaim (inline search-for-html-element))

(defvar *www-a0-pattern-forward* (new-search-pattern :character :forward #\ ))
; FIX load error
;(defvar *www-82-c1-pattern-forward* (new-search-pattern :string-sensitive :forward "‚Á"))
(defvar *www-<-pattern-forward* (new-search-pattern :character :forward #\<))
(defvar *www-&-pattern-forward* (new-search-pattern :character :forward #\&))

(defun search-for-html-element (parse:*mark* pos)
  "Search from parse:*mark* with buffer-parse-html-element.  Pos must be another
   mark at parse:*mark*."
  (let ((node))
    (loop
      (or (find-pattern pos *www-<-pattern-forward*)
	  (return-from search-for-html-element nil))
      (move-mark parse:*mark* pos)
      (when (setq node (buffer-parse-html-element))
	(move-mark parse:*mark* pos)
	(return-from search-for-html-element node))
      (or (mark-after pos)
	  (return-from search-for-html-element nil)))))

(defun www-parse-string (parse:*mark* string)
  "Parse String at parse:*mark*, moving parse:*mark* across any matched characters."
  (let ((len (1- (length string))))
    (loop for index from 0 upto (1- len) do
      (or (eq (next-character parse:*mark*) (char string index))
	  (return-from www-parse-string nil))
      (mark-after parse:*mark*))
    (eq (next-character parse:*mark*) (char string len))))

(declaim (inline www-finish-halfdump buffer-parse-html-element))

;; FIX ::*mark  :*mark
(defun www-finish-halfdump (parse:*mark* buffer)
  "Convert the halfdump tags starting at *mark*."
  (let ((start (copy-mark parse:*mark*))
	(parse:*marks* `((,parse:*mark*)))
	(pos (copy-mark parse:*mark*)))
    (replace-pattern parse:*mark* *www-a0-pattern-forward* " ")
    ;; FIX a guess, to what should this translate?
    ;               this should translate to what?
;    (replace-pattern parse:*mark* *www-82-c1-pattern-forward* (string #\newline))
    (loop
      (let ((node (parse:node-next
		   (parse:node-content (or (search-for-html-element parse:*mark* pos)
					   (return-from nil))))))
	(www-render-html-element-node-macro parse:*mark* buffer))
      (move-mark pos parse:*mark*))
    (move-mark parse:*mark* start)
    (loop
      (or (find-pattern parse:*mark* *www-&-pattern-forward*) (return))
      (mark-after parse:*mark*)
      (case (next-character parse:*mark*)
	(#\a
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\m
	    (mark-after parse:*mark*)
	    (when (eq (next-character parse:*mark*) #\p)
	      (mark-after parse:*mark*)
	      (when (eq (next-character parse:*mark*) #\;)
		(mark-after parse:*mark*)
		(delete-characters parse:*mark* -4))))))
	(#\g
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\t
	    (mark-after parse:*mark*)
	    (when (eq (next-character parse:*mark*) #\;)
	      (delete-characters parse:*mark* -3)
	      (setf (next-character parse:*mark*) #\>)))))
	(#\h
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\e
	    (mark-after parse:*mark*)
	    (when (www-parse-string parse:*mark* "llip;")
	      (delete-characters parse:*mark* -7)
	      (setf (next-character parse:*mark*) #\.)
	      (insert-string parse:*mark* "..")))))
	(#\l
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\d
	    (mark-after parse:*mark*)
	    (when (www-parse-string parse:*mark* "quo;")
	      (delete-characters parse:*mark* -6)
	      (setf (next-character parse:*mark*) #\")))
	   (#\t
	    (mark-after parse:*mark*)
	    (when (eq (next-character parse:*mark*) #\;)
	      (delete-characters parse:*mark* -3)
	      (setf (next-character parse:*mark*) #\<)))))
	(#\m
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\d
	    (mark-after parse:*mark*)
	    (when (www-parse-string parse:*mark* "ash;")
	      (delete-characters parse:*mark* -6)
	      (setf (next-character parse:*mark*) #\-)
	      (insert-character parse:*mark* #\-)))
	   (#\i
	    (mark-after parse:*mark*)
	    (when (www-parse-string parse:*mark* "nus;")
	      (delete-characters parse:*mark* -6)
	      (setf (next-character parse:*mark*) #\-)))))
	(#\n
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\d
	    (mark-after parse:*mark*)
	    (when (www-parse-string parse:*mark* "ash;")
	      (delete-characters parse:*mark* -6)
	      (setf (next-character parse:*mark*) #\-)))))
	(#\r
	 (mark-after parse:*mark*)
	 (case (next-character parse:*mark*)
	   (#\d
	    (mark-after parse:*mark*)
	    (when (www-parse-string parse:*mark* "quo;")
	      (delete-characters parse:*mark* -6)
	      (setf (next-character parse:*mark*) #\")))))
	))
    (let ((buffer (line-buffer (mark-line start))))
      (when (variable-value 'www-hrefs :buffer buffer)
	(setf (variable-value 'www-hrefs :buffer buffer)
	      (nreverse (variable-value 'www-hrefs :buffer buffer)))))
    (delete-mark start)
    (delete-mark pos)))

(declaim (inline execute-w3m))

(defun execute-w3m (page stream &key source)
  (ext:run-program
   "w3m"
;   (list "-dump=half-buffer" page)   maybe for w3mmee
   (list (if source "-dump_source" "-halfdump") page)
   :output stream))

#|
(defun test-render ()
  "Render halfdump in current buffer."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "View") t)
    (setf (buffer-minor-mode buffer "WWW") t)
    (defevar "WWW Hrefs"
      "List of href lists, sorted from first href in buffer to last."
      :buffer buffer
      :value '())
    (defevar "WWW URL"
      "URL of page in buffer."
      :buffer buffer
      :value "FIX test")
    (defevar "WWW Title"
      "Title of page in buffer."
      :buffer buffer
      :value "")
    (setf (value view-return-function) #'(lambda ()))
    (let ((mark (copy-mark (buffer-start-mark buffer))))
      (setf (mark-kind (current-point)) :right-inserting)
      (www-finish-halfdump mark)
      (buffer-start mark)
      (delete-mark mark))))
|#

(defun www-refresh (buffer location
			   &key source (add-to-history t)
			   (start (buffer-start-mark buffer)))
  "Refresh WWW Buffer."
  (setf (variable-value 'www-hrefs :buffer buffer) '())
  (setf (variable-value 'www-url :buffer buffer) location)
  (setf (variable-value 'www-source :buffer buffer) source)
  (setf (variable-value 'www-ids :buffer buffer) '())
  (with-writable-buffer (buffer)
    (delete-region (region start (buffer-end-mark buffer)))
    (let ((mark (copy-mark start)))
      (setf (mark-kind (buffer-point buffer)) :right-inserting)
      (with-output-to-mark (s mark)
	(execute-w3m location s :source source))
      (move-mark mark start)
      (or source (www-finish-halfdump mark buffer))
      ;; FIX only in region from start
      (flush-trailing-whitespace buffer)
      (move-mark mark start)
      ;; Move mark.
      (or source
	  (let ((pos (position #\/ location :from-end t)))
	    (when pos
	      (let ((id-pos (position #\# location :start pos)))
		(when id-pos
		  (let ((assoc (assoc (subseq location (1+ id-pos))
				      (variable-value 'www-ids :buffer buffer)
				      :test #'string=)))
		    (when assoc
		      (move-mark (buffer-point buffer) (cdr assoc))
		      (with-mark ((mark (buffer-point buffer)))
			(move-mark (window-display-start
				    ;; FIX may be called outside buffer window
				    (current-window))
				   (line-start mark))))))))))
      (when add-to-history
	(let* ((current-history (variable-value 'current-www-history
						:buffer buffer))
	       (history-element (cons location ()))
	       (new-history (cons history-element current-history)))
	  (if current-history
	      ;; Link the next "slot" of the previous element to the new
	      ;; history, for moving forward through the history.
	      (setf (cdar current-history) new-history))
	  (setf (variable-value 'current-www-history :buffer buffer)
		new-history)
	  (setf (variable-value 'www-history :buffer buffer)
		new-history)))
      (delete-mark mark))))

(defun www-url-at-mark (mark)
  "Return the URL and type (:mailto or :http) of any resource at $mark."
  (let ((buffer (line-buffer (mark-line mark))))
    (or (editor-bound-p 'www-hrefs :buffer buffer)
	(editor-error "Current buffer must be a WWW buffer."))
    (dolist (ref (variable-value 'www-hrefs :buffer buffer))
      (when (and (mark>= mark (car ref))
		 (mark<= mark (cadr ref)))
	(let ((link (region-to-string (caddr ref))))
	  (return-from
	   nil
	   (cond ((search "://" link) (values link :http))
		 ((and (> (length link) 7)
		       (string= "mailto:" link :end1 7 :end2 7))
		  (values link :mailto))
		 ((eq (aref link 0) #\#)
		  (let* ((www-url (variable-value 'www-url :buffer buffer))
			 (pos (position #\# www-url)))
		    (if pos (setq www-url (subseq www-url 0 pos)))
		    (values (format nil "~A~A" www-url link) :http)))
		 ((eq (aref link 0) #\/)
		  (let* ((www-url (variable-value 'www-url :buffer buffer))
			 (index (search "://" www-url)))
		    (when index
		      (let ((pos (position #\/ www-url
					   :start (+ index 3))))
			(values
			 (if pos
			     (format nil "~A~A" (subseq www-url 0 pos) link)
			     (format nil "~A/~A" www-url link))
			 :http)))))
		 (t
		  (let ((www-url (variable-value 'www-url :buffer buffer)))
		    ;; Return URL up to the last backslash.
		    (setq www-url (subseq www-url
					  0 (position #\/ www-url
						      :from-end t)))
		    (values (format nil "~A/~A" www-url link)
			    :http))))))))))

(defun setup-www-evariables (buffer location)
  (defevar "WWW Hrefs"
    "List of href lists, sorted from first href in buffer to last."
    :buffer buffer
    :value '())
  (defevar "WWW URL"
    "URL of page in buffer."
    :buffer buffer
    :value location)
  (defevar "WWW Title"
    "Title of page in buffer."
    :buffer buffer
    :value "")
  (defevar "WWW IDs"
    "List of IDs (named positions) in buffer."
    :buffer buffer
    :value '())
  (defevar "WWW Source"
    "t if buffer contains page source, else nil."
    :buffer buffer
    :value ())
  (defevar "WWW History"
    "A history of web pages that have been displayed in this buffer."
    :buffer buffer
    :value ())
  (defevar "Current WWW History"
    "The history from the current page to the beginning."
    :buffer buffer
    :value ()))

(defun www-buffer (buffer start location &optional source)
  (or (editor-bound-p 'www-hrefs)
      (setup-www-evariables buffer location))
  (www-refresh buffer location :start start :source source))


;;; Commands.

(defcommand "WWW" (p url buffer-name)
  "Browse the World Wide Web.  With a prefix create a new buffer even if a
   WWW buffer already exists."
  "Browse Url, prompting for an URL if Url in nil.  Use Buffer-name if it
   is true.  If P is true then create a new buffer even if a WWW buffer of
   the correct name already exists."
  (let ((location (if url
		      (if (eq (type-of url) 'pathname)
			  (namestring url)
			  url)
		      (prompt-for-string
		       :prompt "URL: "
		       :default (if (editor-bound-p
				     'www-url
				     :buffer (current-buffer))
				    (value www-url)
				    (value www-home))))))
    (let* ((name (or buffer-name "WWW"))
	   (new (if p
		    (make-unique-buffer name
					:modeline-fields
					(append (value default-modeline-fields)
						(list (modeline-field :www-title)))
					:modes '("Fundamental" "View" "WWW"))
		    (make-buffer name
				 :modeline-fields
				 (append (value default-modeline-fields)
					 (list (modeline-field :www-title)))
				 :modes '("Fundamental" "View" "WWW"))))
	   (buffer (or new (getstring name *buffer-names*))))
      (change-to-buffer buffer)
      (when new
	(setup-www-evariables buffer location)
	(setf (value view-return-function) #'(lambda ())))
      (www-refresh buffer location)
      (update-modeline-field buffer (current-window)
			     (modeline-field :www-title)))))

(defcommand "WWW in Current Buffer" (p url)
  "Browse to a prompted URL in the current buffer"
  "Browse to Url in the current buffer, prompting for an URL if Url is
   ()."
  (declare (ignore p))
  (let ((location (if url
		      (if (eq (type-of url) 'pathname)
			  (namestring url)
			  url)
		      (prompt-for-string
		       :prompt "URL: "
		       :default
		       (if (value www-url)
			   (value www-url)
			   (value www-home))))))
    (www-refresh (current-buffer) location)
    (update-modeline-field (current-buffer) (current-window)
			   (modeline-field :www-title))))

(defcommand "WWW Externally" ()
  "Browse a prompted URL in an external program."
  (view-url (prompt-for-string
	     :prompt "URL: "
	     :default (value www-home))))

(defcommand "Forward WWW Page" ()
  "Show the next page from the history of pages."
  (let* ((hist (value current-www-history))
	 (next (if hist (cdar hist))))
    (if next
	(progn
	  (setv current-www-history next)
	  (www-refresh (current-buffer) (caar next) :add-to-history nil))
	(message "End of history."))))

(defcommand "Backward WWW Page" ()
  "Show the previous page from the history of pages."
  (let* ((hist (value current-www-history))
	 (prev (if hist (cdr hist))))
    (if prev
	(progn
	  (setv current-www-history prev)
	  (www-refresh (current-buffer) (caar prev) :add-to-history nil))
	(message "Beginning of history."))))

(defcommand "Next WWW Reference" ()
  "Move point to the next URL."
  (let ((point (current-point)))
    (dolist (ref (value www-hrefs))
      (when (mark< point (car ref))
	(move-mark point (car ref))
	(return-from nil)))))

;; FIX Previous?
(defcommand "Next WWW Reference" ()
  "Move point to the next URL."
  (let ((point (current-point)))
    (dolist (ref (value www-hrefs))
      (when (mark< point (car ref))
	(move-mark point (car ref))
	(return-from nil)))))

(defcommand "WWW URL at Point" ()
  "Print the URL at Point."
  (let ((url (www-url-at-mark (current-point))))
    (if url
	(progn
	  (message "URL: ~A" url)
	  (push-kill (copy-region (string-to-region url))))
	(message "URL: "))))

(defcommand "WWW Resource from Point" (p)
  "Follow the URL at Point.  With a prefix create a new WWW buffer."
  "Follow the URL at Point.  If P is true create a new WWW buffer."
  (multiple-value-bind (url type)
		       (www-url-at-mark (current-point))
    (when url
      (case type
	(:image
	 (view url))
	(:http
	 (www-command p url (buffer-name (current-buffer))))
	(:mailto
	 (send-message-command)
	 (let ((point (buffer-point (current-buffer))))
	   (insert-string point (subseq url 7))
	   (buffer-end point)))))))

(defcommand "WWW Resource from Point in New Buffer" ()
  "Open the URL at Point in a new buffer."
  (www-resource-from-point-command t))

(defcommand "WWW Resource from Point Externally" ()
  "If there is an URL at point then open the URL in an external program,
   otherwise open the current URL in an external program."
  (multiple-value-bind (url type)
		       (www-url-at-mark (current-point))
    (if url
	(case type
	  (:image
	   (view url))
	  (:http
	   (view-url url))
	  (:mailto
	   (view url)))
	(view-url (value www-url)))))

(defcommand "WWW Refresh" ()
  "Refresh WWW page in current buffer."
  (if (value www-url)
      (www-refresh (current-buffer) (value www-url))
      (editor-error "\"WWW URL\" is nil.")))

(defcommand "WWW Home" ()
  "Browse URL in \"WWW Home\"."
  (if (value www-home)
      (www-refresh (current-buffer) (value www-home))
      (editor-error "\"WWW Home\" is nil.")))

(defcommand "Save WWW URL" ()
  "Save the WWW URL of the current page to the kill ring."
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (message "Saved URL: ~A" (value www-url))
  (ring-push (copy-region (string-to-region (value www-url)))
	     *kill-ring*))

(defcommand "Switch to WWW" ()
  "Switch to WWW buffer if there is one, else WWW a prompted URL.  If the
   current buffer go to a prompted URL in the current buffer."
  (let ((buffer (getstring "WWW" *buffer-names*)))
    (if buffer
	(if (eq buffer (current-buffer))
	    (www-command)
	    (change-to-buffer buffer))
	(www-command))))

(defcommand "View URL" ()
  "View a prompted URL externally."
  (view-url (prompt-for-string
	     :default (url-at-point)
	     :trim t
	     :prompt "Find URL externally: "
	     :help "URL to browse externally.")))

(defcommand "Encyclopedia" (p)
  "View a prompted URL.  With a prefix, view externally."
  (let ((url (concatenate 'simple-string
			 "http://en.wikipedia.org/wiki/"
			 (prompt-for-string
			  :default (or (word-at-point) "")
			  :trim t
			  :prompt (if p
				      "External encyclopedia: "
				      "Encyclopedia: ")
			  :help "Word to open in encyclopedia."))))
    (if p (view-url url) (www-command () url))))

(defcommand "Copy WWW Buffer" (p (buffer (current-buffer)))
  "Create and switch to a copy of the current buffer, which must be a WWW
   Buffer."
  (declare (ignore p))
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (copy-buffer-command nil buffer)
  ;; Refresh to update any font marks in www-hrefs.
  (www-refresh-command))

(defcommand "WWW Page Info" ()
  "Pop up info about the current page."
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (with-pop-up-display (stream)
    (ext:run-program
     "w3m" (list "-dump_head" (value www-url))
     :output stream)))

(defcommand "WWW Toggle Source" (p (buffer (current-buffer)))
  "Toggle display of page source."
  (declare (ignore p))
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (www-refresh buffer (value www-url)
	       :source (if (value www-source) nil t)))


;;;; Links (like bookmarks).

(defun highlight-links-line (line chi-info)
  (or (zerop (line-length line))
      (if (member (next-character (mark line 0)) '(#\tab #\space))
	  (chi-mark line 0 *comment-font* :comment chi-info))))

(defun highlight-links-buffer (buffer)
  (highlight-chi-buffer buffer highlight-links-line))

(defun highlight-visible-links-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-links-line))

(defun setup-links-mode (buffer)
  (highlight-visible-links-buffer buffer)
  (setf (buffer-writable buffer) ())
  (pushnew '("Links" t highlight-visible-links-buffer) *mode-highlighters*))

(defmode "Links" :major-p t
  :setup-function 'setup-links-mode
  :documentation "Bookmark browsing mode.")

(defcommand "Links" ()
  "Switch to the links buffer."
  (find-file-command () ":links")
  (setf (buffer-major-mode (current-buffer)) "Links"))

(defun line-link (line)
  (or (zerop (line-length line))
      (member (next-character (mark line 0)) '(#\tab #\space))
      (line-string line)))

(defcommand "Links WWW" ()
  "Browse the current link in the web browser."
  (let ((link (line-link (current-line))))
    (if link (www-command () link))))

(defcommand "Links WWW Externally" ()
  "Browse the current link in a external web browser."
  "Switch to the links buffer."
  (let ((link (line-link (current-line))))
    (if link (view-url link))))

(defcommand "Links Find" ()
  "Find the current link."
  (let ((link (line-link (current-line))))
    (if link (find-command () link))))

(defcommand "Links Find Next Window" ()
  "Find the current link."
  (let ((link (line-link (current-line))))
    (when link
      (if (eq (next-window (current-window)) (current-window))
	  (split-window-command)
	  (next-window-command))
      (find-command () link))))

(defcommand "Links Edit" ()
  "Edit the buffer."
  (let ((buffer (current-buffer)))
    (setf (buffer-major-mode buffer) "Fundamental")
    (setf (buffer-writable buffer) t)
    (unwind-protect
	(do-recursive-edit)
      (and (buffer-writable buffer)
	   (buffer-modified buffer)
	   (save-file-command () buffer))
      (setf (buffer-writable buffer) ())
      (setf (buffer-major-mode buffer) "Links"))))

(defcommand "Links Next Link" (p)
  "In a Links buffer, move point to the next link.  With a prefix move that
   many links forwards (backwards if prefix is negative)."
  (let ((point (current-point))
	(p (or p 1)))
    (cond ((zerop p))
	  ((minusp p)
	   (while ((count p))
		  ((and (< count 0)
			(line-offset point -1 0)))
	     (or (member (next-character point) '(#\newline #\space #\tab))
		 (incf count))))
	  (t
	   (while ((count (or p 1)))
		  ((and (> count 0)
			(line-offset point 1 0)))
	     (or (member (next-character point) '(#\newline #\space #\tab))
		 (decf count)))))))

(defcommand "Links Previous Link" (p)
  "In a Links buffer, move point to the previous link.  With a prefix move
   that many links backwards (forwards if prefix is negative)."
  (links-next-link-command (- (or p 1))))
