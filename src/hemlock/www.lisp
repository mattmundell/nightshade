;;; -*- Log: hemlock.log; Package: Hemlock -*-

(in-package "HEMLOCK")


;;; Structure.

(defmode "WWW" :major-p nil
  :precedence 4.0
  :documentation
  "World Wide Web browsing mode.")

(defhvar "WWW Home"
  "WWW browsing home page."
  :value "http://localhost/")

(make-modeline-field
 :name :www-title :replace t
 :function
 #'(lambda (buffer window)
     (declare (ignore buffer window))
     ;; FIX bound-p or hang when www and some other buffer in other win
     (if (hemlock-bound-p 'www-title) (value www-title) "")))


;;; Functions.

(eval-when (compile eval load)
  
  (defparser `((:html-element        #\< (or :html-space "")
				     (or :html-mark :html-name-anchor :html-tag))
	       
	       ;; FIX
	       (:html-name-anchor    :html-anchor
				     :html-space :html-name-attribute
				     (or :html-attributes "")
				     (or :html-space "")
				     #\>)
	       
	       ;; single tag, like <br>
	       (:html-mark           (or :html-br
					 :html-title-alt
					 :html-link)
				     (or :html-attributes "")
				     (or :html-space "")
				     #\>)
	       
	       (:html-tag            (or :html-bold
					 :html-anchor
					 :html-_symbol
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
				       (group (many (cond (if (eq ch #\") nil t))))
				       #\")
	       
	       (:html-tag-char    (cond (if (or (eq ch #\>)
						(eq ch #\ )
						(eq ch #\=)) nil t)))
	       (:html-text-char   (cond (if (or (eq ch #\<)) nil t)))
	       
	       (:html-bold        (or #\B #\b))
	       (:html-br          "BR")
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
  (let ((content (node-content node)))
    (etypecase content
      (region
       (count-characters content))
      (string
       (length content)))))

(declaim (inline html-space-length))

(defun html-space-length (node)
  "Return the length of the html-space-node Node."
  (region-node-length (node-content node)))

(declaim (inline html-name-attribute-length))

(defun html-name-attribute-length (node)
  "Return the length of the html-name-attribute-node Node."
  (or (eq (type-of node) 'html-name-attribute-node)
      (editor-error "FIX"))
  (let ((content (node-content node)))
    (+ (region-node-length content)
       1 ; =
       (if (eq (type-of (setq content (node-next (node-next content)))) 'char-node) 1 0)
       (if (eq (type-of (setq content (node-next content))) 'html-value)
	   (count-characters (node-content (node-content content)))
	   (+ (count-characters (node-content (node-next (node-content content)))) 2)))))

(declaim (inline html-attributes-length))

(defun html-attributes-length (attribs-node)
  "Return the length of the html-attributes-node Node."
  (let ((count 0))
    (loop
      for node = (node-content attribs-node) then (node-next node) 
      while node do
      (incf count (html-space-length node))
      (setq node (node-next node))
      (incf count (attribute-pair-length node)))
    count))

(declaim (inline attribute-pair-length))

(defun attribute-pair-length (attribute-node)
  "Return the length of the entire Attribute-node."
  (let* ((node (node-content attribute-node))
	 (chars (1+ (count-characters (node-content node)))))
    (setq node (node-next (node-next node)))
    (if (eq (type-of node) 'char-node)
	(incf chars))
    (let ((attrib-value (node-next node)))
      (etypecase attrib-value
	(html-value-node
	 (let ((val (node-content (node-content attrib-value))))
	   (+ (count-characters val) chars)))
	(html-quoted-value-node
	 (let ((val (node-content
		     (node-next (node-content attrib-value)))))
	   (+ (count-characters val) chars 2)))))))

(defun attribute-length-and-value (attribute-node)
  "Return the region in value node in Attribute-node and the length of the
   region including any quotation marks."
  (let ((attrib-value (node-next
		       (node-next
			(node-next
			 (node-content attribute-node))))))
    (typecase attrib-value
      (html-value-node
       (let ((val (node-content (node-content attrib-value))))
	 (values (count-characters val) val)))
      (html-quoted-value-node
       (let ((val (node-content
		   (node-next (node-content attrib-value)))))
	 (values (+ (count-characters val) 2) val))))))

(defcommand "Reset Profile Time" (p)
  "Reset the profiling time."
  "Reset the profiling time."
  (declare (ignore p))
  (profile::reset-time))

(defcommand "Report Profile Time" (p)
  "Report the profiling time."
  "Report the profiling time."
  (declare (ignore p))
  (with-pop-up-display (*trace-output*)
    (profile::report-time)))

#|  
(eval-when (:execute)
  
  (profile::profile www-finish-halfdump www-command execute-w3m www-render-node www-render-nodes hi::search-by-parser buffer-parse-html-element search-for-html-element ext:run-program www-render-html-element-node copy-mark)
  (profile::unprofile www-finish-halfdump www-command execute-w3m www-render-node www-render-nodes hi::search-by-parser buffer-parse-html-element search-for-html-element ext:run-program www-render-html-element-node copy-mark)
  
  ) ; eval-when eval
|#

(defmacro www-render-nodes (mark nodes)
  "Render Nodes starting at Mark."
  `(loop for node = ,nodes then (node-next node) while node do
     ;     (message "loop nodes node a ~A at ~A" (type-of node) mark)
     (etypecase node
       (region-node
	(character-offset ,mark (count-characters (node-content node))))
       (html-element-node
	(www-render-html-element-node ,mark (node-next
					     (node-content node)))))))

(defmacro www-render-html-element-node-macro (mark)
  "Render HTML element node."
  `(progn
     ;     (message "rend a ~A at ~A" (type-of node) ,mark)
     (delete-characters ,mark (if (eq (type-of node) 'html-space-node)
				  (1+ (html-space-length node))
				  1))
     (setq node (node-next node))
     ;     (message "next a ~A at ~A" (type-of node) ,mark)
     (etypecase node
       (html-mark-node
	(setq node (node-content node))
	(etypecase node
	  (html-title-alt-node
	   (delete-characters ,mark 16) ; "title_alt title="
	   ;; FIX may need to account for html-space-nodes
	   (multiple-value-bind (length value)
				(attribute-length-and-value
				 (node-next (node-content (node-next node))))
	     (delete-characters ,mark length)
	     (setv www-title (region-to-string value))))
	  
	  (html-link-node
	   (setq node (node-next node))
	   (delete-characters
	    ,mark
	    (+ 4 ; "link"
	       (if (eq (type-of node) 'html-attributes-node)
		   (html-attributes-length node)
		   0)
	       (if (eq (type-of (node-next node)) 'html-space-node)
		   (html-space-length (node-next node))
		   0)))))
	(delete-characters ,mark 1)) ; >
       
       (html-name-anchor-node
	(setq node (node-next (node-content node)))
	(delete-characters ,mark
			   (+ 1 ; a
			      (html-space-length node)
			      (html-name-attribute-length (setq node (node-next node)))
			      (progn
				(setq node (node-next node))
				(if (eq (type-of node) 'html-attributes-node)
				    (html-attributes-length node)
				    0))
			      (if (eq (type-of (setq node (node-next node))) 'html-space-node)
				  (html-space-length node)
				  0)
			      1))) ; >
       
       (html-tag-node
	(let* ((tag-node (node-content node))
	       (text-node (node-next (node-next (node-next
						 (node-next tag-node))))))
	  ;	  (message "tag-node a ~A at ~A" (type-of tag-node) ,mark)
	  (etypecase tag-node
	    (html-pre-int-node
	     (delete-characters ,mark 8) ; pre_int>
	     (www-render-nodes ,mark (node-content text-node))
	     (delete-characters ,mark 10)) ; </pre_int>
	    
	    (html-nobr-node
	     (delete-characters ,mark 5) ; nobr>
	     (www-render-nodes ,mark (node-content text-node))
	     (delete-characters ,mark 7)) ; </nobr>
	    
	    (html-bold-node
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 1)
	     (delete-characters ,mark 2) ; b>
	     (www-render-nodes ,mark (node-content text-node))
	     (delete-characters ,mark 4) ; </b>
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 0))
	    
	    (html-anchor-node
	     (delete-characters ,mark 1) ; a
	     (let ((url-region))
	       (do ((attrib (node-content (node-next tag-node))
			    (node-next attrib)))
		   ((eq attrib nil))
		 (etypecase attrib
		   (html-space-node
		    (delete-characters ,mark 1))
		   (html-attribute-node
		    (let* ((content (node-content attrib))
			   (attrib-name (string-upcase (region-to-string
							(node-content content)))))
		      (if (string= attrib-name "HREF")
			  (multiple-value-bind (len value)
					       (attribute-length-and-value attrib)
			    (setq url-region value)
			    (delete-characters
			     ,mark
			     (+ 5
				(if (eq (type-of (node-next (node-next content)))
					'char-node)
				    1 0)
				len)))
			  (if (string= attrib-name "HSEQ")
			      (delete-characters ,mark
						 (+ 5
						    (attribute-length-and-value
						     attrib)))
			      (if (string= attrib-name "TITLE")
				  (delete-characters ,mark
						     (+ 6
							(attribute-length-and-value
							 attrib))))))))))
	       (delete-characters ,mark 1)
	       (let ((fmark (font-mark (mark-line ,mark)
				       (mark-charpos ,mark)
				       6)))
		 (etypecase text-node
		   (region-node)
		   (html-text-node
		    (www-render-nodes ,mark (node-content text-node))))
		 (delete-characters ,mark 4)
		 (setv www-hrefs
		       (cons (list fmark
				   (font-mark (mark-line ,mark)
					      (mark-charpos ,mark)
					      0)
				   url-region)
			     (value www-hrefs))))))
	    
	    (html-_symbol-node
	     (delete-characters ,mark
				(+ 8  ; "_SYMBOL "
				   5  ; TYPE=
				   (attribute-length-and-value
				    (node-next
				     (node-content
				      (node-next tag-node))))
				   1)) ; >
	     ;; FIX should perhaps recurse
	     (do ((text-part (node-content text-node) (node-next text-part)))
		 ((eq text-part nil))
	       (etypecase text-part
		 (html-element-node) ;; FIX
		 (region-node
		  (character-offset ,mark
				    (count-characters (node-content text-part))))))
	     (delete-characters ,mark 10)) ; "</_SYMBOL>"
	    (html-img-alt-node
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 2)
	     (delete-characters
	      ,mark
	      (+ 7 ; "img_alt"
		 (let ((chars 0))
		   (loop
		     for pair = (node-content (node-next tag-node))
		     then (node-next pair)
		     while pair
		     do
		     (incf chars
			   (count-characters (node-content 
					      (node-content pair))))
		     (setq pair (node-next pair))
		     (incf chars (attribute-pair-length pair)))
		   chars)
		 1)) ; >
	     ;; FIX should perhaps recurse
	     (loop
	       for text-part = (node-content text-node) then (node-next text-part)
	       while text-part do
	       (etypecase text-part
		 (html-element-node) ;; FIX
		 (region-node
		  (character-offset ,mark
				    (count-characters (node-content text-part))))))
	     (delete-characters ,mark 10)
	     (font-mark (mark-line ,mark) (mark-charpos ,mark) 0))))))))  ; "</img_alt>"

(defun www-render-html-element-node (mark node)
  "Render HTML element node."
  (www-render-html-element-node-macro mark))

; (defun free-node-marks (node)
;   "Call delete-mark on the marks in Node."
;   (loop
;     for node = (node-content nodes) then (node-next node)
;     while node do
;     (typecase node
;       (region-node
;        (delete-region

(declaim (inline search-for-html-element))

(defvar *www-a0-pattern-forward* (new-search-pattern :character :forward #\ ))
; FIX load error
;(defvar *www-82-c1-pattern-forward* (new-search-pattern :string-sensitive :forward "‚Á"))
(defvar *www-<-pattern-forward* (new-search-pattern :character :forward #\<))
(defvar *www-&-pattern-forward* (new-search-pattern :character :forward #\&))

(defun search-for-html-element (*mark* pos)
  "Search from *mark* with buffer-parse-html-element.  Pos must be another
   mark at *mark*."
  (let ((node))
    (loop
      (or (find-pattern pos *www-<-pattern-forward*)
	  (return-from search-for-html-element nil))
      (move-mark *mark* pos)
      (when (setq node (buffer-parse-html-element))
	(move-mark *mark* pos)
	(return-from search-for-html-element node))
      (or (mark-after pos)
	  (return-from search-for-html-element nil)))))

(defun www-parse-string (*mark* string)
  "Parse String at *mark*, moving *mark* across any matched characters."
  (let ((len (1- (length string))))
    (loop for index from 0 upto (1- len) do
      (or (eq (next-character *mark*) (char string index))
	  (return-from www-parse-string nil))
      (mark-after *mark*))
    (eq (next-character *mark*) (char string len))))

(declaim (inline www-finish-halfdump buffer-parse-html-element))

(defun www-finish-halfdump (*mark*)
  "Convert the halfdump tags starting at *mark*."
  (let ((*marks* `((,*mark*)))
	(pos (copy-mark *mark*)))
    (replace-pattern *mark* *www-a0-pattern-forward* " ")
    ;; FIX a guess, to what should this translate?
;    (replace-pattern *mark* *www-82-c1-pattern-forward* (string #\newline))
    (loop
      (or (find-pattern *mark* *www-&-pattern-forward*)
	  (return))
      (mark-after *mark*)
      (case (next-character *mark*)
	(#\a
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\m
	    (mark-after *mark*)
	    (when (eq (next-character *mark*) #\p)
	      (mark-after *mark*)
	      (when (eq (next-character *mark*) #\;)
		(mark-after *mark*)
		(delete-characters *mark* -4))))))
	(#\g
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\t
	    (mark-after *mark*)
	    (when (eq (next-character *mark*) #\;)
	      (delete-characters *mark* -3)
	      (setf (next-character *mark*) #\>)))))
	(#\h
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\e
	    (mark-after *mark*)
	    (when (www-parse-string *mark* "llip;")
	      (delete-characters *mark* -7)
	      (setf (next-character *mark*) #\.)
	      (insert-string *mark* "..")))))
	(#\l
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\d
	    (mark-after *mark*)
	    (when (www-parse-string *mark* "quo;")
	      (delete-characters *mark* -6)
	      (setf (next-character *mark*) #\")))
	   (#\t
	    (mark-after *mark*)
	    (when (eq (next-character *mark*) #\;)
	      (delete-characters *mark* -3)
	      (setf (next-character *mark*) #\<)))))
	(#\m
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\d
	    (mark-after *mark*)
	    (when (www-parse-string *mark* "ash;")
	      (delete-characters *mark* -6)
	      (setf (next-character *mark*) #\-)
	      (insert-character *mark* #\-)))
	   (#\i
	    (mark-after *mark*)
	    (when (www-parse-string *mark* "nus;")
	      (delete-characters *mark* -6)
	      (setf (next-character *mark*) #\-)))))
	(#\n
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\d
	    (mark-after *mark*)
	    (when (www-parse-string *mark* "ash;")
	      (delete-characters *mark* -6)
	      (setf (next-character *mark*) #\-)))))
	(#\r
	 (mark-after *mark*)
	 (case (next-character *mark*)
	   (#\d
	    (mark-after *mark*)
	    (when (www-parse-string *mark* "quo;")
	      (delete-characters *mark* -6)
	      (setf (next-character *mark*) #\")))))
	))
    (loop
      (let ((node (node-next (node-content (or (search-for-html-element *mark* pos)
					       (return-from nil))))))
	(www-render-html-element-node-macro *mark*))
      (move-mark pos *mark*))
    (delete-mark pos))
  (when (value www-hrefs)
    (setv www-hrefs (nreverse (value www-hrefs)))))

(declaim (inline execute-w3m))

(defun execute-w3m (page stream &key source)
  (ext:run-program
   "w3m"
;   (list "-dump=half-buffer" page)   maybe for w3mmee
   (list (if source "-dump_source" "-halfdump") page)
   :output stream))

(defun test-render ()
  "Render halfdump in current buffer."
  (let ((buffer (current-buffer)))
    (setf (buffer-minor-mode buffer "View") t)
    (setf (buffer-minor-mode buffer "WWW") t)
    (defhvar "WWW Hrefs"
      "List of href lists, sorted from first href in buffer to last."
      :buffer buffer
      :value '())
    (defhvar "WWW URL"
      "URL of page in buffer."
      :buffer buffer
      :value "FIX test")
    (defhvar "WWW Title"
      "Title of page in buffer."
      :buffer buffer
      :value "")
    (setf (value view-return-function) #'(lambda ()))
    (let ((mark (copy-mark (buffer-start-mark buffer))))
      (setf (mark-kind (current-point)) :right-inserting)
      (www-finish-halfdump mark)
      (buffer-start mark)
      (delete-mark mark))))

(defun www-refresh (buffer location &key source)
  "Refresh WWW Buffer."
  (setf (variable-value 'www-hrefs :buffer buffer) '())
  (setf (variable-value 'www-url :buffer buffer) location)
  (setf (variable-value 'www-source :buffer buffer) source)
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (let ((mark (copy-mark (buffer-start-mark buffer))))
      (setf (mark-kind (current-point)) :right-inserting)
      (with-output-to-mark (s mark)
	(execute-w3m location s :source source))
      (buffer-start mark)
      (or source (www-finish-halfdump mark))
      (buffer-start mark)
      (delete-mark mark))))

(defun www-url-at-mark (mark)
  "Return the URL and type (:mailto or :http) of any resource at Mark."
  (or (hemlock-bound-p 'www-hrefs)
      (editor-error "Current buffer must be a WWW buffer."))
  (dolist (ref (value www-hrefs))
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
		(values (format nil "~A~A" (value www-url) link) :http))
	       ((eq (aref link 0) #\/)
		(let* ((www-url (value www-url))
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
		(let ((www-url (value www-url)))
		  ;; Use URL up to the last backslash.
		  (setq www-url (subseq www-url
					0 (position #\/ www-url
						    :from-end t)))
		  (values (format nil "~A/~A" www-url link)
			  :http)))))))))


;;; Commands.

(defcommand "WWW" (p &optional url buffer-name)
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
		       :default (value www-home)))))
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
	(defhvar "WWW Hrefs"
	  "List of href lists, sorted from first href in buffer to last."
	  :buffer buffer
	  :value '())
	(defhvar "WWW URL"
	  "URL of page in buffer."
	  :buffer buffer
	  :value location)
	(defhvar "WWW Title"
	  "Title of page in buffer."
	  :buffer buffer
	  :value "")
	(defhvar "WWW Source"
	  "t if buffer contains page source, else nil."
	  :buffer buffer
	  :value nil)
	(setf (value view-return-function) #'(lambda ())))
      (www-refresh buffer location)
      (update-modeline-field buffer (current-window)
			     (modeline-field :www-title)))))

(defcommand "WWW in Current Buffer" (p &optional url)
  "Browse to a prompted URL in the current buffer"
  "Browse to Url in the current buffer, prompting for an URL if Url is
   nil."
  (declare (ignore p))
  (let ((location (if url
		      (if (eq (type-of url) 'pathname)
			  (namestring url)
			  url)
		      (prompt-for-string
		       :prompt "URL: "
		       :default (value www-home)))))
    (www-refresh (current-buffer) location)
    (update-modeline-field (current-buffer) (current-window)
			   (modeline-field :www-title))))

(defcommand "Next WWW Reference" (p)
  "Move point to the next URL."
  "Move point to the next URL."
  (declare (ignore p))
  (let ((point (current-point)))
    (dolist (ref (value www-hrefs))
      (when (mark< point (car ref))
	(move-mark point (car ref))
	(return-from nil)))))

(defcommand "Next WWW Reference" (p)
  "Move point to the next URL."
  "Move point to the next URL."
  (declare (ignore p))
  (let ((point (current-point)))
    (dolist (ref (value www-hrefs))
      (when (mark< point (car ref))
	(move-mark point (car ref))
	(return-from nil)))))

(defcommand "WWW URL at Point" (p)
  "Print the URL at Point."
  "Print the URL at Point."
  (declare (ignore p))
  (let ((url (www-url-at-mark (current-point))))
    (if url
	(progn
	  (message "URL: ~A" url)
	  (ring-push (copy-region (string-to-region url)) *kill-ring*))
	(message "URL: "))))

(defcommand "WWW Resource from Point" (p)
  "Follow the URL at Point.  With a prefix create a new WWW buffer."
  "Follow the URL at Point.  If P is true create a new WWW buffer."
  (multiple-value-bind (url type)
		       (www-url-at-mark (current-point))
    (when url
      (case type
	(:http
	 (www-command p url (buffer-name (current-buffer))))
	(:mailto
	 (send-message-command nil)
	 (let ((point (buffer-point (current-buffer))))
	   (insert-string point (subseq url 7))
	   (buffer-end point)))))))

(defcommand "WWW Resource from Point in New Buffer" (p)
  "Follow the URL at Point."
  "Follow the URL at Point."
  (declare (ignore p))
  (www-resource-from-point-command t))

(defcommand "WWW Refresh" (p)
  "Refresh WWW page in current buffer."
  "Refresh WWW page in current buffer."
  (declare (ignore p))
  (if (value www-url)
      (www-refresh (current-buffer) (value www-url))
      (editor-error "\"WWW URL\" is nil.")))

(defcommand "WWW Home" (p)
  "Browse URL in \"WWW Home\"."
  "Browse URL in \"WWW Home\"."
  (declare (ignore p))
  (if (value www-home)
      (www-refresh (current-buffer) (value www-home))
      (editor-error "\"WWW Home\" is nil.")))

(defcommand "Save WWW URL" (p)
  "Save the WWW URL of the current page to the kill ring."
  "Save the WWW URL of the current page to the kill ring."
  (declare (ignore p))
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (message "Saved URL: ~A" (value www-url))
  (ring-push (copy-region (string-to-region (value www-url)))
	     *kill-ring*))

(defcommand "Switch to WWW" (p)
  "Switch to WWW buffer if there is one, else prompt for an URL."
  "Switch to WWW buffer if there is one, else prompt for an URL."
  (declare (ignore p))
  (let ((buffer (getstring "WWW" *buffer-names*)))
    (if buffer
	(change-to-buffer buffer)
	(www-command nil))))

(defcommand "Copy WWW Buffer" (p &optional (buffer (current-buffer)))
  "Create and switch to a copy of the current buffer, which must be a WWW
   Buffer."
  "Create and switch to a copy of the current buffer, which must be a WWW
   Buffer."
  (declare (ignore p))
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (copy-buffer-command nil buffer)
  ;; Refresh to update any font marks in www-hrefs.
  (www-refresh-command nil))

(defcommand "WWW Page Info" (p)
  "Pop up info about the current page."
  "Pop up info about the current page."
  (declare (ignore p))
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (with-pop-up-display (stream)
    (ext:run-program
     "w3m" (list "-dump_head" (value www-url))
     :output stream)))

(defcommand "WWW Toggle Source" (p &optional (buffer (current-buffer)))
  "Toggle display of page source."
  "Toggle display of page source."
  (declare (ignore p))
  (or (value www-url) (editor-error "Must be in a WWW buffer."))
  (www-refresh buffer (value www-url) 
	       :source (if (value www-source) nil t)))
