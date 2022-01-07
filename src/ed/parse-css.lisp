;;; Cascading Style Sheet (CSS) parser.
;;
;; FIX add comment parsing

(in-package "ED")

(eval-when (compile eval load)

(defparser
    ;; FIX too much of this is spent handling the space cases
    `((:css            (any :css-space :css-style :css-space))
      (:css-style      :css-style-name :css-space
		       (any #\, :css-space :css-style-name :css-space)
 		       #\{
 		       :css-space (or #\;)
		       :css-space
 		       (or :css-property)
 		       :css-space
 		       (any #\; :css-space :css-property :css-space)
 		       (or #\;) :css-space
		       #\})
      (:css-style-name (group (many :css-style-char))
		       (any #\. (group (many :css-style-char))))
      (:css-property   (group (many :css-namechar)) #\:
		       :css-space (group (many :css-valchar)) :css-space)
      (:css-space      (group (any (or #\space #\newline #\tab))))
      (:css-style-char (cond (fi (memq ch '(#\, #\. #\{ #\} #\space #\newline #\tab)))))
      (:css-namechar   (cond (fi (memq ch '(#\: #\} #\space #\newline #\tab)))))
      (:css-valchar    (cond (fi (memq ch '(#\; #\} #\newline)))))))

) ; eval-when (compile eval load)

(defun style (styles type class)
  "Return the style of CLASS of TYPE in STYLES, as a string."
  (loop for node = (node-content styles) then (node-next node) while node do
    (etypecase node
      (css-space-node)
      (css-style-node
       (let ((name (node-content node)))
	 (if name
	     (loop for cont = name then (node-next cont) while cont do
	       (typecase cont
		 (css-style-name-node
		  (let ((content (node-content name)))
		    (and content
			 (string= (string-upcase (region-to-string (node-content content)))
				  (format () "~A" type))
			 (let ((dot (node-next content)))
			   (if dot
			       (let ((styles-class (node-next dot)))
				 (and styles-class
				      (string= (string-upcase (region-to-string (node-content styles-class)))
					       (format () "~A" class))
				      (return-from style node))))))))
		 (char-node
		  (if (char= (node-content cont) #\{) (return)))))))))))

(defun style-property (style property)
  "Return PROPERTY from STYLE."
  (loop for node = (node-content style) then (node-next node) while node do
    (typecase node
      (css-property-node
       (let ((content (node-content node)))
	 (if (string= (string-upcase (region-to-string (node-content content)))
		      (format () "~A" property))
	     (return (string-trim '(#\space #\tab #\newline)
				  (region-to-string
				   (node-content (node-next (node-next (node-next content)))))))))))))
