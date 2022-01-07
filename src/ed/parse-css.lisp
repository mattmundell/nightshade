;;; Cascading Style Sheet (CSS) parser.
;;
;; FIX add comment parsing

(in-package "ED")

(eval-when (compile eval load)

(parse:defparser
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
      (:css-style-char (cond (fi (memq parse:ch '(#\, #\. #\{ #\} #\space #\newline #\tab)))))
      (:css-namechar   (cond (fi (memq parse:ch '(#\: #\} #\space #\newline #\tab)))))
      (:css-valchar    (cond (fi (memq parse:ch '(#\; #\} #\newline)))))))

) ; eval-when (compile eval load)

(defun style (styles type class)
  "Return the style of CLASS of TYPE in STYLES, as a string."
  (loop for node = (parse:node-content styles) then (parse:node-next node)
    while node do
    (etypecase node
      (css-space-node)
      (css-style-node
       (let ((name (parse:node-content node)))
	 (if name
	     (loop for cont = name then (parse:node-next cont)
	       while cont do
	       (typecase cont
		 (css-style-name-node
		  (let ((content (parse:node-content name)))
		    (and content
			 (string= (string-upcase
				   (parse:node-content content))
				  (format () "~A" type))
			 (let ((dot (parse:node-next content)))
			   (if dot
			       (let ((styles-class (parse:node-next dot)))
				 (and styles-class
				      (string= (string-upcase
						(parse:node-content styles-class))
					       (format () "~A" class))
				      (return-from style node))))))))
		 (parse:char-node
		  (if (char= (parse:node-content cont) #\{) (return)))))))))))

;; FIX sounds like style (verb) property (noun). get/return-style-property?
(defun style-property (style property)
  "Return $property from $style."
  (loop for node = (parse:node-content style) then (parse:node-next node)
    while node do
    (typecase node
      (css-property-node
       (let ((content (parse:node-content node)))
	 (if (string= (string-upcase (parse:node-content content))
		      (format () "~A" property))
	     (return (string-trim '(#\space #\tab #\newline)
				  (parse:node-content
				   (parse:node-next
				    (parse:node-next
				     (parse:node-next content))))))))))))
