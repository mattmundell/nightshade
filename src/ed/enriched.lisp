;;; Text/enriched.

(in-package "ED")


;;;; Parser.

(eval-when (compile eval load)

  (defparser `((:enriched-tag        #\< (group (many :enriched-tag-char)) #\>
				     :enriched-params
				     :enriched-text
				     "</" (group (many :enriched-tag-char)) #\>)

	       (:enriched-params     (or (many :enriched-param)))

  	       (:enriched-param      (fold "<param>")
  				     (or (group (many :enriched-tag-char)))
  				     (fold "</param>"))

	       (:enriched-text       (many (or :enriched-tag
					       (group (many :enriched-text-char)))))

	       (:enriched-tag-char   (cond (fi (member ch '(#\> #\space #\< #\/)))))
	       (:enriched-text-char  (cond (fi (char= ch #\<)))))
    :bufferp t)

  ) ; eval-when


;;;; Rendering.

(defun search-for-enriched-tag (*mark* pos)
  "Search from *mark* with buffer-parse-enriched-tag.  Pos must be another
   mark at *mark*."
  (let ((node))
    (loop
      (or (find-pattern pos *www-<-pattern-forward*)
	  (return-from search-for-enriched-tag ()))
      (move-mark *mark* pos)
      (or (mark-after pos)
	  (return-from search-for-enriched-tag ()))
      (if (char= (next-character pos) #\/)
	  (return-from search-for-enriched-tag ()))
      (when (setq node (buffer-parse-enriched-tag))
	(mark-before pos)
	(move-mark *mark* pos)
	(return-from search-for-enriched-tag node))
      (or (mark-after pos)
	  (return-from search-for-enriched-tag ())))))

(defvar *params* ())
(declaim (special node *params*))
(defmacro render-enriched-node-macro (mark)
  "Render Enriched tag node."
  `(progn
     (etypecase node
       (enriched-params-node
	(let ((content (node-content node)))
	  (etypecase content
	    (string)
	    (region-node)
	    (enriched-param-node
	     ;; FIX Can there be many params?
	     (delete-characters ,mark 7)  ; <param>
	     (let* ((text-node (node-content (node-next (node-content content))))
		    (text (etypecase text-node
			    (region (region-to-string text-node))
			    (string text-node))))
	       (push text *params*)
	       (delete-characters ,mark (length text)))
	     (delete-characters ,mark 8)  ; </param>
	     ))))
       (enriched-text-node
	(let ((content (node-content node)))
	  (typecase content
	    (string)
	    (t
	     (loop for tnode = content then (node-next tnode)
	       while tnode do
	       (etypecase tnode
		 (enriched-tag-node
		  (let ((node (node-next (node-content tnode))))
		    (render-enriched-node ,mark)))
		 (region-node
		  (let ((content (node-content tnode)))
		    (character-offset
		     ,mark
		     (length (etypecase content
			       (region (region-to-string content))
			       (string content))))))))))))
       (region-node
	(let ((name (etypecase (node-content node)
		      (string (node-content node))
		      (region (region-to-string (node-content node)))))
	      (orig-params *params*))
	  (delete-characters ,mark (+ (length name) 2))  ; <name>
	  ;; FIX table?
	  (cond ((string= name "x-color")
		 (setq node (node-next (node-next node)))
		 (render-enriched-node ,mark) ; params
		 (setq node (node-next node))
		 (when *params*
		   (let ((param (string-downcase (car *params*))))
		     ;; FIX table? pass name to font-mark?
		     (cond ((string= param "lightsalmon")
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       special-form-font))
			   ((string= param "chocolate1")
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       *preprocessor-font*))
			   ((string= param "yellow")
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       variable-name-font))
			   ((string= param "aquamarine")
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       *function-name-font*))
			   ((string= param "palegreen")
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       string-font))
			   ((string= param "lightgoldenrod")
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       variable-name-font))
			   (t
			    (font-mark (mark-line ,mark) (mark-charpos *mark*)
				       *error-font*)))))
		 (render-enriched-node ,mark) ; text
		 (font-mark (mark-line ,mark) (mark-charpos ,mark)
			    original-font)
		 (setq *params* orig-params))
		((string= name "bold")
		 (setq node (node-next (node-next node)))
		 (render-enriched-node ,mark) ; params
		 (setq node (node-next node))
		 ;; FIX how to bold and font?
		 (or *params* (font-mark (mark-line ,mark) (mark-charpos ,mark)
					 comment-font))
		 (render-enriched-node ,mark)
		 ;; FIX how to bold and font?
		 (or *params* (font-mark (mark-line ,mark) (mark-charpos ,mark)
					 original-font)))) ; text
	  (delete-characters ,mark (+ (length name) 3))))))) ; </name>

(defun render-enriched-node (*mark*)
  "Render Enriched tag nodes in `nodes' at *MARK*."
  (render-enriched-node-macro *mark*))

(defun render-enriched-nodes (*mark*)
  "Render Enriched tag nodes in `nodes' at *MARK*."
  (loop for node = node then (node-next node) while node do
    (render-enriched-node-macro *mark*)))

(defun render-enriched (*mark*)
  "Convert the enriched tags starting at *mark*."
  (let ((*marks* `((,*mark*)))
	(pos (copy-mark *mark*)))
    (loop
      (let ((node (node-next (node-content (or (search-for-enriched-tag *mark*
									pos)
					       (return-from ()))))))
	(render-enriched-node-macro *mark*))
      (move-mark pos *mark*))
    (delete-mark pos)))


;;;; Mode.

(defun setup-enriched-buffer (buffer)
  "Setup BUFFER for Enriched mode."
  (with-mark ((mark (buffer-start-mark buffer))
	      (start (buffer-start-mark buffer)))
    (line-offset mark 1 0)
    (delete-region (region start mark))
    (let ((string (line-string (mark-line mark))))
      (when (and (> (length string) 12)
		 (string= string "Text-Width: " :end1 12))
	(let ((width (parse-integer (trim-subseq string 12) :errorp ())))
	  (when width
	    (if (editor-bound-p 'fill-column :mode "Enriched")
		(setf (variable-value 'fill-column :mode "Enriched") width)
		(defhvar "Fill Column"
		  "Force text to next line at this column."
		  :mode "Enriched" :value width))))
	(line-offset mark 1 0)
	(delete-region (region start mark))))
    (render-enriched mark)
    (setf (buffer-modified buffer) ())
    (setf (buffer-writable buffer) ())))

(defmode "Enriched" :major-p t :setup-function #'setup-enriched-buffer)

;; FIX ~ auto
(defcommand "Enriched Mode" (p)
  "Put the current buffer into Enriched mode."
  "Put the current buffer into Enriched mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Enriched"))
