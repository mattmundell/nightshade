;;; Text/enriched.

(in-package "ED")


;;;; Parser.

(eval-when (compile eval load)

  (parse:defparser
             `((:enriched-tag        #\< (group (many :enriched-tag-char)) #\>
				     :enriched-params
				     :enriched-text
				     "</" (group (many :enriched-tag-char)) #\>)

	       (:enriched-params     (or (many :enriched-param)))

  	       (:enriched-param      (fold "<param>")
  				     (or (group (many :enriched-tag-char)))
  				     (fold "</param>"))

	       (:enriched-text       (many (or :enriched-tag
					       (group (many :enriched-text-char)))))

	       (:enriched-tag-char   (cond (fi (member parse:ch '(#\> #\space #\< #\/)))))
	       (:enriched-text-char  (cond (fi (char= parse:ch #\<)))))
    :bufferp t)

  ) ; eval-when


;;;; Rendering.

(defun search-for-enriched-tag (parse:*mark* pos)
  "Search from *mark* with buffer-parse-enriched-tag.  Pos must be another
   mark at *mark*."
  (let ((node))
    (loop
      (or (find-pattern pos *www-<-pattern-forward*)
	  (return-from search-for-enriched-tag ()))
      (move-mark parse:*mark* pos)
      (or (mark-after pos)
	  (return-from search-for-enriched-tag ()))
      (if (char= (next-character pos) #\/)
	  (return-from search-for-enriched-tag ()))
      (when (setq node (buffer-parse-enriched-tag))
	(mark-before pos)
	(move-mark parse:*mark* pos)
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
	(let ((content (parse:node-content node)))
	  (etypecase content
	    (string)
	    (parse:region-node)
	    (enriched-param-node
	     ;; FIX Can there be many params?
	     (delete-characters ,mark 7)  ; <param>
	     (let* ((text-node (parse:node-content (parse:node-next (parse:node-content content))))
		    (text (etypecase text-node
			    (region (region-to-string text-node))
			    (string text-node))))
	       (push text *params*)
	       (delete-characters ,mark (length text)))
	     (delete-characters ,mark 8)  ; </param>
	     ))))
       (enriched-text-node
	(let ((content (parse:node-content node)))
	  (typecase content
	    (string)
	    (t
	     (loop for tnode = content then (parse:node-next tnode)
	       while tnode do
	       (etypecase tnode
		 (enriched-tag-node
		  (let ((node (parse:node-next (parse:node-content tnode))))
		    (render-enriched-node ,mark)))
		 (parse:region-node
		  (let ((content (parse:node-content tnode)))
		    (character-offset
		     ,mark
		     (length (etypecase content
			       (region (region-to-string content))
			       (string content))))))))))))
       (parse:region-node
	(let ((name (etypecase (parse:node-content node)
		      (string (parse:node-content node))
		      (region (region-to-string (parse:node-content node)))))
	      (orig-params *params*))
	  (delete-characters ,mark (+ (length name) 2))  ; <name>
	  ;; FIX table?
	  (cond ((string= name "x-color")
		 (setq node (parse:node-next (parse:node-next node)))
		 (render-enriched-node ,mark) ; params
		 (setq node (parse:node-next node))
		 (when *params*
		   (let ((param (string-downcase (car *params*))))
		     ;; FIX table? pass name to font-mark?
		     (cond ((string= param "lightsalmon")
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *special-form-font*))
			   ((string= param "chocolate1")
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *preprocessor-font*))
			   ((string= param "yellow")
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *variable-font*))
			   ((string= param "aquamarine")
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *function-font*))
			   ((string= param "palegreen")
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *string-font*))
			   ((string= param "lightgoldenrod")
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *variable-font*))
			   (t
			    (font-mark (mark-line ,mark)
				       (mark-charpos parse:*mark*)
				       *error-font*)))))
		 (render-enriched-node ,mark) ; text
		 (font-mark (mark-line ,mark) (mark-charpos ,mark)
			    *original-font*)
		 (setq *params* orig-params))
		((string= name "bold")
		 (setq node (parse:node-next (parse:node-next node)))
		 (render-enriched-node ,mark) ; params
		 (setq node (parse:node-next node))
		 ;; FIX how to bold and font?
		 (or *params* (font-mark (mark-line ,mark) (mark-charpos ,mark)
					 *comment-font*))
		 (render-enriched-node ,mark)
		 ;; FIX how to bold and font?
		 (or *params* (font-mark (mark-line ,mark) (mark-charpos ,mark)
					 *original-font*)))) ; text
	  (delete-characters ,mark (+ (length name) 3))))))) ; </name>

(defun render-enriched-node (parse:*mark*)
  "Render Enriched tag nodes in `nodes' at *MARK*."
  (render-enriched-node-macro parse:*mark*))

(defun render-enriched-nodes (parse:*mark*)
  "Render Enriched tag nodes in `nodes' at *MARK*."
  (loop for node = node then (parse:node-next node) while node do
    (render-enriched-node-macro parse:*mark*)))

(defun render-enriched (parse:*mark*)
  "Convert the enriched tags starting at *mark*."
  (let ((parse:*marks* `((,parse:*mark*)))
	(pos (copy-mark parse:*mark*)))
    (loop
      (let ((node (parse:node-next
		   (parse:node-content (or (search-for-enriched-tag parse:*mark*
								    pos)
					   (return-from ()))))))
	(render-enriched-node-macro parse:*mark*))
      (move-mark pos parse:*mark*))
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
		(defevar "Fill Column"
		  "Force text to next line at this column."
		  :mode "Enriched" :value width))))
	(line-offset mark 1 0)
	(delete-region (region start mark))))
    (render-enriched mark)
    (setf (buffer-modified buffer) ())
    (setf (buffer-writable buffer) ())))

(defmode "Enriched" :major-p t :setup-function #'setup-enriched-buffer)

;; FIX ~ auto
(defcommand "Enriched Mode" ()
  "Put the current buffer into Enriched mode."
  (setf (buffer-major-mode (current-buffer)) "Enriched"))
