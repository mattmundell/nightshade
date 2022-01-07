;;; GNU Info mode.

;; TODO: Remember previous position of point in node.
;;       Scroll across nodes.
;;       Handle more than one buffer of same manual.
;;       Improve speed of loading large multiple-file manuals.
;;       Show from Index (i).
;;       Search (s).
;;       Go To Node (g).
;;       FIXes below.

(in-package "ED")


;;;; Highlighting.

(defun highlight-info-line (line chi-info)
  (or (line-previous line)
      (progn
	;; Highlight navigation line.
	(chi-mark line 0 *original-font* :comment chi-info)
	(return-from highlight-info-line)))
  (or (line-previous (line-previous line))
      (when (plusp (line-length line))
	;; Directory node, second line.
	(chi-mark line 0 *original-font*
		  :window-foreground chi-info)
	(setq *context* :directory)
	(return-from highlight-info-line)))
  (when (eq *context* :directory)
    (let ((line-string (line-string line)))
      (if (and (> (length line-string) 5)
	       (char= (aref line-string 0) #\*)
	       (char= (aref line-string 1) #\space))
	  (progn
	    ;; Highlight menu item.
	    (chi-mark line 1
		      *original-font*
		      :special-form chi-info)
	    (if (position #\tab line-string)
		(chi-mark line (position #\tab line-string)
			  *original-font*
			  :window-foreground chi-info)))
	  ;; Do a chi mark to keep the *context* going.
	  (chi-mark line 0 *original-font*
		    :window-foreground chi-info)))
    (return-from highlight-info-line))
  (fi (and (line-previous
	    (line-previous line))
	   (line-previous
	    (line-previous
	     (line-previous line)))
	   (line-previous
	    (line-previous
	     (line-previous
	      (line-previous line)))))
      ;; Highlight node heading.
      (chi-mark line 0 *original-font* :variable chi-info)
      ;; Highlight node body.
      (let ((line-string (line-string line)))
	(if (zerop (length line-string))
	    (return-from highlight-info-line))
	(cond ((and (char= (aref line-string 0) #\*)
		    (> (line-length line) 4)
		    (char= (aref line-string 1) #\space)
		    (search "::" line-string))
	       ;; Highlight menu item.
	       (chi-mark line 1 *original-font*
			 :special-form chi-info)
	       (chi-mark line (search "::" line-string)
			 *original-font*
			 :window-foreground chi-info))
	      (t
	       ;; Scan the line for links.
	       (let ((pos 0))
		 (when (eq *context* :open-link)
		   ;; The previous line ended in an open link.
		   (chi-mark line 0 *original-font*
			     :special-form chi-info)
		   (setq pos (search "::" line-string))
		   (or pos
		       (progn
			 ;; The link is still open at the end of this
			 ;; line.
			 (setq *context* :open-link)
			 (return-from highlight-info-line)))
		   (chi-mark line pos *original-font*
			     :window-foreground chi-info)
		   (incf pos))
		 ;; FIX How is actual "*note " represented in Info?
		 ;; Scan the rest.
		 (while ((pos (search "*note " line-string
				      :start2 pos)
			      (search "*note " line-string
				      :start2 pos)))
			(pos)
		   (chi-mark line (+ pos 5) *original-font*
			     :special-form chi-info)
		   (let ((end (search "::" line-string
				      :start2 (1+ pos))))
		     (or end
			 (progn
			   ;; The line ends with an open link.
			   (setq *context* :open-link)
			   (return-from highlight-info-line)))
		     (chi-mark line end *original-font*
			       :window-foreground chi-info)
		     (setq pos (1+ end)))))))))
  (setq *context* ()))

(defun highlight-visible-info-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-info-line))

(defun setup-info-mode (buffer)
  (highlight-visible-info-buffer buffer)
  (pushnew '("Info" () highlight-visible-info-buffer)
	   *mode-highlighters*))

;;;; Mode.

(defmode "Info" :major-p nil
  :precedence 4.0
  :setup-function #'setup-info-mode
  :documentation
  "GNU Info Mode.")


;;;; Helper functions.

(defun find-info-file-buffer (filename)
  "Like find-file-buffer, tries Filename with Info extensions and handles
   compressed files."
  (when (probe-file filename)
    (return-from find-info-file-buffer (find-file-buffer filename)))
  (let ((name (concatenate 'simple-string filename ".info")))
    (when (probe-file name)
      (return-from find-info-file-buffer (find-file-buffer name)))
    (let ((name (concatenate 'simple-string name ".gz")))
      (or (probe-file name)
	  (progn
	    (setq name (concatenate 'simple-string filename ".gz"))
	    (or (probe-file name)
		(editor-error "Failed to find Info file named like ~A (name was ~A)."
			      filename name))))
      (let ((buffer (make-buffer (prin1-to-string (gensym)))))
	(with-output-to-mark (s (copy-mark (buffer-start-mark buffer)))
	  (let ((process (run-program "gzip"
				      `("-d" "-c" ,(os-namestring name))
				      :output s)))
	    (process-close process)
	    buffer))))))

(defmacro replace-include-map ()
  "Replace multi-part map at point with actual text."
  '(progn
     (delete-region (region (copy-mark (line-start (line-offset begin -1)))
			    (line-offset begin 2)))
     (do* ((ichars (line-string (mark-line begin))
		   (line-string (mark-line begin)))
	   (pos (search ": " ichars) (search ": " ichars))
	   (fseparator (new-search-pattern :string-sensitive
					   :forward
					   "")))
	  ((eq pos nil)
	   (or (find-pattern begin separator)
	       (return result))
	   (character-offset begin 2)
	   (setq chars (line-string (mark-line begin))))
       ;; FIX Rather read directly into current buffer?
       (let* ((buffer (find-info-file-buffer
		       (concatenate 'simple-string
				    "info:"
				    (subseq ichars 0 pos))))
	      (mark (buffer-start-mark buffer)))
	 ;; Skip the introduction.
	 (find-pattern mark fseparator)
	 (insert-region begin (region mark (buffer-end-mark buffer)))
	 (delete-region (region (copy-mark begin) (line-start (line-offset begin 1))))
	 (delete-buffer buffer)))))

(defun parse-info-file (filename)
  "Parse the GNU Info file Filename into an Info list, returning the list."
  (let* ((b (find-info-file-buffer filename))
	 (separator (new-search-pattern :string-sensitive
					:backward
					""))
	 (begin (copy-mark (buffer-end-mark b)))
	 (end (copy-mark begin))
	 (result ()))
    ;; Parse the nodes of the manual into an alist.
    (loop
      (or (find-pattern begin separator)
	  (return result))
      (character-offset begin 2)

      ;; File: name.info,  Node: Top,  Next: Introduction,  Prev: (dir),  Up: (dir)
      (let ((chars (line-string (mark-line begin)))
	    (node ()))
	(if (string= chars "Indirect:")
	    (replace-include-map))
	(or (string= chars "Tag Table:")
	    (string= chars "End Tag Table")
	    (progn
	      (let* ((pos1 (or (search "File: " chars)
			       (editor-error "Failed to parse File field. chars: ~A" chars)))
		     (pos2 (or (search "," chars :start2 pos1)
			       (search "	" chars :start2 pos1)
			       (editor-error "Failed to parse File field."))))
		(push `(:file ,(subseq chars (+ pos1 6) pos2)) node)
		(setq pos1 (or (search "Node: " chars)
			       (editor-error "Failed to parse Name field.")))
		(setq pos2 (or (search "," chars :start2 pos1)
			       (search "	This is the top" chars
				       :start2 pos1)
			       (editor-error "Failed to parse Name field.")))
		(push `(:name ,(subseq chars (+ pos1 6) pos2)) node)
		(setq pos1 (search "Next: " chars :start2 pos2))
		(when pos1
		  (setq pos2 (or (search "," chars :start2 pos1)
				 (editor-error "Failed to parse Next field.")))
		  (push `(:next ,(subseq chars (+ pos1 6) pos2)) node))
		(setq pos1 (search "Prev: " chars :start2 pos2))
		(when pos1
		  (setq pos2 (or (search "," chars :start2 pos1)
				 (editor-error "Failed to parse Prev field.")))
		  (push `(:previous ,(subseq chars (+ pos1 6) pos2)) node))
		(setq pos1 (search "Up: " chars :start2 pos2))
		(when pos1
		  (push `(:parent ,(subseq chars (+ pos1 4))) node)))

	      (line-start begin)

	      (push (nconc node
			   (cons `(:content ,(copy-region (region begin end)))
				 nil))
		    result))))
      (character-offset begin -2)
      (move-mark end begin))
    (delete-buffer b)      ;; FIX unwind-protect this
    result))

(defun find-info-node (name tree)
  "Find node named Name in Tree."
  (dolist (node tree)
    (let ((a (assoc :name node)))
      (if (string= (cadr a) name)
	  (return node)))))

;; FIX handle * envsubst: (gettext)envsubst Invocation.
;;                                 ^^^^^^^^^^^^^^^^^^^
(defun info-node-at-point (&optional (mark (current-point)))
  "Return name of node referenced at point, if any.  Return second argument
   true if reference to external info manual."
  (let* ((chars (region-to-string
		 (region mark (line-end (copy-mark mark)))))
	 (start (and (> (length chars) 4)
		     (string= chars "* " :end1 2)
		     0)))
    (when start
      (let ((end (search ":" chars :start2 start)))
	(when end
	  (if (and (> (length chars) (+ end 2))
		   (char= (char chars (1+ end)) #\:))
	      ;; * node::
	      (subseq chars (+ start 2) end)
	      (if (and (> (length chars) (+ end 2))
		       (char= (char chars (+ end 2)) #\())
		  ;; * node: (...
		  (progn
		    ;; Swap sense of start and end variables.
		    (setq end (search ": (" chars :start2 start))
		    (setq start (search ")" chars :start2 end))
		    (values (subseq chars (+ end 3) start) t))
		  (let ((end2 (search "." chars :start2 (1+ end))))
		    (if end2
			;; * index:     node.
			(string-left-trim '(#\space #\tab)
					  (subseq chars
						  (1+ end)
						  end2))
			;; Assume * node:       ...
			(subseq chars (+ start 2) end))))))))))

(defun show-info-node (name-or-node &key (add-to-history t))
  "If Name-or-node is \"(dir)\" switch to GNU Info dir, else show
   Name-or-node in current buffer."
  (let ((buffer (current-buffer))
	(node (if (consp name-or-node)
		  name-or-node
		  (if (string= name-or-node "(dir)")
		      (return-from show-info-node (info-command))
		      (find-info-node name-or-node
				       (value info-tree))))))
    (when node
      (with-writable-buffer (buffer)
	(delete-region (buffer-region buffer))
	(insert-region (buffer-start-mark buffer)
		       (cadr (assoc :content node)))
	(setv info-node node)
	(when add-to-history
	  (let* ((current-history (value current-info-history))
		 (history-element (cons node nil))
		 (new-history (cons history-element current-history)))
	    (if current-history
		;; Link the next "slot" of the previous element to the new
		;; history, for moving forward through the history.
		(setf (cdar current-history) new-history))
	    (setv current-info-history new-history)
	    (setv info-history new-history)))
	(exchange-point-and-mark-command)
	;; Setup the menu table (any line starting with *).
	(if (editor-bound-p 'info-menu :buffer buffer)
	    (clrstring (variable-value 'info-menu :buffer buffer))
	    (defevar "Info Menu"
	      "The menu on the info node displayed in this buffer."
	      :buffer buffer
	      :value (make-string-table)))
	(let ((menu (variable-value 'info-menu :buffer buffer)))
	  (do-buffer-lines (line buffer)
	    (multiple-value-bind (item external)
				 (info-node-at-point (mark line 0))
	      (when item
		(or (string= (string-downcase item) "menu")
		    (setf (getstring item menu) (if external
						    :external
						    :internal)))))))))))


;;;; Commands.

(defcommand "Info" (p (filename "info:dir"))
  "Switch to the GNU Info directory buffer, creating it if necessary."
  "Switch to the buffer for GNU Info file Filename, creating it if
   necessary."
  (declare (ignore p))
  (let* ((buf-name (format nil "Info ~A" filename))
	 (buffer (getstring buf-name *buffer-names*)))
    (if buffer
	(change-to-buffer buffer)
	(let* ((tree (parse-info-file filename))
	       (node (car tree))
	       ;; Make buffer after parse-info-file, in case that fails.
	       (buffer (make-buffer buf-name
				    :modes '("Fundamental" "View" "Info"))))
	  (change-to-buffer buffer)
;	  (message "") ;; FIX
;	  (force-output *echo-area-stream*)
	  (setf (value view-return-function) #'(lambda ()))
	  (defevar "Info Tree"
	    "The tree of nodes associated with this buffer."
	    :buffer buffer
	    :value tree)
	  (defevar "Info Node"
	    "The info node displayed in this buffer."
	    :buffer buffer
	    :value node)
	  (defevar "Info History"
	    "A history of info nodes displayed in this buffer."
	    :buffer buffer
	    :value ())
	  (defevar "Current Info History"
	    "The history from the current node to the beginning."
	    :buffer buffer
	    :value ())
	  (show-info-node node)))))

(defcommand "Next Info Node" ()
  "Show the next Info node, if there is such a node."
  (let ((nexta (assoc :next (value info-node))))
    (when nexta
      (show-info-node (cadr nexta)))))

(defcommand "Previous Info Node" ()
  "Show the previous Info node, if there is such a node."
  (let ((preva (assoc :previous (value info-node))))
    (when preva
      (show-info-node (cadr preva)))))

(defcommand "Parent Info Node" ()
  "Show the parent of the currently displayed node."
  (let ((parenta (assoc :parent (value info-node))))
    (when parenta
      (show-info-node (cadr parenta)))))

(defcommand "Top Info Node" ()
  "Show the top of the tree of the currently displayed node."
  (show-info-node "Top"))

(defcommand "Forward Info Node" ()
  "Show the next node from the history of nodes."
  (let* ((hist (value current-info-history))
	 (next (if hist (cdar hist))))
    (if next
	(progn
	  (setv current-info-history next)
	  (show-info-node (caar next) :add-to-history nil))
	(message "End of history."))))

(defcommand "Backward Info Node" ()
  "Show the previous node from the history of nodes."
  (let* ((hist (value current-info-history))
	 (prev (if hist (cdr hist))))
    (if prev
	(progn
	  (setv current-info-history prev)
	  (show-info-node (caar prev) :add-to-history nil))
	(message "Beginning of history."))))

;; FIX refs may be anywhere on line
;; FIX     incl return pos in line
(defcommand "Next Info Reference" ()
  "Move point to the next reference, wrapping if required.
   Return the position of the reference in the line it is on."
  (let ((point (current-point))
	(buffer (current-buffer))
	(found nil))
    (flet ((frob (line)
	     (or (line-next line)
		 (setq line (mark-line (buffer-start-mark buffer))))))
      (do* ((last-line (mark-line point))
	    (line (frob last-line) (frob line)))
	 ((or (let ((chars (line-string line)))
		(if (and (> (length chars) 4)
			 (string= chars "* " :end1 2))
		    (if (string= chars "Menu" :start1 2 :end1 6)
			nil
			(progn
			  (setq found 0)
			  (line-start point line)))))
	      (eq line last-line))
	  found)))))

(defcommand "Previous Info Reference" ()
  "Move point to the previous reference, wrapping if required."
  ;; FIX
  (next-info-reference-command))

(defcommand "Info Node from Point" ()
  "Show the node cited by the first reference after point."
  (multiple-value-bind (name external)
		       (info-node-at-point (current-point))
    (if (or name
	    (let* ((double-colon (new-search-pattern :string-sensitive
						     :backward
						     "::"))
		   (dc-mark (copy-mark (current-point)))
		   (star (new-search-pattern :string-sensitive
					     :backward
					     "* "))
		   (star-mark (copy-mark (current-point))))
	      (when (and (find-pattern star-mark star)
			 (if (find-pattern dc-mark double-colon)
			     (mark< dc-mark star-mark)
			     t))
		(multiple-value-setq (name external)
		  (info-node-at-point star-mark))
		t)))
	(when name
	  (if external
	      (info-command () (concatenate 'simple-string
					     "info:"
					     name))
	      (show-info-node name))))))

(defcommand "Info Node from Menu" ()
  "Show a prompted menu node."
  (when (value info-menu)
    (let ((item (prompt-for-keyword (list (value info-menu))
				    :prompt "Menu item: "
				    :help "Enter a menu item to visit.")))
      (if (eq (getstring item (value info-menu)) :external)
	  (info-command () (concatenate 'simple-string
					 "info:"
					 item))
	  (show-info-node item)))))
