;;; GNU Info mode.

;; TODO: Remember previous position of point in node.
;;       Scroll across nodes.
;;       Handle more than one buffer of same manual.
;;       Improve speed of loading large multiple-file manuals.
;;       Show from Menu (m).
;;       Show from Index (i).
;;       Search (s).
;;       Goto Node (g).
;;       FIXes below.

(in-package "HEMLOCK")


;;; Mode.

(defmode "GInfo" :major-p nil
  :precedence 4.0
  :documentation
  "GNU Info Mode.")


;;; Helper functions.

(defun find-ginfo-file-buffer (filename)
  "Like find-file-buffer, tries Filename with Info extensions and handles
   compressed files."
  (when (probe-file filename)
    (return-from find-ginfo-file-buffer (find-file-buffer filename)))
  (let ((name (concatenate 'simple-string filename ".info")))
    (when (probe-file name)
      (return-from find-ginfo-file-buffer (find-file-buffer name)))
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
				      `("-d" "-c" ,(unix-namestring name))
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
       (let* ((buffer (find-ginfo-file-buffer
		       (concatenate 'simple-string
				    "ginfo:"
				    (subseq ichars 0 pos))))
	      (mark (buffer-start-mark buffer)))
	 ;; Skip the introduction.
	 (find-pattern mark fseparator)
	 (insert-region begin (region mark (buffer-end-mark buffer)))
	 (delete-region (region (copy-mark begin) (line-start (line-offset begin 1))))
	 (delete-buffer buffer)))))

(defun parse-ginfo-file (filename)
  "Parse the GNU Info file Filename into an Info list, returning the list."
  (let* ((b (find-ginfo-file-buffer filename))
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

(defun find-ginfo-node (name tree)
  "Find node named Name in Tree."
  (dolist (node tree)
    (let ((a (assoc :name node)))
      (if (string= (cadr a) name)
	  (return node)))))

(defun show-ginfo-node (name-or-node &key (add-to-history t))
  "If Name-or-node is \"(dir)\" switch to GNU Info dir, else show
   Name-or-node in current buffer."
  (let ((buffer (current-buffer))
	(node (if (consp name-or-node)
		  name-or-node
		  (if (string= name-or-node "(dir)")
		      (return-from show-ginfo-node (ginfo-command nil))
		      (find-ginfo-node name-or-node
				       (value ginfo-tree))))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (insert-region (buffer-start-mark buffer)
		     (cadr (assoc :content node)))
      (setv ginfo-node node)
      (when add-to-history
	(let* ((current-history (value current-ginfo-history))
	       (history-element (cons node nil))
	       (new-history (cons history-element current-history)))
	  (if current-history
	      ;; Link the next "slot" of the previous element to the new
	      ;; history, for moving forward through the history.
	      (setf (cdar current-history) new-history))
	  (setv current-ginfo-history new-history)
	  (setv ginfo-history new-history)))
      (exchange-point-and-mark-command nil))))


;;; Commands.

(defcommand "GInfo" (p &optional (filename "ginfo:dir"))
  "Switch to the GNU Info directory buffer, creating it if necessary."
  "Switch to the buffer for GNU Info file Filename, creating it if
   necessary."
  (declare (ignore p))
  (let* ((buf-name (format nil "GInfo ~A" filename))
	 (buffer (getstring buf-name *buffer-names*)))
    (if buffer
	(change-to-buffer buffer)
	(let* ((tree (parse-ginfo-file filename))
	       (node (car tree))
	       ;; Make buffer after parse-ginfo-file, in case that fails.
	       (buffer (make-buffer buf-name
				    :modes '("Fundamental" "View" "GInfo"))))
	  (change-to-buffer buffer)
;	  (message "") ;; FIX
;	  (force-output *echo-area-stream*)
	  (setf (value view-return-function) #'(lambda ()))
	  (defhvar "GInfo Tree"
	    "The tree of nodes associated with this buffer."
	    :buffer buffer
	    :value tree)
	  (defhvar "GInfo Node"
	    "The info node displayed in this buffer."
	    :buffer buffer
	    :value node)
	  (defhvar "GInfo History"
	    "A history of info nodes displayed in this buffer."
	    :buffer buffer
	    :value ())
	  (defhvar "Current GInfo History"
	    "The history from the current node to the beginning."
	    :buffer buffer
	    :value ())
	  (show-ginfo-node node)))))

(defcommand "Next GInfo Node" (p)
  "Show the next Info node, if there is such a node."
  "Show the next Info node, if there is such a node."
  (declare (ignore p))
  (let ((nexta (assoc :next (value ginfo-node))))
    (when nexta
      (show-ginfo-node (cadr nexta)))))

(defcommand "Previous GInfo Node" (p)
  "Show the previous Info node, if there is such a node."
  "Show the previous Info node, if there is such a node."
  (declare (ignore p))
  (let ((preva (assoc :previous (value ginfo-node))))
    (when preva
      (show-ginfo-node (cadr preva)))))

(defcommand "Parent GInfo Node" (p)
  "Show the parent of the currently displayed node."
  "Show the parent of the currently displayed node."
  (declare (ignore p))
  (let ((parenta (assoc :parent (value ginfo-node))))
    (when parenta
      (show-ginfo-node (cadr parenta)))))

(defcommand "Top GInfo Node" (p)
  "Show the top of the tree of the currently displayed node."
  "Show the top of the tree of the currently displayed node."
  (declare (ignore p))
  (show-ginfo-node "Top"))

(defcommand "Forward GInfo Node" (p)
  "Show the next node from the history of nodes."
  "Show the next node from the history of nodes."
  (declare (ignore p))
  (let* ((hist (value current-ginfo-history))
	 (next (if hist (cdar hist))))
    (if next
	(progn
	  (setv current-ginfo-history next)
	  (show-ginfo-node (caar next) :add-to-history nil))
	(message "End of history."))))

(defcommand "Backward GInfo Node" (p)
  "Show the previous node from the history of nodes."
  "Show the previous node from the history of nodes."
  (declare (ignore p))
  (let* ((hist (value current-ginfo-history))
	 (prev (if hist (cdr hist))))
    (if prev
	(progn
	  (setv current-ginfo-history prev)
	  (show-ginfo-node (caar prev) :add-to-history nil))
	(message "Beginning of history."))))

;; FIX refs may be anywhere on line
;; FIX     incl return pos in line
(defcommand "Next GInfo Reference" (p)
  "Move point to the next reference, wrapping if required.
Return the position of the reference in the line it is on."
  "Move point to the next reference, wrapping if required.
Return the position of the reference in the line it is on."
  (declare (ignore p))
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

(defcommand "Previous GInfo Reference" (p)
  "Move point to the previous reference, wrapping if required."
  "Move point to the previous reference, wrapping if required."
  (declare (ignore p))
  ;; FIX
  (next-ginfo-reference-command nil))

;; FIX handle * envsubst: (gettext)envsubst Invocation.
;;                                 ^^^^^^^^^^^^^^^^^^^
(defcommand "GInfo Node from Point" (p)
  "Show the node cited by the first reference after point."
  "Show the node cited by the first reference after point."
  (declare (ignore p))
  (let* ((chars (line-string (mark-line (current-point))))
	 (start (or (and (> (length chars) 4)
			 (string= chars "* " :end1 2)
			 0)
		    (let ((pos (next-ginfo-reference-command nil)))
		      (when pos
			(setq chars (line-string (mark-line (current-point))))
			pos)))))
    (when start
      (let ((end (search "::" chars :start2 start)))
	(if end
	  (show-ginfo-node (subseq chars (+ start 2) end))
	  (progn
	    ;; Swap sense of start and end.
	    (setq end (search ": (" chars :start2 start))
	    (setq start (search ")" chars :start2 end))
	    (ginfo-command nil (concatenate 'simple-string
					    "ginfo:"
					    (subseq chars (+ end 3) start)))))))))
