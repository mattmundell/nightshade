;;; Info mode.

;; FIX TODO: Remember previous position of point in node.
;;       Scroll across nodes.
;;       Handle more than one buffer of same manual.
;;       Improve speed of loading large multiple-file manuals.
;;       Show from Menu (m).
;;       Show from Index (i).
;;       Search (s).
;;       Goto Node (g).
;;       FIXes below.

(in-package "ED")


;;; Structure.

(defmode "Info" :major-p nil
  :precedence 6.0 ; Must be higher than View mode.
  :documentation
  "Info Mode.")

(defstruct (directory-node (:constructor make-directory-node
					 (&key content next previous parent))
			   (:include parse::node))
  "A directory node.")

(defevar "Info Directory"
  "The Info Directory node."
  :value ())

#| both ~400MB
(progn
  (with-open-file (file ":tmp/info" :direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (let ((*package* (or (find-package "ED")
			 (editor-error "Failed to find ED package."))))
      (write (value info-directory)
	     :stream file :readably t)
      (terpri file))))

(progn
  (with-open-file (file ":tmp/info2" :direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (let ((*package* (or (find-package "ED")
			 (editor-error "Failed to find ED package."))))
      (write (value info-directory)
	     :stream file :readably nil)
      (terpri file))))
|#

(defvar info-dir-manuals '( ;"target:doc/ed/user/user.mss"
			    ;"target:doc/ed/cim/cim.mss"
			    ;"target:doc/rtguts.mss"
			    ;"target:doc/building.mss"
			   "target:doc/fas.mss"
			   )
  "List of Scribe manuals for the Info directory.")

#|
(defun get-info-directory ()
  "Return the Info directory node."
  (or (value info-directory)
      (do ((dir (make-directory-node))
	   (manuals info-dir-manuals (cdr manuals))
	   (content nil))
	  ((eq manuals nil)
	   (setv info-directory dir))
	(let* ((parse::*stream* (open (car manuals) :direction :input))
	       (parse::*streams* `((,parse::*stream* 0)))
	       (first-stream parse::*streams*))
	  (when parse::*stream*
	    (let ((node (parse-manual)))
	      (when node
		(setf (parse:node-parent node) dir)
		(if content
		    (progn
		      (setf (parse:node-next content) node)
		      (setf (parse:node-previous node) content))
		    (setf (parse:node-content dir) node))
		(setq content node)))
	    (mapcar (lambda (l) (close (car l))) first-stream))))))
|#

(defvar *info* ())

(defun get-info-directory ()
  "Return the Info directory node."
  (or *info*
      (editor-error "FIX need parse-manual (in parse-scribe.lisp)")
#|
      (setq *info*
	    (let ((dir (make-directory-node)))
	      (do ((manuals info-dir-manuals (cdr manuals))
		   (content nil))
		  ((eq manuals nil)
		   dir)
		(msg "Parsing ~A" (car manuals))
		(let* ((parse::*stream* (open (car manuals) :direction :input))
		       (parse::*streams* `((,parse::*stream* 0)))
		       (first-stream parse::*streams*))
		  (when parse::*stream*
		    (let ((node (parse-manual)))
		      (when node
			(setf (parse:node-parent node) dir)
			(if content
			    (progn
			      (setf (parse:node-next content) node)
			      (setf (parse:node-previous node) content))
			    (setf (parse:node-content dir) node))
			(setq content node)))
		    (mapcar (lambda (l) (close (car l))) first-stream))))
	      dir))
|#
      )
  t)

(defun get-info-directory () "Documentation")


;;; Commands.

(defcommand "Info" (p node)
  "Switch to the Info buffer, creating it if necessary."
  (declare (ignore p))
  (let* ((buf-name (format () "Info"))
	 (buffer (getstring buf-name *buffer-names*)))
    (if buffer
	(progn
	  (change-to-buffer buffer)
	  (when node
	    (or (eq (value info-node) node)
		(show-node node))))
	(let* ((tree (or node (get-info-directory)))
	       (node tree)
	       (buffer (make-buffer buf-name
				    :modes '("Fundamental" "Info" "View"))))
	  (setf (buffer-pathname buffer) #p"n:src/")
	  (change-to-buffer buffer)
	  (setf (value view-return-function) #'(lambda ()))
	  (defevar "Info Top"
	    "The top node associated with the current node."
	    :buffer buffer
	    :value tree)
	  (defevar "Info Node"
	    "The info node in this buffer."
	    :buffer buffer
	    :value node)
	  (defevar "Info Parent"
	    "The parent of the current node."
	    :buffer buffer
	    :value node)
	  (defevar "Info Next"
	    "The node next to the current node."
	    :buffer buffer
	    :value node)
	  (defevar "Info Previous"
	    "The node before the current node."
	    :buffer buffer
	    :value node)
	  (defevar "Info References"
	    "List of reference node associations."
	    :buffer buffer
	    :value (make-hash-table :test 'equal))
	  (defevar "Info History"
	    "A history of info nodes that have been displayed in this buffer."
	    :buffer buffer
	    :value ())
	  (defevar "Current Info History"
	    "The history from the current node to the beginning."
	    :buffer buffer
	    :value ())
	  (show-node node)))))

(defcommand "Info Directory" ()
  "Show the next Info directory."
  (info-command nil (get-info-directory)))

(defcommand "Next Info Node" ()
  "Show the next Info node, if there is such a node."
  (let ((next (value info-next)))
    (if next
	(show-node next)
	(message "Last node."))))

(defcommand "Previous Info Node" ()
  "Show the previous Info node, if there is such a node."
  (let ((previous (value info-previous)))
    (if previous
	(show-node previous)
	(message "First node."))))

(defcommand "Parent Info Node" ()
  "Show the parent of the currently displayed node."
  (let ((parent (value info-parent)))
    (if parent
	(show-node parent)
	(message "Top node."))))

(defcommand "Top Info Node" ()
  "Show the top of the tree of the currently displayed node."
  (let ((top (value info-top)))
    (when top
      (show-node top))))

(defcommand "Forward Info Node" ()
  "Show the next node from the history of nodes."
  (let* ((hist (value current-info-history))
	 (next (if hist (cadar hist))))
    (if next
	(progn
	  ;; Set the position in the current element.
	  (setf (caddar hist)
		(count-characters (region (buffer-start-mark (current-buffer))
					  (buffer-point (current-buffer)))))
	  (setv current-info-history next)
	  (show-node (caar next) :add-to-history ())
	  (character-offset (current-point) (caddar next)))
	(message "End of history."))))

(defcommand "Backward Info Node" ()
  "Show the previous node from the history of nodes."
  (let* ((hist (value current-info-history))
	 (prev (if hist (cdr hist))))
    (if prev
	(progn
	  ;; Set the position in the current element.
	  (setf (caddar hist)
		(count-characters (region (buffer-start-mark (current-buffer))
					  (buffer-point (current-buffer)))))
	  (setv current-info-history prev)
	  (show-node (caar prev) :add-to-history ())
	  (character-offset (current-point) (caddar prev)))
	(message "Beginning of history."))))

;; FIX     incl return pos in line
(defcommand "Next Info Reference" (p)
  "Move point to the next reference, wrapping if required.
   Return the position of the reference in the line it is on."
  (let ((mark (copy-mark (current-point))))
    (if p (mark-before mark) (mark-after mark))
    (if (find-character mark #\[ :backward p)
	(move-mark (current-point) mark))))
#|
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
|#

(defcommand "Previous Info Reference" (p)
  "Move point to the previous reference, wrapping if required."
  (next-info-reference-command (fi p)))

(defhistory *info-prompt-history* *info-prompt-history-pointer* 200)

(defun prompt-for-info-node ()
  (prompt-for-keyword (list lisp::*documentation*)
		      :prompt "Info node: "
		      :history *info-prompt-history*
		      :history-pointer
		      '*info-prompt-history-pointer*))

(defcommand "Read Info Node" ()
  "Show a prompted node."
  (show-node (prompt-for-info-node)))

(defcommand "Info Node from Point" ()
  "Show the node cited by the first reference after point."
  (let* ((point (current-point))
	 (mark (copy-mark point)))
    (when (and (or (char= (next-character mark) #\[)
		   (find-character mark #\[ :backward t))
	       (equal (mark-line mark) (mark-line point)))
      (let ((mark2 (copy-mark point)))
	(when (if (find-character mark2 #\] :backward t)
		  (mark> mark mark2)
		  t)
	  (let ((mark3 (copy-mark point)))
	    (when (find-character mark3 #\])
	      (mark-after mark)
	      ;(mark-before mark3)
	      (let ((name (string-trim '(#\space)
				       (region-to-string (region mark
								 mark3)))))
		(if (and (> (length name) 4)
			 (string= name "file:" :end1 5))
		    (find-command () (string-trim '(#\space)
						  (subseq name 5)))
		    (show-node name))))))))))

(defcommand "Refresh Info" ()
  "Refresh the info buffer."
  (show-node (value info-node) :add-to-history ()))

#|
  (let* ((chars (line-string (mark-line (current-point))))
	 (start (or (and (> (length chars) 4)
			 (string= chars "* " :end1 2)
			 0)
		    (let ((pos (next-info-reference-command)))
		      (when pos
			(setq chars
			      (line-string (mark-line (current-point))))
			pos)))))
    (when start
      (let ((end (search "::" chars :start2 start)))
	(if end
	    (let* ((node-name (subseq chars (+ start 2) end))
		   (node (lookup-reference node-name)))
	      (show-node (or node node-name)))
	    (progn
	      (editor-error "FIX")
	      ;; Swap sense of start and end.
	      (setq end (search ": (" chars :start2 start))
	      (setq start (search ")" chars :start2 end))
	      (info-command nil
			    (concatenate 'simple-string
					 "info:"
					 (subseq chars
						 (+ end 3) start)))))))))
|#

(defcommand "Edit Node Source" ()
  "Edit the source of the current node."
  (let ((title (value info-node)))
    (force-output)
    (typecase title
      (string
       (force-output)
       (let ((node (getstring title lisp::*documentation*))
	     (offset (count-characters (region (buffer-start-mark (current-buffer))
					       (buffer-point (current-buffer))))))
	 (force-output)
	 (when node
	   (let ((file (lisp::docnode-file node))
		 (pos (lisp::docnode-position node)))
	     (when file
	       (change-to-buffer (find-file-buffer file))
	       (when pos
		 (buffer-start (current-point))
		 (character-offset (current-point) (+ pos offset 1))))))))
      (t (editor-error "Only docnode editing is implemented.")))))


;;; Helper functions.

(defun show-node (node &key (add-to-history t))
  "Insert $node in current buffer."
  (or (getstring node lisp::*documentation*)
      (editor-error "Failed to find node `~A'" node))
  (let* ((buffer (current-buffer))
	 (mark (copy-mark (current-point)))
	 (pos (count-characters (region (buffer-start-mark buffer)
					(buffer-point buffer)))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      ;; FIX Use string-table?
      (setv info-references (make-hash-table :test 'equal))
      (insert-node mark node)
      (when add-to-history
	(let* ((current-history (value current-info-history))
	       (history-element (list node () 0))
	       (new-history (cons history-element current-history)))
	  (when current-history
	    ;; Link the next "slot" of the previous element to the new
	    ;; history, for moving forward through the history.
	    (setf (cadar current-history) new-history)
	    ;; Set the position in the previous element.
	    (setf (caddar current-history) pos))
	  (setv current-info-history new-history)
	  (setv info-history new-history)))
      (exchange-point-and-mark-command))))

(defun lookup-reference (name)
  "Return node referred to by Name."
  (or (gethash name (value ed::info-references)) name))

#|
(defun find-heading (head)
  "Find a heading node in head-node Head."
  (do ((part (parse:node-content head) (parse:node-next part)))
      ((eq part nil))
    (typecase part
      (text-node
       (do ((text-part (parse:node-content part) (parse:node-next text-part)))
	   ((eq text-part nil))
	 (typecase text-part
	   (heading-node
	    (return-from find-heading text-part))))))))
|#

(defun insert-nodes (mark node)
  "Insert Node and any siblings following Node, recursively, at Mark."
  (insert-node mark node)
  (if (parse:node-next node) (insert-nodes mark (parse:node-next node))))

#|
(defun insert-source-node (mark node)
  "Insert Node at Mark, recursively."
  (etypecase node
    (string
     (let ((doc (getstring node lisp::*documentation*))
	   (start (copy-mark mark :right-inserting)))
       (or doc (error "Failed to find doc node ~A." node))
       (insert-string mark node)
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-string mark (parse:node-content doc))
       (loop while (mark< start mark) do
	 (when (char= (next-character start) #\[)
	   (delete-characters start)
	   (insert-character start #\*)
	   (or (find-character start #\])
	       (error "Expected closing bracket."))
	   (delete-characters start)
	   (reverse-find-attribute start :word-delimiter #'zerop)
	   (insert-string start "::"))
	 (line-offset start 1 0))))

    ;; Scribe manual nodes.
    (epsilon-node)
    (make-node)
    (head-part-node)
    (comment-node)
    (comment-block-node)
    (tabclear-node)
    (tabdivide-node)
    (newpage-node)
    (blankspace-node)
    (label-node)
    (index-node)
    (include-node)
    (tag-node)

    (directory-node
     (insert-string mark "Info directory")
     (insert-character mark #\newline)
     (insert-string mark "**************")
     (insert-character mark #\newline)
     (insert-character mark #\newline)
     (let ((manual (parse:node-content node)))
       (when manual
	 (do ((manual manual (parse:node-next manual)))
	     ((eq manual nil))
	   (let* ((head (parse:node-next (parse:node-content manual)))
 		  (heading (find-heading head))
 		  (arg-node (parse:node-next
 			     (parse:node-next (parse:node-content heading))))
 		  (arg (parse:node-content
 			(parse:node-next
 			 (parse:node-content (parse:node-content arg-node))))))
;		  (arg (string-to-region "foo")))
	     (insert-string mark "[")
	     (insert-region mark arg)
	     (insert-string mark "]")
	     (insert-character mark #\newline))))
       ;(insert-node mark "_Top_")
       ))

    (heading-node
     (let* ((arg-node (parse:node-next (parse:node-next (parse:node-content node))))
	    (arg (parse:node-next (parse:node-content (parse:node-content arg-node)))))
       (insert-node mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters (parse:node-content arg)))
	 (insert-character mark #\*))
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (line-part-node
     (insert-nodes mark (parse:node-next (parse:node-content node)))) ; pass @
    (paragraph-part-node
     (insert-nodes mark (parse:node-next (parse:node-content node)))) ; pass @

    (paragraph-node
     (do ((part (parse:node-content node) (parse:node-next part))
	  (inserted nil))
	 ((eq part nil)
	  (if inserted
	      ;; Nicer to insert this newline only if there is more to
	      ;; follow.  Maybe the newline should be inserted before the
	      ;; paragraph instead of after.
	      (insert-character mark #\newline)))
       (typecase part
	 (char-node)
; 	  ;; Pass newlines.
; 	  (let ((next (parse:node-next part)))
; 	    (while (and next (typecase next (char-node)))
; 	      (setq next (parse:node-next next)))
; ;	    (insert-character mark #\newline)
; ;	    (or next
; ; 		(progn
; ; 		  (let ((old-point (copy-mark (current-point)))
; ; 			(point (current-point)))
; ; 		    (move-mark point mark)
; ; ;		    (elet ((ed::fill-paragraph-confirm nil))
; ; 		    (ed::fill-paragraph-command)
; ; 		    (move-mark point old-point))))
; 	    ))
	 (line-node
	  (do* ((ele (parse:node-content part) (parse:node-next ele))
		(insert nil))
	       ((or insert (eq ele nil))
		(when insert
		  (insert-node mark part)
		  (insert-character mark #\newline)
		  (setq inserted t)))
	    (typecase ele
	      (line-part-node
	       (let ((part-content (parse:node-next (parse:node-content ele))))
		 (typecase part-content
		   ((or index-node label-node tag-node comment-node
			tabclear-node newpage-node))
		   (t
		    (setq insert ele)))))
	      (region-node
	       (or (every (lambda (ch) (or (eq ch #\ ) (eq ch #\tab)))
			  (region-to-string (parse:node-content ele)))
		   (setq insert ele)))
	      (t
	       (setq insert ele)))))
	 (t
	  (setq inserted t)
	  (insert-node mark part)
	  (insert-character mark #\newline)))))

;     (line-node
;      (let ((content (parse:node-content node)))
;        (typecase content
; 	 (line-part-node
; 	  (let ((part (parse:node-next (parse:node-content content))))
; 	    (typecase part
; 	      (index-node
; 	       (ed::message "index")
; 	       ;; Move over index and newline when index alone on line.
; 	       (when (parse:node-next content)
; 		 (insert-nodes mark content)))
; 	      (t
; 	       (insert-nodes mark content)))))
; 	 (t
; 	  (insert-nodes mark content)))))

    (manual-node
     (let* ((make (parse:node-content node))
	    (heading (parse:node-next make))
	    (chapter (parse:node-next heading)))
       (insert-string mark "#[ ")
       (insert-character mark #\newline)
       (insert-node mark make)
       (insert-node mark heading)
       (etypecase chapter
	 (epsilon-node)
	 (chapter-node
	  (do ((chapter (parse:node-next heading) (parse:node-next chapter)))
	      ((eq chapter nil))
	    (let* ((next (parse:node-next (parse:node-content chapter)))
		   (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content next))))))
	      (insert-string mark "[ ")
	      (insert-region mark arg)
	      (insert-string mark " ]")
	      (insert-character mark #\newline)))))
       (insert-string mark "]#")
       (insert-character mark #\newline)))

;     (text-node
;      (let ((content (parse:node-content node)))
;        (typecase content
; 	 (char-node
; 	  (if (eq (parse:node-content content) #\newline)
; 	      (do ((next (parse:node-next content) (parse:node-next next)))
; 		  ((or (eq next nil)
; 		       (typecase next
; 			 (paragraph-node
; 			  (insert-nodes mark next)
; 			  t)))))
; 	      (editor-error "A char-node in a text-node should be a newline.")))
; 	 (t
; 	  (insert-nodes mark content)))))

    (chapter-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-string mark "#[ ")
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (do ((part (parse:node-next (parse:node-next (parse:node-content node))) ; @chap, argument
		  (parse:node-next part))
	    (first t))
	   ((eq part nil))
	 (typecase part
	   (section-node
	    (when first
	      (setq first nil))
	    (let* ((arg-node (parse:node-next (parse:node-content part)))
		   (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
	      (insert-string mark "[ ")
	      (insert-region mark arg)
	      (insert-string mark " ]")
	      (insert-character mark #\newline)))
	   (t
	    (insert-node mark part))))
       (insert-string mark "]#")
       (insert-character mark #\newline)))

    (section-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-string mark "#[ ")
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (do ((part (parse:node-next (parse:node-next (parse:node-content node))) ; @section, argument
		  (parse:node-next part))
	    (first t))
	   ((eq part nil)
	    (when first
	      (let ((next (parse:node-next node)))
		(if next
		    t
		    (do* ((parent (parse:node-parent node) (parse:node-parent parent)))
			 ((eq parent nil))
		      (let ((next (parse:node-next parent)))
			(when next
			  (return-from nil))))))))
	 (typecase part
	   (subsection-node
	    (when first
	      (setq first nil))
	    (let* ((arg-node (parse:node-next (parse:node-content part)))
		   (arg (parse:node-content (parse:node-next (parse:node-content
						  (parse:node-content arg-node))))))
	      (insert-string mark "[ ")
	      (insert-region mark arg)
	      (insert-string mark " ]")
	      (insert-character mark #\newline)))
	   (t
	    (insert-node mark part))))
       (insert-string mark "]#")
       (insert-character mark #\newline)))

    (subsection-node
     (let ((next (parse:node-next node)))
       (if next
	   t
	   (do* ((parent (parse:node-parent node) (parse:node-parent parent)))
		((eq parent nil))
	     (let ((next (parse:node-next parent)))
	       (when next
		 (return-from nil))))))
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-string mark "#[ ")
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-nodes mark (parse:node-next arg-node))
       (insert-string mark "]#")
       (insert-character mark #\newline)))

    (named-para-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       ;; The underlining may be too strong.
       (dotimes (i (count-characters arg))
	 (insert-character mark #\-))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-node mark (parse:node-next arg-node))))

    ;; For "example" blocks.
    (block-node
     ;; FIX indent
     (do ((part (parse:node-next (parse:node-content node))
		(parse:node-next part))
	  (first t))
	 ((eq part nil)
	  (insert-character mark #\newline))
       (etypecase part
	 (char-node)
	 (epsilon-node)
	 (block-end-node)
	 (paragraph-node
	  (if first
	      (progn
		(setq first nil)
		(insert-character mark #\newline)
		(insert-character mark #\newline))
	      (insert-character mark #\newline))
	  (insert-node mark part))
	 (line-node
	  (insert-node mark part)
	  (insert-character mark #\newline)))))

    (center-block-node
     ;; FIX center-line-command (maybe *center*)
     (insert-node mark (parse:node-next (parse:node-content node))))

    (text-block-node
     (insert-node mark (parse:node-next (parse:node-content node))))

    (format-block-node
     (insert-node mark (parse:node-next (parse:node-content node))))

    (example-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-character mark #\newline)
       ;; FIX indent
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (program-eg-block-node
     ;; FIX indent
     (insert-character mark #\newline)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\newline))

    (program-eg-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-character mark #\newline)
       ;; FIX indent
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (description-node
     (insert-character mark #\newline)
     (do ((descript (parse:node-next (parse:node-next (parse:node-content node)))
		    (parse:node-next descript))
	  (first t))
	 ((eq descript nil))
       (etypecase descript
	 (char-node)
	 (epsilon-node)
	 (description-end-node)
	 (descript-node
	  (let ((arg (parse:node-content descript)))
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "  ")
	    (insert-node mark arg)
	    (insert-character mark #\newline)
	    (insert-string mark "     ")
	    (do ((para-part (parse:node-content (parse:node-next (parse:node-next (parse:node-next arg))))
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))
; 			  (progn
; 			    (let ((old-point (copy-mark (current-point)))
; 				  (point (current-point)))
; 			      (move-mark point mark)
; 			      (elet ((ed::fill-lisp-comment-paragraph-confirm nil))
; 				(ed::fill-lisp-comment-paragraph-command))
; 			      (move-mark point old-point)))
			  ))))
		(t
		 (insert-node mark para-part)))))))))

    (enumerate-node
     (insert-character mark #\newline)
     (do ((item (parse:node-next (parse:node-next (parse:node-content node)))
		(parse:node-next item))
	  (item-num 0)
	  (first t))
	 ((eq item nil))
       (etypecase item
	 (char-node)
	 (enumerate-end-node)
	 (paragraph-node
	  (incf item-num)
	  (if first
	      (setq first nil)
	      (progn
		(insert-character mark #\newline)
		(insert-character mark #\newline)))
	  (insert-string mark (format nil "  ~A) " item-num))
	  (do ((para-part (parse:node-content item)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part))))))))

    (itemize-node
     (insert-character mark #\newline)
     (do ((item (parse:node-next (parse:node-next (parse:node-content node)))
		(parse:node-next item))
	  (first t))
	 ((eq item nil))
       (etypecase item
	 (char-node)
	 (itemize-end-node)
	 (paragraph-node
	  (if first
	      (setq first nil)
	      (progn
		(insert-character mark #\newline)
		(insert-character mark #\newline)))
	  (insert-string mark "  - ")
	  (do ((para-part (parse:node-content item)
			  (parse:node-next para-part)))
	      ((eq para-part nil))
	    (typecase para-part
	      (char-node
	       ;; Pass newlines.
	       (do ((next (parse:node-next para-part) (parse:node-next next)))
		   ((or (eq next nil)
			(typecase next (char-node nil) (t t)))
		    (if next
			(progn
			  (insert-character mark #\newline)
			  (insert-string mark "     "))))))
	      (t
	       (insert-node mark para-part))))))))

    (defcom-arg-node
      (let* ((name (parse:node-next (parse:node-next (parse:node-content node))))
	     (next (parse:node-next name)))
	(insert-node mark name)
	(typecase next
	  (d-a-body-node
	   (let ((bind (parse:node-next (parse:node-content next))))
	     (insert-character mark #\ )
	     (insert-node mark bind))))))

    (defevar-arg-node
      (let* ((name (parse:node-next (parse:node-next (parse:node-content node))))
	     (next (parse:node-next name)))
	(insert-node mark name)
	(etypecase next
	  (epsilon-node)
	  (d-a-spec-node
	   (let ((bind (parse:node-next (parse:node-next (parse:node-content next)))))
;	     (insert-string mark "  ")
	     (insert-character mark #\ )
	     (typecase bind
	       (region-node
		(insert-character mark #\{)
		(insert-node mark bind)
		(insert-character mark #\}))
	       (t
		(insert-node mark bind))))))))

    (defcom-node
     (insert-string mark " - Command: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defcom1-node
	    (insert-string mark " - Command: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defevar1-node
	    (insert-string mark " - Editor Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defun-arg-node
      (let* ((name (parse:node-next (parse:node-next (parse:node-content node))))
	     (next (parse:node-next name)))
	(insert-node mark name)
	;; FIX pkg, label
	(setq next (parse:node-next next))
	(setq next (parse:node-next next))
	(typecase next
	  (defun-arg-args-node
	   (let ((args (parse:node-next (parse:node-content next))))
	     (insert-character mark #\ )
	     (insert-node mark args))))
	(setq next (parse:node-next next))
	(typecase next
	  (defun-arg-keys-node
	    (let ((args (parse:node-next (parse:node-content next))))
	     (insert-string mark " &key ")
	     (insert-node mark args))))))

    (defun-node
     (insert-string mark " - Function: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defun1-node
	    (insert-string mark " - Function: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
 	   (defevar1-node
 	    (insert-string mark " - Editor Variable: ")
 	    (let ((arg (parse:node-next (parse:node-content body-part))))
 	      (insert-node mark arg)
 	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defvar-arg-node
      (let ((name (parse:node-next (parse:node-next (parse:node-content node)))))
	(insert-node mark name)))

    (defvar-node
     (insert-string mark " - Variable: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
;  	   (defevar1-node
;  	    (insert-string mark " - Editor Variable: ")
;  	    (let ((arg (parse:node-next (parse:node-content body-part))))
;  	      (insert-node mark arg)
;  	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defcon-node
     (insert-string mark " - Constant: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defcon1-node
	    (insert-string mark " - Constant: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
;  	   (defevar1-node
;  	    (insert-string mark " - Editor Variable: ")
;  	    (let ((arg (parse:node-next (parse:node-content body-part))))
;  	      (insert-node mark arg)
;  	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defmac-node
     (insert-string mark " - Macro: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defmac1-node
	    (insert-string mark " - Macro: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
; 	   (defevar1-node
; 	    (insert-string mark " - Variable: ")
; 	    (let ((arg (parse:node-next (parse:node-content body-part))))
; 	      (insert-node mark arg)
; 	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defevar-node
     (insert-string mark " - Editor Variable: ")
     ;; FIX same as in defcom-node
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (epsilon-node)
	   (defcom1-node
	    (insert-string mark " - Command: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defevar1-node
	    (insert-string mark " - Editor Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (argument-node
     (insert-node mark (parse:node-next (parse:node-content (parse:node-content node)))))

    (bracedarg-node
     (do* ((part (parse:node-next (parse:node-content (parse:node-content node))) next)
	   (next (parse:node-next part) (parse:node-next next)))
	  ((eq next nil))
       (insert-node mark part)))

    (hemlock-node
     (insert-string mark "the editor"))
    (emacs-node
     (insert-string mark "Emacs"))
    (windows-node
     (insert-string mark "X Windows"))
    (clisp-node
     (insert-string mark "Common Lisp"))
    (llisp-node
     (insert-string mark "Lisp"))
    (mh-node
     (insert-string mark "MH"))
    (at-node
     (insert-character mark #\@))
    (tab-node
     (insert-character mark #\tab))
    (dash-node
     (insert-string mark "--"))
    (nil-node
     (insert-string mark "nil"))
    (hid-node
     (insert-character mark #\`)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\'))
    (var-node
     (insert-character mark #\`)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\'))
    (kwd-node
     (insert-character mark #\:)
     (insert-node mark (parse:node-next (parse:node-content node))))
    (binding-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (i-node
     (insert-node mark (parse:node-next (parse:node-content node))))
    (f-node ;; FIX Font?
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (w-node ;; FIX Wide?
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (bf-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (b-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (center-node
     ;; FIX center-line-command (maybe *center*)
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (val-node
     (insert-string mark "value:")
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    ;; What is comma ("@;")?
    (comma-node)
    (multiple-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (optional-node
     (insert-string mark "&optional"))
    (rest-node
     (insert-string mark "&rest"))
    (key-node
     (insert-string mark "&key"))
    (mstar-node
     (insert-character mark #\{)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\})
     (insert-character mark #\*))
    (mor-node
     (insert-character mark #\|))
    (mgroup-node
     (insert-character mark #\()
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\)))
    (mopt-node
     (insert-character mark #\[)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\]))

    (ref-node
     (insert-character mark #\[)
     (let ((content (parse:node-content node)))
       (insert-node mark (parse:node-next content)))
     (insert-character mark #\]))

    (char-node
     (let ((content (parse:node-content node)))
       (or (eq content #\newline)
	   (insert-character mark content))))
    (node
     (let ((content (parse:node-content node)))
       (etypecase content
	 (node
	  (insert-nodes mark content))
	 (region
	  (insert-region mark content))
	 (base-char
	  (or (eq content #\newline)
	      (insert-character mark content)))
	 (string
	  (insert-string mark content)))))))
|#

#|
(progn (setq *info* (ed::value ed::info-node)) t)
|#

#|
(defun translate-all-manuals ()
  (while ((node *info* (parse:node-next node)))
	 (node)
    (translate-manuals node)))

(defun translate-manuals (node &optional buff)
  "Translate manuals to embedded doc format."
  (let ((buffer (or buff (make-unique-buffer "manual"))))
    (insert-source-node (buffer-point buffer) node)
    ;; Recurse into subsections.
    (while ((node (parse:node-content node)
		  (parse:node-next node)))
	   (node)
      (typecase node
	(manual-node
	 (translate-manuals node buffer))
	(chapter-node
	 (translate-manuals node buffer))
	(section-node
	 (translate-manuals node buffer))
	(subsection-node
	 (translate-manuals node buffer))))))
|#

(defun filter-info (mark)
  (while () ((find-character mark #\{))
    (cond ((at* "{function:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 10)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (with-output-to-mark (out mark)
			   (let* ((function (read-from-string name))
				  (args (function-args function)))
			     ;; FIX if macro " - Macro:"
			     (format out " - Function: ~A~:[ ~A~;~A~]~%~%   ~A~&"
				     name
				     args (or args "")
				     (documentation function 'function)))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{command:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 9)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (with-output-to-mark (out mark)
			   (let* ((command (getstring name *command-names*))
				  (function (if command (command-function command)))
				  (args (if function (function-args function))))
			     (format out " - Command: ~A~:[ ~A~;~A~]~%~%   ~A~&"
				     name
				     args (or args "")
				     (if command (command-documentation command))))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{variable:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 10)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let ((doc (documentation (read-from-string name) 'variable)))
			   (with-output-to-mark (out mark)
			     (format out " - Variable: ~A  ~A~%~@[~%   ~A~]~&"
				     name
				     (eval (read-from-string name))
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{evariable:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 11)
		   (let* ((name (string-trim '(#\space #\newline)
					     (region-to-string (region mark end))))
			  (var (string-to-variable name)))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let ((doc (if var (variable-documentation var))))
			   (with-output-to-mark (out mark)
			     (format out " - Editor variable: ~A  ~A~%~@[~%   ~A~]~&"
				     name
				     (if var (variable-value var))
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{package:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 9)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let* ((package (find-package (string-upcase name)))
				(doc (documentation package t))
				(nicks (mapcar (lambda (nick) (string-downcase nick))
					       (package-nicknames package))))
			   (with-output-to-mark (out mark)
			     (format out " - Package: ~A~@[ ~A~]~@[~%~%    ~A~]~&"
				     name
				     nicks
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{type:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 6)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let* ((type (read-from-string name))
				(doc (documentation type 'type)))
			   (with-output-to-mark (out mark)
			     (format out " - Type: ~A~@[~%~%    ~A~]~&"
				     name
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{alien-type:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 12)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let* ((type (read-from-string name))
				(doc (documentation type 'alien-type)))
			   (with-output-to-mark (out mark)
			     (format out " - Alien Type: ~A~@[~%~%    ~A~]~&"
				     name
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{condition:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 11)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let* ((type (read-from-string name))
				(doc (documentation type 'type)))
			   (with-output-to-mark (out mark)
			     (format out " - Condition: ~A~@[~%~%    ~A~]~&"
				     name
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  ((at* "{mode:" mark)
	   (let ((end (copy-mark mark)))
	     (if (find-character end #\})
		 (progn
		   (delete-characters end)
		   (delete-characters mark 6)
		   (let ((name (string-trim '(#\space #\newline)
					    (region-to-string (region mark end)))))
		     (delete-region (region mark end))
		     (fi name
			 (insert-string mark "??")
			 (let* ((doc (mode-documentation name)))
			   (with-output-to-mark (out mark)
			     (format out " - ~:[Minor~;Major~] Mode: ~A~@[~%~%    ~A~]~&"
				     (mode-major-p name)
				     name
				     doc doc))))))
		 (mark-after mark)))
	   (mark-after mark))
	  (t (mark-after mark)))))

;; FIX Ed Eval Defun of this (near bottom of file)  too big for lispmode?
;; fill-lisp-comment-paragraph-command
;; fill-paragraph-command
(defun insert-node (mark node)
  "Insert Node at Mark, recursively."
  (etypecase node
    ;; The name of a reader macro doc node.
    (string
     (setv ed::info-node node)
     (let ((doc (getstring node lisp::*documentation*))
	   (start (copy-mark mark :right-inserting)))
       (or doc (error "Failed to find doc node ~A." node))
       (insert-string mark node)
       (insert-character mark #\newline)
       (dotimes (i (length node)) (insert-character mark #\-))
       (insert-character mark #\newline)
       (move-mark start mark)
       (insert-string mark (lisp::docnode-content doc))
       (filter-info start)))
#|
    ;; Scribe manual nodes.
    (epsilon-node)
    (make-node)
    (head-part-node)
    (comment-node)
    (comment-block-node)
    (tabclear-node)
    (tabdivide-node)
    (newpage-node)
    (blankspace-node)
    (label-node)
    (index-node)
    (include-node)
    (tag-node)

    (directory-node
     (setv ed::info-node node)
     (setv ed::info-previous nil)
     (setv ed::info-parent node)
     (setv ed::info-top node)
     (setv ed::info-next nil)
     (insert-string mark "Info directory")
     (insert-character mark #\newline)
     (insert-string mark "**************")
     (insert-character mark #\newline)
     (insert-character mark #\newline)
     (let ((manual (parse:node-content node)))
       (when manual
	 (do ((manual manual (parse:node-next manual)))
	     ((eq manual nil))
	   (let* ((head (parse:node-next (parse:node-content manual)))
 		  (heading (find-heading head))
 		  (arg-node (parse:node-next
 			     (parse:node-next (parse:node-content heading))))
 		  (arg (parse:node-content
 			(parse:node-next
 			 (parse:node-content (parse:node-content arg-node))))))
;		  (arg (string-to-region "foo")))
	     (setf (gethash (region-to-string arg)
			    (value ed::info-references))
		   manual)
	     (insert-string mark "* ")
	     (insert-region mark arg)
	     (insert-string mark "::")
	     (insert-character mark #\newline))))
       (insert-node mark "_Top_")))

    (heading-node
     (let* ((arg-node (parse:node-next (parse:node-next (parse:node-content node))))
	    (arg (parse:node-next (parse:node-content (parse:node-content arg-node)))))
       (insert-node mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters (parse:node-content arg)))
	 (insert-character mark #\*))
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (line-part-node
     (insert-nodes mark (parse:node-next (parse:node-content node)))) ; pass @
    (paragraph-part-node
     (insert-nodes mark (parse:node-next (parse:node-content node)))) ; pass @

    (paragraph-node
     (do ((part (parse:node-content node) (parse:node-next part))
	  (inserted nil))
	 ((eq part nil)
	  (if inserted
	      ;; Nicer to insert this newline only if there is more to
	      ;; follow.  Maybe the newline should be inserted before the
	      ;; paragraph instead of after.
	      (insert-character mark #\newline)))
       (typecase part
	 (char-node)
; 	  ;; Pass newlines.
; 	  (let ((next (parse:node-next part)))
; 	    (while (and next (typecase next (char-node)))
; 	      (setq next (parse:node-next next)))
; ;	    (insert-character mark #\newline)
; ;	    (or next
; ; 		(progn
; ; 		  (let ((old-point (copy-mark (current-point)))
; ; 			(point (current-point)))
; ; 		    (move-mark point mark)
; ; ;		    (elet ((ed::fill-paragraph-confirm nil))
; ; 		    (ed::fill-paragraph-command)
; ; 		    (move-mark point old-point))))
; 	    ))
	 (line-node
	  (do* ((ele (parse:node-content part) (parse:node-next ele))
		(insert nil))
	       ((or insert (eq ele nil))
		(when insert
		  (insert-node mark part)
		  (insert-character mark #\newline)
		  (setq inserted t)))
	    (typecase ele
	      (line-part-node
	       (let ((part-content (parse:node-next (parse:node-content ele))))
		 (typecase part-content
		   ((or index-node label-node tag-node comment-node
			tabclear-node newpage-node))
		   (t
		    (setq insert ele)))))
	      (region-node
	       (or (every (lambda (ch) (or (eq ch #\ ) (eq ch #\tab)))
			  (region-to-string (parse:node-content ele)))
		   (setq insert ele)))
	      (t
	       (setq insert ele)))))
	 (t
	  (setq inserted t)
	  (insert-node mark part)
	  (insert-character mark #\newline)))))

;     (line-node
;      (let ((content (parse:node-content node)))
;        (typecase content
; 	 (line-part-node
; 	  (let ((part (parse:node-next (parse:node-content content))))
; 	    (typecase part
; 	      (index-node
; 	       (ed::message "index")
; 	       ;; Move over index and newline when index alone on line.
; 	       (when (parse:node-next content)
; 		 (insert-nodes mark content)))
; 	      (t
; 	       (insert-nodes mark content)))))
; 	 (t
; 	  (insert-nodes mark content)))))

    (manual-node
     (setv ed::info-node node)
     (setv ed::info-previous nil)
     (setv ed::info-parent (get-info-directory))
     (setv ed::info-top (get-info-directory))
     (let* ((make (parse:node-content node))
	    (heading (parse:node-next make))
	    (chapter (parse:node-next heading)))
       (insert-node mark make)
       (insert-node mark heading)
       (etypecase chapter
	 (epsilon-node)
	 (chapter-node
	  (setv ed::info-next chapter)
	  (do ((chapter (parse:node-next heading) (parse:node-next chapter)))
	      ((eq chapter nil))
	    (let* ((next (parse:node-next (parse:node-content chapter)))
		   (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content next))))))
	      (setf (gethash (region-to-string arg) (value ed::info-references))
		    chapter)
	      (insert-string mark "* ")
	      (insert-region mark arg)
	      (insert-string mark "::")
	      (insert-character mark #\newline)))))))

;     (text-node
;      (let ((content (parse:node-content node)))
;        (typecase content
; 	 (char-node
; 	  (if (eq (parse:node-content content) #\newline)
; 	      (do ((next (parse:node-next content) (parse:node-next next)))
; 		  ((or (eq next nil)
; 		       (typecase next
; 			 (paragraph-node
; 			  (insert-nodes mark next)
; 			  t)))))
; 	      (editor-error "A char-node in a text-node should be a newline.")))
; 	 (t
; 	  (insert-nodes mark content)))))

    (chapter-node
     (setv ed::info-node node)
     (setv ed::info-previous (or (parse:node-previous node)
				 (parse:node-parent node)))
     (setv ed::info-parent (parse:node-parent node))
     (setv ed::info-top (parse:node-parent node))
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters arg))
	 (insert-character mark #\*))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (do ((part (parse:node-next (parse:node-next (parse:node-content node))) ; @chap, argument
		  (parse:node-next part))
	    (first t))
	   ((eq part nil))
	 (typecase part
	   (section-node
	    (when first
	      (setq first nil)
	      (setv ed::info-next part))
	    (let* ((arg-node (parse:node-next (parse:node-content part)))
		   (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
	      (setf (gethash (region-to-string arg) (value ed::info-references))
		    part)
	      (insert-string mark "* ")
	      (insert-region mark arg)
	      (insert-string mark "::")
	      (insert-character mark #\newline)))
	   (t
	    (insert-node mark part))))))

    (section-node
     (setv ed::info-node node)
     (setv ed::info-previous (or (parse:node-previous node)
				 (parse:node-parent node)))
     (setv ed::info-parent (parse:node-parent node))
     (setv ed::info-top (parse:node-parent (parse:node-parent node)))
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters arg))
	 (insert-character mark #\=))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (do ((part (parse:node-next (parse:node-next (parse:node-content node))) ; @section, argument
		  (parse:node-next part))
	    (first t))
	   ((eq part nil)
	    (when first
	      (let ((next (parse:node-next node)))
		(if next
		    (setv ed::info-next next)
		    (do* ((parent (parse:node-parent node) (parse:node-parent parent)))
			 ((eq parent nil))
		      (let ((next (parse:node-next parent)))
			(when next
			  (setv ed::info-next next)
			  (return-from nil))))))))
	 (typecase part
	   (subsection-node
	    (when first
	      (setq first nil)
	      (setv ed::info-next part))
	    (let* ((arg-node (parse:node-next (parse:node-content part)))
		   (arg (parse:node-content (parse:node-next (parse:node-content
						  (parse:node-content arg-node))))))
	      (setf (gethash (region-to-string arg) (value ed::info-references))
		    part)
	      (insert-string mark "* ")
	      (insert-region mark arg)
	      (insert-string mark "::")
	      (insert-character mark #\newline)))
	   (t
	    (insert-node mark part))))))

    (subsection-node
     (setv ed::info-node node)
     (let ((previous (parse:node-previous node)))
       (setv ed::info-previous (typecase previous
				 (null)
				 (subsection-node
				  previous)
				 (t
				  (parse:node-parent node)))))
     (setv ed::info-parent (parse:node-parent node))
     (setv ed::info-top
	   (block nil
	     (parse:node-parent (or (parse:node-parent (or (parse:node-parent node)
					       (return-from nil)))
			      (return-from nil)))))
     (let ((next (parse:node-next node)))
       (if next
	   (setv ed::info-next next)
	   (do* ((parent (parse:node-parent node) (parse:node-parent parent)))
		((eq parent nil))
	     (let ((next (parse:node-next parent)))
	       (when next
		 (setv ed::info-next next)
		 (return-from nil))))))
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters arg))
	 (insert-character mark #\-))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-nodes mark (parse:node-next arg-node))))

    (named-para-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       ;; The underlining may be too strong.
       (dotimes (i (count-characters arg))
	 (insert-character mark #\-))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-node mark (parse:node-next arg-node))))

    ;; For "example" blocks.
    (block-node
     ;; FIX indent
     (do ((part (parse:node-next (parse:node-content node))
		(parse:node-next part))
	  (first t))
	 ((eq part nil)
	  (insert-character mark #\newline))
       (etypecase part
	 (char-node)
	 (epsilon-node)
	 (block-end-node)
	 (paragraph-node
	  (if first
	      (progn
		(setq first nil)
		(insert-character mark #\newline)
		(insert-character mark #\newline))
	      (insert-character mark #\newline))
	  (insert-node mark part))
	 (line-node
	  (insert-node mark part)
	  (insert-character mark #\newline)))))

    (center-block-node
     ;; FIX center-line-command (maybe *center*)
     (insert-node mark (parse:node-next (parse:node-content node))))

    (text-block-node
     (insert-node mark (parse:node-next (parse:node-content node))))

    (format-block-node
     (insert-node mark (parse:node-next (parse:node-content node))))

    (example-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-character mark #\newline)
       ;; FIX indent
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (program-eg-block-node
     ;; FIX indent
     (insert-character mark #\newline)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\newline))

    (program-eg-node
     (let* ((arg-node (parse:node-next (parse:node-content node)))
	    (arg (parse:node-content (parse:node-next (parse:node-content (parse:node-content arg-node))))))
       (insert-character mark #\newline)
       ;; FIX indent
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (description-node
     (insert-character mark #\newline)
     (do ((descript (parse:node-next (parse:node-next (parse:node-content node)))
		    (parse:node-next descript))
	  (first t))
	 ((eq descript nil))
       (etypecase descript
	 (char-node)
	 (epsilon-node)
	 (description-end-node)
	 (descript-node
	  (let ((arg (parse:node-content descript)))
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "  ")
	    (insert-node mark arg)
	    (insert-character mark #\newline)
	    (insert-string mark "     ")
	    (do ((para-part (parse:node-content (parse:node-next (parse:node-next (parse:node-next arg))))
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))
; 			  (progn
; 			    (let ((old-point (copy-mark (current-point)))
; 				  (point (current-point)))
; 			      (move-mark point mark)
; 			      (elet ((ed::fill-lisp-comment-paragraph-confirm nil))
; 				(ed::fill-lisp-comment-paragraph-command))
; 			      (move-mark point old-point)))
			  ))))
		(t
		 (insert-node mark para-part)))))))))

    (enumerate-node
     (insert-character mark #\newline)
     (do ((item (parse:node-next (parse:node-next (parse:node-content node)))
		(parse:node-next item))
	  (item-num 0)
	  (first t))
	 ((eq item nil))
       (etypecase item
	 (char-node)
	 (enumerate-end-node)
	 (paragraph-node
	  (incf item-num)
	  (if first
	      (setq first nil)
	      (progn
		(insert-character mark #\newline)
		(insert-character mark #\newline)))
	  (insert-string mark (format nil "  ~A) " item-num))
	  (do ((para-part (parse:node-content item)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part))))))))

    (itemize-node
     (insert-character mark #\newline)
     (do ((item (parse:node-next (parse:node-next (parse:node-content node)))
		(parse:node-next item))
	  (first t))
	 ((eq item nil))
       (etypecase item
	 (char-node)
	 (itemize-end-node)
	 (paragraph-node
	  (if first
	      (setq first nil)
	      (progn
		(insert-character mark #\newline)
		(insert-character mark #\newline)))
	  (insert-string mark "  - ")
	  (do ((para-part (parse:node-content item)
			  (parse:node-next para-part)))
	      ((eq para-part nil))
	    (typecase para-part
	      (char-node
	       ;; Pass newlines.
	       (do ((next (parse:node-next para-part) (parse:node-next next)))
		   ((or (eq next nil)
			(typecase next (char-node nil) (t t)))
		    (if next
			(progn
			  (insert-character mark #\newline)
			  (insert-string mark "     "))))))
	      (t
	       (insert-node mark para-part))))))))

    (defcom-arg-node
      (let* ((name (parse:node-next (parse:node-next (parse:node-content node))))
	     (next (parse:node-next name)))
	(insert-node mark name)
	(typecase next
	  (d-a-body-node
	   (let ((bind (parse:node-next (parse:node-content next))))
	     (insert-character mark #\ )
	     (insert-node mark bind))))))

    (defevar-arg-node
      (let* ((name (parse:node-next (parse:node-next (parse:node-content node))))
	     (next (parse:node-next name)))
	(insert-node mark name)
	(etypecase next
	  (epsilon-node)
	  (d-a-spec-node
	   (let ((bind (parse:node-next (parse:node-next (parse:node-content next)))))
;	     (insert-string mark "  ")
	     (insert-character mark #\ )
	     (typecase bind
	       (region-node
		(insert-character mark #\{)
		(insert-node mark bind)
		(insert-character mark #\}))
	       (t
		(insert-node mark bind))))))))

    (defcom-node
     (insert-string mark " - Command: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defcom1-node
	    (insert-string mark " - Command: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defevar1-node
	    (insert-string mark " - Editor Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defun-arg-node
      (let* ((name (parse:node-next (parse:node-next (parse:node-content node))))
	     (next (parse:node-next name)))
	(insert-node mark name)
	;; FIX pkg, label
	(setq next (parse:node-next next))
	(setq next (parse:node-next next))
	(typecase next
	  (defun-arg-args-node
	   (let ((args (parse:node-next (parse:node-content next))))
	     (insert-character mark #\ )
	     (insert-node mark args))))
	(setq next (parse:node-next next))
	(typecase next
	  (defun-arg-keys-node
	    (let ((args (parse:node-next (parse:node-content next))))
	     (insert-string mark " &key ")
	     (insert-node mark args))))))

    (defun-node
     (insert-string mark " - Function: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defun1-node
	    (insert-string mark " - Function: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
 	   (defevar1-node
 	    (insert-string mark " - Editor Variable: ")
 	    (let ((arg (parse:node-next (parse:node-content body-part))))
 	      (insert-node mark arg)
 	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defvar-arg-node
      (let ((name (parse:node-next (parse:node-next (parse:node-content node)))))
	(insert-node mark name)))

    (defvar-node
     (insert-string mark " - Variable: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
;  	   (defevar1-node
;  	    (insert-string mark " - Editor Variable: ")
;  	    (let ((arg (parse:node-next (parse:node-content body-part))))
;  	      (insert-node mark arg)
;  	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defcon-node
     (insert-string mark " - Constant: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defcon1-node
	    (insert-string mark " - Constant: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
;  	   (defevar1-node
;  	    (insert-string mark " - Editor Variable: ")
;  	    (let ((arg (parse:node-next (parse:node-content body-part))))
;  	      (insert-node mark arg)
;  	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defmac-node
     (insert-string mark " - Macro: ")
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defmac1-node
	    (insert-string mark " - Macro: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
; 	   (defevar1-node
; 	    (insert-string mark " - Variable: ")
; 	    (let ((arg (parse:node-next (parse:node-content body-part))))
; 	      (insert-node mark arg)
; 	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defevar-node
     (insert-string mark " - Editor Variable: ")
     ;; FIX same as in defcom-node
     (let ((def (parse:node-next (parse:node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (parse:node-next def) (parse:node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (epsilon-node)
	   (defcom1-node
	    (insert-string mark " - Command: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defevar1-node
	    (insert-string mark " - Editor Variable: ")
	    (let ((arg (parse:node-next (parse:node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (paragraph-node
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "     ")
	    ;; FIX same as in description-node
	    (do ((para-part (parse:node-content body-part)
			    (parse:node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (parse:node-next para-part) (parse:node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (argument-node
     (insert-node mark (parse:node-next (parse:node-content (parse:node-content node)))))

    (bracedarg-node
     (do* ((part (parse:node-next (parse:node-content (parse:node-content node))) next)
	   (next (parse:node-next part) (parse:node-next next)))
	  ((eq next nil))
       (insert-node mark part)))

    (hemlock-node
     (insert-string mark "Hemlock"))
    (emacs-node
     (insert-string mark "Emacs"))
    (windows-node
     (insert-string mark "X Windows"))
    (clisp-node
     (insert-string mark "Common Lisp"))
    (llisp-node
     (insert-string mark "Lisp"))
    (mh-node
     (insert-string mark "MH"))
    (at-node
     (insert-character mark #\@))
    (tab-node
     (insert-character mark #\tab))
    (dash-node
     (insert-string mark "--"))
    (nil-node
     (insert-string mark "nil"))
    (hid-node
     (insert-character mark #\`)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\'))
    (var-node
     (insert-character mark #\`)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\'))
    (kwd-node
     (insert-character mark #\:)
     (insert-node mark (parse:node-next (parse:node-content node))))
    (binding-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (i-node
     (insert-node mark (parse:node-next (parse:node-content node))))
    (f-node ;; FIX Font?
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (w-node ;; FIX Wide?
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (bf-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (b-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (center-node
     ;; FIX center-line-command (maybe *center*)
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (val-node
     (insert-string mark "value:")
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    ;; What is comma ("@;")?
    (comma-node)
    (multiple-node
     (insert-nodes mark (parse:node-next (parse:node-content node))))
    (optional-node
     (insert-string mark "&optional"))
    (rest-node
     (insert-string mark "&rest"))
    (key-node
     (insert-string mark "&key"))
    (mstar-node
     (insert-character mark #\{)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\})
     (insert-character mark #\*))
    (mor-node
     (insert-character mark #\|))
    (mgroup-node
     (insert-character mark #\()
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\)))
    (mopt-node
     (insert-character mark #\[)
     (insert-node mark (parse:node-next (parse:node-content node)))
     (insert-character mark #\]))

    (char-node
     (let ((content (parse:node-content node)))
       (or (eq content #\newline)
	   (insert-character mark content))))
    (node
     (let ((content (parse:node-content node)))
       (etypecase content
	 (node
	  (insert-nodes mark content))
	 (region
	  (insert-region mark content))
	 (base-char
	  (or (eq content #\newline)
	      (insert-character mark content)))
	 (string
	  (insert-string mark content)))))
|#
    ))
