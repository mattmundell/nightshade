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

(in-package "HEMLOCK")



;;; Structure.

(defmode "Info" :major-p nil
  :precedence 4.0
  :documentation
  "Info Mode.")


(defstruct (directory-node (:constructor make-directory-node
					 (&key content next previous parent))
			   (:include node))
  "A directory node.")

(defhvar "Info Directory"
  "The Info Directory node."
  :value nil)

#| both ~400MB
(progn
  (with-open-file (file ":tmp/info" :direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (let ((*package* (or (find-package "HEMLOCK")
			 (editor-error "Failed to find Hemlock package."))))
      (write (value info-directory)
	     :stream file :readably t)
      (terpri file))))

(progn
  (with-open-file (file ":tmp/info2" :direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (let ((*package* (or (find-package "HEMLOCK")
			 (editor-error "Failed to find Hemlock package."))))
      (write (value info-directory)
	     :stream file :readably nil)
      (terpri file))))
|#

(defvar info-dir-manuals '("target:docs/hem/user/user.mss"
			   "target:docs/hem/cim/cim.mss")
  "List of Scribe manuals for the Info directory.")

(defun get-info-directory ()
  "Return the Info directory node."
  (or (value info-directory)
      (do ((dir (make-directory-node))
	   (manuals info-dir-manuals (cdr manuals))
	   (content nil))
	  ((eq manuals nil)
	   (setv info-directory dir))
	(let* ((*stream* (open (car manuals) :direction :input))
	       (*streams* `((,*stream* 0)))
	       (first-stream *streams*))
	  (when *stream*
	    (let ((node (parse-manual)))
	      (when node
		(setf (node-parent node) dir)
		(if content
		    (progn
		      (setf (node-next content) node)
		      (setf (node-previous node) content))
		    (setf (node-content dir) node))
		(setq content node)))
	    (mapcar (lambda (l) (close (car l))) first-stream))))))



;;; Commands.

(defcommand "Info" (p &optional node)
  "Switch to the Info buffer, creating it if necessary."
  "Switch to the buffer for Info node Node, creating it if necessary."
  (declare (ignore p))
  (let* ((buf-name (format nil "Info"))
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
				    :modes '("Fundamental" "View" "Info"))))
	  (change-to-buffer buffer)
	  (setf (value view-return-function) #'(lambda ()))
	  (defhvar "Info Top"
	    "The top node associated with the current node."
	    :buffer buffer
	    :value tree)
	  (defhvar "Info Node"
	    "The info node in this buffer."
	    :buffer buffer
	    :value node)
	  (defhvar "Info Parent"
	    "The parent of the current node."
	    :buffer buffer
	    :value node)
	  (defhvar "Info Next"
	    "The node next to the current node."
	    :buffer buffer
	    :value node)
	  (defhvar "Info Previous"
	    "The node before the current node."
	    :buffer buffer
	    :value node)
	  (defhvar "Info References"
	    "List of reference node associations."
	    :buffer buffer
	    :value (make-hash-table :test 'equal))
	  (defhvar "Info History"
	    "A history of info nodes that have been displayed in this buffer."
	    :buffer buffer
	    :value ())
	  (defhvar "Current Info History"
	    "The history from the current node to the beginning."
	    :buffer buffer
	    :value ())
	  (show-node node)))))

(defcommand "Info Directory" (p)
  "Show the next Info directory."
  "Show the next Info directory."
  (declare (ignore p))
  (info-command nil (get-info-directory)))

(defcommand "Next Info Node" (p)
  "Show the next Info node, if there is such a node."
  "Show the next Info node, if there is such a node."
  (declare (ignore p))
  (let ((next (value info-next)))
    (if next
	(show-node next)
	(message "Last node."))))

(defcommand "Previous Info Node" (p)
  "Show the previous Info node, if there is such a node."
  "Show the previous Info node, if there is such a node."
  (declare (ignore p))
  (declare (ignore p))
  (let ((previous (value info-previous)))
    (if previous
	(show-node previous)
	(message "First node."))))

(defcommand "Parent Info Node" (p)
  "Show the parent of the currently displayed node."
  "Show the parent of the currently displayed node."
  (declare (ignore p))
  (let ((parent (value info-parent)))
    (if parent
	(show-node parent)
	(message "Top node."))))

(defcommand "Top Info Node" (p)
  "Show the top of the tree of the currently displayed node."
  "Show the top of the tree of the currently displayed node."
  (declare (ignore p))
  (let ((top (value info-top)))
    (when top
      (show-node top))))

(defcommand "Forward Info Node" (p)
  "Show the next node from the history of nodes."
  "Show the next node from the history of nodes."
  (declare (ignore p))
  (let* ((hist (value current-info-history))
	 (next (if hist (cdar hist))))
    (if next
	(progn
	  (setv current-info-history next)
	  (show-node (caar next) :add-to-history nil))
	(message "End of history."))))

(defcommand "Backward Info Node" (p)
  "Show the previous node from the history of nodes."
  "Show the previous node from the history of nodes."
  (declare (ignore p))
  (let* ((hist (value current-info-history))
	 (prev (if hist (cdr hist))))
    (if prev
	(progn
	  (setv current-info-history prev)
	  (show-node (caar prev) :add-to-history nil))
	(message "Beginning of history."))))

;; FIX     incl return pos in line
(defcommand "Next Info Reference" (p)
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

(defcommand "Previous Info Reference" (p)
  "Move point to the previous reference, wrapping if required."
  "Move point to the previous reference, wrapping if required."
  (declare (ignore p))
  ;; FIX
  (next-info-reference-command nil))

;; FIX handle * envsubst: (gettext)envsubst Invocation.
;;                                 ^^^^^^^^^^^^^^^^^^^
(defcommand "Info Node from Point" (p)
  "Show the node cited by the first reference after point."
  "Show the node cited by the first reference after point."
  (declare (ignore p))
  (let* ((chars (line-string (mark-line (current-point))))
	 (start (or (and (> (length chars) 4)
			 (string= chars "* " :end1 2)
			 0)
		    (let ((pos (next-info-reference-command nil)))
		      (when pos
			(setq chars
			      (line-string (mark-line (current-point))))
			pos)))))
    (when start
      (let ((end (search "::" chars :start2 start)))
	(if end
	    (show-node (lookup-reference (subseq chars (+ start 2) end)))
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



;;; Helper functions.

(defun show-node (node &key (add-to-history t))
  "Insert Node in current buffer."
  (let ((buffer (current-buffer))
	(mark (copy-mark (current-point))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      ;; FIX Use string-table?
      (setv info-references (make-hash-table :test 'equal))
      (insert-node mark node)
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
      (exchange-point-and-mark-command nil))))

(defun lookup-reference (name)
  "Return node referred to by Name."
  (gethash name (value ed::info-references)))

(defun find-heading (head)
  "Find a heading node in head-node Head."
  (do ((part (node-content head) (node-next part)))
      ((eq part nil))
    (typecase part
      (text-node
       (do ((text-part (node-content part) (node-next text-part)))
	   ((eq text-part nil))
	 (typecase text-part
	   (heading-node
	    (return-from find-heading text-part))))))))

(defun insert-nodes (mark node)
  "Insert Node and any siblings following Node, recursively, at Mark."
  (insert-node mark node)
  (if (node-next node) (insert-nodes mark (node-next node))))

;; fill-lisp-comment-paragraph-command
;; fill-paragraph-command
(defun insert-node (mark node)
  "Insert Node at Mark, recursively."
  (etypecase node
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
     (let ((manual (node-content node)))
       (when manual
	 (do ((manual manual (node-next manual)))
	     ((eq manual nil))
	   (let* ((head (node-next (node-content manual)))
 		  (heading (find-heading head))
 		  (arg-node (node-next
 			     (node-next (node-content heading))))
 		  (arg (node-content
 			(node-next
 			 (node-content (node-content arg-node))))))
;		  (arg (string-to-region "foo")))
	     (setf (gethash (region-to-string arg)
			    (value ed::info-references))
		   manual)
	     (insert-string mark "* ")
	     (insert-region mark arg)
	     (insert-string mark "::")
	     (insert-character mark #\newline))))))

    (heading-node
     (let* ((arg-node (node-next (node-next (node-content node))))
	    (arg (node-next (node-content (node-content arg-node)))))
       (insert-node mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters (node-content arg)))
	 (insert-character mark #\*))
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (line-part-node
     (insert-nodes mark (node-next (node-content node)))) ; pass @
    (paragraph-part-node
     (insert-nodes mark (node-next (node-content node)))) ; pass @

    (paragraph-node
     (do ((part (node-content node) (node-next part))
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
; 	  (let ((next (node-next part)))
; 	    (while (and next (typecase next (char-node)))
; 	      (setq next (node-next next)))
; ;	    (insert-character mark #\newline)
; ;	    (or next
; ; 		(progn
; ; 		  (let ((old-point (copy-mark (current-point)))
; ; 			(point (current-point)))
; ; 		    (move-mark point mark)
; ; ;		    (hlet ((ed::fill-paragraph-confirm nil))
; ; 		    (ed::fill-paragraph-command nil)
; ; 		    (move-mark point old-point))))
; 	    ))
	 (line-node
	  (do* ((ele (node-content part) (node-next ele))
		(insert nil))
	       ((or insert (eq ele nil))
		(when insert
		  (insert-node mark part)
		  (insert-character mark #\newline)
		  (setq inserted t)))
	    (typecase ele
	      (line-part-node
	       (let ((part-content (node-next (node-content ele))))
		 (typecase part-content
		   ((or index-node label-node tag-node comment-node
			tabclear-node newpage-node))
		   (t
		    (setq insert ele)))))
	      (region-node
	       (or (every (lambda (ch) (or (eq ch #\ ) (eq ch #\tab)))
			  (region-to-string (node-content ele)))
		   (setq insert ele)))
	      (t
	       (setq insert ele)))))
	 (t
	  (setq inserted t)
	  (insert-node mark part)
	  (insert-character mark #\newline)))))

;     (line-node
;      (let ((content (node-content node)))
;        (typecase content
; 	 (line-part-node
; 	  (let ((part (node-next (node-content content))))
; 	    (typecase part
; 	      (index-node
; 	       (ed::message "index")
; 	       ;; Move over index and newline when index alone on line.
; 	       (when (node-next content)
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
     (let* ((make (node-content node))
	    (heading (node-next make))
	    (chapter (node-next heading)))
       (insert-node mark make)
       (insert-node mark heading)
       (etypecase chapter
	 (epsilon-node)
	 (chapter-node
	  (setv ed::info-next chapter)
	  (do ((chapter (node-next heading) (node-next chapter)))
	      ((eq chapter nil))
	    (let* ((next (node-next (node-content chapter)))
		   (arg (node-content (node-next (node-content (node-content next))))))
	      (setf (gethash (region-to-string arg) (value ed::info-references))
		    chapter)
	      (insert-string mark "* ")
	      (insert-region mark arg)
	      (insert-string mark "::")
	      (insert-character mark #\newline)))))))

;     (text-node
;      (let ((content (node-content node)))
;        (typecase content
; 	 (char-node
; 	  (if (eq (node-content content) #\newline)
; 	      (do ((next (node-next content) (node-next next)))
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
     (setv ed::info-previous (or (node-previous node)
				 (node-parent node)))
     (setv ed::info-parent (node-parent node))
     (setv ed::info-top (node-parent node))
     (let* ((arg-node (node-next (node-content node)))
	    (arg (node-content (node-next (node-content (node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters arg))
	 (insert-character mark #\*))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (do ((part (node-next (node-next (node-content node))) ; @chap, argument
		  (node-next part))
	    (first t))
	   ((eq part nil))
	 (typecase part
	   (section-node
	    (when first
	      (setq first nil)
	      (setv ed::info-next part))
	    (let* ((arg-node (node-next (node-content part)))
		   (arg (node-content (node-next (node-content (node-content arg-node))))))
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
     (setv ed::info-previous (or (node-previous node)
				 (node-parent node)))
     (setv ed::info-parent (node-parent node))
     (setv ed::info-top (node-parent (node-parent node)))
     (let* ((arg-node (node-next (node-content node)))
	    (arg (node-content (node-next (node-content (node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters arg))
	 (insert-character mark #\=))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (do ((part (node-next (node-next (node-content node))) ; @section, argument
		  (node-next part))
	    (first t))
	   ((eq part nil)
	    (when first
	      (let ((next (node-next node)))
		(if next
		    (setv ed::info-next next)
		    (do* ((parent (node-parent node) (node-parent parent)))
			 ((eq parent nil))
		      (let ((next (node-next parent)))
			(when next
			  (setv ed::info-next next)
			  (return-from nil))))))))
	 (typecase part
	   (subsection-node
	    (when first
	      (setq first nil)
	      (setv ed::info-next part))
	    (let* ((arg-node (node-next (node-content part)))
		   (arg (node-content (node-next (node-content
						  (node-content arg-node))))))
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
     (let ((previous (node-previous node)))
       (setv ed::info-previous (typecase previous
				 (null)
				 (subsection-node
				  previous)
				 (t
				  (node-parent node)))))
     (setv ed::info-parent (node-parent node))
     (setv ed::info-top
	   (block nil
	     (node-parent (or (node-parent (or (node-parent node)
					       (return-from nil)))
			      (return-from nil)))))
     (let ((next (node-next node)))
       (if next
	   (setv ed::info-next next)
	   (do* ((parent (node-parent node) (node-parent parent)))
		((eq parent nil))
	     (let ((next (node-next parent)))
	       (when next
		 (setv ed::info-next next)
		 (return-from nil))))))
     (let* ((arg-node (node-next (node-content node)))
	    (arg (node-content (node-next (node-content (node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       (dotimes (i (count-characters arg))
	 (insert-character mark #\-))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-nodes mark (node-next arg-node))))

    (named-para-node
     (let* ((arg-node (node-next (node-content node)))
	    (arg (node-content (node-next (node-content (node-content arg-node))))))
       (insert-region mark arg)
       (insert-character mark #\newline)
       ;; The underlining may be too strong.
       (dotimes (i (count-characters arg))
	 (insert-character mark #\-))
       (insert-character mark #\newline)
       (insert-character mark #\newline)
       (insert-node mark (node-next arg-node))))

    ;; For "example" blocks.
    (block-node
     ;; FIX indent
     (do ((part (node-next (node-content node))
		(node-next part))
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
     (insert-node mark (node-next (node-content node))))

    (text-block-node
     (insert-node mark (node-next (node-content node))))

    (format-block-node
     (insert-node mark (node-next (node-content node))))

    (example-node
     (let* ((arg-node (node-next (node-content node)))
	    (arg (node-content (node-next (node-content (node-content arg-node))))))
       (insert-character mark #\newline)
       ;; FIX indent
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (program-eg-block-node
     ;; FIX indent
     (insert-character mark #\newline)
     (insert-node mark (node-next (node-content node)))
     (insert-character mark #\newline))

    (program-eg-node
     (let* ((arg-node (node-next (node-content node)))
	    (arg (node-content (node-next (node-content (node-content arg-node))))))
       (insert-character mark #\newline)
       ;; FIX indent
       (insert-region mark arg)
       (insert-character mark #\newline)
       (insert-character mark #\newline)))

    (description-node
     (insert-character mark #\newline)
     (do ((descript (node-next (node-next (node-content node)))
		    (node-next descript))
	  (first t))
	 ((eq descript nil))
       (etypecase descript
	 (char-node)
	 (epsilon-node)
	 (description-end-node)
	 (descript-node
	  (let ((arg (node-content descript)))
	    (if first
		(setq first nil)
		(progn
		  (insert-character mark #\newline)
		  (insert-character mark #\newline)))
	    (insert-string mark "  ")
	    (insert-node mark arg)
	    (insert-character mark #\newline)
	    (insert-string mark "     ")
	    (do ((para-part (node-content (node-next (node-next (node-next arg))))
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
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
; 			      (hlet ((ed::fill-lisp-comment-paragraph-confirm nil))
; 				(ed::fill-lisp-comment-paragraph-command nil))
; 			      (move-mark point old-point)))
			  ))))
		(t
		 (insert-node mark para-part)))))))))

    (enumerate-node
     (insert-character mark #\newline)
     (do ((item (node-next (node-next (node-content node)))
		(node-next item))
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
	  (do ((para-part (node-content item)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
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
     (do ((item (node-next (node-next (node-content node)))
		(node-next item))
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
	  (do ((para-part (node-content item)
			  (node-next para-part)))
	      ((eq para-part nil))
	    (typecase para-part
	      (char-node
	       ;; Pass newlines.
	       (do ((next (node-next para-part) (node-next next)))
		   ((or (eq next nil)
			(typecase next (char-node nil) (t t)))
		    (if next
			(progn
			  (insert-character mark #\newline)
			  (insert-string mark "     "))))))
	      (t
	       (insert-node mark para-part))))))))

    (defcom-arg-node
      (let* ((name (node-next (node-next (node-content node))))
	     (next (node-next name)))
	(insert-node mark name)
	(typecase next
	  (d-a-body-node
	   (let ((bind (node-next (node-content next))))
	     (insert-character mark #\ )
	     (insert-node mark bind))))))

    (defhvar-arg-node
      (let* ((name (node-next (node-next (node-content node))))
	     (next (node-next name)))
	(insert-node mark name)
	(etypecase next
	  (epsilon-node)
	  (d-a-spec-node
	   (let ((bind (node-next (node-next (node-content next)))))
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
     (let ((def (node-next (node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (node-next def) (node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defcom1-node
	    (insert-string mark " - Command: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defhvar1-node
	    (insert-string mark " - Editor Variable: ")
	    (let ((arg (node-next (node-content body-part))))
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
	    (do ((para-part (node-content body-part)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defun-arg-node
      (let* ((name (node-next (node-next (node-content node))))
	     (next (node-next name)))
	(insert-node mark name)
	;; FIX pkg, label
	(setq next (node-next next))
	(setq next (node-next next))
	(typecase next
	  (defun-arg-args-node
	   (let ((args (node-next (node-content next))))
	     (insert-character mark #\ )
	     (insert-node mark args))))
	(setq next (node-next next))
	(typecase next
	  (defun-arg-keys-node
	    (let ((args (node-next (node-content next))))
	     (insert-string mark " &key ")
	     (insert-node mark args))))))

    (defun-node
     (insert-string mark " - Function: ")
     (let ((def (node-next (node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (node-next def) (node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defun1-node
	    (insert-string mark " - Function: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
 	   (defhvar1-node
 	    (insert-string mark " - Editor Variable: ")
 	    (let ((arg (node-next (node-content body-part))))
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
	    (do ((para-part (node-content body-part)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defvar-arg-node
      (let ((name (node-next (node-next (node-content node)))))
	(insert-node mark name)))

    (defvar-node
     (insert-string mark " - Variable: ")
     (let ((def (node-next (node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (node-next def) (node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
;  	   (defhvar1-node
;  	    (insert-string mark " - Editor Variable: ")
;  	    (let ((arg (node-next (node-content body-part))))
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
	    (do ((para-part (node-content body-part)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
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
     (let ((def (node-next (node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (node-next def) (node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defcon1-node
	    (insert-string mark " - Constant: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defvar1-node
	    (insert-string mark " - Variable: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
;  	   (defhvar1-node
;  	    (insert-string mark " - Editor Variable: ")
;  	    (let ((arg (node-next (node-content body-part))))
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
	    (do ((para-part (node-content body-part)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
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
     (let ((def (node-next (node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (node-next def) (node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (defmac1-node
	    (insert-string mark " - Macro: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
; 	   (defhvar1-node
; 	    (insert-string mark " - Variable: ")
; 	    (let ((arg (node-next (node-content body-part))))
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
	    (do ((para-part (node-content body-part)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (defhvar-node
     (insert-string mark " - Editor Variable: ")
     ;; FIX same as in defcom-node
     (let ((def (node-next (node-content node))))
       (insert-node mark def)
       (insert-character mark #\newline)
       (do ((body-part (node-next def) (node-next body-part))
	    (first t))
	   ((eq body-part nil))
	 (typecase body-part
	   (epsilon-node)
	   (defcom1-node
	    (insert-string mark " - Command: ")
	    (let ((arg (node-next (node-content body-part))))
	      (insert-node mark arg)
	      (insert-character mark #\newline)))
	   (defhvar1-node
	    (insert-string mark " - Editor Variable: ")
	    (let ((arg (node-next (node-content body-part))))
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
	    (do ((para-part (node-content body-part)
			    (node-next para-part)))
		((eq para-part nil))
	      (typecase para-part
		(char-node
		 ;; Pass newlines.
		 (do ((next (node-next para-part) (node-next next)))
		     ((or (eq next nil)
			  (typecase next (char-node nil) (t t)))
		      (if next
			  (progn
			    (insert-character mark #\newline)
			    (insert-string mark "     "))))))
		(t
		 (insert-node mark para-part)))))))))

    (argument-node
     (insert-node mark (node-next (node-content (node-content node)))))

    (bracedarg-node
     (do* ((part (node-next (node-content (node-content node))) next)
	   (next (node-next part) (node-next next)))
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
     (insert-node mark (node-next (node-content node)))
     (insert-character mark #\'))
    (var-node
     (insert-character mark #\`)
     (insert-node mark (node-next (node-content node)))
     (insert-character mark #\'))
    (kwd-node
     (insert-character mark #\:)
     (insert-node mark (node-next (node-content node))))
    (binding-node
     (insert-nodes mark (node-next (node-content node))))
    (i-node
     (insert-node mark (node-next (node-content node))))
    (f-node ;; FIX Font?
     (insert-nodes mark (node-next (node-content node))))
    (w-node ;; FIX Wide?
     (insert-nodes mark (node-next (node-content node))))
    (bf-node
     (insert-nodes mark (node-next (node-content node))))
    (b-node
     (insert-nodes mark (node-next (node-content node))))
    (center-node
     ;; FIX center-line-command (maybe *center*)
     (insert-nodes mark (node-next (node-content node))))
    (val-node
     (insert-string mark "value:")
     (insert-nodes mark (node-next (node-content node))))
    ;; What is comma ("@;")?
    (comma-node)
    (multiple-node
     (insert-nodes mark (node-next (node-content node))))
    (optional-node
     (insert-string mark "&optional"))
    (rest-node
     (insert-string mark "&rest"))
    (key-node
     (insert-string mark "&key"))
    (mstar-node
     (insert-character mark #\{)
     (insert-node mark (node-next (node-content node)))
     (insert-character mark #\})
     (insert-character mark #\*))
    (mor-node
     (insert-character mark #\|))
    (mgroup-node
     (insert-character mark #\()
     (insert-node mark (node-next (node-content node)))
     (insert-character mark #\)))
    (mopt-node
     (insert-character mark #\[)
     (insert-node mark (node-next (node-content node)))
     (insert-character mark #\]))

    (char-node
     (let ((content (node-content node)))
       (or (eq content #\newline)
	   (insert-character mark content))))
    (node
     (let ((content (node-content node)))
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
