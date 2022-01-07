;;; Source comparison.

(in-package "ED")

;; FIX add stream comparison


#[ Source Comparison

These commands can be used to compare the text in two buffers, and to
generate a new version that combines features of both versions.

{evariable:Source Compare Destination}
{command:Compare Buffers}
{command:Buffer Changes}
{command:Merge Buffers}

There are various variables that control exactly how the comparison is
done.

{evariable:Source Compare Ignore Case}
{evariable:Source Compare Ignore Indentation}
{evariable:Source Compare Ignore Extra Newlines}
{evariable:Source Compare Number of Lines}
]#

(defevar "Source Compare Ignore Extra Newlines"
  "If true, `Compare Buffers' and `Merge Buffers' treat all groups of
   newlines as if they were a single newline."
  :value t)

(defevar "Source Compare Ignore Case"
  "If true, `Compare Buffers' and `Merge Buffers' treat all letters as if
   they are of the same case.")

(defevar "Source Compare Ignore Indentation"
  "If true `Compare Buffers' and `Merge Buffers' ignore initial whitespace
   when comparing lines.")

(defevar "Source Compare Number of Lines"
  "The number of lines Source Compare and Source Merge compare when
   resyncronizing after a difference has been encountered."
  :value 3)

(defevar "Source Compare Destination"
  "The buffer name offered when comparison commands prompt for a results
   buffer.  Set to the new destination after each prompt."
  :value "Comparison")

(defevar "Source Compare Style"
  "Formating style for text comparison output. () for a verbose style,
   :terse for a terser one."
  :value ())

(defcommand "Buffer Changes" ()
  "Compare the contents of the current buffer with the disk version of the
   associated file, displaying the result in the current window.  Read the
   file into the buffer `Buffer Changes File', and generate the comparison
   in the buffer `Buffer Changes Result'."
  (let ((buffer (current-buffer)))
    (or (buffer-pathname buffer)
	(editor-error "No pathname associated with buffer."))
    (let ((other-buffer (or (getstring "Buffer Changes File" *buffer-names*)
			    (make-buffer "Buffer Changes File")))
	  (result-buffer (or (getstring "Buffer Changes Result" *buffer-names*)
			     (make-buffer "Buffer Changes Result"))))
      (visit-file-command nil (buffer-pathname buffer) other-buffer)
      (delete-region (buffer-region result-buffer))
      (compare-buffers-command nil buffer other-buffer result-buffer)
      (delete-buffer other-buffer))))

(defcommand "Compare Buffers" (p buffer-a buffer-b dest-buffer
				 pathname-a pathname-b)
  "Perform a source comparison on two prompted buffers, directing the
   output to a prompted buffer.  With a prefix only compare the regions in
   the buffer.  Select the output buffer during the comparison so that
   progress is visible."
  (compare-buffers p buffer-a buffer-b dest-buffer
		   pathname-a pathname-b))

;;; `compare-buffers' creates two temporary buffers when there is a prefix.
;;; These get deleted afterwards.  $buffer-a and $buffer-b are used for
;;; names in banners in either case.
;;;
(defun compare-buffers (p buffer-a buffer-b dest-buffer
			  &optional pathname-a pathname-b)
  "Perform a source comparison on two prompted buffers, directing the
   output to a prompted buffer.  With a prefix only compare the regions in
   the buffer.  Select the output buffer during the comparison so that
   progress is visible."
  (srccom-choose-comparison-functions)
  (let ((equal t))
    (multiple-value-bind (buffer-a buffer-b dest-point
				   delete-buffer-a delete-buffer-b)
			 (get-srccom-buffers "Compare buffer: "
					     buffer-a buffer-b
					     dest-buffer p)
      (with-output-to-mark (log dest-point)
	(case (value source-compare-style)
	  (:terse (format log "+++ ~A~%--- ~A.~%"
			  (or pathname-a (buffer-name buffer-a))
			  (or pathname-b (buffer-name buffer-b))))
	  (t (format log "Comparison of ~A and ~A.~%~%"
		     (or pathname-a (buffer-name buffer-a))
		     (or pathname-b (buffer-name buffer-b)))))
	(with-mark ((mark-a (buffer-start-mark (or delete-buffer-a buffer-a)))
		    (mark-b (buffer-start-mark (or delete-buffer-b buffer-b))))
	  (let ((heading-a
		 (case (value source-compare-style)
		   (:terse (format nil
				   "+++++++++++++++~%"))
		   (t (if pathname-a
			  (format nil "**** File ~A:~%" pathname-a)
			  (format nil "**** Buffer ~A:~%" (buffer-name buffer-a))))))
		(heading-b
		 (case (value source-compare-style)
		   (:terse (format nil
				   "---------------~%"))
		   (t (if pathname-b
			  (format nil "**** File ~A:~%" pathname-b)
			  (format nil "**** Buffer ~A:~%" (buffer-name buffer-b))))))
		(terse (value source-compare-style)))
	    (loop
	      (multiple-value-bind (diff-a diff-b)
				   (srccom-find-difference mark-a mark-b)
		(or diff-a (return nil))
		(setq equal ())
		(write-string heading-a log)
		(insert-region dest-point diff-a)
		(write-string heading-b log)
		(insert-region dest-point diff-b)
		(or terse (format log "***************~%~%"))
		(move-mark mark-a (region-end diff-a))
		(move-mark mark-b (region-end diff-b))
		(or (line-offset mark-a 1) (return))
		(or (line-offset mark-b 1) (return))))
	    (or terse (format log "Done.~%")))))
      (if delete-buffer-a (delete-buffer delete-buffer-a))
      (if delete-buffer-b (delete-buffer delete-buffer-b)))
    equal))

(defun compare-sub-entities (one two file check-for-links log
				 buffer-one buffer-two log-buffer equal)
  (let ((two-file (merge-pathnames file two)))
    (if (directoryp file :check-for-links check-for-links)
	(fi (directoryp two-file :check-for-links check-for-links)
	    (progn
	      (format log "~A is a directory and ~A is a file.~%~%"
		      file two-file)
	      (setq equal ())))
	(if (file-kind two-file :check-for-links check-for-links)
	    (if (directoryp two-file :check-for-links check-for-links)
		(progn
		  (format log "~A is a file and ~A is a directory.~%~%"
			  file two-file)
		  (setq equal ()))
		(block file-compare
		  (when check-for-links
		    (if (symlinkp two-file)
			(if (symlinkp file)
			    (if (equal (symlink-dest file)
				       (symlink-dest two-file))
				(return-from file-compare)
				(progn
				  (format log "~A points to ~A, while ~A points to ~A.~%~%"
					  file two-file
					  (symlink-dest file)
					  (symlink-dest two-file))
				  (setq equal ())
				  (return-from file-compare)))
			    (progn
			      (format log "~A is a file and ~A is a symlink.~%~%"
				      file two-file)
			      (setq equal ())
			      (return-from file-compare)))
			(if (symlinkp file)
			    (progn
			      (format log "~A is a symlink and ~A is a file.~%~%"
				      file two-file)
			      (setq equal ())
			      (return-from file-compare)))))
		  ;; FIX skip offer to revert to CKP
		  (read-buffer-file file buffer-one)
		  (read-buffer-file two-file buffer-two)
		  (or (let ((current-dir (current-directory)))
			(unwind-protect
			    (compare-buffers nil
					     buffer-one
					     buffer-two
					     log-buffer
					     file
					     two-file)
			  ;; Ensure that the current directory stays the same.
			  (setf (current-directory) current-dir)))
		      (setq equal ()))))
	    (progn
	      (format log "~A is only present in ~A.~%"
		      (file-namestring file) one)
	      (setq equal ()))))
    equal))

(defcommand "Compare Files" (p file1 file2 output-buffer
			       &key silent check-for-links)
  "Compare two prompted files."
  (declare (ignore p))
  (let* ((one (namestring
	       (or file1
		   (prompt-for-file
		    :prompt "First file: "
		    :help "Name of first of files to compare."
		    :default (buffer-pathname (current-buffer))
		    :must-exist t))))
	 (two (namestring
	       (or file2
		   (prompt-for-file
		    :prompt "Second file: "
		    :help "Name of second of files to compare."
		    :default (buffer-pathname (current-buffer))
		    :must-exist t))))
	 (buffer-one (make-unique-buffer (namestring one)))
	 (buffer-two (make-unique-buffer (namestring two))))
    (unwind-protect
	(if (directoryp one :check-for-links check-for-links)
	    (let ((equal t)
		  (one-entities 0)
		  (two-entities 0)
		  (log-buffer (or output-buffer
				  (prompt-for-buffer
				   :prompt "Putting results in buffer: "
				   :must-exist nil
				   :default-string
				   (value source-compare-destination)))))
	      (declare (ignore two-checked))
	      (setq log-buffer (or output-buffer (make-buffer log-buffer)))
	      (setf (value source-compare-destination)
		    (buffer-name log-buffer))
	      (change-to-buffer log-buffer)
	      (with-output-to-mark (log (buffer-point log-buffer))
		(elet ((source-compare-style :terse))
		  (fi (directoryp two :check-for-links check-for-links)
		      (progn
			(format log "~A is a directory and ~A is a file.~%~%"
				one two)
			(setq equal ()))
		      ;; FIX should this use streams? so can be split into lib fun
		      (let ((two (ensure-trailing-slash
				  (merge-pathnames two (current-directory)))))
			;; Count the number of entities in two.
			(in-directory two
			  (do-files (file ""
					  :recurse t
					  :follow-links (fi check-for-links))
			    (incf two-entities)))
			;; Recurse into one, comparing with two and
			;; counting entities.
			(in-directory (ensure-trailing-slash one)
			  (do-files (file ""
					  :recurse t
					  :follow-links (fi check-for-links))
			    (incf one-entities)
 			    (setq equal
 				  (compare-sub-entities one two file
							check-for-links
 							log buffer-one buffer-two
 							log-buffer equal)))
			  (or (= one-entities two-entities)
			      (progn
				(format log "~A contains more entities than ~A.~%"
					two one)
				(setq equal ())))))))
		(or silent (message "Directory comparison done."))
		equal))
	    (progn
	      ;; FIX update as above
	      (if (directoryp two :check-for-links check-for-links)
		  ;; FIX log instead
		  (editor-error "~A is a file and ~A is a directory."
				one two))
	      (read-buffer-file one buffer-one)
	      (read-buffer-file two buffer-two)
	      (prog1 (compare-buffers nil buffer-one buffer-two
				      output-buffer)
		(or silent (message "File comparison done.")))))
      (delete-buffer buffer-one)
      (delete-buffer buffer-two))))

(defun merge-files (pathname-a pathname-b)
  "Merge PATHNAME-B into PATHNAME-A."
  (let ((dir (current-directory)))
    (unwind-protect
	(with-temp-buffer (buffer)
	  (with-temp-buffer (buffer-a pathname-a)
	    (with-temp-buffer (buffer-b pathname-b)
	      (merge-buffers-command () buffer-a buffer-b buffer))
	    (delete-region (buffer-region buffer-a))
	    (insert-region (buffer-point buffer-a) (buffer-region buffer))
	    (write-buffer-file buffer-a pathname-a)))
      ;; Ensure the current directory stays the same, as the merge commands
      ;; change the current buffer.
      (setf (current-directory) dir))))

;; FIX flag/version that only compares as far as needed  mayb stream-based lib version
;; FIX     name  consist w eq equal eql = mark=  file-equal(p) file= entity= ent=
(defun compare-files (pathname-a pathname-b &key check-for-links)
  "Return true if $pathname-b is equivalent to $pathname-a."
  (with-temp-buffer (buffer)
    (or (probe-file pathname-a) (error "File A must exist."))
    (or (probe-file pathname-b) (error "File B must exist."))
    (and (equal (file-size pathname-a) (file-size pathname-b))
	 (compare-files-command () pathname-a pathname-b buffer
				:silent t :check-for-links check-for-links))))

;;; "Merge Buffers" creates two temporary buffers when there is a prefix.
;;; These get deleted when we're done.  Buffer-a and Buffer-b are used for
;;; names in banners in either case.
;;;
(defcommand "Merge Buffers" (p buffer-a buffer-b dest-buffer)
  "Combine two prompted buffers into a prompted output buffer.  Copy text
   that is identical in the two comparison buffers to the output buffer.
   On encountering conflicting text, display the conflicting sections in
   the output buffer and prompt for a key-event indicating what action to
   take.  Accept the following responses:

     1
	 Use the first version of the text.

     2
	 Use the second version.

     b
	Insert the string \"<<<<<<<\" followed by both versions.  This is
	indented as a placemarker in order to resolve the conflict later.

     C-r
	Do a recursive edit and ask again when the edit is exited."
  (srccom-choose-comparison-functions)
  (multiple-value-bind (buffer-a buffer-b dest-point
		        delete-buffer-a delete-buffer-b)
		       (get-srccom-buffers "Merge buffer: " buffer-a buffer-b
					   dest-buffer p)
    (with-output-to-mark (stream dest-point)
      (let ((region-a (buffer-region (or delete-buffer-a buffer-a))))
	(with-mark ((temp-a (region-start region-a) :right-inserting)
		    (temp-b dest-point :right-inserting)
		    (mark-a (region-start region-a))
		    (mark-b (region-start
			     (buffer-region (or delete-buffer-b buffer-b)))))
	  (clear-echo-area)
	  (loop
	    (multiple-value-bind (diff-a diff-b)
				 (srccom-find-difference mark-a mark-b)
	      (when (null diff-a)
		(insert-region dest-point (region temp-a (region-end region-a)))
		(return nil))
	      ;; Copy the part that's the same.
	      (insert-region dest-point (region temp-a (region-start diff-a)))
	      ;; Put both versions in the buffer, and prompt for which one to use.
	      (move-mark temp-a dest-point)
	      (format stream "~%**** Buffer ~A (1):~%" (buffer-name buffer-a))
	      (insert-region dest-point diff-a)
	      (move-mark temp-b dest-point)
	      (format stream "~%**** Buffer ~A (2):~%" (buffer-name buffer-b))
	      (insert-region dest-point diff-b)
	      (command-case
		  (:prompt "Resolve conflict: "
		   :help "Type one of these characters to say how to merge:")
		(#\1 "Use the text from buffer 1."
		     (delete-region (region temp-b dest-point))
		     (delete-characters temp-a)
		     (delete-region
		      (region temp-a
			      (line-start temp-b
					  (line-next (mark-line temp-a))))))
		(#\2 "Use the text from buffer 2."
		     (delete-region (region temp-a temp-b))
		     (delete-characters temp-b)
		     (delete-region
		      (region temp-b
			      (line-start temp-a
					  (line-next (mark-line temp-b))))))
		; FIX use cvs style <<<<<<<<<<< marking
		(#\b "Insert both versions with special marking around them."
		     ;; FIX
		     ;(format temp-a "~%<<<<<<< ~A" (buffer-name buffer-a))
		     (insert-string temp-a "
<<<<<<<")
		     (insert-string temp-b "
=======")
		     (insert-string dest-point "
>>>>>>>")) ; FIX terpri?
		(#\a "Align window at start of difference display."
		     (line-start
		      (move-mark
		       (window-display-start
			(car (buffer-windows (line-buffer (mark-line temp-a)))))
		       temp-a))
		     (reprompt))
		(:recursive-edit "Enter a recursive edit."
				 (with-mark ((save dest-point))
				   (do-recursive-edit)
				   (move-mark dest-point save))
				 (reprompt)))
	      (redisplay)
	      (move-mark mark-a (region-end diff-a))
	      (move-mark mark-b (region-end diff-b))
	      (move-mark temp-a mark-a)
	      (or (line-offset mark-a 1) (return))
	      (or (line-offset mark-b 1) (return))))))
      (message "Done."))
    (when delete-buffer-a
      (delete-buffer delete-buffer-a)
      (delete-buffer delete-buffer-b))))

(defun get-srccom-buffers (first-prompt buffer-a buffer-b dest-buffer p)
  (or buffer-a
      (setf buffer-a (prompt-for-buffer :prompt first-prompt
					:must-exist t
					:default (current-buffer))))
  (or buffer-b
      (setf buffer-b (prompt-for-buffer :prompt "With buffer: "
					:must-exist t
					:default (other-buffer))))
  (or dest-buffer
      (setf dest-buffer
	    (prompt-for-buffer :prompt "Putting results in buffer: "
			       :must-exist nil
			       :default-string
			       (value source-compare-destination))))
  (if (stringp dest-buffer)
      (setf dest-buffer (make-buffer dest-buffer))
      (buffer-end (buffer-point dest-buffer)))
  (setf (value source-compare-destination) (buffer-name dest-buffer))
  (change-to-buffer dest-buffer)
  (let* ((alt-buffer-a (if p (make-buffer (prin1-to-string (gensym)))))
	 (alt-buffer-b (if alt-buffer-a
			   (make-buffer (prin1-to-string (gensym))))))
    (when alt-buffer-a
      (ninsert-region (buffer-point alt-buffer-a)
		      (copy-region (if (mark< (buffer-point buffer-a)
					      (buffer-mark buffer-a))
				       (region (buffer-point buffer-a)
					       (buffer-mark buffer-a))
				       (region (buffer-mark buffer-a)
					       (buffer-point buffer-a)))))
      (ninsert-region (buffer-point alt-buffer-b)
		      (copy-region (if (mark< (buffer-point buffer-b)
					      (buffer-mark buffer-b))
				       (region (buffer-point buffer-b)
					       (buffer-mark buffer-b))
				       (region (buffer-mark buffer-b)
					       (buffer-point buffer-b))))))
    (values buffer-a buffer-b (current-point) alt-buffer-a alt-buffer-b)))
#|
(defun get-srccom-buffers (first-prompt buffer-a buffer-b dest-buffer p)
  (unless buffer-a
    (setf buffer-a (prompt-for-buffer :prompt first-prompt
				      :must-exist t
				      :default (current-buffer))))
  (unless buffer-b
    (setf buffer-b (prompt-for-buffer :prompt "With buffer: "
				      :must-exist t
				      :default (previous-buffer))))
  (unless dest-buffer
    (let* ((name (value source-compare-destination))
	   (temp-default (getstring name *buffer-names*))
	   (default (or temp-default (make-buffer name))))
      (setf dest-buffer (prompt-for-buffer :prompt "Putting results in buffer: "
					   :must-exist nil
					   :default default))
      ;; Delete the default buffer if it did already exist and was not chosen.
      (unless (or (eq dest-buffer default) temp-default)
	(delete-buffer default))))
  (if (stringp dest-buffer)
      (setf dest-buffer (make-buffer dest-buffer))
      (buffer-end (buffer-point dest-buffer)))
  (setf (value source-compare-destination) (buffer-name dest-buffer))
  (change-to-buffer dest-buffer)
  (let* ((alt-buffer-a (if p (make-buffer (prin1-to-string (gensym)))))
	 (alt-buffer-b (if alt-buffer-a
			   (make-buffer (prin1-to-string (gensym))))))
    (when alt-buffer-a
      (ninsert-region (buffer-point alt-buffer-a)
		      (copy-region (if (mark< (buffer-point buffer-a)
					      (buffer-mark buffer-a))
				       (region (buffer-point buffer-a)
					       (buffer-mark buffer-a))
				       (region (buffer-mark buffer-a)
					       (buffer-point buffer-a)))))
      (ninsert-region (buffer-point alt-buffer-b)
		      (copy-region (if (mark< (buffer-point buffer-b)
					      (buffer-mark buffer-b))
				       (region (buffer-point buffer-b)
					       (buffer-mark buffer-b))
				       (region (buffer-mark buffer-b)
					       (buffer-point buffer-b))))))
    (values buffer-a buffer-b (current-point) alt-buffer-a alt-buffer-b)))
|#


;;;; Functions that find the differences between two buffers.

(defun srccom-find-difference (mark-a mark-b)
  "Returns as multiple values two regions of text that are different in the
  lines following Mark-A and Mark-B.  If no difference is encountered, Nil
  is returned."
  (multiple-value-bind (diff-a diff-b)
		       (srccom-different-lines mark-a mark-b)
    (when diff-a
      (multiple-value-bind (same-a same-b)
			   (srccom-similar-lines diff-a diff-b)
	(values (region diff-a same-a)
		(region diff-b same-b))))))

;;; These are set by SRCCOM-CHOOSE-COMPARISON-FUNCTIONS depending on something.
;;;
(defvar *srccom-line=* nil)
(defvar *srccom-line-next* nil)

(defun srccom-different-lines (mark-a mark-b)
  "Returns as multiple values two marks pointing to the first different lines
  found after Mark-A and Mark-B.  Nil is returned if no different lines are
  found."
  (do ((line-a (mark-line mark-a) (funcall *srccom-line-next* line-a))
       (mark-a (copy-mark mark-a))
       (line-b (mark-line mark-b) (funcall *srccom-line-next* line-b))
       (mark-b (copy-mark mark-b)))
      (())
    (cond ((null line-a)
	   (return (if line-b
		       (values mark-a mark-b))))
	  ((null line-b)
	   (return (values mark-a mark-b))))
    (line-start mark-a line-a)
    (line-start mark-b line-b)
    (unless (funcall *srccom-line=* line-a line-b)
      (return (values mark-a mark-b)))))

(defun srccom-similar-lines (mark-a mark-b)
  "Returns as multiple values two marks pointing to the first similar lines
  found after Mark-A and Mark-B."
  (do ((line-a (mark-line mark-a) (funcall *srccom-line-next* line-a))
       (cmark-a (copy-mark mark-a))
       (line-b (mark-line mark-b) (funcall *srccom-line-next* line-b))
       (cmark-b (copy-mark mark-b))
       (temp)
       (window-size (value source-compare-number-of-lines)))
      (())
    ;; If we hit the end of one buffer, then the difference extends to the end
    ;; of both buffers.
    (if (or (null line-a) (null line-b))
	(return
	 (values
	  (buffer-end-mark (line-buffer (mark-line mark-a)))
	  (buffer-end-mark (line-buffer (mark-line mark-b))))))
    (line-start cmark-a line-a)
    (line-start cmark-b line-b)
    ;; Three cases:
    ;;  1] Difference will be same length in A and B.  If so, Line-A = Line-B.
    ;;  2] Difference will be longer in A.  If so, Line-A = something in B.
    ;;  3] Difference will be longer in B.  If so, Line-B = something in A.
    (cond ((and (funcall *srccom-line=* line-a line-b)
		(srccom-check-window line-a line-b window-size))
	   (return (values cmark-a cmark-b)))
	  ((and (setq temp (srccom-line-in line-a mark-b cmark-b))
		(srccom-check-window line-a temp window-size))
	   (return (values cmark-a (line-start cmark-b temp))))
	  ((and (setq temp (srccom-line-in line-b mark-a cmark-a))
		(srccom-check-window temp line-b window-size))
	   (return (values (line-start cmark-a temp) cmark-b))))))

(defun srccom-line-in (line start end)
  "Checks to see if there is a Line Srccom-Line= to the given Line in the
  region delimited by the Start and End marks.  Returns that line if so, or
  Nil if there is none."
  (do ((current (mark-line start) (funcall *srccom-line-next* current))
       (terminus (funcall *srccom-line-next* (mark-line end))))
      ((eq current terminus) nil)
    (if (funcall *srccom-line=* line current)
	(return current))))

(defun srccom-check-window (line-a line-b count)
  "Verifies that the Count lines following Line-A and Line-B are Srccom-Line=.
  If so, returns T.  Otherwise returns Nil."
  (do ((line-a line-a (funcall *srccom-line-next* line-a))
       (line-b line-b (funcall *srccom-line-next* line-b))
       (index 0 (1+ index)))
      ((= index count) t)
    (if (not (funcall *srccom-line=* line-a line-b))
	(return nil))))



;;;; Functions that control the comparison of text.

;;; SRCCOM-CHOOSE-COMPARISON-FUNCTIONS -- Internal.
;;;
;;; This initializes utility functions for comparison commands based on
;;; editor variables.
;;;
(defun srccom-choose-comparison-functions ()
  (setf *srccom-line=*
	(if (value source-compare-ignore-case)
	    (if (value source-compare-ignore-indentation)
		#'srccom-ignore-case-and-indentation-line=
		#'srccom-case-insensitive-line=)
	    (if (value source-compare-ignore-indentation)
		#'srccom-ignore-indentation-case-sensitive-line=
		#'srccom-case-sensitive-line=)))
  (setf *srccom-line-next*
	(if (value source-compare-ignore-extra-newlines)
	    #'srccom-line-next-ignoring-extra-newlines
	    #'line-next)))
#|
(defun srccom-choose-comparison-functions ()
  "This function should be called by a ``top level'' source compare utility
  to initialize the lower-level functions that compare text."
  (setf *srccom-line=*
	(if (value source-compare-ignore-case)
	    #'srccom-case-insensitive-line=
	    #'srccom-case-sensitive-line=))
  (setf *srccom-line-next*
	(if (value source-compare-ignore-extra-newlines)
	    #'srccom-line-next-ignoring-extra-newlines
	    #'line-next)))
|#

;;; SRCCOM-LINE-NEXT-IGNORING-EXTRA-NEWLINES -- Internal.
;;;
;;; This is the value of *srccom-line-next* when "Source Compare Ignore Extra
;;; Newlines" is true.
;;;
(defun srccom-line-next-ignoring-extra-newlines (line)
  (if (null line) nil
      (do ((line (line-next line) (line-next line)))
	  ((or (null line) (not (blank-line-p line))) line))))

;;; SRCCOM-IGNORE-CASE-AND-INDENTATION-LINE=	   -- Internal.
;;; SRCCOM-CASE-INSENSITIVE-LINE=		   -- Internal.
;;; SRCCOM-IGNORE-INDENTATION-CASE-SENSITIVE-LINE= -- Internal.
;;; SRCCOM-CASE-SENSITIVE-LINE=			   -- Internal.
;;;
;;; These are the value of *srccom-line-=* depending on the orthogonal values
;;; of "Source Compare Ignore Case" and "Source Compare Ignore Indentation".
;;;
(macrolet ((def-line= (name test &optional ignore-indentation)
		      `(defun ,name (line-a line-b)
			 (or (eq line-a line-b)		; if they're both NIL
			     (and line-a
				  line-b
				  (let* ((chars-a (line-string line-a))
					 (len-a (length chars-a))
					 (chars-b (line-string line-b))
					 (len-b (length chars-b)))
				    (declare (simple-string chars-a chars-b))
				    (cond
				     ((and (= len-a len-b)
					   (,test chars-a chars-b)))
				     ,@(if ignore-indentation
					   `((t
					      (flet ((frob (chars len)
						       (dotimes (i len nil)
							 (let ((char (schar chars i)))
							   (unless
							       (or (char= char #\space)
								   (char= char #\tab))
							     (return i))))))
						(let ((i (frob chars-a len-a))
						      (j (frob chars-b len-b)))
						  (if (and i j)
						      (,test chars-a chars-b
							     :start1 i :end1 len-a
							     :start2 j :end2 len-b)
						      )))))))))))))

  (def-line= srccom-ignore-case-and-indentation-line= string-equal t)

  (def-line= srccom-case-insensitive-line= string-equal)

  (def-line= srccom-ignore-indentation-case-sensitive-line= string= t)

  (def-line= srccom-case-sensitive-line= string=))

#|
;;; SRCCOM-CASE-INSENSITIVE-LINE= -- Internal.
;;;
;;; Returns t if line-a and line-b contain STRING-EQUAL text.
;;;
(defun srccom-case-insensitive-line= (line-a line-b)
  (or (eq line-a line-b)		; if they're both NIL
      (and line-a
	   line-b
	   (let ((chars-a (line-string line-a))
		 (chars-b (line-string line-b)))
	     (declare (simple-string chars-a chars-b))
	     (and (= (length chars-a) (length chars-b))
		  (string-equal chars-a chars-b))))))

;;; SRCCOM-CASE-SENSITIVE-LINE= -- Internal.
;;;
;;; Returns t if line-a and line-b contain STRING= text.
;;;
(defun srccom-case-sensitive-line= (line-a line-b)
  (or (eq line-a line-b)		; if they're both NIL
      (and line-a
	   line-b
	   (let ((chars-a (line-string line-a))
		 (chars-b (line-string line-b)))
	     (declare (simple-string chars-a chars-b))
	     (and (= (length chars-a) (length chars-b))
		  (string= chars-a chars-b))))))
|#
