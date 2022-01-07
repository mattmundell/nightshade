;;; Handling of variations in the end of line marker.

(in-package "ED")

;; TODO: Cater for other line ends.

(defun hide-line-ends (buffer &optional existp)
  "If $buffer looks to have extra characters to mark the end of each line
   then strip off the characters, noting them in *Ed Line End*."
  (declare (ignore existp))
  (when (iterate check ((mark (copy-mark (buffer-start-mark buffer)))
			(i 0))
	  (line-end mark)
	  (if (< i 10)
	      (when (char= (previous-character mark) #\return)
		(if (line-offset mark 1)
		    (check mark (1+ i))
		    t))
	      t))
    (if (editor-bound-p 'ed::ed-line-end :buffer buffer)
	(setf (variable-value 'ed::ed-line-end :buffer buffer)
	      #\return)
	(defevar "Ed Line End"
	  "Line end for current file.  Set in `hide-line-ends'."
	  :value #\return
	  :buffer buffer))
    (iterate strip ((mark (copy-mark (buffer-start-mark buffer))))
      (line-end mark)
      (kill-characters mark -1)
      (if (line-offset mark 1)
	  (strip mark)))
    (setf (buffer-modified buffer) ())))

(defun show-line-ends (buffer &optional existp)
  "If *Ed Line End* is set add the character in *Ed Line End* to the end of
   each line."
  (declare (ignore existp))
  (when (and (editor-bound-p 'ed::ed-line-end :buffer buffer)
	     (char= (variable-value 'ed::ed-line-end :buffer buffer)
		    #\return))
    (iterate show ((mark (copy-mark (buffer-start-mark buffer))))
      (line-end mark)
      (insert-character mark #\return)
      (if (line-offset mark 1)
	  (show mark)))))

(add-hook read-file-hook 'hide-line-ends)
(add-hook before-write-file-hook 'show-line-ends)
(add-hook write-file-hook 'hide-line-ends)
