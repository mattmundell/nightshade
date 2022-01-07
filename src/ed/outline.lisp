;;; Outline mode.  Hierarchical sectioning of a buffer where a section
;;; begins with a line beginning with a number of a special character,
;;; where the number of characters determines the depth of nesting of the
;;; section.

(in-package "ED")

(defevar "Outline Character"
  "Character which starts Outline sections."
  :value #\*)

(defmode "Outline" :major-p nil
  :setup-function 'setup-outline-buffer
  :cleanup-function 'cleanup-outline-buffer)

(declaim (special *mode-highlighters*))

(defun setup-outline-buffer (buffer)
  (highlight-outline buffer)
  (pushnew '("Outline" nil highlight-outline) *mode-highlighters*))

(defun cleanup-outline-buffer (buffer)
  (do-buffer-lines (line buffer)
   (let ((info (getf (line-plist line) 'outline-ch-info)))
     (when info
       (dolist (fmark (ch-info-font-marks info))
	 (delete-font-mark fmark))))
   (remf (line-plist line) 'outline-ch-info)))

(defun check-outline-line (line)
  "Return the outline ch-info for Line if line needs to be considered for
   highlighting."
  (let ((info (getf (line-plist line) 'outline-ch-info)))
    (if info
	(if (eq (line-signature line) (ch-info-signature info))
	    nil
	    (progn
	      (setf (ch-info-signature info) (line-signature line))
	      info))
	(let ((info (make-ch-info (line-signature line))))
	  (setf (getf (line-plist line) 'outline-ch-info) info)
	  info))))

(defun highlight-outline (buffer)
  "Highlight section headings in Outline Buffer."
  (let ((ochar (value outline-character)))
    (do-buffer-lines (line buffer)
      (when (> (line-length line) 0)
	(let ((info (check-outline-line line)))
	  (when info
	    (dolist (fmark (ch-info-font-marks info))
	      (delete-font-mark fmark))
	    (let ((mark (mark line 0)))
	      (when (eq (next-character mark) ochar)
		(push (color-mark line 0
				  (while ((i 1 (1+ i)))
					 ((and (mark-after mark)
					       (eq (next-character mark)
						   ochar))
					  (nth (mod i 6)
					       '(:comment
						 :string
						 :variable
						 :function
						 :preprocessor
						 :special-form)))))
		      (ch-info-font-marks info))))))))))
