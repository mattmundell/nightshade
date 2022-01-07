;;; Commands and functions for sorting buffer text.

(in-package "ED")

(defun sort-lines (region &optional (predicate 'string<))
  "Sort the lines in Region according to Predicate."
  (let* ((lines '())
	 (region-start (region-start region))
	 (end (region-end region))
	 (end-line (if (zerop (mark-charpos end))
		       (mark-line end)
		       (if (line-offset end 1)
			   (mark-line end)))))
    ;; List the region lines.
    (do* ((point (copy-mark region-start))
	  (point-line (mark-line point) (mark-line point)))
	 ((eq point-line end-line))
      (push (cons (line-string (mark-line point))
		  (line-plist (mark-line point)))
	    lines)
      (or (line-offset point 1)
	  (return-from nil)))
    (when lines
      (let ((new-region (copy-region region))
	    (old-region (copy-region region)))
	(delete-region (if end-line
			   (region region-start (mark end-line 0))
			   region))
	;; Sort the list and re-insert the lines.
	(let ((mark (current-point)))
	  (move-mark mark region-start)
	  (dolist (line (sort lines
			      (lambda (one two)
				(funcall predicate (car one) (car two)))))
	    (insert-string mark (car line))
	    (setf (line-plist (mark-line mark)) (cdr line))
	    (insert-character mark #\newline)))
	(make-region-undo :twiddle "sort-lines"
			  new-region
			  old-region)))))

(defcommand "Sort Region Lines" (p)
  "Sort the lines in the current region alphabetically.
   With a prefix reverse the sort."
  "Sort the lines in the current region alphabetically.
   With a prefix reverse the sort."
  (sort-lines (current-region) (if p 'string> 'string<)))

(defcommand "Sort Buffer Lines" (p)
  "Sort the lines in the current buffer alphabetically.
   With a prefix reverse the sort."
  "Sort the lines in the current buffer alphabetically.
   With a prefix reverse the sort."
  (sort-lines (buffer-region (current-buffer))
	      (if p 'string> 'string<)))
