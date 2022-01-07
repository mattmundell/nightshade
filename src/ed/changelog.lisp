;;; Change log mode.

(in-package "ED")

#[ Changelogs ]#

(defun setup-changelog-mode (buffer)
  (highlight-visible-changelog-buffer buffer)
  (pushnew '("ChangeLog" t highlight-visible-changelog-buffer)
	   *mode-highlighters*))

(defmode "ChangeLog" :major-p t
  :setup-function 'setup-changelog-mode)

(declaim (special *context*))

(defun highlight-changelog-line (line chi-info)
  (let ((next (next-character (mark line 0))))
    (when next
      (or (char= next #\tab)
	  ;; A header line.
	  (progn
	    (chi-mark line 0
		      (if (char= next #\space)
			  *error-font* *variable-font*)
		      (if (char= next #\space) :error :variable)
		      chi-info)
	    (return-from highlight-changelog-line)))
      (let ((chars (line-string line)))
	(if (eq (length chars) 2)
	    ;; Just a tab on a line.
	    (return-from highlight-changelog-line))

	(case *context*
	  (:files
	   ;; More files following those on the previous line.
	   (chi-mark line 0 *string-font* :string chi-info)
	   (let ((end (position #\: chars)))
	     (if end
		 (progn
		   (chi-mark line (1+ end) *original-font*
			     :window-foreground
			     chi-info)
		   (setq *context* ()))))))

	(case (aref chars 1)
	  (#\*
	   ;; The start of an entry.
	   (chi-mark line 0 *string-font*
		     :string
		     chi-info)
	   (let ((open (position #\( chars))
		 (close (position #\) chars))
		 (end (position #\: chars)))
	     (fi end
		 ;; File list wraps.
		 (setq *context* :files)
		 (if (and open close
			  (> end close open))
		     (progn
		       (chi-mark line open *special-form-font*
				 :special-form
				 chi-info)
		       (chi-mark line (1+ end) *original-font*
				 :window-foreground
				 chi-info))
		     (chi-mark line (1+ end) *original-font*
			       :window-foreground
			       chi-info)))
	     (return-from highlight-changelog-line)))
	  (#\(
	   ;; Further identifiers in an entry.
	   (let ((end (or (position #\: chars)
			  (position #\) chars))))
	     (when end
	       (chi-mark line 0 *special-form-font*
			 :special-form
			 chi-info)
	       (chi-mark line (1+ end) *original-font*
			 :window-foreground
			 chi-info)))
	   (return-from highlight-changelog-line)))))))

(defun highlight-changelog-buffer (buffer)
  (highlight-chi-buffer buffer highlight-changelog-line))

(defun highlight-visible-changelog-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-changelog-line))
