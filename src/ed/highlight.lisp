;;; Highlighting.

(in-package "ED")


;;;; Open parens.

(defevar "Highlight Open Parens"
  "When true, causes open parens to be displayed in a different font when
   the cursor is directly to the right of the corresponding close paren.")

(defevar "Open Paren Finder Function"
  "Should be a function that takes a mark for input and returns either NIL
   if the mark is not after a close paren, or two (temporary) marks
   surrounding the corresponding open paren."
  :value 'lisp-open-paren-finder-function)

(defvar *open-paren-font-marks* nil
  "The pair of font-marks surrounding the currently highlighted open-
   paren or nil if there isn't one.")

(defvar *open-paren-highlight-font* 2
  "The index into the font-map for the open paren highlighting font.")

; FIX reverse vid must swap
(defevar "Open Paren Highlight Foreground"
  "The foreground color for the open paren highlight."
  :value :string)

(defevar "Open Paren Highlight Background"
  "The background color for the open paren highlight."
  :value :original)

;;; MAYBE-HIGHLIGHT-OPEN-PARENS is a redisplay hook that matches parens by
;;; highlighting the corresponding open-paren after a close-paren is
;;; typed.
;;;
(defun maybe-highlight-open-parens (window)
  (declare (ignore window))
  (when (value highlight-open-parens)
    (if (and (value highlight-active-region) (region-active-p))
	(kill-open-paren-font-marks)
	(when (mark-line (current-point))
	  (multiple-value-bind
	      (start end)
	      (funcall (value open-paren-finder-function)
		       (current-point))
	    (if (and start end)
		(set-open-paren-font-marks start end)
		(kill-open-paren-font-marks)))))))
;;;
(add-hook redisplay-hook 'maybe-highlight-open-parens)

(defun set-open-paren-font-marks (start end)
  (if *open-paren-font-marks*
      (let ((fmark (region-start *open-paren-font-marks*)))
	(if (mark-line fmark)
	    (or (mark= fmark start)
		(move-font-mark fmark start))
	    (let ((fmark (font-mark (mark-line start)
				    (mark-charpos start)
				    *open-paren-highlight-font*)))
	      (setf (font-mark-fore-color fmark)
		    (color (value open-paren-highlight-foreground)))
	      (setf (font-mark-back-color fmark)
		    (color (value open-paren-highlight-background)))
	      (setf (region-start *open-paren-font-marks*) fmark)))
	(setq fmark (region-end *open-paren-font-marks*))
	(if (mark-line fmark)
	    (or (mark= fmark end)
		(move-font-mark fmark end))
	    (let ((fmark (font-mark (mark-line end)
				    (mark-charpos end)
				    *open-paren-highlight-font*)))
	      (setf (font-mark-fore-color fmark)
		    (color (value open-paren-highlight-foreground)))
	      (setf (font-mark-back-color fmark)
		    (color (value open-paren-highlight-background)))
	      (setf (region-end *open-paren-font-marks*) fmark))))
      (let ((line (mark-line start)))
	(setf *open-paren-font-marks*
	      (let ((start-mark (font-mark line (mark-charpos start)
					   *open-paren-highlight-font*)))
		(setf (font-mark-fore-color start-mark)
		      (color (value open-paren-highlight-foreground)))
		(setf (font-mark-back-color start-mark)
		      (color (value open-paren-highlight-background)))
		(region
		 start-mark
		 (font-mark line (mark-charpos end) 0)))))))

(defun kill-open-paren-font-marks ()
  (when *open-paren-font-marks*
    (delete-font-mark (region-start *open-paren-font-marks*))
    (delete-font-mark (region-end *open-paren-font-marks*))
    (setf *open-paren-font-marks* nil)))


;;;; Active regions.

(defvar *active-region-font-marks* nil)
(defvar *active-region-highlight-font* 3
  "The index into the font-map for the active region highlighting font.")

(defevar "Active Region Highlight Foreground"
  "The foreground color for the active region highlight."
  :value :window-foreground)

(defevar "Active Region Highlight Background"
  "The background color for the active region highlight."
  :value :function)

(defvar *highlighted-region* ())

;;; HIGHLIGHT-ACTIVE-REGION is a redisplay hook for active regions.
;;; Since it is too hard to know how the region may have changed when it is
;;; active and already highlighted, if it does not check out to being exactly
;;; the same, we just delete all the font marks and make new ones.  When
;;; the current window is the echo area window, just pretend everything is
;;; okay; this keeps the region highlighted while we're in there.
;;;
(defun highlight-active-region (&optional window)
  (unless (eq window *echo-area-window*)
    (when (value highlight-active-region)
      (cond ((region-active-p)
	     (cond ((not *active-region-font-marks*)
		    (set-active-region-font-marks))
		   ((check-active-region-font-marks))
		   (t (kill-active-region-font-marks)
		      (set-active-region-font-marks))))
	    (*active-region-font-marks*
	     (kill-active-region-font-marks)
	     (clear-chi-signatures *highlighted-region*)
	     (setq *highlighted-region* ()))))))
;;;
;;; Moved to context highlighting hook below.
;(add-hook redisplay-hook 'highlight-active-region)
(add-hook activate-region-hook 'highlight-active-region)
(add-hook pacify-region-hook 'highlight-active-region)

;;; LAST-CHI-MARK  --  Internal
;;;
;;; Return the context highlighting font mark that is active before or at
;;; $end-mark.  If given font mark $candidate, include it in the considered
;;; marks, preferring the other marks.
;;;
(defun last-chi-mark (end-mark &optional candidate)
  (let* ((end-line (mark-line end-mark))
	 (chi-info (and end-line
			(getf (line-plist end-line) 'chi-info)))
	 (last candidate))
    (when chi-info
      (let* ((end-charpos (mark-charpos end-mark))
	     (marks (chi-info-font-marks chi-info)))
	(dolist (mark marks)
	  (if (and (<= (mark-charpos mark) end-charpos)
		   (if last
		       ;; Use `>=' to prefer the chi marks to $candidate.
		       (>= (mark-charpos mark) (mark-charpos last))
		       t))
	      (setq last mark)))))
    last))

(defun set-active-region-font-marks ()
  (flet ((stash-a-mark (m &optional (font *active-region-highlight-font*)
			  (set-colors t))
	   (let ((fmark (font-mark (mark-line m) (mark-charpos m) font)))
	     (if set-colors
		 (progn
		   (setf (font-mark-fore-color fmark)
			 (color (value
				 active-region-highlight-foreground)))
		   (setf (font-mark-back-color fmark)
			 (color (value
				 active-region-highlight-background)))))
	     (push fmark *active-region-font-marks*))))
    (let* ((region (current-region nil nil))
	   (start (region-start region))
	   (end (region-end region)))
      (setq *highlighted-region* region)
      (with-mark ((mark start))
	(or (mark= mark end)
	    (let ((last-chi-mark (last-chi-mark end))
		  last-chi-font last-chi-fore last-chi-back)
	      (if last-chi-mark
		  (setq last-chi-font (edi::font-mark-font last-chi-mark) ; FIX
			last-chi-fore (font-mark-fore-color last-chi-mark)
			last-chi-back (font-mark-back-color last-chi-mark)))
	      (note-and-free-chi-marks region)
	      (loop
		(stash-a-mark mark)
		(or (line-offset mark 1 0) (return))
		(when (mark> mark end) (return)))
	      ;; Always create the ending mark (even if "end" starts a
	      ;; line), so that `check-active-region-font-marks' can expect
	      ;; it.
	      (if last-chi-mark
		  ;; Create a new font mark at the end of the region like
		  ;; last-chi-mark.
		  (let ((fmark (font-mark (mark-line end)
					  (mark-charpos end)
					  last-chi-font)))
		    (setf (font-mark-fore-color fmark)
			  last-chi-fore)
		    (setf (font-mark-back-color fmark)
			  last-chi-back)
		    (push fmark *active-region-font-marks*))
		  (stash-a-mark end 0 ())))))))
  (setf *active-region-font-marks* (nreverse *active-region-font-marks*)))

(defun kill-active-region-font-marks ()
  (dolist (mark *active-region-font-marks*)
    (delete-font-mark mark))
  (setf *active-region-font-marks* ())
  ;; Revert any context highlighting marks.
  (dolist (info *active-chi-mark-info*)
    (let* ((line (nth 1 info))
	   (chi-info (getf (line-plist line) 'chi-info)))
      (when chi-info
	(let ((new (font-mark line (nth 0 info)
			      (nth 2 info))))
	  (setf (font-mark-fore-color new) (nth 3 info))
	  (setf (font-mark-back-color new) (nth 4 info))
	  (setf (chi-info-font-marks chi-info)
		(cons new (chi-info-font-marks chi-info)))))))
  (setq *active-chi-mark-info* ()))

;;; CHECK-ACTIVE-REGION-FONT-MARKS returns t if the current region is the
;;; same as that what is highlighted on the screen.  Assume
;;; *active-region-font-marks* contains at least one mark.
;;;
(defun check-active-region-font-marks ()
  (let* ((region (current-region nil nil))
	 (end (region-end region))
	 (first-active-mark (car *active-region-font-marks*))
	 (last-active-mark (last *active-region-font-marks*)))
    (with-mark ((start (region-start region)))
      (if (mark= start end)
	  (return-from check-active-region-font-marks))

      (or (and first-active-mark (car last-active-mark))
	  (return-from check-active-region-font-marks))

      (or (and (mark-line first-active-mark)
	       (mark-line (car last-active-mark)))
	  (return-from check-active-region-font-marks))

      (or (eq (current-buffer)
	      (line-buffer (mark-line first-active-mark)))
	  (return-from check-active-region-font-marks))

      (and (mark= start first-active-mark)
	   (mark= end (car last-active-mark))
	   (return-from check-active-region-font-marks t))

      (or (and (mark<= start first-active-mark)
	       (mark>= end (car last-active-mark)))
	  (return-from check-active-region-font-marks ()))

      (flet ((include-active-line (line charpos position)
	       (let ((fmark (font-mark
			     line charpos
			     *active-region-highlight-font*)))
		 (setf (font-mark-fore-color fmark)
		       (color
			(value
			 active-region-highlight-foreground)))
		 (setf (font-mark-back-color fmark)
		       (color
			(value
			 active-region-highlight-background)))
		 (ecase position
		   (:start
		    (setq *active-region-font-marks*
			  (cons fmark
				*active-region-font-marks*)))
		   (:end
		    (let ((last (car last-active-mark)))
		      (setf (car last-active-mark) fmark
			    (cdr last-active-mark) (list last))
		      (setf last-active-mark
			    (cdr last-active-mark))))))))

	(flet ((include-active-region (region charpos position
					      &optional skip-first-p)
		 (do-region-lines (line region
					(or skip-first-p
					    (include-active-line
					     line
					     charpos
					     position))
					:backwards (eq position
						       :start))
		   (if skip-first-p
		       (setq skip-first-p ())
		       (include-active-line line 0 position)))))

	  (when (mark> end (car last-active-mark))
	    ;; Extension of previous region at the end.
	    (let* ((old-end (copy-mark (car last-active-mark)))
		   (last-chi-mark (last-chi-mark
				   end
				   (let ((mark (car last-active-mark)))
				     (if (eq (mark-line end)
					     (mark-line mark))
					 mark))))
		   last-active-font last-active-fore last-active-back)
	      (when last-chi-mark
		(setq last-active-font
		      (edi::font-mark-font last-chi-mark)) ; FIX
		(setq last-active-fore
		      (font-mark-fore-color last-chi-mark))
		(setq last-active-back
		      (font-mark-back-color last-chi-mark)))
	      (include-active-region
	       (region (car last-active-mark) end)
	       0
	       :end
	       t)
	      (delete-font-mark (car last-active-mark))
	      ;; Create a new active font mark at the end of the
	      ;; region, either like last-chi-mark or plain.
	      (if last-chi-mark
		  (let ((fmark (font-mark
				(mark-line end)
				(mark-charpos end)
				last-active-font)))
		    (setf (font-mark-fore-color fmark) last-active-fore)
		    (setf (font-mark-back-color fmark) last-active-back)
		    (setf (car last-active-mark) fmark))
		  (setf (car last-active-mark)
			(font-mark (mark-line end)
				   (mark-charpos end)
				   0)))
	      (note-and-free-chi-marks (region old-end end))))

	  (when (mark< start first-active-mark)
	    ;; Extension of previous region from the beginning.
	    (let ((region (region start first-active-mark)))
	      (include-active-region region (mark-charpos start)
				     :start)
	      (note-and-free-chi-marks region)))

	  (setq *highlighted-region* region)

	  t)))))


;;;; Context (syntax).

;; FIX check fonts in range

(defvar *original-font* 0
  "Index of the font to use for normal text.")
(defvar *comment-font* 0
  "Index of the font to use for comments.")
(defvar *string-font* 0
  "Index of the font to use for strings.")
(defvar *variable-font* 0
  "Index of the font to use for variable names.")
(defvar *function-font* 0
  "Index of the font to use for variable names.")
(defvar *preprocessor-font* 0
  "Index of the font to use for preprocessor directives.")
(defvar *special-form-font* 0
  "Index of the font to use for special forms.")
(defvar *error-font* 0
  "Index of the font to use for errors.")

(defvar *underline-font* 0 ; 19 FIX out of range
  "Index of the font to use for underlining.")

(proclaim '(inline search-for-qmark))

(defun check-highlight-line (line property)
  "Return the highlight ch-info associated with symbol $property for $line
   if line needs to be considered for highlighting."
  (let ((info (getf (line-plist line) property)))
    (if info
	(fi (eq (line-signature line) (ch-info-signature info))
	    (progn
	      (setf (ch-info-signature info) (line-signature line))
	      info))
	(let ((info (make-ch-info (line-signature line))))
	  (setf (getf (line-plist line) property) info)
	  info))))

(defun search-for-qmark (string &optional (start 0))
  "Return position of first \" in $string if there are any, else nil.  Skip
   over any quoted quotation marks (like \\\")."
  (do ((string-start (position #\" string :start start)
		     (position #\" string :start (1+ string-start))))
      ((or (eq string-start ())
	   (zerop string-start)
	   (and (plusp string-start)
		(evenp (let ((count 0))
			 (loop
			   for start downfrom (1- string-start) to 0
			   while (eq (aref string start) #\\) do
			   (incf count))
			 count))))
       string-start)))

;; FIX consider integration with lispmode and other modes
(defstruct (ch-info (:constructor make-ch-info (signature)))
  "Per-line context highlighting information."
  (font-marks ())
  signature)

(defstruct (chi-info (:constructor make-chi-info (signature))
		     (:include ch-info))
  "Per-line context highlighting information for syntaxes with multi-line
   comments and strings."
  begins-quoted
  ending-quoted
  begins-commented
  ending-commented
  start-context
  end-context)

(defevar "Highlight Context"
  "When true any per-mode context highlighters will be called on the
   appropriate hooks."
  :value t)

(defcommand "Highlight Context" ()
  "Call any per-mode context highlighters for the current buffer, even if
   *Highlight Context* is ()."
  (elet ((highlight-context t))
    (highlight-modes (current-buffer))))

;;; clear-chi-signatures  --  Internal.
;;;
;;; Clear signatures on any chi-marks in $region.
;;;
(defun clear-chi-signatures (region)
  (if (mark> (region-start region) (region-end region))
      ;; Assume something like `Exchange Point and Mark' happened.
      (let ((old-end (region-end region))
	    (old-start (region-start region)))
	(setf (region-end region) old-start
	      (region-start region) old-end)))
  (until ((line (mark-line (region-start region)) (line-next line))
	  (end (line-next (mark-line (region-end region)))))
	 ((eq line end))
    (let ((chi-info (getf (line-plist line) 'chi-info)))
      (when chi-info
	(setf (chi-info-signature chi-info) ())))))

;;; clear-chi-marks  --  Internal.
;;;
;;; Clear any chi-marks from $region.
;;;
(defun clear-chi-marks (region &optional preserve)
  (let* ((start-mark (region-start region))
	 (end-mark (region-end region))
	 (line (mark-line start-mark))
	 (end (mark-line end-mark)))
    (if (eq line end)
	;; First and last line.
	(let ((chi-info (getf (line-plist line) 'chi-info))
	      (start-charpos (mark-charpos start-mark))
	      (end-charpos (mark-charpos end-mark)))
	  (when chi-info
	    (dolist (mark (chi-info-font-marks chi-info))
	      (or (and preserve (mark= mark preserve))
		  (let ((charpos (mark-charpos mark)))
		    (when (and (>= charpos start-charpos)
			       (<= charpos end-charpos))
		      (delete-font-mark mark)
		      (setf (chi-info-font-marks chi-info)
			    (delq mark
				  (chi-info-font-marks chi-info)))))))))
	(progn
	  ;; First line.
	  (let ((chi-info (getf (line-plist line) 'chi-info))
		(start-charpos (mark-charpos start-mark)))
	    (when chi-info
	      (dolist (mark (chi-info-font-marks chi-info))
	      (or (and preserve (mark= mark preserve))
		  (when (>= (mark-charpos mark) start-charpos)
		    (delete-font-mark mark)
		    (setf (chi-info-font-marks chi-info)
			  (delq mark
				(chi-info-font-marks chi-info))))))))
	  (until ((line (line-next line) (line-next line))
		  (end-charpos (mark-charpos end-mark)))
		 ((eq line end)
		  ;; Last line.
		  (let ((chi-info (getf (line-plist line) 'chi-info)))
		    (when chi-info
		      (dolist (mark (chi-info-font-marks chi-info))
			(or (and preserve (mark= mark preserve))
			    (when (<= (mark-charpos mark) end-charpos)
			      (delete-font-mark mark)
			      (setf (chi-info-font-marks chi-info)
				    (delq mark
					  (chi-info-font-marks
					   chi-info)))))))))
	    ;; Middle line.
	    (let ((chi-info (getf (line-plist line) 'chi-info)))
	      (when chi-info
		(dolist (mark (chi-info-font-marks chi-info))
		  (or (and preserve (mark= mark preserve))
		      (delete-font-mark mark)))
		(setf (chi-info-font-marks chi-info) ()))))))))

(defvar *active-chi-mark-info* ())

;;; note-and-free-chi-marks  --  Internal.
;;;
;;; Note and free any chi-marks in $region.
;;;
(defun note-and-free-chi-marks (region)
  (let* ((start-mark (region-start region))
	 (end-mark (region-end region))
	 (line (mark-line start-mark))
	 (end (mark-line end-mark)))
    (if (eq line end)
	;; First and last line.
	(let ((chi-info (getf (line-plist line) 'chi-info))
	      (start-charpos (mark-charpos start-mark))
	      (end-charpos (mark-charpos end-mark))
	      (old))
	  (when chi-info
	    (dolist (mark (chi-info-font-marks chi-info))
	      (let ((charpos (mark-charpos mark)))
		(when (and (mark-line mark)
			   (>= charpos start-charpos)
			   (<= charpos end-charpos))
		  (push (list (mark-charpos mark)
			      (mark-line mark)
			      (edi::font-mark-font mark) ; FIX
			      (font-mark-fore-color mark)
			      (font-mark-back-color mark))
			*active-chi-mark-info*)
		  (push mark old))))
	    (setf (chi-info-font-marks chi-info)
		  (set-difference (chi-info-font-marks chi-info)
				  old))
	    (dolist (mark old) (delete-font-mark mark))))
	(progn
	  ;; First line.
	  (let ((chi-info (getf (line-plist line) 'chi-info))
		(start-charpos (mark-charpos start-mark))
		(old))
	    (when chi-info
	      (dolist (mark (chi-info-font-marks chi-info))
		(when (and (mark-line mark)
			   (>= (mark-charpos mark) start-charpos))
		  (push (list (mark-charpos mark)
			      (mark-line mark)
			      (edi::font-mark-font mark) ; FIX
			      (font-mark-fore-color mark)
			      (font-mark-back-color mark))
			*active-chi-mark-info*)
		  (push mark old)))
	      (setf (chi-info-font-marks chi-info)
		    (set-difference (chi-info-font-marks chi-info)
				    old))
	      (dolist (mark old) (delete-font-mark mark))))
	  (until ((line (line-next line) (line-next line))
		  (end-charpos (mark-charpos end-mark)))
		 ((eq line end)
		  ;; Last line.
		  (let ((chi-info (getf (line-plist line) 'chi-info))
			(old))
		    (when chi-info
		      (dolist (mark (chi-info-font-marks chi-info))
			(when (and (mark-line mark)
				   (<= (mark-charpos mark)
				       end-charpos))
			  (push (list (mark-charpos mark)
				      (mark-line mark)
				      (edi::font-mark-font mark) ; FIX
				      (font-mark-fore-color mark)
				      (font-mark-back-color mark))
				*active-chi-mark-info*)
			  (push mark old))
			(setf (chi-info-font-marks chi-info)
			      (set-difference (chi-info-font-marks chi-info)
					      old))
			(dolist (mark old) (delete-font-mark mark))))))
	    ;; Middle line.
	    (let ((chi-info (getf (line-plist line) 'chi-info))
		  (old))
	      (when chi-info
		(dolist (mark (chi-info-font-marks chi-info))
		  (when (mark-line mark)
		    (push (list (mark-charpos mark)
				(mark-line mark)
				(edi::font-mark-font mark) ; FIX
				(font-mark-fore-color mark)
				(font-mark-back-color mark))
			  *active-chi-mark-info*))
		  (push mark old))
		(setf (chi-info-font-marks chi-info)
		      (set-difference (chi-info-font-marks chi-info)
				      old))
		(dolist (mark old) (delete-font-mark mark)))))))))

(defvar *mode-highlighters* ()
  "List of lists (mode-name major-mode-p highlighter-function).  Every time
   a file is read into a buffer or a buffer is changed the
   highlighter-function of any listed mode that is active in the buffer is
   run on the buffer.")

(defun highlight-modes (buffer &optional existp)
  (declare (ignore existp))
  (when (and (value highlight-context) (bufferp buffer))
    (let ((major (buffer-major-mode buffer)))
      (dolist (hi *mode-highlighters*)
	(when (if (cadr hi)
		  (string= (car hi) major)
		  (buffer-minor-mode buffer (car hi)))
	  (handler-case (funcall (caddr hi) buffer)
	    (error (condition) (values () condition))))))))

(defun highlight-modes-visible ()
  "Call per-mode highlighters on each visible buffer."
  (dolist (window *window-list*)
    (highlight-modes (window-buffer window))))

(defun highlight-modes-visible-window (window)
  "Call per-mode highlighters on the buffer in Window."
  (highlight-modes (window-buffer window))
  ;; Run after mode highlighters to be able to clear mode highlighter font
  ;; marks.
  (highlight-active-region window))
;;
(add-hook redisplay-hook #'highlight-modes-visible-window)

(defun ensure-chi-info (line)
  (or (getf (line-plist line) 'chi-info)
      (setf (getf (line-plist line) 'chi-info)
	    (make-chi-info (line-signature line)))))

(defun line-same-p (line)
  (let ((font-info (getf (line-plist line) 'chi-info)))
    (and font-info
	 (eq (line-signature line) (chi-info-signature font-info)))))

(proclaim '(special *last-used-mark*))

(defun chi-mark (line pos font color chi-info)
  (if (cdr *last-used-mark*)
      (let ((fmark (cadr *last-used-mark*)))
	(setq *last-used-mark* (cdr *last-used-mark*))
	(if (mark-line fmark)
	    (progn
	      (move-font-mark fmark (mark line pos))
	      (setf (edi::font-mark-font fmark) font) ; FIX
	      (setf (font-mark-fore-color fmark) (color color)))
	    (let ((fmark (font-mark line pos font)))
	      (setf (font-mark-fore-color fmark) (color color))
	      (setf (car *last-used-mark*) fmark))))
      (let ((fmark (font-mark line pos font)))
	(setf (font-mark-fore-color fmark) (color color))
	(push fmark (chi-info-font-marks chi-info)))))

(defmacro highlight-chi-buffer (buffer line-highlight-fun)
  "Highlighting context in Buffer.  Line-highlight-fun is called on each
   line that needs updating."
  `(let ((*context*)
	 (line (mark-line (buffer-start-mark ,buffer))))
     ;; Move to the first changed line.
     (while () ((and line (line-same-p line)))
       (setq line (line-next line)))
     (when line
       ;; Setup context from previous line.
       (let ((previous (line-previous line)))
	 (when previous
	   (let ((chi-info (getf (line-plist previous) 'chi-info)))
	     (if chi-info
		 (setq *context* (chi-info-end-context chi-info))))))
       ;; Call the highlighter on each line.
       (catch 'highlight-exit
	 (while () (line)
	   (ensure-chi-info line)
	   (let* ((chi-info (getf (line-plist line) 'chi-info))
		  (*last-used-mark* (cons nil (chi-info-font-marks chi-info))))
	     (setf (chi-info-start-context chi-info) *context*)
	     ,(list line-highlight-fun 'line 'chi-info)
	     (setf (chi-info-end-context chi-info) *context*)
	     (setf (chi-info-signature chi-info)
		   (line-signature line))
	     ;; Free any remaining font marks.
	     (when (cdr *last-used-mark*)
	       (dolist (old-mark (cdr *last-used-mark*))
		 (delete-font-mark old-mark))
	       (if (car *last-used-mark*)
		   (setf (cdr *last-used-mark*) nil)
		   (setf (chi-info-font-marks chi-info) nil))))
	   ;; FIX + If the line is the same as it was before then skip to the
	   ;; next changed line?
	   (setq line (line-next line)))))))

(defmacro highlight-visible-chi-buffer (buffer line-highlight-fun)
  "Highlight context in visible portions of Buffer.  Line-highlight-fun is
   called on each line that needs updating."
  `(dolist (window (buffer-windows ,buffer))
     (let ((*context*)
	   (line (mark-line (window-display-start window)))
	   (end (mark-line (window-display-end window))))
       (or () ;(eq (edi::window-first-changed window) edi::the-sentinel) FIX
	   (progn
	     ;; Move to the first changed line.
	     (loop while (and line (line<= line end) (line-same-p line)) do
	       (setq line (line-next line)))
	     (when (and line (line<= line end))
	       ;; Setup string and comment context from the nearest
	       ;; previous up-to-date line.
	       (let ((previous (line-previous line)))
		 (loop while previous do
		   (if (line-same-p previous) (return))
		   (setq previous (line-previous previous)))
		 (if previous
		   (let ((chi-info (getf (line-plist previous) 'chi-info)))
		     (if chi-info
			 (setq *context* (chi-info-end-context chi-info)))
		     (setq line (line-next previous)))
		   (setq line (mark-line (buffer-start-mark buffer)))))
	       ;; Call the highlighter on each line.
	       (catch 'highlight-exit
		 (loop while (and line (line<= line end)) do
		   (ensure-chi-info line)
		   (let* ((chi-info (getf (line-plist line) 'chi-info))
			  (*last-used-mark*
			   (cons () (chi-info-font-marks chi-info))))
		     (setf (chi-info-start-context chi-info) *context*)
		     ,(list line-highlight-fun 'line 'chi-info)
		     (setf (chi-info-end-context chi-info) *context*)
		     (setf (chi-info-signature chi-info)
			   (line-signature line))
		     ;; Free any remaining font marks.
		     (when (cdr *last-used-mark*)
		       (dolist (old-mark (cdr *last-used-mark*))
			 (delete-font-mark old-mark))
		       (if (car *last-used-mark*)
			   (setf (cdr *last-used-mark*) nil)
			   (setf (chi-info-font-marks chi-info) nil))))
		   ;; FIX + If the line is the same as it was before then skip to the
		   ;; next changed line?
		   (setq line (line-next line)))
		 ;; Clear the signature on the line following the window, so that
		 ;; the next highlighting continues from there.
		 (when line
		   (ensure-chi-info line)
		   (let ((chi-info (getf (line-plist line) 'chi-info)))
		     (setf (chi-info-signature chi-info) nil))))))))))


;;;; Trailing space.

; FIX reverse vid must swap
(defevar "Trailing Space Highlight Foreground"
  "The foreground color for the trailing whitespace highlight."
  :value :window-foreground)

(defevar "Trailing Space Highlight Background"
  "The background color for the trailing whitespace highlight."
  :value :comment)

(defun update-trailing-space-highlight (&optional name kind where value)
  "Update all buffers to match value of *Highlight Trailing Space*."
  (declare (ignore name))
  (case kind
    (:buffer
     (if value
	 (rehighlight-trailing-space where)
	 (clear-trailing-space-highlight where)))
    ((or :global :current :mode)
     (dolist (buffer *buffer-list*)
       (if value
	   (rehighlight-trailing-space buffer)
	   (clear-trailing-space-highlight buffer))))))

(defun check-h-t-s-line (line)
  "Return the trailing highlight ch-info for Line if line needs to be
   considered for highlighting."
  (let ((info (getf (line-plist line) 'trailing-ch-info)))
    (if info
	(if (eq (line-signature line) (ch-info-signature info))
	    nil
	    (progn
	      (setf (ch-info-signature info) (line-signature line))
	      info))
	(let ((info (make-ch-info (line-signature line))))
	  (setf (getf (line-plist line) 'trailing-ch-info) info)
	  info))))

(defun get-h-t-s-line-info (line)
  "Return the trailing highlight ch-info for Line if line needs to be
   considered for highlighting."
  (let ((info (getf (line-plist line) 'trailing-ch-info)))
    (or info
	(let ((info (make-ch-info (line-signature line))))
	  (setf (getf (line-plist line) 'trailing-ch-info) info)
	  info))))

(defun clear-trailing-space-highlight (buffer)
  (do-buffer-lines (line buffer)
    (when (> (line-length line) 0)
      (let ((info (getf (line-plist line) 'trailing-ch-info)))
	(when info
	  (dolist (fmark (ch-info-font-marks info))
	    (if (mark-line fmark) (delete-font-mark fmark)))
	  (setf (ch-info-font-marks info) nil))))))

(defmacro rehighlight-line-trailing-space (info)
  `(progn
     (dolist (fmark (ch-info-font-marks ,info))
       (if (mark-line fmark) (delete-font-mark fmark)))
     (setf (ch-info-font-marks ,info) nil)
     (let ((mark (mark line (line-length line))))
       (when (reverse-find-attribute mark :whitespace #'zerop)
	 (flet ((setf-color (fmark)
		  (setf (font-mark-fore-color fmark)
			(color (value trailing-space-highlight-foreground)))
		  (setf (font-mark-back-color fmark)
			(color (value trailing-space-highlight-background)))
		  fmark))
	   (if (eq (mark-line mark) line)
	       (if (plusp (character-attribute :space
					       (next-character mark)))
		   (push (setf-color (font-mark line
					       (mark-charpos mark) 9)) ; FIX 9
			 (ch-info-font-marks ,info)))
	       (push (setf-color (font-mark line 0 9))
		     (ch-info-font-marks ,info))))))))

(defun rehighlight-trailing-space (buffer)
  (do-buffer-lines (line buffer)
    (when (> (line-length line) 0)
      (rehighlight-line-trailing-space (get-h-t-s-line-info line))))
  ;; Clear any trailing space highlighting from the line at point.
  (let ((info (getf (line-plist (mark-line (buffer-point buffer)))
		    'trailing-ch-info)))
    (when info
      (dolist (fmark (ch-info-font-marks info))
	(if (mark-line fmark) (delete-font-mark fmark)))
      (setf (ch-info-font-marks info) ())
      ;; Force line to be reconsidered on next refresh.
      (setf (ch-info-signature info) ()))))

(defcommand "Highlight Trailing Space" (p)
  "Highlight trailing whitespace on lines in the current buffer.  With a
   prefix clear any such highlighting."
  "Highlight trailing whitespace on lines in the current buffer.  If P is
   true clear any such highlighting."
  (if p
      (clear-trailing-space-highlight (current-buffer))
      (rehighlight-trailing-space (current-buffer))))

(defun highlight-trailing-space (buffer &optional existp)
  "Highlight trailing space on lines in Buffer."
  (declare (ignore existp))
  (when (value highlight-trailing-space)
    (do-buffer-lines (line buffer)
      (when (> (line-length line) 0)
	(let ((info (check-h-t-s-line line)))
	  (when info (rehighlight-line-trailing-space info)))))
    ;; Clear any highlighting on point line before point.
    (let* ((point (buffer-point buffer))
	   (info (getf (line-plist (mark-line point))
		       'trailing-ch-info)))
      (when info
	(dolist (fmark (ch-info-font-marks info))
	  (when (mark<= fmark point)
	    (if (mark-line fmark) (delete-font-mark fmark))
	    (setf (ch-info-font-marks info)
		  (delq fmark (ch-info-font-marks info)))))
	;; Force line to be reconsidered on next refresh.
	(setf (ch-info-signature info) ())))))

(defun highlight-window-trailing-space (window)
  ;; FIX may need to use window height instead
  (let ((end-line (mark-line (window-display-end window))))
    (loop
      for line = (mark-line (window-display-start window))
               then (line-next line)
      while (and line (line<= line end-line))
      do
      (when (> (line-length line) 0)
	(let ((info (check-h-t-s-line line)))
	  (when info (rehighlight-line-trailing-space info)))))
    (let* ((point (buffer-point (window-buffer window)))
	   (info (get-h-t-s-line-info (mark-line point))))
      (dolist (fmark (ch-info-font-marks info))
	(if (mark-line fmark)
	    (when (mark<= fmark point)
	      (if (mark-line fmark) (delete-font-mark fmark))
	      (setf (ch-info-font-marks info)
		    (delq fmark (ch-info-font-marks info))))
	    (setf (ch-info-font-marks info)
		  (delq fmark (ch-info-font-marks info)))))
      ;; Force line to be reconsidered on next refresh.
      (setf (ch-info-signature info) ()))))

(defun highlight-visible-trailing-space (buffer &optional existp)
  "Highlight trailing whitespace in the visible portions of buffer."
  (declare (ignore existp))
  (dolist (window (buffer-windows buffer))
    (or (eq window *echo-area-window*)
	(highlight-window-trailing-space window))))
;;
(add-hook read-file-hook #'highlight-visible-trailing-space)

(defun highlight-all-visible-trailing-space ()
  "Highlight trailing whitespace in the visible portions of all buffers."
  (dolist (window *window-list*)
    (or (eq window *echo-area-window*)
	(highlight-window-trailing-space window))))
;;
(add-hook after-command-hook #'highlight-all-visible-trailing-space)

(defun highlight-current-window-trailing-space ()
  "Highlight trailing space on lines in current window."
  (highlight-visible-trailing-space (window-buffer (current-window))))

(defun highlight-current-buffer-trailing-space ()
  "Highlight trailing space on lines in current buffer."
  (highlight-trailing-space (current-buffer)))

(defevar "Highlight Trailing Space"
  "If true trailing whitespace on lines will be highlighted."
  :value t
  :hooks '(update-trailing-space-highlight))


;;;; Comments.

(defun update-comments-highlight (&optional name kind where value)
  "Update all buffers to match value of *Highlight Comments*."
  (declare (ignore name where))
  (case kind
    (:global
     (dolist (buffer *buffer-list*)
       (if value
	   (rehighlight-comments buffer)
	   (clear-comments-highlight buffer))))))

(defun check-highlight-comments-line (line)
  "Return the comment highlight ch-info for Line if line needs to be
   considered for highlighting."
  (let ((info (getf (line-plist line) 'comments-ch-info)))
    (if info
	(if (eq (line-signature line) (ch-info-signature info))
	    nil
	    (progn
	      (setf (ch-info-signature info) (line-signature line))
	      info))
	(let ((info (make-ch-info (line-signature line))))
	  (setf (getf (line-plist line) 'comments-ch-info) info)
	  info))))

(defun get-highlight-comments-line-info (line)
  "Return the outline ch-info for Line if line needs to be considered for
   highlighting."
  (let ((info (getf (line-plist line) 'comments-ch-info)))
    (or info
	(let ((info (make-ch-info (line-signature line))))
	  (setf (getf (line-plist line) 'comments-ch-info) info)
	  info))))

(defun clear-comments-highlight (buffer)
  (do-buffer-lines (line buffer)
    (when (> (line-length line) 0)
      (let ((info (getf (line-plist line) 'comments-ch-info)))
	(when info
	  (dolist (fmark (ch-info-font-marks info))
	    (delete-font-mark fmark)))))))

(defmacro rehighlight-line-comments (info comment-string comment-string-len)
  `(progn
     (dolist (fmark (ch-info-font-marks ,info))
       (delete-font-mark fmark))
     (let ((mark (mark line 0)))
       (when (and (find-attribute mark :whitespace #'zerop)
		  (eq (mark-line mark) line))
	 (let* ((string (subseq (line-string line) (mark-charpos mark))))
	   (if (and (>= (length string) ,comment-string-len)
		    (string= string ,comment-string
			     :end1 ,comment-string-len
			     :end2 ,comment-string-len))
	       (let ((fmark (font-mark line 0 1))) ; FIX 1
		 (setf (font-mark-fore-color fmark) (color :comment))
		 (push fmark
		       (ch-info-font-marks ,info)))))))))

(defun rehighlight-comments (buffer)
  (let ((comment-string (value comment-start)))
    (when comment-string
      (let ((comment-string-len (length comment-string)))
	(do-buffer-lines (line buffer)
	  (when (> (line-length line) 0)
	    (rehighlight-line-comments (get-highlight-comments-line-info line)
				       comment-string
				       comment-string-len)))))))

(defcommand "Highlight Comments" (p)
  "Highlight single-line comments in the current buffer.  With a prefix
   clear any such highlighting."
  "Highlight single-line comments in the current buffer.  If P is true
   clear any such highlighting."
  (if p
      (clear-comments-highlight (current-buffer))
      (rehighlight-comments (current-buffer))))

(defun highlight-comments (buffer &optional existp)
  "Highlight single-line comments on lines in Buffer."
  (declare (ignore existp))
  (when (value highlight-comments)
    (let ((comment-string (value comment-start)))
      (when comment-string
	(let ((comment-string-len (length comment-string)))
	  (do-buffer-lines (line buffer)
	    (when (> (line-length line) 0)
	      (let ((info (check-highlight-comments-line line)))
		(when info
		  (rehighlight-line-comments info
					     comment-string
					     comment-string-len))))))))))

(defun highlight-visible-comments (buffer &optional existp)
  "Highlight single-line comments on lines in Buffer that are visible in
   any window."
  (declare (ignore existp))
  (when (value highlight-comments)
    (let ((comment-string (value comment-start)))
      (when comment-string
	(let ((comment-string-len (length comment-string)))
	  (dolist (window (buffer-windows buffer))
	    (let ((end-line (mark-line (window-display-end window))))
	      (loop for line = (mark-line (window-display-start window))
		             then (line-next line)
		    while (and line (line<= line end-line)) do
		(when (> (line-length line) 0)
		  (let ((info (check-highlight-comments-line line)))
		    (when info
		      (rehighlight-line-comments info
						 comment-string
						 comment-string-len))))))))))))
;;
(add-hook read-file-hook #'highlight-comments)
(add-hook after-change-hook #'highlight-comments)

(defevar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :hooks '(update-comments-highlight))
