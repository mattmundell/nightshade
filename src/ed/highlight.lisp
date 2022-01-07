;;; Highlighting.

(in-package "ED")


;;;; Open parens.

(defhvar "Highlight Open Parens"
  "When non-nil, causes open parens to be displayed in a different font when
   the cursor is directly to the right of the corresponding close paren."
  :value nil)

(defhvar "Open Paren Finder Function"
  "Should be a function that takes a mark for input and returns either NIL
   if the mark is not after a close paren, or two (temporary) marks
   surrounding the corresponding open paren."
  :value 'lisp-open-paren-finder-function)


(defvar *open-paren-font-marks* nil
  "The pair of font-marks surrounding the currently highlighted open-
   paren or nil if there isn't one.")

(defvar *open-paren-highlight-font* 2
  "The index into the font-map for the open paren highlighting font.")

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
	    (setf (region-start *open-paren-font-marks*)
		  (font-mark (mark-line start)
			     (mark-charpos start)
			     *open-paren-highlight-font*)))
	(setq fmark (region-end *open-paren-font-marks*))
	(if (mark-line fmark)
	    (or (mark= fmark end)
		(move-font-mark fmark end))
	    (setf (region-end *open-paren-font-marks*)
		  (font-mark (mark-line end)
			     (mark-charpos end)
			     *open-paren-highlight-font*))))
      (let ((line (mark-line start)))
	(setf *open-paren-font-marks*
	      (region
	       (font-mark line (mark-charpos start)
			  *open-paren-highlight-font*)
	       (font-mark line (mark-charpos end) 0))))))

(defun kill-open-paren-font-marks ()
  (when *open-paren-font-marks*
    (delete-font-mark (region-start *open-paren-font-marks*))
    (delete-font-mark (region-end *open-paren-font-marks*))
    (setf *open-paren-font-marks* nil)))


;;;; Active regions.

(defvar *active-region-font-marks* nil)
(defvar *active-region-highlight-font* 3
  "The index into the font-map for the active region highlighting font.")

;;; HIGHLIGHT-ACTIVE-REGION is a redisplay hook for active regions.
;;; Since it is too hard to know how the region may have changed when it is
;;; active and already highlighted, if it does not check out to being exactly
;;; the same, we just delete all the font marks and make new ones.  When
;;; the current window is the echo area window, just pretend everything is
;;; okay; this keeps the region highlighted while we're in there.
;;;
(defun highlight-active-region (window)
  (unless (eq window *echo-area-window*)
    (when (value highlight-active-region)
      (cond ((region-active-p)
	     (cond ((not *active-region-font-marks*)
		    (set-active-region-font-marks))
		   ((check-active-region-font-marks))
		   (t (kill-active-region-font-marks)
		      (set-active-region-font-marks))))
	    (*active-region-font-marks*
	     (kill-active-region-font-marks))))))
;;;
(add-hook redisplay-hook 'highlight-active-region)

(defun set-active-region-font-marks ()
  (flet ((stash-a-mark (m &optional (font *active-region-highlight-font*))
	   (push (font-mark (mark-line m) (mark-charpos m) font)
		 *active-region-font-marks*)))
    (let* ((region (current-region nil nil))
	   (start (region-start region))
	   (end (region-end region)))
      (with-mark ((mark start))
	(unless (mark= mark end)
	  (loop
	    (stash-a-mark mark)
	    (or (line-offset mark 1 0) (return))
	    (when (mark>= mark end) (return)))
	  (or (start-line-p end) (stash-a-mark end 0))))))
  (setf *active-region-font-marks* (nreverse *active-region-font-marks*)))

(defun kill-active-region-font-marks ()
  (dolist (m *active-region-font-marks*)
    (delete-font-mark m))
  (setf *active-region-font-marks* nil))

;;; CHECK-ACTIVE-REGION-FONT-MARKS returns t if the current region is the same
;;; as that what is highlighted on the screen.  This assumes
;;; *active-region-font-marks* is non-nil.  At the very beginning, our start
;;; mark must not be at the end; it must be at the first font mark; and the
;;; font marks must be in the current buffer.  We don't make font marks if the
;;; start is at the end, so if this is the case, then they just moved together.
;;; We return nil in this case to kill all the font marks and make new ones, but
;;; no new ones will be made.
;;;
;;; Sometimes we hack the font marks list and return t because we can easily
;;; adjust the highlighting to be correct.  This keeps all the font marks from
;;; being killed and re-established.  In the loop, if there are no more font
;;; marks, we either ended a region already highlighted on the next line down,
;;; or we have to revamp the font marks.  Before returning here, we see if the
;;; region ends one more line down at the beginning of the line.  If this is
;;; true, then the user is simply doing "Next Line" at the beginning of the
;;; line.
;;;
;;; Each time through the loop we look at the top font mark, move our roving
;;; mark down one line, and see if they compare.  If they are not equal, the
;;; region may still be the same as that highlighted on the screen.  If this
;;; is the last font mark, not at the beginning of the line, and it is at the
;;; region's end, then this last font mark is in the middle of a line somewhere
;;; changing the font from the highlighting font to the default font.  Return
;;; t.
;;;
;;; If our roving mark is not at the current font mark, but it is at or after
;;; the end of the active region, then the end of the active region has moved
;;; before its previous location.
;;;
;;; Otherwise, move on to the next font mark.
;;;
;;; If our roving mark never moved onto a next line, then the buffer ends on the
;;; previous line, and the last font mark changes from the highlighting font to
;;; the default font.
;;;
(defun check-active-region-font-marks ()
  (let* ((region (current-region nil nil))
	 (end (region-end region)))
    (with-mark ((mark (region-start region)))
      (let ((first-active-mark (car *active-region-font-marks*))
	    (last-active-mark (last *active-region-font-marks*)))
	(if (and (mark/= mark end)
		 (eq (current-buffer)
		     (line-buffer (mark-line first-active-mark)))
		 (mark= first-active-mark mark))
	    (let ((marks (cdr *active-region-font-marks*)))
	      (loop
		(unless marks
		  (let ((res (and (line-offset mark 1 0)
				  (mark= mark end))))
		    (when (and (not res)
			       (line-offset mark 1 0)
			       (mark= mark end)
			       (start-line-p (car last-active-mark)))
		      (setf (cdr last-active-mark)
			    (list (font-mark (line-previous (mark-line mark))
					     0
					     *active-region-highlight-font*)))
		      (return t))
		    (return res)))
		(let ((fmark (car marks)))
		  (if (line-offset mark 1 0)
		      (cond ((mark/= mark fmark)
			     (return (and (not (cdr marks))
					  (not (start-line-p fmark))
					  (mark= fmark end))))
			    ((mark>= mark end)
			     (return nil))
			    (t (setf marks (cdr marks))))

		      (return (and (not (cdr marks))
				   (not (start-line-p fmark))
				   (mark= fmark end))))))))))))


;;;; Context (syntax).

(defvar *original-font* 0
  "Index of the font to use for normal text.")
(defvar *comment-font* 1
  "Index of the font to use for comments.")
(defvar *string-font* 2
  "Index of the font to use for strings.")
(defvar *variable-name-font* 3
  "Index of the font to use for variable names.")
(defvar *function-name-font* 4
  "Index of the font to use for variable names.")
(defvar *preprocessor-font* 5
  "Index of the font to use for preprocessor directives.")
(defvar *special-form-font* 6
  "Index of the font to use for special forms.")
(defvar *error-font* 7
  "Index of the font to use for errors.")

(defvar *underline-font* 19
  "Index of the font to use for errors.")

(proclaim '(inline search-for-qmark))

(defun search-for-qmark (string &optional (start 0))
  "Return position of first \" in String if there are any, else nil.  Skip
   any quoted quotation marks (like \\\")."
  (do ((string-start (position #\" string :start start)
		     (position #\" string :start (1+ string-start))))
      ((or (eq string-start nil)
	   (zerop string-start)
	   (and (plusp string-start)
		(oddp (loop
			for start downfrom (1- string-start) count start
			while (eq (aref string start) #\\)))))
       string-start)))

;; FIX consider integration with lispmode and other modes
(defstruct (ch-info (:constructor make-ch-info (signature)))
  "Per-line context highlighting information."
  (font-marks ())
  (signature))

(defstruct (chi-info (:constructor make-chi-info (signature))
		     (:include ch-info))
  "Per-line context highlighting information for syntaxes with multi-line
   comments and strings."
  (begins-quoted)
  (ending-quoted)
  (begins-commented)
  (ending-commented))

(defhvar "Highlight Context"
  "When true any per-mode context highlighters will be called on the
   appropriate hooks."
  :value t)

(defcommand "Highlight Context" (p)
  "Call any per-mode context highlighters for the current buffer, even if
   \"Highlight Context\" is nil."
  "Call any per-mode context highlighters for the current buffer, even if
   \"Highlight Context\" is nil."
  (declare (ignore p))
  (hlet ((highlight-context t))
    (highlight-modes (current-buffer))))

(defvar *mode-highlighters* nil
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
	  (funcall (caddr hi) buffer))))))

(defun highlight-modes-visible ()
  "Call per-mode highlighters on each visible buffer."
  (dolist (window *window-list*)
    (highlight-modes (window-buffer window))))

(defun highlight-modes-visible-window (window)
  "Call per-mode highlighters on the buffer in Window."
  (highlight-modes (window-buffer window)))
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

(defun chi-mark (line pos font chi-info)
  (if (cdr *last-used-mark*)
      (let ((fmark (cadr *last-used-mark*)))
	(setq *last-used-mark* (cdr *last-used-mark*))
	(if (mark-line fmark)
	    (setf (edi::font-mark-font (move-font-mark fmark
						      (mark line pos)))
		  font)
	    (setf (car *last-used-mark*) (font-mark line pos font))))
      (push (font-mark line pos font) (chi-info-font-marks chi-info))))

(defmacro highlight-chi-buffer (buffer line-highlight-fun)
  "Highlighting context in Buffer.  Line-highlight-fun is called on each
   line that needs updating."
  `(let ((*in-string*)
	 (*in-comment*)
	 (line (mark-line (buffer-start-mark ,buffer))))
     ;; Move to the first changed line.
     (loop while (and line (line-same-p line)) do
       (setq line (line-next line)))
     (when line
       ;; Setup string and comment context from previous line.
       (let ((previous (line-previous line)))
	 (when previous
	   (let ((chi-info (getf (line-plist previous) 'chi-info)))
	     (when chi-info
	       (setq *in-string* (chi-info-ending-quoted chi-info))
	       (setq *in-comment* (chi-info-ending-commented chi-info))))))
       ;; Call the highlighter on each line.
       (loop while line do
	 (ensure-chi-info line)
	 (let* ((chi-info (getf (line-plist line) 'chi-info))
		(*last-used-mark* (cons nil (chi-info-font-marks chi-info))))
	   (setf (chi-info-begins-quoted chi-info) *in-string*)
	   (setf (chi-info-begins-commented chi-info) *in-comment*)
	   ,(list line-highlight-fun 'line 'chi-info)
	   (setf (chi-info-ending-quoted chi-info) *in-string*)
	   (setf (chi-info-ending-commented chi-info) *in-comment*)
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
	 (setq line (line-next line))))))

(defmacro highlight-visible-chi-buffer (buffer line-highlight-fun)
  "Highlight context in visible portions of Buffer.  Line-highlight-fun is
   called on each line that needs updating."
  `(dolist (window (buffer-windows ,buffer))
     (let ((*in-string*)
	   (*in-comment*)
	   (line (mark-line (window-display-start window)))
	   (end (mark-line (window-display-end window))))
       (or (eq (edi::window-first-changed window) edi::the-sentinel)
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
		     (when chi-info
		       (setq *in-string* (chi-info-ending-quoted chi-info))
		       (setq *in-comment* (chi-info-ending-commented chi-info)))
		     (setq line (line-next previous)))
		   (setq line (mark-line (buffer-start-mark buffer)))))
	       ;; Call the highlighter on each line.
	       (loop while (and line (line<= line end)) do
		 (ensure-chi-info line)
		 (let* ((chi-info (getf (line-plist line) 'chi-info))
			(*last-used-mark*
			 (cons () (chi-info-font-marks chi-info))))
		   (setf (chi-info-begins-quoted chi-info) *in-string*)
		   (setf (chi-info-begins-commented chi-info) *in-comment*)
		   ,(list line-highlight-fun 'line 'chi-info)
		   (setf (chi-info-ending-quoted chi-info) *in-string*)
		   (setf (chi-info-ending-commented chi-info) *in-comment*)
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
		   (setf (chi-info-signature chi-info) nil)))))))))


;;;; Trailing space.

(defun update-trailing-space-highlight (&optional name kind where value)
  "Update all buffers to match value of \"Highlight Trailing Space\"."
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
  (do-lines (line buffer)
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
	 (if (eq (mark-line mark) line)
	     (if (plusp (character-attribute :space
					     (next-character mark)))
		 (push (font-mark line (mark-charpos mark) 9)
		       (ch-info-font-marks ,info)))
	     (push (font-mark line 0 9)
		   (ch-info-font-marks ,info)))))))

(defun rehighlight-trailing-space (buffer)
  (do-lines (line buffer)
    (when (> (line-length line) 0)
      (rehighlight-line-trailing-space (get-h-t-s-line-info line))))
  ;; Clear any trailing space highlighting from the line at point.
  (let ((info (getf (line-plist (mark-line (buffer-point buffer)))
		    'trailing-ch-info)))
    (when info
      (dolist (fmark (ch-info-font-marks info))
	(if (mark-line fmark) (delete-font-mark fmark)))
      (setf (ch-info-font-marks info) nil)
      ;; Force line to be reconsidered on next refresh.
      (setf (ch-info-signature info) nil))))

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
    (do-lines (line buffer)
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
	(setf (ch-info-signature info) nil)))))

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
      (setf (ch-info-signature info) nil))))

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

(defhvar "Highlight Trailing Space"
  "If true trailing whitespace on lines will be highlighted."
  :value t
  :hooks '(update-trailing-space-highlight))


;;;; Comments.

(defun update-comments-highlight (&optional name kind where value)
  "Update all buffers to match value of \"Highlight Comments\"."
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
  (do-lines (line buffer)
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
			     :end1 ,comment-string-len :end2 ,comment-string-len))
	       (push (font-mark line 0 1)
		     (ch-info-font-marks ,info))))))))

(defun rehighlight-comments (buffer)
  (let ((comment-string (value comment-start)))
    (when comment-string
      (let ((comment-string-len (length comment-string)))
	(do-lines (line buffer)
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
	  (do-lines (line buffer)
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

(defhvar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :value nil
  :hooks '(update-comments-highlight))
