;;; -*- Log: hemlock.log; Package: Hemlock; Mode: Editor -*-
;;;
;;; Highlighting open paren and active region.

(in-package "HEMLOCK")



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
	(multiple-value-bind
	    (start end)
	    (funcall (value open-paren-finder-function)
		     (current-point))
	  (if (and start end)
	      (set-open-paren-font-marks start end)
	      (kill-open-paren-font-marks))))))
;;;
(add-hook redisplay-hook 'maybe-highlight-open-parens)

(defun set-open-paren-font-marks (start end)
  (if *open-paren-font-marks*
      (flet ((maybe-move (dst src)
	       (or (mark= dst src)
		   (move-font-mark dst src))))
	(declare (inline maybe-move))
	(maybe-move (region-start *open-paren-font-marks*) start)
	(maybe-move (region-end *open-paren-font-marks*) end))
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
	  (unless (start-line-p end) (stash-a-mark end 0))))))
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




;;; Lisp context highlighting.

(defvar original-font 0
  "Number of the font to use for normal text.")
(defvar comment-font 1
  "Number of the font to use for comments.")
(defvar string-font 2
  "Number of the font to use for strings.")
(defvar variable-name-font 3
  "Number of the font to use for variable names.")
(defvar special-form-font 6
  "Number of the font to use for special forms.")

(defvar special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of Lisp special form names.")

(defun init-context-highlighting ()
  "Initialize context highlighting."
  (clrhash special-forms)

  ;; FIX lisp mode defines indents for all? special forms

  (setf (gethash "let" special-forms) t)
  (setf (gethash "let*" special-forms) t)
  (setf (gethash "hlet" special-forms) t)

  (setf (gethash "block" special-forms) t)
  (setf (gethash "progn" special-forms) t)
  (setf (gethash "prog" special-forms) t)
  (setf (gethash "prog1" special-forms) t)
  (setf (gethash "prog2" special-forms) t)
  (setf (gethash "prog*" special-forms) t)

  (setf (gethash "return" special-forms) t)
  (setf (gethash "return-from" special-forms) t)
  (setf (gethash "go" special-forms) t)
  (setf (gethash "if" special-forms) t) 
  (setf (gethash "do" special-forms) t)
  (setf (gethash "do*" special-forms) t)
  (setf (gethash "loop" special-forms) t)
  (setf (gethash "when" special-forms) t)
  (setf (gethash "cond" special-forms) t)
  (setf (gethash "unless" special-forms) t)

  (setf (gethash "declare" special-forms) t)
  (setf (gethash "defun" special-forms) t)
  (setf (gethash "defcommand" special-forms) t)
  (setf (gethash "defconst" special-forms) t)
  (setf (gethash "defvar" special-forms) t)
  (setf (gethash "defhvar" special-forms) t)
  (setf (gethash "defsetf" special-forms) t)
  (setf (gethash "defmacro" special-forms) t))

(declaim (inline search-for-qmark))

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

#|
(profile::profile ed::highlight-context ed::delete-line-font-marks ed::search-for-qmark < min search position font-mark)
(profile::unprofile ed::highlight-context ed::delete-line-font-marks ed::search-for-qmark < min search position font-mark)
|#

;; FIX consider integration with lispmode and other modes
(defstruct (chi-info (:constructor make-chi-info (signature)))
  "Per-line context highlighting information."
  (begins-quoted)
  (ending-quoted)
  (begins-commented)
  (ending-commented)
  (font-marks ())
  (signature))

(declaim (inline line-same chi-mark))

(defun line-same (line)
  (let ((font-info (getf (line-plist line) 'chi-info)))
    (if font-info
	(or (eq (line-signature line) (chi-info-signature font-info))
	    (progn (setf (chi-info-signature font-info)
			 (line-signature line))
	      nil))
	(progn
	  (setf (getf (line-plist line) 'chi-info)
		(make-chi-info (line-signature line)))
	  nil))))

(declaim (special *last-used-mark*))

(defun chi-mark (line pos font chi-info)
  (if (cdr *last-used-mark*)
      (prog1
	  (setf (hi::font-mark-font (move-font-mark (cadr *last-used-mark*)
						    (mark line pos)))
		font)
	(setq *last-used-mark* (cdr *last-used-mark*)))
      (push (font-mark line pos font) (chi-info-font-marks chi-info))))

(defmacro context-highlight-line ()
  `(when (next-character (mark line 0))
     (let ((chars (line-string line))
	   (pos 0))
       (if in-string
	   (progn
	     (chi-mark line 0 string-font chi-info)
	     (setq pos (1+ (or (search-for-qmark chars)
			       (return-from do-line))))
	     (chi-mark line pos original-font chi-info)
	     (setq in-string nil))
	   (when in-comment
	     (chi-mark line 0 comment-font chi-info)
	     (setq pos (+ (or (search "|#" chars)
			      (return-from do-line))
			  2))
	     (chi-mark line pos original-font chi-info)
	     (setq in-comment nil)))
       (loop
	 (let ((string (or (search-for-qmark chars pos)
			   most-positive-fixnum))
	       (comment (or (position #\; chars :start pos) 
			    most-positive-fixnum))
	       (multic (or (search "#|" chars :start2 pos)
			   most-positive-fixnum))
	       (oparen (or (position #\( chars :start pos)
			   most-positive-fixnum)))
	   (cond ((< string (min comment multic oparen))
		  (chi-mark line string string-font chi-info)
		  (setq pos (search-for-qmark chars (1+ string)))
		  (if pos
		      (chi-mark line (incf pos) original-font chi-info)
		      (progn
			(setq in-string t)
			(return-from do-line))))

		 ((< comment (min string multic oparen))
		  (chi-mark line comment comment-font chi-info)
		  (return-from do-line))

		 ((< multic (min comment string oparen))
		  (chi-mark line multic comment-font chi-info)
		  (setq pos (search "|#" chars :start2 (+ multic 2)))
		  (if pos
		      (chi-mark line (incf pos 2) original-font chi-info)
		      (progn
			(setq in-comment t)
			(return-from do-line))))

		 ((< oparen (min multic comment string))
		  (let ((mark (mark line (1+ oparen))))
		    (if (eq (next-character mark) #\")
			(setq pos (1+ oparen))
			(progn
			  (setq pos
				(mark-charpos (find-attribute mark
							      :word-delimiter)))
			  (or (memq (next-character mark) '(#\  #\) #\newline))
			      (setq pos (mark-charpos
					 (find-attribute (mark-after mark)
							 :word-delimiter))))
			  ;; This also highlights special form names in lists.
			  (when (gethash (subseq chars (1+ oparen) pos)
					 special-forms)
			    ;; FIX Highlight next word if last was defun, defvar, def...
			    (chi-mark line (1+ oparen) special-form-font chi-info)
			    (chi-mark line pos original-font chi-info))))))

		 (t
		  (return-from do-line))))))))

;; FIX maybe this should use find-pattern, or [a mod of] lispmode parsing
;; FIX can (,;,#|," be found using char attributes?  (,;,"  #| is a string
;; FIX (let (;; comment  (highlight first ;)
;; FIX highlight &optional...
(defun highlight-context (buffer)
  "Simple Lisp syntax highlighting."
  (let ((in-string nil)
	(in-comment nil)
	(line (mark-line (buffer-start-mark buffer))))
    (loop while (and line (line-same line)) do
      (setq line (line-next line)))
    (or line (return-from highlight-context))
    (let ((chi-info (getf (line-plist line) 'chi-info)))
      (setq in-string (chi-info-begins-quoted chi-info))
      (setq in-comment (chi-info-begins-commented chi-info)))
    (loop while line do
      (line-same line) ; To ensure chi-info line property exists.
      (let* ((chi-info (getf (line-plist line) 'chi-info))
	     (*last-used-mark* (cons nil (chi-info-font-marks chi-info))))
	(block do-line
	  (setf (chi-info-begins-quoted chi-info) in-string)
	  (setf (chi-info-begins-commented chi-info) in-comment)
	  (context-highlight-line))
	(setf (chi-info-ending-quoted chi-info) in-string)
	(setf (chi-info-ending-commented chi-info) in-comment)
	(when (cdr *last-used-mark*)
	  (dolist (old-mark (cdr *last-used-mark*))
	    (delete-font-mark old-mark))
	  (if (car *last-used-mark*)
	      (setf (cdr *last-used-mark*) nil)
	      (setf (chi-info-font-marks chi-info) nil))))
      ;; FIX + If the line is the same as it was before then skip to the
      ;; next changed line?
      (setq line (line-next line)))))

(init-context-highlighting)
