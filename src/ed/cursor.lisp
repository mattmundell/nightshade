;;; Cursor positioning and recentering.

(in-package "EDI")

(export '(mark-to-cursorpos center-window displayed-p scroll-window
	  mark-column cursorpos-to-mark move-to-column))

#[ Cursor Positions

A cursor position is an absolute position within a window's coordinate
system.  The origin is in the upper-left-hand corner and the unit is
character positions.

{function:ed:mark-to-cursorpos}
{function:ed:cursorpos-to-mark}
{function:ed:last-key-event-cursorpos}
{function:ed:mark-column}
{function:ed:move-to-column}
{function:ed:show-mark}
]#


;;;; Mark-To-Cursorpos
;;;
;;; Since performance analysis showed that HALF of the time in the editor
;;; was being spent in this function, I threw all of the tricks in the
;;; book at it to try and make it tenser.
;;;
;;; The algorithm is roughly as follows:
;;;
;;;    1) Eliminate the annoying boundry condition of the mark being
;;; off the end of the window, if it is return NIL now.
;;;    2) If the charpos is on or immediately after the last character
;;; in the line, then find the last dis-line on which the line is
;;; displayed.  We know that the mark is at the end of this dis-line
;;; because it is known to be on the screen.  X position is trivially
;;; derived from the dis-line-length.
;;;    3) Call Real-Line-Length or Cached-Real-Line-Length to get the
;;; X position and number of times wrapped.

(proclaim '(special the-sentinel))

(eval-when (compile eval)
;;; find-line
;;;
;;; Find a dis-line which line is displayed on which starts before charpos,
;;; setting ypos and dis-line to the dis-line and it's index.  Offset is
;;; expected to be the mark-charpos of the display-start for the window
;;; initially, and is set to offset within line that Dis-Line begins.
;;; Charpos is the mark-charpos of the mark we want to find.  Check if same
;;; as *redisplay-favorite-line* and then scan if not.
;;;
(defmacro find-line (line offset charpos ypos dis-lines dis-line)
  (declare (ignore charpos))
  `(cond
    ;; No lines at all, fail.
    ((eq ,dis-lines the-sentinel) nil)
    ;; On the first line, offset is already set, so just set dis-line and
    ;; ypos and fall through.
    ((eq (dis-line-line (car ,dis-lines)) ,line)
     (setq ,dis-line ,dis-lines  ,ypos 0))
    ;; Look farther down.
    ((do ((l (cdr ,dis-lines) (cdr l)))
	 ((eq l the-sentinel))
       (when (eq (dis-line-line (car l)) ,line)
	 (setq ,dis-line l  ,ypos (dis-line-position (car l)) ,offset 0)
	 (return t))))
    (t
     (error "Horrible flaming lossage, sorry man."))))

;;; find-last
;;;
;;; Find the last dis-line on which line is displayed, set ypos and
;;; dis-line.
;;;
(defmacro find-last (line ypos dis-line)
  `(do ((trail ,dis-line dl)
	(dl (cdr ,dis-line) (cdr dl)))
       ((not (eq (dis-line-line (car dl)) ,line))
	(setq ,dis-line (car trail)  ,ypos (dis-line-position ,dis-line)))))

;;; find-charpos
;;;
;;; Special-Case mark at end of line, if not punt out to real-line-length
;;; function.  Return the correct values.
;;;
(defmacro find-charpos (line offset charpos length ypos dis-line width
			     fun chars)
  (declare (ignore chars))
  `(cond
    ((= ,charpos ,length)
     (find-last ,line ,ypos ,dis-line)
     (values (min (dis-line-length ,dis-line) (1- ,width)) ,ypos))
    ((= ,charpos (1- ,length))
     (multiple-value-bind (x dy)
			  (,fun ,line (1- ,width) ,offset ,charpos)
       (if (and (not (zerop dy)) (zerop x))
	   (values (1- ,width) (1- (+ ,ypos dy)))
	   (values x (+ ,ypos dy)))))
    (t
     (multiple-value-bind (x dy)
			  (,fun ,line (1- ,width) ,offset ,charpos)
	  (values x (+ ,ypos dy))))))

); eval-when (compile eval)

;;; real-line-length
;;;
;;; Return as values the X position and the number of times wrapped if one FIX
;;; to display the characters from Start to End of Line starting at an X
;;; position of 0 wrapping Width wide.  %SP-Find-Character-With-Attribute
;;; is used to find charaters with funny representation much as in
;;; Compute-Line-Image.
;;;
;;; TODO If the character immediately before start translates to multiple
;;; characters and has wrapped midway through the characters, and there is
;;; a tab within tab-width characters of start, then the length will be too
;;; long (by however many characters wrapped).  This was causing an error
;;; in hunk-put-string and others (hunk-draw.lisp), which now watch for it.
;;; To rectify, the preceding characters need to be checked (keeping the
;;; line width in mind).  In the worst case (in every case?) every
;;; character in the line would be checked, which is bad if the line is
;;; very long.  Tabs are a pain in the ass?
;;;
(defun real-line-length (line width start end)
  (declare (fixnum width start end))
  (do ((xpos 0)
       (ypos 0)
       (chars (line-chars line))
       (losing 0)
       (dy 0))
      ((= start end) (values xpos ypos))
    (declare (fixnum xpos ypos dy) (simple-string chars)
	     (type (or fixnum null) losing))
    (setq losing (%fcwa chars start end losing-char))
    (when (null losing)
      (multiple-value-setq (dy xpos) (truncate (+ xpos (- end start)) width))
      (return (values xpos (+ ypos dy))))
    (multiple-value-setq (dy xpos) (truncate (+ xpos (- losing start)) width))
    (setq ypos (+ ypos dy)  start losing)
    (do ((last (or (%fcwa chars start end winning-char) end)) str)
	((= start last))
      (declare (fixnum last))
      (setq str (get-rep (schar chars start)))
      (incf start)
      (or (simple-string-p str) (setq str (funcall str xpos line)))
      (multiple-value-setq (dy xpos) (truncate (+ xpos (strlen str)) width))
      (setq ypos (+ ypos dy)))))

;;; cached-real-line-length
;;;
;;; The same as Real-Line-Length, except does it for the cached line.  The
;;; line argument is ignored, but present to make the arglists the same.
;;;
;;; TODO Same issue as cached-real-line-length.
;;;
(defun cached-real-line-length (line width start end)
  (declare (fixnum width start end))
  (let ((offset (- right-open-pos left-open-pos))
	(bound 0))
    (declare (fixnum offset bound))
    (cond
     ((>= start left-open-pos)
      (setq start (+ start offset)  bound (setq end (+ end offset))))
     ((> end left-open-pos)
      (setq bound left-open-pos  end (+ end offset)))
     (t
      (setq bound end)))

    (do ((xpos 0)
	 (ypos 0)
	 (losing 0)
	 (dy 0))
	(())
      (declare (fixnum xpos ypos dy)
	       (type (or fixnum null) losing))
      (when (= start bound)
	(if (= start end) (return (values xpos ypos)))
	(setq start right-open-pos  bound end))
      (setq losing (%fcwa open-chars start bound losing-char))
      (cond
       (losing
	(multiple-value-setq (dy xpos)
	  (truncate (+ xpos (- losing start)) width))
	(setq ypos (+ ypos dy)  start losing)
	(do ((last (or (%fcwa open-chars start bound winning-char) bound)) str)
	    ((= start last))
	  (declare (fixnum last))
	  (setq str (get-rep (schar open-chars start)))
	  (incf start)
	  (or (simple-string-p str) (setq str (funcall str xpos line)))
	  (multiple-value-setq (dy xpos)
	    (truncate (+ xpos (strlen str)) width))
	  (setq ypos (+ ypos dy))))
       (t
	(multiple-value-setq (dy xpos)
	  (truncate (+ xpos (- bound start)) width))
	(setq ypos (+ ypos dy)  start bound))))))

;;; mark-to-cursorpos  --  Public
;;;
(defun mark-to-cursorpos (mark window)
  "Return as multiple values the X and Y position on which $mark is being
   displayed in $window.  Return () if $mark is out of view on the window."
  (maybe-update-window-image window)
  (let* ((line (mark-line mark))
	 (number (line-number line))
	 (charpos (mark-charpos mark))
	 (dis-lines (cdr (window-first-line window)))
	 (width (window-width window))
	 (start (window-display-start window))
	 (offset (mark-charpos start))
	 (start-number (line-number (mark-line start)))
	 (end (window-display-end window))
	 (end-number (line-number (mark-line end)))
	 (ypos 0)
	 dis-line)
    (declare (fixnum width charpos ypos number end-number))
    (cond
     ((or (< number start-number)
	  (and (= number start-number) (< charpos offset))
	  (> number end-number)
	  (and (= number end-number) (> charpos (mark-charpos end)))) nil)
     (t
      (find-line line offset charpos ypos dis-lines dis-line)
      (cond
       ((eq line open-line)
	(let ((len (- line-cache-length (- right-open-pos left-open-pos))))
	  (declare (fixnum len))
	  (find-charpos line offset charpos len ypos dis-line width
			cached-real-line-length open-chars)))
       (t
	(let* ((chars (line-chars line))
	       (len (strlen chars)))
	  (declare (fixnum len) (simple-string chars))
	  (find-charpos line offset charpos len ypos dis-line width
			real-line-length chars))))))))

;;; Dis-Line-Offset-Guess  --  Internal
;;;
;;; Move Mark by Offset display lines.  The mark is assumed to be at the
;;; beginning of a display line, and we attempt to leave it at one.  We
;;; assume all characters print one wide.  Width is the width of the window
;;; we are displaying in.
;;;
(defun dis-line-offset-guess (mark offset width)
  (let ((w (1- width)))
    (if (minusp offset)
	(dotimes (i (- offset) t)
	  (let ((pos (mark-charpos mark)))
	    (if (>= pos w)
		(character-offset mark (- w))
		(let ((prev (line-previous (mark-line mark))))
		  (or prev (return nil))
		  (multiple-value-bind
		      (lines chars)
		      (truncate (line-length prev) w)
		    (move-to-position mark
				      (cond ((zerop lines) 0)
					    ((< chars 2)
					     (* w (1- lines)))
					    (t
					     (* w lines)))
				      prev))))))
	(dotimes (i offset t)
	  (let ((left (- (line-length (mark-line mark))
			 (mark-charpos mark))))
	    (if (> left width)
		(character-offset mark w)
		(or (line-offset mark 1 0)
		    (return nil))))))))

;;; maybe-recenter-window  --  Internal
;;;
(defun maybe-recenter-window (window)
  "Update the dis-lines for WINDOW and recenter if the point is off the
   screen."
  (unless (%displayed-p (buffer-point (window-buffer window)) window)
    (center-window window (buffer-point (window-buffer window)))
    t))

;;; center-window  --  Public
;;;
(defun center-window (window mark)
  "Attempt to adjust $window's display start so the that $mark is
   vertically centered within $window."
  (let ((height (window-height window))
	(start (window-display-start window)))
    (move-mark start mark)
    (or (dis-line-offset-guess start (- (truncate height 2))
			       (window-width window))
	(move-mark start (buffer-start-mark (window-buffer window))))
    (update-window-image window)
    ;; If that doesn't work, panic and make the start the point.
    (unless (%displayed-p mark window)
      (move-mark start mark)
      (update-window-image window))))

;;; %Displayed-P  --  Internal
;;;
;;; If Mark is within the displayed bounds in Window, then return true,
;;; otherwise false.  We assume the window image is up to date.
;;;
(defun %displayed-p (mark window)
  (let ((start (window-display-start window))
	(end (window-display-end window)))
    (not (or (mark< mark start) (mark> mark end)
	     (if (mark= mark end)
		 (let ((ch (next-character end)))
		   (and ch (char/= ch #\newline)))
		 nil)))))

;;; Displayed-p  --  Public
;;;
;;; Update the window image and then check if the mark is displayed.
;;;
(defun displayed-p (mark window)
  "Return true if either the character before or the character after $mark
   is being displayed in $window, else return ()."
  (maybe-update-window-image window)
  (%displayed-p mark window))

;;; scroll-window  --  Public
;;;
;;; This is not really right, since it uses dis-line-offset-guess.
;;; Probably if there is any screen overlap then we figure it out exactly.
;;;
(defun scroll-window (window n)
  "Scroll $window down $n display lines; if $n is negative scroll up.
   Leave the cursor at the same text position.  If the cursor moves off the
   screen move the cursor to the end of the window closest to its old
   position."
  (let* ((start (window-display-start window))
	 (buffer (line-buffer (mark-line start))))
    (cond ((and (minusp n)
		(mark= (buffer-start-mark buffer) start))
	   (buffer-start (buffer-point buffer)))
	  ((and (plusp n)
		(mark= (window-display-end (current-window))
		       (buffer-end-mark (current-buffer))))
	   (buffer-end (buffer-point buffer)))
	  (t
	   (let ((point (window-point window))
		 (width (window-width window))
		 (height (window-height window)))
	     (cond ((dis-line-offset-guess start n width))
		   ((minusp n)
		    (buffer-start start))
		   (t
		    (buffer-end start)
		    (let ((fraction (- (truncate height 3) height)))
		      (dis-line-offset-guess start fraction width))))
	     (update-window-image window)
	     (let ((iscurrent (eq window *current-window*))
		   (bpoint (buffer-point (window-buffer window))))
	       (when iscurrent (move-mark point bpoint))
	       (unless (%displayed-p point window)
		 (move-mark point start)
		 (dis-line-offset-guess point (truncate (window-height window) 2)
					width)
		 (when iscurrent (move-mark bpoint point))))))))
  t)


;;; Mark-Column  --  Public
;;;
(defun mark-column (mark)
  "Return the X position at which $mark would be displayed, supposing its
   line was displayed on an single screen line.  Take into consideration
   tabs and control characters."
  (let ((charpos (mark-charpos mark))
	(line (mark-line mark)))
    (if (eq line open-line)
	(values (cached-real-line-length line 10000 0 charpos))
	(values (real-line-length line 10000 0 charpos)))))

;;; Find-Position  --  Internal
;;;
;;; Return the charpos which corresponds to the specified X position within
;;; Line.  If there is no such position between Start and End then return
;;; NIL.
;;;
(defun find-position (line position start end width)
  (do* ((cached (eq line open-line))
	(lo start)
	(hi (1- end))
	(probe (truncate (+ lo hi) 2) (truncate (+ lo hi) 2)))
       ((> lo hi)
	(if (= lo end) nil hi))
    (let ((val (if cached
		   (cached-real-line-length line width start probe)
		   (real-line-length line width start probe))))
      (cond ((= val position) (return probe))
	    ((< val position) (setq lo (1+ probe)))
	    (t (setq hi (1- probe)))))))

;;; Cursorpos-To-Mark  --  Public
;;;
;;; Find the right dis-line, then zero in on the correct position using
;;; real-line-length.
;;;
(defun cursorpos-to-mark (x y window)
  "Return as a mark the text position which corresponds to the ($x, $y)
   position within $window, or () if that position is out of view on
   window."
  (check-type window window)
  (let ((width (window-width window))
	(first (window-first-line window)))
    (when (>= x width)
      (return-from cursorpos-to-mark nil))
    (do* ((prev first dl)
	  (dl (cdr first) (cdr dl))
	  (ppos (mark-charpos (window-display-start window))
		(if (eq (dis-line-line (car dl)) (dis-line-line (car prev)))
		    (dis-line-end (car prev)) 0)))
	((eq dl the-sentinel)
	 (copy-mark (window-display-end window) :temporary))
      (when (= (dis-line-position (car dl)) y)
	(let* ((line (dis-line-line (car dl)))
	       (end (dis-line-end (car dl))))
	  (return (mark line (or (find-position line x ppos end width) end))))))))

;;; Move-To-Column  --  Public
;;;
;;; Just look up the charpos using find-position...
;;;
(defun move-to-column (mark column &optional (line (mark-line mark)))
  "Move $mark to the position on $line which corresponds to $column.
   Analogous to `move-to-position'.

   If the line would is shorter than the specified column then simply
   return ().  Since a character may be displayed on more than one column
   on the screen, several values of column may cause mark to be moved to
   the same position."
  (let ((res (find-position line column 0 (line-length line) 10000)))
    (if res
	(move-to-position mark res line))))
