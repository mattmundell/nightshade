;;; Screen painting routines for the IBM RT running X.

(in-package "EDI")

(defparameter hunk-height-limit 80 "Maximum possible height for any hunk.")
(defparameter hunk-width-limit 200 "Maximum possible width for any hunk.")
(defparameter hunk-top-border 2 "Clear area at beginning.")
(defparameter hunk-left-border 1 "Clear area before first character.")
(defparameter hunk-bottom-border 3 "Minimum Clear area at end.")
(defparameter hunk-thumb-bar-bottom-border 10
  "Minimum Clear area at end including room for thumb bar." )
(defparameter hunk-modeline-top 2
  "Extra background pixels above modeline chars.")
(defparameter hunk-modeline-bottom 2
  "Extra background pixels below modeline chars.")


;;;; Character translations for CLX.

;;; EDITOR-TRANSLATE-DEFAULT.
;;;
;;; CLX glyph drawing routines allow for a character translation function.  The
;;; default one takes a string (any kind) or a vector of numbers and slams them
;;; into the outgoing request buffer.  When the argument is a string, it stops
;;; processing if it sees a character that is not GRAPHIC-CHAR-P.  For each
;;; graphical character, the function ultimately calls CHAR-CODE.
;;;
;;; The editor only passes simple-strings in, and these can only contain
;;; graphical characters because of the line image builder, except for one
;;; case -- *line-wrap-char* which anyone can set.  Those who want to do
;;; evil things with this should know what they are doing: if they want a
;;; funny glyph as a line wrap char, then they should use CODE-CHAR on the
;;; font index.  This allows the following function to translate everything
;;; with CHAR-CODE, and everybody's happy.
;;;
;;; Actually, the editor can passes the line string when doing
;;; random-typeout which does contain ^L's, tabs, etc.  Under X10 these
;;; came out as funny glyphs, and under X11 the output is aborted without
;;; this function.
;;;
(defun editor-translate-default (src src-start src-end font dst dst-start)
  (declare (simple-string src)
	   (fixnum src-start src-end dst-start)
	   (vector dst)
	   (ignore font))
  (do ((i src-start (1+ i))
       (j dst-start (1+ j)))
      ((>= i src-end) i)
    (declare (fixnum i j))
    (setf (aref dst j) (char-code (schar src i)))))

(defvar *glyph-translate-function* #'xlib:translate-default)


;;;; Drawing a line.

(eval-when (compile eval)

;;; HUNK-PUT-STRING takes a character (x,y) pair and computes at which
;;; pixel coordinate to draw string with font from start to end.  This
;;; macro assumes hunk, font-family and color-map to be bound by the caller.
;;;
(defmacro hunk-put-string (x y font fore-color back-color
			     string start end)
  (let ((gcontext (gensym)))
    `(let ((,gcontext (bitmap-hunk-gcontext hunk)))
#|
       (format t "(hunk-put-string ~A ~A . ~A ~A ~A ~A ~A)~%"
	       ,x ,y ,fore-color ,back-color ,string ,start ,end)
|#
       (xlib:with-gcontext (,gcontext :font ,font
				      :function xlib:x-boole-copy)
	 (if ,fore-color
	     (setf (xlib:gcontext-foreground ,gcontext) ,fore-color))
	 (if ,back-color
	     (setf (xlib:gcontext-background ,gcontext) ,back-color))
	 (xlib:draw-image-glyphs
	  (bitmap-hunk-xwindow hunk) ,gcontext
	  (+ hunk-left-border (* ,x (font-family-width font-family)))
	  (+ hunk-top-border (* ,y (font-family-height font-family))
	     (font-family-baseline font-family))
	  ,string :start ,start :end ,end
	  :translate *glyph-translate-function*)))))

); eval-when (compile eval)

;;; Hunk-Write-String  --  Internal
;;;
;;; A historical vestige used by bitmap hunk streams.  Use default font (0)
;;; and color, and bind font-family and color-map for HUNK-PUT-STRING.
;;;
(defun hunk-write-string (hunk x y string start end)
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (font (svref (font-family-map font-family) 0)))
    (hunk-put-string x y font :window-foreground :window-background
		     string start end)))

;;; Hunk-Write-Line  --  Internal
;;;
;;; Paint a dis-line on a hunk, taking font-changes into consideration.
;;; The area of the hunk drawn on is assumed to be cleared.  If supplied,
;;; the line is written at Position, and the position in the dis-line is
;;; ignored.
;;;
(defun hunk-write-line (hunk dl &optional
			     (position (dis-line-position dl)))
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (font-map (font-family-map font-family))
	 (device (bitmap-hunk-device hunk))
	 (chars (dis-line-chars dl))
	 (length (dis-line-length dl)))
    (let ((last 0)
	  (last-fore-color)
	  (last-back-color)
	  (last-font (svref font-map 0)))
      (do ((change (dis-line-font-changes dl)
		   (font-change-next change)))
	  ((null change)
	   (hunk-put-string last position last-font
			    last-fore-color last-back-color
			    chars last length))
	(let ((x (font-change-x change)))
	  (hunk-put-string last position last-font
			   last-fore-color last-back-color
			   chars last x)
	  (setq last x
		last-font (svref font-map (font-change-font change))
		last-fore-color (let ((color (font-change-fore-color change)))
				  (case color
				    (:window-foreground)
				    ((()))
				    (t (device-color device color))))
		last-back-color (let ((color (font-change-back-color change)))
				  (case color
				    (:window-background)
				    ((()))
				    (t (device-color device color))))))))))

;;; We hack this since the X11 servers aren't clever about
;;; DRAW-IMAGE-GLYPHS; that is, they literally clear the line, and then
;;; blast the new glyphs.  Hacking replace line on the color Megapel
;;; display is SLOW!
;;;
(defvar *hack-hunk-replace-line* t)

;;; Hunk-Replace-Line  --  Internal
;;;
;;; Similar to Hunk-Write-Line, but the line need not be clear.
;;;
(defun hunk-replace-line (hunk dl &optional
			       (position (dis-line-position dl)))
  (if *hack-hunk-replace-line*
      (hunk-replace-line-on-a-pixmap hunk dl position)
      (old-hunk-replace-line hunk dl position)))

(defun old-hunk-replace-line (hunk dl &optional
				   (position (dis-line-position dl)))
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (font-map (font-family-map font-family))
	 (device (bitmap-hunk-device hunk))
	 (chars (dis-line-chars dl))
	 (length (dis-line-length dl))
	 (height (font-family-height font-family)))
    (let ((last 0)
	  (last-fore-color)
	  (last-back-color)
	  (last-font (svref font-map 0)))
      (do ((change (dis-line-font-changes dl) (font-change-next change)))
	  ((null change)
	   (hunk-put-string last position last-font
			    last-fore-color last-back-color
			    chars last length)
	   (let ((dx (+ hunk-left-border
			(* (font-family-width font-family) length))))
	     (xlib:clear-area (bitmap-hunk-xwindow hunk)
			      :x dx
			      :y (+ hunk-top-border (* position height))
			      :width (- (bitmap-hunk-width hunk) dx)
			      :height height)))
	(let ((x (font-change-x change)))
	  (hunk-put-string last position last-font
			   last-fore-color last-back-color
			   chars last x)
	  (setq last x
		last-font (svref font-map (font-change-font change))
		last-fore-color (let ((color (font-change-fore-color change)))
				  (case color
				    (:window-foreground)
				    ((()))
				    (t (device-color device color))))
		last-back-color (let ((color (font-change-back-color change)))
				  (case color
				    (:window-background)
				    ((()))
				    (t (device-color device color))))))))))

(defvar *hunk-replace-line-pixmap* nil)

(defun hunk-replace-line-pixmap ()
  (if *hunk-replace-line-pixmap*
      *hunk-replace-line-pixmap*
      (let* ((hunk (window-hunk *current-window*))
	     (gcontext (bitmap-hunk-gcontext hunk))
	     (screen (xlib:display-default-screen
		      (bitmap-device-display (device-hunk-device hunk))))
	     (height (font-family-height *default-font-family*))
	     (pixmap (xlib:create-pixmap
		     :width (* hunk-width-limit
			       (font-family-width *default-font-family*))
		     :height height :depth (xlib:screen-root-depth screen)
		     :drawable (xlib:screen-root screen))))
	(xlib:with-gcontext
	    (gcontext :function xlib:x-boole-1
		      :foreground
		      (color-pixel
		       (bitmap-device-display
			(bitmap-hunk-device hunk))
		       (or (value ed::initial-background-color)
			   '(1 1 1))))
	  (xlib:draw-rectangle pixmap gcontext 0 0 hunk-left-border height t))
	(setf *hunk-replace-line-pixmap* pixmap))))

(eval-when (compile eval)

;;; HUNK-REPLACE-LINE-STRING takes a character (x,y) pair and computes at
;;; which pixel coordinate to draw string with font from start to end.
;;; This macros assumes hunk, font-family and color-map to be bound by the
;;; caller.  Draw the text on a pixmap and later blast it out to overcome
;;; line flicker, for example, if the server clears the entire line before
;;; drawing text.
;;;
(defmacro hunk-replace-line-string (x y font fore-color back-color
				      string start end)
  (declare (ignore y))
  `(xlib:with-gcontext (gcontext :font ,font)
     (if ,fore-color
	 (setf (xlib:gcontext-foreground gcontext) ,fore-color))
     (if ,back-color
	 (setf (xlib:gcontext-background gcontext) ,back-color))
     (xlib:draw-image-glyphs
      (hunk-replace-line-pixmap) gcontext
      (+ hunk-left-border (* ,x (font-family-width font-family)))
      (font-family-baseline font-family)
      ,string :start ,start :end ,end
      :translate *glyph-translate-function*)))
) ;eval-when

(defun hunk-replace-line-on-a-pixmap (hunk dl position)
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (font-map (font-family-map font-family))
	 (device (bitmap-hunk-device hunk))
	 (chars (dis-line-chars dl))
	 (length (dis-line-length dl))
	 (height (font-family-height font-family))
	 (last 0)
	 (last-fore-color)
	 (last-back-color)
	 (last-font (svref font-map 0))
	 (gcontext (bitmap-hunk-gcontext hunk)))
    (do ((change (dis-line-font-changes dl) (font-change-next change)))
	((null change)
	 (hunk-replace-line-string last position last-font
				   last-fore-color last-back-color
				   chars last length)
	 (let* ((dx (+ hunk-left-border
		       (* (font-family-width font-family) length)))
		(dy (+ hunk-top-border (* position height)))
		(xwin (bitmap-hunk-xwindow hunk)))
	   (xlib:with-gcontext (gcontext :exposures nil)
	     (xlib:copy-area (hunk-replace-line-pixmap) gcontext
			     0 0 dx height xwin 0 dy))
	   (xlib:clear-area xwin :x dx :y dy
			    :width (- (bitmap-hunk-width hunk) dx)
			    :height height)))
      (let ((x (font-change-x change)))
	(hunk-replace-line-string last position last-font
				  last-fore-color last-back-color
				  chars last x)
	(setq last x
	      last-font (svref font-map (font-change-font change))
	      last-fore-color (let ((color (font-change-fore-color change)))
				(case color
				  (:window-foreground)
				  ((()))
				  (t (device-color device color))))
	      last-back-color (let ((color (font-change-back-color change)))
				(case color
				  (:window-background)
				  ((()))
				  (t (device-color device color)))))))))

(defvar *hunk-replace-modeline-pixmap* nil)

(defun hunk-replace-modeline-pixmap ()
  (if *hunk-replace-modeline-pixmap*
      *hunk-replace-modeline-pixmap*
      (let* ((hunk (window-hunk *current-window*))
	     (gcontext (bitmap-hunk-gcontext hunk))
	     (screen (xlib:display-default-screen
		      (bitmap-device-display (device-hunk-device hunk))))
	     (height (+ hunk-modeline-top
			(font-family-height *default-font-family*)
			hunk-modeline-bottom))
	     (pixmap (xlib:create-pixmap
		     :width (* hunk-width-limit
			       (font-family-width *default-font-family*))
		     :height height :depth (xlib:screen-root-depth screen)
		     :drawable (xlib:screen-root screen))))
	(xlib:with-gcontext
	    (gcontext :function xlib:x-boole-1
		      :foreground
		      (color-pixel
		       (bitmap-device-display
			(bitmap-hunk-device hunk))
		       (or (value ed::initial-background-color)
			   '(1 1 1))))
	  (xlib:draw-rectangle pixmap gcontext 0 0 hunk-left-border height t))
	(setf *hunk-replace-modeline-pixmap* pixmap))))

;;; HUNK-REPLACE-MODELINE
;;;
(defun hunk-replace-modeline (hunk)
  (if *hack-hunk-replace-line*
      (hunk-replace-modeline-on-a-pixmap hunk)
      (old-hunk-replace-modeline hunk)))

;;; OLD-HUNK-REPLACE-MODELINE sets the entire mode line to the the
;;; foreground color, so the initial bits where no characters go also is
;;; highlighted.  Then the text is drawn background on foreground
;;; (hightlighted).  This function assumes that BITMAP-HUNK-MODELINE-POS
;;; will not return nil; that is, there is a modeline.  This function
;;; should assume the gcontext's font is the default font of the hunk.  We
;;; must LET bind the foreground and background values before entering
;;; XLIB:WITH-GCONTEXT due to a non-obvious or incorrect implementation.
;;;
(defun old-hunk-replace-modeline (hunk)
  (let* ((dl (bitmap-hunk-modeline-dis-line hunk))
	 (font-family (bitmap-hunk-font-family hunk))
	 (default-font (svref (font-family-map font-family) 0))
	 (modeline-pos (bitmap-hunk-modeline-pos hunk))
	 (xwindow (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (window (device-hunk-window hunk))
	 (device (device-hunk-device hunk))
	 (fore-color (device-color device
				   (window-modeline-fore-color window)))
	 (back-color (device-color device
				   (window-modeline-back-color window))))
    (xlib:with-gcontext (gcontext :foreground back-color)
      (xlib:draw-rectangle xwindow gcontext 0 modeline-pos
			   (bitmap-hunk-width hunk)
			   (+ hunk-modeline-top hunk-modeline-bottom
			      (font-family-height font-family))
			   t))
    (xlib:with-gcontext (gcontext :foreground fore-color
				  :background back-color
				  :font default-font)
      (xlib:draw-image-glyphs xwindow gcontext hunk-left-border
			      (+ modeline-pos hunk-modeline-top
				 (font-family-baseline font-family))
			      (dis-line-chars dl)
			      :end (dis-line-length dl)
			      :translate *glyph-translate-function*))))

#|
(defun old-hunk-replace-modeline (hunk)
  (let* ((dl (bitmap-hunk-modeline-dis-line hunk))
	 (font-family (bitmap-hunk-font-family hunk))
	 (default-font (svref (font-family-map font-family) 0))
	 (modeline-pos (bitmap-hunk-modeline-pos hunk))
	 (xwindow (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (window (device-hunk-window hunk))
	 (device (device-hunk-device hunk))
	 (fore-color (device-color device
				   (window-modeline-fore-color window)))
	 (back-color (device-color device
				   (window-modeline-back-color window))))
    (xlib:with-gcontext (gcontext :foreground back-color)
      (xlib:draw-rectangle xwindow gcontext 0 modeline-pos
			   (bitmap-hunk-width hunk)
			   (+ hunk-modeline-top hunk-modeline-bottom
			      (font-family-height font-family))
			   t))
    (xlib:with-gcontext (gcontext :foreground fore-color
				  :background back-color
				  :font default-font)
      (xlib:draw-image-glyphs xwindow gcontext hunk-left-border
			      (+ modeline-pos hunk-modeline-top
				 (font-family-baseline font-family))
			      (dis-line-chars dl)
			      :end (dis-line-length dl)
			      :translate *glyph-translate-function*))))
|#

(defun hunk-replace-modeline-on-a-pixmap (hunk)
  (let* ((dl (bitmap-hunk-modeline-dis-line hunk))
	 (font-family (bitmap-hunk-font-family hunk))
	 (default-font (svref (font-family-map font-family) 0))
	 (modeline-pos (bitmap-hunk-modeline-pos hunk))
	 (xwindow (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (height (+ hunk-modeline-top hunk-modeline-bottom
		    (font-family-height font-family)))
	 (length (dis-line-length dl))
	 (pixmap (hunk-replace-modeline-pixmap))
	 (window (device-hunk-window hunk))
	 (device (device-hunk-device hunk))
	 (fore-color (device-color device
				   (window-modeline-fore-color window)))
	 (back-color (device-color device
				   (window-modeline-back-color window))))
    (xlib:with-gcontext (gcontext :foreground back-color)
      (xlib:draw-rectangle pixmap
			   gcontext 0 0
			   (bitmap-hunk-width hunk)
			   height
			   t))
    (xlib:with-gcontext (gcontext :foreground fore-color
				  :background back-color
				  :font default-font)
      (xlib:draw-image-glyphs pixmap
			      gcontext hunk-left-border
			      (+ hunk-modeline-top
				 (font-family-baseline font-family))
			      (dis-line-chars dl)
			      :end length
			      :translate *glyph-translate-function*))
    (let* ((dx (+ hunk-left-border
		  (* (font-family-width font-family) length)))
	   (dy modeline-pos))
      (xlib:with-gcontext (gcontext :exposures nil)
	(xlib:copy-area pixmap gcontext
			0 0 dx height xwindow 0 dy))
      (xlib:with-gcontext (gcontext :foreground back-color)
	(xlib:draw-rectangle xwindow
			     gcontext dx dy
			     (- (bitmap-hunk-width hunk) dx)
			     height
			     t)))))


;;;; Cursor/Border color manipulation.

;;; *editor-listener* is set to t by default because we can't know from X
;;; whether we come up with the pointer in our window.  There is no initial
;;; :enter-window event.  Defaulting this to nil causes the cursor to be
;;; hollow when the window comes up under the mouse, and you have to know
;;; how to fix it.  Defaulting it to t causes the cursor to always come up
;;; full, as if the editor is the X listener, but this recovers naturally
;;; as you move into the window.  This also coincides with the editor's
;;; border coming up highlighted, even when the editor is not the listener.
;;;
(defvar *editor-listener* t
  "Highlight border when the cursor is dropped and the editor can receive
   input.")
(defvar *current-highlighted-border* nil
  "When true, the bitmap-hunk with the highlighted border.")

(defvar *hunk-cursor-x* 0 "The current cursor X position in pixels.")
(defvar *hunk-cursor-y* 0 "The current cursor Y position in pixels.")
(defvar *cursor-hunk* nil "Hunk the cursor is displayed on.")
(defvar *cursor-dropped* nil) ; True if the cursor is currently displayed.

;;; HUNK-SHOW-CURSOR locates the cursor at character position (x,y) in hunk.
;;; If the cursor is currently displayed somewhere, then lift it, and display
;;; it at its new location.
;;;
(defun hunk-show-cursor (hunk x y)
  (unless (and (= x *hunk-cursor-x*)
	       (= y *hunk-cursor-y*)
	       (eq hunk *cursor-hunk*))
    (let ((cursor-down *cursor-dropped*))
      (when cursor-down
	(format t "hunk-show-cursor lift~%")
	(lift-cursor))
      (setf *hunk-cursor-x* x)
      (setf *hunk-cursor-y* y)
      (setf *cursor-hunk* hunk)
      (when cursor-down
	(format t "hunk-show-cursor drop~%")
	(drop-cursor)))))

;;; FROB-CURSOR is the note-read-wait method for bitmap redisplay.  We
;;; show a cursor and highlight the listening window's border when waiting
;;; for input.
;;;
(defun frob-cursor (on)
  (if on (drop-cursor) (lift-cursor)))

(proclaim '(special *default-border-pixmap* *highlight-border-pixmap*))

;;; DROP-CURSOR and LIFT-CURSOR are separate functions from FROB-CURSOR
;;; because they are called in a couple of places (e.g.,
;;; HUNK-EXPOSED-REGION and SMART-WINDOW-REDISPLAY).  When the cursor is
;;; being dropped, since this means the editor is listening in the
;;; *cursor-hunk*, make sure the border of the window is highlighted as
;;; well.
;;;
(declaim (special *lifted*))
(defun drop-cursor ()
  (unless *cursor-dropped*
    (or *editor-listener* (cursor-invert-center))
    (cursor-invert)
    (when *editor-listener*
      (cond (*current-highlighted-border*
	     (unless (eq *current-highlighted-border* *cursor-hunk*)
	       (setf (xlib:window-border
		      (window-group-xparent
		       (bitmap-hunk-window-group *current-highlighted-border*)))
		     *default-border-pixmap*)
	       (setf (xlib:window-border
		      (window-group-xparent
		       (bitmap-hunk-window-group *cursor-hunk*)))
		     *highlight-border-pixmap*)
	       ;; For complete gratuitous pseudo-generality, should force
	       ;; output on *current-highlighted-border* device too.
	       (xlib:display-force-output
		(bitmap-device-display (device-hunk-device *cursor-hunk*)))))
	    (t (setf (xlib:window-border
		      (window-group-xparent
		       (bitmap-hunk-window-group *cursor-hunk*)))
		     *highlight-border-pixmap*)
	       (xlib:display-force-output
		(bitmap-device-display (device-hunk-device *cursor-hunk*)))))
      (setf *current-highlighted-border* *cursor-hunk*))
    (setq *lifted* ())
    (setq *cursor-dropped* t)))

;;;
(declaim (special *lifted*))
(defun lift-cursor ()
  (when *cursor-dropped*
    (setq *lifted* t)
    (or *editor-listener* (cursor-invert-center))
    (cursor-invert)
    (setq *cursor-dropped* nil)))

(defun cursor-invert-center ()
  (let ((family (bitmap-hunk-font-family *cursor-hunk*))
	(gcontext (bitmap-hunk-gcontext *cursor-hunk*)))
    (xlib:with-gcontext (gcontext :function xlib:x-boole-invert)
      (xlib:draw-rectangle (bitmap-hunk-xwindow *cursor-hunk*)
			   gcontext
			   (+ hunk-left-border
			      (* *hunk-cursor-x* (font-family-width family))
			      (font-family-cursor-x-offset family)
			      1)
			   (+ hunk-top-border
			      (* *hunk-cursor-y* (font-family-height family))
			      (font-family-cursor-y-offset family)
			      1)
			   (- (font-family-cursor-width family) 2)
			   (- (font-family-cursor-height family) 2)
			   t)))
  (xlib:display-force-output
   (bitmap-device-display (device-hunk-device *cursor-hunk*))))

(defun cursor-invert ()
  (let ((family (bitmap-hunk-font-family *cursor-hunk*))
	(gcontext (bitmap-hunk-gcontext *cursor-hunk*)))
    (xlib:with-gcontext (gcontext :function xlib:x-boole-invert)
      (xlib:draw-rectangle (bitmap-hunk-xwindow *cursor-hunk*)
			   gcontext
			   (+ hunk-left-border
			      (* *hunk-cursor-x* (font-family-width family))
			      (font-family-cursor-x-offset family))
			   (+ hunk-top-border
			      (* *hunk-cursor-y* (font-family-height family))
			      (font-family-cursor-y-offset family))
			   (font-family-cursor-width family)
			   (font-family-cursor-height family)
			   t)))
  (xlib:display-force-output
   (bitmap-device-display (device-hunk-device *cursor-hunk*))))


;;;; Clearing and Copying Lines.

(defun hunk-clear-lines (hunk start count)
  (let ((height (font-family-height (bitmap-hunk-font-family hunk))))
    (xlib:clear-area (bitmap-hunk-xwindow hunk)
		     :x 0 :y (+ hunk-top-border (* start height))
		     :width (bitmap-hunk-width hunk)
		     :height (* count height))))

(defun hunk-copy-lines (hunk src dst count)
  (let ((height (font-family-height (bitmap-hunk-font-family hunk)))
	(xwindow (bitmap-hunk-xwindow hunk)))
    (xlib:copy-area xwindow (bitmap-hunk-gcontext hunk)
		    0 (+ hunk-top-border (* src height))
		    (bitmap-hunk-width hunk) (* height count)
		    xwindow 0 (+ hunk-top-border (* dst height)))))


;;;; Drawing bottom border meter.

;;; HUNK-DRAW-BOTTOM-BORDER assumes eight-character-space tabs.  The LOGAND
;;; calls in the loop are testing for no remainder when dividing by 8, 4,
;;; and other.  This lets us quickly draw longer notches at tab stops and
;;; half way in between.  This function assumes that
;;; BITMAP-HUNK-MODELINE-POS will not return nil; that is, that there is a
;;; modeline.
;;;
(defun hunk-draw-bottom-border (hunk)
  (when (bitmap-hunk-thumb-bar-p hunk)
    (let* ((xwindow (bitmap-hunk-xwindow hunk))
	   (gcontext (bitmap-hunk-gcontext hunk))
	   (modeline-pos (bitmap-hunk-modeline-pos hunk))
	   (font-family (bitmap-hunk-font-family hunk))
	   (font-width (font-family-width font-family)))
      (xlib:clear-area xwindow :x 0 :y (- modeline-pos
					  hunk-thumb-bar-bottom-border)
		       :width (bitmap-hunk-width hunk)
		       :height hunk-bottom-border)
      (let ((x (+ hunk-left-border (ash font-width -1)))
	    (y7 (- modeline-pos 7))
	    (y5 (- modeline-pos 5))
	    (y3 (- modeline-pos 3)))
	(dotimes (i (bitmap-hunk-char-width hunk))
	  (cond ((zerop (logand i 7))
		 (xlib:draw-rectangle xwindow gcontext
				      x y7 (if (= i 80) 2 1) 7 t))
		((zerop (logand i 3))
		 (xlib:draw-rectangle xwindow gcontext x y5 1 5 t))
		(t
		 (xlib:draw-rectangle xwindow gcontext x y3 1 3 t)))
	  (incf x font-width))))))
