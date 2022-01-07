;;; Screen allocation functions: screen management and event handlers for
;;; the editor under X.

(in-package "EDI")

(export '(make-xwindow-like-window *create-window-hook* *delete-window-hook*
	  *random-typeout-hook* *create-initial-windows-hook*))

#[ CLX Interface

[ Graphics Window Hooks        ]
[ Entering and Leaving Windows ]
[ How to Lose Up-Events        ]
]#

#[ Graphics Window Hooks

This section describes a few hooks used internally to handle graphics
windows that manifest editor windows.  Some heavy users of the editor as a
tool have needed these in the past, but typically functions that replace
the default values of these hooks must be written in the "EDI" package.
All of these symbols are internal to this package.

If you need this level of control for your application, consult the current
implementation for code fragments that will be useful in correctly writing your
own window hook functions.

{variable:edi:*create-window-hook*}
{variable:edi:*delete-window-hook*}
{variable:edi:*random-typeout-hook*}
{variable:edi:*create-initial-windows-hook*}
]#

#[ Entering and Leaving Windows

{evariable:Enter Window Hook}
{evariable:Exit Window Hook}
]#

#[ How to Lose Up-Events

Often the only useful activity user's design for the mouse is to click on
something.  The editor sees a character representing the down event, but what do
you do with the up event character that you know must follow?  Having the
command eat it would be tasteless, and would inhibit later customizations that
make use of it, possibly adding on to the down click command's functionality.
Bind the corresponding up character to the command described here.

{command:Do Nothing}
]#

(proclaim '(special *echo-area-window*))

;;; We have an internal notion of window groups on bitmap devices.  Every
;;; editor window has a hunk slot which holds a structure with information
;;; about physical real-estate on some device.  Bitmap-hunks have an X window
;;; and a window-group.  The X window is a child of the window-group's window.
;;; The echo area, pop-up display window, and the initial window are all in
;;; their own group.
;;;
;;; MAKE-WINDOW splits the current window which is some child window in a group.
;;; If the user supplied an X window, it becomes the parent window of some new
;;; group, and we make a child for the editor window.  If the user supplies
;;; ask-user, we prompt for a group/parent window.  We link the hunks for
;;; NEXT-WINDOW and PREVIOUS-WINDOW only within a group, so the group maintains
;;; a stack of windows that always fill the entire group window.
;;;

;;; This is the object set for editor windows.  All types of incoming X
;;; events on standard editing windows have the same handlers via this set.
;;; We also include the group/parent windows in here, but they only handle
;;; :configure-notify events.
;;;
(defvar *editor-windows*
  (system:make-object-set "Editor Windows" #'ext:default-clx-event-handler))


;;;; Some window making parameters.

;;; These could be parameters, but they have to be set after the display is
;;; opened.  These are set in INIT-BITMAP-SCREEN-MANAGER.

(defvar *default-border-pixmap* nil
  "This is the default color of X window borders.  It defaults to a grey
   pattern.")

(defvar *highlight-border-pixmap* nil
  "This is the color of the border of the current window when the mouse
   cursor is over any editor window.")


;;;; Exposed region handling.

;;; :exposure events are sent because we selected them.  :graphics-exposure
;;; events are generated because of a slot in our graphics contexts.  These are
;;; generated from using XLIB:COPY-AREA when the source could not be generated.
;;; Also, :no-exposure events are sent when a :graphics-exposure event could
;;; have been sent but wasn't.
;;;

;;; This is an old handler that doesn't do anything clever about multiple
;;; exposures.
;(defun hunk-exposed-region (hunk &key y height &allow-other-keys)
(defun old-hunk-exposed-region (hunk event-key event-window x y width height
				 foo bar &optional baz quux)
  (declare (ignore event-key event-window x width foo bar baz quux))
  (if (bitmap-hunk-lock hunk)
      (setf (bitmap-hunk-trashed hunk) t)
      (let ((liftp (and (eq *cursor-hunk* hunk) *cursor-dropped*)))
	(when liftp (lift-cursor))
	;; (hunk-draw-top-border hunk)
	(let* ((font-family (bitmap-hunk-font-family hunk))
	       (font-height (font-family-height font-family))
	       (co (font-family-cursor-y-offset font-family))
	       (start (truncate (- y hunk-top-border) font-height))
	       (end (ceiling (- (+ y height) hunk-top-border) font-height))
	       (start-bit (+ (* start font-height) co hunk-top-border))
	       (nheight (- (* (- end start) font-height) co))
	       (end-line (bitmap-hunk-end hunk)))
	  (declare (fixnum font-height co start end start-bit nheight))
	  (xlib:clear-area (bitmap-hunk-xwindow hunk) :x 0 :y start-bit
			   :width (bitmap-hunk-width hunk) :height nheight)
	  (do ((dl (bitmap-hunk-start hunk) (cdr dl))
	       (i 0 (1+ i)))
	      ((or (eq dl end-line) (= i start))
	       (do ((i i (1+ i))
		    (dl dl (cdr dl)))
		   ((or (eq dl end-line) (= i end)))
		 (declare (fixnum i))
		 (hunk-write-line hunk (car dl) i)))
	    (declare (fixnum i)))
	  (when (and (bitmap-hunk-modeline-pos hunk)
		     (>= (the fixnum (+ nheight start-bit))
			 (the fixnum (bitmap-hunk-modeline-pos hunk))))
	    (hunk-replace-modeline hunk)))
	(when liftp (drop-cursor)))))

;;; HUNK-EXPOSED-REGION redisplays the appropriate rectangle from the hunk
;;; dis-lines.  Don't do anything if the hunk is trashed since redisplay is
;;; probably about to fix everything; specifically, this keeps new windows
;;; from getting drawn twice (once for the exposure and once for being trashed).
;;;
;;; Exposure and graphics-exposure events pass in a different number of
;;; arguments, with some the same but in a different order, so we just bind
;;; and ignore foo, bar, baz, and quux.
;;;
(defun hunk-exposed-region (hunk event-key event-window x y width height
				 foo bar &optional baz quux)
  (declare (ignore event-key event-window x width foo bar baz quux))
  (unless (bitmap-hunk-trashed hunk)
    (let ((liftp (and (eq *cursor-hunk* hunk) *cursor-dropped*))
	  (display (bitmap-device-display (device-hunk-device hunk))))
      (when liftp (lift-cursor))
      (multiple-value-bind (y-peek height-peek)
			   (exposed-region-peek-event display
						      (bitmap-hunk-xwindow hunk))
	(if y-peek
	    (let ((n (coelesce-exposed-regions hunk display
					       y height y-peek height-peek)))
	      (write-n-exposed-regions hunk n))
	    (write-one-exposed-region hunk y height)))
      (xlib:display-force-output display)
      (if liftp (drop-cursor)))))
;;;
(ext:serve-exposure *editor-windows* #'hunk-exposed-region)
(ext:serve-graphics-exposure *editor-windows* #'hunk-exposed-region)


;;; HUNK-NO-EXPOSURE handles this bullshit event that gets sent without its
;;; being requested.
;;;
(defun hunk-no-exposure (hunk event-key event-window major minor send-event-p)
  (declare (ignore hunk event-key event-window major minor send-event-p))
  t)
;;;
(ext:serve-no-exposure *editor-windows* #'hunk-no-exposure)

;;; EXPOSED-REGION-PEEK-EVENT returns the position and height of an :exposure
;;; or :graphics-exposure event on win if one exists.  If there are none, then
;;; nil and nil are returned.
;;;
(defun exposed-region-peek-event (display win)
  (xlib:display-finish-output display)
  (let (result-y result-height)
    (xlib:process-event
     display :timeout 0
     :handler #'(lambda (&key event-key event-window window y height
			      &allow-other-keys)
		  (cond ((and (or (eq event-key :exposure)
				  (eq event-key :graphics-exposure))
			      (or (eq event-window win) (eq window win)))
			 (setf result-y y)
			 (setf result-height height)
			 t)
			(t ()))))
    (values result-y result-height)))

;;; COELESCE-EXPOSED-REGIONS insert sorts exposed region events from the X
;;; input queue into *coelesce-buffer*.  Then the regions are merged into the
;;; same number or fewer regions that are vertically distinct
;;; (non-overlapping).  When this function is called, one event has already
;;; been popped from the queue, the first event that caused HUNK-EXPOSED-REGION
;;; to be called.  That information is passed in as y1 and height1.  There is
;;; a second event that also has already been popped from the queue, the
;;; event resulting from peeking for multiple "exposure" events.  That info
;;; is passed in as y2 and height2.
;;;
(defun coelesce-exposed-regions (hunk display y1 height1 y2 height2)
  (let ((len 0))
    (declare (fixnum len))
    ;;
    ;; Insert sort the exposeevents as we pick them off the event queue.
    (let* ((font-family (bitmap-hunk-font-family hunk))
	   (font-height (font-family-height font-family))
	   (co (font-family-cursor-y-offset font-family))
	   (xwindow (bitmap-hunk-xwindow hunk)))
      ;;
      ;; Insert the region the exposedregion handler was called on.
      (multiple-value-bind (start-line start-bit end-line expanded-height)
			   (exposed-region-bounds y1 height1 co font-height)
	(setf len
	      (coelesce-buffer-insert start-bit start-line
				      expanded-height end-line len)))
      ;;
      ;; Peek for exposedregion events on xwindow, inserting them into
      ;; the buffer.
      (let ((y y2)
	    (height height2))
	(loop
	  (multiple-value-bind (start-line start-bit end-line expanded-height)
			       (exposed-region-bounds y height co font-height)
	    (setf len
		  (coelesce-buffer-insert start-bit start-line
					  expanded-height end-line len)))
	  (multiple-value-setq (y height)
	    (exposed-region-peek-event display xwindow))
	  (unless y (return)))))
    (coelesce-exposed-regions-merge len)))

;;; *coelesce-buffer* is a vector of records used to sort exposure events on a
;;; single hunk, so we can merge them into fewer, larger regions of exposure.
;;; COELESCE-BUFFER-INSERT places elements in this buffer, and each element
;;; is referenced with COELESCE-BUFFER-ELT.  Each element of the coelescing
;;; buffer has the following accessors defined:
;;;    COELESCE-BUFFER-ELT-START	in pixels.
;;;    COELESCE-BUFFER-ELT-START-LINE	in dis-lines.
;;;    COELESCE-BUFFER-ELT-HEIGHT	in pixels.
;;;    COELESCE-BUFFER-ELT-END-LINE	in dis-lines.
;;; These are used by COELESCE-BUFFER-INSERT, COELESCE-EXPOSED-REGIONS-MERGE,
;;; and WRITE-N-EXPOSED-REGIONS.

(defvar *coelesce-buffer-fill-ptr* 25)
(defvar *coelesce-buffer* (make-array *coelesce-buffer-fill-ptr*))
(dotimes (i *coelesce-buffer-fill-ptr*)
  (setf (svref *coelesce-buffer* i) (make-array 4)))

(defmacro coelesce-buffer-elt-start (elt)
  `(svref ,elt 0))
(defmacro coelesce-buffer-elt-start-line (elt)
  `(svref ,elt 1))
(defmacro coelesce-buffer-elt-height (elt)
  `(svref ,elt 2))
(defmacro coelesce-buffer-elt-end-line (elt)
  `(svref ,elt 3))
(defmacro coelesce-buffer-elt (i)
  `(svref *coelesce-buffer* ,i))

;;; COELESCE-BUFFER-INSERT inserts an exposed region record into
;;; *coelesce-buffer* such that start is less than all successive
;;; elements.  Returns the new length of the buffer.
;;;
(defun coelesce-buffer-insert (start start-line height end-line len)
  (declare (fixnum start start-line height end-line len))
  ;;
  ;; Add element if len is to fill pointer.  If fill pointer is to buffer
  ;; length, then grow buffer.
  (when (= len (the fixnum *coelesce-buffer-fill-ptr*))
    (when (= (the fixnum *coelesce-buffer-fill-ptr*)
	     (the fixnum (length (the simple-vector *coelesce-buffer*))))
      (let ((new (make-array (ash (length (the simple-vector *coelesce-buffer*))
				  1))))
	(replace (the simple-vector new) (the simple-vector *coelesce-buffer*)
		 :end1 *coelesce-buffer-fill-ptr*
		 :end2 *coelesce-buffer-fill-ptr*)
	(setf *coelesce-buffer* new)))
    (setf (coelesce-buffer-elt len) (make-array 4))
    (incf *coelesce-buffer-fill-ptr*))
  ;;
  ;; Find point to insert record: start, start-line, height, and end-line.
  (do ((i 0 (1+ i)))
      ((= i len)
       ;; Start is greater than all previous starts.  Add it to the end.
       (let ((region (coelesce-buffer-elt len)))
	 (setf (coelesce-buffer-elt-start region) start)
	 (setf (coelesce-buffer-elt-start-line region) start-line)
	 (setf (coelesce-buffer-elt-height region) height)
	 (setf (coelesce-buffer-elt-end-line region) end-line)))
    (declare (fixnum i))
    (when (< start (the fixnum
			(coelesce-buffer-elt-start (coelesce-buffer-elt i))))
      ;;
      ;; Insert new element at i, using storage allocated at element len.
      (let ((last (coelesce-buffer-elt len)))
	(setf (coelesce-buffer-elt-start last) start)
	(setf (coelesce-buffer-elt-start-line last) start-line)
	(setf (coelesce-buffer-elt-height last) height)
	(setf (coelesce-buffer-elt-end-line last) end-line)
	;;
	;; Shift elements after i (inclusively) to the right.
	(do ((j (1- len) (1- j))
	     (k len j)
	     (terminus (1- i)))
	    ((= j terminus))
	  (declare (fixnum j k terminus))
	  (setf (coelesce-buffer-elt k) (coelesce-buffer-elt j)))
	;;
	;; Stash element to insert at i.
	(setf (coelesce-buffer-elt i) last))
      (return)))
  (1+ len))

;;; COELESCE-EXPOSED-REGIONS-MERGE merges/coelesces the regions in
;;; *coelesce-buffer*.  It takes the number of elements and returns the new
;;; number of elements.  The regions are examined one at a time relative to
;;; the current one.  The current region remains so, with next advancing
;;; through the buffer, until a next region is found that does not overlap
;;; and is not adjacent.  When this happens, the current values are stored
;;; in the current region, and the buffer's element after the current element
;;; becomes current.  The next element that was found not to be in contact
;;; the old current element is stored in the new current element by copying
;;; its values there.  The buffer's elements always stay in place, and their
;;; storage is re-used.  After this process which makes the next region be
;;; the current region, the next pointer is incremented.
;;;
(defun coelesce-exposed-regions-merge (len)
    (let* ((current 0)
	   (next 1)
	   (current-region (coelesce-buffer-elt 0))
	   (current-height (coelesce-buffer-elt-height current-region))
	   (current-end-line (coelesce-buffer-elt-end-line current-region))
	   (current-end-bit (+ (the fixnum
				    (coelesce-buffer-elt-start current-region))
			       current-height)))
      (declare (fixnum current next current-height
		       current-end-line current-end-bit))
      (loop
	(let* ((next-region (coelesce-buffer-elt next))
	       (next-start (coelesce-buffer-elt-start next-region))
	       (next-height (coelesce-buffer-elt-height next-region))
	       (next-end-bit (+ next-start next-height)))
	  (declare (fixnum next-start next-height next-end-bit))
	  (cond ((<= next-start current-end-bit)
		 (let ((extra-height (- next-end-bit current-end-bit)))
		   (declare (fixnum extra-height))
		   ;; Maybe the next region is contained in the current.
		   (when (plusp extra-height)
		     (incf current-height extra-height)
		     (setf current-end-bit next-end-bit)
		     (setf current-end-line
			   (coelesce-buffer-elt-end-line next-region)))))
		(t
		 ;;
		 ;; Update current record since next does not overlap
		 ;; with current.
		 (setf (coelesce-buffer-elt-height current-region)
		       current-height)
		 (setf (coelesce-buffer-elt-end-line current-region)
		       current-end-line)
		 ;;
		 ;; Move to new distinct region, copying data from next region.
		 (incf current)
		 (setf current-region (coelesce-buffer-elt current))
		 (setf (coelesce-buffer-elt-start current-region) next-start)
		 (setf (coelesce-buffer-elt-start-line current-region)
		       (coelesce-buffer-elt-start-line next-region))
		 (setf current-height next-height)
		 (setf current-end-bit next-end-bit)
		 (setf current-end-line
		       (coelesce-buffer-elt-end-line next-region)))))
	(incf next)
	(when (= next len)
	  (setf (coelesce-buffer-elt-height current-region) current-height)
	  (setf (coelesce-buffer-elt-end-line current-region) current-end-line)
	  (return)))
      (1+ current)))

;;; EXPOSED-REGION-BOUNDS returns as multiple values the first line affected,
;;; the first possible bit affected (accounting for the cursor), the end line
;;; affected, and the height of the region.
;;;
(defun exposed-region-bounds (y height cursor-offset font-height)
  (declare (fixnum y height cursor-offset font-height))
  (let* ((start (truncate (the fixnum (- y hunk-top-border))
			  font-height))
	 (end (ceiling (the fixnum (- (the fixnum (+ y height))
				      hunk-top-border))
		       font-height)))
    (values
     start
     (+ (the fixnum (* start font-height)) cursor-offset hunk-top-border)
     end
     (- (the fixnum (* (the fixnum (- end start)) font-height))
	cursor-offset))))

(defun write-n-exposed-regions (hunk n)
  (declare (fixnum n))
  (let* (;; Loop constants.
	 (end-dl (bitmap-hunk-end hunk))
	 (xwindow (bitmap-hunk-xwindow hunk))
	 (hunk-width (bitmap-hunk-width hunk))
	 ;; Loop variables.
	 (dl (bitmap-hunk-start hunk))
	 (i 0)
	 (region (coelesce-buffer-elt 0))
	 (start-line (coelesce-buffer-elt-start-line region))
	 (start (coelesce-buffer-elt-start region))
	 (height (coelesce-buffer-elt-height region))
	 (end-line (coelesce-buffer-elt-end-line region))
	 (region-idx 0))
    (declare (fixnum i start start-line height end-line region-idx))
    ;(format t "write-n-exposed-regions~%")
    (loop
      (xlib:clear-area xwindow :x 0 :y start :width hunk-width :height height)
      ;; Find this regions first line.
      (loop
	(when (or (eq dl end-dl) (= i start-line))
	  (return))
	(incf i)
	(setf dl (cdr dl)))
      ;; Write this region's lines.
      (loop
	(when (or (eq dl end-dl) (= i end-line))
	  (return))
	(hunk-write-line hunk (car dl) i)
	(incf i)
	(setf dl (cdr dl)))
      ;; Get next region unless we're done.
      (when (= (incf region-idx) n) (return))
      (setf region (coelesce-buffer-elt region-idx))
      (setf start (coelesce-buffer-elt-start region))
      (setf start-line (coelesce-buffer-elt-start-line region))
      (setf height (coelesce-buffer-elt-height region))
      (setf end-line (coelesce-buffer-elt-end-line region)))
    ;;
    ;; Check for modeline exposure.
    (setf region (coelesce-buffer-elt (1- n)))
    (setf start (coelesce-buffer-elt-start region))
    (setf height (coelesce-buffer-elt-height region))
    (when (and (bitmap-hunk-modeline-pos hunk)
	       (> (+ start height)
		  (- (bitmap-hunk-modeline-pos hunk)
		     (bitmap-hunk-bottom-border hunk))))
      (hunk-replace-modeline hunk)
      (hunk-draw-bottom-border hunk))))

(defun write-one-exposed-region (hunk y height)
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (font-height (font-family-height font-family))
	 (co (font-family-cursor-y-offset font-family))
	 (start-line (truncate (- y hunk-top-border) font-height))
	 (end-line (ceiling (- (+ y height) hunk-top-border) font-height))
	 (start-bit (+ (* start-line font-height) co hunk-top-border))
	 (nheight (- (* (- end-line start-line) font-height) co))
	 (hunk-end-line (bitmap-hunk-end hunk)))
    (declare (fixnum font-height co start-line end-line start-bit nheight))
    #|
    (format t "write-one-exposed-region~%")
    (format t "(clear ~A :x 0 :y ~A :width ~A :height ~A)~%"
	    (bitmap-hunk-xwindow hunk)
	    start-bit
	    (bitmap-hunk-width hunk)
	    nheight)
    |#
    (xlib:clear-area (bitmap-hunk-xwindow hunk) :x 0 :y start-bit
		     :width (bitmap-hunk-width hunk) :height nheight)
    (do ((dl (bitmap-hunk-start hunk) (cdr dl))
	 (i 0 (1+ i)))
	((or (eq dl hunk-end-line) (= i start-line))
	 (do ((i i (1+ i))
	      (dl dl (cdr dl)))
	     ((or (eq dl hunk-end-line) (= i end-line)))
	   (declare (fixnum i))
	   (hunk-write-line hunk (car dl) i)))
      (declare (fixnum i)))
    (when (and (bitmap-hunk-modeline-pos hunk)
	       (> (+ start-bit nheight)
		  (- (bitmap-hunk-modeline-pos hunk)
		     (bitmap-hunk-bottom-border hunk))))
      (hunk-replace-modeline hunk)
      (hunk-draw-bottom-border hunk))))


;;;; Resized window handling.

;;; :configure-notify events are sent because we select :structure-notify.
;;; This buys us a lot of events we have to write dummy handlers to ignore.
;;;
;;; :resize-request events are sent because we select :resize-request.
;;; :visibility-notify events are sent because we select :visibility-notify
;;; to get :resize-request.
;;;

(defun hunk-resized (object event-key event-window width
			    height send-event-p)
  (declare (ignore event-key event-window send-event-p))
  (ed:msg "hr . . . ~A ~A ." width height)
  (typecase object
    (bitmap-hunk
     (fi (and (= width (bitmap-hunk-width object))
	      (= height (bitmap-hunk-height object)))
	 (progn
	   #|
	   (hunk-changed object width height ())
	   ;; Under X11, don't redisplay since an exposure event is coming next.
	   (setf (bitmap-hunk-trashed object) t)
	   |#
	   (setf (bitmap-hunk-trashed object) t)
	   (hunk-changed object width height t))))
    (window-group
     (let ((old-width (window-group-width object))
	   (old-height (window-group-height object)))
       (fi (and (= width old-width) (= height old-height))
	   (window-group-changed object width height))))))
;;;
(ext:serve-resize-request *editor-windows* #'hunk-resized)

(defun handle-visibility-notify (object key window state send-event-p)
  (declare (ignore object key window state send-event-p))
  t)
;;;
(ext:serve-visibility-notify *editor-windows* #'hunk-resized)

;;; HUNK-RECONFIGURED -- Internal.
;;;
;;; This must note that the hunk changed to prevent certain redisplay problems
;;; with recentering the window that caused bogus lines to be drawn after the
;;; actual visible text in the window.  We must also indicate the hunk is
;;; trashed to eliminate exposure event handling that comes after resizing.
;;; This also causes a full redisplay on the window which is the easiest and
;;; generally best looking thing.
;;;
(defun hunk-reconfigured (object event-key event-window window x y width
				 height border-width above-sibling
				 override-redirect-p send-event-p)
  (declare (ignore event-key event-window window x y border-width
		   above-sibling override-redirect-p send-event-p))
  (typecase object
    (bitmap-hunk
     (fi (and (= width (bitmap-hunk-width object))
	      (= height (bitmap-hunk-height object)))
	 (progn
	   (hunk-changed object width height nil)
	   ;; Under X11, don't redisplay since an exposure event is coming next.
	   (setf (bitmap-hunk-trashed object) t))))
    (window-group
     (let ((old-width (window-group-width object))
	   (old-height (window-group-height object)))
       (fi (and (= width old-width) (= height old-height))
	   (window-group-changed object width height))))))
;;;
(ext:serve-configure-notify *editor-windows* #'hunk-reconfigured)


;;; HUNK-IGNORE-EVENT ignores the following unrequested events.  They all take
;;; at least five arguments, but then there are up to four more optional.
;;;
(defun hunk-ignore-event (hunk event-key event-window window one
			       &optional two three four five)
  (declare (ignore hunk event-key event-window window one two three four five))
  t)
;;;
(ext:serve-destroy-notify *editor-windows* #'hunk-ignore-event)
(ext:serve-unmap-notify *editor-windows* #'hunk-ignore-event)
(ext:serve-map-notify *editor-windows* #'hunk-ignore-event)
(ext:serve-reparent-notify *editor-windows* #'hunk-ignore-event)
(ext:serve-gravity-notify *editor-windows* #'hunk-ignore-event)
(ext:serve-circulate-notify *editor-windows* #'hunk-ignore-event)
(ext:serve-client-message *editor-windows* #'hunk-ignore-event)
(ext:serve-property-notify *editor-windows* #'hunk-ignore-event)


;;;; Interface to X input events.

;;; HUNK-KEY-INPUT and HUNK-MOUSE-INPUT.
;;; Each key and mouse event is turned into a character via
;;; EXT:TRANSLATE-CHARACTER or EXT:TRANSLATE-MOUSE-CHARACTER, either of which
;;; may return nil.  Nil is returned for input that is considered uninteresting
;;; input; for example, shift and control.
;;;

(defun hunk-key-input (hunk event-key event-window root child same-screen-p x y
		       root-x root-y modifiers time key-code send-event-p)
  (declare (ignore event-key event-window root child same-screen-p root-x
		   root-y time send-event-p))
#|
  (format t "(hunk-process-input ~A ...)~%" hunk)
  (format t "x: ~A~%" x)
  (format t "y: ~A~%" y)
  (format t "key-code: ~A~%" key-code)
  (format t "modifiers: ~A~%" modifiers)
  (format t "(device-hunk-device hunk): ~A~%" (device-hunk-device hunk))
|#
  (declare (ignore hunk))
  (let ((hunk (window-hunk (current-window))))
    (hunk-process-input hunk
			(ext:translate-key-event
			 (bitmap-device-display (device-hunk-device hunk))
			 key-code modifiers)
			x y)))
;;;
(ext:serve-key-press *editor-windows* #'hunk-key-input)

(defun hunk-mouse-input (hunk event-key event-window root child same-screen-p x y
			 root-x root-y modifiers time key-code send-event-p)
  (declare (ignore event-window root child same-screen-p root-x root-y
		   time send-event-p))
  (hunk-process-input hunk
		      (ext:translate-mouse-key-event key-code modifiers
						     event-key)
		      x y))
;;;
(ext:serve-button-press *editor-windows* #'hunk-mouse-input)
(ext:serve-button-release *editor-windows* #'hunk-mouse-input)

(defun hunk-process-input (hunk char x y)
  #|
  (format t "(hunk-process-input ~A ~A ~A ~A)~%"
	  hunk char x y)
  |#
  (when char
    (let* ((font-family (bitmap-hunk-font-family hunk))
	   (font-width (font-family-width font-family))
	   (font-height (font-family-height font-family))
	   (ml-pos (bitmap-hunk-modeline-pos hunk))
	   (height (bitmap-hunk-height hunk))
	   (width (bitmap-hunk-width hunk))
	   (handler (bitmap-hunk-input-handler hunk))
	   (char-width (bitmap-hunk-char-width hunk)))
      (cond ((not (and (< -1 x width) (< -1 y height)))
	     (funcall handler hunk char nil nil))
	    ((and ml-pos (> y (- ml-pos (bitmap-hunk-bottom-border hunk))))
	     (funcall handler hunk char
		      ;; (/ width x) doesn't handle ends of thumb bar
		      ;; and eob right, so do a bunch of truncating.
		      (min (truncate x (truncate width char-width))
			   (1- char-width))
		      nil))
	    (t
	     (let* ((cx (truncate (- x hunk-left-border) font-width))
		    (temp (truncate (- y hunk-top-border) font-height))
		    (char-height (bitmap-hunk-char-height hunk))
		    ;; Extra bits below bottom line and above modeline and
		    ;; thumb bar are considered part of the bottom line since
		    ;; we have already picked off the y=nil case.
		    (cy (if (< temp char-height) temp (1- char-height))))
	       (if (and (< -1 cx char-width)
			(< -1 cy))
		   (funcall handler hunk char cx cy)
		   (funcall handler hunk char nil nil))))))))


;;;; Handling boundary crossing events.

; FIX (more in xcoms.lisp)
(defvar *input-requires-mouse* ()
  "If true then the editor only accepts input when the mouse is in one of
   the editor windows.")

;;; Entering and leaving a window are handled basically the same except
;;; that it is possible to get an entering event under X without getting an
;;; exiting event; specifically, when the mouse is in an editor window that
;;; is over another window, and someone buries the top window, the editor
;;; only gets an entering event on the lower window (no exiting event for
;;; the buried window).
;;;
;;; :enter-notify and :leave-notify events are sent because we select
;;; :enter-window and :leave-window events.
;;;

(defun hunk-mouse-entered (hunk event-key event-window root child same-screen-p
			   x y root-x root-y state time mode kind send-event-p)
  (declare (ignore event-key event-window child root same-screen-p
		   x y root-x root-y state time mode kind send-event-p))
  (when *input-requires-mouse*
    (when (and *cursor-dropped* (not *editor-listener*))
      (cursor-invert-center))
    (setf *editor-listener* t)
    (let ((current-hunk (window-hunk (current-window))))
      (unless (and *current-highlighted-border*
		   (eq *current-highlighted-border* current-hunk))
	(setf (xlib:window-border (window-group-xparent
				   (bitmap-hunk-window-group current-hunk)))
	      *highlight-border-pixmap*)
	(xlib:display-force-output
	 (bitmap-device-display (device-hunk-device current-hunk)))
	(setf *current-highlighted-border* current-hunk))))
  (let ((window (bitmap-hunk-window hunk)))
    (when window (invoke-hook ed::enter-window-hook window))))
;;;
(ext:serve-enter-notify *editor-windows* #'hunk-mouse-entered)

(defun hunk-mouse-left (hunk event-key event-window root child same-screen-p
			x y root-x root-y state time mode kind send-event-p)
  (declare (ignore event-key event-window child root same-screen-p
		   x y root-x root-y state time mode kind send-event-p))
  (when *input-requires-mouse*
    (setf *editor-listener* nil)
    (when *cursor-dropped* (cursor-invert-center))
    (when *current-highlighted-border*
      (setf (xlib:window-border (window-group-xparent
				 (bitmap-hunk-window-group
				  *current-highlighted-border*)))
	    *default-border-pixmap*)
      (xlib:display-force-output
       (bitmap-device-display (device-hunk-device *current-highlighted-border*)))
      (setf *current-highlighted-border* nil)))
  (let ((window (bitmap-hunk-window hunk)))
    (when window (invoke-hook ed::exit-window-hook window))))
;;;
(ext:serve-leave-notify *editor-windows* #'hunk-mouse-left)


;;;; Handling focus events.

;; *xwindow-hunk-map*  --  Internal
;;
;; Alist mapping xwindows to hunks.
;;
(defvar *xwindow-hunk-map* ())

;; XWINDOW-HUNK  --  Internal
;;
;; Return the hunk associated with $xwindow.
;;
(defun xwindow-hunk (xwindow)
  (cdr (assoc xwindow *xwindow-hunk-map* :test #'equalp)))

;;; :focus-in events are sent because we select :focus-in events.
;;;

(defun hunk-focus-in (hunk event-key event-window mode details send-event-p)
  (declare (ignore hunk event-key mode details send-event-p))
  (let ((hunk (xwindow-hunk event-window)))
    (fi hunk
	(format t "hunk-focus-in hunk (), event-window ~A~%" event-window)
	(let ((window (bitmap-hunk-window hunk)))
	  (if (eq window *echo-area-window*)
	      (if *input-requires-mouse* (lift-cursor))
	      (setf (current-window) window))))))
;;;
(ext:serve-focus-in *editor-windows* #'hunk-focus-in)

(defun hunk-focus-out (hunk event-key event-window mode details send-event-p)
  (declare (ignore hunk event-key mode details send-event-p))
  (if *input-requires-mouse*
      (or *cursor-dropped*
	  (let ((hunk (xwindow-hunk event-window)))
	    (fi hunk
		(format t "out hunk (), event-window ~A~%" event-window)
		(let ((window (bitmap-hunk-window hunk)))
		  (or (eq window *echo-area-window*)
		      (bitmap-hunk-trashed hunk)
		      (if (eq *cursor-hunk* hunk)
			  (drop-cursor)))))))))
;;;
(ext:serve-focus-out *editor-windows* #'hunk-focus-out)

#|
(defun hunk-keymap-notify (hunk event-key event-window key-vector send-event-p)
  (declare (ignore hunk event-key key-vector send-event-p))
  (let ((hunk (xwindow-hunk event-window)))
    (fi hunk
	(format t "keymap-notify hunk (), event-window ~A~%" event-window)
	(if (bitmap-hunk-trashed hunk)
	    (format t "hunk trashed ~A~%" hunk)
	    (let ((window (bitmap-hunk-window hunk)))
	      (if (eq window *echo-area-window*)
		  (lift-cursor)
		  (setf (current-window) window)))))))
;;;
(ext:serve-keymap-notify *editor-windows* #'hunk-keymap-notify)
|#


;;;; Handling selections.

(defun hunk-selection-notify (hunk &optional event-key event-window
				   selection target property time
				   send-event-p)
  (declare (ignore hunk event-key event-window selection target property
		   time send-event-p))
  t)
;;;
(ext:serve-selection-notify *editor-windows* #'hunk-selection-notify)

(defun hunk-selection-request (hunk &optional event-key event-window
				    requestor selection target property
				    time send-event-p)
  (xlib:handle-selection-request (bitmap-device-display
				  (device-hunk-device hunk))
				 event-key event-window requestor
				 selection target property time
				 send-event-p)
  t)
;;;
(ext:serve-selection-request *editor-windows* #'hunk-selection-request)

(defun hunk-selection-clear (hunk &optional event-key event-window
				  selection time send-event-p)
  (xlib:handle-selection-clear (bitmap-device-display
				(device-hunk-device hunk))
			       event-key event-window
			       selection time send-event-p)
  t)
;;;
(ext:serve-selection-request *editor-windows* #'hunk-selection-request)


;;;; Making a Window.

(defparameter minimum-window-height 100
  "If the window created by splitting a window would be shorter than this,
   then we create an overlapped window the same size instead.")

;;; The width must be that of a tab for the screen image builder, and the
;;; height must be one line (two with a modeline).
;;;
(defconstant minimum-window-lines 2
  "Windows must have at least this many lines.")
(defconstant minimum-window-columns 10
  "Windows must be at least this many characters wide.")

(eval-when (compile eval load)
(defconstant xwindow-border-width 2 "X border around X windows")
(defconstant xwindow-border-width*2 (* xwindow-border-width 2))
); eval-when

;;; We must name windows (set the "name" property) to get around a bug in
;;; awm and twm.  They will not handle menu clicks without a window having
;;; a name.  We set the name to this silly thing.
;;;
(defvar *editor-window-count* 0)
;;;
(defun new-editor-window-name ()
  (let ((*print-base* 10))
    (format nil "Nightshade editor ~S" (incf *editor-window-count*))))

(proclaim '(inline surplus-window-height surplus-window-height-w/-modeline))
;;;
(defun surplus-window-height (thumb-bar-p)
  (+ hunk-top-border (if thumb-bar-p
			 hunk-thumb-bar-bottom-border
			 hunk-bottom-border)))
;;;
(defun surplus-window-height-w/-modeline (thumb-bar-p)
  (+ (surplus-window-height thumb-bar-p)
     hunk-modeline-top
     hunk-modeline-bottom))

;;; DEFAULT-CREATE-WINDOW-HOOK -- Internal.
;;;
;;; This is the default value for *create-window-hook*.  It makes an X window
;;; for a new group/parent on the given display possibly prompting the user.
;;;
(defun default-create-window-hook (display x y width height name font-family
				   &optional modelinep thumb-bar-p)
  (maybe-prompt-user-for-window
   (xlib:screen-root (xlib:display-default-screen display))
   x y width height font-family modelinep thumb-bar-p name))

;;; MAYBE-PROMPT-USER-FOR-WINDOW -- Internal.
;;;
;;; This makes an X window and sets its standard properties according to
;;; supplied values.  When some of these are nil, the window manager should
;;; prompt the user for those missing values when the window gets mapped.  We
;;; use this when making new group/parent windows.  Returns the window without
;;; mapping it.
;;;
(defun maybe-prompt-user-for-window (root x y width height font-family
				     modelinep thumb-bar-p icon-name)
  (let ((font-height (font-family-height font-family))
	(font-width (font-family-width font-family))
	(extra-y (surplus-window-height thumb-bar-p))
	(extra-y-w/-modeline (surplus-window-height-w/-modeline thumb-bar-p)))
    (create-window-with-properties
     root x y
     (if width (+ (* width font-width) hunk-left-border))
     (if height
	 (if modelinep
	     (+ (* (1+ height) font-height) extra-y-w/-modeline)
	     (+ (* height font-height) extra-y)))
     font-width font-height icon-name
     (+ (* minimum-window-columns font-width) hunk-left-border)
     (if modelinep
	 (+ (* (1+ minimum-window-lines) font-height) extra-y-w/-modeline)
	 (+ (* minimum-window-lines font-height) extra-y))
     t)))

(defvar *create-window-hook* #'default-create-window-hook
  "A function called when `make-window' executes under CLX.

   The function must accept the CLX display, x (from `make-window'), y
   (from `make-window'), width (from `make-window'), height (from
   `make-window'), a name for the window's icon-name, font-family (from
   `make-window'), modelinep (from `make-window'), and whether the window
   will have a thumb-bar meter.

   The function must return a window, or () to indicate failure.")

;;; BITMAP-MAKE-WINDOW -- Internal.
;;;
(defun bitmap-make-window (device start modelinep window font-family
				  ask-user x y width-arg height-arg proportion)
  (let* ((display (bitmap-device-display device))
	 (thumb-bar-p (value ed::thumb-bar-meter))
	 (hunk (make-bitmap-hunk
		:font-family font-family
		:end the-sentinel  :trashed t
		:input-handler #'window-input-handler
		:device device
		:thumb-bar-p (and modelinep thumb-bar-p))))
    (multiple-value-bind
	(xparent xwindow)
	(maybe-make-x-window-and-parent window display start ask-user x y
					width-arg height-arg font-family
					modelinep thumb-bar-p proportion)
      (or xwindow (return-from bitmap-make-window nil))
      (let ((window-group (make-window-group xparent
					     (xlib:drawable-width xparent)
					     (xlib:drawable-height xparent))))
	(setf (bitmap-hunk-xwindow hunk) xwindow)
	(pushnew (cons xwindow hunk) *xwindow-hunk-map* :test #'equalp)
	(setf (bitmap-hunk-window-group hunk) window-group)
	(setf (bitmap-hunk-gcontext hunk)
	      (default-gcontext xwindow font-family))
	;;
	;; Select input and enable event service before showing the window.
	(setf (xlib:window-event-mask xwindow) child-interesting-xevents-mask)
	(setf (xlib:window-event-mask xparent) group-interesting-xevents-mask)
	(add-xwindow-object xwindow hunk *editor-windows*)
	(add-xwindow-object xparent window-group *editor-windows*))
      (when xparent (xlib:map-window xparent))
      (xlib:map-window xwindow)
      (xlib:display-finish-output display)
      ;; A window is only really mapped when it is viewable.  It is said to
      ;; be mapped if a map request has been sent whether it is handled or
      ;; not.
      (until () ((and (eq (xlib:window-map-state xwindow) :viewable)
		      (eq (xlib:window-map-state xparent) :viewable))))
      ;; Clear the window.
      (setf (xlib:window-background xwindow)
	    (color-pixel display
			 (or (value ed::initial-background-color)
			     '(1 1 1)))) ; White.
      (xlib:clear-window xwindow)
      ;;
      ;; Find out how big it is...
      (xlib:with-state (xwindow)
	(set-hunk-size hunk (xlib:drawable-width xwindow)
		       (xlib:drawable-height xwindow) modelinep)))
    (setf (bitmap-hunk-window hunk)
	  (window-for-hunk hunk start modelinep))
    ;; If window is true, then it is a new group/parent window, so don't
    ;; link it into the current window's group.  When ask-user is true, we
    ;; make a new group too.
    (cond ((or window ask-user)
	   ;; This occurs when we make the world's first editor window.
	   (or *current-window*
	       (setq *current-window* (bitmap-hunk-window hunk)))
	   (setf (bitmap-hunk-previous hunk) hunk)
	   (setf (bitmap-hunk-next hunk) hunk))
	  (t
	   (let ((h (window-hunk *current-window*)))
	     (shiftf (bitmap-hunk-next hunk) (bitmap-hunk-next h) hunk)
	     (setf (bitmap-hunk-previous (bitmap-hunk-next hunk)) hunk)
	     (setf (bitmap-hunk-previous hunk) h))))
    (push hunk (device-hunks device))
    (bitmap-hunk-window hunk)))

;;; MAYBE-MAKE-X-WINDOW-AND-PARENT -- Internal.
;;;
;;; BITMAP-MAKE-WINDOW calls this.  If xparent is non-nil, we clear it and
;;; return it with a child that fills it.  If xparent is nil, and ask-user
;;; is true, then we invoke *create-window-hook* to get a parent window and
;;; return it with a child that fills it.  By default, we make a child in
;;; the CURRENT-WINDOW's parent.
;;;
(defun maybe-make-x-window-and-parent (xparent display start ask-user x y width
				       height font-family modelinep thumb-p
				       proportion)
  (let ((icon-name (buffer-name (line-buffer (mark-line start)))))
    (cond (xparent
	   ;(check-type xparent xlib:window)
	   (typep xparent 'xlib:window)
	   (let ((width (xlib:drawable-width xparent))
		 (height (xlib:drawable-height xparent)))
	     (xlib:clear-area xparent :width width :height height)
	     (modify-parent-properties :set xparent modelinep thumb-p
				       (font-family-width font-family)
				       (font-family-height font-family))
	     (values xparent (xwindow-for-xparent xparent icon-name))))
	  (ask-user
	   (let ((xparent (funcall *create-window-hook*
				   display x y width height icon-name
				   font-family modelinep thumb-p)))
	     (values xparent (xwindow-for-xparent xparent icon-name))))
	  (t
	   (let ((xparent (window-group-xparent
			   (bitmap-hunk-window-group
			    (window-hunk (current-window))))))
	     (values xparent
		     (create-window-from-current
		      proportion font-family modelinep thumb-p xparent
		      icon-name)))))))

;;; XWINDOW-FOR-XPARENT -- Internal.
;;;
;;; This returns a child of xparent that completely fills that parent window.
;;; We supply the font-width and font-height as nil because these are useless
;;; for child windows.
;;;
(defun xwindow-for-xparent (xparent icon-name)
  (xlib:with-state (xparent)
    (create-window-with-properties xparent 0 0
				   (xlib:drawable-width xparent)
				   (xlib:drawable-height xparent)
				   nil nil icon-name)))

;;; CREATE-WINDOW-FROM-CURRENT -- Internal.
;;;
;;; This makes a child window on parent by splitting the current window.  If
;;; the result will be too small, this returns nil.  If the current window's
;;; height is odd, the extra pixel stays with it, and the new window is one
;;; pixel smaller.
;;;
(defun create-window-from-current (proportion font-family modelinep thumb-p
				   parent icon-name)
  (let* ((cur-hunk (window-hunk *current-window*))
	 (cwin (bitmap-hunk-xwindow cur-hunk)))
    ;; Compute current window's height and take a proportion of it.
    (xlib:with-state (cwin)
      (let* ((cw (xlib:drawable-width cwin))
	     (ch (xlib:drawable-height cwin))
	     (cy (xlib:drawable-y cwin))
	     (new-ch (truncate (* ch (- 1 proportion))))
	     (font-height (font-family-height font-family))
	     (font-width (font-family-width font-family))
	     (cwin-min (minimum-window-height
			(font-family-height
			 (bitmap-hunk-font-family cur-hunk))
			(bitmap-hunk-modeline-pos cur-hunk)
			(bitmap-hunk-thumb-bar-p cur-hunk)))
	     (new-min (minimum-window-height font-height modelinep
					     thumb-p)))
	(declare (fixnum cw cy ch new-ch))
#|
	(ed::msg "ch ~A" ch)
	(ed::msg "n-ch ~A" new-ch)
	(ed::msg "cy ~A" cy)
	(ed::msg "mp ~A" modelinep)
	(ed::msg "fh ~A" font-height)
|#
	;; See if we have room for a new window.  This should really
	;; check the current window and the new one against their
	;; relative fonts and the minimal window columns and line
	;; (including whether there is a modeline).
	(if (and (> new-ch cwin-min)
		 (> (- ch new-ch) new-min))
	    (let ((win (create-window-with-properties
			parent 0 (+ cy new-ch)
			cw (- ch new-ch) font-width font-height
			icon-name)))
	      ;; No need to reshape current editor window structure here
	      ;; since this call will send an appropriate event.
	      (setf (xlib:drawable-height cwin) new-ch)
	      ;; Set hints on parent, so the user can't resize it to be
	      ;; smaller than what will hold the current number of
	      ;; children.
	      (modify-parent-properties :add parent modelinep
					thumb-p
					(font-family-width font-family)
					font-height)
	      win))))))

;;; MAKE-XWINDOW-LIKE-WINDOW -- Interface.
;;;
;;; The window name is set to get around an awm and twm bug that inhibits menu
;;; clicks unless the window has a name; this could be used better.
;;;
(defun make-xwindow-like-window (window)
  "This returns an group/parent xwindow with dimensions suitable for making
   an editor window like the argument window.  The new window's position
   should be the same as the argument window's position relative to the
   root.  When setting standard properties, we set x, y, width, and height
   to tell window managers to put the window where we intend without
   querying the user."
  (let* ((hunk (window-hunk window))
	 (font-family (bitmap-hunk-font-family hunk))
	 (xwin (bitmap-hunk-xwindow hunk)))
    (multiple-value-bind (x y)
			 (window-root-xy xwin)
      (create-window-with-properties
       (xlib:screen-root (xlib:display-default-screen
			  (bitmap-device-display (device-hunk-device hunk))))
       x y (bitmap-hunk-width hunk) (bitmap-hunk-height hunk)
       (font-family-width font-family)
       (font-family-height font-family)
       (buffer-name (window-buffer window))
       ;; When the user hands this window to MAKE-WINDOW, it will set the
       ;; minimum width and height properties.
       nil nil
       t))))


;;;; Deleting a window.

;;; DEFAULT-DELETE-WINDOW-HOOK -- Internal.
;;;
(defun default-delete-window-hook (xparent)
  (xlib:destroy-window xparent))
;;;
(defvar *delete-window-hook* #'default-delete-window-hook
  "A function called when `delete-window' executes under CLX.

   The function is passed the CLX window and the editor window.")

;;; BITMAP-DELETE-WINDOW  --  Internal
;;;
;;;
(defun bitmap-delete-window (window)
  (let* ((hunk (window-hunk window))
	 (xwindow (bitmap-hunk-xwindow hunk))
	 (xparent (window-group-xparent (bitmap-hunk-window-group hunk)))
	 (display (bitmap-device-display (device-hunk-device hunk))))
    (remove-xwindow-object xwindow)
    (setq *window-list* (delete window *window-list*))
    (when (eq *current-highlighted-border* hunk)
      (setf *current-highlighted-border* nil))
    (when (and (eq *cursor-hunk* hunk) *cursor-dropped*) (lift-cursor))
    (xlib:display-force-output display)
    (bitmap-delete-and-reclaim-window-space xwindow window)
    (while () ((deleting-window-drop-event display xwindow)))
    (let ((device (device-hunk-device hunk)))
      (setf (device-hunks device) (delete hunk (device-hunks device))))
    (cond ((eq hunk (bitmap-hunk-next hunk))
	   ;; Is this the last window in the group?
	   (remove-xwindow-object xparent)
	   (xlib:display-force-output display)
	   (funcall *delete-window-hook* xparent)
	   (while () ((deleting-window-drop-event display xparent)))
	   (let ((window (find-if-not #'(lambda (window)
					  (eq window *echo-area-window*))
				      *window-list*)))
	     (setf (current-buffer) (window-buffer window)
		   (current-window) window)))
	  (t
	   (modify-parent-properties :delete xparent
				     (bitmap-hunk-modeline-pos hunk)
				     (bitmap-hunk-thumb-bar-p hunk)
				     (font-family-width
				      (bitmap-hunk-font-family hunk))
				     (font-family-height
				      (bitmap-hunk-font-family hunk)))
	   (let ((next (bitmap-hunk-next hunk))
		 (prev (bitmap-hunk-previous hunk)))
	     (setf (bitmap-hunk-next prev) next)
	     (setf (bitmap-hunk-previous next) prev))))
    (let ((buffer (window-buffer window)))
      (setf (buffer-windows buffer) (delete window (buffer-windows buffer)))))
  nil)

;;; BITMAP-DELETE-AND-RECLAIM-WINDOW-SPACE -- Internal.
;;;
;;; This destroys the X window after obtaining its necessary state information.
;;; If the previous or next window (in that order) is "stacked" over or under
;;; the target window, then it is grown to fill in the newly opened space.  We
;;; fetch all the necessary configuration data up front, so we don't have to
;;; call XLIB:DESTROY-WINDOW while in the XLIB:WITH-STATE.
;;;
(defun bitmap-delete-and-reclaim-window-space (xwindow hwindow)
  (multiple-value-bind (y height)
		       (xlib:with-state (xwindow)
			 (values (xlib:drawable-y xwindow)
				 (xlib:drawable-height xwindow)))
    (xlib:destroy-window xwindow)
    (let ((hunk (window-hunk hwindow)))
      (xlib:free-gcontext (bitmap-hunk-gcontext hunk))
      (unless (eq hunk (bitmap-hunk-next hunk))
	(unless (maybe-merge-with-previous-window hunk y height)
	  (merge-with-next-window hunk y height))))))

;;; MAYBE-MERGE-WITH-PREVIOUS-WINDOW -- Internal.
;;;
;;; This returns true when it grows the previous hunk to include the
;;; argument hunk's screen space.
;;;
(defun maybe-merge-with-previous-window (hunk y h)
  (declare (fixnum y h))
  (let* ((prev (bitmap-hunk-previous hunk))
	 (prev-xwin (bitmap-hunk-xwindow prev)))
    (xlib:with-state (prev-xwin)
      (if (< (xlib:drawable-y prev-xwin) y)
	  (incf (xlib:drawable-height prev-xwin) h)))))

;;; MERGE-WITH-NEXT-WINDOW -- Internal.
;;;
;;; This trys to grow the next hunk's window to make use of the space created
;;; by deleting hunk's window.  If this is possible, then we must also move the
;;; next window up to where hunk's window was.
;;;
;;; When we reconfigure the window, we must set the hunk trashed.  This is
;;; a hack since twm is broken again and is sending exposure events before
;;; reconfigure notifications.  The editor relies on the protocol's
;;; statement that reconfigures come before exposures to set the hunk
;;; trashed before getting the exposure.  For now, we'll do it here too.
;;;
(defun merge-with-next-window (hunk y h)
  (declare (fixnum y h))
  (let* ((next (bitmap-hunk-next hunk))
	 (next-xwin (bitmap-hunk-xwindow next)))
    ;; Fetch height before setting y to save an extra round trip to the X
    ;; server.
    (let ((next-h (xlib:drawable-height next-xwin)))
      (setf (xlib:drawable-y next-xwin) y)
      (setf (xlib:drawable-height next-xwin) (+ next-h h)))
    (setf (bitmap-hunk-trashed next) t)
    (let ((hints (xlib:wm-normal-hints next-xwin)))
      (setf (xlib:wm-size-hints-y hints) y)
      (setf (xlib:wm-normal-hints next-xwin) hints))))

;;; DELETING-WINDOW-DROP-EVENT -- Internal.
;;;
;;; This checks for an event on win.  If there is one, remove it from the
;;; queue and return t.  Otherwise, return nil.
;;;
(defun deleting-window-drop-event (display win)
  (xlib:display-finish-output display)
  (let ((result nil))
    (xlib:process-event
     display :timeout 0
     :handler #'(lambda (&key event-window window &allow-other-keys)
		  (if (or (eq event-window win) (eq window win))
		      (setf result t)
		      nil)))
    result))

;;; MODIFY-PARENT-PROPERTIES -- Internal.
;;;
;;; This adds or deletes from xparent's min-height and min-width hints, so the
;;; window manager will hopefully prevent users from making a window group too
;;; small to hold all the windows in it.  We add to the height when we split
;;; windows making additional ones, and we delete from it when we delete a
;;; window.
;;;
;;; NOTE, THIS FAILS TO MAINTAIN THE WIDTH CORRECTLY.  We need to maintain the
;;; width as the MAX of all the windows' minimal widths.  A window's minimal
;;; width is its font's width multiplied by minimum-window-columns.
;;;
(defun modify-parent-properties (type xparent modelinep thumb-p
				 font-width font-height)
  (let ((hints (xlib:wm-normal-hints xparent)))
    (xlib:set-wm-properties
     xparent
     :resource-name "Nightshade Editor"
     :x (xlib:wm-size-hints-x hints)
     :y (xlib:wm-size-hints-y hints)
     :width (xlib:drawable-width xparent)
     :height (xlib:drawable-height xparent)
     :user-specified-position-p t
     :user-specified-size-p t
     :width-inc (xlib:wm-size-hints-width-inc hints)
     :height-inc (xlib:wm-size-hints-height-inc hints)
     :min-width (or (xlib:wm-size-hints-min-width hints)
		    (+ (* minimum-window-columns font-width) hunk-left-border))
     :min-height
     (let ((delta (minimum-window-height font-height modelinep thumb-p)))
       (ecase type
	 (:delete (- (xlib:wm-size-hints-min-height hints) delta))
	 (:add (+ (or (xlib:wm-size-hints-min-height hints) 0)
		  delta))
	 (:set delta))))))

;;; MINIMUM-WINDOW-HEIGHT -- Internal.
;;;
;;; This returns the minimum height necessary for a window given some of its
;;; parameters.  This is the number of lines times font-height plus any extra
;;; pixels for aesthetics.
;;;
(defun minimum-window-height (font-height modelinep thumb-p)
  (if modelinep
      (+ (* (1+ minimum-window-lines) font-height)
	 (surplus-window-height-w/-modeline thumb-p))
      (+ (* minimum-window-lines font-height)
	 (surplus-window-height thumb-p))))


;;;; Next and Previous windows.

(defun bitmap-next-window (window)
  "Return the next window after Window, wrapping around if Window is the
   bottom window."
  (check-type window window)
  (bitmap-hunk-window (bitmap-hunk-next (window-hunk window))))

(defun bitmap-previous-window (window)
  "Return the previous window after Window, wrapping around if Window is
   the top window."
  (check-type window window)
  (bitmap-hunk-window (bitmap-hunk-previous (window-hunk window))))


;;;; Setting window parameters.

;;; %SET-WINDOW-WIDTH  --  Internal
;;;
;;; Since we don't support non-full-width windows, this does nothing.
;;;
(defun %set-window-width (window new-value)
  (declare (ignore window))
  new-value)

;;; %SET-WINDOW-HEIGHT  --  Internal
;;;
;;; Can't change window height either.
;;;
(defun %set-window-height (window new-value)
  (declare (ignore window))
  new-value)

(defun bitmap-set-foreground-color (window color)
  "Set the foreground color of $window to $color."
  (let* ((hunk (window-hunk window))
	 (display (bitmap-device-display (device-hunk-device hunk)))
	 (gcontext (bitmap-hunk-gcontext hunk)))
    (setf (xlib:gcontext-foreground gcontext)
	  (color-pixel display color))))

(defun bitmap-set-background-color (window color)
  "Set the background color of $window to $color."
  (let* ((hunk (window-hunk window))
	 (display (bitmap-device-display (device-hunk-device hunk)))
	 (gcontext (bitmap-hunk-gcontext hunk)))
    (setf (xlib:gcontext-background gcontext)
	  (color-pixel display color))
    (setf (xlib:window-background (bitmap-hunk-xwindow hunk))
	  (color-pixel display color))))


;;;; Colors

(defun bitmap-make-color (device color)
  (let ((display (bitmap-device-display device)))
    (color-pixel display color)))


;;;; Random Typeout

;;; Random typeout is done to a bitmap-hunk-output-stream
;;; (Bitmap-Hunk-Stream.Lisp).  These streams have an associated hunk
;;; that is used for its font-family, foreground and background color,
;;; and X window pointer.  The hunk is not associated with any editor
;;; window, and the low level painting routines that use hunk dimensions
;;; are not used for output.  The X window is resized as necessary with
;;; each use, but the hunk is only registered for input and boundary
;;; crossing event service; therefore, it never gets exposure or changed
;;; notifications.

;;; These are set in INIT-BITMAP-SCREEN-MANAGER.
;;;
(defvar *random-typeout-start-x* 0
  "Where we put the the random typeout window.")
(defvar *random-typeout-start-y* 0
  "Where we put the the random typeout window.")
(defvar *random-typeout-start-width* 0
  "How wide the random typeout window is.")

;;; DEFAULT-RANDOM-TYPEOUT-HOOK  --  Internal
;;;
;;; The default hook-function for random typeout.  Nothing very fancy for
;;; now.  If not given a window, makes one on top of the initial editor
;;; window using specials set in INIT-BITMAP-SCREEN-MANAGER.  If given a
;;; window, we will change the height subject to the constraint that the
;;; bottom won't be off the screen.  Any resulting window has input and
;;; boundary crossing events selected, an editor cursor defined, and is
;;; mapped.
;;;
(defun default-random-typeout-hook (device window height)
  (declare (fixnum height))
  (let* ((display (bitmap-device-display device))
	 (root (xlib:screen-root (xlib:display-default-screen display)))
	 (full-height (xlib:drawable-height root))
	 (actual-height (if window
			    (multiple-value-bind (x y) (window-root-xy window)
			      (declare (ignore x) (fixnum y))
			      (min (- full-height y xwindow-border-width*2)
				   height))
			    (min (- full-height *random-typeout-start-y*
				    xwindow-border-width*2)
				 height)))
	 (win (cond (window
		     (setf (xlib:drawable-height window) actual-height)
		     window)
		    (t
		     (let ((win (xlib:create-window
				 :parent root
				 :x *random-typeout-start-x*
				 :y *random-typeout-start-y*
				 :width *random-typeout-start-width*
				 :height actual-height
				 :background
				 (color-pixel
				  display
				  (or (value ed::initial-background-color)
				      '(1 1 1))) ; White.
				 :border-width xwindow-border-width
				 :border *default-border-pixmap*
				 :event-mask random-typeout-xevents-mask
				 :override-redirect :on :class :input-output
				 :cursor *editor-cursor*
				 :input ())))
		       (xlib:set-wm-properties
			win :name "Pop-up Display" :icon-name "Pop-up Display"
			:resource-name "Nightshade Editor"
			:x *random-typeout-start-x*
			:y *random-typeout-start-y*
			:width *random-typeout-start-width*
			:height actual-height
			:user-specified-position-p t :user-specified-size-p t
			;; Tell OpenLook pseudo-X11 server we want input.
			:input :on)
		       win))))
	 (gcontext (fi window (default-gcontext win))))
    (values win gcontext)))

(defvar *random-typeout-hook* #'default-random-typeout-hook
  "A function called when random typeout occurs under CLX.

   The function is passed the editor device, a pre-existing CLX window or
   (), and the number of pixels needed to display the number of lines
   requested in the with-pop-up-display form.

   It should return a window, and if a new window is created, then a CLX
   gcontext must be the second value.")

;;; BITMAP-RANDOM-TYPEOUT-SETUP  --  Internal
;;;
;;; This function is called by the with-random-typeout macro to set things
;;; up.  It calls the *Random-Typeout-Hook* to get a window to work with,
;;; and then adjusts the random typeout stream's data-structures to match.
;;;
(defun bitmap-random-typeout-setup (device stream height)
  (let* ((*more-prompt-action* :empty)
	 (hwin-exists-p (random-typeout-stream-window stream))
	 (hwindow (if hwin-exists-p
		      (change-bitmap-random-typeout-window hwin-exists-p height)
		      (setf (random-typeout-stream-window stream)
			    (make-bitmap-random-typeout-window
			     device
			     (buffer-start-mark
			      (line-buffer
			       (mark-line (random-typeout-stream-mark stream))))
			     height)))))
    (let ((xwindow (bitmap-hunk-xwindow (window-hunk hwindow)))
	  (display (bitmap-device-display device)))
      (xlib:display-finish-output display)
      (until ((exit)) (exit)
	(xlib:event-case (display :timeout 0)
	  (:exposure (event-window)
	    (or (equalp event-window xwindow)
		(setq exit t))
	    ())
	  (t ()
	     (setq exit t)))))))

(defun change-bitmap-random-typeout-window (hwindow height)
  (update-modeline-field (window-buffer hwindow) hwindow :more-prompt)
  (let* ((hunk (window-hunk hwindow))
	 (xwin (bitmap-hunk-xwindow hunk)))
    ;;
    ;; *random-typeout-hook* sets the window's height to the right value.
    (funcall *random-typeout-hook* (device-hunk-device hunk) xwin
	     (+ (* height (font-family-height (bitmap-hunk-font-family hunk)))
		hunk-top-border (bitmap-hunk-bottom-border hunk)
		hunk-modeline-top hunk-modeline-bottom))
    (xlib:with-state (xwin)
      (hunk-changed hunk (xlib:drawable-width xwin) (xlib:drawable-height xwin)
		    nil))
    ;;
    ;; We push this on here because we took it out the last time we cleaned up.
    (push hwindow (buffer-windows (window-buffer hwindow)))
    (setf (bitmap-hunk-trashed hunk) t)
    (xlib:map-window xwin)
    ;; A window is only really mapped when it is viewable.  It is said to
    ;; be mapped if a map request has been sent whether it is handled or
    ;; not.
    (loop (when (eq (xlib:window-map-state xwin) :viewable)
	    (return)))
    ;; Clear the window.
    (setf (xlib:window-background xwin)
	  (color-pixel (bitmap-device-display (bitmap-hunk-device hunk))
		       (or (value ed::initial-background-color)
			   '(1 1 1)))) ; White.
    (xlib:clear-window xwin)
    (setf (xlib:window-priority xwin) :above))
  hwindow)

(defun make-bitmap-random-typeout-window (device mark height)
  (format t "(make-bitmap-random-typeout-window . . .)~%")
  (let* ((display (bitmap-device-display device))
	 (hunk (make-bitmap-hunk
		:font-family *default-font-family*
		:end the-sentinel :trashed t
		:input-handler #'window-input-handler
		:device device :thumb-bar-p nil)))
    (multiple-value-bind
	(xwindow gcontext)
	(funcall *random-typeout-hook*
		 device (bitmap-hunk-xwindow hunk)
		 (+ (* height (font-family-height *default-font-family*))
		    hunk-top-border (bitmap-hunk-bottom-border hunk)
		    hunk-modeline-top hunk-modeline-bottom))
      ;;
      ;; When gcontext, we just made the window, so tie some stuff together.
      (when gcontext
	(setf (xlib:gcontext-font gcontext)
	      (svref (font-family-map *default-font-family*) 0))
	(setf (bitmap-hunk-xwindow hunk) xwindow)
	(pushnew (cons xwindow hunk) *xwindow-hunk-map* :test #'equalp)
	(setf (bitmap-hunk-gcontext hunk) gcontext)
	;;
	;; Select input and enable event service before showing the window.
	(setf (xlib:window-event-mask xwindow) random-typeout-xevents-mask)
	(add-xwindow-object xwindow hunk *editor-windows*))
      ;;
      ;; Put the window on the screen so it's visible and we can know the size.
      (xlib:map-window xwindow)
      (xlib:display-finish-output display)
      ;; A window is only really mapped when it is viewable.  It is said to
      ;; be mapped if a map request has been sent whether it is handled or
      ;; not.
      (loop
	(when (eq (xlib:window-map-state xwindow) :viewable)
	  (return)))
      ;; Clear the window.
      (setf (xlib:window-background xwindow)
	    (color-pixel display
			 (or (value ed::initial-background-color)
			     '(1 1 1)))) ; White.
      (xlib:clear-window xwindow)
      (xlib:with-state (xwindow)
	(set-hunk-size hunk (xlib:drawable-width xwindow)
		       (xlib:drawable-height xwindow) t))
      ;;
      ;; Get an editor window and hide it from the rest of the editor.
      (let ((hwin (window-for-hunk hunk mark *random-typeout-ml-fields*)))
	(update-modeline-field (window-buffer hwin) hwin :more-prompt)
	(setf (bitmap-hunk-window hunk) hwin)
	(setf *window-list* (delete hwin *window-list*))
	hwin))))

;;; RANDOM-TYPEOUT-CLEANUP  --  Internal
;;;
;;; Clean up after random typeout.  This just removes the window from the
;;; screen and sets the more-prompt action back to normal.
;;;
(defun bitmap-random-typeout-cleanup (stream degree)
  (when degree
    (xlib:unmap-window (bitmap-hunk-xwindow
			(window-hunk (random-typeout-stream-window stream))))))


;;;; Initialization.

;;; DEFAULT-CREATE-INITIAL-WINDOWS-HOOK makes the initial windows, main and
;;; echo.  The main window is made according to "Default Initial Window X",
;;; "Default Initial Window Y", "Default Initial Window Width", and "Default
;;; Initial Window Height", prompting the user for any unspecified components.
;;; DEFAULT-CREATE-INITIAL-WINDOWS-ECHO is called to return the location and
;;; size of the echo area including how big its font is, and the main xwindow
;;; is potentially modified by this function.  The window name is set to get
;;; around an awm and twm bug that inhibits menu clicks unless the window has a
;;; name; this could be used better.
;;;
(defun default-create-initial-windows-hook (device)
  (let ((root (xlib:screen-root (xlib:display-default-screen
				 (bitmap-device-display device)))))
    (let* ((xwindow (maybe-prompt-user-for-window
		     root
		     (value ed::default-initial-window-x)
		     (value ed::default-initial-window-y)
		     (value ed::default-initial-window-width)
		     (value ed::default-initial-window-height)
		     *default-font-family*
		     t ;modelinep
		     (value ed::thumb-bar-meter)
		     "Nightshade Editor")))
      (setf (xlib:window-border xwindow) *highlight-border-pixmap*)
      (let ((main-win (make-window (buffer-start-mark *current-buffer*)
				   :device device
				   :window xwindow)))
	(multiple-value-bind
	    (echo-x echo-y echo-width echo-height)
	    (default-create-initial-windows-echo
		(xlib:drawable-height root)
		(window-hunk main-win))
	  (format t "echo-x ~A, echo-y ~A, echo-width ~A, echo-height ~A~%"
		  echo-x echo-y echo-width echo-height)
	  (let ((echo-xwin (make-echo-xwindow root echo-x echo-y echo-width
					      echo-height)))
	    (setf *echo-area-window*
		  (elet ((ed::thumb-bar-meter nil))
		    (make-window
		     (buffer-start-mark *echo-area-buffer*)
		     :device device :modelinep t
		     :window echo-xwin)))))
	(setf *current-window* main-win)))))

;;; DEFAULT-CREATE-INITIAL-WINDOWS-ECHO makes the echo area window as wide as
;;; the main window and places it directly under it.  If the echo area does not
;;; fit on the screen, we change the main window to make it fit.  There is
;;; a problem in computing main-xwin's x and y relative to the root window
;;; which is where we line up the echo and main windows.  Some losing window
;;; managers (awm and twm) reparent the window, so we have to make sure
;;; main-xwin's x and y are relative to the root and not some false parent.
;;;
(defun default-create-initial-windows-echo (full-height hunk)
  (declare (fixnum full-height))
  (format t "xwindow-border-width ~A~%" xwindow-border-width)
  (format t "full-height ~A~%" full-height)
  (let ((font-family (bitmap-hunk-font-family hunk))
	(xwindow (bitmap-hunk-xwindow hunk))
	(xparent (window-group-xparent (bitmap-hunk-window-group hunk))))
    (xlib:with-state (xwindow)
      (let ((w (xlib:drawable-width xwindow))
	    (h (xlib:drawable-height xwindow)))
	(declare (fixnum w h))
	(format t "w ~A~%" w)
	(format t "h ~A~%" h)
	(multiple-value-bind (x y)
			     (window-root-xy xwindow
					     (xlib:drawable-x xwindow)
					     (xlib:drawable-y xwindow))
	  (declare (fixnum x y))
	  (format t "x ~A~%" x)
	  (format t "y ~A~%" y)
	  (let* ((ff-height (font-family-height font-family))
		 (ff-width (font-family-width font-family))
		 (echo-height (+ (* ff-height 4)
				 hunk-top-border hunk-bottom-border
				 hunk-modeline-top hunk-modeline-bottom)))
	    (declare (fixnum echo-height))
	    (format t "ff-height ~A~%" ff-height)
	    (format t "ff-width ~A~%" ff-width)
	    (format t "echo-height ~A~%" echo-height)
	    (if (<= (+ y h echo-height xwindow-border-width*2) full-height)
		(values x (+ y h xwindow-border-width*2)
			w echo-height ff-width ff-height)
		(let* ((newh (- full-height y echo-height xwindow-border-width*2
				;; Since y is really the outside y, subtract
				;; two more borders, so the echo area's borders
				;; both appear on the screen.
				xwindow-border-width*2)))
		  (format t "newh ~A~%" newh)
		  (setf (xlib:drawable-height xparent) newh)
		  (values x (+ y newh xwindow-border-width*2)
			  w echo-height ff-width ff-height)))))))))

(defvar *create-initial-windows-hook* #'default-create-initial-windows-hook
  "A function used during screen manager initialization to make the first
   windows, typically the `Main' and `Echo Area' windows.

   It takes an editor device as a required argument.  It sets
   *current-window* and *echo-area-window*.")

(defun make-echo-xwindow (root x y width height)
  (let* ((font-width (font-family-width *default-font-family*))
	 (font-height (font-family-height *default-font-family*)))
    (create-window-with-properties root x y width height
				   font-width font-height
				   "Echo Area" nil nil t)))

(defun init-bitmap-screen-manager (display)
  ;;
  ;; Setup stuff for X interaction.
  (cond ((value ed::reverse-video)
	 (setf *cursor-background-color* (make-black-color display))
	 (setf *cursor-foreground-color* (make-white-color display))
	 (or (value ed::initial-background-color)
	     (setv ed::initial-background-color '(0 0 0))) ; Black.
	 (or (value ed::initial-foreground-color)
	     (setv ed::initial-foreground-color '(1 1 1)))) ; White.
	(t (setf *cursor-background-color* (make-white-color display))
	   (setf *cursor-foreground-color* (make-black-color display))
	   (or (value ed::initial-background-color)
	       (setv ed::initial-background-color '(1 1 1))) ; White.
	   (or (value ed::initial-foreground-color)
	       (setv ed::initial-foreground-color '(0 0 0))))) ; Black.

  (setf *highlight-border-pixmap*
	(xlib:screen-black-pixel (xlib:display-default-screen display)))
  ;(setf *default-border-pixmap* (get-editor-grey-pixmap display))
  (setf *default-border-pixmap* (color-pixel display '(0 0 0)))
  (get-editor-cursor display)
  (add-hook ed::make-window-hook 'define-window-cursor)
  ;;
  ;; Make the device for the rest of initialization.
  (let ((device (make-default-bitmap-device display)))
    (pushnew device *devices*)
    ;;
    ;; Create initial windows.
    (funcall *create-initial-windows-hook* device)
    ;;
    ;; Setup random typeout over the user's main window.
    (let ((xwindow (bitmap-hunk-xwindow (window-hunk *current-window*))))
      (xlib:with-state (xwindow)
	(multiple-value-bind (x y)
			     (window-root-xy xwindow (xlib:drawable-x xwindow)
					     (xlib:drawable-y xwindow))
	  (setf *random-typeout-start-x* x)
	  (setf *random-typeout-start-y* y))
	(setf *random-typeout-start-width* (xlib:drawable-width xwindow)))))
  (add-hook ed::window-buffer-hook 'set-window-name-for-window-buffer)
  (add-hook ed::buffer-name-hook 'set-window-name-for-buffer-name)
  (add-hook ed::set-window-hook 'set-window-hook-raise-fun)
  (add-hook ed::buffer-modified-hook 'raise-echo-area-when-modified))

(defun make-bitmap-device (&rest args)
  (let ((device (apply #'%make-bitmap-device args)))
    device))

(defun make-default-bitmap-device (display)
  (make-bitmap-device
   :name "Windowed Bitmap Device"
   :init #'init-bitmap-device
   :exit #'exit-bitmap-device
   :smart-redisplay #'smart-window-redisplay
   :dumb-redisplay #'dumb-window-redisplay
   :after-redisplay #'bitmap-after-redisplay
   :clear nil
   :note-read-wait #'frob-cursor
   :put-cursor #'hunk-show-cursor
   :show-mark #'bitmap-show-mark
   :next-window #'bitmap-next-window
   :previous-window #'bitmap-previous-window
   :make-window #'bitmap-make-window
   :delete-window #'bitmap-delete-window
   :set-foreground-color #'bitmap-set-foreground-color
   :set-background-color #'bitmap-set-background-color
   :force-output #'bitmap-force-output
   :finish-output #'bitmap-finish-output
   :random-typeout-setup #'bitmap-random-typeout-setup
   :random-typeout-cleanup #'bitmap-random-typeout-cleanup
   :random-typeout-full-more #'do-bitmap-full-more
   :random-typeout-line-more #'update-bitmap-line-buffered-stream
   :beep #'bitmap-beep
   :make-color #'bitmap-make-color
   :display display))

(defun init-bitmap-device (device)
  (let ((display (bitmap-device-display device)))
    (ext:flush-display-events display)
    (editor-window display t)))

(defun exit-bitmap-device (device)
  (editor-window (bitmap-device-display device) nil))

(defun bitmap-finish-output (device window)
  (declare (ignore window))
  (xlib:display-finish-output (bitmap-device-display device)))

(defun bitmap-force-output ()
  (xlib:display-force-output
   (bitmap-device-display (device-hunk-device (window-hunk (current-window))))))

(defun bitmap-after-redisplay (device)
  (let ((display (bitmap-device-display device)))
    (loop (unless (ext:object-set-event-handler display) (return)))))


;;;; Miscellaneous.

;;; HUNK-RESET is called in redisplay to make sure the hunk is up to date.
;;; If the size is wrong, or it is trashed due to font changes, then we
;;; call HUNK-CHANGED.  We also clear the hunk.
;;;
(defun hunk-reset (hunk)
  (let ((xwindow (bitmap-hunk-xwindow hunk))
	(trashed (bitmap-hunk-trashed hunk)))
    (when trashed
      (setf (bitmap-hunk-trashed hunk) nil)
      (xlib:with-state (xwindow)
	(let ((w (xlib:drawable-width xwindow))
	      (h (xlib:drawable-height xwindow)))
	  (when (or (/= w (bitmap-hunk-width hunk))
		    (/= h (bitmap-hunk-height hunk))
		    (eq trashed :font-change))
	    (hunk-changed hunk w h nil)))))
    (xlib:clear-area xwindow :width (bitmap-hunk-width hunk)
		     :height (bitmap-hunk-height hunk))
    (hunk-draw-bottom-border hunk)))

;;; HUNK-CHANGED -- Internal.
;;;
;;; HUNK-RESET and the changed window handler call this.  Don't go through
;;; REDISPLAY-WINDOW-ALL since the window changed handler updates the window
;;; image.
;;;
(defun hunk-changed (hunk new-width new-height redisplay)
  (set-hunk-size hunk new-width new-height)
  (funcall (bitmap-hunk-changed-handler hunk) hunk)
  (when redisplay (dumb-window-redisplay (bitmap-hunk-window hunk))))

;;; WINDOW-GROUP-CHANGED -- Internal.
;;;
;;; HUNK-RECONFIGURED calls this when the hunk was a window-group.  This finds
;;; the windows in the changed group, sorts them by their vertical stacking
;;; order, and tries to resize the windows proportioned by their old sizes
;;; relative to the old group size.  If that fails, this tries to make all the
;;; windows the same size, dividing up the new group's size.
;;;
(defun window-group-changed (window-group new-width new-height)
  (let ((xparent (window-group-xparent window-group))
	(affected-windows nil)
	(count 0)
	(old-xparent-height (window-group-height window-group)))
    (setf (window-group-width window-group) new-width)
    (setf (window-group-height window-group) new-height)
    (dolist (window *window-list*)
      (let ((test (window-group-xparent (bitmap-hunk-window-group
					 (window-hunk window)))))
	(when (eq test xparent)
	  (push window affected-windows)
	  (incf count))))
    ;; Probably should insertion sort them, but I'm lame.
    ;;
    (xlib:with-state (xparent)
      (sort affected-windows #'<
	    :key #'(lambda (window)
		     (xlib:drawable-y
		      (bitmap-hunk-xwindow (window-hunk window))))))
    (let ((start 0))
      (declare (fixnum start))
      (do ((windows affected-windows (cdr windows)))
	  ((endp windows))
	(let* ((xwindow (bitmap-hunk-xwindow (window-hunk (car windows))))
	       (new-child-height (round
				  (* new-height
				     (/ (xlib:drawable-height xwindow)
					old-xparent-height))))
	       (hunk (window-hunk (car windows))))
	  ;; If there is not enough room for one of the windows, space them out
	  ;; evenly so there will be room.
	  ;;
	  (when (< new-child-height (minimum-window-height
				     (font-family-height
				      (bitmap-hunk-font-family hunk))
				     (bitmap-hunk-modeline-pos hunk)
				     (bitmap-hunk-thumb-bar-p hunk)))
	    (reconfigure-windows-evenly affected-windows new-width new-height)
	    (return))
	  (xlib:with-state (xwindow)
	    (setf (xlib:drawable-y xwindow) start
		  ;; Make the last window absorb or lose the number of pixels
		  ;; lost in rounding.
		  ;;
		  (xlib:drawable-height xwindow) (if (cdr windows)
						     new-child-height
						     (- new-height start))
		  (xlib:drawable-width xwindow) new-width
		  start (+ start new-child-height 1))))))))

(defun reconfigure-windows-evenly (affected-windows new-width new-height)
  (let ((count (length affected-windows)))
    (multiple-value-bind
	(pixels-per-window remainder)
	(truncate new-height count)
      (let ((count-1 (1- count)))
	(do ((windows affected-windows (cdr windows))
	     (i 0 (1+ i)))
	    ((endp windows))
	  (let ((xwindow (bitmap-hunk-xwindow (window-hunk (car windows)))))
	    (setf (xlib:drawable-y xwindow) (* i pixels-per-window))
	    (setf (xlib:drawable-width xwindow) new-width)
	    (if (= i count-1)
		(return (setf (xlib:drawable-height
			       (bitmap-hunk-xwindow
				(window-hunk (car windows))))
			      (+ pixels-per-window remainder)))
		(setf (xlib:drawable-height xwindow) pixels-per-window))))))))

;;; SET-HUNK-SIZE  --  Internal
;;;
;;; Given a pixel size for a bitmap hunk, set the char size.  If the window
;;; is too small, we refuse to admit it; if the user makes unreasonably
;;; small windows, our only responsibity is to not blow up.  X will clip
;;; any stuff that doesn't fit.
;;;
(defun set-hunk-size (hunk w h &optional modelinep)
  ;(ed::msg "s . . ~A ~A" h modelinep)
  (let* ((font-family (bitmap-hunk-font-family hunk))
	 (font-width (font-family-width font-family))
	 (font-height (font-family-height font-family)))
    ;(ed::msg "fh ~A" font-height)
    (setf (bitmap-hunk-height hunk) h)
    (setf (bitmap-hunk-width hunk) w)
    (setf (bitmap-hunk-char-width hunk)
	  (max (truncate (- w hunk-left-border) font-width)
	       minimum-window-columns))
    (let* ((h-minus-borders (- h hunk-top-border
			       (bitmap-hunk-bottom-border hunk)))
	   (hwin (bitmap-hunk-window hunk))
	   (modelinep (or modelinep (and hwin (window-modeline-buffer hwin)))))
      ;(ed::msg "hmb ~A" h-minus-borders)
      (setf (bitmap-hunk-char-height hunk)
	    (max (if modelinep
		     (1- (truncate (- h-minus-borders
				      hunk-modeline-top hunk-modeline-bottom)
				   font-height))
		     (truncate h-minus-borders font-height))
		 minimum-window-lines))
      ;(ed::msg "bhch ~A" (bitmap-hunk-char-height hunk))
      (setf (bitmap-hunk-modeline-pos hunk)
	    (if modelinep (- h font-height
			     hunk-modeline-top hunk-modeline-bottom))))))

;;; BITMAP-HUNK-BOTTOM-BORDER -- Internal.
;;;
(defun bitmap-hunk-bottom-border (hunk)
  (if (bitmap-hunk-thumb-bar-p hunk)
      hunk-thumb-bar-bottom-border
      hunk-bottom-border))

;;; DEFAULT-GCONTEXT is used when making hunks.
;;;
(defun default-gcontext (drawable &optional font-family)
  (xlib:create-gcontext
   :drawable drawable
   :foreground (color-pixel (xlib:drawable-display drawable)
			    (or (value ed::initial-foreground-color)
				'(0 0 0)))
   :background (color-pixel (xlib:drawable-display drawable)
			    (or (value ed::initial-background-color)
				'(1 1 1)))
   :font (if font-family (svref (font-family-map font-family) 0))))

;;; WINDOW-ROOT-XY returns the x and y coordinates for a window relative to
;;; its root.  Some window managers reparent the editor's window, so we
;;; have to mess around possibly to get this right.  If x and y are
;;; supplied, they are relative to xwin's parent.
;;;
(defun window-root-xy (xwin &optional x y)
  (multiple-value-bind (children parent root)
		       (xlib:query-tree xwin)
    (declare (ignore children))
    (if (eq parent root)
	(if (and x y)
	    (values x y)
	    (xlib:with-state (xwin)
	      (values (xlib:drawable-x xwin) (xlib:drawable-y xwin))))
	(multiple-value-bind
	    (tx ty)
	    (if (and x y)
		(xlib:translate-coordinates parent x y root)
		(xlib:with-state (xwin)
		  (xlib:translate-coordinates
		   parent (xlib:drawable-x xwin) (xlib:drawable-y xwin) root)))
	  (values (- tx xwindow-border-width)
		  (- ty xwindow-border-width))))))

;;; CREATE-WINDOW-WITH-PROPERTIES makes an X window with parent.  X, y, w, and
;;; h are possibly nil, so we supply zero in this case.  This would be used
;;; for prompting the user.  Some standard properties are set to keep window
;;; managers in line.  We name all windows because awm and twm window managers
;;; refuse to honor menu clicks over windows without names.  Min-width and
;;; min-height are optional and only used for prompting the user for a window.
;;;
(defun create-window-with-properties (parent x y w h font-width font-height
				      icon-name
				      &optional min-width min-height
				      window-group-p)
  (let* ((win (xlib:create-window
	       :parent parent :x (or x 0) :y (or y 0)
	       :width (or w 0) :height (or h 0)
	       :background (if window-group-p
			       :none
			       (color-pixel (xlib:window-display parent)
					    (or (value ed::initial-background-color)
						'(1 1 1)))) ; White.
	       :border-width (if window-group-p xwindow-border-width 0)
	       :border (if window-group-p *default-border-pixmap* nil)
	       :class :input-output
	       :input ())))
    (xlib:set-wm-properties
     win :name (new-editor-window-name) :icon-name icon-name
     :resource-name "Nightshade Editor"
     :x x :y y :width w :height h
     :user-specified-position-p t :user-specified-size-p t
     :width-inc font-width :height-inc font-height
     :min-width min-width :min-height min-height
     ;; Tell OpenLook pseudo-X11 server we want input.
     :input :on)
    win))

;;; SET-WINDOW-HOOK-RAISE-FUN is a "Set Window Hook" function controlled by
;;; "Set Window Autoraise".  When autoraising, check that it isn't only the
;;; echo area window that we autoraise; if it is only the echo area window,
;;; then see if window is the echo area window.
;;;
(defun set-window-hook-raise-fun (window)
  (let ((auto (value ed::set-window-autoraise)))
    (when (and auto
	       (or (not (eq auto :echo-only))
		   (eq window *echo-area-window*)))
      (let* ((hunk (window-hunk window))
	     (win (window-group-xparent (bitmap-hunk-window-group hunk))))
	(xlib:map-window win)
	(setf (xlib:window-priority win) :above)
	(xlib:display-force-output
	 (bitmap-device-display (device-hunk-device hunk)))))))

;;; REVERSE-VIDEO-HOOK-FUN is called when the variable "Reverse Video" is set.
;;; If we are running on a windowed bitmap, we first setup the default
;;; foregrounds and backgrounds.  Having done that, we get a new cursor.  Then
;;; we do over all the hunks, updating their graphics contexts, cursors, and
;;; backgrounds.  The current window's border is given the new highlight pixmap.
;;; Lastly, we update the random typeout hunk and redisplay everything.
;;;
(defun reverse-video-hook-fun (name kind where new-value)
  (declare (ignore name kind where))
  (fi (windowed-monitor-p)
      (tty-reverse-video new-value)
      (progn
	(if (editor-bound-p 'reverse-video)
	    (if (eq new-value (value ed::reverse-video))
		(return-from reverse-video-hook-fun))
	    (or new-value
		(return-from reverse-video-hook-fun)))
	(let ((old-fore (value ed::initial-foreground-color)))
	  (setv ed::initial-foreground-color
		(value ed::initial-background-color))
	  (setv ed::initial-background-color old-fore))
	(let ((old-fore (value ed::initial-modeline-foreground-color)))
	  (setv ed::initial-modeline-foreground-color
		(value ed::initial-modeline-background-color))
	  (setv ed::initial-modeline-background-color old-fore))
	(when *editor-has-been-entered*
	  (let* ((current-window (current-window))
		 (current-hunk (window-hunk current-window))
		 (device (device-hunk-device current-hunk))
		 (display (bitmap-device-display device)))
	    (cond
	     (new-value
	      (setf *cursor-background-color* (make-black-color display))
	      (setf *cursor-foreground-color* (make-white-color display)))
	     (t
	      (setf *cursor-background-color* (make-white-color display))
	      (setf *cursor-foreground-color* (make-black-color display))))
	    (setf *highlight-border-pixmap*
		  (xlib:screen-black-pixel (xlib:display-default-screen display)))
	    (get-editor-cursor display)
	    (dolist (hunk (device-hunks device))
	      (reverse-video-frob-hunk hunk))
	    (dolist (rt-info *random-typeout-buffers*)
	      (reverse-video-frob-hunk
	       (window-hunk (random-typeout-stream-window (cdr rt-info)))))
	    (setf (xlib:window-border (bitmap-hunk-xwindow current-hunk))
		  *highlight-border-pixmap*)))))
  (if *editor-has-been-entered*
      (redisplay-all)))

(defun reverse-video-frob-hunk (hunk)
  (let* ((gcontext (bitmap-hunk-gcontext hunk))
	 (old-fore (xlib:gcontext-foreground gcontext)))
    (setf (xlib:gcontext-foreground gcontext)
	  (xlib:gcontext-background gcontext))
    (setf (xlib:gcontext-background gcontext) old-fore)
    (let ((xwin (bitmap-hunk-xwindow hunk)))
      (setf (xlib:window-cursor xwin) *editor-cursor*)
      (setf (xlib:window-background xwin)
	    (xlib:gcontext-foreground gcontext)))))
