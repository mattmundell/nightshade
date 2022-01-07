;;; Interface to Xlib.
;;
;; This file provides all of the Xlib interface used by the editor.  The
;; functions and structures were created by referring only to the man pages
;; from the libx11-dev Debian package, the Nightshade source code, the Xlib
;; manual and ICCCM.  This code was written many months after flushing the
;; original copyrighted CMUCL Xlib interface from the Nightshade tree.

(in-package "XLIB")

;; FIX
;;(alien:load-foreign "n:build/lisp/xlib.o" :libraries '("-lX11"))

;; FIX
;; somewhere in man pages lists that some functions return err code (instd of signalling err event)

(use-package "ALIEN")
(use-package "C-CALL")
(use-package "SYSTEM")
(use-package "EXT")

(export '(bell
	  bitmap-image
	  ;;
	  boole-0 boole-1 boole-and boole-and-reversed
	  boole-copy boole-and-inverted boole-noop
	  boole-xor boole-or boole-nor boole-equiv
	  boole-invert boole-or-reverse boole-copy-inverted
	  boole-or-inverted boole-nand
	  ;;
	  clear-area clear-window
	  create-gcontext create-pixmap create-cursor
	  create-font-cursor create-window
	  clipboard
	  close-display
	  color-pixel
	  copy-area
	  cut-buffer
	  default-keysym-index
	  destroy-window
	  discard-current-event
	  display display-default-screen display-fd
	  display-finish-output display-force-output
	  display-roots
	  draw-rectangle draw-image-glyphs
	  drawable-x drawable-y drawable-width drawable-height
	  drawable-display
	  event-case event-listen
	  font-ascent font-descent font-path
	  free-cursor free-gcontext free-pixmap
	  get-best-authorization
	  gcontext-background gcontext-foreground gcontext-font
	  gcontext-plane-mask
	  handle-selection-clear handle-selection-request
	  image-x-hot image-y-hot
	  keycode->keysym
	  list-fonts
	  make-color make-event-mask make-state-mask
	  map-window
	  max-char-width
	  name-error
	  open-display open-font
	  process-event
	  put-image
	  query-tree
	  read-bitmap-file
	  screen-black-pixel screen-white-pixel
	  screen-root screen-root-depth
	  screen-width
	  selection
	  set-standard-properties set-selection set-wm-properties
	  translate-default translate-coordinates
	  unmap-window
	  window
	  window-background window-border window-cursor window-display
	  window-event-mask window-id window-map-stat window-name
	  window-priority window-property window-wm-name
	  with-gcontext with-state
	  wm-normal-hints
	  wm-size-hints-height-inc wm-size-hints-min-height
	  wm-size-hints-min-width wm-size-hints-min-width-inc
	  wm-size-hints-min-height-inc
	  wm-size-hints-width-inc wm-size-hints-x wm-size-hints-y))


;;;; Structures.

;; FIX The aliens returned from alien XLIB functions are stored in these
;; structures as integers instead of SAPs (and aliens?).  This is so that
;; least `equalp' works on these types, because SAPs (and aliens?) can only
;; be compared with `sap='.

(defstruct (display (:constructor %make-display))
  (x-display () :type (or integer null))
  ;input-stream
  fd)

;;; Internal
;;;
;;; Return a display structure from SAP $display.
;;;
(defun make-display (display)
  (declare (type system-area-pointer display))
  (%make-display
   :x-display (sap-int display)
   :fd (x-connection-number display)
   #|
   :input-stream (make-fd-stream
		  (x-connection-number display)
		  :buffering :none
		  :input t
		  :auto-close t)
   |#
   ))

(defstruct (drawable)
  ;; FIX Can a drawable be moved to another display?  If so, the
  ;; XWindowAttributes contains a screen field that could be used to
  ;; XDisplayOfScreen, instead of caching the display here.
  (display () :type (or display null))
  (x-drawable () :type (or integer null)))

(defstruct (window (:include drawable))
  ; FIX dummy for window-priority, where is this property in libx11? XRaiseWindow?
  (%priority :above))

(defstruct (gcontext)
  (display () :type (or display null))
  drawable
  (x-gcontext () :type (or integer null)))

(defstruct (cursor)
  (display () :type (or display null))
  (x-cursor () :type (or integer null)))

(defstruct (image)
  x-hot
  y-hot
  width
  height
  data)

(defstruct (pixmap (:include drawable))
  x-hot
  y-hot
  width
  height)

(defstruct (ximage)
  ximage)


;;;; Common X.

(def-alien-routine ("XFree" x-free) int
  (data system-area-pointer)) ; type (* void)


;;;; Display, Screen.

;;; Public.
;;;
(defun get-best-authorization (machine display-num protocol)
  "Return auth name and auth data."
  (declare (ignore machine display-num protocol))
  (values))

(def-alien-routine ("XConnectionNumber" x-connection-number) int
  (display system-area-pointer))

(def-alien-routine ("XSynchronize" x-synchronize) int
  (display system-area-pointer)
  (onoff boolean))

(def-alien-routine ("XSetErrorHandler" x-set-error-handler) int
  (handler (function int (* t) (* t)))) ; type int (*)(Display*, XErrorEvent*)

(def-alien-routine ("XOpenDisplay" x-open-display)
		   system-area-pointer ; type void*
  (display-string c-string))

(def-alien-type ()
  (struct x-error-event
    (type int)
    (display system-area-pointer) ; type Display*
    (serial unsigned-long)
    (error-code unsigned-char)
    (request-code unsigned-char)
    (minor-code unsigned-char)
    (resourceid (* t)))) ; type XID  FIX

(defvar *alien-caller-2-hooks* '(handle-x-error))

(defun serve-alien-caller-2 (one two)
  (loop for hook in *alien-caller-2-hooks* do
    (if (funcall hook one two)
	(return-from serve-alien-caller-2 t))))

(defun handle-x-error (display event)
  (with-alien ((display system-area-pointer ; type Display*
			:local
			display)
	       (event (* (struct x-error-event))
		      :local
		      (sap-alien event (* (struct x-error-event)))))
    (error "x error on display ~A: ~A" event display)))

(def-alien-routine ("handle_xlib_error" x-handle-xlib-error) int
  (display system-area-pointer)
  (event (* t))) ; type XErrorEvent

;;; Public.
;;;
(defun open-display (host-name
		     &key display
		          authorization-name
			  authorization-data)
  "Connect to the X server on $display-number on $host-name.

   Return the display, or () on failure."
  (declare (ignore authorization-data authorization-name))
  (let ((d (x-open-display (format () "~A:~A"
				   (or host-name "")
				   display))))
    (fi* (zerop (sap-int d))
      ;(x-set-error-handler (alien:function x-handle-xlib-error))
      (with-alien ((handle-error (function int (* t) (* t))
				 :extern "handle_xlib_error"))
	(x-set-error-handler handle-error))
      (x-synchronize d t)
      (make-display d))))

(def-alien-routine ("XCloseDisplay" x-close-display) int
  (display system-area-pointer))

;;; Public.
;;;
(defun close-display (display)
  "Close the connection to $display."
  (x-close-display (int-sap (display-x-display display))))

(def-alien-routine ("XScreenOfDisplay" x-screen-of-display)
		   system-area-pointer ; type Screen*
  (display system-area-pointer) ; type Display*
  (number c-call:int))

(def-alien-routine ("XScreenCount" x-screen-count) int
  (display system-area-pointer))

;;; Public.
;;;
(defun display-roots (display)
  "Return the screens of $display as a list."
  (let* ((sap-display (int-sap (display-x-display display)))
	 (count (x-screen-count sap-display)))
    (when count
      (collect ((screens))
	(loop for i from 0 to (1- count) do
	  (screens (sap-int (x-screen-of-display sap-display i))))
	(screens)))))

(def-alien-routine ("XDefaultScreenOfDisplay" x-default-screen-of-display)
		   system-area-pointer ; type Screen*
  (display system-area-pointer))

;; Public
;;
(defun display-default-screen (display)
  "Return the default screen of $display."
  (let ((screen (sap-int
		 (x-default-screen-of-display
		  (int-sap (display-x-display display))))))
    (fi (zerop screen) screen)))

(defun set-display-default-screen (display screen)
  "Set the default screen of $display to $screen."
  (declare (ignore display screen))
#|
  ;; FIX dubious; how to set DefaultScreen(display)?
  (let ((sap-display (int-sap (display-x-display display))))
    (setf (deref (x-default-screen-of-display sap-display))
	  (deref screen))
    )
|#
  )
;;
(defsetf display-default-screen set-display-default-screen
  "Set the default screen of $display to $screen.")

(defun display-finish-output (display)
  ; FIX (x-flush (display-display display))?
  ;(finish-output (display-input-stream display))
  )

(defun display-force-output (display)
  ; FIX (x-sync (display-display display))?
  ;(force-output (display-input-stream display))
  )

#|
(def-alien-routine ("XWidthOfScreen" x-width-of-screen) int
  (display system-area-pointer)) ; type Screen*

;;; Public
;;;
(defun screen-width (screen)
  "Return the width of $screen."
  (x-width-of-screen (int-sap screen)))
|#


;;;; Fonts.

; FIX what type is a font id? ie what type is type Font?
(def-alien-type x-font-id-type int)

(def-alien-type x-time unsigned-long)

(def-alien-routine ("XListFonts" x-list-fonts) (* c-string)
  (display system-area-pointer)
  (pattern c-string)
  (maxnames int)
  (actual_count_return (* int)))

;;; Public
;;;
(defun list-fonts (display &optional (pattern "*") (max 1024))
  "Return a list of the names of the fonts on $display that match
   $pattern, and the number of listed fonts."
  (with-alien ((count int)
	       (pat c-string :local pattern)
	       (list (* c-string)
		     :local
		     (x-list-fonts (int-sap (display-x-display display))
				   pat max (addr count))))
    (fi (null-alien list)
	(collect ((names))
	  (dotimes (time count)
	    (names (deref list time)))
	  (values (names) count)))))

(def-alien-type ()
  (struct x-char
    (lbearing short)
    (rbearing short)
    (width short)
    (ascent short)
    (descent short)
    (attributes unsigned-short)))

(def-alien-type ()
  (struct x-font
    (ext-data (* t)) ; type XExtData
    (fid x-font-id-type)
    (direction unsigned-int)
    (min-char-or-byte2 unsigned-int)
    (max-char-or-byte2 unsigned-int)
    (min-byte1 unsigned-int)
    (max-byte1 unsigned-int)
    (all-chars-exist boolean)
    (default-chars unsigned-int)
    (n-properties int)
    (properties (* t))  ; type XFontProp
    (min-bounds (struct x-char))
    (max-bounds (struct x-char))
    (max-bounds (* (struct x-char)))
    (ascent int)
    (descent int)))

(def-alien-routine ("XLoadQueryFont" x-load-query-font)
		   (* (struct x-font))
  (display system-area-pointer)
  (name c-string))

(def-alien-routine ("XQueryFont" x-query-font)
		   (* (struct x-font))
  (display system-area-pointer)
  (font-id x-font-id-type))

;; FIX org to run on gc? finalize?
(def-alien-routine ("XFreeFont" x-free-font) int
  (display system-area-pointer)
  (font (* (struct x-font))))

(define-condition xlib:name-error ())

;;; Public.
;;;
(defun open-font (display name)
  "Open on $display the font described by string $name, returning the font
   or () on failure."
  (with-alien ((font (* (struct x-font))
		     :local
		     (x-load-query-font
		      (int-sap (display-x-display display))
		      name)))
    (if (null-alien font)
	(throw 'name-error ())
	font)))

;;; Public
;;;
(defun max-char-width (font)
  "Return the maximum width of any character in $font."
  (- (slot (slot font 'max-bounds) 'rbearing)
     (slot (slot font 'min-bounds) 'lbearing)))

;;; Public
;;;
(defun font-ascent (font)
  "Return the ascent of $font."
  (slot font 'ascent))

;;; Public
;;;
(defun font-descent (font)
  "Return the ascent of $font."
  (slot font 'descent))

(def-alien-routine ("XGetFontPath" x-get-font-path)
		   system-area-pointer ; type char**
  (display system-area-pointer)
  (npaths (* int)))

(def-alien-routine ("XFreeFontPath" x-free-font-path) int
  (font (* (* char))))

;;; Public
;;;
(defun font-path (display)
  "Return the font path on $display."
  ; The XGetFontPath manual says the font path strings are implementation
  ; dependent, so what's the correct way to do this?
  (with-alien ((count int)
	       (str c-string)
	       (list (* (* char))
		     :local
		     (x-get-font-path
		      (int-sap (display-x-display display))
		      (addr count))))
    (setf str (deref list))
    (collect ((path))
      (with-alien ((list2 (* char) :local (deref list)))
	(loop repeat count do
	  (setf str list2)
	  (path str)
	  (setq list2
		(sap-alien (int-sap (+ (sap-int (alien-sap list2))
				       (length str)
				       1))
			   (* char)))))
      (x-free-font-path list)
      (path))))

(def-alien-routine ("XSetFontPath" x-set-font-path) int
  (display system-area-pointer)
  (directories (* (* char)))
  (ndirs int))

(defun set-font-path (display pathnames)
  "Set the font path on $display to the pathnames listed in $pathnames.

   Return true on success, else ()."
  (let* ((ndirs (length pathnames))
	 (dirs (make-alien c-string ndirs))
	 (index 0))
    (dolist (pathname pathnames)
      (setf (deref dirs index) pathname)
      (incf index))
    (zerop (x-set-font-path (int-sap (display-x-display display))
			    dirs ndirs))))
;;
(defsetf font-path set-font-path
  "Set the font path on $display to the pathnames listed in $pathnames.

   Return true on success, else ().")


;;;; Colour, more screens.

(def-alien-routine ("XBlackPixelOfScreen" x-black-pixel-of-screen) long
  (screen system-area-pointer))

(defun screen-black-pixel (screen)
  "Return the black pixel value of $screen."
  (x-black-pixel-of-screen (int-sap screen)))

(def-alien-routine ("XWhitePixelOfScreen" x-white-pixel-of-screen) long
  (screen system-area-pointer))

(defun screen-white-pixel (screen)
  "Return the white pixel value of $screen."
  (x-white-pixel-of-screen (int-sap screen)))

(def-alien-type ()
  (struct color
    (pixel  unsigned-long)
    (red    unsigned-short)
    (green  unsigned-short)
    (blue   unsigned-short)
    (flags  char)
    (pad    char)))

;;; Public
;;;
(defun color-pixel (color)
  "Return the pixel value of color."
  (slot color 'pixel))

(def-alien-routine ("XDefaultColormapOfScreen" x-default-colormap-of-screen)
		   system-area-pointer ; type Colormap
  (screen (* t)))

(def-alien-routine ("XAllocColor" x-alloc-color)
		   system-area-pointer ; type Status
  (display system-area-pointer)
  (colormap (* t)) ; type ColorMap
  (screen-in-out (* (struct x-color))))

;;; Public
;;;
(defun make-color (display
		   &key (red 0.0)
		        (green 0.0)
			(blue 0.0))
  "Return an X color."
  (let ((color (make-alien (struct color))))
    (setf (slot color 'red) (truncate (* 65535 red))
	  (slot color 'green) (truncate (* 65535 green))
	  (slot color 'blue) (truncate (* 65535 blue)))
    (let* ((display (int-sap (display-x-display display)))
	   (screen (x-default-screen-of-display display)))
      (x-alloc-color display
		     (x-default-colormap-of-screen
		      screen)
		     (alien-sap color))
      color)))

(def-alien-routine ("XDefaultDepthOfScreen" x-default-depth-of-screen) int
  (screen (* t)))

;;; Public
;;;
(defun screen-root-depth (screen)
  "Return the depth of the root window of $screen."
  (x-default-depth-of-screen (int-sap screen)))

(def-alien-routine ("XRootWindowOfScreen" x-root-window-of-screen)
		   system-area-pointer ; type void*
  (screen system-area-pointer)) ; type Screen*

(def-alien-routine ("XDisplayOfScreen" x-display-of-screen)
		   system-area-pointer ; type void*
  (screen system-area-pointer)) ; type Screen*

;;; Public
;;;
(defun screen-root (screen)
  "Return the root window of $screen."
  (make-window :x-drawable (sap-int (x-root-window-of-screen
				     (int-sap screen)))
	       :display (make-display (x-display-of-screen
				       (int-sap screen)))))


;;;; Bitmaps, Pixmaps.

(def-alien-routine ("XWriteBitmapFile" x-write-bitmap-file) int
  (display system-area-pointer) ; type Display
  (filename c-string)
  (bitmap (* t)) ; type Pixmap
  (width unsigned-int)
  (height unsigned-int)
  (x-hot int)
  (y-hot int))

;;; Public
;;;
(defun write-bitmap-file (pixmap pathname)
  "Write $pixmap to $pathname."
  ;; FIX returns errors
  (x-write-bitmap-file (int-sap (display-x-display
				 (pixmap-display pixmap)))
		       (namestring (os-namestring pathname ()))
		       (int-sap (pixmap-x-drawable pixmap))
		       (pixmap-width pixmap)
		       (pixmap-height pixmap)
		       -1 -1))

(def-alien-routine ("XReadBitmapFile" x-read-bitmap-file) int
  (display system-area-pointer)
  (drawable (* t)) ; type Drawable
  (filename c-string)
  (width-return (* unsigned-int))
  (height-return (* unsigned-int))
  (bitmap-return (* t))
  (x-hot-return (* int))
  (y-hot-return (* int)))

; FIX note in xlib.c
;(def-alien-variable ("BitmapSuccess" x-bitmap-success) int)

;;; Public
;;;
(defun read-bitmap-file (drawable pathname)
  "Read a bitmap from a file."
  ;; FIX returns errors
  (let ((pixmap (create-pixmap :drawable drawable)))
    (with-alien ((width unsigned-int)
		 (height unsigned-int)
		 (data (* unsigned-char))
		 (x-hot int)
		 (y-hot int)
		 (pmap (* t) :local (sap-alien
				     (int-sap (pixmap-x-drawable pixmap))
				     (* t)))
		 (x-pathname c-string :local (namestring
					      (truename pathname))))
      (format t "x-pathname: ~A~%" x-pathname)
      (format t "pmap: ~A~%" pmap)
      (force-output *standard-output*)
      ;; FIX if should compare with x-bitmap-success
      (if (plusp (x-read-bitmap-file (int-sap
				      (display-x-display
				       (drawable-display drawable)))
				     (int-sap
				      (drawable-x-drawable drawable))
				     x-pathname
				     (addr width)
				     (addr height)
				     (addr pmap)
				     (addr x-hot)
				     (addr y-hot)))
	  (progn
	    (free-pixmap pixmap)
	    ())
	  (progn
	    (setf (pixmap-x-hot pixmap) x-hot)
	    (setf (pixmap-y-hot pixmap) y-hot)
	    (setf (pixmap-width pixmap) width)
	    (setf (pixmap-height pixmap) height)
	    pixmap)))))

(def-alien-routine ("XReadBitmapFileData" x-read-bitmap-file-data) int
  (filename c-string)
  (width-return (* unsigned-int))
  (height-return (* unsigned-int))
  (data-return (* unsigned-char))
  (x-hot-return (* int))
  (y-hot-return (* int)))

;;; Public
;;;
(defun read-bitmap-file-data (pathname)
  "Read a bitmap from a file."
  ;; FIX returns errors
  (with-alien ((width unsigned-int)
	       (height unsigned-int)
	       (data (* unsigned-char))
	       (x-hot int)
	       (y-hot int)
	       (x-pathname c-string :local (namestring
					    (truename pathname))))
    ; FIX what allocates data? man says must XFree
    ;         weird for it to be (* u) instd of (* (* u))
    (if (zerop (x-read-bitmap-file-data x-pathname
					(addr width)
					(addr height)
					data
					(addr x-hot)
					(addr y-hot)))
	(make-image :x-hot x-hot
		    :y-hot y-hot
		    :width width
		    :height height
		    :data data))))
; FIX (x-free data); when image gc'd?

;;; Public
;;;
(defun bitmap-image (&rest rows)
  "Return an image created from the bit vectors listed in the args."
  (let* ((bit-len (length (car rows)))
	 (len (multiple-value-bind
		  (res rem)
		  (truncate (length (car rows)) 8)
		(if (plusp rem) (1+ res) res)))
	 (data (make-alien unsigned-char (* (length rows) len)))
	 (data-offset 0)) ; in bytes
    ;; Fill array "data" with the bitmap defined in $rows.
    (loop for row in rows do
      (let ((row-offset 0)) ; in bits
	(loop for byte from 0 to (1- len) do
	  (let ((char 0))
	    ;; Translate the bits to a char code.
	    (loop for bit from 0 to 7
	      while (< (+ row-offset bit) bit-len) do
	      (incf char (expt (bit row (+ row-offset bit)) 2))
	      (incf row-offset))
	    ;; Write the char code to the array "data".
	    (setf (deref data (+ data-offset byte)) char))
	  (incf data-offset len))))
    ;; Create the image.
    (make-image :x-hot 0
		:y-hot 0
		:width bit-len
		:height (length rows)
		:data data)))

(def-alien-routine ("XCreatePixmap" x-create-pixmap)
		   system-area-pointer ; type Pixmap
  (display system-area-pointer) ; type Display*
  (drawable system-area-pointer) ; type Drawable
  (width unsigned-int)
  (height unsigned-int)
  (depth unsigned-int))

;;; Public
;;;
(defun create-pixmap (&key (width 16)
			   (height 16)
			   (depth 16)
			   drawable)
  (or drawable (error "Drawable required."))
  (let* ((display (drawable-display drawable))
	 (pixmap (sap-int (x-create-pixmap
			   (int-sap (display-x-display display))
			   (int-sap (drawable-x-drawable drawable))
			   width height
			   (or depth
			       (let ((screen
				      (x-default-screen-of-display
				       (int-sap
					(display-x-display display)))))
				 (screen-root-depth
				  (sap-int screen))))))))
    (fi (zerop pixmap)
	(make-pixmap :x-drawable pixmap :display display
		     :width width :height height))))

(def-alien-routine ("XFreePixmap" x-free-pixmap) int
  (display system-area-pointer) ; type Display
  (pixmap (* t))) ; type Pixmap

(defun free-pixmap (pixmap)
  (x-free-pixmap (int-sap (display-x-display (pixmap-display pixmap)))
		 (int-sap (pixmap-x-drawable pixmap))))

(def-alien-routine ("XGetImage" x-get-image)
		   system-area-pointer ; type XImage*
  (display system-area-pointer) ; type Display*
  (drawable (* t)) ; type Drawable
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (plane-mask unsigned-long)
  (format int))

(def-alien-variable ("xYBitmap" x-xy-bitmap) int)
(def-alien-variable ("xYPixmap" x-xy-pixmap) int)

;;; Public
;;;
(defun get-image (drawable gcontext
		  &key (x 0) (y 0) (width 1) (height 1))
  "Get an image of $width by $height at $x and $y of $drawable."
  (let ((img (x-get-image (int-sap (display-x-display
				    (drawable-display drawable)))
			  (int-sap (drawable-x-drawable drawable))
			  x y width height
			  (gcontext-plane-mask gcontext)
			  x-xy-pixmap)))
    (format t "img: ~A~%" img)
    (fi (zerop (sap-int img))
	(make-ximage :ximage img))))

(def-alien-routine ("XBitmapPad" x-bitmap-pad) int
  (display system-area-pointer)) ; type Display*

(def-alien-routine ("XBitmapUnit" x-bitmap-unit) int
  (display system-area-pointer)) ; type Display*

(def-alien-routine ("XCreateImage" x-create-image)
		   system-area-pointer ; type XImage*
  (display system-area-pointer) ; type Display*
  (visual system-area-pointer) ; type Visual*
  (depth unsigned-int)
  (format int)
  (offset int)
  (data (* char))
  (width unsigned-int)
  (height unsigned-int)
  (bitmap-pad int)
  (bytes-per-line int))

(def-alien-routine ("XPutImage" x-put-image) int
  (display system-area-pointer) ; type Display*
  (drawable (* t)) ; type Drawable
  (gcontext (* t)) ; type GC
  (image (* t)) ; type Image*
  (src-x int)
  (src-y int)
  (dest-x int)
  (dest-y int)
  (width unsigned-int)
  (height unsigned-int))

(def-alien-routine ("XDefaultVisualOfScreen" x-default-visual-of-screen)
		   system-area-pointer ; type Visual*
  (screen system-area-pointer)) ; type Screen*

(def-alien-routine ("XDestroyImage" x-destroy-image) int
  (Image (* t))) ; type Image

;;; Public
;;;
(defun put-image (drawable gcontext image
		  &key
		  (x 0) (y 0)
		  (width 1) (height 1)
		  (bitmap-p t)) ; FIX
  "Put $width by $height of $image onto $drawable at position $x $y."
  (declare (ignore bitmap-p))
  (let* ((display (int-sap (display-x-display
			    (drawable-display drawable))))
	 (screen (x-default-screen-of-display display)))
    (with-alien ((img (* t) ; type XImage*
		      :local
		      (x-create-image display
				      (x-default-visual-of-screen
				       screen)
				      (screen-root-depth
				       (sap-int screen))
				      x-xy-bitmap
				      0 ; offset
				      (cast (image-data image)
					    (* char))
				      (image-width image)
				      (image-height image)
				      ;; FIX check
				      (x-bitmap-pad display)
				      (multiple-value-bind
					  (res rem)
					  (truncate (x-bitmap-unit
						     display)
						    8)
					(if (plusp rem)
					    (1+ res) res)))))
      (format t "img: ~A~%" img)
      (fi (null-alien img)
	  (progn
	    (x-put-image display
			 (int-sap (drawable-x-drawable drawable))
			 (int-sap (gcontext-x-gcontext gcontext))
			 img
			 0 0
			 x y
			 width height)
	    (format t "put~%")
	    (x-destroy-image img))))))


;;;; Graphics contexts.

(def-alien-variable ("gCForeground" x-gc-foreground) long)
(def-alien-variable ("gCBackground" x-gc-background) long)
(def-alien-variable ("gCFunction" x-gc-function) long)
(def-alien-variable ("gCFont" x-gc-font) long)
(def-alien-variable ("gCPlaneMask" x-gc-plane-mask) long)
(def-alien-variable ("gCGraphicsExposures" x-gc-graphics-exposures)
		    long)

(def-alien-type ()
  (struct x-gc-values
    (function int)
    (plane-mask unsigned-long)
    (foreground unsigned-long)
    (background unsigned-long)
    (line-width int)
    (line-style int)
    (cap-style int)
    (join-style int)
    (fill-style int)
    (fill-rule int)
    (arc-mode int)
    (tile (* t)) ; type Pixmap
    (stipple (* t)) ; type Pixmap
    (ts-x-origin int)
    (ts-y-origin int)
    (font x-font-id-type) ; type Font
    (subwindow-mode int)
    (graphics-exposures boolean)
    (clip-x-origin int)
    (clip-y-origin int)
    (clip-mask (* t)) ; type Pixmap
    (dash-offset int)
    (dash char)))

(def-alien-routine ("XCreateGC" x-create-gc)
		   system-area-pointer ; type GC
  (display system-area-pointer)
  (drawable (* t)) ; type Drawable
  (value-mask unsigned-long)
  (values (* (struct x-gc-values))))

(defvar boole-0             #x0)
(defvar boole-and           #x1)
(defvar boole-and-reversed  #x2)
(defvar boole-copy          #x3)
(defvar boole-and-inverted  #x4)
(defvar boole-noop          #x5)
(defvar boole-xor           #x6)
(defvar boole-or            #x7)
(defvar boole-nor           #x8)
(defvar boole-equiv         #x9)
(defvar boole-invert        #xa)
(defvar boole-or-reverse    #xb)
(defvar boole-copy-inverted #xb)
(defvar boole-or-inverted   #xb)
(defvar boole-nand          #xb)
(defvar boole-1             #xf)

(defvar x-all-planes #xFFFFFFFFFFFFFFFF
  "Mask for all planes.")

;;; Public
;;;
(defun create-gcontext (&key drawable
			     function
			     foreground
			     background
			     font
			     exposures)
  "Create and return a graphics context."
  (or drawable (error "FIX need drawable"))
  (with-alien ((gc-values (struct x-gc-values))
	       (mask unsigned-long :local 0))
    (setf (slot gc-values 'plane-mask) x-all-planes)
    (when foreground
      (setf (slot gc-values 'foreground) foreground)
      (setq mask (logior mask x-gc-foreground)))
    (when background
      (setf (slot gc-values 'background) background)
      (setq mask (logior mask x-gc-background)))
    (when function
      (setf (slot gc-values 'function) function)
      (setq mask (logior mask x-gc-function)))
    (when font
      (setf (slot gc-values 'font)
	    (slot (deref font) 'fid))
      (setq mask (logior mask x-gc-font)))
    (when exposures
      (setf (slot gc-values 'graphics-exposures) exposures)
      (setq mask (logior mask x-gc-graphics-exposures)))
    (let ((display (drawable-display drawable)))
      (make-gcontext :x-gcontext
		     (sap-int (x-create-gc
			       (int-sap (display-x-display display))
			       (int-sap (drawable-x-drawable drawable))
			       mask
			       (addr gc-values)))
		     :display display
		     :drawable drawable))))

(def-alien-routine ("XFreeGC" x-free-gc) int
  (display system-area-pointer)
  (gc (* t))) ; type GC

;;; Public
;;;
(defun free-gcontext (gcontext)
  "Free the graphics context $gcontext."
  (x-free-gc (int-sap (display-x-display (gcontext-display gcontext)))
	     (int-sap (gcontext-x-gcontext gcontext))))

;;; Public
;;;
(defmacro with-gcontext ((gcontext
			  &key exposures
			       foreground
			       background
			       font
			       function)
			 &body body)
  (let ((gc-values (gensym)))
    `(with-alien ((,gc-values (struct x-gc-values)))
       (x-get-gc-values (int-sap (display-x-display
				  (gcontext-display ,gcontext)))
			(int-sap (gcontext-x-gcontext ,gcontext))
			(logior x-gc-background
				x-gc-foreground
				x-gc-font
				x-gc-function)
			(addr ,gc-values))
       (let ((,gcontext (create-gcontext
			 :drawable (gcontext-drawable ,gcontext)
			 :foreground (or ,foreground
					 (slot ,gc-values 'foreground))
			 :background (or ,background
					 (slot ,gc-values 'background))
			 :font (or ,font
				   (x-query-font
				    (int-sap (display-x-display
					      (gcontext-display
					       ,gcontext)))
				    (slot ,gc-values 'font)))
			 :exposures (or ,exposures
					(slot ,gc-values
					      'graphics-exposures))
			 :function (or ,function
				       (slot ,gc-values 'function)))))
	 (unwind-protect
	     (progn ,@body)
	   (free-gcontext ,gcontext))))))

(def-alien-routine ("XGetGCValues" x-get-gc-values)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display*
  (gc system-area-pointer) ; type GC
  (valuemask unsigned-long)
  (values-return (* t))) ; type XGCValues

(def-alien-routine ("XChangeGC" x-change-gc) int
  (display system-area-pointer) ; type Display*
  (gc system-area-pointer) ; type GC
  (valuemask unsigned-long)
  (values (* t))) ; type XGCValues

;;; Public
;;;
(defun gcontext-background (gcontext)
  (with-alien ((gc-values (struct x-gc-values)))
    (x-get-gc-values (int-sap (display-x-display
			       (gcontext-display gcontext)))
		     (int-sap (gcontext-x-gcontext gcontext))
		     x-gc-background
		     (addr gc-values))
    (slot gc-values 'background)))

(defun set-gcontext-background (gcontext background)
  "Set the background in $gcontext to $background."
  (with-alien ((gc-values (struct x-gc-values)))
    (setf (slot gc-values 'background) background)
    (x-change-gc (int-sap (display-x-display
			   (gcontext-display gcontext)))
		 (int-sap (gcontext-x-gcontext gcontext))
		 x-gc-background
		 (addr gc-values))))
;;
(defsetf gcontext-background set-gcontext-background
  "Set the background in $gcontext to $background.")

;;; Public
;;;
(defun gcontext-foreground (gcontext)
  (with-alien ((gc-values (struct x-gc-values)))
    (x-get-gc-values (int-sap (display-x-display
			       (gcontext-display gcontext)))
		     (int-sap (gcontext-x-gcontext gcontext))
		     x-gc-foreground
		     (addr gc-values))
    (slot gc-values 'foreground)))

(defun set-gcontext-foreground (gcontext foreground)
  "Set the foreground in $gcontext to $foreground."
  (with-alien ((gc-values (struct x-gc-values)))
    (setf (slot gc-values 'foreground) foreground)
    (x-change-gc (int-sap (display-x-display
			   (gcontext-display gcontext)))
		 (int-sap (gcontext-x-gcontext gcontext))
		 x-gc-foreground
		 (addr gc-values))))
;;
(defsetf gcontext-foreground set-gcontext-foreground
  "Set the foreground in $gcontext to $foreground.")

;;; Public
;;;
(defun gcontext-font (gcontext)
  (with-alien ((gc-values (struct x-gc-values)))
    (x-get-gc-values (int-sap (display-x-display
			       (gcontext-display gcontext)))
		     (int-sap (gcontext-x-gcontext gcontext))
		     x-gc-font
		     (addr gc-values))
    (x-query-font
     (int-sap (display-x-display
	       (gcontext-display
		gcontext)))
     (slot gc-values 'font))))

(defun set-gcontext-font (gcontext font)
  (with-alien ((gc-values (struct x-gc-values)))
    (setf (slot gc-values 'font)
	  (slot (deref font) 'fid))
    (x-change-gc (int-sap (display-x-display
			   (gcontext-display gcontext)))
		 (int-sap (gcontext-x-gcontext gcontext))
		 x-gc-font
		 (addr gc-values))))
;;
(defsetf gcontext-font set-gcontext-font
  "Set the font in $gcontext to $font.")

;;; Public
;;;
(defun gcontext-plane-mask (gcontext)
  (with-alien ((gc-values (struct x-gc-values)))
    (x-get-gc-values (int-sap (display-x-display
			       (gcontext-display gcontext)))
		     (int-sap (gcontext-x-gcontext gcontext))
		     x-gc-plane-mask
		     (addr gc-values))
    (slot gc-values 'plane-mask)))


;;;; Events structures.

(def-alien-type ()
  (struct x-any-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer))) ; type Window

(def-alien-type ()
  (struct x-button-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (root (* t)) ; type Window
    (child (* t)) ; aka subwindow; type Window
    (time x-time) ; type Time
    (x int)
    (y int)
    (root-x int)
    (root-y int)
    (state unsigned-int)
    (button unsigned-int)
    (same-screen-p boolean)))

(def-alien-type x-button-released-event (struct x-button-event))
(def-alien-type x-button-pressed-event (struct x-button-event))

(def-alien-type ()
  (struct x-key-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (root (* t)) ; type Window
    (child (* t)) ; aka subwindow; type Window
    (time x-time) ; type Time
    (x int)
    (y int)
    (root-x int)
    (root-y int)
    (state unsigned-int)
    (code unsigned-int) ; a.k.a keycode
    (same-screen-p boolean)))

(def-alien-type x-key-released-event (struct x-key-event))

(def-alien-type ()
  (struct x-motion-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (root (* t)) ; type Window
    (child (* t)) ; aka subwindow; type Window
    (time x-time) ; type Time
    (x int)
    (y int)
    (root-x int)
    (root-y int)
    (state unsigned-int)
    (hint-p char)
    (same-screen-p boolean)))

(def-alien-type x-pointer-moved-event (struct x-motion-event))

(def-alien-type ()
  (struct x-crossing-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (root (* t)) ; type Window
    (child (* t)) ; aka subwindow; type Window
    (time x-time) ; type Time
    (x int)
    (y int)
    (root-x int)
    (root-y int)
    (mode int)
    (detail int)
    (same-screen-p boolean)
    (focus boolean)
    (state unsigned-int)))

(def-alien-type x-enter-notify-event (struct x-crossing-event))
(def-alien-type x-leave-notify-event (struct x-crossing-event))

(def-alien-type ()
  (struct x-expose-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (x int)
    (y int)
    (width int)
    (height int)
    (count int)))

(def-alien-type ()
  (struct x-graphics-expose-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; aka drawable, type Drawable
    (x int)
    (y int)
    (width int)
    (height int)
    (count int)
    (major int) ; aka major-code
    (minor int))) ; aka minor-code

(def-alien-type ()
  (struct x-no-expose-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; aka drawable, type Drawable
    (x int)
    (y int)
    (width int)
    (height int)
    (count int)
    (major int) ; aka major-code
    (minor int))) ; aka minor-code

(def-alien-type ()
  (struct x-focus-change-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (mode int)
    (detail int)))

(def-alien-type x-focus-in-event (struct x-focus-change-event))
(def-alien-type x-focus-out-event (struct x-focus-change-event))

(def-alien-type ()
  (struct x-keymap-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (key-vector (array char 32))))

(def-alien-type ()
  (struct x-visibility-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (state int)))

(def-alien-type ()
  (struct x-create-window-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (parent (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (x int)
    (y int)
    (width int)
    (height int)
    (border-width int)
    (override-redirect-p boolean)))

(def-alien-type ()
  (struct x-destroy-window-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer))) ; type Window

(def-alien-type ()
  (struct x-unmap-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (configure-p boolean)))

(def-alien-type ()
  (struct x-map-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (override-redirect-p boolean)))

(def-alien-type ()
  (struct x-map-request-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (parent (* t)))) ; type Window

(def-alien-type ()
  (struct x-reparent-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (parent (* t)) ; type Window
    (x int)
    (y int)
    (override-redirect-p boolean)))

(def-alien-type ()
  (struct x-configure-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (x int)
    (y int)
    (width int)
    (height int)
    (border-width int) ; aka border-height
    (above-sibling (* t)) ; type Window
    (override-redirect-p boolean)))

(def-alien-type ()
  (struct x-gravity-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (x int)
    (y int)))

(def-alien-type ()
  (struct x-resize-request-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (width int)
    (height int)))

(def-alien-type ()
  (struct x-configure-request-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (x int)
    (y int)
    (width int)
    (height int)
    (border-width int) ; aka border-height
    (above-sibling (* t)) ; type Window
    (stack-mode int) ; aka detail
    (value-mask unsigned-long)))

(def-alien-type ()
  (struct x-circulate-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (place int)))

(def-alien-type ()
  (struct x-circulate-request-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (event (* t)) ; type Window
    (window system-area-pointer) ; type Window
    (place int)))

(def-alien-type ()
  (struct x-property-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (atom system-area-pointer) ; type Atom
    (time x-time) ; type Time
    (state int)))

(def-alien-type ()
  (struct x-selection-clear-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (selection system-area-pointer) ; type Atom
    (time x-time))) ; type Time

(def-alien-type ()
  (struct x-selection-request-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; aka owner, type Window
    (requestor system-area-pointer) ; type Window
    (selection system-area-pointer) ; type Atom
    (target system-area-pointer) ; type Atom
    (property system-area-pointer) ; type Atom
    (time x-time))) ; type Time

; For x-selection-notify.
(def-alien-type ()
  (struct x-selection-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; aka requestor, type Window
    (selection system-area-pointer) ; type Atom
    (target system-area-pointer) ; type Atom
    (property system-area-pointer) ; type Atom
    (time x-time)))

(def-alien-type ()
  (struct x-color-map-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (colormap (* t)) ; type Colormap
    (new-p boolean)
    (installed-p int))) ; FIX was state

(def-alien-type ()
  (struct x-mapping-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (request int)
    (first-keycode int)
    (count int)))

(def-alien-type ()
  (union x-c-m-e-data
    (array char 20)
    (array short 10)
    (array long 5)))

(def-alien-type ()
  (struct x-client-message-event
    (type int)
    (serial unsigned-long)
    (send-event-p boolean)
    (display system-area-pointer) ; type Display
    (window system-area-pointer) ; type Window
    (message-type (* t)) ; type Atom
    (format int)
    (data (union x-c-m-e-data))))

(def-alien-type ()
  (union x-event
    (type int)
    (xany (struct x-any-event))
    (xkey (struct x-key-event))
    (xbutton (struct x-button-event))
    (xmotion (struct x-motion-event))
    (xcrossing (struct x-crossing-event))
    (xfocus (struct x-focus-change-event))
    (xexpose (struct x-expose-event))
    (xgraphicsexpose (struct x-graphics-expose-event))
    (xnoexpose (struct x-no-expose-event))
    (xvisibility (struct x-visibility-event))
    (xcreatewindow (struct x-create-window-event))
    (xdestroywindow (struct x-destroy-window-event))
    (xunmap (struct x-unmap-event))
    (xmap (struct x-map-event))
    (xmaprequest (struct x-map-request-event))
    (xreparent (struct x-reparent-event))
    (xconfigure (struct x-configure-event))
    (xgravity (struct x-gravity-event))
    (xresizerequest (struct x-resize-request-event))
    (xconfigurerequest (struct x-configure-request-event))
    (xcirculate (struct x-circulate-event))
    (xcirculaterequest (struct x-circulate-request-event))
    (xproperty (struct x-property-event))
    (xselectionclear (struct x-selection-clear-event))
    (xselectionrequest (struct x-selection-request-event))
    (xselection (struct x-selection-event))
    (xcolormap (struct x-color-map-event))
    (xclient (struct x-client-message-event))
    (xmapping (struct x-mapping-event))
    (xerror (struct x-error-event))
    (xkeymap (struct x-keymap-event))
    (pad (array long 24))))

(def-alien-variable ("keyPress" x-key-press) int)
(def-alien-variable ("keyRelease" x-key-release) int)
(def-alien-variable ("buttonPress" x-button-press) int)
(def-alien-variable ("buttonRelease" x-button-release) int)
(def-alien-variable ("motionNotify" x-motion-notify) int)
(def-alien-variable ("enterNotify" x-enter-notify) int)
(def-alien-variable ("leaveNotify" x-leave-notify) int)
(def-alien-variable ("exposure" x-exposure) int)
(def-alien-variable ("graphicsExposure" x-graphics-exposure) int)
(def-alien-variable ("noExposure" x-no-exposure) int)
(def-alien-variable ("focusIn" x-focus-in) int)
(def-alien-variable ("focusOut" x-focus-out) int)
(def-alien-variable ("keymapNotify" x-keymap-notify) int)
(def-alien-variable ("visibilityNotify" x-visibility-notify) int)
(def-alien-variable ("createNotify" x-create-notify) int)
(def-alien-variable ("destroyNotify" x-destroy-notify) int)
(def-alien-variable ("unmapNotify" x-unmap-notify) int)
(def-alien-variable ("mapNotify" x-map-notify) int)
(def-alien-variable ("mapRequest" x-map-request) int)
(def-alien-variable ("reparentNotify" x-reparent-notify) int)
(def-alien-variable ("configureNotify" x-configure-notify) int)
(def-alien-variable ("gravityNotify" x-gravity-notify) int)
(def-alien-variable ("resizeRequest" x-resize-request) int)
(def-alien-variable ("configureRequest" x-configure-request) int)
(def-alien-variable ("circulateNotify" x-circulate-notify) int)
(def-alien-variable ("circulateRequest" x-circulate-request) int)
(def-alien-variable ("propertyRequest" x-property-notify) int)
(def-alien-variable ("selectionClear" x-selection-clear) int)
(def-alien-variable ("selectionRequest" x-selection-request) int)
(def-alien-variable ("selectionNotify" x-selection-notify) int)
(def-alien-variable ("colormapNotify" x-colormap-notify) int)
(def-alien-variable ("mappingNotify" x-mapping-notify) int)
(def-alien-variable ("clientMessage" x-client-message) int)


;;;; Event masks.

(def-alien-variable ("keyPressMask" x-key-press-mask) int)
(def-alien-variable ("keyReleaseMask" x-key-release-mask) int)
(def-alien-variable ("buttonPressMask" x-button-press-mask) int)
(def-alien-variable ("buttonReleaseMask" x-button-release-mask) int)
(def-alien-variable ("enterWindowMask" x-enter-window-mask) int)
(def-alien-variable ("leaveWindowMask" x-leave-window-mask) int)
(def-alien-variable ("exposureMask" x-exposure-mask) int)

(def-alien-variable ("substructureRedirectMask"
		     x-substructure-redirect-mask) int)
(def-alien-variable ("substructureNotifyMask"
		     x-substructure-notify-mask) int)
(def-alien-variable ("structureNotifyMask" x-structure-notify-mask) int)
(def-alien-variable ("resizeRedirectMask" x-resize-redirect-mask) int)

;(defvar x-key-press-mask              #b0000000000000000000001)
;(defvar x-key-release-mask            #b0000000000000000000010)
;(defvar x-button-press-mask           #b0000000000000000000100)
;(defvar x-button-release-mask         #b0000000000000000001000)
;(defvar x-enter-window-mask           #b0000000000000000010000)
;(defvar x-leave-window-mask           #b0000000000000000100000)
 (defvar x-motion-notify-mask          #b0000000000000001000000)
;(defvar x-FIX-mask                    #b0000000000000010000000)
;(defvar x-FIX-mask                    #b0000000000000100000000)
;(defvar x-FIX-mask                    #b0000000000001000000000)
;(defvar x-FIX-mask                    #b0000000000010000000000)
;(defvar x-FIX-mask                    #b0000000000100000000000)
;(defvar x-FIX-mask                    #b0000000001000000000000)
;(defvar x-FIX-mask                    #b0000000010000000000000)
 (defvar x-keymap-notify-mask          #b0000000100000000000000)
;(defvar x-exposure-mask               #b0000001000000000000000)
;(defvar x-FIX-mask                    #b0000010000000000000000)
;(defvar x-structure-notify-mask       #b0000100000000000000000)
 (defvar x-visibility-notify-mask      #b0001000000000000000000)
 (defvar x-resize-redirect-mask        #b0001000000000000000000) ; FIX
;(defvar x-keymap-notify-mask          #b0010000000000000000000)
;(defvar x-substructure-redirect-mask  #b0100000000000000000000)
 (defvar x-focus-change-mask           #b1000000000000000000000)

; structure-notify produces configure-notify, destroy-notify,
;                           map-notify, unmap-notify, reparent-notify,
;                           gravity-notify, circulate-notify,
;                           client-message and property-notify.


;;;; Events.

(def-alien-routine ("XSync" x-sync) int
  (display system-area-pointer) ; type Display*
  (discard boolean))

(defvar *release-current-event* ()
  "Bound to () around event handler calls.  If true afterwards the event is
   left off the X queue (if it was going to be returned to the queue at
   all).")

;;; Public
;;;
(defun discard-current-event (display)
  "Throw away the current event on $display."
  (declare (ignore display))
  (setq *release-current-event* t))

(def-alien-routine ("XEventMaskOfScreen" x-event-mask-of-screen) long
  (screen system-area-pointer)) ; type Screen

(def-alien-routine ("XCheckMaskEvent" x-check-mask-event) long
  (display system-area-pointer) ; type Display*
  (mask long)
  (event (* (union x-event))))

(defvar *event-map*
  `((,x-key-press      :key-press      (struct x-key-event) ,x-key-press-mask)
    (,x-key-release    :key-release    (struct x-key-event) ,x-key-release-mask)
    (,x-button-press   :button-press   (struct x-button-event) ,x-button-press-mask)
    (,x-button-release :button-release (struct x-button-event) ,x-button-release-mask)
    (,x-motion-notify  :motion-notify  (struct x-motion-event) ,x-motion-notify-mask)
    (,x-enter-notify   :enter-notify   (struct x-crossing-event) ,x-enter-window-mask)
    (,x-enter-notify   :enter-window   (struct x-crossing-event) ,x-enter-window-mask)
    (,x-leave-notify   :leave-notify   (struct x-crossing-event) ,x-leave-window-mask)
    (,x-leave-notify   :leave-window   (struct x-crossing-event) ,x-leave-window-mask)
    (,x-exposure       :exposure       (struct x-expose-event) ,x-exposure-mask)
    (,x-graphics-exposure :graphics-exposure
			  (struct x-graphics-expose-event)
			  ,x-exposure-mask)
    (,x-no-exposure    :no-exposure    (struct x-no-expose-event))
    (,x-focus-in       :focus-in       (struct x-focus-change-event)
		       ,x-focus-change-mask)
    (,x-focus-out      :focus-out      (struct x-focus-change-event)
		       ,x-focus-change-mask)
    (,x-keymap-notify  :keymap-notify  (struct x-keymap-event)
		       ,x-keymap-notify-mask)
    (,x-visibility-notify :visibility-notify
			  (struct x-visibility-event)
			  ,x-visibility-notify-mask)
    (,x-create-notify  :create-notify  (struct x-create-window-event))
    (,x-destroy-notify :destroy-notify (struct x-destroy-window-event))
    (,x-unmap-notify   :unmap-notify   (struct x-unmap-event))
    (,x-map-notify     :map-notify     (struct x-map-event))
    (,x-map-request    :map-request    (struct x-map-request-event))
    (,x-reparent-notify :reparent-notify (struct x-reparent-event))
    (,x-configure-notify :configure-notify (struct x-configure-event)
			 ,x-structure-notify-mask)
    ;; FIX from e:bit-screen.lisp:
    ;;   :configure-notify events are sent because we select :structure-notify.
    ;;   This buys us a lot of events we have to write dummy handlers to ignore.
    ;; (destroy-notify unmap-notify map-notify reparent-notify
    ;;  gravity-notify circulate-notify client-message)
    (,x-configure-notify :structure-notify (struct x-configure-event)
			 ,x-structure-notify-mask)
    (,x-circulate-notify :circulate-notify (struct x-circulate-event)
			 ,x-substructure-redirect-mask)
    (,x-circulate-request :circulate-request
			  (struct x-circulate-request-event)
			  ,x-substructure-redirect-mask)
    (,x-gravity-notify :gravity-notify (struct x-gravity-event))
    (,x-resize-request :resize-request (struct x-resize-request-event))
    (,x-configure-request :configure-request
			  (struct x-configure-request-event))
    (,x-circulate-request :circulate-request
			  (struct x-circulate-request-event))
    (,x-property-notify :property-notify (struct x-property-event))
    (,x-selection-clear :selection-clear
			(struct x-selection-clear-event))
    (,x-selection-request :selection-request
			  (struct x-selection-request-event))
    (,x-selection-notify :selection-notify
			 (struct x-selection-event))
    (,x-colormap-notify :colormap-notify
			(struct x-color-map-event))
    (,x-mapping-notify  :mapping-notify
		        (struct x-mapping-event))
    (,x-client-message  :client-message
		        (struct x-client-message-event)))
  "Alist mapping event types to `event-case' clause symbols.")

(defun key-from-type (type)
  "Return the `event-case' clause keyword associated with event $type."
  (let ((assoc (assoc type *event-map*)))
    (if assoc (values (cadr assoc) (caddr assoc)))))

(defun type-from-key (key)
  "Return the `event-case' clause keyword associated with event $type."
  (car (rassoc key *event-map* :key #'car)))

(defun alien-type-from-key (key)
  "Return the alien type associated with `event-case' clause $key."
  (caddr (rassoc key *event-map* :key #'car)))

(defun event-mask-from-key (key)
  "Return the alien type associated with `event-case' clause $key."
  (cadddr (rassoc key *event-map* :key #'car)))

;;; Public
;;;
(defun make-event-mask (&rest events)
  "Return an event mask to mask the events listed in $events."
  (apply #'logior
	 (mapcar (lambda (event)
		   (or (event-mask-from-key event)
		       ;; FIX
		       0
		       #|
		       (error "Failed to find mask of event ~A"
			      event)
		       |#
		       ))
		 events)))

(def-alien-routine ("XSelectInput" x-select-input) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (event-mask long))

;;; Internal
;;;
;;; Alist mapping windows to event masks.
;;;
(defvar *window-event-masks* ())

;;; Public
;;;
(defun window-event-mask (window)
  "Return the event mask of $window."
  (cdr (assoc window *window-event-masks*)))
;;
(defun set-window-event-mask (window mask)
  "Request $window to select on the events in $mask."
  (x-select-input (int-sap (display-x-display (window-display window)))
		  (int-sap (window-x-drawable window))
		  mask)
  (pushnew (cons window mask) *window-event-masks*))
;;
(defsetf window-event-mask set-window-event-mask
  "Request $window to select on the events in $mask.")

(def-alien-routine ("XPutBackEvent" x-put-back-event) void
  (display system-area-pointer) ; type Display*
  (event (* t))) ; type XEvent*

(defvar *event-case-slots* '((:key-press . (root subwindow
					    same-screen-p x y
					    root-x root-y state
					    time code send-event-p)))
  "Alist mapping event keys to correctly ordered slot names passed to
   handling cases in `event-case'.")

(defun nth-slot (key n)
  "Return the $n'th slot name for event type $key."
  (if (minusp n) (error "$n must be > 0."))
  (case n
    (0 (error "Position 0 is for the key variable"))
    (1 'window)
    (nth (- n 2)
	 (cdr (assoc *event-case-slots* key)))))

(def-alien-routine ("XNextEvent" x-next-event) int
  (display system-area-pointer) ; type Display*
  (event-return (* (union x-event))))

;;; Public
;;;
(defmacro event-case ((display &key discard-p timeout) &body cases)
  "event-case (display [:option value]*)
     {({(event*) | event} ({slot}*) form*)}*

   A `case' macro for handling the next event on $display.

   Evaluate the forms in the first clause with an event `eql' to the next
   event that arrives on $display, binding each symbol named in slot to the
   value of the slot with that name in the event.  If a singleton event is
   t then make that a fallback clause.

   Wait up to $timeout seconds for the next event, forever if $timeout is
   ()."
  (collect ((clauses) (keys))
    (let ((event (gensym)) (key-var (gensym)) (result (gensym)) otherwise-p)
      (flet ((collect-clause (key slots forms)
	       (collect ((slot-lets))
		 (loop for slot in slots
		   for n from 1 do
		   ;; Guessing that "slot" is the name of the slot in the
		   ;; event structure (or 'event-window or 'event-key).
		   ;; Seems rather bogus.
		   (slot-lets
		    (case (intern (symbol-name slot) "XLIB")
		      (xlib::event-window
		       `(,slot
			 (make-window
				  :x-drawable
				  (sap-int
				   (slot (deref ,event)
					 ',(intern "WINDOW" "XLIB")))
				  :display ,display)))
		      (xlib::event-key
		       `(,slot ,key-var))
		      (t
		       `(,slot (slot (deref ,event)
				     ',(intern (symbol-name slot)
					       "XLIB")))))))
		 (clauses
		  (if (slot-lets)
		      `(,key
			(force-output)
			(let ((,event
			       (cast
				(addr ,event)
				(* ,(if (eq key t)
					(list 'struct 'x-any-event)
					(or (alien-type-from-key
					     (if (listp key)
						 (car key)
						 key))
					    (error "Failed to find type for ~A"
						   key)))))))
			  (let ,(slot-lets)
			    ;(format t "key 1 was ~A~%" ',key)
			    ,@forms)))
		      `(,key
			;(format t "key 2 was ~A~%" ',key)
			,@forms))))))
	(loop for case in cases do
	  (if (eq (car case) t)
	      (setq otherwise-p t)
	      (keys (if (listp (car case))
			(car case)
			(list (car case)))))
	  ;(format t "collecting clause ~A~%" case)
	  (collect-clause (car case) (cadr case) (cddr case))))
      (let ((x-call (if (or otherwise-p
			    ;; FIX in these cases x-call must get events until one matches a key
			    ;;     and if they're the only clauses mask with mask ()
			    (let ((keylist (apply #'append (keys))))
			      (or (member :selection-request keylist)
				  (member :selection-notify keylist))))
			;; Any type of event.
			`(if (event-listen ,display)
			     (progn
			       ;(warn "next-event")
			       (let ((ret (x-next-event
					   (int-sap (display-x-display
						     ,display))
					   (addr ,event))))
				 ;(warn "ret: ~A" ret)
				 ret)
			       1)
			     0)
			;; Some of the types of events.
			`(x-check-mask-event
			  (int-sap (display-x-display ,display))
			  (xlib::make-event-mask ,@(apply #'append (keys)))
			  (addr ,event)))))
	`(progn
	   (if (if ,timeout (plusp ,timeout) t)
	       (error "FIX implement waiting"))
	   (with-alien ((,event (union x-event)))
	     ; FIX
	     ;(declaim (inline x-check-mask-event x-put-back-event
	     ;                 x-event-mask-of-screen display-default-screen))?
	     (fi (zerop ,x-call)
		 (let* ((,key-var (progn
#|
				    (format t "event type: ~A~%"
					    (slot ,event 'type))
|#
				    (key-from-type (slot ,event 'type))))
			(*release-current-event*)
			(,result (progn
				   ;(format t ",key-var: ~A~%" ,key-var)
				   (,(if otherwise-p 'case 'ecase)
				    ,key-var
				    ,@(clauses)))))
		   ;(format t "result: ~A~%" ,result)
		   (or ,discard-p
		       *release-current-event*
		       ,result
		       (progn
			 (x-put-back-event
			  (int-sap (display-x-display ,display))
			  (addr ,event))))
		   t))))))))

(defun handle-event (event handler)
  "Call $handler on the slots of $event according to the type of $event."
  (case (slot event 'type)
    ((#.x-key-press #.x-key-release)
     (with-alien ((event (struct x-key-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    (if (eq (slot event 'type) x-key-press)
				  :key-press
				  :key-release)
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:window       (slot event 'window)
		:root         (slot event 'root)
		:subwindow    (slot event 'child)
		:time         (slot event 'time)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:root-x       (slot event 'root-x)
		:root-y       (slot event 'root-y)
		:state        (slot event 'state)
		:code         (slot event 'code)
		:same-screen-p  (slot event 'same-screen-p))))
    ((#.x-button-press #.x-button-release)
     (with-alien ((event (struct x-button-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    (if (eq (slot event 'type) x-button-press)
				  :button-press
				  :button-release)
		:window       (slot event 'window)
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:window       (slot event 'window)
		:root         (slot event 'root)
		:subwindow    (slot event 'child)
		:time         (slot event 'time)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:root-x       (slot event 'root-x)
		:root-y       (slot event 'root-y)
		:state        (slot event 'state)
		:button       (slot event 'button)
		:same-screen-p  (slot event 'same-screen-p))))
    (#.x-motion-notify
     (with-alien ((event (struct x-motion-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :motion-notify
		:window       (slot event 'window)
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:root         (slot event 'root)
		:subwindow    (slot event 'child)
		:time         (slot event 'time)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:root-x       (slot event 'root-x)
		:root-y       (slot event 'root-y)
		:state        (slot event 'state)
		:hint-p       (slot event 'hint-p)
		:same-screen-p  (slot event 'same-screen-p))))
    ((#.x-enter-notify #.x-leave-notify)
     (with-alien ((event (struct x-crossing-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    (if (eq (slot event 'type) x-enter-notify)
				  :enter-notify
				  :leave-notify)
		:drawable     (slot event 'window)
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:root         (slot event 'root)
		:subwindow    (slot event 'child)
		:time         (slot event 'time)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:root-x       (slot event 'root-x)
		:root-y       (slot event 'root-y)
		:mode         (slot event 'mode)
		:detail       (slot event 'detail)
		:same-screen-p  (slot event 'same-screen-p)
		:focus        (slot event 'focus)
		:state        (slot event 'state))))
    (#.x-exposure
     (with-alien ((event (struct x-expose-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :exposure
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:window       (slot event 'window)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:width        (slot event 'width)
		:height       (slot event 'height)
		:count        (slot event 'count))))
    (#.x-graphics-exposure
     (with-alien ((event (struct x-graphics-expose-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :graphics-exposure
		:drawable     (slot event 'window)
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:width        (slot event 'width)
		:height       (slot event 'height)
		:count        (slot event 'count)
		:minor        (slot event 'minor)
		:major        (slot event 'major))))
    (#.x-no-exposure
     (with-alien ((event (struct x-no-expose-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :no-exposure
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:drawable     (slot event 'window)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:width        (slot event 'width)
		:height       (slot event 'height)
		:count        (slot event 'count)
		:minor        (slot event 'minor)
		:major        (slot event 'major))))
    ((.#x-focus-in .#x-focus-out)
     (with-alien ((event (struct x-focus-change-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    (if (eq (slot event 'type) x-focus-in)
				  :focus-in
				  :focus-out)
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:mode         (slot event 'mode)
		:detail       (slot event 'detail))))
    (.#x-keymap-notify
     (with-alien ((event (struct x-keymap-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :keymap-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:key-vector   (slot event 'key-vector))))
    (.#x-visibility-notify
     (with-alien ((event (struct x-visibility-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :visibility-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:state        (slot event 'state))))
    (.#x-create-notify
     (with-alien ((event (struct x-create-window-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :create-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:width        (slot event 'width)
		:height       (slot event 'height)
		:border-width (slot event 'border-width)
		:override-redirect-p (slot event 'override-redirect-p))))
    (.#x-destroy-notify
     (with-alien ((event (struct x-destroy-window-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :destroy-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p))))
    (.#x-unmap-notify
     (with-alien ((event (struct x-unmap-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :unmap-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:configure-p  (slot event 'configure-p))))
    (.#x-map-notify
     (with-alien ((event (struct x-map-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :map-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:override-redirect-p (slot event 'override-redirect-p))))
    (.#x-map-request
     (with-alien ((event (struct x-map-request-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :map-request
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		;:window       (slot event 'window)
		:parent       (slot event 'parent))))
    (.#x-reparent-notify
     (with-alien ((event (struct x-reparent-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :reparent-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:parent       (slot event 'parent)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:override-redirect-p (slot event 'override-redirect-p))))
    (.#x-configure-notify
     (with-alien ((event (struct x-configure-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :configure-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:width        (slot event 'width)
		:height       (slot event 'height)
		:border-width         (slot event 'border-width)
		:above-sibling        (slot event 'above-sibling)
		:override-redirect-p  (slot event 'override-redirect-p))))
    (.#x-gravity-notify
     (with-alien ((event (struct x-gravity-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :gravity-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:event        (slot event 'event)
		:x            (slot event 'x)
		:y            (slot event 'y))))
    (.#x-resize-request
     (with-alien ((event (struct x-resize-request-event)
			 :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :resize-request
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:event        (slot event 'event)
		:width        (slot event 'width)
		:height       (slot event 'height))))
    (.#x-configure-request
     (with-alien ((event (struct x-configure-request-event)
			 :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :configure-request
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:event        (slot event 'event)
		:x            (slot event 'x)
		:y            (slot event 'y)
		:height       (slot event 'height)
		:width        (slot event 'width)
		:stack-mode   (slot event 'stack-mode)
		:value-mask   (slot event 'value-mask))))
    (.#x-circulate-notify
     (with-alien ((event (struct x-circulate-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :circulate-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:event        (slot event 'event)
		:place        (slot event 'place))))
    (.#x-circulate-request
     (with-alien ((event (struct x-circulate-request-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :circulate-request
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:event        (slot event 'event)
		:place        (slot event 'place))))
    (.#x-property-notify
     (with-alien ((event (struct x-property-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :property-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:atom         (slot event 'atom)
		:state        (slot event 'state))))
    (.#x-selection-clear
     (with-alien ((event (struct x-selection-clear-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :selection-clear
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:selection    (slot event 'selection)
		:time         (slot event 'time))))
    (.#x-selection-request
     (with-alien ((event (struct x-selection-request-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :selection-request
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:event-window (slot event 'requestor)
		:selection    (slot event 'selection)
		:target       (slot event 'target)
		:property     (slot event 'property)
		:time         (slot event 'time))))
    (.#x-selection-notify
     (with-alien ((event (struct x-selection-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :selection-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:selection    (slot event 'selection)
		:target       (slot event 'target)
		:property     (slot event 'property)
		:time         (slot event 'time))))
    (.#x-colormap-notify
     (with-alien ((event (struct x-color-map-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :colormap-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:colormap     (slot event 'colormap)
		:new-p        (slot event 'new-p)
		:installed-p  (slot event 'installed-p))))
    (.#x-mapping-notify
     (with-alien ((event (struct x-mapping-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :mapping-notify
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:request      (slot event 'request)
		:first-keycode (slot event 'first-keycode)
		:count        (slot event 'count))))
    (.#x-client-message
     (with-alien ((event (struct x-client-message-event) :local event))
       (funcall handler
		:event-window (slot event 'window)
		:event-key    :client-message
		:display      (make-display (slot event 'display))
		:type         (slot event 'type)
		:serial       (slot event 'serial)
		:send-event   (slot event 'send-event-p)
		:message-type (slot event 'message-type)
		:format       (slot event 'format)
		:data         (slot event 'data))))
    (t (format t "FIX handle-event passing ~A~%" (slot event 'type))
       (format t "    (typeof ) ~A~%" (type-of (slot event 'type)))
       (format t "    (eq . 22) ~A~%" (eq (slot event 'type) 22))
       (format t "    (eq . x-configure-notify) ~A~%"
	       (eq (slot event 'type)
		   x-configure-notify))
       ())))

(def-alien-variable ("true" x-true) int)

;;; Public
;;;
(defun process-event (display
		      &key timeout
		           handler)
  "Process a single event on $display with $timeout, passing details of the
   event to $handler."
  (or (zerop timeout)
      (error "FIX implement timeout <> 0"))
  (with-alien ((event (union x-event)))
    (if (eq (x-check-mask-event
	     (int-sap (display-x-display display))
	     (x-event-mask-of-screen
	      (int-sap
	       (display-default-screen display)))
	     (addr event))
	    x-true)
	(let (*release-current-event*)
	  (unwind-protect
	      ;(ed::msg "p-e event type ~A" (slot event 'type))
	      ;(ed::msg "p-e event window ~A" (slot event 'window))
	      (or (handle-event event handler)
		  (progn
		    (x-put-back-event
		     (int-sap (display-x-display display))
		     (addr event))
		    (setq *release-current-event* ())
		    ()))
	    (if *release-current-event*
		(x-put-back-event
		 (int-sap (display-x-display display))
		 (addr event))))))))

(def-alien-routine ("XPending" x-pending) int
  (display system-area-pointer)) ; type Display*

;;; Public
;;;
(defun event-listen (display)
  "Return t if an event is available on $display."
  (plusp (x-pending (int-sap (display-x-display display)))))

(def-alien-variable ("button1Mask" x-button1-mask) int)
(def-alien-variable ("button2Mask" x-button2-mask) int)
(def-alien-variable ("button3Mask" x-button3-mask) int)
(def-alien-variable ("button4Mask" x-button4-mask) int)
(def-alien-variable ("button5Mask" x-button5-mask) int)
(def-alien-variable ("shiftMask" x-shift-mask) int)
(def-alien-variable ("lockMask" x-lock-mask) int)
(def-alien-variable ("controlMask" x-control-mask) int)
(def-alien-variable ("mod1Mask" x-mod1-mask) int)
(def-alien-variable ("mod2Mask" x-mod2-mask) int)
(def-alien-variable ("mod3Mask" x-mod3-mask) int)
(def-alien-variable ("mod4Mask" x-mod4-mask) int)
(def-alien-variable ("mod5Mask" x-mod5-mask) int)

(defvar *state-masks* `((:mod-1 . ,x-mod1-mask)
			(:mod-2 . ,x-mod2-mask)
			(:mod-3 . ,x-mod3-mask)
			(:mod-4 . ,x-mod4-mask)
			(:mod-5 . ,x-mod5-mask)
			(:control . ,x-control-mask)
			(:lock . ,x-lock-mask)
			(:shift . ,x-shift-mask)
			(:button1 . ,x-button1-mask)
			(:button2 . ,x-button2-mask)
			(:button3 . ,x-button3-mask)
			(:button4 . ,x-button4-mask)
			(:button5 . ,x-button5-mask))
  "Alist mapping `make-state-mask' modifier keywords to X modifer masks.")

;;; Public
;;;
(defun make-state-mask (&rest modifiers)
  "Return a state mask for a key or button event from $modifiers.

   Each modifier can be one of :mod-1, :mod-2, :mod-3, :mod-4, :mod-5,
   :control, :lock, :shift, :button1, :button2, :button3, :button4 and
   :button5."
  (fi (car modifiers)
      0
      (logior (cdr (or (assoc (car modifiers) *state-masks*)
		       (error "Failed to match modifier ~A." (car modifiers))))
	      (apply #'make-state-mask (cdr modifiers)))))


;;;; Windows.

(def-alien-type ()
  (struct x-set-window-attributes
    (background-pixmap (* t)) ; type Pixmap
    (background-pixel unsigned-long)
    (border-pixmap (* t)) ; type Pixmap
    (border-pixel unsigned-long)
    (bit-gravity int)
    (win-gravity int)
    (backing-store int)
    (backing-planes unsigned-long)
    (backing-pixel unsigned-long)
    (save-under boolean)
    (event-mask long)
    (do-not-propogate-mask long)
    (override-redirect-p long)
    (colormap (* t)) ; type Colormap
    (cursor system-area-pointer))) ; type Cursor

(def-alien-routine ("XCreateWindow" x-create-window)
		   system-area-pointer ; type Window
  (display system-area-pointer) ; type Display
  (parent (* t)) ; type Window
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (border-width unsigned-int)
  (depth int)
  (class unsigned-int)
  (visual system-area-pointer) ; type Visual*
  (valuemask unsigned-long)
  (attributes (* (struct x-set-window-attributes))))

(def-alien-routine ("XCreateSimpleWindow" x-create-simple-window)
		   system-area-pointer ; type Window
  (display system-area-pointer) ; type Display
  (parent (* t)) ; type Window
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (border-width unsigned-int)
  (border unsigned-long)
  (background unsigned-long))

(def-alien-variable ("inputOutput" x-input-output) int)
(def-alien-variable ("inputOnly" x-input-only) int)
(def-alien-variable ("copyFromParent" x-copy-from-parent) int)

(def-alien-variable ("cWBackPixel" x-cw-back-pixel) long)
(def-alien-variable ("cWBackPixmap" x-cw-back-pixmap) long)
(def-alien-variable ("cWBorderPixel" x-cw-border-pixel) long)
(def-alien-variable ("cWBorderPixmap" x-cw-border-pixmap) long)
(def-alien-variable ("cWCursor" x-cw-cursor) long)
(def-alien-variable ("cWOverrideRedirect" x-cw-override-redirect) long)


;;; Public
;;;
(defun create-window (&key parent
			   (x 0)
			   (y 0)
			   (width 0)
			   (height 0)
			   depth
			   background
			   (border-width 0)
			   border
			   (class :input-output)
			   event-mask
			   override-redirect
			   cursor
			   (input t))
  "Create an X window at positions $x and $y, with $width, $height, $depth
   and $border-width, having parent window $parent.

   Set the background pixel according to $background and the border pixmap
   according to $border.

   Set the class of window according to $class, which must be one of
   :input-output, :input-only and :copy-from-parent."
  (with-alien ((valuemask long :local 0)
	       (attributes (struct x-set-window-attributes)))
    (when background
      (or (eq background :none) ; FIX correct?
	  (typecase background
	    (number
	     ;; $pixmap is a pixel.
	     (setf (slot attributes 'background-pixel)
		   background)
	     (logior valuemask x-cw-back-pixel))
	    (t
	     (setf (slot attributes 'background-pixmap)
		   background)
	     (logior valuemask x-cw-back-pixmap)))))
    (when border
      (or (eq border :none) ; FIX correct?
	  (typecase border
	    (number
	     ;; $border is a pixel.
	     (setf (slot attributes 'border-pixel)
		   border)
	     (logior valuemask x-cw-border-pixel))
	    (t
	     ;; $border is a pixmap.
	     (setf (slot attributes 'border-pixmap)
		   (int-sap (pixmap-x-drawable border)))
	     (logior valuemask x-cw-border-pixmap)))))
    (when override-redirect
      (setf (slot attributes 'override-redirect-p) x-true)
      (logior valuemask x-cw-override-redirect))
    (when cursor
      (setf (slot attributes 'cursor)
	    (int-sap (cursor-x-cursor cursor)))
      (logior valuemask x-cw-cursor))
    (let* ((display (int-sap (display-x-display
			      (window-display parent))))
	   (depth (or depth
		      (x-default-depth-of-screen
		       (x-default-screen-of-display display))))
	   (x-win (x-create-window
		   display
		   (int-sap (window-x-drawable parent))
		   x y
		   width height
		   border-width
		   depth
		   (ecase class
		     (:input-output x-input-output)
		     (:input x-input-only)
		     (:copy-from-parent x-copy-from-parent))
		   ; FIX ok to always copy from parent screen?
		   (x-default-visual-of-screen
		    (int-sap
		     (display-default-screen (window-display parent))))
		   valuemask
		   (if (zerop valuemask)
		       (sap-alien (int-sap 0)
				  (* (struct x-set-window-attributes)))
		       (addr attributes)))))
      (let ((window (make-window :x-drawable (sap-int x-win)
				 :display (make-display display))))
	(if event-mask
	    (set-window-event-mask window event-mask))
	(set-wm-input-hint display x-win input)
	window))))

(def-alien-routine ("XDestroyWindow" x-destroy-window) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer)) ; type Window

(defun destroy-window (window)
  (x-destroy-window (int-sap (display-x-display
			      (window-display window)))
		    (int-sap (window-x-drawable window))))

(def-alien-routine ("XMapWindow" x-map-window) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer)) ; type Window

;;; Public
;;;
(defun map-window (window)
  (zerop (x-map-window (int-sap (display-x-display
				 (window-display window)))
		       (int-sap (window-x-drawable window)))))

(def-alien-routine ("XUnmapWindow" x-unmap-window) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer)) ; type Window

;;; Public
;;;
(defun unmap-window (window)
  (zerop (x-unmap-window (int-sap (display-x-display
				   (window-display window)))
			 (int-sap (window-x-drawable window)))))

(def-alien-routine ("XClearWindow" x-clear-window) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer)) ; type Window

;;; Public
;;;
(defun clear-window (window)
  "Clear the entire area of $window."
  (x-clear-window (int-sap (display-x-display
			    (window-display window)))
		  (int-sap (window-x-drawable window))))

(def-alien-routine ("XClearArea" x-clear-area) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int)
  (exposures boolean))

;;; Public
;;;
(defun clear-area (window
		   &key (x 0)
		        (y 0)
			(width 0)
			(height 0)
			generate-exposures-p)
  "Clear an area of $window."
  (x-clear-area (int-sap (display-x-display (window-display window)))
		(int-sap (window-x-drawable window))
		x y width height generate-exposures-p))

(def-alien-routine ("XSetWindowBorderPixmap" x-set-window-border-pixmap)
		   int
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window
  (border-pixmap (* t))) ; type Pixmap

(def-alien-routine ("XSetWindowBorder" x-set-window-border)
		   int
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window
  (border-pixel unsigned-long))

;; Public
;;
;; FIX only exporting for setf
;;
(defun window-border (window)
  (declare (ignore window))
  (error "FIX implement"))
;;
(defun set-window-border (window pixmap)
  (etypecase pixmap
    (number
     ;; $pixmap is a pixel.
     (x-set-window-border (int-sap (display-x-display
				    (window-display window)))
			  (int-sap (window-x-drawable window))
			  pixmap))
    (pixmap
     (or (eq (display-x-display (window-display window))
	     (display-x-display (pixmap-display pixmap)))
	 (error "$window and $pixmap must be on the same display."))
     (x-set-window-border-pixmap
      (int-sap (display-x-display (window-display window)))
      (int-sap (window-x-drawable window))
      (int-sap (pixmap-x-drawable pixmap))))))
;; FIX how to export setf? (more below)
;;
(defsetf window-border set-window-border
  "Set the window-border of $window to $pixmap.")

(def-alien-routine ("XDefineCursor" x-define-cursor) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window
  (cursor system-area-pointer)) ; type Cursor

;; FIX defined to implement defsetf; correct?
(defun window-cursor (window)
  (declare (ignore window))
  (error "FIX"))
;;
(defun set-window-cursor (window cursor)
  "Define $cursor in $window."
  (x-define-cursor (int-sap (display-x-display (window-display window)))
		   (int-sap (window-x-drawable window))
		   (int-sap (cursor-x-cursor cursor))))
;;
(defsetf window-cursor set-window-cursor
  "Define $cursor in $window.")

(def-alien-routine ("XSetWindowBackground" x-set-window-background) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window
  (background-pixel unsigned-long))

;; FIX defined to implement defsetf; correct?
(defun window-background (window)
  (declare (ignore window))
  (error "FIX"))
;;
(defun set-window-background (window background)
  "Set the background pixel of $window to $background."
  (x-set-window-background
   (int-sap (display-x-display (window-display window)))
   (int-sap (window-x-drawable window))
   background))
;;
(defsetf window-background set-window-background
  "Set the background pixel of $window to $background.")

(def-alien-routine ("XTranslateCoordinates" x-translate-coordinates)
		   boolean
  (display system-area-pointer) ; type Display*
  (src-w (* t)) ; type Window
  (dest-w (* t)) ; type Window
  (src-x int)
  (src-y int)
  (dest-x-return (* int))
  (dest-y-return (* int))
  (child-return (* t))) ; type Window*

(defun translate-coordinates (source x y dest)
  "Return the x an y translated window coordinates of $source $x and $y
   with respect to $dest."
  (with-alien ((child (* t)) ; type Window
	       (dest-x int)
	       (dest-y int))
    (if (x-translate-coordinates (int-sap (display-x-display
					   (window-display source)))
				 (int-sap (window-x-drawable source))
				 (int-sap (window-x-drawable dest))
				 x y
				 (addr dest-x)
				 (addr dest-y)
				 (addr child))
	(values dest-x dest-y))))

;;; Public
;;;
(defun window-id (window)
  "Return the window id of $window."
  ; FIX A guess; this function is only used in eg in manual.
  (window-x-drawable window))

(def-alien-routine ("XQueryTree" x-query-tree) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (root-return (* (* t))) ; type Window*
  (parent-return (* (* t))) ; type Window*
  (children-return (* (* (* t)))) ; type Window**
  (nchildren-return (* unsigned-int)))

;;; Public
;;;
(defun query-tree (window)
  "Return the children, parent and root of $window."
  (let ((display (window-display window)))
    (with-alien ((root (* t)) ; type Window
		 (parent (* t)) ; type Window
		 (children (* (* t))) ; type Window*
		 (child-count unsigned-int))
      (x-query-tree (int-sap (display-x-display display))
		    (int-sap (window-x-drawable window))
		    (addr root)
		    (addr parent)
		    (addr children)
		    (addr child-count))
      (collect ((child))
	(dotimes (i child-count)
	  (child (make-window
		  :x-drawable (sap-int (alien-sap (deref children i)))
		  :display display)))
	(values (child)
		(make-window :x-drawable (sap-int (alien-sap parent))
			     :display display)
		(make-window :x-drawable (sap-int (alien-sap root))
			     :display display))))))
;; FIX XFree children on collection


;;;; Atoms.

(def-alien-routine ("XGetAtomName" x-get-atom-name) c-string
  (display system-area-pointer) ; type Display
  (atom system-area-pointer)) ; type Atom

(def-alien-routine ("XInternAtom" x-intern-atom)
		   system-area-pointer ; type Atom
  (display system-area-pointer) ; type Display
  (atom-name c-string)
  (only-if-exists boolean))

(def-alien-routine ("XInternAtoms" x-intern-atoms)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display
  (names (* c-string))
  (count int)
  (only-if-exists boolean)
  (atoms-return system-area-pointer)) ; type *Atom


;;;; Window attributes and properties.

(def-alien-type ()
  (struct x-window-attributes
    (x int)
    (y int)
    (width int)
    (height int)
    (border-width int)
    (depth int)
    (visual system-area-pointer) ; type Visual*
    (root (* t)) ; type Window
    (class int)
    (bit-gravity int)
    (win-gravity int)
    (backing-store int)
    (backing-planes unsigned-long)
    (backing-pixel unsigned-long)
    (save-under boolean)
    (colormap (* t)) ; type Colormap
    (map-installed boolean)
    (map-state int)
    (all-event-masks long)
    (your-event-mask long)
    (do-not-propogate-mask long)
    (override-redirect-p boolean)
    (screen system-area-pointer))) ; type Screen*

(defvar *available-attributes* ()
  "Assocation list mapping windows and attributes of all windows for which
   attributes are available.  Used by `with-state'.")

(def-alien-routine ("XGetWindowAttributes" x-get-window-attributes) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window
  (attributes (* (struct x-window-attributes))))

;;; Public
;;;
(defmacro with-state ((window) &body body)
  "with-state (window) form*

   Evaluate $body in a context where the attributes of $window are
   available."
  (let ((attribs (gensym))
	(assoc (gensym)))
    `(with-alien ((,attribs (struct x-window-attributes)))
       (x-get-window-attributes
	(int-sap (display-x-display (window-display ,window)))
	(int-sap (window-x-drawable ,window))
	(addr ,attribs))
       (let ((,assoc (cons ,window (addr ,attribs))))
	 (push ,assoc *available-attributes*)
	 (unwind-protect
	     (progn ,@body)
	   (setq *available-attributes*
		 (delete ,assoc *available-attributes*
			 :test #'equal)))))))

(def-alien-variable ("isUnmapped" x-is-unmapped) int)
(def-alien-variable ("isUnviewable" x-is-unviewable) int)
(def-alien-variable ("isViewable" x-is-viewable) int)

;;; Public
;;;
(defun window-map-state (window)
  (with-state (window)
    (let ((assoc (assoc window *available-attributes* :test #'equalp)))
      (or assoc (error "Attributes for ~A should be available." window))
      (case (slot (cdr assoc) 'map-state)
	(#.x-is-unmapped :unmapped)
	(#.x-is-unviewable :unviewable)
	(#.x-is-viewable :viewable)))))

(def-alien-routine ("XSetIconName" x-set-icon-name) void
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (icon-name c-string))

;;; Public
;;;
(defun set-standard-properties (window
				&key icon-name)
  "Set standard properties on $window."
  ;; FIX what else should be set?
  (if icon-name
      (with-alien ((x-icon-name c-string :local icon-name))
	(x-set-icon-name
	 (int-sap (display-x-display (window-display window)))
	 (int-sap (window-x-drawable window))
	 x-icon-name))))

(def-alien-routine ("XRaiseWindow" x-raise-window) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer)) ; type Window

(def-alien-routine ("XLowerWindow" x-lower-window) int
  (display system-area-pointer) ; type Display
  (window system-area-pointer)) ; type Window

;;; Public
;;;
;; FIX defined to implement defsetf; correct?
(defun window-priority (window)
  (error "FIX"))
;;
(defun set-window-priority (window priority)
  "Set the background pixel of $window to $background."
  (ecase priority
    (:above
     (x-raise-window (int-sap (display-x-display (window-display window)))
		     (int-sap (window-x-drawable window))))
    (:below
     (x-lower-window (int-sap (display-x-display (window-display window)))
		     (int-sap (window-x-drawable window))))))
;;
(defsetf window-priority set-window-priority
  "Set the priority of $window to $priority.")

(def-alien-routine ("XGetWindowProperty" x-get-window-property) int
  (display system-area-pointer) ; type *Display
  (window system-area-pointer) ; type Window
  (property system-area-pointer) ; type Atom
  (long-offset long)
  (long-length long)
  (delete boolean)
  (req-type system-area-pointer) ; type Atom
  (actual-return-type (* system-area-pointer)) ; type *Atom
  (actual-format-return (* int))
  (nitems-return (* unsigned-long))
  (bytes-after-return (* unsigned-long))
  (prop-return (* (* unsigned-char)))) ; type **unsigned-char

(defvar *window-property-max-length* 2048)

;;; Public.
;;;
(defun window-property (display window name)
  "Return string property $name of $window on $display."
  (declare (string name))
  (let* ((x-display (int-sap (display-x-display display)))
	 (property (x-intern-atom x-display name t))
	 (string-type (int-sap 0))) ; AnyPropertyType (x-intern-atom x-display "STRING" t)))
    (with-alien ((actual-type system-area-pointer) ; type Atom
		 (format int)
		 (length unsigned-long)
		 (length-after unsigned-long)
		 (data (* unsigned-char)))
      (fi* (zerop (sap-int property))
	(format
	 t
	 "ret: ~A~%"
	 (x-get-window-property (int-sap (display-x-display display))
				(int-sap (window-x-drawable window))
				property
				0 ; offset
				*window-property-max-length* ; FIX retry
				() ; free
				string-type ; required type
				(addr actual-type)
				(addr format)
				(addr length)
				(addr length-after)
				(addr data)))
	(format t "format ~A~%" format)
	; FIX xfree (deref data)
	(cast data c-string)))))

(defvar x-prop-mode-replace 0)
(defvar x-prop-mode-prepend 1)
(defvar x-prop-mode-append  2)

(def-alien-routine ("XChangeProperty" x-change-property) int
  (display system-area-pointer) ; type *Display
  (window system-area-pointer) ; type Window
  (property system-area-pointer) ; type Atom
  (type system-area-pointer) ; type Atom
  (format int)
  (mode int)
  (data c-string)
  (nelements int))

;;; Public.
;;;
(defun set-window-property (display window name string)
  "Set property $name of $window on $display to $string."
  (declare (string name))
  (let* ((x-display (int-sap (display-x-display display)))
	 (property (x-intern-atom x-display name ()))
	 (string-type (x-intern-atom x-display "STRING" ())))
    (fi* (zerop (sap-int property))
      ;; FIX errors?
      (format
       t
       "ret: ~A~%"
       (x-change-property (int-sap (display-x-display display))
			  (int-sap (window-x-drawable window))
			  property
			  string-type
			  8 ; Char array.
			  x-prop-mode-replace
			  string
			  (length string))))))

#|
(defun text-property (display window name)
  "Return text property $name of $window on $display."
  (declare (string name))
  (let* ((x-display (int-sap (display-x-display display)))
	 (property (x-intern-atom x-display name t)))
    (with-alien ((data system-area-pointer))
      (fi* (zerop (sap-int property))
	(format
	 t
	 "ret: ~A~%"
	 (x-get-window-property (int-sap (display-x-display display))
				(int-sap (window-x-drawable window))
				property
				0 ; offset
				*window-property-max-length* ; FIX retry
				() ; free
				string-type ; required type
				(addr actual-type)
				(addr format)
				(addr length)
				(addr length-after)
				(addr data)))
	(format t "format ~A~%" format)
	; FIX xfree (deref data)
	data))))
|#

(def-alien-routine ("XListProperties" x-list-properties) system-area-pointer ; *Atom
  (display system-area-pointer) ; type *Display
  (window system-area-pointer) ; type Window
  (num-prop-return (* int)))

(defun window-properties (display window)
  "Return the names of the properties of $window, which is on $display."
  (with-alien ((count int))
    (with-alien ((props (* system-area-pointer)
			:local
			(x-list-properties (int-sap (display-x-display display))
					   (int-sap (window-x-drawable window))
					   (addr count))))
      (format t "Window has ~A properties.~%" count)
      (dotimes (i count)
	(format t "  ~A~%" (x-get-atom-name (int-sap (display-x-display display)) (deref props i))))
      props)))

(def-alien-routine ("XDeleteProperty" x-delete-property) int
  (display system-area-pointer) ; type *Display
  (window system-area-pointer) ; type Window
  (property system-area-pointer)) ; type Atom

(def-alien-routine ("XSetTextProperty" x-set-text-property) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (text-prop (* (struct x-text-property)))
  (property system-area-pointer)) ; type Atom

(def-alien-routine ("XGetTextProperty" x-get-text-property)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (text-prop-return (* (struct x-text-property)))
  (property system-area-pointer)) ; type Atom


;;;; Selections.

(def-alien-routine ("XGetSelectionOwner" x-get-selection-owner)
		   system-area-pointer ; type Window
  (display system-area-pointer) ; type *Display
  (selection system-area-pointer)) ; type Atom

(def-alien-routine ("XSetSelectionOwner" x-set-selection-owner) int
  (display system-area-pointer) ; type *Display
  (selection system-area-pointer) ; type Atom
  (owner system-area-pointer) ; type Window
  (time x-time)) ; type Time

(def-alien-routine ("XSendEvent" x-send-event)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (propagate boolean)
  (event-mask long)
  (event-send (* (union x-event))))

(def-alien-routine ("XConvertSelection" x-convert-selection)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display*
  (selection system-area-pointer) ; type Atom
  (target system-area-pointer) ; type Atom
  (property system-area-pointer) ; type Atom
  (requestor system-area-pointer) ; type Window
  (time x-time)) ; type Time

(defvar x-current-time 0)

;;; Public
;;;
(defun selection (display window &optional (name "PRIMARY"))
  "Return string selection $name on $display via a property of $window."
  (let* ((x-display-sap (int-sap (display-x-display display)))
	 (x-window (int-sap (window-x-drawable window)))
	 (selection (x-intern-atom x-display-sap name ()))
	 (target-type (x-intern-atom x-display-sap "STRING" ()))
	 (target-name (format () "IN_~A" name))
	 (target-property (x-intern-atom x-display-sap
					 target-name
					 ())))
    (fi (or (zerop (sap-int selection))
	    (zerop (sap-int target-type))
	    (zerop (sap-int target-property)))
	(let ((x-owner (x-get-selection-owner x-display-sap selection)))
	  (format t "x-owner: ~A~%" x-owner)
	  (fi (zerop (sap-int x-owner))
	      (with-alien ((event (struct x-selection-request-event)))
		(x-convert-selection x-display-sap
				     selection
				     target-type
				     target-property
				     x-window
				     ; FIX apparently supposed to send time
				     ;     of event that triggered this
				     x-current-time)
		;; Try accept the selection-notify event.
		(until ((time 0 (1+ time))
			(exit))
		       ((or exit (> time 10)))
		  (sleep 1/100)
		  (event-case (display :timeout 0)
		    (:SELECTION-NOTIFY (event-window)
		      (when (equal window event-window)
			(setq exit t)))
		    (t ())))
		;; FIX get property described in selection-notify
		;; FIX free property after
		;; (x-delete-property x-display-sap x-window target-property)
		(window-property display window target-name)))))))

;;; Public
;;;
(defun clipboard (display window)
  "Return the clipboard on $display via the IN_CLIPBOARD property of
   $window."
  (selection display window "CLIPBOARD"))

(def-alien-routine ("XWindowEvent" x-window-event) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (event-mask long)
  (event-return (* (union x-event))))

(def-alien-routine ("XCheckTypedWindowEvent" x-check-typed-window-event)
		   boolean
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (event-type int)
  (event-return (* (union x-event))))

;;; Internal
;;;
;;; Return the time for the last event by generating a property-notify
;;; event on $window on $display.
;;;
(defun event-time (display window)
  (let* ((x-display (int-sap (display-x-display display)))
	 (x-window (int-sap (window-x-drawable window)))
	 (property (x-intern-atom x-display "XXX" ()))
	 (string-type (x-intern-atom x-display "STRING" t)))
    (fi* (and (zerop (sap-int property))
	      (zerop (sap-int string-type)))
      ;; FIX errors?
      (format
       t
       "ret: ~A~%"
       (x-change-property x-display
			  x-window
			  property
			  string-type
			  8 ; Char array.
			  x-prop-mode-append
			  ""
			  0))
      ;; FIX Try read for the resulting property-notify event.
      (with-alien ((event (union x-event)))
	(sleep 1/100)
	(if (x-check-typed-window-event x-display
					x-window
					x-property-notify
					(addr event))
	    (with-alien ((notify-event (* (struct x-property-event))
				       :local
				       (cast (addr event)
					     (* (struct x-property-event)))))
	      (format t "event-time: ~A~%" (slot notify-event 'time))
	      (slot notify-event 'time))))
#|
       ;; Try read the time from the resulting property-notify event.
       (until ((return-time)
	       (exit)
	       (time 0 (1+ time)))
	      ((or exit (> time 10))
	       return-time)
	 (sleep 1/100)
	 (event-case (display :timeout 0)
	   (:PROPERTY-NOTIFY (event-window time)
	      (format t "pn e-w ~A~%" event-window)
	      (when (equal window event-window)
		(setq return-time time)
		(setq exit t)))))
|#
       )))

(defvar *selection-times* ())

;; FIX clear out on window free

(defun store-selection-start (window name time)
  (let ((assoc (assoc (cons window name) *selection-times*
		      :test #'equalp)))
    (if assoc
	(setf (car (cdr assoc)) time)
	(push (cons (cons window name) (list time))
	      *selection-times*))))

(defun store-selection-end (window name time)
  (let ((assoc (assoc (cons window name) *selection-times*
		      :test #'equalp)))
    (if assoc
	(setf (car (cddr assoc)) time)
	(pushnew (cons (cons window name) (list () time))
		 *selection-times*))))

(defun store-selection-fetcher (window name function)
  (let ((assoc (assoc (cons window name) *selection-times*
		      :test #'equalp)))
    (if assoc
	(setf (car (cdddr assoc)) function)
	(pushnew (cons (cons window name) (list () () function))
		 *selection-times*))))

(defun get-selection-start (window name)
  (let ((assoc (assoc (cons window name) *selection-times*
		      :test #'equalp)))
    (if assoc (cadr assoc))))

(defun get-selection-end (window name)
  (let ((assoc (assoc (cons window name) *selection-times*
		      :test #'equalp)))
    (if assoc (caddr assoc))))

(defun get-selection-fetcher (window name)
  (let ((assoc (assoc (cons window name) *selection-times*
		      :test #'equalp)))
    (if assoc (cadddr assoc))))

;;; Public
;;;
(defun set-selection (display window string-or-function
			      &optional
			      (name "PRIMARY"))
  "Set selection $name on $display to $string-or-function served by
   $window.

   If $string-or-function is a function then call $string-or-function to
   get the selection string when a client requests the selection, otherwise
   serve the string $string-or-function."
  (format t "set ~A~%" name)
  (let* ((name (or name "PRIMARY"))
	 (x-display-sap (int-sap (display-x-display display)))
	 (x-window-sap (int-sap (window-x-drawable window)))
	 (selection (x-intern-atom x-display-sap name t))
	 (selection-time (or (event-time display window)
			     x-current-time)))
    (fi* (zerop (sap-int selection))
      (x-set-selection-owner x-display-sap
			     selection
			     x-window-sap
			     selection-time)
      (let ((x-owner (x-get-selection-owner x-display-sap selection)))
	(format t "x-owner: ~A~%" x-owner)
	(format t "x-window-sap: ~A~%" x-window-sap)
	(when (eq (sap-int x-owner) (sap-int x-window-sap))
	  (etypecase string-or-function
	    (function
	     (store-selection-fetcher window name
				      string-or-function))
	    (string
	     (set-window-property display window
				  name string-or-function)))
	  (store-selection-start window name selection-time)
	  t)))))
;;;
(defsetf selection set-selection
  "Set $selection on $display to $string.")

;;; Public
;;;
(defun handle-selection-request (display event-key event-window requestor
					 selection target property time
					 send-event-p)
  "Handle a selection-request event."
  (declare (ignore event-key send-event-p))
  (format t "(handle-selection-request )~%")
  (let* ((x-display-sap (int-sap (display-x-display display)))
	 (x-requestor-sap requestor)
	 (selection-name (x-get-atom-name x-display-sap selection))
	 ;(string-target (x-intern-atom x-display-sap "STRING" t))
	 ;(string-target (x-intern-atom x-display-sap "COMPOUND_TEXT" t))
	 (start-time (get-selection-start event-window selection-name))
	 (end-time (get-selection-end event-window selection-name))
	 (selection-fetcher (get-selection-fetcher event-window
						   selection-name))
	 (refuse-p))
    (with-alien ((union-event (union x-event)))
      (setf (slot union-event 'type) x-selection-notify)
      (with-alien ((event (* (struct x-selection-event))
			  :local
;			  (slot union-event 'xselection)))
			  (cast (addr union-event)
				(* (struct x-selection-event)))))
	(flet ((refuse ()
		 (format t "  refuse~%")
		 (setq refuse-p t)
		 (setf (slot (deref event) 'property) (int-sap 0)))) ; FIX None
	  (format t "  selection-fetcher: ~A~%" selection-fetcher)
	  (format t "  time: ~A~%" time)
	  (format t "  start-time: ~A~%" start-time)
	  (format t "  end-time: ~A~%" end-time)
	  (or selection-fetcher
	      (progn
		(or start-time (refuse))
		(if (or (< time start-time)
			(and end-time (> time end-time)))
		    (refuse))))
	  (format t "  target name: ~A~%" (x-get-atom-name x-display-sap
							   target))
	  (or (zerop (sap-int property))
	      (string= (x-get-atom-name x-display-sap target)
		       "STRING")
#|
	      (string= (x-get-atom-name x-display-sap target)
		       "COMPOUND_TEXT")
	      (string= (x-get-atom-name x-display-sap target)
		       "UTF8_STRING")
|#
	      (refuse))
	  ;; FIX only get string if going to send
	  (format t "  get string~%")
	  (let* ((string (if selection-fetcher
			     (funcall selection-fetcher selection time)
			     (window-property display event-window
					      selection-name)))
		 ;; FIX to support old style clients if property is None
		 ;;     use target as property
		 (property (if (zerop (sap-int property))
			       (progn
				 (format t "  property was zero~%")
				 target)
			       property))
		 (property-name (x-get-atom-name x-display-sap property)))
	    ;; FIX to support old style clients if property is None use
	    ;;     target as property
	    (format t "  property-name: ~A~%" property-name)
	    (format t "  string: ~A~%" string)
	    (or string (refuse))
	    (or refuse-p
		(let ((requestor-window (make-window
					 :display display
					 :x-drawable
					 (sap-int x-requestor-sap))))
		  (setf (slot (deref event) 'property) property)
		  (format t "  set string in requestor~%")
		  (format t "    requestor-sap: ~A~%" x-requestor-sap)
		  ;; FIX might be better to use given property, if any
		  ;; FIX check return
		  (set-window-property display
				       requestor-window
				       property-name
				       string)))
	    (format t "  set slot selection to ~A~%" selection)
	    (setf (slot (deref event) 'selection) selection)
	    (format t "  set slot target to ~A~%" target)
	    (setf (slot (deref event) 'target) target)
	    (format t "  set slot time to ~A~%" time)
	    (setf (slot (deref event) 'time) time)
	    (format t "  set slot window to ~A~%" x-requestor-sap)
	    (setf (slot (deref event) 'window) x-requestor-sap)
	    (format t "  set slot display to ~A~%" x-display-sap)
	    (setf (slot (deref event) 'display) x-display-sap)
	    (format t "  set slot send-event-p to ~A~%" x-true)
	    (setf (slot (deref event) 'send-event-p) x-true)
	    (format t "  send event~%")
	    (format
	     t "  send event ret ~A~%"
	     (x-send-event x-display-sap
			   x-requestor-sap
			   () ; Propagate.
			   0 ; Event mask.
			   (addr union-event)))))))))

;;; Public
;;;
(defun handle-selection-clear (display event-key event-window
				       selection time
				       send-event-p)
  "Handle a selection-clear event."
  (declare (ignore event-key send-event-p))
  (let* ((x-display-sap (int-sap (display-x-display display)))
	 (selection-name (x-get-atom-name x-display-sap selection)))
    (store-selection-end event-window time selection-name)))


;;;; Cursors.

(def-alien-routine ("XFreeCursor" x-free-cursor) int
  (display system-area-pointer) ; type Display*
  (cursor (* t))) ; type Cursor

;;; Public
;;;
(defun free-cursor (cursor)
  (x-free-cursor (int-sap (display-x-display (cursor-display cursor)))
		 (int-sap (cursor-x-cursor cursor))))

(def-alien-routine ("XCreatePixmapCursor" x-create-pixmap-cursor)
		   system-area-pointer ; type Cursor
  (display system-area-pointer) ; type Display*
  (source (* t)) ; type Pixmap
  (mask (* t)) ; type Pixmap
  (foreground-color (* t)) ; type XColor*
  (background-color (* t)) ; type XColor*
  (x unsigned-int)
  (y unsigned-int))

;;; Public
;;;
(defun create-cursor (&key source mask
			   x y
			   foreground background)
  "Create a cursor from pixmap $source with $mask, $foreground colour and
   $background colour, FIX at $x $y."
  (let* ((display (pixmap-display source))
	 (cursor (sap-int (x-create-pixmap-cursor
			   (int-sap (display-x-display display))
			   source mask
			   foreground background
			   x y))))
    ;; FIX check cursor?
    (make-cursor :x-cursor cursor :display display)))

(def-alien-routine ("XCreateFontCursor" x-create-font-cursor)
		   system-area-pointer ; type Cursor
  (display system-area-pointer) ; type Display
  (shape unsigned-int))

;;; Public
;;;
(defun create-font-cursor (display shape)
  "Create cursor number $shape for $display."
  ;; FIX check x-create-font-cursor return?
  (make-cursor :x-cursor (sap-int (x-create-font-cursor
				   (int-sap
				    (display-x-display display))
				   shape))
	       :display display))


;;;; Drawing.

(def-alien-routine ("XDrawPoint" x-draw-point) int
  (display system-area-pointer) ; type Display
  (drawable (* t)) ; type Drawable
  (gc (* t)) ; type GC
  (x int)
  (y int))

;;; Public
;;;
(defun draw-point (drawable gcontext x y)
  "Draw a point."
  (or (eq (display-x-display (drawable-display drawable))
	  (display-x-display (gcontext-display gcontext)))
      (error "Drawable and gcontext must be on same display."))
  (x-draw-point (int-sap (display-x-display
			  (drawable-display drawable)))
		(int-sap (drawable-x-drawable drawable))
		(int-sap (gcontext-x-gcontext gcontext))
		x y))

(def-alien-routine ("XDrawLine" x-draw-line) int
  (display system-area-pointer) ; type Display
  (drawable (* t)) ; type Drawable
  (gc (* t)) ; type GC
  (x1 int)
  (y1 int)
  (x2 int)
  (y2 int))

;;; Public
;;;
(defun draw-line (drawable gcontext x1 y1 x2 y2)
  "Draw a point."
  (or (eq (display-x-display (drawable-display drawable))
	  (display-x-display (gcontext-display gcontext)))
      (error "Drawable and gcontext must be on same display."))
  (x-draw-line (int-sap (display-x-display (drawable-display drawable)))
	       (int-sap (drawable-x-drawable drawable))
	       (int-sap (gcontext-x-gcontext gcontext))
	       x1 y1 x2 y2))

(def-alien-routine ("XDrawRectangle" x-draw-rectangle) int
  (display system-area-pointer) ; type Display
  (drawable (* t)) ; type Drawable
  (gc (* t)) ; type GC
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int))

(def-alien-routine ("XFillRectangle" x-fill-rectangle) int
  (display system-area-pointer) ; type Display
  (drawable (* t)) ; type Drawable
  (gc (* t)) ; type GC
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int))

;;; Public
;;;
(defun draw-rectangle (drawable gcontext x y width height &optional fillp)
  "Draw a rectangle of $width and $height at $x $y on $drawable.

   If $fillp is true fill the rectangle."
  (or (eq (display-x-display (drawable-display drawable))
	  (display-x-display (gcontext-display gcontext)))
      (error "Drawable and gcontext must be on same display."))
  (x-draw-rectangle (int-sap (display-x-display
			      (drawable-display drawable)))
		    (int-sap (drawable-x-drawable drawable))
		    (int-sap (gcontext-x-gcontext gcontext))
		    x y
		    width height)
  (if fillp
      (x-fill-rectangle (int-sap (display-x-display
				  (drawable-display drawable)))
			(int-sap (drawable-x-drawable drawable))
			(int-sap (gcontext-x-gcontext gcontext))
			x y
			width height)))

(def-alien-routine ("XCopyArea" x-copy-area) int
  (display system-area-pointer) ; type Display
  (src (* t)) ; type Drawable
  (dest (* t)) ; type Drawable
  (gc (* t)) ; type GC
  (src-x int)
  (src-y int)
  (width unsigned-int)
  (height unsigned-int)
  (dest-x int)
  (dest-y int))

;;; Public
;;;
(defun copy-area (source gcontext x y width height
		  dest dest-x dest-y)
  "Copy area of $width by $height size from drawable $source at $x $y to
   $dest at $dest-x $dest-y."
  (or (and (eq (display-x-display (drawable-display source))
	       (display-x-display (drawable-display dest)))
	   (eq (display-x-display (drawable-display dest))
	       (display-x-display (gcontext-display gcontext))))
      (error "Source, gcontext and destination must be on same display."))
  (x-copy-area (int-sap (display-x-display (gcontext-display gcontext)))
	       (int-sap (drawable-x-drawable source))
	       (int-sap (drawable-x-drawable dest))
	       (int-sap (gcontext-x-gcontext gcontext))
	       x y width height
	       dest-x dest-y))

(def-alien-routine ("XGetGeometry" x-get-geometry)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display
  (drawable (* t)) ; type Drawable
  (root (* t)) ; type Window*
  (x (* int))
  (y (* int))
  (width (* unsigned-int))
  (height (* unsigned-int))
  (border-width (* unsigned-int))
  (depth (* unsigned-int)))

(def-alien-routine ("XMoveResizeWindow" x-move-resize-window)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display
  (window system-area-pointer) ; type Window*
  (x int)
  (y int)
  (width unsigned-int)
  (height unsigned-int))

;; FIX in e:bit-screen these are called inside a with-state
;; FIX hard to find in editor
;;; Public
;;;
(macrolet ((frob (slot)
	     `(defun ,(read-from-string (format () "drawable-~A" slot))
		     (drawable)
		(with-alien ((root (* t)) ; type Window
			     (x int)
			     (y int)
			     (width unsigned-int)
			     (height unsigned-int)
			     (border-width unsigned-int)
			     (depth unsigned-int))
		  (x-get-geometry (int-sap
				   (display-x-display
				    (drawable-display drawable)))
				  (int-sap
				   (drawable-x-drawable drawable))
				  (addr root)
				  (addr x)
				  (addr y)
				  (addr width)
				  (addr height)
				  (addr border-width)
				  (addr depth))
		  ,slot))))
  (frob x)
  (frob y)
  (frob width)
  (frob height))

(defun move-and-resize-window (window
			       &key x y width height)
  (declare (type window window))
  (let ((x-display-sap (int-sap
			(display-x-display
			 (drawable-display window))))
	(x-drawable-sap (int-sap
			 (drawable-x-drawable window))))
    (with-alien ((root (* t)) ; type Window
		 (window-x int)
		 (window-y int)
		 (window-width unsigned-int)
		 (window-height unsigned-int)
		 (border-width unsigned-int)
		 (depth unsigned-int))
      (x-get-geometry x-display-sap
		      x-drawable-sap
		      (addr root)
		      (addr window-x)
		      (addr window-y)
		      (addr window-width)
		      (addr window-height)
		      (addr border-width)
		      (addr depth))
      (x-move-resize-window x-display-sap
			    x-drawable-sap
			    (or x window-x)
			    (or y window-y)
			    (or width window-width)
			    (or height window-height)))))

(defun set-drawable-x (drawable x)
  (move-and-resize-window drawable :x x))
;;
(defsetf drawable-x set-drawable-x
  "Set the x of $drawable to $x.")

(defun set-drawable-y (drawable y)
  (move-and-resize-window drawable :y y))
;;
(defsetf drawable-y set-drawable-y
  "Set the y of $drawable to $y.")

(defun set-drawable-width (drawable width)
  (move-and-resize-window drawable :width width))
;;
(defsetf drawable-width set-drawable-width
  "Set the width of $drawable to $width.")

(defun set-drawable-height (drawable height)
  (move-and-resize-window drawable :height height))
;;
(defsetf drawable-height set-drawable-height
  "Set the height of $drawable to $height.")

(def-alien-routine ("XDrawImageString" x-draw-image-string) int
  (display system-area-pointer) ; type Display*
  (drawable (* t)) ; type Drawable
  (gcontext (* t)) ; type GC
  (x int)
  (y int)
  (string c-string)
  (length int))

;;; Public
;;;
(defun translate-default (string)
  "Return as many graphics character as there are from the front of
   $string."
  (loop for i from 0 do
    (if (= i (length string))
	(return-from translate-default string))
    (or (graphic-char-p (aref string i))
	(return-from translate-default (subseq string 0 i)))))

;;; Public
;;;
(defun draw-image-glyphs (drawable gcontext x y string
			  &key (start 0) end (translate #'translate-default))
  "Draw the glyphs in $string from $start to $end onto $drawable at $x $y,
   translating the string with the function $translate-default first."
  (let ((new-string (funcall translate (subseq string start end))))
    (with-alien ((x-string c-string :local new-string))
      (x-draw-image-string (int-sap (display-x-display
				     (drawable-display drawable)))
			   (int-sap (drawable-x-drawable drawable))
			   (int-sap (gcontext-x-gcontext gcontext))
			   x y
			   x-string
			   (length new-string)))))



;;;; Key symbols.

; FIX what type is KeyCode?
(def-alien-type x-key-code-type int)

(def-alien-routine ("XKeycodeToKeysym" x-keycode-to-keysym) int
  (display system-area-pointer) ; type Display
  (keycode x-key-code-type) ; type KeyCode
  (index int))

;;; Public
;;;
(defun keycode->keysym (display code index)
  "Return the key sumbol for $code on $display."
  (x-keycode-to-keysym (int-sap (display-x-display display))
		       code index))

;;; Public
;;;
(defun default-keysym-index (display scan-code bits)
  "Return the keysym index for $scan-code and $bits on $display."
;   (with-alien ((keysyms-per-keycode int :locals 0))
;     (x-get-keyboard-mapping (int-sap (display-x-display display))
; 			    scan-code
; 			    1
; 			    (addr keysyms-per-keycode))
;     (+ (* (- scan-code first-keycode) keysyms-per-keycode) 0))
  ;; FIX what should this do?
  0)


;;;; Cut buffer.

(def-alien-routine ("XFetchBytes" x-fetch-bytes)
		   system-area-pointer ; type char*
  (display system-area-pointer) ; type Display
  (nbytes (* int)))

(def-alien-routine ("XFetchBuffer" x-fetch-buffer)
		   system-area-pointer ; type char*
  (display system-area-pointer) ; type Display
  (nbytes (* int))
  (buffer int))

;;; Public
;;;
(defun cut-buffer (display &optional (n 0))
  "Return the text in the cut buffer on $display."
  (with-alien ((nbytes int)
	       (bytes (* char)
		      :local
		      (x-fetch-buffer (int-sap
				       (display-x-display display))
				      (addr nbytes)
				      n)))
    (if (plusp nbytes) (cast bytes c-string))))

(def-alien-routine ("XStoreBytes" x-store-bytes) int
  (display system-area-pointer) ; type Display
  (bytes c-string)
  (nbytes int))

(def-alien-routine ("XStoreBuffer" x-store-buffer) int
  (display system-area-pointer) ; type Display
  (bytes c-string)
  (nbytes int)
  (buffer int))

(defun set-cut-buffer (display string)
  "Add $string to the cut buffer of $display."
  (x-store-buffer (int-sap (display-x-display display))
		  string
		  (length string)
		  0))
;;
(defsetf cut-buffer set-cut-buffer
  "Store $string in the cut buffer on $display.")


;;;; Window manager hints and properties.

(def-alien-type ()
  (struct x-wm-hints
    (flags long)
    (input boolean)
    (initial-state int)
    (icon-pixmap (* t)) ; type Pixmap
    (icon-window system-area-pointer) ; type Window
    (icon-x int)
    (icon-y int)
    (icon-mask (* t)) ; type Pixmap
    (window-group (* t)))) ; type XID

(def-alien-routine ("XGetWMHints" x-get-wm-hints)
		   system-area-pointer ; type (* (struct x-wm-hints))
  (display system-area-pointer) ; type Display*
  (window system-area-pointer)) ; type Window

(def-alien-routine ("XSetWMHints" x-set-wm-hints) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (wmhints (* (struct x-wm-hints))))

(defun set-wm-input-hint (display window bool)
  "Set the input hint on $window on $display to $bool."
  (with-alien ((hints (* (struct x-wm-hints))
		      :local
		      (sap-alien (x-get-wm-hints display window)
				 (* (struct x-wm-hints)))))
    (fi* (null-alien hints)
      (setf (slot hints 'input) bool)
      (x-set-wm-hints display window hints))))

(def-alien-routine ("XGetWMNormalHints" x-get-wm-normal-hints) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (sizehints (* t)) ; type XSizeHints*
  (supplied-return (* long)))

(def-alien-type ()
  (struct aspect
    (x int)
    (y int)))

(def-alien-type ()
  (struct x-size-hints
    (flags long)
    ;; historic
    (x int)
    (y int)
    (width int)
    (height int)
    ;;
    (min-width int)
    (min-height int)
    (max-width int)
    (max-height int)
    (width-inc int)
    (height-inc int)
    (min-aspect (struct aspect))
    (max-aspect (struct aspect))
    (base-width int)
    (base-height int)
    (win-gravity int)))

#|
(DEFUN X-ALLOC-SIZE-HINTS ()
  (WITH-ALIEN
   ((X-ALLOC-SIZE-HINTS #'SYSTEM-AREA-POINTER :EXTERN "XAllocSizeHints"))
   (VALUES (ALIEN-FUNCALL X-ALLOC-SIZE-HINTS))))

(COMPILER-LET ((ALIEN::*AUXILIARY-TYPE-DEFINITIONS* 'NIL))
  (SYMBOL-MACROLET ((X-ALLOC-SIZE-HINTS
                     (ALIEN::%HEAP-ALIEN
                      '#<ALIEN::HEAP-ALIEN-INFO (FOREIGN-SYMBOL-ADDRESS
                                                 '"XAllocSizeHints") #'SYSTEM-AREA-POINTER>)))
    (VALUES (ALIEN-FUNCALL X-ALLOC-SIZE-HINTS))))
|#

(def-alien-routine ("XAllocSizeHints" x-alloc-size-hints)
		   ; type (* (struct x-size-hints))
		   system-area-pointer)

#|
(def-alien-variable ("pAllHints" x-p-all-hints) int)
(def-alien-variable ("uSPosition" x-us-position) int)
(def-alien-variable ("uSSize" x-us-size) int)
|#

;;; Public
;;;
(defun wm-normal-hints (window)
  "Return the normal hints for $window, if any, else ()."
  (with-alien ((hints (* (struct x-size-hints))
		      :local
		      (sap-alien (x-alloc-size-hints)
				 (* (struct x-size-hints)))))
    (fi* (null-alien hints)
      ;(setf (slot hints 'flags) x-p-all-hints)
      ;(setf (slot hints 'flags) (logior 4 8 16 32 64 128))
      (setf (slot hints 'flags) 4) ;(logior 4 8 16 32 64 128))
      (with-alien ((supplied-return long :local 0))
	(if (zerop (x-get-wm-normal-hints
		    (int-sap (display-x-display (window-display window)))
		    (int-sap (window-x-drawable window))
		    hints
		    (addr supplied-return)))
	    (prog2
	     (x-free hints)
	     ())
	    ;; FIX free on collection?
	    hints)))))

(def-alien-routine ("XSetWMNormalHints" x-set-wm-normal-hints) int
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (hints (* t))) ; type XSizeHints*

(defun set-wm-normal-hints (window hints)
  "Set the normal hints of $window to $hints."
  (x-set-wm-normal-hints
   (int-sap (display-x-display (window-display window)))
   (int-sap (window-x-drawable window))
   hints))
;;
(defsetf wm-normal-hints set-wm-normal-hints
  "Set the normal hints of $window to $hints.")

;;; Public
;;;
(defun wm-size-hints-x (hints)
  "Return the x value of $hints."
  (slot hints 'x)) ; FIX historic slot

;;; Public
;;;
(defun wm-size-hints-y (hints)
  "Return the y value of $hints."
  (slot hints 'y)) ; FIX historic slot

;;; Public
;;;
(defun wm-size-hints-width-inc (hints)
  "Return the width increment value of $hints."
  (slot hints 'width-inc))

;;; Public
;;;
(defun wm-size-hints-height-inc (hints)
  "Return the height increment value of $hints."
  (slot hints 'height-inc))

;;; Public
;;;
(defun wm-size-hints-min-width (hints)
  "Return the minimum width value of $hints."
  (slot hints 'min-width))

;;; Public
;;;
(defun wm-size-hints-min-height (hints)
  "Return the minimun height value of $hints."
  (slot hints 'min-height))

(def-alien-type ()
  (struct x-class-hint
    (res-name c-string)
    (res-class c-string)))

(def-alien-type ()
  (struct x-text-property
    (value (* unsigned-char))
    (encoding system-area-pointer) ; type Atom
    (format int)
    (nitems unsigned-long)))

(def-alien-routine ("XSetWMProperties" x-set-wm-properties) void
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (window-name (* (struct x-text-property)))
  (icon-name (* (struct x-text-property)))
  (argv (* (* char)))
  (argc int)
  (normal-hints (* (struct x-size-hints)))
  (wm-hints (* (struct x-wm-hints)))
  (class-hints (* (struct x-class-hint))))

(def-alien-routine ("XStringListToTextProperty"
		    x-string-list-to-text-property)
		   system-area-pointer ; type Status
  (list (* (* char)))
  (count int)
  (text-prop-return (* (struct x-text-property))))

(def-alien-routine ("XTextPropertyToStringList"
		    x-text-property-to-string-list)
		   system-area-pointer ; type Status
  (text-prop (* (struct x-text-property)))
  (list-return (* (* (* char))))
  (count-return (* int)))

(def-alien-routine ("XGetClassHint" x-get-class-hint)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (class-hints-return (* t))) ; type XClassHint*

(def-alien-routine ("XAllocClassHint" x-alloc-class-hint)
		   ; type (* (struct x-class-hint))
		   system-area-pointer)

(def-alien-routine ("XFree" x-free) int
  (data (* t)))

(def-alien-routine ("XSetWMName" x-set-wm-name) void
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (window-name (* (struct x-text-property))))

(def-alien-routine ("XFetchName" x-fetch-name)
		   system-area-pointer ; Status
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (window-name (* c-string)))

;;; Public
;;;
(defun window-name (window)
  (with-alien ((string c-string))
    (fi (zerop
	 (sap-int (x-fetch-name (int-sap (display-x-display
					  (window-display window)))
				(int-sap (window-x-drawable window))
				      (addr string))))
	string)))

#|
(def-alien-routine ("XGetWMName" x-get-wm-name)
		   system-area-pointer ; type Status
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (window-name (* (struct x-text-property))))

;;; Public
;;;
(defun window-wm-name (window)
  (with-alien ((x-name-prop (struct x-text-property))
	       (x-string-list (* (* char)))
	       (x-count int :local 0))
    (x-get-wm-name
     (int-sap (display-x-display (window-display window)))
     (int-sap (window-x-drawable window))
     (addr x-name-prop))
    (x-text-property-to-string-list (addr x-name-prop)
				    (addr x-string-list)
				    (addr x-count))
    (if (plusp x-count)
	(cast (deref x-string-list) c-string))))
|#

;;; Public
;;;
(defun set-wm-properties (window
			  &key name
			       icon-name
			       resource-name
			       x y
			       width height
			       user-specified-position-p
			       user-specified-size-p
			       width-inc
			       height-inc
			       min-width
			       min-height
			       input)
  "Set properties of $window."
  ;; $input  used in ed as :on to "tell OpenLook pseudo-X11 server we want
  ;;         input".
  (declare (ignore input))
  (or window (error "$window ()"))
  (with-alien ((x-name-prop (struct x-text-property))
	       (x-icon-prop (struct x-text-property))
	       (x-class-hint system-area-pointer :local (int-sap 0)))
    (if name
	(with-alien ((x-name c-string :local name))
	  (x-string-list-to-text-property (addr x-name)
					  1
					  (addr x-name-prop))
	  (x-set-wm-name
	   (int-sap (display-x-display (window-display window)))
	   (int-sap (window-x-drawable window))
	   (addr x-name-prop))))
    (if icon-name
	(with-alien ((x-icon c-string :local icon-name))
	  (x-string-list-to-text-property (addr x-icon)
					  1
					  (addr x-icon-prop))))
    (let ((normal-hints (wm-normal-hints window))
	  (window (int-sap (window-x-drawable window)))
	  (display (int-sap (display-x-display
			     (window-display window)))))
      (or normal-hints
	  (progn
	    (setq normal-hints (sap-alien (x-alloc-size-hints)
					  (* (struct x-size-hints))))
	    (if (null-alien normal-hints)
		(setq normal-hints ())
		(logior (slot normal-hints 'flags) 0))))
      (when normal-hints
	;; FIX maybe clear (or set ppos) if u-s-p-p is ()a
	(if user-specified-position-p
	    ;; FIX may need to clear Pposition
	    (setf (slot normal-hints 'flags)
		  (logior (slot normal-hints 'flags) 1))) ; 1 was x-us-position
	(if user-specified-size-p
	    ;; FIX may need to clear Psize
	    (setf (slot normal-hints 'flags)
		  (logior (slot normal-hints 'flags) 2))) ; 2 was x-us-size
	(when x
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 4))
	  (setf (slot normal-hints 'x) x))
	(when y
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 4))
	  (setf (slot normal-hints 'y) y))
	(when width
	  ;; FIX maybe requires next key (more below)
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 8))
	  (setf (slot normal-hints 'width) width))
	(when height
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 8))
	  (setf (slot normal-hints 'height) height))
	(when width-inc
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 64))
	  (setf (slot normal-hints 'width-inc) width-inc))
	(when height-inc
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 64))
	  (setf (slot normal-hints 'height-inc) height-inc))
	(when min-width
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 16))
	  (setf (slot normal-hints 'min-width) min-width))
	(when min-height
	  (setf (slot normal-hints 'flags)
		(logior (slot normal-hints 'flags) 16))
	  (setf (slot normal-hints 'min-height) min-height)))
      (when resource-name
	(setq x-class-hint (x-alloc-class-hint))
	(fi* (zerop (sap-int x-class-hint))
	  (x-get-class-hint display window x-class-hint)
	  (setf (slot (sap-alien x-class-hint
				 (* (struct x-class-hint)))
		      'res-name)
		resource-name)))
      (x-set-wm-properties display
			   window
			   (if name
			       (addr x-name-prop)
			       (sap-alien (int-sap 0)
					  (* (struct x-text-property))))
			   (if icon-name
			       (addr x-icon-prop)
			       (sap-alien (int-sap 0)
					  (* (struct x-text-property))))
			   (sap-alien (int-sap 0) (* (* char))) ; argv
			   0 ; argc
			   (or normal-hints
			       (sap-alien (int-sap 0)
					  (* (struct x-size-hints))))
			   (sap-alien (int-sap 0) (* (struct x-wm-hints)))
			   (sap-alien x-class-hint
				      (* (struct x-class-hint))))
      (when resource-name
	(x-free x-class-hint)))))


;;;; Bell.

(def-alien-routine ("XBell" x-bell) int
  (display system-area-pointer) ; type Display
  (percent int))

(defun bell (display &optional (percent 100))
  (x-bell (int-sap (display-x-display display)) percent))


;;;; Test.
#|
(font-path d)
(cut-buffer d)
(display-finish-output d)
(display-force-output d)

---

(setq d (open-display () :display 0.0))

(setq w (create-window :parent (screen-root (car (display-roots d)))
		       :width 150 :height 150
		       :background (screen-white-pixel
				    (display-default-screen d))))
(setq m (make-event-mask :key-press :button-press :button-release :enter-window :leave-window :exposure))
(setq m (make-event-mask :key-press :button-press :button-release :enter-window :leave-window :exposure
			 :graphics-exposure :no-exposure :resize-request))

(setq m (make-event-mask :button-press :button-release :enter-window :leave-window :exposure
			 :graphics-exposure :no-exposure :resize-request))

(setf (window-event-mask w) m)

(window-event-mask w)
(event-listen d)

(map-window w)

---

(setq w (create-window :parent (screen-root (car (display-roots d)))
		       :width 50 :height 50
		       :border-width 100
		       :background (screen-white-pixel
				    (display-default-screen d))
		       :class :input))

---

(setf (window-background w)
      (screen-white-pixel (display-default-screen d)))
(clear-window w)

(setf (window-background w)
      (screen-black-pixel (display-default-screen d)))
(clear-area w :x 0 :y 0 :width 50 :height 50 :generate-exposures-p t)
(clear-area w :x 50 :y 50 :width 50 :height 50 :generate-exposures-p t)

(setq s (display-default-screen d))
(setq g (create-gcontext :drawable w
			 :function boole-1
			 :foreground (screen-black-pixel s)
			 :background (screen-white-pixel s)))

(draw-point w g 100 100)
(draw-point w g 101 101)
(draw-point w g 102 102)
(draw-point w g 103 103)

(draw-point w g 110 110)

(draw-line w g 103 103 35 106)

(draw-rectangle w g 35 70 50 50 t)
(draw-rectangle w g2 35 70 50 50 t)
(draw-rectangle w g2 35 70 50 50)

(draw-image-glyphs w g 35 70 "abc")
(draw-image-glyphs w g 10 70 "abc")

(copy-area w g 0 0 10 10 w2 0 0)
(copy-area w g 0 0 10 10 w2 10 10)
(copy-area w g 0 0 10 10 w2 20 20)

---

(draw-image-glyphs w g 10 10 "top")

(setq g2 (create-gcontext :drawable w
			  :function boole-1
			  :foreground (screen-white-pixel s)
			  :background (screen-black-pixel s)))

(setq i (get-image w g2 :x 0 :y 0 :width 50 :height 50))
(put-image w g2 i :x 100 :y 100 :width 50 :height 50)

(setq p (create-pixmap :width 50 :height 50 :x-drawable w))
(copy-area w g2 0 0 50 50 p 0 0)

(draw-image-glyphs p g 10 10 "abc")
(setq i (get-image p g :x 0 :y 0 :width 50 :height 50))
(put-image w g i :x 50 :y 50 :width 50 :height 50)

(draw-image-glyphs p g2 0 0 "abc")
(setq i (get-image p g2 :x 0 :y 0 :width 50 :height 50))
(put-image w g2 i :x 50 :y 50 :width 50 :height 50)

---

(setq s (display-default-screen d))
(setq g2 (create-gcontext :drawable w
			  :function boole-1
			  :foreground (screen-white-pixel s)
			  :background (screen-black-pixel s)))
(setq g (create-gcontext :drawable w
			 :function boole-1
			 :foreground (screen-black-pixel s)
			 :background (screen-white-pixel s)))


(setq p (create-pixmap :width 50 :height 50 :x-drawable (screen-root s)))
(copy-area w g2 0 0 50 50 p 0 0)

(write-bitmap-file p ":tmp/test2.pixmap")

(setq i (get-image w g :x 0 :y 0 :width 50 :height 50))


(setq p (create-pixmap :x-drawable (screen-root s) :width 50 :height 50 :depth 1))
(setq p (create-pixmap :x-drawable w :width 50 :height 50 :depth 1))
(setq b (xlib:read-bitmap-file (screen-root s) "/home/mattm/tmp/test.pixmap"))

(setq i (get-image b g2 :x 0 :y 0 :width 50 :height 50))
(setq i (get-image b g :x 0 :y 0 :width 50 :height 50))
(setq i (get-image b g :x 0 :y 0 :width 16 :height 16))
(setq i (get-image b g :x 0 :y 0 :width 10 :height 10))
(put-image w g i :x 0 :y 0 :width 16 :height 16)
(put-image w g2 i :x 0 :y 0 :width 16 :height 16)
(put-image w g2 i :x 50 :y 50 :width 50 :height 50)


---

;; orig
(defun get-cursor-pixmap (root pathname)
  (let* ((image (xlib:read-bitmap-file pathname))
	 (pixmap (xlib:create-pixmap :width 16 :height 16
				     :depth 1 :x-drawable root))
	 (gc (xlib:create-gcontext
	      :drawable pixmap :function boole-1
	      :foreground (screen-black-pixel  (xlib:display-default-screen d))
	      :background (screen-white-pixel  (xlib:display-default-screen d)))))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width 16 :height 16)
    (xlib:free-gcontext gc)
    (values pixmap (xlib:image-x-hot image) (xlib:image-y-hot image))))

(defun get-cursor-pixmap (root pathname)
  (let* ((image (xlib:read-bitmap-file pathname))
	 (pixmap (xlib:create-pixmap :width 16 :height 16
				     :depth 1 :x-drawable root))
	 (gc (xlib:create-gcontext
	      :drawable pixmap :function boole-1
	      :foreground (screen-black-pixel  (xlib:display-default-screen d))
	      :background (screen-white-pixel  (xlib:display-default-screen d)))))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width 16 :height 16)
    (xlib:free-gcontext gc)
    (values pixmap (xlib:image-x-hot image) (xlib:image-y-hot image))))

(setq b (xlib:read-bitmap-file w (truename "e:hemlock11.cursor")))
(setq b (xlib:read-bitmap-file  "e:hemlock11.cursor"))
(get-cursor-pixmap (screen-root s) (truename "e:hemlock11.cursor"))
(setq p (xlib:create-pixmap :width 16 :height 16 :depth 1 :x-drawable (screen-root s)))
(setq gc (xlib:create-gcontext
	  :drawable p :function boole-1
	  :foreground (screen-black-pixel  (xlib:display-default-screen d))
	  :background (screen-white-pixel  (xlib:display-default-screen d))))
(put-image p gc b :x 0 :y 0 :width 16 :height 16)

(multiple-value-bind (cursor-pixmap cursor-x-hot cursor-y-hot)
		     (get-cursor-pixmap (screen-root s) "e:hemlock11.cursor")
  (setq c (xlib:create-cursor :source cursor-pixmap
			      :mask (get-cursor-pixmap
				     (screen-root s)
				     (truename "e:hemlock11.mask"))
			      :x cursor-x-hot :y cursor-y-hot
			      :foreground (make-white-color)
			      :background (make-black-color))))

(setf (window-cursor w) c)

---

(setq n (wm-normal-hints w))

(set-wm-properties
 w
 :name "test name")

(set-wm-properties
 w
 :icon-name "test name")

(set-wm-properties
 w
 :resource-name "test resource")


(set-wm-properties
 w
 :x 150
 :y 150)

(set-wm-properties
 w
 :min-width 100
 :min-height 100)

(set-wm-properties
 w
 :width-inc 25
 :height-inc 25)

(def-alien-routine ("XSetWMNormalHints" x-set-wm-normal-hints) void
  (display system-area-pointer) ; type Display*
  (window system-area-pointer) ; type Window
  (size-hints (* (struct x-size-hints))))

(let ((normal-hints (wm-normal-hints w))
      (window (int-sap (window-x-drawable w)))
      (display (int-sap (display-x-display (window-display w)))))
  (or normal-hints
      (setq normal-hints (x-alloc-size-hints)))
  (setf (slot normal-hints 'flags)
	(logior (slot normal-hints 'flags) 16))
  (setf (slot normal-hints 'min-width) 10)
  (x-set-wm-normal-hints display window normal-hints))

(set-wm-properties
 w
 :resource-name "test resource"
 :width 50
 :height 50
 :user-specified-position-p t
 :user-specified-size-p t
 :width-inc 1
 :height-inc 1
 :min-width 10
 :min-height 10)





(multiple-value-bind (children parent root)
		     (query-tree w)
  children
  parent
  roo)



(with-alien ((attribs (struct x-window-attributes)))
  (x-get-window-attributes (int-sap (display-x-display (window-display w)))
			   (int-sap (window-x-drawable w))
			   (addr attribs))
  (slot attribs 'width))



(event-listen d)
(while () ((event-listen d))
  (process-event
   d
   :timeout 0
   :handler (lambda (&key event-key event-window window x y height time keycode &allow-other-keys)
	      (format t "event-key: ~A~%" event-key)
	      (format t "event-window: ~A~%" event-window)
	      (when (eq event-window (window-x-drawable w))
		(format t "event-key: ~A~%" event-key)
		(format t "event-window: ~A~%" event-window)
		(format t "time: ~A~%" time)
		(format t "x: ~A~%" x)
		(format t "y: ~A~%" y)
		(format t "height: ~A~%" height)
		(format t "keycode: ~A~%" keycode)
		;(draw-rectangle p g 0 0 0 30 t)
		t))))


(event-listen d)

(while () ((event-listen d))
 (process-event
 d
 :timeout 0
 :handler (lambda (&key event-key event-window window x y height time keycode &allow-other-keys)

	    (format t "handling~%")
	    (format t "event-window: ~A~%" event-window)
	    (when (eq (alien-sap event-window)
		      (window-x-drawable w))
	      (format t "event-key: ~A~%" event-key)
	      (format t "event-window: ~A~%" event-window)
	      (format t "time: ~A~%" time)
	      (format t "x: ~A~%" x)
	      (format t "y: ~A~%" y)
	      (format t "height: ~A~%" height)
	      (format t "keycode: ~A~%" keycode)
	      ;(draw-rectangle p g 0 0 0 30 t)
	      ())
	    ())))

(window-x-drawable w) 14680065

(format t "handling...~%")


;(loop

(defun f (d)
 (with-alien ((event (union x-event)))
    (when (x-peek-event (int-sap (display-x-display d)) (addr event))
      (with-alien ((event (* (struct x-any-event))
			  :local (cast (addr event)
				       (* (struct x-any-event)))))
	(let ((event-window (slot (deref event) 'window))
	      (event-key (slot (deref event) 'type)))
	  (format t "event type: ~A~%" event-key)
	  (format t "event key: ~A~%" (key-from-type event-key))
	  (format t "event-window: ~A~%" event-window))))))

)

(event-listen d)
(loop
  (with-alien ((event (union x-event)))
    (when (x-window-event (int-sap (display-x-display d))
			  (int-sap (window-x-drawable w))
			  m
			  (addr event))
      (let ((event-window (slot event 'window))
	    (event-key (slot event 'type)))
	(ed::msg "event type: ~A" event-key)
	(ed::msg "event key: ~A" (key-from-type event-key))
	(ed::msg "event-window: ~A" event-window)
	;(draw-rectangle p g 0 0 0 30 t)
	))))


(event-case (d :timeout 0)
  (t () t))

(with-state (w)
  (drawable-height w))

(unmap-window w)
(destroy-window w)
(unmap-window w2)
(destroy-window w2)
(close-display d)

(with-alien ((x-icon-prop (struct x-text-property)))
  (with-alien ((x-icon c-string :local "name"))
    (x-string-list-to-text-property (addr x-icon) 1 (addr x-icon-prop))
    (with-alien ((x c-string :local (slot x-icon-prop 'value)))
      x)))

(defvar sss t)

(defun serve-alien-caller-2 (one two)
  (declare (ignore one two))
  (setq sss 1)
  ;(error "s-a-c-2")
  )

(def-alien-routine ("call_serve_alien_caller_2" call-serve-alien-caller-2) int
  (display system-area-pointer)
  (event (* t)))

(with-alien ((a (* t))
	     (b (* t)))
  (call-serve-alien-caller-2 a b))


(defun serve-alien-caller-0 ()
  (format t "s-a-c-0"))

(def-alien-routine ("call_serve_alien_caller_0" call-serve-alien-caller-0) int
  (display system-area-pointer)
  (event (* t)))

(with-alien ((a (* t))
	     (b (* t)))
  (call-serve-alien-caller-0 a b))

xxx

(progn
  (setq d (open-display () :display 0.0))
  (setq w (create-window :parent (screen-root (car (display-roots d)))
			 :width 150 :height 150
			 :background (screen-white-pixel
				      (display-default-screen d))))
  (setq w2 (create-window :parent (screen-root (car (display-roots d)))
			  :width 150 :height 150
			  :background (screen-white-pixel
				       (display-default-screen d))))
  (setq m (make-event-mask :key-press :button-press :button-release :enter-window :leave-window :exposure))
  (setf (window-event-mask w) m)
  (setf (window-event-mask w2) m)

  (map-window w)
  (map-window w2))

(progn
  (setq d (open-display () :display 0.0))
  (setq w (create-window :parent (screen-root (car (display-roots d)))
			 :width 150 :height 150
			 :background (screen-white-pixel
				      (display-default-screen d))
			 :input ()
			 :override-redirect t))
  ;(setq m (make-event-mask :key-press :button-press :button-release :enter-window :leave-window :exposure))
  (xlib:set-wm-properties
   w :name "test window" ; :icon-name icon-name
   :resource-name "Test Window"
   :x 0 :y 0 :width 150 :height 150
   :user-specified-position-p t :user-specified-size-p t
   :width-inc 10 :height-inc 20
   :min-width 100 :min-height 100
   ;; Tell OpenLook pseudo-X11 server we want input.
   :input :on)
  (setq m
	(make-event-mask :key-press ; :key-release
			 :button-press :button-release
			 :enter-window :leave-window
			 :exposure
			 :structure-notify ; :configure-notify
			 :focus-in
			 ;;
 			 :graphics-exposure
 			 :focus-out
 			 :visibility-notify
 			 :resize-request
;			 :motion-notify
			 :property-notify
			 ;:selection-clear :selection-notify :selection-request
			 :client-message
			 :circulate-notify :circulate-request
			 :gravity-notify
			 :map-request
			 :reparent-notify
			 ))
  ;(setq m #b000000000000000000001)  ; x-key-press-mask
  ;        #b110101000000000111101
  ;(setq m #b000000000000000000010)  ; x-key-release-mask
  ;(setq m #b000000000000001000000)  ; x-motion-notify-mask
  ;(setq m #b111111111111111111111)  ; all
  ;(setq m #b000000011111110000000)  ; next 7, yet to see event
  ;(setq m #b000000000000010000000)  ; yet to see event
  ;(setq m #b000000000000100000000)  ; yet
  ;(setq m #b000000000001000000000)  ; err on roll when holding down roller (also motion-notify)
  ;(setq m #b000000000010000000000)  ; yet
  ;(setq m #b000000000100000000000)  ; yet
  ;(setq m #b000000001000000000000)  ; yet
  ;(setq m #b000000010000000000000)  ; yet
  ;(setq m #b000000100000000000000)  ; x-keymap-notify-mask
  ;(setq m #b111111011111111111111)  ; all save keymap-notify
  ;(setq m #b000001000000000000000)  ; x-exposure-mask
  ;(setq m #b000010000000000000000)  ; FIX
  ;(setq m #b000100000000000000000)  ; x-structure-notify-mask
  ;(setq m #b001000000000000000000)  ; x-resize-redirect-mask, x-visibility-notify-mask
  ;        #b110101000000000111101
  ;(setq m #b100000000000000000000)  ; yet to see
  ;(setq m #b1000000000000000000000)  ; err on start (focus-in)
  ;(setq m #b10000000000000000000000)  ; err on start FIX
  ;(setq m #b100000000000000000000000)  ; yet see
  ;(setq m #b1000000000000000000000000)  ; yet see
  ;(setq m #b11111111110000000000000000000000000)  ; + 10  yet
  ;(setq m #b1111111111111111111100000000000000000000000000000000000)  ; + 20  yet
  ;(setq m #b111111111111111111110000000000000000000000000000000000000000000000000000000)  ; + 20  yet
  ;(setq m #b11111111111111111111111111111111111111111100000000000000000000000)

  (setf (window-event-mask w) m)
  (map-window w))

(translate-coordinates w 0 0 w2)

(bell d 1)

(with-alien ((event (union x-event)))
  (x-check-mask-event (int-sap (display-x-display d))
		      (x-event-mask-of-screen
		       (int-sap
			(display-default-screen d)))
		      (addr event)))

(x-event-mask-of-screen
 (int-sap (display-default-screen d)))

(with-alien ((event (union x-event)))
  (addr event))

(display-x-display d)

---

(def-alien-routine ("XPeekEvent" x-peek-event) int
  (display system-area-pointer) ; type Display*
  (event-return (* (union x-event))))

(event-listen d)

(with-alien ((event (union x-event)))
  (if (x-peek-event (int-sap (display-x-display d)) (addr event))
      (format t "peeked 1~%")
      (format t "peeked 0~%")))


(with-alien ((event (union x-event)))
  (when (x-peek-event (int-sap (display-x-display d)) (addr event))
    (with-alien ((xany (* (struct x-any-event))
		       :local
		       (cast (addr event) (* (struct x-any-event)))))
      (let ((event-window (slot xany 'window))
	    (event-key (slot xany 'type)))
	(when (eq event-window (window-x-drawable w))
	  (format t "event type: ~A~%" event-key)
	  (format t "event key: ~A~%" (key-from-type event-key))
	  (format t "event-window: ~A~%" event-window))))))


---

(with-alien ((event (union x-event)))
  (when (x-next-event (int-sap (display-x-display d)) (addr event))
    (with-alien ((xany (* (struct x-any-event))
		       :local
		       (cast (addr event) (* (struct x-any-event)))))
      (let ((event-window (slot xany 'window))
	    (event-key (slot xany 'type)))
	(format t "event-window: ~A~%" event-window)
	(when (eq event-window (window-x-drawable w))
	  (format t "event type: ~A~%" event-key)
	  (format t "event key: ~A~%" (key-from-type event-key))
	  (format t "event-window: ~A~%" event-window))))))

---

(event-listen d)

(event-case (d :timeout 0)
  ((:KEY-PRESS :KEY-RELEASE :BUTTON-PRESS :BUTTON-RELEASE)
   (event-key event-window root child same-screen-p
	      x y root-x root-y state time code send-event-p)
   (format t "event-window: ~A~%" event-window)
   )
  (:FOCUS-IN
   (event-window mode detail send-event-p)
   (format t "focus-in~%")
   (format t "event-window: ~A~%" event-window))
  (:SELECTION-NOTIFY
   (event-window)
   (format t "selection-notify~%")
   (format t "event-window: ~A~%" event-window)
   t)
  (t (event-key)
   (format t "skipped event ~A~%" event-key)))

(event-case (d :timeout 0)
  ((:KEY-PRESS :KEY-RELEASE :BUTTON-PRESS :BUTTON-RELEASE)
   (key window)
   (format t "window: ~A~%" window)
   )
  (t ()
   (format t "skipped event~%")))

---

(setf (window-background w)
      (screen-white-pixel (display-default-screen d)))
(clear-window w)

---

(defparameter editor-grey-bitmap-data
  '(#*10 #*01))

(defvar *default-foreground-pixel*
  (screen-white-pixel (display-default-screen d)))
(defvar *default-background-pixel*
  (screen-black-pixel (display-default-screen d)))

(defun get-editor-grey-pixmap (display)
  (let* ((screen (xlib:display-default-screen display))
	 (depth (xlib:screen-root-depth screen))
	 (root (xlib:screen-root screen))
	 (height (length editor-grey-bitmap-data))
	 (width (length (car editor-grey-bitmap-data)))
	 (image (apply #'xlib:bitmap-image editor-grey-bitmap-data))
	 (pixmap (xlib:create-pixmap :width width :height height
				     :depth depth :x-drawable root))
	 (gc (xlib:create-gcontext :drawable pixmap
				   :function boole-1
				   :foreground *default-foreground-pixel*
				   :background *default-background-pixel*)))
    (xlib:put-image pixmap gc image
		    :x 0 :y 0 :width width :height height :bitmap-p t)
    (xlib:free-gcontext gc)
    pixmap))

(setq i (apply #'xlib:bitmap-image editor-grey-bitmap-data))

(setq s (display-default-screen d))
(setq g (create-gcontext :drawable w
			 :function boole-1
			 :foreground (screen-black-pixel s)
			 :background (screen-white-pixel s)))

(xlib:put-image w g i
		:x 0 :y 0
		:width (length editor-grey-bitmap-data)
		:height (length (car editor-grey-bitmap-data))
		:bitmap-p t)

---

(setq b (xlib:read-bitmap-file w2 "/home/mattm/tmp/test.pixmap"))
(copy-area b g 0 0 50 50 w2 50 50)
(copy-area b g 0 0 50 50 w 50 50)
(copy-area b g 0 0 10 10 w 50 50)
(copy-area w2 g 0 0 10 10 w2 50 50)

(setq p (xlib:create-pixmap :width 10 :height 10
			    :depth 24 :drawable w))

(x-default-depth-of-screen
 (x-default-screen-of-display (int-sap (display-x-display d))))

(copy-area w2 g 100 100 10 10 p 0 0)
(copy-area p g 0 0 10 10 w 10 10)

(copy-area w2 g 100 100 10 10 w 0 0)

---

(setq data (read-bitmap-file-data "/home/mattm/tmp/test.pixmap"))

(xlib:put-image w g data
		:x 0 :y 0
		:width (length editor-grey-bitmap-data)
		:height (length (car editor-grey-bitmap-data))
		:bitmap-p t)

---

(setq c (create-font-cursor d 0))
(setf (window-cursor w) c)

(setf (window-cursor w) (create-font-cursor d 1))
(setf (window-cursor w) (create-font-cursor d 2))

(dotimes (i 128)
  (format t "cursor ~A~%" i)
  (setf (window-cursor w) (create-font-cursor d i))
  (sleep 1))

---

(check-type d display)
(type-of d)
(typep d 'display)

(typep w 'window)

====

( rogn
  (setq s (display-default-screen d))

  (setq g (create-gcontext :drawable w
			   :function boole-1
			   :foreground (screen-black-pixel s)
			   :background (screen-white-pixel s)))

  (setq set (system:make-object-set "Test" #'ext:default-clx-event-handler))

  (defun handle-exposed-region (w &optional event-key event-window x y width height
				  foo bar baz quux)
    (declare (ignore event-key event-window x width foo bar baz quux))
    (setf (window-background w)
	  (screen-white-pixel (display-default-screen d)))
    (clear-window w)
    (draw-rectangle w g 35 70 50 50)
    t)

  (handle-exposed w)

  (add-xwindow-object w2 w2 set)

  (ext:serve-key-press set #'handle-exposed-region)

  (ext:enable-clx-event-handling d #'ext:object-set-event-handler))

(add-xwindow-object w w set)
(handle-exposed-region w2)

(ext:serve-graphics-exposure set #'handle-exposed-region)
(ext:serve-no-exposure set #'handle-exposed-region)
(ext:serve-enter-notify set #'handle-exposed-region)
(ext:serve-motion-notify set #'handle-exposed-region)
(ext:serve-leave-notify set #'handle-exposed-region)

==

(progn
  (defvar *counter* 0)

  (setq s (display-default-screen d))

  (setq font
	(or (xlib:open-font d
			    "*-courier-medium-r-normal--*-120-*")
	    (error "failed to open font")))

  (setq g (create-gcontext :drawable w
			   :function boole-1
;			   :font font
			   :foreground (screen-black-pixel s)
			   :background (screen-white-pixel s)))

  (setq set (system:make-object-set "Test" #'ext:default-clx-event-handler))

  (defun handle-key-press (w &optional event-key event-window root child same-screen-p x y
			      root-x root-y modifiers time key-code send-event-p)
    (declare (ignore event-window root child same-screen-p root-x
		     root-y time send-event-p  x y modifiers))
    (format t "handle-key-press ~A~%" key-code)
    (setf (window-background w)
	  (screen-white-pixel (display-default-screen d)))
    (clear-window w)
    (draw-rectangle w g 35 70 50 50)
    (draw-image-glyphs w g 100 10 (format () "~A" (incf *counter*)))

    (xlib:with-gcontext (g :font font)
      (draw-image-glyphs w g 100 100 (format () "~A" event-key)))
    ;(draw-image-glyphs w g 100 100 (format () "~A" event-key))

    (xlib:with-gcontext (g :font font
			   :function xlib:boole-xor
			   :foreground (logxor
					(screen-black-pixel
					 (display-default-screen d))
					(screen-white-pixel
					 (display-default-screen d))))
      (draw-rectangle w g 100 90 10 10 t))

    t)

  (defun handle-key-press2 (w &optional event-key event-window root child same-screen-p x y
			      root-x root-y modifiers time key-code send-event-p)
    (declare (ignore event-window root child same-screen-p root-x
		     root-y time send-event-p  x y modifiers))
    (format t "handle-key-press ~A~%" key-code)
    (setf (window-background w)
	  (screen-white-pixel (display-default-screen d)))
    (clear-window w)
    (draw-rectangle w g 35 70 50 50)
    (draw-image-glyphs w g 100 10 (format () "~A" (incf *counter*)))
    (draw-image-glyphs w g 50 50 (format () "~A" (primary-selection d w)))

    (xlib:with-gcontext (g :font font)
      (draw-image-glyphs w g 100 100 (format () "~A" event-key)))
    ;(draw-image-glyphs w g 100 100 (format () "~A" event-key))

    (xlib:with-gcontext (g :font font
			   :function xlib:boole-xor
			   :foreground (logxor
					(screen-black-pixel
					 (display-default-screen d))
					(screen-white-pixel
					 (display-default-screen d))))
      (draw-rectangle w g 100 90 10 10 t))

    t)

  (defun handle-exposed-region (w &optional event-key event-window x y width height
				  foo bar baz quux aa ab ac ad)
    (declare (ignore event-window x y width foo bar baz quux aa ab ac ad))
    (handler-case
	(progn
	  (setf (window-background w)
		(screen-white-pixel (display-default-screen d)))
	  (clear-window w)
	  (draw-rectangle w g 35 70 100 100)
	  (draw-image-glyphs w g 10 10 (format () "~A" (incf *counter*)))
	  (draw-image-glyphs w g 100 100 (format () "~A" event-key)))
      (error () (format t "handle-exposed-region error~%")))
    t)

  (defun pass (w &optional event-key event-window x y width height
				  foo bar baz quux aa ab ac ad)
    (declare (ignore w event-window x width foo bar baz quux aa ab ac ad))
    (format t "passing ~A~%" event-key)
    t)

  ;(add-xwindow-object w2 w2 set)
  (add-xwindow-object w w set)

  (defun handle-selection-notify (w &optional event-key event-window selection
				    target property time send-event-p)
    (declare (ignore event-key event-window selection target property time send-event-p))
    (format t "selection-notify~%")
    t)

  (ext:serve-key-press set #'handle-key-press2)
  (ext:serve-key-release set #'handle-key-press)
  (ext:serve-button-press set #'handle-key-press)
  (ext:serve-button-release set #'handle-key-press)
  (ext:serve-exposure set #'handle-exposed-region)
  (ext:serve-graphics-exposure set #'handle-exposed-region)
  (ext:serve-no-exposure set #'handle-exposed-region)
  (ext:serve-enter-notify set #'handle-exposed-region)
  (ext:serve-motion-notify set #'handle-exposed-region)
  (ext:serve-leave-notify set #'handle-exposed-region)
  (ext:serve-configure-notify set #'handle-exposed-region)
  (ext:serve-configure-request set #'handle-exposed-region)
  ;(ext:serve-visibility-notify set #'handle-exposed-region)
  (ext:serve-visibility-notify set #'pass)
  (ext:serve-resize-request set #'handle-exposed-region)
  (ext:serve-focus-in set #'pass)
  (ext:serve-focus-out set #'pass)
  (ext:serve-create-notify set #'pass)
  (ext:serve-destroy-notify set #'pass)
  (ext:serve-selection-request set #'pass)
  (ext:serve-selection-clear set #'pass)
  (ext:serve-selection-notify set #'handle-selection-notify)
  (ext:serve-colormap-notify set #'pass)
  ;;
  (ext:serve-gravity-notify set #'pass)
  (ext:serve-unmap-notify set #'pass)
  (ext:serve-map-notify set #'handle-exposed-region)
  (ext:serve-reparent-notify set #'pass)
  (ext:serve-circulate-notify set #'pass)
  (ext:serve-circulate-request set #'pass)
  (ext:serve-client-message set #'pass)
  (ext:serve-gravity-notify set #'pass)
  (ext:serve-property-notify set #'pass)

  (setq ext::*object-set-event-handler-print* t)
  (ext:enable-clx-event-handling d #'ext:object-set-event-handler)

  (display-force-output d)

  (dotimes (i 100000) (system:serve-event)))

(key-from-type (slot #:G123 'type))

(ext:disable-clx-event-handling d)


(setq w (create-window :parent (screen-root (car (display-roots d)))
		       :width 150 :height 150
		       :background (screen-white-pixel (display-default-screen d))))

(setq w3 (make-window :x-drawable (window-x-drawable w) :display d))

==

(setf (window-event-mask w)
      (make-event-mask :key-press :button-press
		       :button-release :enter-window
		       :leave-window :exposure))

(setf (window-event-mask w) most-positive-fixnum)

(setf (window-event-mask w) 18446744073709551615)

(setf (window-event-mask w)
      (make-event-mask :key-press :button-press
		       :button-release :enter-window
		       :leave-window :exposure
		       :graphics-exposure
		       :focus-in :focus-out
		       :visibility-notify
		       :configure-notify
		       :resize-request
		       :motion-notify
		       ))

(defun handle-show-event (display)
  (format t "handle-show-event~%")
  (format t "display ~A~%" display)
  (with-alien ((event (union x-event)))
    (when (x-next-event (int-sap (display-x-display display))
			(addr event))
      (with-alien ((xany (* (struct x-any-event))
			 :local
			 (cast (addr event) (* (struct x-any-event)))))
	(let ((event-window (slot xany 'window))
	      (event-key (slot xany 'type)))
	  (format t "event-window: ~A~%" event-window)
	  (format t "event type: ~A~%" event-key)
	  (format t "event key: ~A~%" (key-from-type event-key))
	  (format t "event-window: ~A~%~%" event-window)))
      t)))

(defvar *show-events-mask*
  ;(make-event-mask :key-press :button-press))
  ; Seems like anything higher than this turns off events.
  #b1111111111111111111111111)

(defun show-events ()
  "Open a window, serve events, list any events served."
  (let* ((d (open-display () :display 0.0))
	 (w (create-window :parent (screen-root (car (display-roots d)))
			   :width 150 :height 150
			   :background (screen-white-pixel
					(display-default-screen d))
			   :input ()
			   :override-redirect t))
	 (w2 (create-window :parent (screen-root (car (display-roots d)))
			    :width 150 :height 150
			    :x 0 :y 150
			    :background (screen-black-pixel
					 (display-default-screen d))
			    :input ()
			    :override-redirect t)))
    (xlib:set-wm-properties
     w :name "test window 1" ; :icon-name icon-name
     :resource-name "Test Window"
     :x 0 :y 0 :width 150 :height 150
     :user-specified-position-p t :user-specified-size-p t
     :width-inc 10 :height-inc 20
     :min-width 100 :min-height 100
     ;; Tell OpenLook pseudo-X11 server we want input.
     :input :on)
    (xlib:set-wm-properties
     w :name "test window 2" ; :icon-name icon-name
     :resource-name "Test Window"
     :x 0 :y 0 :width 150 :height 150
     :user-specified-position-p t :user-specified-size-p t
     :width-inc 10 :height-inc 20
     :min-width 100 :min-height 100
     ;; Tell OpenLook pseudo-X11 server we want input.
     :input :on)
    (setf (window-event-mask w) *show-events-mask*)
    (setf (window-event-mask w2) *show-events-mask*)
    (map-window w)
    (map-window w2)
    (unwind-protect
	(with-clx-event-handling (d #'handle-show-event)
	   (display-force-output d)
	   (format t "serving~%")
	   (dotimes (i 100000) (system:serve-event)))
      (unmap-window w)
      (unmap-window w2)
      (close-display d))))

|#
