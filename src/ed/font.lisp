;;; Various functions that make up the user interface to fonts.

(in-package "EDI")

(export '(color color-mark
	  font-mark #| FIX font-mark-font |#
	  font-mark-fore-color font-mark-back-color
	  delete-font-mark delete-line-font-marks move-font-mark
	  window-font))
;;; Default-font used to be in the above list, but that caused a name
;;; conflict because "Default Font" is an editor variable.  It is now
;;; exported by the export list in rompsite.lisp.

(defvar *default-font-family* (make-font-family))


;;;; Creating, Deleting, and Moving.

(defun font-mark (line charpos font &optional (kind :right-inserting))
  "Return a font on $line at $charpos with $font.  Font marks must be
   permanent marks."
  (or (eq kind :right-inserting)
      (eq kind :left-inserting)
      (error "A Font-Mark must be :left-inserting or :right-inserting."))
  (or (and (>= font 0) (< font font-map-size))
      (error "Font number ~S out of range." font))
  (let ((new (internal-make-font-mark line charpos kind font)))
    (new-font-mark new line)
    (push new (line-marks line))
    new))

(defstruct (color-mapping)
  color            ; Editor colour structure.
  keyword
  doc)

;;; COLOR
;;;
;;; The structure in *color-map* and each color mark.
;;;
(defstruct (color (:constructor %make-color))
  device-colors)

(defun make-color (ed-color)
  (let ((color (%make-color)))
    (collect ((devices))
      (dolist (device *devices*)
	(devices (cons device
		       (funcall (device-make-color device)
				device ed-color))))
      (setf (color-device-colors color) (devices)))
    color))

(defun device-color (device color)
  (cdr (assoc device (color-device-colors color))))

(defvar *color-map-size* 0
  "The length of *color-map* array.")

(defvar *color-map* (make-array 0
				:fill-pointer t
				:element-type 'color-mapping)
  "Mapping of color ID's to display-dependant color representation.")

(defun color-id-from-keyword (keyword)
  (declare (type keyword keyword))
  (position keyword *color-map* :key #'color-mapping-keyword))

(defun color (color)
  (etypecase color
    (string (error "FIX implement color names"))
    (list
     (if color (make-color color)))
    (keyword
     ;; FIX maybe if the search fails and names a colour (:red), add the colour
     (case color
       (:window-foreground :window-foreground)
       (t
	(let ((map (find color *color-map* :key #'color-mapping-keyword)))
	  (if map (color-mapping-color map))))))))

(defun add-color-mapping ()
  (or (vector-push-extend (make-color-mapping) *color-map*)
      (error "Failed to extend color mapping array."))
  (prog1
      *color-map-size*
    (incf *color-map-size*)))

(defun defcolor (keyword color doc)
  (let ((id (or (color-id-from-keyword keyword)
		;; Search for a free entry.
		(color-id-from-keyword ())
		(add-color-mapping))))
    ;; Update the entry.
    (let ((mapping (aref *color-map* id)))
      (setf (color-mapping-keyword mapping) keyword)
      (setf (color-mapping-color mapping) (make-color color))
      (setf (color-mapping-doc mapping) doc))))

;; FIX if a device is made after any ed color structure, then every ed
;; color structure must be updated
;;     maybe update as encounter on device
;;         (ie if assoc fails, create device color)

#|
;; FIX could be per-device, then array could be of specific type
(defun make-device-color-map (device)
  (let ((array (make-array *color-map-size* :fill-pointer t)))
    (dotimes (index *color-map-size*)
      (setf (aref array index)
	    (let ((mapping (aref *color-map* index)))
	      (if (color-mapping-color mapping)
		  (funcall (device-make-color device)
			   device
			   (color-mapping-color mapping))))))
    array))
|#

(defun color-mark (line charpos fore-color
			&optional back-color
			(kind :right-inserting))
  "Return a font mark on $line at $charpos with $color and the current
   font.  Font marks must be permanent marks."
  (or (eq kind :right-inserting)
      (eq kind :left-inserting)
      (error "A Font-Mark must be :left-inserting or :right-inserting."))
  (let ((new (internal-make-font-mark line charpos kind #|FIX|# 0)))
    (setf (font-mark-fore-color new) (color fore-color))
    (setf (font-mark-back-color new) (color back-color))
    (new-font-mark new line)
    (push new (line-marks line))
    new))

(defun delete-font-mark (font-mark)
  "Delete font mark $font-mark."
  (check-type font-mark font-mark)
  (let ((line (mark-line font-mark)))
    (when line
      (setf (line-marks line) (delq font-mark (line-marks line)))
      (nuke-font-mark font-mark line)
      (setf (mark-line font-mark) nil))))

(defun delete-line-font-marks (line)
  "Delete all font marks on $line."
  (dolist (m (line-marks line))
    (when (fast-font-mark-p m)
      (delete-font-mark m))))

(defun move-font-mark (font-mark new-position)
  "Move font mark $font-mark to location of mark $new-position."
  (check-type font-mark font-mark)
  (let ((old-line (mark-line font-mark))
	(new-line (mark-line new-position)))
    (nuke-font-mark font-mark old-line)
    (move-mark font-mark new-position)
    (new-font-mark font-mark new-line)
    font-mark))

(defun nuke-font-mark (mark line)
  (new-font-mark mark line))

(defun new-font-mark (mark line)
  (declare (ignore mark))
  (let ((buffer (line-%buffer line))
	(number (line-number line)))
    (when (bufferp buffer)
      (dolist (w (buffer-windows buffer))
	(setf (window-tick w) (1- (buffer-modified-tick buffer)))
	(let ((first (cdr (window-first-line w))))
	  (unless (or (> (line-number (dis-line-line (car first))) number)
		      (> number
			 (line-number
			  (dis-line-line (car (window-last-line w))))))
	    (do ((dl first (cdr dl)))
		((or (null dl)
		     (eq (dis-line-line (car dl)) line))
		 (when dl
		   (setf (dis-line-old-chars (car dl)) :font-change))))))))))


;;;; Referencing and setting font ids.

(defun window-font (window font)
  "Return a font id for $window and $font."
  (svref (font-family-map (bitmap-hunk-font-family (window-hunk window))) font))

(defun %set-window-font (window font font-object)
  (unless (and (>= font 0) (< font font-map-size))
    (error "Font number ~S out of range." font))
  (setf (bitmap-hunk-trashed (window-hunk window)) :font-change)
  (let ((family (bitmap-hunk-font-family (window-hunk window))))
    (when (eq family *default-font-family*)
      (setq family (copy-font-family family))
      (setf (font-family-map family) (copy-seq (font-family-map family)))
      (setf (bitmap-hunk-font-family (window-hunk window)) family))
    (setf (svref (font-family-map family) font) font-object)))

(defun default-font (font)
  "Return the font id for $font out of the default font family."
  (svref (font-family-map *default-font-family*) font))

(defun %set-default-font (font font-object)
  (or (and (>= font 0) (< font font-map-size))
      (error "Font number ~S out of range." font))
  (dolist (w *window-list*)
    (when (eq (bitmap-hunk-font-family (window-hunk w)) *default-font-family*)
      (setf (bitmap-hunk-trashed (window-hunk w)) :font-change)))
  (setf (svref (font-family-map *default-font-family*) font) font-object))
