;;; -*- Package: Render -*-
;;;
;;; 3D points-based renderer with Xlib interface.

(defpackage "RENDER"
  (:version 0)
  (:use "COMMON-LISP" "XLIB")
  (:documentation "3D points-based renderer with Xlib interface.

Simple example:

    (in-package :render)
    (camera)
    (with-transform (rotate 30 0 1 0) (draw-cubie))"))

(in-package "RENDER")

(use-package "XLIB")


(defvar *window-width* 200)

(defvar *window-height* 200)

(defstruct (point
            (:constructor new-point (x y z &optional (w 1.0)))
            (:constructor))
  (x 0.0)
  (y 0.0)
  (z 0.0)
  (w 1.0))


;;;; Operations on vectors/points.
;;;
;;; Functions ignore w in all arguments.  Vector-valued functions produce
;;; results with w = 1.0

(defun vsub (p1 p2)
  (new-point (- (point-x p1) (point-x p2))
             (- (point-y p1) (point-y p2))
             (- (point-z p1) (point-z p2))))

(defun vadd (p1 p2)
  (new-point (+ (point-x p1) (point-x p2))
             (+ (point-y p1) (point-y p2))
             (+ (point-z p1) (point-z p2))))

(defun vscale (s pt)
  (new-point (* s (point-x pt))
             (* s (point-y pt))
             (* s (point-z pt))))

(defun vlength (pt)
  (let ((x (point-x pt))
        (y (point-y pt))
        (z (point-z pt)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun vnormalize (pt)
  (let ((d (vlength pt))
        (x (point-x pt))
        (y (point-y pt))
        (z (point-z pt)))
    (new-point (/ x d) (/ y d) (/ z d))))

(defun vcross (p1 p2)
  (new-point (- (* (point-y p1) (point-z p2))
                (* (point-z p1) (point-y p2)))
             (- (* (point-z p1) (point-x p2))
                (* (point-x p1) (point-z p2)))
             (- (* (point-x p1) (point-y p2))
                (* (point-y p1) (point-x p2)))))

(defun vdot (p1 p2)
  (+ (* (point-x p1) (point-x p2))
     (* (point-y p1) (point-y p2))
     (* (point-z p1) (point-z p2))))

(defun project-point (pt)
  "Project homogeneous point pt by dividing through by w."
  (let ((x (point-x pt))
        (y (point-y pt))
        (z (point-z pt))
        (w (point-w pt)))
    (new-point (/ x w) (/ y w) (/ z w))))

(defun to-radians (degrees)
  (* (/ pi 180.0) degrees))

(defun to-degrees (radians)
  (* (/ 180.0 pi) radians))

(defvar *transform-stack* (list (make-array '(4 4)
                                            :initial-contents
                                            '((1.0 0.0 0.0 0.0)
                                              (0.0 1.0 0.0 0.0)
                                              (0.0 0.0 1.0 0.0)
                                              (0.0 0.0 0.0 1.0)))))
(defvar *ctm* (car *transform-stack*))

(defun copy-matrix (matrix)
  (let ((copy (make-array '(4 4))))
    (dotimes (i 16)
      (setf (row-major-aref copy i) (row-major-aref matrix i)))
    copy))

(defun identity-matrix ()
  "Return a copy of the identity matrix."
  (make-array '(4 4) :initial-contents '((1.0 0.0 0.0 0.0)
                                         (0.0 1.0 0.0 0.0)
                                         (0.0 0.0 1.0 0.0)
                                         (0.0 0.0 0.0 1.0))))

(defun zero-matrix ()
  (make-array '(4 4) :initial-element 0.0))

(defun transform-begin ()
  "Push a copy of the current transformation onto the transformation
   stack."
  (push (copy-matrix *ctm*) *transform-stack*)
  (setf *ctm* (car *transform-stack*)))

(defun transform-end ()
  (when (null (pop *transform-stack*))
    (error "transform stack underflow"))
  (setf *ctm* (car *transform-stack*)))

(defmacro with-transform (&body body)
  `(progn
     (transform-begin)
     ,@body
     (transform-end)))

(defun reset-transform ()
  "Clear the current transformation to the identity matrix."
  (dotimes (i 16)
    (setf (row-major-aref *ctm* i) 0.0))
  (dotimes (i 4)
    (setf (aref *ctm* i i) 1.0)))

(defun matrix-multiply (m1 m2)
  (let ((m (zero-matrix))
        (tmp 0.0))
    (dotimes (i 4)
      (dotimes (j 4)
        (setf tmp 0.0)
        (dotimes (k 4)
          (incf tmp (* (aref m1 i k) (aref m2 k j))))
        (setf (aref m i j) tmp)))
    m))

(defun transform (matrix)
  "Replace current transformation with matrix."
  (dotimes (i 16)
    (setf (row-major-aref *ctm* i) (row-major-aref matrix i))))

(defun concat-transform (matrix)
  "Concatenate transformation specified by matrix onto the current
   transformation."
  (let ((m (zero-matrix))
        (tmp 0.0))
    (dotimes (i 4)
      (dotimes (j 4)
        (setf tmp 0.0)
        (dotimes (k 4)
          (incf tmp (* (aref matrix i k) (aref *ctm* k j))))
        (setf (aref m i j) tmp)))
    (transform m)))

(defun translate (dx dy dz)
  "Concatenate a translation onto the current transformation."
  (let ((m (identity-matrix)))
    (setf (aref m 3 0) dx
          (aref m 3 1) dy
          (aref m 3 2) dz)
    (concat-transform m)))

(defun scale (sx sy sz)
  "Concatenate a scale transformation onth the current transformation."
  (let ((m (identity-matrix)))
    (setf (aref m 0 0) sx
          (aref m 1 1) sy
          (aref m 2 2) sz)
    (concat-transform m)))

;; FIX is this OK (presumably copied algo from this book)
;;         replace with version from a pd ref?
;;; Graphics Gems I, p. 466
(defun rotate (angle dx dy dz)
  "Concatenate rotation of angle degrees around the axis specified by the
   vector between the origin and the point (dx, dy, dz)."
  (let* ((u (vnormalize (new-point dx dy dz)))
         (dx (point-x u))
         (dy (point-y u))
         (dz (point-z u))
         (m (identity-matrix))
         (theta (to-radians angle))
         (s (sin theta))
         (c (cos theta))
         (b (- 1.0 c)))
    (setf (aref m 0 0) (+ (* b dx dx) c)
          (aref m 0 1) (+ (* b dx dy) (* s dz))
          (aref m 0 2) (- (* b dx dz) (* s dy))
          (aref m 1 0) (- (* b dx dy) (* s dz))
          (aref m 1 1) (+ (* b dy dy) c)
          (aref m 1 2) (+ (* b dy dz) (* s dx))
          (aref m 2 0) (+ (* b dx dz) (* s dy))
          (aref m 2 1) (- (* b dy dz) (* s dx))
          (aref m 2 2) (+ (* b dz dz) c))
    (concat-transform m)))

(defun perspective (fov near far)
  (let ((m (identity-matrix))
        (h (tan (to-radians (/ fov 2.0)))))
    (setf (aref m 2 2) (/ (* far h) (- far near))
          (aref m 2 3) h
          (aref m 3 2) (/ (* near far h) (- near far))
          (aref m 3 3) 0.0)
    (concat-transform m)))

(defun lookat (eye lookat up)
  (let* ((m (identity-matrix))
         (up (vnormalize up))
         (n (vnormalize (vsub lookat eye)))
         (u (vnormalize (vcross up n)))
         (v (vcross n u)))
    (setf (aref m 0 0) (point-x u)
          (aref m 0 1) (point-x v)
          (aref m 0 2) (point-x n)
          (aref m 1 0) (point-y u)
          (aref m 1 1) (point-y v)
          (aref m 1 2) (point-y n)
          (aref m 2 0) (point-z u)
          (aref m 2 1) (point-z v)
          (aref m 2 2) (point-z n))
    (concat-transform m)
    (translate (- (point-x eye)) (- (point-y eye)) (- (point-z eye)))))

(defun transform-point (pt)
  "Transform pt by current transformation."
  (let ((x (point-x pt))
        (y (point-y pt))
        (z (point-z pt))
        (w (point-w pt)))
    (new-point (+ (* x (aref *ctm* 0 0)) (* y (aref *ctm* 1 0))
                  (* z (aref *ctm* 2 0)) (* w (aref *ctm* 3 0)))
               (+ (* x (aref *ctm* 0 1)) (* y (aref *ctm* 1 1))
                  (* z (aref *ctm* 2 1)) (* w (aref *ctm* 3 1)))
               (+ (* x (aref *ctm* 0 2)) (* y (aref *ctm* 1 2))
                  (* z (aref *ctm* 2 2)) (* w (aref *ctm* 3 2)))
               (+ (* x (aref *ctm* 0 3)) (* y (aref *ctm* 1 3))
                  (* z (aref *ctm* 2 3)) (* w (aref *ctm* 3 3))))))

(defun viewport (xres yres)
  (let ((aspect (/ yres xres)))
    (translate (* 0.5 xres) (* 0.5 yres) 0.0)
    (scale (* 0.5 xres) (* 0.5 yres aspect) 1.0)))

(defun viewport-map (pt)
  (new-point (* 0.5 *window-width* (+ (point-x pt) 1.0))
	     (- *window-height*
		(* 0.5 *window-height* (+ (point-y pt) 1.0)))
	     (point-z pt)))


(defun draw-polygon (vertex-list)
  (let ((previous-vertex (car (last vertex-list))))
    (dolist (v vertex-list)
      (let ((s0 (project-point (transform-point previous-vertex)))
            (s1 (project-point (transform-point v))))
        (moveto (viewport-map s0))
        (lineto (viewport-map s1))
        (setf previous-vertex v)))))

(defun draw-axes ()
  (draw-polygon (list (new-point 0 0 0)
		      (new-point 1 0 0)))
  (draw-polygon (list (new-point 0 0 0)
		      (new-point 0 1 0)))
  (draw-polygon (list (new-point 0 0 0)
		      (new-point 0 0 1))))

(defun draw-cubie ()
  "Draw a cube with one corner cut off, like in MECG." ; FIX what's MECG?
  (with-transform
      (translate -0.5 -0.5 -0.5)
    (draw-polygon (list (new-point 0 0 1)
			(new-point 1 0 1)
			(new-point 1 0.5 1)
			(new-point 0.5 1 1)
			(new-point 0 1 1)))
    (draw-polygon (list (new-point 1 0 1)
			(new-point 1 0 0)
			(new-point 1 1 0)
			(new-point 1 1 0.5)
			(new-point 1 0.5 1)))
    (draw-polygon (list (new-point 0 1 1)
			(new-point 0.5 1 1)
			(new-point 1 1 0.5)
			(new-point 1 1 0)
			(new-point 0 1 0)))
    (draw-polygon (list (new-point 0 0 1)
			(new-point 0 1 1)
			(new-point 0 1 0)
			(new-point 0 0 0)))
    (draw-polygon (list (new-point 0 0 0)
			(new-point 0 1 0)
			(new-point 1 1 0)
			(new-point 1 0 0)))
    (draw-polygon (list (new-point 0 0 1)
			(new-point 0 0 0)
			(new-point 1 0 0)
			(new-point 1 0 1)))
    (draw-polygon (list (new-point 1 0.5 1)
			(new-point 1 1 0.5)
			(new-point 0.5 1 1)))))

(defun camera ()
  (reset-transform)
  (perspective 60 0.0001 1000)
  (lookat (new-point 0 0 -3)
	  (new-point 0 0 0)
	  (new-point 0 1 0)))


;;;; Xlib interface.

(defvar *display* (xlib:open-display () :display 0.0))

(defvar *screen* (xlib:display-default-screen *display*))

(defvar *win* (xlib:create-window
	       :parent (xlib:screen-root *screen*)
	       :x 200
	       :y 200
	       :width *window-width*
	       :height *window-height*
	       :background (xlib:screen-white-pixel *screen*)))

(defvar *gcontext* (xlib:create-gcontext
		    :drawable *win*
		    :background (xlib:screen-white-pixel *screen*)
		    :foreground (xlib:screen-black-pixel *screen*)
		    :font (xlib:open-font *display* "fixed")))

(defvar *old-x* 0)

(defvar *old-y* 0)

(defun init-clx ()
  (xlib:map-window *win*)
  (xlib:display-force-output *display*))

(init-clx)

(defun lineto (vertex)
  (xlib:draw-line *win* *gcontext* *old-x* *old-y*
		  (round (point-x vertex)) (round (point-y vertex)))
  (xlib:display-force-output *display*))

(defun moveto (vertex)
  (setf *old-x* (round (point-x vertex))
	*old-y* (round (point-y vertex))))

(defun clear-window ()
  (xlib:clear-area *win* :width *window-width* :height *window-height*)
  (xlib:display-force-output *display*))
