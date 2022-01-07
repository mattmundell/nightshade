;;; -*- Package: XEvents -*-
;;;
;;; Utilities for getting information from X.

(defpackage "XINFO"
  (:version 1)
  (:use "COMMON-LISP")
  (:export "MONITOR")
  (:documentation "X events monitor."))

(in-package "XINFO")

(export '(window))

(defun handle-event (display)
  (format t "display       ~A~%" display)
  (or (xlib:process-event
       display
       :timeout 0
       :handler (lambda (&key event-key event-window &allow-other-keys)
		  (format t "key           ~A~%" event-key)
		  (format t "window        ~A~%~%" event-window)
		  t))
      (xlib:event-case (display :timeout 0)
	(t (event-key event-window)
	   (format t "key           ~A~%" event-key)
	   (format t "window        ~A~%~%" event-window)
	   t))))

(defvar *monitor-events-mask*
  ;(make-event-mask :key-press :button-press))
  ; Seems like anything higher than this turns off events.
  ;#b1111111111111111111111111)
  #b111111111111111111111)

(defun monitor (&optional id)
  "Create a window, serve events, list any events served.

   If $id is true, monitor window $id instead of creating a new window."
  ;; FIX make display match window?
  (let* ((display (xlib:open-display () :display 0.0))
	 (window (if id
		     (xlib:window-from-id display id)
		     (xlib:create-window
		      :parent (xlib:screen-root
			       (car (xlib:display-roots display)))
		      :width 150 :height 150
		      :background
		      (xlib:screen-white-pixel
		       (xlib:display-default-screen display))
		      :input ()
		      :border-width 2
		      ;:class :input-output
		      :override-redirect t))))
    (or id
	(xlib:set-wm-properties
	 window
	 :name "Event Monitor Window" ; :icon-name icon-name
	 :resource-name "Event Monitor Window"
	 :x 0 :y 0 :width 150 :height 150
	 :user-specified-position-p t :user-specified-size-p t
	 :width-inc 10 :height-inc 20
	 :min-width 100 :min-height 100
	 ;; Tell OpenLook pseudo-X11 server we want input.
	 :input :on))
    (setf (xlib:window-event-mask window) *monitor-events-mask*)
    (or id (xlib:map-window window))
    (unwind-protect
	(ext:with-clx-event-handling (display #'handle-event)
	   (xlib:display-force-output display)
	   (format t "Monitoring events on window ~A...~%~%" window)
	   (dotimes (i 100000) (system:serve-event)))
      (or id (xlib:unmap-window window))
      (xlib:close-display display))))
