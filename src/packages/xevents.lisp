;;; -*- Package: XEvents -*-
;;;
;;; X events monitor.

(defpackage "XEVENTS"
  (:version 0)
  (:use "COMMON-LISP")
  (:export "monitor")
  (:documentation "X events monitor."))

(in-package "XEVENTS")

(defun handle-monitor-event (display)
  (format t "handle-monitor-event~%")
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

(defvar *monitor-events-mask*
  ;(make-event-mask :key-press :button-press))
  ; Seems like anything higher than this turns off events.
  #b1111111111111111111111111)

(defun monitor-events ()
  "Open a window, serve events, list any events served."
  (let* ((display (open-display () :display 0.0))
	 (window (create-window :parent (screen-root
					 (car (display-roots d)))
				:width 150 :height 150
				:background (screen-white-pixel
					     (display-default-screen d))
				:input ()
				:override-redirect t)))
    (xlib:set-wm-properties
     window :name "test window" ; :icon-name icon-name
     :resource-name "Test Window"
     :x 0 :y 0 :width 150 :height 150
     :user-specified-position-p t :user-specified-size-p t
     :width-inc 10 :height-inc 20
     :min-width 100 :min-height 100
     ;; Tell OpenLook pseudo-X11 server we want input.
     :input :on)
    (setf (window-event-mask window) *monitor-events-mask*)
    (map-window window)
    (unwind-protect
	(with-clx-event-handling (display #'handle-monitor-event)
	   (display-force-output display)
	   (format t "serving~%")
	   (dotimes (i 100000) (system:serve-event)))
      (unmap-window window)
      (close-display display))))
