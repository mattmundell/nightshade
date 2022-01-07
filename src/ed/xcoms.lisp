;;; Commands and support specifically for X related features.

(in-package "HEMLOCK")


(defcommand "Region to Cut Buffer" (p)
  "Place the current region into the X cut buffer."
  "Place the current region into the X cut buffer."
  (declare (ignore p))
  (store-cut-string (hi::bitmap-device-display
		     (hi::device-hunk-device (hi::window-hunk (current-window))))
		    (region-to-string (current-region))))

(defcommand "Insert Cut Buffer" (p)
  "Insert the X cut buffer at current point."
  "Insert the X cut buffer at current point.  Returns nil when it is empty."
  (declare (ignore p))
  (let ((str (fetch-cut-string (hi::bitmap-device-display
				(hi::device-hunk-device
				 (hi::window-hunk (current-window)))))))
    (if str
	(let ((point (current-point)))
	  (push-buffer-mark (copy-mark point))
	  (insert-string (current-point) str))
	(editor-error "X cut buffer empty.")))
  (setf (last-command-type) :ephemerally-active))
