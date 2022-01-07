;;; Commands and support specifically for X related features.

(in-package "ED")

(defcommand "Region to Cut Buffer" (p)
  "Place the current region into the X cut buffer."
  "Place the current region into the X cut buffer."
  (declare (ignore p))
  (store-cut-string (edi::bitmap-device-display
		     (edi::device-hunk-device (edi::window-hunk (current-window))))
		    (region-to-string (current-region))))

(defcommand "Insert Cut Buffer" (p)
  "Insert the X cut buffer at current point."
  "Insert the X cut buffer at current point.  Returns nil when it is empty."
  (declare (ignore p))
  (let ((str (fetch-cut-string (edi::bitmap-device-display
				(edi::device-hunk-device
				 (edi::window-hunk (current-window)))))))
    (if str
	(let ((point (current-point)))
	  (push-buffer-mark (copy-mark point))
	  (insert-string (current-point) str))
	(editor-error "X cut buffer empty.")))
  (setf (last-command-type) :ephemerally-active))
