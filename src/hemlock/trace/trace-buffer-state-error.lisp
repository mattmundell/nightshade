;;; -*- Log: hemlock.log; Package: Hemlock-Internals; Mode: Editor -*-

(defun update-all (buffer flag)
  (declare (ignore flag))
  (let ((field (modeline-field :buffer-state)))
;    (message "~A" (buffer-windows buffer))
    (dolist (window (buffer-windows buffer))
      (or (eq window *echo-area-window*)
	  (when (buffer-modeline-field-p buffer field)
	    (update-modeline-field buffer window field))))))

(let ((b (make-buffer "a")))
  (ed::change-to-buffer b)
  (let* ((field (modeline-field :buffer-state))
	 (finfo (internal-buffer-modeline-field-p b field)))
    ;; FIX where are these fields initialized?
    ;(ml-field-info-start finfo) ; nil
    (ml-field-info-end finfo) ; nil
    ;(window-modeline-buffer (car (buffer-windows b)))
    (update-all b nil)
    ))


    (message "finfo ~A" finfo)
    (message "finfo start ~A" (ml-field-info-start finfo))))

  ;(update-modeline-field b (car (buffer-windows b)) (modeline-field :buffer-state))
  ;(update-all b nil)
  ;(invoke-hook ed::buffer-modified-hook b nil)

  (message "~A" b)
)
