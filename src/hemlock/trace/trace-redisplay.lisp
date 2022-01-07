;;; -*- Package: HI; Mode: Lisp, Editor -*-

(redisplay-window-all (current-window))
(redisplay-window (current-window))

(funcall (device-smart-redisplay (device-hunk-device (window-hunk (current-window))))
	 (current-window)))
(funcall (device-smart-redisplay (device-hunk-device (window-hunk (current-window))))
	 (current-window)))
(funcall (device-dumb-redisplay (device-hunk-device (window-hunk (current-window))))
	 (current-window)))

(device-smart-redisplay (device-hunk-device (window-hunk (current-window))))

(tty-smart-window-redisplay (current-window))
;;(tty-semi-dumb-window-redisplay (current-window))
(tty-dumb-window-redisplay (current-window))

(eq (window-first-changed (current-window)) the-sentinel)

(let ((hunk (window-hunk (current-window))))
  (message "~A" (tty-device-screen-image (device-hunk-device hunk))))

  (hi::print-lines (device-hunk-device hunk) hunk (car (window-last-line (current-window)))))


(let* ((window (current-window))
       (hunk (window-hunk window))
       (device (device-hunk-device hunk)))
  (tty-device-insert-char-init-string device))

im  insert init ^[[4h   should be ^[[?4h
ei  insert end  ^[[4l   should be ^[[?4l
ic  insert ch init ^[[@
ip  insert ch end  nil

  hunk
  (clear-echo-area)
  (tty-smart-line-redisplay2 device hunk
			     (car (window-last-line window))))

abcdefghijk
abcdefghij
