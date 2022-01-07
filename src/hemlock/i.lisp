;; (in-package "HI")

;; (%init-screen-manager dsp)
;; ;; (let ((device (device-hunk-device (window-hunk (current-window)))))
;; ;;   (funcall (device-init device) device))
;; (device-write-string (tty-device-init-string dsp))
;; (redisplay-all)

(in-package "HI")

(let ((device (device-hunk-device (window-hunk (current-window)))))
  (setf (device-random-typeout-full-more device) #'do-tty-full-more))


;;(sb-unix::unix-write 1 (termcap :clear-to-eol (get-termcap "rxvt")) 0 1)
;;(sb-unix::unix-write 1 (termcap :clear-to-eol (get-termcap "rxvt")) 0 2)

;;;(wc :clear-to-eol)

#|
(defvar buf (make-string 256))
(declaim (simple-string buf))

(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 0 buf 0 1)
(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 4 buf 1 2)
(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 8 buf 2 3)
(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 12 buf 3 4)
(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 16 buf 4 5)
(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 16 buf 5 6)
(sb-c::%byte-blt (format nil "~A[H~A[2J" #\Esc #\Esc) 16 buf 6 7)

;;(sb-unix::unix-write 1 buf 0 (* 4 7))  ;; the rest is padding

|#

(defun cursor-motion2 (device x y)
  (let ((x-add-char (tty-device-cm-x-add-char device))
	(y-add-char (tty-device-cm-y-add-char device))
	(x-condx-add (tty-device-cm-x-condx-char device))
	(y-condx-add (tty-device-cm-y-condx-char device))
	(one-origin (tty-device-cm-one-origin device)))
    (when x-add-char (incf x x-add-char))
    (when (and x-condx-add (> x x-condx-add))
      (incf x (tty-device-cm-x-condx-add-char device)))
    (when y-add-char (incf y y-add-char))
    (when (and y-condx-add (> y y-condx-add))
      (incf y (tty-device-cm-y-condx-add-char device)))
    (when one-origin (incf x) (incf y)))
  ;;(device-write-string (tty-device-cm-string1 device))
  (let ((reversep (tty-device-cm-reversep device))
	(x-pad (tty-device-cm-x-pad device))
	(y-pad (tty-device-cm-y-pad device)))
    (if reversep
	(cm-output-coordinate2 x x-pad)
	(cm-output-coordinate2 y y-pad))
    ;;(device-write-string (tty-device-cm-string2 device))
    (if reversep
	(cm-output-coordinate2 y y-pad)
	(cm-output-coordinate2 x x-pad))
    ;;(device-write-string (tty-device-cm-string3 device))
    ))

(defun cm-output-coordinate2 (coordinate pad)
  (cond (pad
	 (let ((i (1- cm-coordinate-buffer-len)))
	   (loop
	     (when (= i -1) (error "Terminal has too many lines!"))
	     (multiple-value-bind (tens ones)
				  (truncate coordinate 10)
	       (setf (schar *cm-coordinate-buffer* i) (digit-char ones))
	       (when (zerop tens)
		 (dotimes (n (- pad (- cm-coordinate-buffer-len i)))
		   (decf i)
		   (setf (schar *cm-coordinate-buffer* i) #\0))
		   (print "*c-c-b*:")
		   (print (char-code (aref *cm-coordinate-buffer* 0)))
		   (print (char-code (aref *cm-coordinate-buffer* 1)))
		   (print (char-code (aref *cm-coordinate-buffer* 2)))
		   (print (char-code (aref *cm-coordinate-buffer* 3)))
		   (print (char-code (aref *cm-coordinate-buffer* 4)))
;; 		 (device-write-string *cm-coordinate-buffer* i
;; 				      cm-coordinate-buffer-len)
		 (return))
	       (decf i)
	       (setf coordinate tens)))))
	(t (tty-write-char (code-char coordinate)))))

(defun cursor-motion2xx (device x y)
  (let ((x-add-char (tty-device-cm-x-add-char device))
	(y-add-char (tty-device-cm-y-add-char device))
	(x-condx-add (tty-device-cm-x-condx-char device))
	(y-condx-add (tty-device-cm-y-condx-char device))
	(one-origin (tty-device-cm-one-origin device)))
    (when x-add-char (incf x x-add-char))
    (when (and x-condx-add (> x x-condx-add))
      (incf x (tty-device-cm-x-condx-add-char device)))
    (when y-add-char (incf y y-add-char))
    (when (and y-condx-add (> y y-condx-add))
      (incf y (tty-device-cm-y-condx-add-char device)))
    (when one-origin (incf x) (incf y)))
  ;;(device-write-string (tty-device-cm-string1 device))
  (sb-c::%byte-blt (tty-device-cm-string1 device)
		   0
		   *cursor-motion-buffer*
		   0 (setq *cursor-motion-buffer-index*
			   (tty-device-cm-string1-len device)))
  (print "*c-m-b*:")
  (print (char-code (aref *cursor-motion-buffer* 0)))
  (print (char-code (aref *cursor-motion-buffer* 1)))
  (print (char-code (aref *cursor-motion-buffer* 2)))
  (let ((reversep (tty-device-cm-reversep device))
	(x-pad (tty-device-cm-x-pad device))
	(y-pad (tty-device-cm-y-pad device)))
    (if reversep
	(cm-output-coordinate2 x x-pad)
	(cm-output-coordinate2 y y-pad))
    (print "*c-m-b*:")
    (print (char-code (aref *cursor-motion-buffer* 0)))
    (print (char-code (aref *cursor-motion-buffer* 1)))
    (print (char-code (aref *cursor-motion-buffer* 2)))
    ;;(device-write-string (tty-device-cm-string2 device))
    (sb-c::%byte-blt (tty-device-cm-string2 device)
		     0
		     *cursor-motion-buffer*
		     *cursor-motion-buffer-index*
		     (incf *cursor-motion-buffer-index*
			   (tty-device-cm-string2-len device)))
    (print "*c-m-b*:")
    (print (char-code (aref *cursor-motion-buffer* 0)))
    (print (char-code (aref *cursor-motion-buffer* 1)))
    (print (char-code (aref *cursor-motion-buffer* 2)))
    (if reversep
	(cm-output-coordinate2 y y-pad)
	(cm-output-coordinate2 x x-pad))
    (print "*c-m-b*:")
    (print (char-code (aref *cursor-motion-buffer* 0)))
    (print (char-code (aref *cursor-motion-buffer* 1)))
    (print (char-code (aref *cursor-motion-buffer* 2)))
    ;;(device-write-string (tty-device-cm-string3 device))
    (sb-c::%byte-blt (tty-device-cm-string3 device)
		     0
		     *cursor-motion-buffer*
		     *cursor-motion-buffer-index*
		     (incf *cursor-motion-buffer-index*
			   (tty-device-cm-string3-len device))))
  (print "*c-m-b*:")
  (print (char-code (aref *cursor-motion-buffer* 0)))
  (print (char-code (aref *cursor-motion-buffer* 1)))
  (print (char-code (aref *cursor-motion-buffer* 2)))
  (multiple-value-bind (len rem)
      (truncate (/ *cursor-motion-buffer-index* 4))
    (when (> rem 0)
      ;; Ensure rest of last char in *cursor-motion-buffer* is clear.
      (sb-c::%byte-blt (make-string 3 :initial-element #\nul)
		       0
		       *cursor-motion-buffer*
		       *cursor-motion-buffer-index*
		       (+ *cursor-motion-buffer-index* 3))
      (incf len))
    ;;(device-write-string (subseq *cursor-motion-buffer* 0 len))
    )
  (print "*c-m-b*:")
  (print (char-code (aref *cursor-motion-buffer* 0)))
  (print (char-code (aref *cursor-motion-buffer* 1)))
  (print (char-code (aref *cursor-motion-buffer* 2)))
  )

(defun cursor-motion2x (device x y)
  ;; clear buffer
  (setq *cursor-motion-buffer* (make-string 300))
  (setq *cursor-motion-buffer-index* 0)
  (let ((x-add-char (tty-device-cm-x-add-char device))
	(y-add-char (tty-device-cm-y-add-char device))
	(x-condx-add (tty-device-cm-x-condx-char device))
	(y-condx-add (tty-device-cm-y-condx-char device))
	(one-origin (tty-device-cm-one-origin device)))
    (when x-add-char (incf x x-add-char))
    (when (and x-condx-add (> x x-condx-add))
      (incf x (tty-device-cm-x-condx-add-char device)))
    (when y-add-char (incf y y-add-char))
    (when (and y-condx-add (> y y-condx-add))
      (incf y (tty-device-cm-y-condx-add-char device)))
    (when one-origin (incf x) (incf y)))
  ;;(device-write-string (tty-device-cm-string1 device))
  (print (format nil "string1-len: ~A" (tty-device-cm-string1-len device)))
  (sb-c::%byte-blt (tty-device-cm-string1 device)
		   0
		   *cursor-motion-buffer*
		   0 (setq *cursor-motion-buffer-index*
			   (tty-device-cm-string1-len device)))
  (let ((reversep (tty-device-cm-reversep device))
	(x-pad (tty-device-cm-x-pad device))
	(y-pad (tty-device-cm-y-pad device)))
    (print (format nil "x: ~A" x))
    (print (format nil "y: ~A" y))
    (print (format nil "x-pad: ~A" x-pad))
    (print (format nil "y-pad: ~A" y-pad))
    (print (format nil "reversep: ~A" reversep))
    (if reversep
	(cm-output-coordinate2 x x-pad)
	(cm-output-coordinate2 y y-pad))
    (print "*c-m-b*:")
    (print (char-code (aref *cursor-motion-buffer* 0)))
    (print (char-code (aref *cursor-motion-buffer* 1)))
    (print (char-code (aref *cursor-motion-buffer* 2)))
    ;;(device-write-string (tty-device-cm-string2 device))
    (print (format nil "string2-len: ~A" (tty-device-cm-string2-len device)))
    (sb-c::%byte-blt (tty-device-cm-string2 device)
 		     0
 		     *cursor-motion-buffer*
 		     *cursor-motion-buffer-index*
 		     (incf *cursor-motion-buffer-index*
 			   (tty-device-cm-string2-len device)))
    (print (format nil "*cursor-motion-buffer-index*: ~A" *cursor-motion-buffer-index*))
    (if reversep
 	(cm-output-coordinate2 y y-pad)
      (cm-output-coordinate2 x x-pad))
    ;;(device-write-string (tty-device-cm-string3 device))
    (print (format nil "string3-len: ~A" (tty-device-cm-string3-len device)))
    (sb-c::%byte-blt (tty-device-cm-string3 device)
 		     0
 		     *cursor-motion-buffer*
 		     *cursor-motion-buffer-index*
 		     (incf *cursor-motion-buffer-index*
 			   (tty-device-cm-string3-len device)))
    (print (format nil "*cursor-motion-buffer-index*: ~A" *cursor-motion-buffer-index*))
    )
  (multiple-value-bind (len rem)
      (truncate (/ *cursor-motion-buffer-index* 4))
    (when (> rem 0)
      (print (format nil "*cursor-motion-buffer-index*: ~A" *cursor-motion-buffer-index*))
      ;; Ensure rest of last char in *cursor-motion-buffer* is clear.
      (sb-c::%byte-blt (make-string 3 :initial-element #\nul)
		       0
		       *cursor-motion-buffer*
		       *cursor-motion-buffer-index*
		       (+ *cursor-motion-buffer-index* 3))
      (incf len))
    ;;(device-write-string (subseq *cursor-motion-buffer* 0 len))
    )
  (print "*c-m-b*:")
  (print (char-code (aref *cursor-motion-buffer* 0)))
  (print (char-code (aref *cursor-motion-buffer* 1)))
  (print (char-code (aref *cursor-motion-buffer* 2)))
  ;;(print (string-to-octets *cursor-motion-buffer*))
  )

(defun cm-output-coordinate2xxx (coordinate pad)
  (cond
   (pad
    ;; FIX byte-blt directly into *cursor-motion-buffer*?
    (let ((i (1- cm-coordinate-buffer-len)))
      (loop
       (when (= i -1) (error "Terminal has too many lines!"))
       (multiple-value-bind (tens ones)
	   (truncate coordinate 10)
	 (setf (schar *cm-coordinate-buffer* i) (digit-char ones))
	 (when (zerop tens)
	   (dotimes (n (- pad (- cm-coordinate-buffer-len i)))
	     (decf i)
	     (setf (schar *cm-coordinate-buffer* i) #\0))
	   ;;(device-write-string *cm-coordinate-buffer* i
	   ;;		       cm-coordinate-buffer-len)
	   (print (format nil "*cursor-motion-buffer-index*: ~A" *cursor-motion-buffer-index*))
	   (single-byte-char-string (subseq *cm-coordinate-buffer*
					    i cm-coordinate-buffer-len)
				    *cursor-motion-buffer*
				    (- cm-coordinate-buffer-len i)
				    *cursor-motion-buffer-index*)
	   (incf *cursor-motion-buffer-index*
		 (- cm-coordinate-buffer-len i))
	   (return))
	 (decf i)
	 (setf coordinate tens)))))
   ;;(t (tty-write-char (code-char coordinate)))
   (t (progn
	;; FIX byte-blt directly into *cursor-motion-buffer*?
	(setf (schar *cm-coordinate-buffer* 0) (code-char coordinate))
	(sb-c::%byte-blt *cm-coordinate-buffer* 0
			 *cursor-motion-buffer*
			 *cursor-motion-buffer-index*
			 (incf *cursor-motion-buffer-index* 1))))))


(defun cm-output-coordinate2x0 (coordinate pad)
  (cond
   (pad
    ;; FIX byte-blt directly into *cursor-motion-buffer*?
    (let ((i (1- cm-coordinate-buffer-len)))
      (loop
       (when (= i -1) (error "Terminal has too many lines!"))
       (multiple-value-bind (tens ones)
	   (truncate coordinate 10)
	 (setf (schar *cm-coordinate-buffer* i) (digit-char ones))
	 (when (zerop tens)
	   (dotimes (n (- pad (- cm-coordinate-buffer-len i)))
	     (decf i)
	     (setf (schar *cm-coordinate-buffer* i) #\0))
	   ;;(device-write-string *cm-coordinate-buffer* i
	   ;;		       cm-coordinate-buffer-len)
	   (single-byte-char-string (subseq *cm-coordinate-buffer*
					    i cm-coordinate-buffer-len)
				    *cursor-motion-buffer*
				    (- cm-coordinate-buffer-len i)
				    *cursor-motion-buffer-index*)
	   (incf *cursor-motion-buffer-index*
		 (- cm-coordinate-buffer-len i))
	   (return))
	 (decf i)
	 (setf coordinate tens)))))
   ;;(t (tty-write-char (code-char coordinate)))
   (t (progn
	;; FIX byte-blt directly into *cursor-motion-buffer*?
	(setf (schar *cm-coordinate-buffer* 0) (code-char coordinate))
	(sb-c::%byte-blt *cm-coordinate-buffer* 0
			 *cursor-motion-buffer*
			 *cursor-motion-buffer-index*
			 (incf *cursor-motion-buffer-index* 1))))))

(defun cm-output-coordinate2x (coordinate pad)
  (cond
   (pad
    (let ((i (1- cm-coordinate-buffer-len)))
      (loop
       (when (= i -1) (error "Terminal has too many lines!"))
       (multiple-value-bind (tens ones)
	   (truncate coordinate 10)
	 (setf (schar *cm-coordinate-buffer* i) (digit-char ones))
	 (when (zerop tens)
	   (dotimes (n (- pad (- cm-coordinate-buffer-len i)))
	     (decf i)
	     (setf (schar *cm-coordinate-buffer* i) #\0))
	   ;;(device-write-string *cm-coordinate-buffer* i
	   ;;		       cm-coordinate-buffer-len)
	   (print "*cm-coordinate-buffer*:")
	   (print (char-code (aref *cm-coordinate-buffer* 0)))
	   (print (char-code (aref *cm-coordinate-buffer* 1)))
	   (print (char-code (aref *cm-coordinate-buffer* 2)))
	   (print (char-code (aref *cm-coordinate-buffer* 3)))
	   (print (char-code (aref *cm-coordinate-buffer* 4)))
	   (print (format nil "i: ~A" i))
	   (print (format nil "*cursor-motion-buffer-index*: ~A" *cursor-motion-buffer-index*))
	   (print "subseq:")
	   (print (char-code (aref (subseq *cm-coordinate-buffer*
					   i cm-coordinate-buffer-len)
				   0)))
;; 	   (sb-c::%byte-blt (subseq *cm-coordinate-buffer*
;; 				    i cm-coordinate-buffer-len)
;; 			    0
;; 			    *cursor-motion-buffer*
;; 			    *cursor-motion-buffer-index*
;; 			    (incf *cursor-motion-buffer-index* 1))
 	   (single-byte-char-string (subseq *cm-coordinate-buffer*
 					    i cm-coordinate-buffer-len)
 				    *cursor-motion-buffer*
 				    (- cm-coordinate-buffer-len i)
 				    *cursor-motion-buffer-index*)
	   (print "*c-m-b*:")
	   (print (char-code (aref *cursor-motion-buffer* 0)))
	   (print (char-code (aref *cursor-motion-buffer* 1)))
	   (print (char-code (aref *cursor-motion-buffer* 2)))
	   (incf *cursor-motion-buffer-index*
		 (- cm-coordinate-buffer-len i))
	   (print (format nil "*cursor-motion-buffer-index*: ~A" *cursor-motion-buffer-index*))
	   (return))
	 (decf i)
	 (setf coordinate tens)))))
   ;;(t (tty-write-char (code-char coordinate)))
   (t (progn
	(setf (schar *cm-coordinate-buffer* 0) (code-char coordinate))
	(sb-c::%byte-blt *cm-coordinate-buffer* 0
			 *cursor-motion-buffer*
			 *cursor-motion-buffer-index*
			 (incf *cursor-motion-buffer-index* 1))))))

;;;(cursor-motion2 (device-hunk-device (window-hunk (current-window))) 0 0)
(defun tst (x y)
  (cursor-motion2 (device-hunk-device (window-hunk (current-window))) x y))

;;(tst 0 0) x1 y1: (mapcar 'code-char '(27 91 49 59  49 72))  ; 3B31'5B1B 4831 (993090331 18481)
;;(tst 2 1) x3 y2: (mapcar 'code-char '(27 91 50 59  51 72))  ; 3B32'5B1B 4833 (993155867 18483)

;(cursor-motion (device-hunk-device (window-hunk (current-window))) 1 1)
;(clear-device (device-hunk-device (window-hunk (current-window))))
