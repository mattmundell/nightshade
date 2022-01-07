(in-package "HEMLOCK-INTERNALS")

(defun in () (load "in"))

(defun wc (str)
  (sb-unix::unix-write 1 str 0 (length str))
  t)

(defun ed1 (&optional x
	   &key (init t)
	        (display #-sbcl (cdr (assoc :display ext:*environment-list*))
			 #+sbcl (sb-posix:getenv "DISPLAY")))
  "Invokes the editor, Hemlock.  If X is supplied and is a symbol, the
   definition of X is put into a buffer, and that buffer is selected.  If X is
   a pathname, the file specified by X is visited in a new buffer.  If X is not
   supplied or Nil, the editor is entered in the same state as when last
   exited.  When :init is supplied as t (the default), the file
   \"hemlock-init.lisp\", or \".hemlock-init.lisp\" is loaded from the home
   directory, but the Lisp command line switch -hinit can be used to specify a
   different name.  Any compiled version of the source is preferred when
   choosing the file to load.  If the argument is non-nil and not t, then it
   should be a pathname that will be merged with the home directory."
  (when *in-the-editor* (error "The editor is already running."))
  (let ((*in-the-editor* t)
	(display (unless *editor-has-been-entered*
		   (maybe-load-hemlock-init init)
		   ;; Device dependent initialization.
		   (init-raw-io display))))
    (catch 'editor-top-level-catcher
      (site-wrapper-macro
       (unless *editor-has-been-entered*
	 ;; Make an initial window, and set up redisplay's internal
	 ;; data structures.
	 (%init-redisplay display)
	 (setq *editor-has-been-entered* t)
	 ;; Pick up user initializations to be done after initialization.
	 (invoke-hook (reverse *after-editor-initializations-funs*)))
       (catch 'hemlock-exit
	 (catch 'editor-top-level-catcher
	   (cond ((and x (symbolp x))
		  (let* ((name (nstring-capitalize
				(concatenate 'simple-string "Edit " (string x))))
			 (buffer (or (getstring name *buffer-names*)
				     (make-buffer name)))
			 (*print-case* :downcase))
		    (delete-region (buffer-region buffer))
		    (with-output-to-mark
			(*standard-output* (buffer-point buffer))
		      (eval `(pprint (function-lambda-expression #',x)))
		      (terpri)
		      (ed::change-to-buffer buffer)
		      (buffer-start (buffer-point buffer)))))
		 ((or (stringp x) (pathnamep x))
		  (ed::find-file-command () x))
		 (x
		  (error
		   "~S is not a symbol or pathname.  I can't edit it!" x))))

	 (invoke-hook ed::entry-hook)
	 (unwind-protect
;; 	   (loop
;; 	    (catch 'editor-top-level-catcher
;; 	      (handler-bind ((error #'(lambda (condition)
;; 					(lisp-error-error-handler condition
;; 								  :internal))))
;; 		(invoke-hook ed::abort-hook)
;; 		(%command-loop))))
	   (invoke-hook ed::exit-hook)))))))

(defun ed (&optional x
	   &key (init t)
	        (display #-sbcl (cdr (assoc :display ext:*environment-list*))
			 #+sbcl (sb-posix:getenv "DISPLAY")))
  "Invokes the editor, Hemlock.  If X is supplied and is a symbol, the
   definition of X is put into a buffer, and that buffer is selected.  If X is
   a pathname, the file specified by X is visited in a new buffer.  If X is not
   supplied or Nil, the editor is entered in the same state as when last
   exited.  When :init is supplied as t (the default), the file
   \"hemlock-init.lisp\", or \".hemlock-init.lisp\" is loaded from the home
   directory, but the Lisp command line switch -hinit can be used to specify a
   different name.  Any compiled version of the source is preferred when
   choosing the file to load.  If the argument is non-nil and not t, then it
   should be a pathname that will be merged with the home directory."
  (when *in-the-editor* (error "You are already in the editor, you bogon!"))
  (let ((*in-the-editor* t)
	(display (unless *editor-has-been-entered*
		   (maybe-load-hemlock-init init)
		   ;; Device dependent initialization.
		   (init-raw-io display))))
    (catch 'editor-top-level-catcher

;;;      (site-wrapper-macro

      (unwind-protect
	  (progn
	    (when *editor-has-been-entered*
	      (let ((device (device-hunk-device (window-hunk (current-window)))))

;;;		(funcall (device-init device) device)

		(setf *hemlock-input-handler*
		      (system:add-fd-handler 0 :input #'get-editor-tty-input))
		(standard-device-init)
		(device-write-string (tty-device-init-string device))
		(with-open-file (f "out3" :direction :output :if-exists :append)
		   (format f "out3")
		   (format f "\"~A\"" (tty-device-init-string device)))
		(redisplay-all)

;;; FIX
;;; where is the image written to the tty?
;;; why does it look like newlines should be added?

		(clear-device device)
;;;		(device-write-string "x~%yxxyzxyzabcdefghijk")
		(device-write-string "x")
		(redisplay-all)

		(device-write-string (tty-device-open-line-string device))
		(device-write-string (format nil "~A[H~A[2J" #\esc #\esc))
		(cursor-motion device 0 0)
		(device-write-string "abc")
		(tty-force-output)
;;;		(redisplay-all)

;;;		(funcall (device-put-cursor device) (window-hunk (current-window)) 0 0)
;;;		(device-write-string "zZz\n")
;;;		(redisplay-all)
;;;		(device-write-string (tty-device-clear-string device))
;;;		(device-write-string "zZz\n")
		))

;;; [...]

       (let ((device (device-hunk-device (window-hunk (current-window)))))
	 (funcall (device-exit device) device))

;;;) ;; site-wrapper-macro

       ))  ;; unwind-protect; progn

       )))

#|

(defvar dsp nil)

(defun ed (&optional x)
  (let ((display (init-raw-io nil)))
    (setq dsp display)
;;;    (site-wrapper-macro
     ;; Make an initial window, and set up redisplay's internal
     ;; data structures.
;;;     (%init-redisplay display)

     (%init-screen-manager display)
     (device-write-string (tty-device-init-string display))
     ;;(redisplay-all)

     ;;    (load "i"))
;;;     )
    ))

(defun get-terminal-attributes (&optional (fd 1))
  (alien:with-alien ((winsize (alien:struct unix:winsize))
                     #-(or glibc2 bsd)
		     (sgtty (alien:struct unix:sgttyb))
                     #+bsd ; termios
		     (tios (alien:struct unix:termios)))
    (let ((size-win (unix:unix-ioctl fd unix:TIOCGWINSZ
				     (alien:alien-sap winsize)))
          #-(or glibc2 bsd)
	  (speed-win (unix:unix-ioctl fd unix:TIOCGETP
				      (alien:alien-sap sgtty)))
	  #+bsd
	  (speed-win (unix:unix-tcgetattr fd (alien:alien-sap tios))))
      (flet ((frob (val)
	       (if (and size-win (not (zerop val)))
		   val
		   nil)))
	(values
	 (frob (alien:slot winsize 'unix:ws-row))
	 (frob (alien:slot winsize 'unix:ws-col))
         #-(or glibc2 bsd)
	 (and speed-win
	      (setq *terminal-baud-rate*
		    (svref unix:terminal-speeds
			   (alien:slot sgtty 'unix:sg-ospeed))))
	 #+bsd
	 (and speed-win
	      (setq *terminal-baud-rate* (unix:unix-cfgetospeed tios)))
         #+glibc2
         4800)))))

(defun tst (&optional (fd 0))
  (alien:with-alien ((winsize (alien:struct unix:winsize))
                     #-(or glibc2 bsd)
		     (sgtty (alien:struct unix:sgttyb))
                     #+bsd ; termios
		     (tios (alien:struct unix:termios)))
    (let ((size-win (unix:unix-ioctl fd unix:TIOCGWINSZ
				     (alien:alien-sap winsize)))
          #-(or glibc2 bsd)
	  (speed-win (unix:unix-ioctl fd unix:TIOCGETP
				      (alien:alien-sap sgtty)))
	  #+bsd
	  (speed-win (unix:unix-tcgetattr fd (alien:alien-sap tios))))
      (flet ((frob (val)
	       (if (and size-win (not (zerop val)))
		   val
		   nil)))
	(values
	 (frob (alien:slot winsize 'unix:ws-row))
	 (frob (alien:slot winsize 'unix:ws-col))
         #-(or glibc2 bsd)
	 (and speed-win
	      (setq *terminal-baud-rate*
		    (svref unix:terminal-speeds
			   (alien:slot sgtty 'unix:sg-ospeed))))
	 #+bsd
	 (and speed-win
	      (setq *terminal-baud-rate* (unix:unix-cfgetospeed tios)))
         #+glibc2
         4800)))))

(defun tstx ()
(alien:with-alien ((winsize (alien:struct unix:winsize)))
    (let ((size-win (unix:unix-ioctl 0 unix:TIOCGWINSZ
				     (alien:alien-sap winsize))))

      (flet ((frob (val)
	       (if (and size-win (not (zerop val)))
		   val
		   nil)))
	(values
	 (frob (alien:slot winsize 'unix:ws-row))
	 (frob (alien:slot winsize 'unix:ws-col))
	 4800)))))

(multiple-value-bind
    (rows cols)
    (tst)
  (print rows)
  (print cols))

(alien:with-alien ((winsize (alien:struct unix:winsize)))
   (unix:unix-ioctl 0 unix:TIOCGWINSZ
	  	      (alien:alien-sap winsize))
   (print (alien:slot winsize 'unix:ws-row))
   (print (alien:slot winsize 'unix:ws-col)))


  (let ((termcap (get-termcap "linux"))
	(device (%make-tty-device :name "linux")))
    (when (termcap :overstrikes termcap)
      (error "Terminal sufficiently irritating -- not currently supported."))
    ;;
    ;; Similar device slots.
    (setf (device-init device) #'init-tty-device)
    (setf (device-exit device) #'exit-tty-device)
    (setf (device-smart-redisplay device)
	  (if (and (termcap :open-line termcap) (termcap :delete-line termcap))
	      #'tty-smart-window-redisplay
	      #'tty-semi-dumb-window-redisplay))
    (setf (device-dumb-redisplay device) #'tty-dumb-window-redisplay)
    (setf (device-clear device) #'clear-device)
    (setf (device-put-cursor device) #'tty-put-cursor)
    (setf (device-show-mark device) #'tty-show-mark)
    (setf (device-next-window device) #'tty-next-window)
    (setf (device-previous-window device) #'tty-previous-window)
    (setf (device-make-window device) #'tty-make-window)
    (setf (device-delete-window device) #'tty-delete-window)
    (setf (device-random-typeout-setup device) #'tty-random-typeout-setup)
    (setf (device-random-typeout-cleanup device) #'tty-random-typeout-cleanup)
    (setf (device-random-typeout-full-more device) #'do-tty-full-more)
    (setf (device-random-typeout-line-more device)
	  #'update-tty-line-buffered-stream)
    (setf (device-force-output device) #'tty-force-output)
    (setf (device-finish-output device) #'tty-finish-output)
    (setf (device-beep device) #'tty-beep)
    ;;
    ;; A few useful values.
    (setf (tty-device-dumbp device)
	  (not (and (termcap :open-line termcap)
		    (termcap :delete-line termcap))))
    ;;
    ;; Get size and speed.
    (multiple-value-bind  (lines cols speed)
			  (get-terminal-attributes)
      (setf (tty-device-lines device) (or lines (termcap :lines termcap)))
      (let ((cols (or cols (termcap :columns termcap))))
	(setf (tty-device-columns device)
	      (if (termcap :auto-margins-p termcap)
		  (1- cols) cols)))
      (setf (tty-device-speed device) speed)
      )
)

|#
