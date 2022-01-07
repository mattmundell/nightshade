;;; OS interface functions for running under BSD Unix.

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(pushnew :bsd *features*)
(pushnew :freebsd *features*)

(setq *software-type* #+FreeBSD "FreeBSD" #-FreeBSD "BSD")

(defvar *software-version* nil "Version string for supporting software")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (or *software-version*
      (setf *software-version*
	    (string-trim '(#\newline)
			 (with-output-to-string (stream)
			   (run-program "/usr/bin/uname"
					'("-r")
					:output stream))))))


;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defun os-init ()
  (setf *software-version* nil))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (or err?
	(error "Unix system call getrusage failed: ~A."
	       (unix:get-unix-error-msg utime)))

    (values utime stime majflt)))


;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  ;; probably should call getpagesize()
  4096)
