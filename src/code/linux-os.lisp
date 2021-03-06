;;; OS interface functions for running under Linux.

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))
#+nil
(export '(*task-self* *task-data* *task-notify*))

(pushnew :linux *features*)

(setq *software-type* "Linux")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  #+nil
  (string-trim '(#\newline)
	       (with-output-to-string (stream)
		 (run-program "/usr/cs/etc/version" ; Site dependent???
			      nil :output stream)))
  "n/a")


;; FIX init-os?
;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defvar *task-self*)

(defun os-init ()			; don't know what to do here
  #+nil
  (setf *task-self* (mach:mach-task_self))
  #+sparc ;; Can't use #x20000000 thru #xDFFFFFFF, but mach tries to let us.
  (system:allocate-system-memory-at (system:int-sap #x20000000) #xc0000000))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;; Return system time, user time and number of page faults.
;;;
(defun get-system-info ()
  (multiple-value-bind (success utime stime maxrss ixrss idrss
				isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (or success
	(error "Unix system call getrusage failed: ~A."
	       (unix:get-unix-error-msg utime)))

    (values utime stime majflt)))

;;; GET-PAGE-SIZE  --  Interface
;;;
;;; Return the system page size.
;;;
(defun get-page-size ()
  ;; probably should call getpagesize()
  4096)
