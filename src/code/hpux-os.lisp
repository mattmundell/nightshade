;;; OS interface functions for running under HPUX.

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(pushnew :hpux *features*)
(setq *software-type* "HPUX")

(defvar *software-version* nil "Version string for supporting software")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
          (let ((version-line
                 (with-output-to-string (stream)
                   (run-program "/usr/bin/uname"
                                '("-r" )
                                :output stream
                                :pty nil
                                :error nil))))
            (subseq version-line 0 (1- (length version-line))))))
  *software-version*)

;;; OS-INIT -- interface.
;;;
;;; Other OS dependent initializations.
;;;
(defun os-init ()
  ;; Decache version on save, because it might not be the same when we restart.
  (setf *software-version* nil))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime maxrss ixrss idrss isrss minflt majflt)
      (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (cond ((null err?)
	   (error "Unix system call getrusage failed: ~A."
		  (unix:get-unix-error-msg utime)))
	  (T
	   (values utime stime majflt)))))

;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (multiple-value-bind (val err)
		       (unix:unix-getpagesize)
    (unless val
      (error "Getpagesize failed: ~A" (unix:get-unix-error-msg err)))
    val))
