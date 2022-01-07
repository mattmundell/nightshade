;;; Special proclamations for variables that are referenced in the code
;;; sources before they are defined.  There are also some function
;;; proclamations to make some common functions be known, avoiding large
;;; amounts of work in recording the calls that are done before the
;;; definition.

(in-package "LISP")

(declaim (special *keyword-package* *lisp-package* *package* *query-io*
		  *terminal-io* *error-output* *trace-output* *debug-io*
		  *standard-input* *standard-output*
		  *evalhook* *applyhook* *command-line-switches*
		  *command-switch-demons* ext::temporary-foreign-files
		  *display-event-handlers* original-lisp-environment
		  *environment-list* *read-default-float-format*
		  *read-suppress* *readtable* *print-base* *print-radix*
		  *print-length* *print-level* *print-pretty* *print-escape*
		  *print-case* *print-circle* *print-gensym* *print-array*
		  defmacro-error-string defsetf-error-string
		  std-lisp-readtable
		  debug::*in-the-debugger*
		  conditions::*handler-clusters*
		  conditions::*restart-clusters*
		  *gc-inhibit* *need-to-collect-garbage*
		  defmacro-error-string deftype-error-string
		  defsetf-error-string %sp-interrupts-inhibited
		  *software-interrupt-vector* *load-verbose*
		  *load-print-stuff* *in-compilation-unit*
		  *aborted-compilation-units* char-name-alist
		  *default-pathname-defaults* *beep-function*
		  *gc-notify-before* *gc-notify-after*))


(declaim (ftype (function * *)
		find-keyword keyword-test assert-error
		assert-prompt check-type-error case-body-error print-object
		describe-object pcl::check-wrapper-validity))

#-clx
(declaim (ftype (function * *)
		disable-clx-event-handling
		extensions::call-display-event-handler
		;xlib::display-input-stream
		xlib::display-fd xlib:event-listen
		flush-display-events))

#-ed
(declaim (ftype (function * *)
		edi::current-window
		edi::device-exit
		edi::device-hunk-device
		edi::device-init
		ed::ts-stream-p ed::ts-stream-wire
		edi::window-hunk))

;; FIX
;;; Gray streams functions not defined until after PCL is loaded.
(declaim (ftype (function * *)
		stream-advance-to-column stream-clear-input
		stream-clear-output stream-finish-output stream-force-output
		stream-fresh-line stream-line-column stream-line-length
		stream-listen stream-peek-char stream-read-byte
		stream-read-char stream-read-char-no-hang stream-read-line
		stream-start-line-p stream-terpri stream-unread-char
		stream-write-byte stream-write-char stream-write-string))
