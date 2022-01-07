;;; -*- Package: System -*-
;;;
;;; Utility to load subsystems and save a new core.

(in-package "USER")

(block abort
  (let ((output-file #p"library:lisp.core")
	(load-gray-streams t)
	(load-clm t)
	(load-clx t)
	(load-editor t)
	(other ()))
    (loop
      (fresh-line)
      (format t " 1: specify result file (currently ~S)~%"
	      (namestring output-file))
      (format t " 2: toggle loading of the Gray Stream library, currently ~
		 ~:[dis~;en~]abled.~%"
	      load-gray-streams)
      (format t " 3: toggle loading of the CLX X library, currently ~
		 ~:[dis~;en~]abled.~%"
	      load-clx)
      (format t " 4: toggle loading of Motif and the graphical debugger, ~
		 currently ~:[dis~;en~]abled.~
		 ~:[~%    (would force loading of CLX.)~;~]~%"
	      load-clm load-clx)
      (format t " 5: toggle loading the editor, currently ~
		 ~:[dis~;en~]abled.~
		 ~:[~%    (would force loading of CLX.)~;~]~%"
	      load-editor load-clx)
      (format t " 6: specify some site-specific file to load.~@
		 ~@[    Current files:~%~{      ~S~%~}~]"
	      (mapcar #'namestring other))
      (format t " 7: configure according to current options.~%")
      (format t " 8: abort the configuration process.~%")
      (format t "~%Option number: ")
      (force-output)
      (flet ((file-prompt (prompt)
	       (format t prompt)
	       (force-output)
	       (pathname (string-trim " 	" (read-line)))))
	(let ((res (ignore-errors (read-from-string (read-line)))))
	  (case res
	    (1
	     (setq output-file (file-prompt "Result core file name: ")))
	    (2
	     (setq load-gray-streams (not load-gray-streams)))
	    (3
	     (or (setq load-clx (not load-clx))
		 (setq load-editor nil)))
	    (4
	     (when (setq load-clm (not load-clm))
	       (setq load-clx t)))
	    (5
	     (when (setq load-editor (not load-editor))
	       (setq load-clx t)))
	    (6
	     (setq other
		   (append other
			   (list (file-prompt "File(s) to load ~
					       (can have wildcards): ")))))
	    (7 (return))
	    (8
	     (format t "~%Aborted.~%")
	     (return-from abort))))))

    (gc-off)
    (when load-gray-streams
      (load "library:subsystems/gray-streams-library"))
    (when load-clx
      (setf *features* (delete :no-clx *features* :test #'eq))
      (load "library:subsystems/clx-library"))
    (when load-clm
      (setf *features* (delete :no-clm *features* :test #'eq))
      (load "library:subsystems/clm-library"))
    (when load-editor
      (setf *features* (delete :no-editor *features* :test #'eq))
      (load "library:subsystems/ed-library"))
    (dolist (f other) (load f))

    (setq *info-environment*
	  (list* (make-info-environment :name "Working")
		 (compact-info-environment (first *info-environment*)
					   :name "Auxiliary")
		 (rest *info-environment*)))

    (when (probe-file output-file)
      (multiple-value-bind
	  (ignore old new)
	  (rename-file output-file
		       (concatenate 'string (namestring output-file)
				    ".BAK"))
	(declare (ignore ignore))
	(format t "~&Saved ~S as ~S.~%" (namestring old) (namestring new))))

    ;;
    ;; Enable the garbage collector.  But first fake it into thinking that
    ;; we don't need to garbage collect.  The save-lisp is going to call
    ;; purify so any garbage will be collected then.
    (setf lisp::*need-to-collect-garbage* ())
    (gc-on)
    ;;
    ;; Save the lisp.
    (save-lisp output-file)))

(quit)
