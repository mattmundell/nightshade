;;; Terminal control.

(in-package "TERMINAL")

(export '(attribute))

(defun attribute (fd attribute)
  "Return the $attribute of the terminal on descriptor $fd."
  (alien:with-alien ((tios (alien:struct unix:termios)))
    (multiple-value-bind
	(val err)
	(unix:unix-tcgetattr fd (alien:alien-sap tios))
      (or val
	  (error "Failed to tcgetattr: ~S."
		 (unix:get-unix-error-msg err))))
    (ecase attribute
      ((:output-crnl :output-nlcr)
       (let ((flags (alien:slot tios 'unix:c-oflag)))
	 (ecase attribute
	   (:output-crnl
	    (logtest flags unix:tty-ocrnl))
	   (:output-nlcr
	    (logtest flags unix:tty-onlcr))))))))

#|
(attribute edi::*editor-file-descriptor* :output-crnl)
(attribute edi::*editor-file-descriptor* :output-nlcr)
|#

(defun set-attribute (fd attribute value)
  "Set the $attribute of the terminal on descriptor $fd to $value."
  (alien:with-alien ((tios (alien:struct unix:termios)))
    (multiple-value-bind
	(val err)
	(unix:unix-tcgetattr fd (alien:alien-sap tios))
      (or val
	  (error "Failed to tcgetattr: ~S."
		 (unix:get-unix-error-msg err))))
    (ecase attribute
      ((:output-crnl :output-nlcr)
       (ecase attribute
	 (:output-crnl
	  (setf (alien:slot tios 'unix:c-oflag)
		(if value
		    (logior (alien:slot tios 'unix:c-oflag)
			    unix:tty-ocrnl)
		    (logand (alien:slot tios 'unix:c-oflag)
			    (logcom unix:tty-ocrnl))))
	  value)
	 (:output-nlcr
	  (setf (alien:slot tios 'unix:c-oflag)
		(if value
		    (logior (alien:slot tios 'unix:c-oflag)
			    unix:tty-onlcr)
		    (logand (alien:slot tios 'unix:c-oflag)
			    (logcom unix:tty-onlcr))))))
       (multiple-value-bind
	   (val err)
	   (unix:unix-tcsetattr fd unix:tcsaflush
				(alien:alien-sap tios))
	 (or val
	     (error "Failed to tcsetattr: ~S."
		    (unix:get-unix-error-msg err))))
       value))))
;;
(defsetf attribute set-attribute)

#|
(setf (attribute edi::*editor-file-descriptor* :output-crnl) t)
|#
