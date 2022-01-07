;; Read from a gzip stream.

(let ((err nil)
      (sap nil)
      (len 20)

      (process (run-program "gzip"
			    `("-d" "-c" ,(unix-namestring "info:coreutils.info.gz"))
			    :wait nil
			    :output :stream)))
  (when process

    (setf sap (system:allocate-system-memory len))
    (or (fd-stream-p (process-output process))
	(editor-error "some other type of stream"))
    (multiple-value-bind
	(bytes err3)
	(unix:unix-read (fd-stream-fd (process-output process)) sap len)
      (if (or (null bytes) (not (= len bytes)))
	  (setq err err3)
	  (setq err nil)))

    (process-close process)

    (when err
      (editor-error "unix error ~A." (unix:get-unix-error-msg err)))

    (let* ((chars (make-string len)))
      (%primitive byte-blt sap 0 chars 0 len)
      (insert-string (current-point) chars))

    ))
