;;; Commands to build the system.

;; FIX move most to n:src/Build.lisp

(in-package "ED")

(defevar "Source Directory"
  "Source directory for build commands.")

(defevar "Build Directory"
  "Destination directory for build commands.")

(defevar "Builder Directory"
  "Location of the system used to build the source.")

(defcommand "Set Build Directory" ()
  "Set the build directory."
  (setv build-directory
	(namify (prompt-for-file
		 :prompt "Build directory: "
		 :must-exist nil
		 :help "Name of build destination directory."
		 :default (value build-directory)))))

(defcommand "Set Source Directory" ()
  "Set the source directory."
  (setv source-directory
	(namify (prompt-for-file
		 :prompt "Source directory: "
		 :must-exist nil
		 :help "Name of build source directory."
		 :default (value source-directory)))))

(defcommand "Set Builder Directory" ()
  "Set the builder directory."
  (setv builder-directory
	(namify (prompt-for-file
		 :prompt "Builder directory: "
		 :must-exist nil
		 :help "Name of builder directory."
		 :default (value builder-directory)))))

(defcommand "Set Builder Slave" ()
  "Set the slave used for building."
  (let ((info (nth-value
	       1
	       (prompt-for-keyword (list *server-names*)
				   :prompt "Existing server name: "
				   :help
				   "Enter the name of an existing eval server."
				   :must-exist t))))
    (setq *builder-slave* (server-info-slave-buffer info))))

(defun get-build-directory ()
  "Return the build directory."
  (namestring (truename (namify (or (value build-directory)
				    build:*build-directory*)))))

(defun get-source-directory ()
  "Return the source directory."
  (namestring (truename (namify (or (value source-directory)
				    build:*source-directory*)))))

(defun get-builder-directory ()
  "Return the builder directory."
  (namestring (truename (namify (or (value builder-directory)
				    build:*builder-directory*)))))

(defvar *builder-slave* ()
  "Slave for building build.")

(defun get-builder-slave ()
  "Return the builder slave, creating it if required."
  ;; FIX check if server matches builder-directory
  (if (and *builder-slave*
	   (let ((buffer (getstring (buffer-name *builder-slave*)
				    *buffer-names*)))
	     (and buffer
		  ;; Check if builder is alive.
		  (editor-bound-p 'server-info
				  :buffer *builder-slave*))))
      *builder-slave*
      (elet ((slave-utility (format () "~A/bin/nightshade" (get-builder-directory)))
	     (confirm-slave-creation))
	(let* ((name (unique-buffer-name "Builder"))
	       (info (create-slave name)))
	  (if info
	      (setq *builder-slave* (server-info-slave-buffer info)))))))

(defun queue-build-form (string)
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (string-eval string
		 :server (variable-value 'server-info
					 :buffer slave))))

(defcommand "Clean All" ()
  "Clean the build directory."
  (queue-build-form (format () "(build:build \"~A/\" \"clean\")"
			    (get-source-directory))))

(defcommand "Clean Lisp" ()
  "Clean the Lisp code in the build directory."
  (queue-build-form (format () "(build:build \"~A/\" \"clean-lisp\")"
			    (get-source-directory))))

(defcommand "Clean Editor" ()
  "Clean the editor code in the build directory."
  (queue-build-form (format () "(build:build \"~A/\" \"clean-editor\")"
			    (get-source-directory))))

(defcommand "Clean Bin" ()
  "Clean the C image loader directory."
  (queue-build-form (format () "(build:build \"~A/\" \"clean-bin\")"
			    (get-source-directory))))

(defcommand "Build All" ()
  "Build all systems."
  (queue-build-form (format () "(build:build \"~A/\" \"all\")"
			    (get-source-directory))))

(defcommand "Build Clone" ()
  "Invoke the \"clone\" rule in the Builder slave, which updates the
   builder directory to be a copy of the latest build while keeping the old
   builder.  The Builder slave remains the previous version."
  (queue-build-form (format () "(build:build \"~A/\" \"clone\")"
			    (get-source-directory))))

(defcommand "Build Lisp" ()
  "Build the Lisp code."
  (queue-build-form (format () "(build:build \"~A/\" \"lisp\")"
			    (get-source-directory))))

(defcommand "Build Editor" ()
  "Build the Editor."
  (queue-build-form (format () "(build:build \"~A/\" \"editor\")"
			    (get-source-directory))))

(defcommand "Build Kernel" ()
  "Build the kernel image."
  (queue-build-form (format () "(build:build \"~A/\" \"kernel\")"
			    (get-source-directory))))

(defcommand "Build Bin" ()
  "Build the C image loader."
  (queue-build-form (format () "(build:build \"~A/\" \"bin\")"
			    (get-source-directory))))

(defcommand "Build Header" ()
  "Build the header for the C image loader."
  (queue-build-form (format () "(build:build \"~A/\" \"header\")"
			    (get-source-directory))))

(defvar *build-core-buffer* nil)

(defun get-build-core-buffer ()
  "Return the buffer for use in Build Core."
  (or (and *build-core-buffer*
	   (car (memq *build-core-buffer* *buffer-list*)))
      (setq *build-core-buffer* (make-unique-buffer "Build Core"))))

(defcommand "Build Core" ()
  "Build final Lisp core."
  (elet ((save-all-files-confirm))
    (save-all-files-command))
  ;; FIX get build core slave
  (let ((buffer (get-build-core-buffer)))
    (delete-region (buffer-region buffer))
    (change-to-buffer buffer)
    (let ((*standard-output* (make-editor-output-stream
			      (copy-mark (current-point)))))
      (build:build "n:src/" "core"))
    (setf (buffer-modified buffer) ())))

(defcommand "Build Boot" ()
  "Build the boot code."
  (queue-build-form (format () "(build:build \"~A/\" \"boot\")"
			    (get-source-directory))))

(defvar *build-boot-core-buffer* nil)

(defun get-build-boot-core-buffer ()
  "Return the buffer for use in Build Boot Core."
  (or (and *build-boot-core-buffer*
	   (car (memq *build-boot-core-buffer* *buffer-list*)))
      (setq *build-boot-core-buffer*
	    (make-unique-buffer "Build Boot Core"))))

(defcommand "Build Boot Core" ()
  "Build the boot CD."
  (elet ((save-all-files-confirm))
    (save-all-files-command))
  ;; FIX get build core slave
  (let ((buffer (get-build-boot-core-buffer)))
    (delete-region (buffer-region buffer))
    (change-to-buffer buffer)
    (let ((*standard-output* (make-editor-output-stream
			      (copy-mark (current-point)))))
      (build:build "n:src/" "boot-core"))
    (setf (buffer-modified buffer) ())))

(defcommand "Build CD" ()
  "Build the boot CD."
  (queue-build-form (format () "(build:build \"~A/\" \"cd\")"
			    (get-source-directory))))

(defcommand "Build Doc" ()
  "Build the documentation for the website."
  (queue-build-form (format () "(build:build \"~A/\" \"doc\")"
			    (get-source-directory))))

(defcommand "Test All" ()
  "Run all system tests."
  (queue-build-form (format () "(build:build \"~A/\" \"test\")"
			    (get-source-directory))))

(defcommand "Test Code" ()
  "Run all system tests."
  (queue-build-form (format () "(build:build \"~A/\" \"test-code\")"
			    (get-source-directory))))

(defcommand "Test Ed" ()
  "Run all system tests."
  (queue-build-form (format () "(build:build \"~A/\" \"test-ed\")"
			    (get-source-directory))))


;;;; Building distribution archive.

(defcommand "Build Distribution" ()
  "Build distribution, which includes binary and source."
  (queue-build-form (format () "(build:build \"~A/\" \"dist\")"
			    (get-source-directory))))


;;; Syncing commands.

(defcommand "Build Manifest" ()
  "Update the times in the manifest files in the current source tree."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((buffer (make-unique-buffer "Build Manifest")))
    (change-to-buffer buffer)
    (let ((*standard-output* (make-editor-output-stream (current-point)
							:none)))
      (format *standard-output* "Merging...~%")
      (sync:update-manifest-times "n:"))
    (setf (buffer-modified buffer) ())))

(defcommand "Build Merge" (p)
  "Merge from a prompted location.  With a prefix output verbose messages."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((buffer (make-unique-buffer "Build Merge")))
    (change-to-buffer buffer)
    (let ((*standard-output* (make-editor-output-stream (current-point)
							:none)))
      (format *standard-output* "Merging...~%")
      (sync:merge-from-server (prompt-for-file
			       :prompt "Merge from: "
			       :must-exist ()
			       :help "Pathname from which to merge.")
			      p))
    (setf (buffer-modified buffer) ())))
