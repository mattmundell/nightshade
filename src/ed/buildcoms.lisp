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
	(lisp::namify (prompt-for-file
		 :prompt "Build directory: "
		 :must-exist nil
		 :help "Name of build destination directory."
		 :default (value build-directory)))))

(defcommand "Set Source Directory" ()
  "Set the source directory."
  (setv source-directory
	(lisp::namify (prompt-for-file
		 :prompt "Source directory: "
		 :must-exist nil
		 :help "Name of build source directory."
		 :default (value source-directory)))))

(defcommand "Set Builder Directory" ()
  "Set the builder directory."
  (setv builder-directory
	(lisp::namify (prompt-for-file
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
  "Return the build directory, prompting for a directory if required."
  (namify (or (value build-directory)
	      (set-build-directory-command))))

(defun get-source-directory ()
  "Return the source directory, prompting for a directory if required."
  (namify (or (value source-directory)
	      (set-source-directory-command))))

(defun get-builder-directory ()
  "Return the builder directory, prompting for a directory if required."
  (namify (or (value builder-directory)
	      (set-builder-directory-command))))

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
      (elet ((slave-utility (format () "~A/lisp/nightshade" (get-builder-directory)))
	     (slave-utility-switches
	      `("-core"
		,(format () "~A/lisp/lisp.core" (get-builder-directory))))
	     (confirm-slave-creation))
	(let* ((name (unique-buffer-name "Builder"))
	       (info (create-slave name)))
	  (if info
	      (setq *builder-slave* (server-info-slave-buffer info)))))))

(defcommand "Clean All" ()
  "Clean the build directory."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"clean\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Clean Lisp" ()
  "Clean the Lisp code in the build directory."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"clean-lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Clean Editor" ()
  "Clean the editor code in the build directory."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"clean-editor\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

;;; FIX run in process mode  shell-command?
;;;       or as slaves, maybe build: can use current slave

(defcommand "Build All" ()
  "Build all systems."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"all\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Build Lisp" ()
  "Build the Lisp code."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Build Editor" ()
  "Build the Editor."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"editor\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Build Kernel" ()
  "Build the kernel image."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"kernel\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

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

(defcommand "Build Doc" ()
  "Build the documentation for the website."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"doc\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Test All" ()
  "Run all system tests."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"test\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Test Code" ()
  "Run all system tests."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"test-code\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))

(defcommand "Test Ed" ()
  "Run all system tests."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"test-ed\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))


;;;; Building distribution archive.

(defcommand "Build Distribution" ()
  "Build distribution, which includes binary and source."
  (elet ((save-all-files-confirm nil))
    (save-all-files-command))
  (let ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream "(build:build \"~A/\" \"dist\")"
	      (get-source-directory))
      (confirm-typescript-input-command))))


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
