;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; Commands to build the system.

(in-package "HEMLOCK")

(defhvar "Source Directory"
  "Source directory for build commands."
  :value nil)

(defhvar "Build Directory" 
  "Destination directory for build commands."
  :value nil)

(defhvar "Builder Directory" 
  "Location of the system used to build the source."
  :value nil)

(defcommand "Set Build Directory" (p)
  "Set the build directory."
  "Set the build directory."
  (declare (ignore p))
  (setv build-directory
	(dired-namify (prompt-for-file
		       :prompt "Build directory: "
		       :must-exist nil
		       :help "Name of build destination directory."
		       :default (value build-directory)))))

(defcommand "Set Source Directory" (p)
  "Set the source directory."
  "Set the source directory."
  (declare (ignore p))
  (setv source-directory
	(dired-namify (prompt-for-file
		       :prompt "Source directory: "
		       :must-exist nil
		       :help "Name of build source directory."
		       :default (value source-directory)))))

(defcommand "Set Builder Directory" (p)
  "Set the builder directory."
  "Set the builder directory."
  (declare (ignore p))
  (setv builder-directory
	(dired-namify (prompt-for-file
		       :prompt "Builder directory: "
		       :must-exist nil
		       :help "Name of builder directory."
		       :default (value builder-directory)))))

(defun get-build-directory ()
  "Return the build directory, prompting for a directory if required."
  (dired-namify (or (value build-directory)
		    (set-build-directory-command nil))))

(defun get-source-directory ()
  "Return the source directory, prompting for a directory if required."
   (dired-namify (or (value source-directory)
		     (set-source-directory-command nil))))

(defun get-builder-directory ()
  "Return the builder directory, prompting for a directory if required."
  (dired-namify (or (value builder-directory)
		    (set-builder-directory-command nil))))

(defvar *builder-slave* nil
  "Slave for building build.")

(defun get-builder-slave ()
  "Return the builder slave, creating it if required."
  ;; FIX check if server matches builder-directory
  ;; FIX may need to check if builder still alive
  (or *builder-slave*
      (hlet ((slave-utility (format nil "~A/lisp/lisp" (get-builder-directory)))
	     (slave-utility-switches
	      `("-core"
		,(format nil "~A/lisp/lisp.core" (get-builder-directory)))))
	(let ((info (create-slave "Builder")))
	  (if info
	      (setq *builder-slave* (server-info-slave-buffer info)))))))

(defun input-source-cleaning (stream)
  "Put commands into Stream to clean the source tree."
  (format stream
	  "(setq user::build \"~A\")"
	  (get-source-directory))
  (confirm-typescript-input-command nil)
  (format stream
	  "(load \"~A/tools/clean-build.lisp\")"
	  (get-source-directory))
  (confirm-typescript-input-command nil))

(defcommand "Clean" (p)
  "Clean the build directory."
  "Clean the build directory."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (input-source-cleaning stream)
      (format stream
	      "(setq user::build \"~A\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/clean-build.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Clean Lisp" (p)
  "Clean the lisp code in the build directory."
  "Clean the lisp code in the build directory."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (input-source-cleaning stream)
      (format stream
	      "(setq user::build \"~A/code\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/clean-build.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Clean Hemlock" (p)
  "Clean the Hemlock code in the build directory."
  "Clean the Hemlock code in the build directory."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (input-source-cleaning stream)
      (format stream
	      "(setq user::build \"~A/hemlock\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/clean-build.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Build" (p)
  "Build all systems."
  "Build all systems."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream
	      "(setq user::src \"~A\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(setq user::target \"~A\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(setq user::systems '(:lisp :compiler :hemlock :kernel))")
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/build-world.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Build Some" (p)
  "Build some of build, prompting for a list of systems to build."
  "Build some of build, prompting for a list of systems to build."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave))
	 (systems (prompt-for-string
		   :prompt "Systems to build: "
		   :default ":lisp :compiler :kernel")))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream
	      "(setq user::src \"~A\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(setq user::target \"~A\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream "(setq user::systems '(~A))" systems)
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/build-world.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Build Lisp" (p)
  "Build the Lisp code."
  "Build the Lisp code."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream
	      "(setq user::src \"~A\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(setq user::target \"~A\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream "(setq user::systems '(:lisp))")
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/build-world.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Build Hemlock" (p)
  "Build Hemlock."
  "Build Hemlock."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream
	      "(setq user::src \"~A\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(setq user::target \"~A\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream "(setq user::systems '(:hemlock))")
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/build-world.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defcommand "Build Kernel" (p)
  "Build the kernel image."
  "Build the kernel image."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let* ((slave (get-builder-slave)))
    (change-to-buffer slave)
    (with-output-to-mark (stream (current-point))
      (format stream
	      "(setq user::src \"~A\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil)
      (format stream
	      "(setq user::target \"~A\")"
	      (get-build-directory))
      (confirm-typescript-input-command nil)
      (format stream "(setq user::systems '(:kernel))")
      (confirm-typescript-input-command nil)
      (format stream
	      "(load \"~A/tools/build-world.lisp\")"
	      (get-source-directory))
      (confirm-typescript-input-command nil))))

(defvar *build-core-buffer* nil)

(defun get-build-core-buffer ()
  "Return the buffer for use in Build Core."
  (or (and *build-core-buffer*
	   (car (memq *build-core-buffer* *buffer-list*)))
      (setq *build-core-buffer* ( #|FIX|# hi::make-unique-buffer "Build Core"))))

(defcommand "Build Core" (p)
  "Build final Lisp core."
  "Build final Lisp core."
  (declare (ignore p))
  (hlet ((save-all-files-confirm nil))
    (save-all-files-command nil))
  (let ((buffer (get-build-core-buffer)))
    (delete-region (buffer-region buffer))
    (change-to-buffer buffer))
  (with-output-to-mark (stream (current-point))
    (ext::run-program (format nil "~A/tools/build-core"
			      (get-source-directory))
		      (list (get-build-directory))
		      :wait t
		      :output stream
		      :error :output)))