;;; Package and package directory editor.

(in-package "ED")

(defvar *list-packages-max-indent* 25)

(defun list-packages (packages stream)
  (let ((indent (min (+ (apply #'longest-length
			       (mapcar #'package-name packages))
			2)
		     *list-packages-max-indent*)))
    (mapc (lambda (package)
	    (let ((nicks (package-nicknames package))
		  (package-name (package-name package)))
	      (if (and nicks (plusp (length (car nicks))))
		  (progn
		    (format stream (format nil "~~@~D<~~A~~>" indent)
			    package-name)
		    (write-string (mapconcat #'identity nicks " ")
				  stream))
		  (format stream "~A" package-name))
	      (write-char #\newline stream)))
	  packages)))

(defcommand "List Packages" (p)
  "List all packages."
  "List all packages."
  (declare (ignore p))
  (with-pop-up-display (stream)
    (list-packages (list-all-packages) stream)))


;;;; Packdired mode.

(defvar *packdired-buffer* nil
  "The Packdired buffer.")
(defvar *packdired-packages* nil
  "The packages listed in the Packdired buffer.  All packages for now.")

(defmode "Packdired" :major-p t
  :documentation
  "Packdired is a mode for editing the directory of packages.")

(defun delete-packdired-buffers (buffer)
  (when (eq buffer *packdired-buffer*)
    (setf *packdired-buffer* nil)
    (setf *packdired-packages* nil)))

(defcommand "Edit Packages" (p)
  "Create or switch to the package directory."
  "Create or switch to the package directory."
  (packdired-command p))

(defcommand "Packdired" (p)
  "Create or switch to the package directory."
  "Create or switch to the package directory."
  (declare (ignore p))
  (let ((buf (or *packdired-buffer*
		 (make-buffer "Packdired" :modes '("Packdired")
			      :delete-hook (list #'delete-packdired-buffers)))))
    (or *packdired-buffer*
	(progn
	  (setq *packdired-buffer* buf)
	  (setq *packdired-packages* (list-all-packages))
	  (setf (buffer-writable buf) t)
	  (with-output-to-mark (stream (buffer-point buf))
	    (list-packages *packdired-packages* stream))
	  (setf (buffer-writable buf) nil)
	  (setf (buffer-modified buf) nil)
	  (let ((fields (buffer-modeline-fields *packdired-buffer*)))
	    (setf (cdr (last fields))
		  (list (or (modeline-field :packdired-cmds)
			    (make-modeline-field
			     :name :packdired-cmds :width 18
			     :function
			     #'(lambda (buffer window)
				 (declare (ignore buffer window))
				 "  Type ? for help.")))))
	    (setf (buffer-modeline-fields *packdired-buffer*) fields))
	  (buffer-start (buffer-point buf))))
    (change-to-buffer buf)))

(defcommand "Packdired Quit" (p)
  "Kill the Packdired buffer."
  "Kill the Packdired buffer."
  (declare (ignore p))
  (when *packdired-buffer* (delete-buffer-if-possible *packdired-buffer*)))

(defcommand "Packdired Help" (p)
  "Show Packdired mode help."
  "Show Packdired mode help."
  (declare (ignore p))
  (describe-mode-command nil "Packdired"))

(defcommand "Packdired Refresh" (p)
  "Refresh the current buffer."
  "Refresh the current buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (when (string= (buffer-major-mode buffer) "Packdired")
      (setf (buffer-writable buffer) t)
      (delete-region (buffer-region buffer))
      (with-output-to-mark (stream (buffer-point buffer))
	(list-packages *packdired-packages* stream))
      (setf (buffer-writable buffer) nil)
      (setf (buffer-modified buffer) nil)
      (buffer-start (buffer-point buffer)))))

(defun package-at-mark (mark)
  (when (blank-line-p (mark-line mark))
    (editor-error "Point must be on a package line."))
  (nth (1- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark)))
				mark)))
       *packdired-packages*))

(defcommand "Packdired Edit Package" (p)
  "Edit the package at point."
  "Edit the package at point."
  (declare (ignore p))
  (packed-command nil (package-at-mark (current-point))))

(defcommand "Packdired Edit Package in Other Window" (p)
  "Edit the package at point in the other window."
  "Edit the package at point in the other window."
  (declare (ignore p))
  (let ((package (package-at-mark (current-point))))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command nil)
	(next-window-command nil))
    (packed-command nil package)))


;;;; Packed mode.

(defmode "Packed" :major-p t
  :documentation
  "Packed is a mode for editing packages.")

(defcommand "Edit Package" (p)
  "Edit a prompted package in a buffer."
  "Edit PACKAGE in a buffer, prompting for a package if PACKAGE is nil."
  (packed-command p))

(defcommand "Packed" (p &optional package)
  "Edit a prompted package in a buffer."
  "Edit PACKAGE in a buffer, prompting for a package if PACKAGE is nil."
  (declare (ignore p))
  ;; FIX get-mode-buffer  gets existing buffer in mode else creates new one
  (let ((buf (make-unique-buffer
	      (format nil "Packed ~A" (package-name package))
	      :modes '("Packed"))))
    (setf (buffer-writable buf) t)
    (defhvar "Package"
      "The package in the current buffer."
      :buffer buf
      :value package)
    (with-output-to-mark (stream (buffer-point buf))
      (describe-package package stream))
    (setf (buffer-writable buf) nil)
    (setf (buffer-modified buf) nil)
    (let ((fields (buffer-modeline-fields buf)))
      (setf (cdr (last fields))
	    (list (or (modeline-field :packed-cmds)
		      (make-modeline-field
		       :name :packed-cmds :width 18
		       :function
		       #'(lambda (buffer window)
			   (declare (ignore buffer window))
			   "  Type ? for help.")))))
      (setf (buffer-modeline-fields buf) fields))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)))

(defcommand "Packed Quit" (p)
  "Kill the Packed buffer."
  "Kill the Packed buffer."
  (declare (ignore p))
  (delete-buffer-if-possible (current-buffer)))

(defcommand "Packed Help" (p)
  "Show Packed mode help."
  "Show Packed mode help."
  (declare (ignore p))
  (describe-mode-command nil "Packed"))

(defcommand "Packed Refresh" (p)
  "Refresh the current buffer."
  "Refresh the current buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (when (string= (buffer-major-mode buffer) "Packed")
      (setf (buffer-writable buffer) t)
      (delete-region (buffer-region buffer))
      (with-output-to-mark (stream (buffer-point buffer))
	(describe-package (value package) stream))
      (setf (buffer-writable buffer) nil)
      (setf (buffer-modified buffer) nil)
      (buffer-start (buffer-point buffer)))))
