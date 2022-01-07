;;; Package and package directory editor, and package manager.

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

(defcommand "List Packages" ()
  "List all packages."
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
  (packdired-command p))

(defcommand "Packdired" ()
  "Create or switch to the package directory."
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

(defcommand "Packdired Quit" ()
  "Kill the Packdired buffer."
  (when *packdired-buffer* (delete-buffer-if-possible *packdired-buffer*)))

(defcommand "Packdired Help" ()
  "Show Packdired mode help."
  (describe-mode-command nil "Packdired"))

(defcommand "Packdired Refresh" ()
  "Refresh the current buffer."
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

(defcommand "Packdired Edit Package" ()
  "Edit the package at point."
  (packed-command nil (package-at-mark (current-point))))

(defcommand "Packdired Edit Package in Other Window" ()
  "Edit the package at point in the other window."
  (let ((package (package-at-mark (current-point))))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command)
	(next-window-command))
    (packed-command nil package)))


;;;; Packed mode.

(defmode "Packed" :major-p t
  :documentation
  "Packed is a mode for editing packages.")

(defcommand "Edit Package" (p)
  "Edit a prompted package in a buffer."
  "Edit $package in a buffer, prompting for a package if $package is nil."
  (packed-command p))

;; FIX prompt
(defcommand "Packed" (p package)
  "Edit a prompted package in a buffer."
  "Edit $package in a buffer, prompting for a package if $package is nil."
  (declare (ignore p))
  ;; FIX get-mode-buffer  gets existing buffer in mode else creates new one
  (let ((buf (make-unique-buffer
	      (format () "Packed ~A"
		      (if (packagep package)
			  (package-name package)
			  (lisp::pkg-info-name package)))
	      :modes '("Packed"))))
    (setf (buffer-writable buf) t)
    (defevar "Package"
      "The package in the current buffer."
      :buffer buf
      :value package)
    (with-output-to-mark (stream (buffer-point buf))
      (if (packagep package)
	  (describe-package package stream)
	  (describe-package-from-meta package stream)))
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

(defcommand "Packed Quit" ()
  "Kill the Packed buffer."
  (delete-buffer-if-possible (current-buffer)))

(defcommand "Packed Help" ()
  "Show Packed mode help."
  (describe-mode-command nil "Packed"))

(defcommand "Packed Refresh" ()
  "Refresh the current buffer."
  (let ((buffer (current-buffer)))
    (when (string= (buffer-major-mode buffer) "Packed")
      (setf (buffer-writable buffer) t)
      (delete-region (buffer-region buffer))
      (with-output-to-mark (stream (buffer-point buffer))
	(let ((package (value package)))
	  (if (packagep package)
	      (describe-package package stream)
	      (describe-package-from-meta package stream))))
      (setf (buffer-writable buffer) nil)
      (setf (buffer-modified buffer) nil)
      (buffer-start (buffer-point buffer)))))


;;;; Package management.

#[ The Package Manager

{command:Manage Packages}
]#

(defvar *packman-buffer* nil
  "The Package Manager buffer.")

(defmode "PackMan" :major-p t
  :documentation
  "Package management.")

(defun delete-packman-buffers (buffer)
  (if (eq buffer *packman-buffer*)
      (setf *packman-buffer* nil)))

;; FIX another in code:mh.lisp
(defun replace-newlines (string)
  "Replace newlines in $string with spaces, returning $string."
  (iterate rep ((i (1- (length string))))
    (if (char= (char string i) #\newline)
	(setf (char string i) #\space))
    (if (plusp i) (rep (1- i))))
  string)

(defun refresh-manage-package (buffer window)
  (let ((mark (copy-mark (buffer-point buffer)
			 :left-inserting))
	(metas))
    (unwind-protect
	(progn
	  ;; FIX improve sort-lines and sort the buffer lines instead of
	  ;; buffering here too
	  ;;
	  ;; Buffer meta info.
	  (package:do-packages (meta)
	    (push meta metas))
	  ;; Sort meta info.
	  (setq metas (sort metas #'string<=
			    :key #'lisp::pkg-info-name))
	  ;; Insert meta info.
	  (dolist (meta metas)
	    (let ((name (lisp::pkg-info-name meta)))
	      (setf (getf (line-plist (mark-line mark)) 'package) meta)
	      (insert-string
	       mark
	       (let ((doc (lisp::pkg-info-doc meta)))
		 ;; First two spaces reserved for D and *.
		 (format () "  ~C~C~C ~@20<~A~>~:[~;~A~]~%"
			 (if (and (package:installed-p name)
				  (if (package:local-version name)
				      (> (package:local-version name)
					 (package:meta-version name))
				      t))
			     #\m
			     #\space)
			 (if (package::loaded-p name)
			     #\l
			     #\space)
			 (if (package::installed-p name)
			     #\i
			     #\space)
			 name
			 doc
			 (if doc
			     (replace-newlines
			      (safe-subseq doc
					   0 (- (window-width window)
						26))))))))))
      (delete-mark mark))))

(defcommand "Update Meta Info" ()
  "Update the database of package meta information."
  (when (prompt-for-y-or-n :prompt "Update package meta information? ")
    (if (package:update-meta-from-server)
	(packman-refresh-command)
	(editor-error "Failed to update package meta information."))))

(defcommand "Manage Packages" ()
  "Create or switch to the package manager."
  (package::ensure-meta-loaded)
  (let ((buf (or *packman-buffer*
		 (make-buffer "PackMan"
			      :modes '("PackMan")
			      :delete-hook (list #'delete-packman-buffers)))))
    (or *packman-buffer*
	(progn
	  (setq *packman-buffer* buf)
	  (refresh-manage-package buf (current-window))
	  (setf (buffer-writable buf) nil)
	  (setf (buffer-modified buf) nil)
	  (let ((fields (buffer-modeline-fields *packman-buffer*)))
	    (setf (cdr (last fields))
		  (list (or (modeline-field :packman-cmds)
			    (make-modeline-field
			     :name :packman-cmds :width 18
			     :function
			     #'(lambda (buffer window)
				 (declare (ignore buffer window))
				 "  Type ? for help.")))))
	    (setf (buffer-modeline-fields *packman-buffer*) fields))
	  (buffer-start (buffer-point buf))))
    (change-to-buffer buf)))

(defcommand "Commit Package" ()
  "If the current package has changed then copy it back to the repository."
  (let ((name (lisp::pkg-info-name
	       (package-managed-at-mark (current-point)))))
    (or (package:local-version name)
	(editor-error
	 "Local version of package must have a version number."))
    (when (prompt-for-y-or-n :prompt `("Commit package ~A? " ,name))
      ;; FIX mv to package:commit
      ;; FIX lock server
      (if (package:update-meta-from-server)
	  (progn
	    (or (package:meta-version name)
		(editor-error
		 "Failed to get server version number of package ~A."
		 name))
	    (if (> (package:local-version name)
		   (package:meta-version name))
		(progn
		  (package:commit name)
		  ;; FIX commit must  update server meta
		  ;; FIX              release server
		  )
		(if (= (package:local-version name)
		       (package:meta-version name))
		    (editor-error "Package ~A is up-to-date on the server." name)
		    (editor-error "Package ~A is newer on the server." name)))
	    (packman-refresh-command))
	  (editor-error "Failed to update package meta information.")))))

(defcommand "Packman Help" ()
  "Show Packman mode help."
  (describe-mode-command nil "Packman"))

(defcommand "Packman Refresh" ()
  "Refresh the current buffer."
  (let* ((buffer (current-buffer))
	 (lines (count-lines (region (buffer-start-mark buffer)
				     (buffer-point buffer)))))
    (when (string= (buffer-major-mode buffer) "PackMan")
      (setf (buffer-writable buffer) t)
      (delete-region (buffer-region buffer))
      (refresh-manage-package buffer (current-window))
      (setf (buffer-writable buffer) nil)
      (setf (buffer-modified buffer) nil)
      (buffer-start (buffer-point buffer))
      (line-offset (buffer-point buffer) (1- lines)))))

(defun package-managed-at-mark (mark)
  (or (getf (line-plist (mark-line mark)) 'package)
      (editor-error "Point must be on a package line.")))

(defcommand "Install Package" ()
  "Install the package listed on the current line."
  (let ((name (lisp::pkg-info-name
	       (package-managed-at-mark (current-point)))))
    (if (package:installed-p name)
	(editor-error "Package ~A is installed already." name))
    (package:install name)
    (packman-refresh-command)))

(defcommand "Test Package" ()
  "Test the package listed on the current line."
  (let ((name (lisp::pkg-info-name
	       (package-managed-at-mark (current-point)))))
    (or (package:installed-p name)
	;; FIX offer to install
	(editor-error "Install ~A first." name))
    (or (package:loaded-p name)
	;; FIX offer to load
	(editor-error "Load ~A first." name))
    (let ((dir (package:test-dir name)))
      (or dir
	  (editor-error "Failed to find test directory for package ~A."
			name))
      (let ((buffer (make-unique-buffer (format () "Test Package ~A"
						name))))
	(change-to-buffer buffer)
	(let ((*standard-output* (make-editor-output-stream (current-point)
							    :none)))
	  (deftest:test-from-dir dir))))))

(defcommand "Flush Package" ()
  "Flush the package listed on the current line from the system."
  (let ((name (lisp::pkg-info-name
	       (package-managed-at-mark (current-point)))))
    (when (and (package::installed-p name)
	       (prompt-for-y-or-n :prompt
				  (format () "Flush package ~A? "
					  name)))
      (package::flush name)
      (packman-refresh-command))))

(defcommand "Editor Load Package" ()
  "Load the package listed on the current line."
  (let* ((package (package-managed-at-mark (current-point)))
	 (name (lisp::pkg-info-name package)))
    (if (package:loaded-p name)
	(editor-error "Package ~A is loaded already." name))
    (or (package:installed-p name)
	;; FIX offer to install
	(editor-error "Install ~A first." name))
    (let ((requires (package:local-requires name)))
      (collect ((loads))
	(dolist (name requires)
	  (or (package:loaded-p name) (loads name)))
	(if (loads)
	    (when (with-pop-up-window (buffer window)
		    (let ((point (buffer-point buffer)))
		      (dolist (name (loads))
			(insert-string
			 point
			 (format () "~A~%" name))))
		    (prompt-for-y-or-n :prompt
				       (format ()
					       "Load package ~A and the required package~P? "
					       name
					       (length requires))
				       :default t
				       :must-exist t
				       :default-string "Y"))
	      (dolist (name requires)
		(or (package:installed-p name)
		    ;; FIX offer to install
		    (editor-error "Install ~A first." name))
		(or (package:loaded-p name)
		    (package:load-package name)))
	      (package:load-package name)
	      (packman-refresh-command))
	    (progn
	      (package:load-package name)
	      (packman-refresh-command)))))))

(defcommand "Describe Package" ()
  "Describe the package listed on the current line."
  (let* ((package (package-managed-at-mark (current-point)))
	 (name (lisp::pkg-info-name package)))
    (packed-command () (or (find-package name) package))))

(defcommand "Edit Package" ()
  "Edit the package listed on the current line."
  (let* ((package (package-managed-at-mark (current-point)))
	 (name (lisp::pkg-info-name package)))
    (or (package::installed-p name)
	;; FIX offer to install
	(editor-error "Install ~A first." name))
    (let ((pathname (package:local-file name)))
      (if pathname
	  (find-file-command () pathname)
	  (editor-error "Failed to find source for package ~A." name)))))

(defcommand "Describe Package in Other Window" ()
  "Edit in the other window the package listed at point."
  (let* ((package (package-managed-at-mark (current-point)))
	 (name (lisp::pkg-info-name package)))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command)
	(next-window-command))
    (packed-command () (or (find-package name) package))))
