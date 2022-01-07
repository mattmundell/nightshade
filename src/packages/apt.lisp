;;; -*- Package: ED -*-
;;;
;;; Apt interface.
;;;
;;; This file is public domain.

(defpackage "APT"
  (:version 1)
  (:use "LISP" "EXTENSIONS" "ED")
  (:documentation "Apt interface."))

(in-package "ED")


;;;; Apt mode.

(defun os-package-on-line (line)
  (let* ((string (line-string line))
	 (pos (search " - " string)))
    (if pos (subseq string 0 pos))))

(defun highlight-apt-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((package (os-package-on-line line)))
      (when package
	(chi-mark line 0 *string-font* :string chi-info)
	(chi-mark line (length package) *original-font*
		  :window-foreground chi-info)))))

(defun highlight-apt-buffer (buffer)
  (highlight-chi-buffer buffer highlight-apt-line))

(defun highlight-visible-apt-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-apt-line))

(defun setup-apt-mode (buffer)
  (highlight-visible-apt-buffer buffer)
  (pushnew '("Apt" () highlight-visible-apt-buffer)
	   *mode-highlighters*))

(defmode "Apt" :major-p ()
  :setup-function #'setup-apt-mode)


;;;; AptInfo mode.

(defun highlight-apt-info-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((string (line-string line)))
      (when (plusp (length string))
	(or (member (char string 0) '(#\space #\tab))
	    (let ((pos (position #\: string)))
	      (when pos
		(chi-mark line 0 *special-form-font*
			  :special-form chi-info)
		(chi-mark line (1+ pos) *original-font*
			  :window-foreground chi-info))))))))

(defun highlight-apt-info-buffer (buffer)
  (highlight-chi-buffer buffer highlight-apt-info-line))

(defun highlight-visible-apt-info-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-apt-info-line))

(defun setup-apt-info-mode (buffer)
  (highlight-visible-apt-info-buffer buffer)
  (pushnew '("AptInfo" () highlight-visible-apt-info-buffer)
	   *mode-highlighters*))

(defmode "AptInfo" :major-p ()
  :setup-function #'setup-apt-info-mode)


;;;; Commands.

(defun apt-new-name (prefix suffix)
  (while ((num 0 (1+ num))
	  (name (format () "~A ~A" prefix suffix)
		(format () "~A ~A ~A" prefix suffix num)))
	 ((getstring name *buffer-names*)
	  name)))

(defcommand "Apt" ()
  "List a prompted search for packages."
  (let* ((search (prompt-for-string
		  :prompt "OS package search: "))
	 (buffer (make-new-shell () ()
				 (format () "apt-cache search ~A"
					 search)
				 :clear-buffer t)))
    (setf (buffer-name buffer) (apt-new-name "OS-pkgs" search))
    (setf (buffer-minor-mode buffer "Apt") t)
    (buffer-start (buffer-point buffer))))

(defcommand "Apt Add" ()
  "Install the package listed on the current line."
  (let* ((name (os-package-on-line (current-line)))
	 (buffer (make-new-shell () ()
				 (format () "sudo apt-get install ~A"
					 name)
				 :clear-buffer t)))
    (setf (buffer-name buffer) (apt-new-name "OS-pkg-install" name))))

(defcommand "Apt Remove" ()
  "Remove the package listed on the current line."
  (let* ((name (os-package-on-line (current-line)))
	 (buffer (make-new-shell () ()
				 (format () "sudo apt-get remove ~A"
					 name)
				 :clear-buffer t)))
    (setf (buffer-name buffer)
	  (apt-new-name "OS-pkg-remove" name))))

(defcommand "Apt Info" ()
  "Show info about the package on the current line."
  (let* ((name (os-package-on-line (current-line)))
	 (buffer (make-new-shell () ()
				 (format () "apt-cache show ~A"
					 name)
				 :clear-buffer t)))
    (setf (buffer-name buffer)
	  (apt-new-name "OS-pkg-info" name))
    (setf (buffer-minor-mode buffer "AptInfo") t)
    (buffer-start (buffer-point buffer))))

(defcommand "Apt Contents" ()
  "Show the contents of the package on the current line."
  (let* ((name (os-package-on-line (current-line)))
	 (buffer (make-new-shell
		  () ()
		  (format () "dpkg --listfiles ~A" name)
		  :clear-buffer t)))
    (setf (buffer-name buffer)
	  (apt-new-name "OS-pkg-contents" name))
    (buffer-start (buffer-point buffer))))

(defcommand "Apt Update" ()
  "Update the package list."
  (let ((buffer (make-new-shell () ()
				"sudo apt-get update"
				:clear-buffer t)))
    (setf (buffer-name buffer)
	  (apt-new-name "OS-cache-update" ""))))


;;;; Bindings.

(bind-key "Scroll Window Up" #k"delete" :mode "Apt")
(bind-key "Scroll Window Down" #k"space" :mode "Apt")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Apt")
(bind-key "Next Line" #k"tab" :mode "Apt")
(bind-key "Next Line" #k"n" :mode "Apt")
(bind-key "Previous Line" #k"meta-tab" :mode "Apt")
(bind-key "Previous Line" #k"p" :mode "Apt")
(bind-key "Apt Add" #k"a" :mode "Apt")
(bind-key "Apt Remove" #k"r" :mode "Apt")
(bind-key "Apt Info" #k"i" :mode "Apt")
(bind-key "Apt Contents" #k"c" :mode "Apt")
(bind-key "Apt Contents" #k"l" :mode "Apt")
(bind-key "Apt Update" #k"u" :mode "Apt")
