;;; Menu.

(in-package "ED")

#[ Menu

The `Menu' command brings up a menu of items, each of which can be
selected.

{command:Menu}
]#

(defevar "Show Menu On Start"
  "If true show the menu on start."
  :value t)

(defevar "Menu"
  "The main menu."
  :value '(("Home" ("Edit the home of ~A."
		    (let ((full-name (user-full-name)))
		      (if (plusp (length full-name))
			  full-name
			  (user-name))))
	    "h")
	   ("/" "Edit the root of the file system."
	    "/")
	   ("Desktop" "Edit the Desktop.")
	   ("")
	   ("Help" "Choose a help options.")
	   ("Find" "Open a file or directory for editing.")
	   ("Info" "Read documentation.")
	   ("Tutorial" "Do the editor tutorial.")
	   ("Packdired" "Browse installed packages.")
	   ("Manage Packages" "Enter the package manager.")
; 	   ("Config" "Configure the system."
; 	   ("apps..."   "Run applications.")
; 	   ("recent files"   "Browse the `Find' history.")
	   ("Bufed" "Browse the list of buffers.")
	   ("Browse Kill Ring" "Browse the history of kills.")
	   ("Evented" "Edit the list of scheduled events (reminders, etc).")
	   ("")
	   ("Editor Evaluate Expression" "Evaluate an expression in the context of the editor.")
	   ("Select Slave" "Switch to a slave Lisp.")
	   ("Break" "Break out of the editor to the top level.")
	   ("")
	   ("")
	   ("Save All Files and Exit" "Exit the editor.")
	   ("")
	   ("Suspend" ("Suspend ~A." (machine-instance)))
	   ("Reboot" ("Reboot ~A." (machine-instance)))
	   ("Halt" ("Halt ~A." (machine-instance)))
	   ("")
	   ("Exit Menu" "Exit this menu.")))

(defvar *apps-menu* ())

(defun select-menu-item (mark &optional prefix)
  "Select the menu item on the line of $mark."
  (let ((line (mark-line mark)))
    (let* ((buffer (line-buffer line))
	   (item (nth (1- (count-lines
			   (region (buffer-start-mark buffer)
				   mark)))
		      (variable-value 'menu :buffer buffer))))
      (and (car item)
	   (plusp (length (car item)))
	   (call-command (car item)
			 (getstring (car item) *command-names*)
			 prefix)))))

(defun setup-menu-buffer (buffer)
  (defevar "Menu"
    "The menu shown in this buffer."
    :buffer buffer
    :value (value menu))
  (delete-region (buffer-region buffer))
  (let ((mark (copy-mark (buffer-point buffer))))
    (let ((longest 0))
      (loop for item in (value menu) do
	(if (> (length (car item)) longest)
	    (setq longest (length (car item)))))
      (incf longest 3)
      ;; Insert the menu.
      (loop for item in (value menu) do
	(insert-string mark (car item))
	(when (cadr item)
	  (loop repeat (- longest (length (car item)))
	    do (insert-character mark #\space))
	  (let ((command (if (consp (cadr item))
			     (apply #'format () (caadr item)
				    (mapcar #'eval (cdadr item)))
			     (cadr item))))
	    (insert-string mark command)
	    (setf (getf (line-plist (mark-line mark))
			'primary-click-hook)
		  #'select-menu-item))
#| FIX bind key
	  (ed::msg "ca ~A, caddr ~A" (car item) (caddr item))
	  (and (car item)
	       (plusp (length (car item)))
	       (caddr item)
	       (progn
		 (ed::msg "bind ~A to ~A" (car item) (caddr item))
		 (bind-key (car item) (caddr item) :buffer buffer)))
|#
	  )
	(insert-character mark #\newline))
      (buffer-start (buffer-point buffer))
      (setf (buffer-modified buffer) ())
      (setf (buffer-writable buffer) ()))))

(defmode "Menu" :major-p t :setup-function #'setup-menu-buffer)

(defvar *menu-buffer* ()
  "The main menu buffer.")

(defun cleanup-menu (buffer)
  "Clean up after the menu buffer is deleted."
  (declare (ignore buffer))
  (setq *menu-buffer* ()))

(defcommand "Menu" ()
  "Switch to the menu buffer, creating it if necessary."
  (or *menu-buffer*
      (setq *menu-buffer*
	    (make-unique-buffer "Menu"
				:modes '("Menu")
				:delete-hook (list #'cleanup-menu))))
  (change-to-buffer *menu-buffer*))

(defcommand "Select Menu Item" (p)
  "Select the item under point."
  (if (blank-line-p (current-line))
      (editor-error "Point must be on a menu item."))
  (select-menu-item (current-point) p))

(defcommand "Exit Menu" ()
  "Exit the menu in the current buffer."
  (or (editor-bound-p 'menu :buffer (current-buffer))
      (editor-error "Point must be in a menu buffer."))
  (rotate-buffers-forward-command))


;;;; Initialization.

(after-editor-initializations
 (if (value show-menu-on-start) (menu-command)))
