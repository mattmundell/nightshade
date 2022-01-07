;;; Menu.

(in-package "ED")

#[ Menu

The `Menu' command brings up the main menu of items.  Each item can be
selected to perform some action.

{command:Menu}

The [Applications] submenu of the main menu provides access to
applications such as a mail reader and a diary.
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
	   ("/"         "Edit the root of the file system."
	    "/")
	   ("Desktop"   "Edit the Desktop.")
	   ("")
	   ("Help"      "Choose a help option.")
	   ;; FIX find sounds like search
	   ("Find"      "Open a file or directory for editing.")
	   ("Info"      "Read documentation.")
	   ("Tutorial"  "Do the editor tutorial.")
	   ("Packdired" "Browse installed packages.")
	   ("Manage Packages" "Enter the package manager.")
; 	   ("Config" "Configure the system."
 	   ("Menu"      "Mail, diary, IRC, News..." "Applications"
	    (("Browse Folders" "Browse email folders.")
	     ("Calendar"       "Bring up the calendar.")
	     ("Diary"          "Show today's diary entries.")
	     ("Dired"          "Manage files.")
	     ("Edit System Processes"
	                       "Browse processes of underlying OS.")
	     ("FTP"
	                       "Start a File Tranfer Protocol sesson.")
	     ("Goto Today in Diary"
	                       "Switch to the diary file.")
	     ("Gopher"         "Browse Gopher sites.")
	     ("Ginfo"          "Browse the GNU Info system.")
	     ("Incorporate and Read New Mail"
	                       "Fetch and read email.")
	     ("IRC"            "Instant chat.")
	     ("Locate"         "Find a file on the file system.")
	     ("Netnews"        "Read Network News (NNTP).")
	     ("Shell"          "Start a shell on the underlying OS.")
	     ("Telnet"         "Start a remote shell via Telnet.")
	     ("WWW"            "Browse the World Wide Web.")))
; 	   ("recent files"     "Browse the `Find' history.")
	   ("Bufed"            "Browse the list of buffers.")
	   ("Browse Kill Ring" "Browse the history of kills.")
	   ("Evented"
	    "Edit the list of scheduled events (reminders, etc).")
	   ("")
	   ("Editor Evaluate Expression"
	    "Evaluate an expression in the context of the editor.")
	   ("Select Slave"     "Switch to a slave Lisp.")
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

(defun select-menu-item (mark &optional prefix)
  "Select the menu item on the line of $mark."
  (let ((line (mark-line mark)))
    (let* ((buffer (line-buffer line))
	   (item (nth (1- (count-lines
			   (region (buffer-start-mark buffer)
				   mark)))
		      (variable-value 'menu :buffer buffer)))
	   (name (car item)))
      (and name
	   (plusp (length name))
	   (if (string= name "Menu")
	       (menu-command () (cadddr item) (caddr item))
	       (call-command name
			     (getstring name *command-names*)
			     prefix))))))

(defun setup-menu-buffer (buffer menu)
  (defevar "Menu"
    "The menu shown in this buffer."
    :buffer buffer
    :value menu)
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (let ((mark (copy-mark (buffer-point buffer))))
      (let ((longest 0))
	(dolist (item menu)
	  (if (> (length (car item)) longest)
	      (setq longest (length (car item)))))
	;; Insert the menu.
	(dolist (item menu)
	  (let* ((menu-p (string= (car item) "Menu"))
		 (name (if menu-p
			   (caddr item)
			   (car item))))
	    (insert-string mark name)
	    (when (cadr item)
	      (loop repeat (- longest (length name))
		do (insert-character mark #\space))
	      (insert-character mark #\space)
	      (insert-character mark (if menu-p #\> #\space))
	      (insert-character mark #\space)
	      (insert-character mark #\space)
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
	    (insert-character mark #\newline)))
	(buffer-start (buffer-point buffer))))))

(defmode "Menu" :major-p t)

(defcommand "Menu" (p (menu (value menu)) name)
  "Switch to the menu buffer, creating it if necessary."
  (declare (ignore p))
  (let* ((buffer-name (if name (format () "Menu ~A" name) "Menu"))
	 (buffer (getstring buffer-name *buffer-names*)))
    (if (and buffer
	     (string= (buffer-major-mode buffer) "Menu")
	     ;; FIX and check if ed var menu eq
	     )
	(change-to-buffer buffer)
	(let ((buffer (make-unique-buffer buffer-name
					  :modes '("Menu"))))
	  (setup-menu-buffer buffer menu)
	  (change-to-buffer buffer)))))

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
