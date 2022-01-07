;;; File groups.
;;;
;;;    The "Compile Group" and "List Compile Group" commands in
;;;    lispeval.lisp also know about groups.
;;;
;;; This file provides commands for manipulating groups of files that make
;;; up a larger system.  A file group is a set of files whose names are
;;; listed in some other file.  At any given time one group of files is the
;;; Active group.  The Select Group command makes a group the Active group,
;;; prompting for the name of a definition file if the group has not been
;;; selected before.  Once a group has been selected once, the name of the
;;; definition file associated with that group is retained.  If one wishes
;;; to change the name of the definition file after a group has been
;;; selected, one should call Select Group with a prefix argument.

(in-package "ED")

(export '(*active-file-group* do-active-group))


#[ File Groups

A file group is a set of files, upon which various editing operations can be
performed.  The files in a group are specified by a file in the following
format:

  - Any line which begins with one "@" is ignored.

  - Any line which does not begin with an "@" is the name of a file in the
    group.

  - A line which begins with "@@" specifies another file having this
    syntax, which is recursively examined to find more files in the group.

This syntax is used for historical reasons.  Although any number of file
groups may be read into the editor, there is only one active group, which
is the file group implicitly used by all of the file group commands.
[Compiling Files] describes the `Compile Group' command.

{command:Select Group}
{command:Group Query Replace}
{command:Group Replace}
{command:Group Search}

These commands attempt to save each file processed before going on to the
next one in the group.

{evariable:Group Find File}
{evariable:Group Save File Confirm}
]#

#[ File Groups (extension)

File groups provide a simple way of collecting the files that compose a
system and naming that collection.  The editor supports commands for
searching, replacing, and compiling groups.

{variable:ed:*active-file-group*}
{function:ed:do-active-group}
]#


(defvar *file-groups* (make-string-table)
  "A string table of file groups.")

(defvar *active-file-group* ()
  "The list of files in the currently active group.")

(defvar *active-file-group-name* ()
  "The name of the currently active group.")

(defun current-group ()
  *active-file-group-name*)


;;;; Selecting the active group.

#| FIX
     This command prompts for the name of a file group to make the active group.
     If the name entered is not the name of a group whose definition has been
     read, then the user is prompted for the name of a file to read the group
     definition from.  The name of the default pathname is the name of the
     group, and the type is "upd".
|#

(defcommand "Select Group" (p group-name pathname)
  "Make a prompted group or wildcard pathname the active group.  If the
   group still needs to be defined then prompt for the name of the defining
   file.  With a prefix argument, change the definition file associated
   with the group or pathname."
  (let* ((group-name
	  (or group-name
	      (prompt-for-keyword
	       (list *file-groups*)
	       :must-exist ()
	       :prompt "Select Group: "
	       :help
	       "Type the name of the file group you wish to become the active group.")))
	 (old (getstring group-name *file-groups*))
	 (pathname
	  (fi (wild-pathname-p group-name)
	      (or pathname
		  (if (and old (not p))
		      old
		      (prompt-for-file :must-exist t
				       :prompt "From File: "
				       :default (merge-pathnames
						 (make-pathname
						  :name group-name
						  :type "upd")
						 (value pathname-defaults))))))))
    (setq *active-file-group-name* group-name)
    (setq *active-file-group*
	  (if pathname
	      (nreverse (read-file-group pathname ()))
	      group-name))
    (setf (getstring group-name *file-groups*) pathname)))

;;; READ-FILE-GROUP reads an Update format file and returns a list of pathnames
;;; of the files named in that file.  This guy knows about @@ indirection and
;;; ignores empty lines and lines that begin with @ but not @@.  A simpler
;;; scheme could be used for non-Spice implementations, but all this hair is
;;; probably useful, so Update format may as well be a standard for this sort
;;; of thing.
;;;
(defun read-file-group (pathname tail)
  (with-open-file (file pathname)
    (do* ((name (read-line file nil nil) (read-line file nil nil))
	  (length (if name (length name)) (if name (length name))))
	 ((null name) tail)
      (declare (type (or simple-string null) name))
      (cond ((zerop length))
	    ((char= (char name 0) #\@)
	     (when (and (> length 1) (char= (char name 1) #\@))
	       (setq tail (read-file-group
			   (merge-pathnames (subseq name 2)
					    pathname)
			   tail))))
	    (t
	     (push (merge-pathnames (pathname name) pathname) tail))))))


;;;; DO-ACTIVE-GROUP.

(defevar "Group Find File"
  "The group searching and replacing commands read each file into its own
   buffer using `Find File'.  Since this may result in large amounts of
   memory being consumed by extra buffers, this variable controls whether
   to delete the buffer after processing it.  When this variable is false
   the commands delete the buffer if they created the buffer; however, in
   all cases, if the user leaves the buffer modified, the commands leave
   the buffer intact.")

(defevar "Group Save File Confirm"
  "If true, the group searching and replacing commands ask for confirmation
   before saving any modified file."
  :value t)

(defmacro do-active-group ((file &optional recurse backups literally)
			   &rest forms)
  "Iterate over *active-file-group*, executing $forms once for each file
   with $file bound to the file.  While the forms are executing, buffer the
   file as the current buffer with the point at the beginning of the
   buffer.  If the active group is (), signal an editor-error.

   Read each file into its own buffer using find-file-buffer.  Since extra
   buffers may consume large amounts of memory, *Group Find File* controls
   whether to delete the buffer after executing the forms.  When the
   variable is false, delete the buffer if it was created; however,
   regardless of this variable, if the user leaves the buffer modified, the
   buffer persists after the forms have completed.  Always save the
   location of the buffer's point before processes a buffer that already
   existed, and restore it afterwards.

   After processing a buffer, if it is modified, try to save it.  If *Group
   Save File Confirm* is true, ask for confirmation."
  (let ((n-start-buf (gensym))
	(n-save (gensym))
	(unique-part (gensym))
	(wild-part (gensym)))
    `(progn
       (or *active-file-group* (select-group-command))

       (let ((,n-start-buf (current-buffer)))
	 (unwind-protect
	     (if (consp *active-file-group*)
		 (dolist (,file *active-file-group*)
		   (catch 'file-not-found
		     (group-with-file (,file ,literally)
		       (with-mark ((,n-save (current-point) :right-inserting))
			 (unwind-protect
			     (progn
			       (buffer-start (current-point))
			       ,@forms)
			   (move-mark (current-point) ,n-save)))
		       (group-save-file))))
		 (multiple-value-bind (,unique-part ,wild-part)
				      (common-prefix *active-file-group*)
		   (let ((,unique-part (truename ,unique-part)))
		     (in-directory ,unique-part
		       (do-files (,file ,wild-part
					:backups ,backups
					:recurse ,recurse)
			 (catch 'file-not-found
			   (or (directoryp (merge-pathnames ,file ,unique-part))
			       (group-with-file ((merge-pathnames ,file
								  ,unique-part)
						 ,literally)
				 (with-mark ((,n-save (current-point) :right-inserting))
				   (unwind-protect
				       (progn
					 (buffer-start (current-point))
					 ,@forms)
				     (move-mark (current-point) ,n-save)))
				 (group-save-file)))))))))
	   (if (member ,n-start-buf *buffer-list*)
	       (setf (current-buffer) ,n-start-buf
		     (window-buffer (current-window)) ,n-start-buf)
	       (editor-error "Original buffer deleted.")))))))

(defmacro old-do-active-group ((file &optional recurse backups literally)
			   &rest forms)
  "Iterate over `*active-file-group*', executing $forms once for each file
   with $file bound to the file.  While the forms are executing, buffer the
   file as the current buffer with the point at the beginning of the
   buffer.  If the active group is (), signal an editor-error.

   Read each file into its own buffer using find-file-buffer.  Since
   unwanted buffers may consume large amounts of memory, `Group Find File'
   controls whether to delete the buffer after executing the forms.  When
   the variable is false, delete the buffer if it did not previously exist;
   however, regardless of this variable, if the user leaves the buffer
   modified, the buffer persists after the forms have completed.  Always
   save the location of the buffer's point before processes a buffer that
   already existed, and restores it afterwards.

   After processing a buffer, if it is modified, try to save it.  If `Group
   Save File Confirm' is true, ask for confirmation."
  (let ((n-buf (gensym))
	(n-start-buf (gensym))
	(n-save (gensym))
	(unique-part (gensym))
	(wild-part (gensym)))
    `(progn
       (or *active-file-group* (select-group-command))

       (let ((,n-start-buf (current-buffer))
	     ,n-buf)
	 (unwind-protect
	     (if (consp *active-file-group*)
		 (dolist (,file *active-file-group*)
		   (catch 'file-not-found
		     (setq ,n-buf (group-read-file ,file ,n-buf ,literally))
		     (with-mark ((,n-save (current-point) :right-inserting))
		       (unwind-protect
			   (progn
			     (buffer-start (current-point))
			     ,@forms)
			 (move-mark (current-point) ,n-save)))
		     (group-save-file)))
		 (multiple-value-bind (,unique-part ,wild-part)
				      (common-prefix *active-file-group*)
		   (let ((,unique-part (truename ,unique-part)))
		     (in-directory ,unique-part
		       (do-files (,file ,wild-part
					:backups ,backups
					:recurse ,recurse)
			 (catch 'file-not-found
			   (or (directoryp (merge-pathnames ,file ,unique-part))
			       (progn
				 (setq ,n-buf (group-read-file
					       (merge-pathnames ,file
								,unique-part)
					       ,n-buf
					       ,literally))
				 (with-mark ((,n-save (current-point) :right-inserting))
				   (unwind-protect
				       (progn
					 (buffer-start (current-point))
					 ,@forms)
				     (move-mark (current-point) ,n-save)))
				 (group-save-file)))))))))
	   (if (member ,n-start-buf *buffer-list*)
	       (setf (current-buffer) ,n-start-buf
		     (window-buffer (current-window)) ,n-start-buf)
	       (editor-error "Original buffer deleted.")))))))

;;; GROUP-READ-FILE reads in files for the group commands via DO-ACTIVE-GROUP.
;;; We use FIND-FILE-BUFFER, which creates a new buffer when the file hasn't
;;; already been read, to get files in, and then we delete the buffer if it is
;;; newly created and "Group Find File" is false.  This lets FIND-FILE-BUFFER
;;; do all the work.  We don't actually use the "Find File" command, so the
;;; buffer history isn't affected.
;;;
;;; Search-Buffer is any temporary search buffer left over from the last file
;;; that we want deleted.  We don't do the deletion if the buffer is modified.
;;;
(defun group-read-file (name search-buffer &optional literally)
  ;(ed::msg "cd ~A (lit ~A)" (current-directory) literally)
  ;(ed::msg "name ~A" name)
  (unless (probe-file name)
    (message "Failed to find ~A." name)
    (throw 'file-not-found ()))
  ;(ed::msg "Find ~A (~A)." name search-buffer)
  (multiple-value-bind (buffer created-p)
		       (find-file-buffer name literally)
    (setf (current-buffer) buffer)
    (setf (window-buffer (current-window)) buffer)

    (if search-buffer
	(or (buffer-modified search-buffer)
	    (eq search-buffer (current-buffer))
	    (progn
	      (dolist (w (buffer-windows search-buffer))
		(setf (window-buffer w) (current-buffer)))
	      (delete-buffer search-buffer))))

    (if created-p
	(fi (value group-find-file) (current-buffer)))))

(defmacro group-with-file ((name &optional literally) &body body)
  (let ((buffer (gensym)) (created-p (gensym))
	(window (gensym)) (previous-buffer (gensym))
	(previous-directory (gensym)))
    `(progn
       ;(ed::msg "cd ~A (lit ~A)" (current-directory) ,literally)
       ;(ed::msg "name ~A" ,name)
       (unless (probe-file ,name)
	 (message "Failed to find ~A." ,name)
	 (throw 'file-not-found ()))
       (let ((,previous-directory (current-directory))
	     (,previous-buffer (current-buffer)))
	 (multiple-value-bind (,buffer ,created-p)
			      (find-file-buffer ,name ,literally)
	   (unwind-protect
	       (progn
		 (setf (current-buffer) ,buffer)
		 (setf (window-buffer (current-window)) ,buffer)
		 ,@body)
	     (setf (window-buffer (current-window)) ,previous-buffer)
	     (setf (current-buffer) ,previous-buffer)
	     (setf (current-directory) ,previous-directory)
	     (if ,created-p
		 (or (buffer-modified ,buffer)
		     (equal ,previous-buffer ,buffer)
		     (progn
		       (dolist (,window (buffer-windows ,buffer))
			 (setf (window-buffer ,window) ,previous-buffer))
		       (delete-buffer ,buffer))))))))))

;;; GROUP-SAVE-FILE is used by DO-ACTIVE-GROUP.
;;;
(defun group-save-file ()
  (let* ((buffer (current-buffer))
	 (pn (buffer-pathname buffer))
	 (name (namestring pn)))
    (when (and (buffer-modified buffer)
	       (or (not (value group-save-file-confirm))
		   (prompt-for-y-or-n
		    :prompt (list "Save changes in ~A? " name)
		    :default t)))
      (save-file-command))))


;;;; Searching and Replacing commands.

(defcommand "Group Search" ()
  "Search each file in the active group for a prompted string.  Read in
   each file as if by `Find File'.  On finding an occurrence, prompt for a
   key-event indicating what action to take.  The following commands are
   defined:

     Escape, Space, y
	 Exit `Group Search'.

     Delete, Backspace, n
	 Continue searching for the next occurrence of the string.

     !
	 Continue the search at the beginning of the next file, skipping the
	 remainder of the current file.

     C-r
	 Go into a recursive edit at the current location, and continue the
	 search when it is exited."
  (let ((string (prompt-for-string :prompt "Group Search: "
				   :help "String to search for in active file group"
				   :default *last-search-string*)))
    (get-search-pattern string :forward)
    (do-active-group (file)
      (do ((won (find-pattern (current-point) *last-search-pattern*)
		(find-pattern (current-point) *last-search-pattern*)))
	  ((not won))
	(character-offset (current-point) won)
	(command-case
	    (:prompt "Group Search: "
		     :help "Type a character indicating the action to perform."
		     :change-window nil)
	  (:no "Search for the next occurrence.")
	  (:do-all "Go on to the next file in the group."
		   (return nil))
	  ((:exit :yes) "Exit the search."
	   (return-from group-search-command))
	  (:recursive-edit "Enter a recursive edit."
			   (do-recursive-edit)
			   (get-search-pattern string :forward)))))
    (message "All files in group ~S searched." *active-file-group-name*)))

(defmacro with-group ((pathname &optional name) &body body)
  "Execute $body with $group as the current group."
  (let ((old-pathname (gensym)) (name-or-path (gensym)) (new-pathname (gensym)))
    `(let* ((,new-pathname (merge-pathnames ,pathname #p"*.*.*"))
	    (,name-or-path (or ,name (namestring ,new-pathname)))
	    (*active-file-group-name* ,name-or-path)
	    (*active-file-group* ,new-pathname)
	    (,old-pathname (getstring ,name *file-groups*)))
       (unwind-protect
	   (progn
	     (setf (getstring ,name-or-path *file-groups*) ,new-pathname)
	     ,@body)
	 (setf (getstring ,name-or-path *file-groups*) ,old-pathname)))))

(defun search-group (string &optional recurse backups)
  "Search each file in the active group for $string.  Read in each file as
   if by `Find File'.  Return a list of matches, where each match is a
   list: file name, matching line number and text of matching line."
  (get-search-pattern string :forward)
  (collect ((matches))
    (do-active-group (file recurse backups t)
      (do ((won (find-pattern (current-point) *last-search-pattern*)
		(find-pattern (current-point) *last-search-pattern*)))
	  ((fi won))
       (let ((point (current-point))
	     (buffer (current-buffer)))
	 (character-offset point won)
	 (matches (list file
			(string (count-lines (region (buffer-start-mark buffer)
						     point)))
			(line-string (current-line)))))))
    (matches)))

(defcommand "Group Replace" ()
  "Execute a string replace on each file in the active group using prompted
   target and replacement strings."
  (let* ((target (prompt-for-string :prompt "Group Replace: "
				    :help "Target string"
				    :default *last-search-string*))
	 (replacement (prompt-for-string :prompt "With: "
					 :help "Replacement string")))
    (do-active-group (file)
      (query-replace-function () target replacement
			      "Group Replace on previous file" t))
    (message "Replacement done in all files in group ~S."
	     *active-file-group-name*)))

(defcommand "Group Query Replace" ()
  "Execute an interactive string replace on each file in the active group
   using prompted target and replacement strings.  Read in each file as if
   `Find File' were used and process as if by `Query Replace'."
  (let ((target (prompt-for-string :prompt "Group Query Replace: "
				   :help "Target string"
				   :default *last-search-string*)))
    (let ((replacement (prompt-for-string :prompt "With: "
					  :help "Replacement string")))
      (do-active-group (file)
        (or (query-replace-function
	     () target replacement "Group Query Replace on previous file")
	    (return nil)))
      (message "Replacement done in all files in group ~S."
	       *active-file-group-name*))))
