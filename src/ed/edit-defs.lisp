;;; Editing DEFMACRO and DEFUN definitions.  Also, has directory
;;; translation code for moved and/or different sources.

(in-package "ED")

(export '(add-definition-dir-translation delete-definition-dir-translation))

#[ Editing Definitions

The Lisp compiler annotates each compiled function object with the source
file from which the function was originally defined.  The definition
editing commands use this information to locate and edit the source for
functions defined in the environment.  The defining file is recorded as an
absolute pathname.

{command:Edit Definition}
{command:Goto Definition}
{command:Edit Command Definition}
The definition editing commands have a directory translation mechanism that
allow the sources to be found when they are not in the location where
compilation was originally done.
{command:Add Definition Directory Translation}
{command:Delete Definition Directory Translation}
{evariable:Editor Definition Info}
]#

#[ Definition Editing

The editor provides commands for finding the definition of a function,
macro, or command and placing the user at the definition in a buffer.  If
the editor cannot get at the information, then these commands do not work.
If the Lisp system does not store an absolute pathname, independent of the
machine on which the maintainer built the system, then users need a way of
translating a source pathname to one that will be able to locate the
source.

{function:ed:add-definition-dir-translation}
{function:ed:delete-definition-dir-translation}
]#

;;; Directory translation for definition editing commands.

(defvar *definition-directory-translation-table*
  (make-string-table)
  "Editor string table for translating directory namestrings to other ones,
   so a function defined in /x/y/z/.../file.ext will actually be looked for
   in /whatever/.../file.ext.")

(defun add-definition-dir-translation (dir1 dir2)
  "Map directory pathname $dir1 to $dir2.

   This is used in commands like `Edit Definition'.

   On a successive invocation using the same $dir1 push into a translation
   list.

   When [FIX] the editor seeks a definition source file, and it has a
   translation, then it tries the translations in order.  This is useful
   for sources that are on various machines, some of which may be down.
   When the editor tries to find a translation, it first looks for
   translations of longer directory pathnames, finding more specific
   translations before shorter, more general ones."
  (push (pathname dir2)
	(getstring (directory-namestring dir1)
		   *definition-directory-translation-table*)))

(defun delete-definition-dir-translation (directory)
  "Delete the mapping of $directory to all other directories for definition
   editing commands."
  (delete-string (directory-namestring directory)
		 *definition-directory-translation-table*))

(defcommand "Add Definition Directory Translation" ()
  "Map a prompted directory namestring to a second prompted namestring for
   definition editing commands.  Longer (more specific) directory
   specifications are matched before shorter (more general) ones.
   Successive uses of this push into a list of translations that will be
   tried in order of traversing the list."
  (let* ((dir1 (prompt-for-file :prompt "Preimage dir1: "
				:help "directory namestring."
				:must-exist nil))
	 (dir2 (prompt-for-file :prompt "Postimage dir2: "
				:help "directory namestring."
				:must-exist nil)))
    (add-definition-dir-translation dir1 dir2)))

(defcommand "Delete Definition Directory Translation" ()
  "Delete a prompted directory namestring from the directory translation
   table for the definition editing commands."
  (delete-definition-dir-translation
   (prompt-for-file :prompt "Directory: "
		    :help "directory namestring."
		    :must-exist nil)))


;;;; Definition Editing Commands.

;;; These commands use a slave Lisp to determine the file the function is
;;; defined in.  They do a synchronous evaluation of DEFIITION-EDITING-INFO.
;;; Then, in the editor Lisp, GO-TO-DEFINITION possibly translates the file
;;; name, finds the file, and tries to search for the defining form.


;;; For the "Go to Definition" search pattern, we just use " " as the initial
;;; pattern, so we can make a search pattern.  Invocation of the command alters
;;; the search pattern.

(defvar *go-to-def-pattern*
  (new-search-pattern :string-insensitive :forward " "))

(defvar *last-go-to-def-string* "")
(proclaim '(simple-string *last-go-to-def-string*))

;;; GET-DEFINITION-PATTERN takes a type and a name.  It returns a search
;;; pattern for finding the defining form for name using
;;; *go-to-def-pattern* and *last-go-to-def-string* destructively.  The
;;; pattern contains a trailing space to avoid finding functions earlier
;;; in the file with the function's name as a prefix.  This is not necessary
;;; with type :command since the name is terminated with a ".
;;;
;;; FIX (defun kernel:current-sp () (kernel:current-sp))
;;;
(defun get-definition-pattern (type name)
  (declare (simple-string name))
  (let ((string (ecase type
		  ((:function :unknown-function)
		   (concatenate 'simple-string "(defun " name " "))
		  ((:macro :unknown-macro)
		   (concatenate 'simple-string "(defmacro " name " "))
		  (:command
		   (concatenate 'simple-string
				"(defcommand \""
				(nsubstitute #\space #\-
					     (subseq name 0 (- (length name) 8))
					     :test #'char=)
				"\""))
		  (:editor-variable
		   (concatenate 'simple-string "(defevar \"" name "\"")))))
    (declare (simple-string string))
    (cond ((string= string *last-go-to-def-string*)
	   *go-to-def-pattern*)
	  (t (setf *last-go-to-def-string* string)
	     (new-search-pattern :string-insensitive :forward
				 string *go-to-def-pattern*)))))

(defevar "Editor Definition Info"
  "When true, the editor Lisp is used to determine definition editing
   information; otherwise, the current eval server is used.  This variable
   is true in `Eval' and `Editor' modes."
  :value ())

(defcommand "Goto Definition" ()
  "Edit the definition of the symbol at the beginning of the current list.

   If the symbol is defined as a `defun' or `defmacro' form then go to the
   definition, otherwise simply find the defining file.  If the function is
   interpreted, then look for it in the current buffer."
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((mark1 point)
		(mark2 point))
      (or (backward-up-list mark1)
	  (editor-error "Point must be inside a list."))
      (form-offset (move-mark mark2 (mark-after mark1)) 1)
      (let ((fun-name (region-to-string (region mark1 mark2))))
	(get-def-info-and-go-to-it fun-name)))))

(defcommand "Edit Definition" ()
  "Prompt for a function/macro and go to it for editing.

   Use the current eval server to find out in which file the function is
   defined.  If something other than defun or defmacro defined the
   function, then simply read in the file, otherwise try to find its
   definition point within the file.  If the function is uncompiled, then
   look for it in the current buffer.  Use a valid eval server if there is
   one, otherwise use the editor Lisp's environment."
  (let ((fun-name (prompt-for-string
		   :prompt "Name: "
		   :help "Symbol name of function."
		   :default (name-at-point))))
    (get-def-info-and-go-to-it fun-name)))

(defun get-def-info-and-go-to-it (fun-name)
  (let ((in-editor-p (value editor-definition-info))
	(info (value current-eval-server)))
    (if (fi info t in-editor-p)
	(multiple-value-bind (pathname type name)
			     (in-lisp
			      (definition-editing-info fun-name))
	  (or in-editor-p
	      (message "Editing definition from editor Lisp ..."))
	  (go-to-definition pathname type name))
	(let ((results (eval-form-in-server
			info
			(format () "(ed::definition-editing-info ~S)"
				fun-name))))
	  (go-to-definition (read-from-string (first results)) ; file
			    (read-from-string (second results)) ; type
			    (read-from-string (third results))))))) ; name

(defun find-variable-source (name)
  "Find the file in ed: that defines editor variable $name."
  ; FIX exit search at first match
  (car (search-files "ed:" (concat "(defevar \"" name "\""))))

(defcommand "Edit Editor Variable Definition" (p &optional name)
  "Prompt for an editor variable and go to it for editing.

   Use the current eval server to find out in which file the variable is
   defined.  Use a valid eval server if there is one, otherwise use the
   editor Lisp's environment."
  (declare (ignore p))
  (multiple-value-bind (name var)
		       (if name
			   (values name ())
			   (prompt-for-variable
			    :help "Name of variable to edit."
			    :prompt "Editor Variable: "))
    (declare (ignore var))
    ;; FIX
    (message "Searching for variable definition...")
    (let ((source (find-variable-source name)))
      (if source ; (variable-source var)
	(go-to-definition (merge-pathnames source "ed:") ; (variable-source var) ; file
			  :editor-variable ; type
			  name)
	(editor-error "Variable source is love.")))))

;;; "Edit Command Definition" is a hack due to creeping evolution in
;;; GO-TO-DEFINITION.  We specify :function type and a name with "-COMMAND"
;;; instead of :command type and the real command name because this causes
;;; the right pattern to be created for searching.  We could either specify
;;; that you always edit command definitions with this command (breaking
;;; "Go to Definition" for commands called as functions), fixing the code,
;;; or we can hack this command so everything works.
;;;
(defcommand "Edit Command Definition" (p)
  "Edit the definition of a prompted editor command.  If a prefix argument
   is specified, then prompt for a key and edit the definition of the
   command bound to that key."
  (multiple-value-bind
      (name command)
      (if p
	  (multiple-value-bind (key cmd)
			       (prompt-for-key :prompt "Edit command bound to: ")
	    (declare (ignore key))
	    (values (command-name cmd) cmd))
	  (prompt-for-keyword (list *command-names*)
			      :prompt "Command to edit: "
			      :history *extended-command-history*
			      :history-pointer
			      '*extended-command-history-pointer*))
    (go-to-definition (fun-defined-from-pathname (command-function command))
		      :function
		      (concatenate 'simple-string name "-COMMAND"))))

;;; GO-TO-DEFINITION tries to find name in file with a search pattern based
;;; on type (defun or defmacro).  File may be translated to another source
;;; file, and if type is a function that cannot be found, we try to find a
;;; command by an appropriate name.
;;;
(defun go-to-definition (file type name)
  (let ((pattern (get-definition-pattern type name)))
    (cond
     (file
      (setf file (go-to-definition-file file))
      (let* ((buffer (find-file-command nil file))
	     (point (buffer-point buffer))
	     (name-len (length name)))
	(declare (fixnum name-len))
	(with-mark ((def-mark point))
	  (buffer-start def-mark)
	  (unless (find-pattern def-mark pattern)
	    (if (and (or (eq type :function) (eq type :unknown-function))
		     (> name-len 7)
		     (string= name "COMMAND" :start1 (- name-len 7)))
		(let ((prev-search-str *last-go-to-def-string*))
		  (unless (find-pattern def-mark
					(get-definition-pattern :command name))
		    (editor-error "~A is not defined with ~S or ~S, ~
				   but this is the defined-in file."
				  (string-upcase name) prev-search-str
				  *last-go-to-def-string*)))
		(editor-error "~A is not defined with ~S, ~
			       but this is the defined-in file."
			      (string-upcase name) *last-go-to-def-string*)))
	  (if (eq buffer (current-buffer))
	      (push-buffer-mark (copy-mark point)))
	  (move-mark point def-mark))))
     (t
      (when (or (eq type :unknown-function) (eq type :unknown-macro))
	(with-mark ((m (buffer-start-mark (current-buffer))))
	  (or (find-pattern m pattern)
	      (editor-error
	       "~A is not compiled and not defined in current buffer with ~S"
	       (string-upcase name) *last-go-to-def-string*))
	  (let ((point (current-point)))
	    (push-buffer-mark (copy-mark point))
	    (move-mark point m))))))))

;;; GO-TO-DEFINITION-FILE takes a pathname and translates it to another
;;; according to "Add Definition Directory Translation".  Take the first
;;; probe-able translation, or probe file if no translations are found.
;;; If no existing file is found, an editor error is signaled.
;;;
(defun go-to-definition-file (file)
  (multiple-value-bind (unmatched-dir new-dirs file-name)
		       (maybe-translate-definition-file file)
    (loop
      (when (null new-dirs)
	(unless (probe-file file)
	  (if unmatched-dir
	      (editor-error "Cannot find file ~S or any of its translations."
			    file)
	      (editor-error "Cannot find file ~S." file)))
	(return file))
      (let ((f (translate-definition-file unmatched-dir (pop new-dirs)
					  file-name)))
	(when (probe-file f)
	  (setf file f)
	  (return f))))))

;;; MAYBE-TRANSLATE-DEFINITION-FILE tries each directory subsequence from
;;; the most specific to the least looking a user defined translation.
;;; This returns the portion of the input directory sequence that was not
;;; matched (to be merged with the mapping of the matched portion), the
;;; list of post image directories, and the file name.
;;;
(defun maybe-translate-definition-file (file)
  (let* ((pathname (pathname file))
	 (maybe-truename (or (probe-file pathname) pathname))
	 (dirs (pathname-directory maybe-truename))
	 (len (length dirs))
	 (i len))
    (declare (fixnum len i))
    (loop
      (when (<= i 1) (return nil))
      (let ((new-dirs (getstring (directory-namestring
				 (make-pathname :defaults "/"
						:directory (subseq dirs 0 i)))
				*definition-directory-translation-table*)))
	(when new-dirs
	  (return (values (subseq dirs i len) new-dirs
			  (file-namestring maybe-truename)))))
      (decf i))))

;;; TRANSLATE-DEFINITION-FILE creates a directory sequence from unmatched-dir
;;; and new-dir, creating a translated pathname for GO-TO-DEFINITION.
;;;
(defun translate-definition-file (unmatched-dir new-dir file-name)
  (make-pathname :defaults "/"
		 :directory (append (pathname-directory new-dir)
				    unmatched-dir)
		 :name file-name))

(defun definition-editing-info (string)
 "Return the pathname from which the global definition of the symbol named
  $string was defined."
  (let ((symbol (read-from-string string)))
    (check-type symbol symbol)
    (let ((macro (macro-function symbol))
	  (name (symbol-name symbol)))
      (if macro
	  (let ((file (fun-defined-from-pathname macro)))
	    (if file
		(values file :macro name)
		(values nil :unknown-macro name)))
	  (if (fboundp symbol)
	      (let ((file (fun-defined-from-pathname symbol)))
		(if file
		    (values file :function name)
		    (values nil :unknown-function name)))
	      (error "~S is not a function." symbol))))))
