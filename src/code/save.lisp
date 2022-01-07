;;; Dump the current lisp image into a core file.  All the real work is
;;; done be C.  Also contains various high-level initialization stuff:
;;; loading init files and parsing environment variables.

(in-package "LISP")

(in-package "EXTENSIONS")
(export '(print-herald *herald-items* save-lisp *before-save-initializations*
	  *after-save-initializations* *environment-list* *editor-lisp-p*))

(in-package "LISP")


#[ Saving a Core Image

A mechanism exists to save a running Lisp core image and later restore it.
This is more convenient than loading several files into a Lisp on startup.
The main problem is the large size of saved Lisp images, typically at least
20 megabytes.

{function:ext:save-lisp}

To resume a saved file, type:

    lisp -core file

{function:purify}
]#


(defvar *before-save-initializations* nil
  "This is a list of functions which are called before creating a saved
   core image.  These functions are executed in the child process which has
   no ports, so they cannot do anything that tries to talk to the outside
   world.")

(defvar *after-save-initializations* nil
  "This is a list of functions which are called when a saved core image
   starts up.  The system itself should be initialized at this point, but
   applications might not be.")

(defvar *environment-list* nil
  "An alist mapping environment variables (as keywords) to either values")

(defvar *editor-lisp-p* nil
  "This is true if and only if the lisp was started with the -edit switch.")

;;; Filled in by the startup code.
(defvar lisp-environment-list)

;;; PARSE-UNIX-SEARCH-LIST  --  Internal
;;;
;;; Return a list of the directories that are in the specified Unix
;;; environment variable.  Return NIL if the variable is undefined.
;;;
(defun parse-unix-search-list (var)
  (let ((path (cdr (assoc var ext::*environment-list*))))
    (when path
      (do* ((i 0 (1+ p))
	    (p (position #\: path :start i)
	       (position #\: path :start i))
	    (pl ()))
	   ((null p)
	    (let ((s (subseq path i)))
	      (fi (string= s "")
		  (push (concatenate 'simple-string s "/") pl)))
	    (nreverse pl))
	(let ((s (subseq path i p)))
	  (fi (string= s "")
	      (push (concatenate 'simple-string s "/") pl)))))))

;;; ENVIRONMENT-INIT  --  Internal
;;;
;;; Parse the LISP-ENVIRONMENT-LIST into a keyword alist.  Set up default
;;; search lists.
;;;
(defun environment-init ()
  (setq *environment-list* ())
  (dolist (ele lisp-environment-list)
    (let ((=pos (position #\= (the simple-string ele))))
      (when =pos
	(push (cons (intern (string-upcase (subseq ele 0 =pos))
			    *keyword-package*)
		    (subseq ele (1+ =pos)))
	      *environment-list*))))
  (setq *current-directory* (current-unix-directory))
  (setf (search-list "path:") (parse-unix-search-list :path))
  (setf (search-list "home:")
	(or (parse-unix-search-list :home)
	    (list (current-unix-directory))))
  (setf (search-list ":") "home:")

  (setf (search-list "library:")
	(or (parse-unix-search-list :nightshadelib)
	    '(#+mach  "/usr/misc/.nightshade/lib/"
	      #+linux "/usr/local/lib/nightshade/"
	      #-(or mach linux) "/usr/local/lib/nightshade/lib/"))))


;;;; SAVE-LISP itself.

(alien:def-alien-routine "save" (alien:boolean)
  (file c-call:c-string)
  (initial-function (alien:unsigned #.vm:word-bits)))

(defun save-lisp (core-file-name &key
				 (purify t)
				 (root-structures ())
				 (environment-name "Auxiliary")
				 (init-function #'%top-level)
				 (load-init-file t)
				 (site-init "library:site-init")
				 (print-herald t)
				 (process-command-line t))
  "Save a core image in the file named $core-file-name.  The following
   keywords are defined:

     $purify
         If true, do a purifying GC, which moves all dynamically allocated
         objects into static space so that they stay pure.  This takes
         somewhat longer than the normal GC and reduces the amount of work
         the garbage collector must do when the resulting core image runs.
         Also, if more than one Lisp is running on the same machine, this
         maximizes the amount of memory that can be shared between the two
         processes.  Use `ext:purify' to do the purifying GC.

     $root-structures
         A list of the main entry points in any newly loaded systems.  This
         may be supplied to improve locality and/or GC performance.  Only
         meaningful if :purify is true.  Related to `ext:purify'.

     $environment-name
         Also passed to `ext:purify' when :purify is true.

     $init-function
         The function that starts running when the created core file is
         resumed.  The function `%top-level' simply invokes the top level
         read-eval-print loop.  If the function returns the lisp exits.

     $load-init-file
         If true, then look for an init file to load into the resumed
	 core; either the one specified on the command line,
	 home:nightshade.fasl or home:nightshade.lisp.

     $site-init
         If true, then the name of the site init file to load.  The default
	 is library:site-init.  The existence of this file is optional.

     $print-herald
         If true, print out the system herald when starting.

     $process-command-line
         If true, process the command line switches and perform the
         appropriate actions."
  #+mp (mp::shutdown-multi-processing)
  (if (fboundp 'eval:flush-interpreted-function-cache)
      (eval:flush-interpreted-function-cache))
  (if (fboundp 'cancel-finalization)
      (cancel-finalization sys:*tty*))
  (if purify
      (purify :root-structures root-structures
	      :environment-name environment-name)
      #-gencgc (gc) #+gencgc (gc :full t))
  (dolist (f *before-save-initializations*) (funcall f))
  (labels
      ((%restart-lisp ()
	 (let (*script-mode*)
	   (with-simple-restart (abort "Exit Nightshade.")
	     (catch 'top-level-catcher
	       (reinit)
	       (environment-init)
	       (dolist (f *after-save-initializations*) (funcall f))
	       (if process-command-line (ext::process-command-strings))
	       (setf *editor-lisp-p* nil)
	       (macrolet ((find-switch (name)
			    `(find ,name *command-line-switches*
				   :key #'cmd-switch-name
				   :test #'(lambda (x y)
					     (declare (simple-string x y))
					     (string-equal x y)))))
		 (let ((lib (assoc :nightshadelib ext:*environment-list*)))
		   (if lib (setf (search-list "library:") (cdr lib))))
		 (if site-init
		     (load site-init :if-does-not-exist nil :verbose nil))
		 (and process-command-line
		      (find-switch "edit")
		      (setf *editor-lisp-p* t))
		 (and load-init-file
		      (or (and process-command-line (find-switch "noinit"))
			  (let* ((cl-switch (find-switch "init"))
				 (name (and cl-switch
					    (or (cmd-switch-value cl-switch)
						(car (cmd-switch-words cl-switch))))))
			    (if name
				(load (merge-pathnames name #p"home:")
				      :if-does-not-exist ())
				(load (config:config-pathname "lisp")
				      :if-does-not-exist () :verbose ())))))
		 (if process-command-line
		     (ext::invoke-switch-demons *command-line-switches*
						*command-switch-demons*))
		 (or *editor-lisp-p*
		     (find-switch "slave")
		     *batch-mode*
		     (when (stringp (car *command-line-words*))
		       (setq *script-mode* t)))
		 (or *script-mode* (if print-herald (print-herald)))))
	     (if *editor-lisp-p*
		 (restart-case
		     (catch 'top-level-catcher
		       (let ((initp (fi (ext:get-command-line-switch "noinit"))))
			 (if (stringp (car ext:*command-line-words*))
			     (ed (car ext:*command-line-words*) :init initp)
			     (ed () :init initp))))
		   (top () :report "Exit to the Top-Level.")))
	     (or *script-mode* *batch-mode* (funcall init-function)))
	   (when (or *script-mode* *batch-mode*)
	     (if *script-mode* (ext::quiet-switch-demon :dummy))
	     (handler-bind
		 ((error #'(lambda (condition)
			     (format *error-output*
				     "Error in ~:[script~;batch~] processing:~%~A~%"
				     *batch-mode*
				     condition)
			     (backtrace 200 *error-output*)
			     (throw '%end-of-the-world 1))))
	       (catch 'top-level-catcher
		 ;; Load any files that were given as arguments.
		 (loop for word in ext:*command-line-words* while (stringp word) do
		   (load word :verbose () :restart-p ()))
		 (if *batch-mode* (funcall init-function))))))
	 0)
       (restart-lisp ()
	 (unix:unix-exit
	  (catch '%end-of-the-world
	    (unwind-protect
		(%restart-lisp)
	      (finish-standard-output-streams))))))

    (let ((initial-function (get-lisp-obj-address #'restart-lisp)))
      (without-gcing
	(save (os-namestring core-file-name nil) initial-function))))
  ())


;;;; PRINT-HERALD support.

(defvar *herald-items* ()
  "Determines what PRINT-HERALD prints (the system startup banner.)  This
   is a database which can be augmented by each loaded system.  The format
   is a property list which maps from subsystem names to the banner
   information for that system.  This list can be manipulated with GETF --
   entries are printed in reverse order, so the newest entry is printed
   last.  Usually the system feature keyword is used as the system name.  A
   given banner is a list of strings and functions (or function names).
   Strings are printed, and functions are called with an output stream
   argument.")

(setf (getf *herald-items* :nightshade)
      `("Nightshade "
	,#'(lambda (stream) (write-string (version) stream))
	", running on "
	,#'(lambda (stream) (write-string (machine-instance) stream))
	"."))

(setf (getf *herald-items* :help)
      '(terpri "(quit) to exit, (help) for help."))

(setf (getf *herald-items* :subsystems)
      '(terpri "Loaded subsystems:"))

;;; PRINT-HERALD  --  Public
;;;
(defun print-herald (&optional (stream *standard-output*))
  "Print some descriptive information about the Lisp system version and
   configuration."
  (let ((res ()))
    (while ((item *herald-items* (cddr item)))
	   (item)
      (push (second item) res))

    (fresh-line stream)
    (dolist (item res)
      (dolist (thing item)
	(typecase thing
	  (string
	   (write-string thing stream))
	  (function (funcall thing stream))
	  ((or symbol cons)
	   (funcall (fdefinition thing) stream))
	  (t
	   (error "Unrecognized *HERALD-ITEMS* entry: ~S." thing))))
      (fresh-line stream))
    (terpri stream))

  (values))


;;;; Random functions used by worldload.

(defun assert-user-package ()
  (or (eq *package* (find-package "USER"))
      (error "Change *PACKAGE* to the USER package and try again.")))

;;; MAYBE-BYTE-LOAD  --  Interface
;;;
;;; If Name has been byte-compiled, and :runtime is a feature, then load
;;; the byte-compiled version, otherwise just do normal load.
;;;
(defun maybe-byte-load (name &optional (load-native t))
  (let ((bname (make-pathname
		:defaults name
		:type #.(c:backend-byte-fasl-file-type c:*target-backend*))))
    (cond ((and (featurep :runtime)
		(probe-file bname))
	   (load bname))
	  (load-native
	   (load name)))))

;;; BYTE-LOAD-OVER  --  Interface
;;;
;;; Replace a cold-loaded native object file with a byte-compiled one, if
;;; it exists.
;;;
(defun byte-load-over (name)
  (load (make-pathname
	 :defaults name
	 :type #.(c:backend-byte-fasl-file-type c:*target-backend*))
	:if-does-not-exist nil))
