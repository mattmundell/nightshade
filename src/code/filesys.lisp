;;; File system interface functions.  This file is pretty Unix specific.

(in-package "LISP")

;; FIX remove/flush/clear/release-file
;; FIX consistent dir naming
;; FIX consider copy-dir, perhaps from e:dired.lisp
;; FIX consider copy,delete,compare-ent which do files/dirs
;;     (merge in from dired.lisp copy,del-file?)
;;     maybe always specify dirs with trailing slash
;;         too much overhead if dir exists already?
;; FIX consider touch-file,touch-dir,touch-entity
;;         vs touch-file, ensure-directories-exist
;;                           ^- if dir exists leaves time same
;;         maybe  touch,ensure-file, touch,ensure-dir[s]
(export '(truename probe-file user-homedir-pathname directory
          rename-file copy-file symlink-file touch-file truncate-file
	  add-dir
	  delete-file trash-file delete-dir
	  file-author file-kind file-mode file-size file-stats file-write-date
	  hiddenp hidden-name-p
	  directorify namify ensure-trailing-slash
	  directoryp directory-name-p filep file-name-p
	  set-file-mode set-file-write-date
	  symlinkp symlink-dest host-path))

(use-package "EXTENSIONS")

(in-package "EXTENSIONS")

(export '(print-directory
	  ; FIX if analagous to *-files below would print all files,dirs in given pathname
	  ;         print-files-from-list? pass list arg as key to directory?
	  print-files
	  complete-file ambiguous-files
	  current-directory file-writable os-namestring
	  ; FIX *-entities *-ent
	  map-files do-files list-files
	  map-dirs do-dirs list-dirs
	  in-directory
	  pick-new-file pick-new-dir pick-new-name
	  parse-mode-string))

(in-package "LISP")


#[ Filesystem Operations

[ Wildcard Matching                   ]
[ File Name Completion                ]
[ Miscellaneous Filesystem Operations ]
]#

#[ Wildcard Matching

Unix filesystem operations such as `open' will accept wildcard pathnames
that match a single file (of course, `directory' allows any number of
matches.)  Filesystem operations treat :wild-inferiors the same as :wild.

{function:directory}
{function:ext:print-directory}
]#

#[ File Name Completion

{function:ext:complete-file}
{function:ext:ambiguous-files}
]#

#[ Miscellaneous Filesystem Operations

{function:ext:current-directory}
{function:ext:file-writable}
{function:ext:os-namestring}
]#


;;;; Unix pathname host support.

;;; Unix namestrings have the following format:
;;;
;;; namestring := [ directory ] [ file [ type [ version ]]]
;;; directory := [ "/" | search-list ] { file "/" }*
;;; search-list := [^:/]*:
;;; file := [^/]*
;;; type := "." [^/.]*
;;; version := "." ([0-9]+ | "*")
;;;
;;; Note: this grammer is ambiguous.  The string foo.bar.5 can be parsed
;;; as either just the file specified or as specifying the file, type, and
;;; version.  Therefore, we use the following rules when confronted with
;;; an ambiguous file.type.version string:
;;;
;;; - If the first character is a dot, it's part of the file.  It is not
;;; considered a dot in the following rules.
;;;
;;; - If there is only one dot, it seperates the file and the type.
;;;
;;; - If there are multiple dots and the stuff following the last dot
;;; is a valid version, then that is the version and the stuff between
;;; the second to last dot and the last dot is the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the following
;;; characters, it is considered part of a wildcard pattern and has the
;;; following meaning.
;;;
;;; ? - matches any character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;
;;; Any of these special characters can be preceeded by a backslash to
;;; cause it to be treated as a regular character.

(defun remove-backslashes (namestr start end)
  "Remove and occurences of \\ from the string because we've already
   checked for whatever they may have been backslashed."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((result (make-string (- end start)))
	 (dst 0)
	 quoted)
    (do ((src start (1+ src)))
	((= src end))
      (cond (quoted
	     (setf (schar result dst) (schar namestr src))
	     (setf quoted ())
	     (incf dst))
	    (t
	     (let ((char (schar namestr src)))
	       (cond ((char= char #\\)
		      (setq quoted t))
		     (t
		      (setf (schar result dst) char)
		      (incf dst)))))))
    (when quoted
      (error 'namestring-parse-error
	     :complaint "Backslash in bad place."
	     :namestring namestr
	     :offset (1- end)))
    (shrink-vector result dst)))

(defvar *ignore-wildcards* ())

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (if *ignore-wildcards*
      (subseq namestr start end)
      (collect ((pattern))
	(let (quoted any-quotes last-regular-char
	      (index start))
	  (flet ((flush-pending-regulars ()
		   (when last-regular-char
		     (pattern (if any-quotes
				  (remove-backslashes namestr
						      last-regular-char
						      index)
				  (subseq namestr last-regular-char index)))
		     (setf any-quotes ())
		     (setf last-regular-char ()))))
	    (loop
	      (when (>= index end)
		(return))
	      (let ((char (schar namestr index)))
		(cond (quoted
		       (incf index)
		       (setf quoted ()))
		      ((char= char #\\)
		       (setf quoted t)
		       (setf any-quotes t)
		       (or last-regular-char
			   (setf last-regular-char index))
		       (incf index))
		      ((char= char #\?)
		       (flush-pending-regulars)
		       (pattern :single-char-wild)
		       (incf index))
		      ((char= char #\*)
		       (flush-pending-regulars)
		       (pattern :multi-char-wild)
		       (incf index))
		      ((char= char #\[)
		       (let ((close-bracket
			      (position #\] namestr :start index :end end)))
			 (if close-bracket
			     (progn
			       (flush-pending-regulars)
			       (pattern (list :character-set
					      (subseq namestr
						      (1+ index)
						      close-bracket)))
			       (setf index (1+ close-bracket)))
			     (progn
			       ;; Same as t branch below.
			       (or last-regular-char
				   (setf last-regular-char index))
			       (incf index)))))
		      (t
		       (or last-regular-char
			   (setf last-regular-char index))
		       (incf index)))))
	    (flush-pending-regulars)))
	(cond ((null (pattern))
	       "")
	      ((null (cdr (pattern)))
	       (let ((piece (first (pattern))))
		 (typecase piece
		   ((member :multi-char-wild) :wild)
		   (simple-string piece)
		   (t
		    (make-pattern (pattern))))))
	      (t
	       (make-pattern (pattern)))))))

(defun extract-name-type-and-version (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((last-dot (position #\. namestr :start (1+ start) :end end
			     :from-end t))
	 (second-to-last-dot (and last-dot
				  (position #\. namestr :start (1+ start)
					    :end last-dot :from-end t)))
	 (version :newest))
#|
    ;; If there is a second-to-last dot, check to see if there is a valid
    ;; version after the last dot.
    (when second-to-last-dot
      (cond ((and (= (+ last-dot 2) end)
		  (char= (schar namestr (1+ last-dot)) #\*))
	     (setf version :wild))
	    ((and (< (1+ last-dot) end)
		  (do ((index (1+ last-dot) (1+ index)))
		      ((= index end) t)
		    (or (char<= #\0 (schar namestr index) #\9)
			(return ()))))
	     (setf version
		   (parse-integer namestr :start (1+ last-dot) :end end)))
	    (t
	     (setf second-to-last-dot ()))))
|#
    ;; If there is a second-to-last dot, check to see if there is a wild
    ;; version after the last dot.
    (when second-to-last-dot
      (cond ((and (= (+ last-dot 2) end)
		  (char= (schar namestr (1+ last-dot)) #\*))
	     (setf version :wild))
	    (t
	     (setf second-to-last-dot ()))))
    (cond (second-to-last-dot
	   (values (maybe-make-pattern namestr start second-to-last-dot)
		   (maybe-make-pattern namestr
				       (1+ second-to-last-dot)
				       last-dot)
		   version))
	  (last-dot
	   (values (maybe-make-pattern namestr start last-dot)
		   (maybe-make-pattern namestr (1+ last-dot) end)
		   version))
	  (t
	   (values (maybe-make-pattern namestr start end)
		   ()
		   version)))))

;;; Take a string and return a list of cons cells that mark the char
;;; separated subseq.  The first value t if absolute directories location.
;;;
(defun split-at-slashes (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((absolute (and (/= start end)
		       (char= (schar namestr start) #\/))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash seperated chunks.
    (collect ((pieces))
      (loop
	(let ((slash (position #\/ namestr :start start :end end)))
	  (pieces (cons start (or slash end)))
	  (or slash (return))
	  (setf start (1+ slash))))
      (values absolute (pieces)))))

(defun maybe-extract-search-list (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let (quoted)
    (do ((index start (1+ index)))
	((= index end)
	 (values () start))
      (if quoted
	  (setf quoted ())
	  (case (schar namestr index)
	    (#\\
	     (setf quoted t))
	    (#\:
	     (return (values (remove-backslashes namestr start index)
			     (1+ index)))))))))

(defun parse-os-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (multiple-value-bind
      (absolute pieces)
      (split-at-slashes namestr start end)
    (let ((search-list
	   (if absolute
	       ()
	       (let ((first (car pieces)))
		 (multiple-value-bind
		     (search-list new-start)
		     (maybe-extract-search-list namestr
						(car first) (cdr first))
		   (when search-list
		     (setf absolute t)
		     (setf (car first) new-start))
		   search-list)))))
      (multiple-value-bind
	  (name type version)
	  (let* ((tail (car (last pieces)))
		 (tail-start (car tail))
		 (tail-end (cdr tail)))
	    (unless (= tail-start tail-end)
	      (setf pieces (butlast pieces))
	      (extract-name-type-and-version namestr tail-start tail-end)))
	;; Make sure there are no illegal characters in the name such as
	;; #\Null and #\/.
	(when (and (stringp name)
                   (find-if #'(lambda (x)
				(or (char= x #\Null) (char= x #\/)))
			    name))
	  (error 'parse-error))
	;; Now we have everything we want.  So return it.
	(values () ; no host for unix namestrings.
		() ; no devices for unix namestrings.
		(collect ((dirs))
		  (when search-list
		    (dirs (intern-search-list search-list)))
		  (dolist (piece pieces)
		    (let ((piece-start (car piece))
			  (piece-end (cdr piece)))
		      (unless (= piece-start piece-end)
			(cond ((string= namestr ".." :start1 piece-start
					:end1 piece-end)
			       (dirs :up))
			      ((string= namestr "**" :start1 piece-start
					:end1 piece-end)
			       (dirs :wild-inferiors))
			      (t
			       (dirs (maybe-make-pattern namestr
							 piece-start
							 piece-end)))))))
		  (cond (absolute
			 (cons :absolute (dirs)))
			((dirs)
			 (cons :relative (dirs)))
			(t
			 ())))
		name
		type
		version)))))

(defun unparse-unix-host (pathname)
  (declare (type pathname pathname)
	   (ignore pathname))
  "Unix")

(defun unparse-unix-piece (thing)
  (etypecase thing
    ((member :wild) "*")
    (simple-string
     (let* ((srclen (length thing))
	    (dstlen srclen))
       (dotimes (i srclen)
	 (case (schar thing i)
	   ((#\* #\? #\[)
	    (incf dstlen))))
       (let ((result (make-string dstlen))
	     (dst 0))
	 (dotimes (src srclen)
	   (let ((char (schar thing src)))
	     (case char
	       ((#\* #\? #\[)
		(setf (schar result dst) #\\)
		(incf dst)))
	     (setf (schar result dst) char)
	     (incf dst)))
	 result)))
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string
	    (strings piece))
	   (symbol
	    (ecase piece
	      (:multi-char-wild
	       (strings "*"))
	      (:single-char-wild
	       (strings "?"))))
	   (cons
	    (case (car piece)
	      (:character-set
	       (strings "[")
	       (strings (cdr piece))
	       (strings "]"))
	      (t
	       (error "Invalid pattern piece: ~S" piece))))))
       (apply #'concatenate
	      'simple-string
	      (strings))))))

(defun unparse-unix-directory-list (directory)
  (declare (type list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
	(:absolute
	 (cond ((search-list-p (car directory))
		(pieces (search-list-name (pop directory)))
		(pieces ":"))
	       (t
		(pieces "/"))))
	(:relative
	 ;; Nothing special.
	 ))
      (dolist (dir directory)
	(typecase dir
	  ((member :up)
	   (pieces "../"))
	  ((member :back)
	   (error ":BACK cannot be represented in namestrings."))
	  ((member :wild-inferiors)
	   (pieces "**/"))
	  ((or simple-string pattern)
	   (pieces (unparse-unix-piece dir))
	   (pieces "/"))
	  (t
	   (error "Invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-unix-directory (pathname)
  (declare (type pathname pathname))
  (unparse-unix-directory-list (%pathname-directory pathname)))

(defun unparse-unix-file (pathname)
  (declare (type pathname pathname))
  (collect ((strings))
    (let* ((name (%pathname-name pathname))
	   (type (%pathname-type pathname))
	   (type-supplied (not (or (null type) (eq type :unspecific))))
	   (version (%pathname-version pathname))
	   (version-supplied (not (or (null version) (eq version :newest)))))
      (if name (strings (unparse-unix-piece name)))
      (when type-supplied
	(or name (error "Must specify a file with the type: ~S" pathname))
	(strings ".")
	(strings (unparse-unix-piece type)))
      (when version-supplied
	(or type-supplied
	    (error "Must supply a type with the version: ~S" pathname))
	(strings (if (eq version :wild)
		     ".*"
		     (format () ".~D" version)))))
    (and (strings) (apply #'concatenate 'simple-string (strings)))))

(defun unparse-os-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
	       (unparse-unix-directory pathname)
	       (unparse-unix-file pathname)))

(defun unparse-unix-enough (pathname defaults)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
	   (error "~S cannot be represented relative to ~S"
		  pathname defaults)))
    (collect ((strings))
      (let* ((pathname-directory (%pathname-directory pathname))
	     (defaults-directory (%pathname-directory defaults))
	     (prefix-len (length defaults-directory))
	     (result-dir
	      (cond ((and (> prefix-len 1)
			  (>= (length pathname-directory) prefix-len)
			  (compare-component (subseq pathname-directory
						     0 prefix-len)
					     defaults-directory))
		     ;; Pathname starts with a prefix of default.  So just
		     ;; use a relative directory from then on out.
		     (cons :relative (nthcdr prefix-len pathname-directory)))
		    ((eq (car pathname-directory) :absolute)
		     ;; We are an absolute pathname, so we can just use it.
		     pathname-directory)
		    (t
		     ;; We are a relative directory.  So we lose.
		     (lose)))))
	(strings (unparse-unix-directory-list result-dir)))
      (let* ((pathname-version (%pathname-version pathname))
	     (version-needed (and pathname-version
				  (not (eq pathname-version :newest))))
	     (pathname-type (%pathname-type pathname))
	     (type-needed (or version-needed
			      (and pathname-type
				   (not (eq pathname-type :unspecific)))))
	     (pathname-name (%pathname-name pathname))
	     (name-needed (or type-needed
			      (and pathname-name
				   (not (compare-component pathname-name
							   (%pathname-name
							    defaults)))))))
	(when name-needed
	  (or pathname-name (lose))
	  (strings (unparse-unix-piece pathname-name)))
	(when type-needed
	  (when (or (null pathname-type) (eq pathname-type :unspecific))
	    (lose))
	  (strings ".")
	  (strings (unparse-unix-piece pathname-type)))
	(when version-needed
	  (typecase pathname-version
	    ((member :wild)
	     (strings ".*"))
	    (integer
	     (strings (format () ".~D" pathname-version)))
	    (t
	     (lose)))))
      (apply #'concatenate 'simple-string (strings)))))

(defstruct (unix-host
	    (:include host
		      (:parse #'parse-os-namestring)
		      (:unparse #'unparse-os-namestring)
		      (:unparse-host #'unparse-unix-host)
		      (:unparse-directory #'unparse-unix-directory)
		      (:unparse-file #'unparse-unix-file)
		      (:unparse-enough #'unparse-unix-enough)
		      (:customary-case :lower))
	    (:make-load-form-fun make-unix-host-load-form)))

(defvar *unix-host* (make-unix-host))

(defun make-unix-host-load-form (host)
  (declare (ignore host))
  '*unix-host*)


;;;; Wildcard matching.

(defun %enumerate-files (directory pathname verify-existance function
				   check-for-subdirs)
  (declare (simple-string directory))
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname))
	(version (%pathname-version pathname)))
    (cond
     ((member name '(() :unspecific))
      (when (if verify-existance
		;; FIX beware, probe-file via file-kind calls enumerate-matches
		(if (remote-pathname-p directory)
		    (probe-file directory)
		    (unix:unix-file-kind directory))
		t)
	(funcall function directory)))
     ((or (pattern-p name)
	  (pattern-p type)
	  (eq name :wild)
	  (eq version :wild) ;; FIX added
	  (eq type :wild))
      (let ((dir (unix:open-dir directory)))
	(when dir
	  (unwind-protect
	      (loop
		(let ((file (unix:read-dir dir)))
		  (if file
		      (or (string= file ".")
			  (string= file "..")
			  (multiple-value-bind
			      (file-name file-type file-version)
			      (let ((*ignore-wildcards* t))
				(extract-name-type-and-version
				 file 0 (length file)))
			    (when (and (components-match file-name name)
				       (components-match file-type type)
				       (components-match file-version
							 version))
			      (funcall
			       function
			       (if (and check-for-subdirs
					(eq (unix:unix-file-kind
					     (namestring
					      (merge-pathnames
					       file
					       directory)))
					    :directory))
				   (concatenate 'string
						directory
						file
						"/")
				   (concatenate 'string
						directory
						file))))))
		      (return))))
	    (unix:close-dir dir)))))
     (t
      (let ((file (concatenate 'string directory name)))
	(or (null type) (eq type :unspecific)
	    (setf file (concatenate 'string file "." type)))
	(or (member version '(() :newest :wild))
	    (setf file (concatenate 'string file "."
				    (quick-integer-to-string version))))
	(if check-for-subdirs
	    (let ((kind (unix:unix-file-kind file t))) ; FIX if remote?
	      (when (if verify-existance kind t)
		(funcall function (if (eq kind :directory)
				      (concatenate 'string
						   file
						   "/")
				      file))))
	    (when (if verify-existance
		      ;; FIX if remote?
		      ;;     beware, probe-file via file-kind calls enumerate-matches
		      (unix:unix-file-kind file t)
		      t)
	      (funcall function file))))))))

(defun %enumerate-remote-files (directory pathname verify-existance
					  function)
  (declare (simple-string directory))
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname))
	(version (%pathname-version pathname)))
    (cond ((member name '(() :unspecific))
	   (when (if verify-existance
		     (if (remote-pathname-p directory)
			 (probe-file directory)
			 (unix:unix-file-kind directory))
		     t)
	     (funcall function directory)))
	  ((or (pattern-p name)
	       (pattern-p type)
	       (eq name :wild)
	       (eq type :wild))
	   (internet:do-remote-directory (directory file)
	     (or (string= file ".")
		 (string= file "..")
		 (multiple-value-bind
		     (file-name file-type file-version)
		     (let ((*ignore-wildcards* t))
		       (extract-name-type-and-version
			file 0 (length file)))
		   (when (and (components-match file-name name)
			      (components-match file-type type)
			      (components-match file-version version))
		     (funcall function
			      (concatenate 'string
					   directory
					   file)))))))
	  (t
	   (let ((file (concatenate 'string directory name)))
	     (or (null type) (eq type :unspecific)
		 (setf file (concatenate 'string file "." type)))
	     (or (member version '(() :newest :wild))
		 (setf file (concatenate 'string file "."
					 (quick-integer-to-string version))))
	     (when (if verify-existance
		       (if (remote-pathname-p pathname)
			   (probe-file directory)
			   (unix:unix-file-kind file t))
		       t)
	       (funcall function file)))))))

;;; %enumerate-directories  --   Internal
;;;
;;; The directory node and device numbers are maintained for the current
;;; path during the search for the detection of paths loops upon
;;; :wild-inferiors.
;;;
(defun %enumerate-directories (head tail pathname verify-existance
			       follow-links nodes function
			       check-for-subdirs)
  (declare (simple-string head))
  (macrolet ((unix-xstat (name)
	       `(if follow-links
		    (unix:unix-stat ,name)
		    (unix:unix-lstat ,name)))
	     (with-directory-node-noted ((head) &body body)
	       `(multiple-value-bind (res dev ino mode)
		    (unix-xstat ,head)
		  (when (and res (eql (logand mode unix:s-ifmt) unix:s-ifdir))
		    (let ((nodes (cons (cons dev ino) nodes)))
		      ,@body))))
	     (do-directory-entries ((name directory) &body body)
	       `(let ((dir (unix:open-dir ,directory)))
		  (when dir
		    (unwind-protect
			 (loop
			  (let ((,name (unix:read-dir dir)))
			    (cond ((null ,name)
				   (return))
				  ((string= ,name "."))
				  ((string= ,name ".."))
				  (t
				   ,@body))))
		      (unix:close-dir dir))))))
    (fi tail
	(%enumerate-files head pathname verify-existance function
			  check-for-subdirs)
	(let ((piece (car tail)))
	  (etypecase piece
	    (simple-string
	     (let ((head (concatenate 'string head piece)))
	       (with-directory-node-noted (head)
		 (%enumerate-directories (concatenate 'string head "/")
					 (cdr tail) pathname
					 verify-existance follow-links
					 nodes function
					 check-for-subdirs))))
	    (search-list
	     (or (string= head "/")
 		 (error "A search list can only come first in a pathname."))
	     (%enumerate-remote-directories (concat (remote-pathname-host pathname)
						    ":")
					    (cdr tail) pathname
					    verify-existance follow-links
					    nodes function))
	    ((member :wild-inferiors)
	     (%enumerate-directories head (rest tail) pathname
				     verify-existance follow-links
				     nodes function
				     check-for-subdirs)
	     (do-directory-entries (name head)
	       (let ((subdir (concatenate 'string head name)))
		 (multiple-value-bind (res dev ino mode)
		     (unix-xstat subdir)
		   (declare (type (or fixnum null) mode))
		   (when (and res (eql (logand mode unix:s-ifmt) unix:s-ifdir))
		     (unless (dolist (dir nodes ())
			       (when (and (eql (car dir) dev)
					  (eql (cdr dir) ino))
				 (return t)))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'string subdir "/")))
			 (%enumerate-directories subdir tail pathname
						 verify-existance
						 follow-links
						 nodes function
						 check-for-subdirs))))))))
	    ((or pattern (member :wild))
	     (do-directory-entries (name head)
	       (when (or (eq piece :wild) (pattern-matches piece name))
		 (let ((subdir (concatenate 'string head name)))
		   (multiple-value-bind (res dev ino mode)
		       (unix-xstat subdir)
		     (declare (type (or fixnum null) mode))
		     (when (and res
				(eql (logand mode unix:s-ifmt) unix:s-ifdir))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'string subdir "/")))
			 (%enumerate-directories subdir (rest tail) pathname
						 verify-existance follow-links
						 nodes function
						 check-for-subdirs))))))))
	    ((member :up)
	     (let ((head (concatenate 'string head "..")))
	       (with-directory-node-noted (head)
		 (%enumerate-directories (concatenate 'string head "/")
					 (rest tail) pathname
					 verify-existance follow-links
					 nodes function
					 check-for-subdirs)))))))))

;;; %enumerate-remote-directories  --   Internal
;;;
(defun %enumerate-remote-directories (head tail pathname verify-existance
				      follow-links nodes function)
  (declare (simple-string head))
  (macrolet ((with-directory-node-noted ((head) &body body)
	       (declare (ignore head))
	       `(progn
		  ;; FIX circular check
		  ,@body))
	     (do-directory-entries ((name directory) &body body)
	       `(let ((dir (unix:open-dir ,directory)))
		  (when dir
		    (unwind-protect
			 (loop
			  (let ((,name (unix:read-dir dir)))
			    (cond ((null ,name)
				   (return))
				  ((string= ,name "."))
				  ((string= ,name ".."))
				  (t
				   ,@body))))
		      (unix:close-dir dir))))))
    (fi tail
	(%enumerate-remote-files head pathname verify-existance function)
	(let ((piece (car tail)))
	  (etypecase piece
	    (simple-string
	     (let ((head (concatenate 'string head piece)))
	       (with-directory-node-noted (head)
		 (%enumerate-remote-directories (concatenate 'string head "/")
						(cdr tail) pathname
						verify-existance follow-links
						nodes function))))
	    (search-list
	     (error "A search list can only come first in a pathname."))
	    ((member :wild-inferiors)
	     (error "FIX %enumerate-remote-directories with :wild-inferiors")
	     (%enumerate-remote-directories head (rest tail) pathname
					    verify-existance follow-links
					    nodes function)
	     (do-directory-entries (name head)
	       (let ((subdir (concatenate 'string head name)))
		 (multiple-value-bind (res dev ino mode)
		     (file-stats subdir)
		   (declare (type (or fixnum null) mode))
		   (when (and res (eql (logand mode unix:s-ifmt) unix:s-ifdir))
		     (unless (dolist (dir nodes ())
			       (when (and (eql (car dir) dev)
					  (eql (cdr dir) ino))
				 (return t)))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'string subdir "/")))
			 (%enumerate-remote-directories subdir tail pathname
							verify-existance follow-links
							nodes function))))))))
	    ((or pattern (member :wild))
	     (error "FIX %enumerate-remote-directories with :wild")
	     #|
	     (internet:do-remote-directory (dir name head)
	       (when (or (eq piece :wild) (pattern-matches piece name))
		 (let ((subdir (concatenate 'string head name)))
		   (multiple-value-bind (res dev ino mode)
		       (unix-xstat subdir)
		     (declare (type (or fixnum null) mode))
		     (when (and res
				(eql (logand mode unix:s-ifmt) unix:s-ifdir))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'string subdir "/")))
			 (%enumerate-remote-directories subdir (rest tail) pathname
							verify-existance follow-links
							nodes function)))))))
	     |#
	     )
	    ((member :up)
	     (error "FIX %enumerate-remote-directories with :up")
	     (let ((head (concatenate 'string head "..")))
	       (with-directory-node-noted (head)
		 (%enumerate-remote-directories (concatenate 'string head "/")
						(rest tail) pathname
						verify-existance follow-links
						nodes function)))))))))

(defmacro enumerate-matches ((var pathname &optional result
				  &key
				  (check-for-subdirs t)
				  (verify-existance t) (follow-links t))
			     &body body)
  (let ((body-name (gensym)))
    `(block ()
       (flet ((,body-name (,var)
		,@body))
	 (%enumerate-matches (pathname ,pathname)
			     ,verify-existance ,follow-links
			     #',body-name ,check-for-subdirs)
	 ,result))))

(defun %enumerate-matches (pathname verify-existance follow-links
				    function check-for-subdirs)
  ;; FIX pass check-for-subdirs to remote functions
  (if (pathname-type pathname)
      (or (pathname-name pathname)
	  (error "Pathname must have a name if it has a type:~%  ~S" pathname)))
  (let ((directory (pathname-directory pathname)))
    (if directory
	(ecase (car directory)
	  (:absolute
	   (if (remote-pathname-p pathname)
	       (%enumerate-remote-directories (concat (remote-pathname-host pathname)
						      ":/"
						      ;; FIX a guess
						      (directory-namestring pathname))
					      () pathname
					      verify-existance follow-links
					      () function)
	       (%enumerate-directories (directory-namestring pathname)
				       () pathname
				       verify-existance follow-links
				       () function
				       check-for-subdirs)))
	  (:relative
	   (if (remote-pathname-p (current-directory))
	       (%enumerate-remote-directories ""
					      (cdr directory) pathname
					      verify-existance follow-links
					      () function)
	       (%enumerate-directories (directory-namestring pathname)
				       () pathname
				       verify-existance follow-links
				       () function
				       check-for-subdirs))))
	(if (remote-pathname-p pathname)
	    (%enumerate-remote-files "" pathname verify-existance function)
	    (%enumerate-files "" pathname verify-existance function
			      check-for-subdirs)))))


;;;; OS-NAMESTRING -- public
;;;
(defun os-namestring (pathname &optional (for-input t) executable-only)
  "Convert $pathname for operating system (OS) calls.

   Return a string for $pathname that is suitable for passing to the
   underlying OS.

   Expand search-lists and wildcards.  If $pathname a search list alone,
   for example \"home:\", then return the first directory in the search
   list, and if that search list is empty return ().

   When $for-input is true, return the string if $pathname exists,
   otherwise return ().

   If $executable-only is true, return the string if an executable version
   of $pathname exists, else return ().

   For symlinks, check for existance on the symlink and check for
   executability on symlink destinations.

   Signal an error if $pathname names more than one file."
  (let ((path (let ((lpn (pathname pathname)))
		(if (logical-pathname-p lpn)
		    (namestring (translate-logical-pathname lpn))
		    pathname))))
    (if (remote-pathname-p path)
	path
	(enumerate-search-list (pathname path)
          (collect ((names))
	    (enumerate-matches (name pathname ()
				     :verify-existance for-input
				     :follow-links t)
			       (when (if executable-only
					 (and (eq (unix:unix-file-kind name) :file)
					      (unix:unix-access name unix:x_ok))
					 t)
				 (names name)))
	    (let ((names (names)))
	      (when names
		(if (cdr names)
		    (error 'simple-file-error
			   :format-control "~S is ambiguous:~{~%  ~A~}"
			   :format-arguments (list pathname names)))
		(return (car names)))))))))


;;;; TRUENAME and PROBE-FILE.

;;; Truename  --  Public
;;;
;;; Another silly file function trivially different from another function.  ; FIX just use probe-file?
;;;
(defun truename (pathname)
  "Return the pathname for the actual file described by pathname.  An error
   of type file-error is signalled if no such file exists, or the pathname
   is wild."
  (if (wild-pathname-p pathname)
      (error 'simple-file-error
	     :format-control "Bad place for a wild pathname."
	     :pathname pathname)
      (let ((result (probe-file pathname)))
	(or result
	    (error 'simple-file-error
		   :pathname pathname
		   :format-control "The file ~S does not exist."
		   :format-arguments (list (namestring pathname))))
	result)))

;; FIX replaced by remote-pathname-local
;;; Host-Path  --  Public
;;;
(defun host-path (pathname)
  "Return the directory and file part of PATHNAME as a namestring, as
   though the file was on the local host.  That is, strip off the search
   list prefix, so host:/dir/file becomes /dir/file."
  (let ((dir (cddr (pathname-directory pathname))))
    (namestring
     (make-pathname
      :directory (cons (if (and (plusp (length (car dir)))
				(char= (char (car dir) 0) #\/))
			   :ABSOLUTE
			   :RELATIVE)
		       dir)
      :defaults (pathname pathname)))))

;;; Probe-File  --  Public
;;;
(defun probe-file (pathname &key check-for-links)
  "Return a pathname which is the truename of $pathname if it exists, else
   return ().

   Signal an error of type file-error if pathname is wild.

   Return the type of $pathname as a second value.  If $check-for-links is
   true and $pathname is a symlink then return :link instead of the type of
   the truename."
  (if (wild-pathname-p pathname)
      (error 'simple-file-error
	     :pathname pathname
	     :format-control "Bad place for a wild pathname.")
      (if (remote-pathname-p pathname)
	  (internet:probe-remote-file pathname)
	  (let ((namestring (os-namestring pathname t)))
	    (when namestring
	      (let ((kind (unix:unix-file-kind (namify namestring) check-for-links)))
		(when kind
		  (let ((truename (unix:unix-resolve-links
				   (unix:unix-maybe-prepend-current-directory
				    namestring))))
		    (if truename
			(let ((*ignore-wildcards* t))
			  (values (pathname (unix:unix-simplify-pathname truename))
				  kind))
			(values () kind))))))))))


;;;; Other random operations.

;;; Rename-File  --  Public
;;;
(defun rename-file (file new-name &key check-for-links)
  "Rename $file to have the specified $new-name.  If $file is a stream open
   to a file, then rename the associated file.

   When $file is a symlink and $check-for-links is true then rename the
   symlink.

   When $file is a symlink and $check-for-links is false rename the
   destination of the symlink and adjust the symlink to point to the new
   name."
  (let* ((original (if check-for-links file (truename file)))
	 (original-namestring (os-namestring original (fi check-for-links)))
	 (new (merge-pathnames (merge-pathnames new-name
						(current-directory))
			       original))
	 (new-namestring (os-namestring new ())))
    (or new-namestring
	(error 'simple-file-error
	       :pathname new
	       :format-control "Failed to create ~S."
	       :format-arguments (list new)))
    (if (directoryp file)
	;; This fails for cross-device links, at least on Linux, so it is
	;; only used to rename directories.
	(multiple-value-bind (res error)
			     (unix:unix-rename original-namestring
					       new-namestring)
	  (or res
	      (error 'simple-file-error
		     :pathname new
		     :format-control "Failed to rename ~A to ~A: ~A"
		     :format-arguments (list original new
					     (unix:get-unix-error-msg error))))
	  (if (streamp file)
	      (file-name file new-namestring))
	  (values new original (truename new)))
	(progn
	  (copy-file original-namestring new-namestring
		     :check-for-links check-for-links)
	  (delete-file original-namestring)
	  (or check-for-links
	      (when (symlinkp file)
		(let ((dest (symlink-dest file)))
		  (delete-file file)
		  (symlink-file file (merge-pathnames new-name dest)))))
	  (if (streamp file) (file-name file new-namestring))
	  (values new original
		  (if check-for-links
		      ;; Symlink may have broken by being moved.
		      ;; FIX consistent?
		      (probe-file new)
		      (truename new)))))))

;; FIX rename read-file
(defun fs-read-file (fd ses-name)
  (multiple-value-bind (winp dev-or-err ino mode nlink uid gid rdev size)
		       (unix:unix-fstat fd)
    (declare (ignore ino nlink uid gid rdev))
    (or winp
	(error "Failed to open ~S: ~A."
	       ses-name (unix:get-unix-error-msg dev-or-err)))
    (let ((storage (system:allocate-system-memory size)))
      (multiple-value-bind (read-bytes err)
			   (unix:unix-read fd storage size)
	(when (or (null read-bytes) (not (= size read-bytes)))
	  (system:deallocate-system-memory storage size)
	  (error "Failed to read file ~S: ~A."
		 ses-name
		 (unix:get-unix-error-msg err))))
      (values storage size mode))))

;; FIX rename write-file
(defun fs-write-file (ses-name data byte-count mode)
  (multiple-value-bind (fd err) (unix:unix-creat ses-name #o644)
    (or fd
	(error "Failed to create file ~S: ~A"
	       ses-name (unix:get-unix-error-msg err)))
    (multiple-value-bind (winp err) (unix:unix-write fd data 0 byte-count)
      (or winp
	  (error "Failed to write file ~S: ~A"
		 ses-name
		 (unix:get-unix-error-msg err))))
    (unix:unix-fchmod fd (logand mode #o777))
    (unix:unix-close fd)))

;; FIX consider moving remote file handling to a layer above this, that
;;       shadows functions like this one
;;         link to separating the unix interface into a hardware interface
;;             ie mv unix calls out of here, group them with native interface

;; FIX doc return,errors
;;; Copy-File  --  Public
;;;
(defun copy-file (file new-file &key check-for-links)
  "Copy $file to $new-file, signalling a file-error on failure or if $file
   names a directory (i.e. ends in a slash) or is a directory.

   If $check-for-links is true, then preserve symbolic links."
  (if (directory-name-p file)
      (error 'simple-file-error
	     :pathname file
	     :format-control "Source names a directory: ~A."
	     :format-arguments
	     (list file)))
  (if (directoryp file :check-for-links check-for-links)
      (error 'simple-file-error
	     :pathname file
	     :format-control "Source is a directory: ~A."
	     :format-arguments
	     (list file)))
  (if (directoryp new-file)
      (setq new-file (merge-pathnames new-file (file-namestring file)))
      (if (directory-name-p new-file)
	  (error 'simple-file-error
		 :pathname new-file
		 :format-control "If $dest names a directory it must exist: ~A."
		 :format-arguments
		 (list new-file))))
  ;;; Now $file and $new-file both have directory and file namestrings.
  (if (remote-pathname-p file)
      (if (remote-pathname-p new-file)
	  (let ((pn (pick-new-file)))
	    (delete-file pn)
	    (internet:get-remote-file file pn)
	    (internet:put-remote-file pn new-file))
	  (internet:get-remote-file file new-file))
      (if (remote-pathname-p new-file)
	  (internet:put-remote-file file new-file)
	  (if (and check-for-links (symlinkp file))
	      (let (dest)
		(in-directory (directory-namestring file)
		  (setq dest
			(symlink-dest (file-namestring file))))
		(in-directory (directory-namestring new-file)
		  (symlink-file (file-namestring new-file) dest)))
	      (multiple-value-bind
		  (fd err)
		  (unix:unix-open (os-namestring file)
				  unix:o_rdonly 0)
		(or fd
		    (error "Opening ~S failed: ~A."
			   (os-namestring file) err))
		(unwind-protect
		    (multiple-value-bind (data byte-count mode)
					 (fs-read-file fd file)
		      (unwind-protect
			  (fs-write-file (os-namestring new-file ())
					 data byte-count mode)
			(system:deallocate-system-memory data
							 byte-count)))
		  (unix:unix-close fd)))))))

; FIX pathname.lisp, public
;;; Relate-Pathname  --  Public
;;;
(defun relate-pathname (dest source)
  "Relate $dest to $source.  Return a pathname like $dest, adjusted to be
   relative to the directory named in $source.  Signal an error if $source
   is absolute."
  (if (eq (car (pathname-directory dest)) :absolute)
      (return-from relate-pathname dest))
  (if (eq (car (pathname-directory source)) :absolute)
      (error "$source must be a relative pathname: ~A." source))
  (let ((ups 0) (downs 0))
    ;; Count the number of ups and downs in $source.
    (loop for dir = (cdr (pathname-directory source))
      then (cdr dir)
      while dir do
      (if (eq (car dir) :up)
	  (if (plusp downs) (decf downs) (incf ups))
	  (incf downs)))
    ;; Adjust $dest to match, if needed.
    (if (or (plusp ups) (plusp downs))
	(let ((dir (let ((cur-dir (pathname-directory
				   (current-directory)))
			 (dest-dir (pathname-directory dest)))
		     (cons :relative
			   (append
			    (make-list downs
				       :initial-element :up)
			    (last (cdr cur-dir) ups)
			    (cdr dest-dir))))))
	  (if dir
	      (make-pathname
	       :directory dir
	       :defaults dest)
	      (make-pathname :defaults dest)))
	(make-pathname :defaults dest))))

;;; Symlink-File  --  Public
;;;
(defun symlink-file (link dest)
  "Symlink $link to $dest, so that $link points to $dest.  Return true
   on success, else signal a file-error."
  ;; Fill in the file name if $link (the new link) is a directory.
  (if (directoryp link :check-for-links t)
      (setq link (merge-pathnames
		    (ensure-trailing-slash link)
		    (or (file-namestring dest)
			(car (last (pathname-directory dest)))))))
  (if (eq (car (pathname-directory link)) :absolute)
      ;; $link is absolute, so make $dest absolute too.
      ; FIX maybe probe-file and merge w current
      (setq dest (truename dest))
      (if (eq (car (pathname-directory dest)) :absolute)
	  ;; $dest is absolute, so make $link absolute too.
	  (setq link (or (probe-file link)
			   (merge-pathnames link (current-directory))))
	  ;; Adjust $dest (the existing file) to be relative to the
	  ;; directory named in $link.
	  (setq dest (relate-pathname dest link))))
  ;; Link.
  (multiple-value-bind
      (res err)
      (unix:unix-symlink (namestring dest) (namify (namestring link)))
    (or res
	(error 'simple-file-error
	       :pathname link
	       :format-control "Failed to symlink ~A to ~A: ~A."
	       :format-arguments
	       (list link dest
		     (unix:get-unix-error-msg err))))))

;;; Touch-File  --  Public
;;;
(defun touch-file (file &key check-for-links)
  "Set the write date of $file to the current time, returning the current
   time, ensuring that the file exists.

   If $check-for-links is true and $file is a symlink then set the time of
   the link instead of the destination of the link."
  (if (probe-file file)
      ;(setf (file-write-date file) (get-universal-time)))
      (set-file-write-date file (get-universal-time)
			   :check-for-links check-for-links)
      (with-open-file (stream file :direction :output :if-does-not-exist :create))))

;;; Truncate-File  --  Public
;;;
(defun truncate-file (file)
  ;; FIX return?
  "Truncate $file.  That is, flush an content of $file, leaving it empty."
  (with-open-file (stream file :direction :io :if-exists :supersede
			  :if-does-not-exist :error)))

(defvar *trash-root* "/trash/")

;; FIX reconsider, test
;; FIX trash like scramble, rename recycle-file bin-file free-file release-file
;;; Trash-File  --  Public
;;;
(defun trash-file (file)
  "Trash $file, returning #t.  On failure signal a file-error."
  (let ((namestring (os-namestring file t)))
    (or namestring
	(error 'simple-file-error
	       :pathname file
	       :format-control "~S must exist to be trashed."
	       :format-arguments (list file)))
    (let ((name (concatenate 'simple-string
			      *trash-root*
			      (if (eq (char namestring 0) #\/)
				  (subseq namestring 1)
				  namestring))))
    (ensure-directories-exist name)
    (rename-file namestring name)))
  t)

;;; Delete-File  --  Public
;;;
(defun delete-file (file)
  "Delete $file, returning true.  On failure signal a file-error.  If $file
   is a symlink then delete the symlink instead of the destination."  ; FIX is this consistent?
  (if (remote-pathname-p file)
      (or (internet:delete-remote-file file)
	  (error 'simple-file-error
		 :pathname file
		 :format-control "~A remains: FIX remote error."
		 :format-arguments (list file)))
      (let ((namestring (os-namestring (namify file) t)))
	(if (streamp file) (close file :abort t))
	(or namestring
	    (error 'simple-file-error
		   :pathname file
		   :format-control "File must exist: ~S."
		   :format-arguments (list file)))
	(multiple-value-bind (res err) (unix:unix-unlink namestring)
	  (or res
	      (error 'simple-file-error
		     :pathname namestring
		     :format-control "~A remains: ~A."
		     :format-arguments (list namestring
					     (unix:get-unix-error-msg err))))))))

;; FIX consistent error handling

;;; Delete-Dir  --  Public
;;;
(defun delete-dir (file)
  "Delete directory $file, returning true.  On failure signal a file error."
  (if (remote-pathname-p file)
      (or (internet:release-remote-directory file)
	  (error 'simple-file-error
		 :pathname file
		 :format-control "~A remains: FIX remote error."
		 :format-arguments (list file)))
      (let ((namestring (os-namestring file t)))
	(or namestring
	    (error 'simple-file-error
		   :pathname file
		   :format-control "File must exist: ~S."
		   :format-arguments (list file)))

	(multiple-value-bind (res err) (unix:unix-rmdir namestring)
	  (or res
	      (error 'simple-file-error
		     :pathname namestring
		     :format-control "Failed to delete ~A: ~A."
		     :format-arguments (list namestring
					     (unix:get-unix-error-msg err))))))))

(defun add-dir (pathname)
  "Add directory $pathname, returning true.  On failure signal a
   file-error."
  (if (remote-pathname-p pathname)
      (or (internet:add-remote-directory pathname)
	  (error 'simple-file-error
		 :pathname pathname
		 :format-control
		 "Failed to create remote directory."
		 :format-arguments (list pathname)))
      (multiple-value-bind (winp err)
			   (unix:unix-mkdir (os-namestring
					     (namify pathname)
					     ())
					    #o755)
	(fi winp
	    (error 'simple-file-error
		   :pathname pathname
		   :format-control
		   "Failed to create directory ~S: ~A"
		   :format-arguments
		   (list (os-namestring (namify pathname) ())
			 (unix:get-unix-error-msg err)))))))


;;; User-Homedir-Pathname  --  Public
;;;
;;; Return home:, which is set up at initialization time.
;;;
(defun user-homedir-pathname (&optional host)
  "Return the home directory of the logged in user as a pathname.  Obtain
   the directory from the logical name \"home:\"."
  (declare (ignore host))
  #p"home:")

;;; File-Kind  --  Public
;;;
(defun file-kind (file &key check-for-links)
  "If $file exists return the kind of $file, which can be :file,
   :directory, :link or :special, otherwise return ()."
  (nth-value 1 (probe-file file :check-for-links check-for-links)))

;;; File-Write-Date  --  Public
;;;
(defun file-write-date (file &key check-for-links)
  "If $file exists return $file's modification date in universal format,
   else return ().  Signal an error of type file-error if $file is a wild
   pathname."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     :format-control "Bad place for a wild pathname.")
      (if (remote-pathname-p file)
	  (multiple-value-bind (reslt err ino mode nlink uid gid rdev size atime mtime)
			       (internet:remote-file-stats file)
	    (declare (ignore err ino mode nlink uid gid rdev size atime))
	    (if reslt (+ unix-to-universal-time mtime)))
	  (let ((name (os-namestring file t)))
	    (when name
	      (multiple-value-bind
		  (res dev-or-err ino mode nlink uid gid rdev size atime mtime)
		  (if check-for-links
		      (unix:unix-lstat name)
		      (unix:unix-stat name))
		(declare (ignore ino mode nlink uid gid rdev size atime))
		(if res
		    (+ unix-to-universal-time mtime)
		    (values () dev-or-err))))))))

;; FIX Warning: Hairy setf expander for function FILE-WRITE-DATE.
;;
(defun set-file-write-date (file date &key check-for-links)
  "Set the modification date of $file to universal format $date.  Signal an
   error of type file-error if $file is a wild pathname."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     :format-control "Bad place for a wild pathname.")
      (if (remote-pathname-p file)
	  (internet:set-remote-write-date file date)
	  (let ((name (os-namestring file t)))
	    (when name
	      ;; FIX how to set time of link? possible?
	      (if (and check-for-links (symlinkp file))
		  ;; Silently succeed.
		  (return-from set-file-write-date t)))
	      (let ((unix-date (- date unix-to-universal-time)))
		(multiple-value-bind (status errno)
				     ;; FIX get usec how?
				     (unix:unix-utimes name
						       unix-date 0
						       unix-date 0)
		  (or status
		      (error 'simple-file-error
			     :pathname file
			     :format-control
			     (format () "Failed to set write date of file ~S: ~A"
				     name
				     (unix:get-unix-error-msg errno))))
		  date))))))
;;
(defsetf file-write-date set-file-write-date)

;; FIX setf?
;;; File-Author  --  Public
;;;
(defun file-author (file)
  "Return the author of $file as a string if author can be determined, else
   ().  Signals an error of type file-error if file doesn't exist, or file
   is a wild pathname."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     "Bad place for a wild pathname.")
      (let ((name (os-namestring (pathname file) t)))
	(or name
	    (error 'simple-file-error
		   :pathname file
		   :format-control "file must exist: ~A."
		   :format-arguments (list file)))
	(multiple-value-bind (winp dev ino mode nlink uid)
			     (unix:unix-stat name)
	  (declare (ignore dev ino mode nlink))
	  (if winp (lookup-login-name uid))))))

;; FIX file-perm
;; FIX check-for-links
;;; File-Mode  --  Public
;;;
(defun file-mode (file)
  "Return the file mode of $file, or () on failure to get the mode.  Signal
   an error of type file-error if $file doesn't exist, or if $file is a
   wild pathname."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     "Bad place for a wild pathname.")
      (let ((name (os-namestring (pathname file) t)))
	(or name
	    (error 'simple-file-error
		   :pathname file
		   :format-control "~S doesn't exist."
		   :format-arguments (list file)))
	(multiple-value-bind (res dev ino mode)
			     (unix:unix-stat name)
	  (declare (ignore dev ino))
	  (if res (logand mode (logior unix:writeall
				       unix:readall
				       unix:execall)))))))

(defun parse-mode-perm (string pos)
  "Return the base permissions (ie as if setting group permissions) defined
   in $string at position $pos."
  (let ((mode 0))
    (loop for char from pos to (1- (length string)) do
      (setq mode (logior mode
			 (ecase (aref string char)
			   (#\r #o4)
			   (#\w #o2)
			   (#\x #o1)))))
    mode))
#| FIX
(parse-mode-perm "wrx" 0)
|#

(defun update-mode (mode update-string &optional (pos 0))
  "Return $mode, updated according to $update-string."
  ;; FIX set*
  (flet ((update-one (multiple mode update-string pos)
	   (multiple-value-bind (mode pos add-p)
				(update-mode mode
					     update-string
					     (+ pos 1))
	     (values
	      (if add-p
		  (logior mode
			  (* multiple
			     (parse-mode-perm update-string pos)))
		  (logand mode
			  ; FIX logcom type error
			  (logxor (* multiple
				     (parse-mode-perm
				      update-string
				      pos))
				  #xffffffff)))
	      pos
	      add-p))))
    (ecase (aref update-string pos)
      (#\+ (values mode (+ pos 1) t))
      (#\- (values mode (+ pos 1)))
      (#\u
       (update-one #o100 mode update-string pos))
      (#\g
       (update-one #o10 mode update-string pos))
      (#\o
       (update-one #o1 mode update-string pos))
      (#\a
       ;; FIX this parses the rest of the string 3 times
       (multiple-value-bind
	   (mode pos2)
	   (update-one #o100 mode update-string pos)
	 (declare (ignore pos2))
	 (multiple-value-bind
	     (mode pos2)
	     (update-one #o10 mode update-string pos)
	   (declare (ignore pos2))
	   (update-one #o1 mode update-string pos)))))))
#| FIX
(update-mode 0 "u+rwx")
(update-mode 0 "g+rwx")
(update-mode 0 "o+rwx")
(update-mode 0 "u+r")
(update-mode 0 "g+r")
(update-mode 0 "ug+r")
(update-mode 0 "u-r")
(update-mode (update-mode 0 "ug+r") "u-wr")
(update-mode 0 "a-r")
(update-mode 0 "a+r")
(update-mode 0 "ugo+r")
|#

(defun set-file-mode (file mode)
  ; FIX add node [doc perm strings]
  "Set the mode of $file to $mode.

   Mode can be a Unix mode number, one of the Unix permission symbols
   described for `unix:unix-chmod', or an update string such as \"a+wr\"."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     "Bad place for a wild pathname.")
      (let ((name (os-namestring (pathname file) t)))
	(or name
	    (error 'simple-file-error
		   :pathname file
		   :format-control "~S must exist."
		   :format-arguments (list file)))
	(when (stringp mode)
	  (multiple-value-bind (success dev ino current-mode)
			       (unix:unix-stat name)
	    (declare (ignore ino))
	    (or success
		(error 'simple-file-error
		       :pathname file
		       :format-control "failed to stat ~S"
		       :format-arguments
		       (list file
			     (unix:get-unix-error-msg dev))))
	    (setq mode (update-mode current-mode mode))))
	(multiple-value-bind (res errno)
			     (unix:unix-chmod name mode)
	  (if res
	      (logand mode (logior unix:execall
				   unix:readall
				   unix:writeall))
	      (error "Error setting mode:~A"
		     (unix:get-unix-error-msg errno)))))))
;;;
(defsetf file-mode set-file-mode)

;; FIX should (length #p"/a/b") call this? or (length (namestring #p"/a/b")
;;; File-Size  --  Public
;;;
(defun file-size (file &key check-for-links)
  "Return the file size of $file, or () on failure.  Signal an error of
   type file-error if file doesn't exist, or file is a wild pathname."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     "Bad place for a wild pathname.")
      (let ((name (os-namestring (pathname file) t)))
	(or name
	    (error 'simple-file-error
		   :pathname file
		   :format-control "File must exist: ~S."
		   :format-arguments (list file)))
	(multiple-value-bind (res dev ino mode nlink uid gid rdev size)
			     (if check-for-links
				 (unix:unix-lstat name)
				 (unix:unix-stat name))
	  (declare (ignore dev ino mode nlink uid gid rdev))
	  (if res size)))))

;;; File-Stats  --  Public
;;;
(defun file-stats (file &key check-for-links)
  "Return information about $file: success-p dev-or-err ino mode nlink uid
   gid rdev size atime mtime namestring." ; FIX explain each
  (multiple-value-bind (success-p dev-or-err ino mode nlink uid
				  gid rdev size atime mtime name)
		       (if (remote-pathname-p file)
			   (internet:remote-file-stats file)
			   ;; Local.
			   (let ((namestring (os-namestring file)))
			     (if namestring
				 (multiple-value-bind
				     (success-p dev-or-err ino mode nlinks
						uid gid rdev size atime mtime)
				     (if check-for-links
					 (unix:unix-lstat namestring)
					 (unix:unix-stat namestring))
				   (values success-p dev-or-err ino mode nlinks
					   uid gid rdev size atime mtime
					   namestring)))))
    (values success-p dev-or-err ino mode nlink uid gid rdev size atime mtime name)))


;;;; Directory enumeration.

;;; Public.
;;;
;;; FIX very big macro  flet and pass name to %do-files
;;; FIX check for cycles when follow-links and recurse
;;; FIX name  does dirs too  do-ents do-entities do-entries
;;;
(defmacro do-files ((name dir-pathname
		     &key (all t) (backups t) (check-for-subdirs t)
		     follow-links recurse)
		    &body body)
  ;; FIX return the return of body on the final file
  ;; FIX should be able to return from do-files with (return )
  "Evaluate $body with $name bound to each entity (file or directory) in
   $dir-pathname.

   Keys:
      $all           if true include hidden files (files starting with a .)
      $backups       if true include backup files (files ending in: ~ BAK CKP)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories (FIX can cycle)."
  (let* ((pn (gensym))
	 (slash (gensym))
	 (file (gensym))
	 (dir (gensym)))
    `(iterate do-dir ((,dir ,dir-pathname))
       (enumerate-search-list
	(,pn (merge-pathnames (directory-namestring ,dir)
			      (make-pathname
			       :name (or (pathname-name ,dir) :wild)
			       :type (or (pathname-type ,dir) :wild)
			       :version (or (pathname-version ,dir) :wild))))
	(enumerate-matches (,name ,pn () :follow-links ,follow-links
				  :check-for-subdirs ,check-for-subdirs)
	  ;; Depth first. FIX should do the dir name first?
	  (if (and ,recurse (eq (unix:unix-file-kind (namify ,name)
						     (fi ,follow-links))
				:directory))
	      (do-dir (concatenate 'string ,name "/")))
	  (cond ((and ,backups ,all)
		 ,@body)
		(,backups
		 ;; Filter out hidden files.
		 (let ((,slash (position #\/ (namify ,name) :from-end t)))
		   (when (if ,slash
			     (or (= (1+ ,slash) (length ,name))
				 (fi (char= (schar ,name (1+ ,slash)) #\.) t))
			     (fi (char= (schar ,name 0) #\.) t))
		     ,@body)))
		(,all
		 ;; Filter out backup files.
		 (or (let ((,name (if (and ,check-for-subdirs
					   (eq (unix:unix-file-kind ,name t)
					       :directory))
				     ;; Strip off the slash.  Assume
				     ;; name is at least one char.
				     (subseq ,name 0 (1- (length ,name)))
				     ,name)))
		       (or (char= (schar ,name (1- (length ,name))) #\~)
			   (string= (pathname-type ,name) "BAK")
			   (string= (pathname-type ,name) "CKP")
			   (string= (pathname-version ,name) "BAK")
			   (string= (pathname-version ,name) "CKP")))
		     (progn
		       ,@body)))
		(t
		 ;; Filter out both backup and hidden files.
		 (let ((,file (if (and ,check-for-subdirs
				       (eq (unix:unix-file-kind ,name t)
					   :directory))
				  ;; Strip off the slash.  Assume name
				  ;; is at least one char.
				  (subseq ,name 0 (1- (length ,name)))
				  ,name)))
		   (or (char= (schar ,file (1- (length ,file))) #\~)
		       (string= (pathname-type ,file) "BAK")
		       (string= (pathname-type ,file) "CKP")
		       (string= (pathname-version ,file) "BAK")
		       (string= (pathname-version ,file) "CKP")
		       (let ((,slash (position #\/ ,file :from-end t)))
			 (when (if ,slash
				   (or (= (1+ ,slash) (length ,file))
				       (fi (char= (schar ,file (1+ ,slash)) #\.) t))
				   (fi (char= (schar ,file 0) #\.) t))
			   ,@body)))))))))))

;;; Public.
;;;
;;; FIX check for cycles when follow-links and recurse
;;;
(defmacro do-dirs ((name dir-pathname
			 &key
			 (all t) (backups t) (check-for-subdirs t)
			 follow-links recurse)
		   &body body)
  "Evaluate $body with $name bound to each directory in $dir-pathname.

   The directory specified in $dir-pathname must end in a slash, and may be
   followed by a file name.

   Keys:
      $all           if true include hidden files (files starting with a .)
      $backups       if true include backup files (files ending in: ~ BAK CKP)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories (FIX can cycle)."
  (let ((pn (gensym))
	(slash (gensym))
	(dir (gensym)))
    `(iterate do-dir ((,dir ,dir-pathname))
       (enumerate-search-list
	(,pn (merge-pathnames (directory-namestring ,dir)
			      (make-pathname :name :wild
					     :type :wild
					     :version :wild)))
	(enumerate-matches (,name ,pn ()
				  :follow-links ,follow-links
				  :check-for-subdirs ,check-for-subdirs)
	  (when (eq (unix:unix-file-kind ,name) :directory)
	    ,(cond
	      ((and backups all)
	       `(progn ,@body))
	      (backups
	       `(let ((,slash (position #\/ ,name :from-end t)))
		  (when (if ,slash
			    (or (= (1+ ,slash) (length ,name))
				(fi (char= (schar ,name (1+ ,slash)) #\.) t))
			    (fi (char= (schar ,name 0) #\.) t))
		    ,@body)))
	      (all
	       `(or (char= (schar ,name (1- (length ,name))) #\~)
		    (string= (pathname-type ,name) "BAK")
		    (string= (pathname-type ,name) "CKP")
		    (string= (pathname-version ,name) "BAK")
		    (string= (pathname-version ,name) "CKP")
		    (progn
		      ,@body)))
	      (t
	       `(or (char= (schar ,name (1- (length ,name))) #\~)
		    (string= (pathname-type ,name) "BAK")
		    (string= (pathname-type ,name) "CKP")
		    (string= (pathname-version ,name) "BAK")
		    (string= (pathname-version ,name) "CKP")
		    (let ((,slash (position #\/ ,name :from-end t)))
		      (when (if ,slash
				(or (= (1+ ,slash) (length ,name))
				    (fi (char= (schar ,name (1+ ,slash)) #\.) t))
				(fi (char= (schar ,name 0) #\.) t))
			,@body)))))
	    ;; FIX only recurse into dir if file name matched?
	    ;; Depth first.
	    (if (and ,recurse
		     (if ,follow-links
			 t
			 (eq (unix:unix-file-kind (namify ,name) t)
			     :directory)))
		(do-dir (concatenate 'string ,name "/")))))))))

;;; Internal.
;;;
(defun enumerate-names (pathname &optional all follow-links backups recurse
				 check-for-subdirs)
  "Return a list of pathnames for $pathname.

   Options:
      $all           if true include hidden files (files starting with a .)
      $backups       if true include backup files (files ending in: ~ BAK CKP)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories (FIX can cycle)."
  (let (results)
    (do-files (name pathname
		    :all all
		    :follow-links follow-links
		    :backups backups
		    :recurse recurse
		    :check-for-subdirs check-for-subdirs)
      (push name results))
    results))

;;; Public.
;;;
(defun map-files (pathname function
		  &key (all t) (backups t) (check-for-subdirs t)
		  follow-links recurse)
  "Call $function on each entiry (file or directory) that matches
   $pathname.

   Keys:
      $all           if true include hidden files (files starting with a .)
      $backups       if true include backup files (files ending in: ~ BAK CKP)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories.

   The macro `do-files' is a much faster alternative."  ;; FIX is that because do-files is very big?
  (do-files (name pathname
		  :all all
		  :follow-links follow-links
		  :backups backups
		  :recurse recurse
		  :check-for-subdirs check-for-subdirs)
    (funcall function name)))

;;; Public.
;;;
(defun map-dirs (pathname function
			  &key (all t) (backups t) (check-for-subdirs t)
			  follow-links recurse)
  "Call $function on each directory in $pathname.

   The directory specified in $pathname must end in a slash, and may be
   followed by a file name.

   Keys:
      $all           if true include hidden files (files starting with a .)
      $backups       if true include backup files (files ending in: ~ BAK CKP)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories.

   The macro `do-dirs' is a much faster alternative."
  (do-dirs (directory pathname
		      :all all
		      :backups backups
		      :check-for-subdirs check-for-subdirs
		      :follow-links follow-links
		      :recurse recurse)
    (funcall function directory)))

;;; Public.
;;;
(defun list-files (pathname &optional (predicate #'identity)
			    &key
			    (all t) (backups t)
			    (check-for-subdirs t)
			    follow-links
			    recurse)
  "Return a list of all entities (files and directories) in $pathname that
   match $predicate.

   Keys:
      $all           if true include hidden files (files starting with a .)
      $backups       if true include backup files (files ending in a ~)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories.

   Call $predicate on a single argument, the file name."
  (collect ((results))
    (do-files (file pathname
		    :all all
		    :backups backups
		    :check-for-subdirs check-for-subdirs
		    :follow-links follow-links
		    :recurse recurse)
      (if (funcall predicate file)
	  (results file)))
    (results)))

;;; Public.
;;;
(defun list-dirs (pathname &optional (predicate #'identity)
			   &key
			   (all t) (backups t)
			   (check-for-subdirs t)
			   follow-links
			   recurse)
  "Return a list of all directories in $pathname that match $predicate.

   The directory specified in $pathname must end in a slash, and may be
   followed by a file name.

   Keys:
      $all           if true include hidden dirs (dirs starting with a .)
      $backups       if true include backup dirs (dirs ending in a ~)
      $check-for-subdirs  if true append a slash ('/') to directory names.
      $follow-links  if true follow symbolic links
      $recurse       if true recurse into subdirectories.

   Call $predicate on a single argument, the directory name."
  (collect ((results))
    (do-dirs (dir pathname
		  :all all
		  :backups backups
		  :check-for-subdirs check-for-subdirs
		  :follow-links follow-links
		  :recurse recurse)
      (if (funcall predicate dir)
	  (results dir)))
    (results)))

;;; Public.
;;;
;;; FIX check-for-links vs follow-links
;;;
(defun directory (&optional pathname
		       &key (all t) (backups t)
		            (check-for-subdirs t)
			    (absolute t) (truenamep t)
			    recurse (follow-links t))
  "Return a list of pathnames, one for each file that matches $pathname.

   Throw a file error if the directory named by the common prefix (the part
   before the first wildcard) of $pathname is missing.

   The keyword arguments have the following functions:

     $all
         Include files beginning with dot, such as \".login\", similar to
         Unix \"ls -a\".  Always leave out Unix dot and dot-dot.

     $backups
         If true then also list backup files (files ending in ~ or with
         case-folded extensions BAK or CKP).

     $check-for-subdirs
         If true then append a slash ('/') to directory names.  Overridden
         if $truenamep is true (the truename of a directory always has the
         slash).

     $recurse
         If set then include pathnames of subdirectories, recursively.

     $follow-links
         If true, then recurse into symbolic links to directories when
         $recurse is t, otherwise treat symbolic links as files.

     $truenamep
         Call `truename' on each element of the result.  This converts each
         resulting entity to an absolute pathname (overriding $absolute)
         with all symbolic links expanded out.  This option can easily
         result in pathnames being returned are in a directory other than
         directory specified by $pathname.

     $absolute
         If true then return absolute pathnames, otherwise return pathnames
         relative to the current directory.  Overridden if $truename is
         true."
  ;; FIX absolute ()
  (let ((pathname (or pathname (current-directory))))
    (or (probe-file (common-prefix (directory-namestring pathname)))
	(error 'simple-file-error
	       :pathname pathname
	       :format-control
	       "$pathname must name an existing directory: ~S."
	       :format-arguments (list pathname)))
    (let* ((*ignore-wildcards* ())
	   (merge-dir (fi truenamep
			  (if absolute
			      (current-directory)
#| FIX was
                              (truename
			       (common-prefix
				(directory-namestring
				 pathname)))
|#
			  ))))
      ;; FIX Surely this is a slow way.  rather use do-files directly?
      (mapcar #'(lambda (name)
		  (if truenamep
		      (truename name)
		      (if absolute
			  (merge-pathnames name merge-dir)
			  (pathname name))))
	      ;; FIX when would there be duplicates?
	      (sort (delete-duplicates (enumerate-names pathname
							all follow-links
							backups recurse
							check-for-subdirs)
				       :test #'string=)
		    #'string<)))))

;;; Public.
;;;
(defmacro in-directory (directory &body forms)
  "Make $directory current for the duration of $forms."
  (let ((cwd (gensym)))
    `(let ((,cwd (ext:current-directory)))
       (unwind-protect
	   (progn
	     (setf (ext:current-directory) (directory-namestring ,directory))
	     ,@forms)
	 (setf (ext:current-directory) ,cwd)))))


;;;; Printing directories.

;;; PRINT-DIRECTORY is exported from EXTENSIONS.
;;;
;;; FIX Should this have :follow-links?  At least for :recurse?
;;;
(defun print-directory (&optional pathname stream
				  &key all (backups t)
				       (check-for-subdirs t)
				       coldefs recurse return-list verbose)
  "Print a terse, multi-column wildname listing of directory $pathname to
   $stream.  The listing is sorted by pathname with `string<'.

   If $return-list if true then return a list of the matched pathnames,
   else return ().

   Other keywords:

     $all
         If true then include Unix dot files (as with Unix \"ls -a\").

     $backups
         If true then include backup files (files ending in ~, or with
         case-folded extensions BAK or CKP).

     $check-for-subdirs
         If true then append a slash ('/') to directory names.

     $coldefs
         If true output in the column format described by $coldefs, else in
         the usual column format.  [FIX] doc these.

     $recurse
         If true then recurse into directories.

     $verbose
         If true, then output a long listing of miscellaneous information,
         one file per line."
  (let* ((*standard-output* (out-synonym-of stream))
	 (pathname (or pathname (current-directory)))
	 (contents (sort (delete-duplicates
			  (list-files pathname #'identity
				      :all all
				      :check-for-subdirs check-for-subdirs
				      :follow-links ()  :backups backups
				      :recurse recurse))
			 #'string<)))
    (if verbose
	(print-directory-verbose pathname contents all return-list coldefs)
	(print-directory-formatted pathname contents all return-list))))

;; FIX format-files? print sounds like printer
;;; PRINT-FILES is exported from EXTENSIONS.
;;;
(defun print-files (pathname files
			     &optional stream
			     &key verbose return-list coldefs)
  "Print a terse, multi-column wildname listing of $files to $stream.  That
   is, print the list of $files in the same way `print-directory' prints a
   pathname.

   The files in $files can be absolute or relative.  Relative files are
   relative to $pathname.

   $files may be a list of lists, where the first element of each list is
   the file name and the rest of the list is extra information (typically
   used with column definitions from $coldefs).

   Keywords:

     $coldefs
         If true output in the column format described by $coldefs, else in
         the usual column format.  [FIX] doc these.

     $return-list
         If true then return a list of the matched pathnames, else return
         ().

     $verbose
         If true, then output a long listing of miscellaneous information,
         one file per line."
  ;; FIX make the header line of the listing optional and configurable
  (let ((*standard-output* (out-synonym-of stream)))
    (if verbose
	(print-directory-verbose pathname files () return-list coldefs)
	(print-directory-formatted pathname files () return-list))))

;;; PRINT-MODE is internal.
;;;
(defun print-mode (mode)
  (macrolet ((frob (bit name &optional sbit sname negate)
	       `(if ,(if negate
			 `(not (logbitp ,bit mode))
			 `(logbitp ,bit mode))
		    ,(if sbit
			 `(if (logbitp ,sbit mode)
			      (write-char ,sname)
			      (write-char ,name))
			 `(write-char ,name))
		    (write-char #\-))))
    (frob 15 #\d () () t)
    (frob 8 #\r)
    (frob 7 #\w)
    (frob 6 #\x 11 #\s)
    (frob 5 #\r)
    (frob 4 #\w)
    (frob 3 #\x 10 #\s)
    (frob 2 #\r)
    (frob 1 #\w)
    (frob 0 #\x)))

;;; PARSE-MODE-STRING is public.
;;;
(defun parse-mode-string (string)
  "Return the mode (an integer) described by $string.

   Allow trailing junk in $string."
  (let ((mode 0))
    (fi (>= (length string) 10)
	(error "Mode $string must be at least 10 characters.")
	(progn
	  (setq string (string-downcase string))
	  (macrolet ((frob (pos bit name &optional sbit sname)
		       (if sbit
			   `(let ((ch (char string ,pos)))
			      (fi (char= ch #\-)
				  (if (char= ch ,name)
				      (setf (logbitp ,bit mode) 1)
				      (if (char= ch ,sname)
					  (progn
					    (setf (logbitp ,bit mode)
						  1)
					    (setf (logbitp ,sbit mode)
						  1))
					  (error "Char ~D must be ~C, ~C or -: ~C"
						 ,pos ,name ,sname ch)))))
			   `(let ((ch (char string ,pos)))
			      (if (char= ch ,name)
				  (setf (logbitp ,bit mode) 1)
				  (or (char= ch #\-)
				      (error "Char ~D must be ~C or -: ~C"
					     ,pos ,name ch)))))))
	    (let ((ch (char string 0)))
	      (if (char= ch #\-)
		  (setf (logbitp 15 mode) 1)
		  (if (char= ch #\d)
		      (setf (logbitp 14 mode) 1)
		      (error "Char 0 must be d or -: ~C" ch))))
	    (frob 1 8 #\r)
	    (frob 2 7 #\w)
	    (frob 3 6 #\x 11 #\s)
	    (frob 4 5 #\r)
	    (frob 5 4 #\w)
	    (frob 6 3 #\x 10 #\s)
	    (frob 7 2 #\r)
	    (frob 8 1 #\w)
	    (frob 9 0 #\x 15 #\t))))
    mode))

(defun print-directory-verbose (pathname contents all return-list coldefs)
  (declare (ignore all))
  (let ((result)
	(col-posns)
;	(root-len (length (namestring pathname)))
	)
    (format t "v Directory of ~A :~%" (namestring pathname))
    (in-directory pathname
      (if coldefs
	  ;;; Print according to column definitions.
	  (let ((pos 0)
		(first t))
	    (or (listp coldefs) (error "COLDEFS must be a list."))
	    (dolist (file-or-more contents)
	      (let* ((file (if (listp file-or-more)
			       (car file-or-more)
			       file-or-more)))
		(multiple-value-bind
		    (reslt dev-or-err ino mode nlink uid gid rdev size atime mtime namestring)
		    (file-stats file)
		  (declare (ignore ino gid rdev atime namestring)
			   (fixnum uid mode))
		  (let ((tail (subseq file 0)))
		    (fi reslt
			(progn
			  (format t "Failed to stat ~A -- ~A.~%"
				  tail
				  (if dev-or-err (unix:get-unix-error-msg dev-or-err) ""))
			  (if return-list (push () result)))
			(progn
			  (dolist (coldef coldefs)
			    (typecase coldef
			      (string
			       (princ coldef)
			       (when first
				 (push `(,coldef . ,pos) col-posns)
				 (incf pos (length coldef))))
			      (number
			       ;; Assume file-or-more is a list.
			       (princ (nth coldef file-or-more))
			       (when first
				 (push `(,coldef . ,pos) col-posns)
				 (incf pos (length (nth coldef file-or-more)))))
			      (keyword
			       (case coldef
				 (:mode  (print-mode mode)
					 (when first
					   (push `(:mode . ,pos) col-posns)
					   (incf pos 10)))
				 (:nlink (format t "~2D" nlink)
					 (when first
					   (push `(:nlink . ,pos) col-posns)
					   (incf pos 2)))
				 (:uid   (format t "~8A"
						 (or (lookup-login-name uid) uid))
					 (when first
					   (push `(:uid . ,pos) col-posns)
					   (incf pos 8)))
				 (:size  (format t "~8D" size)
					 (when first
					   (push `(:size . ,pos) col-posns)
					   (incf pos 8)))
				 (:date  (format t "~12A"
						 (multiple-value-bind (sec min hr date month year day ds tz)
								      (get-decoded-time)
						   (declare (ignore sec min hr date month day ds))
						   (decode-universal-time-for-files mtime year tz)))
					 (when first
					   (push `(:date . ,pos) col-posns)
					   (incf pos 12)))
				 (:name
				  (if first (push `(:name . ,pos) col-posns))
				  (let ((name (if (= (logand mode unix:s-ifmt) unix:s-iflnk)
						  ;; FIX could print min part of link path
						  (let ((link (symlink-dest (namify file))))
						    (format () "~A --> ~A"
							    ;(subseq (namestring file) root-len)
							    (namify file)
							    link))
						  (format () "~A"
							  ;(subseq (namestring file) root-len)
							  file))))
				    (if first (incf pos (length name)))
				    (format t name)))))))
			  (terpri)
			  (when return-list
			    (push tail result)))))))
	      (setq first ())))
	  ;;; Print the usual column format directly.
	  (progn
	    (setq col-posns
		  '((:mode . 0) (:links . 12) (:user . 14) (:size . 23)
		    (:date . 32) (:name . 45)))
	    (dolist (file contents)
	      (multiple-value-bind
		  (reslt dev-or-err ino mode nlink uid gid rdev size atime mtime namestring)
		  (file-stats file :check-for-links t)
		(declare (ignore ino gid rdev atime)
			 (type (or fixnum null) uid mode))
		(fi reslt
		    (progn
		      (if return-list (push () result))
		      (format t "Failed to stat ~A -- ~A.~%"
			      file
			      (if dev-or-err (unix:get-unix-error-msg dev-or-err) "")))
		    (let ((tail file)) ; FIX (and namestring (file-namestring namestring))))
		      ;;
		      ;; Print characters for file modes.
		      (print-mode mode)
		      ;;
		      ;; Print the rest.
		      (multiple-value-bind (sec min hour date month year day ds tz)
					   (get-decoded-time)
			(declare (ignore sec min hour date month day ds))
			(format t " ~2D ~8A ~8D ~12A "
				nlink
				(or (lookup-login-name uid) uid)
				size
				(decode-universal-time-for-files mtime year tz))
			(if (eq (file-kind file :check-for-links t) :link)
			    ;; FIX could print min part of link path
			    (let ((link (symlink-dest (namify namestring))))
			      (format t "~A --> ~A~%"
				      (namify tail) link))
			    (format t "~A~%" tail)))
		      (when return-list
			(push tail result)))))))))
    (values (nreverse result) col-posns)))

(defun decode-universal-time-for-files (time current-year tz)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time (+ time unix-to-universal-time)
					      (- (* tz 60)))
    (declare (ignore sec))
    (format () "~A ~2,' D ~:[ ~D~;~*~2,'0D:~2,'0D~]"
	    (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
		      "Sep" "Oct" "Nov" "Dec")
		   (1- month))
	    day (= current-year year) year hour min)))

(defun print-directory-formatted (pathname result all return-list)
  (declare (ignore all) (list result))
  (let ((width (or (line-length *standard-output*) 80))
	(cnt 0)
	(max-len 0)
	; FIX
	;(remote-p (remote-pathname-p pathname))
	)
    (declare (fixnum max-len cnt))
    ;;
    ;; Analyse the data.
    (dolist (file result)
      ;; FIX Why is `os-namestring' needed? (breaks `print-directory-80')
      (let* ( ;(name (if remote-p file (os-namestring file)))
	     (len (length (namestring file))))
	(declare (fixnum len))
	(if (> len max-len) (setq max-len len))
	(incf cnt)))
    ;;
    ;; Output.
    ;;
    ;; Change to the directory first, to be sure that it exists.
    (in-directory pathname
      (format t "Directory of ~A :~%" (namestring pathname))
      (when (plusp cnt)
	(let* ((col-width (1+ max-len))
	       (cols (max (truncate width col-width) 1))
	       (lines (ceiling cnt cols))
	       (cols (if (zerop lines) cols (ceiling cnt lines))))
	  (declare (fixnum cols lines))
	  (dotimes (i lines)
	    (declare (fixnum i))
	    (dotimes (j cols)
	      (declare (fixnum j))
	      (decf cnt)
	      (let ((name (nth (+ i (the fixnum (* j lines))) result)))
		(when name
		  (let ((name (namestring name)))
		    (write-string name)
		    (or (eql j (1- cols))
			(zerop cnt)
			(dotimes (i (- col-width (length (the simple-string name))))
			  (write-char #\space)))))))
	    (terpri)))
	(when return-list
	  result)))))


;;;; Translating uid's and gid's.

(defvar *uid-hash-table* (make-hash-table)
  "Hash table for keeping track of uid's and login names.")

;;; LOOKUP-LOGIN-NAME translates a user id into a login name.  Previous
;;; lookups are cached in a hash table since groveling the passwd(s) files
;;; is somewhat expensive.  The table may hold () for id's that cannot
;;; be looked up since this means the files are searched in their entirety
;;; each time this id is translated.
;;;
(defun lookup-login-name (uid)
  (multiple-value-bind (login-name foundp) (gethash uid *uid-hash-table*)
    (if foundp
	login-name
	(setf (gethash uid *uid-hash-table*)
	      (get-group-or-user-name :user uid)))))

(defvar *gid-hash-table* (make-hash-table)
  "Hash table for keeping track of gid's and group names.")

;;; LOOKUP-GROUP-NAME translates a group id into a group name.  Previous
;;; lookups are cached in a hash table since groveling the group(s) files
;;; is somewhat expensive.  The table may hold () for id's that cannot
;;; be looked up since this means the files are searched in their entirety
;;; each time this id is translated.
;;;
(defun lookup-group-name (gid)
  (multiple-value-bind (group-name foundp) (gethash gid *gid-hash-table*)
    (if foundp
	group-name
	(setf (gethash gid *gid-hash-table*)
	      (get-group-or-user-name :group gid)))))

(defun get-group-or-user-name (group-or-user id)
  "Returns the simple-string user or group name of the user whose uid or
   gid is id, or () if no such user or group exists.  Group-or-user is
   either :group or :user."
  (let ((id-string (let ((*print-base* 10)) (prin1-to-string id))))
    (declare (simple-string id-string))
    (ecase group-or-user
      (:user
       (unix::unix-user-name id))
      (:group
       (or (get-group-or-user-name-aux id-string "/etc/group")
	   (get-group-or-user-name-aux id-string "/etc/groups"))))))

(defun get-group-or-user-name-aux (id-string passwd-file)
  (with-open-file (stream passwd-file)
    (loop
      (let ((entry (read-line stream ())))
	(unless entry (return ()))
	(let ((name-end (position #\: (the simple-string entry)
				  :test #'char=)))
	  (when name-end
	    (let ((id-start (position #\: (the simple-string entry)
				      :start (1+ name-end) :test #'char=)))
	      (when id-start
		(incf id-start)
		(let ((id-end (position #\: (the simple-string entry)
					:start id-start :test #'char=)))
		  (when (and id-end
			     (string= id-string entry
				      :start2 id-start :end2 id-end))
		    (return (subseq entry 0 name-end))))))))))))


;;;; File completion.

;;; COMPLETE-FILE -- Public
;;;
;;; FIX (current-directory) was *default-pathname-defaults*
;;;
(defun complete-file (pathname &key (directory (current-directory))
			       ignore-types)
  "Attempt to complete $pathname to the longest unique prefix for a file or
   directory name.

   On success, return the longest prefix, otherwise return ().  Return a
   second value #t if the prefix exists and is unique, () otherwise.  A
   unique prefix is always a complete pathname.

   If supplied, use $directory as the \"working directory\" when doing
   completion.

   Skip over any pathnames that have types (a.k.a. extensions) that are in
   the list of strings $ignore-types.

   Always use the name of a symbolic link, instead of following the link
   and using the name of the destination.

   Signal an error if $pathname is wild or missing.

   For the name completion, treat directory names like file names.  So, for
   an empty directory /dir/, return /dir and #t if $pathname is /dir, and
   return () and () if $pathname is /dir/.  And for a directory /dir2/
   which contains files a and b, return /dir2 and #t if $pathname is /dir2,
   and /dir2/ and () if $pathname is /dir2/."
  (let ((files (directory (complete-file-directory-arg pathname directory)
			  :check-for-subdirs ()
			  :truenamep ()
			  :absolute ())))
    (cond ((null files)
	   (values () ()))
	  ((null (cdr files))
	   (values (merge-pathnames (file-namestring (car files))
				    pathname)
		   t))
	  (t
	   (let ((good-files
		  (delete-if #'(lambda (pathname)
				 (and (simple-string-p
				       (pathname-type pathname))
				      (member (pathname-type pathname)
					      ignore-types
					      :test #'string=)))
			     files)))
	     (cond ((null good-files)
		    (return-from complete-file (values () ())))
		   ((null (cdr good-files))
		    (return-from complete-file
				 (values (merge-pathnames (file-namestring
							   (car good-files))
							  pathname)
					 t)))
		   (t
		    (setf files good-files)))
	     (let ((common (file-namestring (car files))))
	       (dolist (file (cdr files))
		 (let ((name (file-namestring file)))
		   (dotimes (i (min (length common) (length name))
			       (when (< (length name) (length common))
				 (setf common name)))
		     (unless (char= (schar common i) (schar name i))
		       (setf common (subseq common 0 i))
		       (return)))))
	       (values (merge-pathnames common pathname)
		       ())))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory-arg (pathname defaults)
  (let* ((pathname (merge-pathnames pathname (directory-namestring defaults)))
	 (type (pathname-type pathname)))
    (flet ((append-multi-char-wild (thing)
	     (etypecase thing
	       (null :wild)
	       (pattern
		(make-pattern (append (pattern-pieces thing)
				      (list :multi-char-wild))))
	       (simple-string
		(make-pattern (list thing :multi-char-wild))))))
      (if (or (null type) (eq type :unspecific))
	  ;; There is no type.
	  (make-pathname :defaults pathname
	    :name (append-multi-char-wild (pathname-name pathname))
	    :type :wild)
	  ;; There already is a type, so just extend it.
	  (make-pathname :defaults pathname
	    :name (pathname-name pathname)
	    :type (append-multi-char-wild (pathname-type pathname)))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
			&optional (defaults *default-pathname-defaults*))
  "Return a list of all pathnames which are possible completions of
   $pathname.

   Look in the directory specified by $defaults as well as down the search
   list. FIX what search list?

   Always return absolute pathnames.  Check symlinks instead of checking
   destinations of symlinks.  Expand search lists in the result.

   Name directories like files and expect the same for $pathname.  That is,
   leave the trailing slashes off directory names.  For example, given an
   empty directory named /dir, return /dir when $pathname is /dir and ()
   when $pathname is /dir/.  In the same way, given a directory /dir
   containing a file a, return /dir when $pathname is /dir and '(a) when
   $pathname is /dir/.

   Signal an error if $pathname is wild."
  (if (probe-file (common-prefix (directory-namestring pathname)))
      (directory (complete-file-directory-arg pathname defaults)
		 :truenamep ()
		 :check-for-subdirs ())))


;;; File-writable -- exported from extensions.
;;;
(defun file-writable (pathname)
  "Return #t if the current user can write to $pathname, else ().

   If $pathname is a symbolic link then check the destination of the link.

   If $pathname is missing, return ()."
  (if (and (lisp::extract-search-list pathname ())
	   (fi (search-list-defined-p pathname)))
      (let ((protocol
	     (lisp::search-list-name
	      (lisp::extract-search-list (pathname pathname) ()))))
	(case= protocol
	  (t
	   ;;; Try connect to a host named like the search list.
	   (let ((account (internet:make-inet-account protocol)))
	     (internet:fill-from-netrc account)
; 	     (if (internet:ftp-probe-file account (host-path pathname))
; 		 pathname)
	     ;; FIX check access
	     ;; (internet:remote-file-writable file)?
	     t))))
      (let ((pathname (os-namestring pathname ())))
	(cond ((null pathname)
	       ())
	      ((unix:unix-file-kind pathname)
	       (values (unix:unix-access pathname unix:w_ok)))
	      (t
	       (values
		(unix:unix-access (subseq pathname
					  0
					  (or (position #\/ pathname :from-end t)
					      0))
				  (logior unix:w_ok unix:x_ok))))))))


;;; Pathname-Order  --  Internal
;;;
(defun pathname-order (x y)
  "Predicate to order pathnames by.  Goes by name."
  (let ((xn (%pathname-name x))
	(yn (%pathname-name y)))
    (if (and xn yn)
	(let ((res (string-lessp xn yn)))
	  (cond ((not res) ())
		((= res (length (the simple-string xn))) t)
		((= res (length (the simple-string yn))) ())
		(t t)))
	xn)))


(defvar *current-directory*)

;;; Current-Directory  --  Public
;;;
(defun current-directory ()
  "Return the pathname for the current working directory.

   This may be changed with setf."
  *current-directory*)

(defun current-unix-directory ()
  (multiple-value-bind (gr dir-or-error)
		       (unix:unix-current-directory)
    (if gr
	(let ((*ignore-wildcards* t))
	  (pathname (concatenate 'simple-string dir-or-error "/")))
	(error dir-or-error))))

;;; %Set-Current-Directory  --  Internal
;;;
(defun %set-current-directory (new-val)
  (let ((new-val (merge-pathnames new-val (current-directory))))
    (if (remote-pathname-p new-val)
	(setq *current-directory* new-val)
	;; FIX Should these be file errors?
	(let ((namestring (os-namestring new-val t)))
	  (or namestring (error "~S doesn't exist." new-val))
	  (multiple-value-bind (gr error)
			       (unix:unix-chdir namestring)
	    (if gr
		(setq *current-directory* namestring)
		(error (format () "~A: \"~A\""
			       (unix:get-unix-error-msg error)
			       namestring))))))
    new-val))
;;;
(defsetf current-directory %set-current-directory)

(defun filesys-init ()
  (setf *default-pathname-defaults*
	(%make-pathname *unix-host* () () () () :newest))
  (setq *current-directory* (current-unix-directory))
  ())


;;; Public
;;;
;;; FIX ensure-dirs  ensure-path (also touches any trailing filename)
;;;
(defun ensure-directories-exist (pathname)
  "Ensure that all the directories in $pathname exist.  Directories are
   created readable, writable and searchable by all.  Only create the first
   of the possible expansions if $pathname contains a search list.  On
   success return t, else return $pathname and a string describing the
   error."
  (if (wild-pathname-p pathname) (error "Wild pathname given."))
  (let ((dir "/") (result pathname) err)
    (loop
      for parts =
      (cdr (pathname-directory
	    (if (remote-pathname-p pathname)
		(progn
		  (setq dir (concat (remote-pathname-host pathname) ":"))
		  (remote-pathname-local pathname))
		(enumerate-search-list
		 (path (merge-pathnames pathname
					;; FIX was unix-cur-dir
					(current-directory)))
		 (return path)))))
      then (cdr parts) while parts do
      (setq dir (concatenate 'simple-string dir
			     (if (eq (car parts) :up) ".." (car parts))
			     "/"))
      (or (probe-file dir)
	  ;; FIX error handling (in general)
	  (progn
	    (handler-case
	      (add-dir dir)
	    (error (condition)
	       (setq result () err (format () "~A" condition))
	       ()))
	    (if err (return)))))
    (values result err)))


;;; FIX mv to pathname?

;;; Public implementation dependent slime.
;;;
;;; FIX dirify?
;;;
(defun directorify (pathname)
  "Return a pathname like $pathname, ensuring that the returned pathname
   ends in a slash if $pathname names a directory.

   If $pathname is a symlink then check whether the destination of the link
   is a directory, still returning a pathname like $pathname.

   This is similar to `ensure-trailing-slash', which always adds the
   trailing slash."
  (if (remote-pathname-p pathname)
      pathname
      (let ((directory (ext:os-namestring (common-prefix pathname))))
	(if directory
	    (if (directory-name-p directory)
		directory
		(if (directoryp directory)
		    (pathname (concatenate 'simple-string (namestring directory) "/"))
		    directory))
	    pathname))))

;; FIX rename dir-namify ensure-trailing-/ ensure-/
;; FIX another like this somewhere?
(defun ensure-trailing-slash (pathname)
  "Return a string for $pathname, ensuring that the string ends in a
   backslash.

   This is similar to `directorify', which only adds the trailing slash if
   $pathname names an existing directory."
  (let* ((string (namestring pathname))
	 (last (1- (length string))))
    (fi (char= (schar string last) #\/)
	(concat string "/")
	string)))

;;; Public implementation dependent slime.
;;;
(defun namify (pathname)
  "Return a string for $pathname, stripping off any trailing slash if
   $pathname is a directory name."
  (let ((string (namestring pathname)))
    (if (zerop (length string))
	""
	(let ((last (1- (length string))))
	  (if (char= (schar string last) #\/)
	      (subseq string 0 last)
	      string)))))

;;; Public
;;;
(defun directory-name-p (pathname)
  "Return #t if $pathname is a directory name, that is, if the name and
   type components are ()."
  (and (eq (pathname-name pathname) ())
       (eq (pathname-type pathname) ())))

;;; Public
;;;
(defun directoryp (pathname &key check-for-links)
  "Return true if $pathname names a directory, that is, if the file named
   by $pathname exists as a directory."
  (if (remote-pathname-p pathname)
      ;; FIX remote-file-kind? build remote into file-kind?
      (multiple-value-bind (success-p kind)
			   (probe-file pathname
				       :check-for-links check-for-links)
	(and success-p (eq kind :directory)))
      (eq (file-kind (or (os-namestring pathname)
			 (return-from directoryp ()))
		     :check-for-links check-for-links)
	  :directory)))

;;; Public
;;;
(defun file-name-p (pathname)
  "Return #t if $pathname is a file name (vs a directory name), that is,
   return #t if any of the name, type or version components of $pathname
   are set."
  (if (or (pathname-name pathname)
	  (pathname-type pathname)
	  (pathname-version pathname))
      t))

;;; Public
;;;
(defun filep (pathname &key check-for-links)
  "Return #t if $pathname names a file (vs a directory), that is if the
   file named by $pathname exists as a file (including special files and,
   if $check-for-links is true, symbolic links)."
  (if (if check-for-links
	  (member (file-kind pathname :check-for-links t)
		  '(:file :special :link))
	  (member (file-kind pathname) '(:file :special)))
      t))

;;; Public
;;;
(defun symlinkp (pathname)
  "Return #t if $pathname names a symbolic link."
  (eq (file-kind pathname :check-for-links t) :link))

;;; Public
;;;
(defun symlink-dest (pathname)
  "Return the destination of symlink $pathname."
  (unix:unix-readlink (namestring pathname)))

;;; Public
;;;
(defun hidden-name-p (pathname)
  "Return #t if $pathname is named like a hidden file or directory.

   The name of a hidden file begins with a period (\".\")."
  ;; .  ..  /.  /..
  ;; /a/b   /a/.b   /a/b.c   /a/.b.c
  ;; /a/d/  /a/.d/  /a/d.c/  /a/.d.c/
  (let ((name (namestring pathname)))
    (if (and name (plusp (length name)))
	(let ((slash (position #\/ name :from-end t)))
	  (if slash
	      (if (= (1+ slash) (length name))
		  ;; Last character a slash, search for a previous slash.
		  (let ((slash (position #\/ name :from-end t
					 :end (1- slash))))
		    (if slash
			(char= (schar name (1+ slash)) #\.)
			(char= (schar name 0) #\.)))
		  (char= (schar name (1+ slash)) #\.))
	      (char= (schar name 0) #\.))))))

;;; Public
;;;
(defun hiddenp (pathname &key check-for-links)
  "Return #t if $pathname names a hidden file or directory.

   The name of a hidden file begins with a period (\".\").

   If $pathname is a symlink: act on the destination of $pathname if
   $check-for-links is true or on the $pathname otherwise."
  (hidden-name-p (if check-for-links
		     (if (symlinkp pathname)
			 pathname
			 (truename pathname))
		     (truename pathname))))



;; FIX rename new-file,dir,name?

(defvar *last-new-code* 0
  "The last integer suffix returned for a picked file name.")

;;; Public
;;;
(defun pick-new-file (&optional (base "/tmp/tmp~D-~D"))
  "Create a uniquely named file from $base.  Return the name of the file.

   Produce the file name by passing $base and two numeric arguments to
   `format'.

   Signal an error if $base contains wildcards, or if $base contains any
   format directives other than two ~D's."
  (or (eq (format:directive-count base "~D") 2)
      (error "$base must have two ~~D `format' directives: ~A"
	     base))
  (until* ((code (1+ *last-new-code*) (1+ code))
	   (name (format () base (unix:unix-getpid) code)
		 (format () base (unix:unix-getpid) code)))
	  ((with-open-file (stream name
				   :if-exists ()
				   :if-does-not-exist :create)
	     t)
	   (setq *last-new-code* code)
	   name)))

;;; Public
;;;
(defun pick-new-dir (&optional (base "/tmp/tmp~D-~D/"))
  "Create a uniquely named directory from $base.  Return the name of the
   directory.

   Produce the directory name by passing $base and two numeric arguments to
   `format'.

   Signal an error if $base contains wildcards, or if $base contains more
   or less than two ~D format directives."
  (or (eq (format:directive-count base "~D") 2)
      (error "$base must have two ~~D `format' directives: ~A"
	     base))
  (while* ((code (1+ *last-new-code*) (1+ code))
	   (name (format () base (unix:unix-getpid) code)
		 (format () base (unix:unix-getpid) code)))
	  ((probe-file name)
	   ;; FIX Some other process may create the dir here.
	   (add-dir name)
	   (setq *last-new-code* code)
	   name)))

;;; Public
;;;
(defun pick-new-name (&optional (base "/tmp/tmp~D-~D"))
  "Create and return a unique name from $base.

   Produce the name by passing $base and two numeric arguments to
   `format'.

   Signal an error if $base contains wildcards, or if $base contains any
   format directives other than two ~D's."
  (or (eq (format:directive-count base "~D") 2)
      (error "$base must have two ~~D `format' directives: ~A"
	     base))
  (let ((pid (unix:unix-getpid)))
    (iterate pick ((code (1+ *last-new-code*))
		   (last ""))
      (let ((name (format () base pid code)))
	(if (string= name last)
	    (error "Generated name is same as previous one: ~A~%~
		    Does $base (~A) have format specifiers?"
		   name base))
	(fi (probe-file name :check-for-links t)
	    (progn
	      (setq *last-new-code* code)
	      name)
	    (pick (1+ code) name))))))
