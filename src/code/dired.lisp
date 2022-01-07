;;; -*- Package: dired -*-
;;;
;;; Site dependent code for dired.

;;; FIX consider mving to ~filesys.lisp  eg used in code:db.lisp

(defpackage "DIRED"
  (:shadow "RENAME-FILE" "COPY-FILE" "DELETE-FILE" "SYMLINK-FILE" "CURRENT-DIRECTORY")
  (:export "COPY-FILE" "RENAME-FILE" "SYMLINK-FILE" "FIND-FILE" "DELETE-FILE"
	   "MAKE-DIRECTORY"
	   "*UPDATE-DEFAULT*" "*CLOBBER-DEFAULT*" "*RECURSE-DEFAULT*"
	   "*REPORT-FUNCTION*" "*ERROR-FUNCTION*" "*YESP-FUNCTION*"
	   "PATHNAMES-FROM-PATTERN")
  (:documentation "Site dependant file manipulation interface for Dired."))

(in-package "DIRED")

#[ File Utilities

Some implementations of the editor provide extensive directory editing
commands, `Dired', including a single wildcard feature.  An asterisk
denotes a wildcard.

{function:dired:copy-file}
{function:dired:rename-file}
{function:dired:delete-file}
{function:dired:find-file}
{function:dired:make-directory}
{function:dired:pathnames-from-pattern}
{variable:dired:*update-default*}
{variable:dired:*clobber-default*}
{variable:dired:*recurse-default*}
{variable:dired:*report-function*}
{variable:dired:*error-function*}
{variable:dired:*yesp-function*}
]#


;;;; Exported parameters.

(defparameter *update-default* ()
  "Update arguments fall back to this value.")

(defparameter *clobber-default* t
  "Clobber arguments fall back to this value.")

(defparameter *check-for-links-fallback* ()
  "Check-for-links arguments fall back to this value.")

(defparameter *recurse-default* ()
  "Recursive arguments fall back to this value.")


;;;; WILDCARDP

(defconstant wildcard-char #\*
  "Wildcard designator for file names will match any substring.")

(defmacro wildcardp (file-namestring)
  `(position wildcard-char (the simple-string ,file-namestring) :test #'char=))


;;;; User interaction functions, variable declarations, and their defaults.

(defun default-error-function (string &rest args)
  (apply #'error string args))
;;;
(defvar *error-function* #'default-error-function
  "A function called when an error is encountered.")

(defun default-report-function (string &rest args)
  (apply #'format t string args))
;;;
(defvar *report-function* #'default-report-function
  "A function called when a message needs to be written.")

(defun default-yesp-function (string &rest args)
  (apply #'format t string args)
  (let ((answer (nstring-downcase (string-trim '(#\space #\tab) (read-line)))))
    (declare (simple-string answer))
    (or (string= answer "")
	(string= answer "y")
	(string= answer "yes")
	(string= answer "ye"))))
;;;
(defvar *yesp-function* #'default-yesp-function
  "Function used to query whether to perform an action, for example when
   clobbering an existing file.")


;;;; Copy-File

(defstruct (wild-match (:print-function print-wild-match)
		       (:constructor make-wild-match (file substitute)))
  "Information about wildcard matches.  File is the FIX Sesame namestring
   of the file matched, and substitute is a substring of the
   file-namestring of file."
  file
  substitute)

(defun print-wild-match (obj str n)
  (declare (ignore n))
  (format str "#<Wild-Match  ~S  ~S>"
	  (wild-match-file obj) (wild-match-substitute obj)))

(defun copy-file (spec1 spec2 &key (update *update-default*)
				   (clobber *clobber-default*)
				   (directory () directoryp)
				   check-for-links)
  "Copy $spec1 to $spec2.  Accept a single wildcard in the filename portion
   of the specification, and accept files or directories.  Maintain the
   source write date on the destination file.

   If $spec1 and $spec2 are both directories, recursively copy the files
   and subdirectory structure of $spec1; if $spec2 is in the subdirectory
   structure of spec1, stop recursing at that point.  If $spec1 is like
   FIX \"/spec/*\" then copy only the files from $spec1 to directory $spec2.

   If $spec1 is a file, and $spec2 is a directory, then copy $spec1 into
   $spec2 with the same pathname-name as $spec1.

   If $update is true, then only copy files if the source is newer than the
   destination.

   If $check-for-links is true, then preserve symbolic links.

   If $update and $clobber are both (), and the destination exists, the
   copying process prompts whether the destination should be overwritten.

   If $directory is supplied, it is a list of pathnames and $spec1 is a
   pattern containing one wildcard.  Copy each of the pathnames whose
   pathname-name matches the pattern $spec1 into $spec2.  $spec2 is either
   a directory or a pathname whose pathname-name contains a wildcard."
  (cond
   ((not directoryp)
    (let* ((ses-name1 (ext:os-namestring spec1 t))
	   (exists1p (file-kind ses-name1))
	   ;; FIX when both dirs only works if spec2 exists
	   ;;       could copy dir spec1 to spec2 otherwise
	   (ses-name2 (ext:os-namestring (namify spec2) nil))
	   (pname1 (pathname ses-name1))
	   (pname2 (pathname ses-name2))
	   (dirp1 (directoryp pname1 :check-for-links check-for-links))
	   (dirp2 (or (directoryp pname2 :check-for-links check-for-links)
		      (and (directory-name-p spec2) (symlinkp spec2))))
	   (wildp1 (if (file-namestring pname1)
		       (wildcardp (file-namestring pname1))))
	   (wildp2 (if (file-namestring pname2)
		       (wildcardp (file-namestring pname2)))))
      (and dirp1 wildp1
	   (funcall *error-function*
		    "Wildcard in directory name -- ~S." pname1))
      (and dirp2 wildp2
	   (funcall *error-function*
		    "Wildcard in directory name -- ~S." pname2))
      (when dirp1
	(setq ses-name1 (ensure-trailing-slash ses-name1))
	(setq pname1 (pathname ses-name1))
	(or dirp2
	    (if (probe-file pname2)
		(funcall *error-function*
			 "$spec1 is a directory and $spec2 is a file.")
		(setq dirp2 t))))
      (when dirp2
	(setq ses-name2 (ensure-trailing-slash ses-name2))
	(setq pname2 (pathname ses-name2)))
      (if wildp2
	  (or wildp1
	      (funcall *error-function*
		       "Source must have wildcards if destination has wildcards.")))
      (if wildp1
	  (or wildp2 dirp2
	      (funcall *error-function*
		       "If source has wildcards then destination ~
			must have wildcards or be a directory.")))
      (cond ((and dirp1 dirp2)
	     (or (directory-existsp ses-name1)
		 (funcall *error-function*
			  "Source directory must exist -- ~S." pname1))
	     (if (directory-existsp ses-name2)
		 ;; Copy the dir into the existing dir.
		 (let ((ses-sub1 (concat
				  (directorify ses-name2)
				  (file-namestring (namify ses-name1)))))
		   (add-directory (ensure-trailing-slash ses-sub1))
		   (recursive-copy pname1
				   (ensure-trailing-slash
				    (merge-pathnames
				     ses-sub1
				     (ext:current-directory)))
				   update clobber pname2
				   ses-name1 ses-sub1
				   check-for-links))
		 ;; Mirror the dir at the same level, with the given name.
		 (progn
		   (add-directory ses-name2)
		   (recursive-copy pname1
				   (truename pname2)
				   update clobber pname2
				   ses-name1 ses-name2
				   check-for-links))))
	    (dirp2
	     ;; Merge $pname2 with $pname1 to pick up a similar
	     ;; file-namestring.
	     (copy-file-1 pname1 wildp1 exists1p
			  (if (directoryp pname1
					  :check-for-links check-for-links)
			      (concat (namify pname2) pname1)
			      (merge-pathnames pname2 pname1))
			  wildp1 update clobber check-for-links))
	    (t (copy-file-1 pname1 wildp1 exists1p
			    pname2 wildp2 update clobber check-for-links)))))
    (directory
     (if (pathname-directory spec1)
	 (funcall *error-function*
		  "Spec1 is just a pattern when supplying directory -- ~S."
		  spec1))
     (let* ((pname2 (pathname (ext:os-namestring spec2 nil)))
	    (dirp2 (directoryp pname2 :check-for-links check-for-links))
	    (wildp1 (wildcardp spec1))
	    (wildp2 (if (file-namestring pname2)
			(wildcardp (file-namestring pname2)))))
       (or wildp1
	   (funcall *error-function*
		    "Pattern, ~S, must contain a wildcard."
		    spec1))
       (or wildp2 dirp2
	   (funcall *error-function*
		    "If source has wildcards then destination must either ~
		     have wildcards or be a directory."))
       (copy-wildcard-files spec1 wildp1
			    (if dirp2 (merge-pathnames pname2 spec1) pname2)
			    (if dirp2 wildp1 wildp2)
			    update clobber directory check-for-links))))
  (values))

;;; RECURSIVE-COPY
;;;
(defun recursive-copy (pname1 pname2 update clobber
		       forbidden-dir ses-name1 ses-name2 check-for-links)
  "Copy the files in directory $pname1 to directory $pname2, recursively
   descending into subdirectories.  If a subdirectory of $pname1 does not
   exist in pname2, it is created.  Assume $pname1 exists.

   If $pname2 is absolute copy the contents of $pname1 into $pname2,
   otherwise copy the directory $pname1 into $pname2.

   $forbidden-dir is originally the same as pname2; to prevent recursing
   into $pname2 when $pname2 is in the subdirectory structure of $pname1.
   Also check every directory of $pname1 for circularity before recursing
   into that directory.

   Return #t on copying any files." ; FIX confirm this
  (funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1 ses-name2)
  ;; FIX given that :follow-links t expands symlinks to their truenames
  ;;       does this produce the correct dests when check-for-links?
  ;;           eg when symlink out of root of tree being copied
  (dolist (spec (directory (directory-namestring pname1)
			   :follow-links (fi check-for-links)
			   ; Leave names of symlinks intact.
			   :truenamep ()
			   ; Also copy backup files.
			   :backups t))
    (let ((spec-ses-name (namestring spec)))
      (if (directoryp spec :check-for-links check-for-links)
	  (or (equal (pathname spec-ses-name) forbidden-dir)
	      (circular-p spec)
	      (let* ((dir2-pname (merge-dirs spec pname2))
		     (dir2-ses-name (namestring dir2-pname)))
		(or (directory-existsp dir2-ses-name)
		    (add-directory dir2-ses-name))
		(recursive-copy spec dir2-pname update clobber forbidden-dir
				spec-ses-name dir2-ses-name check-for-links)
		(funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1
			 ses-name2)))
	  (copy-file-2 (namify spec-ses-name)
		       (namestring (merge-pathnames pname2 (namify spec)))
		       update clobber check-for-links)))))

#|
(defun merge-dirs (pname1 pname2)
  "Pick out the last directory name in the pathname $pname1 and add it to
   the end of the sequence of directory names from $pname2, returning a
   pathname."
  (let* ((dirs1 (pathname-directory pname1))
	 (dirs2 (pathname-directory pname2))
	 (dirs2-len (length dirs2))
	 (new-dirs2 (make-array (1+ dirs2-len))))
    (declare (simple-vector dirs1 dirs2 new-dirs2))
    (replace new-dirs2 dirs2)
    (setf (svref new-dirs2 dirs2-len)
	  (svref dirs1 (1- (length dirs1))))
    (make-pathname :directory new-dirs2 :device :absolute)))
|#

(defun merge-dirs (pname1 pname2)
  "Pick out the last directory name in the pathname $pname1 and add it to
   the end of the sequence of directory names from $pname2, returning a
   pathname."
  (let* ((dirs1 (pathname-directory pname1))
	 (dirs2 (pathname-directory pname2))
	 (dirs2-len (length dirs2))
	 (new-dirs2 (make-list (1+ dirs2-len))))
    (replace new-dirs2 dirs2)
    (setf (nth dirs2-len new-dirs2)
	  (nth (1- (length dirs1)) dirs1))
    (make-pathname :directory new-dirs2 :device :unspecific)))

(defun copy-file-1 (pname1 wildp1 exists1p pname2 wildp2 update clobber
			   check-for-links)
  "Takes pathnames which both contain the same number of wildcards, at most
   one.  $wildp1 and $wildp2 are either nil or indexes into the
   file-namestring of $pname1 and $pname2, respectively, indicating the
   position of the wildcard character.  If there is a wildcard, then
   resolve the wildcard and copy those matching files; otherwise, simply
   call `copy-file-2'."
  (if wildp1
      (copy-wildcard-files pname1 wildp1 pname2 wildp2 update clobber ()
			   check-for-links)
      (let ((ses-name1 (namestring pname1)))
	(or exists1p (funcall *error-function*
			      "~S must exist." ses-name1))
	(copy-file-2 ses-name1 (namestring pname2) update clobber
		     check-for-links))))

(defun copy-wildcard-files (pname1 wildp1 pname2 wildp2 update clobber
				   &optional directory check-for-links)
  (multiple-value-bind (dst-before dst-after)
		       (before-wildcard-after (file-namestring pname2) wildp2)
    (dolist (match (resolve-wildcard pname1 wildp1 directory))
      (copy-file-2 (wild-match-file match)
		   (namestring (concatenate 'simple-string
					    (directory-namestring pname2)
					    dst-before
					    (wild-match-substitute match)
					    dst-after))
		   update clobber check-for-links))))

(defun copy-file-2 (ses-name1 ses-name2 update clobber check-for-links)
  "Copy $ses-name1 to $ses-name2 depending on the values of $update and
   $clobber, as described for `copy-file'.

   If $ses-name2 doesn't exist, then just copy it; otherwise, if $update,
   then only copy it if the destination's write date precedes the source's,
   and if not $clobber and not $update, then prompt for confirmation before
   doing the copy."
  (multiple-value-bind (secs1 err)
		       (file-write-date ses-name1
					:check-for-links check-for-links)
    (or secs1
	(funcall *error-function* "Stat of ~S failed: ~A."
		 ses-name1 err))
    (cond ((fi (probe-file ses-name2 :check-for-links check-for-links))
	   (do-the-copy ses-name1 ses-name2 secs1 check-for-links))
	  (update
	   (let ((secs2 (file-write-date ses-name2
					 :check-for-links check-for-links)))
	     (cond (clobber
		    (do-the-copy ses-name1 ses-name2 secs1 check-for-links))
		   ((and (> secs2 secs1)
			 (funcall *yesp-function*
				  "~&~S  ==>  ~S~%  ~
				  ** Destination is newer than source.  ~
				  Overwrite it? "
				  ses-name1 ses-name2))
		    (do-the-copy ses-name1 ses-name2 secs1 check-for-links))
		   ((< secs2 secs1)
		    (do-the-copy ses-name1 ses-name2 secs1 check-for-links)))))
	  ((fi clobber)
	   (if (funcall *yesp-function*
			"~&~S  ==>  ~S~%  ** Destination already exists.  ~
			 Overwrite it? "
			ses-name1 ses-name2)
	       (do-the-copy ses-name1 ses-name2 secs1 check-for-links)))
	  (t (do-the-copy ses-name1 ses-name2 secs1 check-for-links)))))

(defun do-the-copy (ses-name1 ses-name2 secs1 &optional check-for-links)
  (handler-case
      (progn
	(lisp:copy-file ses-name1 ses-name2
			:check-for-links check-for-links)
	; FIX
	;(setf (file-write-date ses-name2) secs1)
	(lisp::set-file-write-date ses-name2 secs1 :check-for-links t))
    (error (err) (funcall *error-function* "~A" err)))
  ;(funcall *report-function* "~&~S  ==>~%  ~S~%" ses-name1 ses-name2)
  )


;;;; Rename-File

(defun rename-file (spec1 spec2 &key (clobber *clobber-default*)
			  (directory () directoryp))
  "Rename $spec1 to $spec2.  Accept a single wildcard in the filename
   portion of the specified files.

   A directory is specified with a trailing slash.  If $spec1 names a
   directory (or is a directory) and $spec2 names a file then rename $spec1
   to $spec2.  If $spec2 is a directory merge $spec2 with $spec1 to form
   the destination specification.

   If $clobber is (), and $spec2 exists, then ask for confirmation before
   renaming.

   When $directory is supplied, it is a list of pathnames and $spec1 is a
   pattern containing one wildcard.  Copy each of the pathnames whose
   pathname-name matches the pattern $spec1 to $spec2.  $spec2 is then
   either a directory or a pathname whose pathname-name contains a
   wildcard."
  (if (and directoryp directory)
      (progn
	(if (pathname-directory spec1)
	    (funcall *error-function*
		     "Spec1 is just a pattern when supplying directory -- ~S."
		     spec1))
	(let* ((pname2 (pathname (ext:os-namestring spec2 nil)))
	       (dirp2 (directoryp pname2))
	       (wildp1 (wildcardp spec1))
	       (wildp2 (if (file-namestring pname2)
			   (wildcardp (file-namestring pname2)))))
	  (or wildp1
	      (funcall *error-function*
		       "Pattern, ~S, does not contain a wildcard."
		       spec1))
	  (or wildp2
	      dirp2
	      (funcall *error-function*
		       "Wildcard source with unique destination only~
		        possible if destination a directory."))
	  (rename-wildcard-files spec1 wildp1
				 (if dirp2 (merge-pathnames pname2 spec1) pname2)
				 (if dirp2 wildp1 wildp2)
				 clobber directory)))
      (progn
	(let* ((ses-name1 (ext:os-namestring spec1 t))
	       (exists1p (file-kind ses-name1))
	       (ses-name2 (or (ext:os-namestring spec2 ())
			      (funcall *error-function*
				       "Destination directory must exist: ~A." spec2)))
	       (pname1 (pathname ses-name1))
	       (pname2 (pathname ses-name2))
	       (dirp2 (directoryp pname2))
	       (wildp1 (if (file-namestring pname1)
			   (wildcardp (file-namestring pname1))))
	       (wildp2 (if (file-namestring pname2)
			   (wildcardp (file-namestring pname2)))))
	  (if (and dirp2 wildp2)
	      (funcall *error-function*
		       "Cannot have wildcards in directory names -- ~S." pname2))
	  (if wildp2
	      (fi wildp1
		  (funcall *error-function*
			   "Cannot handle destination having wildcards without ~
			    source having wildcards.")))
	  (if wildp1
	      (or wildp2 dirp2
		  (funcall *error-function*
			   "If source has wildcards then destination ~
			    must either be a directory or have wildcards.")))
	  (rename-file-1 pname1 wildp1 exists1p
			 (if dirp2
			     (merge-pathnames pname2 (namify pname1))
			     pname2)
			 wildp2 clobber))))
  (values))

;;; RENAME-FILE-1 takes pathnames which either both contain a single wildcard
;;; or none.  Wildp1 and Wildp2 are either nil or indexes into the
;;; file-namestring of pname1 and pname2, respectively, indicating the position
;;; of the wildcard character.  If there is no wildcard, then simply call
;;; RENAME-FILE-2; otherwise, resolve the wildcard and rename those matching files.
;;;
(defun rename-file-1 (pname1 wildp1 exists1p pname2 wildp2 clobber)
  (if wildp1
      (rename-wildcard-files pname1 wildp1 pname2 wildp2 clobber)
      (let ((ses-name1 (namestring pname1)))
	(or exists1p (funcall *error-function*
			      "~S must exist." ses-name1))
	(rename-file-2 ses-name1 (namestring pname2) clobber))))

(defun rename-wildcard-files (pname1 wildp1 pname2 wildp2 clobber
				   &optional directory)
  (multiple-value-bind (dst-before dst-after)
		       (before-wildcard-after (file-namestring pname2) wildp2)
    (dolist (match (resolve-wildcard pname1 wildp1 directory))
      (rename-file-2 (wild-match-file match)
		     (namestring (concatenate 'simple-string
					      (directory-namestring pname2)
					      dst-before
					      (wild-match-substitute match)
					      dst-after))
		     clobber))))

(defun rename-file-2 (ses-name1 ses-name2 clobber)
  (cond ((and (probe-file ses-name2) (fi clobber))
	 (when (funcall *yesp-function*
			"~&~S  ==>  ~S~%  ** Destination already exists.  ~
			 Overwrite it? "
			ses-name1 ses-name2)
	   (sub-rename-file ses-name1 ses-name2)
	   (funcall *report-function*
		    "~&~S  ==>~%  ~S~%" ses-name1 ses-name2)))
	(t (sub-rename-file ses-name1 ses-name2)
	   (funcall *report-function*
		    "~&~S  ==>~%  ~S~%" ses-name1 ses-name2))))


;;;; Symlink-File

(defun symlink-file (source dest &key clobber)
  "Symlink name $source to file $dest.

   If $clobber is true overwrite $source if $source exists, else prompt to
   overwrite."
  (let* ((ses-name-src (ext:os-namestring source nil))
	 (ses-name-dest (ext:os-namestring dest t))
	 (exists-dest-p (file-kind ses-name-dest))
	 (pname-src (pathname ses-name-src))
	 (pname-dest (pathname ses-name-dest))
	 (namestring-dest (namestring pname-dest))
	 (namestring-src (namestring pname-src))
	 (wildp-src (if (file-namestring pname-src)
			(wildcardp (file-namestring pname-src))))
	 (wildp-dest (if (file-namestring pname-dest)
			 (wildcardp (file-namestring pname-dest)))))
    (if wildp-dest
	(funcall *error-function*
		 "Link destination contains a wildcard -- ~S." pname-dest))
    (if wildp-src
	(funcall *error-function*
		 "Link source contains a wildcard -- ~S." pname-dest))
    (or exists-dest-p
	(funcall *error-function* "Destination of link must exist."))
    (when (if (file-kind ses-name-src :check-for-links t)
	      (when (or clobber
			(funcall *yesp-function*
				 "~&~S  -->  ~S~%  ~
				  ** Link file already exists.  ~
				  Overwrite it? "
				 ses-name-src ses-name-dest))
		(delete-file ses-name-src
			     :clobber t
			     :recurse t)
		t)
	      t)
      (sub-symlink-file namestring-src namestring-dest)
      (funcall *report-function*
	       "~&~S  -->~%  ~S~%" namestring-src namestring-dest))))


;;;; Find-File

(defun find-file (file-name &optional (directory "")
			    (find-all-p () find-all-suppliedp))
  "Find the file with file-namestring $file-name, recursively looking in
   $directory.

   If $find-all-p is true, then continue searching even after finding a
   first occurrence of file.  $file-name may contain a single wildcard, in
   which case $find-all falls back to true instead of ()."
  (let* ((file (coerce file-name 'simple-string))
	 (wildp (wildcardp file))
	 (find-all-p (if find-all-suppliedp find-all-p wildp)))
    (declare (simple-string file))
    (catch 'found-file
      (if wildp
	  (multiple-value-bind (before after)
			       (before-wildcard-after file wildp)
	    (find-file-aux file directory find-all-p before after))
	  (find-file-aux file directory find-all-p))))
  (values))

(defun find-file-aux (the-file directory find-all-p &optional before after)
  (declare (simple-string the-file))
  (dolist (spec (directory directory :all t))
    (let* ((spec-ses-name (namestring spec))
	   (spec-file-name (file-namestring spec-ses-name)))
      (declare (simple-string spec-ses-name spec-file-name))
      (if (directoryp spec)
	  (find-file-aux the-file spec find-all-p before after)
	  (when (if before
		    (find-match before after spec-file-name :no-cons)
		    (string-equal the-file spec-file-name))
	    (print spec-ses-name)
	    (unless find-all-p (throw 'found-file t)))))))


;;;; Delete-File
;;;

;; FIX is there a case for a check-for-links arg?
;; FIX rename clobber to prompt-to-overwrite?
;; FIX code: trash-file
;;; DELETE-FILE
;;;
;;; If spec is a directory, but recursive is nil, just pass the directory
;;; down through, letting LISP:DELETE-FILE signal an error if the directory
;;; is not empty.
;;;
(defun delete-file (spec &key (recurse *recurse-default*)
			      (clobber *clobber-default*))
  "Delete the files specified by $spec, accepting a single wildcard in the
   filename portion of $spec.  A directory is specified with a trailing
   slash.

   Ask for confirmation on each file if :clobber is ().

   On encountering a symlink, delete the link instead of the destination of
   the link.

   If $recurse is true and $spec is a directory then recursively delete the
   entirety of the directory and its subdirectory structure.  A directory
   may be specified with $recurse false, as long as the directory is
   empty."
  (let* ((pname (pathname spec))
	 (wildp (if (file-namestring pname)
		    (wildcardp (file-namestring pname))))
	 (dirp (fi wildp (directoryp pname :check-for-links t))))
    (if dirp
	(let ((ses-name (ext:os-namestring spec t)))
	  (if recurse
	      (when (or clobber
			(funcall *yesp-function*
				 "~&Recursively delete ~S? "
				 (namestring spec)))
		(recursive-delete pname ses-name))
	      (delete-file-2 ses-name clobber)))
	(delete-file-1 pname spec wildp clobber)))
  (values))

(defun recursive-delete (directory dir-ses-name)
  (dolist (spec (directory (directory-namestring directory)
			   ; Treat symlinks directly.
			   :truenamep () :follow-links ()
			   ; Include backup files.
			   :backups t))
    (let ((spec-ses-name (namestring spec)))
      (if (directoryp spec :check-for-links t)
	  (recursive-delete (pathname spec-ses-name) spec-ses-name)
	  (delete-file-3 spec-ses-name))))
  (delete-file-3 dir-ses-name)
  (funcall *report-function* "~&~A~%" dir-ses-name))

(declaim (inline delete-file-1 delete-file-2))

(defun delete-file-1 (pname spec wildp clobber)
  (declare (ignore pname))
  (if wildp
#|
      FIX old version, does wildcard matching manually, update others above
      (dolist (match (resolve-wildcard pname wildp () :check-for-links t))
	(delete-file-2 (wild-match-file match) clobber))
|#
      (dolist (match (directory spec :follow-links ()))
	(delete-file-2 match clobber))
      (delete-file-2 (ext:os-namestring spec) clobber)))

(defun delete-file-2 (ses-name clobber)
  (when (or clobber (funcall *yesp-function* "~&Delete ~S? " ses-name))
    (if (directoryp ses-name :check-for-links t)
	(delete-directory ses-name)
	(lisp:delete-file (namify ses-name)))
    (funcall *report-function* "~&~A~%" ses-name)))

(defun delete-file-3 (ses-name)
  (if (directoryp ses-name :check-for-links t)
      (delete-directory ses-name)
      (lisp:delete-file ses-name)))


;;;; Wildcard resolution

(defun pathnames-from-pattern (pattern files)
  "Return a list of pathnames from files whose file-namestrings match
   $pattern.  $pattern must be a string of length and must contains only
   one asterisk.  $files must only contain files."
  (declare (simple-string pattern))
  (if (string= pattern "")
      (funcall *error-function* "Must be a non-empty pattern."))
  (or (= (count wildcard-char pattern :test #'char=) 1)
      (funcall *error-function* "Pattern must contain one asterisk."))
  (multiple-value-bind (before after)
		       (before-wildcard-after pattern (wildcardp pattern))
    (let ((result nil))
      (dolist (f files result)
	(let* ((ses-namestring (namestring f))
	       (f-namestring (file-namestring ses-namestring))
	       (match (find-match before after f-namestring)))
	  (when match (push f result)))))))

;;; RESOLVE-WILDCARD takes a pathname with a wildcard and the position of the
;;; wildcard character in the file-namestring and returns a list of wild-match
;;; objects.  When directory is supplied, pname is just a pattern, or a
;;; file-namestring.  It is an error for directory to be anything other than
;;; absolute pathnames in the same directory.  Each wild-match object contains
;;; the Sesame namestring of a file in the same directory as pname, or
;;; directory, and a simple-string representing what the wildcard matched.
;;;
(defun resolve-wildcard (pname wild-pos &optional directory &key check-for-links)
  (multiple-value-bind (before after)
		       (before-wildcard-after (if directory
						  pname
						  (file-namestring pname))
					      wild-pos)
    (let (result)
      (dolist (f (or directory (directory (directory-namestring pname)
					  :all t
					  :follow-links (fi check-for-links)))
		 (nreverse result))
	(or (directoryp f :check-for-links check-for-links)
	    (let* ((ses-namestring (namestring f))
		   (f-namestring (file-namestring ses-namestring))
		   (match (find-match before after f-namestring)))
	      (if match
		  (push (make-wild-match ses-namestring match) result))))))))

;;; FIND-MATCH takes a "before wildcard" and "after wildcard" string and a
;;; file-namestring.  If before and after match a substring of file-namestring
;;; and are respectively left bound and right bound, then anything left in
;;; between is the match returned.  If no match is found, nil is returned.
;;; NOTE: if version numbers ever really exist, then this code will have to be
;;; changed since the file-namestring of a pathname contains the version number.
;;;
(defun find-match (before after file-namestring &optional no-cons)
  (declare (simple-string before after file-namestring))
  (let ((before-len (length before))
	(after-len (length after))
	(name-len (length file-namestring)))
    (if (>= name-len (+ before-len after-len))
	(let* ((start (if (string= before file-namestring
				   :end1 before-len :end2 before-len)
			  before-len))
	       (end (- name-len after-len))
	       (matchp (and start
			    (string= after file-namestring :end1 after-len
				     :start2 end :end2 name-len))))
	  (if matchp
	      (if no-cons
		  t
		  (subseq file-namestring start end)))))))

(defun before-wildcard-after (file-namestring wild-pos)
  (declare (simple-string file-namestring))
  (values (subseq file-namestring 0 wild-pos)
	  (subseq file-namestring (1+ wild-pos) (length file-namestring))))


;;;; Miscellaneous Utilities (e.g., MAKEDIR).

(defun make-directory (name)
  "Create directory $name.  If $name exists, then signal an error."
  (let ((ses-name (ext:os-namestring name nil)))
    (when (file-kind ses-name)
      (funcall *error-function* "Name already exists -- ~S" ses-name))
    (add-directory ses-name))
  t)


;;;; Unix operations.

;;; SUB-RENAME-FILE originally existed because lisp::rename-file merges the
;;; new name with the old name to pick up defaults, and this has issues
;;; with Unix-oid names.  For example, renaming "foo.bar" to ".baz" causes
;;; a result of "foo.baz"!  SUB-RENAME-FILE now uses lisp::rename-file, so
;;; FIX watchout for these issues, or maybe improve lisp::rename-file.
;;;
(defun sub-rename-file (ses-name1 ses-name2)
  (handler-case
      (lisp:rename-file ses-name1 ses-name2 :check-for-links t)
    (error (err)
	   (funcall *error-function* "~A" err))))

(defun sub-symlink-file (ses-source ses-dest)
  (handler-case
      (lisp:symlink-file ses-source ses-dest)
    (error (err) (funcall *error-function* "~A" err))))


;;;; Misc. Utility Utilities

(defun directory-existsp (ses-name)
  (directoryp ses-name))

(defun add-directory (ses-name)
  (declare (simple-string ses-name))
  (handler-case
      (lisp:add-dir ses-name)
    (error (err) (funcall *error-function* "~A" err))))

(defun delete-directory (ses-name)
  (declare (simple-string ses-name))
  (handler-case
      (lisp:delete-dir ses-name)
    (error (err) (funcall *error-function* "~A" err))))
