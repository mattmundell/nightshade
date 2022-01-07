;;;; Syncing source trees.

(in-package "SYNC")

(export '(copy-manifest-tree update-manifest-times merge-from-server))

(defstruct (manifest-entry
	    (:constructor make-manifest-entry (name time)))
  "An entry from a manifest file."
  name
  time)

;; FIX use m-p or make merge-pathnames work for these

(defun copy-manifest-tree (dir dest-dir)
  "Copy the tree recursively described by the .manifest file in $dir to
   $dest-dir."
  (or (probe-file (format () "~A.manifest" dir))
      (error "Directory ~A should have a .manifest file." dir))
  (with-open-file (stream (format () "~A.manifest" dir))
    (let ((dest (concatenate 'simple-string dest-dir ".manifest")))
      (ensure-directories-exist (directory-namestring dest))
      (copy-file (concatenate 'simple-string
			      (namestring (ensure-trailing-slash dir))
			      ".manifest")
		 dest))
    (ext::do-lines (line stream)
      (multiple-value-bind (file pos)
			   (read-from-string line)
	(when (and file (stringp file))
	  (let ((stamp (read-from-string line t () :start pos)))
	    (fi* (and stamp (minusp stamp))
	      (if (directory-name-p file)
		  (copy-manifest-tree (concatenate 'simple-string
						   dir file)
				      (concatenate 'simple-string
						   dest-dir file))
		  (let ((dest (concatenate 'simple-string dest-dir file)))
		    (ensure-directories-exist (directory-namestring dest))
		    (or (directoryp file :check-for-links t)
			(copy-file (concatenate 'simple-string
						(namestring
						 (ensure-trailing-slash dir))
						file)
				   dest)))))))))))

(defun read-manifest (pathname)
  "Return the manifest described in $pathname."
  (if (probe-file pathname)
      (with-open-file (stream pathname)
	(collect ((manifest))
	  (ext::do-lines (string stream)  ; FIX ::
	    (multiple-value-bind (file pos)
				 (read-from-string string)
	      (and file
		   (stringp file)
		   (let ((time (read-from-string string () () :start pos)))
		     (manifest (make-manifest-entry file time))))))
	  (manifest)))))
#|
FIX deftest
(read-manifest (build-server-pathname "localhost"
				      "/home/matt/src/nightshade/src/code/"
				      ".manifest"))
(read-manifest (build-server-pathname "localhost:/usr/local/src/nightshade/"
				      "src/code/"
				      ".manifest"))
|#

(defun update-entry (client-manifest server-entry)
  "Update CLIENT-MANIFEST to contain SERVER-ENTRY, replacing any existing
   entry for the file described by SERVER-ENTRY.  Return the new client
   manifest."
  (let ((client-entry (manifest-assoc client-manifest
				      (manifest-entry-name server-entry))))
    (if client-entry
	;; Update client manifest entry time stamp to match server.
	(setf (manifest-entry-time client-entry)
	      (manifest-entry-time server-entry))
	;; Add a new client manifest entry.
	(push (make-manifest-entry (manifest-entry-name server-entry)
				   (manifest-entry-time server-entry))
	      client-manifest)))
  client-manifest)

(defun build-server-pathname (server directory name)
  "Return a pathname for file NAME in DIRECTORY on SERVER."
  (merge-pathnames (merge-pathnames name directory)
		   (if (or (find #\/ (namestring server))
			   (find #\: (namestring server)))
		       server
		       (concatenate 'simple-string
				    (namestring server) ":"))))

(defun add-from-server (client-manifest server-entry directory server)
  "Add the file described in SERVER-ENTRY to the current directory.
   CLIENT-MANIFEST is the manifest of the current directory.  Return the
   new client manifest."
  ;; Add to client manifest.
  (setq client-manifest (update-entry client-manifest server-entry))
  ;; Check for existing client file.
  (when (probe-file (manifest-entry-name server-entry)
		    :check-for-links t)
    (let ((new-name (pick-new-name (format () "~A--~~D-~~D.sync-moved"
					   (manifest-entry-name
					    server-entry)))))
      (format t "File ~A exists already on client.~%    Moving it to ~A first.~%"
	      new-name)
      (rename-file (manifest-entry-name server-entry) new-name)))
  ;; Fetch file or directory from server.
  (format t "Copying ~A ~A ~A into client~%"
	   server directory (manifest-entry-name server-entry))
  (copy-file (build-server-pathname server directory
				    (manifest-entry-name server-entry))
	     (manifest-entry-name server-entry))
  ;; Update client manifest to match time of server entry.
  (let ((client-entry (manifest-assoc client-manifest
				      (manifest-entry-name server-entry))))
    (if (manifest-entry-time server-entry)
	(setf (manifest-entry-time client-entry)
	      (manifest-entry-time server-entry))))
  client-manifest)

(defun merge-file-from-server (client-manifest server directory
					       client-entry server-entry)
  "Merge any additions in the file described by CLIENT-ENTRY on DIRECTORY
   on SERVER into the current directory.  CLIENT-MANIFEST is the manifest
   of the current directory.  Return the new client manifest."
  (format t "Merging~%    Server: ~A~%    Dir: ~A~%    Entry name: ~A~%"
	  server directory (manifest-entry-name client-entry))
  ;; Merge the server file directly into the client file.
  (ed::merge-files (manifest-entry-name client-entry)
		   (build-server-pathname server directory
					  (manifest-entry-name client-entry)))
  ;; Only update the client time stamp if it is less than the server's.
  (if (< (manifest-entry-time server-entry)
	 (manifest-entry-time client-entry))
      (progn
	(setf (manifest-entry-name client-entry)
	      (manifest-entry-name server-entry))
	(setf (manifest-entry-time client-entry)
	      (manifest-entry-time server-entry)))
      client-manifest))

(defun manifest-assoc (client-manifest name)
  "Return the entry associated with NAME in CLIENT-MANIFEST."
  (declare (type string name))
  (find name client-manifest
	:test #'(lambda (name entry)
		  (string= (manifest-entry-name entry) name))))

(defun update-manifest-file (manifest)
  "Replace the manifest file in the current directory with $manifest."
  (with-open-file (out ".manifest"
		       :direction :output
		       :if-exists :supersede)
    (loop for entry in manifest do
      (format out "~S ~D~%"
	      (manifest-entry-name entry)
	      (manifest-entry-time entry)))))

(defun update-manifest-times (directory)
  "Update the manifest times in the tree rooted at $directory."
  (in-directory directory
    ;(format t "In ~A~%" directory)
    (let ((client-manifest (read-manifest ".manifest")))
      (dolist (entry client-manifest)
	(let ((entry-name (manifest-entry-name entry)))
	  ;(format t "Considering ~A~%" entry-name)
	  (if (directoryp entry-name :check-for-links t)
	      (update-manifest-times entry-name)
	      (let ((file-time (file-write-date entry-name
						:check-for-links t))
		    (entry-time (manifest-entry-time entry)))
		;(format t "file-time: ~A~%" file-time)
		(if file-time
		    (when (if entry-time (> file-time entry-time) t)
		      (format t "Updating ~A in ~A to ~A.~%"
			      entry-name directory file-time)
		      (setf (manifest-entry-time entry) file-time))
		    (progn
		      (format t "Marking ~A as subtracted (~A).~%"
			      entry-name
			      (if entry-time
				  (if (minusp entry-time)
				      entry-time
				      (- entry-time))
				  -1))
		      (setf (manifest-entry-time entry)
			    (if entry-time
				(if (minusp entry-time)
				    entry-time (- entry-time))
				-1))))))))
      (update-manifest-file client-manifest))
    ;(format t "Out ~A~%" directory)
    ))

(defun yesp-function (&key prompt)
  (declare (ignore prompt))
  t)
;;;
(defvar *yesp-function* #'yesp-function
  "Function used to query to continue after each verbose message in
   `merge-directory-from-server'.")

(defun y-or-n-p-function (&key prompt)
  (apply #'y-or-n-p prompt))
;;;
(defvar *y-or-n-p-function* #'y-or-n-p-function
  "Function used to query to remove file on client if removed on server and
   modified on client.")

(defun merge-directory-from-server (server directory &optional verbose-p)
  "Recursively merge any additions in DIRECTORY on SERVER into the current
   source.  Assume that the current directory is the parent of the local
   DIRECTORY."
  (flet ((verbose (format &rest args)
	   "Print a message if `verbosep' is true."
	   (when verbose-p
	     (apply #'format t format args)
	     (terpri)
	     (or (funcall *yesp-function* :prompt "Continue? ")
		 (error "Merge cancelled.")))))
    (verbose "Merging from directory ~A." directory)
    (in-directory (or directory (current-directory))
      (or directory (setq directory ""))
      (verbose "In ~A." (current-directory))
      #|
      (format t "server path: ~A~%" (build-server-pathname server
							   directory
							   ".manifest"))
      (format t "server manifest: ~A~%" (read-manifest
					 (build-server-pathname server
								directory
								".manifest")))
      |#
      (let ((client-manifest (read-manifest ".manifest")))
	(unwind-protect
	    ;; Loop over server manifest.
	    (loop for server-entry in (read-manifest
				       (build-server-pathname server
							      directory
							      ".manifest"))
	      do
	      (let ((server-mod-time (manifest-entry-time server-entry))
		    (server-entry-name (manifest-entry-name server-entry)))
		(verbose "Considering ~A (mod time ~A)."
			 server-entry-name server-mod-time)
		(verbose "(In ~A.)" (current-directory))
		(if (and server-mod-time (minusp server-mod-time))
		    ;; Marked subtracted on server.
		    (progn
		      (verbose "   Marked subtracted on server.")
		      (when (if (> (file-write-date server-entry-name)
				   (- server-mod-time))
				;; Client file modified after server subtraction.
				;; FIX ed provide *query-io*, and show
				;; stdout properly, so y-or-n-p will work
				;; directly
				(funcall *y-or-n-p-function*
				 :prompt `("~A modified on client and removed on server.  Remove on client?"
					   ,server-entry-name))
				t)
			;; Subtract the file from the client, recursively.
			(format t "Subtracting ~A/~A.~%"
				(current-directory) server-entry-name)
			(setq client-manifest
			      (update-entry client-manifest server-entry))
			(trash-file server-entry-name)))
		    ;; Still exists on server.
		    (let ((client-entry (manifest-assoc client-manifest
							server-entry-name)))
		      (verbose "   Still exists on server.")
		      (if client-entry
			  ;; In client manifest.
			  (progn
			    (verbose "   Exists in client manifest.")
			    (verbose "   (probe-file ~A) => ~A" server-entry-name
				     (probe-file server-entry-name
						 :check-for-links t))
			    (if (probe-file server-entry-name)
				;; Exists on client.
				(let ((client-mod-time (manifest-entry-time client-entry)))
				  (verbose "   Exists on client (mod time ~A)." client-mod-time)
				  (if (directoryp (manifest-entry-name client-entry)
						  :check-for-links t)
				      ;; Recurse into subdirectory.
				      (progn
					(verbose "   Recursing into ~A."
						 (manifest-entry-name client-entry))
					(merge-directory-from-server
					 (concatenate 'simple-string
						      (namestring server)
						      directory)
					 (manifest-entry-name client-entry)
					 verbose-p))
				      (fi (and server-mod-time client-mod-time)
					  ;; Added on server and client.
					  (format t "~A added on client and server, keeping client version.~%"
						  server-entry-name)
					  (or (equal server-mod-time client-mod-time)
					      (merge-file-from-server client-manifest
								      server directory
								      client-entry
								      server-entry)))))
				;; Needed on client.
				(progn
				  (verbose "   Needed on client.")
				  (setq client-manifest
					(add-from-server client-manifest server-entry
							 directory server)))))
			  ;; Added on the server.
			  (progn
			    (verbose "   Added on server.")
			    (setq client-manifest
				  (add-from-server client-manifest server-entry
						   directory server))))))))
	  (verbose "Updating client manifest.")
	  (update-manifest-file client-manifest))))))

(defun merge-from-server (server &optional verbosep)
  "Merge any additions on SERVER into the current source."
  (in-directory "n:"
    (format t "In ~A.~%" (current-directory))
    (merge-directory-from-server server () verbosep)))
