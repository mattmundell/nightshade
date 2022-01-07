;;; Configuration.

(in-package "CONFIG")

(export '(config-pathname from-config-file probe-config-file
	  to-config-file))

(defvar *config-directory* #p"home:.nightshade/")

(defmacro from-config-file ((stream pathname) &body body)
  "Execute $body with $stream open for read on $pathname merged with
   *config-directory*."
  `(progn
     (ensure-directories-exist *config-directory*)
     (with-open-file (,stream (merge-pathnames ,pathname *config-directory*)
			      :direction :input
			      :if-exists ()
			      :if-does-not-exist :create)
       ,@body)))

(defmacro to-config-file ((stream pathname) &body body)
  "Execute $body with $stream open for append on $pathname merged with
   *config-directory*.

   To truncate the file before writing, call (truncate-file $stream) in
   $body."
  `(progn
     (ensure-directories-exist *config-directory*)
     (with-open-file (,stream (merge-pathnames ,pathname *config-directory*)
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create)
       ,@body)))

(defun probe-config-file (pathname)
  "Return a pathname which is the truename of $pathname if it exists after
   being merged with *config-directory*, else return ().  Signal an error
   of type file-error if pathname is wild."
  (probe-file (merge-pathnames pathname *config-directory*)))

(defun config-pathname (pathname)
  "Return the pathname of $pathname in *config-directory*."
  (merge-pathnames pathname *config-directory*))
