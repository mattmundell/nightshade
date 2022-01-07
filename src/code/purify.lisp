;;; Storage purifier.

(in-package "LISP")
(export 'ext::purify "EXT")

(alien:def-alien-routine ("purify" %purify) c-call:void
  (static-roots c-call:unsigned-long)
  (read-only-roots c-call:unsigned-long))

;;; COMPACT-ENVIRONMENT-AUX  --  Internal
;;;
;;; Compact the info environment.  Written with gratuitous recursion to
;;; make sure that our (and compact-info-environment's) local variables are
;;; above the stack top when purify runs.
;;;
(defun compact-environment-aux (name n)
  (cond
   ((zerop n)
    (let ((old-ie (car *info-environment*)))
      (setq *info-environment*
	    (list* (make-info-environment :name "Working")
		   (compact-info-environment (first *info-environment*)
					     :name name)
		   (rest *info-environment*)))
      (shrink-vector (c::volatile-info-env-table old-ie) 0)))
   (t
    (compact-environment-aux name (1- n))
    n)))

(defun purify (&key root-structures (environment-name "Auxiliary"))
  "Optimize garbage collection by moving all currently live objects into
   static storage.  Once statically allocated, the objects are kept in
   memory forever, even if all pointers to them are dropped.  This function
   should generally be called after a large system has been loaded and
   initialized.

   $root-structures is an optional list of objects which should be copied
   first to maximize locality.  This should be a list of the main entry
   points for the resulting core image.  The purification process tries to
   localize symbols, functions, etc., in the core image so that paging
   performance is improved.  The default value is () which means that Lisp
   objects will still be localized although probably less optimally than
   they could be.

   Move structures defined with the (:PURE T) option into read-only
   storage, further reducing GC cost.  Also move list and vector slots of
   pure structures into read-only storage.

   $environment-name is gratuitous documentation for compacted version of
   the current global environment (as seen in c::*info-environment*).  If
   () is supplied, then turn off environment compaction."

  (when environment-name (compact-environment-aux environment-name 200))

  (let ((*gc-notify-before*
	 #'(lambda (bytes-in-use)
	     (declare (ignore bytes-in-use))
	     (write-string "[Purifying: ")
	     (force-output)))
	(*internal-gc*
	 #'(lambda ()
	     (%purify (get-lisp-obj-address root-structures)
		      (get-lisp-obj-address nil))))
	(*gc-notify-after*
	 #'(lambda (&rest ignore)
	     (declare (ignore ignore))
	     (write-line "done.]"))))
    #-gencgc (gc t)
    #+gencgc (gc :verbose t))
  nil)
