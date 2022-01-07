;;;; Site specific initialization.

(in-package "SYSTEM")

;(setq *short-site-name* "Short site name")
;(setq *long-site-name* "Long site name")

;; Set the location of the sources at this site, enabling the editor and
;; inspector to find them.  Try for a directory relative to the location of
;; the currently running program.  If that fails, use /usr/local/.
;;
;; A site administrator can set this if s/he splits up the source, binary
;; and libs.
;;
(setf (search-list "n:")
      ;; FIX Consider consistency when NIGHTSHADELIB set.
      (or (let ((loc (concatenate 'simple-string
				  (directory-namestring
				   ext:*command-line-utility-name*)
				  "../src/nightshade/")))
	    ;; If loc is relative the current dir must be the same as it
	    ;; was when the program started.
	    (if (probe-file loc) (list (truename loc))))
	  "/usr/local/src/nightshade/"))

(setf (search-list "target:") "n:src/")
