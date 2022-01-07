;;; Generic site specific initialization.  This can be used as a template
;;; for "library:site-init" files.

(in-package "SYSTEM")

(setq *short-site-name* "Short site name")
(setq *long-site-name* "Long site name")

; (rplaca
;  (cdr (member :subsystems *herald-items*))
;  '("Loaded subsystems:" terpri))

;;; If the sources are installed on this system then include the following
;;; form and change it to point to the source location to allow the editor
;;; and debugger to find sources for functions in the core.
;(setf (search-list "target:") "<the source tree root>/")
