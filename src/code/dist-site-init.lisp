;;; Site specific initialization.

(in-package "SYSTEM")

;(setq *short-site-name* "Short site name")
;(setq *long-site-name* "Long site name")

;;; If the sources are installed on this system then include the following
;;; form and change it to point to the source location to allow the editor
;;; and inspector to find sources for functions in the core.
(setf (search-list "target:") "/usr/local/src/nightshade/src/")

(setf (search-list "n:") "/usr/local/src/nightshade/")
(setf (search-list "ni:") "/usr/local/src/nightshade/")
