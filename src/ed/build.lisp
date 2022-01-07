;; Interface to build system.

(in-package "ED")

(defcommand "Build" (p)
  "Prompt for and build one of the targets defined in Build.lisp in the
   current directory.  With an argument first prompt for a directory to
   make the directory current."
  "Prompt for and build one of the targets defined in Build.lisp in the
   current directory.  With an argument first prompt for a directory to
   make the directory current."
  (if p (error "FIX p"))
  (build:with-build (current-directory)
    (build:build-target
     (prompt-for-keyword (list build:*targets*)
			 :default build:*first-target*
			 :prompt "Target: "
			 :help "Enter name of target to build."))))

(defcommand "Rebuild" (p)
  ;; FIX
  "Prompt for and build one of the targets defined in Build.lisp in the
   current directory.  With an argument first prompt for a directory to
   make the directory current."
  "Prompt for and build one of the targets defined in Build.lisp in the
   current directory.  With an argument first prompt for a directory to
   make the directory current."
  (if p (error "FIX p"))
  (build:with-build (current-directory)
    (build:build-target
     (prompt-for-keyword (list build:*targets*)
			 :default build:*first-target*
			 :prompt "Target: "
			 :help "Enter name of target to build.")
     :force)))
