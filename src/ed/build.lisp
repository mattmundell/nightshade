;; Interface to build system.

(in-package "ED")

(defvar *build-target-history* (make-ring 50)
  "Previous build targets.")

(defvar *build-target-history-pointer* 0
  "Current position during a historical exploration.")

(defcommand "Build" (p)
  "Prompt for and build one of the targets defined in Build.lisp in the
   current directory.

   With an argument first prompt for a directory to make the directory
   current."
  (if p (error "FIX p"))
  (build:with-build (current-directory)
    (build:build-target
     (prompt-for-keyword (list build:*targets*)
			 :default build:*first-target*
			 :history *build-target-history*
			 :history-pointer
			 '*build-target-history-pointer*
			 :prompt "Target: "
			 :help "Enter name of target to build."))))

(defcommand "Rebuild" (p)
  "Prompt for and build one of the targets defined in Build.lisp in the
   current directory, even if the targets are up to date.

   With an argument first prompt for a directory to make the directory
   current."
  (if p (error "FIX p"))
  (build:with-build (current-directory)
    (build:build-target
     (prompt-for-keyword (list build:*targets*)
			 :default build:*first-target*
			 :history *build-target-history*
			 :history-pointer
			 '*build-target-history-pointer*
			 :prompt "Target: "
			 :help "Enter name of target to build.")
     :force)))
