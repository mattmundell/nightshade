;;; Rules for building the system.

(setq *phonies*
      '("all" "clean" "clone" "core" "dist" "header"
	"test" "test-ed" "test-code"))


;;; Helpers.

(defun get-source-directory ()
  "Return the source directory, FIX prompting for a directory if required."
  (namestring (truename (namify build:*source-directory*))))

(defun get-build-directory ()
  "Return the build directory, prompting for a directory if required."
  (namestring (truename (namify build:*build-directory*))))

(defun get-builder-directory ()
  "Return the builder directory, prompting for a directory if required."
  (namestring (truename (namify build:*builder-directory*))))

#|
(defun get-source-directory ()
  "Return the source directory, FIX prompting for a directory if required."
  (namify user::*source-directory*))

(defun get-build-directory ()
  "Return the build directory, prompting for a directory if required."
  (namify user::*build-directory*))

(defun get-builder-directory ()
  "Return the builder directory, prompting for a directory if required."
  (namify user::*builder-directory*))
|#


;;; The rules.

;(deftarget "all" (#| "kernel" "lisp" "ed" "dist" |#))

#|
~/src/nightshade/builder/bin/nightshade

(build:build "n:src/" "all")
|#

(deftarget "all" ()
  "Build all subsystems."
  (setf (search-list ":") "home:") ; FIX
  (setf user::src (get-source-directory))
  (setf user::target (get-build-directory))
  (setf user::systems '(:lisp :compiler :ed :kernel))
  (load (open (format () "~A/tools/build-world.lisp" (get-source-directory)))))

(deftarget "lisp" ()
  "Build the Lisp code."
  (setf (search-list ":") "home:") ; FIX
  (setf user::src (get-source-directory))
  (setf user::target (get-build-directory))
  (setf user::systems '(:lisp))
  (load (open (format () "~A/tools/build-world.lisp" (get-source-directory)))))

(deftarget "editor" ()
  "Build the Editor code."
  (setf (search-list ":") "home:") ; FIX
  (setf user::src (get-source-directory))
  (setf user::target (get-build-directory))
  (setf user::systems '(:ed))
  (load (open (format () "~A/tools/build-world.lisp" (get-source-directory)))))

(deftarget "boot" ()
  "Build the boot code."
  (setf (search-list ":") "home:") ; FIX
  (setf user::src (get-source-directory))
  (setf user::target (get-build-directory))
  (setf user::systems '(:boot))
  (load (open (format () "~A/tools/build-world.lisp" (get-source-directory)))))

(deftarget "old-boot-cd" ()
  "Build the boot CD."
  (setf (search-list ":") "home:") ; FIX
  (setf user::src (get-source-directory))
  (setf user::target (get-build-directory))
  (load (open (format () "~A/tools/cdbuild.lisp" (get-source-directory)))))

#|
~/src/nightshade/builder/bin/nightshade

(progn
  (setf (search-list "target:") '("/home/mattm/src/nightshade/build/" "/home/mattm/src/nightshade/src/"))
  (declaim (special *save-target))
  (setf *save-target* "/usr/local/src/nightshade/src/")
  (load (open "target:tools/cdbuild.lisp")))
|#

(deftarget "boot-core" ( #| "boot" |#)
  "Build the boot CD core."
  ;; FIX can slaves run before ed is loaded?
  (let ((commands
	 (format ()
		 "(setf (search-list \"target:\") '(\"~A/\" \"~A/../src/\"))
		  (declaim (special *save-target))
		  (setf *save-target* \"/usr/local/src/nightshade/src/\")
		  (load (open \"target:tools/cdbuild.lisp\"))
		  (quit)"
		 (get-build-directory)
		 (get-build-directory))))
;     (system:with-screen
;      (let ((process
; 	    (ext:run-program
; 	     (format () "~A/bin/nightshade" (get-build-directory))
;            ()
; 	     :wait t :input :stream
; 	     :output *standard-output* :error :output)))
;        (write-string commands (process-input process))))))
    (with-input-from-string (input commands)
      (in-directory (concat (namestring (lisp::directorify (get-build-directory)))
			    "lisp/")
	(ext:run-program
	 (format () "~A/bin/nightshade" (get-build-directory))
	 ()
	 :wait t :input input
	 :output *standard-output*
	 :error :output)))))

(deftarget "cd" ( #| "boot-core" |#)
  "Build the boot CD iso."
  (if (probe-file "target:nightshade.iso")
      (delete-file "target:nightshade.iso"))
  (cd:make-cd-image "target:nightshade.iso"
		    :boot
		    #+(and mach sparc) "/usr/tmp/cd-boot.core"
		    #-(and mach sparc) "target:bin/cd-boot.core"
		    :verbose t))

#|
~/src/nightshade/builder/bin/nightshade

(build:build "n:src/" clean)
|#

(deftarget "clean" ()
  "Clean the build tree."
  (setf user::build (get-build-directory))
  (load (format () "~A/tools/clean-build.lisp" (get-source-directory))))

(deftarget "clean-lisp" ()
  "Clean the Lisp code in the build tree."
  (setf user::build (format () "~A/code" (get-build-directory)))
  (load (format () "~A/tools/clean-build.lisp" (get-source-directory))))

(deftarget "clean-editor" ()
  "Clean the editor code in the build tree."
  (setf user::build (format () "~A/ed" (get-build-directory)))
  (load (format () "~A/tools/clean-build.lisp" (get-source-directory))))

(deftarget "clean-bin" ()
  "Clean the C image loader program directory."
  (in-directory (concat (namestring (lisp::directorify (get-build-directory)))
			"bin/")
    (ext::run-program
     "/bin/sh" (list "-c" "make clean")
     :wait t :input ()
     :output *standard-output* :error :output)))

#|
~/src/nightshade/build/bin/nightshade -core ~/src/nightshade/build/bin/kernel.core

(progn
  (setf (search-list "target:") '("/home/mattm/src/nightshade/build/" "/home/mattm/src/nightshade/src/"))
  (declaim (special *save-target))
  (setf *save-target* "/usr/local/src/nightshade/src/")
  (load (open "target:tools/worldload.lisp")))
|#

(deftarget "core" ( #| "kernel" "lisp" "ed" |#)
  "Build the core."
  ;; FIX can slaves run before ed is loaded?
  (let ((commands
	 (format ()
		 "(setf (search-list \"target:\") '(\"~A/\" \"~A/../src/\"))
		  (declaim (special lisp::*save-target))
		  (setq lisp::*save-target* \"/usr/local/src/nightshade/src/\")
		  (load (open \"target:tools/worldload.lisp\"))
		  ; FIX on error switch back to standard input,
		  ;     for interaction.
		  ; FIX repl has something for this already, err: ~~ switching to terminal after eof
		  ;(setf *standard-input* *standard-output*)
		  (format t \"Failed.\")
		  (quit)"
		 (get-build-directory)
		 (get-build-directory))))
;     (system:with-screen
;      (let ((process
; 	    (ext:run-program
; 	     (format () "~A/bin/nightshade" (get-build-directory))
; 	     (list "-core" (format () "~A/bin/kernel.core"
; 				   (get-build-directory)))
; 	     :wait t :input :stream
; 	     :output *standard-output* :error :output)))
;        (write-string commands (process-input process))))))
    (with-input-from-string (input commands)
      (in-directory (concat (namestring (lisp::directorify (get-build-directory)))
			    "bin/")
	(ext:run-program
	 (format () "~A/bin/nightshade" (get-build-directory))
	 (list "-core" (format () "~A/bin/kernel.core"
			       (get-build-directory)))
	 :wait t :input input
	 :output *standard-output*
	 :error :output)))))

(deftarget "header" ()
  "Build the header file for the core loader program."
  (load "c:generic/new-genesis")
  (let ((dummy (pick-new-name)))
    (lisp::genesis () "n:build/bin/nightshade.nm" dummy
		   "n:build/bin/nightshade.map"
		   "n:build/bin/internals.h")
    (if (probe-file dummy) (delete-file dummy))))

(deftarget "bin" ()
  "Build the core loader program."
  (in-directory (concat (namestring (lisp::directorify (get-build-directory)))
			"bin/")
    (ext::run-program
     "/bin/sh" (list "-c" "make all")
     :wait t :input ()
     :output *standard-output* :error :output)))

(deftarget "clone" ()
  "Update the builder to be a copy of the latest build, keeping the old
   builder."
  (format t "Cloning build as new builder...~%")
  (in-directory "n:"
    (let* ((old-build (file-namestring (namify (truename "build/"))))
	   (new-build (string (1+ (parse-integer old-build))))
	   (new-builder (string (parse-integer old-build))))
      (if (probe-file new-build)
	  (error "New build directory already exists: ~A" new-build))
      (dired:copy-file old-build new-build :check-for-links t)
      (delete-file "build")
      (symlink-file "build" new-build)
      (delete-file "builder")
      (symlink-file "builder" new-builder)
      (format t "Cloning build as new builder... done.~%"))))

(deftarget "dist" ( #| "kernel" "lisp" "ed" |#)
  "Build a binary and source distribution.  Return the name of the
   resulting archive."
  (format t "Updating manifest...~%")
  (sync:update-manifest-times "n:")
  (let* ((base (pick-new-dir))
	 (bin (concatenate 'simple-string
			   base
			   "bin/"))
	 (lib (concatenate 'simple-string
			   base
			   "lib/nightshade/"))
	 (man (concatenate 'simple-string
			   base
			   "man/man1/"))
	 (src (concatenate 'simple-string
			   base
			   "src/nightshade/"))
	 (src-builder-file "1")
	 (src-builder (concatenate 'simple-string
				   src
				   src-builder-file
				   "/"))
	 (dest-root (concatenate 'simple-string
				 base
				 "src/nightshade/"))
; 	 (share (concatenate 'simple-string
; 			     base
; 			     "share/nightshade/"))
	 )
    (format t "Copying to ~A...~%" base)
    (ensure-directories-exist bin)
    (ensure-directories-exist lib)
    (ensure-directories-exist man)
;    (ensure-directories-exist share)
    (ensure-directories-exist src)
    (ensure-directories-exist src-builder)
    (in-directory src
      (symlink-file "builder" src-builder-file)
      (symlink-file "build" src-builder-file))
    (in-directory src-builder
      (ensure-directories-exist "assembly/alpha/")
      (ensure-directories-exist "assembly/hppa/")
      (ensure-directories-exist "assembly/mips/")
      (ensure-directories-exist "assembly/rt/")
      (ensure-directories-exist "assembly/sparc/")
      (ensure-directories-exist "assembly/x86/")
      (ensure-directories-exist "bin/")
      (ensure-directories-exist "code/")
      (ensure-directories-exist "compiler/alpha/")
      (ensure-directories-exist "compiler/generic/")
      (ensure-directories-exist "compiler/hppa/")
      (ensure-directories-exist "compiler/mips/")
      (ensure-directories-exist "compiler/rt/")
      (ensure-directories-exist "compiler/sparc/")
      (ensure-directories-exist "compiler/x86/")
      (ensure-directories-exist "ed/")
      (ensure-directories-exist "lib/nightshade/packages/")
      (ensure-directories-exist "src/")
      (ensure-directories-exist "tools/")
      (in-directory "bin/"
	(to-file (out "nightshade.nm")
	  (format out "Map file for nightshade version 0~%" out))
	(symlink-file "nightshade" "../../../../bin/nightshade")
	(symlink-file "lisp.core" "../../../../lib/nightshade/lisp.core")
	(symlink-file "Makefile" "../../src/bin/GNUmakefile")
	(symlink-file "Config" "../../src/bin/Config.linux_gencgc"))
      (in-directory "lib/nightshade/"
	(symlink-file "lisp.core" "../../bin/lisp.core"))
      (in-directory "src/"
	(symlink-file "nightshade" "../../")))
    (copy-file (concatenate 'simple-string
			    (get-build-directory)
			    "/features.lisp")
	       src-builder)
    (ensure-directories-exist dest-root)
    (let ((source-directory (get-source-directory)))
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/bin/nightshade")
		 bin)
      (unix:unix-chmod (concatenate 'simple-string
				    bin
				    "nightshade")
		       (logior unix:execall unix:readall))
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/bin/nightshade.nm")
		 bin)
      (in-directory bin
	(symlink-file "ne" "nightshade")
	(unix:unix-chmod "ne"
			 (logior unix:execall unix:readall))
	(symlink-file "ni" "nightshade")
	(unix:unix-chmod "ni"
			 (logior unix:execall unix:readall))
	(symlink-file "net" "nightshade")
	(unix:unix-chmod "net"
			 (logior unix:execall unix:readall))
	(symlink-file "nit" "nightshade")
	(unix:unix-chmod "nit"
			 (logior unix:execall unix:readall)))
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/bin/lisp.core")
		 lib)
      (copy-file (concatenate 'simple-string
			      source-directory
			      "/etc/nightshade.1")
		 man)
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/spell-dictionary.bin")
		 lib)
      (sync:copy-manifest-tree (format () "~A/../" source-directory)
			       dest-root)
      (copy-file (concatenate 'simple-string
			      source-directory
			      "/code/dist-site-init.lisp")
		 (concatenate 'simple-string
			      lib
			      "site-init.lisp"))
      (in-directory src-builder
	(in-directory "lib/nightshade/"
	  (copy-file (concatenate 'simple-string
				  source-directory
				  "/code/dist-site-init.lisp")
		     "site-init.lisp")
	  (copy-file (concatenate 'simple-string
				  (get-build-directory)
				  "/spell-dictionary.bin")
		     "spell-dictionary.bin")))
      (format t "Creating archive...~%")
      (let* ((name (format () "~A/../../nightshade-~A--~A.tar.bz2"
			   (get-build-directory)
			   (read-line (open (format ()
						    "~A/VERSION"
						    (get-source-directory))))
			   ;; FIX get build time from core
			   ;; FIX improve format-time or format
			   (multiple-value-bind
			       (secs mins hours day month year)
			       (decode-universal-time (get-universal-time))
			     (declare (ignore secs))
			     (string-downcase
			      ;; FIX ~2D  so that prints 08 for 8
			      (format () "~2,'0D-~2,'0D-~2,'0D-~2,'0Dh~2,'0D"
				      year
				      month
				      day
				      hours
				      mins)))))
	     (cmd (format () "tar jcvf ~A ." name)))
	(format t "Shell command: ~A" cmd)
	(in-directory base
	  (ext::run-program
	   "/bin/sh" (list "-c" cmd)
	   :wait t :input ()
	   :output *standard-output* :error :output))
	(dired:delete-file base :clobber t :recurse t)
	(format t "Done.~%")
	name))))


;;; Tests.

;; FIX stop on fail

(deftarget "test" ("test-code" "test-ed")
  "Run all the tests.")

(deftarget "test-code" ()
  "Run the code tests."
  (deftest:test-from-dir "n:src/tests/code/"))

(deftarget "test-ed" ()
  "Run the editor tests."
  (deftest:test-from-dir "n:src/tests/ed/"))


;;; Documentation.

(deftarget "doc" ()
  "Generate the documentation."
  (format t "Removing old documentation...~%")
  (ensure-directories-exist (directorify (get-build-directory)))
  (in-directory (directorify (get-build-directory))
    (in-directory "doc/"
      (ensure-directories-exist "html/")
      (ensure-directories-exist "text/")
      (ensure-directories-exist "latex/")))
  (do-files (file
	     (truename (merge-pathnames
			"doc/html/"
			(directorify (get-build-directory)))))
    (delete-file file))
  (do-files (file
	     (truename (merge-pathnames
			"doc/text/"
			(directorify (get-build-directory)))))
    (delete-file file))
  (do-files (file
	     (truename (merge-pathnames
			"doc/latex/"
			(directorify (get-build-directory)))))
    (delete-file file))
  (format t "Copying in style sheets...~%")
  (symlink-file (merge-pathnames "doc/html/style.css"
				 (directorify (get-build-directory)))
		"etc:style.css")
  (symlink-file (merge-pathnames "doc/text/style.css"
				 (directorify (get-build-directory)))
		"etc:style.css")
  (symlink-file (merge-pathnames "doc/latex/style.css"
				 (directorify (get-build-directory)))
		"etc:style.css")
  ;(format t "   ~A~%" (docnode:docnode-to-doc "Lisp"))
  ;(format t "   ~A~%" (docnode:docnode-to-doc "Buffers"))
  ;(format t "   ~A~%" (docnode:docnode-to-doc "System Usage"))
  (in-directory (directorify (get-build-directory))
    (in-directory "doc/"
      (in-directory "html/"
	(format t "Generating new HTML documentation...~%")
	(do-strings (node-name node lisp::*documentation*)
	  (declare (ignore node))
	  (let ((pathname (format () "~A.html" node-name)))
	    (format t "  ~A~%" node-name)
	    (with-open-file (stream pathname
				    :direction :output
				    :if-does-not-exist :create)
	      ;(format t "  ~A~%" (type-of (docnode:docnode-to-doc node-name)))
	      ;(format t "   ~A~%" (docnode:docnode-to-doc node-name))
	      (doc:doc-to-html (docnode:docnode-to-doc node-name)
			       stream
			       (format () "~A - Documentation - Nightshade" node-name)
			       "style.css"))
	    ;(return)
	    )))
      (in-directory "text/"
	(format t "Generating new plain text documentation...~%")
	(do-strings (node-name node lisp::*documentation*)
	  (declare (ignore node))
	  (let ((pathname (format () "~A.txt" node-name)))
	    (format t "  ~A~%" node-name)
	    (with-open-file (stream pathname
				    :direction :output
				    :if-does-not-exist :create)
	      (doc:doc-to-text (docnode:docnode-to-doc node-name)
			       stream
			       (css:css "style.css"))))))
      (in-directory "latex/"
	(format t "Generating new LaTeX documentation...~%")
	(do-strings (node-name node lisp::*documentation*)
	  (declare (ignore node))
	  (let ((pathname (format () "~A.tex" node-name)))
	    (format t "  ~A~%" node-name)
	    (with-open-file (stream pathname
				    :direction :output
				    :if-does-not-exist :create)
	      (doc:doc-to-latex (docnode:docnode-to-doc node-name)
				stream
				(css:css "style.css")))))))))
