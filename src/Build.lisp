;;; Rules for building the system.

(setq *phonies* '("all" "test" "test-ed" "test-code" "clean"))


;;; Helpers.

(defun get-source-directory ()
  "Return the source directory, FIX prompting for a directory if required."
  (namify build:*source-directory*))

(defun get-build-directory ()
  "Return the build directory, prompting for a directory if required."
  (namify build:*build-directory*))

(defun get-builder-directory ()
  "Return the builder directory, prompting for a directory if required."
  (namify build:*builder-directory*))

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
~/src/nightshade/builder/lisp/nightshade -core ~/src/nightshade/builder/lisp/lisp.core

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

(deftarget "editor" ()
  "Build the Lisp code."
  (setf (search-list ":") "home:") ; FIX
  (setf user::src (get-source-directory))
  (setf user::target (get-build-directory))
  (setf user::systems '(:kernel))
  (load (open (format () "~A/tools/build-world.lisp" (get-source-directory)))))

#|
~/src/nightshade/builder/lisp/nightshade -core ~/src/nightshade/builder/lisp/lisp.core

(build:build "n:src/" clean)
|#

(deftarget "clean" ()
  "Clean the build tree."
  (setf user::build (get-build-directory))
  (load (format () "~A/tools/clean-build.lisp" (get-source-directory))))

(deftarget "clean=lisp" ()
  "Clean the Lisp code in the build tree."
  (setf user::build (format () "~A/code" (get-build-directory)))
  (load (format () "~A/tools/clean-build.lisp" (get-source-directory))))

(deftarget "clean-editor" ()
  "Clean the editor code in the build tree."
  (setf user::build (format () "~A/ed" (get-build-directory)))
  (load (format () "~A/tools/clean-build.lisp" (get-source-directory))))

#|
~/src/nightshade/build/lisp/nightshade -core ~/src/nightshade/build/lisp/kernel.core

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
		  (declaim (special *save-target))
		  (setf *save-target* \"/usr/local/src/nightshade/src/\")
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
; 	     (format () "~A/lisp/nightshade" (get-build-directory))
; 	     (list "-core" (format () "~A/lisp/kernel.core"
; 				   (get-build-directory)))
; 	     :wait t :input :stream
; 	     :output *standard-output* :error :output)))
;        (write-string commands (process-input process))))))
    (with-input-from-string (input commands)
      (in-directory (concat (namestring (lisp::directorify (get-build-directory)))
			    "lisp/")
	(ext:run-program
	 (format () "~A/lisp/nightshade" (get-build-directory))
	 (list "-core" (format () "~A/lisp/kernel.core"
			       (get-build-directory)))
	 :wait t :input input
	 :output *standard-output*
	 :error :output)))))

(deftarget "c-core" ()
  "Build the C core.  Return the name of the resulting archive."
  (in-directory (concat (namestring (lisp::directorify (get-build-directory)))
			"lisp/")
    (ext::run-program
     "/bin/sh" (list "-c" "make all")
     :wait t :input ()
     :output *standard-output* :error :output)))

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
	 (src-builder-file "builder1")
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
      (ensure-directories-exist "code/")
      (ensure-directories-exist "compiler/alpha/")
      (ensure-directories-exist "compiler/generic/")
      (ensure-directories-exist "compiler/hppa/")
      (ensure-directories-exist "compiler/mips/")
      (ensure-directories-exist "compiler/rt/")
      (ensure-directories-exist "compiler/sparc/")
      (ensure-directories-exist "compiler/x86/")
      (ensure-directories-exist "ed/")
      (ensure-directories-exist "lisp/")
      (ensure-directories-exist "tools/")
      (in-directory "lisp/"
	(symlink-file "Makefile"
		      "../../src/lisp/GNUMakefile")
	(symlink-file "Config"
		      "../../src/lisp/Config.linux_gencgc")))

    (copy-file (concatenate 'simple-string
			    (get-build-directory)
			    "/features.lisp")
	       src-builder)
    (ensure-directories-exist dest-root)
    (let ((source-directory (get-source-directory)))
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/lisp/nightshade")
		 bin)
      (unix:unix-chmod (concatenate 'simple-string
				    bin
				    "nightshade")
		       (logior unix:execall unix:readall))
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/lisp/nightshade.nm")
		 bin)
      (copy-file (concatenate 'simple-string
			      source-directory
			      "/etc/ne")
		 bin)
      (unix:unix-chmod (concatenate 'simple-string
				    bin
				    "ne")
		       (logior unix:execall unix:readall))
      (copy-file (concatenate 'simple-string
			      source-directory
			      "/etc/ni")
		 bin)
      (unix:unix-chmod (concatenate 'simple-string
				    bin
				    "ni")
		       (logior unix:execall unix:readall))
      (copy-file (concatenate 'simple-string
			      source-directory
			      "/etc/net")
		 bin)
      (unix:unix-chmod (concatenate 'simple-string
				    bin
				    "net")
		       (logior unix:execall unix:readall))
      (copy-file (concatenate 'simple-string
			      source-directory
			      "/etc/nit")
		 bin)
      (unix:unix-chmod (concatenate 'simple-string
				    bin
				    "nit")
		       (logior unix:execall unix:readall))
      (copy-file (concatenate 'simple-string
			      (get-build-directory)
			      "/lisp/lisp.core")
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
  (do-files (file
	     (truename (merge-pathnames
			"doc/"
			(directorify (get-build-directory)))))
    (delete-file file))
  (format t "Copying in style sheet...~%")
  (symlink-file (merge-pathnames "doc/style.css"
				 (directorify (get-build-directory)))
		"etc:style.css")
  (format t "Generating new documentation...~%")
  ;(format t "   ~A~%" (docnode:docnode-to-doc "Lisp"))
  ;(format t "   ~A~%" (docnode:docnode-to-doc "Buffers"))
  ;(format t "   ~A~%" (docnode:docnode-to-doc "System Usage"))
  (do-strings (node-name node lisp::*documentation*)
    (declare (ignore node))
    (let ((pathname (merge-pathnames (format () "doc/~A.html" node-name)
				     (lisp::directorify (get-build-directory)))))
      (ensure-directories-exist pathname)
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
