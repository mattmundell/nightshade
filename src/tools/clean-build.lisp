;;; -*- Package: User -*-
;;;
;;; Clean the Lisp part (i.e. built from Lisp code) of a build directory.
;;;
;;; Intended for a command like
;;;
;;;    lisp -load src/tools/clean-build.lisp
;;;
;;; or
;;;
;;;    build-small/lisp/lisp -core build-small/lisp/lisp.core \
;;;        -eval "(defvar build \"build2/\")" \
;;;        -load src/tools/clean-build.lisp

(in-package "USER")

(defvar build "./build/"
  "Location of the build directory to clean.")

(defun filter (file)
  (let ((type (pathname-type file)))
    (when (and type
	       (or (string= type "fasl")
		   (eq (char type (1- (length type))) #\f)
		   (string= type "assem")))
      (delete-file file)))
  nil)

(defun clean-build (directory)
  "Clean build Directory."
  (format t "Cleaning ~A...~%" directory)
  (or (probe-file directory)
      (error "Directory must exist."))
  ;; Compiled files.
  (map-files (format nil "~A/" directory) #'filter
	     :all nil :backups nil :recurse t)
  ;; Logs.
  (dolist (file (lisp::enumerate-names (format nil "~A/*.log" directory)))
    (delete-file file))
  (dolist (file (lisp::enumerate-names (format nil "~A/*.log.OLD" directory)))
    (delete-file file))
  t)

(clean-build build)
;(quit)
