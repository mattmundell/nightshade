;;; -*- Package: USER -*-
;;;
;;; This file compiles the editor.

#+bootstrap
(progn
  (when (ext:get-command-line-switch "slave")
    (error "Cannot compile the editor in a slave due to its clobbering needed
    typescript routines by renaming the package."))

  ;;; Blast the old packages in case they are around.  We do this solely to
  ;;; prove the editor can compile cleanly without its having to exist
  ;;; already.
  ;;;
  (copy-packages '("ED" "HI")))


;;; Stuff to set up the packages the editor uses.
;;;
(unless (find-package "HEMLOCK-INTERNALS")
  (make-package "HEMLOCK-INTERNALS"
		:nicknames '("HI")
		:use '("LISP" "EXTENSIONS" "SYSTEM")))

(unless (find-package "HEMLOCK")
  (make-package "HEMLOCK"
		:nicknames '("ED")
		:use '("LISP" "HEMLOCK-INTERNALS" "EXTENSIONS" "SYSTEM")))
;;;
(export 'c::compile-from-stream (find-package "C"))


(in-package "EXTENSIONS")

;;; FIX else assumes editor loaded
(or (fboundp 're-initialize-key-events)
    (defun re-initialize-key-events () nil))

(in-package "USER")

(defvar *byte-compile* #+small t #-small :maybe)

(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

(with-compiler-log-file
    ("target:compile-ed.log"
     :optimize
     '(optimize (debug #-small 2 #+small .5)
		(speed 2) (inhibit-warnings 2)
		(safety #-small 1 #+small 0))
     :optimize-interface
     '(optimize-interface (debug .5))
     :context-declarations
     '(((:or :external (:match "$%SET-"))
	(declare (optimize (safety 2))
		 (optimize-interface (debug 1))))
       (:macro (declare (optimize (speed 0))))))

(comf "target:code/globals")
(comf "target:code/struct" :load t)
(comf "target:ed/charmacs")
(comf "target:ed/key-event" :load t)
(comf "target:ed/struct")
;(comf "target:ed/struct-ed")
(comf "target:ed/rompsite")
;;;
;;; This is necessary since all the #k uses in the editor will expand into
;;; EXT:MAKE-KEY-EVENT calls with keysyms and bits from the compiling Lisp,
;;; not for the Lisp new code will run in.  This destroys the compiling
;;; Lisp with respect to running code with #k's compiled for it, but it
;;; causes the compilation to see new keysyms, modifiers, and CLX modifier
;;; maps correctly for the new system.
;;;
(ext::re-initialize-key-events)
(comf "target:ed/keysym-defs")
(comf "target:ed/input")
(comf "target:ed/macros" :byte-compile t :load t)
(comf "target:ed/line")
(comf "target:ed/ring")
(comf "target:ed/table")
(comf "target:ed/htext1")
(comf "target:ed/htext2")
(comf "target:ed/htext3")
(comf "target:ed/htext4")
(comf "target:ed/search1")
(comf "target:ed/search2")
(comf "target:ed/linimage")
(comf "target:ed/cursor")
(comf "target:ed/syntax")
(comf "target:ed/winimage")
#+clx (comf "target:ed/hunk-draw")
;(comf "target:ed/bit-stream")
(comf "target:ed/termcap")
(comf "target:ed/display")
#+clx (comf "target:ed/bit-display")
(comf "target:ed/tty-disp-rt")
(with-compilation-unit (:optimize '(optimize (safety 2) (debug 3)))
  (comf "target:ed/tty-display")) ; FIX Buggy...
;(comf "target:ed/tty-stream")
(comf "target:ed/pop-up-stream")
(comf "target:ed/screen")
#+clx (comf "target:ed/bit-screen")
(comf "target:ed/tty-screen")
(comf "target:ed/window")
(comf "target:ed/font")
(comf "target:ed/interp")
(comf "target:ed/vars")
(comf "target:ed/buffer")
(comf "target:ed/files")
(comf "target:ed/streams")
(comf "target:ed/echo" :byte-compile t)
(comf "target:ed/main" :byte-compile t)
(comf "target:ed/echocoms" :byte-compile t)
(comf "target:ed/defsyn")

(comf "target:ed/ts-buf")
(comf "target:ed/ts-stream")

(with-compilation-unit
    (:optimize
     '(optimize (safety 2) (speed 0))
     :context-declarations
     '(((:match "-COMMAND$")
	(declare (optimize (safety #+small 0 #-small 1))
		 (optimize-interface (safety 2))))))

(comf "target:ed/at-point" :byte-compile t)
(comf "target:ed/command" :byte-compile t)
(comf "target:ed/morecoms" :byte-compile t)
(comf "target:ed/undo" :byte-compile t)
(comf "target:ed/killcoms" :byte-compile t)
(comf "target:ed/searchcoms" :byte-compile t)
(comf "target:ed/filecoms" :byte-compile t)
(comf "target:ed/indent" :byte-compile t)
(comf "target:ed/highlight")
(comf "target:ed/lispmode")
(comf "target:ed/comments" :byte-compile t)
(comf "target:ed/fill")
(comf "target:ed/parse" :load t)
(comf "target:ed/parse-scribe")
(comf "target:ed/info" :byte-compile t)
(comf "target:ed/ginfo" :byte-compile t)
(comf "target:ed/text" :byte-compile t)
(comf "target:ed/doccoms" :byte-compile t)
(comf "target:ed/srccom" :byte-compile t)
(comf "target:ed/abbrev" :byte-compile t)
(comf "target:ed/group" :byte-compile t)
(comf "target:ed/overwrite" :byte-compile t)
(comf "target:ed/gosmacs" :byte-compile t)
(comf "target:ed/eval-server" :byte-compile t)
(comf "target:ed/dylan" :byte-compile t)
(comf "target:ed/lispbuf" :byte-compile t)
(comf "target:ed/lispeval" :byte-compile t)
(comf "target:ed/icom" :byte-compile t)
(comf "target:ed/hi-integrity" :byte-compile t)
(comf "target:ed/ed-integrity" :byte-compile t)
(comf "target:ed/scribe" :byte-compile t)
(comf "target:ed/c")
(comf "target:ed/pascal" :byte-compile t)
(comf "target:ed/shell-script" :byte-compile t)
(comf "target:ed/python" :byte-compile t)
(comf "target:ed/make" :byte-compile t)
(comf "target:ed/m4" :byte-compile t)
(comf "target:ed/edit-defs" :byte-compile t)
(comf "target:ed/auto-save" :byte-compile t)
(comf "target:ed/register" :byte-compile t)
#+clx (comf "target:ed/xcoms" :byte-compile t)
(comf "target:ed/unixcoms" :byte-compile t)
(comf "target:ed/dired" :byte-compile t)
(comf "target:ed/db" :byte-compile t)
(comf "target:ed/mh")
(comf "target:ed/compile" :byte-compile t)
(comf "target:ed/diredcoms" :byte-compile t)
(comf "target:ed/bufed" :byte-compile t)
(comf "target:ed/lisp-lib" :byte-compile t)
(comf "target:ed/completion" :byte-compile t)
;; FIX results in make-new-shell arg 1 nil when passed as t via "s c l i b" via .h "saving s c l i b"
(comf "target:ed/shell" :byte-compile t)
;(comf "target:ed/shell")
(comf "target:ed/debug" :byte-compile t)
(comf "target:ed/netnews" :byte-compile t)
(comf "target:ed/rcs" :byte-compile t)
;; FIX results in type error when kill descr-buffer in cvs-commit recurse edit of descr-buffer
(comf "target:ed/vc" :byte-compile t)
;(comf "target:ed/vc")
(comf "target:ed/dabbrev" :byte-compile t)
(comf "target:ed/calendar")
(comf "target:ed/sort" :byte-compile t)
(comf "target:ed/csv")
(comf "target:ed/www")
(comf "target:ed/outline" :byte-compile t)
(comf "target:ed/buildcoms" :byte-compile t)
(comf "target:ed/break" :byte-compile t)

) ;WITH-COMPILATION-UNIT for commands

;; Stuff we want compiled native:

(comf "target:ed/spell-rt")
(comf "target:ed/spell-corr")
(comf "target:ed/spell-aug")
(comf "target:ed/spell-build")
(comf "target:ed/spellcoms")
(comf "target:ed/kbdmac")

(comf "target:ed/bindings")
(comf "target:ed/hacks")

) ;WITH-COMPILER-LOG-FILE

(unless (probe-file "target:ed/spell-dictionary.bin")
  (load "target:ed/spell-rt")
  (load "target:ed/spell-corr")
  (load "target:ed/spell-aug")
  (load "target:ed/spell-build")
  (funcall (fdefinition (intern "BUILD-DICTIONARY" "SPELL"))
	   "target:ed/spell-dictionary.text"
	   "target:ed/spell-dictionary.bin"))

(cat-if-anything-changed
 "target:ed/ed-library"
 "target:ed/rompsite"
 "target:ed/struct"
 ;"target:ed/struct-ed"
 "target:ed/charmacs"
 "target:ed/key-event"
 ;(ext::re-initialize-key-events)
 "target:ed/keysym-defs"
 "target:ed/input"
 "target:ed/macros"
 "target:ed/line"
 "target:ed/ring"
 "target:ed/vars"
 "target:ed/buffer"
 "target:ed/interp"
 "target:ed/syntax"
 "target:ed/htext1"
 "target:ed/htext2"
 "target:ed/htext3"
 "target:ed/htext4"
 "target:ed/at-point"
 "target:ed/files"
 "target:ed/search1"
 "target:ed/search2"
 "target:ed/table"
 #+clx "target:ed/hunk-draw"
 "target:ed/window"
 "target:ed/screen"
 "target:ed/winimage"
 "target:ed/linimage"
 "target:ed/display"
 "target:ed/termcap"
 #+clx "target:ed/bit-display"
 "target:ed/tty-disp-rt"
 "target:ed/tty-display"
 "target:ed/pop-up-stream"
 #+clx "target:ed/bit-screen"
 "target:ed/tty-screen"
 "target:ed/cursor"
 "target:ed/font"
 "target:ed/streams"
 "target:ed/hacks"
 "target:ed/main"
 "target:ed/echo"
 "target:ed/echocoms"
 "target:ed/command"
 "target:ed/indent"
 "target:ed/comments"
 "target:ed/morecoms"
 "target:ed/undo"
 "target:ed/killcoms"
 "target:ed/parse"
 "target:ed/parse-scribe"
 "target:ed/searchcoms"
 "target:ed/filecoms"
 "target:ed/info"
 "target:ed/ginfo"
 "target:ed/doccoms"
 "target:ed/srccom"
 "target:ed/group"
 "target:ed/fill"
 "target:ed/text"
 "target:ed/highlight"
 "target:ed/lispmode"
 "target:ed/ts-buf"
 "target:ed/ts-stream"
 "target:ed/eval-server"
 "target:ed/lispbuf"
 "target:ed/lispeval"
 "target:ed/spell-rt"
 "target:ed/spell-corr"
 "target:ed/spell-aug"
 "target:ed/spellcoms"
 "target:ed/overwrite"
 "target:ed/abbrev"
 "target:ed/icom"
 "target:ed/kbdmac"
 "target:ed/defsyn"
 "target:ed/scribe"
 "target:ed/c"
 "target:ed/pascal"
 "target:ed/dylan"
 "target:ed/shell-script"
 "target:ed/python"
 "target:ed/make"
 "target:ed/m4"
 "target:ed/edit-defs"
 "target:ed/auto-save"
 "target:ed/register"
 #+clx "target:ed/xcoms"
 "target:ed/unixcoms"
 "target:ed/mh"
 "target:ed/compile"
 "target:ed/dired"
 "target:ed/diredcoms"
 "target:ed/bufed"
 "target:ed/lisp-lib"
 "target:ed/completion"
 "target:ed/shell"
 "target:ed/debug"
 "target:ed/netnews"
 "target:ed/rcs"
 "target:ed/vc"
 "target:ed/dabbrev"
 "target:ed/db"
 "target:ed/calendar"
 "target:ed/sort"
 "target:ed/csv"
 "target:ed/www"
 "target:ed/outline"
 "target:ed/buildcoms"
 "target:ed/break"
 "target:ed/bindings")
