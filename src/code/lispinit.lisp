;;; Initialization, plus some other random functions that could do with a
;;; better place.

(in-package :lisp)

(export '(most-positive-fixnum most-negative-fixnum sleep
	  ++ +++ ** *** // ///))

(in-package :system)
(export '(compiler-version scrub-control-stack))

(in-package :extensions)
(export '(quit *prompt* prompt-long))

(in-package :lisp)

;;; Make the error system enable interrupts.

(defconstant most-positive-fixnum #.vm:target-most-positive-fixnum
  "The fixnum closest in value to positive infinity.")

(defconstant most-negative-fixnum #.vm:target-most-negative-fixnum
  "The fixnum closest in value to negative infinity.")


;;;; Random information.

;; Set for kernel.core.  Overwritten for lisp.core in worldload.lisp.
(defvar *version* #.(read-line (open "target:VERSION")))

;; FIX Set for kernel.core.  Overwritten for lisp.core in worldload.lisp.
(defvar *build-time* 0)

;;; Must be initialized in %INITIAL-FUNCTION before the DEFVAR runs.
(declaim
  #-gengc
  (special *gc-inhibit* *already-maybe-gcing*
	   *need-to-collect-garbage* *gc-verbose*
	   *before-gc-hooks* *after-gc-hooks*
	   #+x86 *pseudo-atomic-atomic*
	   #+x86 *pseudo-atomic-interrupted*
	   unix::*interrupts-enabled*
	   unix::*interrupt-pending*
	   *type-system-initialized*)
  #+gengc
  (special *gc-verbose* *before-gc-hooks* *after-gc-hooks*
	   *type-system-initialized*))


;;;; Random magic specials.

;;; These are filled in by Genesis.

#-gengc
(progn

(defvar *current-catch-block*)
(defvar *current-unwind-protect-block*)
(defvar *free-interrupt-context-index*)

); #-gengc progn


;;;; Random stuff that needs to be in the cold load which would otherwise be
;;;; byte-compiled.
;;;;
;(defvar edi::*in-the-editor* nil)

;;;; Called by defmacro expanders.

;;; VERIFY-KEYWORDS -- internal
;;;
;;; Determine if key-list is a valid list of keyword/value pairs.  Do not
;;; signal the error directly, 'cause we don't know how it should be signaled.
;;;
(defun verify-keywords (key-list valid-keys allow-other-keys)
  (do ((already-processed nil)
       (unknown-keyword nil)
       (remaining key-list (cddr remaining)))
      ((null remaining)
       (if (and unknown-keyword
		(not allow-other-keys)
		(not (lookup-keyword :allow-other-keys key-list)))
	   (values :unknown-keyword (list unknown-keyword valid-keys))
	   (values nil nil)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
	   (return (values :dotted-list key-list)))
	  ((null (cdr remaining))
	   (return (values :odd-length key-list)))
	  ((member (car remaining) already-processed)
	   (return (values :duplicate (car remaining))))
	  ((or (eq (car remaining) :allow-other-keys)
	       (member (car remaining) valid-keys))
	   (push (car remaining) already-processed))
	  (t
	   (setf unknown-keyword (car remaining))))))

(defun lookup-keyword (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return (cadr remaining)))))
;;;
(defun keyword-supplied-p (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return t))))

(in-package "CONDITIONS")

(defvar *break-on-signals* nil
  "When (typep condition *break-on-signals*) is true, then calls to
   `signal' will enter the debugger prior to signalling that condition.")

(defun signal (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, nil is returned.  If
   (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked before
   any signalling is done."
  (let ((condition (coerce-to-condition datum arguments
					'simple-condition 'signal))
        (*handler-clusters* *handler-clusters*))
    (let ((obos *break-on-signals*)
	  (*break-on-signals* nil))
      (when (typep condition obos)
	(break "~A~%Break entered because of *break-on-signals* (now NIL.)"
	       condition)))
    (loop
      (or *handler-clusters* (return))
      (let ((cluster (pop *handler-clusters*)))
	(dolist (handler cluster)
	  (when (typep condition (car handler))
	    (funcall (cdr handler) condition)))))
    nil))

;;; COERCE-TO-CONDITION is used in SIGNAL, ERROR, CERROR, WARN, and
;;; INVOKE-DEBUGGER for parsing the hairy argument conventions into a single
;;; argument that's directly usable by all the other routines.
;;;
(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'condition)
	 (if arguments
	     (cerror "Ignore the additional arguments."
		     'simple-type-error
		     :datum arguments
		     :expected-type 'null
		     :format-control "You may not supply additional arguments ~
				     when giving ~S to ~S."
		     :format-arguments (list datum function-name)))
	 datum)
        ((symbolp datum) ; Roughly, (subtypep datum 'condition).
         (apply #'make-condition datum arguments))
        ((or (stringp datum) (functionp datum))
	 (make-condition default-type
                         :format-control datum
                         :format-arguments arguments))
        (t
         (error 'simple-type-error
		:datum datum
		:expected-type '(or symbol string)
		:format-control "Bad argument to ~S: ~S"
		:format-arguments (list function-name datum)))))

#[ Exceptions

{function:catch}
{function:throw}
{function:error}
]#

(defun error (datum &rest arguments)
  "Invokes the signal facility on a condition formed from datum and arguments.
   If the condition is not handled, the debugger is invoked."
  (kernel:infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-error 'error))
	  (debug:*stack-top-hint* debug:*stack-top-hint*))
      (or (and (condition-function-name condition) debug:*stack-top-hint*)
	  (multiple-value-bind
	      (name frame)
	      (kernel:find-caller-name)
	    (or (condition-function-name condition)
		(setf (condition-function-name condition) name))
	    (or debug:*stack-top-hint*
		(setf debug:*stack-top-hint* frame))))
      (let ((debug:*stack-top-hint* nil))
	(signal condition))
      (invoke-debugger condition))))

;;; CERROR must take care to not use arguments when datum is already a
;;; condition object.
;;;
(defun cerror (continue-string datum &rest arguments)
  (kernel:infinite-error-protect
    (with-simple-restart
	(continue "~A" (apply #'format nil continue-string arguments))
      (let ((condition (if (typep datum 'condition)
			   datum
			   (coerce-to-condition datum arguments
						'simple-error 'error)))
	    (debug:*stack-top-hint* debug:*stack-top-hint*))
	(unless (and (condition-function-name condition)
		     debug:*stack-top-hint*)
	  (multiple-value-bind
	      (name frame)
	      (kernel:find-caller-name)
	    (unless (condition-function-name condition)
	      (setf (condition-function-name condition) name))
	    (unless debug:*stack-top-hint*
	      (setf debug:*stack-top-hint* frame))))
	(with-condition-restarts condition (list (find-restart 'continue))
	  (let ((debug:*stack-top-hint* nil))
	    (signal condition))
	  (invoke-debugger condition)))))
  nil)

(defun break (&optional (datum "Break") &rest arguments)
  "Prints a message and invokes the debugger without allowing any possibility
   of condition handling occurring."
  (system:with-screen
   (kernel:infinite-error-protect
    (with-simple-restart (continue "Return from BREAK.")
      (let ((debug:*stack-top-hint*
	     (or debug:*stack-top-hint*
		 (nth-value 1 (kernel:find-caller-name)))))
	(invoke-debugger
	 (coerce-to-condition datum arguments 'simple-condition 'break))))))
  nil)

(defun warn (datum &rest arguments)
  "Warns about a situation by signalling a condition formed by datum and
   arguments.  While the condition is being signaled, a muffle-warning restart
   exists that causes WARN to immediately return nil."
  (kernel:infinite-error-protect
    (let ((condition (coerce-to-condition datum arguments
					  'simple-warning 'warn)))
      (check-type condition warning "a warning condition")
      (restart-case (signal condition)
	(muffle-warning ()
	  :report "Skip warning."
	  (return-from warn nil)))
      (format *error-output* "~&~@<Warning:  ~3i~:_~A~:>~%" condition)))
  nil)

(in-package "LISP")


;;;; Documentation.

(defvar *documentation* (make-string-table))

(defstruct (docnode (:constructor
		     make-doc-node (content &optional file position)))
  "A documentation node."
  content    ; doc text
  file       ; file in which node is defined
  position)  ; position in file

#[ Documentation

[ Introduction      ]     A brief overview.

[ Editor Tutorial   ]     Editor introduction, for new users.
[ Editor            ]     Using the editor.
[ Applications      ]     Editor extensions: Mail, News and more.

[ Read-Eval-Print   ]     The command line interface.
[ Lisp              ]     The programming language of this system.
[ Lisp Tutorial     ]     A quick tutorial.
[ Editor Extension  ]     How to extend the editor.
[ Scripting         ]     Writing standalone scripts.
[ System Usage      ]     Lisp libraries for the programmer.
[ Compiler          ]     The compiler; styles and techniques it encourages.
[ Debugger          ]     The run-time program inspector.
[ Profiler          ]

[ Installation      ]     Installing the system.
[ System Building   ]     Building and testing the system.

[ Internal Design   ]     Internal system details.
[ Directory Layout  ]     Layout of source and build directories.

[ Reference Tables  ]

[ Support           ]     Getting help from outside the system.
[ Feedback          ]     Giving feedback, reporting errors.

[ Authors           ]     Acknowledgments.
[ Future Plans      ]     TODO.
[ Copying           ]     The legal rights to this work.
]#

; single-language, n-dimensional

#[ Introduction

Nightshade is a dynamic programming environment.

The goal of the Nightshade project is an entirely public domain system
defined in a single language, which a single person can maintain fairly
easily.

The system is most probably entirely in the public domain already.  Most of
the [Authors] have been contacted to verify that they have given up
ownership rights to the system.

Included in the system is an Emacs-like editor, an interpreter, a byte
compiler, a native compiler, a run-time program inspector, a profiler and
higher-level libraries for tasks such as internet connection, mail handling
and document processing.  The primary human interface to the system is the
editor, which has extensions for file management, reading mail and reading
network news.
]#

#[ Scripting

Scripts can be created by adding an interpreter directive as the first
line in a file, as in

    #!/usr/bin/ni
    ;;
    ;; Print a greeting.

    (princ "Hello world.")
    (terpri)

The Nightshade binary must be installed to /usr/bin/ for this to work,
typically by linking to the distributed binary, as in

    (symlink-file "/usr/bin/ni" "/usr/local/bin/nightshade")

The script is evaluated by the Lisp interpreter, so the code in the script
must be valid Lisp.  In particular, hashes (#) are treated as reader macros
as usual, so comments should be made with ; or #|.

Below is an example of a simple CGI script.

    #!/usr/bin/ni
    #!
    #! Simple Nightshade CGI script.

    (let ((data (with-output-to-string (stream)
                  (format stream "<HTML><BODY>")
                  (format stream "cgi.lisp at ~A" (format-time))
                  (format stream "</BODY></HTML>~%"))))
      (format t "Content-Length: ~A~%" (length data))
      (format t "Connection: close~%")
      (format t "Content-Type: text/html~%~%")
      (write-string data))
]#

#[ Reference Tables

[ ASCII        ]
[ Numbers      ]     Decimal, hex, binary.
[ C Precedence ]     Operator precedence in the C programming language.

[ Mail Bindings Wallchart    ]   Editor mail reader key bindings.
[ Netnews Bindings Wallchart ]   Editor news reader key bindings.
]#

#[ ASCII

[FIX]
]#

#[ Number table

[FIX]
]#

#[ C Precedence

[FIX]
]#

#[ Support

Please email matt@mundell.ukfsn.org for any help.
]#

#[ Feedback

Reports of errors and general feedback are welcome and appreciated.  Please
email them to matt@mundell.ukfsn.org.
]#

#[ Future Plans

The file etc:TODO (src/etc/TODO in the distribution) roughly lists plans
and ideas for future work.  The editor command `Edit TODO' (which is bound
to "meta-g t") brings up the TODO file.
]#

#[ Copying

As of version 1b Nightshade is most probably entirely public domain.  It is
based on CMUCL 18c.

All parts of CMUCL 18c that were explicitly marked as copyrighted have been
removed.  This includes PCL, the CLX interface, the MIT version of the loop
macro and some of the contrib directory.

Everyone who committed to the CMUCL 18c repository after the CMU public
domain release has confirmed that the additions they made are public
domain.  They are Douglas Crosher (dtc), Robert MacLachlan (ram) and Paul
Werkowski (pw).

Contributions committed on behalf of the contributor after the CMU public
domain release were recorded in the CVS logs.  The status of these
additions is:

        - confirmed public domain by contributor
                  Marco Antoniotti, Julian Dolby, Peter Van Eynde, Fred Gilham,
                  Eric Marsden, Mike McDonald, Timothy Miller, Tim Moore, Ken Olum,
                  Raymond Toy.

        - reverted
                  Pierpaolo Bernardi, Casper Dik, Pierre Mai, Juergen Weiss.

        - pending contact with the author
                  Casper Dik, Marcus Krummenacker, Simon.

Every new addition to the CMUCL 18c base is public domain.  A few of these
new additions come from outside the Nightshade project; their sources are
detailed in the file src/etc/AUTHORS.
]#

#[ Internal Design

[ Directory Layout       ]  Layout of source and build directories.

[ Compiler Organisation  ]
[ Compiler Retargeting   ]
[ Run-Time System        ]
[ Virtual Machine        ]
[ Package Structure      ]  Packages relevant to the compiler
[ Compiler Glossary      ]

[ Writing System Tests   ]
]#

#[ Run-Time System
[ Type System          ]
[ Info Database        ]
[ Interpreter          ]
[ Debugger Information ]
[ Object Format        ]
[ Low-level            ]
[ Fasload File Format  ]
]#

#[ Low-level
[ Memory Management            ]
\section{Stacks and Globals}
\section{Heap Layout}
\section{Garbage Collection}
[ Interface to C and Assembler ]
[ Low-level debugging          ]
[ Core File Format             ]
]#

#[ Type System
[FIX]
]#

#[ Info Database
[FIX]
]#


#[ System Usage

[ Command Line Options                       ]

[ Default Interrupts for Lisp                ]

== Sort of part of the language ==

FIX ref to interfaces to compile, byte-compiler, interpreter

[ Packages                                   ]
[ Garbage Collection                         ]
[ The Reader                                 ]

== Libraries ==

[ Load                                       ]
[ Test Suite                                 ]  `deftest'
[ The Inspector                              ]
[ Saving a Core Image                        ]
[ Describe                                   ]
[ Running Programs from Lisp                 ]
[ Pathnames                                  ]
[ Filesystem Operations                      ]
[ Time Parsing and Formatting                ]
[ Unix Interface                             ]
[ Event Dispatching with SERVE-EVENT         ]
[ Aliens                                     ]
[ Interprocess Communication                 ]
[ Debugger Programmer Interface              ]
]#

#[ Interprocess Communication

Nightshade offers a facility for interprocess communication (IPC) on top of
using Unix system calls and the complications of that level of IPC.  There
is a simple remote-procedure-call (RPC) package build on top of TCP/IP
sockets.

[ The REMOTE Package ]
[ The WIRE Package   ]
[ Out-Of-Band Data   ]
]#


;;; %Initial-Function is called when a cold system starts up.  First we
;;; zoom down the *lisp-initialization-functions* doing things that wanted
;;; to happen at "load time."  Then we initialize the various subsystems
;;; and call the read-eval-print loop.  The top-level Read-Eval-Print loop
;;; (%top-level) is executed until someone (most likely the Quit function)
;;; throws to the tag %end-of-the-world.  We quit this way so that all
;;; outstanding cleanup forms in unwind-protects will get executed.

(proclaim '(special *lisp-initialization-functions*
		    *load-time-values*))

(eval-when (compile)
  (defmacro print-and-call (name)
    `(progn
       (%primitive print ,(symbol-name name))
       (,name))))

(defun hexstr (thing)
  (let ((addr (kernel:get-lisp-obj-address thing))
	(str (make-string 10)))
    (setf (char str 0) #\0
	  (char str 1) #\x)
    (dotimes (i 8)
      (let* ((nib (ldb (byte 4 0) addr))
	     (chr (char "0123456789abcdef" nib)))
	(declare (type (unsigned-byte 4) nib)
		 (base-char chr))
	(setf (char str (- 9 i)) chr
	      addr (ash addr -4))))
    str))

(defun %initial-function ()  ;; FIX change name to verb  initiate init -kernel
  "Spin the world."
  (%primitive print "In initial-function, and running.")
  #-gengc (setf *already-maybe-gcing* t)
  #-gengc (setf *gc-inhibit* t)
  #-gengc (setf *need-to-collect-garbage* nil)
  (setf *gc-verbose* #-gengc t #+gengc nil)
  (setf *before-gc-hooks* nil)
  (setf *after-gc-hooks* nil)
  #-gengc (setf unix::*interrupts-enabled* t)
  #-gengc (setf unix::*interrupt-pending* nil)
  (setf *type-system-initialized* nil)
  (setf *break-on-signals* nil)
  #+gengc (setf conditions::*handler-clusters* nil)

  ;; Many top-level forms call INFO, (SETF INFO).
  (print-and-call c::globaldb-init)

  ;; Set up the fdefn database.
  (print-and-call fdefn-init)

  ;; Some of the random top-level forms call Make-Array, which calls
  ;; Subtypep.
  (print-and-call typedef-init)
  (print-and-call class-init)
  (print-and-call type-init)

  (let ((funs (nreverse *lisp-initialization-functions*)))
    (%primitive print "Calling initialization functions...")
    (dolist (fun funs)
      #+() (%primitive print (hexstr fun))
      (typecase fun
	(function
	 (funcall fun))
	(cons
	 (case (car fun)
	   (:load-time-value
	    (setf (svref *load-time-values* (third fun))
		  (funcall (second fun))))
	   (:load-time-value-fixup
	    #-gengc
	    (setf (sap-ref-32 (second fun) 0)
		  (get-lisp-obj-address
		   (svref *load-time-values* (third fun))))
	    #+gengc
	    (do-load-time-value-fixup (second fun) (third fun) (fourth fun)))
	   #+(and x86 gencgc)
	   (:load-time-code-fixup
	    (vm::do-load-time-code-fixup (second fun) (third fun) (fourth fun)
					 (fifth fun)))
	   (t
	    (%primitive print
			"Bogus fixup in *lisp-initialization-functions*")
	    (%halt))))
	(t
	 (%primitive print
		     "Bogus function in *lisp-initialization-functions*")
	 (%halt))))
    (%primitive print "Done calling initialization functions."))
  (makunbound '*lisp-initialization-functions*)	; So it gets GC'ed.
  (makunbound '*load-time-values*)

  ;; Only do this after top level forms have run, 'cause thats where
  ;; deftypes are.
  (setf *type-system-initialized* t)

  (print-and-call os-init)
  (print-and-call filesys-init)

  (print-and-call reader-init)
  ;; Note: sharpm and backq not yet loaded, so this is not the final RT.
  (setf *readtable* (copy-readtable std-lisp-readtable))

  (print-and-call stream-init)
  (print-and-call loader-init)
  (print-and-call package-init)
  (print-and-call kernel::signal-init)
  (setf (alien:extern-alien "internal_errors_enabled" boolean) t)

  (set-floating-point-modes :traps '(:overflow #-x86 :underflow :invalid
					       :divide-by-zero))
  ;; This is necessary because some of the initial top level forms might
  ;; have changed the compilation policy in strange ways.
  (print-and-call c::proclaim-init)

  (print-and-call kernel::class-finalize)

  (%primitive print "Done initializing.")

  #-gengc (setf *already-maybe-gcing* nil)
  #+gengc (setf *gc-verbose* t)
  (defun add-documentation (string &optional file position)
    "Add $string as a node of documentation.  The first line of $string is
     the title of the node."
    (with-input-from-string (stream string)
      (let ((title (string-trim '(#\space #\tab) (read-line stream))))
	(setf (getstring title *documentation*)
	      (make-doc-node (subseq string
				     (file-position stream))
			     file
			     position))))
    t)
  ;;; Add any documentation accumulated so far.
  ;;;
  (do ((doc *pre-doc* (cdr doc)))
      ((null doc))
    (add-documentation (caar doc) (cadar doc) (caddar doc)))
  (terpri)
  (princ "Nightshade kernel core image ")
  (princ (version))
  (princ ".")
  (terpri)
  (princ "[Current package: ")
  (princ (package-%name *package*))
  (princ "]")
  (terpri)
  (unix:unix-exit (catch '%end-of-the-world (%top-level))))

#+gengc
(defun do-load-time-value-fixup (object offset index)
  (declare (type index offset))
  (macrolet ((lose (msg)
	       `(progn
		  (%primitive print ,msg)
		  (%halt))))
    (let ((value (svref *load-time-values* index)))
      (typecase object
	(list
	 (case offset
	   (0 (setf (car object) value))
	   (1 (setf (cdr object) value))
	   (t (lose "Bogus offset in cons cell."))))
	(instance
	 (setf (%instance-ref object (- offset vm:instance-slots-offset))
	       value))
	(code-component
	 (setf (code-header-ref object offset) value))
	(simple-vector
	 (setf (svref object (- offset vm:vector-data-offset)) value))
	(t
	 (lose "Unknown kind of object for load-time-value fixup."))))))


;;;; Initialization functions:

;;; Print seems to not like x86 NPX denormal floats like
;;; least-negative-single-float, so the :underflow exceptions is disabled
;;; by default.  The user can explicitly enable them if desired.

(defun reinit ()
  (system:block-interrupts
   (without-gcing
    (os-init)
    (stream-reinit)
    (kernel::signal-init)
    (gc-init)
    (setf (alien:extern-alien "internal_errors_enabled" boolean) t)
    (set-floating-point-modes :traps
			      '(:overflow #-x86 :underflow :invalid
					  :divide-by-zero))
    ;; Clear pseudo atomic in case this core wasn't compiled with support.
    #+x86 (setf lisp::*pseudo-atomic-atomic* 0))))


;;;; Miscellaneous external functions:

;;; Quit gets us out, one way or another.

(defun quit (&optional recklessly-p)
  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
   true."
  (if recklessly-p
      (unix:unix-exit 0)
      (throw '%end-of-the-world 0)))

(defconstant repl-help-string
  "
This is the read-eval-print loop.  It reads a Lisp expression, evaluates
the expression, prints the result, then repeats.  Any Lisp expression can
be entered at the prompt, for example

    (use-package \"SHELL\")

makes the :shell package available in the current packages.

To exit, enter (quit).  To start the editor, enter (ed).

FIX Special symbols  * ** *** + ++ +++ -

The :shell package provides many Lisp equivalents of Unix style commands
for working in the read-eval-print loop.  These include

        (pwd)             print the working (current) directory
        (cd)              change to home:
        (cd \"/dir/\")      change to directory dir
        (ls)              list the current directory briefly
        (ls -l)           list the current directory verbosely
        (rm \"file\")       remove file
        (mv \"f\" \"g\")      move f to g
        (touch \"file\")    touch file.

The text printed as the prompt is controlled by the variable ext:*prompt*,
which can be set to a string or a function, for example

  (setq ext:*prompt* #'ext::prompt-long)

produces a prompt with the current package, user, host and directory.

FIX There's more information in the [Read-Eval-Print] documentation, which
is accessible inside the editor.
")

(defun help (&optional (stream *standard-output*))
  "Print a help message about the read-eval-print loop to $stream."
  (write-line repl-help-string stream)
  t)

#-mp ; Multi-processing version defined in multi-proc.lisp.
(defun sleep (n)
  "This function causes execution to be suspended for N seconds.  N may be
   any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
            Must be a non-negative, non-complex number."
	   n))
  (multiple-value-bind (sec usec)
    (if (integerp n)
	(values n 0)
	(multiple-value-bind (sec frac) (truncate n)
	  (values sec(truncate frac 1e-6))))
    (unix:unix-select 0 0 0 0 sec usec))
  nil)


;;;; SCRUB-CONTROL-STACK

(defconstant bytes-per-scrub-unit 2048)

;;; Scrub-control-stack.
;;;
#-x86
(defun scrub-control-stack ()
  "Zero the unused portion of the control stack so that old objects are not
   kept alive because of uninitialized stack variables."
  (declare (optimize (speed 3) (safety 0))
	   (values (unsigned-byte 20)))
  (labels
      ((scrub (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		(look (sap+ ptr bytes-per-scrub-unit) 0 count))
	       (t
		(setf (sap-ref-32 ptr offset) 0)
		(scrub ptr (+ offset vm:word-bytes) count))))
       (look (ptr offset count)
	 (declare (type system-area-pointer ptr)
		  (type (unsigned-byte 16) offset)
		  (type (unsigned-byte 20) count)
		  (values (unsigned-byte 20)))
	 (cond ((= offset bytes-per-scrub-unit)
		count)
	       ((zerop (sap-ref-32 ptr offset))
		(look ptr (+ offset vm:word-bytes) count))
	       (t
		(scrub ptr offset (+ count vm:word-bytes))))))
    (let* ((csp (sap-int (c::control-stack-pointer-sap)))
	   (initial-offset (logand csp (1- bytes-per-scrub-unit))))
      (declare (type (unsigned-byte 32) csp))
      (scrub (int-sap (- csp initial-offset))
	     (* (floor initial-offset vm:word-bytes) vm:word-bytes)
	     0))))

;;; Scrub-control-stack.
;;;
;;; On the x86 port the stack grows downwards, and to support grow on
;;; demand stacks the stack must be decreased as it is scrubbed.
;;;
#+x86
(defun scrub-control-stack ()
  "Zero the unused portion of the control stack so that old objects are not
   kept alive because of uninitialized stack variables."
  (scrub-control-stack))


#[ Read-Eval-Print

{function:quit}
{function:help}

shell package

]#

;;;; TOP-LEVEL loop.

(defvar / nil
  "Holds a list of all the values returned by the most recent top-level EVAL.")
(defvar // nil "Gets the previous value of / when a new value is computed.")
(defvar /// nil "Gets the previous value of // when a new value is computed.")
(defvar * nil "Holds the value of the most recent top-level EVAL.")
(defvar ** nil "Gets the previous value of * when a new value is computed.")
(defvar *** nil "Gets the previous value of ** when a new value is computed.")
(defvar + nil "Holds the value of the most recent top-level READ.")
(defvar ++ nil "Gets the previous value of + when a new value is read.")
(defvar +++ nil "Gets the previous value of ++ when a new value is read.")
(defvar - nil "Holds the form curently being evaluated.")
(defvar *prompt* "* "
  "The top-level prompt string.  This also may be a function of no arguments
   that returns a simple-string.")
(defvar *in-top-level-catcher* nil
  "True if we are within the Top-Level-Catcher.  This is used by interrupt
  handlers to see whether it is o.k. to throw.")

(defun interactive-eval (form)
  "Evaluate FORM, returning whatever it returns but adjust ***, **, *, +++, ++,
  +, ///, //, /, and -."
  (setf - form)
  (let ((results (multiple-value-list (eval form))))
    (finish-standard-output-streams)
    (setf /// //
	  // /
	  / results
	  *** **
	  ** *
	  * (car results)))
  (setf +++ ++
	++ +
	+ -)
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    (setf * nil)
    (cerror "Go on with * set to NIL."
	    "EVAL returned an unbound marker."))
  (values-list /))

(defconstant eofs-before-quit 10)

(defun %top-level ()
  "Top-level read-eval-print loop."
  (let  ((* nil) (** nil) (*** nil)
	 (- nil) (+ nil) (++ nil) (+++ nil)
	 (/// nil) (// nil) (/ nil)
	 (magic-eof-cookie (cons :eof nil))
	 (number-of-eofs 0))
    (loop
      (with-simple-restart (abort "Return to Top-Level.")
	(catch 'top-level-catcher
	  (unix:unix-sigsetmask 0)
	  (let ((*in-top-level-catcher* t))
	    (loop
	      (scrub-control-stack)
	      (or *batch-mode*
		  (progn
		    (fresh-line)
		    (princ (if (functionp *prompt*)
			       (funcall *prompt*)
			       *prompt*))
		    (force-output)))
	      (let ((form (read *standard-input* nil magic-eof-cookie)))
		(cond ((not (eq form magic-eof-cookie))
		       (let ((results
			      (multiple-value-list (interactive-eval form))))
			 (or *batch-mode*
			     (dolist (result results)
			       (fresh-line)
			       (prin1 result))))
		       (setf number-of-eofs 0))
		      ((< number-of-eofs 1)
		       (if *batch-mode*
			   (quit)
			   (let ((stream (make-synonym-stream '*terminal-io*)))
			     (setf *standard-input* stream)
			     (setf *standard-output* stream)
			     (format t "~&Received EOF on *standard-input*, ~
					switching to *terminal-io*.~%")))
		       (incf number-of-eofs))
		      ((> number-of-eofs eofs-before-quit)
		       (format t "~&Received more than ~D EOFs; Aborting.~%"
			       eofs-before-quit)
		       (quit))
		      (t
		       (format t "~&Received EOF.~%")))))))))))


;;; %Halt  --  Interface
;;;
;;; A convenient way to get into the assembly level debugger.
;;;
(defun %halt ()
  (with-screen
   (%primitive halt)))


#[ Authors

Many people contributed to the CMU Common Lisp (CMUCL) on which Nightshade
is based.  After the CMU public domain release of CMUCL many people also
worked on that same CMUCL.

The sections below list all known authors of all the Nightshade code and
documentation.  Thanks to them all.

The file etc:AUTHORS (src/etc/AUTHORS) provides more details of the
contributions.

== Authors noted in the original CMUCL source and documentation ==

David Adam, Dan Aronson, David Axmark, Miles Bader, Joseph Bates, Blaine
Burks, Rick Busdiecker, Bill Chiles, David Dill, Casper Dik, Carl Ebeling,
Scott E. Fahlman, Neal Feinberg, Charles L. Forgy, Mike Garland, Joseph
Ginder, Paul Gleichauf, Dario Guise, Sean Hallgren, Steven Handerson,
Richard Harris, Jim Healy, Joerg-Cyril Hoehl, Christopher Hoover, Todd
Kaufmann, John Kolojejchick, Jim Kowalski, Dan Kuokka, Jim Large, Simon
Leinen, Sandra Loosemore, William Lott, Robert A.  MacLachlan, Bill Maddox,
David B.  McDonald, Tim Moore, Jim Muller, Lee Schumacher, Guy L.  Steele
Jr., Dave Touretzky, Walter van Roggen, Ivan Vazquez, Skef Wholey, George
Wood, Jamie W.  Zawinski and Dan Zigmond.

== Post CMU Authors ==

Marco Antoniotti, Mike Clarkson, Douglas T. Crosher, Julian Dolby, Fred
Gilham, Richard Harris, Marcus Krummenacker, Akira Kurihara, Eric Marsden,
Makoto Matsumoto, Mike McDonald, Timothy Miller, T.  Nishimura, Ken Olum,
Alexander Petrov, Tom Russ, Sam Steingold, Raymond Toy, Peter Van Eynde,
Paul F.  Werkowski and Simon XXX.

== Authors of integrated public domain code ==

Stewart M. Clamen, Nachum Dershowitz, Luke Gorrie, Juri Pakaste, Edward M.
Reingold and Thomas Russ.

== Post CMUCL Authors ==

Matthew Mundell
]#


#[ FIX

A reference to this node marks documentation that needs work.
]#
