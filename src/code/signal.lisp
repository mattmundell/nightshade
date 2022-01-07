;;; Code for handling Unix signals.

(in-package "UNIX")

(use-package "KERNEL")
(export '(unix-signal-name unix-signal-description unix-signal-number
	  sigmask unix-sigblock unix-sigpause unix-sigsetmask unix-kill
	  unix-killpg))

(in-package "KERNEL")
(export '(signal-init))

(in-package "SYSTEM")
(export '(block-interrupts with-interrupts with-enabled-interrupts
	  enable-interrupt ignore-interrupt default-interrupt))

(in-package "UNIX")

;;; FIX These should be where?
;;;
(defconstant sig_dfl 0)
(defconstant sig_ign 1)

(proclaim '(special lisp::lisp-command-line-list))


#[ Unix Interrupts

Nightshade provides access to all the Unix signals that can be generated
under Unix.  It should be noted that with certain use of this capability
can be fatal to the running Lisp.  The following macros and functions allow
access to the Unix interrupt system.  The signal names as specified in
section 2 of the "Unix Programmer's Manual" are exported from the Unix
package.

[ Changing Interrupt Handlers ]
[ Examples of Signal Handlers ]
]#


;;;; Utilities for dealing with signal names and numbers.

(defstruct (unix-signal
	    (:constructor make-unix-signal (%name %number %description)))
  %name                             ; Signal keyword
  (%number nil :type integer)       ; Unix signal number
  (%description nil :type string))  ; Documentation

(defvar *unix-signals* nil
  "A list of unix signal structures.")

(eval-when (compile eval)
;(setf *unix-signals* nil) ; pve: sigh or else...   FIX
(defmacro def-unix-signal (name number description)
  (let ((symbol (intern (symbol-name name))))
    `(progn
       (push (make-unix-signal ,name ,number ,description) *unix-signals*)
       ;;
       ;; This is to make the new signal lookup stuff compatible with old
       ;; code which expects the symbol with the same print name as our
       ;; keywords to be a constant with a value equal to the signal
       ;; number.
       (defconstant ,symbol ,number ,description)
       (export ',symbol))))
) ;eval-when

(defun unix-signal-or-lose (arg)
  (let ((signal (find arg *unix-signals*
		      :key (etypecase arg
			     (symbol #'unix-signal-%name)
			     (number #'unix-signal-%number)))))
    (or signal
	(error "~S is not a valid signal name or number." arg))
    signal))

(defun unix-signal-name (signal)
  "Return the name of the signal as a string.  Signal should be a valid
   signal number or a keyword of the standard Unix signal name."
  (symbol-name (unix-signal-%name (unix-signal-or-lose signal))))

(defun unix-signal-description (signal)
  "Return a string describing signal.  Signal should be a valid signal
   number or a keyword of the standard Unix signal name."
  (unix-signal-%description (unix-signal-or-lose signal)))

(defun unix-signal-number (signal)
  "Return the number of the given signal.  Signal should be a valid signal
   number or a keyword of the standard Unix signal name."
  (unix-signal-%number (unix-signal-or-lose signal)))

#[ Default Interrupts for Lisp

Several interrupt handlers are present on startup:

  % SIGINT (^c)

    causes Lisp to enter a break loop.  This puts you into the debugger
    which allows you to look at the current state of the computation.  If
    you proceed from the break loop, the computation will proceed from
    where it was interrupted.

  % SIGQUIT (^\)

    causes Lisp to do a throw to the top-level.  This causes the current
    computation to be aborted, and control returned to the top-level
    read-eval-print loop.

  % SIGTSTP (^z)

    causes Lisp to suspend execution and return to the Unix shell.  If
    control is returned to Lisp, the computation will proceed from where
    it was interrupted.

  % SIGILL, SIGBUS, SIGSEGV and SIGFPE

    cause Lisp to signal an error.

For keyboard interrupt signals, the standard interrupt character is in
parentheses.  Local configuration may set up other interrupt characters.
There may be a pause before a signal is processed due to Lisp having to
reach a point where it can be interrupted safely.  The computation will
continue until a safe point is reached and then the interrupt will be
processed.  Signal handler definition is described in [Changing Interrupt
Handlers].
]#

;;; Known signals
;;;
(def-unix-signal :CHECK      0 "Check")

(def-unix-signal :SIGHUP     1 "Hangup")
(def-unix-signal :SIGINT     2 "Interrupt")
(def-unix-signal :SIGQUIT    3 "Quit")
(def-unix-signal :SIGILL     4 "Illegal instruction")
(def-unix-signal :SIGTRAP    5 "Trace trap")
(def-unix-signal :SIGIOT     6 "Iot instruction") ; Compatibility
(def-unix-signal :SIGABRT    6 "C abort()")
#-linux
(def-unix-signal :SIGEMT     7 "Emt instruction")
(def-unix-signal :SIGFPE     8 "Floating point exception")
(def-unix-signal :SIGKILL    9 "Kill")
(def-unix-signal :SIGBUS #-linux 10 #+linux 7 "Bus error")
(def-unix-signal :SIGSEGV   11 "Segmentation violation")
#-linux
(def-unix-signal :SIGSYS    12 "Bad argument to system call")
(def-unix-signal :SIGPIPE   13 "Write on a pipe with no one to read it")
(def-unix-signal :SIGALRM   14 "Alarm clock")
(def-unix-signal :SIGTERM   15 "Software termination signal")
#+linux
(def-unix-signal :SIGSTKFLT 16 "Stack fault on coprocessor")
(def-unix-signal :SIGURG    #+svr4 21 #-(or hpux svr4 linux) 16 #+hpux 29
  #+linux 23 "Urgent condition present on socket")
(def-unix-signal :SIGSTOP   #-(or hpux svr4 linux) 17 #+hpux 24 #+svr4 23
  #+linux 19 "Stop")
(def-unix-signal :SIGTSTP   #-(or hpux svr4 linux) 18 #+hpux 25 #+svr4 24
  #+linux 20 "Stop signal generated from keyboard")
(def-unix-signal :SIGCONT   #-(or hpux svr4 linux) 19 #+hpux 26 #+svr4 25
  #+linux 18 "Continue after stop")
(def-unix-signal :SIGCHLD
		 #-(or linux hpux svr4) 20
                 #+(or hpux svr4) 18
                 #+linux 17
  "Child status has changed")
(def-unix-signal :SIGTTIN   #-(or hpux svr4) 21 #+hpux 27 #+svr4 26
  "Background read attempted from control terminal")
(def-unix-signal :SIGTTOU   #-(or hpux svr4) 22 #+hpux 28 #+svr4 27
  "Background write attempted to control terminal")
(def-unix-signal :SIGIO     #-(or svr4 hpux irix linux) 23 #+(or svr4 hpux irix) 22
  #+linux 29
  "I/O is possible on a descriptor")
#-hpux
(def-unix-signal :SIGXCPU   #-svr4 24 #+svr4 30  "Cpu time limit exceeded")
#-hpux
(def-unix-signal :SIGXFSZ   #-svr4 25 #+svr4 31 "File size limit exceeded")
(def-unix-signal :SIGVTALRM #-(or hpux svr4) 26 #+hpux 20 #+svr4 28
    "Virtual time alarm")
(def-unix-signal :SIGPROF   #-(or hpux svr4 linux) 27 #+hpux 21 #+svr4 29
  #+linux 30 "Profiling timer alarm")
(def-unix-signal :SIGWINCH  #-(or hpux svr4) 28 #+hpux 23 #+svr4 20
    "Window size change")
(def-unix-signal :SIGUSR1   #-(or hpux svr4 linux) 30 #+(or hpux svr4) 16
  #+linux 10 "User defined signal 1")
(def-unix-signal :SIGUSR2   #-(or hpux svr4 linux) 31 #+(or hpux svr4) 17
  #+linux 12 "User defined signal 2")
;;;
;;; These are Mach Specific
#+mach
(def-unix-signal :SIGEMSG   30 "Mach Emergency message")
#+mach
(def-unix-signal :SIGMSG    31 "Mach message")

;;; SVR4 (or Solaris?) specific signals
#+svr4
(def-unix-signal :SIGWAITING 32 "Process's lwps are blocked")

;;; SIGMASK -- Public
;;;
(defmacro sigmask (&rest signals)
  "Returns a mask given a set of signals."
  (apply #'logior
	 (mapcar #'(lambda (signal)
		     (ash 1 (1- (unix-signal-number signal))))
		 signals)))


;;;; System calls that deal with signals.

(proclaim '(inline real-unix-kill))

(alien:def-alien-routine ("kill" real-unix-kill) c-call:int
  (pid c-call:int)
  (signal c-call:int))

(defun unix-kill (pid signal)
  "Unix-kill sends the signal signal to the process with process id pid.
   Signal should be a valid signal number or a keyword of the standard Unix
   signal name."
  (real-unix-kill pid (unix-signal-number signal)))

(proclaim '(inline real-unix-killpg))

(alien:def-alien-routine ("killpg" real-unix-killpg) c-call:int
  (pgrp c-call:int)
  (signal c-call:int))

(defun unix-killpg (pgrp signal)
  "Unix-killpg sends the signal signal to the all the process in process
   group PGRP.  Signal should be a valid signal number or a keyword of the
   standard Unix signal name."
  (real-unix-killpg pgrp (unix-signal-number signal)))

(alien:def-alien-routine ("sigblock" unix-sigblock) c-call:unsigned-long
  "Unix-sigblock cause the signals specified in mask to be added to the set
   of signals currently being blocked from delivery.  The macro sigmask is
   provided to create masks."
  (mask c-call:unsigned-long))

(alien:def-alien-routine ("sigpause" unix-sigpause) c-call:void
  "Unix-sigpause sets the set of masked signals to its argument and then
   waits for a signal to arrive, restoring the previous mask upon its
   return."
  (mask c-call:unsigned-long))

(alien:def-alien-routine ("sigsetmask" unix-sigsetmask) c-call:unsigned-long
  "Unix-sigsetmask sets the current set of masked signals (those being
   blocked from delivery) to the argument.  The macro sigmask can be used
   to create the mask.  The previous value of the signal mask is returned."
  (mask c-call:unsigned-long))


;;;; C routines that actually do all the work of establishing signal handlers.

(alien:def-alien-routine ("install_handler" install-handler)
			 c-call:unsigned-long
  (signal c-call:int)
  (handler c-call:unsigned-long))


#[ Changing Interrupt Handlers

{function:system:with-enabled-interrupts}
{function:system:block-interrupts}
{function:system:with-interrupts}
{function:system:with-screen}
{function:system:enable-interrupt}
{function:system:ignore-interrupt}
{function:system:default-interrupt}
]#


;;;; Interface to enabling and disabling signal handlers.

(defun enable-interrupt (signal handler)
  "Establish $handler as the handler for $signal, returning the old
   function associated with $signal.

   Install a permanent handler.  `with-enabled-interrupts' establishes a
   handler temporarily."
;; FIX from manual: \hide{The optional \var{character} can be specified for
;; the SIGINT, SIGQUIT, and SIGTSTP signals and causes that character to
;; generate the appropriate signal from the keyboard.} \hide{Without
;; \var{character},} \code{enable-interrupt} returns the old function
;; associated with the signal.  \hide{When \var{character} is specified for
;; SIGINT, SIGQUIT, or SIGTSTP, it returns the old character code.}
  (declare (type (or function (member :default :ignore)) handler))
  (without-gcing
   (let ((result (install-handler (unix-signal-number signal)
				  (case handler
				    (:default sig_dfl)
				    (:ignore sig_ign)
				    (t
				     (kernel:get-lisp-obj-address handler))))))
     (cond ((= result sig_dfl) :default)
	   ((= result sig_ign) :ignore)
	   (t (the function (kernel:make-lisp-obj result)))))))

;; FIX fallback-interrupt?
(defun default-interrupt (signal)
  "Tell the Unix signal mechanism to perform the default action for
   $signal.  For details on what the default action for a signal is, see
   the signal(7) in the Unix manuals.  In general, it is likely to ignore
   the signal or to cause a core dump."
  (enable-interrupt signal :default))

(defun ignore-interrupt (signal)
  "Set the Unix signal mechanism to ignore $signal which means that the
   Lisp process will never see the signal.  Return the old function
   associated with the signal if there is one, else ()."
  (enable-interrupt signal :ignore))


;;;; Default LISP signal handlers.

;;; Most of these just call ERROR to report the presence of the signal.

(defmacro define-signal-handler (name what
				      &optional (function 'error)
				                (get-screen t))
  (let ((call `(,function ,(concatenate 'simple-string what " at #x~x.")
			  (with-alien ((scp (* sigcontext) scp))
			    (sap-int (vm:sigcontext-program-counter scp))))))
    `(defun ,name (signal code scp)
       (declare (ignore signal code)
		(type system-area-pointer scp)
		(optimize (inhibit-warnings 3)))
       ,(if get-screen
	    (cons 'system:with-screen call)
	    call))))

;; FIX sigtstp? sigstop?  issues with c-m-z exit recursive edit (switch to c-c c-c?)

;; FIX handle-sig*
(define-signal-handler sigint-handler "Interrupted" break nil)
(define-signal-handler sigill-handler "Illegal Instruction")
(define-signal-handler sigtrap-handler "Breakpoint/Trap")
(define-signal-handler sigabrt-handler "SIGABRT")
#-linux
(define-signal-handler sigemt-handler "SIGEMT")
(define-signal-handler sigbus-handler "Bus Error")
(define-signal-handler sigsegv-handler "Segmentation Violation")
#-linux
(define-signal-handler sigsys-handler "Bad Argument to a System Call")
(define-signal-handler sigpipe-handler "SIGPIPE")
(define-signal-handler sigalrm-handler "SIGALRM")

(defun sigquit-handler (signal code scp)
  (declare (ignore signal code scp))
  (throw 'lisp::top-level-catcher nil))

;; FIX init-signal
(defun signal-init ()
  "Enable all the default signals that Lisp knows how to deal with."
  (or (member "-monitor" lisp::lisp-command-line-list :test #'string=)
      (enable-interrupt :sigint #'sigint-handler))
  (enable-interrupt :sigquit #'sigquit-handler)
  (enable-interrupt :sigill #'sigill-handler)
  (enable-interrupt :sigtrap #'sigtrap-handler)
  (enable-interrupt :sigabrt #'sigabrt-handler)
  #-linux
  (enable-interrupt :sigemt #'sigemt-handler)
  (enable-interrupt :sigfpe #'vm:sigfpe-handler)
  (enable-interrupt :sigbus #'sigbus-handler)
  (enable-interrupt :sigsegv #'sigsegv-handler)
  #-linux
  (enable-interrupt :sigsys #'sigsys-handler)
  (enable-interrupt :sigpipe #'sigpipe-handler)
  (enable-interrupt :sigalrm #'sigalrm-handler)
  ())


;;;; Macros for dynamically enabling and disabling signal handling.

;;; Notes on how the block-interrupts/with-interrupts stuff works.
;;;
;;; Before invoking the supplied handler for any of the signals that can be
;;; blocked, the C interrupt support code checks to see if
;;; *interrupts-enabled* has been bound to NIL.  If so, it saves the signal
;;; number and the value of the signal mask (from the sigcontext), sets the
;;; signal mask to block all blockable signals, sets *interrupt-pending*
;;; and returns without handling the signal.
;;;
;;; When we drop out the without interrupts, we check to see if
;;; *interrupt-pending* has been set.  If so, we call do-pending-interrupt,
;;; which generates a SIGTRAP.  The C code invokes the handler for the
;;; saved signal instead of the SIGTRAP after replacing the signal mask in
;;; the sigcontext with the saved value.  When that handler returns, the
;;; original signal mask is installed, allowing any other pending signals
;;; to be handled.
;;;
;;; This means that the cost of block-interrupts is just a special
;;; binding in the case when no signals are delivered (the normal case).
;;; It's only when a signal is actually delivered that we use any system
;;; calls, and by then the cost of the extra system calls are lost in the
;;; noise when compared with the cost of delivering the signal in the first
;;; place.

;;; DO-PENDING-INTERRUPT  --  internal
;;;
;;; Magically converted by the compiler into a break instruction.
;;;
(defun do-pending-interrupt ()
  (do-pending-interrupt))

#-gengc (progn

(defvar *interrupts-enabled* t)
(defvar *interrupt-pending* nil)

;; FIX stall-interrupts?
;;; BLOCK-INTERRUPTS  --  public
;;;
(defmacro block-interrupts (&body body)
  "Block any interrupts during $body.  Process the interrupts afterwards."
  (let ((name (gensym)))
    `(flet ((,name () ,@body))
       (if *interrupts-enabled*
	   (unwind-protect
	       (let ((*interrupts-enabled* nil))
		 (,name))
	     (when *interrupt-pending*
	       (do-pending-interrupt)))
	   (,name)))))

;;; WITH-INTERRUPTS  --  public
;;;
(defmacro with-interrupts (&body body)
  "Allow interrupts while executing $body.  As interrupts are normally
   allowed, this is only useful inside a $block-interrupts.  This is
   useful, for example, if $body is going to enter a break loop or do some
   long computation that might need to be interrupted."
  (let ((name (gensym)))
    `(flet ((,name () ,@body))
       (if *interrupts-enabled*
	   (,name)
	   (let ((*interrupts-enabled* t))
	     (when *interrupt-pending*
	       (do-pending-interrupt))
	     (,name))))))

) ; #-gengc progn

;;; On the GENGC system, we have to do it slightly differently because of
;;; the existance of threads.  Each thread has a suspends_disabled_count in
;;; its mutator structure.  When this value is other then zero, the low
;;; level stuff will not suspend the thread, but will instead set the
;;; suspend_pending flag (also in the mutator).  So when we finish the
;;; block-interrupts, we just check the suspend_pending flag and trigger
;;; a do-pending-interrupt if necessary.

#+gengc
(defmacro block-interrupts (&body body)
  `(unwind-protect
       (progn
	 (locally
	   (declare (optimize (speed 3) (safety 0)))
	   (incf (kernel:mutator-interrupts-disabled-count)))
	 ,@body)
     (locally
       (declare (optimize (speed 3) (safety 0)))
       (when (and (zerop (decf (kernel:mutator-interrupts-disabled-count)))
		  (not (zerop (kernel:mutator-interrupt-pending))))
	 (do-pending-interrupt)))))


;;;; WITH-ENABLED-INTERRUPTS

(defmacro with-enabled-interrupts (interrupt-list &body body)
  "with-enabled-interrupts ({(interrupt function)}*) {form}*

   Establish $function as a handler for the Unix signal $interrupt which
   should be a number between 1 and 31 inclusive.  A signal handler
   function specified as () will set the Unix signal handler to the
   fallback which is normally either to ignore the signal or to cause a
   core dump, depending on the particular signal."
;; FIX "the third should be an optional character used to generate the
;; signal from the keyboard.  This last item is only useful for the SIGINT,
;; SIGQUIT, and SIGTSTP signals." from old doc
  (let ((il (gensym))
	(it (gensym)))
    `(let ((,il NIL))
       (unwind-protect
	   (progn
	     ,@(do* ((item interrupt-list (cdr item))
		     (intr (caar item) (caar item))
		     (ifcn (cadar item) (cadar item))
		     (forms NIL))
		    ((null item) (nreverse forms))
		 (when (symbolp intr)
		   (setq intr (symbol-value intr)))
		 (push `(push `(,,intr ,(enable-interrupt ,intr ,ifcn)) ,il)
		       forms))
	     ,@body)
	 (dolist (,it (nreverse ,il))
	   (enable-interrupt (car ,it) (cadr ,it)))))))


#[ Examples of Signal Handlers

The following code is the signal handler used by the Lisp system for the
SIGINT signal.

    (defun handle-sigint (signal code scp)
      (declare (ignore signal code)
               (type system-area-pointer scp)
               (optimize (inhibit-warnings 3)))
      (break "interrupted at #x~x."
             (with-alien ((scp (* sigcontext) scp))
                         (sap-int (x86:sigcontext-program-counter scp)))))

`break' is called to enter a break loop, enable inspection of the current
state of the computation.  On exiting the break loop, the computation will
restart from where it was interrupted.

The following function is the Lisp signal handler for the SIGTSTP signal
which suspends a process and returns to the Unix shell.

    (defun handle-sigtstp (signal code scp)
      (declare (ignore signal code scp))
      (system:with-screen
       (unix:unix-kill (unix:unix-getpid) unix:sigstop)))

The editor relinquishes control of the screen for the body of
`with-screen'.  Lisp uses this interrupt handler to catch the SIGTSTP
signal because it is necessary to get out of the editor in a clean way
before returning to the shell.

To set up such interrupt handlers, the following is recommended:

   (with-enabled-interrupts ((Unix:SIGINT #'handle-sigint)
                             (Unix:SIGTSTP #'handle-sigtstp))
     <code to execute with the handlers enabled>)
]#
