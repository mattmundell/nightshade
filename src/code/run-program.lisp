;;; RUN-PROGRAM and friends.  Facility for running unix programs inside a
;;; lisp.

(in-package "EXTENSIONS")

(export '(run-program process-status process-exit-code process-core-dumped
	  process-wait process-kill process-input process-output process-plist
	  process-pty process-error process-status-hook process-alive-p
	  process-close process-pid process-p))


#[ Running Programs from Lisp

It is possible to run external programs from Lisp using the following
function.  [Process Accessors] provide access to the returned process
structure.

{function:run-program}
]#

#[ Process Accessors

Interfaces to the process returned by `run-program':

{function:ext:process-p}
{function:ext:process-pid}
{function:ext:process-status}
{function:ext:process-exit-code}
{function:ext:process-core-dumped}
{function:ext:process-pty}
{function:ext:process-input}
{function:ext:process-output}
{function:ext:process-error}

If the corresponding stream was created, these functions return the input,
output or error file descriptor, otherwise they return ().

{function:ext:process-status-hook}
{function:ext:process-plist}
{function:ext:process-wait}
{function:ext:process-kill}
{function:ext:process-alive-p}
{function:ext:process-close}
]#


;;;; Import WAIT3 from unix.

(alien:def-alien-routine ("wait3" c-wait3) c-call:int
  (status c-call:int :out)
  (options c-call:int)
  (rusage c-call:int))

(eval-when (load eval compile)
  (defconstant wait-wnohang #-svr4 1 #+svr4 #o100)
  (defconstant wait-wuntraced #-svr4 2 #+svr4 4)
  (defconstant wait-wstopped #-svr4 #o177 #+svr4 wait-wuntraced))

(defun wait3 (&optional do-not-hang check-for-stopped)
  "Return any available status information on child processes."
  (multiple-value-bind (pid status)
		       (c-wait3 (logior (if do-not-hang
					  wait-wnohang
					  0)
					(if check-for-stopped
					  wait-wuntraced
					  0))
				0)
    (cond ((or (minusp pid)
	       (zerop pid))
	   nil)
	  ((eql (ldb (byte 8 0) status)
		wait-wstopped)
	   (values pid
		   :stopped
		   (ldb (byte 8 8) status)))
	  ((zerop (ldb (byte 7 0) status))
	   (values pid
		   :exited
		   (ldb (byte 8 8) status)))
	  (t
	   (let ((signal (ldb (byte 7 0) status)))
	     (values pid
		     (if (or (eql signal unix:sigstop)
			     (eql signal unix:sigtstp)
			     (eql signal unix:sigttin)
			     (eql signal unix:sigttou))
		       :stopped
		       :signaled)
		     signal
		     (not (zerop (ldb (byte 1 7) status)))))))))


;;;; Process control stuff.

(defvar *active-processes* nil
  "List of process structures for all active processes.")

(defstruct (process (:print-function %print-process)
		    (:predicate process-p))
  pid			    ; PID of child process.
  %status		    ; Either :RUNNING, :STOPPED, :EXITED, or :SIGNALED.
  exit-code		    ; Either exit code or signal
  core-dumped		    ; T if a core image was dumped.
  pty			    ; Stream to child's pty or nil.
  input			    ; Stream to child's input or nil.
  output		    ; Stream from child's output or nil.
  error			    ; Stream from child's error output or nil.
  status-hook		    ; Closure to call when PROC changes status.
  plist			    ; Place for clients to stash tings.
  cookie)		    ; List of the number of pipes from the subproc.

(setf (documentation 'process-p 'function)
  "Return t if $process is a process, else return ()")

(setf (documentation 'process-pid 'function)
  "Return the process ID, an integer, for $process.")

(setf (documentation 'process-status-hook 'function)
  "Return the function that is called whenever $process's status changes.
   The function takes the $process as a required argument.

   This is `setf'able.")

(setf (documentation 'process-exit-code 'function)
  "Return either the exit code for $process, if it is :exited, or the
   termination signal of $process if it is :signaled.

   The result is arbitrary for processes that are still alive.")

(setf (documentation 'process-core-dumped 'function)
  "Return t if a Unix signal terminated the $process and caused it to dump
   a Unix core image.")

(setf (documentation 'process-pty 'function)
  "Return the two-way stream connected to $PROCESS's Unix PTY connection if
   there is one, else ().")

(setf (documentation 'process-plist 'function)
  "Return annotations of $process.

   Setf'able.

   This is available solely for users to associate information easily with
   $process.")

(defun %print-process (proc stream depth)
  (declare (ignore depth))
  (format stream "#<process ~D ~S>"
	  (process-pid proc)
	  (process-status proc)))

;;; PROCESS-STATUS -- Public.
;;;
(defun process-status (proc)
  "Return the current status of process $proc.  The result is one of
   :running, :stopped, :exited, :signaled."
  (get-processes-status-changes)
  (process-%status proc))

;;; PROCESS-WAIT -- Public.
;;;
(defun process-wait (process &optional check-for-stopped)
  "Wait for $process to finish.  Return $process.  If $check-for-stopped is
   t, also return when $process has stopped."
  (loop
    (case (process-status process)
      (:running)
      (:stopped
       (when check-for-stopped
	 (return)))
      (t
       (when (zerop (car (process-cookie process)))
	 (return))))
    (system:serve-all-events 1))
  process)

#-hpux
;;; FIND-CURRENT-FOREGROUND-PROCESS -- internal
;;;
(defun find-current-foreground-process (proc)
  "Find the current foreground process group id of $proc."
  (alien:with-alien ((result c-call:int))
    (multiple-value-bind
	(wonp error)
	(unix:unix-ioctl (system:fd-stream-fd (ext:process-pty proc))
			 unix:TIOCGPGRP
			 (alien:alien-sap (alien:addr result)))
      (or wonp (error "TIOCPGRP ioctl failed: ~S"
		      (unix:get-unix-error-msg error)))
      result))
  (process-pid proc))

;;; PROCESS-KILL -- public
;;;
(defun process-kill (proc signal &optional (whom :pid))
  "Send the Unix $signal to process $proc.  $signal should be the number of
   the signal or a keyword with the Unix name (for example, :sigsegv).

   $WHOM should be one of the following:

    :pid
	Send the signal to $process only.

    :process-group
	Send the signal to $process's group.

    :pty-process-group
        Send the signal to the process group currently in the foreground on
        the Unix PTY connected to $process.  Useful to signal a program
        running under the shell instead of signalling the shell itself.  If
        `process-pty' of $process is (), throw an error."
  (let ((pid (ecase whom
	       ((:pid :process-group)
		(process-pid proc))
	       (:pty-process-group
		#-hpux
		(find-current-foreground-process proc)))))
    (multiple-value-bind
	(okay errno)
	(case whom
	  #+hpux
	  (:pty-process-group
	   (unix:unix-ioctl (system:fd-stream-fd (process-pty proc))
			    unix:TIOCSIGSEND
			    (system:int-sap
			     (unix:unix-signal-number signal))))
	  ((:process-group #-hpux :pty-process-group)
	   (unix:unix-killpg pid signal))
	  (t
	   (unix:unix-kill pid signal)))
      (cond ((not okay)
	     (values nil errno))
	    ((and (eql pid (process-pid proc))
		  (= (unix:unix-signal-number signal) unix:sigcont))
	     (setf (process-%status proc) :running)
	     (setf (process-exit-code proc) nil)
	     (when (process-status-hook proc)
	       (funcall (process-status-hook proc) proc))
	     t)
	    (t
	     t)))))

;;; PROCESS-ALIVE-P -- public
;;;
(defun process-alive-p (process)
  "Return t if $process is still alive (i.e. if $process's status is either
   :running or :stopped) else ()."
  (let ((status (process-status process)))
    (if (or (eq status :running)
	    (eq status :stopped))
      t
      ())))

;;; PROCESS-CLOSE -- public
;;;
(defun process-close (process)
  "Close all the streams connected to $process and finish maintaining the
   status slot.  This can be called after use of the process to reclaim
   system resources."
  (macrolet ((frob (stream abort)
	       `(when ,stream (close ,stream :abort ,abort))))
    (frob (process-pty    process)   t) ; Don't FLUSH-OUTPUT to dead process.
    (frob (process-input  process)   t) ; 'cause it will generate SIGPIPE.
    (frob (process-output process) nil)
    (frob (process-error  process) nil))
  (system:block-interrupts
   (setf *active-processes* (delete process *active-processes*)))
  process)

;;; SIGCHLD-HANDLER -- Internal.
;;;
;;; This is the handler for sigchld signals that RUN-PROGRAM establishes.
;;;
(defun sigchld-handler (ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (get-processes-status-changes))

;;; GET-PROCESSES-STATUS-CHANGES -- Internal.
;;;
(defun get-processes-status-changes ()
  (loop
    (multiple-value-bind (pid what code core)
			 (wait3 t t)
      (unless pid
	(return))
      (let ((proc (find pid *active-processes* :key #'process-pid)))
	(when proc
	  (setf (process-%status proc) what)
	  (setf (process-exit-code proc) code)
	  (setf (process-core-dumped proc) core)
	  (when (process-status-hook proc)
	    (funcall (process-status-hook proc) proc))
	  (when (or (eq what :exited)
		    (eq what :signaled))
	    (system:block-interrupts
	      (setf *active-processes*
		    (delete proc *active-processes*)))))))))


;;;; RUN-PROGRAM and close friends.

(defvar *close-on-error* ()
  "List of file descriptors to close when `run-program' exits due to an
   error.")
(defvar *close-in-parent* ()
  "List of file descriptors to close when `run-program' returns in the
   parent.")
(defvar *handlers-installed* ()
  "List of handlers installed by `run-program'.")

;;; FIND-A-PTY -- internal
;;;
;;; Finds a pty that is not in use. Returns three values: the file
;;; descriptor for the master side of the pty, the file descriptor for the
;;; slave side of the pty, and the name of the tty device for the slave
;;; side.
;;;
#-(or linux irix)
(defun find-a-pty ()
  "Returns the master fd, the slave fd, and the name of the tty"
  (dolist (char '(#\p #\q))
    (dotimes (digit 16)
      (let* ((master-name (format nil "/dev/pty~C~x" char digit))
	     (master-fd (unix:unix-open master-name
					unix:o_rdwr
					#o666)))
	(when master-fd
	  (let* ((slave-name (format nil "/dev/tty~C~x" char digit))
		 (slave-fd (unix:unix-open slave-name
					   unix:o_rdwr
					   #o666)))
	    (when slave-fd
	      ; Maybe put a vhangup here?
              #-glibc2
	      (alien:with-alien ((stuff (alien:struct unix:sgttyb)))
		(let ((sap (alien:alien-sap stuff)))
		  (unix:unix-ioctl slave-fd unix:TIOCGETP sap)
		  (setf (alien:slot stuff 'unix:sg-flags) #o300) ; EVENP|ODDP
		  (unix:unix-ioctl slave-fd unix:TIOCSETP sap)
		  (unix:unix-ioctl master-fd unix:TIOCGETP sap)
		  (setf (alien:slot stuff 'unix:sg-flags)
			(logand (alien:slot stuff 'unix:sg-flags)
				(lognot 8))) ; ~ECHO
		  (unix:unix-ioctl master-fd unix:TIOCSETP sap)))
	      (return-from find-a-pty
			   (values master-fd
				   slave-fd
				   slave-name)))
	  (unix:unix-close master-fd))))))
  (error "Failed to find a pty."))

#+()
(defun find-a-pty ()
  "Return the master fd, the slave fd, and the name of the tty."
  (alien:with-alien ((master-fd c-call:int) (slave-fd c-call:int))
    (if (zerop (unix:unix-openpty (alien:alien-sap (alien:addr master-fd))
				  (alien:alien-sap (alien:addr slave-fd))
				  (int-sap 0)
				  (int-sap 0)
				  (int-sap 0)))
	1
	2)))

#+linux
(alien:def-alien-routine ("unlockpt" unlockpt) c-call:int
  (fd c-call:int))

#+linux
(alien:def-alien-routine ("ptsname" ptsname) c-call:c-string
  (fd c-call:int))

#+linux
(defun find-a-pty ()
  "Return the master fd, the slave fd, and the name of the tty."
  (let ((master-fd (unix:unix-open "/dev/ptmx"
					      unix:o_rdwr
					      #o666)))
    (if (plusp master-fd)
	    (progn
	      (when (minusp (unlockpt master-fd))
		(unix:unix-close master-fd)
		(error "unlockpt"))
	      (let ((slave-name (ptsname master-fd)))
		(when (eq slave-name (int-sap 0))
		  (unix:unix-close master-fd)
		  (error "ptsname"))
		(let ((slave-fd (unix:unix-open slave-name
						unix:o_rdwr
						#o666)))
; FIX
; 		  ; Maybe put a vhangup here?
; 		  #-glibc2
; 		  (alien:with-alien ((stuff (alien:struct unix:sgttyb)))
; 				    (let ((sap (alien:alien-sap stuff)))
; 				      (unix:unix-ioctl slave-fd unix:TIOCGETP sap)
; 				      (setf (alien:slot stuff 'unix:sg-flags) #o300) ; EVENP|ODDP
; 				      (unix:unix-ioctl slave-fd unix:TIOCSETP sap)
; 				      (unix:unix-ioctl master-fd unix:TIOCGETP sap)
; 				      (setf (alien:slot stuff 'unix:sg-flags)
; 					    (logand (alien:slot stuff 'unix:sg-flags)
; 						    (lognot 8))) ; ~ECHO
; 				      (unix:unix-ioctl master-fd unix:TIOCSETP sap)))
		  (values master-fd
			  slave-fd
			  slave-name)))))))

#+irix
(alien:def-alien-routine ("_getpty" c-getpty) c-call:c-string
  (fildes c-call:int :out)
  (oflag c-call:int)
  (mode c-call:int)
  (nofork c-call:int))

#+irix
(defun find-a-pty ()
  "Returns the master fd, the slave fd, and the name of the tty."
  (multiple-value-bind (line master-fd)
    (c-getpty (logior unix:o_rdwr unix:o_ndelay) #o600 0)
    (let* ((slave-name line)
	   (slave-fd (unix:unix-open slave-name unix:o_rdwr #o666)))
      (when slave-fd
	; Maybe put a vhangup here?
        #-glibc2
	(alien:with-alien ((stuff (alien:struct unix:sgttyb)))
          (let ((sap (alien:alien-sap stuff)))
	    (unix:unix-ioctl slave-fd unix:TIOCGETP sap)
	    (setf (alien:slot stuff 'unix:sg-flags) #o300) ; EVENP|ODDP
	    (unix:unix-ioctl slave-fd unix:TIOCSETP sap)
	    (unix:unix-ioctl master-fd unix:TIOCGETP sap)
	    (setf (alien:slot stuff 'unix:sg-flags)
		  (logand (alien:slot stuff 'unix:sg-flags)
			  (lognot 8))) ; ~ECHO
	    (unix:unix-ioctl master-fd unix:TIOCSETP sap)))
	(return-from find-a-pty
		     (values master-fd
			     slave-fd
			     slave-name))))
    (unix:unix-close master-fd))
  (error "Could not find a pty."))

;;; OPEN-PTY -- internal
;;;
(defun open-pty (pty cookie)
  (when pty
    (multiple-value-bind
	(master slave name)
	(find-a-pty)
      (push master *close-on-error*)
      (push slave *close-in-parent*)
      (when (streamp pty)
	(multiple-value-bind (new-fd errno) (unix:unix-dup master)
	  (or new-fd
	      (error "Failed to unix:unix-dup ~D: ~A"
		     master (unix:get-unix-error-msg errno)))
	  (push new-fd *close-on-error*)
	  (copy-descriptor-to-stream new-fd pty cookie)))
      (values name
	      (system:make-fd-stream master :input t :output t)))))

(defmacro round-bytes-to-words (n)
  `(logand (the fixnum (+ (the fixnum ,n) 3)) (lognot 3)))

(defun string-list-to-c-strvec (string-list)
  ;;
  ;; Make a pass over string-list to calculate the amount of memory
  ;; needed to hold the strvec.
  (let ((string-bytes 0)
	;; We need an extra for the null, and an extra 'cause exect clobbers
	;; argv[-1].
	(vec-bytes (* #-alpha 4 #+alpha 8 (+ (length string-list) 2))))
    (declare (fixnum string-bytes vec-bytes))
    (dolist (s string-list)
      (check-type s simple-string)
      (incf string-bytes (round-bytes-to-words (1+ (length s)))))
    ;;
    ;; Now allocate the memory and fill it in.
    (let* ((total-bytes (+ string-bytes vec-bytes))
	   (vec-sap (system:allocate-system-memory total-bytes))
	   (string-sap (sap+ vec-sap vec-bytes))
	   (i #-alpha 4 #+alpha 8))
      (declare (type (and unsigned-byte fixnum) total-bytes i)
	       (type system:system-area-pointer vec-sap string-sap))
      (dolist (s string-list)
	(declare (simple-string s))
	(let ((n (length s)))
	  ;;
	  ;; Blast the string into place
	  (kernel:copy-to-system-area (the simple-string s)
				      (* vm:vector-data-offset vm:word-bits)
				      string-sap 0
				      (* (1+ n) vm:byte-bits))
	  ;;
	  ;; Blast the pointer to the string into place
	  (setf (sap-ref-sap vec-sap i) string-sap)
	  (setf string-sap (sap+ string-sap (round-bytes-to-words (1+ n))))
	  (incf i #-alpha 4 #+alpha 8)))
      ;; Blast in last null pointer
      (setf (sap-ref-sap vec-sap i) (int-sap 0))
      (values vec-sap (sap+ vec-sap #-alpha 4 #+alpha 8) total-bytes))))

(defmacro with-c-strvec ((var str-list) &body body)
  (let ((sap (gensym "SAP-"))
	(size (gensym "SIZE-")))
    `(multiple-value-bind
	 (,sap ,var ,size)
	 (string-list-to-c-strvec ,str-list)
       (unwind-protect
	   (progn
	     ,@body)
	 (system:deallocate-system-memory ,sap ,size)))))

(defun %spawn (program args env pty-name stdin stdout stderr)
  "Fork $program, setting the standard streams to $stdin, $stdout and
   $stderr."
  (let ((pid (unix:unix-fork)))
    (or (zerop pid) (return-from %spawn pid))

    ;; Put the process in a unique process group.
    #+hpux
    (unix:unix-setsid)
    #+(or linux svr4)
    (unix:unix-setpgrp 0 0)
    #-(or linux svr4 hpux)
    (unix:unix-setpgrp 0 (unix:unix-getpid))

    (when pty-name
      ;; Make the process part of some other pty.
      (let ((fd (unix:unix-open "/dev/tty" unix:o_rdwr 0)))
	(when (> fd 0)
	  (alien:with-alien ((alien-zero c-call:int 0))
	    (unix:unix-ioctl fd unix:TIOCNOTTY
			     (alien:alien-sap (alien:addr alien-zero))))
	  (unix:unix-close fd)))
      (let ((fd (unix:unix-open pty-name unix:o_rdwr 0)))
	(unix:unix-dup2 fd 0)
	(unix:unix-dup2 fd 1)
	(unix:unix-dup2 fd 2)
	(unix:unix-close fd)))

    ;; Set up standard streams.
    (if (>= stdin 0) (unix:unix-dup2 stdin 0))
    (if (>= stdout 0) (unix:unix-dup2 stdout 1))
    (if (>= stderr 0) (unix:unix-dup2 stderr 2))

    ;; Close all other file descriptors.
    (loop
      for fd downfrom (1- #+svr4 (unix:unix-sysconf
				  #| FIX |# unix:_SC_OPEN_MAX)
			  #-svr4 (unix:unix-getdtablesize))
      to 3 do
      (unix:unix-close fd))

    ;; Exec the program.
    (unix:unix-execve program args env)

    ;; Exec failed, so try /bin/sh.
    (unix:unix-execve "/bin/sh"
		      (cons "sh" (cons program (cdr args)))
		      env)

    ;; Shell exec failed, exit with error.
    (unix:unix-exit 1)))

#|
(%spawn "/home/matt/src/tests/c/parsing" nil nil nil 0 0 0)
(%spawn "/home/matt/src/tests/c/parsing" nil *environment-list* nil 0 0 0)
(%spawn "/home/matt/bin/test" nil nil nil 0 0 0)
(%spawn "/home/matt/bin/test" nil *environment-list* nil 0 0 0)

(ed::with-pop-up-display (stream)
	(ext::run-program "/home/matt/src/tests/c/parsing" nil :output stream))

(ed::with-pop-up-display (stream)
	(ext::run-program "/home/matt/src/tests/c/parsing" nil
			  :input t
			  :output t))

(let ((cookie (list 0)))
  (multiple-value-bind (pty-name)
		       (open-pty t cookie)
    (ed::message "pty-name: ~S" pty-name)
    (%spawn "/home/matt/src/tests/c/parsing" nil nil pty-name 0 0 0)))

(ed::with-pop-up-display (stream)
	(ext::run-program "/home/matt/bin/test" nil :output stream))
|#

;;; RUN-PROGRAM -- public
;;;
;;; RUN-PROGRAM uses fork and execve to run a program. Strange stuff
;;; happens to keep the unix state of the world coherent.
;;;
;;; The child process needs to get it's input from somewhere, and send it's
;;; output (both standard and error) to somewhere. We have to do different
;;; things depending on where these somewheres really are.
;;;
;;; For input, there are five options:
;;; - T: Just leave fd 0 alone. Pretty simple.
;;; - "file": Read from the file. We need to open the file and pull the
;;; descriptor out of the stream. The parent should close this stream after
;;; the child is up and running to free any storage used in the parent.
;;; - NIL: Same as "file", but use "/dev/null" as the file.
;;; - :STREAM: Use unix-pipe to create two descriptors. Use system:make-fd-stream
;;; to create the output stream on the writeable descriptor, and pass the
;;; readable descriptor to the child. The parent must close the readable
;;; descriptor for EOF to be passed up correctly.
;;; - a stream: If it's a fd-stream, just pull the descriptor out of it.
;;; Otherwise make a pipe as in :STREAM, and copy everything across.
;;;
;;; For output, there are n options:
;;; - T: Leave descriptor 1 alone.
;;; - "file": dump output to the file.
;;; - NIL: dump output to /dev/null.
;;; - :STREAM: return a stream that can be read from.
;;; - a stream: if it's a fd-stream, use the descriptor in it. Otherwise, copy
;;; stuff from output to stream.
;;;
;;; For error, there are all the same options as output plus:
;;; - :OUTPUT: redirect to the same place as output.
;;;
;;; RUN-PROGRAM returns a process struct for the process if the fork worked,
;;; and NIL if it did not.
;;;
(defun run-program (program args
			    &key
			    (env *environment-list*)
			    (wait t)
			    pty input if-input-does-not-exist output
			    (if-output-exists :error)
			    (error :output)
			    (if-error-exists :error)
			    status-hook)
  "Run $program in a child process.

   $program should be a pathname or string naming the program.  $args
   should be a list of strings to passes to $program as normal Unix
   parameters.

   Return either a process structure (which is accessible via [process
   accessors]) if the program starts successfully, or () if forking the
   child process fails.

   Other than sharing file descriptors as explained in the keyword argument
   descriptions below, close all file descriptors in the child process
   before running the program.  `process-close' can always be called after
   `run-program' to reclaim the system resources of the process.  This is
   necessary only when :stream is supplied for one of $input, $output, or
   $error, and when $pty is true.

   Keyword arguments:

     $env
         An a-list mapping keywords and simple-strings.  If $env is
         specified, use the value given, otherwise combine the environment
         passed to Lisp with the one specified.

     $wait
         If true, wait until the child process terminates, else continue
         running Lisp while the child process runs.

     $pty
         Either t, (), or a stream.  If true, the subprocess is established
         under a PTY.  If $pty is a stream, all output to this pty is sent
         to this stream.  If $pty is t the `process-pty' slot is filled in
         with a stream connected to a PTY that can read output and write
         input.

     $input
         How the program gets input.  If specified as a string, it is the
         name of a file that contains input for the child process.  The
         file is opened as standard input.  If specified as () then
         standard input is the file \"/dev/null\".  If specified as t, the
         program uses the current standard input.  This may cause problems
         if $wait is () since two processes may use the terminal at the same
         time.  If specified as :stream, then the `process-input' slot
         contains an output stream.  Anything written to this stream goes
         to the program as input.  $input may also be an input stream that
         already contains all the input for the process.  In this case all
         the input is read from the stream before returning.

     $if-input-does-not-exist
         If the input file does not exist:
            ()       simply return ()
            :create  create the named file
            :error   signal an error.

     $output
         Either t, (), a pathname, a stream, or :stream.  If t, the
         standard output for the current process is inherited.  This may be
         problematic if $wait is () since two processes may write to the
         terminal at the same time.  If (), use \"/dev/null\".  If a
         pathname, use the file so specified.  If a stream, write all the
         output from the process to this stream. If :stream, fill the
         `process-output' slot with a stream that can be read to get the
         output.

     $if-output-exists
        If the output file already exists:
           :error      generate an error if the file already exists.
           :supersede  replace the file with the output from the program.
           :append     append the output from the program to the file.
           ()          simply return ().

     $error
         Like $output, only set the file to the program's standard error.
         Additionally, $error can be :output, in which case route the
         program's error output to the same place specified for $output.
         If specified as :stream, fill the `process-error' slot with a
         stream similar to the `process-output' slot of $output.

     $if-error-exists
         What to do if the error output file already exists.  As for
         $if-output-exists.

     $status-hook
         A function to call whenever the process changes status.  This is
         especially useful when specifying $wait as ().  The function takes
         the process as a required argument.

     $before-execve
         A function to run in the child process before it becomes the
         program to run.  This is useful for actions that must only affect
         the child."
  ;; Make sure the interrupt handler is installed.
  (system:enable-interrupt unix:sigchld #'sigchld-handler)
  ;; Make sure all the args are okay.
  (or (every #'simple-string-p args)
      (error "All args to program must be simple strings -- ~S." args))
  ;; Pre-pend the program to the argument list.
  (push (namestring program) args)
  (when (remote-pathname-p (current-directory))
    (let ((account (internet:make-inet-account
		    (remote-pathname-host (current-directory)))))
      (internet:fill-from-netrc account)
      ;; FIX cater for other protocols (telnet?)
      ;;     ~ (setq args (internet:prepare-remote-command args))
      (push "-Y" args)
      (push (format () "~A@~A"
		    (internet::account-user account)
		    (internet::inet-account-server account))
	    args)
      (push "ssh" args)
      (setq program "ssh")))
  ;; Clear random specials used by GET-DESCRIPTOR-FOR to communicate
  ;; cleanup info.  Also, establish proc at this level so we can return it.
  (let (*close-on-error* *close-in-parent* *handlers-installed* proc)
    (unwind-protect
	(let ((pfile (os-namestring (merge-pathnames program "path:") t t))
	      (cookie (list 0)))
	  (or pfile (error "Failed to find program: ~S" program))
	  (multiple-value-bind
	      (stdin input-stream)
	      (get-descriptor-for input cookie :direction :input
				  :if-does-not-exist if-input-does-not-exist)
	    (multiple-value-bind
		(stdout output-stream)
		(get-descriptor-for output cookie :direction :output
				    :if-exists if-output-exists)
	      (multiple-value-bind
		  (stderr error-stream)
		  (if (eq error :output)
		      (values stdout output-stream)
		      (get-descriptor-for error cookie :direction :output
					  :if-exists if-error-exists))
		(multiple-value-bind (pty-name pty-stream)
				     (open-pty pty cookie)
		  ;; Ensure that the the process struct is installed in
		  ;; *active-processes* before the child death notification
		  ;; occurs.
		  (system:block-interrupts
		   (let ((child-pid
			  (without-gcing
			   (%spawn pfile args env pty-name
				   stdin stdout stderr))))
		     (or child-pid
			 (error "Failed to fork child process: ~A"
				(unix:get-unix-error-msg)))
		     (setf proc (make-process :pid child-pid
					      :%status :running
					      :pty pty-stream
					      :input input-stream
					      :output output-stream
					      :error error-stream
					      :status-hook status-hook
					      :cookie cookie))
		     (push proc *active-processes*))))))))
      (dolist (fd *close-in-parent*)
	(unix:unix-close fd))
      (unless proc
	(dolist (fd *close-on-error*)
	  (unix:unix-close fd))
	(dolist (handler *handlers-installed*)
	  (system:remove-fd-handler handler))))
    (and wait proc (process-wait proc))
    proc))

;;; COPY-DESCRIPTOR-TO-STREAM -- internal
;;;
;;; Install a handler for any input that shows up on the file descriptor.
;;; The handler reads the data and writes it to the stream.
;;;
(defun copy-descriptor-to-stream (descriptor stream cookie)
  (incf (car cookie))
  (let ((string (make-string 256))
	handler)
    (setf handler
	  (system:add-fd-handler descriptor :input
	    #'(lambda (fd)
		(declare (ignore fd))
		(loop
		  (or handler (return))
		  (multiple-value-bind
		      (result readable/errno)
		      (unix:unix-select (1+ descriptor) (ash 1 descriptor)
					0 0 0)
		    (cond ((null result)
			   (error "Failed to select on sub-process: ~A"
				  (unix:get-unix-error-msg readable/errno)))
			  ((zerop result)
			   (return))))
		  (alien:with-alien ((buf (alien:array c-call:char 256)))
		    (multiple-value-bind
			(count errno)
			(unix:unix-read descriptor (alien-sap buf) 256)
		      (cond ((or (and (null count)
				      (eql errno unix:eio))
				 (eql count 0))
			     (system:remove-fd-handler handler)
			     (setf handler nil)
			     (decf (car cookie))
			     (unix:unix-close descriptor)
			     (return))
			    ((null count)
			     (system:remove-fd-handler handler)
			     (setf handler nil)
			     (decf (car cookie))
			     (error "Could not read input from sub-process: ~A"
				    (unix:get-unix-error-msg errno)))
			    (t
			     (kernel:copy-from-system-area
			      (alien-sap buf) 0
			      string (* vm:vector-data-offset vm:word-bits)
			      (* count vm:byte-bits))
			     (write-string string stream
					   :end count)))))))))))

;;; GET-DESCRIPTOR-FOR -- internal
;;;
;;; Find a file descriptor to use for object given the direction. Returns
;;; the descriptor. If object is :STREAM, returns the created stream as the
;;; second value.
;;;
(defun get-descriptor-for (object cookie &rest keys &key direction
				  &allow-other-keys)
  (cond ((eq object t)
	 ;; No new descriptor is needed.
	 (values -1 nil))
	((eq object nil)
	 ;; Use /dev/null.
	 (multiple-value-bind
	     (fd errno)
	     (unix:unix-open "/dev/null"
			     (case direction
			       (:input unix:o_rdonly)
			       (:output unix:o_wronly)
			       (t unix:o_rdwr))
			     #o666)
	   (or fd (error "Could not open \"/dev/null\": ~A"
			 (unix:get-unix-error-msg errno)))
	   (push fd *close-in-parent*)
	   (values fd nil)))
	((eq object :stream)
	 (multiple-value-bind
	     (read-fd write-fd)
	     (unix:unix-pipe)
	   (or read-fd
	       (error "Failed to create pipe: ~A"
		      (unix:get-unix-error-msg write-fd)))
	   (case direction
	     (:input
	      (push read-fd *close-in-parent*)
	      (push write-fd *close-on-error*)
	      (let ((stream (system:make-fd-stream write-fd :output t)))
		(values read-fd stream)))
	     (:output
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (let ((stream (system:make-fd-stream read-fd :input t)))
		(values write-fd stream)))
	     (t
	      (unix:unix-close read-fd)
	      (unix:unix-close write-fd)
	      (error "Direction must be either :INPUT or :OUTPUT, not ~S"
		     direction)))))
	((or (pathnamep object) (stringp object))
	 (with-open-stream (file (apply #'open object keys))
	   (multiple-value-bind
	       (fd errno)
	       (unix:unix-dup (system:fd-stream-fd file))
	     (cond (fd
		    (push fd *close-in-parent*)
		    (values fd nil))
		   (t
		    (error "Failed to duplicate file descriptor: ~A"
			   (unix:get-unix-error-msg errno)))))))
	((system:fd-stream-p object)
	 (values (system:fd-stream-fd object) nil))
	((streamp object)
	 (ecase direction
	   (:input
	    (dotimes (count
		      256
		      (error "Failed to open a temporary file in /tmp"))
	      (let* ((name (format nil "/tmp/.run-program-~D" count))
		     (fd (unix:unix-open name
					 (logior unix:o_rdwr
						 unix:o_creat
						 unix:o_excl)
					 #o666)))
		(unix:unix-unlink name)
		(when fd
		  (let ((newline (string #\Newline)))
		    (loop
		      (multiple-value-bind
			  (line no-cr)
			  (read-line object nil nil)
			(unless line
			  (return))
			(unix:unix-write fd line 0 (length line))
			(if no-cr
			  (return)
			  (unix:unix-write fd newline 0 1)))))
		  (unix:unix-lseek fd 0 unix:l_set)
		  (push fd *close-in-parent*)
		  (return (values fd nil))))))
	   (:output
	    (multiple-value-bind (read-fd write-fd)
				 (unix:unix-pipe)
	      (unless read-fd
		(error "Failed to create pipe: ~A"
		       (unix:get-unix-error-msg write-fd)))
	      (copy-descriptor-to-stream read-fd object cookie)
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (values write-fd nil)))))
	(t
	 (error "Erroneous option to run-program: ~S" object))))
