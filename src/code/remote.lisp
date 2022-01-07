;;; A simple remote procedure call mechanism implemented on top of
;;; wire.lisp.

(in-package "WIRE")

(export '(remote remote-value remote-value-bind create-request-server
	  destroy-request-server connect-to-remote-server))


#[ The REMOTE Package

The remote package provides simple RPC facility including interfaces for
creating servers, connecting to already existing servers, and calling
functions in other Lisp processes.  The routines for establishing a
connection between two processes, `create-request-server' and
`connect-to-remote-server', return wire structures.  A wire maintains the
current state of a connection, and all the RPC forms require a wire to
indicate where to send requests.

[ Connecting Servers and Clients ]
[ Remote Evaluations             ]
[ Remote Objects                 ]
[ Host Addresses                 ]
]#

#[ Connecting Servers and Clients

Before a client can connect to a server, it must know the network address
on which the server accepts connections.  Network addresses consist of a
host address or name, and a port number.  Host addresses are either a
string of the form vancouver.slisp.cs.cmu.edu or a 32 bit unsigned integer.
Port numbers are 16 bit unsigned integers.  Note: port in this context has
a separate concept from Mach ports and message passing.

When a process wants to receive connection requests (that is, become a
server), it first picks an integer to use as the port.  Only one server
(Lisp or otherwise) can use a given port number on a given machine at any
particular time.  This can be an iterative process to find a free port:
picking an integer and calling `create-request-server'.  This function
signals an error if the chosen port is unusable.  The suggested approach is
to write a loop using `handler-case', catching conditions of type error,
since this function does not signal more specific conditions.

{function:wire:create-request-server}
{function:wire:destroy-request-server}
{function:wire:connect-to-remote-server}
]#

#[ Remote Evaluations

After the server and client have connected, they each have a wire allowing
function evaluation in the other process.  This RPC mechanism has three
flavors: for side-effect only, for a single value, and for multiple values.

Only a limited number of data types can be sent across wires as arguments
for remote function calls and as return values: integers inclusively less
than 32 bits in length, symbols, lists, and remote-objects ([Remote
Objects]).  The system sends symbols as two strings, the package name and
the symbol name, and if the package doesn't exist remotely, the remote
process signals an error.  FIX The system ignores other slots of symbols.
Lists may be any tree of the above valid data types.  To send other data
types you must represent them in terms of these supported types.  For
example, you could use `prin1-to-string' locally, send the string, and
use `read-from-string' remotely.

{function:wire:remote}
{function:wire:wire-force-output}
{function:wire:remote-value}
{function:wire:remote-value-bind}
]#

#[ Remote Objects

The wire mechanism only directly supports a limited number of data types
for transmission as arguments for remote function calls and as return
values: integers inclusively less than 32 bits in length, symbols, lists.
Sometimes it is useful to allow remote processes to refer to local data
structures without allowing the remote process to operate on the data.  We
have remote-objects to support this without the need to represent the data
structure in terms of the above data types, to send the representation to
the remote process, to decode the representation, to later encode it again,
and to send it back along the wire.

You can convert any Lisp object into a remote-object.  When you send
a remote-object along a wire, the system simply sends a unique token
for it.  In the remote process, the system looks up the token and
returns a remote-object for the token.  When the remote process
needs to refer to the original Lisp object as an argument to a
remote call back or as a return value, it uses the remote-object it
has which the system converts to the unique token, sending that
along the wire to the originating process.  Upon receipt in the
first process, the system converts the token back to the same
(`eq') remote-object.

{function:wire:make-remote-object}
{function:wire:remote-object-p}
{function:wire:remote-object-local-p}
{function:wire:remote-object-eq}
{function:wire:remote-object-value}
{function:wire:forget-remote-translation}
]#

#[ Host Addresses

The operating system maintains a database of all the valid host
addresses.  You can use this database to convert between host names
and addresses and vice-versa.

{function:ext:lookup-host-entry}

The functions `ext:host-entry-name', `ext:host-entry-aliases',
`ext:host-entry-addr-list' and `ext:host-entry-addr' each return the
indicated slot from the host-entry structure.  `host-entry-addr' returns
the primary (first) address from the list returned by
`host-entry-addr-list'.
]#


(defstruct remote-wait
  value1 value2 value3 value4 value5
  abort
  finished)

(defvar *pending-returns* nil
  "AList of wire . remote-wait structs")

;;; MAYBE-NUKE-REMOTE-WAIT -- internal
;;;
;;; If the remote wait has finished, remove the external translation.
;;; Otherwise, mark the remote wait as finished so the next call to
;;; MAYBE-NUKE-REMOTE-WAIT will really nuke it.
;;;
(defun maybe-nuke-remote-wait (remote)
  (cond ((remote-wait-finished remote)
	 (forget-remote-translation remote)
	 t)
	(t
	 (setf (remote-wait-finished remote)
	       t)
	 nil)))

;;; REMOTE -- public
;;;
;;; Execute the body remotely.  Subforms are executed locally in the lexical
;;; environment of the macro call.  No values are returned.
;;;
(defmacro remote (wire-form &body forms)
  "Arrange for the process at the other end of $wire to invoke each of the
   functions in $forms, returning a () values.  A call to
   `wire-force-output' ensures that the system sends the remote evaluation
   requests over the wire.

   Each form in $forms looks like a function call textually, and has some
   odd constraints and semantics.  The function position of the form must
   be the symbolic name of a function.  Evaluate each of the argument
   subforms for each of the $forms locally in the current context, sending
   these values as the arguments for the functions.

   Consider the following example:

      (defun write-remote-string (str)
	(declare (simple-string str))
	(wire:remote wire
	  (write-string str)))

   Pass the value of \"str\" in the local process over the wire with a
   request to invoke `write-string' on the value."
  (let ((wire (gensym)))
    `(let ((,wire ,wire-form))
       ,@(mapcar #'(lambda (form)
 		     `(wire-output-funcall ,wire
					   ',(car form)
					   ,@(cdr form)))
	   forms)
       (values))))

;;; REMOTE-VALUE-BIND -- public
;;;
;;; Send to remote forms. First, a call to the correct dispatch routine based
;;; on the number of args, then the actual call. The dispatch routine will get
;;; the second funcall and fill in the correct number of arguments.
;;; Note: if there are no arguments, we don't even wait for the function to
;;; return, cause we can kind of guess at what the currect results would be.
;;;
(defmacro remote-value-bind (wire-form vars form &rest body)
  "Bind $vars to the multiple values of $form (which is executed remotely).

   If the client unwinds past the call to `remote-value-bind', the server
   continues running, and the system flushes the values the server sends
   back.

   Only execute the forms in $body if the remote function returned normally
   (versus throwing or signalling), otherwise return ()."
  (cond
   ((null vars)
    `(progn
       (remote ,wire-form ,form)
       ,@body))
   (t
    (let ((remote (gensym))
	  (wire (gensym)))
      `(let* ((,remote (make-remote-wait))
	      (,wire ,wire-form)
	      (*pending-returns* (cons (cons ,wire ,remote)
				       *pending-returns*)))
	 (unwind-protect
	     (let ,vars
	       (remote ,wire
		 (,(case (length vars)
		     (1 'do-1-value-call)
		     (2 'do-2-value-call)
		     (3 'do-3-value-call)
		     (4 'do-4-value-call)
		     (5 'do-5-value-call)
		     (t 'do-n-value-call))
		  (make-remote-object ,remote))
		 ,form)
	       (wire-force-output ,wire)
	       (loop
		 (system:serve-all-events)
		 (when (remote-wait-finished ,remote)
		   (return)))
	       (unless (remote-wait-abort ,remote)
		 ,(case (length vars)
		    (1 `(setf ,(first vars) (remote-wait-value1 ,remote)))
		    (2 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)))
		    (3 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)
			      ,(third vars) (remote-wait-value3 ,remote)))
		    (4 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)
			      ,(third vars) (remote-wait-value3 ,remote)
			      ,(fourth vars) (remote-wait-value4 ,remote)))
		    (5 `(setf ,(first vars) (remote-wait-value1 ,remote)
			      ,(second vars) (remote-wait-value2 ,remote)
			      ,(third vars) (remote-wait-value3 ,remote)
			      ,(fourth vars) (remote-wait-value4 ,remote)
			      ,(fifth vars) (remote-wait-value5 ,remote)))
		    (t
		     (do ((remaining-vars vars (cdr remaining-vars))
			  (form (list 'setf)
				(nconc form
				       (list (car remaining-vars)
					     `(pop values)))))
			 ((null remaining-vars)
			  `(let ((values (remote-wait-value1 ,remote)))
			     ,form)))))
		 ,@body))
	   (maybe-nuke-remote-wait ,remote)))))))

;;; REMOTE-VALUE -- public
;;;
;;; Alternate interface to getting the single return value of a remote
;;; function. Works pretty much just the same, except the single value is
;;; returned.
;;;
(defmacro remote-value (wire-form form &optional
				  (on-server-unwind
				   `(error "Remote server unwound")))
  "Similar to `remote'.  Execute the single $form remotely.

   Return the value returned by the function call in the remote process.
   The value must be a valid type the system can send over a wire.  Output
   to the wire is forced, as with `wire-force-output'.

   If the client unwinds past the call to `remote-value', the server
   continues running, and the system flushes the value the server sends
   back.

   The form $on-server-unwind is evaluated if the server unwinds instead of
   returning.

   If the server unwinds past the remotely requested call, instead of
   returning normally, `remote-value' returns two values, () and t.
   Otherwise this returns the result of the remote evaluation and ()."
  (let ((remote (gensym))
	(wire (gensym)))
    `(let* ((,remote (make-remote-wait))
	    (,wire ,wire-form)
	    (*pending-returns* (cons (cons ,wire ,remote)
				     *pending-returns*)))
       (unwind-protect
	   (progn
	     (remote ,wire
	       (do-1-value-call (make-remote-object ,remote))
	       ,form)
	     (wire-force-output ,wire)
	     (loop
	       (system:serve-all-events)
	       (when (remote-wait-finished ,remote)
		 (return))))
	 (maybe-nuke-remote-wait ,remote))
       (if (remote-wait-abort ,remote)
	 ,on-server-unwind
	 (remote-wait-value1 ,remote)))))

;;; DEFINE-FUNCTIONS -- internal
;;;
;;; Defines two functions, one that the client runs in the server, and one
;;; that the server runs in the client:
;;;
;;; DO-n-VALUE-CALL -- internal
;;;
;;; Executed by the remote process. Reads the next object off the wire and
;;; sends the value back. Unwind-protect is used to make sure we send something
;;; back so the requestor doesn't hang.
;;;
;;; RETURN-n-VALUE -- internal
;;;
;;; The remote procedure returned the given value, so fill it in the
;;; remote-wait structure. Note, if the requestor has aborted, just throw
;;; the value away.
;;;
(defmacro define-functions (values)
  (let ((do-call (intern (format nil "~:@(do-~D-value-call~)" values)))
	(return-values (intern (format nil "~:@(return-~D-value~:P~)" values)))
	(vars nil))
    (dotimes (i values)
      (push (gensym) vars))
    (setf vars (nreverse vars))
    `(progn
       (defun ,do-call (result)
	 (let (worked ,@vars)
	   (unwind-protect
	       (progn
		 (multiple-value-setq ,vars
		   (wire-get-object *current-wire*))
		 (setf worked t))
	     (if worked
	       (remote *current-wire*
		 (,return-values result ,@vars))
	       (remote *current-wire*
		 (remote-return-abort result)))
	     (wire-force-output *current-wire*))))
       (defun ,return-values (remote ,@vars)
	 (let ((result (remote-object-value remote)))
	   (unless (maybe-nuke-remote-wait result)
	     ,@(let ((setf-forms nil))
		 (dotimes (i values)
		   (push `(setf (,(intern (format nil
						  "~:@(remote-wait-value~D~)"
						  (1+ i)))
				 result)
				,(nth i vars))
			 setf-forms))
		 (nreverse setf-forms))))
	 nil))))

(define-functions 1)
(define-functions 2)
(define-functions 3)
(define-functions 4)
(define-functions 5)

;;; DO-N-VALUE-CALL -- internal
;;;
;;; For more values then 5, all the values are rolled into a list and passed
;;; back as the first value, so we use RETURN-1-VALUE to return it.
;;;
(defun do-n-value-call (result)
  (let (worked values)
    (unwind-protect
	(progn
	  (setf values
		(multiple-value-list (wire-get-object *current-wire*)))
	  (setf worked t))
      (if worked
	(remote *current-wire*
	  (return-1-values result values))
	(remote *current-wire*
	  (remote-return-abort result)))
      (wire-force-output *current-wire*))))

;;; REMOTE-RETURN-ABORT -- internal
;;;
;;; The remote call aborted instead of returned.
;;;
(defun remote-return-abort (result)
  (setf result (remote-object-value result))
  (unless (maybe-nuke-remote-wait result)
    (setf (remote-wait-abort result) t)))

;;; SERVE-REQUESTS -- internal
;;;
;;; Serve all pending requests on the given wire.
;;;
(defun serve-requests (wire on-death)
  (handler-bind
      ((wire-eof #'(lambda (condition)
		     (declare (ignore condition))
		     (system:invalidate-descriptor (wire-fd wire))
		     (unix:unix-close (wire-fd wire))
		     (dolist (pending *pending-returns*)
		       (when (eq (car pending)
				 wire)
			 (unless (maybe-nuke-remote-wait (cdr pending))
			   (setf (remote-wait-abort (cdr pending))
				 t))))
		     (when on-death
		       (funcall on-death))
		     (return-from serve-requests (values))))
       (wire-error #'(lambda (condition)
		       (declare (ignore condition))
		       (system:invalidate-descriptor (wire-fd wire)))))
    (loop
      (unless (wire-listen wire)
	(return))
      (wire-get-object wire)))
  (values))

;;; NEW-CONNECTION -- internal
;;;
;;; Maybe build a new wire and add it to the servers list of fds.  If the
;;; user Supplied a function, close the socket if it returns NIL.
;;; Otherwise, install the wire.
;;;
(defun new-connection (socket addr on-connect)
  (let ((wire (make-wire socket))
	(on-death nil))
    (if (or (null on-connect)
	    (multiple-value-bind (okay death-fn)
				 (funcall on-connect wire addr)
	      (setf on-death death-fn)
	      okay))
      (system:add-fd-handler socket :input
	#'(lambda (socket)
	    (declare (ignore socket))
	    (serve-requests wire on-death)))
      (ext:close-socket socket))))

;;; REQUEST-SERVER structure
;;;
;;; Just a simple handle on the socket and system:serve-event handler that
;;; make up a request server.
;;;
(defstruct (request-server
	    (:print-function %print-request-server))
  socket
  handler)

(defun %print-request-server (rs stream depth)
  (declare (ignore depth))
  (print-unreadable-object (rs stream :type t)
    (format stream "for ~D" (request-server-socket rs))))

;;; CREATE-REQUEST-SERVER -- Public.
;;;
;;; Create a TCP/IP listener on the given port.  If anyone tries to connect
;;; to it, call NEW-CONNECTION to do the connecting.
;;;
(defun create-request-server (port &optional on-connect)
  "Create a request server on $port.

   Whenever a program connects to it, call $on-connect with the newly
   created wire and the address of the connector.

   $on-connect should return two values, whether to accept the connection
   and a function that the system should call when the connection
   terminates.  If the first return is (), terminate and release the
   connection; otherwise, accept it.  If $on-connect is () allow all
   connections.

   Return the manifestation of the server that `destroy-request-server'
   takes to terminate the connection."
  (let* ((socket (ext:create-inet-listener port))
	 (handler (system:add-fd-handler socket :input
		    #'(lambda (socket)
			(multiple-value-bind
			    (newconn addr)
			    (ext:accept-tcp-connection socket)
			  (new-connection newconn addr on-connect))))))
    (make-request-server :socket socket
			 :handler handler)))

;;; DESTROY-REQUEST-SERVER -- Public.
;;;
;;; Remove the request server from SERVER's list of file descriptors and
;;; close the socket behind it.
;;;
(defun destroy-request-server (server)
  "Quit accepting connections to the request server $server."
  (system:remove-fd-handler (request-server-handler server))
  (ext:close-socket (request-server-socket server))
  nil)

;;; CONNECT-TO-REMOTE-SERVER -- Public.
;;;
;;; A handler is installed to handle return values, etc.
;;;
(defun connect-to-remote-server (hostname port &optional on-death)
  "Attempt to connect to a remote server at the $port} on $host and return
   the created wire structure if it is successful.  If $on-death is true,
   then invoke $on-death when the created connection terminates."
  (let* ((socket (ext:connect-to-inet-socket hostname port))
	 (wire (make-wire socket)))
    (system:add-fd-handler socket :input
      #'(lambda (socket)
	  (declare (ignore socket))
	  (serve-requests wire on-death)))
    wire))
