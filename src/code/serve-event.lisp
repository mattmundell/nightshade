;;; Port and X event handling.

(in-package "LISP")

(in-package "SYSTEM")

(export '(with-fd-handler add-fd-handler remove-fd-handler invalidate-descriptor
	  serve-event serve-all-events wait-until-fd-usable
	  make-object-set object-set-operation *xwindow-table*
	  map-xwindow add-xwindow-object remove-xwindow-object))

(in-package "EXTENSIONS")

(export '(*display-event-handlers*))

(in-package "LISP")


#[ Event Dispatching with SERVE-EVENT

It is common to have multiple activities simultaneously operating in the
same Lisp process.  Furthermore, Lisp programmers tend to expect a flexible
development environment.  It must be possible to load and modify
application programs without requiring modifications to other running
programs.  Nightshade achieves this by having a central scheduling
mechanism based on an event-driven, object-oriented paradigm.

An event is some interesting happening that should cause the Lisp process
to wake up and do something.  These events include X events and activity on
Unix file descriptors.  The object-oriented mechanism is only available
with the first two, and it is optional with X events as described later in
this chapter.  In an X event, the window ID is the object capability and
the X event type is the operation code.  The Unix file descriptor input
mechanism simply consists of an association list of a handler to call when
input shows up on a particular file descriptor.

[ Object Sets                                   ]
[ The SERVE-EVENT Function                      ]
[ Using SERVE-EVENT with Unix File Descriptors  ]
[ Using SERVE-EVENT with the CLX Interface to X ]
[ SERVE-EVENT Example                           ]
]#


;;;; Object set stuff.

#[ Object Sets

An object set is a collection of objects that have the same implementation
for each operation.  Externally the object is represented by the object
capability and the operation is represented by the operation code.  Within
Lisp, the object is represented by an arbitrary Lisp object, and the
implementation for the operation is represented by an arbitrary Lisp
function.  The object set mechanism maintains this translation from the
external to the internal representation.

{function:system:make-object-set}
{function:system:object-set-operation}
{function:system:add-xwindow-object}
{function:system:remove-xwindow-object}
]#

;;; Hashtable from ports to objects.  Each entry is a cons (object . set).
;;;
;(defvar *port-table* (make-hash-table :test #'eql))

;;; Hashtable from windows to objects.  Each entry is a cons (object . set).
;;;
(defvar *xwindow-table* (make-hash-table :test #'equalp))

(defstruct (object-set
	    (:constructor make-object-set
			  (name &optional
				(default-handler #'default-default-handler)))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Object Set ~S>" (object-set-name s)))))
  name					; Name, for descriptive purposes.
  (table (make-hash-table :test #'eq))  ; Message-ID or xevent-type --> handler fun.
  default-handler)

(setf (documentation 'make-object-set 'function)
  "Make a new object set.

   $name identifies the object set when it is printed.

   $default-handler is the function used as a handler when an undefined
   operation occurs on an object in the set.

   The serve-<operation> functions exported from the extensions package are
   useful for defining operations for X events.
   `system:add-xwindow-object' adds objects.

   Initially the object set is empty (of objects and operations).")

;;; Default-Default-Handler  --  Internal
;;;
;;; If no such operation defined, signal an error.
;;;
(defun default-default-handler (object)
  (error "You lose, object: ~S" object)) ; FIX

; FIX enable port version?
; FIX hard to find with editor.

;;; MAP-XWINDOW and MAP-PORT return as multiple values the object and
;;; object set mapped to by a xwindow or port in *xwindow-table* or
;;; *port-table*.
;;;
(macrolet ((defmapper (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "MAP-" (symbol-name name)))
		      (,name)
		 ,(format nil "Return as multiple values the object and ~
		               object-set mapped to by ~A."
			  (string-downcase (symbol-name name)))
		 (let ((temp (gethash ,name ,table)))
		   (if temp
		       (values (car temp) (cdr temp))
		       (values nil nil))))))
  ;(defmapper port *port-table*)
  (defmapper xwindow *xwindow-table*))

;;; ADD-PORT-OBJECT and ADD-XWINDOW-OBJECT store an object/object-set pair
;;; mapped to by a port or xwindow in either *port-table* or *xwindow-table*.
;;;
(macrolet ((def-add-object (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "ADD-" (symbol-name name)
					    "-OBJECT"))
		      (,name object object-set)

		 ,(format ()
  "Add $port or $window to $object-set.  $object is an arbitrary Lisp
   object that is associated with the $port or $window capability.  $window
   is a CLX window.  When an event occurs, `system:serve-event' passes
   $object as an argument to the handler function."
			  (string-downcase (symbol-name name)))
		 (check-type object-set object-set)
		 (setf (gethash ,name ,table) (cons object object-set))
		 object)))
  ;(def-add-object port *port-table*)
  (def-add-object xwindow *xwindow-table*))

;;; REMOVE-PORT-OBJECT and REMOVE-XWINDOW-OBJECT remove a port or xwindow and
;;; its associated object/object-set pair from *port-table* or *xwindow-table*.
;;;
(macrolet ((def-remove-object (name table)
	      `(defun ,(intern (concatenate 'simple-string
					    "REMOVE-" (symbol-name name)
					    "-OBJECT"))
		      (,name)
		 ,(format ()
			  "Remove ~A and its associated object/object-set pair."
			  (string-downcase (symbol-name name)))
		 (remhash ,name ,table))))
  ;(def-remove-object port *port-table*)
  (def-remove-object xwindow *xwindow-table*))

;;; Object-Set-Operation  --  Public
;;;
;;; Look up the handler function for a given message ID.
;;;
(defun object-set-operation (object-set message-id)
  "Return the handler function that is the implementation of the operation
   corresponding to $message-id in $object-set.

   When set with `setf', the setter function establishes the new handler.

   The serve-<operation> functions exported from the extensions package for
   X events call this on behalf of the user when announcing a new operation
   for an object set."
  (check-type object-set object-set)
  (check-type message-id fixnum)
  (values (gethash message-id (object-set-table object-set))))

;;; %Set-Object-Set-Operation  --  Internal
;;;
;;; The setf inverse for Object-Set-Operation.
;;;
(defun %set-object-set-operation (object-set message-id new-value)
  (check-type object-set object-set)
  (check-type message-id fixnum)
  (setf (gethash message-id (object-set-table object-set)) new-value))
;;;
(defsetf object-set-operation %set-object-set-operation
  "Sets the handler function for an object set operation.")


;;;; File descriptor IO noise.

#[ Using SERVE-EVENT with Unix File Descriptors

Object sets are not available for use with file descriptors, as there are
only two operations possible on file descriptors: input and output.
Instead, a handler for either input or output can be registered with
`system:serve-event' for a specific file descriptor.  Whenever any input
shows up, or output is possible on this file descriptor, the function
associated with the handler for that descriptor is funcalled with the
descriptor as it's single argument.

{function:system:add-fd-handler}
{function:system:remove-fd-handler}
{function:system:with-fd-handler}
{function:system:wait-until-fd-usable}
{function:system:invalidate-descriptor}
]#

(defstruct (handler
	    (:print-function %print-handler)
	    (:constructor make-handler (direction descriptor function)))
  ;; Reading or writing...
  (direction () :type (member :input :output))
  ;;
  ;; File descriptor this handler is tied to.
  (descriptor 0 :type (mod #.unix:fd-setsize))

  active		      ; T iff this handler is running.
  (function () :type function) ; Function to call.
  bogus			      ; T if this descriptor is bogus.
  )

(defun %print-handler (handler stream depth)
  (declare (ignore depth))
  (format stream "#<Handler for ~A on ~:[~;BOGUS ~]descriptor ~D: ~S>"
	  (handler-direction handler)
	  (handler-bogus handler)
	  (handler-descriptor handler)
	  (handler-function handler)))

(defvar *descriptor-handlers* ()
  "List of all the currently active handlers for file descriptors")

;;; ADD-FD-HANDLER -- public
;;;
;;; Add a new handler to *descriptor-handlers*.
;;;
(defun add-fd-handler (fd direction function)
  "Install and return a new handler for file descriptor $fd.

   $direction can be either :input if the system should invoke the handler
   when input is available or :output if the system should invoke the
   handler when output is possible.

   Return a unique object representing the handler.

   $function must take one argument, the file descriptor.

   The value returned should be passed to `system:remove-fd-handler' after
   last use."
  (assert (member direction '(:input :output))
	  (direction)
	  "Invalid direction ~S, must be either :INPUT or :OUTPUT" direction)
  (let ((handler (make-handler direction fd function)))
    (push handler *descriptor-handlers*)
    handler))

;;; REMOVE-FD-HANDLER -- public
;;;
;;; Remove an old handler from *descriptor-handlers*.
;;;
(defun remove-fd-handler (handler)
  "Remove $handler from the list of active handlers."
  (setf *descriptor-handlers*
	(delete handler *descriptor-handlers*
		:test #'eq)))

;;; INVALIDATE-DESCRIPTOR -- public
;;;
;;; Search *descriptor-handlers* for any reference to fd, and nuke 'em.
;;;
(defun invalidate-descriptor (fd)
  "Remove any handlers refering to $fd.

   This should only be used in drastic cases (such as I/O errors).
   Normally, `remove-fd-handler' is used instead."
  (setf *descriptor-handlers*
	(delete fd *descriptor-handlers*
		:key #'handler-descriptor)))

;;; WITH-FD-HANDLER -- Public.
;;;
;;; Add the handler to *descriptor-handlers* for the duration of BODY.
;;;
(defmacro with-fd-handler ((fd direction function) &rest body)
  "Establish a handler with `system:add-fd-handler' for the duration of
   $body.  $direction should be either :INPUT or :OUTPUT, $fd is the file
   descriptor to use, and $function is the function to call whenever $fd is
   usable."
  (let ((handler (gensym)))
    `(let (,handler)
       (unwind-protect
	   (progn
	     (setf ,handler (add-fd-handler ,fd ,direction ,function))
	     ,@body)
	 (when ,handler
	   (remove-fd-handler ,handler))))))

;;; HANDLER-DESCRIPTORS-ERROR -- Internal.
;;;
;;; First, get a list and mark bad file descriptors.  Then signal an error
;;; offering a few restarts.
;;;
(defun handler-descriptors-error ()
  (let ((bogus-handlers nil))
    (dolist (handler *descriptor-handlers*)
      (unless (or (handler-bogus handler)
		  (unix:unix-fstat (handler-descriptor handler)))
	(setf (handler-bogus handler) t)
	(push handler bogus-handlers)))
    (restart-case (error "~S ~[have~;has a~:;have~] bad file descriptor~:P."
			 bogus-handlers (length bogus-handlers))
      (remove-them () :report "Remove bogus handlers."
       (setf *descriptor-handlers*
	     (delete-if #'handler-bogus *descriptor-handlers*)))
      (retry-them () :report "Retry bogus handlers."
       (dolist (handler bogus-handlers)
	 (setf (handler-bogus handler) nil)))
      (continue () :report "Go on, leaving handlers marked as bogus."))))


;;;; Serve-all-events, serve-event, and friends.

#[ The SERVE-EVENT Function

The `system:serve-event' function is the standard way for an application
to wait for something to happen.  For example, the Lisp system calls
`system:serve-event' when it wants input from X or a terminal stream.
The idea behind `system:serve-event' is that it knows the appropriate
action to take when any interesting event happens.  If an application calls
`system:serve-event' when it is idle, then any other applications with
pending events can run.  This allows several applications to run "at the
same time" without interference, even though there is only one thread of
control.  Note that if an application is waiting for input of any kind,
then other applications will get events.

{function:system:serve-event}
{function:system:serve-all-events}
]#

(declaim (start-block wait-until-fd-usable serve-event serve-all-events))

;;; DECODE-TIMEOUT  --  Internal
;;;
;;; Break a real timeout into seconds and microseconds.
;;;
(defun decode-timeout (timeout)
  (declare (values (or index null) index))
  (typecase timeout
    (integer (values timeout 0))
    (null (values nil 0))
    (real
     (multiple-value-bind (q r)
			  (truncate (coerce timeout 'single-float))
       (declare (type index q) (single-float r))
       (values q (the (values index t) (truncate (* r 1f6))))))
    (t
     (error "Timeout is not a real number or NIL: ~S" timeout))))

;;; WAIT-UNTIL-FD-USABLE -- Public.
;;;
;;; Wait until FD is usable for DIRECTION. The timeout given to serve-event
;;; is recalculated each time through the loop so that WAIT-UNTIL-FD-USABLE
;;; will timeout at the correct time irrespective of how many events are
;;; handled in the meantime.
;;;
(defun wait-until-fd-usable (fd direction &optional timeout)
  "Wait for up to $timeout seconds for $fd to become usable for $direction
   (either :input or :output).  If $timeout is (), waits for up to ever."
  (declare (type (or real null) timeout))
  (let (usable)
    (multiple-value-bind (to-sec to-usec)
			 (decode-timeout timeout)
      (declare (type (or index null) to-sec to-usec))
      (multiple-value-bind
	  (stop-sec stop-usec)
	  (if to-sec
	      (multiple-value-bind (okay start-sec start-usec)
				   (unix:unix-gettimeofday)
		(declare (ignore okay))
		(let ((usec (+ to-usec start-usec))
		      (sec (+ to-sec start-sec)))
		  (declare (type (unsigned-byte 31) usec sec))
		  (if (>= usec 1000000)
		      (values (1+ sec) (- usec 1000000))
		      (values sec usec))))
	      (values 0 0))
	(declare (type (unsigned-byte 31) stop-sec stop-usec))
	(with-fd-handler (fd direction #'(lambda (fd)
					   (declare (ignore fd))
					   (setf usable t)))
	  (loop
	    (sub-serve-event to-sec to-usec)

	    (when usable
	      (return t))

	    (when timeout
	      (multiple-value-bind (okay sec usec)
				   (unix:unix-gettimeofday)
		(declare (ignore okay))
		(when (or (> sec stop-sec)
			  (and (= sec stop-sec) (>= usec stop-usec)))
		  (return nil))
		(setq to-sec (- stop-sec sec))
		(cond ((> usec stop-usec)
		       (decf to-sec)
		       (setq to-usec (- (+ stop-usec 1000000) usec)))
		      (t
		       (setq to-usec (- stop-usec usec))))))))))))

(defvar *display-event-handlers* nil
  "This is an alist mapping displays to user functions to be called when
   SYSTEM:SERVE-EVENT notices input on a display connection.  Do not modify
   this directly; use EXT:ENABLE-CLX-EVENT-HANDLING.  A given display
   should be represented here only once.")

;;; SERVE-ALL-EVENTS -- public
;;;
;;; Wait for up to timeout seconds for an event to happen. Make sure all
;;; pending events are processed before returning.
;;;
(defun serve-all-events (&optional timeout)
  "Serve all pending events, with $timeout.  Return t after servicing at
   least one event, and () otherwise."
  (do ((res nil)
       (sval (serve-event timeout) (serve-event 0)))
      ((null sval) res)
    (setq res t)))

;;; SERVE-EVENT -- public
;;;
;;; Serve a single event.
;;;
(defun serve-event (&optional timeout)
  "Receive on all ports and Xevents and dispatch to the appropriate handler
   function.

   If $timeout is specified, wait the specified time (in seconds) and then
   return, otherwise wait for the next event.  If $timeout is zero then
   poll for any events immediately available for processing.  Return t if
   at least one event is serviced, and () otherwise.  Depending on the
   application, a program may want to repeat the call to serve-event as
   long as the return is true.

   If input is available on any designated file descriptor, then call the
   appropriate handler function supplied by `system:add-fd-handler'.

   Since events for many different applications may arrive simultaneously,
   an application waiting for a specific event must loop on
   `system:serve-event' until the desired event happens.  Since programs
   such as the editor call `system:serve-event' for input, it is usually
   enough for other programs to rely on their handlers running when
   applications such as the editor wait on input."
  (multiple-value-bind (to-sec to-usec)
		       (decode-timeout timeout)
    (sub-serve-event to-sec to-usec)))

;;; Check for any X displays with pending events.
;;;
(defun handle-queued-clx-event ()
  (dolist (d/h *display-event-handlers*)
    (let* ((d (car d/h))
	   (disp-fd (xlib::display-fd d)))
      (declare (inline member))
      ;;
      ;; If in the *descriptor-handlers*, then we are already waiting for
      ;; input on that display, and we don't want to do it recursively.
      (when (and (dolist (hand *descriptor-handlers* t)
		   (when (and (eql (handler-descriptor hand) disp-fd)
			      (not (eq (handler-function hand)
				       #'ext::call-display-event-handler)))
		     (return ())))
		 (xlib::event-listen d))
	(handler-bind ((error #'(lambda (condx)
				  (declare (ignore condx))
				  (flush-display-events d))))
	  (unless (funcall (cdr d/h) d)
	    (disable-clx-event-handling d)
	    (error "Event-listen was true, but handler didn't handle: ~%~S"
		   d/h)))
	(return-from handle-queued-clx-event t)))))

;;; These macros are chunks of code from SUB-SERVE-EVENT.  They randomly
;;; reference the READ-FDS and WRITE-FDS Alien variables (which would be
;;; consed if passed as function arguments.)
;;;
(eval-when (compile eval)

;;; CALC-MASKS -- Internal.
;;;
;;; Initialize the fd-sets for `unix-select' and return the active
;;; descriptor count.
;;;
(defmacro calc-masks ()
  '(progn
     (unix:fd-zero read-fds)
     (unix:fd-zero write-fds)
     (let ((count 0))
       (declare (type index count))
       (dolist (handler *descriptor-handlers*)
	 (or (handler-active handler)
	     (handler-bogus handler)
	     (let ((fd (handler-descriptor handler)))
	       (ecase (handler-direction handler)
		 (:input (unix:fd-set fd read-fds))
		 (:output (unix:fd-set fd write-fds)))
	       (when (> fd count)
		 (setf count fd)))))
       (1+ count))))

;;; Call file descriptor handlers according to the readable and writable masks
;;; returned by select.
;;;
(defmacro call-fd-handler ()
  '(let ((result ()))
     (dolist (handler *descriptor-handlers*)
       (let ((desc (handler-descriptor handler)))
	 (when (ecase (handler-direction handler)
		 (:input (unix:fd-isset desc read-fds))
		 (:output (unix:fd-isset desc write-fds)))
	   (unwind-protect
	       (progn
		 ;; FIX --
		 ;; Doesn't work -- ACK   (FIX Akira Kurihara?)
		 ;(setf (handler-active handler) t)
		 ;; -- FIX
		 (funcall (handler-function handler) desc))
	     (setf (handler-active handler) ()))
	   (ecase (handler-direction handler)
	     (:input (unix:fd-clr desc read-fds))
	     (:output (unix:fd-clr desc write-fds)))
	   (setf result t)))
       result)))

) ; eval-when (compile eval)

;;; When a *periodic-polling-function* is defined the server will not
;;; block for more than the maximum event timeout and will call the
;;; polling function if it does times out. One important use of this
;;; is to periodically call process-yield.
;;;
(declaim (type (or null function) *periodic-polling-function*))
(defvar *periodic-polling-function*
  #-mp nil #+mp #'mp:process-yield)
(declaim (type (unsigned-byte 29) *max-event-to-sec* *max-event-to-usec*))
(defvar *max-event-to-sec* 1)
(defvar *max-event-to-usec* 0)

;;; SUB-SERVE-EVENT  --  Internal
;;;
;;; Takes timeout broken into seconds and microseconds.
;;;
(defun sub-serve-event (to-sec to-usec)
  (declare (type (or null (unsigned-byte 29)) to-sec to-usec))

  (if (handle-queued-clx-event) (return-from sub-serve-event t))

  (let ((call-polling-fn ()))
    (when (and *periodic-polling-function*
	       ;; Enforce a maximum timeout.
	       (or (null to-sec)
		   (> to-sec *max-event-to-sec*)
		   (and (= to-sec *max-event-to-sec*)
			(> to-usec *max-event-to-usec*))))
      (setf to-sec *max-event-to-sec*)
      (setf to-usec *max-event-to-usec*)
      (setf call-polling-fn t))

    ;; Next, wait for something to happen.
    (alien:with-alien ((read-fds (alien:struct unix:fd-set))
		       (write-fds (alien:struct unix:fd-set)))
      (let ((count (calc-masks)))
	(multiple-value-bind
	      (value err)
	    (unix:unix-fast-select
	     count
	     (alien:addr read-fds) (alien:addr write-fds)
	     () to-sec to-usec)

	  ;; Now see what it was (if anything).
	  (cond (value
		 (cond ((zerop value)
			;; Timed out.
			(if call-polling-fn
			    (funcall *periodic-polling-function*)))
		       (t
			(call-fd-handler))))
		((eql err unix:eintr)
		 ;; We did an interrupt.
		 t)
		(t
		 ;; One of the file descriptors is bad.
		 (handler-descriptors-error)
		 ())))))))


#[ Using SERVE-EVENT with the CLX Interface to X

As described in [Object Sets], an object set is a collection of objects,
CLX windows in this case, with some set of operations, event keywords, with
corresponding implementations, the same handler functions.  Since X allows
multiple display connections from a given process, you can avoid using
object sets if every window in an application or display connection behaves
the same.  If a particular X application on a single display connection has
windows that want to handle certain events differently, then using object
sets is a convenient way to organize this since you need some way to map
the window/event combination to the appropriate functionality.

The following is a discussion of functions exported from the extensions
package that facilitate handling CLX events through `system:serve-event'.

{function:ext:open-clx-display}
{function:ext:flush-display-events}

[ Without Object Sets ]
[ With Object Sets    ]
]#

#[ Without Object Sets

Since most applications that use CLX can avoid the complexity of object sets,
these routines are described in a separate section.  The routines described in
the next section that use the object set mechanism are based on these
interfaces.

{function:ext:enable-clx-event-handling}
{function:ext:disable-clx-event-handling}
{function:ext:with-clx-event-handling}
]#

#[ With Object Sets

This section discusses the use of object sets and `system:serve-event' to
handle CLX events.  This is necessary when a single X application has
distinct windows that want to handle the same events in different ways.
Basically, you need some way of asking for a given window which way you
want to handle some event because this event is handled differently
depending on the window.  Object sets provide this feature.

For each CLX event-key symbol-name XXX (for example, key-press), there
is a function serve-XXX of two arguments, an object set and a function.
The serve-XXX function establishes the function as the handler for the
:XXX event in the object set.  As described in [Object Sets],
`system:add-xwindow-object' associates some Lisp object with a CLX window in
an object set.  When `system:serve-event' notices activity on a window, it
calls the function given to `ext:enable-clx-event-handling'.  If this
function is `ext:object-set-event-handler', it calls the function given to
`serve-XXX', passing the object given to `system:add-xwindow-object'
and the event's slots as well as a couple other arguments described below.

To use object sets in this way:

  * Create an object set.

  * Define some operations on it using the `serve-XXX' functions.

  * Add an object for every window on which you receive requests.  This can be the
    CLX window itself or some structure more meaningful to your application.

  * Call `system:serve-event' to service an X event.

{function:object-set-event-handler}
]#


#[ SERVE-EVENT Example

This section contains two examples using `system:serve-event'.  The first
one does not use object sets, and the second, slightly more complicated one
does.

[ Without Object Sets Example ]
[ With Object Sets Example    ]
]#

#[ Without Object Sets Example

This example defines an input handler for a CLX display connection.  It
only recognizes :key-press events.  The body of the example loops over
`system:serve-event' to get input.

    (in-package "SERVER-EXAMPLE")

    (defun my-input-handler (display)
      (xlib:event-case (display :timeout 0)
        (:key-press (event-window code state)
         (format t "KEY-PRESSED (Window = ~D) = ~S.~%"
                      (xlib:window-id event-window)
                 ;; FIX See Editor Extension Manual for convenient
                 ;; input mapping function.
                 (ext:translate-character display code state))
          ;; Make XLIB:EVENT-CASE discard the event.
          t)))

    (defun server-example ()
      "An example of using the SYSTEM:SERVE-EVENT function and object sets to
       handle CLX events."
      (let* ((display (ext:open-clx-display))
             (screen (display-default-screen display))
             (black (screen-black-pixel screen))
             (white (screen-white-pixel screen))
             (window (create-window :parent (screen-root screen)
                                    :x 0 :y 0 :width 200 :height 200
                                    :background white :border black
                                    :border-width 2
                                    :event-mask
                                    (xlib:make-event-mask :key-press))))
        ;; Wrap code in UNWIND-PROTECT, so we clean up after ourselves.
        (unwind-protect
            (progn
              ;; Enable event handling on the display.
              (ext:enable-clx-event-handling display #'my-input-handler)
              ;; Map the windows to the screen.
              (map-window window)
              ;; Make sure we send all our requests.
              (display-force-output display)
              ;; Call serve-event for 100,000 events or immediate timeouts.
              (dotimes (i 100000) (system:serve-event)))
          ;; Disable event handling on this display.
          (ext:disable-clx-event-handling display)
          ;; Get rid of the window.
          (destroy-window window)
          ;; Pick off any events the X server has already queued for our
          ;; windows, so we don't choke since SYSTEM:SERVE-EVENT is no longer
          ;; prepared to handle events for us.
          (loop
           (unless (deleting-window-drop-event *display* window)
            (return)))
          ;; Close the display.
          (xlib:close-display display))))

    (defun deleting-window-drop-event (display win)
      "Check for any events on win.  If there is one, remove it from the
       event queue and return t; otherwise, return nil."
      (xlib:display-finish-output display)
      (let ((result nil))
        (xlib:process-event
         display :timeout 0
         :handler #'(lambda (&key event-window &allow-other-keys)
                      (if (eq event-window win)
                          (setf result t)
                          nil)))
        result))
]#

#[ With Object Sets Example

This example involves more work, but you get a little more for your effort.
It defines two objects, input-box and slider, and establishes a :key-press
handler for each object, :key-pressed and :slider-pressed.  We have two
object sets because we handle events on the windows manifesting these
objects differently, but the events come over the same display connection.

    (in-package "SERVER-EXAMPLE")

    (defstruct (input-box (:print-function print-input-box)
                          (:constructor make-input-box (display window)))
      "Our program knows about input-boxes, and it doesn't care how they
       are implemented."
      display        ; The CLX display on which my input-box is displayed.
      window)        ; The CLX window in which the user types.
    ;;;
    (defun print-input-box (object stream n)
      (declare (ignore n))
      (format stream "#<Input-Box ~S>" (input-box-display object)))

    (defvar *input-box-windows*
            (system:make-object-set "Input Box Windows"
                                    #'ext:default-clx-event-handler))

    (defun key-pressed (input-box event-key event-window root child
                        same-screen-p x y root-x root-y modifiers time
                        key-code send-event-p)
      "This is our :key-press event handler."
      (declare (ignore event-key root child same-screen-p x y
                       root-x root-y time send-event-p))
      (format t "KEY-PRESSED (Window = ~D) = ~S.~%"
              (xlib:window-id event-window)
              ;; See Editor Extension Manual for convenient
              ;; input mapping function.
              (ext:translate-character (input-box-display input-box)
                                         key-code modifiers)))
    ;;;
    (ext:serve-key-press *input-box-windows* #'key-pressed)

    (defstruct (slider (:print-function print-slider)
                       (:include input-box)
                       (:constructor %make-slider
                                        (display window window-width max)))
      "Our program knows about sliders too, and these provide input values
       zero to max."
      bits-per-value  ; bits per discrete value up to max.
      max)            ; End value for slider.
    ;;;
    (defun print-slider (object stream n)
      (declare (ignore n))
      (format stream "#<Slider ~S  0..~D>"
              (input-box-display object)
              (1- (slider-max object))))
    ;;;
    (defun make-slider (display window max)
      (%make-slider display window
                      (truncate (xlib:drawable-width window) max)
                    max))

    (defvar *slider-windows*
            (system:make-object-set "Slider Windows"
                                    #'ext:default-clx-event-handler))

    (defun slider-pressed (slider event-key event-window root child
                           same-screen-p x y root-x root-y modifiers time
                           key-code send-event-p)
      "This is our :key-press event handler for sliders.  Probably this is
       a mouse thing, but for simplicity here we take a character typed."
      (declare (ignore event-key root child same-screen-p x y
                       root-x root-y time send-event-p))
      (format t "KEY-PRESSED (Window = ~D) = ~S  -->  ~D.~%"
              (xlib:window-id event-window)
              ;; See Editor Extension Manual for convenient
              ;; input mapping function.
              (ext:translate-character (input-box-display slider)
                                         key-code modifiers)
              (truncate x (slider-bits-per-value slider))))
    ;;;
    (ext:serve-key-press *slider-windows* #'slider-pressed)

    (defun server-example ()
      "An example of using the SYSTEM:SERVE-EVENT function and object sets to
       handle CLX events."
      (let* ((display (ext:open-clx-display))
             (screen (display-default-screen display))
             (black (screen-black-pixel screen))
             (white (screen-white-pixel screen))
             (iwindow (create-window :parent (screen-root screen)
                                     :x 0 :y 0 :width 200 :height 200
                                     :background white :border black
                                     :border-width 2
                                     :event-mask
                                     (xlib:make-event-mask :key-press)))
             (swindow (create-window :parent (screen-root screen)
                                     :x 0 :y 300 :width 200 :height 50
                                     :background white :border black
                                     :border-width 2
                                     :event-mask
                                     (xlib:make-event-mask :key-press)))
             (input-box (make-input-box display iwindow))
             (slider (make-slider display swindow 15)))
        ;; Wrap code in UNWIND-PROTECT, so we clean up after ourselves.
        (unwind-protect
            (progn
              ;; Enable event handling on the display.
              (ext:enable-clx-event-handling display
                                             #'ext:object-set-event-handler)
              ;; Add the windows to the appropriate object sets.
              (system:add-xwindow-object iwindow input-box
                                         *input-box-windows*)
              (system:add-xwindow-object swindow slider
                                         *slider-windows*)
              ;; Map the windows to the screen.
              (map-window iwindow)
              (map-window swindow)
              ;; Make sure we send all our requests.
              (display-force-output display)
              ;; Call server for 100,000 events or immediate timeouts.
              (dotimes (i 100000) (system:serve-event)))
          ;; Disable event handling on this display.
          (ext:disable-clx-event-handling display)
          (delete-window iwindow display)
          (delete-window swindow display)
          ;; Close the display.
          (xlib:close-display display))))

    (defun delete-window (window display)
      ;; Remove the windows from the object sets before destroying them.
      (system:remove-xwindow-object window)
      ;; Destroy the window.
      (destroy-window window)
      ;; Pick off any events the X server has already queued for our
      ;; windows, so we don't choke since SYSTEM:SERVE-EVENT is no longer
      ;; prepared to handle events for us.
      (loop
       (unless (deleting-window-drop-event display window)
         (return))))

    (defun deleting-window-drop-event (display win)
      "Check for any events on win.  If there is one, remove it from the
       event queue and return t; otherwise, return nil."
      (xlib:display-finish-output display)
      (let ((result nil))
        (xlib:process-event
         display :timeout 0
         :handler #'(lambda (&key event-window &allow-other-keys)
                      (if (eq event-window win)
                          (setf result t)
                          nil)))
        result))
]#
