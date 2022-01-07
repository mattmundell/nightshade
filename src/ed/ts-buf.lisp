;;; Code for processing input to and output from slaves using typescript
;;; streams.  It maintains the stuff that hacks on the typescript buffer
;;; and maintains its state.

(in-package "ED")

#[ Typescripts

Both slave buffers and background buffers are typescripts.  The typescript
protocol allows other processes to do stream-oriented interaction in a the editor
buffer similar to that of a terminal.  When there is a typescript in a buffer,
the `Typescript' minor mode is present.  Some of the commands described in
this section are also used by `Eval' mode (page pagerefeval-mode.)

Typescripts are simple to use.  The editor inserts output from the process
into the buffer.  To give the process input, use normal editing to insert
the input at the end of the buffer, and then type Return to confirm sending
the input to the process.

{command:Confirm Typescript Input}
{evariable:Unwedge Interactive Input Confirm}

The editor maintains a history of interactive inputs.

{command:Kill Interactive Input}
{command:Next Interactive Input}
{command:Previous Interactive Input}
{command:Search Previous Interactive Input}
{evariable:Interactive History Length}
{evariable:Minimum Interactive Input Length}
{command:Reenter Interactive Input}
{command:Interactive Beginning of Line}
{evariable:Input Wait Alarm}
{evariable:Slave GC Alarm}

Some typescripts have associated information which these commands access,
allowing the editor to control the process which uses the typescript.

{command:Typescript Slave BREAK}
{command:Typescript Slave to Top Level}
{command:Typescript Slave Status}
]#

(defevar "Input Wait Alarm"
  "The action to take when a slave Lisp goes into an input wait on a
   typescript that isn't currently displayed in any window.

   The following are legal values:

     :loud-message
	Display a message with `loud-message'.

     :message
	Display a message with `message'.

     ()
	Don't do anything."
  :value :loud-message)


;;;; Structures.

(defstruct (ts-data
	    (:print-function
	     (lambda (ts s d)
	       (declare (ignore ts d))
	       (write-string "#<TS Data>" s)))
	    (:constructor
	     make-ts-data (buffer
			   &aux
			   (fill-mark (copy-mark (buffer-end-mark buffer)
						 :right-inserting)))))
  buffer		      ; The buffer we are in
  stream		      ; Stream in the slave.
  wire			      ; Wire to slave
  server		      ; Server info struct.
  fill-mark		      ; Mark where output goes.  This is actually the
			      ;   "Buffer Input Mark" which is :right-inserting,
			      ;   and we make sure it is :left-inserting for
			      ;   inserting output.
  )


;;;; Output routines.

;;; TS-BUFFER-OUTPUT-STRING --- internal interface.
;;;
;;; Called by the slave to output stuff in the typescript.  Can also be
;;; called by other random parts of the editor when they want to output
;;; stuff to the buffer.  Since this is called for value from the slave, we
;;; have to be careful about what values we return, so the result can be
;;; sent back.  It is called for value only as a synchronization thing.
;;;
;;; Whenever the output is gratuitous, we want it to go behind the prompt.
;;; When it's gratuitous, and we're not at the line-start, then we can output
;;; it normally, but we also make sure we end the output in a newline for
;;; visibility's sake.
;;;
(defun ts-buffer-output-string (ts string &optional gratuitous-p)
  "Outputs STRING to the typescript described with TS. The output is inserted
   before the fill-mark and the current input."
  (when (wire:remote-object-p ts)
    (setf ts (wire:remote-object-value ts)))
  (system:block-interrupts
    (let ((mark (ts-data-fill-mark ts)))
      (cond ((and gratuitous-p (not (start-line-p mark)))
	     (with-mark ((m mark :left-inserting))
	       (line-start m)
	       (insert-string m string)
	       (unless (start-line-p m)
		 (insert-character m #\newline))))
	    (t
	     (setf (mark-kind mark) :left-inserting)
	     (insert-string mark string)
	     (when (and gratuitous-p (not (start-line-p mark)))
	       (insert-character mark #\newline))
	     (setf (mark-kind mark) :right-inserting)))))
  (values))

;;; TS-BUFFER-FINISH-OUTPUT --- internal interface.
;;;
;;; Redisplays the windows. Used by ts-stream in order to finish-output.
;;;
(defun ts-buffer-finish-output (ts)
  (declare (ignore ts))
  (redisplay)
  nil)

;;; TS-BUFFER-CHARPOS --- internal interface.
;;;
;;; Used by ts-stream in order to find the charpos.
;;;
(defun ts-buffer-charpos (ts)
  (mark-charpos (ts-data-fill-mark (if (wire:remote-object-p ts)
				       (wire:remote-object-value ts)
				       ts))))

;;; TS-BUFFER-LINE-LENGTH --- internal interface.
;;;
;;; Used by ts-stream to find out the line length.  Returns the width of the
;;; first window, or 80 if there are no windows.
;;;
(defun ts-buffer-line-length (ts)
  (let* ((ts (if (wire:remote-object-p ts)
		 (wire:remote-object-value ts)
		ts))
	 (window (car (buffer-windows (ts-data-buffer ts)))))
    (if window
	(window-width window)
	80))) ; Seems like a good number to me.


;;;; Input routines.

(defun ts-buffer-ask-for-input (remote)
  (let* ((ts (wire:remote-object-value remote))
	 (buffer (ts-data-buffer ts)))
    (unless (buffer-windows buffer)
      (let ((input-wait-alarm
	     (if (editor-bound-p 'input-wait-alarm
				  :buffer buffer)
	       (variable-value 'input-wait-alarm
			       :buffer buffer)
	       (variable-value 'input-wait-alarm
			       :global))))
	(when input-wait-alarm
	  (if (eq input-wait-alarm :loud-message)
	      (loud-message "Waiting for input in buffer ~A."
			    (buffer-name buffer))
	      (message "Waiting for input in buffer ~A."
		       (buffer-name buffer)))))))
  nil)

(defun ts-buffer-clear-input (ts)
  (let* ((ts (if (wire:remote-object-p ts)
		 (wire:remote-object-value ts)
		 ts))
	 (buffer (ts-data-buffer ts))
	 (mark (ts-data-fill-mark ts)))
    (or (mark= mark (buffer-end-mark buffer))
	(with-mark ((start mark))
	  (line-start start)
	  (let ((prompt (region-to-string (region start mark)))
		(end (buffer-end-mark buffer)))
	    (or (zerop (mark-charpos end))
		(insert-character end #\Newline))
	    (insert-string end "[Input Cleared]")
	    (insert-character end #\Newline)
	    (insert-string end prompt)
	    (move-mark mark end)))))
  ())

(defun ts-buffer-set-stream (ts stream)
  (let ((ts (if (wire:remote-object-p ts)
		(wire:remote-object-value ts)
		ts)))
    (setf (ts-data-stream ts) stream)
    (wire:remote (ts-data-wire ts)
      (ts-stream-set-line-length stream (ts-buffer-line-length ts))))
  ())


;;;; Typescript mode.

(defun setup-typescript (buffer)
  (let ((ts (make-ts-data buffer)))
    (defevar "Current Package"
      "The package used for evaluation of Lisp in this buffer."
      :buffer buffer)

    (defevar "Typescript Data"
      "The ts-data structure for this buffer"
      :buffer buffer
      :value ts)

    (defevar "Buffer Input Mark"
      "Beginning of typescript input in this buffer."
      :value (ts-data-fill-mark ts)
      :buffer buffer)

    (defevar "Interactive History"
      "A ring of the regions input to the editor typescript."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))

    (defevar "Interactive Pointer"
      "Pointer into the editor typescript input history."
      :buffer buffer
      :value 0)

    (defevar "Searching Interactive Pointer"
      "Pointer into *Interactive History*."
      :buffer buffer
      :value 0)))

(defmode "Typescript"
  :setup-function #'setup-typescript
  :documentation "The Typescript mode is used to interact with slave lisps.")


;;; TYPESCRIPTIFY-BUFFER -- Internal interface.
;;;
;;; Buffer creation code for eval server connections calls this to setup a
;;; typescript buffer, tie things together, and make some local editor
;;; variables.
;;;
(defun typescriptify-buffer (buffer server wire)
  (setf (buffer-minor-mode buffer "Typescript") t)
  (let ((info (variable-value 'typescript-data :buffer buffer)))
    (setf (ts-data-server info) server)
    (setf (ts-data-wire info) wire)
    (defevar "Server Info"
      "Server-info structure for this buffer."
      :buffer buffer :value server)
    (defevar "Current Eval Server"
      "The Server-Info object for the server currently used for evaluation and
       compilation."
      :buffer buffer :value server)
    info))

(defun ts-buffer-wire-died (ts)
  (setf (ts-data-stream ts) nil)
  (setf (ts-data-wire ts) nil)
  (buffer-end (ts-data-fill-mark ts) (ts-data-buffer ts))
  (ts-buffer-output-string ts (format nil "~%~%Slave died!~%")))

(defun unwedge-typescript-buffer ()
  (typescript-slave-to-top-level-command)
  (buffer-end (current-point) (current-buffer)))

(defevar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-typescript-buffer
  :mode "Typescript")

(defevar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Move to the prompt? "
  :mode "Typescript")

;;; TYPESCRIPT-DATA-OR-LOSE -- internal
;;;
;;; Return the typescript-data for the current buffer, or die trying.
;;;
(defun typescript-data-or-lose ()
  (if (editor-bound-p 'typescript-data)
      (let ((ts (value typescript-data)))
	(if ts
	    ts
	    (editor-error "Failed to find the typescript data?")))
      (editor-error "Must be in a typescript buffer.")))

(defcommand "Confirm Typescript Input" ()
  "Send text that has been inserted at the end of the current buffer to the
   process reading on the buffer's typescript.  Before sending the text,
   move the point to the end of the buffer and insert a newline.

   Input may be edited as much as is desired before it is confirmed.  The
   `Indent New Line' command is often useful for inserting newlines before
   confirming the input.

   If the process reading on the buffer's typescript is busy, then queue
   the text instead of sending it immediately.  Any number of inputs may be
   typed ahead in this fashion.  Make sure that the inputs and outputs get
   interleaved correctly so that when all input has been read, the buffer
   looks the same as it would have if the input had been received
   immediately.

   If the buffer's point is before the start of the input area, then
   various actions can occur.  When `Unwedge Interactive Input Confirm' is
   set ask the user if the the input buffer should be fixed, which
   typically results in refreshing the input area at the end of the buffer.
   This also has the effect of throwing the slave Lisp to top level, which
   cancels any pending operations or queued input.  This is the only way to
   be sure the user is cleanly set up again after messing up the input
   region.  When this is (), simply beep and echo that the input area is
   invalid."
  (let ((ts (typescript-data-or-lose)))
    (let ((input (get-interactive-input)))
      (when input
	(let ((string (region-to-string input)))
	  (declare (simple-string string))
	  (insert-character (current-point) #\newline)
	  (wire:remote (ts-data-wire ts)
	    (ts-stream-accept-input (ts-data-stream ts)
				    (concatenate 'simple-string
						 string
						 (string #\newline))))
	  (wire:wire-force-output (ts-data-wire ts))
	  (buffer-end (ts-data-fill-mark ts)
		      (ts-data-buffer ts)))))))

(defcommand "Typescript Slave Break" ()
  "Interrupt the slave Lisp process associated with this interactive buffer,
   causing it to invoke BREAK and hence enter a break loop."
  (send-oob-to-slave "B"))

(defcommand "Typescript Slave to Top Level" ()
  "Interrupt the slave Lisp process associated with this interactive buffer,
   causing it to throw to the top level read-eval-print loop."
  (send-oob-to-slave "T"))

(defcommand "Typescript Slave Status" ()
  "Interrupt the current process and causes it to print status information
   on `error-output':

         lisp; Used 0:06:03, 3851 faults.  In: SYSTEM:SERVE-EVENT

   Include in the message the process run-time, the total number of page
   faults and the name of the currently running function.  Useful for
   determining whether the slave is stuck in a loop."
  (send-oob-to-slave "S"))

(defun send-oob-to-slave (string)
  (let* ((ts (typescript-data-or-lose))
	 (wire (ts-data-wire ts))
	 (socket (wire:wire-fd wire)))
    (unless socket
      (editor-error "The slave is no longer alive."))
    (ext:send-character-out-of-band socket (schar string 0))))
